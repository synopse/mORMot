/// Domain-Driven-Design toolbox for mORMot
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit mORMotDDD;

{
    This file is part of Synopse mORmot framework.

    Synopse mORMot framework. Copyright (C) 2015 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2015
  the Initial Developer. All Rights Reserved.

  Contributor(s):

  
  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

  Version 1.18
  - first public release, corresponding to Synopse mORMot Framework 1.18

}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  SysUtils,
  Classes,
  Contnrs,
  Variants,
  SynCommons,
  SynCrypto,
  mORMot;

{ some mORMot conventions about DDD implementation:

 * most services methods should return an enumerate:
  - Exceptions should be raised only in case of a real failure (e.g. unexpected
    execution context, service shutdown...) but most of the time an enumerate
    will be used to manage errors
  - no textual error message should be sent by the application layer: it is up
    to the end-user application to react to a given unexpected result
  - within the DDD services, CQRS methods will use a TCQRSResult enumerate
    and advanced error process, with an optional error text for debugging
  - first value should mean success, so that TInterfaceStub would let the
    test pass by returning the default 0 ordinal

 * persistence ignorance is mostly implemented via CQRS repository services:
  - we implement the "Command Query Responsibility Segregation" pattern
    to potentially increase scaling ability of reading, and allow distributed
    transactions via a service-based two-phase commit, implementing the Unit Of
    Work pattern
  - no ID should be transmitted most of the time, but write commands have to
    follow a read query which would specify the corresponding item so that
    the ID would be stored during the process
  - I*Query interfaces should have some SelectBy*() methods to change the
    current selected aggregate, which could later on be retrieved by a
    Get(out aAggregate: T....) method
  - I*Command instances would be our way of implementing the Unit Of Work
  - I*Command interfaces should have standard Add/Update/Delete methods,
    expecting a previous SelectBy*() call for Update/Delete: those methods
    should prepare the corresponding data change
  - I*Command interfaces should have Commit to perform all the pending
    changes (which may be implemented by transactions or via TSQLRestBatch)
  - I*Command interfaces should abort the process if the instance is
    released without any prior call to Commit (e.g. rollback the transaction
    or free any pending TSQLRestBatch instance)

  TODO:
   - manage Authentication expiration

}

{ *********** Some Domain-Driven-Design Common Definitions }

type
  /// abstract ancestor for all Domain-Driven-Design related Exceptions
  EDDDException = class(ESynException);

  /// Exception type linked to CQRS repository service methods
  ECQRSException = class(EDDDException);


{ *********** Persistence / Repository Interfaces }

type
  /// result enumerate for I*Query/I*Command CQRS repository service methods
  // - cqrsSuccess will map the default TInterfaceStub returned value
  // - cqrsBadRequest would indicate that the method was not called in the
  // expected workflow sequence
  // - cqrsNotFound appear after a I*Query SelectBy*() method with no match
  // - cqrsNoMoreData indicates a GetNext*() method has no more matching data
  // - cqrsDataLayerError indicates a low-level error at database level
  // - cqrsInvalidContent for any I*Command method with invalid aggregate input
  // value (e.g. a missing field)
  // - cqrsAlreadyExists for a I*Command.Add method with a primay key conflict
  // - cqrsNoPriorQuery for a I*Command.Update/Delete method with no prior
  // call to SelectBy*()
  // - cqrsNoPriorCommand for a I*Command.Commit with no prior Add/Update/Delete
  // - cqrsUnspecifiedError will be used for any other kind of error
  TCQRSResult =
    (cqrsSuccess, cqrsUnspecifiedError, cqrsBadRequest,
     cqrsNotFound, cqrsNoMoreData, cqrsDataLayerError,
     cqrsInvalidContent, cqrsAlreadyExists,
     cqrsNoPriorQuery, cqrsNoPriorCommand);

  /// generic interface, to be used for CQRS I*Query types definition
  // - TCQRSQueryObject class will allow to easily implement LastError* members 
  ICQRSQuery = interface(IInvokable)
    ['{923614C8-A639-45AD-A3A3-4548337923C9}']
    /// should return the last error as an enumerate
    function GetLastError: TCQRSResult;
    /// should return addition information for the last error
    // - may be a plain string, or a JSON document stored as TDocVariant
    function GetLastErrorInfo: variant;
  end;

  /// which kind of process is about to take place after an ORMBegin()
  TCQRSQueryAction = (
    qaNone, qaSelect, qaGet, qaCommandDirect, qaCommandOnSelect, qaCommit);

  /// define one or several process to take place after an ORMBegin()
  TCQRSQueryActions = set of TCQRSQueryAction;

  /// the current step of a TCQRSQuery state machine
  // - basic state diagram is defined by the methods execution:
  // - qsNone refers to the default state, with no currently selected values,
  // nor any pending write request
  // - qsQuery corresponds to a successful I*Query.Select*(), expecting
  // either a I*Query.Get*(), or a I*Command.Add/Update/Delete
  // - qsCommand corresponds to a successful I*Command.Add/Update/Delete,
  // expected a I*Command.Commit
  TCQRSQueryState = (qsNone, qsQuery, qsCommand);

  /// to be inherited to implement CQRS I*Query services extended error process
  // - you should never assign directly a cqrs* value to a method result, but
  // rather use the ORMBegin/ORMResult/ORMResultMsg methods provided by this class:
  // ! function TMyService.MyMethod: TCQRSResult;
  // ! begin
  // !   ORMBegin(qsNone,result); // reset the error information to cqrsUnspecifiedError
  // !   ... // do some work
  // !   if error then
  // !     ORMResultMsg(cqrsUnspecifiedError,'Oups! For "%"',[name]) else
  // !     ORMResult(cqrsSuccess); // instead of result := cqrsSuccess
  // !   end;
  // - the methods are implemented as a simple state machine
  TCQRSQueryObject = class(TInjectableObject, ICQRSQuery)
  protected
    fLastErrorAddress: ^TCQRSResult;
    fLastError: TCQRSResult;
    fLastErrorContext: variant;
    fAction: TCQRSQueryAction;
    fState: TCQRSQueryState;
    // method to be called at first for LastError process
    function ORMBegin(aAction: TCQRSQueryAction; var aResult: TCQRSResult;
      aError: TCQRSResult=cqrsUnspecifiedError): boolean; virtual;
    function ORMError(aError: TCQRSResult): TCQRSResult; virtual;
    // methods to be used to set the process end status
    procedure ORMResult(Error: TCQRSResult); virtual;
    procedure ORMResultMsg(Error: TCQRSResult; const ErrorMessage: RawUTF8); overload;
    procedure ORMResultMsg(Error: TCQRSResult;
      ErrorMsgFmt: PUTF8Char; const ErrorMsgArgs: array of const); overload;
    procedure ORMResultDoc(Error: TCQRSResult; const ErrorInfo: variant);
    procedure ORMResultJSON(Error: TCQRSResult;
      JSONFmt: PUTF8Char; const Args,Params: array of const);
    function GetLastError: TCQRSResult; 
    function GetLastErrorInfo: variant; virtual;
  published
    /// the last error, as an enumerate
    property LastError: TCQRSResult read GetLastError;
    /// the action currently processing
    property Action: TCQRSQueryAction read fAction;
    /// current step of the TCQRSQueryObject state machine 
    property State: TCQRSQueryState read fState;
  end;


{ *********** Other Cross-Cutting Interfaces }

type
  /// the data type which will be returned during a password challenge
  // - in practice, will be e.g. Base-64 encoded SHA-256 binary hash
  TAuthQueryNonce = RawUTF8;

  TAuthInfoName = RawUTF8;

  /// DDD object used to store authentication information
  TAuthInfo = class(TPersistent)
  protected
    fLogonName: TAuthInfoName;
  published
    /// the textual identifier by which the user would recognize himself
    property LogonName: TAuthInfoName read fLogonName write fLogonName;
  end;

  /// service to authenticate credentials via a dual pass challenge
  IAuthQuery = interface(ICQRSQuery)
    ['{5FB1E4A6-B432-413F-8958-1FA1857D1195}']
    /// initiate the first phase of a dual pass challenge authentication
    function ChallengeSelectFirst(const aLogonName: RawUTF8): TAuthQueryNonce;
    /// validate the first phase of a dual pass challenge authentication
    function ChallengeSelectFinal(const aChallengedPassword: TAuthQueryNonce): TCQRSResult;
    /// set the credential for Get() or further IAuthCommand.Update/Delete
    // - this method execution will be disabled for most clients
    function SelectByName(const aLogonName: RawUTF8): TCQRSResult;
    /// retrieve some information about the current selected credential
    function Get(out aAggregate: TAuthInfo): TCQRSResult;
  end;

  /// service to update or register new authentication credentials
  IAuthCommand = interface(IAuthQuery)
    ['{8252727B-336B-4105-80FD-C8DFDBD4801E}']
    /// register a new credential, from its LogonName/HashedPassword values
    // - aHashedPassword should match the algorithm expected by the actual
    // implementation class
    // - on success, the newly created credential will be the currently selected
    function Add(const aLogonName: RawUTF8; aHashedPassword: TAuthQueryNonce): TCQRSResult;
    /// update the current selected credential password
    // - aHashedPassword should match the algorithm expected by the actual
    // implementation class
    // - will be allowed only for the current challenged user
    function UpdatePassword(const aHashedPassword: TAuthQueryNonce): TCQRSResult;
    /// delete the current selected credential
    // - this method execution will be disabled for most clients
    function Delete: TCQRSResult;
    /// write all pending changes prepared by Add/UpdatePassword/Delete methods
    function Commit: TCQRSResult;
  end;


{ *********** Application Layer Implementation }

type
  /// abstract class for implementing an Application Layer service
  TDDDApplication = class(TInjectableObject)
  protected
  end;


{ *********** Cross-Cutting Layer Implementation}

{ ----- Persistence / Repository Implementation using mORMot's ORM }

type
  TDDDRepositoryRestObjectMapping = class;
  TDDDRepositoryRestQuery = class;
  TDDDRepositoryRestClass = class of TDDDRepositoryRestQuery;

  /// abstract ancestor for all persistence/repository related Exceptions
  EDDDRepository = class(ESynException)
  public
    /// constructor like FormatUTF8() which will also serialize the caller info 
    constructor CreateUTF8(Caller: TDDDRepositoryRestObjectMapping;
      Format: PUTF8Char; const Args: array of const);
  end;

  /// store reference of several mapping definitions
  TDDDRepositoryRestObjectMappingObjArray = array of TDDDRepositoryRestObjectMapping;

  /// home repository of several TPersistent classes via one or several REST
  // - this shared class will be used to manage a service-wide Repository,
  // i.e. will handle all actual I*Query/I*Command implementation classes
  // - would e.g. handle BATCH proccess, or transactional process
  TDDDRepositoryRestMapper = class
  protected
    fMap: TDDDRepositoryRestObjectMappingObjArray;
  public
    /// finalize all mapping information
    destructor Destroy; override;
    /// register DDD's TPersistent repository over an ORM's TSQLRecord
    // - will raise an exception if the aggregate has already been defined
    function AddMap(
      const aInterface: TGUID; aImplementation: TDDDRepositoryRestClass;
      aAggregate: TPersistentClass; aRest: TSQLRest; aTable: TSQLRecordClass;
      const TableAggregatePairs: array of RawUTF8): TDDDRepositoryRestObjectMapping;
    /// retrieve the registered mapping definition of a given TPersistent in Map[]
    // - returns -1 if the TPersistence class is unknown
    function GetMapIndex(const aInterface: TGUID): integer;
    /// retrieve the registered mapping definition of a given TPersistent
    // - raise an EDDDRepository exception if the TPersistence class is unknown
    function GetMap(const aInterface: TGUID): TDDDRepositoryRestObjectMapping;
    /// read-only access to all mapping definitions
    property Map: TDDDRepositoryRestObjectMappingObjArray read fMap;
  end;

  /// handle field mapping between a DDD's TPersistent and an ORM's TSQLRecord
  // - it will centralize some helper classes, used by actual TDDDRepositoryRest
  TDDDRepositoryRestObjectMapping = class(TInterfaceResolver)
  protected
    fMapper: TDDDRepositoryRestMapper;
    fInterface: TInterfaceFactory;
    fImplementation: TDDDRepositoryRestClass;
    fRest: TSQLRest;
    fAggregate: TPersistentClass;
    fAggregateHasCustomCreate: boolean;
    fTable: TSQLRecordClass;
    fPropsMapping: TSQLRecordPropertiesMapping;
    fPropsMappingVersion: cardinal;
    fProps: TSQLPropInfoList;
    fORMProps: TSQLPropInfoList;
    // TSQLPropInfoList correspondance, as filled by ComputeMapping:
    fAggregateToTable: TSQLPropInfoDynArray;
    fTableToAggregate: TSQLPropInfoDynArray;
    procedure ComputeMapping;
    function TryResolve(aInterface: PTypeInfo; out Obj): boolean; override;
  public
    /// initialize the mapping between a DDD Aggregate and a mORMot ORM class
    // - by default, field names should match on both sides - but you can
    // specify a custom field mapping as TSQLRecord,Aggregate pairs
    // - any missing or unexpected field on any side will just be ignored
    constructor Create(aMapper: TDDDRepositoryRestMapper;
      const aInterface: TGUID; aImplementation: TDDDRepositoryRestClass;
      aAggregate: TPersistentClass; aRest: TSQLRest; aTable: TSQLRecordClass;
      const TableAggregatePairs: array of RawUTF8); reintroduce; overload;
    /// finalize all mapping information 
    destructor Destroy; override;
    /// clear all properties of a given DDD Aggregate
    procedure AggregateClear(aAggregate: TPersistent);
    /// create a new DDD Aggregate instance
    function AggregateCreate: TPersistent;
    /// serialize a DDD Aggregate as JSON
    // - you can optionaly force the generated JSON to match the mapped
    // TSQLRecord fields, so that it would be compatible with ORM's JSON
    procedure AggregateToJSON(aAggregate: TPersistent; W: TJSONSerializer;
      ORMMappedFields: boolean; aID: TID); overload;
    /// serialize a DDD Aggregate as JSON RawUTF8
    function AggregateToJSON(aAggregate: TPersistent; ORMMappedFields: boolean;
      aID: TID): RawUTF8; overload;
    /// convert a DDD Aggregate into an ORM TSQLRecord instance
    procedure AggregateToTable(aAggregate: TPersistent; aID: TID; aDest: TSQLRecord);
    /// convert a ORM TSQLRecord instance into a DDD Aggregate
    procedure AggregateFromTable(aSource: TSQLRecord; aAggregate: TPersistent);
    /// the home repository owning this mapping definition
    property Mapper: TDDDRepositoryRestMapper read fMapper;
    /// the mapped DDD's TPersistent published properties RTTI 
    property Props: TSQLPropInfoList read fProps;
    /// access to the Aggregate / ORM field mapping
    property FieldMapping: TSQLRecordPropertiesMapping read fPropsMapping;
  published
    /// the associated I*Query / I*Command repository interface
    property Repository: TInterfaceFactory read fInterface;
    /// the associated TSQLRest instance
    property Rest: TSQLRest read fRest;
    /// the mapped DDD's TPersistent
    property Aggregate: TPersistentClass read fAggregate;
    /// the ORM's TSQLRecord used for actual storage
    property Table: TSQLRecordClass read fTable;
  end;

  /// abstract class to implement I*Query interface using ORM's TSQLRecord
  TDDDRepositoryRestQuery = class(TCQRSQueryObject)
  protected
    fMapping: TDDDRepositoryRestObjectMapping;
    fORM: TSQLRecord;
    function ORMBegin(aAction: TCQRSQueryAction; var aResult: TCQRSResult;
      aError: TCQRSResult=cqrsUnspecifiedError): boolean; override;
    // one-by-one retrieval in local ORM: TSQLRecord
    function ORMSelectOne(ORMWhereClauseFmt: PUTF8Char;
      const Bounds: array of const; ForcedBadRequest: boolean=false): TCQRSResult;
    function ORMGetAggregate(aAggregate: TPersistent): TCQRSResult;
    // list retrieval - using cursor-like access via ORM.FillOne
    function ORMSelectAll(ORMWhereClauseFmt: PUTF8Char;
      const Bounds: array of const; ForcedBadRequest: boolean=false): TCQRSResult;
    function ORMGetNextAggregate(aAggregate: TPersistent; aRewind: boolean=false): TCQRSResult;
    function ORMGetAllAggregates(var aAggregateObjArray): TCQRSResult;
  public
    /// you should not have to use this constructor, since the instances would
    // be injected by TDDDRepositoryRestObjectMapping.InternalResolve
    constructor Create(aMapping: TDDDRepositoryRestObjectMapping); reintroduce; virtual;
    /// finalize the used memory
    destructor Destroy; override;
  published
    /// access to the associated mapping
    property Mapping: TDDDRepositoryRestObjectMapping read fMapping;
    /// access to the current state of the underlying mapped TSQLRecord
    // - is nil if no query was run yet
    // - contains the queried object after a successful Select*() method
    property ORM: TSQLRecord read fORM;
  end;

  /// abstract class to implement I*Command interface using ORM's TSQLRecord
  TDDDRepositoryRestCommand = class(TDDDRepositoryRestQuery)
  protected
    fCommand: TSQLOccasion;
    procedure ORMResult(Error: TCQRSResult); override;
    // - this default implementation will check the status vs command, and
    // call fORM.FilterAndValidate 
    // - you should override it, if you need a specific behavior
    procedure ORMPrepareForCommit(aCommand: TSQLOccasion); virtual;
    /// this default implementation will perform Add/Update/Delete on fORM
    // - you should override it, if you need a specific behavior
    procedure InternalCommit; virtual;
    /// do-nothing abstract rollback method
    procedure InternalRollback; virtual;
  public
    /// finalize the Unit Of Work context
    // - any uncommited change will be lost
    destructor Destroy; override;
    /// perform a deletion on the currently selected aggregate
    // - this is a generic operation which would work for any class
    // - if you do not need this method, just do not declare it in I*Command
    function Delete: TCQRSResult; virtual;
    /// write all pending changes prepared by Add/UpdatePassword/Delete methods
    // - will process the current fORM using the fCommand
    function Commit: TCQRSResult; virtual;
  protected
    /// access to the current process state
    property Command: TSQLOccasion read fCommand;
  end;


{ ----- Authentication Implementation using SHA-256 dual step challenge }

type
  /// ORM object to persist authentication information
  TSQLRecordAuthInfo = class(TSQLRecord)
  private
    fLogonName: RawUTF8;
    fHashedPassword: RawUTF8;
  published
    property LogonName: RawUTF8 read fLogonName write fLogonName;
    property HashedPassword: RawUTF8 read fHashedPassword write fHashedPassword;
  end;

  /// generic class for implementing authentication
  // - do not instantiate this abstract class, but e.g. TDDDAuthenticationSHA256
  // or TDDDAuthenticationMD5
  TDDDAuthenticationAbstract = class(TDDDRepositoryRestCommand,IAuthCommand)
  protected
    fChallengeLogonName: RawUTF8;
    fChallengeNonce: TAuthQueryNonce;
    class function ComputeHashPassword(const aLogonName,aPassword: RawUTF8): TAuthQueryNonce;
    /// overriden classes should override this method with the proper algorithm
    class function DoHash(const aValue: TAuthQueryNonce): TAuthQueryNonce; virtual; abstract;
  public
    /// initiate the first phase of a dual pass challenge authentication
    function ChallengeSelectFirst(const aLogonName: RawUTF8): TAuthQueryNonce;
    /// validate the first phase of a dual pass challenge authentication
    function ChallengeSelectFinal(const aChallengedPassword: TAuthQueryNonce): TCQRSResult;
    /// set the credential for Get() or further IAuthCommand.Update/Delete
    // - this method execution will be disabled for most clients
    function SelectByName(const aLogonName: RawUTF8): TCQRSResult;
    /// retrieve some information about the current selected credential
    function Get(out aAggregate: TAuthInfo): TCQRSResult;
    /// register a new credential, from its LogonName/HashedPassword values
    // - on success, the newly created credential will be the currently selected
    function Add(const aLogonName: RawUTF8; aHashedPassword: TAuthQueryNonce): TCQRSResult;
    /// update the current selected credential password
    function UpdatePassword(const aHashedPassword: TAuthQueryNonce): TCQRSResult;
    /// class method to be used on the client side to compute the password
    // - is basically
    // !   result := DoHash(aLogonName+':'+aChallengeFromServer+':'+
    // !   ComputeHashPassword(aLogonName,aPlainPassword));
    class function ClientComputeChallengedPassword(
      const aLogonName,aPlainPassword: RawUTF8;
      const aChallengeFromServer: TAuthQueryNonce): TAuthQueryNonce; virtual;
  end;

  /// implements authentication using SHA-256 hashing
  // - more secure than TDDDAuthenticationMD5
  TDDDAuthenticationSHA256 = class(TDDDAuthenticationAbstract)
  protected
    /// will use SHA-256 algorithm for hashing, and the class name as salt
    class function DoHash(const aValue: TAuthQueryNonce): TAuthQueryNonce; override;
  end;

  /// implements authentication using MD5 hashing
  // - less secure than TDDDAuthenticationSHA256
  TDDDAuthenticationMD5 = class(TDDDAuthenticationAbstract)
  protected
    /// will use MD5 algorithm for hashing, and the class name as salt
    class function DoHash(const aValue: TAuthQueryNonce): TAuthQueryNonce; override;
  end;



implementation


{ *********** Persistence / Repository Interfaces }

{ TCQRSQueryObject }

function TCQRSQueryObject.GetLastError: TCQRSResult;
begin
  result := fLastError;
end;

function TCQRSQueryObject.GetLastErrorInfo: Variant;
begin
  result := fLastErrorContext;
end;

const
  NEEDS_QUERY   = [qaGet, qaCommandOnSelect];
  NEEDS_COMMAND = [qaCommit];
  ACTION_TO_STATE: array[TCQRSQueryAction] of TCQRSQueryState = (
    // qsNone = no state change after this action
    qsNone, qsQuery, qsNone, qsCommand, qsCommand, qsNone);

function TCQRSQueryObject.ORMBegin(aAction: TCQRSQueryAction;
  var aResult: TCQRSResult; aError: TCQRSResult): boolean;
begin
  fLastErrorAddress := @aResult;
  fLastErrorAddress^ := aError;
  VarClear(fLastErrorContext);
  if (aAction in NEEDS_QUERY) and (fState<qsQuery) then begin
    ORMResult(cqrsNoPriorQuery);
    result := false;
    exit;
  end;
  if (aAction in NEEDS_COMMAND) and (fState<qsCommand) then begin
    ORMResult(cqrsNoPriorCommand);
    result := false;
    exit;
  end;
  fAction := aAction;
  result := true;
end;

function TCQRSQueryObject.ORMError(aError: TCQRSResult): TCQRSResult;
begin
  ORMBegin(qaNone,result);
  ORMResult(aError);
end;

procedure TCQRSQueryObject.ORMResult(Error: TCQRSResult);
begin
  if fLastErrorAddress=nil then
    raise ECQRSException.CreateUTF8('%.ORMResult(%) with no prior ORMBegin',
      [self,GetEnumName(TypeInfo(TCQRSResult),ord(Error))^]);
  fLastErrorAddress^ := Error;
  fLastError := Error;
  fLastErrorContext := _JsonFast(ObjectToJSONDebug(self));
  TDocVariantData(fLastErrorContext).AddValue('LocalTime',NowToString);
  if Error=cqrsSuccess then
    if ACTION_TO_STATE[fAction]<>qsNone then
      fState := ACTION_TO_STATE[fAction];
  fAction := qaNone;
end;

procedure TCQRSQueryObject.ORMResultDoc(Error: TCQRSResult;
  const ErrorInfo: variant);
begin
  ORMResult(Error);
  TDocVariantData(fLastErrorContext).AddValue('Info',ErrorInfo);
end;

procedure TCQRSQueryObject.ORMResultJSON(Error: TCQRSResult;
  JSONFmt: PUTF8Char; const Args,Params: array of const);
begin
  ORMResultDoc(Error,_JsonFastFmt(JSONFmt,Args,Params));
end;

procedure TCQRSQueryObject.ORMResultMsg(Error: TCQRSResult;
  const ErrorMessage: RawUTF8);
begin
  ORMResult(Error);
  TDocVariantData(fLastErrorContext).AddValue('Msg',ErrorMessage);
end;

procedure TCQRSQueryObject.ORMResultMsg(Error: TCQRSResult;
  ErrorMsgFmt: PUTF8Char; const ErrorMsgArgs: array of const);
begin
  ORMResultMsg(Error,FormatUTF8(ErrorMsgFmt,ErrorMsgArgs));
end;


{ ----- Persistence / Repository Implementation using mORMot's ORM }

{ EDDDRepository }

constructor EDDDRepository.CreateUTF8(
  Caller: TDDDRepositoryRestObjectMapping; Format: PUTF8Char;
  const Args: array of const);
begin
  if Caller=nil then
    inherited CreateUTF8(Format,Args) else
    inherited CreateUTF8('% - %',[FormatUTF8(Format,Args),ObjectToJSONDebug(Caller)]);
end;


{ TDDDRepositoryRestObjectMapping }

constructor TDDDRepositoryRestObjectMapping.Create(
  aMapper: TDDDRepositoryRestMapper;
  const aInterface: TGUID; aImplementation: TDDDRepositoryRestClass;
  aAggregate: TPersistentClass; aRest: TSQLRest; aTable: TSQLRecordClass;
  const TableAggregatePairs: array of RawUTF8);
begin
  inherited Create;
  fMapper := aMapper;
  fImplementation := aImplementation;
  fRest := aRest;
  fAggregate := aAggregate;
  fAggregateHasCustomCreate := fAggregate.InheritsFrom(TPersistentWithCustomCreate);
  fTable := aTable;
  fInterface := TInterfaceFactory.Get(aInterface);
  if fInterface=nil then
    raise EDDDRepository.CreateUTF8(self,
     '%.Create(%): Interface not registered - you could use TInterfaceFactory.'+
     'RegisterInterfaces()',[self,GUIDToShort(aInterface)]);
  if (fAggregate=nil) or (fRest=nil) or (fTable=nil) or (fImplementation=nil) then
    raise EDDDRepository.CreateUTF8(self,'Invalid %.Create(nil)',[self]);
  fORMProps := fTable.RecordProps.Fields;
  fPropsMapping.Init(aTable,RawUTF8(fAggregate.ClassName),aRest,false);
  fPropsMapping.MapFields(['ID','####']); // no ID/RowID for our aggregates
  fPropsMapping.MapFields(TableAggregatePairs);
  fProps := TSQLPropInfoList.Create(fAggregate,[pilAllowIDFields]);
  SetLength(fAggregateToTable,fProps.Count);
  SetLength(fTableToAggregate,fORMProps.Count);
  ComputeMapping;
end;

destructor TDDDRepositoryRestObjectMapping.Destroy;
begin
  fProps.Free;
  inherited;
end;

procedure TDDDRepositoryRestObjectMapping.ComputeMapping;
procedure EnsureCompatible(agg,rec: TSQLPropInfo);
begin
  if agg.SQLDBFieldType<>rec.SQLDBFieldType then
    raise EDDDRepository.CreateUTF8(self,
      '% types do not match at DB level: %.%:%=% and %.%:%=%',[self,
      fAggregate,agg.Name,agg.SQLFieldRTTITypeName,agg.SQLDBFieldTypeName,
      fTable,rec.Name,rec.SQLFieldRTTITypeName,rec.SQLDBFieldTypeName]);
end;
var i,ndx: integer;
begin
  if fPropsMapping.MappingVersion=fPropsMappingVersion then
    exit;
  for i := 0 to fProps.Count-1 do begin
    ndx := fPropsMapping.ExternalToInternalIndex(fProps.List[i].Name);
    if ndx<0 then // ID/RowID or TPersistent property not defined in TSQLRecord
      fAggregateToTable[i] := nil else begin
      fAggregateToTable[i] := fORMProps.List[ndx];
      EnsureCompatible(fProps.List[i],fAggregateToTable[i]);
    end;
  end;
  for i := 0 to fORMProps.Count-1 do begin
    ndx := fProps.IndexByName(fORMProps.List[i].Name);
    if ndx<0 then // TSQLRecord property not defined in the TPersistent
      fTableToAggregate[i] := nil else begin
      fTableToAggregate[i] := fProps.List[ndx];
      EnsureCompatible(fTableToAggregate[i],fORMProps.List[i]);
    end;
  end;
  fPropsMappingVersion := fPropsMapping.MappingVersion;
end;

function TDDDRepositoryRestObjectMapping.TryResolve(
  aInterface: PTypeInfo; out Obj): boolean;
begin
  if fInterface.InterfaceTypeInfo<>aInterface then
    result := false else begin
    IInterface(Obj) := fImplementation.Create(self);
    result := true;
  end;
end;

procedure TDDDRepositoryRestObjectMapping.AggregateClear(
  aAggregate: TPersistent);
var i: integer;
begin
  if aAggregate<>nil then
    for i := 0 to fProps.Count-1 do
      fProps.List[i].SetValue(aAggregate,nil,false);
end;

function TDDDRepositoryRestObjectMapping.AggregateCreate: TPersistent;
begin
  if fAggregateHasCustomCreate then
    result := TPersistentWithCustomCreateClass(fAggregate).Create else
    result := fAggregate.Create;
end;

procedure TDDDRepositoryRestObjectMapping.AggregateToJSON(
  aAggregate: TPersistent; W: TJSONSerializer; ORMMappedFields: boolean;
  aID: TID);
var i: integer;
begin
  ComputeMapping;
  if aAggregate=nil then begin
    W.AddShort('null');
    exit;
  end;
  W.Add('{');
  if aID<>0 then begin
    W.AddShort('"RowID":');
    W.Add(aID);
    W.Add(',');
  end;
  for i := 0 to fProps.Count-1 do begin
    if ORMMappedFields then
      if fAggregateToTable[i]=nil then
        continue else
        W.AddFieldName(fAggregateToTable[i].Name) else
      W.AddFieldName(fProps.List[i].Name);
    fProps.List[i].GetJSONValues(aAggregate,W);
    W.Add(',');
  end;
  W.CancelLastComma;
  W.Add('}');
end;

function TDDDRepositoryRestObjectMapping.AggregateToJSON(
  aAggregate: TPersistent; ORMMappedFields: boolean; aID: TID): RawUTF8;
var W: TJSONSerializer;
begin
  if aAggregate=nil then begin
    result := 'null';
    exit;
  end;
  W := TJSONSerializer.CreateOwnedStream;
  try
    AggregateToJSON(aAggregate,W,ORMMappedFields,aID);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

procedure TDDDRepositoryRestObjectMapping.AggregateToTable(
  aAggregate: TPersistent; aID: TID; aDest: TSQLRecord);
var i: integer;
    Value: RawUTF8;
    wasString: boolean;
begin
  ComputeMapping;
  if aDest=nil then
    raise EDDDRepository.CreateUTF8(self,'%.AggregateToTable(%,%,%=nil)',
      [self,aAggregate,aID,fTable]);
  aDest.ClearProperties;
  aDest.ID := aID;
  if aAggregate<>nil then
    for i := 0 to fProps.Count-1 do
    if fAggregateToTable[i]<>nil then begin
      fProps.List[i].GetValueVar(aAggregate,false,Value,@wasString);
      fAggregateToTable[i].SetValueVar(aDest,Value,wasString);
    end;
end;

procedure TDDDRepositoryRestObjectMapping.AggregateFromTable(
  aSource: TSQLRecord; aAggregate: TPersistent);
var i: integer;
    Value: RawUTF8;
    wasString: boolean;
begin
  ComputeMapping;
  if aAggregate=nil then
    raise EDDDRepository.CreateUTF8(self,'%.AggregateFromTable(%=nil)',[self,fAggregate]);
  if aSource=nil then begin
    AggregateClear(aAggregate);
    exit;
  end;
  for i := 0 to fProps.Count-1 do begin
    if fAggregateToTable[i]<>nil then
      fAggregateToTable[i].GetValueVar(aSource,false,Value,@wasString) else
      Value := '';
    fProps.List[i].SetValueVar(aAggregate,Value,wasString);
  end;
end;


{ TDDDRepositoryRestMapper }

function TDDDRepositoryRestMapper.AddMap(
  const aInterface: TGUID; aImplementation: TDDDRepositoryRestClass;
  aAggregate: TPersistentClass; aRest: TSQLRest; aTable: TSQLRecordClass;
  const TableAggregatePairs: array of RawUTF8): TDDDRepositoryRestObjectMapping;
begin
  if GetMapIndex(aInterface)>=0 then
    raise EDDDRepository.CreateUTF8(nil,'Duplicated GUID for %.AddMap(%,%,%)',
      [self,GUIDToShort(aInterface),aImplementation,aAggregate]);
  result := TDDDRepositoryRestObjectMapping.Create(self,
    aInterface,aImplementation,aAggregate,aRest,aTable,TableAggregatePairs);
  ObjArrayAdd(fMap,result);
end;

destructor TDDDRepositoryRestMapper.Destroy;
begin
  ObjArrayClear(fMap);
  inherited;
end;

function TDDDRepositoryRestMapper.GetMap(
  const aInterface: TGUID): TDDDRepositoryRestObjectMapping;
var i: integer;
begin
  i := GetMapIndex(aInterface);
  if i<0 then
    raise EDDDRepository.CreateUTF8(nil,'%.GetMap(%)=nil',
      [self,GUIDToShort(aInterface)]);
  result := fMap[i];
end;

function TDDDRepositoryRestMapper.GetMapIndex(
  const aInterface: TGUID): integer;
begin
  for result := 0 to length(fMap)-1 do
    if IsEqualGUID(fMap[result].fInterface.InterfaceIID,aInterface) then
      exit;
  result := -1;
end;


{ TDDDRepositoryRestQuery }

constructor TDDDRepositoryRestQuery.Create(
  aMapping: TDDDRepositoryRestObjectMapping);
begin
  fMapping := aMapping;
  fORM := fMapping.fTable.Create;
end;

destructor TDDDRepositoryRestQuery.Destroy;
begin
  fORM.Free;
  inherited;
end;

function TDDDRepositoryRestQuery.ORMBegin(aAction: TCQRSQueryAction;
  var aResult: TCQRSResult; aError: TCQRSResult): boolean;
begin
  result := inherited ORMBegin(aAction,aResult,aError);
  if aAction=qaSelect then
    ORM.ClearProperties;
end;

function TDDDRepositoryRestQuery.ORMSelectOne(ORMWhereClauseFmt: PUTF8Char;
   const Bounds: array of const; ForcedBadRequest: boolean): TCQRSResult;
begin
  ORMBegin(qaSelect,result);
  if ForcedBadRequest then
    ORMResult(cqrsBadRequest) else
    if Mapping.Rest.Retrieve(ORMWhereClauseFmt,[],Bounds,ORM) then
      ORMResult(cqrsSuccess) else
      ORMResult(cqrsNotFound);
end;

function TDDDRepositoryRestQuery.ORMSelectAll(
  ORMWhereClauseFmt: PUTF8Char; const Bounds: array of const;
  ForcedBadRequest: boolean): TCQRSResult;
begin
  ORMBegin(qaSelect,result);
  if ForcedBadRequest then
    ORMResult(cqrsBadRequest) else
    if ORM.FillPrepare(Mapping.Rest,ORMWhereClauseFmt,[],Bounds) then
      ORMResult(cqrsSuccess) else
      ORMResult(cqrsNotFound);
end;

function TDDDRepositoryRestQuery.ORMGetAggregate(
  aAggregate: TPersistent): TCQRSResult;
begin
  if ORMBegin(qaGet,result) then begin
    Mapping.AggregateFromTable(ORM,aAggregate);
    ORMResult(cqrsSuccess);
  end;
end;

function TDDDRepositoryRestQuery.ORMGetNextAggregate(
  aAggregate: TPersistent; aRewind: boolean): TCQRSResult;
begin
  if ORMBegin(qaGet,result) then
    if (aRewind and ORM.FillRewind) or
       ((not aRewind) and ORM.FillOne) then begin
      Mapping.AggregateFromTable(ORM,aAggregate);
      ORMResult(cqrsSuccess);
    end else
      ORMResult(cqrsNoMoreData);
end;

function TDDDRepositoryRestQuery.ORMGetAllAggregates(
  var aAggregateObjArray): TCQRSResult;
var res: TPersistentDynArray absolute aAggregateObjArray;
    i: integer;
begin
  if ORMBegin(qaGet,result) then begin
    SetLength(res,ORM.FillTable.RowCount);
    i := 0;
    if ORM.FillRewind then
    repeat
      res[i] := Mapping.AggregateCreate;
      Mapping.AggregateFromTable(ORM,res[i]);
      inc(i);
    until not ORM.FillOne;
    if i=length(res) then
      ORMResult(cqrsSuccess) else begin
      ObjArrayClear(res);
      ORMResult(cqrsNoMoreData);
    end;
  end;
end;


{ TDDDRepositoryRestCommand }

destructor TDDDRepositoryRestCommand.Destroy;
begin
  InternalRollback;
  inherited Destroy;
end;

procedure TDDDRepositoryRestCommand.ORMResult(Error: TCQRSResult);
begin
  inherited ORMResult(Error);
  if Error<>cqrsSuccess then
    fCommand := soSelect;
end;

function TDDDRepositoryRestCommand.Delete: TCQRSResult;
begin
  if ORMBegin(qaCommandOnSelect,result) then
    ORMPrepareForCommit(soDelete);
end;

procedure TDDDRepositoryRestCommand.ORMPrepareForCommit(
  aCommand: TSQLOccasion);
var msg: RawUTF8;
begin
  fCommand := aCommand; // overriden ORMResult() will reset to soSelect on error
  case aCommand of
  soSelect: begin
    ORMResult(cqrsBadRequest);
    exit;
  end;
  soUpdate,soDelete:
    if (fState<qsQuery) or (ORM.ID=0) then begin
      ORMResult(cqrsNoPriorQuery);
      exit;
    end;
  end;
  msg := ORM.FilterAndValidate(Mapping.Rest);
  if msg<>'' then
    ORMResultMsg(cqrsDataLayerError,msg) else
    ORMResult(cqrsSuccess);
end;

procedure TDDDRepositoryRestCommand.InternalCommit;
begin
  case fCommand of
  soSelect:
    ORMResult(cqrsBadRequest);
  soInsert:
    if Mapping.Rest.Add(ORM,true)<>0 then
      ORMResult(cqrsSuccess) else
      ORMResult(cqrsDataLayerError);
  soUpdate:
    if Mapping.Rest.Update(ORM) then
      ORMResult(cqrsSuccess) else
      ORMResult(cqrsDataLayerError);
  soDelete:
    if Mapping.Rest.Delete(ORM.RecordClass,ORM.ID) then
      ORMResult(cqrsSuccess) else
      ORMResult(cqrsDataLayerError);
  end;
end;

procedure TDDDRepositoryRestCommand.InternalRollback;
begin // overriden methods may do something 
end;

function TDDDRepositoryRestCommand.Commit: TCQRSResult;
begin
  if ORMBegin(qaCommit,result) then
    InternalCommit;
end;



{ ----- Authentication Implementation using SHA-256 dual step challenge }

{ TDDDAuthenticationAbstract }

function TDDDAuthenticationAbstract.ChallengeSelectFirst(
  const aLogonName: RawUTF8): TAuthQueryNonce;
begin
  fChallengeLogonName := Trim(aLogonName);
  fChallengeNonce := DoHash(aLogonName+NowToString);
end;

function TDDDAuthenticationAbstract.ChallengeSelectFinal(
  const aChallengedPassword: TAuthQueryNonce): TCQRSResult;
begin
  if (fChallengeLogonName='') or (fChallengeNonce='') then
    result := ORMError(cqrsBadRequest) else
    result := SelectByName(fChallengeLogonName);
  if result<>cqrsSuccess then
    exit;
  ORMBegin(qaSelect,result);
  if DoHash(fChallengeLogonName+':'+fChallengeNonce+':'+
     (ORM as TSQLRecordAuthInfo).HashedPassword)=aChallengedPassword then
    ORMResult(cqrsSuccess) else
    ORMResultMsg(cqrsBadRequest,'Wrong Password for "%"',[fChallengeLogonName]);
  fChallengeNonce := '';
  fChallengeLogonName := '';
end;

class function TDDDAuthenticationAbstract.ComputeHashPassword(
  const aLogonName, aPassword: RawUTF8): TAuthQueryNonce;
begin
  result := DoHash(aLogonName+':'+aPassword);
end;

class function TDDDAuthenticationAbstract.ClientComputeChallengedPassword(
  const aLogonName,aPlainPassword: RawUTF8; const aChallengeFromServer: TAuthQueryNonce): TAuthQueryNonce;
begin // see TDDDAuthenticationAbstract.ChallengeSelectFinal
  result := DoHash(aLogonName+':'+aChallengeFromServer+':'+
    ComputeHashPassword(aLogonName,aPlainPassword));
end;

function TDDDAuthenticationAbstract.SelectByName(
  const aLogonName: RawUTF8): TCQRSResult;
begin
  result := ORMSelectOne('LogonName=?',[aLogonName],(aLogonName=''));
end;

function TDDDAuthenticationAbstract.Get(
  out aAggregate: TAuthInfo): TCQRSResult;
begin
  result := ORMGetAggregate(aAggregate);
end;

function TDDDAuthenticationAbstract.Add(const aLogonName: RawUTF8;
  aHashedPassword: TAuthQueryNonce): TCQRSResult;
begin
  if not ORMBegin(qaCommandDirect,result) then
    exit;
  with ORM as TSQLRecordAuthInfo do begin
    LogonName := aLogonName;
    HashedPassword := aHashedPassword;
  end;
  ORMPrepareForCommit(soInsert);
end;

function TDDDAuthenticationAbstract.UpdatePassword(
  const aHashedPassword: TAuthQueryNonce): TCQRSResult;
begin
  if not ORMBegin(qaCommandOnSelect,result) then
    exit;
  (ORM as TSQLRecordAuthInfo).HashedPassword := aHashedPassword;
  ORMPrepareForCommit(soUpdate);
end;


{ TDDDAuthenticationSHA256 }

class function TDDDAuthenticationSHA256.DoHash(
  const aValue: TAuthQueryNonce): TAuthQueryNonce;
begin
  result := SHA256(RawUTF8(ClassName)+aValue);
end;

{ TDDDAuthenticationMD5 }

class function TDDDAuthenticationMD5.DoHash(
  const aValue: TAuthQueryNonce): TAuthQueryNonce;
begin
  result := MD5(RawUTF8(ClassName)+aValue);
end;



initialization
  TInterfaceFactory.RegisterInterfaces(
    [TypeInfo(IAuthQuery),TypeInfo(IAuthCommand)]);
end.
