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
  // - cqrsDataLayerError indicates a low-level error at database level
  // - cqrsInvalidContent for any I*Command method with invalid aggregate input
  // value (e.g. a missing field)
  // - cqrsAlreadyExists for a I*Command.Add method with a primay key conflict
  // - cqrsNoPriorQuery for a I*Command.Update/Delete method with no prior
  // call to SelectBy*()
  // - cqrsUnspecifiedError will be used for any other kind of error
  TCQRSResult =
    (cqrsSuccess, cqrsUnspecifiedError, cqrsBadRequest,
     cqrsNotFound, cqrsDataLayerError,
     cqrsInvalidContent, cqrsAlreadyExists, cqrsNoPriorQuery);

  /// generic interface, to be used for CQRS I*Query types definition
  // - TCQRSQueryObject class will allow to easily implement LastError* members 
  ICQRSQuery = interface(IInvokable)
    ['{923614C8-A639-45AD-A3A3-4548337923C9}']
    /// should return the last error as an enumerate
    function LastError: TCQRSResult;
    /// should return addition information for the last error
    // - may be a plain string, or a JSON document stored as TDocVariant
    function LastErrorInfo: variant;
  end;
            
  /// to be inherited to implement CQRS I*Query services extended error process
  // - you should never assign directly a cqrs* value to a method result, but
  // rather use the Reset/SetResult/SetResultMsg methods provided by this class:
  // ! function TMyService.MyMethod: TCQRSResult;
  // ! begin
  // !   Reset(result); // reset the error information to cqrsUnspecifiedError
  // !   ... // do some work
  // !   if error then
  // !     SetResultMsg(cqrsUnspecifiedError,'Oups! For "%"',[name]) else
  // !     SetResult(cqrsSuccess); // instead of result := cqrsSuccess
  // !   end;
  TCQRSQueryObject = class(TInjectableObject, ICQRSQuery)
  protected
    fLastErrorAddress: ^TCQRSResult;
    fLastErrorAddressIsQuery: boolean;
    fLastError: TCQRSResult;
    fLastErrorInfo: variant;
    fLastQueryError: TCQRSResult;
    /// overloaded protected methods to be used for LastError process
    procedure Reset(var result: TCQRSResult;
      Error: TCQRSResult=cqrsUnspecifiedError); virtual;
    procedure ORMBeforeQuery(var result: TCQRSResult;
      Error: TCQRSResult=cqrsUnspecifiedError); virtual;
    function ORMAfterQuery(var aResult: TCQRSResult;
      aError: TCQRSResult=cqrsUnspecifiedError): boolean; virtual;
    procedure SetResult(Error: TCQRSResult); virtual;
    procedure SetResultMsg(Error: TCQRSResult; const ErrorMessage: RawUTF8); overload;
    procedure SetResultMsg(Error: TCQRSResult;
      ErrorMsgFmt: PUTF8Char; const ErrorMsgArgs: array of const); overload;
    procedure SetResultDoc(Error: TCQRSResult; const ErrorInfo: variant); 
    procedure SetResultJSON(Error: TCQRSResult;
      JSONFmt: PUTF8Char; const Args,Params: array of const); 
  public
    /// returns the last error as an enumerate
    function LastError: TCQRSResult; virtual;
    /// returns addition information for the last error
    // - may be some text, or a JSON document stored as TDocVariant   
    // - if no specific information has been defined, a generic English text
    // will be computed by this method
    function LastErrorInfo: variant; virtual;
  end;


{ *********** Other Cross-Cutting Interfaces }

type
  /// the data type which will be returned during a password challenge
  // - in practice, will be e.g. Base-64 encoded SHA-256 binary hash
  TAuthQueryNonce = RawUTF8;

  /// used to store authentication information
  TAuthInfo = class(TPersistent)
  protected
    fLogonName: RawUTF8;
  published
    /// the textual identifier by which the user would recognize himself
    property LogonName: RawUTF8 read fLogonName write fLogonName;
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
  /// abstract ancestor for all persistence/repository related Exceptions
  EDDDRepository = class(ESynException);

  TDDDRepositoryRestObjectMapping = class;
  TDDDRepositoryRestQuery = class;
  TDDDRepositoryRestClass = class of TDDDRepositoryRestQuery;

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
    function AddMap(aAggregate: TPersistentClass;
      const aInterface: TGUID; aImplementation: TDDDRepositoryRestClass;
      aRest: TSQLRest; aTable: TSQLRecordClass;
      const TableAggregatePairs: array of RawUTF8): TDDDRepositoryRestObjectMapping;
    /// retrieve the registered mapping definition of a given TPersistent in Map[]
    // - returns -1 if the TPersistence class is unknown
    function GetMapIndex(aAggregate: TPersistentClass): integer;
    /// retrieve the registered mapping definition of a given TPersistent
    // - raise an EDDDRepository exception if the TPersistence class is unknown
    function GetMap(aAggregate: TPersistentClass): TDDDRepositoryRestObjectMapping;
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
    fTable: TSQLRecordClass;
    fMapping: TSQLRecordPropertiesMapping;
    fMappingVersion: cardinal;
    fProps: TSQLPropInfoList;
    fORMProps: TSQLPropInfoList;
    // TSQLPropInfoList correspondance, as filled by ComputeMapping:
    fAggregateToTable: TSQLPropInfoDynArray;
    fTableToAggregate: TSQLPropInfoDynArray;
    procedure ComputeMapping;
    function InternalResolve(aInterface: PTypeInfo; out Obj): boolean; override;
  public
    /// initialize the mapping between a DDD Aggregate and a mORMot ORM class
    // - by default, field names should match on both sides - but you can
    // specify a custom field mapping as TSQLRecord,Aggregate pairs
    // - any missing or unexpected field on any side will just be ignored
    constructor Create(aMapper: TDDDRepositoryRestMapper;
      aAggregate: TPersistentClass;
      const aInterface: TGUID; aImplementation: TDDDRepositoryRestClass;
      aRest: TSQLRest; aTable: TSQLRecordClass;
      const TableAggregatePairs: array of RawUTF8); reintroduce; overload;
    /// clear all properties of a given DDD Aggregate
    procedure AggregateClear(aAggregate: TPersistent);
    /// serialize a DDD Aggregate as JSON
    // - you can optionaly force the generated JSON to match the mapped
    // TSQLRecord fields
    procedure AggregateToJSON(aAggregate: TPersistent; W: TJSONSerializer;
      ORMMappedFields: boolean; aID: TID); 
    /// convert a DDD Aggregate into an ORM TSQLRecord instance
    procedure AggregateToTable(aAggregate: TPersistent; aID: TID; aDest: TSQLRecord);
    /// convert a ORM TSQLRecord instance into a DDD Aggregate
    procedure AggregateFromTable(aSource: TSQLRecord; aAggregate: TPersistent);
    /// finalize all mapping information 
    destructor Destroy; override;
    /// the home repository owning this mapping definition
    property Mapper: TDDDRepositoryRestMapper read fMapper;
    /// the associated TSQLRest instance
    property Rest: TSQLRest read fRest;
    /// the mapped DDD's TPersistent
    property Aggregate: TPersistentClass read fAggregate;
    /// the mapped DDD's TPersistent published properties RTTI 
    property Props: TSQLPropInfoList read fProps;
    /// the ORM's TSQLRecord used for actual storage
    property Table: TSQLRecordClass read fTable;
    /// access to the Aggregate / ORM field mapping
    property Mapping: TSQLRecordPropertiesMapping read fMapping;
  end;

  /// abstract class to implement I*Query interface using ORM's TSQLRecord
  TDDDRepositoryRestQuery = class(TCQRSQueryObject)
  protected
    fMapping: TDDDRepositoryRestObjectMapping;
    fORM: TSQLRecord;
    function ORMGetOneAndSetResult(WhereClauseFmt: PUTF8Char;
      const Bounds: array of const): TCQRSResult;
    function ORMToAggregateAndSetResult(aAggregate: TPersistent): TCQRSResult;
    // SetResult(cqrsDataLayerError) will add some additional information
    procedure SetResult(Error: TCQRSResult); override;
  public
    /// you should not have to use this constructor, since the instances would
    // be injected by TDDDRepositoryRestObjectMapping.InternalResolve
    constructor Create(aMapping: TDDDRepositoryRestObjectMapping); reintroduce; virtual;
    /// finalize the used memory
    destructor Destroy; override;
    /// access to the current state of the underlying mapped TSQLRecord
    property ORM: TSQLRecord read fORM;
  end;

  /// abstract class to implement I*Command interface using ORM's TSQLRecord
  TDDDRepositoryRestCommand = class(TDDDRepositoryRestQuery)
  protected
  public
    /// finalize the Unit Of Work context
    // - any uncommited change will be lost
    destructor Destroy; override;
    /// perform a deletion on the currently selected aggregate
    // - this is a generic operation which would work for any class
    // - if you do not need this method, just do not declare it in I*Command
    function Delete: TCQRSResult; virtual;
  end;


{ ----- Authentication Implementation using SHA-256 dual step challenge }

type
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
    // temporary abstract method - TODO: use the ORM as real repository
    function GetHashedPasswordFromRepository(const aLogonName: RawUTF8): TAuthQueryNonce; virtual; abstract;
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
    /// write all pending changes prepared by Add/UpdatePassword/Delete methods
    function Commit: TCQRSResult;
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

function TCQRSQueryObject.LastError: TCQRSResult;
begin
  result := fLastError;
end;

function TCQRSQueryObject.LastErrorInfo: Variant;
begin
  if TVarData(fLastErrorInfo).VType=varEmpty then
    RawUTF8ToVariant(GetEnumNameTrimed(TypeInfo(TCQRSResult),fLastError),result) else
    result := fLastErrorInfo;
end;

procedure TCQRSQueryObject.Reset(var result: TCQRSResult; Error: TCQRSResult);
begin
  fLastErrorAddress := @result;
  fLastErrorAddressIsQuery := false;
  result := Error;
  fLastError := Error;
  VarClear(fLastErrorInfo);
end;

procedure TCQRSQueryObject.ORMBeforeQuery(var result: TCQRSResult;
  Error: TCQRSResult);
begin
  Reset(result,Error);
  fLastErrorAddressIsQuery := true;
end;

function TCQRSQueryObject.ORMAfterQuery(var aResult: TCQRSResult;
  aError: TCQRSResult): boolean;
begin
  result := fLastQueryError=cqrsSuccess;
  if not result then
    aError := cqrsNoPriorQuery;
  Reset(aResult,aError);
end;

procedure TCQRSQueryObject.SetResult(Error: TCQRSResult);
begin
  if fLastErrorAddress=nil then
    raise ECQRSException.CreateUTF8('%.SetResult(%) with no prior Start',
      [self,GetEnumName(TypeInfo(TCQRSResult),ord(Error))^]);
  fLastErrorAddress^ := Error;
  if fLastErrorAddressIsQuery then
    fLastQueryError := Error;
  fLastError := Error;
end;

procedure TCQRSQueryObject.SetResultDoc(Error: TCQRSResult;
  const ErrorInfo: variant);
begin
  SetResult(Error);
  fLastErrorInfo := ErrorInfo;
end;

procedure TCQRSQueryObject.SetResultJSON(Error: TCQRSResult;
  JSONFmt: PUTF8Char; const Args,Params: array of const);
begin
  SetResultDoc(Error,_JsonFastFmt(JSONFmt,Args,Params));
end;

procedure TCQRSQueryObject.SetResultMsg(Error: TCQRSResult;
  const ErrorMessage: RawUTF8);
begin
  SetResult(Error);
  RawUTF8ToVariant(ErrorMessage,fLastErrorInfo);
end;

procedure TCQRSQueryObject.SetResultMsg(Error: TCQRSResult;
  ErrorMsgFmt: PUTF8Char; const ErrorMsgArgs: array of const);
begin
  SetResultMsg(Error,FormatUTF8(ErrorMsgFmt,ErrorMsgArgs));
end;


{ ----- Persistence / Repository Implementation using mORMot's ORM }

{ TDDDRepositoryRestObjectMapping }

constructor TDDDRepositoryRestObjectMapping.Create(
  aMapper: TDDDRepositoryRestMapper; aAggregate: TPersistentClass;
  const aInterface: TGUID; aImplementation: TDDDRepositoryRestClass;
  aRest: TSQLRest; aTable: TSQLRecordClass; const TableAggregatePairs: array of RawUTF8);
  procedure AddParentsFirst(aClassType: TClass);
  var P: PPropInfo;
      nfo: TSQLPropInfo;
      i: Integer;
  begin
    if aClassType=nil then
      exit; // no RTTI information (e.g. reached TObject level)
    AddParentsFirst(aClassType.ClassParent);
    for i := 1 to InternalClassPropInfo(aClassType,P) do begin
      nfo := TSQLPropInfoRTTI.CreateFrom(P,fProps.Count,false);
      if nfo<>nil then
        fProps.Add(aTable,nfo);
      P := P^.Next;
    end;
  end;
begin
  inherited Create;
  fMapper := aMapper;
  fInterface := TInterfaceFactory.Get(aInterface);
  if fInterface=nil then
    raise EServiceException.CreateUTF8(
    '%.Create(%,%,%,%): Interface not registered - you could use TInterfaceFactory.'+
    'RegisterInterfaces()',[self,aAggregate,GUIDToShort(aInterface),aRest,aTable]);
  fImplementation := aImplementation;
  fRest := aRest;
  fAggregate := aAggregate;
  fTable := aTable;
  fORMProps := fTable.RecordProps.Fields;
  fMapping.Init(aTable,RawUTF8(fAggregate.ClassName),aRest,false);
  fMapping.MapFields(TableAggregatePairs);
  fProps := TSQLPropInfoList.Create;
  AddParentsFirst(fAggregate);
  SetLength(fAggregateToTable,fProps.Count);
  SetLength(fTableToAggregate,fProps.Count);
  ComputeMapping;
end;

destructor TDDDRepositoryRestObjectMapping.Destroy;
begin
  fProps.Free;
  inherited;
end;

procedure TDDDRepositoryRestObjectMapping.AggregateClear(
  aAggregate: TPersistent);
var i: integer;
begin
  if aAggregate<>nil then
    for i := 0 to fProps.Count-1 do
      fProps.List[i].SetValue(aAggregate,nil,false);
end;

procedure TDDDRepositoryRestObjectMapping.AggregateToJSON(
  aAggregate: TPersistent; W: TJSONSerializer; ORMMappedFields: boolean;
  aID: TID);
var i: integer;
begin
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

procedure TDDDRepositoryRestObjectMapping.AggregateToTable(
  aAggregate: TPersistent; aID: TID; aDest: TSQLRecord);
var i: integer;
    Value: RawUTF8;
    wasString: boolean;
begin
  if aDest=nil then
    raise EDDDRepository.CreateUTF8('%.AggregateToTable(%,%,aDest: %=nil)',
      [self,aAggregate,aID,fTable]);
  if aAggregate=nil then begin
    aDest.ClearProperties;
    exit;
  end;
  for i := 0 to fProps.Count-1 do begin
    if fAggregateToTable[i]<>nil then
      fProps.List[i].GetValueVar(aAggregate,false,Value,@wasString) else
      Value := '';
    fAggregateToTable[i].SetValue(aDest,pointer(Value),wasString);
  end;
  aDest.ID := aID;
end;

procedure TDDDRepositoryRestObjectMapping.AggregateFromTable(
  aSource: TSQLRecord; aAggregate: TPersistent);
var i: integer;
    Value: RawUTF8;
    wasString: boolean;
begin
  if aAggregate=nil then
    raise EDDDRepository.CreateUTF8('%.AggregateFromTable(%,aAggregate: %=nil)',
      [self,aSource,fAggregate]);
  if aSource=nil then begin
    AggregateClear(aAggregate);
    exit;
  end;
  for i := 0 to fProps.Count-1 do begin
    if fAggregateToTable[i]=nil then
      Value := '' else
      fAggregateToTable[i].GetValueVar(aSource,false,Value,@wasString);
    fProps.List[i].SetValue(aAggregate,pointer(Value),wasString);
  end;
end;

procedure TDDDRepositoryRestObjectMapping.ComputeMapping;
var i: integer;
begin
  for i := 0 to fProps.Count-1 do begin
    fAggregateToTable[i] := nil;
    fTableToAggregate[i] := nil;
  end;

end;

function TDDDRepositoryRestObjectMapping.InternalResolve(
  aInterface: PTypeInfo; out Obj): boolean;
begin
  if fInterface.InterfaceTypeInfo=aInterface then begin
    IInterface(Obj) := fImplementation.Create(self);
    result := true;
  end else
    result := false;
end;



{ TDDDRepositoryRestMapper }

function TDDDRepositoryRestMapper.AddMap(aAggregate: TPersistentClass;
  const aInterface: TGUID; aImplementation: TDDDRepositoryRestClass;
  aRest: TSQLRest; aTable: TSQLRecordClass;
  const TableAggregatePairs: array of RawUTF8): TDDDRepositoryRestObjectMapping;
begin
  if GetMapIndex(aAggregate)>=0 then
    raise EDDDRepository.CreateUTF8('Duplicated %.AddMap(%)',[self,aAggregate]);
  result := TDDDRepositoryRestObjectMapping.Create(self,
    aAggregate,aInterface,aImplementation,aRest,aTable,TableAggregatePairs);
  ObjArrayAdd(fMap,result);
end;

destructor TDDDRepositoryRestMapper.Destroy;
begin
  ObjArrayClear(fMap);
  inherited;
end;

function TDDDRepositoryRestMapper.GetMap(
  aAggregate: TPersistentClass): TDDDRepositoryRestObjectMapping;
var i: integer;
begin
  i := GetMapIndex(aAggregate);
  if i<0 then
    raise EDDDRepository.CreateUTF8('%.GetMap(%)=nil',[self,aAggregate]);
  result := fMap[i];
end;

function TDDDRepositoryRestMapper.GetMapIndex(
  aAggregate: TPersistentClass): integer;
begin
  for result := 0 to length(fMap)-1 do
    if fMap[result].Aggregate=aAggregate then
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

procedure TDDDRepositoryRestQuery.SetResult(Error: TCQRSResult);
begin
  if Error=cqrsDataLayerError then
    inherited SetResultJSON(Error,
      '{LocalTime:?,Repository:?,Aggregate:?,ORM:?,ID:?,Rest:?,SQLSet:?}',[],
      [NowToString,fMapping.fInterface.InterfaceTypeInfo^.Name,fMapping.Aggregate,
       fMapping.Table,fORM.ID,fMapping.Rest,fORM.GetSQLSet]) else
    inherited SetResult(Error);
end;

function TDDDRepositoryRestQuery.ORMGetOneAndSetResult(WhereClauseFmt: PUTF8Char;
   const Bounds: array of const): TCQRSResult;
begin
  ORMBeforeQuery(result);
  fORM.ClearProperties;
  if fMapping.Rest.Retrieve(WhereClauseFmt,[],Bounds,fORM) then
    SetResult(cqrsSuccess) else
    SetResult(cqrsNotFound);
end;

function TDDDRepositoryRestQuery.ORMToAggregateAndSetResult(
  aAggregate: TPersistent): TCQRSResult;
begin
  if ORMAfterQuery(result) then begin
    fMapping.AggregateFromTable(fORM,aAggregate);
    SetResult(cqrsSuccess);
  end;
end;


{ TDDDRepositoryRestCommand }

destructor TDDDRepositoryRestCommand.Destroy;
begin

  inherited Destroy;
end;

function TDDDRepositoryRestCommand.Delete: TCQRSResult;
begin
  if ORMAfterQuery(result) then 
    if fMapping.Rest.Delete(fMapping.Table,fORM.ID) then
      SetResult(cqrsSuccess) else
      SetResult(cqrsDataLayerError);
end;


{ ----- Authentication Implementation using SHA-256 dual step challenge }

{ TDDDAuthenticationAbstract }

function TDDDAuthenticationAbstract.ChallengeSelectFirst(
  const aLogonName: RawUTF8): TAuthQueryNonce;
begin
  fChallengeLogonName := aLogonName;
  fChallengeNonce := DoHash(aLogonName+NowToString);
end;

function TDDDAuthenticationAbstract.ChallengeSelectFinal(
  const aChallengedPassword: TAuthQueryNonce): TCQRSResult;
begin
  Reset(result);
  if (fChallengeLogonName='') or (fChallengeNonce='') then
    SetResult(cqrsBadRequest) else
    if DoHash(fChallengeLogonName+':'+fChallengeNonce+':'+
       GetHashedPasswordFromRepository(fChallengeLogonName))=aChallengedPassword then
      SetResult(cqrsSuccess) else
      SetResultMsg(cqrsBadRequest,'Wrong Password for "%"',[fChallengeLogonName]);
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
  result := ORMGetOneAndSetResult('LogonName=?',[aLogonName]);
end;

function TDDDAuthenticationAbstract.Get(
  out aAggregate: TAuthInfo): TCQRSResult;
begin
  result := ORMToAggregateAndSetResult(aAggregate);
end;

function TDDDAuthenticationAbstract.Add(const aLogonName: RawUTF8;
  aHashedPassword: TAuthQueryNonce): TCQRSResult;
begin
  if not ORMAfterQuery(result) then
    exit;

end;

function TDDDAuthenticationAbstract.UpdatePassword(
  const aHashedPassword: TAuthQueryNonce): TCQRSResult;
begin
  if not ORMAfterQuery(result) then
    exit;

end;

function TDDDAuthenticationAbstract.Commit: TCQRSResult;
begin
  Reset(result);

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
