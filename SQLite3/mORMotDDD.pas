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
  SynLog,
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
  // - cqrsDDDValidationFailed will be trigerred when
  // - cqrsInvalidContent for any I*Command method with invalid aggregate input
  // value (e.g. a missing field)
  // - cqrsAlreadyExists for a I*Command.Add method with a primay key conflict
  // - cqrsNoPriorQuery for a I*Command.Update/Delete method with no prior
  // call to SelectBy*()
  // - cqrsNoPriorCommand for a I*Command.Commit with no prior Add/Update/Delete
  // - cqrsUnspecifiedError will be used for any other kind of error
  TCQRSResult =
    (cqrsSuccess, cqrsUnspecifiedError, cqrsBadRequest,
     cqrsNotFound, cqrsNoMoreData, cqrsDataLayerError, cqrsDDDValidationFailed,
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
    qaNone,
    qaSelect, qaGet,
    qaCommandDirect, qaCommandOnSelect, 
    qaCommit);

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
    procedure ORMResult(Error: TCQRSResult); 
    procedure ORMSuccessIf(SuccessCondition: boolean;
      ErrorIfFalse: TCQRSResult=cqrsDataLayerError);
    procedure ORMResultMsg(Error: TCQRSResult; const ErrorMessage: RawUTF8); overload;
    procedure ORMResultMsg(Error: TCQRSResult;
      ErrorMsgFmt: PUTF8Char; const ErrorMsgArgs: array of const); overload;
    procedure ORMResultDoc(Error: TCQRSResult; const ErrorInfo: variant);
    procedure ORMResultJSON(Error: TCQRSResult;
      JSONFmt: PUTF8Char; const Args,Params: array of const);
    function GetLastError: TCQRSResult;
    function GetLastErrorInfo: variant; virtual;
    procedure InternalORMResult(Error: TCQRSResult); virtual;
    procedure AfterInternalORMResult; virtual;
  published
    /// the last error, as an enumerate
    property LastError: TCQRSResult read GetLastError;
    /// the action currently processing
    property Action: TCQRSQueryAction read fAction;
    /// current step of the TCQRSQueryObject state machine
    property State: TCQRSQueryState read fState;
  end;



{ *********** Cross-Cutting Layer Implementation}

{ ----- Persistence / Repository Implementation using mORMot's ORM }

type
  TDDDRepositoryRestFactory = class;
  TDDDRepositoryRestQuery = class;
  TDDDRepositoryRestClass = class of TDDDRepositoryRestQuery;

  /// abstract ancestor for all persistence/repository related Exceptions
  EDDDRepository = class(ESynException)
  public
    /// constructor like FormatUTF8() which will also serialize the caller info
    constructor CreateUTF8(Caller: TDDDRepositoryRestFactory;
      Format: PUTF8Char; const Args: array of const);
  end;

  /// store reference of several factories, each with one mapping definition
  TDDDRepositoryRestFactoryObjArray = array of TDDDRepositoryRestFactory;

  /// home repository of several TPersistent factories using REST storage
  // - this shared class will be can to manage a service-wide repositories,
  // i.e. will handle all actual I*Query/I*Command implementation classes
  // accross a set of TSQLRest instances
  // - is designed to optimize BATCH or transactional process
  TDDDRepositoryRestManager = class
  protected
    fFactory: TDDDRepositoryRestFactoryObjArray;
  public
    /// finalize all factories
    destructor Destroy; override;
    /// register DDD's TPersistent repository over an ORM's TSQLRecord
    // - will raise an exception if the aggregate has already been defined
    function AddFactory(
      const aInterface: TGUID; aImplementation: TDDDRepositoryRestClass;
      aAggregate: TPersistentClass; aRest: TSQLRest; aTable: TSQLRecordClass;
      const TableAggregatePairs: array of RawUTF8): TDDDRepositoryRestFactory;
    /// retrieve the registered definition of a given TPersistent in Factory[]
    // - returns -1 if the TPersistence class is unknown
    function GetFactoryIndex(const aInterface: TGUID): integer;
    /// retrieve the registered Factory definition of a given TPersistent
    // - raise an EDDDRepository exception if the TPersistence class is unknown
    function GetFactory(const aInterface: TGUID): TDDDRepositoryRestFactory;
    /// read-only access to all defined persistent factories
    property Factory: TDDDRepositoryRestFactoryObjArray read fFactory;
  end;

  /// implement DDD's TPersistent factory over one ORM's TSQLRecord
  // - it will centralize some helper classes and optimized class mapping
  TDDDRepositoryRestFactory = class(TInterfaceResolver)
  protected
    fOwner: TDDDRepositoryRestManager;
    fInterface: TInterfaceFactory;
    fImplementation: TDDDRepositoryRestClass;
    fImplementationEntry: PInterfaceEntry;
    fRest: TSQLRest;
    fAggregate: TPersistentClass;
    fAggregateHasCustomCreate: boolean;
    fTable: TSQLRecordClass;
    fAggregateRTTI: TSQLPropInfoList;
    // stored in fGarbageCollector, following fAggregateProp[]
    fGarbageCollector: TObjectDynArray;
    fFilter: array of array of TSynFilter;
    fValidate: array of array of TSynValidate;
    // TSQLPropInfoList correspondance, as filled by ComputeMapping:
    fAggregateToTable: TSQLPropInfoObjArray;
    fAggregateProp: TSQLPropInfoRTTIObjArray;
    // store custom field mapping between TSQLRecord and Aggregate
    fPropsMapping: TSQLRecordPropertiesMapping;
    fPropsMappingVersion: cardinal;
    procedure ComputeMapping;
    function GetImplementationName: string;
    function GetAggregateName: string;
    function GetTableName: string;
    // override those methods to customize the data marshalling
    procedure AggregatePropToTable(
      aAggregate: TPersistent; aAggregateProp: TSQLPropInfo;
      aRecord: TSQLRecord; aRecordProp: TSQLPropInfo); virtual;
    procedure TablePropToAggregate(
      aRecord: TSQLRecord; aRecordProp: TSQLPropInfo;
      aAggregate: TPersistent; aAggregateProp: TSQLPropInfo); virtual;
    // to implement the TInterfaceResolver abilities
    function TryResolve(aInterface: PTypeInfo; out Obj): boolean; override;
  public
    /// will compute the ORM TSQLRecord* source code type definitions
    // corresponding to DDD aggregate objects into a a supplied file name
    // - will generate one TSQLRecord* per aggregate class level, following the
    // inheritance hierarchy
    // - dedicated DDD types will be translated into native ORM types (e.g. RawUTF8)
    // - if no file name is supplied, it will generate a dddsqlrecord.inc file
    // in the executable folder
    // - could be used as such:
    // ! TDDDRepositoryRestFactory.ComputeSQLRecord([TPersonContactable,TAuthInfo]);
    // - once created, you may refine the ORM definition, by adding
    // ! ...  read f.... write f... stored AS_UNIQUE;
    // for fields which should be unique, and/or
    // ! ... read f... write f... index #;
    // to specify a field width for SQL storage which requires it, and/or define
    // !protected
    // !  class procedure InternalDefineModel(Props: TSQLRecordProperties); override;
    // to add some ORM-level filters/validators, or low-level :
    // !class procedure TSQLRecordMyAggregate.InternalDefineModel(
    // !  Props: TSQLRecordProperties);
    // !begin
    // !  AddFilterNotVoidText(['HashedPassword']);
    // !  Props.SetCustomCollation('Field','BINARY');
    // !  Props.AddFilterOrValidate('Email',TSynValidateEmail.Create);
    // !end;
    class procedure ComputeSQLRecord(const aAggregate: array of TPersistentClass;
      DestinationSourceCodeFile: TFileName='');
    /// initialize the DDD Aggregate factory using a mORMot ORM class
    // - by default, field names should match on both sides - but you can
    // specify a custom field mapping as TSQLRecord,Aggregate pairs
    // - any missing or unexpected field on any side will just be ignored
    constructor Create(
      const aInterface: TGUID; aImplementation: TDDDRepositoryRestClass;
      aAggregate: TPersistentClass; aRest: TSQLRest; aTable: TSQLRecordClass;
      const TableAggregatePairs: array of RawUTF8;
      aOwner: TDDDRepositoryRestManager=nil); reintroduce; overload;
    /// initialize the DDD Aggregate factory using a mORMot ORM class
    // - this overloaded constructor does not expect any custom fields
    // - any missing or unexpected field on any side will just be ignored
    constructor Create(
      const aInterface: TGUID; aImplementation: TDDDRepositoryRestClass;
      aAggregate: TPersistentClass; aRest: TSQLRest; aTable: TSQLRecordClass;
      aOwner: TDDDRepositoryRestManager=nil); reintroduce; overload;
    /// finalize the factory
    destructor Destroy; override;
    /// register a custom filter or validator to some Aggregate's fields
    // - once added, the TSynFilterOrValidate instance will be owned to
    // this factory, until it is released
    // - the field names should be named from their full path (e.g. 'Email' or
    // 'Address.Country.Iso') unless aFieldNameFlattened is TRUE, which will
    // expect ORM-like naming (e.g. 'Address_Country')
    // - if '*' is specified as field name, it will be applied to all text
    // fields, so the following will ensure that all text fields will be
    // trimmed for spaces:
    // !   AddFilterOrValidate(['*'],TSynFilterTrim.Create);
    // - filters and validators will be applied to the
    // - the same filtering classes as with the ORM can be applied to DDD's
    // aggregates, e.g. TSynFilterUpperCase, TSynFilterLowerCase or
    // TSynFilterTrim
    // - the same validation classes as with the ORM can be applied to DDD's
    // aggregates, e.g. TSynValidateText.Create for a void field,
    // TSynValidateText.Create('{MinLength:5}') for a more complex test
    // (including custom password strength validation if TSynValidatePassWord
    // is not enough), TSynValidateIPAddress.Create or TSynValidateEmail.Create
    // for some network settings, or TSynValidatePattern.Create()
    // - you should not define TSynValidateUniqueField here, which could't be
    // checked at DDD level, but rather set a "stored AS_UNIQUE" attribute
    // in the corresponding property of the TSQLRecord type definition
    procedure AddFilterOrValidate(const aFieldNames: array of RawUTF8;
      aFilterOrValidate: TSynFilterOrValidate; aFieldNameFlattened: boolean=false); virtual;
    /// set a local I*Query or I*Command instance corresponding to this factory
    procedure Get(out Obj); virtual;
    /// clear all properties of a given DDD Aggregate
    procedure AggregateClear(aAggregate: TPersistent);
    /// create a new DDD Aggregate instance
    function AggregateCreate: TPersistent;
    /// perform filtering and validation on a supplied DDD Aggregate
    // - all logic defined by AddFilterOrValidate() will be processed
    function AggregateFilterAndValidate(aAggregate: TPersistent;
      aInvalidFieldIndex: PInteger=nil): RawUTF8; virtual;
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
    /// the home repository owning this factory
    property Owner: TDDDRepositoryRestManager read fOwner;
    /// the DDD's TPersistent handled by this factory
    property Aggregate: TPersistentClass read fAggregate;
    /// the ORM's TSQLRecord used for actual storage
    property Table: TSQLRecordClass read fTable;
    /// the mapped DDD's TPersistent published properties RTTI
    property Props: TSQLPropInfoList read fAggregateRTTI;
    /// access to the Aggregate / ORM field mapping
    property FieldMapping: TSQLRecordPropertiesMapping read fPropsMapping;
  published
    /// the associated I*Query / I*Command repository interface
    property Repository: TInterfaceFactory read fInterface;
    /// the associated TSQLRest instance
    property Rest: TSQLRest read fRest;
    /// the class name which will implement each repository instance 
    property ImplementationClass: string read GetImplementationName;
    /// the DDD's TPersistent class name handled by this factory
    property AggregateClass: string read GetAggregateName;
    /// the ORM's TSQLRecord class name used for actual storage
    property TableClass: string read GetTableName;
  end;

  /// abstract repository class to implement I*Query interface using RESTful ORM
  TDDDRepositoryRestQuery = class(TCQRSQueryObject)
  protected
    fFactory: TDDDRepositoryRestFactory;
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
    function ORMSelectCount(ORMWhereClauseFmt: PUTF8Char; const Args,Bounds: array of const;
      out aResultCount: integer; ForcedBadRequest: boolean=false): TCQRSResult;
    // will log any error on the owner Rest server
    procedure AfterInternalORMResult; override;
  public
    /// you should not have to use this constructor, since the instances would
    // be injected by TDDDRepositoryRestFactory.TryResolve()
    constructor Create(aFactory: TDDDRepositoryRestFactory); reintroduce; virtual;
    /// finalize the used memory
    destructor Destroy; override;
    /// return the number all currently selected aggregates
    // - returns 0 if no select was available, 1 if it was a ORMGetSelectOne(),
    // or the number of items after a ORMGetSelectAll()
    // - this is a generic operation which would work for any class
    // - if you do not need this method, just do not declare it in I*Command
    function GetCount: integer; virtual;
  published
    /// access to the associated rfactory
    property Factory: TDDDRepositoryRestFactory read fFactory;
    /// access to the current state of the underlying mapped TSQLRecord
    // - is nil if no query was run yet
    // - contains the queried object after a successful Select*() method
    property ORM: TSQLRecord read fORM;
  end;

  /// abstract class to implement I*Command interface using ORM's TSQLRecord
  // - it will use an internal TSQLRestBatch for commit
  TDDDRepositoryRestCommand = class(TDDDRepositoryRestQuery)
  protected
    fBatch: TSQLRestBatch;
    fBatchAutomaticTransactionPerRow: cardinal;
    fBatchOptions: TSQLRestBatchOptions;
    fBatchResults: TIDDynArray;
    procedure ORMEnsureBatchExists;
    // this default implementation will check the status vs command,
    // call DDD's + ORM's FilterAndValidate, then add to the internal BATCH
    // - you should override it, if you need a specific behavior
    procedure ORMPrepareForCommit(aCommand: TSQLOccasion;
      aAggregate: TPersistent); virtual;
    /// minimal implementation using AggregateToTable() conversion
    function ORMAdd(aAggregate: TPersistent): TCQRSResult; virtual;
    function ORMUpdate(aAggregate: TPersistent): TCQRSResult; virtual;
    /// this default implementation will send the internal BATCH
    // - you should override it, if you need a specific behavior
    procedure InternalCommit; virtual;
    /// on rollback, delete the internal BATCH
    procedure InternalRollback; virtual;
  public
    /// this constructor will set default fBatch options
    constructor Create(aFactory: TDDDRepositoryRestFactory); override;
    /// finalize the Unit Of Work context
    // - any uncommited change will be lost
    destructor Destroy; override;
    /// perform a deletion on the currently selected aggregate
    // - this is a generic operation which would work for any class
    // - if you do not need this method, just do not declare it in I*Command
    function Delete: TCQRSResult; virtual;
    /// perform a deletion on all currently selected aggregates
    // - this is a generic operation which would work for any class
    // - if you do not need this method, just do not declare it in I*Command
    function DeleteAll: TCQRSResult; virtual;
    /// write all pending changes prepared by Add/UpdatePassword/Delete methods
    // - will process the current fORM using the fCommand
    function Commit: TCQRSResult; virtual;
  end;



{ *********** Application Layer Implementation }

type
  /// abstract class for implementing an Application Layer service
  TDDDApplication = class(TInjectableObject)
  protected

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
  InternalORMResult(Error);
  AfterInternalORMResult;
end;

procedure TCQRSQueryObject.InternalORMResult(Error: TCQRSResult);
begin
  if fLastErrorAddress=nil then
    raise ECQRSException.CreateUTF8('%.ORMResult(%) with no prior ORMBegin',
      [self,GetEnumName(TypeInfo(TCQRSResult),ord(Error))^]);
  fLastErrorAddress^ := Error;
  fLastError := Error;
  if Error<>cqrsSuccess then begin
    fLastErrorContext := _JsonFast(ObjectToJSONDebug(self));
    TDocVariantData(fLastErrorContext).AddValue('LocalTime',NowToString);
  end else
    if ACTION_TO_STATE[fAction]<>qsNone then
      fState := ACTION_TO_STATE[fAction];
  fAction := qaNone;
end;

procedure TCQRSQueryObject.AfterInternalORMResult;
begin
end;

procedure TCQRSQueryObject.ORMSuccessIf(SuccessCondition: boolean;
  ErrorIfFalse: TCQRSResult);
begin
  if SuccessCondition then
    ORMResult(cqrsSuccess) else
    ORMResult(ErrorIfFalse);
end;

procedure TCQRSQueryObject.ORMResultDoc(Error: TCQRSResult;
  const ErrorInfo: variant);
begin
  InternalORMResult(Error);
  TDocVariantData(fLastErrorContext).AddValue('Info',ErrorInfo);
  AfterInternalORMResult;
end;

procedure TCQRSQueryObject.ORMResultJSON(Error: TCQRSResult;
  JSONFmt: PUTF8Char; const Args,Params: array of const);
begin
  ORMResultDoc(Error,_JsonFastFmt(JSONFmt,Args,Params));
end;

procedure TCQRSQueryObject.ORMResultMsg(Error: TCQRSResult;
  const ErrorMessage: RawUTF8);
begin
  InternalORMResult(Error);
  TDocVariantData(fLastErrorContext).AddValue('Msg',ErrorMessage);
  AfterInternalORMResult;
end;

procedure TCQRSQueryObject.ORMResultMsg(Error: TCQRSResult;
  ErrorMsgFmt: PUTF8Char; const ErrorMsgArgs: array of const);
begin
  ORMResultMsg(Error,FormatUTF8(ErrorMsgFmt,ErrorMsgArgs));
end;


{ ----- Persistence / Repository Implementation using mORMot's ORM }

{ EDDDRepository }

constructor EDDDRepository.CreateUTF8(
  Caller: TDDDRepositoryRestFactory; Format: PUTF8Char;
  const Args: array of const);
begin
  if Caller=nil then
    inherited CreateUTF8(Format,Args) else
    inherited CreateUTF8('% - %',[FormatUTF8(Format,Args),ObjectToJSONDebug(Caller)]);
end;


{ TDDDRepositoryRestFactory }

constructor TDDDRepositoryRestFactory.Create(
  const aInterface: TGUID; aImplementation: TDDDRepositoryRestClass;
  aAggregate: TPersistentClass; aRest: TSQLRest; aTable: TSQLRecordClass;
  const TableAggregatePairs: array of RawUTF8; aOwner: TDDDRepositoryRestManager);
begin
  inherited Create;
  fOwner := aOwner;
  fImplementation := aImplementation;
  fImplementationEntry := aImplementation.GetInterfaceEntry(aInterface);
  fRest := aRest;
  fAggregate := aAggregate;
  fAggregateHasCustomCreate := fAggregate.InheritsFrom(TPersistentWithCustomCreate);
  fTable := aTable;
  fInterface := TInterfaceFactory.Get(aInterface);
  if fInterface=nil then
    raise EDDDRepository.CreateUTF8(self,
     '%.Create(%): Interface not registered - you could use TInterfaceFactory.'+
     'RegisterInterfaces()',[self,GUIDToShort(aInterface)]);
  if fImplementationEntry=nil then
    raise EDDDRepository.CreateUTF8(self,'%.Create: % does not implement %',
      [self,aImplementation,fInterface.InterfaceTypeInfo^.Name]);
  if (fAggregate=nil) or (fRest=nil) or (fTable=nil) or (fImplementation=nil) then
    raise EDDDRepository.CreateUTF8(self,'Invalid %.Create(nil)',[self]);
  fPropsMapping.Init(aTable,RawUTF8(fAggregate.ClassName),aRest,false);
  fPropsMapping.MapFields(['ID','####']); // no ID/RowID for our aggregates
  fPropsMapping.MapFields(TableAggregatePairs);
  fAggregateRTTI := TSQLPropInfoList.Create(fAggregate,[pilAllowIDFields,pilSubClassesFlattening]);
  SetLength(fAggregateToTable,fAggregateRTTI.Count);
  SetLength(fAggregateProp,fAggregateRTTI.Count);
  ComputeMapping;
  Rest.LogClass.Add.Log(sllDDDInfo,'Started %',[self]);
end;

constructor TDDDRepositoryRestFactory.Create(const aInterface: TGUID;
  aImplementation: TDDDRepositoryRestClass; aAggregate: TPersistentClass;
  aRest: TSQLRest; aTable: TSQLRecordClass;
  aOwner: TDDDRepositoryRestManager);
begin
  Create(aInterface,aImplementation,aAggregate,aRest,aTable,[],aOwner);
end;

destructor TDDDRepositoryRestFactory.Destroy;
begin
  Rest.LogClass.Add.Log(sllDDDInfo,'Destroying %',[self]);
  fAggregateRTTI.Free;
  ObjArrayClear(fGarbageCollector);
  inherited;
end;

class procedure TDDDRepositoryRestFactory.ComputeSQLRecord(
  const aAggregate: array of TPersistentClass; DestinationSourceCodeFile: TFileName);
const RAW_TYPE: array[TSQLFieldType] of RawUTF8 = (
    // values left to '' will use the RTTI type
    '',                 // sftUnknown
    'RawUTF8',          // sftAnsiText
    'RawUTF8',          // sftUTF8Text
    '',                 // sftEnumerate
    '',                 // sftSet
    '',                 // sftInteger
    '',                 // sftID = TSQLRecord(aID)
    'TRecordReference', // sftRecord = TRecordReference
    'boolean',          // sftBoolean
    'double',           // sftFloat
    'TDateTime',        // sftDateTime
    'TTimeLog',         // sftTimeLog
    'currency',         // sftCurrency
    '',                 // sftObject
    'variant',          // sftVariant
    'TSQLRawBlob',      // sftBlob
    'variant',          // sftBlobDynArray
    '',                 // sftBlobCustom
    'variant',          // sftUTF8Custom
    '',                 // sftMany
    'TModTime',         // sftModTime
    'TCreateTime',      // sftCreateTime
    '');                // sftTID
var hier: TClassDynArray;
    a,i,f: integer;
    code,aggname,recname,parentrecname: RawUTF8;
    map: TSQLPropInfoList;
    rectypes: TRawUTF8DynArray;
begin
  {$ifdef KYLIX3} hier := nil; {$endif to make compiler happy}
  if DestinationSourceCodeFile='' then
    DestinationSourceCodeFile := ExtractFilePath(paramstr(0))+'ddsqlrecord.inc';
  for a := 0 to high(aAggregate) do begin
    hier := ClassHierarchyWithField(aAggregate[a]);
    code := code+#13#10'type';
    parentrecname := 'TSQLRecord';
    for i := 0 to high(hier) do begin
      aggname := RawUTF8(hier[i].ClassName);
      recname := 'TSQLRecord'+copy(aggname,2,100);
      map := TSQLPropInfoList.Create(hier[i],
        [pilAllowIDFields,pilSubClassesFlattening,pilSingleHierarchyLevel]);
      try
        code := FormatUTF8('%'#13#10+
          '  /// ORM class corresponding to % DDD aggregate'#13#10+
          '  % = class(%)'#13#10'  protected'#13#10,
          [code,aggname,recname,parentrecname]);
        SetLength(rectypes,map.count);
        for f := 0 to map.Count-1 do
        with map.List[f] do begin
          rectypes[f] := RAW_TYPE[SQLFieldType];
          if rectypes[f]='' then
            if SQLFieldType=sftInteger then begin
              rectypes[f] := 'Int64';
              if InheritsFrom(TSQLPropInfo) then
                with TSQLPropInfoRTTI(map.List[f]).PropType^ do
                  if (Kind=tkInteger) and (OrdType<>otULong) then
                    rectypes[f] := 'integer'; // only cardinal -> Int64
            end else
              rectypes[f]:= SQLFieldRTTITypeName;
          code := FormatUTF8('%    f%: %; // %'#13#10,
            [code,Name,rectypes[f],SQLFieldRTTITypeName]);
        end;
        code := code+'  published'#13#10;
        for f := 0 to map.Count-1 do
        with map.List[f] do
          code := FormatUTF8('%    /// maps %.%'#13#10+
            '    property %: % read f% write f%;'#13#10,
            [code,aggname,NameUnflattened,Name,rectypes[f],Name,Name]);
        code := code+'  end;'#13#10;
      finally
        map.Free;
      end;
      parentrecname := recname;
    end;
  end;
  FileFromString(code,DestinationSourceCodeFile);
end;

procedure TDDDRepositoryRestFactory.ComputeMapping;
{ TODO:
  Complex types flattening for aggregates (allowing any sub level):
  * User.Email -> TSQLUser.EMail
  * User.Name.First -> TSQLUser.Name_First
  * User.Address.Street1 -> TSQLUser.Address_Street1
  * User.Address.Country.Iso  -> TSQLUser.Address_Country (Iso is single prop)
  * User.BirthDate.Value -> TSQLUser.BirthDate (Value is single prop)
  T*ObjArray properties: Order.Line[] TOrderLineObjArray -> which one?
  -> TSQLOrder.Line as variant (JSON) ?
  -> TSQLOrder.Line as JSON dynarray (new feature request) ?
}
procedure EnsureCompatible(agg,rec: TSQLPropInfo);
begin
  if agg.SQLDBFieldType<>rec.SQLDBFieldType then
    raise EDDDRepository.CreateUTF8(self,
      '% types do not match at DB level: %.%:%=% and %.%:%=%',[self,
      fAggregate,agg.Name,agg.SQLFieldRTTITypeName,agg.SQLDBFieldTypeName,
      fTable,rec.Name,rec.SQLFieldRTTITypeName,rec.SQLDBFieldTypeName]);
end;
var i,ndx: integer;
    ORMProps: TSQLPropInfoObjArray;
begin
  ORMProps := fTable.RecordProps.Fields.List;
  for i := 0 to fAggregateRTTI.Count-1 do begin
    fAggregateProp[i] := fAggregateRTTI.List[i] as TSQLPropInfoRTTI;
    ndx := fPropsMapping.ExternalToInternalIndex(fAggregateProp[i].Name);
    if ndx<0 then // ID/RowID or TPersistent property not defined in TSQLRecord
      fAggregateToTable[i] := nil else begin
      fAggregateToTable[i] := ORMProps[ndx];
      EnsureCompatible(fAggregateProp[i],fAggregateToTable[i]);
    end;
  end;
  fPropsMappingVersion := fPropsMapping.MappingVersion;
end;

function TDDDRepositoryRestFactory.TryResolve(
  aInterface: PTypeInfo; out Obj): boolean;
begin
  if fInterface.InterfaceTypeInfo<>aInterface then
    result := false else
    result := GetInterfaceFromEntry(fImplementation.Create(self),fImplementationEntry,Obj);
end;

procedure TDDDRepositoryRestFactory.AggregatePropToTable(
  aAggregate: TPersistent; aAggregateProp: TSQLPropInfo;
  aRecord: TSQLRecord; aRecordProp: TSQLPropInfo);
begin
  if aRecordProp<>nil then
    aAggregateProp.CopyProp(aAggregate,aRecordProp,aRecord);
end;

procedure TDDDRepositoryRestFactory.TablePropToAggregate(
  aRecord: TSQLRecord; aRecordProp: TSQLPropInfo; aAggregate: TPersistent;
  aAggregateProp: TSQLPropInfo);
begin
  if aRecordProp=nil then
    aAggregateProp.SetValue(aAggregate,nil,false) else
    aRecordProp.CopyProp(aRecord,aAggregateProp,aAggregate);
end;

procedure TDDDRepositoryRestFactory.Get(out Obj);
begin
  if not GetInterfaceFromEntry(fImplementation.Create(self),fImplementationEntry,Obj) then
    raise ECQRSException.CreateUTF8('%.Get(%)',[self,fInterface.InterfaceTypeInfo^.Name]);
end;

procedure TDDDRepositoryRestFactory.AggregateClear(
  aAggregate: TPersistent);
var i: integer;
begin
  if aAggregate<>nil then
    for i := 0 to high(fAggregateProp) do
      with fAggregateProp[i] do
        SetValue(Flattened(aAggregate),nil,false);
end;

function TDDDRepositoryRestFactory.AggregateCreate: TPersistent;
begin
  if fAggregateHasCustomCreate then
    result := TPersistentWithCustomCreateClass(fAggregate).Create else
    result := fAggregate.Create;
end;

procedure TDDDRepositoryRestFactory.AggregateToJSON(aAggregate: TPersistent;
  W: TJSONSerializer; ORMMappedFields: boolean; aID: TID);
var i: integer;
begin
  if fPropsMapping.MappingVersion<>fPropsMappingVersion then
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
  for i := 0 to high(fAggregateProp) do begin
    if ORMMappedFields then
      if fAggregateToTable[i]=nil then
        continue else
        W.AddFieldName(fAggregateToTable[i].Name) else
        W.AddFieldName(fAggregateProp[i].Name);
    with fAggregateProp[i] do
      GetJSONValues(Flattened(aAggregate),W);
    W.Add(',');
  end;
  W.CancelLastComma;
  W.Add('}');
end;

function TDDDRepositoryRestFactory.AggregateToJSON(
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

procedure TDDDRepositoryRestFactory.AggregateToTable(
  aAggregate: TPersistent; aID: TID; aDest: TSQLRecord);
var i: integer;
begin
  if fPropsMapping.MappingVersion<>fPropsMappingVersion then
    ComputeMapping;
  if aDest=nil then
    raise EDDDRepository.CreateUTF8(self,'%.AggregateToTable(%,%,%=nil)',
      [self,aAggregate,aID,fTable]);
  aDest.ClearProperties;
  aDest.ID := aID;
  if aAggregate<>nil then
    for i := 0 to high(fAggregateProp) do
      AggregatePropToTable(aAggregate,fAggregateProp[i],aDest,fAggregateToTable[i]);
end;

procedure TDDDRepositoryRestFactory.AggregateFromTable(
  aSource: TSQLRecord; aAggregate: TPersistent);
var i: integer;
begin
  if fPropsMapping.MappingVersion<>fPropsMappingVersion then
    ComputeMapping;
  if aAggregate=nil then
    raise EDDDRepository.CreateUTF8(self,'%.AggregateFromTable(%=nil)',[self,fAggregate]);
  if aSource=nil then
    AggregateClear(aAggregate) else
    for i := 0 to high(fAggregateProp) do
      TablePropToAggregate(aSource,fAggregateToTable[i],aAggregate,fAggregateProp[i]);
end;

function TDDDRepositoryRestFactory.GetImplementationName: string;
begin
  if self=nil then
    result := '' else
    result := fImplementation.ClassName;
end;

function TDDDRepositoryRestFactory.GetAggregateName: string;
begin
  if self=nil then
    result := '' else
    result := fAggregate.ClassName;
end;

function TDDDRepositoryRestFactory.GetTableName: string;
begin
  if self=nil then
    result := '' else
    result := fTable.ClassName;
end;

procedure TDDDRepositoryRestFactory.AddFilterOrValidate(
  const aFieldNames: array of RawUTF8; aFilterOrValidate: TSynFilterOrValidate;
  aFieldNameFlattened: boolean);
var f,ndx: integer;
    arr: ^TPointerDynArray;
begin
  if aFilterOrValidate=nil then
    exit;
  ObjArrayAdd(fGarbageCollector,aFilterOrValidate);
  for f := 0 to high(aFieldNames) do begin
    if aFilterOrValidate.InheritsFrom(TSynValidate) then
      arr := @fValidate else
      arr := @fFilter;
    if arr^=nil then
      SetLength(arr^,fAggregateRTTI.Count);
    if aFieldNames[f]='*' then begin // apply to all text fields
      for ndx := 0 to high(fAggregateProp) do
        if fAggregateProp[ndx].SQLFieldType in [sftAnsiText,sftUTF8Text] then
          aFilterOrValidate.AddOnce(TSynFilterOrValidateObjArray(arr^[ndx]),false);
    end else begin
      if aFieldNameFlattened then
        ndx := fAggregateRTTI.IndexByNameUnflattenedOrExcept(aFieldNames[f]) else
        ndx := fAggregateRTTI.IndexByNameOrExcept(aFieldNames[f]);
      aFilterOrValidate.AddOnce(TSynFilterOrValidateObjArray(arr^[ndx]),false);
    end;
  end;
end;

function TDDDRepositoryRestFactory.AggregateFilterAndValidate(
  aAggregate: TPersistent; aInvalidFieldIndex: PInteger): RawUTF8;
var f,i: integer;
    Value: TRawUTF8DynArray; // avoid twice retrieval
    Old: RawUTF8;
    msg: string;
    str: boolean;
begin
  if (aAggregate=nil) or (aAggregate.ClassType<>fAggregate) then
    raise EDDDRepository.CreateUTF8(self,
      '%.AggregateFilterAndValidate(%) expected a % instance',
      [self,aAggregate,fAggregate]);
  // first process all filters
  SetLength(Value,fAggregateRTTI.Count);
  for f := 0 to high(fFilter) do
    if fFilter[f]<>nil then begin
      with fAggregateProp[f] do
        GetValueVar(Flattened(aAggregate),false,Value[f],@str);
      Old := Value[f];
      for i := 0 to high(fFilter[f]) do
        fFilter[f,i].Process(f,Value[f]);
      if Old<>Value[f] then
        with fAggregateProp[f] do
          SetValueVar(Flattened(aAggregate),Value[f],str);
    end;
  // then validate the content
  for f := 0 to high(fValidate) do
    if fValidate[f]<>nil then begin
      if Value[f]='' then // if not already retrieved
        with fAggregateProp[f] do
          GetValueVar(Flattened(aAggregate),false,Value[f],nil);
      for i := 0 to high(fValidate[f]) do
        if not fValidate[f,i].Process(f,Value[f],msg) then begin
          if aInvalidFieldIndex<>nil then
            aInvalidFieldIndex^ := f;
          if msg='' then
            // no custom message -> show a default message
            msg := format(sValidationFailed,[GetCaptionFromClass(fValidate[f,i].ClassType)]);
          result := FormatUTF8('%.%: %',
            [fAggregate,fAggregateProp[f].NameUnflattened,msg]);
          exit;
        end;
    end;
  result := ''; // if we reached here, there was no error
end;


{ TDDDRepositoryRestManager }

function TDDDRepositoryRestManager.AddFactory(
  const aInterface: TGUID; aImplementation: TDDDRepositoryRestClass;
  aAggregate: TPersistentClass; aRest: TSQLRest; aTable: TSQLRecordClass;
  const TableAggregatePairs: array of RawUTF8): TDDDRepositoryRestFactory;
begin
  if GetFactoryIndex(aInterface)>=0 then
    raise EDDDRepository.CreateUTF8(nil,'Duplicated GUID for %.AddFactory(%,%,%)',
      [self,GUIDToShort(aInterface),aImplementation,aAggregate]);
  result := TDDDRepositoryRestFactory.Create(
    aInterface,aImplementation,aAggregate,aRest,aTable,TableAggregatePairs,self);
  ObjArrayAdd(fFactory,result);
  aRest.LogClass.Add.Log(sllDDDInfo,'Added factory % to %',[result,self]);
end;

destructor TDDDRepositoryRestManager.Destroy;
begin
  ObjArrayClear(fFactory);
  inherited;
end;

function TDDDRepositoryRestManager.GetFactory(
  const aInterface: TGUID): TDDDRepositoryRestFactory;
var i: integer;
begin
  i := GetFactoryIndex(aInterface);
  if i<0 then
    raise EDDDRepository.CreateUTF8(nil,'%.GetFactory(%)=nil',
      [self,GUIDToShort(aInterface)]);
  result := fFactory[i];
end;

function TDDDRepositoryRestManager.GetFactoryIndex(
  const aInterface: TGUID): integer;
begin
  for result := 0 to length(fFactory)-1 do
    if IsEqualGUID(fFactory[result].fInterface.InterfaceIID,aInterface) then
      exit;
  result := -1;
end;


{ TDDDRepositoryRestQuery }

constructor TDDDRepositoryRestQuery.Create(
  aFactory: TDDDRepositoryRestFactory);
begin
  fFactory := aFactory;
  fORM := fFactory.Table.Create;
end;

destructor TDDDRepositoryRestQuery.Destroy;
begin
  fORM.Free;
  inherited;
end;

procedure TDDDRepositoryRestQuery.AfterInternalORMResult;
begin
  inherited AfterInternalORMResult;
  if (fLastError<>cqrsSuccess) and
     (sllDDDError in Factory.Rest.LogFamily.Level) then
    Factory.Rest.LogClass.Add.Log(sllDDDError,'%',[fLastErrorContext]);
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
    ORMSuccessIf(Factory.Rest.Retrieve(ORMWhereClauseFmt,[],Bounds,ORM),cqrsNotFound);
end;

function TDDDRepositoryRestQuery.ORMSelectAll(
  ORMWhereClauseFmt: PUTF8Char; const Bounds: array of const;
  ForcedBadRequest: boolean): TCQRSResult;
begin
  ORMBegin(qaSelect,result);
  if ForcedBadRequest then
    ORMResult(cqrsBadRequest) else
    ORMSuccessIf(ORM.FillPrepare(Factory.Rest,ORMWhereClauseFmt,[],Bounds),cqrsNotFound);
end;

function TDDDRepositoryRestQuery.ORMSelectCount(
  ORMWhereClauseFmt: PUTF8Char; const Args,Bounds: array of const;
  out aResultCount: integer; ForcedBadRequest: boolean): TCQRSResult;
var tmp: Int64;
begin
  ORMBegin(qaNone,result); // qaNone and not qaSelect which would fill ORM
  if ForcedBadRequest then
    ORMResult(cqrsBadRequest) else
    if Factory.Rest.OneFieldValue(
        Factory.Table,'count(*)',ORMWhereClauseFmt,Args,Bounds,tmp) then begin
       aResultCount := tmp;
       ORMResult(cqrsSuccess)
    end else
      ORMResult(cqrsNotFound);
end;

function TDDDRepositoryRestQuery.GetCount: integer;
var dummy: TCQRSResult;
begin
  if not ORMBegin(qaGet,dummy) then
    result := 0 else
    if ORM.FillTable<>nil then
      result := ORM.FillTable.RowCount else
      if ORM.ID=0 then
        result := 0 else
        result := 1;
end;

function TDDDRepositoryRestQuery.ORMGetAggregate(
  aAggregate: TPersistent): TCQRSResult;
begin
  if ORMBegin(qaGet,result) then begin
    Factory.AggregateFromTable(ORM,aAggregate);
    ORMResult(cqrsSuccess);
  end;
end;

function TDDDRepositoryRestQuery.ORMGetNextAggregate(
  aAggregate: TPersistent; aRewind: boolean): TCQRSResult;
begin
  if ORMBegin(qaGet,result) then
    if (aRewind and ORM.FillRewind) or
       ((not aRewind) and ORM.FillOne) then begin
      Factory.AggregateFromTable(ORM,aAggregate);
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
    while ORM.FillOne do begin
      res[i] := Factory.AggregateCreate;
      Factory.AggregateFromTable(ORM,res[i]);
      inc(i);
    end;
    if i=length(res) then
      ORMResult(cqrsSuccess) else begin
      ObjArrayClear(res);
      ORMResult(cqrsNoMoreData);
    end;
  end;
end;



{ TDDDRepositoryRestCommand }

constructor TDDDRepositoryRestCommand.Create(
  aFactory: TDDDRepositoryRestFactory);
begin
  inherited Create(aFactory);
  fBatchAutomaticTransactionPerRow := 1000; // for better performance
end;

destructor TDDDRepositoryRestCommand.Destroy;
begin
  InternalRollback;
  inherited Destroy;
end;

function TDDDRepositoryRestCommand.Delete: TCQRSResult;
begin
  if ORMBegin(qaCommandOnSelect,result) then
    ORMPrepareForCommit(soDelete,nil);
end;

function TDDDRepositoryRestCommand.DeleteAll: TCQRSResult;
var i: integer;
begin
  if ORMBegin(qaCommandOnSelect,result) then
    if ORM.FillTable=nil then
      ORMPrepareForCommit(soDelete,nil) else
      if fState<qsQuery then
        ORMResult(cqrsNoPriorQuery) else begin
        ORMEnsureBatchExists;
        for i := 1 to ORM.FillTable.RowCount do
          if fBatch.Delete(ORM.FillTable.IDColumnHiddenValue(i))<0 then begin
            ORMResult(cqrsDataLayerError);
            exit;
          end;
        ORMResult(cqrsSuccess);
      end;
end;

function TDDDRepositoryRestCommand.ORMAdd(aAggregate: TPersistent): TCQRSResult;
begin
  if ORMBegin(qaCommandDirect,result) then begin
    Factory.AggregateToTable(aAggregate,0,ORM);
    ORMPrepareForCommit(soInsert,aAggregate);
  end;
end;

function TDDDRepositoryRestCommand.ORMUpdate(aAggregate: TPersistent): TCQRSResult;
begin
  if ORMBegin(qaCommandOnSelect,result) then begin
    Factory.AggregateToTable(aAggregate,ORM.ID,ORM);
    ORMPrepareForCommit(soUpdate,aAggregate);
  end;
end;

procedure TDDDRepositoryRestCommand.ORMEnsureBatchExists;
begin
  if fBatch=nil then
    fBatch := TSQLRestBatch.Create(Factory.Rest,Factory.Table,
      fBatchAutomaticTransactionPerRow,fBatchOptions);
end;

procedure TDDDRepositoryRestCommand.ORMPrepareForCommit(
  aCommand: TSQLOccasion; aAggregate: TPersistent);
var msg: RawUTF8;
    ndx: integer;
begin
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
  if aCommand in [soUpdate,soInsert] then begin
    if aAggregate<>nil then begin
      msg := Factory.AggregateFilterAndValidate(aAggregate);
      if msg<>'' then begin
        ORMResultMsg(cqrsDDDValidationFailed,msg);
        exit;
      end;
    end;
    msg := ORM.FilterAndValidate(Factory.Rest);
    if msg<>'' then begin
      ORMResultMsg(cqrsDataLayerError,msg);
      exit;
    end;
  end;
  ORMEnsureBatchExists;
  ndx := -1;
  case aCommand of
  soInsert: ndx := fBatch.Add(ORM,true);
  soUpdate: ndx := fBatch.Update(ORM);
  soDelete: ndx := fBatch.Delete(ORM.ID);
  end;
  ORMSuccessIf(ndx>=0);
end;

procedure TDDDRepositoryRestCommand.InternalCommit;
begin
  if fBatch.Count=0 then
    ORMResult(cqrsBadRequest) else begin
    ORMSuccessIf(Factory.Rest.BatchSend(fBatch,fBatchResults)=HTML_SUCCESS);
    FreeAndNil(fBatch);
  end;
end;

procedure TDDDRepositoryRestCommand.InternalRollback;
begin
  FreeAndNil(fBatch);
  fBatchResults := nil;
end;

function TDDDRepositoryRestCommand.Commit: TCQRSResult;
begin
  if ORMBegin(qaCommit,result) then
    InternalCommit;
end;


end.