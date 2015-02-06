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
    /// initialize the DDD Aggregate factory using a mORMot ORM class
    // - by default, field names should match on both sides - but you can
    // specify a custom field mapping as TSQLRecord,Aggregate pairs
    // - any missing or unexpected field on any side will just be ignored
    constructor Create(aOwner: TDDDRepositoryRestManager;
      const aInterface: TGUID; aImplementation: TDDDRepositoryRestClass;
      aAggregate: TPersistentClass; aRest: TSQLRest; aTable: TSQLRecordClass;
      const TableAggregatePairs: array of RawUTF8); reintroduce; overload;
    /// finalize the factory
    destructor Destroy; override;
    /// set a local I*Query or I*Command instance corresponding to this factory
    procedure Get(out Obj); virtual;
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
    /// the home repository owning this factory
    property Owner: TDDDRepositoryRestManager read fOwner;
    /// the mapped DDD's TPersistent published properties RTTI 
    property Props: TSQLPropInfoList read fProps;
    /// access to the Aggregate / ORM field mapping
    property FieldMapping: TSQLRecordPropertiesMapping read fPropsMapping;
  published
    /// the associated I*Query / I*Command repository interface
    property Repository: TInterfaceFactory read fInterface;
    /// the associated TSQLRest instance
    property Rest: TSQLRest read fRest;
    /// the DDD's TPersistent handled by this factory
    property Aggregate: TPersistentClass read fAggregate;
    /// the ORM's TSQLRecord used for actual storage
    property Table: TSQLRecordClass read fTable;
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
  public
    /// you should not have to use this constructor, since the instances would
    // be injected by TDDDRepositoryRestFactory.TryResolve()
    constructor Create(aFactory: TDDDRepositoryRestFactory); reintroduce; virtual;
    /// finalize the used memory
    destructor Destroy; override;
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
    // - this default implementation will check the status vs command,
    // call fORM.FilterAndValidate, then add to the internal BATCH
    // - you should override it, if you need a specific behavior
    procedure ORMPrepareForCommit(aCommand: TSQLOccasion); virtual;
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
  Caller: TDDDRepositoryRestFactory; Format: PUTF8Char;
  const Args: array of const);
begin
  if Caller=nil then
    inherited CreateUTF8(Format,Args) else
    inherited CreateUTF8('% - %',[FormatUTF8(Format,Args),ObjectToJSONDebug(Caller)]);
end;


{ TDDDRepositoryRestFactory }

constructor TDDDRepositoryRestFactory.Create(aOwner: TDDDRepositoryRestManager;
  const aInterface: TGUID; aImplementation: TDDDRepositoryRestClass;
  aAggregate: TPersistentClass; aRest: TSQLRest; aTable: TSQLRecordClass;
  const TableAggregatePairs: array of RawUTF8);
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
  fORMProps := fTable.RecordProps.Fields;
  fPropsMapping.Init(aTable,RawUTF8(fAggregate.ClassName),aRest,false);
  fPropsMapping.MapFields(['ID','####']); // no ID/RowID for our aggregates
  fPropsMapping.MapFields(TableAggregatePairs);
  fProps := TSQLPropInfoList.Create(fAggregate,[pilAllowIDFields]);
  SetLength(fAggregateToTable,fProps.Count);
  SetLength(fTableToAggregate,fORMProps.Count);
  ComputeMapping;
end;

destructor TDDDRepositoryRestFactory.Destroy;
begin
  fProps.Free;
  inherited;
end;

procedure TDDDRepositoryRestFactory.ComputeMapping;
{ TODO:
  Complex types flatening for aggregates (allowing 1 sub level at first?):
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
    ndx := fProps.IndexByName(fPropsMapping.FieldNames[i]);
    if ndx<0 then // TSQLRecord property not defined in the TPersistent
      fTableToAggregate[i] := nil else begin
      fTableToAggregate[i] := fProps.List[ndx];
      EnsureCompatible(fTableToAggregate[i],fORMProps.List[i]);
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
    for i := 0 to fProps.Count-1 do
      fProps.List[i].SetValue(aAggregate,nil,false);
end;

function TDDDRepositoryRestFactory.AggregateCreate: TPersistent;
begin
  if fAggregateHasCustomCreate then
    result := TPersistentWithCustomCreateClass(fAggregate).Create else
    result := fAggregate.Create;
end;

procedure TDDDRepositoryRestFactory.AggregateToJSON(
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

procedure TDDDRepositoryRestFactory.AggregateFromTable(
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


{ TDDDRepositoryRestManager }

function TDDDRepositoryRestManager.AddFactory(
  const aInterface: TGUID; aImplementation: TDDDRepositoryRestClass;
  aAggregate: TPersistentClass; aRest: TSQLRest; aTable: TSQLRecordClass;
  const TableAggregatePairs: array of RawUTF8): TDDDRepositoryRestFactory;
begin
  if GetFactoryIndex(aInterface)>=0 then
    raise EDDDRepository.CreateUTF8(nil,'Duplicated GUID for %.AddFactory(%,%,%)',
      [self,GUIDToShort(aInterface),aImplementation,aAggregate]);
  result := TDDDRepositoryRestFactory.Create(self,
    aInterface,aImplementation,aAggregate,aRest,aTable,TableAggregatePairs);
  ObjArrayAdd(fFactory,result);
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
  fORM := fFactory.fTable.Create;
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
    repeat
      res[i] := Factory.AggregateCreate;
      Factory.AggregateFromTable(ORM,res[i]);
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
    ORMPrepareForCommit(soDelete);
end;

procedure TDDDRepositoryRestCommand.ORMPrepareForCommit(
  aCommand: TSQLOccasion);
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
  msg := ORM.FilterAndValidate(Factory.Rest);
  if msg<>'' then begin
    ORMResultMsg(cqrsDataLayerError,msg);
    exit;
  end;
  if fBatch=nil then
    fBatch := TSQLRestBatch.Create(Factory.Rest,Factory.Table,
      fBatchAutomaticTransactionPerRow,fBatchOptions);
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
