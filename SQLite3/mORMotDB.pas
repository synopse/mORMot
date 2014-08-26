/// Virtual Tables for external DB access for mORMot
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit mORMotDB;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2014 Arnaud Bouchez
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

  Portions created by the Initial Developer are Copyright (C) 2014
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

  Version 1.15
  - first public release, corresponding to mORMot Framework 1.15

  Version 1.16
  - TSQLRestServerStaticExternal.EngineList method now handles an optional
    integer pointer, to return the count of row data (excluding field names)

  Version 1.17
  - changed column named 'RowID' into 'ID' since it is reserved e.g. in Oracle
  - external direct insert, update or delete actions (i.e. when the
    TSQLRestServerStaticExternal instance is called directly) will now
    flush the low-level SQLite3 DB cache, as expected by the virtual tables
  - added TSQLRestServerStaticExternal.AdaptSQLForEngineList overridden method to
    handle most generic SELECT to by-pass the SQLite3 virtual module for speed
  - added TSQLRestServerStaticExternal.EndCurrentThread overridden method which
    will be called e.g. by TSQLite3HttpServer or TSQLRestServerNamedPipeResponse
    for each terminating threads, to release external connection resource
    (calling TSQLDBConnectionPropertiesThreadSafe.EndCurrentThread method)
  - any direct or virtual-table based insertion to the external database will
    now use a binding matching the exact time of each column: it will e.g. allow
    to support DBMS which does not accept date/time to be supplied as ISO-8601
    text, and make more efficient data conversion (like avoid conversion to
    floating-point from a currency value) - code shared with BATCH mode and newly
    added TSQLRestServerStaticExternal.ExecuteFromJSON() protected method
  - inlined parameters in any SQL query will bind explicitely TDateTime values
    if the parameter is transmitted as DateToSQL() or DateTimeToSQL() TEXT
  - removed TSQLRecordExternal class type, to allow any TSQLRecord (e.g.
    TSQLRecordMany) to be used with VirtualTableExternalRegister() - there was
    indeed no implementation requirement to force a specific class type
  - now create properly UNIQUE fields (i.e. "stored AS_UNIQUE") in external tables
  - handle NULL values for BLOBs as expected

  Version 1.18
  - unit SQLite3DB.pas renamed mORMotDB.pas
  - TSQLRestServerStaticExternal renamed TSQLRestStorageExternal
  - huge performance boost when inserting individual data rows, by maintaining
    the IDs in memory instead of executing "select max(id)" - added new property
    EngineAddUseSelectMaxID to unset this optimization
  - new function VirtualTableExternalRegisterAll(), to register all tables
    of a mORMot model to be handled via a specified database
  - TSQLRestStorageExternal.AdaptSQLForEngineList() will now accept
    'select count(*) from TableName [where...]' statements directly (virtual
    behavior for count(*) is to loop through all records, which may be slow),
    and 'IN (...)' or 'IS NULL' / 'IS NOT NULL' where clauses
  - now TSQLRestStorageExternal will call TSQLRestServer.OnUpdateEvent and
    OnBlobUpdateEvent callbacks, if defined (even in BATCH mode)
  - BatchDelete() will now split its batch statement executed following
    TSQLDBConnectionProperties.BatchMaxSentAtOnce property expectations 
  - now TSQLRestStorageExternal won't create any columns for external
    tables with unsupported published property types (sftUnknown or sftMany),
    just like TSQLRecord.GetSQLCreate() method
  - now handles TSQLDBConnectionProperties.ForcedSchemaName as expected
  - fixed issue in TSQLRestStorageExternal.EngineDeleteWhere() when
    calling commands like MyDB.Delete(TSQLMyClass, 'PLU < ?', [20000])
  - TSQLRestStorageExternal.EngineDeleteWhere() will handle more border cases,
    and will split DELETE FROM table WHERE ID IN (....) in several intervals 
  - fixed errors when executing JOINed queries (e.g. via FillPrepareMany)
  - fixed ticket [3c41462594] in TSQLRestStorageExternal.ExecuteFromJSON()
  - fixed ticket [9a821d26ee] in TSQLRestStorageExternal.Create() not
    creating any missing field
  - fixed ticket [b109c22750] about SQLite3 cache not flushed after CRUD updates
  - ensure no INDEX is created for SQLite3 which generates an index for ID/RowID
  - ensure DESC INDEX is created for Firebird ID column, as expected for
    faster MAX(ID) execution - see http://www.firebirdfaq.org/faq205
  - fix TSQLRestStorageExternal.CreateSQLMultiIndex() to set ColumnIndexed=TRUE,
    and fixed ticket [929cb6fc3047c5f78b95] by ignoring BLOB fields
  - fixed TSQLRestStorageExternal.UpdateBlobFields() to return true
    if no BLOB field is defined, and to proper handle multi-field update
  - fixed ticket [21c2d5ae96] when inserting/updating blob-only table content
  - handle null binding in TSQLRestStorageExternal.ExecuteInlined()
  - added TSQLRestStorageExternal.TableHasRows/TableRowCount overrides
  - added TSQLRestStorageExternal.PrepareInlinedForRows() and
    PrepareDirectForRows() methods to call new ExecutePreparedAndFetchAllAsJSON()
    method of ISQLDBStatement as expected by TSQLDBProxyStatement
  - optimized TSQLRestStorageExternal.UpdateBlobFields()/RetrieveBlobFields()
    methods, updating/retrieving all BLOB fields at once in the same SQL statement
  - this unit will now set SynDBLog := TSQLLog during its initialization
  - replaced confusing TVarData by a new dedicated TSQLVar memory structure,
    shared with SynDB and mORMot units (includes methods refactoring)

}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  Windows,
  SysUtils,
  Classes,
  SynCommons,
  mORMot,
  SynDB;

type
  /// REST server with direct access to a SynDB-based external database
  // - handle all REST commands, using the external SQL database connection,
  // and prepared statements
  // - is used by TSQLRestServer.URI for faster RESTful direct access
  // - for JOINed SQL statements, the external database is also defined as
  // a SQLite3 virtual table, via the TSQLVirtualTableExternal[Cursor] classes
  TSQLRestStorageExternal = class(TSQLRestStorage)
  protected
    /// values retrieved from fStoredClassProps.ExternalDB settings
    fTableName: RawUTF8;
    fProperties: TSQLDBConnectionProperties;
    fSelectOneDirectSQL, fSelectAllDirectSQL, fSelectTableHasRowsSQL: RawUTF8;
    fRetrieveBlobFieldsSQL, fUpdateBlobfieldsSQL: RawUTF8;
    fEngineUseSelectMaxID: Boolean;
    fEngineLockedLastID: integer;
    /// external column layout as retrieved by fProperties
    // - used internaly to guess e.g. if the column is indexed
    // - fFieldsExternal[] contains the external table info, and the internal
    // column name is available via fFieldsExternalToInternal[]
    fFieldsExternal: TSQLDBColumnDefineDynArray;
    /// gives the index of each fFieldsExternal[] item in Props.Fields[]
    // - is >=0 for index in Props.Fields[], -1 for RowID/ID, -2 if unknown
    // - use InternalFieldNameToFieldExternalIndex() to convert from column name
    fFieldsExternalToInternal: TIntegerDynArray;
    /// gives the index of each in Props.Fields[]+1 in fFieldsExternal[]
    // - expects [0] of RowID/ID, [1..length(fFieldNames)] for others
    fFieldsInternalToExternal: TIntegerDynArray;
    // multi-thread BATCH process is secured via Lock/UnLock critical section
    fBatchMethod: TSQLURIMethod;
    fBatchCapacity, fBatchCount, fBatchAddedID: integer;
    // BATCH sending uses TEXT storage for direct sending to database driver
    fBatchValues: TRawUTF8DynArray;
    fBatchIDs: TIntegerDynArray;
    /// get fFieldsExternal[] index using fFieldsExternalToInternal[] mapping
    // - do handle ID/RowID fields and published methods
    function InternalFieldNameToFieldExternalIndex(const InternalFieldName: RawUTF8): integer;
    /// create, prepare and bound inlined parameters to a thread-safe statement
    // - this implementation will call the ThreadSafeConnection virtual method,
    // then bound inlined parameters as :(1234): and call its Execute method
    // - should return nil on error, and not raise an exception
    function PrepareInlinedForRows(const aSQL: RawUTF8): ISQLDBStatement;
    /// overloaded method using FormatUTF8() and binding SynDB parameters
    function PrepareDirectForRows(SQLFormat: PUTF8Char; const Args, Params: array of const): ISQLDBStatement;
    /// create, prepare, bound inlined parameters and execute a thread-safe statement
    // - this implementation will call the ThreadSafeConnection virtual method,
    // then bound inlined parameters as :(1234): and call its Execute method
    // - should return nil on error, and not raise an exception
    function ExecuteInlined(const aSQL: RawUTF8; ExpectResults: Boolean): ISQLDBRows; overload;
    /// overloaded method using FormatUTF8() and inlined parameters
    function ExecuteInlined(SQLFormat: PUTF8Char; const Args: array of const; ExpectResults: Boolean): ISQLDBRows; overload;
    /// overloaded method using FormatUTF8() and binding SynDB parameters
    function ExecuteDirect(SQLFormat: PUTF8Char; const Args, Params: array of const;
      ExpectResults: Boolean): ISQLDBRows;
    /// overloaded method using FormatUTF8() and binding SynDB parameters
    function ExecuteDirectSQLVar(SQLFormat: PUTF8Char; const Args: array of const;
       var Params: TSQLVarDynArray; LastIntegerParam: integer; ParamsMatchCopiableFields: boolean): boolean;
    // overridden methods calling the external engine with SQL via Execute
    function EngineRetrieve(TableModelIndex, ID: integer): RawUTF8; override;
    function EngineExecute(const aSQL: RawUTF8): boolean; override;
    function EngineLockedNextID: Integer; virtual;
    function EngineAdd(TableModelIndex: integer; const SentData: RawUTF8): integer; override;
    function EngineUpdate(TableModelIndex, ID: integer; const SentData: RawUTF8): boolean; override;
    function EngineDeleteWhere(TableModelIndex: integer; const SQLWhere: RawUTF8;
      const IDs: TIntegerDynArray): boolean; override;
    function EngineList(const SQL: RawUTF8; ForceAJAX: Boolean=false; ReturnedRowCount: PPtrInt=nil): RawUTF8; override;
    // BLOBs should be access directly, not through slower JSON Base64 encoding
    function EngineRetrieveBlob(TableModelIndex, aID: integer;
      BlobField: PPropInfo; out BlobData: TSQLRawBlob): boolean; override;
    function EngineUpdateBlob(TableModelIndex, aID: integer;
      BlobField: PPropInfo; const BlobData: TSQLRawBlob): boolean; override;
    function EngineSearchField(const FieldName: ShortString;
      const FieldValue: array of const; var ResultID: TIntegerDynArray): boolean;
    // overridden method returning TRUE for next calls to EngineAdd/Update/Delete 
    // will properly handle operations until InternalBatchStop is called
    function InternalBatchStart(Method: TSQLURIMethod): boolean; override;
    // internal method called by TSQLRestServer.RunBatch() to process fast sending
    // to remote database engine (e.g. Oracle bound arrays or MS SQL Bulk insert)
    procedure InternalBatchStop; override;
    /// called internally by EngineAdd/EngineUpdate/EngineDelete in batch mode
    function InternalBatchAdd(const aValue: RawUTF8; aID: integer): integer;
    /// TSQLRestServer.URI use it for Static.EngineList to by-pass virtual table
    // - overridden method to handle most potential simple queries, e.g. like
    // $ SELECT Field1,RowID FROM table WHERE RowID=... AND/OR/NOT Field2=
    // - change 'RowID' into 'ID' column name, internal field names into
    // mapped external field names ('AS [InternalFieldName]' if needed), and
    // SQLTableName into fTableName
    // - any 'LIMIT #' clause will be changed into the appropriate SQL statement
    // - handle also statements to avoid slow virtual table full scan, e.g. 
    // $ SELECT count(*) FROM table
    function AdaptSQLForEngineList(var SQL: RawUTF8): boolean; override;
    /// run INSERT of UPDATE from the corresponding JSON object
    // - Occasion parameter shall be only either soInsert or soUpate
    // - each JSON field will be bound with the proper SQL type corresponding to
    // the real external table columns (e.g. as TEXT for variant)
    // - returns 0 on error, or the Updated/Inserted ID 
    function ExecuteFromJSON(const SentData: RawUTF8; Occasion: TSQLOccasion;
      UpdatedID: integer): integer;
    /// compute the INSERT or UPDATE statement as decoded from a JSON object
    function JSONDecodedPrepareToSQL(var Decoder: TJSONObjectDecoder;
      out ExternalFields: TRawUTF8DynArray; out Types: TSQLDBFieldTypeArray;
      Occasion: TSQLOccasion): RawUTF8;     
  public
    /// initialize the remote database connection
    // - you should not use this, but rather call VirtualTableExternalRegister() 
    // - RecordProps.ExternalDatabase will map the associated TSQLDBConnectionProperties
    // - RecordProps.ExternalTableName will retrieve the real full table name,
    // e.g. including any database schema prefix
    constructor Create(aClass: TSQLRecordClass; aServer: TSQLRestServer); override;
    /// delete a row, calling the external engine with SQL
    // - made public since a TSQLRestStorage instance may be created
    // stand-alone, i.e. without any associated Model/TSQLRestServer
    function EngineDelete(TableModelIndex, ID: integer): boolean; override;
    /// search for a numerical field value
    // - return true on success (i.e. if some values have been added to ResultID)
    // - store the results into the ResultID dynamic array
    function SearchField(const FieldName: RawUTF8; FieldValue: Integer;
      var ResultID: TIntegerDynArray): boolean; overload; override;
    /// search for a field value, according to its SQL content representation
    // - return true on success (i.e. if some values have been added to ResultID)
    // - store the results into the ResultID dynamic array
    function SearchField(const FieldName, FieldValue: RawUTF8;
      var ResultID: TIntegerDynArray): boolean; overload; override;
    /// overridden method for direct external database engine call
    function TableRowCount(Table: TSQLRecordClass): integer; override;
    /// overridden method for direct external database engine call
    function TableHasRows(Table: TSQLRecordClass): boolean; override;
     {{ begin a transaction (implements REST BEGIN Member)
     - to be used to speed up some SQL statements like Insert/Update/Delete
     - must be ended with Commit on success
     - must be aborted with Rollback if any SQL statement failed
     - return true if no transaction is active, false otherwise }
    function TransactionBegin(aTable: TSQLRecordClass; SessionID: cardinal=1): boolean; override;
    {{ end a transaction (implements REST END Member)
     - write all pending SQL statements to the external database }
    procedure Commit(SessionID: cardinal=1); override;
    {{ abort a transaction (implements REST ABORT Member)
     - restore the previous state of the database, before the call to TransactionBegin }
    procedure RollBack(SessionID: cardinal=1); override;
     /// overridden method for direct external database engine call
    function UpdateBlobFields(Value: TSQLRecord): boolean; override;
     /// overridden method for direct external database engine call
    function RetrieveBlobFields(Value: TSQLRecord): boolean; override;
    /// update a field value of the external database
    function EngineUpdateField(TableModelIndex: integer;
      const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUTF8): boolean; override;
    /// create one index for all specific FieldNames at once
    // - this method will in fact call the SQLAddIndex method, if the index
    // is not already existing
    // - for databases which do not support indexes on BLOB fields (i.e. all
    // engine but SQLite3), such FieldNames will be ignored
    function CreateSQLMultiIndex(Table: TSQLRecordClass; const FieldNames: array of RawUTF8;
      Unique: boolean; IndexName: RawUTF8=''): boolean; override;
    /// this method is called by TSQLRestServer.EndCurrentThread method just
    // before a thread is finished to ensure that the associated external DB
    // connection will be released for this thread
    // - this overridden implementation will clean thread-specific connections,
    // i.e. call TSQLDBConnectionPropertiesThreadSafe.EndCurrentThread method
    // - this method shall be called directly, nor from the main thread
    procedure EndCurrentThread(Sender: TThread); override;
    /// reset the internal cache of external table maximum ID
    // - next EngineAdd/BatchAdd will execute SELECT max(ID) FROM externaltable
    procedure ResetMaxIDCache;

    /// retrieve the REST server instance corresponding to an external TSQLRecord
    // - just map aServer.StaticVirtualTable[] and will return nil if not
    // a TSQLRestStorageExternal
    // - you can use it e.g. to call MapField() method in a fluent interface
    class function Instance(aClass: TSQLRecordClass;
      aServer: TSQLRestServer): TSQLRestStorageExternal;
    /// retrieve the external database connection associated to a TSQLRecord
    // - just map aServer.StaticVirtualTable[] and will return nil if not
    // a TSQLRestStorageExternal
    class function ConnectionProperties(aClass: TSQLRecordClass;
      aServer: TSQLRestServer): TSQLDBConnectionProperties; overload;
    /// the associated external database connection
    function ConnectionProperties: TSQLDBConnectionProperties; overload;
    /// by default, any INSERT will compute the new ID from an internal variable
    // - it is very fast and reliable, unless external IDs can be created
    // outside this engine
    // - you can set EngineAddUseSelectMaxID=true to execute a slower
    // 'select max(ID) from TableName' SQL statement
    property EngineAddUseSelectMaxID: Boolean read fEngineUseSelectMaxID
      write fEngineUseSelectMaxID;
  end;

  {{ A Virtual Table cursor for reading a TSQLDBStatement content
    - this is the cursor class associated to TSQLVirtualTableExternal }
  TSQLVirtualTableCursorExternal = class(TSQLVirtualTableCursor)
  protected
    fStatement: ISQLDBStatement;
    fSQL: RawUTF8;
    fHasData: boolean;
  public
    /// called to begin a search in the virtual table, creating a SQL query
    // - the TSQLVirtualTablePrepared parameters were set by
    // TSQLVirtualTable.Prepare and will contain both WHERE and ORDER BY statements
    // (retrieved by x_BestIndex from a TSQLite3IndexInfo structure)
    // - Prepared will contain all prepared constraints and the corresponding
    // expressions in the Where[].Value field
    // - will move cursor to first row of matching data
    // - will return false on low-level database error (but true in case of a
    // valid call, even if HasData will return false, i.e. no data match)
    // - all WHERE and ORDER BY clauses are able to be translated into a plain 
    // SQL statement calling the external DB engine
    // - will create the internal fStatement from a SQL query, bind the
    // parameters, then execute it, ready to be accessed via HasData/Next
    function Search(const Prepared: TSQLVirtualTablePrepared): boolean; override;
    /// called to retrieve a column value of the current data row
    // - if aColumn=VIRTUAL_TABLE_ROWID_COLUMN(-1), will return the row ID
    // as varInt64 into aResult
    // - will return false in case of an error, true on success
    function Column(aColumn: integer; var aResult: TSQLVar): boolean; override;
    /// called after Search() to check if there is data to be retrieved
    // - should return false if reached the end of matching data
    function HasData: boolean; override;
    /// called to go to the next row of matching data
    // - should return false on low-level database error (but true in case of a
    // valid call, even if HasData will return false, i.e. no data match)
    function Next: boolean; override;
    /// read-only access to the SELECT statement
    property SQL: RawUTF8 read fSQL;
  end;
  
  {{ A SynDB-based virtual table for accessing any external database
   - for ORM access, you should use VirtualTableExternalRegister method to
     associated this virtual table module to any TSQLRecord class
   - transactions are handled by this module, according to the external database }
  TSQLVirtualTableExternal = class(TSQLVirtualTable)
  public { overridden methods }
    /// returns the main specifications of the associated TSQLVirtualTableModule
    // - this is a read/write table, without transaction (yet), associated to the
    // TSQLVirtualTableCursorExternal cursor type, with 'External' as module name
    // and TSQLRestStorageExternal as the related static class
    // - no particular class is supplied here, since it will depend on the
    // associated Static TSQLRestStorageExternal instance
    class procedure GetTableModuleProperties(var aProperties: TVirtualTableModuleProperties);
      override;
    /// called to determine the best way to access the virtual table
    // - will prepare the request for TSQLVirtualTableCursor.Search()
    // - this overridden method will let the external DB engine perform the search,
    // using a standard SQL "SELECT * FROM .. WHERE .. ORDER BY .." statement
    // - in Where[], Expr must be set to not 0 if needed for Search method,
    // and OmitCheck always set to true since double check is not necessary
    // - OmitOrderBy will be set to true since double sort is not necessary
    // - EstimatedCost will receive the estimated cost, with lowest value if
    // fStatic.fFieldsExternal[].ColumnIndexed is set (i.e. if column has an index)
    function Prepare(var Prepared: TSQLVirtualTablePrepared): boolean; override;
    /// called when a DROP TABLE statement is executed against the virtual table
    // - returns true on success, false otherwise
    function Drop: boolean; override;
    /// called to delete a virtual table row
    // - returns true on success, false otherwise
    function Delete(aRowID: Int64): boolean; override;
    /// called to insert a virtual table row content
    // - column order follows the Structure method, i.e. StoredClassProps.Fields[] order
    // - returns true on success, false otherwise
    // - returns the just created row ID in insertedRowID on success
    function Insert(aRowID: Int64; var Values: TSQLVarDynArray;
      out insertedRowID: Int64): boolean; override;
    /// called to update a virtual table row content
    // - column order follows the Structure method, i.e. StoredClassProps.Fields[] order
    // - returns true on success, false otherwise
    function Update(oldRowID, newRowID: Int64; var Values: TSQLVarDynArray): boolean; override;
  end;


/// register on the Server-side an external database for an ORM class
// - will associate the supplied class with a TSQLVirtualTableExternal module
// (calling aModel.VirtualTableRegister method), even if the class does not
// inherit from TSQLRecordVirtualTableAutoID (it can be any plain TSQLRecord or
// TSQLRecordMany sub-class for instance)
// - note that TSQLModel.Create() will reset all supplied classes to be defined
// as non virtual (i.e. Kind=rSQLite3)
// - this function shall be called BEFORE TSQLRestServer.Create (the server-side
// ORM must know if the database is to be managed as internal or external)
// - this function (and the whole unit) is NOT to be used on the client-side
// - the TSQLDBConnectionProperties instance should be shared by all classes,
// and released globaly when the ORM is no longer needed
// - the full table name, as expected by the external database, could be
// provided here (SQLTableName will be used internaly as table name when
// called via the associated SQLite3 Virtual Table) - if no table name is
// specified (''), will use SQLTableName (e.g. 'Customer' for 'TSQLCustomer')
// - typical usage is therefore for instance:
// !  Props := TOleDBMSSQLConnectionProperties.Create('.\SQLEXPRESS','AdventureWorks2008R2','','');
// !  Model := TSQLModel.Create([TSQLCustomer],'root');
// !  VirtualTableExternalRegister(Model,TSQLCustomer,Props,'Sales.Customer');
// !  Server := TSQLRestServerDB.Create(aModel,'application.db'),true)
// - the supplied aExternalDB parameter is stored within aClass.RecordProps, so
// the instance must stay alive until all database access to this external table
// is finished (e.g. use a private/protected property)
// - server-side may omit a call to VirtualTableExternalRegister() if the need of
// an internal database is expected: it will allow custom database configuration
// at runtime, depending on the customer's expectations (or license)
// - after registration, you can tune the field-name mapping by calling
// ! aModel.Props[aClass].ExternalDB.MapField(..)
function VirtualTableExternalRegister(aModel: TSQLModel; aClass: TSQLRecordClass;
  aExternalDB: TSQLDBConnectionProperties; const aExternalTableName: RawUTF8=''): boolean;

/// register all tables of the model to be external
// - by default, all tables are handled by the SQLite3 engine, unless they
// are explicitely declared as external via VirtualTableExternalRegister: this
// function can be used to register all tables to be handled by an external DBs
// - this function shall be called BEFORE TSQLRestServer.Create (the server-side
// ORM must know if the database is to be managed as internal or external)
// - this function (and the whole unit) is NOT to be used on the client-side
// - the TSQLDBConnectionProperties instance should be shared by all classes,
// and released globaly when the ORM is no longer needed
// - by default, TSQLAuthUser and TSQLAuthGroup tables will be handled via the
// external DB, but you can avoid it for speed when handling session and security
// - after registration, you can tune the field-name mapping by calling
// ! aModel.Props[aClass].ExternalDB.MapField(..)
function VirtualTableExternalRegisterAll(aModel: TSQLModel;
  aExternalDB: TSQLDBConnectionProperties; DoNotCacheUserGroupTables: boolean=false): boolean;


implementation

function VirtualTableExternalRegister(aModel: TSQLModel; aClass: TSQLRecordClass;
  aExternalDB: TSQLDBConnectionProperties; const aExternalTableName: RawUTF8): boolean;
var ExternalTableName: RawUTF8;
    Props: TSQLModelRecordProperties;
begin
  result := False;
  if (aModel=nil) or (aClass=nil) or (aExternalDB=nil) then
    exit; // avoid GPF
  Props := aModel.Props[aClass];
  if Props=nil then
    exit; // if aClass is not part of the model
  Props.Kind := rCustomAutoID; // force creation use of SQLite3 virtual table
  if aExternalTableName='' then
    ExternalTableName := Props.Props.SQLTableName else
    ExternalTableName := aExternalTableName;
  result := aModel.VirtualTableRegister(aClass,TSQLVirtualTableExternal,
    aExternalDB.SQLFullTableName(ExternalTableName),aExternalDB);
end;

function VirtualTableExternalRegisterAll(aModel: TSQLModel;
  aExternalDB: TSQLDBConnectionProperties; DoNotCacheUserGroupTables: boolean=false): boolean;
var i: integer;
begin
  if (aModel=nil) or (aExternalDB=nil) then begin
    result := false;
    exit; // avoid GPF
  end;
  result := true;
  for i := 0 to high(aModel.Tables) do
    if DoNotCacheUserGroupTables and ((aModel.Tables[i]=TSQLAuthGroup)
       or (aModel.Tables[i]=TSQLAuthUser)) then
      continue else
    if not VirtualTableExternalRegister(aModel,aModel.Tables[i],aExternalDB,'') then
      result := false;
end;


{ TSQLRestStorageExternal }

procedure TSQLRestStorageExternal.Commit(SessionID: cardinal);
begin
  inherited Commit(SessionID); // reset fTransactionActive + write all TSQLVirtualTableJSON
  try
    fProperties.ThreadSafeConnection.Commit;
  except
    on Exception do
      ; // just catch exception
  end;
end;

constructor TSQLRestStorageExternal.Create(aClass: TSQLRecordClass;
  aServer: TSQLRestServer);

  procedure FieldsInternalInit;
  var i,n,int: integer;
  begin
    n := length(fFieldsExternal);
    SetLength(fFieldsExternalToInternal,n);
    with StoredClassProps.ExternalDB do begin
      SetLength(fFieldsInternalToExternal,length(FieldNames)+1);
      for i := 0 to high(fFieldsInternalToExternal) do
        fFieldsInternalToExternal[i] := -1;
      for i := 0 to n-1 do begin
        int := ExternalToInternalIndex(fFieldsExternal[i].ColumnName);
        fFieldsExternalToInternal[i] := int;
        inc(int); // fFieldsInternalToExternal[0]=RowID, then follows fFieldsExternal[]
        if int>=0 then
          fFieldsInternalToExternal[int] := i;
      end;
    end;
  end;
  function FieldsExternalIndexOf(const ColName: RawUTF8): integer;
  begin
    for result := 0 to high(fFieldsExternal) do
      if IdemPropNameU(fFieldsExternal[result].ColumnName,ColName) then
        exit;
    result := -1;
  end;
  function PropInfoToExternalField(Prop: TSQLPropInfo;
    var Column: TSQLDBColumnProperty): boolean;
  const
    mORMotType: array[TSQLFieldType] of TSQLDBFieldType =
      (ftUnknown,   // sftUnknown
       ftUTF8,      // sftAnsiText
       ftUTF8,      // sftUTF8Text
       ftInt64,     // sftEnumerate
       ftInt64,     // sftSet
       ftInt64,     // sftInteger
       ftInt64,     // sftID
       ftInt64,     // sftRecord
       ftInt64,     // sftBoolean
       ftDouble,    // sftFloat
       ftDate,      // sftDateTime
       ftInt64,     // sftTimeLog
       ftCurrency,  // sftCurrency
       ftUTF8,      // sftObject
  {$ifndef NOVARIANTS}
       ftUTF8,      // sftVariant
  {$endif}
       ftBlob,      // sftBlob
       ftBlob,      // sftBlobDynArray
       ftBlob,      // sftBlobCustom
       ftUTF8,      // sftUTF8Comp
       ftUnknown,   // sftMany
       ftInt64,     // sftModTime
       ftInt64);    // sftCreateTime
  begin
    result := false;
    Column.ColumnType := mORMotType[Prop.SQLFieldType];
    if Column.ColumnType=ftUnknown then
      exit; // ignore unkwnown fields
    Column.ColumnName := StoredClassProps.ExternalDB.FieldNames[Prop.PropertyIndex];
    Column.ColumnAttr := Prop.FieldWidth;
    Column.ColumnUnique := aIsUnique in Prop.Attributes;
    result := true;
  end;

var SQL: RawUTF8;
    i,f: integer;
    Field: TSQLDBColumnProperty;
    FieldAdded: Boolean;
    CreateColumns: TSQLDBColumnPropertyDynArray;
begin
  inherited Create(aClass,aServer);
  // initialize external DB properties
  if fStoredClassProps=nil then
    raise EBusinessLayerException.CreateUTF8(
      '%.Create: StoredClassProps needed for %',[self,StoredClass]);
  fTableName := StoredClassProps.ExternalDB.TableName;
  fProperties := StoredClassProps.ExternalDB.ConnectionProperties as TSQLDBConnectionProperties;
  if fProperties=nil then
    raise EBusinessLayerException.CreateUTF8(
      '%.Create: No external DB defined for %',[self,StoredClass]);
  if Owner<>nil then
    try
      Owner.ServerTimeStamp := fProperties.ThreadSafeConnection.ServerTimeStamp;
    except
      on E: Exception do ; // ignore any error here
    end;
  // create corresponding external table if necessary, and retrieve its fields info
  fProperties.GetFields(fTableName,fFieldsExternal);
  if fFieldsExternal=nil then begin
    // table is not yet existing -> try to create it
    with aClass.RecordProps do begin
      SetLength(CreateColumns,Fields.Count+1);
      CreateColumns[0].ColumnName := StoredClassProps.ExternalDB.RowIDFieldName;
      CreateColumns[0].ColumnType := ftUnknown;
      CreateColumns[0].ColumnUnique := true;
      f := 1;
      for i := 0 to Fields.Count-1 do
        if PropInfoToExternalField(Fields.List[i],CreateColumns[f]) then
          inc(f);
      if f<>Length(CreateColumns) then
        SetLength(CreateColumns,f); // just ignore non handled field types
    end;
    SQL := fProperties.SQLCreate(fTableName,CreateColumns,false);
    if SQL<>'' then
      if ExecuteDirect(pointer(SQL),[],[],false)<>nil then begin
        fProperties.GetFields(fTableName,fFieldsExternal); // fields from DB after create
        if fFieldsExternal=nil then
          raise EORMException.CreateUTF8('%.Create: external table creation % failed:'+
            ' GetFields() returned nil - SQL="%"',[self,StoredClass,fTableName,SQL]);
      end;
  end;
  FieldsInternalInit;
  // create any missing field if necessary
  FieldAdded := false;
  with StoredClassRecordProps do
  for f := 0 to Fields.Count-1 do
    if Fields.List[f].SQLFieldType in COPIABLE_FIELDS then // ignore sftMany 
    /// real database columns exist for Simple + Blob fields (not Many)
    if FieldsExternalIndexOf(fStoredClassProps.ExternalDB.FieldNames[f])<0 then begin
      // add new missing Field
      Finalize(Field);
      fillchar(Field,sizeof(Field),0);
      if PropInfoToExternalField(Fields.List[f],Field) then begin
        SQL := fProperties.SQLAddColumn(fTableName,Field);
        if (SQL<>'') and (ExecuteDirect(pointer(SQL),[],[],false)<>nil) then
          FieldAdded := true else
          raise EORMException.CreateUTF8('%.Create: %: unable to create external '+
            'missing field %.% - SQL="%"',
            [self,StoredClass,fTableName,Fields.List[f].Name,SQL]);
      end;
    end;
  if FieldAdded then begin
    fProperties.GetFields(fTableName,fFieldsExternal); // get from DB after ALTER TABLE
    FieldsInternalInit;
  end;
  // compute the SQL statements used internaly for external DB requests
  with StoredClassProps.ExternalDB do begin
    fSelectOneDirectSQL := FormatUTF8('select % from % where %=?',
      [SQL.TableSimpleFields[true,false],fTableName,RowIDFieldName]);
    fSelectAllDirectSQL := FormatUTF8('select %,% from %',
      [SQL.InsertSet,RowIDFieldName,fTableName]);
    fRetrieveBlobFieldsSQL := InternalCSVToExternalCSV(
      StoredClassRecordProps.SQLTableRetrieveBlobFields);
    fUpdateBlobFieldsSQL := InternalCSVToExternalCSV(
      StoredClassRecordProps.SQLTableUpdateBlobFields,'=?,','=?');
  end;
  fSelectTableHasRowsSQL := FormatUTF8('select ID from % limit 1',
    [StoredClassRecordProps.SQLTableName]);
  AdaptSQLForEngineList(fSelectTableHasRowsSQL);
end;

function TSQLRestStorageExternal.AdaptSQLForEngineList(var SQL: RawUTF8): boolean;
var Prop: ShortString; // to avoid any temporary memory allocation
    P: PUTF8Char;
    W: TTextWriter;

  function PropHandleField(WithAliasIfNeeded: boolean): boolean;
  var int: integer;
  begin
    result := true;
    if IsRowIDShort(Prop) then
    with StoredClassProps.ExternalDB do begin
      W.AddString(RowIDFieldName);
      if WithAliasIfNeeded and not(0 in FieldNamesMatchInternal) then
        W.AddShort(' as ID');
      exit;
    end;
    Prop[ord(Prop[0])+1] := #0; // make ASCIIZ
    int := StoredClassRecordProps.Fields.IndexByName(@Prop[1]);
    if int<0 then
      result := false else
      with StoredClassProps.ExternalDB do begin
        W.AddString(FieldNames[int]);
        if WithAliasIfNeeded and not(int+1 in FieldNamesMatchInternal) then
          W.AddStrings([' as ',StoredClassRecordProps.Fields.List[int].Name]);
      end;
  end;
  procedure GetFieldProp;
  var i,L: integer;
      B: PUTF8Char;
  begin
    Prop[0] := #0;
    if P^=#0 then
      exit;
    P := GotoNextNotSpace(P); // trim left
    B := P;
    while ord(P^) in IsIdentifier do inc(P); // go to end of field name
    L := P-B;
    if L>250 then
      exit; // avoid potential buffer overflow
    Prop[0] := AnsiChar(L);
    for i := 0 to L-1 do
      Prop[i+1] := NormToUpperAnsi7[B[i]];
    P := GotoNextNotSpace(P); // trim right
  end;
  procedure WritePropAndGetFieldProp;
  var i: integer;
  begin
    if W.LastChar<>' ' then
      W.Add(' ');
    for i := 1 to length(Prop) do
      Prop[i] := NormToLower[Prop[i]];
    W.AddShort(Prop);
    W.Add(' ');
    GetFieldProp;
  end;
  function NextPropHandleField: boolean;
  begin
    GetFieldProp;
    result := PropHandleField(false);
  end;
  function NextPropHandleInternalTable: boolean;
  begin
    GetFieldProp;
    with StoredClassRecordProps do
    if IdemPropName(Prop,pointer(SQLTableName),length(SQLTableName)) then begin
      W.AddString(fTableName);
      result := true;
    end else
      result := false;
  end;

label Order,Limit,null;
var Pos: record AfterSelect, WhereClause, Limit, LimitRowCount: integer; end;
    B: PUTF8Char;
    err: integer;
    NewSQL: RawUTF8;
begin
  //result := inherited AdaptSQLForEngineList(SQL); broken if MapField() used
  result := false;
  if SQL='' then
    exit;
  // e.g. 'SELECT Field1,Field2 FROM table WHERE Field1=... AND/OR/NOT Field2=..
  fillchar(Pos,sizeof(Pos),0);
  P := pointer(SQL);
  GetFieldProp;
  if Prop<>'SELECT' then
    exit;
  W := TTextWriter.CreateOwnedStream(length(SQL)*2);
  try
    W.AddShort('select ');
    Pos.AfterSelect := W.TextLength+1;
    repeat
      GetFieldProp;
      if Prop='' then exit;
      if (Prop='COUNT') and IdemPChar(P,'(*)') then begin
        inc(P,3);
        GetFieldProp;
        if Prop<>'FROM' then
          exit;
        W.AddShort('count(*)');
        if P^ in [#0,';'] then begin
          result := NextPropHandleInternalTable;
          exit;
        end;
        break; // will process 'select count(*) from tablename where ...'
      end else
      if not PropHandleField(true) then
        exit; // unknown field name
      if P^=',' then begin
        W.Add(',');
        inc(P);
      end else begin
        GetFieldProp;
        if Prop<>'FROM' then
          exit else
          break;
      end;
    until false;
    W.AddShort(' from ');
    if not NextPropHandleInternalTable then
      exit;
    GetFieldProp;
    if Prop='ORDER' then begin // simple ORDER BY clause is accepted
      Pos.WhereClause := -W.TextLength-2; // WhereClausePos<0 for ORDER BY position
Order:GetFieldProp;
      if Prop<>'BY' then
        exit;
      if W.LastChar<>' ' then
        W.Add(' ');
      W.AddShort('order by ');
      if not NextPropHandleField then
        exit; // unknown field name in 'ORDER BY' clause
      GetFieldProp;
      if Prop='LIMIT' then begin
Limit:  Pos.Limit := W.TextLength+1;
        GetFieldProp; // do not write LIMIT now
        if Prop='' then
          exit; // expect e.g. LIMIT 100
        Prop[ord(Prop[0])+1] := #0;
        Pos.LimitRowCount := GetInteger(@Prop[1],err);
        if err<>0 then
          exit; // expects a number for LIMIT
      end else
        if Prop<>'' then
          exit; // unexpected clause 
      if not (GotoNextNotSpace(P)^ in [#0,';']) then
        exit; // allow only one column name or one LIMIT ### expression
    end else
    if Prop='WHERE' then
    repeat
      WritePropAndGetFieldProp; // write as 'where' 'and' 'or'
      Pos.WhereClause := W.TextLength+1;
      if Prop='NOT' then
        WritePropAndGetFieldProp; // allow  field1=456 AND NOT field2='Toto'
      if (Prop='') or not PropHandleField(false) then
        exit; // unknown field name or 'LIMIT' / 'ORDER BY' clause
      B := P;
      if P^='=' then
        inc(P) else
      if P^ in ['>','<'] then
        if P[1] in ['=','>'] then
          inc(P,2) else
          inc(P) else
      if IdemPChar(P,'LIKE ') then begin
        GetFieldProp;
        W.AddShort(' like ');
        B := nil;
      end else
      if IdemPChar(P,'IS NULL') then begin
        inc(P,7);
        W.Add(' ');
        goto null;
      end else
      if IdemPChar(P,'IS NOT NULL') then begin
        inc(P,11);
        W.Add(' ');
        goto null;
      end else
        exit; // only "= > >= < <= <> LIKE" or "IS [NOT] NULL"
      if B<>nil then
        W.AddNoJSONEscape(B,P-B);
      P := GotoNextNotSpace(P);
      B := P;
      if PWord(P)^=ord(':')+ord('(') shl 8 then
        P := GotoNextNotSpace(P+2); // +2 to ignore :(...): parameter
      if P^ in ['''','"'] then
        P := GotoEndOfQuotedString(P);
      repeat inc(P) until P^ in [#0..' ',';',')']; // go to end of value
      P := GotoNextNotSpace(P);
      if PWord(P)^=ord(')')+ord(':')shl 8 then
        inc(P,2); // ignore :(...): parameter
null: P := GotoNextNotSpace(P);
      W.AddNoJSONEscape(B,P-B);
      if P^ in [#0,';'] then
        break; // properly ended the WHERE clause
      GetFieldProp;
      if Prop='ORDER' then
        goto Order else
      if Prop='LIMIT' then
        goto Limit else
      if (Prop<>'AND') and (Prop<>'OR') then
        exit;
    until false else
    if Prop='LIMIT' then
      goto Limit else
    if Prop<>'' then
      exit;
    if W.LastChar=' ' then
      W.CancelLastChar;
    W.SetText(NewSQL);
    NewSQL := trim(NewSQL);
  finally
    W.Free;
  end;
  if Pos.Limit>0 then
    if not fProperties.AdaptSQLLimitForEngineList(NewSQL,
       Pos.LimitRowCount,Pos.AfterSelect,Pos.WhereClause,Pos.Limit) then
      exit;
  SQL := Trim(NewSQL);
  result := true;
end;

function TSQLRestStorageExternal.EngineLockedNextID: Integer;
// fProperties.SQLCreate: ID Int64 PRIMARY KEY -> compute unique RowID
// (not all DB engines handle autoincrement feature - e.g. Oracle does not)
var Rows: ISQLDBRows;
begin
  if (fEngineLockedLastID=0) or EngineAddUseSelectMaxID then begin
    // first method call -> retrieve value from DB
    Rows := ExecuteDirect('select max(%) from %',
      [StoredClassProps.ExternalDB.RowIDFieldName,fTableName],[],true);
    if (Rows<>nil) and Rows.Step then
      fEngineLockedLastID := Rows.ColumnInt(0) else
      fEngineLockedLastID := 0;
  end;
  inc(fEngineLockedLastID);
  result := fEngineLockedLastID;
end;

function TSQLRestStorageExternal.InternalBatchStart(
  Method: TSQLURIMethod): boolean;
const BATCH: array[mPOST..mDELETE] of TSQLDBStatementCRUD = (
  cCreate, cUpdate, cDelete);
begin
  result := false; // means BATCH mode not supported
  if (self<>nil) and (method in [mPOST..mDELETE]) and
     (BATCH[method] in fProperties.BatchSendingAbilities) then begin
    StorageLock(true); // protected by try..finally in TSQLRestServer.RunBatch
    try
      if fBatchMethod<>mNone then
        raise EORMException.CreateUTF8('Missing previous %.InternalBatchStop(%)',
          [self,StoredClass]);
      if Method=mPOST then
        fBatchAddedID := EngineLockedNextID else
        fBatchAddedID := 0;
      fBatchMethod := Method;
      fBatchCount := 0;
      result := true; // means BATCH mode is supported
    finally
      if not result then
        StorageUnLock;
    end;
  end;
end;

procedure TSQLRestStorageExternal.InternalBatchStop;
var i,j,n,max,BatchBegin,BatchEnd,ValuesMax: integer;
    Query: ISQLDBStatement;
    NotifySQLEvent: TSQLEvent;
    SQL,privateCopy: RawUTF8;
    P: PUTF8Char;
    Fields, ExternalFields: TRawUTF8DynArray;
    Types: TSQLDBFieldTypeArray;
    Values: TRawUTF8DynArrayDynArray;
    Occasion: TSQLOccasion;
    Decode: TJSONObjectDecoder;
begin
  if fBatchMethod=mNone then
    raise EORMException.CreateUTF8('%.InternalBatchStop(%).BatchMethod=mNone',
      [self,StoredClass]);
  try
    if fBatchCount=0 then
      exit; // nothing to do
    if (Owner<>nil) and (fBatchMethod=mDelete) then // notify BEFORE deletion
      for i := 0 to fBatchCount-1 do
        Owner.InternalUpdateEvent(seDelete,fStoredClassProps.TableIndex,fBatchIDs[i],'',nil);
    with fProperties do
      if BatchMaxSentAtOnce>0 then
        max := BatchMaxSentAtOnce else
        max := 1000;
    BatchBegin := 0;
    BatchEnd := fBatchCount-1;
    repeat
      case fBatchMethod of
      mPost, mPut: begin
        assert(fBatchIDs<>nil);
        BatchEnd := fBatchCount-1;
        for i := BatchBegin to BatchEnd do begin
          privateCopy := fBatchValues[i];
          P := @privateCopy[1]; // make copy before in-place decoding
          while P^ in [#1..' ','{','['] do inc(P);
          if fBatchMethod=mPost then
            Occasion := soInsert else
            Occasion := soUpdate;
          case Occasion of
          soInsert: // mPost=INSERT with the supplied fields and computed ID
            Decode.Decode(P,nil,pQuoted,fBatchIDs[i],true);
          soUpdate: // mPut=UPDATE with the supplied fields and ID set appart
            Decode.Decode(P,nil,pQuoted,0,true);
          end;
          if Fields=nil then begin
            Decode.AssignFieldNamesTo(Fields);
            SQL := JSONDecodedPrepareToSQL(Decode,ExternalFields,Types,Occasion);
            SetLength(Values,Decode.FieldCount);
            ValuesMax := fBatchCount-BatchBegin;
            if ValuesMax>max then
              ValuesMax := max;
            for j := 0 to Decode.FieldCount-1 do
              SetLength(Values[j],ValuesMax);
          end else
            if not Decode.SameFieldNames(Fields) then
              break; // this item would break the SQL statement
          n := i-BatchBegin;
          for j := 0 to high(Fields) do
            Values[j,n] := Decode.FieldValues[j]; // regroup by parameter
          if Occasion=soUpdate then // ?=ID parameter
            Values[length(Fields),n] := Int32ToUtf8(fBatchIDs[i]);
          BatchEnd := i; // mark fBatchValues[i] has to be copied in Values[]
          if n+1>=max then
            break; // do not send too much items at once, for better speed
        end;
      end;
      mDelete: begin
        SQL := FormatUTF8('delete from % where %=?',
          [fTableName,fStoredClassProps.ExternalDB.RowIDFieldName]);
        n := BatchEnd-BatchBegin+1;
        if n+1>=max then begin
          n := max; // do not send too much items at once, for better speed
          BatchEnd := BatchBegin+max-1;
        end;
        SetLength(Values,1);
        SetLength(Values[0],n);
        for i := 0 to n-1 do
          Values[0,i] := Int32ToUTF8(fBatchIDs[BatchBegin+i]);
      end;
      end;
      n := BatchEnd-BatchBegin+1;
      if n<=0 then
        break;
      if (fBatchMethod=mPost) and Assigned(fProperties.OnBatchInsert) then
        // use multiple insert dedicated function if available
        fProperties.OnBatchInsert(
          fProperties,fTableName,ExternalFields,Types,n,Values) else begin
        // use array binding
        Query := fProperties.NewThreadSafeStatementPrepared(SQL,false);
        try
          case fBatchMethod of
          mPost, mPut:
            for i := 0 to high(Values) do
              Query.BindArray(i+1,Types[i],Values[i],n);
          mDelete:
            Query.BindArray(1,ftInt64,Values[0],n);
          end;
          Query.ExecutePrepared;
        finally
          Query := nil;
        end;
      end;
      if Owner<>nil then begin
        // add/update/delete should flush DB cache
        Owner.FlushInternalDBCache;
        // force deletion coherency
        if fBatchMethod=mDelete then
          for i := 0 to high(Values) do
            Owner.AfterDeleteForceCoherency(
              fStoredClass,GetInteger(pointer(Values[i])));
      end;
      Fields := nil; // force new sending block
      BatchBegin := BatchEnd+1;
    until BatchBegin>=fBatchCount;
    if Owner<>nil then begin
      if fBatchMethod in [mPost,mPut] then begin
        if fBatchMethod=mPost then
          NotifySQLEvent := seAdd else
          NotifySQLEvent := seUpdate;
        for i := 0 to fBatchCount-1 do
          Owner.InternalUpdateEvent(NotifySQLEvent,fStoredClassProps.TableIndex,
            fBatchIDs[i],fBatchValues[i],nil);
      end;
      Owner.FlushInternalDBCache;
    end;
  finally
    if fBatchMethod=mPost then
      fEngineLockedLastID := fBatchAddedID+fBatchCount;
    SetLength(fBatchValues,0);
    SetLength(fBatchIDs,0);
    fBatchCount := 0;
    fBatchCapacity := 0;
    fBatchMethod := mNone;
    StorageUnLock;
  end;
end;

function TSQLRestStorageExternal.InternalBatchAdd(
  const aValue: RawUTF8; aID: integer): integer;
begin
  result := fBatchAddedID+fBatchCount;
  if fBatchCount>=fBatchCapacity then begin
    fBatchCapacity := fBatchCapacity+64+fBatchCount shr 3;
    SetLength(fBatchIDs,fBatchCapacity);
    if aValue<>'' then
      SetLength(fBatchValues,fBatchCapacity);
  end;
  if aValue<>'' then
    fBatchValues[fBatchCount] := aValue;
  if aID=0 then
    aID := result;
  fBatchIDs[fBatchCount] := aID;
  inc(fBatchCount);
end;

function TSQLRestStorageExternal.EngineAdd(TableModelIndex: integer;
  const SentData: RawUTF8): integer;
begin
  if (TableModelIndex<0) or (fModel.Tables[TableModelIndex]<>fStoredClass) then
    result := 0 else // avoid GPF
  if fBatchMethod<>mNone then
    if fBatchMethod<>mPOST then
      result := 0 else
      result := InternalBatchAdd(SentData,0) else begin
    result := ExecuteFromJSON(SentData,soInsert,0); // UpdatedID=0 -> insert with EngineLockedNextID
    if (result>0) and (Owner<>nil) then begin
      Owner.InternalUpdateEvent(seAdd,TableModelIndex,result,SentData,nil);
      Owner.FlushInternalDBCache;
    end;
  end;
end;

function TSQLRestStorageExternal.EngineUpdate(TableModelIndex, ID: integer;
  const SentData: RawUTF8): boolean;
begin
  if (ID<=0) or (TableModelIndex<0) or (Model.Tables[TableModelIndex]<>fStoredClass) then
    result := false else
    if fBatchMethod<>mNone then
      if fBatchMethod<>mPUT then
        result := false else
        result := InternalBatchAdd(SentData,ID)>=0 else begin
      result := ExecuteFromJSON(SentData,soUpdate,ID)=ID;
      if result and (Owner<>nil) then begin
        Owner.InternalUpdateEvent(seUpdate,TableModelIndex,ID,SentData,nil);
        Owner.FlushInternalDBCache;
      end;
    end;
end;

function TSQLRestStorageExternal.EngineDelete(TableModelIndex, ID: integer): boolean;
begin
  if (ID<=0) or (TableModelIndex<0) or (Model.Tables[TableModelIndex]<>fStoredClass) then
    result := false else
    if fBatchMethod<>mNone then
      if fBatchMethod<>mDELETE then
        result := false else
        result := InternalBatchAdd('',ID)>=0 else begin
      if Owner<>nil then // notify BEFORE deletion
        Owner.InternalUpdateEvent(seDelete,TableModelIndex,ID,'',nil);
      result := ExecuteDirect('delete from % where %=?',
        [fTableName,StoredClassProps.ExternalDB.RowIDFieldName],[ID],false)<>nil;
      if result and (Owner<>nil) then
        Owner.FlushInternalDBCache;
    end;
end;

function TSQLRestStorageExternal.EngineDeleteWhere(TableModelIndex: integer;
  const SQLWhere: RawUTF8; const IDs: TIntegerDynArray): boolean;
var i,n: integer;
    aSQLWhereUpper: RawUTF8;
    InClause: TIntegerDynArray;
begin
  result := false;
  if (IDs=nil) or (SQLWhere='') or
     (TableModelIndex<0) or (Model.Tables[TableModelIndex]<>fStoredClass) then
    exit;
  if fBatchMethod<>mNone then
    if fBatchMethod<>mDELETE then
      exit else
      for i := 0 to high(IDs) do
        InternalBatchAdd('',IDs[i]) else begin
    if Owner<>nil then // notify BEFORE deletion
      for i := 0 to high(IDs) do
        Owner.InternalUpdateEvent(seDelete,TableModelIndex,IDs[i],'',nil);
    aSQLWhereUpper := UpperCase(SQLWhere);
    if IdemPChar(pointer(aSQLWhereUpper),'LIMIT ') or
       // LIMIT is not handled by SQLite3 when built from amalgamation
       // see http://www.sqlite.org/compile.html#enable_update_delete_limit
       IdemPChar(pointer(aSQLWhereUpper),'ORDER BY ') or
       (PosEx(' FROM ',aSQLWhereUpper)>0) then begin
      SetLength(InClause,200); // send by chunks
      for i := 0 to length(IDs) div Length(InClause) do begin
        if length(IDs)<(i+1)*length(InClause) then
          n := length(IDs)-i*length(InClause) else
          n := length(InClause);
        Move(IDs[i*length(InClause)],InClause[0],n*sizeof(Integer));
        if ExecuteInlined('delete from % where %',[fTableName,
            IntegerDynArrayToCSV(InClause,n,'RowID in (',')')],false)=nil then
          exit;
      end;
      exit;
    end else
    if ExecuteInlined('delete from % where %',[fTableName,SQLWhere],false)=nil then
      exit;
    if result and (Owner<>nil) then
      Owner.FlushInternalDBCache;
  end;
  result := true;
end;

function TSQLRestStorageExternal.EngineList(const SQL: RawUTF8;
  ForceAJAX: Boolean; ReturnedRowCount: PPtrInt): RawUTF8;
var Stmt: ISQLDBStatement;
begin
  if ReturnedRowCount<>nil then
    raise ESQLDBException.CreateUTF8('%.EngineList(ReturnedRowCount<>nil) for %',
      [self,StoredClass]);
  Stmt := PrepareInlinedForRows(SQL);
  if Stmt=nil then
    result := '' else
    Stmt.ExecutePreparedAndFetchAllAsJSON(
      ForceAJAX or (Owner=nil) or (not Owner.NoAJAXJSON),result);
end;

function TSQLRestStorageExternal.EngineRetrieve(TableModelIndex, ID: integer): RawUTF8;
var Stmt: ISQLDBStatement;
begin // TableModelIndex is not useful here
  result := '';
  if (self=nil) or (ID<=0) then
    exit;
  Stmt := PrepareDirectForRows(pointer(fSelectOneDirectSQL),[],[ID]);
  if Stmt<>nil then begin
    Stmt.ExecutePreparedAndFetchAllAsJSON(true,result); // Expanded=true -> '[{"ID":10,...}]'#10
    if IsNotAjaxJSON(pointer(result)) then
      // '{"fieldCount":2,"values":["ID","FirstName"]}'#$A -> ID not found
      result := '' else
      // list '[{...}]'#10 -> object '{...}'
      result := copy(result,2,length(result)-3);
  end;
end;

function TSQLRestStorageExternal.EngineExecute(
  const aSQL: RawUTF8): boolean;
begin
  if aSQL='' then
    result := false else
    result := ExecuteInlined(aSQL,false)<>nil;
end;

function TSQLRestStorageExternal.TableHasRows(Table: TSQLRecordClass): boolean;
var Rows: ISQLDBRows;
begin
  if (self=nil) or (Table<>fStoredClass) then
    result := false else begin
    Rows := ExecuteDirect(pointer(fSelectTableHasRowsSQL),[],[],true);
    if Rows=nil then
      result := false else
      result := Rows.Step;
  end;
end;

function TSQLRestStorageExternal.TableRowCount(Table: TSQLRecordClass): integer;
var Rows: ISQLDBRows;
begin
  if (self=nil) or (Table<>fStoredClass) then
    result := 0 else begin
    Rows := ExecuteDirect('select count(*) from %',[fTableName],[],true);
    if (Rows=nil) or not Rows.Step then
      result := 0 else
      result := Rows.ColumnInt(0);
  end;
end;

function TSQLRestStorageExternal.EngineRetrieveBlob(TableModelIndex, aID: integer;
  BlobField: PPropInfo; out BlobData: TSQLRawBlob): boolean;
var Rows: ISQLDBRows;
begin
  result := false;
  if (aID<=0) or (not BlobField^.IsBlob) or
     (TableModelIndex<0) or (Model.Tables[TableModelIndex]<>fStoredClass) then
    exit;
  with StoredClassProps.ExternalDB do
    Rows := ExecuteDirect('select % from % where %=?',
      [InternalToExternal(BlobField^.Name),fTableName,RowIDFieldName],[aID],true);
  if (Rows<>nil) and Rows.Step then
  try
    BlobData := Rows.ColumnBlob(0);
    result := true; // success
    Rows := nil;
  except
    on Exception do
      result := false;
  end;
end;

function TSQLRestStorageExternal.RetrieveBlobFields(Value: TSQLRecord): boolean;
var Rows: ISQLDBRows;
    f: Integer;
    data: TSQLVar;
    temp: RawByteString;
begin
  result := false;
  if (Value<>nil) and (Value.ID>0) and (PSQLRecordClass(Value)^=fStoredClass) then
  with Value.RecordProps do
  if BlobFields<>nil then begin
    Rows := ExecuteDirect('select % from % where %=?',
      [fRetrieveBlobFieldsSQL,fTableName,StoredClassProps.ExternalDB.RowIDFieldName],
      [Value.ID],true);
    if (Rows<>nil) and Rows.Step then
    try
      for f := 0 to High(BlobFields) do begin
        Rows.ColumnToSQLVar(f,data,temp);
        BlobFields[f].SetFieldSQLVar(Value,data);
      end;
      result := true; // success
      Rows := nil;
    except
      on Exception do
        result := false;
    end;
  end;
end;

function TSQLRestStorageExternal.EngineUpdateField(TableModelIndex: integer;
  const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUTF8): boolean;
var Rows: ISQLDBRows;
    ExtWhereFieldName, JSON: RawUTF8;
begin
  if (TableModelIndex<0) or (Model.Tables[TableModelIndex]<>fStoredClass) then
    result := false else
    with StoredClassProps.ExternalDB do begin
      ExtWhereFieldName := InternalToExternal(WhereFieldName);
      result := ExecuteInlined('update % set %=:(%): where %=:(%):',
        [fTableName,InternalToExternal(SetFieldName),SetValue,
         ExtWhereFieldName,WhereValue],false)<>nil;
    if result and (Owner<>nil) then begin
      if Owner.InternalUpdateEventNeeded(TableModelIndex) then begin
        Rows := ExecuteInlined('select % from % where %=:(%):',
          [RowIDFieldName,fTableName,ExtWhereFieldName,WhereValue],true);
        if Rows=nil then
          exit;
        JSON := '{"'+SetFieldName+'":'+SetValue+'}';
        while Rows.Step do
          Owner.InternalUpdateEvent(seUpdate,TableModelIndex,Rows.ColumnInt(0),JSON,nil);
      end;
      Owner.FlushInternalDBCache;
    end;
  end;
end;

function TSQLRestStorageExternal.EngineUpdateBlob(TableModelIndex, aID: integer;
  BlobField: PPropInfo; const BlobData: TSQLRawBlob): boolean;
var Statement: ISQLDBStatement;
    AffectedField: TSQLFieldBits;
begin
  result := false;
  if (aID<=0) or (not BlobField^.IsBlob) or
     (TableModelIndex<0) or (Model.Tables[TableModelIndex]<>fStoredClass) then
    exit;
  try
    if Owner<>nil then
      Owner.FlushInternalDBCache;
    with StoredClassProps.ExternalDB do
      Statement := fProperties.NewThreadSafeStatementPrepared(
        'update % set %=? where %=?',
        [fTableName,InternalToExternal(BlobField^.Name),RowIDFieldName],false);
    if Statement<>nil then begin
      if BlobData='' then
        Statement.BindNull(1) else
        Statement.BindBlob(1,BlobData); // fast explicit BindBlob() call
      Statement.Bind(2,aID);
      Statement.ExecutePrepared;
      if Owner<>nil then begin
        fStoredClassRecordProps.FieldIndexsFromBlobField(BlobField,AffectedField);
        Owner.InternalUpdateEvent(seUpdateBlob,TableModelIndex,aID,'',@AffectedField);
        Owner.FlushInternalDBCache;
      end;
      result := true; // success
    end;
  except
    on Exception do
      result := false;
  end;
end;

function TSQLRestStorageExternal.UpdateBlobFields(Value: TSQLRecord): boolean;
var f, aID: integer;
    temp: array of RawByteString;
    Params: TSQLVarDynArray;
begin
  result := false;
  if (Value<>nil) and (PSQLRecordClass(Value)^=fStoredClass) then
  with Value.RecordProps do
  if BlobFields<>nil then begin
    aID := Value.ID;
    if aID<=0 then
      exit; 
    if Owner<>nil then
      Owner.FlushInternalDBCache;
    SetLength(Params,length(BlobFields));
    SetLength(temp,length(BlobFields));
    for f := 0 to high(Params) do
      BlobFields[f].GetFieldSQLVar(Value,Params[f],temp[f]);
    result := ExecuteDirectSQLVar('update % set % where %=?',
      [fTableName,fUpdateBlobFieldsSQL,StoredClassProps.ExternalDB.RowIDFieldName],
       Params,aID,false);
    if result and (Owner<>nil) then begin
      Owner.InternalUpdateEvent(seUpdateBlob,fStoredClassProps.TableIndex,aID,'',
          @fStoredClassRecordProps.BlobFieldsBits);
      Owner.FlushInternalDBCache;
    end;
  end else
    result := true; // as TSQLRest.UpdateblobFields()
end;

function TSQLRestStorageExternal.PrepareInlinedForRows(const aSQL: RawUTF8): ISQLDBStatement;
begin
  result := nil; // returns nil interface on error
  if self=nil then
    exit;
  try
    result := fProperties.PrepareInlined(aSQL,true);
  except
    on Exception do
      result := nil;
  end;
end;

function TSQLRestStorageExternal.ExecuteInlined(const aSQL: RawUTF8;
  ExpectResults: Boolean): ISQLDBRows;
begin
  result := nil; // returns nil interface on error
  if self=nil then
    exit;
  if (not ExpectResults) and (Owner<>nil) then
    Owner.FlushInternalDBCache; // add/update/delete should flush DB cache
  try
    result := fProperties.ExecuteInlined(aSQL,ExpectResults);
  except
    on Exception do
      result := nil;
  end;
end;

function TSQLRestStorageExternal.ExecuteInlined(SQLFormat: PUTF8Char;
  const Args: array of const; ExpectResults: Boolean): ISQLDBRows;
begin
  result := ExecuteInlined(FormatUTF8(SQLFormat,Args),ExpectResults);
end;

function TSQLRestStorageExternal.PrepareDirectForRows(SQLFormat: PUTF8Char;
  const Args, Params: array of const): ISQLDBStatement;
var Query: ISQLDBStatement;
begin
  result := nil;
  if self=nil then
    exit;
  Query := fProperties.NewThreadSafeStatementPrepared(SQLFormat,Args,true);
  if Query<>nil then
  try
    Query.Bind(Params);
    result := Query;
  except
    on Exception do
      result := nil;
  end;
end;

function TSQLRestStorageExternal.ExecuteDirect(SQLFormat: PUTF8Char;
  const Args, Params: array of const; ExpectResults: Boolean): ISQLDBRows;
var Query: ISQLDBStatement;
begin
  result := nil;
  if self=nil then
    exit;
  if (not ExpectResults) and (Owner<>nil) then
    Owner.FlushInternalDBCache; // add/update/delete should flush DB cache
  Query := fProperties.NewThreadSafeStatementPrepared(SQLFormat,Args,ExpectResults);
  if Query<>nil then
  try
    Query.Bind(Params);
    Query.ExecutePrepared;
    result := Query;
  except
    on Exception do
      result := nil;
  end;
end;

function TSQLRestStorageExternal.ExecuteDirectSQLVar(SQLFormat: PUTF8Char;
  const Args: array of const; var Params: TSQLVarDynArray; LastIntegerParam: integer;
  ParamsMatchCopiableFields: boolean): boolean;
var Query: ISQLDBStatement;
    ParamsCount, f: integer;
begin
  result := false;
  if Self<>nil then
  try
    Query := fProperties.NewThreadSafeStatementPrepared(SQLFormat,Args,false);
    if Query=nil then
      exit;
    ParamsCount := length(Params);
    if ParamsMatchCopiableFields and
       (ParamsCount<>Length(fStoredClassRecordProps.CopiableFields)) then
      raise EORMException.CreateUTF8('%.ExecuteDirectSQLVar(ParamsMatchCopiableFields) for %',
       [self,StoredClass]);
    for f := 0 to ParamsCount-1 do
      if ParamsMatchCopiableFields and
         (fStoredClassRecordProps.CopiableFields[f].SQLFieldType=sftDateTime) and
         (Params[f].VType=ftUTF8) then
        Query.BindDateTime(f+1,Iso8601ToDateTimePUTF8Char(Params[f].VText)) else
        Query.Bind(f+1,Params[f]);
    if LastIntegerParam<>0 then
      Query.Bind(ParamsCount+1,LastIntegerParam);
    Query.ExecutePrepared;
    result := true; // success
  except
    on Exception do
      result := false;
  end;
end;

procedure TSQLRestStorageExternal.RollBack(SessionID: cardinal);
begin
  inherited RollBack(SessionID); // reset fTransactionActive
  try
    fProperties.ThreadSafeConnection.Rollback;
  except
    on Exception do
      ; // just catch exception
  end;
end;

function TSQLRestStorageExternal.EngineSearchField(
  const FieldName: ShortString; const FieldValue: array of const;
  var ResultID: TIntegerDynArray): boolean;
var n: Integer;
    Rows: ISQLDBRows;
begin
  n := 0;
  Rows := ExecuteDirect('select % from % where %=?',
    [StoredClassProps.ExternalDB.RowIDFieldName,fTableName,FieldName],FieldValue,true);
  if Rows<>nil then
    while Rows.Step do
      AddInteger(ResultID,n,Rows.ColumnInt(0));
  SetLength(ResultID,n);
  result := n>0;
end;

function TSQLRestStorageExternal.SearchField(const FieldName: RawUTF8;
  FieldValue: Integer; var ResultID: TIntegerDynArray): boolean;
begin
  result := EngineSearchField(FieldName,[FieldValue],ResultID);
end;

function TSQLRestStorageExternal.SearchField(const FieldName, FieldValue: RawUTF8;
  var ResultID: TIntegerDynArray): boolean;
begin
  result := EngineSearchField(FieldName,[FieldValue],ResultID);
end;

function TSQLRestStorageExternal.TransactionBegin(
  aTable: TSQLRecordClass; SessionID: cardinal): boolean;
begin
  result := false;
  if (aTable=fStoredClass) and inherited TransactionBegin(aTable,SessionID) then
  try
    fProperties.ThreadSafeConnection.StartTransaction;
    result := true; // success
  except
    on Exception do
      result := false;
  end;
end;

function TSQLRestStorageExternal.CreateSQLMultiIndex(
  Table: TSQLRecordClass; const FieldNames: array of RawUTF8;
  Unique: boolean; IndexName: RawUTF8): boolean;
var SQL: RawUTF8;
    ExtFieldNames: TRawUTF8DynArray;
    IntFieldIndex: TIntegerDynArray;
    Descending: boolean;
    i,n,extfield: integer;
begin
  result := false;
  Descending := false;
  n := length(FieldNames);
  if (self=nil) or (fProperties=nil) or (Table<>fStoredClass) or (n<=0) then
    exit;
  StoredClassProps.ExternalDB.InternalToExternalDynArray(
    FieldNames,ExtFieldNames,@IntFieldIndex);
  if n=1 then begin // handle case of index over a single column
    if IntFieldIndex[0]<0 then // ID/RowID?
      case fProperties.DBMS of
      dSQLite: begin
        result := true; // SQLite3 always generates an index for ID/RowID
        exit;
      end;
      dFirebird:  // see http://www.firebirdfaq.org/faq205
        Descending := true;
      end;
    if not Descending then begin // we identify just if indexed, not the order
      extfield := fFieldsInternalToExternal[IntFieldIndex[0]+1];
      if (extfield>=0) and (fFieldsExternal[extfield].ColumnIndexed) then begin
        result := true; // column already indexed
        exit;
      end;
    end;
  end;
  if not (fProperties.DBMS in DB_HANDLEINDEXONBLOBS) then
    // BLOB fields cannot be indexed (only in SQLite3)
    for i := 0 to n-1 do begin
      extfield := fFieldsInternalToExternal[IntFieldIndex[i]+1];
      if (extfield>=0) and
         (fFieldsExternal[extfield].ColumnType in [ftBlob,ftUTF8]) and
         (fFieldsExternal[extfield].ColumnLength<=0) then begin
        if i=0 then
          exit; // impossible to create an index with no field!
        SetLength(ExtFieldNames,i); // truncate index to the last indexable field
        break;
      end;
    end;
  SQL := fProperties.SQLAddIndex(fTableName,ExtFieldNames,Unique,Descending,IndexName);
  if (SQL='') or (ExecuteDirect(pointer(SQL),[],[],false)=nil) then
    exit;
  result := true;
  extfield := fFieldsInternalToExternal[IntFieldIndex[0]+1];
  if extfield>=0 then // mark first column as indexed by now
    fFieldsExternal[extfield].ColumnIndexed := true;
end;

class function TSQLRestStorageExternal.Instance(
  aClass: TSQLRecordClass; aServer: TSQLRestServer): TSQLRestStorageExternal;
begin
  if (aClass=nil) or (aServer=nil) then
    result := nil else begin
    result := TSQLRestStorageExternal(aServer.StaticVirtualTable[aClass]);
    if result<>nil then
      if not result.InheritsFrom(TSQLRestStorageExternal) then
        result := nil;
  end;
end;

class function TSQLRestStorageExternal.ConnectionProperties(
  aClass: TSQLRecordClass; aServer: TSQLRestServer): TSQLDBConnectionProperties;
begin
  result := Instance(aClass,aServer).ConnectionProperties;
end;

function TSQLRestStorageExternal.ConnectionProperties: TSQLDBConnectionProperties;
begin
  if self=nil then
    result := nil else
    result := fProperties;
end;
  
function TSQLRestStorageExternal.ExecuteFromJSON(
  const SentData: RawUTF8; Occasion: TSQLOccasion; UpdatedID: integer): integer;
var Decoder: TJSONObjectDecoder;
    SQL: RawUTF8;
    Types: TSQLDBFieldTypeArray;
    ExternalFields: TRawUTF8DynArray;
    InsertedID, F: integer;
    Query: ISQLDBStatement;
begin
  result := 0;
  StorageLock(false); // avoid race condition against max(ID)
  try
    case Occasion of
    soInsert: begin
      InsertedID := JSONRetrieveIDField(pointer(SentData));
      if InsertedID=0 then // no specified "ID":... field value -> compute next
        InsertedID := EngineLockedNextID else
        if result>fEngineLockedLastID then
          fEngineLockedLastID := result;
    end;
    soUpdate:
      if UpdatedID<>0 then
        InsertedID := 0 else
        raise ESQLDBException.CreateUTF8('%.ExecuteFromJSON(%,soUpdate,UpdatedID=%)',
          [self,StoredClass,UpdatedID]);
    else raise ESQLDBException.CreateUTF8('%.ExecuteFromJSON(%,Occasion=%)?',
           [self,StoredClass,ord(Occasion)]);
    end;
    // decode fields
    Decoder.Decode(SentData,nil,pNonQuoted,InsertedID,true);
    if (Decoder.FieldCount=0) and (Occasion=soUpdate) then begin
      result := UpdatedID; // SentData='' -> no column to update
      exit;
    end;
    // compute SQL statement and associated bound parameters
    SQL := JSONDecodedPrepareToSQL(Decoder,ExternalFields,Types,Occasion);
    if Occasion=soUpdate then
      Decoder.FieldValues[Decoder.FieldCount-1] := Int32ToUTF8(UpdatedID);
    // execute statement
    Query := fProperties.NewThreadSafeStatementPrepared(SQL,false);
    if Query=nil then
      exit;
    try
      for F := 0 to Decoder.FieldCount-1 do
      if Decoder.FieldTypeApproximation[F]=ftaNull then
        Query.BindNull(F+1) else
        Query.Bind(F+1,Types[F],Decoder.FieldValues[F],true);
      Query.ExecutePrepared;
    except
      exit; // leave result=0
    end;
    // mark success
    if UpdatedID=0 then
      result := InsertedID else
      result := UpdatedID;
  finally
    StorageUnLock;
  end;
end;

procedure TSQLRestStorageExternal.EndCurrentThread(Sender: TThread);
begin
  if fProperties.InheritsFrom(TSQLDBConnectionPropertiesThreadSafe) then
    TSQLDBConnectionPropertiesThreadSafe(fProperties).EndCurrentThread;
end;

function TSQLRestStorageExternal.InternalFieldNameToFieldExternalIndex(
  const InternalFieldName: RawUTF8): integer;
begin
  result := StoredClassRecordProps.Fields.IndexByNameOrExcept(InternalFieldName);
  result := fFieldsInternalToExternal[result+1];
end;

function TSQLRestStorageExternal.JSONDecodedPrepareToSQL(
  var Decoder: TJSONObjectDecoder; out ExternalFields: TRawUTF8DynArray;
  out Types: TSQLDBFieldTypeArray; Occasion: TSQLOccasion): RawUTF8;
var f,k: Integer;
begin
  SetLength(ExternalFields,Decoder.FieldCount);
  for f := 0 to Decoder.FieldCount-1 do begin
    k := InternalFieldNameToFieldExternalIndex(Decoder.FieldNames[f]);
    if k<0 then
      raise ESQLDBException.CreateUTF8(
        '%.JSONDecodedPrepareToSQL: Unknown field "%" in %',
        [self,Decoder.FieldNames[f],StoredClass]);
    ExternalFields[f] := fFieldsExternal[k].ColumnName;
    Types[f] := fFieldsExternal[k].ColumnType;
  end;
  // compute SQL statement and associated bound parameters
  Decoder.DecodedFieldNames := pointer(ExternalFields);
  result := Decoder.EncodeAsSQLPrepared(fTableName,Occasion,
    StoredClassProps.ExternalDB.RowIDFieldName);
  if Occasion=soUpdate then begin
    Types[Decoder.FieldCount] := ftInt64; // add "where ID=?" parameter
    inc(Decoder.FieldCount);
  end;
end;

procedure TSQLRestStorageExternal.ResetMaxIDCache;
begin
  StorageLock(true);
  fEngineLockedLastID := 0;
  StorageUnLock;
end;


{ TSQLVirtualTableCursorExternal }

function TSQLVirtualTableCursorExternal.Column(aColumn: integer;
  var aResult: TSQLVar): boolean;
var n: cardinal;
begin
  result := false;
  if (self<>nil) and (fStatement<>nil) then
  try
    n := fStatement.ColumnCount-1;
    if aColumn=VIRTUAL_TABLE_ROWID_COLUMN then
      aColumn := n else // RowID is latest column (select %,RowID from..)
      if cardinal(aColumn)>=n then
        exit; // error if aColumn is out of range
    fStatement.ColumnToSQLVar(aColumn,aResult,fColumnTemp);
    result := aResult.VType<>ftUnknown;
  except
    on Exception do
      result := false;
  end;
end;

function TSQLVirtualTableCursorExternal.HasData: boolean;
begin
  result := (self<>nil) and (fStatement<>nil) and fHasData;
end;

function TSQLVirtualTableCursorExternal.Next: boolean;
begin
  result := false;
  if (self<>nil) and (fStatement<>nil) then
  try
    fHasData := fStatement.Step;
    result := true; // success (may be with no more data)
  except
    on Exception do
      fHasData := false; // returns false on error + HasData=false
  end;
end;

const
  SQL_OPER_WITH_PARAM: array[soEqualTo..soGreaterThanOrEqualTo] of RawUTF8 = (
    '=?','<>?','<?','<=?','>?','>=?');

function TSQLVirtualTableCursorExternal.Search(
  const Prepared: TSQLVirtualTablePrepared): boolean;
var i: integer;
begin
  result := false;
  if (Self=nil) or (Table=nil) or (Table.Static=nil) then
    exit;
  with Table.Static as TSQLRestStorageExternal do begin
    if fSQL='' then begin
      // compute the SQL query corresponding to this prepared request
      fSQL := fSelectAllDirectSQL;
      if Prepared.WhereCount<>0 then begin
        for i := 0 to Prepared.WhereCount-1 do
        with Prepared.Where[i] do begin
          if Operation>high(SQL_OPER_WITH_PARAM) then
            exit; // invalid specified operator -> abort search
          if i=0 then
            fSQL := fSQL+' where ' else
            fSQL := fSQL+' and ';
          if StoredClassProps.ExternalDB.AppendFieldName(Column,fSQL) then
            exit; // invalid column index -> abort search
          fSQL := fSQL+SQL_OPER_WITH_PARAM[Operation];
        end;
      end;
      // e.g. 'select FirstName,..,ID from PeopleExternal where FirstName=? and LastName=?'
      for i := 0 to Prepared.OrderByCount-1 do
      with Prepared.OrderBy[i] do begin
        if i=0 then
          fSQL := fSQL+' order by ' else
          fSQL := fSQL+', ';
        if StoredClassProps.ExternalDB.AppendFieldName(Column,fSQL) then
          exit; // invalid column index -> abort search
        if Desc then
          fSQL := fSQL+' desc';
      end;
    end;
    // execute the SQL statement
    try
      fStatement := fProperties.NewThreadSafeStatementPrepared(fSQL,true);
      if fStatement<>nil then begin
        for i := 1 to Prepared.WhereCount do
          fStatement.Bind(i,Prepared.Where[i-1].Value);
        fStatement.ExecutePrepared;
        result := Next; // on execution success, go to the first row
      end;
    except
      on Exception do
        fStatement := nil;
    end;
  end;
end;


{ TSQLVirtualTableExternal }

function TSQLVirtualTableExternal.Drop: boolean;
begin
  if (self=nil) or (Static=nil) then
    result := false else
    with Static as TSQLRestStorageExternal do
      result := ExecuteDirect('drop table %',[fTableName],[],false)<>nil;
end;

class procedure TSQLVirtualTableExternal.GetTableModuleProperties(
  var aProperties: TVirtualTableModuleProperties);
begin
  aProperties.Features := [vtWrite];
  aProperties.CursorClass := TSQLVirtualTableCursorExternal;
  aProperties.StaticClass := TSQLRestStorageExternal;
end;

function TSQLVirtualTableExternal.Prepare(var Prepared: TSQLVirtualTablePrepared): boolean;
var i, col: integer;
    hasIndex: boolean;
    Fields: TSQLPropInfoList;
begin
  result := inherited Prepare(Prepared); // Prepared.EstimatedCost := 1E10;
  if result and (Static<>nil) then
  with Static as TSQLRestStorageExternal do begin
    // mark Where[] clauses will be handled by SQL
    Fields := StoredClassRecordProps.Fields;
    result := false;
    for i := 0 to Prepared.WhereCount-1 do
      with Prepared.Where[i] do
      if (Column<>VIRTUAL_TABLE_IGNORE_COLUMN) and
         (Operation<=high(SQL_OPER_WITH_PARAM)) then begin
        if Column=VIRTUAL_TABLE_ROWID_COLUMN then // is an indexed primary key
          hasIndex := true else begin
          if cardinal(Column)>=cardinal(Fields.Count) then
            exit; // invalid column index -> abort query
          col := fFieldsInternalToExternal[Column+1];
          if col<0 then
            exit; // column not known in the external database -> abort query
          hasIndex := fFieldsExternal[col].ColumnIndexed;
        end;
        OmitCheck := true; // search handled via SQL query
        Value.VType := ftNull; // caller vt_BestIndex() expects <> ftUnknown
        if hasIndex then // the more indexes, the faster
          Prepared.EstimatedCost := Prepared.EstimatedCost/100;
      end;
    // check the OrderBy[] clauses
    if Prepared.OrderByCount>0 then begin
      for i := 0 to Prepared.OrderByCount-1 do
        with Prepared.OrderBy[i] do
        if (Column<>VIRTUAL_TABLE_ROWID_COLUMN) and
           (cardinal(Column)>=cardinal(Fields.Count)) then
          exit; // invalid column index -> abort query
      Prepared.OmitOrderBy := true; // order handled via SQL query
    end;
    result := true; // success
  end;
end;

// here below, virtual write operations do not call Engine*() but direct SQL
// -> InternalUpdateEvent() were already called by MainEngine*() methods

function TSQLVirtualTableExternal.Delete(aRowID: Int64): boolean;
begin
  if (self<>nil) and (Static<>nil) and (aRowID>0) then 
    with Static as TSQLRestStorageExternal do
      result := ExecuteDirect('delete from % where %=?',
        [fTableName,StoredClassProps.ExternalDB.RowIDFieldName],[aRowID],false)<>nil else
    result := false;
end;

function TSQLVirtualTableExternal.Insert(aRowID: Int64;
  var Values: TSQLVarDynArray; out insertedRowID: Int64): boolean;
begin // aRowID is just ignored here since IDs are always auto calculated
  result := false;
  if (self<>nil) and (Static<>nil) then
  with Static as TSQLRestStorageExternal do begin
    StorageLock(false); // to avoid race condition against max(RowID)
    try
      insertedRowID := EngineLockedNextID;
      with StoredClassProps.ExternalDB do
        result := ExecuteDirectSQLVar('insert into % (%,%) values (%,?)',
          [fTableName,SQL.InsertSet,RowIDFieldName,CSVOfValue('?',length(Values))],
          Values,insertedRowID,true);
    finally
      StorageUnLock;
    end;
  end;
end;

function TSQLVirtualTableExternal.Update(oldRowID, newRowID: Int64;
  var Values: TSQLVarDynArray): boolean;
begin
  if (self<>nil) and (Static<>nil) and
     (oldRowID=newRowID) and (newRowID>0) then // don't allow ID change
    with Static as TSQLRestStorageExternal, StoredClassProps.ExternalDB do
      result := ExecuteDirectSQLVar('update % set % where %=?',
        [fTableName,SQL.UpdateSetAll,RowIDFieldName],Values,oldRowID,true) else
    result := false;
end;


initialization
  // all our SynDB related functions shall log to main TSQLLog
  SynDBLog := TSQLLog;
end.