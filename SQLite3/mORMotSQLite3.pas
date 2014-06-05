/// SQLite3 embedded Database engine used as the mORMot SQL kernel
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit mORMotSQLite3;

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


    Initial version: 2008 March, by Arnaud Bouchez

    Version 1.3 - January 22, 2010
    - allow backup directly into a custom file (not only a custom directory)
    - on-the-fly Restore of the database from a compressed backup file
    - some small fixes and multi-compiler enhancements
    - compiler conditional renamed ENHANCEDRTL instead of ENHANCEDTRTL

    Version 1.4 - February 08, 2010
    - whole Synopse SQLite3 database framework released under the GNU Lesser
      General Public License version 3, instead of generic "Public Domain"

    Version 1.5 - March 10, 2010
    - updated engine to version 3.6.23
    - included FTS3 embedded sqlite3fts3.obj version

    Version 1.6
    - SQLite3 database layer updated to version 3.6.23.1

    Version 1.7
    - alter table with newly added fields to a TSQLRecord

    Version 1.8
    - includes Unitary Testing class and functions
    - database update engine to version 3.7.0 (main new feature is WAL)
    - SetWALMode() method for enabling Write-Ahead Logging for the database
    - the RTREE extension is now compiled by default into the engine
    - new tests added (mostly relative to the new functions or classes)

    Version 1.9
    - database engine updated to 3.7.1
    - fix issue in TSQLRestServerDB.CreateMissingTables when an exception occured
    - handle now RowID as a valid alias to the ID field (needed for TSQLRecordFTS3)
    - handle TSQLRecordFTS3 record, for using FTS3 virtual tables,
      i.e. implementing full-text search (including dedicated regression tests)

    Version 1.9.1
    - update engine to version 3.7.2: an obscure but very old bug makes
      SQLite authors recommend to use 3.7.2 for all new development.
      Upgrading from all prior SQLite versions is also recommended.

    Version 1.9.2
    - fixed a potential GPF in TSQLRestClientDB.Destroy

    Version 1.10
    - code modifications to compile with Delphi 6 compiler (Delphi 5 failed due
      to some obscure compiler bugs in SynCrypto.pas)
    - update SQLite3 engine to version 3.7.3

    Version 1.11
    - update SQLite3 engine to version 3.7.4
    - introduces new TSQLite3DB, TSQLite3Statement, TSQLite3Blob, TSQLite3Value
      and TSQLite3FunctionContext types to clarify SQLite3 internal handle usage
    - new sqlite3_busy_timeout and sqlite3_busy_handler low-level functions
    - new TSQLDataBase.BusyTimeout property, to set the database timeout
      value in milliseconds
    - now handles User Defined Functions, via sqlite3_create_function_v2 and
      corresponding sqlite3_result_* functions: as sample, the MOD() function
      is defined in any database opened via TSQLDataBase (it was needed
      to have compatibility with Oracle/MySQL/MSSQL/PostGreSQL engines)
    - protect the TSQLDatabase methods called when self is nil, which could
      occur if the database is not yet initialized (could occur if only a
      TSQLRestStorage exists, like in TTestSQLite3Engine._TSQLRestClientDB)
    - new SOUNDEX() function available in SQL statements (calling SoundExUTF8)
      and associated SOUNDEXFR/SOUNDEXES for french or spanish Soundex variants
    - fixed an issue found out by WladiD about all collation functions:
      If a field with your custom collate ISO8601 is empty '' (not NULL),
      then SQLite calls the registered collate function with length 0 for s1len
      or s2len, but the pointers s1 or s2 map to the string of the previous call
    - added sqlite3_result_error() call to make wrong parameter count error
      explicit during SQL statement internal functions calls
    - handles new FTS4 extension module  - see
      http://sqlite.org/fts3.html#section_1_1 - which is available since 3.7.4
    - new RANK() function available in SQL statements for ranking FTS3/FTS4
      with best performance (used by the new TSQLRest.FTSMatch() overloaded
      method) - see http://www.sqlite.org/fts3.html#appendix_a
    - fixed dual memory release in case of FTS3 use, in TSQLDataBase.Destroy
    - source code modified to be 7 bit Ansi (so will work with all encodings)

    Version 1.12
    - update SQLite3 engine to version 3.7.5
    - fixed sqlite3_result_text() implementation
    - added sqlite3_aggregate_context(), sqlite3_value_numeric_type() and
      sqlite3InternalFree() functions
    - new CONCAT() function available in SQL statements to process fast
      string concatenation
    - now handle automaticaly prepared SQL statements: the parameters must
      be surrounded with :(...): in order to use an internal pool of prepared
      TSQLRequest; example of possible inlined values are :(1234):
      :(12.34): :(12E-34): :("text"): or :('text'): (with double quoting
      inside the text, just like any SQL statement)
    - new sqlite3_stmt_readonly() function and TSQLRequest.IsReadOnly property

    Version 1.13
    - update SQLite3 engine to version 3.7.6.3
    - added sqlite3InternalFreeObject(), sqlite3_malloc/realloc/free(),
      sqlite3_memory_used/highwater(), sqlite3_set_authorizer() and
      sqlite3_update/commit/rollback_hook() functions
    - introducing TSQLVirtualTableModuleSQLite3 / TSQLVirtualTableModuleServerDB
      classes, and associated low-level sqlite3 functions and memory structures,
      in order to implement Virtual Table modules in pure Delphi code via
      common TSQLVirtualTable classes as defined in SQLite3Commons
    - new IntegerDynArrayContains(), RawUTF8DynArrayContainsCase/NoCase() and
      Byte/Word/Cardinal/Int64/CurrencyDynArrayContains(BlobField,I64)
      SQL functions, able to fast search data within T*DynArray and
      TRawUTF8DynArray published properties BLOB (Currency mapped as PInt64)
    - new TSQLDataBaseSQLFunction and TSQLDataBase.RegisterSQLFunction method,
      to implement custom SQL functions with any kind of BLOB data
    - regression test now exclude unfixed order for select (may vary,
      just like happened for 3.7.6 when e.g. indexes started to be used)
    - added regression tests for sftBlobDynArray and sftObject kind of records
    - TSQLRestServerDB now uses faster TDynArrayHashed for its internal prepared
      statement cache
    - fixed issue in TSQLRestClientDB.URI: wrong InternalState returned
    - fastest internal cdecl qsort() function (optimized for PAnsiChar)

    Version 1.14
    - update SQLite3 engine to version 3.7.7.1
    - fixed issue in produced JSON stream using '=' instead of ':'
    - TSQLDatabase.user_version now defined as a property, with a getter and
      a setter methods (not read/only any more)

    Version 1.15
    - updated SQLite3 engine to version 3.7.8
    - unit now tested with Delphi XE2 (32 Bit)
    - transactions now following a safe concurent access (both thread-safe and
      client/connection-safe) - but authentication should be enabled
    - the SQLite3 wrapper is now located in a separate SynSQLite3 unit: this
      will allow to use it as a separate database engine, e.g. using SynDB
      classes without the overhead/features of our mORMot framework
    - statement cache is now shared with SynDBSQLite3, via the new
      TSQLStatementCached object as defined in SynSQLite3.pas
    - now TSQLRestServerDB will unregister any TSQLVirtualTableModuleServerDB
      to avoid random GPF in TSQLVirtualTable.Destroy
    - TSQLRestClientDB and TSQLRestServerDB constructors now accept an optional
      Password parameter, associated to the supplied file name, in order to
      use database encryption

    Version 1.16
    - updated SQLite3 engine to version 3.7.12.1
    - unit now includes FTS3/FTS4 by default (i.e. INCLUDE_FTS3 conditional is
      set in both SQLite3.pas and SynSQLite3.pas units)
    - fixed TSQLRestServerDB.UpdateField(ByID=true) implementation
    - fixed VACUUM failure if there are one or more active SQL statements
    - new overloaded TSQLRestServerDB.UpdateField method
    - TSQLRestServerDB.EngineList()  method now handles an optional integer
      pointer, to return the count of row data
    - updated TSQLRestServerTest published methods to use the new parameter
      layout using "var aParams: TSQLRestServerCallBackParams"

    Version 1.17
    - updated SQLite3 engine to version 3.7.14
    - added overridden TSQLRestServerDB.FlushInternalDBCache method
    - added TSQLRestServerDB.BackupGZ method for live database backup into a
      compressed .gz archive file

    Version 1.18
    - unit SQLite3.pas renamed mORMotSQLite3.pas
    - updated SQLite3 engine to latest version 3.8.5
    - fixed potential GPF issue in TSQLRestServerDB.Destroy when registered
      TSQLVVirtualtableModuleDBs are already destroyed
    - fixed ticket [64c90ade80] in TSQLRestClientDB.Destroy when associated
      FServer failed to initialize
    - replaced confusing TVarData by a new dedicated TSQLVar memory structure,
      shared with SynDB and mORMot units (includes methods refactoring)
    - SQLVarToContext() will now bind '' text instead of null value
    - TSQLRestServerDB.GetAndPrepareStatement() will now recognize
      'INSERT INTO ... DEFAULT VALUES;' as a potential prepared statement
    - added optional LastInsertedID parameter to TSQLRestServerDB.EngineExecute()
      for proper multi-threaded execution - used e.g. by EngineAdd()
    - added TSQLRestServerDB.PrepareVacuum() private method to fix ticket
      [9f3faa8e44] - since VACUUM is buggy in SQLite3, and disconnect all
      virtual tables, it is now a no-op if such virtual tables are defined
    - fixed unexpected call to TSQLRecord.InitializeTable(self,'') when the
      main table was just created as virtual, and the external DB is not void
    - fix ticket [b2f158aa3c] and let VirtualTableExternalRegisterAll() work
      as expected, with no error message (see sample Project14ServerExternal)
    - TSQLRestServerDB.EngineAdd() will now handle forced ID in sent data
    - new constructor TSQLRestClientDB.Create(aRunningServer) for direct access
      to an existing TSQLRestServerDB instance
    - TSQLRestClientDB.Destroy will now unlock records before ending server
    - renamed setter SetDB() to Attach() stand-alone method
    - extraction of TTestSQLite3Engine code into SynSelfTests.pas unit
    - handle null binding in TSQLRestServerDB.GetAndPrepareStatement()
    - optimized TSQLRestServerDB.UpdateBlobFields() and RetrieveBlobFields()
      methods, updating/retrieving all BLOB fields at once in SQL statement
    - fixed TSQLRestServerDB.UpdateBlobFields() to return true if no BLOB field
      is defined (as with TSQLRestServer) - ticket [bfa13889d5]
    - fixed issue in TSQLRestServerDB.Backup() to restore virtual tables
    - fixed ticket [72b3d8e616] in TSQLRestServerDB.Restore()
    - low-level vt_*() callbacks defined for TSQLVirtualTableModuleSQLite3
      will now call SynSQLite3Log.DebuggerNotify() for most SQLITE_ERROR
    - this unit will now set SynSQLite3Log := TSQLLog during its initialization

}

interface

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 WITHLOG SQLITE3_FASTCALL

uses
  Windows,
  SysUtils,
  Classes,
{$ifndef LVCL}
  Contnrs,
{$endif}
  SynZip,
  SynCommons,
  SynSQLite3,
  mORMot;

{$define INCLUDE_FTS3}
{ define this if you want to include the FTS3/FTS4 feature into the library
  - FTS3 is an SQLite module implementing full-text search
  - will include also FTS4 extension module since 3.7.4
  - see http://www.sqlite.org/fts3.html for documentation
  - is defined by default, but can be unset to save about 50 KB of code size
  - should be defined for SynSQLite3, SynSQLite3Static and mORMotSQLite3 units }


{ ****************** SQLite3 database used as kernel of our mORMot framework } 

type
  /// Execute a SQL statement in the local SQLite3 database engine, and get
  // result in memory
  // - all DATA (even the BLOB fields) is converted into UTF-8 TEXT
  // - uses a TSQLTableJSON internaly: faster than sqlite3_get_table()
  // (less memory allocation/fragmentation) and allows efficient caching
  TSQLTableDB = class(TSQLTableJSON)
  private
  public
    {{ Execute a SQL statement, and init TSQLTable fields
     - FieldCount=0 if no result is returned
     - the BLOB data is converted into TEXT: you have to retrieve it with
      a special request explicitely (note that JSON format returns BLOB data)
     - uses a TSQLTableJSON internaly: all currency is transformed to its floating
       point TEXT representation, and allows efficient caching
     - if the SQL statement is in the DB cache, it's retrieved from its cached
       value: our JSON parsing is a lot faster than SQLite3 engine itself,
       and uses less memory
     - will raise an ESQLException on any error }
    constructor Create(aDB: TSQLDatabase; const Tables: array of TSQLRecordClass;
      const aSQL: RawUTF8; Expand: boolean); reintroduce;
  end;

  TSQLRestServerDBClass = class of TSQLRestServerDB;

  TSQLVirtualTableModuleServerDB = class;

  /// REST server with direct access to a SQLite3 database
  // - caching is handled at TSQLDatabase level
  // - SQL statements for record retrieval from ID are prepared for speed
  TSQLRestServerDB = class(TSQLRestServer)
  private
    /// internal copy of the SQLite3 database engine
    fDB: TSQLDataBase;
    /// initialized by Create(aModel,aDBFileName)
    fOwnedDB: TSQLDataBase;
    /// prepared statements with parameters for faster SQLite3 execution
    // - works for SQL code with :(%): internal parameters
    fStatementCache: TSQLStatementCached;
    /// static statement used if fStatementCache is not to be used
    // - for any SQL code with no :(%): internal parameters
    // - only one global is OK: is protected via a nested DB.LockJSON/DB.Unlock
    fStaticStatement: TSQLRequest;
    /// list of TSQLVirtualTableModuleServerDB registered external modules
    // - is a TList and not a TObjectList since instances will be destroyed by
    // the SQLite3 engine via sqlite3InternalFreeModule() private function
    // - here to avoid GPF in TVirtualTable.Destroy
    fRegisteredVirtualTableModules: TList;
    /// check if a VACUUM statement is possible
    // - VACUUM in fact DISCONNECT all virtual modules (sounds like a SQLite3
    // design problem), so calling it during process could break the engine
    // - if you can safely run VACUUM, returns TRUE and release all active
    // SQL statements (otherwise VACUUM will fail)
    // - if there are some static virtual tables, returns FALSE and do nothing:
    // in this case, VACUUM will be a no-op
    function PrepareVacuum(const aSQL: RawUTF8): boolean;
  protected
    /// retrieve a TSQLRequest instance, corresponding to any previous
    // prepared statement using :(%): internal parameters
    // - will return @fStaticStatement if no :(%): internal parameters appear:
    // in this case, the TSQLRequest.Close method must be called
    // - will return a @fStatementCache[].Statement, after having bounded the
    // :(%): parameter values; in this case, TSQLRequest.Close must not be called
    // - expect sftBlob, sftBlobDynArray and sftBlobRecord properties
    // to be encoded as ':("\uFFF0base64encodedbinary"):'
    function GetAndPrepareStatement(const SQL: RawUTF8): PSQLRequest;
    /// reset the cache if necessary
    procedure SetNoAJAXJSON(const Value: boolean); override;
    /// overridden methods for direct sqlite3 database engine call:
    function MainEngineList(const SQL: RawUTF8; ForceAJAX: Boolean; ReturnedRowCount: PPtrInt): RawUTF8; override;
    function MainEngineRetrieve(TableModelIndex, ID: integer): RawUTF8; override;
    function MainEngineAdd(TableModelIndex: integer; const SentData: RawUTF8): integer; override;
    function MainEngineUpdate(TableModelIndex, ID: integer; const SentData: RawUTF8): boolean; override;
    function MainEngineDelete(TableModelIndex, ID: integer): boolean; override;
    function MainEngineDeleteWhere(TableModelIndex: Integer; const SQLWhere: RawUTF8;
      const IDs: TIntegerDynArray): boolean; override;
    function MainEngineRetrieveBlob(TableModelIndex, aID: integer;
      BlobField: PPropInfo; out BlobData: TSQLRawBlob): boolean; override;
    function MainEngineUpdateBlob(TableModelIndex, aID: integer;
      BlobField: PPropInfo; const BlobData: TSQLRawBlob): boolean; override;
    function MainEngineUpdateField(TableModelIndex: integer;
      const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUTF8): boolean; override;
    /// execute one SQL statement
    // - intercept any DB exception and return false on error, true on success
    // - optional LastInsertedID can be set (if ValueInt/ValueUTF8 are nil) to
    // retrieve the proper ID when aSQL is an INSERT statement (thread safe)
    function EngineExecute(const aSQL: RawUTF8; ValueInt: PInt64=nil; ValueUTF8: PRawUTF8=nil;
      LastInsertedID: PInt64=nil): boolean; overload;
    /// EngineExecute directly a SQL statement with supplied parameters
    // - expect the same format as FormatUTF8() function, that is only % with
    // strings (already quoted) and integers (not ? with parameters)
    // - return true on success
    function EngineExecuteFmt(SQLFormat: PUTF8Char; const Args: array of const): boolean;
    /// execute one SQL statement, which must return an integer (Int64) value
    // - intercept any DB exception and return false on error, true on success
    function EngineExecute(const aSQL: RawUTF8; out Value: Int64): boolean; overload;
    /// execute one SQL statement, which must return a UTF-8 encoded value
    // - intercept any DB exception and return false on error, true on success
    function EngineExecute(const aSQL: RawUTF8; out Value: RawUTF8): boolean; overload;
    /// execute one SQL statement, and apply an Event to every record
    // - lock the database during the run
    // - call a fast "stored procedure"-like method for each row of the request;
    // this method must use low-level DB access in any attempt to modify the
    // database (e.g. a prepared TSQLRequest with Reset+Bind+Step), and not
    // the TSQLRestServerDB.Engine*() methods which include a Lock(): this Lock()
    // is performed by the main loop in EngineExecute() and any attempt to
    // such high-level call will fail into an endless loop
    // - caller may use a transaction in order to speed up StoredProc() writing
    // - intercept any DB exception and return false on error, true on success
    function EngineExecute(const aSQL: RawUTF8; StoredProc: TOnSQLStoredProc): boolean; overload;
  public
    {{ begin a transaction (implements REST BEGIN Member)
     - to be used to speed up some SQL statements like Insert/Update/Delete
     - must be ended with Commit on success
     - must be aborted with Rollback if any SQL statement failed
     - return true if no transaction is active, false otherwise }
    function TransactionBegin(aTable: TSQLRecordClass; SessionID: cardinal=1): boolean; override;
    {{ end a transaction (implements REST END Member)
     - write all pending SQL statements to the disk }
    procedure Commit(SessionID: cardinal=1); override;
    {{ abort a transaction (implements REST ABORT Member)
     - restore the previous state of the database, before the call to TransactionBegin }
    procedure RollBack(SessionID: cardinal=1); override;
     /// overridden method for direct SQLite3 database engine call
     // - it will update all BLOB fields at once, in one SQL statement
    function UpdateBlobFields(Value: TSQLRecord): boolean; override;
     /// overridden method for direct SQLite3 database engine call
     // - it will retrieve all BLOB fields at once, in one SQL statement
    function RetrieveBlobFields(Value: TSQLRecord): boolean; override;
     /// overridden method for direct SQLite3 database engine call
    function EngineExecuteAll(const aSQL: RawUTF8): boolean; override;
    {{ backup of the opened Database into an external stream (e.g. a file,
      compressed or not)
     - this method doesn't use the experimental SQLite Online Backup API
      (which restart the backup process on any write: so no good performance
      could be achieved on a working database: this method uses a faster
      lock + copy approach)
     - database is closed, VACCUUMed, copied, then reopened }
    function Backup(Dest: TStream): boolean;
    {{ backup of the opened Database into a .gz compressed file
     - database is closed, VACCUUMed, compressed into .gz file, then reopened
     - default compression level is 2, which is very fast, and good enough for
       a database file content: you may change it into the default 6 level }
    function BackupGZ(const DestFileName: TFileName; CompressionLevel: integer=2): boolean;
    {{ restore a database content on the fly
     - database is closed, source DB file is replaced by the supplied content,
       then reopened }
    function Restore(const ContentToRestore: RawByteString): boolean;
    {{ restore a database content on the fly, from a .gz compressed file
     - database is closed, source DB file is replaced by the supplied content,
       then reopened }
    function RestoreGZ(const BackupFileName: TFileName): boolean;
    /// initialize the associated DB connection
    // - called by Create and on Backup/Restore just after DB.DBOpen
    // - will register all *_in() functions for available TSQLRecordRTree
    // - will register all modules for available TSQLRecordVirtualTable*ID
    // with already registered modules via RegisterVirtualTableModule()
    // - you can override this method to call e.g. DB.RegisterSQLFunction()
    procedure InitializeEngine; virtual;
    /// call this method when the internal DB content is known to be invalid
    // - by default, all REST/CRUD requests and direct SQL statements are
    // scanned and identified as potentially able to change the internal SQL/JSON
    // cache used at SQLite3 database level; but some virtual tables (e.g.
    // TSQLRestStorageExternal classes defined in SQLite3DB) could flush
    // the database content without proper notification
    // - this overridden implementation will call TSQLDataBase.CacheFlush method
    procedure FlushInternalDBCache; override;
  public
    /// initialize a REST server with a database
    // - any needed TSQLVirtualTable class should have been already registered
    // via the RegisterVirtualTableModule() method
    constructor Create(aModel: TSQLModel; aDB: TSQLDataBase;
      aHandleUserAuthentication: boolean=false); overload; virtual;
    /// initialize a REST server with a database, by specifying its filename
    // - TSQLRestServerDB will initialize a owned TSQLDataBase, and free it on Destroy
    // - if specified, the password will be used to cypher this file on disk
    // (the main SQLite3 database file is encrypted, not the wal file during run)
    // - it will then call the other overloaded constructor to initialize the server
    constructor Create(aModel: TSQLModel; const aDBFileName: TFileName;
      aHandleUserAuthentication: boolean=false; const aPassword: RawUTF8=''); reintroduce; overload;
    /// close database and free used memory
    destructor Destroy; override;
    /// Missing tables are created if they don't exist yet for every TSQLRecord
    // class of the Database Model
    // - you must call explicitely this before having called StaticDataCreate()
    // - all table description (even Unique feature) is retrieved from the Model
    // - this method also create additional fields, if the TSQLRecord definition
    // has been modified; only field adding is available, field renaming or
    // field deleting are not allowed in the FrameWork (in such cases, you must
    // create a new TSQLRecord type)
    procedure CreateMissingTables(user_version: cardinal=0); override;

    /// associated database
    property DB: TSQLDataBase read fDB;
  end;

  /// REST client with direct access to a SQLite3 database
  // - a hidden TSQLRestServerDB server is created and called internaly
  TSQLRestClientDB = class(TSQLRestClientURI)
  private
    // use internaly a TSQLRestServerDB to access data in the proper JSON format
    fServer: TSQLRestServerDB;
    fOwnedServer: TSQLRestServerDB;
    fOwnedDB: TSQLDataBase;
    fInternalHeader: RawUTF8;
    function getDB: TSQLDataBase;
  protected
    /// method calling the RESTful server fServer
    procedure InternalURI(var Call: TSQLRestURIParams); override;
    /// overridden protected method do nothing (direct DB access has no connection)
    function InternalCheckOpen: boolean; override;
    /// overridden protected method do nothing (direct DB access has no connection)
    procedure InternalClose; override;
  public
    /// initializes the class, and creates an internal TSQLRestServerDB to
    // internaly answer to the REST queries
    // - aServerClass could be TSQLRestServerDB by default
    constructor Create(aClientModel, aServerModel: TSQLModel; aDB: TSQLDataBase;
      aServerClass: TSQLRestServerDBClass;
      aHandleUserAuthentication: boolean=false); reintroduce; overload;
    /// same as above, from a SQLite3 filename specified
    // - an internal TSQLDataBase will be created internaly and freed on Destroy
    // - aServerClass could be TSQLRestServerDB by default
    // - if specified, the password will be used to cypher this file on disk
    // (the main SQLite3 database file is encrypted, not the wal file during run)
    constructor Create(aClientModel, aServerModel: TSQLModel; const aDBFileName: TFileName;
      aServerClass: TSQLRestServerDBClass;
      aHandleUserAuthentication: boolean=false; const aPassword: RawUTF8=''); reintroduce; overload;
    /// initialize the class, for an existing TSQLRestServerDB
    // - the client TSQLModel will be cloned from the server's one
    // - the TSQLRestServerDB and TSQLDatabase instances won't be managed by the
    // client, but will access directly to the server
    constructor Create(aRunningServer: TSQLRestServerDB); reintroduce; overload;
    /// release the server
    destructor Destroy; override;

    /// retrieve a list of members as a TSQLTable (implements REST GET Collection)
    // - this overridden method call directly the database to get its result,
    // without any URI() call, but with use of DB JSON cache if available
    // - other TSQLRestClientDB methods use URI() function and JSON conversion
    // of only one record properties values, which is very fast
    function List(const Tables: array of TSQLRecordClass; const SQLSelect: RawUTF8 = 'ID';
      const SQLWhere: RawUTF8 = ''): TSQLTableJSON; override;
    /// associated Server
    property Server: TSQLRestServerDB read fServer;
    /// associated database
    property DB: TSQLDataBase read getDB;
  end;

  {{ define a Virtual Table module for a stand-alone SQLite3 engine
   - it's not needed to free this instance: it will be destroyed by the SQLite3
   engine together with the DB connection }
  TSQLVirtualTableModuleSQLite3 = class(TSQLVirtualTableModule)
  protected
    fDB: TSQLDataBase;
    /// used internaly to register the module to the SQLite3 engine
    fModule: TSQLite3Module;
  public
    /// initialize the module for a given DB connection
    // - internally set fModule and call sqlite3_create_module_v2(fModule)
    // - will raise EBusinessLayerException if aDB is incorrect, or SetDB() has
    // already been called for this module
    // - will call sqlite3_check() to raise the corresponding ESQLite3Exception
    // - in case of success (no exception), the SQLite3 engine will release the
    // module by itself; but in case of error (an exception is raised), it is
    // up to the caller to intercept it via a try..except and free the
    // TSQLVirtualTableModuleSQLite3 instance
    procedure Attach(aDB: TSQLDataBase);
    /// retrieve the file name to be used for a specific Virtual Table
    // - overridden method returning a file located in the DB file folder, and
    // '' if the main DB was created as SQLITE_MEMORY_DATABASE_NAME (i.e.
    // ':memory:' so that no file should be written)
    // - of course, if a custom FilePath property value is specified, it will be
    // used, even if the DB is created as SQLITE_MEMORY_DATABASE_NAME
    function FileName(const aTableName: RawUTF8): TFileName; override;
    /// the associated SQLite3 database connection
    property DB: TSQLDataBase read fDB;
  end;

  {{ define a Virtual Table module for a TSQLRestServerDB SQLite3 engine }
  TSQLVirtualTableModuleServerDB = class(TSQLVirtualTableModuleSQLite3)
  public
    /// register the Virtual Table to the database connection of a TSQLRestServerDB server
    // - in case of an error, an excepton will be raised
    constructor Create(aClass: TSQLVirtualTableClass; aServer: TSQLRestServer); override;
    /// clean class instance memory
    // - especially the link to TSQLRestServerDB
    destructor Destroy; override;
  end;

/// initialize a Virtual Table Module for a specified database
// - to be used for low-level access to a virtual module, e.g. with
// TSQLVirtualTableLog
// - when using our ORM, you should call TSQLModel.VirtualTableRegister()
// instead to associate a TSQLRecordVirtual class to a module
// - returns the created TSQLVirtualTableModule instance (which will be a
// TSQLVirtualTableModuleSQLite3 instance in fact)
// - will raise an exception of failure
function RegisterVirtualTableModule(aModule: TSQLVirtualTableClass; aDatabase: TSQLDataBase): TSQLVirtualTableModule;

/// set a TSQLVar into a SQlite3 result context
// - will call the corresponding sqlite3_result_*() function and return true,
// or will return false if the TSQLVar type is not handled
function SQLVarToSQlite3Context(const Res: TSQLVar; Context: TSQLite3FunctionContext): boolean;

/// set a SQLite3 value into a TSQLVar
// - will call the corresponding sqlite3_value_*() function to retrieve the
// data with the less overhead (e.g. memory allocation or copy) as possible
procedure SQlite3ValueToSQLVar(Value: TSQLite3Value; var Res: TSQLVar);



implementation


{ TSQLTableDB }

constructor TSQLTableDB.Create(aDB: TSQLDataBase; const Tables: array of TSQLRecordClass;
  const aSQL: RawUTF8; Expand: boolean);
var JSONCached: RawUTF8;
    R: TSQLRequest;
    n: PtrInt;
begin
  JSONCached := aDB.LockJSON(aSQL,@n);
  if JSONCached='' then // not retrieved from cache -> call SQLite3 engine
    try // faster than sqlite3_get_table(): memory is allocated as a whole
      n := 0;
      JSONCached := R.ExecuteJSON(aDB.DB,aSQL,Expand,@n); // Expand=true for AJAX
      inherited CreateFromTables(Tables,aSQL,JSONCached);
      Assert(n=RowCount);
    finally
      aDB.UnLockJSON(JSONCached,n);
    end
  else begin
    inherited CreateFromTables(Tables,aSQL,JSONCached);
    Assert(n=RowCount);
  end;
end;


{ TSQLRestServerDB }

function TSQLRestServerDB.GetAndPrepareStatement(const SQL: RawUTF8): PSQLRequest;
var i, maxParam: integer;
    Types: TSQLParamTypeDynArray;
    Nulls: TSQLFieldBits;
    Values: TRawUTF8DynArray;
    GenericSQL: RawUTF8;
begin
  GenericSQL := ExtractInlineParameters(SQL,Types,Values,maxParam,Nulls);
  if maxParam=0 then begin
    // SQL code with no valid :(...): internal parameters
    if not (IdemPChar(pointer(SQL),'INSERT INTO ') and
            (PosEx(' DEFAULT VALUES;',SQL,13)=Length(SQL)-15)) then begin
      result := @fStaticStatement;
      result^.Prepare(DB.DB,SQL);
      {$ifdef WITHLOG}
      DB.Log.Log(sllSQL,'% is no prepared statement',SQL,self);
      {$endif}
      exit;
    end;
  end;
  {$ifdef WITHLOG}
  DB.Log.Log(sllSQL,'% prepared with % param%',[SQL,maxParam,PLURAL_FORM[maxParam>1]],self);
  {$endif}
  result := fStatementCache.Prepare(GenericSQL);
  // bind parameters
  assert(sqlite3.bind_parameter_count(result^.Request)=maxParam);
  for i := 0 to maxParam-1 do
  if i in Nulls then
    result^.BindNull(i) else
    case Types[i] of
      sptDateTime, // date/time are stored as ISO-8601 TEXT in SQLite3
      sptText:    result^.Bind(i+1,Values[i]);
      sptBlob:    result^.Bind(i+1,pointer(Values[i]),length(Values[i]));
      sptInteger: result^.Bind(i+1,GetInt64(pointer(Values[i])));
      sptFloat:   result^.Bind(i+1,GetExtended(pointer(Values[i])));
    end;
end;

function TSQLRestServerDB.MainEngineAdd(TableModelIndex: integer; const SentData: RawUTF8): integer;
var SQL: RawUTF8;
    LastID: Int64;
begin
  result := 0;
  if TableModelIndex<0 then
    exit;
  SQL := 'INSERT INTO '+fModel.TableProps[TableModelIndex].Props.SQLTableName;
  if trim(SentData)='' then
    SQL := SQL+' DEFAULT VALUES;' else
    SQL := SQL+GetJSONObjectAsSQL(SentData,false,true,
      JSONRetrieveIDField(pointer(SentData)))+';';
  if EngineExecute(SQL,nil,nil,@LastID) then begin
    result := LastID;
    InternalUpdateEvent(seAdd,fModel.Tables[TableModelIndex],result,nil);
  end;
end;

procedure InternalRTreeIn(Context: TSQLite3FunctionContext;
  argc: integer; var argv: TSQLite3ValueArray); {$ifndef SQLITE3_FASTCALL}cdecl;{$endif}
var aRTree: TSQLRecordRTreeClass;
    BlobA, BlobB: pointer;
begin
  if argc<>2 then begin
    ErrorWrongNumberOfArgs(Context);
    exit;
  end;
  aRTree := sqlite3.user_data(Context);
  BlobA := sqlite3.value_blob(argv[0]);
  BlobB := sqlite3.value_blob(argv[1]);
  if (aRTree=nil) or (BlobA=nil) or (BlobB=nil) then
    sqlite3.result_error(Context,'invalid call') else
    sqlite3.result_int64(Context,byte(aRTree.ContainedIn(BlobA^,BlobB^)));
end;

constructor TSQLRestServerDB.Create(aModel: TSQLModel; aDB: TSQLDataBase;
  aHandleUserAuthentication: boolean);
begin
  fStatementCache.Init(aDB.DB);
  aDB.UseCache := true; // we better use caching in this JSON oriented use
  fDB := aDB;
  if fDB.InternalState=nil then begin // should be done once
    InternalState := 1;
    fDB.InternalState := @InternalState; // to update TSQLRestServerDB.InternalState
  end;
  inherited Create(aModel,aHandleUserAuthentication);
  InitializeEngine;
end;

procedure TSQLRestServerDB.InitializeEngine;
var i,m: integer;
    aModule: TSQLVirtualTableClass;
begin
  for i := 0 to high(Model.TableProps) do
    case Model.TableProps[i].Kind of
    rRTree: // register all *_in() SQL functions
      sqlite3_check(DB.DB,sqlite3.create_function_v2(DB.DB,
        pointer(TSQLRecordRTreeClass(Model.Tables[i]).RTreeSQLFunctionName),
        2,SQLITE_ANY,Model.Tables[i],InternalRTreeIn,nil,nil,nil));
    rCustomForcedID, rCustomAutoID: begin
      aModule := Model.VirtualTableModule(Model.Tables[i]);
      if aModule<>nil then begin
        // perform registration of the module, if needed
        if fRegisteredVirtualTableModules=nil then
          fRegisteredVirtualTableModules := TList.Create else
          with fRegisteredVirtualTableModules do
          for m := 0 to Count-1 do
          if TSQLVirtualTableModuleServerDB(List[m]).fTableClass=aModule then begin
            aModule := nil; // already registered -> do nothing
            break;
          end;
        if aModule<>nil then
          fRegisteredVirtualTableModules.Add(TSQLVirtualTableModuleServerDB.Create(aModule,self));
      end;
    end;
    end;
end;

constructor TSQLRestServerDB.Create(aModel: TSQLModel; const aDBFileName: TFileName;
  aHandleUserAuthentication: boolean; const aPassword: RawUTF8);
begin
  fOwnedDB := TSQLDataBase.Create(aDBFileName,aPassword); // will be freed in Destroy
  Create(aModel,fOwnedDB,aHandleUserAuthentication);
end;

procedure TSQLRestServerDB.CreateMissingTables(user_version: cardinal);
var t,f,nt,nf: integer;
    TableNamesAtCreation, aFields: TRawUTF8DynArray;
    TableJustCreated: TSQLFieldTables;
    aSQL: RawUTF8;
begin
  if DB.TransactionActive then
    raise EBusinessLayerException.Create('CreateMissingTables: Transaction');
  fDB.GetTableNames(TableNamesAtCreation);
  nt := length(TableNamesAtCreation);
  QuickSortRawUTF8(TableNamesAtCreation,nt,nil,@StrIComp);
  fillchar(TableJustCreated,sizeof(TSQLFieldTables),0);
  try
    // create not static and not existing tables
    for t := 0 to high(Model.Tables) do
      if ((fStaticData=nil) or (fStaticData[t]=nil)) then
      // this table is not static -> check if already existing, create if necessary
      with Model.TableProps[t], Props do
      if FastFindPUTF8CharSorted(pointer(TableNamesAtCreation),nt-1,pointer(SQLTableName),@StrIComp)<0 then begin
        if not DB.TransactionActive then
          DB.TransactionBegin; // make initialization faster by using transaction
        DB.Execute(Model.GetSQLCreate(t)); // don't catch exception in constructor
        include(TableJustCreated,t);       // mark to be initialized below
      end else begin
        // this table is existing: check that all fields exist -> create if necessary
        DB.GetFieldNames(aFields,SQLTableName);
        nf := length(aFields);
        QuickSortRawUTF8(aFields,nf,nil,@StrIComp);
        for f := 0 to Fields.Count-1 do
          with Fields.List[f] do
          if SQLFieldType in COPIABLE_FIELDS then
          /// real database columns exist for Simple + Blob fields (not Many)
          if FastFindPUTF8CharSorted(pointer(aFields),nf-1,pointer(Name),@StrIComp)<0 then begin
            aSQL := Model.GetSQLAddField(t,f);
            if aSQL<>'' then begin // need a true field with data
              if not DB.TransactionActive then
                DB.TransactionBegin; // make initialization faster by using transaction
              DB.Execute(aSQL);
            end;
            Model.Tables[t].InitializeTable(self,Name);
          end;
      end;
    if not DB.TransactionActive then
      exit;
    // database schema was modified -> update user version in SQLite3 file
    if user_version<>0 then
      DB.user_version := user_version;
    // initialize new tables AFTER creation of ALL tables
    if not IsZero(@TableJustCreated,sizeof(TSQLFieldTables)) then
      for t := 0 to high(Model.Tables) do
        if t in TableJustCreated then
          if (not(Model.TableProps[t].Kind in IS_CUSTOM_VIRTUAL)) or
             (not TableHasRows(Model.Tables[t])) then // check is really void
            Model.Tables[t].InitializeTable(self,''); // '' for table creation
    DB.Commit;
  except
    on E: Exception do begin
      DB.RollBack; // will close any active Transaction
      raise;     // caller must handle exception
    end;
  end;
end;

function TSQLRestServerDB.MainEngineDelete(TableModelIndex, ID: integer): boolean;
begin
  if (TableModelIndex<0) or (ID<=0) then
    result := false else begin
    // notify BEFORE deletion
    InternalUpdateEvent(seDelete,fModel.Tables[TableModelIndex],ID,nil);
    result := EngineExecuteFmt('DELETE FROM % WHERE RowID=:(%):;',
      [fModel.TableProps[TableModelIndex].Props.SQLTableName,ID]);
  end;
end;

function TSQLRestServerDB.MainEngineDeleteWhere(TableModelIndex: Integer;
  const SQLWhere: RawUTF8; const IDs: TIntegerDynArray): boolean;
var i: integer;
begin
  if (TableModelIndex<0) or (SQLWhere='') or (IDs=nil) then
    result := false else begin
    // notify BEFORE deletion
    for i := 0 to high(IDs) do
      InternalUpdateEvent(seDelete,fModel.Tables[TableModelIndex],IDs[i],nil);
    result := EngineExecuteFmt('DELETE FROM % WHERE %',
      [fModel.TableProps[TableModelIndex].Props.SQLTableName,SQLWhere]);
  end;
end;

destructor TSQLRestServerDB.Destroy;
var i: integer;
begin
  try
    if fRegisteredVirtualTableModules<>nil then
      with fRegisteredVirtualTableModules do
      for i := 0 to Count-1 do
        TSQLVirtualTableModuleServerDB(List[i]).fServer := nil;
    FreeAndNil(fRegisteredVirtualTableModules);
    inherited Destroy;
  finally
    try
      fStatementCache.ReleaseAllDBStatements;
    finally
      fOwnedDB.Free;
    end;
  end;
end;

function TSQLRestServerDB.PrepareVacuum(const aSQL: RawUTF8): boolean;
begin
  result := not IdemPChar(Pointer(aSQL),'VACUUM');
  if result then
    exit;
  result :=  (fStaticVirtualTable=nil) or
    IsZero(fStaticVirtualTable,length(fStaticVirtualTable)*sizeof(pointer));
  if result then
    // VACUUM will fail if there are one or more active SQL statements
    fStatementCache.ReleaseAllDBStatements;
end;

function TSQLRestServerDB.EngineExecute(const aSQL: RawUTF8;
  ValueInt: PInt64=nil; ValueUTF8: PRawUTF8=nil; LastInsertedID: PInt64=nil): boolean;
var Req: PSQLRequest;
begin
  if (self<>nil) and (DB<>nil) then 
  try
    DB.Lock(aSQL);
    try
      result := true;
      if not PrepareVacuum(aSQL) then
        exit; // no-op if there are some static virtual tables around
      Req := GetAndPrepareStatement(aSQL);
      if Req<>nil then
      with Req^ do
      try
        if (ValueInt=nil) and (ValueUTF8=nil) then begin
          // default execution: loop through all rows
          repeat until Step<>SQLITE_ROW;
          if LastInsertedID<>nil then
            LastInsertedID^ := DB.LastInsertRowID;
        end else begin
          // get one row, and retrieve value
          if Step=SQLITE_ROW then
            if ValueInt<>nil then
              ValueInt^ := FieldInt(0) else
              ValueUTF8^ := FieldUTF8(0) else
            result := false;
          end;
      finally
        if Req=@fStaticStatement then
          Close;
      end;
    finally
      DB.UnLock;
    end;
  except
    on E: ESQLite3Exception do begin
      {$ifdef WITHLOG}
      DB.Log.Log(sllError,'% for %',[E,aSQL],self);
      {$else}
      LogToTextFile('TSQLRestServerDB.EngineExecute: '+RawUTF8(E.Message)+#13#10+aSQL);
      {$endif}
      result := false;
    end;
  end else
    result := false;
end;

function TSQLRestServerDB.EngineExecuteFmt(SQLFormat: PUTF8Char;
  const Args: array of const): boolean;
begin
  result := EngineExecute(FormatUTF8(SQLFormat,Args));
end;

function TSQLRestServerDB.EngineExecute(const aSQL: RawUTF8; out Value: Int64): boolean;
begin
  result:= EngineExecute(aSQL,@Value,nil);
end;

function TSQLRestServerDB.EngineExecute(const aSQL: RawUTF8; out Value: RawUTF8): boolean;
begin
  result:= EngineExecute(aSQL,nil,@Value);
end;

function TSQLRestServerDB.EngineExecute(const aSQL: RawUTF8;
  StoredProc: TOnSQLStoredProc): boolean;
var R: TSQLRequest; // we don't use fStatementCache[] here
    Res: integer;
begin
  result := false;
  if (self<>nil) and (DB<>nil) and (aSQL<>'') and Assigned(StoredProc) then
  try
    DB.LockAndFlushCache; // even if aSQL is SELECT, StoredProc may update data
    try
      try
        R.Prepare(DB.DB,aSQL);
        if R.FieldCount>0 then
        repeat
          res := R.Step;
          if res=SQLITE_ROW then
            StoredProc(R); // apply the stored procedure to all rows
        until res=SQLITE_DONE;
        result := true;
      finally
        R.Close; // always release statement
      end;
    finally
      DB.UnLock;
    end;
  except
    on E: ESQLite3Exception do begin
      {$ifdef WITHLOG}
      DB.Log.Log(sllError,'% for %',[E,aSQL],self);
      {$else}
      LogToTextFile('TSQLRestServerDB.EngineExecute Error: '+RawUTF8(E.Message)+#13#10+aSQL);
      {$endif}
      result := false;
    end;
  end;
end;

function TSQLRestServerDB.EngineExecuteAll(const aSQL: RawUTF8): boolean;
begin
  try
    result := true;
    if PrepareVacuum(aSQL) then // no-op if there are any static virtual tables
      DB.ExecuteAll(aSQL); // Execute all statements (don't use fStatementCache[])
  except
    on E: ESQLite3Exception do begin
      {$ifdef WITHLOG}
      DB.Log.Log(sllError,'% for %',[E,aSQL],self);
      {$else}
      LogToTextFile('TSQLRestServerDB.EngineExecuteAll Error: '+RawUTF8(E.Message)+#13#10+aSQL);
      {$endif}
      result := false;
    end;
  end;
end;

function TSQLRestServerDB.MainEngineList(const SQL: RawUTF8; ForceAJAX: Boolean;
  ReturnedRowCount: PPtrInt): RawUTF8;
var Req: PSQLRequest;
    MS: TRawByteStringStream;
    RowCount: integer;
begin
  result := '';
  RowCount := 0;
  if (self<>nil) and (DB<>nil) and (SQL<>'') then begin
    // need a SQL request for R.Execute() to prepare a statement
    result := DB.LockJSON(SQL,ReturnedRowCount); // lock and try from cache
    if result='' then // Execute request if was not got from cache
    try
      try
        Req := GetAndPrepareStatement(SQL);
        if Req<>nil then begin
          MS := TRawByteStringStream.Create;
          try
            try
              RowCount := Req^.Execute(0,'',MS,ForceAJAX or (not NoAJAXJSON));
              result := MS.DataString;
            finally
              if Req=@fStaticStatement then
                Req^.Close;
            end;
          finally
            MS.Free;
          end;
        end;
      except
        on ESQLite3Exception do
          result := '';
      end;
    finally
      DB.UnLockJSON(result,RowCount);
    end;
  end;
  if ReturnedRowCount<>nil then
    ReturnedRowCount^ := RowCount;
end;

function TSQLRestServerDB.MainEngineRetrieve(TableModelIndex, ID: integer): RawUTF8;
var aSQL: RawUTF8;
begin
  if (ID<0) or (TableModelIndex<0) or (result<>'') then
    exit;
  with Model.TableProps[TableModelIndex] do
    aSQL := FormatUTF8('SELECT % FROM % WHERE RowID=:(%):;',
      [SQL.TableSimpleFields[true,false],Props.SQLTableName,ID]);
  result := EngineList(aSQL,true); // ForceAJAX=true -> '[{...}]'#10
  if result<>'' then
    if IsNotAjaxJSON(pointer(result)) then
      // '{"fieldCount":2,"values":["ID","FirstName"]}'#$A -> ID not found
      result := '' else
      // list '[{...}]'#10 -> object '{...}'
      result := copy(result,2,length(result)-3);
end;

function TSQLRestServerDB.MainEngineRetrieveBlob(TableModelIndex, aID: integer;
  BlobField: PPropInfo; out BlobData: TSQLRawBlob): boolean;
var SQL: RawUTF8;
    Req: PSQLRequest;
begin
  result := false;
  if (aID<0) or (TableModelIndex<0) or not BlobField^.IsBlob then
    exit;
  // retrieve the BLOB using SQL
  try
    SQL := FormatUTF8('SELECT % FROM % WHERE RowID=?;',
      [BlobField^.Name,Model.TableProps[TableModelIndex].Props.SQLTableName]);
    DB.Lock(SQL); // UPDATE for a blob field -> no JSON cache flush, but UI refresh
    try
      Req := fStatementCache.Prepare(SQL);
      with Req^ do
      try
        Bind(1,aID);
        if (FieldCount=1) and (Step=SQLITE_ROW) then begin
          BlobData := FieldBlob(0);
          result := true;
        end;
      finally
        if Req=@fStaticStatement then
          Close;
      end;
    finally
      DB.UnLock;
    end;
  except
    on ESQLite3Exception do
      result := false;
  end;
end;

function TSQLRestServerDB.RetrieveBlobFields(Value: TSQLRecord): boolean;
var Static: TSQLRest;
    SQL: RawUTF8;
    f: integer;
    data: TSQLVar;
begin
  result := false;
  if Value=nil then
    exit;
  Static := GetStaticDataServerOrVirtualTable(PSQLRecordClass(Value)^);
  if Static<>nil then
    result := Static.RetrieveBlobFields(Value) else
    if (DB<>nil) and (Value.ID>0) and (PSQLRecordClass(Value)^<>nil) then
    with Value.RecordProps do
    if BlobFields<>nil then begin
      SQL := FormatUTF8('SELECT % FROM % WHERE ROWID=?;',
        [SQLTableRetrieveBlobFields,Table.RecordProps.SQLTableName]);
      DB.Lock(SQL);
      try
        with fStatementCache.Prepare(SQL)^ do begin
          Bind(1,Value.ID);
          if Step<>SQLITE_ROW then
            exit;
          for f := 0 to high(BlobFields) do begin
            SQlite3ValueToSQLVar(FieldValue(f),data);
            BlobFields[f].SetFieldSQLVar(Value,data); // OK for all blobs
          end;
          result := true;
        end;
      finally
        DB.UnLock;
      end;
    end;
end;

procedure TSQLRestServerDB.SetNoAJAXJSON(const Value: boolean);
begin
  inherited;
  if Value=NoAJAXJSON then exit;
  fDB.Cache.Reset; // we changed the JSON format -> cache must be updated
end;

function TSQLRestServerDB.MainEngineUpdate(TableModelIndex, ID: integer;
  const SentData: RawUTF8): boolean;
begin
  if (TableModelIndex<0) or (ID<=0) then
    result := false else
  if SentData='' then // update with no simple field -> valid no-op
    result := true else begin
    // this SQL statement use :(inlined params): for all values
    result := EngineExecuteFmt('UPDATE % SET % WHERE RowID=:(%):;',
      [Model.TableProps[TableModelIndex].Props.SQLTableName,
       GetJSONObjectAsSQL(SentData,true,true),ID]);
    InternalUpdateEvent(seUpdate,Model.Tables[TableModelIndex],ID,nil);
  end;
end;

function TSQLRestServerDB.MainEngineUpdateBlob(TableModelIndex, aID: integer;
  BlobField: PPropInfo; const BlobData: TSQLRawBlob): boolean;
var SQL: RawUTF8;
    AffectedField: TSQLFieldBits;
begin
  if (aID<0) or (TableModelIndex<0) or not BlobField^.IsBlob then
    result := false else
  with Model.TableProps[TableModelIndex].Props do
  try
    SQL := FormatUTF8('UPDATE % SET %=? WHERE RowID=?;',
             [SQLTableName,BlobField^.Name]);
    DB.Lock(SQL); // UPDATE for a blob field -> no JSON cache flush, but UI refresh
    try
      with fStatementCache.Prepare(SQL)^ do begin
        Bind(1,pointer(BlobData),length(BlobData));
        Bind(2,aID);
        repeat
        until Step<>SQLITE_ROW; // Execute all steps of the first statement
        result := true;
      end;
    finally
      DB.UnLock;
    end;
    FieldIndexsFromBlobField(BlobField,AffectedField);
    InternalUpdateEvent(seUpdateBlob,Table,aID,@AffectedField);
  except
    on ESQLite3Exception do
      result := false;
  end;
end;

function TSQLRestServerDB.MainEngineUpdateField(TableModelIndex: integer;
  const SetFieldName, SetValue, WhereFieldName, WhereValue: RawUTF8): boolean;
var WhereID: integer;
begin
  result := false;
  if (TableModelIndex<0) or (SetFieldName='') then
    exit;
  with Model.TableProps[TableModelIndex].Props do
  if Fields.IndexByName(SetFieldName)>=0 then begin
    WhereID := 0;
    if IsRowID(pointer(WhereFieldName)) then begin
      WhereID := GetInteger(Pointer(WhereValue));
      if (WhereID<=0) or not RecordCanBeUpdated(Table,WhereID,seUpdate) then
        exit; // limitation: will only check for update from RowID
    end else
      if Fields.IndexByName(WhereFieldName)<0 then
        exit;
    result := EngineExecuteFmt('UPDATE % SET %=:(%): WHERE %=:(%):',
      [SQLTableName,SetFieldName,SetValue,WhereFieldName,WhereValue]);
    if WhereID>0 then
      InternalUpdateEvent(seUpdate,Table,WhereID,nil);
  end;
end;

function TSQLRestServerDB.UpdateBlobFields(Value: TSQLRecord): boolean;
var Static: TSQLRest;
    SQL: RawUTF8;
    f: integer;
    data: TSQLVar;
    temp: RawByteString;
begin
  result := false;
  if Value=nil then
    exit;
  Static := GetStaticDataServerOrVirtualTable(PSQLRecordClass(Value)^);
  if Static<>nil then
    result := Static.UpdateBlobFields(Value) else
    if (DB<>nil) and (Value.ID>0) and (PSQLRecordClass(Value)^<>nil) then
    with Value.RecordProps do
    if BlobFields<>nil then begin
      SQL := FormatUTF8('UPDATE % SET % WHERE ROWID=?;',
        [SQLTableName,SQLTableUpdateBlobFields]);
      DB.Lock(SQL); // UPDATE for all blob fields -> no cache flush, but UI refresh
      try
        with fStatementCache.Prepare(SQL)^ do begin
          for f := 1 to length(BlobFields) do begin
            BlobFields[f-1].GetFieldSQLVar(Value,data,temp); // OK for all blobs
            if data.VType=ftBlob then
              Bind(f,data.VBlob,data.VBlobLen) else
              BindNull(f); // not possible (BlobFields[] are TSQLPropInfoRTTIRawBlob)
          end;
          Bind(length(BlobFields)+1,Value.ID);
          repeat
          until Step<>SQLITE_ROW; // Execute all steps of the first statement
          result := true;
        end;
      finally
        DB.UnLock;
      end;
      InternalUpdateEvent(seUpdateBlob,PSQLRecordClass(Value)^,Value.ID,@BlobFieldsBits);
    end else
      result := true; // as TSQLRest.UpdateblobFields()
end;

procedure TSQLRestServerDB.Commit(SessionID: cardinal=1);
begin
  inherited Commit(SessionID); // reset fTransactionActive + write all TSQLVirtualTableJSON
  try
    DB.Commit;
  except
    on ESQLite3Exception do
      ; // just catch exception
  end;
end;

procedure TSQLRestServerDB.RollBack(SessionID: cardinal=1);
begin
  inherited; // reset TSQLRestServerDB.fTransactionActive flag
  try
    DB.RollBack; // reset TSQLDataBase.RollBack
  except
    on ESQLite3Exception do
      ; // just catch exception
  end;
end;

function TSQLRestServerDB.TransactionBegin(aTable: TSQLRecordClass; SessionID: cardinal=1): boolean;
begin
  result := inherited TransactionBegin(aTable,SessionID);
  if result then
    // fTransactionActive flag was not already set
    try
      DB.TransactionBegin;
    except
      on ESQLite3Exception do
        result := false;
    end;
end;

function TSQLRestServerDB.Backup(Dest: TStream): boolean;
{$ifdef CPU64} // currently not working on Win64 - never mind
begin
  result := false;
end;
{$else}
var Source: TFileStream;
    Closed: boolean;
    user_version: cardinal;
begin
  result := false;
  if (Self=nil) or (DB=nil) then
    exit;
  fStatementCache.ReleaseAllDBStatements;
  user_version := DB.user_version;
  DB.LockAndFlushCache;
  try
  try
    // perform a VACCUM to recreate the database content
    EngineExecute('VACUUM');
    Closed := false;
    try
      Closed := DB.DBClose=SQLITE_OK;
      // compress the database content file
      Source := TFileStream.Create(DB.FileName,fmOpenRead or fmShareDenyNone);
      try
        Dest.CopyFrom(Source,0);  // Count=0 for whole stream copy
        result := true;
      finally
        Source.Free;
      end;
    finally
      if Closed then begin
        DB.DBOpen; // reopen the database if was previously closed
        FreeAndNil(fRegisteredVirtualTableModules); // force register modules
        InitializeEngine;                   // register functions and modules
        CreateMissingTables(user_version);  // register virtual tables
      end;
    end;
  finally
    DB.UnLock;
  end;
  except
    on E: Exception do
      result := false;
  end;
end;
{$endif}

function TSQLRestServerDB.BackupGZ(const DestFileName: TFileName; CompressionLevel: integer): boolean;
var D,Z: TStream;
begin
  try
    D := TFileStream.Create(DestFileName,fmCreate);
    try
      Z := TSynZipCompressor.Create(D,CompressionLevel,szcfGZ);
      try
        result := Backup(Z);
      finally
        Z.Free;
      end;
    finally
      D.Free;
    end;
  except
    result := false;
  end;
end;

function TSQLRestServerDB.RestoreGZ(const BackupFileName: TFileName): boolean;
begin
  try
    with TSynMemoryStreamMapped.Create(BackupFileName) do
    try
      result := Restore(GZRead(Memory,Size));
    finally
      Free;
    end;
  except
    on Exception do
      result := false;
  end;
end;

function TSQLRestServerDB.Restore(const ContentToRestore: RawByteString): boolean;
{$ifdef CPU64}
begin
  result := false; // life backup/restore does not work with current sqlite3-64.dll
end;
{$else}
var BackupFileName: TFileName;
    user_version: cardinal;
begin
  result := false;
  if (Self=nil) or (DB=nil) or
     not IdemPChar(pointer(ContentToRestore),'SQLITE FORMAT 3') then
    exit; // invalid restore content
  user_version := DB.user_version;
  DB.LockAndFlushCache;
  try
    try
      fStatementCache.ReleaseAllDBStatements;
      if DB.DBClose<>SQLITE_OK then
        exit; // impossible to close DB.FileName (some statement may be opened)
      BackupFileName := ChangeFileExt(DB.FileName,'.bak');
      DeleteFile(BackupFileName);
      try
        if MoveFile(pointer(DB.FileName),pointer(BackupFileName)) then
          if FileFromString(ContentToRestore,DB.FileName,true) and
             (StringFromFile(DB.FileName)=ContentToRestore) then
            result := (DB.DBOpen=SQLITE_OK);
      finally
        if result then
          DeleteFile(BackupFileName) else begin
          // on error, restore previous db file
          DeleteFile(DB.FileName);
          MoveFile(pointer(BackupFileName),pointer(DB.FileName));
          DB.DBOpen; // always reopen the database
        end;
        FreeAndNil(fRegisteredVirtualTableModules); // force register modules
        InitializeEngine;                   // register functions and modules
        CreateMissingTables(user_version);  // register virtual tables
      end;
    finally
      DB.UnLock;
    end;
  except
    on E: Exception do
      result := false;
  end;
end;
{$endif}

procedure TSQLRestServerDB.FlushInternalDBCache;
begin
  inherited;
  if DB<>nil then
    DB.CacheFlush;
end;


{ TSQLRestClientDB }

constructor TSQLRestClientDB.Create(aClientModel, aServerModel: TSQLModel; aDB: TSQLDataBase;
   aServerClass: TSQLRestServerDBClass; aHandleUserAuthentication: boolean);
begin
  aDB.UseCache := true;      // we better use caching in this JSON oriented use
  inherited Create(aClientModel);
  if aServerModel=nil then
    aServerModel := TSQLModel.Create(aClientModel); // clone from client
  // next line will create aModel tables if necessary
  fOwnedServer := aServerClass.Create(aServerModel,aDB,aHandleUserAuthentication);
  fServer := fOwnedServer;
  fServer.NoAJAXJSON := true; // use smaller JSON size in this local use (never AJAX)
end;

constructor TSQLRestClientDB.Create(aClientModel, aServerModel: TSQLModel; const aDBFileName: TFileName;
  aServerClass: TSQLRestServerDBClass; aHandleUserAuthentication: boolean; const aPassword: RawUTF8);
begin
  fOwnedDB := TSQLDatabase.Create(aDBFileName,aPassword);
  Create(aClientModel,aServerModel,fOwnedDB,aServerClass,aHandleUserAuthentication);
end;

constructor TSQLRestClientDB.Create(aRunningServer: TSQLRestServerDB); 
var ClientModel: TSQLModel;
begin
  if aRunningServer=nil then
    raise EORMException.Create('TSQLRestClientDB.Create(nil)');
  ClientModel := TSQLModel.Create(aRunningServer.Model);
  ClientModel.Owner := Self; // auto-free ClientModel in TSQLRest.Destroy
  inherited Create(ClientModel);
  fServer := aRunningServer; // leave fOwnedServer=nil
end;

destructor TSQLRestClientDB.Destroy;
var M: TSQLModel;
begin
  try
    inherited Destroy; // UnLock records + SessionClose
  finally
    if fOwnedServer<>nil then begin
      if fServer=nil then
        M := nil else
        M := fServer.Model;
      if (M<>nil) and (M.Owner<>nil) then
        M := nil; // free associated model only if it's owned by nobody
      try
        FreeAndNil(fOwnedServer);
        fServer := nil;
      finally
        M.Free;
        fOwnedDB.Free;
      end;
    end;
  end;
end;

function TSQLRestClientDB.getDB: TSQLDataBase;
begin
  result := fServer.DB;
end;

function TSQLRestClientDB.List(const Tables: array of TSQLRecordClass;
  const SQLSelect, SQLWhere: RawUTF8): TSQLTableJSON;
var aSQL: RawUTF8;
    n: integer;
begin
  result := nil;
  n := length(Tables);
  if (self<>nil) and (n>0) then
  try // will use JSON cache if available:
    aSQL := Model.SQLFromSelectWhere(Tables,SQLSelect,SQLWhere);
    if n=1 then
      // InternalListJSON will handle both static and DB tables
      result := fServer.ExecuteList(Tables,aSQL) else
      // we access localy the DB -> TSQLTableDB handle Tables parameter
      result := TSQLTableDB.Create(fServer.DB,Tables,aSQL,not fServer.NoAJAXJSON);
    if fServer.DB.InternalState<>nil then
      result.InternalState := fServer.DB.InternalState^;
  except
    on ESQLite3Exception do
      result := nil;
  end;
end;

procedure TSQLRestClientDB.InternalURI(var call: TSQLRestURIParams);
begin
  if fInternalHeader='' then
    fInternalHeader := 'RemoteIP: 127.0.0.1'#13#10'ConnectionID: '+PointerToHex(self);
  if call.InHead<>'' then
    call.InHead := call.InHead+#13#10+fInternalHeader else
    call.InHead := fInternalHeader;
  call.RestAccessRights := @FULL_ACCESS_RIGHTS;
  fServer.URI(call);
  if (call.OutInternalState=0) and (fServer.DB.InternalState<>nil) then
    call.OutInternalState := fServer.DB.InternalState^; // manual update if necessary
end;

function TSQLRestClientDB.InternalCheckOpen: boolean;
begin
  result := true;
end;

procedure TSQLRestClientDB.InternalClose;
begin
end;


{ TSQLVirtualTableModuleSQLite3 }

const
  NULCHAR: AnsiChar = #0;

function SQLVarToSQlite3Context(const Res: TSQLVar; Context: TSQLite3FunctionContext): boolean;
var tmp: array[0..31] of AnsiChar;
begin
  case Res.VType of
    ftNull:
      sqlite3.result_null(Context);
    ftInt64:
      sqlite3.result_int64(Context,Res.VInt64);
    ftDouble:
      sqlite3.result_double(Context,Res.VDouble);
    ftCurrency:
      sqlite3.result_double(Context,Res.VCurrency);
    ftDate: begin
      DateTimeToIso8601ExpandedPChar(Res.VDateTime,tmp);
      sqlite3.result_text(Context,tmp,-1,SQLITE_TRANSIENT_VIRTUALTABLE);
    end;
    // WARNING! use pointer(integer(-1)) instead of SQLITE_TRANSIENT=pointer(-1)
    // due to a bug in Sqlite3 current implementation of virtual tables in Win64
    ftUTF8:
      if Res.VText=nil then
       sqlite3.result_text(Context,@NULCHAR,0,SQLITE_STATIC) else
       sqlite3.result_text(Context,Res.VText,-1,SQLITE_TRANSIENT_VIRTUALTABLE);
    ftBlob:
      sqlite3.result_blob(Context,Res.VBlob,Res.VBlobLen,SQLITE_TRANSIENT_VIRTUALTABLE);
    else begin
      {$ifdef WITHLOG}
      SynSQLite3Log.DebuggerNotify([ord(Res.VType)],'SQLVarToSQlite3Context(%)');
      {$endif}
      result := false; // not handled type
      exit;
    end;
  end;
  result := true;
end;

procedure SQlite3ValueToSQLVar(Value: TSQLite3Value; var Res: TSQLVar);
var ValueType: Integer;
begin
  ValueType := sqlite3.value_type(Value);
  case ValueType of
  SQLITE_NULL:
    Res.VType := ftNull;
  SQLITE_INTEGER: begin
    Res.VType := ftInt64;
    Res.VInt64 := sqlite3.value_int64(Value);
  end;
  SQLITE_FLOAT: begin
    Res.VType := ftDouble;
    Res.VDouble := sqlite3.value_double(Value);
  end;
  SQLITE_TEXT:  begin
    Res.VType := ftUTF8;
    Res.VText := sqlite3.value_text(Value);
  end;
  SQLITE_BLOB: begin
    Res.VType := ftBlob;
    Res.VBlobLen := sqlite3.value_bytes(Value);
    Res.VBlob := sqlite3.value_blob(Value);
  end;
  else begin
    {$ifdef WITHLOG}
    SynSQLite3Log.DebuggerNotify([ValueType],'SQlite3ValueToSQLVar(%)');
    {$endif}
    Res.VType := ftUnknown;
  end;
  end;
end;

function TSQLVirtualTableModuleSQLite3.FileName(const aTableName: RawUTF8): TFileName;
begin
  if FilePath<>'' then
    // if a file path is specified (e.g. by SynDBExplorer) -> always use this
    result := inherited FileName(aTableName) else
  if SameText(DB.FileName,SQLITE_MEMORY_DATABASE_NAME) then
    // in-memory databases virtual tables should remain in memory
    result := '' else
    // change file path to current DB folder
    result := ExtractFilePath(DB.FileName)+ExtractFileName(inherited FileName(aTableName));
end;

function vt_Create(DB: TSQLite3DB; pAux: Pointer;
  argc: Integer; const argv: PPUTF8CharArray;
  var ppVTab: PSQLite3VTab; var pzErr: PUTF8Char): Integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif}
var Module: TSQLVirtualTableModuleSQLite3 absolute pAux;
    Table: TSQLVirtualTable;
    Structure: RawUTF8;
    ModuleName: RawUTF8;
begin
  if Module<>nil then
    ModuleName := Module.ModuleName;
  if (Module=nil) or (Module.DB.DB<>DB) or
     (StrIComp(pointer(ModuleName),argv[0])<>0) then begin
    {$ifdef WITHLOG}
    SynSQLite3Log.DebuggerNotify([argv[0],ModuleName],'vt_Create(%<>%)');
    {$endif}
    result := SQLITE_ERROR;
    exit;
  end;
  ppVTab := sqlite3.malloc(sizeof(TSQLite3VTab));
  if ppVTab=nil then begin
    result := SQLITE_NOMEM;
    exit;
  end;
  fillchar(ppVTab^,sizeof(ppVTab^),0);
  try
    Table := Module.TableClass.Create(Module,RawUTF8(argv[2]),argc-3,@argv[3]);
  except
    on E: Exception do begin
      ExceptionToSqlite3Err(E,pzErr);
      sqlite3.free_(ppVTab);
      result := SQLITE_ERROR;
      exit;
    end;
  end;
  Structure := Table.Structure;
  result := sqlite3.declare_vtab(DB,pointer(Structure));
  if result<>SQLITE_OK then begin
    {$ifdef WITHLOG}
    SynSQLite3Log.DebuggerNotify([ModuleName,Structure],'vt_Create(%) declare_vtab(%)');
    {$endif}
    Table.Free;
    sqlite3.free_(ppVTab);
    result := SQLITE_ERROR;
  end else
    ppVTab^.pInstance := Table;
end;

function vt_Disconnect(pVTab: PSQLite3VTab): Integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif}
begin
  TSQLVirtualTable(pvTab^.pInstance).Free; 
  sqlite3.free_(pVTab);
  result := SQLITE_OK;
end;

function vt_Destroy(pVTab: PSQLite3VTab): Integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif}
begin
  if TSQLVirtualTable(pvTab^.pInstance).Drop then
    result := SQLITE_OK else begin
    {$ifdef WITHLOG}
    SynSQLite3Log.DebuggerNotify([],'vt_Destroy');
    {$endif}
    result := SQLITE_ERROR;
  end;
  vt_Disconnect(pVTab); // release memory
end;

function vt_BestIndex(var pVTab: TSQLite3VTab; var pInfo: TSQLite3IndexInfo): Integer;
  {$ifndef SQLITE3_FASTCALL}cdecl;{$endif}
var Prepared: PSQLVirtualTablePrepared;
    Table: TSQLVirtualTable;
    i, n: Integer;
begin
  result := SQLITE_ERROR;
  Table := TSQLVirtualTable(pvTab.pInstance);
  if (cardinal(pInfo.nOrderBy)>MAX_SQLFIELDS) or
     (cardinal(pInfo.nConstraint)>MAX_SQLFIELDS) then begin
    {$ifdef WITHLOG}
    SynSQLite3Log.DebuggerNotify([pInfo.nOrderBy,pInfo.nConstraint],'nOrderBy=% nConstraint=%');
    {$endif}
    exit; // avoid buffer overflow
  end;
  Prepared := sqlite3.malloc(sizeof(TSQLVirtualTablePrepared));
  try
    // encode the incoming parameters into Prepared^ record
    fillchar(Prepared^,sizeof(Prepared^),0);
    Prepared^.WhereCount := pInfo.nConstraint;
    for i := 0 to pInfo.nConstraint-1 do
    with Prepared^.Where[i], pInfo.aConstraint^[i] do
    if usable then begin
      Column := iColumn;
      case op of
        SQLITE_INDEX_CONSTRAINT_EQ:    Operation := soEqualTo;
        SQLITE_INDEX_CONSTRAINT_GT:    Operation := soGreaterThan;
        SQLITE_INDEX_CONSTRAINT_LE:    Operation := soLessThanOrEqualTo;
        SQLITE_INDEX_CONSTRAINT_LT:    Operation := soLessThan;
        SQLITE_INDEX_CONSTRAINT_GE:    Operation := soGreaterThanOrEqualTo;
        SQLITE_INDEX_CONSTRAINT_MATCH: Operation := soBeginWith;
        else exit; // invalid parameter
      end;
    end else
      Column := VIRTUAL_TABLE_IGNORE_COLUMN;
    assert(sizeof(TSQLVirtualTablePreparedOrderBy)=sizeof(TSQLite3IndexOrderBy));
    if pInfo.nOrderBy>0 then begin
      Prepared^.OrderByCount := pInfo.nOrderBy;
      Move(pInfo.aOrderBy^[0],Prepared^.OrderBy[0],pInfo.nOrderBy*sizeof(Prepared^.OrderBy[0]));
    end;
    // perform the index query
    if not Table.Prepare(Prepared^) then
      exit;
    // update pInfo and store Prepared into pInfo.idxStr for vt_Filter()
    n := 0;
    for i := 0 to pInfo.nConstraint-1 do
    if Prepared^.Where[i].Value.VType<>ftUnknown then begin
      if i<>n then // expression needed for Search() method to be moved at [n]
        move(Prepared^.Where[i],Prepared^.Where[n],sizeof(Prepared^.Where[i]));
      inc(n);
      pInfo.aConstraintUsage[i].argvIndex := n;
      pInfo.aConstraintUsage[i].omit := Prepared^.Where[i].OmitCheck;
    end;
    Prepared^.WhereCount := n; // will match argc in vt_Filter()
    pInfo.orderByConsumed := integer(Prepared^.OmitOrderBy);
    pInfo.estimatedCost := Prepared^.EstimatedCost;
    pInfo.idxStr := pointer(Prepared);
    pInfo.needToFreeIdxStr := 1; // will do sqlite3.free(idxStr) when needed
    result := SQLITE_OK;
  finally
    if result<>SQLITE_OK then
      sqlite3.free_(Prepared); // avoid memory leak on error
  end;
end;

function vt_Filter(var pVtabCursor: TSQLite3VTabCursor; idxNum: Integer; const idxStr: PAnsiChar;
   argc: Integer; var argv: TSQLite3ValueArray): Integer;
  {$ifndef SQLITE3_FASTCALL}cdecl;{$endif}
var Prepared: PSQLVirtualTablePrepared absolute idxStr; // idxNum is not used
    i: integer;
begin
  result := SQLITE_ERROR;
  if Prepared^.WhereCount<>argc then
    exit; // invalid prepared array
  for i := 0 to argc-1 do
    SQlite3ValueToSQLVar(argv[i],Prepared^.Where[i].Value);
  if TSQLVirtualTableCursor(pVtabCursor.pInstance).Search(Prepared^) then
    result := SQLITE_OK else
    {$ifdef WITHLOG}
    SynSQLite3Log.DebuggerNotify([],'vt_Filter');
    {$endif}
end;

function vt_Open(var pVTab: TSQLite3VTab; var ppCursor: PSQLite3VTabCursor): Integer;
  {$ifndef SQLITE3_FASTCALL}cdecl;{$endif}
var Table: TSQLVirtualTable;
begin
  ppCursor := sqlite3.malloc(sizeof(TSQLite3VTabCursor));
  if ppCursor=nil then begin
    result := SQLITE_NOMEM;
    exit;
  end;
  Table := TSQLVirtualTable(pvTab.pInstance);
  if (Table=nil) or (Table.Module=nil) or (Table.Module.CursorClass=nil) then begin
    {$ifdef WITHLOG}
    SynSQLite3Log.DebuggerNotify([],'vt_Open');
    {$endif}
    sqlite3.free_(ppCursor);
    result := SQLITE_ERROR;
    exit;
  end;
  ppCursor.pInstance := Table.Module.CursorClass.Create(Table);
  result := SQLITE_OK;
end;

function vt_Close(pVtabCursor: PSQLite3VTabCursor): Integer;
  {$ifndef SQLITE3_FASTCALL}cdecl;{$endif}
begin
  TSQLVirtualTableCursor(pVtabCursor^.pInstance).Free;
  sqlite3.free_(pVtabCursor);
  result := SQLITE_OK;
end;

function vt_next(var pVtabCursor: TSQLite3VTabCursor): Integer;
  {$ifndef SQLITE3_FASTCALL}cdecl;{$endif}
begin
  if TSQLVirtualTableCursor(pVtabCursor.pInstance).Next then
    result := SQLITE_OK else
    result := SQLITE_ERROR;
end;

function vt_Eof(var pVtabCursor: TSQLite3VTabCursor): Integer;
  {$ifndef SQLITE3_FASTCALL}cdecl;{$endif}
begin
  result := integer(not TSQLVirtualTableCursor(pVtabCursor.pInstance).HasData);
end;

function vt_Column(var pVtabCursor: TSQLite3VTabCursor; sContext: TSQLite3FunctionContext;
  N: Integer): Integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif}
var Res: TSQLVar;
begin
  Res.VType := ftUnknown;
  if (N>=0) and TSQLVirtualTableCursor(pVtabCursor.pInstance).Column(N,Res) and
     SQLVarToSQlite3Context(Res,sContext) then
    result := SQLITE_OK else begin
    {$ifdef WITHLOG}
    SynSQLite3Log.DebuggerNotify([N,ord(Res.VType)],'vt_Column(%) Res=%');
    {$endif}
    result := SQLITE_ERROR;
  end;
end;

function vt_Rowid(var pVtabCursor: TSQLite3VTabCursor; var pRowid: Int64): Integer;
  {$ifndef SQLITE3_FASTCALL}cdecl;{$endif}
var Res: TSQLVar;
begin
  result := SQLITE_ERROR;
  with TSQLVirtualTableCursor(pVtabCursor.pInstance) do
  if Column(-1,Res) then begin
    case Res.VType of
    ftInt64:    pRowID := Res.VInt64;
    ftDouble:   pRowID := trunc(Res.VDouble);
    ftCurrency: pRowID := trunc(Res.VCurrency);
    ftUTF8:     pRowID := GetInt64(Res.VText);
    else begin
      {$ifdef WITHLOG}
      SynSQLite3Log.DebuggerNotify([ord(Res.VType)],'vt_Rowid Res=%');
      {$endif}
      exit;
    end;
    end;
    result := SQLITE_OK;
  end else begin
    {$ifdef WITHLOG}
    SynSQLite3Log.DebuggerNotify([],'vt_Rowid Column');
    {$endif}
  end;
end;

function vt_Update(var pVTab: TSQLite3VTab;
  nArg: Integer; var ppArg: TSQLite3ValueArray;
  var pRowid: Int64): Integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif}
var Values: TSQLVarDynArray;
    Table: TSQLVirtualTable;
    RowID0, RowID1: Int64;
    i: integer;
    OK: boolean;
begin // call Delete/Insert/Update methods according to supplied parameters
  Table := TSQLVirtualTable(pvTab.pInstance);
  result := SQLITE_ERROR;
  if (nArg<=0) or (nArg>1024) then
    exit;
  case sqlite3.value_type(ppArg[0]) of
    SQLITE_INTEGER: RowID0 := sqlite3.value_int64(ppArg[0]);
    SQLITE_NULL:    RowID0 := 0;
    else exit; // invalid call
  end;
  if nArg=1 then
    OK := Table.Delete(RowID0) else begin
    case sqlite3.value_type(ppArg[1]) of
      SQLITE_INTEGER: RowID1 := sqlite3.value_int64(ppArg[1]);
      SQLITE_NULL:    RowID1 := 0;
      else exit; // invalid call
    end;
    SetLength(Values,nArg-2);
    for i := 0 to nArg-3 do
      SQlite3ValueToSQLVar(ppArg[i+2],Values[i]);
    if RowID0=0 then
      OK := Table.Insert(RowID1,Values,pRowid) else
      OK := Table.Update(RowID0,RowID1,Values);
  end;
  if OK then
    result := SQLITE_OK else begin
    {$ifdef WITHLOG}
    SynSQLite3Log.DebuggerNotify([pRowID],'vt_Update(%)');
    {$endif}
  end;
end;

function InternalTrans(pVTab: TSQLite3VTab; aState: TSQLVirtualTableTransaction;
  aSavePoint: integer): integer;
begin
  if TSQLVirtualTable(pvTab.pInstance).Transaction(aState,aSavePoint) then
    result := SQLITE_OK else begin
    {$ifdef WITHLOG}
    SynSQLite3Log.DebuggerNotify([GetEnumName(TypeInfo(TSQLVirtualTableTransaction),
      ord(aState))^,aSavePoint],'Transaction(%,%)');
    {$endif}
    result := SQLITE_ERROR;
  end;
end;

function vt_Begin(var pVTab: TSQLite3VTab): Integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif}
begin
  result := InternalTrans(pVTab,vttBegin,0);
end;

function vt_Commit(var pVTab: TSQLite3VTab): Integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif}
begin
  result := InternalTrans(pVTab,vttCommit,0);
end;

function vt_RollBack(var pVTab: TSQLite3VTab): Integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif}
begin
  result := InternalTrans(pVTab,vttRollBack,0);
end;

function vt_Sync(var pVTab: TSQLite3VTab): Integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif}
begin
  result := InternalTrans(pVTab,vttSync,0);
end;

function vt_SavePoint(var pVTab: TSQLite3VTab; iSavepoint: integer): Integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif}
begin
  result := InternalTrans(pVTab,vttSavePoint,iSavePoint);
end;

function vt_Release(var pVTab: TSQLite3VTab; iSavepoint: integer): Integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif}
begin
  result := InternalTrans(pVTab,vttRelease,iSavePoint);
end;

function vt_RollBackTo(var pVTab: TSQLite3VTab; iSavepoint: integer): Integer; {$ifndef SQLITE3_FASTCALL}cdecl;{$endif}
begin
  result := InternalTrans(pVTab,vttRollBackTo,iSavePoint);
end;

function vt_Rename(var pVTab: TSQLite3VTab; const zNew: PAnsiChar): Integer;
  {$ifndef SQLITE3_FASTCALL}cdecl;{$endif}
begin
  if TSQLVirtualTable(pvTab.pInstance).Rename(RawUTF8(zNew)) then
    result := SQLITE_OK else begin
    {$ifdef WITHLOG}
    SynSQLite3Log.DebuggerNotify([zNew],'vt_Rename(%)');
    {$endif}
    result := SQLITE_ERROR;
  end;
end;

procedure sqlite3InternalFreeModule(p: pointer); {$ifndef SQLITE3_FASTCALL}cdecl;{$endif}
begin
  if (p<>nil) and (TSQLVirtualTableModuleSQLite3(p).fDB<>nil) then
    TSQLVirtualTableModuleSQLite3(p).Free;
end;

procedure TSQLVirtualTableModuleSQLite3.Attach(aDB: TSQLDataBase);
begin
  if aDB=nil then
    raise EBusinessLayerException.CreateFmt('aDB=nil at %s.SetDB()',[ClassName]);
  if fDB<>nil then
    raise EBusinessLayerException.CreateFmt('fDB<>nil at %s.SetDB()',[ClassName]);
  fillchar(fModule,sizeof(fModule),0);
  fModule.iVersion := 1;
  fModule.xCreate := vt_Create;
  fModule.xConnect := vt_Create;
  fModule.xBestIndex := vt_BestIndex;
  fModule.xDisconnect := vt_Disconnect;
  fModule.xDestroy := vt_Destroy;
  fModule.xOpen := vt_Open;
  fModule.xClose := vt_Close;
  fModule.xFilter := vt_Filter;
  fModule.xNext := vt_Next;
  fModule.xEof := vt_Eof;
  fModule.xColumn := vt_Column;
  fModule.xRowid := vt_Rowid;
  if vtWrite in Features then begin
    fModule.xUpdate := vt_Update;
    if vtTransaction in Features then begin
      fModule.xBegin := vt_Begin;
      fModule.xSync := vt_Sync;
      fModule.xCommit := vt_Commit;
      fModule.xRollback := vt_RollBack;
    end;
    if vtSavePoint in Features then begin
      fModule.iVersion := 2;
      fModule.xSavePoint := vt_SavePoint;
      fModule.xRelease := vt_Release;
      fModule.xRollBackTo := vt_RollBackTo;
    end;
    fModule.xRename := vt_Rename;
  end;
  sqlite3_check(aDB.DB,sqlite3.create_module_v2(aDB.DB,pointer(fModuleName),
    fModule,self,sqlite3InternalFreeModule)); // raise ESQLite3Exception on error
  fDB := aDB; // mark successfull create_module() for sqlite3InternalFreeModule
end;


{ TSQLVirtualTableModuleServerDB }

constructor TSQLVirtualTableModuleServerDB.Create(
  aClass: TSQLVirtualTableClass; aServer: TSQLRestServer);
begin
  if not aServer.InheritsFrom(TSQLRestServerDB) then
    raise EBusinessLayerException.CreateFmt('%.Create expects a DB Server',[ClassName]);
  inherited;
  Attach(TSQLRestServerDB(aServer).DB);
  // any exception in Attach() will let release the instance by the RTL
end;

destructor TSQLVirtualTableModuleServerDB.Destroy;
var i: integer;
begin
  if fServer<>nil then
    with fServer as TSQLRestServerDB do
    if fRegisteredVirtualTableModules<>nil then begin
      i := fRegisteredVirtualTableModules.IndexOf(self);
      if i>=0 then
        fRegisteredVirtualTableModules.Delete(i);
    end;
  inherited;
end;


function RegisterVirtualTableModule(aModule: TSQLVirtualTableClass; aDatabase: TSQLDataBase): TSQLVirtualTableModule;
begin
  result := TSQLVirtualTableModuleSQLite3.Create(aModule,nil);
  try
    TSQLVirtualTableModuleSQLite3(result).Attach(aDatabase);
  except
    on Exception do begin
      result.Free; // should be released by hand here
      raise; // e.g. EBusinessLayerException or ESQLite3Exception
    end;
  end;
end;

initialization
  {$ifdef WITHLOG}
  // all our SynSQlite3 related functions shall log to main TSQLLog
  SynSQLite3Log := TSQLLog;
  {$endif}
end.



