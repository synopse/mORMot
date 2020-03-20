/// PostgreSQL direct access classes for SynDB units (not DB.pas based)
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license - see LICENSE.md
unit SynDBPostgres;

{
  *****************************************************************************
   Implementation of TSQLDB* for PostgreSQL using libpg

   Features:
    - fast, minimum memory allocation
    - includes its own simple wrapper to the libpq native client
    - array binding for select statements (caller should use =ANY(?) etc.)

   Limitations:
    - works with PostgreSQL>=7.4 and (v3 protocol)
    - libpg>=8.3 is required (PQunescapeBytea)
    - consider creating the database with UTF8 collation
    - notifications are not implemented
    - Postgres level prepared statements works only for SQLs what starts
      exactly with SELECT INSERT UPDATE DELETE VALUES and not contains ";"
    - parameter parser will fails in case SQL contains comments with ? inside
      (TODO - will be fixed)
    - all query rows are returned at once, caller should care about pagination
      (TODO - implement singleRowMode?)

  Aim of this unit is to provide simple alternative to SynDBZeos for PostgreSQL
  *****************************************************************************
}

interface

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

uses
  {$ifdef MSWINDOWS}
  Windows,
  {$else}
  dynlibs,
  {$endif}
  SysUtils,
  SynCommons,
  SynTable,
  SynDB;

type
  /// exception type associated to the native libpg Interface
  ESQLDBPostgres = class(ESQLDBException);

  /// connection properties which will implement an internal Thread-Safe
  // connection pool
  TSQLDBPostgresConnectionProperties = class(TSQLDBConnectionPropertiesThreadSafe)
  private
    fOids: TCardinalDynArray; // fast O(n) search in L1 cache
    fOidsFieldTypes: TSQLDBFieldTypeDynArray;
    fOidsCount: integer;
  protected
    procedure GetForeignKeys; override;
    /// fill mapping of OID
    // - at runtime mapping can be defined using Oid2FieldType() method
    // - OIDs defined in DB can be retrieved using query
    //  "select oid, typname from pg_type where typtype = 'b' order by oid"
    procedure FillOidMapping; virtual;
  public
    /// initialize the properties
    // - raise an exception in case libpg is not thead-safe
    // - aDatabaseName can be a Connection URI - see
    // https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING
    // - if aDatabaseName contains connection URI with password we recommend to repeat password
    // in aPassword parameter to prevent logging it (see TSQLDBConnectionProperties.DatabaseNameSafe)
    // - better to use environment variables and postgres config file for connection parameters
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassword: RawUTF8); override;
    /// create a new connection
    // - caller is responsible of freeing this instance
    // - this overridden method will create an TSQLDBPostgresConnection instance
    function NewConnection: TSQLDBConnection; override;
    /// add or replace mapping of OID into TSQLDBFieldType
    // - in case mapping for OID is not defined, returns ftUTF8
    function Oid2FieldType(cOID: cardinal): TSQLDBFieldType; {$ifdef HASINLINE} inline; {$endif}
    // add new (or override existed) OID to FieldType mapping
    procedure MapOid(cOid: cardinal; fieldType: TSQLDBFieldType);
  end;


  /// implements a connection via the libpq access layer
  TSQLDBPostgresConnection = class(TSQLDBConnectionThreadSafe)
  protected
    // prepared statement names = SHA-256 of its SQL
    fPrepared: THash256DynArray; // O(n) fast search in L1 cache
    fPreparedCount: integer;
    // the associated low-level provider connection
    fPGConn: pointer;
    // maintain fPrepared[] hash list to identify already cached
    function PrepareCached(const aSQL: RawUTF8; aParamCount: integer;
      out aName: RaWUTF8): integer;
    /// direct execution of SQL statement what do not returns a result
    // - statement should not contains parameters
    // - raise an ESQLDBPostgres on error
    procedure DirectExecSQL(const SQL: RawUTF8);
  public
    /// connect to the specified server
    // - should raise an ESQLDBPostgres on error
    procedure Connect; override;
    /// stop connection to the specified ZEOS database server
    // - should raise an ESQLDBPostgres on error
    procedure Disconnect; override;
    /// return TRUE if Connect has been already successfully called
    function IsConnected: boolean; override;
    /// create a new statement instance
    function NewStatement: TSQLDBStatement; override;
    /// begin a Transaction for this connection
    procedure StartTransaction; override;
    /// commit changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Commit; override;
    /// discard changes of a Transaction for this connection
    // - StartTransaction method must have been called before
    procedure Rollback; override;
    /// direct access to the associated PPGconn connection
    property Direct: pointer read fPGConn;
    /// how many prepared statements are currently cached for this connection
    property PreparedCount: integer read fPreparedCount;
  end;


  /// implements a statement via a Postgres database connection
  TSQLDBPostgresStatement = class(TSQLDBStatementWithParamsAndColumns)
  protected
    fPreparedStmtName: RawUTF8; // = SHA-256 of the SQL
    fParsedSQL: RawUTF8;
    fPreparedParamsCount: longint;
    fRes: pointer;
    fResStatus: integer;
    // pointers to query parameters; initialized by Prepare, filled in Executeprepared
    fPGParams: TPointerDynArray;
    // 0 - text, 1 - binary; initialized by Prepare, filled in Executeprepared
    fPGParamFormats: TIntegerDynArray;
    // non zero for binary params
    fPGparamLengths: TIntegerDynArray;
    /// use
    procedure BindColumns;
    /// raise an exception if Col is out of range according to fColumnCount
    // or rowset is not initialized
    procedure CheckColAndRowset(const Col: integer);
    //reserved for binary protocol fPrmLength: array of Longint;
  public
    /// finalize the statement for a given connection
    destructor Destroy; override;
    /// Prepare an UTF-8 encoded SQL statement
    // - parameters marked as ? will be bound later, before ExecutePrepared call
    // - if ExpectResults is TRUE, then Step() and Column*() methods are available
    // to retrieve the data rows
    // - raise an ESQLDBPostgres on any error
    procedure Prepare(const aSQL: RawUTF8; ExpectResults: boolean = False); overload; override;
    /// Execute a prepared SQL statement
    // - parameters marked as ? should have been already bound with Bind*() functions
    // - this implementation will also handle bound array of values (if any),
    // if IZDatabaseInfo.SupportsArrayBindings is true for this provider
    // - this overridden method will log the SQL statement if sllSQL has been
    // enabled in SynDBLog.Family.Level
    // - raise an ESQLDBPostgres on any error
    procedure ExecutePrepared; override;
    /// gets a number of updates made by latest executed statement
    function UpdateCount: integer; override;
    /// Reset the previous prepared statement
    // - this overridden implementation will reset all bindings and the cursor state
    // - raise an ESQLDBPostgres on any error
    procedure Reset; override;

    /// Access the next or first row of data from the SQL Statement result
    // - return true on success, with data ready to be retrieved by Column*() methods
    // - return false if no more row is available (e.g. if the SQL statement
    // is not a SELECT but an UPDATE or INSERT command)
    // - if SeekFirst is TRUE, will put the cursor on the first row of results
    // - raise an ESQLDBPostgres on any error
    function Step(SeekFirst: boolean = False): boolean; override;
    /// return a Column integer value of the current Row, first Col is 0
    function ColumnInt(Col: integer): int64; override;
    /// returns TRUE if the column contains NULL
    function ColumnNull(Col: integer): boolean; override;
    /// return a Column floating point value of the current Row, first Col is 0
    function ColumnDouble(Col: integer): double; override;
    /// return a Column date and time value of the current Row, first Col is 0
    function ColumnDateTime(Col: integer): TDateTime; override;
    /// return a Column currency value of the current Row, first Col is 0
    function ColumnCurrency(Col: integer): currency; override;
    /// return a Column UTF-8 encoded text value of the current Row, first Col is 0
    function ColumnUTF8(Col: integer): RawUTF8; override;
    /// return a Column as a blob value of the current Row, first Col is 0
    function ColumnBlob(Col: integer): RawByteString; override;
    /// append all columns values of the current Row to a JSON stream
    // - overriden method to avoid temporary memory allocation or conversion
    procedure ColumnsToJSON(WR: TJSONWriter); override;
    /// how many parameters founded during prepare stage
    property PreparedParamsCount: longint read fPreparedParamsCount;
  end;


var
  /// allow to specify a libpq library file name to use
  SynDBPostgresLibrary: TFileName;


implementation

uses
  SynLog,
  SynCrypto; // libpq requires named prepared statements = use SHA-256


{ *********** minimal access to libpq native Postgres client library }

const
  PGRES_EMPTY_QUERY = 0;
  PGRES_COMMAND_OK = 1;
  PGRES_TUPLES_OK = 2;
  PGRES_COPY_OUT = 3;
  PGRES_COPY_IN = 4;
  PGRES_BAD_RESPONSE = 5;
  PGRES_NONFATAL_ERROR = 6;
  PGRES_FATAL_ERROR = 7;

  CONNECTION_OK = 0;
  CONNECTION_BAD = 1;
  CONNECTION_STARTED = 2;
  CONNECTION_MADE = 3;
  CONNECTION_AWAITING_RESPONSE = 4;
  CONNECTION_AUTH_OK = 5;
  CONNECTION_SETENV = 6;
  CONNECTION_SSL_STARTUP = 7;
  CONNECTION_NEEDED = 8;

  PGFMT_TEXT = 0;
  PGFMT_BIN = 1;

type
  PPGconn = type pointer;
  PPGresult = type pointer;
  PPPGresult = ^PPGresult;

  PQnoticeProcessor = procedure(arg: pointer; message: PUTF8Char); cdecl;

  /// direct access to the libpq native Postgres protocol 3 library
  // - only the endpoints needed by this unit are imported
  TSQLDBPostgresLib = class(TSQLDBLib)
  protected
    fLibraryPath: TFileName;
    /// raise an exception on error and clean result
    // - will set pRes to nil if passed
    // - if forceClean is true - will clean passed res in any case
    procedure Check(conn: PPGconn; res: PPGresult;
      pRes: PPPGresult = nil; forceClean: boolean = true);
  public
    LibVersion: function: integer; cdecl;
    IsThreadSafe: function: integer; cdecl;
    SetDBLogin: function(pghost, pgport, pgoptions, pgtty, dbName,
      login, pwd: PUTF8Char): PPGconn; cdecl;
    Status: function(conn: PPGconn): integer; cdecl;
    Finish: procedure(conn: PPGconn); cdecl;
    ResultStatus: function(res: PPGresult): integer; cdecl;
    ResultErrorField: function(res: PPGresult; fieldcode: longint): PUTF8Char; cdecl;
    ErrorMessage: function(conn: PPGconn): PUTF8Char; cdecl;
    SetNoticeProcessor: function(conn: PPGconn; proc: PQnoticeProcessor;
      arg: pointer): PQnoticeProcessor; cdecl;
    Clear: procedure(res: PPGresult); cdecl;
    Freemem: procedure(ptr: pointer); cdecl;
    Exec: function(conn: PPGconn; query: PUTF8Char): PPGresult; cdecl;
    Prepare: function(conn: PPGconn; stmtName, query: PUTF8Char; nParams: integer;
      paramTypes: PCardinal): PPGresult; cdecl;
    ExecPrepared: function(conn: PPGconn; stmtName: PUTF8Char; nParams: integer;
      paramValues: PPchar; paramLengths, paramFormats: PInteger;
      resultFormat: integer): PPGresult; cdecl;
    ExecParams: function(conn: PPGconn; command: PUTF8Char; nParams: integer;
      paramTypes: PCardinal; paramValues: PPchar; paramLengths, paramFormats: PInteger;
      resultFormat: integer):PPGresult; cdecl;
    nfields: function(res: PPGresult): integer; cdecl;
    ntuples: function(res: PPGresult): integer; cdecl;
    cmdTuples: function(res: PPGresult): PUTF8Char; cdecl;
    fname: function(res: PPGresult; field_num: integer): PUTF8Char; cdecl;
    ftype: function(res: PPGresult; field_num: integer): cardinal; cdecl;
    GetValue: function(res: PPGresult; tup_num, field_num: integer): PUTF8Char; cdecl;
    GetLength: function(res: PPGresult; tup_num, field_num: integer): integer; cdecl;
    GetIsNull: function(res: PPGresult; tup_num, field_num: integer): integer; cdecl;
    UnescapeByteA: function(strtext: pointer; retbuflen: PPtrInt): pointer; cdecl;
    /// try to dynamically load the libpq library
    // - raise ESQLDBPostgres if the expected library is not found
    constructor Create;
  end;

const
  PQ_ENTRIES: array[0..23] of PChar = (
    'PQlibVersion', 'PQisthreadsafe', 'PQsetdbLogin', 'PQstatus', 'PQfinish',
    'PQresultStatus', 'PQresultErrorField', 'PQerrorMessage', 'PQsetNoticeProcessor',
    'PQclear', 'PQfreemem', 'PQexec', 'PQprepare', 'PQexecPrepared', 'PQexecParams',
    'PQnfields', 'PQntuples', 'PQcmdTuples', 'PQfname', 'PQftype', 'PQgetvalue',
    'PQgetlength', 'PQgetisnull', 'PQunescapeBytea');

var
  PQ: TSQLDBPostgresLib = nil;

{ TSQLDBPostgresLib }

const
  LIBNAME = {$ifdef MSWINDOWS}'libpq.dll'{$else}
    {$ifdef darwin}'libpq.dylib'{$else}'libpq.so.5'{$endif}{$endif};

constructor TSQLDBPostgresLib.Create;
var
  P: PPointer;
  i: PtrInt;
begin
  fLibraryPath := SynDBPostgresLibrary;
  if fLibraryPath = '' then
    fLibraryPath := LIBNAME;
  fHandle := SafeLoadLibrary(fLibraryPath);
  if fHandle = 0 then
    raise ESQLDBPostgres.CreateUTF8('Unable to find %', [fLibraryPath]);
  P := @@LibVersion;
  for i := 0 to High(PQ_ENTRIES) do
  begin
    P^ := GetProcAddress(fHandle, PQ_ENTRIES[i]);
    if P^ = nil then
    begin
      FreeLibrary(fHandle);
      fHandle := 0;
      raise ESQLDBPostgres.CreateUTF8('Invalid %: missing % - should be 8.3+',
        [LIBNAME, PQ_ENTRIES[i]]);
    end;
    inc(P);
  end;
end;

procedure TSQLDBPostgresLib.Check(conn: PPGconn; res: PPGresult; pRes: PPPGresult;
  forceClean: boolean);
var
  errMsg, errCode: PUTF8Char;
begin
  if (res = nil) or // nil in case of very fatal error, out of emory for example
    (ResultStatus(res) in [PGRES_BAD_RESPONSE, PGRES_NONFATAL_ERROR,
     PGRES_FATAL_ERROR]) then
  begin
    errMsg := ErrorMessage(conn);
    if res <> nil then
      errCode := ResultErrorField(res, Ord('C'){PG_DIAG_SQLSTATE})
    else
      errCode := nil;
    Clear(res);
    if pRes <> nil then
      pRes^ := nil;
    raise ESQLDBPostgres.CreateUTF8('% PGERRCODE: %, %', [self, errCode, errMsg]);
  end
  else if forceClean then
    Clear(res);
end;


{ TSQLDBPostgresConnection }

function TSQLDBPostgresConnection.PrepareCached(const aSQL: RawUTF8; aParamCount: integer;
  out aName: RaWUTF8): integer;
var
  dig: TSHA256Digest;
begin
  dig := SHA256Digest(aSQL);
  aName := SHA256DigestToString(dig);
  result := Hash256Index(pointer(fPrepared), fPreparedCount, @dig);
  if result >= 0 then
    exit; // already prepared
  PQ.Check(fPGConn, PQ.Prepare(fPGConn, pointer(aName), pointer(aSQL), aParamCount, nil));
  result := fPreparedCount;
  inc(fPreparedCount);
  if result = length(fPrepared) then
    SetLength(fPrepared, result + 32);
  fPrepared[result] := dig;
end;

procedure TSQLDBPostgresConnection.DirectExecSQL(const SQL: RawUTF8);
begin
  PQ.Check(fPGConn, PQ.Exec(fPGConn, pointer(SQL)));
end;

procedure SynLogNoticeProcessor({%H-}arg: Pointer; message: PUTF8Char); cdecl;
begin
  SynDBLog.Add.Log(sllTrace, 'PGINFO: %', [message], TObject(arg));
end;

procedure DummyNoticeProcessor({%H-}arg: Pointer; message: PUTF8Char); cdecl;
begin
end;

procedure TSQLDBPostgresConnection.Connect;
var
  log: ISynLog;
begin
  log := SynDBLog.Enter(self, 'Connect');
  Disconnect; // force fTrans=fError=fServer=fContext=nil
  try
    fPGConn := PQ.SetDBLogin(pointer(Properties.ServerName), nil, nil, nil,
      pointer(Properties.DatabaseName), pointer(Properties.UserID),
      pointer(Properties.PassWord));
    if PQ.Status(fPGConn) = CONNECTION_BAD then
      raise ESQLDBPostgres.CreateUTF8('Connection to database % failed [%]',
        [Properties.DatabaseNameSafe, PQ.ErrorMessage(fPGConn)]);
    if log <> nil then
    begin
      PQ.SetNoticeProcessor(fPGConn, SynLogNoticeProcessor, pointer(self));
      log.Log(sllDB, 'Connected to % % using % v%', [fProperties.ServerName,
        fProperties.DatabaseNameSafe, PQ.fLibraryPath, PQ.LibVersion]);
    end
    else // to ensure no performance drop due to notice to console
      PQ.SetNoticeProcessor(fPGConn, DummyNoticeProcessor, nil);
    inherited Connect; // notify any re-connection
  except
    on E: Exception do
    begin
      if log <> nil then
        log.Log(sllError, 'Connect: % on %', [E, Properties.DatabaseNameSafe], self);
      Disconnect; // clean up on fail
      raise;
    end;
  end;
end;

procedure TSQLDBPostgresConnection.Disconnect;
begin
  try
    inherited Disconnect;
  finally
    if fPGConn <> nil then
    begin
      PQ.Finish(fPGConn);
      fPGConn := nil;
    end;
  end;
end;

function TSQLDBPostgresConnection.IsConnected: boolean;
begin
  result := (fPGConn <> nil);
end;

function TSQLDBPostgresConnection.NewStatement: TSQLDBStatement;
begin
  result := TSQLDBPostgresStatement.Create(self);
end;

procedure TSQLDBPostgresConnection.StartTransaction;
var
  log: ISynLog;
begin
  log := SynDBLog.Enter(self, 'StartTransaction');
  if TransactionCount > 0 then
    raise ESQLDBPostgres.CreateUTF8('Invalid %.StartTransaction: nested ' +
      'transactions are not supported by the Postgres - use SAVEPOINT instead', [self]);
  try
    inherited StartTransaction;
    DirectExecSQL('START TRANSACTION');
  except
    on E: Exception do
    begin
      if log <> nil then
        log.Log(sllError, 'StartTransaction: % on %', [E, Properties.DatabaseNameSafe], self);
      if fTransactionCount > 0 then
        Dec(fTransactionCount);
      raise;
    end;
  end;
end;

procedure TSQLDBPostgresConnection.Commit;
begin
  inherited Commit;
  try
    DirectExecSQL('COMMIT');
  except
    inc(fTransactionCount); // the transaction is still active
    raise;
  end;
end;

procedure TSQLDBPostgresConnection.Rollback;
begin
  inherited;
  DirectExecSQL('ROLLBACK');
end;

{ TSQLDBPostgresConnectionProperties }
procedure TSQLDBPostgresConnectionProperties.GetForeignKeys;
begin
  // TODO - how to get field we reference to? (currently consider this is "ID")
  with Execute('SELECT' + '  ct.conname as foreign_key_name, ' +
      '  case when ct.condeferred then 1 else 0 end AS is_disabled, ' +
      '  (SELECT tc.relname from pg_class tc where tc.oid = ct.conrelid) || ''.'' || ' +
      '     (SELECT a.attname FROM pg_attribute a WHERE a.attnum = ct.conkey[1] AND a.attrelid = ct.conrelid) as from_ref, ' +
      '  (SELECT tc.relname from pg_class tc where tc.oid = ct.confrelid) || ''.id'' as referenced_object ' +
      'FROM  pg_constraint ct WHERE contype = ''f''', []) do
    while Step do
      fForeignKeys.Add(ColumnUTF8(2), ColumnUTF8(3));
end;

procedure TSQLDBPostgresConnectionProperties.FillOidMapping;
begin // see pg_type.h (most used first)
  mapOid(23, ftInt64);     // int4
  mapOid(20, ftInt64);     // int8
  mapOid(25, ftUTF8);      // text
  mapOid(701, ftDouble);   // float8
  mapOid(1114, ftDate);    // timestamp
  mapOid(17, ftBlob);      // bytea
  mapOid(1700, ftCurrency);// numeric - our ORM uses NUMERIC(19,4) for currency
  mapOid(16, ftInt64);     // bool
  mapOid(21, ftInt64);     // int2
  mapOid(790, ftCurrency); // money
  mapOid(1184, ftDate);    // timestampz
  mapOid(702, ftDate);     // abstime
  mapOid(1082, ftDate);    // date
  mapOid(1083, ftDate);    // time
  mapOid(1266, ftDate);    // timez
  mapOid(24, ftInt64);     // regproc
  mapOid(26, ftInt64);     // oid
  mapOid(700, ftDouble);   // float4
end; // any unregistered OID will be handled as ftUTF8

constructor TSQLDBPostgresConnectionProperties.Create(
  const aServerName, aDatabaseName, aUserID, aPassword: RawUTF8);
begin
  GlobalLock;
  try
    if PQ = nil then
      GarbageCollectorFreeAndNil(PQ, TSQLDBPostgresLib.Create);
  finally
    GlobalUnLock;
  end;
  if PQ.IsThreadSafe <> 1 then
    raise ESQLDBPostgres.Create('libpq should be compiled in threadsafe mode');
  fDBMS := dPostgreSQL;
  FillOidMapping;
  inherited Create(aServerName, aDatabaseName, aUserID, aPassWord);
end;

function TSQLDBPostgresConnectionProperties.NewConnection: TSQLDBConnection;
begin
  result := TSQLDBPostgresConnection.Create(self);
end;

function TSQLDBPostgresConnectionProperties.Oid2FieldType(cOID: cardinal): TSQLDBFieldType;
var
  i: PtrInt;
begin
  i := IntegerScanIndex(pointer(fOids), fOidsCount, cOID);
  if i >= 0 then
    result := fOidsFieldTypes[i]
  else
    result := ftUTF8;
end;

procedure TSQLDBPostgresConnectionProperties.MapOid(cOid: cardinal;
  fieldType: TSQLDBFieldType);
var
  i: PtrInt;
begin
  i := IntegerScanIndex(pointer(fOids), fOidsCount, cOID);
  if i < 0 then
  begin
    i := FOidsCount;
    inc(FOidsCount);
    if i = length(FOids) then
    begin
      SetLength(fOids, i + 32);
      SetLength(fOidsFieldTypes, i + 32);
    end;
    fOids[i] := cOid;
  end;
  fOidsFieldTypes[i] := fieldType // replace
end;

procedure TSQLDBPostgresStatement.BindColumns;
var
  nCols, c: longint;
  cName: RawUTF8;
  colOID: cardinal;
begin
  fColumn.Clear;
  fColumn.ReHash;
  nCols := PQ.nfields(fRes);
  fColumn.Capacity := nCols;
  for c := 0 to nCols - 1 do
  begin
    cName := PQ.fname(fRes, c);
    with PSQLDBColumnProperty(fColumn.AddAndMakeUniqueName(cName))^ do
    begin
      colOID := PQ.ftype(fRes, c);
      ColumnType := TSQLDBPostgresConnectionProperties(Connection.Properties)
        .Oid2FieldType(colOID);
      // use PQfmod to get additional type info?
    end;
  end;
end;

procedure TSQLDBPostgresStatement.CheckColAndRowset(const Col: integer);
begin
  CheckCol(Col);
  if (fRes = nil) or (fResStatus <> PGRES_TUPLES_OK) then
    raise ESQLDBPostgres.CreateUTF8('%.Execute not called before Column*', [self]);
end;

destructor TSQLDBPostgresStatement.Destroy;
begin
  try
    Reset; // close result if any
  finally
    inherited;
  end;
end;

// see https://www.postgresql.org/docs/9.3/libpq-exec.html

procedure TSQLDBPostgresStatement.Prepare(const aSQL: RawUTF8; ExpectResults: boolean);
var
  log: TSynLog;
  timer: TPrecisionTimer;
  fromcache: integer;
begin
  log := GetSQLLog(timer);
  if aSQL = '' then
    raise ESQLDBPostgres.CreateUTF8('%.Prepare: empty statement', [self]);
  inherited Prepare(aSQL, ExpectResults); // will strip last ;
  fPreparedParamsCount := ReplaceParamsByNumbers(fSQL, fParsedSQL, '$');
  if (fPreparedParamsCount > 0) and (IdemPCharArray(pointer(fParsedSQL),
      ['SELECT', 'INSERT', 'UPDATE', 'DELETE', 'VALUES']) >= 0) then
  begin // preparable
    fromcache := TSQLDBPostgresConnection(fConnection).PrepareCached(
      fParsedSQL, fPreparedParamsCount, fPreparedStmtName);
    if log<>nil then
      log.Log(sllDB,'Prepare % name=% cache=% %',
        [timer.Stop, fPreparedStmtName, fromcache, fParsedSQL], self);
  end
  else if log<>nil then
    log.Log(sllDB,'Prepare % (no cache) %', [timer.Stop, fParsedSQL], self);
  SetLength(fPGParams, fPreparedParamsCount);
  SetLength(fPGParamFormats, fPreparedParamsCount);
  SetLength(fPGparamLengths, fPreparedParamsCount);
end;

procedure TSQLDBPostgresStatement.ExecutePrepared;
var
  i: PtrInt;
  p: PSQLDBParam;
  c: TSQLDBPostgresConnection;
  log: TSynLog;
  logsql: RaWUTF8;
  timer: TPrecisionTimer;
begin
  log := GetSQLLog(timer, @logsql);
  if fParsedSQL = '' then
    raise ESQLDBPostgres.CreateUTF8('%.ExecutePrepared: Statement not prepared', [self]);
  if fParamCount <> fPreparedParamsCount then
    raise ESQLDBPostgres.CreateUTF8('%.ExecutePrepared: Query expects % parameters ' +
      'but % bound', [self, fPreparedParamsCount, fParamCount]);
  inherited ExecutePrepared;
  for i := 0 to fParamCount - 1 do // set parameters as expected by PostgreSQL
  begin
    // mark parameter as textual by default, with no blob len
    fPGParamFormats[i] := 0;
    fPGparamLengths[i] := 0;
    // convert parameter value as text stored in p^.VData
    p := @fParams[i];
    if p^.VArray <> nil then
    begin
      if not (p^.VType in [ftInt64, ftDouble, ftCurrency, ftUTF8]) then
        raise ESQLDBPostgres.CreateUTF8('%.ExecutePrepared: Invalid array type % ' +
          'on bound parameter #%', [Self, ToText(p^.VType)^, i]);
      p^.VData := BoundArrayToJSONArray(p^.VArray);
    end
    else
    begin
      case p^.VType of
        ftNull:
          p^.VData := '';
        ftInt64:
          // use SwapEndian + binary ?
          Int64ToUtf8(p^.VInt64, RawUTF8(p^.VData));
        ftCurrency:
          Curr64ToStr(p^.VInt64, RawUTF8(p^.VData));
        ftDouble:
          ExtendedToStr(PDouble(@p^.VInt64)^, DOUBLE_PRECISION, RawUTF8(p^.VData));
        ftDate:
          // Postgres expects space instead of T in ISO8601 expanded format
          p^.VData := DateTimeToIso8601(PDateTime(@p^.VInt64)^, true, ' ');
        ftUTF8:
          ; // text already in p^.VData
        ftBlob:
        begin
          fPGParamFormats[i] := 1; // binary
          fPGparamLengths[i] := length(p^.VData);
        end;
        else
          raise ESQLDBPostgres.CreateUTF8('%.ExecutePrepared: cannot bind ' +
            'parameter #% of type %', [self, i, ToText(p^.VType)^]);
      end;
    end;
    fPGParams[i] := pointer(p^.VData);
  end;
  c := TSQLDBPostgresConnection(Connection);
  if fPreparedStmtName <> '' then
    fRes := PQ.ExecPrepared(c.fPGConn, pointer(fPreparedStmtName), fPreparedParamsCount,
      pointer(fPGParams), pointer(fPGparamLengths), pointer(fPGParamFormats), PGFMT_TEXT)
  else if fPreparedParamsCount = 0 then
    // PQexec handles multiple SQL commands
    fRes := PQ.Exec(c.fPGConn, pointer(fParsedSQL))
  else
    fRes := PQ.ExecParams(c.fPGConn, pointer(fParsedSQL), fPreparedParamsCount, nil,
      pointer(fPGParams), pointer(fPGparamLengths), pointer(fPGParamFormats), PGFMT_TEXT);
  PQ.Check(c.fPGConn, fRes, @fRes, {forceClean=}false);
  fResStatus := PQ.ResultStatus(fRes);
  if fExpectResults then
  begin
    if fResStatus <> PGRES_TUPLES_OK then
    begin // paranoid check
      PQ.Clear(fRes);
      fRes := nil;
      raise ESQLDBPostgres.CreateUTF8('%.ExecutePrepared: result expected but ' +
        'statement did not return tuples', [self]);
    end;
    fTotalRowsRetrieved := PQ.ntuples(fRes);
    fCurrentRow := -1;
    if fColumn.Count = 0 then // if columns exist then statement is already cached
      BindColumns;
  end;
  if log <> nil then
    log.Log(sllSQL, 'ExecutePrepared: % %', [timer.Stop, logsql], self);
end;

function TSQLDBPostgresStatement.UpdateCount: integer;
begin
  result := GetCardinalDef(PQ.cmdTuples(fRes), 0);
end;

procedure TSQLDBPostgresStatement.Reset;
begin
  if fRes <> nil then
  begin
    PQ.clear(fRes);
    fRes := nil;
  end;
  fResStatus := PGRES_EMPTY_QUERY;
  inherited Reset;
end;

function TSQLDBPostgresStatement.Step(SeekFirst: boolean): boolean;
begin
  if (fRes = nil) or (fResStatus <> PGRES_TUPLES_OK) then
    raise ESQLDBPostgres.CreateUTF8('%.Execute should be called before Step', [self]);
  if SeekFirst then
    fCurrentRow := -1;
  result := fCurrentRow + 1 < fTotalRowsRetrieved;
  if not result then
    exit;
  inc(fCurrentRow);
end;

function TSQLDBPostgresStatement.ColumnInt(Col: integer): int64;
begin
  CheckColAndRowset(Col);
  result := GetInt64(PQ.GetValue(fRes, fCurrentRow, Col));
end;

function TSQLDBPostgresStatement.ColumnNull(Col: integer): boolean;
begin
  CheckColAndRowset(Col);
  result := (PQ.GetIsNull(fRes, fCurrentRow, Col) = 1);
end;

function TSQLDBPostgresStatement.ColumnDouble(Col: integer): double;
begin
  CheckColAndRowset(Col);
  result := GetExtended(PQ.GetValue(fRes, fCurrentRow, Col));
end;

function TSQLDBPostgresStatement.ColumnDateTime(Col: integer): TDateTime;
begin
  CheckColAndRowset(Col);
  Iso8601ToDateTimePUTF8CharVar(PQ.GetValue(fRes, fCurrentRow, Col),
    PQ.GetLength(fRes, fCurrentRow, Col), result);
end;

function TSQLDBPostgresStatement.ColumnCurrency(Col: integer): currency;
begin
  CheckColAndRowset(Col);
  PInt64(@result)^ := StrToCurr64(PQ.GetValue(fRes, fCurrentRow, Col));
end;

function TSQLDBPostgresStatement.ColumnUTF8(Col: integer): RawUTF8;
begin
  CheckColAndRowset(Col);
  FastSetString(result, PQ.GetValue(fRes, fCurrentRow, Col),
    PQ.GetLength(fRes, fCurrentRow, Col));
end;

function TSQLDBPostgresStatement.ColumnBlob(Col: integer): RawByteString;
var
  blob: pointer;
  bloblen: PtrInt;
begin // PGFMT_TEXT was used -> need to convert into binary
  CheckColAndRowset(Col);
  blob := PQ.UnescapeByteA(PQ.GetValue(fRes, fCurrentRow, Col), @bloblen);
  if blob = nil then
    raise ESQLDBPostgres.CreateUTF8('%.ColumnBlob: out of memory', [self]);
  try
    FastSetStringCP(result, blob, bloblen, CP_RAWBYTESTRING);
  finally
    PQ.Freemem(blob);
  end;
end;

procedure TSQLDBPostgresStatement.ColumnsToJSON(WR: TJSONWriter);
var
  col: integer;
  blob: pointer;
  bloblen: PtrInt;
begin
  if (fRes = nil) or (fResStatus <> PGRES_TUPLES_OK) or (fCurrentRow < 0) then
    raise ESQLDBPostgres.CreateUTF8('%.ColumnToJSON unexpected', [self]);
  if WR.Expand then
    WR.Add('{');
  for col := 0 to fColumnCount - 1 do
  with fColumns[col] do
  begin
    if WR.Expand then
      WR.AddFieldName(ColumnName); // add '"ColumnName":'
    if PQ.GetIsNull(fRes, fCurrentRow, col) = 1 then
      WR.AddShort('null')
    else
      case ColumnType of
        ftNull:
          WR.AddShort('null');
        ftInt64:
          WR.AddNoJSONEscape(PQ.GetValue(fRes, fCurrentRow, col));
        ftDouble, ftCurrency:
          WR.AddFloatStr(PQ.GetValue(fRes, fCurrentRow, col));
        ftUTF8, ftDate:
          begin
            WR.Add('"');
            WR.AddJSONEscape(PQ.GetValue(fRes, fCurrentRow, col));
            WR.Add('"');
          end;
        ftBlob:
          if fForceBlobAsNull then
            WR.AddShort('null')
          else
          begin
            blob := PQ.UnescapeByteA(PQ.GetValue(fRes, fCurrentRow, col), @bloblen);
            if blob = nil then
              WR.AddShort('null')
            else
              try
                WR.WrBase64(blob, bloblen, {withmagic=}true);
              finally
                PQ.Freemem(blob);
              end;
          end
        else
          raise ESQLDBPostgres.CreateUTF8('%.ColumnsToJSON: %?', [self, ToText(ColumnType)^]);
      end;
    WR.Add(',');
  end;
  WR.CancelLastComma; // cancel last ','
  if WR.Expand then
    WR.Add('}');
end;


initialization
  TSQLDBPostgresConnectionProperties.RegisterClassNameForDefinition;
end.
