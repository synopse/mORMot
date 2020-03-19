/// PostgreSQL direct access classes for SynDB units (not DB.pas based)
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license - see LICENSE.md
unit SynDBPostgres;

{
  *****************************************************************************
   Implementation of TSQLDB* for PostgreSQL using libpg

   Features:
    - fast, minimum memory allocation
    - array binding for select statements (caller should use =ANY(?) etc.)

   Limitations:
    - FPC only (uses postgres3dyn.pp)
      (TODO - use our own simple wrapper)
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
  {$ifndef FPC}
  SynDBPostgres is for Free Pascal only
  {$endif}
  SysUtils,
  postgres3dyn,
  SynCommons,
  SynTable,
  SynDB;

type
  PPPGresult = ^PPGresult;

  /// exception type associated to the native libpg Interface
  ESQLDBPostgres = class(ESQLDBException);


  /// connection properties which will implement an internal Thread-Safe
  // connection pool
  TSQLDBPostgresConnectionProperties = class(TSQLDBConnectionPropertiesThreadSafe)
  private
    FOids2FieldType: TSynDictionary; // thread-safe Oid/TSQLDBFieldType map
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
    /// release related memory, and all per-thread connections
    destructor Destroy; override;
    /// create a new connection
    // - caller is responsible of freeing this instance
    // - this overridden method will create an TSQLDBPostgresConnection instance
    function NewConnection: TSQLDBConnection; override;
    /// add or replace mapping of OID into TSQLDBFieldType
    // - in case mapping for OID is not defined column type of such OIDs will be ftUTF8
    function Oid2FieldType(const cOID: Oid): TSQLDBFieldType; {$ifdef HASINLINE} inline; {$endif}
    // add new (or override existed) OID to FieldType mapping
    procedure MapOid(const cOid: Oid; const fieldType: TSQLDBFieldType);
  end;


  /// implements a connection via the libpq access layer
  TSQLDBPostgresConnection = class(TSQLDBConnectionThreadSafe)
  protected
    // prepared statement names = SHA-256 of its SQL
    FPQprepared: TRawUTF8List;
    // the associated low-level provider connection
    fPGConn: PPGconn;
    /// raise an exception on error and clean result
    // - will set pRes to nil if passed
    // - if forceClean is true - will clean passed res in any case
    procedure Check(res: PPGresult; pRes: PPPGresult = nil; forceClean: boolean = true);
    /// direct execution of SQL statement what do not returns a result
    // - statement should not contains parameters
    // - raise an ESQLDBPostgres on error
    procedure DirectExecSQL(const SQL: RawUTF8);
  public
    /// connect to a specified database engine
    constructor Create(aProperties: TSQLDBConnectionProperties); override;
    /// release memory and connection
    destructor Destroy; override;
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
    property Direct: PPGconn read fPGConn;
  end;


  /// implements a statement via a Postgres database connection
  TSQLDBPostgresStatement = class(TSQLDBStatementWithParamsAndColumns)
  protected
    fPreparedStmtName: RawUTF8; // = SHA-256 of the SQL
    fParsedSQL: RawUTF8;
    fPreparedParamsCount: longint;
    fRes: PPGresult;
    fResStatus: TExecStatusType; // fRes.resultStatus is not correct
    // pointers to query parameters. initialized by Prepare, filled in Executeprepared
    fPGParams: array of PChar;
    // 0 - text, 1 - binary. initialized by Prepare, filled in Executeprepared
    fPGParamFormats: array of longint;
    // non zero for binary params
    fPGparamLengths: array of longint;
    procedure BindColumnus;
    /// raise an exception if Col is out of range according to fColumnCount
    // or rowset is not initialized
    procedure CheckColAndRowset(const Col: integer); {$ifdef HASINLINE} inline; {$endif}
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
    /// How many parameters founded during prepare stage
    property PreparedParamsCount: longint read fPreparedParamsCount;
  end;

var
  PQlibVersion: function: longint; cdecl;


implementation

uses
  SynLog,
  SynCrypto; // PGSQL expects names for prepared statements = use SHA-256


{ TSQLDBPostgresConnection }

procedure TSQLDBPostgresConnection.Check(res: PPGresult; pRes: PPPGresult;
  forceClean: boolean);
var
  errMsg, errCode: PChar;
begin
  if (res = nil) or // nil in case of very fatal error, out of emory for example
    (PQresultStatus(res) in [PGRES_BAD_RESPONSE, PGRES_NONFATAL_ERROR,
     PGRES_FATAL_ERROR]) then
  begin
    errMsg := PQerrorMessage(fPGConn);
    if res <> nil then
      errCode := PQresultErrorField(res, Ord('C'){PG_DIAG_SQLSTATE})
    else
      errCode := nil;
    PQclear(res);
    if pRes <> nil then
      pRes^ := nil;
    raise ESQLDBPostgres.CreateUTF8('% PGERRCODE: %, %', [self, errCode, errMsg]);
  end
  else if forceClean then
    PQclear(res);
end;

procedure TSQLDBPostgresConnection.DirectExecSQL(const SQL: RawUTF8);
begin
  Check(PQexec(fPGConn, pointer(SQL)));
end;

constructor TSQLDBPostgresConnection.Create(aProperties: TSQLDBConnectionProperties);
begin
  inherited Create(aProperties);
  FPQprepared := TRawUTF8List.Create([fCaseSensitive, fNoDuplicate]);
end;

destructor TSQLDBPostgresConnection.Destroy;
begin
  FPQprepared.Free;
  inherited Destroy;
end;

procedure SynLogNoticeProcessor({%H-}arg: Pointer; message: PAnsiChar); cdecl;
begin
  SynDBLog.Add.Log(sllInfo, 'PGINFO: %', [message]);
end;

procedure TSQLDBPostgresConnection.Connect;
var
  log: ISynLog;
begin
  log := SynDBLog.Enter(self, 'Connect');
  Disconnect; // force fTrans=fError=fServer=fContext=nil
  try
    fPGConn := PQsetdbLogin(pointer(Properties.ServerName), nil, // pgport,
      nil, // pgoptions,
      nil, // pgtty,
      pointer(Properties.DatabaseName), pointer(Properties.UserID),
      pointer(Properties.PassWord));
    if (PQstatus(fPGConn) = CONNECTION_BAD) then
      raise ESQLDBPostgres.CreateUTF8('Connection to database % failed. Reason: %',
        [PQerrorMessage(fPGConn)]);
    if log <> nil then
    begin
      PQsetNoticeProcessor(fPGConn, SynLogNoticeProcessor, pointer(self));
      log.Log(sllDB, 'Connected to % % using % v%',
        [fProperties.ServerName, fProperties.DatabaseNameSafe,
         Postgres3LoadedLibrary, PQlibVersion]);
    end;
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
      PQfinish(fPGConn);
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

procedure TSQLDBPostgresConnectionProperties.fillOidMapping;
begin
  mapOid(702, ftDate);     // abstime
  mapOid(1082, ftDate);    // date
  mapOid(1083, ftDate);    // time
  mapOid(1114, ftDate);    // timestamp
  mapOid(1184, ftDate);    // timestampz
  mapOid(1266, ftDate);    // timez
  mapOid(20, ftInt64);     // int8
  mapOid(21, ftInt64);     // int2
  mapOid(23, ftInt64);     // int4
  mapOid(24, ftInt64);     // regproc
  mapOid(26, ftInt64);     // oid
  mapOid(700, ftDouble);   // float4
  mapOid(701, ftDouble);   // float8
  mapOid(1700, ftDouble);  // numeric
  mapOid(790, ftCurrency); // money
  mapOid(17, ftBlob);      // bytea
end;

constructor TSQLDBPostgresConnectionProperties.Create(
  const aServerName, aDatabaseName, aUserID, aPassword: RawUTF8);
begin
  InitialisePostgres3;
  if PQisthreadsafe() <> 1 then
    raise ESQLDBPostgres.Create('libpq should be compiler in threadsafe mode');
  if @PQunescapeBytea = nil then
    raise ESQLDBPostgres.Create('libpq should be 8.3+ (PQunescapeBytea not found)');
  fDBMS := dPostgreSQL;
  // missed in postgres3dyn
  PQlibVersion := GetProcedureAddress(Postgres3LibraryHandle, 'PQlibVersion');
  FOids2FieldType := TSynDictionary.Create(TypeInfo(TCardinalDynArray),
    TypeInfo(TSQLDBFieldTypeDynArray));
  FillOidMapping;
  inherited Create(aServerName, aDatabaseName, aUserID, aPassWord);
end;

destructor TSQLDBPostgresConnectionProperties.Destroy;
begin
  FOids2FieldType.Free;
  inherited Destroy;
  ReleasePostgres3;
end;

function TSQLDBPostgresConnectionProperties.NewConnection: TSQLDBConnection;
begin
  result := TSQLDBPostgresConnection.Create(self);
end;

function TSQLDBPostgresConnectionProperties.Oid2FieldType(
  const cOID: Oid): TSQLDBFieldType;
begin
  if not FOids2FieldType.FindAndCopy(cOID, result) then
    result := ftUTF8;
end;

procedure TSQLDBPostgresConnectionProperties.MapOid(const cOid: Oid;
  const fieldType: TSQLDBFieldType);
begin
  FOids2FieldType.AddOrUpdate(cOid, fieldType);
end;

procedure TSQLDBPostgresStatement.BindColumnus;
var
  nCols, c: longint;
  cName: RawUTF8;
  colOID: Oid;
begin
  fColumn.Clear;
  fColumn.ReHash;
  nCols := PQnfields(fRes);
  fColumn.Capacity := nCols;
  for c := 0 to nCols - 1 do
  begin
    cName := PQfname(fRes, c);
    with PSQLDBColumnProperty(fColumn.AddAndMakeUniqueName(cName))^ do
    begin
      colOID := PQftype(fRes, c);
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
    raise ESQLDBPostgres.CreateUTF8('%.Execute not called before Column*()', [self]);
end;

destructor TSQLDBPostgresStatement.Destroy;
begin
  try
    Reset; // close result if any
  finally
    inherited;
  end;
end;

procedure TSQLDBPostgresStatement.Prepare(const aSQL: RawUTF8; ExpectResults: boolean);
var
  c: TSQLDBPostgresConnection;
  log: TSynLog;
  timer: TPrecisionTimer;
  fromcache: integer;
begin
  try
    log := SynDBLog.Add;
    if log <> nil then
      if sllDB in log.Family.Level then
        timer.Start
      else
        log := nil;
    if aSQL = '' then
      raise ESQLDBPostgres.CreateUTF8('%.Prepare: empty statement', [self]);
    inherited Prepare(aSQL, ExpectResults); // will strip last ;
    fPreparedParamsCount := ReplaceParamsByNumbers(fSQL, fParsedSQL, '$');
    if (fPreparedParamsCount > 0) and (IdemPCharArray(PUTF8Char(fParsedSQL),
        ['SELECT', 'INSERT', 'UPDATE', 'DELETE', 'VALUES']) >= 0) then
    begin // preparable
      fPreparedStmtName := SHA256(fParsedSQL);
      c := pointer(Connection);
      fromcache := c.FPQprepared.IndexOf(fPreparedStmtName);
      if fromcache < 0 then
      begin // not prepared in this connection yet
        c.Check(PQPrepare(c.fPGConn, pointer(fPreparedStmtName),
          pointer(fParsedSQL), fPreparedParamsCount, nil));
        c.FPQprepared.Add(fPreparedStmtName);
      end;
      if log<>nil then
        log.Log(sllDB,'Prepare % name=% cache=% %',
          [timer.Stop, fPreparedStmtName, fromcache, fParsedSQL],self);
    end
    else if log<>nil then
      log.Log(sllDB,'Prepare % %', [timer.Stop, fParsedSQL],self);
  except
    on E: Exception do
    begin
      if SynDBLog <> nil then
        SynDBLog.Add.Log(sllError, 'Prepare: % on % [%]',
          [E, Connection.Properties.DatabaseNameSafe, aSQL], self);
      raise;
    end;
  end;
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
  timer: TPrecisionTimer;
begin
  try
    log := SynDBLog.Add;
    if log<>nil then
      if sllSQL in log.Family.Level then
        timer.Start
      else
        log := nil;
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
      fRes := PQexecPrepared(c.fPGConn, pointer(fPreparedStmtName), fPreparedParamsCount,
        pointer(fPGParams), pointer(fPGparamLengths), pointer(fPGParamFormats), 0)
    else if fPreparedParamsCount = 0 then // PQexec can include multiple SQL commands
      fRes := PQexec(c.fPGConn, pointer(fParsedSQL))
    else
      fRes := PQexecParams(c.fPGConn, pointer(fParsedSQL), fPreparedParamsCount, nil,
        pointer(fPGParams), pointer(fPGparamLengths), pointer(fPGParamFormats), 0);
    c.Check(fRes, @fRes, false{forceClean});
    fResStatus := PQresultStatus(fRes);
    if fExpectResults then
    begin
      if fResStatus <> PGRES_TUPLES_OK then
      begin // paranoid check
        PQclear(fRes);
        fRes := nil;
        raise ESQLDBPostgres.CreateUTF8('%.ExecutePrepared: result expected but ' +
          'statement did not return tuples', [self]);
      end;
      fTotalRowsRetrieved := PQntuples(fRes);
      fCurrentRow := -1;
      if fColumn.Count = 0 then // if columnus exists then statement is already cached
        BindColumnus;
    end;
    if log <> nil then
      log.Log(sllSQL, 'ExecutePrepared: % %', [timer.Stop, SQLWithInlinedParams], self);
  except
    on E: Exception do
    begin
      if SynDBLog <> nil then
        SynDBLog.Add.Log(sllError, '% %', [E, SQLWithInlinedParams], self);
      raise;
    end;
  end;
end;

function TSQLDBPostgresStatement.UpdateCount: integer;
begin
  result := GetCardinalDef(PQcmdTuples(fRes), 0);
end;

procedure TSQLDBPostgresStatement.Reset;
begin
  if fRes <> nil then
  begin
    PQclear(fRes);
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
  result := GetInt64(PQgetvalue(fRes, fCurrentRow, Col));
end;

function TSQLDBPostgresStatement.ColumnNull(Col: integer): boolean;
begin
  CheckColAndRowset(Col);
  result := (PQgetisnull(fRes, fCurrentRow, Col) = 1);
end;

function TSQLDBPostgresStatement.ColumnDouble(Col: integer): double;
begin
  CheckColAndRowset(Col);
  result := GetExtended(PQgetvalue(fRes, fCurrentRow, Col));
end;

function TSQLDBPostgresStatement.ColumnDateTime(Col: integer): TDateTime;
begin
  CheckColAndRowset(Col);
  Iso8601ToDateTimePUTF8CharVar(PQgetvalue(fRes, fCurrentRow, Col),
    PQgetlength(fRes, fCurrentRow, Col), result);
end;

function TSQLDBPostgresStatement.ColumnCurrency(Col: integer): currency;
begin
  CheckColAndRowset(Col);
  result := GetExtended(PQgetvalue(fRes, fCurrentRow, Col));
end;

function TSQLDBPostgresStatement.ColumnUTF8(Col: integer): RawUTF8;
begin
  CheckColAndRowset(Col);
  FastSetString(result, PQgetvalue(fRes, fCurrentRow, Col),
    PQgetlength(fRes, fCurrentRow, Col));
end;

function TSQLDBPostgresStatement.ColumnBlob(Col: integer): RawByteString;
var
  l: size_t;
  blb: PByte;
begin
  CheckColAndRowset(Col);
  blb := PQunescapeBytea(PByte(PQgetvalue(fRes, fCurrentRow, Col)), @l);
  try
    if blb = nil then
      raise EOutOfMemory.Create('PQunescapeBytea: Out of memory');
    FastSetStringCP(result, blb, l, CP_NONE);
  finally
    if blb <> nil then
      PQfreemem(blb);
  end;
end;

end.
