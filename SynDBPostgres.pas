/// Postgres direct access classes for SynDB units (not DB.pas based)
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license - see LICENSE.md
unit SynDBPostgres;

{
  *****************************************************************************
   Implementation of TSQLDB* for Postgres using libpg
   Features:
    - fast, minimum memory allocation
    - array binding for select statemets (caller should use =ANY(?) etc.)

   Limitations:
    - FPC only (uses postgres3dyn.pp)
    - works with PostgreSQL>=7.4 and (v3 protocol)
    - libpg>=8.3 (PQunescapeBytea)
    - consder database in UTF8 collation
    - notifications is not implemented
    - Postgres level prepared statements works only for SQLs what starts
      exectly with SELECT INSERT UPDATE DELETE VALUES and not contains ";"
    - parameter parser will fails in case SQL contains comments with ? inside (TODO - will be fixed)
    - all query rows are returned at once, caller should care about pagination (TODO - implement singleRowMode?)
  Aim of this unit is to provide simple alternative to SynDBZeos for Postgres DB
  *****************************************************************************
}

interface

{$MODE DELPHI}

uses
  {$ifndef FPC}
  SynDBPostgres is for Free Pascal only
  {$endif}
  SysUtils,
  postgres3dyn,
  SynCommons,
  SynTable,
  SynDB,
  Generics.Collections;

type
  PPPGresult = ^PPGresult;
  /// execption type associated to the native libpg Interface
  ESQLDBPostgres = class(ESQLDBException);
  /// connection properties which will implement an internal Thread-Safe
  // connection pool

  { TSQLDBPostgresConnectionProperties }
  TSQLDBPostgresConnectionProperties = class(TSQLDBConnectionPropertiesThreadSafe)
  private
    FOids2FieldType: TDictionary<Oid, TSQLDBFieldType>;
  protected
    procedure GetForeignKeys; override;
    /// fill mapping of OID ->
    // - in runtime mapping can be defined using oid2FieldType method
    // - OIDs defined in DB can be retrieved using query
    //  "select oid, typname from pg_type where typtype = 'b' order by oid"
    procedure fillOidMapping; virtual;
  public
    /// initialize the properties
    // - throws in case libpg is not sthead-safe
    // - aDatabaseName can be a Connection URI - see
    // https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING
    // - if aDatabaseName contains connection URI with password we recommend to repeat password
    // in aPassword parameter to prevent logging it (see (TSQLDBConnectionProperties.DatabaseameSafe)
    // - better to use environment variables and postgres config file for connection parameters
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassword: RawUTF8);
      override;
    /// release related memory, and all per-thread connections
    destructor Destroy; override;
    /// create a new connection
    // - caller is responsible of freeing this instance
    // - this overridden method will create an TSQLDBPostgresConnection instance
    function NewConnection: TSQLDBConnection; override;
    /// Add r replace mapping of OID into TSQLDBFieldType
    // - in case mapping for OID is not defined column type of such OIDs will be ftUTF8
    function oid2FieldType(const cOID: Oid): TSQLDBFieldType; inline;
    // Add new (or override existed) OID to FieldType mapping
    procedure mapOid(const cOid: Oid; const fieldType: TSQLDBFieldType);
  end;

  /// implements a connection via the libpq access layer

  { TSQLDBPostgresConnection }

  TSQLDBPostgresConnection = class(TSQLDBConnectionThreadSafe)
  private
  protected
    // prepared statement names
    // Initialized with fCaseSensitive, fNoDuplicate so use O(1) for indexOf
    FPQprepared: TRawUTF8List;
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

  { TSQLDBPostgresStatement }

  TSQLDBPostgresStatement = class(TSQLDBStatementWithParamsAndColumns)
  protected
    fPreparedStmtName: RawUTF8;
    fParsedSQL: RawUTF8;
    fPreparedParamsCount: longint;
    fRes: PPGresult;
    fResStatus: TExecStatusType; //fRes.resultStatus is not correct
    // pointers to query parameters. initialized by Prepare, filled in Executeprepared
    fPGParams: array of PChar;
    // 0-text, 1 - binary. initialized by Prepare, filled in Executeprepared
    fPGParamFormats: array of longint;
    // non zero for binary params
    fPGparamLengths: array of longint;
    procedure BindColumnus;
    /// raise an exception if Col is out of range according to fColumnCount
    // or rowset is not initialized
    procedure checkColAndRowset(const Col: integer); inline;
    //reserved for binary protocol fPrmLength: array of Longint;
  public
    constructor Create(aConnection: TSQLDBConnection); override;
    destructor Destroy; override;
    /// Prepare an UTF-8 encoded SQL statement
    // - parameters marked as ? will be bound later, before ExecutePrepared call
    // - if ExpectResults is TRUE, then Step() and Column*() methods are available
    // to retrieve the data rows
    // - raise an ESQLDBPostgres on any error
    procedure Prepare(const aSQL: RawUTF8; ExpectResults: boolean = False);
      overload; override;
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
    property preparedParamsCount: longint read fPreparedParamsCount;
  end;

var
  PQlibVersion: function: longint; cdecl;

implementation

uses
  SynLog,
  SynCrypto; //SHA256

{ TSQLDBPostgresConnection }
procedure TSQLDBPostgresConnection.Check(res: PPGresult; pRes: PPPGresult;
  forceClean: boolean);
var
  errMsg, errCode: PChar;
  errText: RawUTF8;
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
    errText := FormatUTF8('PGERRCODE: %, %', [errCode, errMsg]);
    PQclear(res);
    if pRes <> nil then
      pRes^ := nil;
    raise ESQLDBPostgres.CreateUTF8(errText, [self]);
  end else if forceClean then
   PQclear(res);
end;

procedure TSQLDBPostgresConnection.DirectExecSQL(const SQL: RawUTF8);
begin
  Check(PQexec(fPGConn, PChar(SQL)));
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
    fPGConn := PQsetdbLogin(PChar(Properties.ServerName), nil, // pgport,
      nil, // pgoptions,
      nil, // pgtty,
      PChar(Properties.DatabaseName), PChar(Properties.UserID),
      PChar(Properties.PassWord));
    if (PQstatus(fPGConn) = CONNECTION_BAD) then
      raise ESQLDBPostgres.CreateUTF8('Connection to database % failed. Reson: %',
        [PQerrorMessage(fPGConn)]);

    PQsetNoticeProcessor(fPGConn, SynLogNoticeProcessor, pointer(self));
    if log <> nil then
      log.Log(sllDB, 'Connected to % using % v%',
        [fProperties.ServerName, Postgres3LoadedLibrary, PQlibVersion()]);
    inherited Connect; // notify any re-connection
  except
    on E: Exception do
    begin
      if log <> nil then
        Log.Log(sllError, E);
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
  Result := (fPGConn <> nil);
end;

function TSQLDBPostgresConnection.NewStatement: TSQLDBStatement;
begin
  Result := TSQLDBPostgresStatement.Create(self);
end;

procedure TSQLDBPostgresConnection.StartTransaction;
var
  {%H-}Log: ISynLog;
begin
  Log := SynDBLog.Enter(self, 'StartTransaction');
  if TransactionCount > 0 then
    raise ESQLDBPostgres.CreateUTF8('Invalid %.StartTransaction: nested ' +
      'transactions are not supported by the Postgres - use SAVEPOINT instead', [self]);
  try
    inherited StartTransaction;
    DirectExecSQL('START TRANSACTION');
  except
    on E: Exception do
    begin
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
    Inc(fTransactionCount); // the transaction is still active
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
      '     (SELECT a.attname FROM pg_attribute a WHERE a.attnum = ct.conkey[1] AND a.attrelid = ct.conrelid) as from_ref, '
      +
      '  (SELECT tc.relname from pg_class tc where tc.oid = ct.confrelid) || ''.id'' as referenced_object '
      +
      'FROM  pg_constraint ct WHERE contype = ''f''', []) do
    while Step do
      fForeignKeys.Add(ColumnUTF8(2), ColumnUTF8(3));
end;

procedure TSQLDBPostgresConnectionProperties.fillOidMapping;
begin
  mapOid(702, ftDate); // abstime
  mapOid(1082, ftDate); // date
  mapOid(1083, ftDate); // time
  mapOid(1114, ftDate); // timestamp
  mapOid(1184, ftDate); // timestampz
  mapOid(1266, ftDate); // timez

  mapOid(20, ftInt64); // int8
  mapOid(21, ftInt64); // int2
  mapOid(23, ftInt64); // int4
  mapOid(24, ftInt64); // regproc
  mapOid(26, ftInt64); // oid

  mapOid(700, ftDouble); // float4
  mapOid(701, ftDouble); // float8
  mapOid(1700, ftDouble); // numeric

  mapOid(790, ftCurrency); // money

  mapOid(17, ftBlob); // bytea
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

  FOids2FieldType := TDictionary<Oid, TSQLDBFieldType>.Create();

  fillOidMapping();

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
  Result := TSQLDBPostgresConnection.Create(self);
end;

function TSQLDBPostgresConnectionProperties.oid2FieldType(
  const cOID: Oid): TSQLDBFieldType;
begin
  if not FOids2FieldType.TryGetValue(cOID, Result) then
    Result := ftUTF8;
end;

procedure TSQLDBPostgresConnectionProperties.mapOid(const cOid: Oid;
  const fieldType: TSQLDBFieldType);
begin
  FOids2FieldType.AddOrSetValue(cOid, fieldType);
end;

/// In case aSQL is simple (no ; inside)
// - replace all '?' in the SQL statement with position parameters $1, $2..
// - returns the number of ? parameters found within aSQL
//  For SQl with ; ( several statements should not be parametized)  or w/o ? return as is
// Compliant with Postgres expectations
function ReplaceQParamsByIndex(const aSQL: RawUTF8; var aNewSQL: RawUTF8): integer;
const
  Z = Ord('0');
var
  newL, prmIdx, L: PtrInt;
  pA, P: PAnsiChar;
  procedure CP() inline;
  begin
    P^ := pA^;
    Inc(P);
    Inc(pA);
  end;
begin
  Result := 0;
  L := Length(aSQL);
  pA := PAnsiChar(aSQL);
  // calculate ? parameters count, check for ;
  while (pA^ <> #0) do
  begin
    if (pA^ = '?') then
      Inc(Result)
    else if (pA^ = '''') then
    begin
      repeat // ignore chars inside ' quotes
        Inc(pA);
      until (pA^ = #0) or ((pA^ = '''') and ((pA + 1)^ <> ''''));
      if pA^ = #0 then
        break;
    end else if (pA^ = ';') then begin
      // ; inside - expression complex and can not be prepared
      Result := 0;
      break;
    end;
    Inc(pA);
  end;
  if (Result = 0) then
  begin // no ? parameters found or complex statement
    aNewSQL := aSQL;
    exit;
  end;
  // allocate space for parsed SQL with replaced ? -> $n
  // one additional char per param + one more for 10-99 + one more for 100-999
  newL := L + Result;
  if Result > 9 then
    newL := newL + (Result - 9);
  if (Result > 99) then
    newL := newL + (Result - 99);
  SetLength(aNewSQL, newL);
  P := PAnsiChar(aNewSQL);
  pA := PAnsiChar(aSQL);
  // copy to new SQl with replace
  prmIdx := 1;
  while (pA^ <> #0) do
  begin
    if (pA^ = '?') then
    begin
      P^ := '$';
      Inc(p);
      if (prmIdx > 99) then
      begin
        P^ := Chr(Z + (prmIdx div 100));
        Inc(P);
      end;
      if (prmIdx > 9) then
      begin
        P^ := Chr(Z + (prmIdx mod 100) div 10);
        Inc(P);
      end;
      P^ := Chr(Z + (prmIdx mod 10));
      Inc(P);
      Inc(prmIdx);
      Inc(pA);
    end
    else if (pA^ = '''') then
    begin
      repeat // ignore chars inside ' quotes
        CP();
      until (pA^ = #0) or ((pA^ = '''') and ((pA + 1)^ <> ''''));
      if pA^ = #0 then
        break;
      CP(); //last '
    end
    else
     CP();
  end;
end;

/// Convert array of RawUTF8 to PostgreSQL ARRAY
// ['one', 't"wo'] -> '{"one","t\"wo"}'
// ['1', '2', '3'] -> '{1,2,3}'
procedure UTF8Array2PostgreArray(const Values: array of RawUTF8; var postgreArray: RawByteString);
var i, j, k, n, L: Integer;
    P: PUTF8Char;
begin
  if high(Values)<0 then
    exit;
  L := 2; // '{}'
  inc(L,high(Values)); // , after each element
  for i := 0 to high(Values) do begin
    inc(L,length(Values[i]));
    for j := 2 to length(Values[i])-1 do
      if Values[i][j] = '"' then
        inc(L); // \ before "
  end;
  SetLength(postgreArray,L);
  P := pointer(postgreArray);
  P[0] := '{';
  i := 1;
  for n := 0 to high(Values) do begin
    if Values[n] = '' then continue;
    if Values[n][1] = '''' then begin
      P[i] := '"';
      inc(i);
      for k := 2 to length(Values[n])-1 do begin // skip first and last "
        if Values[n][k] = '"' then begin
          p[i] := '\';
          inc(i);
        end;
        p[i] := Values[n][k];
        inc(i);
      end;
      P[i] := '"';
      inc(i);
    end else
      for k := 1 to length(Values[n]) do begin
        p[i] := Values[n][k];
        inc(i);
      end;
    p[i] := ',';
    inc(i);
  end;
  if (i > 1) then begin
    p[i-1] := '}';
    SetLength(postgreArray,i);
  end else
    p[i] := '}';
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
        .oid2FieldType(colOID);
      //use PQfmod to get additional type info?
    end;
  end;
end;

procedure TSQLDBPostgresStatement.checkColAndRowset(const Col: integer);
begin
  CheckCol(Col);
  if (fRes = nil) or (fResStatus <> PGRES_TUPLES_OK) then
    raise ESQLDBPostgres.CreateUTF8('%.Execute should be called before', [self]);
end;

constructor TSQLDBPostgresStatement.Create(aConnection: TSQLDBConnection);
begin
  inherited Create(aConnection);
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
begin
  if aSQL = '' then
    raise ESQLDBPostgres.Create('Empty statement passed to prepare');
  inherited Prepare(aSQL, ExpectResults); // will strip last ;
  fPreparedParamsCount := ReplaceQParamsByIndex(fSQL, fParsedSQL);
  if (fPreparedParamsCount > 0) and
    (IdemPCharArray(PUTF8Char(fParsedSQL), ['SELECT', 'INSERT', 'UPDATE',
    'DELETE', 'VALUES']) <> -1) then
  begin // preparable
    fPreparedStmtName := SHA256(fParsedSQL);
    if TSQLDBPostgresConnection(Connection).FPQprepared.IndexOf(
      fPreparedStmtName) = -1 then
    begin // not prepared in this connection yet
      TSQLDBPostgresConnection(Connection).Check(
        PQPrepare(TSQLDBPostgresConnection(Connection).fPGConn,
          PChar(fPreparedStmtName), PChar(fParsedSQL), fPreparedParamsCount, nil)
        );
      TSQLDBPostgresConnection(Connection).FPQprepared.Add(fPreparedStmtName);
      SynDBLog.Add.Log(sllDebug, 'Prepare: Statement cache MISS', self);
    end
    else
      SynDBLog.Add.Log(sllDebug, 'Prepare: Statement cache HIT', self);
  end;
  SetLength(fPGParams, fPreparedParamsCount);
  SetLength(fPGParamFormats, fPreparedParamsCount);
  SetLength(fPGparamLengths, fPreparedParamsCount);
end;

procedure TSQLDBPostgresStatement.ExecutePrepared;
var
  strParams: array of RawByteString; // hold strings converted for non-string fParams
  i: longint;
begin
  if fParsedSQL = '' then
    raise ESQLDBPostgres.Create('Statement not prepared');
  if fParamCount <> fPreparedParamsCount then
    raise ESQLDBPostgres.CreateUTF8('Query expect % parameters but % is binded',
      [fPreparedParamsCount, fParamCount]);
  inherited ExecutePrepared;
  with SynDBLog.Add do
    if sllSQL in Family.Level then
      Log(sllSQL, fParsedSQL, self, 2048);
  if (fParamCount > 0) then
  begin // repack parameters as expected by Postgre
    SetLength({%H-}strParams, fParamCount);
    for  i := 0 to fParamCount - 1 do
    begin
      // mark parameter as textual by default
      fPGParamFormats[i] := 0;
      fPGparamLengths[i] := 0;
      if Length(fParams[i].VArray) > 0 then
      begin
        if not (fParams[i].VType in [ftInt64, ftUTF8]) then
          raise ESQLDBPostgres.CreateUTF8('%.ExecutePrepared: Invalid array type on bound parameter #%', [Self,i]);
        UTF8Array2PostgreArray(fParams[i].VArray, strParams[i]);
        fPGParams[i] := pointer(strParams[i]);
      end
      else
      begin
        case fParams[i].VType of
          ftUnknown:
            raise ESQLDBPostgres.CreateUTF8(
              'Can not bind parameter $% of type Unknown', [i]);
          ftNull:
            fPGParams[i] := nil;
          ftInt64:
          begin
            // use SwapEndian + binary ?
            strParams[i] := Int64ToUtf8(fParams[i].VInt64);
            fPGParams[i] := pointer(strParams[i]);
          end;
          ftCurrency: begin
            strParams[i] := Curr64ToStr(fParams[i].VInt64);
            fPGParams[i] := pointer(strParams[i]);
          end;
          ftDouble:
          begin
            strParams[i] := DoubleToStr(PDouble(@fParams[i].VInt64)^);
            fPGParams[i] := pointer(strParams[i]);
          end;
          ftDate:
          begin
            // Posgres expect space instead of T in ISO8601 format
            // with FPC direct cast to TDateTime is MUST (in other case unexpected results)
            strParams[i] := DateTimeToIso8601(TDateTime(fParams[i].VInt64), true, ' ');
            fPGParams[i] := pointer(strParams[i]);
          end;
          ftUTF8:
            fPGParams[i] := pointer(fParams[i].VData);
          ftBlob:
          begin
            fPGParams[i] := pointer(fParams[i].VData);
            fPGParamFormats[i] := 1; // binary
            fPGparamLengths[i] := length(fParams[i].VData);
          end;
        end;
      end;
    end;
  end;
  if (fPreparedStmtName <> '') then
    fRes := PQexecPrepared(TSQLDBPostgresConnection(Connection).fPGConn,
      PChar(fPreparedStmtName), fPreparedParamsCount, pointer(fPGParams),
      pointer(fPGparamLengths), pointer(fPGParamFormats), 0)
  else if fPreparedParamsCount = 0 then // PQexec can include multiple SQL commands
    fRes := PQexec(TSQLDBPostgresConnection(Connection).fPGConn, PChar(fParsedSQL))
  else
    fRes := PQexecParams(TSQLDBPostgresConnection(Connection).fPGConn,
      PChar(fParsedSQL), fPreparedParamsCount, nil, pointer(fPGParams),
      pointer(fPGparamLengths), pointer(fPGParamFormats), 0);
  TSQLDBPostgresConnection(Connection).Check(fRes, @fRes, false{forceClean});

  fResStatus := PQresultStatus(fRes);
  if fExpectResults then
  begin
    if (fResStatus <> PGRES_TUPLES_OK) then
    begin // may be we do not need check?
      PQclear(fRes);
      fRes := nil;
      raise ESQLDBPostgres.Create('Result expected but statement not return tuples');
    end;
    fTotalRowsRetrieved := PQntuples(fRes);
    fCurrentRow := -1;
    if fColumn.Count = 0 then // if columnus exists then statement is already cached
      BindColumnus;
  end;
end;

function TSQLDBPostgresStatement.UpdateCount: integer;
begin
  Result := GetCardinalDef(PQcmdTuples(fRes), 0);
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
  if (SeekFirst) then
    fCurrentRow := -1;
  Result := fCurrentRow + 1 < fTotalRowsRetrieved;
  if not Result then
    exit;
  Inc(fCurrentRow);
end;

function TSQLDBPostgresStatement.ColumnInt(Col: integer): int64;
begin
  checkColAndRowset(Col);
  Result := GetInt64(PQgetvalue(fRes, fCurrentRow, Col));
end;

function TSQLDBPostgresStatement.ColumnNull(Col: integer): boolean;
begin
  checkColAndRowset(Col);
  Result := (PQgetisnull(fRes, fCurrentRow, Col) = 1);
end;

function TSQLDBPostgresStatement.ColumnDouble(Col: integer): double;
begin
  checkColAndRowset(Col);
  Result := GetExtended(PQgetvalue(fRes, fCurrentRow, Col));
end;

function TSQLDBPostgresStatement.ColumnDateTime(Col: integer): TDateTime;
begin
  checkColAndRowset(Col);
  Iso8601ToDateTimePUTF8CharVar(PQgetvalue(fRes, fCurrentRow, Col),
    PQgetlength(fRes, fCurrentRow, Col), Result);
end;

function TSQLDBPostgresStatement.ColumnCurrency(Col: integer): currency;
begin
  checkColAndRowset(Col);
  Result := GetExtended(PQgetvalue(fRes, fCurrentRow, Col));
end;

function TSQLDBPostgresStatement.ColumnUTF8(Col: integer): RawUTF8;
begin
  checkColAndRowset(Col);
  FastSetString(Result, PQgetvalue(fRes, fCurrentRow, Col),
    PQgetlength(fRes, fCurrentRow, Col)
    );
end;

function TSQLDBPostgresStatement.ColumnBlob(Col: integer): RawByteString;
var
  l: size_t;
  blb: PByte;
begin
  checkColAndRowset(Col);
  blb := PQunescapeBytea(PByte(PQgetvalue(fRes, fCurrentRow, Col)), @l);
  try
    if blb = nil then
      raise EOutOfMemory.Create('PQunescapeBytea: Out of memory');
    FastSetStringCP(Result, blb, l, CP_NONE);
  finally
    if blb <> nil then
      PQfreemem(blb);
  end;
end;

end.
