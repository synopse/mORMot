/// ZEOS 7.x direct access classes for SynDB units (not DB.pas based)
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynDBZEOS;

{
  This file is part of Synopse framework.

  Synopse framework. Copyright (C) 2014 Arnaud Bouchez
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
  - delphinium
  - EgonHugeist
  - alexpirate


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
  - first public release, corresponding to mORMot framework 1.18

(*
  Note:
  - if you want to work as expected with SQlite3 backend (but how would need to
    do it, since it will be MUCH slower compared to SynDBSQlite3), you need
    to apply some patches for Zeos < 7.2 :

function TZSQLiteCAPIPreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  if Not Prepared then
     Prepare;
  try
    if LastResultSet <> nil then
      LastResultSet.Close; // reset stmt
    LastResultSet := nil;
    BindInParameters;
    FErrorCode := FPlainDriver.Step(FStmtHandle);
    CheckSQLiteError(FPlainDriver, FStmtHandle, FErrorCode, nil, lcOther,
      ConSettings^.ConvFuncs.ZStringToRaw(SCanNotRetrieveResultsetData, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP),
      ConSettings);
    if ( FErrorCode = SQLITE_ROW ) or ( FErrorCode = SQLITE_DONE) then
      LastResultSet := CreateResultSet(FStmtHandle, FErrorCode);
    Result := LastResultSet;
    inherited ExecuteQueryPrepared;
  except
    raise;
  end;
end;

procedure TZSQLiteResultSet.FreeHandle;
var
  ErrorCode: Integer;
begin
  if FFreeHandle then
  begin
    if Assigned(FStmtHandle) then
      ErrorCode := FPlainDriver.Finalize(FStmtHandle)
    else
      ErrorCode := SQLITE_OK;
    FStmtHandle := nil;
    CheckSQLiteError(FPlainDriver, FStmtHandle, ErrorCode, nil,
      lcOther, 'FINALIZE SQLite VM', ConSettings);
  end
  else
  begin
    if FStmtHandle <> nil then
    begin
      ErrorCode := FPlainDriver.reset(FStmtHandle);
      CheckSQLiteError(FPlainDriver, FStmtHandle, ErrorCode, nil, lcBindPrepStmt, 'Reset Prepared Stmt', ConSettings);
      FStmtHandle := nil;
    end;
    FErrorCode := SQLITE_DONE;
  end;
end;

 ... but you should better upgrade to the latest Zeos 7.2 revision! :)
    See http://synopse.info/forum/viewtopic.php?pid=9146#p9146
 
*)

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

{$I Zeos.inc} // define conditionals like ZEOS72UP and ENABLE_*

interface

uses
  Windows, Types, SysUtils,
  {$IFNDEF DELPHI5OROLDER}
  Variants,
  {$ENDIF}
  Classes, Contnrs,
  SynCommons,
  SynDB,
  // load physical providers as defined by ENABLE_* in Zeos.inc
  // -> you can patch your local Zeos.inc and comment these defines to
  // exclude database engines you don't need
  {$IFNDEF UNIX}
  {$IFDEF ENABLE_ADO}
  ZDbcAdo,
  {$ENDIF}
  {$ENDIF}
  {$IFDEF ENABLE_DBLIB}
  ZDbcDbLib,
  {$ENDIF}
  {$IFDEF ENABLE_MYSQL}
  ZDbcMySql,
  {$ENDIF}
  {$IFDEF ENABLE_POSTGRESQL}
  ZDbcPostgreSql,
  {$ENDIF}
  {$IFDEF ENABLE_INTERBASE}
  ZDbcInterbase6,
  {$ENDIF}
  {$IFDEF ENABLE_SQLITE}
  ZDbcSqLite,
  {$ENDIF}
  {$IFDEF ENABLE_ORACLE}
  ZDbcOracle,
  {$ENDIF}
  {$IFDEF ENABLE_ASA}
  ZDbcASA,
  {$ENDIF}
  {$IFDEF ENABLE_POOLED}
  ZDbcPooled,
  {$ENDIF}
  // main ZDBC units
  ZURL, ZDbcIntfs, ZDbcResultSet;




{ -------------- ZEOS database components direct process }

type
  /// Exception type associated to the ZEOS database components
  ESQLDBZEOS = class(ESQLDBException);


  /// implement properties shared by ZEOS connections
  TSQLDBZEOSConnectionProperties = class(TSQLDBConnectionPropertiesThreadSafe)
  protected
    fURL: TZURL;
    fStatementParams: TStrings;
    fDBMSName: RawUTF8;
    /// initialize fForeignKeys content with all foreign keys of this DB
    // - do nothing by now (ZEOS metadata may be used in the future)
    procedure GetForeignKeys; override;
    /// convert ZDBC field type into mORMot fieldtype
    function TZSQLTypeToTSQLDBFieldType(aNativeType: TZSQLType): TSQLDBFieldType; virtual;
  public
    /// initialize the properties to connect to the ZEOS engine
    // - aServerName shall contain the ZEOS URI, e.g:
    // $ zdbc:firebird-2.0://127.0.0.1:3050/model?username=sysdba;password=masterkey
    // $ zdbc:mysql://192.168.2.60:3306/world?username=root;password=dev
    // $ sqlite
    // i.e. '[zdbc:]PROTOCOL://HOST:PORT[/DATABASE][?paramname=value]'
    // - you can define the TZConnection.LibraryLocation property by setting a
    // '?LibLocation=...' parameter within the aServerName URL value
    // - or simple use TSQLDBZEOSConnectionProperties.URI() class method
    // - aDatabaseName, aUserID, aPassword are used if not already set as URI
    // in aServerName value
    // - you can use Protocols property to retrieve all available protocol names
    constructor Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8); override;
    /// finalize properties internal structures
    destructor Destroy; override;
    /// create a new connection
    // - caller is responsible of freeing this instance
    // - this overridden method will create an TSQLDBZEOSConnection instance
    function NewConnection: TSQLDBConnection; override;

    /// retrieve the column/field layout of a specified table
    // - this overridden method will use ZDBC metadata to retrieve the information
    procedure GetFields(const aTableName: RawUTF8; var Fields: TSQLDBColumnDefineDynArray); override;
    /// get all table names
    // - this overridden method will use ZDBC metadata to retrieve the information
    // - PostgreSQL note: it was reported that some table names expects to be
    // quoted for this DB engine - and ZDBC won't do it for yourself - please
    // ensure you specify the correct quoted table name e.g. when you register
    // the external PostgreSQL table via function VirtualTableExternalRegister()
    procedure GetTableNames(var Tables: TRawUTF8DynArray); override;
    /// access to the database metadata, as retrieved by ZEOS
    // - returns TRUE if metadata interface has been retrieved
    function GetDatabaseMetadata(out meta: IZDatabaseMetadata): boolean;
    /// compute the ZEOS URI from a given database engine
    // - the optional server name can contain a port number, specified after ':'
    // - you can set an optional full path to the client library name,
    // to be completed on the left side with the executable path
    // - possible use may be:
    // ! PropsOracle := TSQLDBZEOSConnectionProperties.Create(
    // !   TSQLDBZEOSConnectionProperties.URI(dOracle,'','oci64\oci.dll'),
    // !   'tnsname','user',pass');
    // ! PropsFirebird := TSQLDBZEOSConnectionProperties.Create(
    // !   TSQLDBZEOSConnectionProperties.URI(dFirebird,'','Firebird\fbembed.dll'),
    // !   'databasefilename','',');
    class function URI(aServer: TSQLDBDefinition;
      const aServerName: RawUTF8; const aLibraryLocation: TFileName='';
      aLibraryLocationAppendExePath: boolean=true): RawUTF8;
  published
    /// the remote DBMS name, as retrieved from ServerName, i.e. ZEOS URL
    property DBMSName: RawUTF8 read fDBMSName;
    /// direct access to the internal TZURL connection parameters
    property ZeosURL: TZURL read fURL;
    /// direct access to the internal statement parameters
    // - i.e. will be used by IZConnection.PrepareStatementWithParams()
    // - default values (set in Create method) try to achieve best permormance
    property ZeosStatementParams: TStrings read fStatementParams;
  end;


  /// implements a connection via the ZEOS access layer
  TSQLDBZEOSConnection = class(TSQLDBConnectionThreadSafe)
  protected
    fDatabase: IZConnection;
  public
    /// prepare a connection to a specified ZEOS database server
    constructor Create(aProperties: TSQLDBConnectionProperties); override;
    /// connect to the specified ZEOS server
    // - should raise an ESQLDBZEOS on error
    procedure Connect; override;
    /// stop connection to the specified ZEOS database server
    // - should raise an ESQLDBZEOS on error
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
    /// access to the associated ZEOS connection instance
    property Database: IZConnection read fDatabase;
  end;

  /// implements a statement via a ZEOS database connection
  TSQLDBZEOSStatement = class(TSQLDBStatementWithParamsAndColumns)
  protected
    fStatement: IZPreparedStatement;
    fResultSet: IZResultSet;
    fResultInfo: IZResultSetMetaData;
  public
    {{ Prepare an UTF-8 encoded SQL statement
      - parameters marked as ? will be bound later, before ExecutePrepared call
      - if ExpectResults is TRUE, then Step() and Column*() methods are available
      to retrieve the data rows
      - raise an ESQLDBZeos on any error }
    procedure Prepare(const aSQL: RawUTF8; ExpectResults: boolean = false); overload; override;
    {{ Execute a prepared SQL statement
      - parameters marked as ? should have been already bound with Bind*() functions
      - this implementation will also loop through all internal bound array
      of values (if any), to implement BATCH mode
      - this overridden method will log the SQL statement if sllSQL has been
        enabled in SynDBLog.Family.Level
      - raise an ESQLDBZeos on any error }
    procedure ExecutePrepared; override;
    {/ Reset the previous prepared statement
     - this overridden implementation will reset all bindings and the cursor state
     - raise an ESQLDBZeos on any error }
    procedure Reset; override;

    {{ Access the next or first row of data from the SQL Statement result
      - return true on success, with data ready to be retrieved by Column*() methods
      - return false if no more row is available (e.g. if the SQL statement
      is not a SELECT but an UPDATE or INSERT command)
      - if SeekFirst is TRUE, will put the cursor on the first row of results
      - raise an ESQLDBZeos on any error }
    function Step(SeekFirst: boolean = false): boolean; override;
    {{ return a Column integer value of the current Row, first Col is 0 }
    function ColumnInt(Col: Integer): Int64; override;
    {{ returns TRUE if the column contains NULL }
    function ColumnNull(Col: Integer): boolean; override;
    {{ return a Column floating point value of the current Row, first Col is 0 }
    function ColumnDouble(Col: Integer): double; override;
    {{ return a Column date and time value of the current Row, first Col is 0 }
    function ColumnDateTime(Col: Integer): TDateTime; override;
    {{ return a Column currency value of the current Row, first Col is 0 }
    function ColumnCurrency(Col: Integer): currency; override;
    {{ return a Column UTF-8 encoded text value of the current Row, first Col is 0 }
    function ColumnUTF8(Col: Integer): RawUTF8; override;
    {{ return a Column as a blob value of the current Row, first Col is 0 }
    function ColumnBlob(Col: Integer): RawByteString; override;
  end;

var
  /// list of all available ZEOS protocols
  // - you have to call SetZEOSProtocols before using it, to update this
  // global list with all initialized ZPlain*Driver units
  // - to be used e.g. within ZEOS URI, as TSQLDBZEOSConnectionProperties.ServerName
  ZEOSProtocols: TRawUTF8DynArray;

/// to be called in order to populate the global ZEOSProtocols list
procedure SetZEOSProtocols;


implementation



{ TSQLDBZEOSConnectionProperties }

constructor TSQLDBZEOSConnectionProperties.Create(const aServerName, aDatabaseName, aUserID, aPassWord: RawUTF8);
const
  PCHARS: array[0..7] of PAnsiChar = (
    'ORACLE','FREETDS_MSSQL','MSSQL','INTERBASE','FIREBIRD','MYSQL','SQLITE','POSTGRESQL');
  TYPES: array[-1..high(PCHARS)] of TSQLDBDefinition = (
    dDefault,dOracle,dMSSQL,dMSSQL,dFirebird,dFirebird,dMySQL,dSQLite,dPostgreSQL);
  // expecting Postgresql + Sybase + ASA support in TSQLDBDefinition
begin
  fServerName :=  aServerName;
  if (fServerName<>'') and (PosEx(':',fServerName)=0) then
    fServerName := fServerName+':';
  if not IdemPChar(Pointer(aServerName),'ZDBC:') then
    fServerName := 'zdbc:'+fServerName;
  fURL := TZURL.Create(UTF8ToString(fServerName));
  if fURL.Database='' then
    fURL.Database := UTF8ToString(aDatabaseName);
  if fURL.UserName='' then
    fURL.UserName := UTF8ToString(aUserID);
  if fURL.Password='' then
    fURL.Password := UTF8ToString(aPassWord);
  StringToUTF8(fURL.Protocol,fDBMSName);
  fDBMS := TYPES[IdemPCharArray(pointer(fDBMSName),PCHARS)];
  inherited Create(aServerName,aDatabaseName,aUserID,aPassWord);
  fURL.Properties.Add('controls_cp=CP_UTF8');
  fUseCache := false; // caching is to be disabled - not found stable enough
  case fDBMS of
  dSQLite: begin
    fSQLCreateField[ftInt64] := ' BIGINT'; // SQLite3 INTEGER = 32bit for ZDBC!
    {$ifdef ZEOS72UP}
    fUseCache := true; // statement cache has been fixed in 7.2 branch
    {$endif}
  end;
  dFirebird: begin
    if (fURL.HostName='') and // Firebird embedded: create db file if needed
       (fURL.Database<>'') and not FileExists(fURL.Database) then
      fURL.Properties.Add('createNewDatabase='+UTF8ToString(
        SQLCreateDatabase(StringToUTF8(fURL.Database))));
    fURL.Properties.Add('codepage=UTF8');
    fUseCache := true; // caching rocks with Firebird ZDBC provider :)
    if Assigned(OnBatchInsert) then begin
      // ZDBC: MultipleValuesInsertFirebird is buggy, MultipleValuesInsert slower
      fBatchSendingAbilities := [];
      OnBatchInsert := nil;
    end;
  end;
  dOracle, dPostgreSQL, dMySQL: begin
    fURL.Properties.Add('codepage=UTF8');
    fUseCache := true;  
  end;
  end;
  fStatementParams := TStringList.Create;
  case fDBMS of
  dOracle:
    // sets OCI_ATTR_PREFETCH_ROWS on prepare a fetch
    // default = 100 on 7.1down and 1000 on 7.2up
    fStatementParams.Add('prefetch_count=100000');
  dSQLite: begin
    {$ifdef ZEOS72UP} // new since 7.2up
    // get access to the native resultset. This only works sequention/forwardonly
    // (step) and YOU have to localize the values
    fStatementParams.Add('ForceNativeResultSet=True');
    // Bind double values instead of ISO formated DateTime-strings
    //fStatementParams.Add('BindDoubleDateTimeValues=True');
    {$endif}
  end;
  dMySQL: begin
    // use mysql real-prepared api instead of string based once
    // actually it's not realy faster.. just a hint:
    // http://dev.mysql.com/doc/refman/5.0/en/c-api-prepared-statement-problems.html
    //fStatementParams.Add('preferprepared=True');
  end;
  end;
  if fDBMS in [dOracle,dPostgreSQL,dMySQL] then begin
    // let's set 1024KB / chunk for synopse  or more?
    // retrieving/submitting lob's in chunks. Default is 4096Bytes / Chunk
    // it's depending to your local network speed e.g. bad WLAN or so
    // for Firebird we always using the blob-segment size
    fStatementParams.Add('chunk_size=1048576');
  end;
  if fDBMS in [dOracle,dPostgreSQL,dFireBird] then begin
    {$ifdef ZEOS72UP} // new since 7.2up
    // Always load the lobs? Or just on accessing them?
    // if you allways copy the data by fetching the row than it doesn't make sence.
    fStatementParams.Add('cachedlob=false'); //default = False
    {$endif}
  end;

end;

procedure TSQLDBZEOSConnectionProperties.GetForeignKeys;
begin
  { TODO : get FOREIGN KEYS from ZEOS metadata ? }
end;

function TSQLDBZEOSConnectionProperties.NewConnection: TSQLDBConnection;
begin
  result := TSQLDBZEOSConnection.Create(self);
end;

destructor TSQLDBZEOSConnectionProperties.Destroy;
begin
  FreeAndNil(fURL);
  FreeAndNil(fStatementParams);
  inherited;
end;

procedure SetZEOSProtocols;
var List: TStringList;
    i,j: integer;
    Protocols: Types.TStringDynArray;
begin
  List := TStringList.Create;
  try
    with DriverManager.GetDrivers do
      for i := 0 to Count-1 do begin
        Protocols := (Items[i] as IZDriver).GetSupportedProtocols;
        for j := 0 to high(Protocols) do
          List.Add(Protocols[j]);
      end;
    List.Sort;
    StringListToRawUTF8DynArray(List,ZEOSProtocols);
  finally
    List.Free;
  end;
end;

function TSQLDBZEOSConnectionProperties.GetDatabaseMetadata(out meta: IZDatabaseMetadata): boolean;
var conn: IZConnection;
begin
  conn := (MainConnection as TSQLDBZEOSConnection).fDatabase;
  result := conn.UseMetadata;
  if result then begin
    meta := conn.GetMetadata;
    meta.ClearCache; // we need to retrieve the actual metadata
  end;
end;

procedure TSQLDBZEOSConnectionProperties.GetTableNames(var Tables: TRawUTF8DynArray);
var meta: IZDatabaseMetadata;
    res: IZResultSet;
    TableTypes: Types.TStringDynArray;
    n: integer;
begin
  if GetDatabaseMetadata(meta) then begin
    SetLength(TableTypes,1);
    TableTypes[0] := 'TABLE';
    res := meta.GetTables('','','',TableTypes);
    n := 0;
    while res.Next do
      AddSortedRawUTF8(Tables,n,SynUnicodeToUtf8(res.GetUnicodeString(3)));
    SetLength(Tables,n);
  end else
    inherited;
end;

procedure TSQLDBZEOSConnectionProperties.GetFields(
  const aTableName: RawUTF8; var Fields: TSQLDBColumnDefineDynArray);
var meta: IZDatabaseMetadata;
    res: IZResultSet;
    n, i: integer;
    Schema, TableName: RawUTF8;
    sSchema, sTableName: string;
    F: TSQLDBColumnDefine;
    FA: TDynArray;
begin
  if GetDatabaseMetadata(meta) then begin
    SQLSplitTableName(UpperCase(aTablename), Schema,TableName);
    sSchema := UTF8ToString(Schema);
    sTableName := meta.GetIdentifierConvertor.Quote(UTF8ToString(TableName));
    res := meta.GetColumns('',sSchema,sTableName,'');
    FA.InitSpecific(TypeInfo(TSQLDBColumnDefineDynArray),Fields,djRawUTF8,@n,true);
    FillChar(F,sizeof(F),0);
    while res.Next do begin
      F.ColumnName := SynUnicodeToUtf8(res.GetUnicodeString(4));
      F.ColumnTypeNative := SynUnicodeToUtf8(res.GetUnicodeString(6));
      F.ColumnType := TZSQLTypeToTSQLDBFieldType(TZSQLType(res.GetInt(5)));
      F.ColumnLength := res.GetInt(7);
      F.ColumnPrecision := res.GetInt(9);
      FA.Add(F);
    end;
    if n>0 then begin
      res := meta.GetIndexInfo('',sSchema,sTableName,false,true);
      while res.Next do begin
        F.ColumnName := SynUnicodeToUtf8(res.GetUnicodeString(9));
        i := FA.Find(F);
        if i>=0 then
          Fields[i].ColumnIndexed := true;
      end;
      SetLength(Fields,n);
      exit;
    end;
  end;
  inherited; // if ZDBC metadata failed -> fall back to generic SQL-based code
end;

function TSQLDBZEOSConnectionProperties.TZSQLTypeToTSQLDBFieldType(aNativeType: TZSQLType): TSQLDBFieldType;
begin
  case aNativeType of
    stBoolean, stByte, stShort, stInteger, stLong
    {$ifdef ZEOS72UP}, stSmall, stWord, stLongWord, stULong{$endif}:
      result := ftInt64;
    stFloat, stDouble:
      result := ftDouble;
    stBigDecimal{$ifdef ZEOS72UP}, stCurrency{$endif}:
      result := ftCurrency;
    stDate, stTime, stTimestamp:
      result := ftDate;
    stString, stUnicodeString, stAsciiStream, stUnicodeStream:
      result := ftUTF8;
    stBytes, stBinaryStream:
      result := ftBlob;
    else raise ESQLDBZEOS.CreateFmt('Unexpected TZSQLType "%s"',
      [{$ifdef PUREPASCAl}IntToStr(ord(aNativeType)){$else}
       GetEnumName(Typeinfo(TZSQLType),ord(aNativeType))^{$endif}]);
  end;
end;

class function TSQLDBZEOSConnectionProperties.URI(aServer: TSQLDBDefinition;
  const aServerName: RawUTF8;
  const aLibraryLocation: TFileName; aLibraryLocationAppendExePath: boolean): RawUTF8;
const
  /// ZDBC provider names corresponding to SynDB recognized SQL engines
  ZEOS_PROVIDER: array[TSQLDBDefinition] of RawUTF8 = (
    '','','oracle:','mssql:','','mysql:','sqlite:',
    'firebird-2.5:','','postgresql-9:','');
begin // return e.g. mysql://192.168.2.60:3306/world?username=root;password=dev
  result := ZEOS_PROVIDER[aServer];
  if (result='') or (aLibraryLocation='') then
    exit;
  if aServerName<>'' then
    result := result+'//'+aServerName;
  if aLibraryLocation<>'' then begin
    result := result+'?LibLocation=';
    if aLibraryLocationAppendExePath then
      result := result+StringToUTF8(ExtractFilePath(ParamStr(0)));
    result := result+StringToUTF8(aLibraryLocation);
  end;
end;


{ TSQLDBZEOSConnection }

constructor TSQLDBZEOSConnection.Create(aProperties: TSQLDBConnectionProperties);
begin
  inherited Create(aProperties);
  fDatabase := DriverManager.GetConnectionWithParams(
    (fProperties as TSQLDBZEOSConnectionProperties).fURL.URL,nil);
  fDatabase.SetAutoCommit(true);
  fDatabase.SetTransactionIsolation(tiNone);
end;

procedure TSQLDBZEOSConnection.Connect;
var Log: ISynLog;
begin
  if fDatabase=nil then
    raise ESQLDBZEOS.CreateFmt('TSQLDBZEOSConnection.Connect() on %s failed: Database=nil',
      [fProperties.ServerName]);
  with (fProperties as TSQLDBZEOSConnectionProperties).fURL do
    Log := SynDBLog.Enter(Self,pointer(FormatUTF8('Connect to % % for % at %:%',
      [Protocol,Database,HostName,Port])),true);
  try
    fDatabase.Open;
    Log.Log(sllDB,'Connected to % using % %',
      [fProperties.ServerName,fProperties.DatabaseName,fDatabase.GetClientVersion]);
    inherited Connect; // notify any re-connection 
  except
    on E: Exception do begin
      Log.Log(sllError,E);
      Disconnect; // clean up on fail
      raise;
    end;
  end;
end;

procedure TSQLDBZEOSConnection.Disconnect;
begin
  try
    inherited Disconnect; // flush any cached statement
  finally
    if (fDatabase<>nil) and not fDatabase.IsClosed then
      fDatabase.Close;
  end;
end;

function TSQLDBZEOSConnection.IsConnected: boolean;
begin
  result := Assigned(fDatabase) and not fDatabase.IsClosed;
end;

function TSQLDBZEOSConnection.NewStatement: TSQLDBStatement;
begin
  result := TSQLDBZEOSStatement.Create(self);
end;

procedure TSQLDBZEOSConnection.StartTransaction;
begin
  inherited StartTransaction;
  fDatabase.SetAutoCommit(false);
  fDatabase.SetTransactionIsolation(tiReadCommitted);
end;

procedure TSQLDBZEOSConnection.Commit;
begin
  inherited Commit;
  fDatabase.Commit;
  fDatabase.SetAutoCommit(true);
  fDatabase.SetTransactionIsolation(tiNone);
end;

procedure TSQLDBZEOSConnection.Rollback;
begin
  inherited Rollback;
  fDatabase.Rollback;
  fDatabase.SetAutoCommit(true);
  fDatabase.SetTransactionIsolation(tiNone);
end;



{ TSQLDBZEOSStatement }

procedure TSQLDBZEOSStatement.Prepare(const aSQL: RawUTF8;
  ExpectResults: boolean);
var Log: ISynLog;
begin
  Log := SynDBLog.Enter(Self);
  if (fStatement<>nil) or (fResultSet<>nil) then
    raise ESQLDBZEOS.CreateFmt('%s.Prepare() shall be called once',[fStatementClassName]);
  inherited Prepare(aSQL,ExpectResults); // connect if necessary
  fStatement := (fConnection as TSQLDBZEOSConnection).fDatabase.
    PrepareStatementWithParams(UTF8ToString(fSQL),
    (fConnection.Properties as TSQLDBZEOSConnectionProperties).fStatementParams);
end;

procedure TSQLDBZEOSStatement.ExecutePrepared;
var i: integer;
    Props: TSQLDBZEOSConnectionProperties;
    Log: ISynLog;
    blob: IZBlob;
    name: string;
begin
  Log := SynDBLog.Enter(Self);
  with Log.Instance do
    if sllSQL in Family.Level then
      LogLines(sllSQL,pointer(SQLWithInlinedParams),self,'--');
  if fStatement=nil then
    raise ESQLDBZEOS.CreateFmt('%s.ExecutePrepared() invalid call',[fStatementClassName]);
  if fResultSet<>nil then
    raise ESQLDBZEOS.CreateFmt('%s.ExecutePrepared() miss a Reset',[fStatementClassName]);
  // 1. bind parameters in fParams[] to fQuery.Params
  for i := 1 to fParamCount do
    with fParams[i-1] do
    case VType of
    ftNull:     fStatement.SetNull(i,stUnknown);
    ftInt64:    fStatement.SetLong(i,VInt64);
    ftDouble:   fStatement.SetDouble(i,PDouble(@VInt64)^);
    ftCurrency: {$ifdef ZEOS72UP}
                fStatement.SetCurrency(i,PCurrency(@VInt64)^);
                {$else}
                fStatement.SetBigDecimal(i,PCurrency(@VInt64)^);
                {$endif}
    ftDate:     fStatement.SetTimestamp(i,PDateTime(@VInt64)^);
    ftUTF8:     {$ifdef ZEOS72UP}
                fStatement.SetUTF8String(i,VData);
                {$else}
                  {$ifdef UNICODE}  // ZWideString = SynUnicode in fact
                  fStatement.SetString(i,UTF8ToSynUnicode(VData));
                  {$else}
                  fStatement.SetString(i,VData); // see controls_cp=CP_UTF8
                  {$endif}
                {$endif}
    ftBlob: begin
      blob := TZAbstractBlob.CreateWithData(Pointer(VData),length(VData)
        {$ifndef ZEOS72UP},fStatement.GetConnection{$endif});
      fStatement.SetBlob(i,stBinaryStream,blob);
    end;
    else
      raise ESQLDBZEOS.CreateFmt('%s.ExecutePrepared: Invalid type on bound parameter #%d',
        [fStatementClassName,i]);
    end;
  // 2. Execute query
  if fExpectResults then begin
    fCurrentRow := -1;
    fResultSet := fStatement.ExecuteQueryPrepared;
    if fResultSet=nil then
      raise ESQLDBZEOS.CreateFmt('%s.ExecutePrepared(%s) returned nil',
        [fStatementClassName,SQLWithInlinedParams]);
    fResultInfo := fResultSet.GetMetadata;
    Props := fConnection.Properties as TSQLDBZEOSConnectionProperties;
    fColumnCount := 0;
    fColumn.ReHash;
    for i := 1 to fResultInfo.GetColumnCount do begin
      name := fResultInfo.GetColumnLabel(i);
      if name='' then
        name := fResultInfo.GetColumnName(i);
      PSQLDBColumnProperty(fColumn.AddAndMakeUniqueName(
        // Delphi<2009: already UTF-8 encoded due to controls_cp=CP_UTF8
        {$ifdef UNICODE}StringToUTF8{$endif}(name)))^.ColumnType :=
          Props.TZSQLTypeToTSQLDBFieldType(fResultInfo.GetColumnType(i));
    end;
  end else
    fStatement.ExecutePrepared;
  // 3. handle out parameters -> TODO (fStatement is IZCallableStatement)
end;

procedure TSQLDBZEOSStatement.Reset;
begin
  if fResultSet<>nil then begin
    fResultInfo := nil;
    fResultSet := nil;
  end;
  if fStatement<>nil then
    fStatement.ClearParameters;
  inherited Reset;
end;

function TSQLDBZEOSStatement.Step(SeekFirst: boolean): boolean;
begin
  if fResultSet=nil then
    raise ESQLDBZEOS.CreateFmt('%s.Step() invalid call',[fStatementClassName]);
  if fColumnCount=0 then // no row returned
    result := false else
  if SeekFirst then begin
    result := fResultSet.First;
    if result then
      fCurrentRow := 1 else
      fCurrentRow := 0;
  end else begin
    result := fResultSet.Next;
    if result then
      inc(fCurrentRow);
  end;
end;

function TSQLDBZEOSStatement.ColumnBlob(Col: Integer): RawByteString;
var blob: IZBlob;
begin
  if (fResultSet=nil) or (cardinal(Col)>=cardinal(fColumnCount)) then
    raise ESQLDBZEOS.CreateFmt('%s.ColumnBlob(%d)',[fStatementClassName,Col]);
  blob := fResultSet.GetBlob(Col+1);
  if (blob=nil) or blob.IsEmpty then
    result := '' else
    result := blob.GetString; // ZAnsiString = RawByteString
end;

function TSQLDBZEOSStatement.ColumnCurrency(Col: Integer): currency;
begin
  if (fResultSet=nil) or (cardinal(Col)>=cardinal(fColumnCount)) then
    raise ESQLDBZEOS.CreateFmt('%s.ColumnCurrency(%d)',[fStatementClassName,Col]);
  {$ifdef ZEOS72UP}
  result := fResultSet.GetCurrency(Col+1);
  {$else}
  result := fResultSet.GetBigDecimal(Col+1);
  {$endif}
end;

function TSQLDBZEOSStatement.ColumnDateTime(Col: Integer): TDateTime;
begin
  if (fResultSet=nil) or (cardinal(Col)>=cardinal(fColumnCount)) then
    raise ESQLDBZEOS.CreateFmt('%s.ColumnDateTime(%d)',[fStatementClassName,Col]);
  result := fResultSet.GetTimestamp(Col+1);
end;

function TSQLDBZEOSStatement.ColumnDouble(Col: Integer): double;
begin
  if (fResultSet=nil) or (cardinal(Col)>=cardinal(fColumnCount)) then
    raise ESQLDBZEOS.CreateFmt('%s.ColumnDouble(%d)',[fStatementClassName,Col]);
  result := fResultSet.GetDouble(Col+1);
end;

function TSQLDBZEOSStatement.ColumnInt(Col: Integer): Int64;
begin
  if (fResultSet=nil) or (cardinal(Col)>=cardinal(fColumnCount)) then
    raise ESQLDBZEOS.CreateFmt('%s.ColumnInt(%d)',[fStatementClassName,Col]);
  result := fResultSet.GetLong(Col+1);
end;

function TSQLDBZEOSStatement.ColumnNull(Col: Integer): boolean;
begin
  if (fResultSet=nil) or (cardinal(Col)>=cardinal(fColumnCount)) then
    raise ESQLDBZEOS.CreateFmt('%s.ColumnNull(%d)',[fStatementClassName,Col]);
  result := fResultSet.IsNull(Col+1);
end;

function TSQLDBZEOSStatement.ColumnUTF8(Col: Integer): RawUTF8;
begin
  if (fResultSet=nil) or (cardinal(Col)>=cardinal(fColumnCount)) then
    raise ESQLDBZEOS.CreateFmt('%s.ColumnUTF8(%d)',[fStatementClassName,Col]);
  {$ifdef ZEOS72UP}
  result := fResultSet.GetUTF8String(Col+1);
  {$else}
    {$ifdef UNICODE}
    StringToUTF8(fResultSet.GetString(Col+1),result);
    {$else}
    result := fResultSet.GetString(Col+1); // thanks to controls_cp=CP_UTF8
    {$endif}
  {$endif}
end;



end.
