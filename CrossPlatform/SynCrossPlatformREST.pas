/// minimum stand-alone cross-platform REST process for mORMot client
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynCrossPlatformREST;

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


  Version 1.18
  - first public release, corresponding to mORMot Framework 1.18
  - would compile with Delphi for any platform, or with FPC or Kylix

}

{$i SynCrossPlatform.inc} // define e.g. HASINLINE

interface

uses
  SysUtils,
  Classes,
  Contnrs,
  Variants,
  SynCrossPlatformSpecific,
  SynCrossPlatformJSON;

const
  /// maximum number of fields in a database Table
  MAX_SQLFIELDS = 256;

  /// the first field in TSQLFieldBits is always ID/RowID
  ID_SQLFIELD = 0;

type
  /// alias to share the same string type between client and server
  RawUTF8 = string;

  /// alias to share the same blob type between client and server
  TSQLRawBlob = TByteDynArray;

  /// fast bit-encoded date and time value
  TTimeLog = type Int64;

  /// used to define a field which shall be set at each modification
  TModTime = type TTimeLog;

  /// used to define a field which shall be set at record creation
  TCreateTime = type TTimeLog;

  /// Exception type raised when working with REST access
  ERestException = class(Exception);

  /// used to store bit set for all available fields in a Table
  // - in this unit, field at index [0] indicates TSQLRecord.ID
  TSQLFieldBits = set of 0..MAX_SQLFIELDS-1;

  {$M+}
  TSQLRest = class;
  TSQLRecord = class;
  TSQLModel = class;

  TSQLRecordClass = class of TSQLRecord;
  TSQLRecordClassDynArray = array of TSQLRecordClass;

  /// handle a JSON result table, as returned by mORMot's REST server ORM
  // - this class is expected to work with TSQLRecord instances only
  // - it will let any "RowID" JSON key match TSQLRecord.ID property
  TSQLTableJSON = class(TJSONTableObject)
  protected
    fInternalState: cardinal;
    /// allow to let "RowID" JSON key match TSQLRecord.ID
    function GetPropInfo(aTypeInfo: TRTTITypeInfo; const PropName: string): TRTTIPropInfo; override;
  public
    /// to be called in a loop to iterate through all data rows
    // - if returned true, Object published properties will contain this row
    function FillOne(Value: TSQLRecord; SeekFirst: boolean=false): boolean;
  end;

  /// store information of one TSQLRecord published property
  TSQLModelInfoPropInfo = record
    /// the name of the published property
    Name: string;
    /// RTTI information about the published property
    RTTI: TRTTIPropInfo;
  end;
  TSQLModelInfoPropInfoDynArray = array of TSQLModelInfoPropInfo;
  
  /// store information of each TSQLRecord class
  TSQLModelInfo = object
  public
    /// the TSQLRecord class type itself
    Table: TSQLRecordClass;
    /// the short name of the class
    // - i.e. 'People' for TSQLRecordPeople
    Name: string;
    /// information about every published property
    // - first is always the ID field
    Prop: TSQLModelInfoPropInfoDynArray;
    /// specifies the "simple" fields, i.e. all non BLOB fields
    SimpleFields: TSQLFieldBits;
    /// specifies the BLOB fields
    BlobFields: TSQLFieldBits;
    /// specifies the TModTime fields
    ModTimeFields: TSQLFieldBits;
    /// specifies the TCreateTime fields
    CreateTimeFields: TSQLFieldBits;
    /// FieldNames='' to retrieve simple fields, '*' all fields, or as specified
    function FieldNamesToFieldBits(const FieldNames: string): TSQLFieldBits;
    /// return the corresponding field names
    function FieldBitsToFieldNames(const FieldBits: TSQLFieldBits): string;
    /// set TModTime and TCreateFields
    procedure ComputeFieldsBeforeWrite(aClient: TSQLRest;
      Value: TSQLRecord; AndCreate: Boolean);
    /// save the specified record as JSON
    function ToJSON(Value: TSQLRecord; const Fields: TSQLFieldBits): string;
  end;
  TSQLModelInfoDynArray = array of TSQLModelInfo;

  /// store the database model
  TSQLModel = class
  protected
    fRoot: string;
    fInfo: TSQLModelInfoDynArray;
  public
    /// initialize the Database Model
    // - set the Tables to be associated with this Model, as TSQLRecord classes
    // - set the optional Root URI path of this Model
    constructor Create(const Tables: array of TSQLRecordClass;
      const aRoot: string='root'); reintroduce;
    /// get index of aTable in Tables[], returns -1 if not found
    function GetTableIndex(aTable: TSQLRecordClass): integer; overload;
    /// get index of aTable in Tables[], returns -1 if not found
    function GetTableIndex(const aTableName: string): integer; overload;
    /// get index of aTable in Tables[], raise an ERestException if not found
    function GetTableIndexExisting(aTable: TSQLRecordClass): integer;
    /// the Root URI path of this Database Model
    property Root: string read fRoot;
    /// the information for each class
    property Info: TSQLModelInfoDynArray read fInfo;
  end;

  /// abstract ORM class to access remote tables
  // - in comparison to mORMot.pas TSQLRecord published fields, dynamic arrays
  // shall be defined as variant (since SynCrossPlatformJSON do not serialize)
  TSQLRecord = class
  protected
    fID: integer;
    fInternalState: cardinal;
    fFill: TSQLTableJSON;
  public
    /// this constructor initializes the record
    constructor Create; overload; virtual;
    /// this constructor loads a record from a REST instance from its ID
    constructor Create(aClient: TSQLRest; aID: integer;
      ForUpdate: boolean=false); overload;
    /// this constructor loads a record from a REST instance
    // - you can bind parameters by using ? in the SQLWhere clause
    // - FieldNames='' retrieve simple fields, '*' all fields, or as specified
    constructor Create(aClient: TSQLRest; const FieldNames, SQLWhere: string;
      const BoundsSQLWhere: array of const); overload;
    /// this constructor ask the server for a list of matching records
    // - you can bind parameters by using ? in the SQLWhere clause
    // - FieldNames='' retrieve simple fields, '*' all fields, or as specified
    // - then you can also loop through all rows with
    // ! while Rec.FillOne do
    // !   dosomethingwith(Rec);
    constructor CreateAndFillPrepare(aClient: TSQLRest; const FieldNames,
      SQLWhere: string; const BoundsSQLWhere: array of const);
    /// fill the specified record from the supplied JSON
    function FromJSON(const aJSON: string): boolean;
    /// fill all published properties of this object with the next available
    // row of data, as returned by CreateAndFillPrepare() constructor
    function FillOne: boolean;
    /// go to the first data row, as returned by CreateAndFillPrepare(),
    // then fill all published properties of this object
    // - you can use it e.g. as:
    // ! while Rec.FillOne do
    // !   dosomethingwith(Rec);
    // ! if Rec.FillRewind then
    // ! repeat
    // !   dosomeotherthingwith(Rec);
    // ! until not Rec.FillOne;
    function FillRewind: boolean;
    /// return the class type of this TSQLRecord
    function RecordClass: TSQLRecordClass;
      {$ifdef HASINLINE}inline;{$endif}
    /// contains the TSQLTableJSON instance after CreateAndFillPrepare()
    property FillTable: TSQLTableJSON read fFill;
    /// internal state counter of the mORMot server at last access time
    // - can be used to check if retrieved data may be out of date
    property InternalState: cardinal read fInternalState;
  published
    /// stores the record's primary key
    property ID: integer read fID write fID;
  end;

  /// abstract REST access class
  TSQLRest = class
  protected
    fModel: TSQLModel;
    fServerTimeStampOffset: TDateTime;
    function GetServerTimeStamp: TTimeLog;
    function ExecuteAdd(tableIndex: integer; const json: string): integer; virtual; abstract;
    function ExecuteUpdate(tableIndex,ID: integer; const json: string): boolean; virtual; abstract;
  public
    /// initialize the class, and associate it to a specified database Model
    constructor Create(aModel: TSQLModel); virtual;
    /// get a member from its ID
    // - return true on success, and fill all simple fields
    function Retrieve(aID: integer; Value: TSQLRecord;
      ForUpdate: boolean=false): boolean; overload; virtual; abstract;
    /// get a member from a where clause
    // - you can bind parameters by using ? in the SQLWhere clause
    // - FieldNames='' retrieve simple fields, '*' all fields, or as specified
    function Retrieve(const FieldNames, SQLWhere: string;
     const BoundsSQLWhere: array of const; Value: TSQLRecord): boolean; overload;
    /// execute directly a SQL statement, expecting a list of results
    // - return a result table on success, nil on failure
    // - you can bind parameters by using ? in the SQLWhere clause
    // - FieldNames='' retrieve simple fields, '*' all fields, or as specified
    function MultiFieldValues(Table: TSQLRecordClass; const FieldNames,
      SQLWhere: string; const BoundsSQLWhere: array of const;
      LimitFirstRow: Boolean=false): TSQLTableJSON; overload;
    /// execute directly a SQL statement, expecting a list of results
    // - return a result table on success, nil on failure
    // - FieldNames='' retrieve simple fields, '*' all fields, or as specified
    function MultiFieldValues(Table: TSQLRecordClass; const FieldNames,
      SQLWhere: string): TSQLTableJSON; overload;
    /// execute directly a SQL statement, returning a list of TSQLRecord 
    // - you can bind parameters by using ? in the SQLWhere clause
    // - FieldNames='' retrieve simple fields, '*' all fields, or as specified
    function RetrieveList(Table: TSQLRecordClass; const FieldNames,
      SQLWhere: string; const BoundsSQLWhere: array of const): TObjectList;
    /// execute directly a SQL statement, returning a list of data rows or nil
    function ExecuteList(const SQL: string): TSQLTableJSON; virtual; abstract;
    /// create a new member, returning the newly created ID, or 0 on error
    function Add(Value: TSQLRecord; SendData: boolean; ForceID: boolean=false): integer; virtual;
    /// delete a member
    function Delete(Table: TSQLRecordClass; ID: integer): boolean; virtual; abstract;
    /// update a member
    // - FieldNames='' update simple fields, '*' all fields, or as specified
    function Update(Value: TSQLRecord; const FieldNames: string=''): boolean; virtual;

    /// the associated data model
    property Model: TSQLModel read fModel;
    /// the current Date and Time, as retrieved from the server at connection
    property ServerTimeStamp: TTimeLog read GetServerTimeStamp;
  end;
  {$M-}

  /// REST client access class
  TSQLRestClientURI = class(TSQLRest)
  protected
    function getURI(aTable: TSQLRecordClass): string;
    function getURIID(aTableExistingIndex: integer; aID: integer): string;
    function getURICallBack(const aMethodName: string; aTable: TSQLRecordClass; aID: integer): string;
    function ExecuteAdd(tableIndex: integer; const json: string): integer; override;
    function ExecuteUpdate(tableIndex,ID: integer; const json: string): boolean; override;
    procedure InternalURI(var Call: TSQLRestURIParams); virtual; abstract;
  public
    /// connect to the REST server, and retrieve its time stamp
    function Connect: boolean;
    /// method calling the remote Server via a RESTful command
    // - calls the InternalURI abstract method
    // - this method will sign the url, if authentication is enabled
    procedure URI(var Call: TSQLRestURIParams); virtual;
    /// get a member from its ID using URI()
    function Retrieve(aID: integer; Value: TSQLRecord;
      ForUpdate: boolean=false): boolean; override;
    /// execute directly a SQL statement, returning a list of rows or nil
    function ExecuteList(const SQL: string): TSQLTableJSON; override;
    /// delete a member
    function Delete(Table: TSQLRecordClass; ID: integer): boolean; override;

    /// wrapper to the protected URI method to call a method on the server, using
    //  a ModelRoot/[TableName/[ID/]]MethodName RESTful GET request
    procedure CallBackGet(const aMethodName: string;
      const aNameValueParameters: array of const; var Call: TSQLRestURIParams;
      aTable: TSQLRecordClass=nil; aID: integer=0);
  end;

  /// REST client via HTTP
  // - note that this implementation is not thread-safe yet
  TSQLRestClientHTTP = class(TSQLRestClientURI)
  protected
    fConnection: TAbstractHttpConnection;
    fParameters: TSQLRestConnectionParams;
    fKeepAlive: Integer;
    procedure InternalURI(var Call: TSQLRestURIParams); override;
  public
    /// access to a mORMot server via HTTP
    constructor Create(const aServer: string; aPort: integer; aModel: TSQLModel;
      aHttps: boolean=false; const aProxyName: string='';
      const aProxyByPass: string=''; aSendTimeout: Cardinal=30000;
      aReceiveTimeout: Cardinal=30000); reintroduce; virtual;
    /// finalize the connection
    destructor Destroy; override;

    /// the associated connection, if active
    property Connection: TAbstractHttpConnection read fConnection;
    /// the connection parameters
    property Parameters: TSQLRestConnectionParams read fParameters;
    /// the keep-alive timout, in ms (20000 by default)
    property KeepAlive: Integer read fKeepAlive write fKeepAlive;
  end;

const
  /// MIME content type used for JSON communication
  JSON_CONTENT_TYPE = 'application/json; charset=UTF-8';
  
  /// HTML Status Code for "Continue"
  HTML_CONTINUE = 100;
  /// HTML Status Code for "Switching Protocols"
  HTML_SWITCHINGPROTOCOLS = 101;
  /// HTML Status Code for "Success"
  HTML_SUCCESS = 200;
  /// HTML Status Code for "Created"
  HTML_CREATED = 201;
  /// HTML Status Code for "Accepted"
  HTML_ACCEPTED = 202;
  /// HTML Status Code for "Non-Authoritative Information"
  HTML_NONAUTHORIZEDINFO = 203;
  /// HTML Status Code for "No Content"
  HTML_NOCONTENT = 204;
  /// HTML Status Code for "Multiple Choices"
  HTML_MULTIPLECHOICES = 300;
  /// HTML Status Code for "Moved Permanently"
  HTML_MOVEDPERMANENTLY = 301;
  /// HTML Status Code for "Found"
  HTML_FOUND = 302;
  /// HTML Status Code for "See Other"
  HTML_SEEOTHER = 303;
  /// HTML Status Code for "Not Modified"
  HTML_NOTMODIFIED = 304;
  /// HTML Status Code for "Use Proxy"
  HTML_USEPROXY = 305;
  /// HTML Status Code for "Temporary Redirect"
  HTML_TEMPORARYREDIRECT = 307;
  /// HTML Status Code for "Bad Request"
  HTML_BADREQUEST = 400;
  /// HTML Status Code for "Unauthorized"
  HTML_UNAUTHORIZED = 401;
  /// HTML Status Code for "Forbidden"
  HTML_FORBIDDEN = 403;
  /// HTML Status Code for "Not Found"
  HTML_NOTFOUND = 404;
  // HTML Status Code for "Method Not Allowed"
  HTML_NOTALLOWED = 405;
  // HTML Status Code for "Not Acceptable"
  HTML_NOTACCEPTABLE = 406;
  // HTML Status Code for "Proxy Authentication Required"
  HTML_PROXYAUTHREQUIRED = 407;
  /// HTML Status Code for "Request Time-out"
  HTML_TIMEOUT = 408;
  /// HTML Status Code for "Internal Server Error"
  HTML_SERVERERROR = 500;
  /// HTML Status Code for "Not Implemented"
  HTML_NOTIMPLEMENTED = 501;
  /// HTML Status Code for "Bad Gateway"
  HTML_BADGATEWAY = 502;
  /// HTML Status Code for "Service Unavailable"
  HTML_UNAVAILABLE = 503;
  /// HTML Status Code for "Gateway Timeout"
  HTML_GATEWAYTIMEOUT = 504;
  /// HTML Status Code for "HTTP Version Not Supported"
  HTML_HTTPVERSIONNONSUPPORTED = 505;


/// true if PropName is either 'ID' or 'RowID'
function IsRowID(const PropName: string): boolean;
  {$ifndef FPC}{$ifdef HASINLINE}inline;{$endif}{$endif}

/// can be used to create a statement with inlined parameters 
function FormatBind(const SQLWhere: string;
  const BoundsSQLWhere: array of const): string;

/// compute a TTimeLog value from Delphi date/time type
function DateTimeToTTimeLog(Value: TDateTime): TTimeLog;

/// convert a TTimeLog value into the Delphi date/time type
function TTimeLogToDateTime(Value: TTimeLog): TDateTime;

/// encode a text as defined by RFC 3986
function UrlEncode(const aValue: string): string; overload;

/// encode name=value pairs as defined by RFC 3986
function UrlEncode(const aNameValueParameters: array of const): string; overload;

/// decode a text as defined by RFC 3986
function UrlDecode(const aValue: string): string;

/// e.g. location := FindHeader(Call.OutHead,'Location:');
function FindHeader(const Headers, Name: string): string;


implementation

function IsRowID(const PropName: string): boolean;
begin
  result := IdemPropName(PropName,'ID') or
            IdemPropName(PropName,'RowID');
end;

function FormatBind(const SQLWhere: string;
  const BoundsSQLWhere: array of const): string;
var i,deb,arg: integer;
    tmpIsString: Boolean;
    tmp: string;
begin
  result := '';
  arg := 0;
  i := 1;
  deb := 1;
  while i<=length(SQLWhere) do
    if SQLWhere[i]='?' then begin
      result := result+copy(SQLWhere,deb,i-deb)+':(';
      if arg>high(BoundsSQLWhere) then
        tmp := 'null' else begin
        tmp := VarRecToValue(BoundsSQLWhere[arg],tmpIsString);
        if tmpIsString then
          DoubleQuoteStr(tmp);
        inc(arg);
      end;
        result := result+tmp+'):';
      inc(i);
      deb := i;
    end else
      inc(i);
  result := result+copy(SQLWhere,deb,i-deb);
end;

function DateTimeToTTimeLog(Value: TDateTime): TTimeLog;
var HH,MM,SS,MS,Y,M,D: word;
    V: Int64;
begin
  DecodeTime(Value,HH,MM,SS,MS);
  DecodeDate(Value,Y,M,D);
  V := HH+D shl 5+M shl 10+Y shl 14-(1 shl 5+1 shl 10);
  result := SS+MM shl 6+V shl 12;
end;

function TTimeLogToDateTime(Value: TTimeLog): TDateTime;
var Y: cardinal;
    Time: TDateTime;
begin
  Y := (Value shr (6+6+5+5+4)) and 4095;
  if (Y=0) or not TryEncodeDate(Y,1+(Value shr (6+6+5+5)) and 15,
       1+(Value shr (6+6+5)) and 31,result) then
    result := 0;
  if (Value and (1 shl (6+6+5)-1)<>0) and
     TryEncodeTime((Value shr (6+6)) and 31,
       (Value shr 6) and 63,Value and 63, 0, Time) then
    result := result+Time;
end;

const
  HexChars: array[0..15] of string = (
    '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');

function UrlEncode(const aValue: string): string; overload;
var utf8: UTF8String;
    i,c: integer;
begin
  utf8 := UTF8Encode(aValue);
  result := '';
  for i := 1 to length(utf8) do begin
    c := ord(utf8[i]);
    case c of
    ord('0')..ord('9'),ord('a')..ord('z'),ord('A')..ord('Z'),
    ord('_'),ord('-'),ord('.'),ord('~'):
              AppendChar(result,char(c));
    ord(' '): AppendChar(result,'+');
    else result := result+'%'+HexChars[c shr 4]+HexChars[c and $F];
    end; // see rfc3986 2.3. Unreserved Characters
  end;
end;

function UrlEncode(const aNameValueParameters: array of const): string; overload;
var a,i: integer;
    name,value: string;
    wasString: Boolean;
begin
  result := '';
  for a := 0 to high(aNameValueParameters) shr 1 do begin
    name := VarRecToValue(aNameValueParameters[a*2],wasString);
    for i := 1 to length(name) do
      if not (ord(name[i]) in [ord('a')..ord('z'),ord('A')..ord('Z')]) then
        raise ERestException.CreateFmt(
          'UrlEncode() expect alphabetic names, not "%s"',[name]);
    value := VarRecToValue(aNameValueParameters[a*2+1],wasString);
    result := result+'&'+name+'='+UrlEncode(value);
  end;
  if result<>'' then
    result[1] := '?';
end;

function HexDecode(a,b: char): integer;
var ai,bi: integer;
begin
  ai := Pos(upcase(a),'0123456789ABCDEF')-1;
  bi := Pos(upcase(b),'0123456789ABCDEF')-1;
  if (ai<0) or (bi<0) then
    result := ord('?') else
    result := ai shl 4+bi;
end;

function UrlDecode(const aValue: string): string;
var utf8: UTF8String;
    i,c,n,len: integer;
begin
  i := 1;
  len := length(aValue);
  n := 0;
  SetLength(utf8,len);
  while i<=length(aValue) do begin
    inc(n);
    c := ord(aValue[i]);
    case c of
    ord('+'):
      utf8[n] := ' ';
    ord('%'): begin
      if i+2<=len then
        utf8[n] := AnsiChar(HexDecode(aValue[i+1],aValue[i+2])) else
        utf8[n] := '?';
      inc(i,2);
    end;
    else if c>127 then
      utf8[n] := '?' else
      utf8[n] := AnsiChar(c);
    end;
    inc(i);
  end;
  SetLength(utf8,n);
  {$ifdef UNICODE}
  result := UTF8ToString(utf8);
  {$else}
  result := Utf8Decode(utf8);
  {$endif}
end;

function FindHeader(const Headers, Name: string): string;
var i: integer;
    line: string;
begin
  result := '';
  i := 1;
  while GetNextCSV(Headers,i,line,#10) do
    if StartWithPropName(line,Name) then begin
      result := trim(copy(line,length(Name)+1,MaxInt));
      exit;
    end;
end;


{ TSQLRecord }

constructor TSQLRecord.Create;
begin
  // do nothing by now: inherited classes may set some properties
end;

constructor TSQLRecord.Create(aClient: TSQLRest; aID: integer;
  ForUpdate: boolean=false);
begin
  Create;
  if aClient<>nil then
    aClient.Retrieve(aID,self,ForUpdate);
end;

constructor TSQLRecord.Create(aClient: TSQLRest;
  const FieldNames, SQLWhere: string; const BoundsSQLWhere: array of const);
begin
  Create;
  if aClient<>nil then
    aClient.Retrieve(SQLWhere,FieldNames,BoundsSQLWhere,self);
end;

constructor TSQLRecord.CreateAndFillPrepare(aClient: TSQLRest;
  const FieldNames, SQLWhere: string;
  const BoundsSQLWhere: array of const);
begin
  Create;
  fFill := aClient.MultiFieldValues(RecordClass,FieldNames,SQLWhere,BoundsSQLWhere);
end;

function TSQLRecord.FillOne: boolean;
begin
  if (self=nil) or (fFill=nil) then
    result := false else
    result := fFill.FillOne(self);
end;

function TSQLRecord.FillRewind: boolean;
begin
  if (self=nil) or (fFill=nil) then
    result := false else
    result := fFill.FillOne(self,true);
end;

function TSQLRecord.FromJSON(const aJSON: string): boolean;
var doc: TJSONVariantData;
    i: Integer;
begin
  if (self=nil) or (aJSON='') then
    result := false else
  if StartWithPropName(aJSON,'{"fieldCount":') then
    with TSQLTableJSON.Create(aJSON) do // non expanded format
    try
      result := StepObject(self);
    finally
      Free;
    end else begin // expanded format
    doc.Init(aJSON);
    for i := 0 to doc.Count-1 do
      if IsRowID(doc.Names[i]) then
        doc.Names[i] := 'ID';
    result := doc.ToObject(self);
  end;
end;

function TSQLRecord.RecordClass: TSQLRecordClass;
begin
  if self=nil then
    result := nil else
    result := TSQLRecordClass(ClassType);
end;

{ TSQLTableJSON }

function TSQLTableJSON.FillOne(Value: TSQLRecord;
  SeekFirst: boolean): boolean;
begin
  result := StepObject(Value,SeekFirst);
  if result then
    Value.fInternalState := fInternalState;
end;

function TSQLTableJSON.GetPropInfo(aTypeInfo: TRTTITypeInfo;
  const PropName: string): TRTTIPropInfo;
begin
  result := inherited GetPropInfo(aTypeInfo,PropName);
  if (result=nil) and IdemPropName(PropName,'RowID') then
    result := inherited GetPropInfo(aTypeInfo,'ID');
end;


{ TSQLModelInfo }

procedure TSQLModelInfo.ComputeFieldsBeforeWrite(aClient: TSQLRest;
  Value: TSQLRecord; AndCreate: Boolean);
var f: Integer;
    fields: TSQLFieldBits;
    TimeStamp: Int64;
begin
  if AndCreate then
    fields := ModTimeFields+CreateTimeFields else
    fields := ModTimeFields;
  if fields=[] then
    exit;
  TimeStamp := aClient.ServerTimeStamp;
  for f := 0 to length(Prop)-1 do
    if f in fields then
      SetInstanceProp(Value,Prop[f].RTTI,TimeStamp);
end;

function TSQLModelInfo.FieldBitsToFieldNames(
  const FieldBits: TSQLFieldBits): string;
var f: integer;
begin
  result := '';
  for f := 0 to length(Prop)-1 do
  if f in FieldBits then
    result := result+Prop[f].Name+',';
  if result<>'' then
    SetLength(result,length(result)-1);
end;

function TSQLModelInfo.FieldNamesToFieldBits(const FieldNames: string): TSQLFieldBits;
var i,f: integer;
    field: string;
begin
  if FieldNames='' then
    result := SimpleFields else
  if FieldNames='*' then
    FillChar(result,sizeof(result),255) else begin
    FillChar(result,sizeof(result),0);
    i := 1;
    while GetNextCSV(FieldNames,i,field) do begin
      if IsRowID(field) then
        Include(result,ID_SQLFIELD) else
        for f := 1 to length(Prop)-1 do
          if IdemPropName(field,Prop[f].Name) then begin
            Include(result,f);
            break;
          end;
    end;
  end;
end;

function TSQLModelInfo.ToJSON(Value: TSQLRecord;
  const Fields: TSQLFieldBits): string;
var i: integer;
begin
  result := '{';
  for i := 0 to length(Prop)-1 do
    if i in Fields then
      result := result+'"'+Prop[i].Name+'":'+
        ValueToJSON(GetInstanceProp(Value,Prop[i].RTTI))+',';
  if result='{' then
    result := 'null' else
    result[Length(Result)] := '}';
end;

{ TSQLModel }

function GetDisplayNameFromClass(C: TClass): string;
begin
  if C=nil then
    result := '' else begin
    result := C.ClassName;
    if IdemPropName(copy(result,1,4),'TSQL') then
      if IdemPropName(copy(result,5,6),'Record') then
        delete(result,1,10) else
        delete(result,1,4) else
      if result[1]<>'T' then
        delete(result,1,1);
  end;
end;

constructor TSQLModel.Create(const Tables: array of TSQLRecordClass;
  const aRoot: string);
var i,j: integer;
    List: TRTTIPropInfoDynArray;
    Names: TStringDynArray;
begin
  SetLength(fInfo,length(Tables));
  for i := 0 to high(fInfo) do
  with fInfo[i] do begin
    Table := Tables[i];
    Name := GetDisplayNameFromClass(Table);
    GetPropsInfo(Table.ClassInfo,Names,List);
    SetLength(Prop,length(List));
    for j := 0 to high(Prop) do begin
      if j=0 then
        Prop[j].Name := 'RowID' else
        Prop[j].Name := Names[j];
      Prop[j].RTTI := List[j];
      if IsBlob(List[j]) then
        include(BlobFields,j) else
        include(SimpleFields,j);
      if IsModTime(List[j]) then
        include(ModTimeFields,j) else
      if IsCreateTime(List[j]) then
        include(CreateTimeFields,j);
    end;
  end;
  if aRoot<>'' then
    if aRoot[length(aRoot)]='/' then
      fRoot := copy(aRoot,1,Length(aRoot)-1) else
      fRoot := aRoot;
end;

function TSQLModel.GetTableIndex(aTable: TSQLRecordClass): integer;
begin
  if self<>nil then
    for result := 0 to High(fInfo) do
      if fInfo[result].Table=aTable then
        exit;
  result := -1;
end;

function TSQLModel.GetTableIndex(const aTableName: string): integer;
begin
  if self<>nil then
    for result := 0 to High(fInfo) do
      if IdemPropName(fInfo[result].Name,aTableName) then
        exit;
  result := -1;
end;

function TSQLModel.GetTableIndexExisting(aTable: TSQLRecordClass): integer;
begin
  if self=nil then
    result := -1 else
    result := GetTableIndex(aTable);
  if result<0 then
    raise ERestException.CreateFmt('%s should be part of the Model',
      [aTable.ClassName]);
end;

{ TSQLRest }

function TSQLRest.Add(Value: TSQLRecord; SendData, ForceID: boolean): integer;
var tableIndex: Integer;
    json: string;
    fields: TSQLFieldBits;
begin
  tableIndex := Model.GetTableIndexExisting(Value.RecordClass);
  if SendData then
  with Model.Info[tableIndex] do begin
    ComputeFieldsBeforeWrite(self,Value,true);
    fields := SimpleFields;
    if not ForceID then
      exclude(fields,ID_SQLFIELD);
    json := ToJSON(Value,fields);  
  end;
  result := ExecuteAdd(tableIndex,json);
end;

constructor TSQLRest.Create(aModel: TSQLModel);
begin
  inherited Create;
  fModel := Model;
end;

function TSQLRest.MultiFieldValues(Table: TSQLRecordClass;
  const FieldNames, SQLWhere: string; const BoundsSQLWhere: array of const;
  LimitFirstRow: Boolean): TSQLTableJSON;
var where: string;
begin
  where := FormatBind(SQLWhere,BoundsSQLWhere);
  if LimitFirstRow then
    where := where+' limit 1';
  result := MultiFieldValues(Table,FieldNames,where);
end;

function TSQLRest.GetServerTimeStamp: TTimeLog;
begin
  result := DateTimeToTTimeLog(Now+fServerTimeStampOffset);
end;

function TSQLRest.MultiFieldValues(Table: TSQLRecordClass;
  const FieldNames, SQLWhere: string): TSQLTableJSON;
var tableIndex: Integer;
    fields,sql: string;
begin
  tableIndex := Model.GetTableIndexExisting(Table);
  with Model.Info[tableIndex] do begin
    fields := FieldBitsToFieldNames(FieldNamesToFieldBits(FieldNames));
    sql := 'select '+fields+' from '+Name;
  end;
  if SQLWhere<>'' then
    sql := sql+' where '+SQLWhere;
  result := ExecuteList(sql);
end;

function TSQLRest.Retrieve(const FieldNames,SQLWhere: string;
  const BoundsSQLWhere: array of const; Value: TSQLRecord): boolean;
var table: TSQLTableJSON;
begin
  table := MultiFieldValues(Value.RecordClass,FieldNames,
    SQLWhere,BoundsSQLWhere,true);
  if table=nil then
    result := false else
    try
      result := table.FillOne(Value);
    finally
      table.Free;
    end;
end;

function TSQLRest.RetrieveList(Table: TSQLRecordClass; const FieldNames,
  SQLWhere: string; const BoundsSQLWhere: array of const): TObjectList;
var rows: TSQLTableJSON;
    rec: TSQLRecord;
begin
  result := TObjectList.Create;
  rows := MultiFieldValues(Table,FieldNames,SQLWhere,BoundsSQLWhere);
  if rows<>nil then
    try
      repeat
        rec := Table.Create;
        if not rows.FillOne(rec) then begin
          rec.Free;
          break;
        end;
        result.Add(rec);
      until false;
    finally
      rows.Free;
    end;
end;

function TSQLRest.Update(Value: TSQLRecord; const FieldNames: string): boolean;
var tableIndex: Integer;
    fields: TSQLFieldBits;
    json: string;
begin
  if (Value=nil) or (Value.ID<=0) then begin
    result := false;
    exit;
  end;
  tableIndex := Model.GetTableIndexExisting(Value.RecordClass);
  with Model.Info[tableIndex] do begin
    fields := FieldNamesToFieldBits(FieldNames)+ModTimeFields;
    exclude(fields,ID_SQLFIELD);
    ComputeFieldsBeforeWrite(self,Value,false);
    json := ToJSON(Value,fields);
  end;
  result := ExecuteUpdate(tableIndex,Value.ID,json);
end;

{ TSQLRestClientURI }

function TSQLRestClientURI.getURI(aTable: TSQLRecordClass): string;
begin
  result := Model.Root;
  if aTable<>nil then
    result := result+'/'+Model.Info[Model.GetTableIndexExisting(aTable)].Name;
end;

function TSQLRestClientURI.getURICallBack(const aMethodName: string;
  aTable: TSQLRecordClass; aID: integer): string;
begin
  result := getURI(aTable);
  if aID>0 then
    result := result+'/'+IntToStr(aID);
  result := result+'/'+aMethodName;
end;

function TSQLRestClientURI.getURIID(aTableExistingIndex: integer; aID: integer): string;
begin
  result := Model.Root+'/'+Model.Info[aTableExistingIndex].Name;
  if aID>0 then
    result := result+'/'+IntToStr(aID);
end;

function TSQLRestClientURI.ExecuteList(const SQL: string): TSQLTableJSON;
var Call: TSQLRestURIParams;
    json: string;
begin
  result := nil;
  if self=nil then
    exit;
  Call.Url := Model.Root;
  Call.Method := 'GET';
  TextToHttpBody(SQL,Call.InBody);
  URI(Call);
  if Call.OutStatus=HTML_SUCCESS then begin
    HttpBodyToText(Call.OutBody,json);
    result := TSQLTableJSON.Create(json);
    result.fInternalState := Call.OutInternalState;
  end;
end;

function TSQLRestClientURI.Retrieve(aID: integer; Value: TSQLRecord;
  ForUpdate: boolean): boolean;
var tableIndex: Integer;
    Call: TSQLRestURIParams;
    json: string;
begin
  tableIndex := Model.GetTableIndexExisting(Value.RecordClass);
  Call.Url := getURIID(tableIndex,aID);
  if ForUpdate then
     Call.Method := 'LOCK' else
     Call.Method := 'GET';
  URI(Call);
  result := Call.OutStatus=HTML_SUCCESS;
  if result then begin
    Value.fInternalState := Call.OutInternalState;
    HttpBodyToText(Call.OutBody,json);
    Value.FromJSON(json);
  end;
end;

procedure TSQLRestClientURI.URI(var Call: TSQLRestURIParams);
begin
  Call.OutStatus := HTML_UNAVAILABLE;
  Call.OutInternalState := 0;
  if self=nil then
    exit;
  // todo: add signature to url according to authentication scheme
  InternalURI(Call);
  Call.OutInternalState := StrToInt64Def(
    FindHeader(Call.OutHead,'Server-InternalState:'),0);
end;

procedure TSQLRestClientURI.CallBackGet(const aMethodName: string;
  const aNameValueParameters: array of const; var Call: TSQLRestURIParams;
  aTable: TSQLRecordClass; aID: integer);
begin
  Call.Url := getURICallBack(aMethodName,aTable,aID)+
    UrlEncode(aNameValueParameters);
  Call.Method := 'GET';
  URI(Call);
end;

function TSQLRestClientURI.Connect: boolean;
var Call: TSQLRestURIParams;
    TimeStamp: Int64;
    tmp: string;
begin
  CallBackGet('TimeStamp',[],Call);
  result := Call.OutStatus=HTML_SUCCESS;
  if not result then
    exit;
  HttpBodyToText(Call.OutBody,tmp);
  if not TryStrToInt64(tmp,TimeStamp) then
    result := false else
    fServerTimeStampOffset := TTimeLogToDateTime(TimeStamp)-Now;
end;

function TSQLRestClientURI.ExecuteAdd(tableIndex: integer;
  const json: string): integer;
var Call: TSQLRestURIParams;
    location: string;
    i: integer;
begin
  result := 0;
  Call.Url := getURIID(tableIndex,0);
  Call.Method := 'POST';
  URI(Call);
  if Call.OutStatus<>HTML_CREATED then
    exit;
  location := FindHeader(Call.OutHead,'Location:');
  for i := length(location) downto 1 do
    if not (ord(location[i]) in [ord('0')..ord('9')]) then begin
      result := StrToIntDef(Copy(location,i+1,100),0);
      break; // // 'Location: root/People/11012' e.g.
    end;
end;

function TSQLRestClientURI.Delete(Table: TSQLRecordClass;
  ID: integer): boolean;
var Call: TSQLRestURIParams;
begin
  result := false;
  if ID<=0 then
    exit;
  Call.Url := getURIID(Model.GetTableIndexExisting(Table),ID);
  Call.Method := 'DELETE';
  URI(Call);
  if Call.OutStatus=HTML_SUCCESS then
    result := true;
end;

function TSQLRestClientURI.ExecuteUpdate(tableIndex,ID: integer;
  const json: string): boolean; 
var Call: TSQLRestURIParams;
begin
  Call.Url := getURIID(tableIndex,ID);
  Call.Method := 'PUT';
  TextToHttpBody(json,Call.InBody);
  URI(Call);
  result := Call.OutStatus=HTML_SUCCESS;
end;


{ TSQLRestClientHTTP }

constructor TSQLRestClientHTTP.Create(const aServer: string;
  aPort: integer; aModel: TSQLModel; aHttps: boolean; const aProxyName,
  aProxyByPass: string; aSendTimeout, aReceiveTimeout: Cardinal);
begin
  inherited Create(aModel);
  fParameters.Server := aServer;
  fParameters.Port := aPort;
  fParameters.Https := aHttps;
  fParameters.ProxyName := aProxyName;
  fParameters.ProxyByPass := aProxyByPass;
  fParameters.SendTimeout := aSendTimeout;
  fParameters.ReceiveTimeout := aReceiveTimeout;
  fKeepAlive := 20000;
end;

destructor TSQLRestClientHTTP.Destroy;
begin
  inherited;
  FreeAndNil(fConnection);
end;

procedure TSQLRestClientHTTP.InternalURI(var Call: TSQLRestURIParams);
var inType: string;
    retry: integer;
begin
  inType := FindHeader(Call.InHead,'content-type:');
  if inType='' then
    inType := JSON_CONTENT_TYPE;
  for retry := 0 to 1 do begin
    if fConnection=nil then
      try
        fConnection := HttpConnectionClass.Create(fParameters);
        // TODO: handle SynLZ compression and SHA/AES encryption
      except
        on Exception do
          FreeAndNil(fConnection);
      end;
    if fConnection=nil then begin
      Call.OutStatus := HTML_NOTIMPLEMENTED;
      exit;
    end;
    try
      fConnection.URI(Call,inType,fKeepAlive);
      break; // do not rety on transmission success
    except
      on Exception do
        FreeAndNil(fConnection); // will retry once (e.g. if connection broken)
    end;
  end;
end;

end.

