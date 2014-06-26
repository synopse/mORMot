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

{$ifdef DWSCRIPT} // always defined since SMS 1.1.2
  {$define ISDWS}           // e.g. for SmartMobileStudio or Delphi Web Script
  {$define ISSMS}           // for SmartMobileStudio
{$else}
  {$i SynCrossPlatform.inc} // define e.g. HASINLINE
{$endif}

interface

{$ifdef ISDWS}
uses
  W3System,
{$else}
uses
  SysUtils,
  Classes,
{$ifdef NEXTGEN}
  System.Generics.Collections,
{$else}
  Contnrs,
{$endif NEXTGEN}
  Variants,
  SynCrossPlatformJSON,
{$endif ISDWS}
  SynCrossPlatformSpecific,
  SynCrossPlatformCrypto;


const
  /// maximum number of fields in a database Table
  MAX_SQLFIELDS = 256;

  /// used as "stored AS_UNIQUE" published property definition in TSQLRecord
  AS_UNIQUE = false;

type
  /// alias to share the same string type between client and server
  RawUTF8 = string;


  TSQLRest = class;
  TSQLRecord = class;
  TSQLModel = class;

  TSQLRecordClass = class of TSQLRecord;
  TSQLRecordClassDynArray = array of TSQLRecordClass;

  {$ifdef ISDWS}

  // circumvent weird DWS / SMS syntax
  cardinal = integer;
  Int64 = integer;
  TPersistent = TObject;
  TObjectList = array of TObject;
  TSQLRawBlob = string;
  TTimeLog = Int64;
  TModTime = TTimeLog;
  TCreateTime = TTimeLog;
  TSQLFieldBit = enum (Low = 0, High = MAX_SQLFIELDS-1);

  ERestException = class(EW3Exception);

  /// handle a JSON result table, as returned by mORMot's REST server ORM
  // - we define a dedicated class to by-pass SynCrossPlatformJSON unit
  TSQLTableJSON = class
  protected
    fInternalState: cardinal;
    fFieldCount, fRowCount, fCurrentRow: integer;
    fFieldNames: TStrArray;
    fValues: TVariantDynArray;
  public
    /// parse the supplied JSON content
    constructor Create(const aJSON: string);
    /// to be called in a loop to iterate through all data rows
    // - if returned true, Object published properties will contain this row
    function FillOne(Value: TSQLRecord; SeekFirst: boolean=false): boolean;
  end;

  {$else}

  /// Exception type raised when working with REST access
  ERestException = class(Exception);

  /// alias to share the same blob type between client and server
  TSQLRawBlob = TByteDynArray;

  /// fast bit-encoded date and time value
  TTimeLog = type Int64;

  /// used to define a field which shall be set at each modification
  TModTime = type TTimeLog;

  /// used to define a field which shall be set at record creation
  TCreateTime = type TTimeLog;

  /// used to identify the a field in a Table as in TSQLFieldBits
  TSQLFieldBit = 0..MAX_SQLFIELDS-1;

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

  {$endif ISDWS}

  /// used to store bit set for all available fields in a Table
  // - in this unit, field at index [0] indicates TSQLRecord.ID
  TSQLFieldBits = set of TSQLFieldBit;

  /// a published property kind
  // - does not match mORMot.pas TSQLFieldType: here we recognize only types
  // which may expect a special behavior in this unit
  TSQLFieldKind = (
    sftUnspecified, sftDateTime, sftTimeLog, sftBlob, sftModTime, sftCreateTime);

  /// a set of published property Kind
  TSQLFieldKinds = set of TSQLFieldKind;

  /// store information of one TSQLRecord published property
  TSQLModelInfoPropInfo = class
  public
    /// the name of the published property
    Name: string;
    /// the property field type
    Kind: TSQLFieldKind;
    {$ifdef ISDWS}
    /// index of the published property in the associated Prop[]
    FieldIndex: TSQLFieldBit;
    {$else}
    /// the property type name, as retrieved from RTTI
    TypeName: string;
    /// RTTI information about the published property
    RTTI: TRTTIPropInfo;
    /// initialize the instance
    constructor CreateFrom(aRTTI: TRTTIPropInfo);
    {$endif}
  end;

  /// store information of all TSQLRecord published properties
  TSQLModelInfoPropInfoDynArray = array of TSQLModelInfoPropInfo;
  
  /// store information of one TSQLRecord class
  TSQLModelInfo = class
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
    /// specifies all fields, including simple and BLOB fields
    AllFields: TSQLFieldBits;
    /// specifies the TModTime fields
    ModTimeFields: TSQLFieldBits;
    /// specifies the TCreateTime fields
    CreateTimeFields: TSQLFieldBits;
    /// specifies the TModTime and TCreateTime fields
    ModAndCreateTimeFields: TSQLFieldBits;
    /// contains all published properties kind
    HasKind: TSQLFieldKinds;
    /// TRUE if has TModTime or TCreateTime fields
    HasTimeFields: boolean;
    {$ifdef ISSMS}
    /// allow fast by-name access to Prop[]
    PropCache: variant;
    {$else}
    /// finalize the memory used
    destructor Destroy; override;
    {$endif}
    /// initialize the class member for the supplied TSQLRecord
    constructor CreateFromRTTI(aTable: TSQLRecordClass);
    /// FieldNames='' to retrieve simple fields, '*' all fields, or as specified
    function FieldNamesToFieldBits(const FieldNames: string;
      IncludeModTimeFields: boolean): TSQLFieldBits;
    /// return the corresponding field names
    function FieldBitsToFieldNames(const FieldBits: TSQLFieldBits): string;
    /// set TModTime and TCreateFields
    procedure ComputeFieldsBeforeWrite(aClient: TSQLRest;
      Value: TSQLRecord; AndCreate: Boolean);
    /// compute the 'SELECT ... FROM ...' corresponding to the supplied fields
    function SQLSelect(const FieldNames: string): string;
    /// save the specified record as JSON for record adding
    function ToJSONAdd(Client: TSQLRest; Value: TSQLRecord; ForceID: boolean): string;
    /// save the specified record as JSON for record update
    function ToJSONUpdate(Client: TSQLRest; Value: TSQLRecord;
      const FieldNames: string): string;
    /// save the specified record as JSON
    function ToJSON(Value: TSQLRecord; const Fields: TSQLFieldBits): string; overload;
  end;

  /// store information of several TSQLRecord class
  TSQLModelInfoDynArray = array of TSQLModelInfo;

  /// store the database model
  TSQLModel = class
  protected
    fRoot: string;
    fInfo: TSQLModelInfoDynArray;
  public
    /// initialize the Database Model
    // - set the Tables to be associated with this Model, as TSQLRecord classes
    // - set the optional Root URI path of this Model - default is 'root'
    constructor Create(const Tables: array of TSQLRecordClass;
      const aRoot: string {$ifndef ISDWS}='root'{$endif});
    /// register a new Table class to this Model
    procedure Add(Table: TSQLRecordClass);
    {$ifndef ISSMS}
    /// finalize the memory used
    destructor Destroy; override;
    {$endif}
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

  {$ifdef ISSMS}
  /// low-level structure used for server-side generated pseudo RTTI
  TRTTIPropInfos = class
  public
    Props: TSQLModelInfoPropInfoDynArray;
    PropCache: variant;
    /// define the published properties
    // - optional PropKinds[] can override default sftUnspecified type
    constructor Create(const PropNames: array of string;
      const PropKinds: array of TSQLFieldKind);
  end;
  {$endif}

  /// abstract ORM class to access remote tables
  // - in comparison to mORMot.pas TSQLRecord published fields, dynamic arrays
  // shall be defined as variant (since SynCrossPlatformJSON do not serialize)
  // - inherit from TPersistent to have RTTI for its published properties
  // (SmartMobileStudio does not allow {$M+} in the source)
  TSQLRecord = class(TPersistent)
  protected
    fID: integer;
    fInternalState: cardinal;
    fFill: TSQLTableJSON;
    {$ifdef ISSMS}
    class function GetRTTI: TRTTIPropInfos;
    /// you should override this method
    class function ComputeRTTI: TRTTIPropInfos; virtual;
    {$endif}
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
    /// finalize the record memory
    destructor Destroy; override;
    /// fill the specified record from the supplied JSON
    function FromJSON(const aJSON: string): boolean;
    {$ifdef ISSMS}
    /// fill the specified record from Names/Values pairs
    function FromNamesValues(const Names: TStrArray; const Values: TVariantDynArray;
      ValuesStartIndex: integer): boolean;
    {$endif}
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

  /// table containing the available user access rights for authentication
  // - is added here since should be part of the model
  // - no wrapper is available to handle AccessRights, since for security
  // reasons it is not available remotely from client side
  TSQLAuthGroup = class(TSQLRecord)
  protected
    fIdent: string;
    fSessionTimeOut: integer;
    fAccessRights: string;
    {$ifdef ISSMS}
    class function ComputeRTTI: TRTTIPropInfos; override;
    {$endif}
  published
    /// the access right identifier, ready to be displayed
    // - the same identifier can be used only once (this column is marked as
    // unique via a "stored AS_UNIQUE" (i.e. "stored false") attribute)
    property Ident: string read fIdent write fIdent
      {$ifndef ISDWS}stored AS_UNIQUE{$endif};
    /// the number of minutes a session is kept alive
    property SessionTimeout: integer read fSessionTimeOut write fSessionTimeOut;
    /// a textual representation of a TSQLAccessRights buffer
    property AccessRights: string read fAccessRights write fAccessRights;
  end;

  /// class of the table containing the available user access rights for authentication
  TSQLAuthGroupClass = class of TSQLAuthGroup;

  /// table containing the Users registered for authentication
  TSQLAuthUser = class(TSQLRecord)
  protected
    fLogonName: string;
    fPasswordHashHexa: string;
    fDisplayName: string;
    fGroup: TSQLAuthGroup;
    fData: TSQLRawBlob;
    {$ifdef ISSMS}
    class function ComputeRTTI: TRTTIPropInfos; override;
    {$endif}
    procedure SetPasswordPlain(const Value: string);
  public
    /// able to set the PasswordHashHexa field from a plain password content
    // - in fact, PasswordHashHexa := SHA256('salt'+PasswordPlain) in UTF-8
    property PasswordPlain: string write SetPasswordPlain;
  published
    /// the User identification Name, as entered at log-in
    // - the same identifier can be used only once (this column is marked as
    // unique via a "stored AS_UNIQUE" - i.e. "stored false" - attribute), and
    // therefore indexed in the database (e.g. hashed in TSQLRestStorageInMemory)
    property LogonName: string read fLogonName write fLogonName
      {$ifndef ISDWS}stored AS_UNIQUE{$endif};
   /// the User Name, as may be displayed or printed
    property DisplayName: string read fDisplayName write fDisplayName;
    /// the hexa encoded associated SHA-256 hash of the password
    property PasswordHashHexa: string read fPasswordHashHexa write fPasswordHashHexa;
    /// the associated access rights of this user
    // - access rights are managed by group
    // - in TAuthSession.User instance, GroupRights property will contain a
    // REAL TSQLAuthGroup instance for fast retrieval in TSQLRestServer.URI
    // - note that 'Group' field name is not allowed by SQLite
    property GroupRights: TSQLAuthGroup read fGroup write fGroup;
    /// some custom data, associated to the User
    // - Server application may store here custom data
    // - its content is not used by the framework but 'may' be used by your
    // application
    property Data: TSQLRawBlob read fData write fData;
  end;

  TSQLRestAuthentication = class;

  /// class used for client authentication
  TSQLRestAuthenticationClass = class of TSQLRestAuthentication;

  /// the possible Server-side instance implementation patterns for
  // interface-based services 
  // - each interface-based service will be implemented by a corresponding
  // class instance on the server: this parameter is used to define how
  // class instances are created and managed
  // - on the Client-side, each instance will be handled depending on the
  // server side implementation (i.e. with sicClientDriven behavior if necessary)
  TServiceInstanceImplementation = (
    sicSingle, sicShared, sicClientDriven, sicPerSession, sicPerUser, sicPerGroup,
    sicPerThread);

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
    // - this overloaded method will update only simple fields of the TSQLRecord
    // - just a wrapper around Update(Value,'') since default string parameters
    // are not yet allowed by DWS compiler
    function Update(Value: TSQLRecord): boolean; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// update a member
    // - you can set FieldNames='' to update simple fields, '*' to update all
    // fields, or specify a CSV list of updated fields
    function Update(Value: TSQLRecord; const FieldNames: string): boolean; overload; virtual;

    /// the associated data model
    property Model: TSQLModel read fModel;
    /// the current Date and Time, as retrieved from the server at connection
    property ServerTimeStamp: TTimeLog read GetServerTimeStamp;
  end;

  /// REST client access class
  TSQLRestClientURI = class(TSQLRest)
  protected
    fAuthentication: TSQLRestAuthentication;
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
    // - we expect reUrlEncodedSQL to be defined in AllowRemoteExecute on
    // server side, since we will encode the SQL at URL level, so that all
    // HTTP client libraires will accept this layout (e.g. Indy or AJAX)
    function ExecuteList(const SQL: string): TSQLTableJSON; override;
    /// delete a member
    function Delete(Table: TSQLRecordClass; ID: integer): boolean; override;

    /// wrapper to the protected URI method to call a method on the server, using
    //  a ModelRoot/[TableName/[ID/]]MethodName RESTful GET request
    procedure CallBackGet(const aMethodName: string;
      const aNameValueParameters: array of const; var Call: TSQLRestURIParams;
      aTable: TSQLRecordClass=nil; aID: integer=0);
    /// decode "result":... content as returned by CallBackGet()
    function CallBackGetResult(const aMethodName: string;
      const aNameValueParameters: array of const;
      aTable: TSQLRecordClass=nil; aID: integer=0): string;
    /// authenticate an User to the current connected Server
    // - using TSQLRestAuthenticationDefault or TSQLRestServerAuthenticationNone
    procedure SetUser(aAuthenticationClass: TSQLRestAuthenticationClass;
      const aUserName, aPassword: string; aHashedPassword: Boolean=False);
    /// close the session initiated with SetUser()
    procedure SessionClose;
  end;

  /// used for client authentication
  TSQLRestAuthentication = class
  protected
    fUser: TSQLAuthUser;
    fSessionID: cardinal;
    fSessionIDHexa8: string;
    procedure SetSessionID(Value: Cardinal);
    // override this method to return the session key
    function ClientComputeSessionKey(Sender: TSQLRestClientURI): string;
      virtual; abstract;
    function ClientSessionComputeSignature(Sender: TSQLRestClientURI;
      const url: string): string; virtual; abstract;
  public
    /// initialize client authentication instance, i.e. the User associated instance
    constructor Create(const aUserName, aPassword: string;
      aHashedPassword: Boolean=false);
    /// finalize the instance
    destructor Destroy; override;
    /// read-only access to the logged user information
    // - only LogonName and PasswordHashHexa are set here
    property User: TSQLAuthUser read fUser;
    /// contains the session ID used for the authentication
    property SessionID: cardinal read fSessionID;
  end;

  /// mORMot secure RESTful authentication scheme
  TSQLRestAuthenticationDefault = class(TSQLRestAuthentication)
  protected
    fSessionPrivateKey: hash32;
    function ClientComputeSessionKey(Sender: TSQLRestClientURI): string; override;
    function ClientSessionComputeSignature(Sender: TSQLRestClientURI;
      const url: string): string; override;
  end;

  /// mORMot weak RESTful authentication scheme
  TSQLRestAuthenticationNone = class(TSQLRestAuthentication)
  protected
    function ClientComputeSessionKey(Sender: TSQLRestClientURI): string; override;
    function ClientSessionComputeSignature(Sender: TSQLRestClientURI;
      const url: string): string; override;
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
      aHttps: boolean=false
    {$ifndef ISSMS}; const aProxyName: string='';
      const aProxyByPass: string=''; aSendTimeout: Cardinal=30000;
      aReceiveTimeout: Cardinal=30000{$endif}); reintroduce; virtual;
    /// finalize the connection
    destructor Destroy; override;

    /// the associated connection, if active
    property Connection: TAbstractHttpConnection read fConnection;
    /// the connection parameters
    property Parameters: TSQLRestConnectionParams read fParameters;
    /// the keep-alive timout, in ms (20000 by default)
    property KeepAlive: Integer read fKeepAlive write fKeepAlive;
  end;


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

const
  /// the first field in TSQLFieldBits is always ID/RowID
  ID_SQLFIELD: TSQLFieldBit = 0;
  
var
  /// contains no field bit set
  NO_SQLFIELDBITS: TSQLFieldBits;

implementation

function IsRowID(const PropName: string): boolean;
begin
  result := IdemPropName(PropName,'ID') or
            IdemPropName(PropName,'RowID');
end;

function FormatBind(const SQLWhere: string;
  const BoundsSQLWhere: array of const): string;
var tmpIsString: Boolean;
    tmp: string;
    i,deb,arg: integer;
{$ifdef ISSMS}
    args: variant;
begin
  asm
    @args=@BoundsSQLWhere;
  end;
{$else}
begin
{$endif}
  result := '';
  arg := 0;
  i := 1;
  deb := 1;
  while i<=length(SQLWhere) do
    if SQLWhere[i]='?' then begin
      result := result+copy(SQLWhere,deb,i-deb)+':(';
      if arg>high(BoundsSQLWhere) then
        tmp := 'null' else begin
        tmp := VarRecToValue(
          {$ifdef ISSMS}args{$else}BoundsSQLWhere{$endif}[arg],tmpIsString);
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
    {$ifndef ISSMS}
    V: Int64;
    {$endif}
begin
  DecodeTime(Value,HH,MM,SS,MS);
  DecodeDate(Value,Y,M,D);
  {$ifdef ISSMS} // JavaScript truncates to 32 bit binary
  result := SS+MM*$40+(HH+D*$20+M*$400+Y*$4000-$420)*$1000;
  {$else}
  V := HH+D shl 5+M shl 10+Y shl 14-(1 shl 5+1 shl 10);
  result := SS+MM shl 6+V shl 12;
  {$endif}
end;

function TTimeLogToDateTime(Value: TTimeLog): TDateTime;
var Y: cardinal;
    Time: TDateTime;
begin
  {$ifdef ISSMS} // JavaScript truncates to 32 bit binary
  Y := (Value div $4000000) and 4095;
  {$else}
  Y := (Value shr (6+6+5+5+4)) and 4095;
  {$endif}
  if (Y=0) or not TryEncodeDate(Y,1+(Value shr (6+6+5+5)) and 15,
       1+(Value shr (6+6+5)) and 31,result) then
    result := 0;
  if (Value and (1 shl (6+6+5)-1)<>0) and
     TryEncodeTime((Value shr (6+6)) and 31,
       (Value shr 6) and 63,Value and 63, 0, Time) then
    result := result+Time;
end;

function UrlEncode(const aValue: string): string; overload;
{$ifdef ISSMS}
begin // see http://www.w3schools.com/jsref/jsref_encodeuricomponent.asp
  result := encodeURIComponent(aValue);
end;
{$else}
const
  HexChars: array[0..15] of string = (
    '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
var i,c: integer;
    utf8: TUTF8Buffer;
begin
  result := '';
{$ifdef NEXTGEN}
  utf8 := TEncoding.UTF8.GetBytes(aValue);
  for i := 0 to high(utf8) do begin
{$else}
  utf8 := UTF8Encode(aValue);
  for i := 1 to length(utf8) do begin
{$endif}
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
{$endif}

function UrlEncode(const aNameValueParameters: array of const): string; overload;
var n,a: integer;
    name,value: string;
    {$ifndef ISSMS}
    wasString: Boolean;
    i: integer;
    {$endif}
begin
  result := '';
  n := high(aNameValueParameters);
  if n>0 then begin
{$ifdef ISSMS} // open parameters are not a true array in JavaScript
    var temp: variant;
    asm
      @temp=@aNameValueParameters;
    end;
    for a := 0 to n div 2 do begin
      name := temp[a*2];
      value := temp[a*2+1];
{$else}
    for a := 0 to n div 2 do begin
      name := VarRecToValue(aNameValueParameters[a*2],wasString);
      for i := 1 to length(name) do
        if not (ord(name[i]) in [ord('a')..ord('z'),ord('A')..ord('Z')]) then
          raise ERestException.CreateFmt(
            'UrlEncode() expect alphabetic names, not "%s"',[name]);
      value := VarRecToValue(aNameValueParameters[a*2+1],wasString);
{$endif}
      result := result+'&'+name+'='+UrlEncode(value);
    end;
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
{$ifdef ISSMS}
begin
  result := decodeURIComponent(aValue);
end;
{$else}
var i,c,n,len: integer;
    utf8: TUTF8Buffer;
begin
  i := 1;
  len := length(aValue);
  n := 0;
  SetLength(utf8,len);
  while i<=length(aValue) do begin
{$ifndef NEXTGEN}
    inc(n);
{$endif}
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
{$ifdef NEXTGEN}
    inc(n);
{$endif}
  end;
  SetLength(utf8,n);
{$ifdef NEXTGEN}
  result := TEncoding.UTF8.GetString(utf8);
{$else}
  {$ifdef UNICODE}
  result := UTF8ToString(utf8);
  {$else}
  result := Utf8Decode(utf8);
  {$endif}
{$endif}
end;
{$endif}

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

{$ifdef ISSMS}

constructor TRTTIPropInfos.Create(const PropNames: array of string;
  const PropKinds: array of TSQLFieldKind);
var name: string;
    cache: variant;
    p: integer;
    prop = new TSQLModelInfoPropInfo;
begin
  prop.Name := 'RowID'; // first Field is RowID
  Props.Add(prop);
  for name in PropNames do begin
    prop.Name := name;
    Props.Add(prop);
  end;
  cache := TVariant.CreateObject;
  for p := 0 to high(Props) do begin
    prop := Props[p];
    prop.FieldIndex := p;
    if p<length(PropKinds) then
      prop.Kind := PropKinds[p-1] else
      prop.Kind := sftUnspecified;
    var upperName := uppercase(prop.Name);
    asm
      @cache[@upperName]=prop;
    end;
  end;
  PropCache := cache;
end;

function Find(PropCache: variant; Name: string; var Info: TSQLModelInfoPropInfo): boolean;
begin
  Name := UpperCase(Name);
  if Name='ID' then
    Name := 'ROWID';
  asm
    @Info=@PropCache[@Name];
  end;
  result := VarIsValidRef(Info);
end;

var
  RTTI_Cache: variant = TVariant.CreateObject;

{$HINTS OFF}
class function TSQLRecord.GetRTTI: TRTTIPropInfos;
begin // use RTTI_Cache as global dictionary of all TSQLRecord's RTTI
  var name = ClassName;
  var value: variant;
  asm
    @value = @RTTI_Cache[@name];
  end;
  if not VarIsValidRef(value) then begin
    value := ComputeRTTI;
    asm
      @RTTI_Cache[@name]=@value;
    end;
  end;
  asm
    return @value;
  end;
end;
{$HINTS ON}

class function TSQLRecord.ComputeRTTI: TRTTIPropInfos;
begin
  result := TRTTIPropInfos.Create([],[]);
end;

{$endif ISSMS}

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

destructor TSQLRecord.Destroy;
begin
  fFill.Free;
  inherited;
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

{$ifdef ISSMS}

function TSQLRecord.FromNamesValues(const Names: TStrArray;
  const Values: TVariantDynArray; ValuesStartIndex: integer): boolean;
var i: integer;
    info: TSQLModelInfoPropInfo;
    rtti: TRTTIPropInfos;
begin
  result := false;
  if ValuesStartIndex+length(Names)>length(Values) then
    exit;
  rtti := GetRTTI;
  for i := 0 to high(Names) do
    if Find(rtti.PropCache,Names[i],info) then begin
      var name := info.Name;
      var value := Values[i+ValuesStartIndex];
      asm
        @self[@name]=@value;
      end;
    end else
      exit;
  result := true;
end;

{$endif}

function TSQLRecord.FromJSON(const aJSON: string): boolean;
var doc: TJSONVariantData;
    table: TSQLTableJSON;
    {$ifndef ISSMS}
    i: Integer;
    {$endif}
begin
  if (self=nil) or (aJSON='') then
    result := false else
  if StartWithPropName(aJSON,'{"fieldCount":') then begin
    table := TSQLTableJSON.Create(aJSON); // non expanded format
    try
      result := table.FillOne(self);
    finally
      table.Free;
    end;
  end else begin // expanded format
    {$ifdef ISSMS}
    doc := TJSONVariantData.Create(aJSON);
    result := FromNamesValues(doc.Names,doc.Values,0);
    {$else}
    doc.Init(aJSON);
    for i := 0 to doc.Count-1 do
      if IsRowID(doc.Names[i]) then
        doc.Names[i] := 'ID';
    result := doc.ToObject(self);
    {$endif}
  end;
end;

function TSQLRecord.RecordClass: TSQLRecordClass;
begin
  if self=nil then
    result := nil else
    result := TSQLRecordClass(ClassType);
end;

{ TSQLTableJSON }

{$ifdef ISDWS} // circumvent weird DWS / SMS syntax

constructor TSQLTableJSON.Create(const aJSON: string);
begin
  var dat = JSON.Parse(aJSON);
  case VariantType(dat) of
  jvObject: begin
    // non expanded format: {"fieldCount":2,"values":["ID","Int",1,0,2,0,3,...]
    fFieldCount := dat.fieldCount;
    var values := dat.values;
    if VariantType(values)<>jvArray then
      exit;
    asm
      @fValues=@values;
    end;
    var n = fValues.Count;
    if (n<fFieldCount) or (n mod fFieldCount<>0) then
      exit;
    for var i := 0 to fFieldCount-1 do
      fFieldNames.Add(string(fValues[i]));
    fRowCount := (n div fFieldCount)-1;
    if fRowCount>0 then
      fCurrentRow := 1;
  end;
  jvArray: begin
    // expanded format: [{"ID":1,"Int":0},{"ID":2,"Int":0},{"ID":3,...]
    asm
      @fValues=@dat;
    end;
    fRowCount := fValues.Count;
    if fRowCount>0 then
      fCurrentRow := 1;
  end;
  end;
end;

function TSQLTableJSON.FillOne(Value: TSQLRecord; SeekFirst: boolean=false): boolean;
begin
  result := false;
  if (Value=nil) or (fRowCount=0) then
    exit;
  if SeekFirst then
    fCurrentRow := 1 else
    if fCurrentRow>fRowCount then
      exit;
  if fFieldNames.Count>0 then begin
    // non expanded format
    result := Value.FromNamesValues(fFieldNames,fValues,fCurrentRow*fFieldCount);
  end else begin
    // expanded format
    var doc := TJSONVariantData.CreateFrom(fValues[fCurrentRow-1]);
    result := Value.FromNamesValues(doc.Names,doc.Values,0);
  end;
  inc(fCurrentRow);
end;

{$else}

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


{ TSQLModelInfoPropInfo }

constructor TSQLModelInfoPropInfo.CreateFrom(aRTTI: TRTTIPropInfo);
begin
  RTTI := aRTTI;
  TypeName := RTTIPropInfoTypeName(RTTI);
  if TypeName='TByteDynArray' then
    Kind := sftBlob else
  if TypeName='TDateTime' then
    Kind := sftDateTime else
  if TypeName='TCreateTime' then
    Kind := sftCreateTime else
  if TypeName='TModTime' then
    Kind := sftModTime;
end;

{$endif ISDWS}


{ TSQLModelInfo }

procedure TSQLModelInfo.ComputeFieldsBeforeWrite(aClient: TSQLRest;
  Value: TSQLRecord; AndCreate: Boolean);
var f: TSQLFieldBit;
    fields: TSQLFieldBits;
    TimeStamp: Int64;
begin
  if not HasTimeFields then
    exit;
  if AndCreate then
    fields := ModAndCreateTimeFields else
    fields := ModTimeFields;
  TimeStamp := aClient.ServerTimeStamp;
  for f := 0 to length(Prop)-1 do
    if f in fields then
      {$ifdef ISSMS} begin
        var name := Prop[ord(f)].Name;
        asm
          @Value[@name]=@TimeStamp;
        end;
      end;
      {$else}
      SetInstanceProp(Value,Prop[f].RTTI,TimeStamp);
      {$endif}
end;

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

constructor TSQLModelInfo.CreateFromRTTI(aTable: TSQLRecordClass);
var f: TSQLFieldBit;
    Kind: TSQLFieldKind;
{$ifdef ISDWS}
    rtti: TRTTIPropInfos;
{$else}
    List: TRTTIPropInfoDynArray;
    Names: TStringDynArray;
{$endif}
begin
  Table := aTable;
  Name := GetDisplayNameFromClass(Table);
  {$ifdef ISDWS}
  rtti := aTable.GetRTTI;
  Prop := rtti.Props;
  PropCache := rtti.PropCache;
  {$else}
  GetPropsInfo(Table.ClassInfo,Names,List);
  SetLength(Prop,length(List));
  for f := 0 to high(List) do begin
    Prop[f] := TSQLModelInfoPropInfo.CreateFrom(List[f]);
    if f=0 then
      Prop[f].Name := 'RowID' else
      Prop[f].Name := Names[f];
  end;
  {$endif}
  for f := 0 to TSQLFieldBit(high(Prop)) do begin
    include(AllFields,f);
    Kind := Prop[ord(f)].Kind;
    include(HasKind,Kind);
    if Kind=sftBlob then
      Include(BlobFields,f) else
      Include(SimpleFields,f);
    case Kind of
    sftModTime: begin
      include(ModTimeFields,f);
      include(ModAndCreateTimeFields,f);
      HasTimeFields := true;
    end;
    sftCreateTime: begin
      include(CreateTimeFields,f);
      include(ModAndCreateTimeFields,f);
      HasTimeFields := true;
    end;
    end;
  end;
end;

{$ifndef ISSMS}
destructor TSQLModelInfo.Destroy;
var i: integer;
begin
  inherited;
  for i := 0 to Length(Prop)-1 do
    Prop[i].Free;
end;
{$endif}

function TSQLModelInfo.FieldBitsToFieldNames(
  const FieldBits: TSQLFieldBits): string;
var f: TSQLFieldBit;
begin
  result := '';
  for f := 0 to length(Prop)-1 do
  if f in FieldBits then
    result := result+Prop[ord(f)].Name+',';
  if result<>'' then
    SetLength(result,length(result)-1);
end;

function TSQLModelInfo.FieldNamesToFieldBits(const FieldNames: string;
  IncludeModTimeFields: boolean): TSQLFieldBits;
var i: integer;
    f: TSQLFieldBit;
    field: string;
begin
  if FieldNames='' then
    result := SimpleFields else
  if FieldNames='*' then
    result := AllFields else begin
    result := NO_SQLFIELDBITS;
    i := 1;
    while GetNextCSV(FieldNames,i,field,',') do begin
      {$ifdef ISSMS}
      var Info: TSQLModelInfoPropInfo;
      if Find(PropCache,field,info) then
        include(result,info.FieldIndex);
      {$else}
      if IsRowID(field) then
        Include(result,ID_SQLFIELD) else
        for f := 1 to length(Prop)-1 do
          if IdemPropName(field,Prop[ord(f)].Name) then begin
            include(result,f);
            break;
          end;
      {$endif}
    end;
    {$ifdef ISSMS}
    if IncludeModTimeFields and (sftModTime in HasKind) then
      for f := 1 to length(Prop)-1 do
        if f in ModTimeFields then
          include(result,f);
    {$else}
    if IncludeModTimeFields then
      result := result+ModTimeFields;
    {$endif}
  end;
end;

function TSQLModelInfo.SQLSelect(const FieldNames: string): string;
begin
  result := 'select '+FieldBitsToFieldNames(FieldNamesToFieldBits(
    FieldNames,false))+' from '+Name;
end;

function TSQLModelInfo.ToJSON(Value: TSQLRecord;
  const Fields: TSQLFieldBits): string;
var f: TSQLFieldBit;
begin
{$ifdef ISSMS}
  var doc: variant;
  for f in Fields do begin // only serialize the main Prop[] fields
    var name := Prop[ord(f)].Name;
    asm
      @doc[@name]=@Value[@name];
    end;
  end;
  result := JSON.Stringify(doc);
{$else}
  result := '{';
  for f := 0 to length(Prop)-1 do
    if f in Fields then
      result := result+'"'+Prop[ord(f)].Name+'":'+
        ValueToJSON(GetInstanceProp(Value,Prop[f].RTTI))+',';
  if result='{' then
    result := 'null' else
    result[Length(Result)] := '}';
{$endif}
end;

function TSQLModelInfo.ToJSONAdd(Client: TSQLRest;
  Value: TSQLRecord; ForceID: boolean): string;
var Fields: TSQLFieldBits;
begin
  ComputeFieldsBeforeWrite(Client,Value,true);
  fields := SimpleFields;
  if not ForceID then
    exclude(fields,ID_SQLFIELD);
  result := ToJSON(Value,fields);
end;

function TSQLModelInfo.ToJSONUpdate(Client: TSQLRest; Value: TSQLRecord;
  const FieldNames: string): string;
var Fields: TSQLFieldBits;
begin
  fields := FieldNamesToFieldBits(FieldNames,true);
  exclude(fields,ID_SQLFIELD);
  ComputeFieldsBeforeWrite(Client,Value,false);
  result := ToJSON(Value,fields);
end;


{ TSQLModel }

procedure TSQLModel.Add(Table: TSQLRecordClass);
var n,i: integer;
begin
  n := length(fInfo);
  for i := 0 to n-1 do
    if fInfo[i].Table=Table then
      raise ERESTException.CreateFmt('%s registered twice',[Table.ClassName]);
  {$ifdef ISSMS}
  fInfo.SetLength(n+1);
  {$else}
  SetLength(fInfo,n+1);
  {$endif}
  fInfo[n] := TSQLModelInfo.CreateFromRTTI(Table);
end;

constructor TSQLModel.Create(const Tables: array of TSQLRecordClass;
  const aRoot: string);
var t: integer;
begin
  {$ifdef ISSMS}
  fInfo.SetLength(length(Tables));
  {$else}
  SetLength(fInfo,length(Tables));
  {$endif}
  for t := 0 to high(fInfo) do
    fInfo[t] := TSQLModelInfo.CreateFromRTTI(Tables[t]);
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

{$ifndef ISSMS}
destructor TSQLModel.Destroy;
var i: integer;
begin
  inherited;
  for i := 0 to high(fInfo) do
    fInfo[i].Free;
end;
{$endif}

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
begin
  tableIndex := Model.GetTableIndexExisting(Value.RecordClass);
  if SendData then
    json := Model.Info[tableIndex].ToJSONAdd(self,Value,ForceID);
  result := ExecuteAdd(tableIndex,json);
end;

constructor TSQLRest.Create(aModel: TSQLModel);
begin
  inherited Create;
  fModel := aModel;
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
var sql: string;
begin
  sql := Model.Info[Model.GetTableIndexExisting(Table)].SQLSelect(FieldNames);
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
  {$ifndef ISSMS} // result is already created as "array of TObject"
  result := TObjectList.Create;
  {$endif}
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

function TSQLRest.Update(Value: TSQLRecord): boolean;
begin
  result := Update(Value,'');
end;

function TSQLRest.Update(Value: TSQLRecord; const FieldNames: string): boolean;
var tableIndex: Integer;
    json: string;
begin
  if (Value=nil) or (Value.ID<=0) then begin
    result := false;
    exit;
  end;
  tableIndex := Model.GetTableIndexExisting(Value.RecordClass);
  json := Model.Info[tableIndex].ToJSONUpdate(self,Value,FieldNames);
  result := ExecuteUpdate(tableIndex,Value.ID,json);
end;

{ TSQLRestClientURI }

function TSQLRestClientURI.getURI(aTable: TSQLRecordClass): string;
begin
  result := Model.Root;
  if (aTable<>nil) and (aTable<>TSQLRecord) then // SMS converts nil->TSQLRecord
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
  // strict HTTP does not allow any body content -> encode SQL at URL
  // so we expect reUrlEncodedSQL to be defined in AllowRemoteExecute
  Call.Url := Model.Root+UrlEncode(['sql',sql]);
  Call.Method := 'GET';
  URI(Call);
  if Call.OutStatus=HTML_SUCCESS then begin
    {$ifdef ISSMS}
    json := Call.OutBody; // XMLHttpRequest did convert UTF-8 into DomString
    {$else}
    HttpBodyToText(Call.OutBody,json);
    {$endif}
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
    {$ifdef ISSMS}
    json := Call.OutBody; // XMLHttpRequest did convert UTF-8 into DomString
    {$else}
    HttpBodyToText(Call.OutBody,json);
    {$endif}
    Value.FromJSON(json);
  end;
end;

procedure TSQLRestClientURI.URI(var Call: TSQLRestURIParams);
var sign: string;
begin
  Call.OutStatus := HTML_UNAVAILABLE;
  Call.OutInternalState := 0;
  if self=nil then
    exit;
  if (fAuthentication<>nil) and (fAuthentication.SessionID<>0) then begin
    if Pos('?',Call.Url)=0 then
      sign := '?session_signature=' else
      sign := '&session_signature=';
    Call.Url := Call.Url+sign+
      fAuthentication.ClientSessionComputeSignature(self,Call.Url);
  end;
  InternalURI(Call);
  Call.OutInternalState := StrToIntDef(
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

function TSQLRestClientURI.CallBackGetResult(const aMethodName: string;
  const aNameValueParameters: array of const; aTable: TSQLRecordClass;
  aID: integer): string;
var {$ifdef ISSMS}
    doc: variant;
    {$else}
    doc: TJSONVariantData;
    jsonres: string;
    {$endif}
    Call: TSQLRestURIParams;
begin
  CallBackGet(aMethodName,aNameValueParameters,Call,aTable,aID);
  if Call.OutStatus<>HTML_SUCCESS then
    result := '' else begin
    {$ifdef ISSMS}
    doc := JSON.Parse(Call.OutBody);
    if VarIsValidRef(result) then
      result := doc.result;
    {$else}
    HttpBodyToText(Call.OutBody,jsonres);
    doc.Init(jsonres);
    result := doc.Value['result'];
    {$endif}
  end;
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
  {$ifdef ISSMS}
  tmp := Call.OutBody; // XMLHttpRequest did convert UTF-8 into DomString
  {$else}
  HttpBodyToText(Call.OutBody,tmp);
  {$endif}
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
  {$ifdef ISSMS}
  Call.InBody := json; // XMLHttpRequest will convert the DomString into UTF-8
  {$else}
  Call.InBody := TextToHttpBody(json);
  {$endif}
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
  {$ifdef ISSMS}
  Call.InBody := json;
  {$else}
  Call.InBody := TextToHttpBody(json);
  {$endif}
  URI(Call);
  result := Call.OutStatus=HTML_SUCCESS;
end;

procedure TSQLRestClientURI.SetUser(aAuthenticationClass: TSQLRestAuthenticationClass;
  const aUserName, aPassword: string; aHashedPassword: Boolean);
var aKey, aSessionID: string;
    i: integer;
begin
  if fAuthentication<>nil then
    SessionClose;
  if aAuthenticationClass=nil then
    exit;
  fAuthentication := aAuthenticationClass.Create(aUserName,aPassword,aHashedPassword);
  aKey := fAuthentication.ClientComputeSessionKey(self);
  i := 1;
  GetNextCSV(aKey,i,aSessionID,'+');
  if TryStrToInt(aSessionID,i) then
    fAuthentication.SetSessionID(i) else begin
    fAuthentication.Free;
    fAuthentication := nil;
  end;
end;

procedure TSQLRestClientURI.SessionClose;
var Call: TSQLRestURIParams;
begin
  if fAuthentication<>nil then
    try
      CallBackGet('auth',['UserName',fAuthentication.User.LogonName,
        'Session',fAuthentication.SessionID],Call);
    finally
      fAuthentication.Free;
      fAuthentication := nil;
    end;
end;

{ TSQLRestClientHTTP }

constructor TSQLRestClientHTTP.Create(const aServer: string;
  aPort: integer; aModel: TSQLModel; aHttps: boolean
  {$ifndef ISSMS}; const aProxyName, aProxyByPass: string;
                   aSendTimeout, aReceiveTimeout: Cardinal{$endif});
begin
  inherited Create(aModel);
  fParameters.Server := aServer;
  fParameters.Port := aPort;
  fParameters.Https := aHttps;
  {$ifndef ISSMS}
  fParameters.ProxyName := aProxyName;
  fParameters.ProxyByPass := aProxyByPass;
  fParameters.SendTimeout := aSendTimeout;
  fParameters.ReceiveTimeout := aReceiveTimeout;
  fKeepAlive := 20000;
  {$endif}
end;

destructor TSQLRestClientHTTP.Destroy;
begin
  inherited;
  fAuthentication.Free;
  fConnection.Free;
end;

procedure TSQLRestClientHTTP.InternalURI(var Call: TSQLRestURIParams);
var inType: string;
    retry: integer;
begin
  inType := FindHeader(Call.InHead,'content-type:');
  if inType='' then begin
    inType := JSON_CONTENT_TYPE;
    Call.InHead := trim(Call.InHead+#13#10'content-type:'+inType);
  end;
  for retry := 0 to 1 do begin
    if fConnection=nil then
      try
        fConnection := HttpConnectionClass.Create(fParameters);
        // TODO: handle SynLZ compression and SHA/AES encryption
      except
        on E: Exception do begin
          fConnection.Free;
          fConnection := nil;
        end;
      end;
    if fConnection=nil then begin
      Call.OutStatus := HTML_NOTIMPLEMENTED;
      exit;
    end;
    try
      fConnection.URI(Call,inType,fKeepAlive);
      break; // do not retry on transmission success
    except
      on E: Exception do begin
        fConnection.Free;
        fConnection := nil;
      end; // will retry once (e.g. if connection broken)
    end;
  end;
end;


{ TSQLAuthUser }

function SHA256Compute(const Values: array of string): string;
var buf: THttpBody;
    a: integer;
    sha: TSHA256;
begin
  sha := TSHA256.Create;
  try
    for a := 0 to high(Values) do begin
      buf := TextToHttpBody(Values[a]);
      sha.Update(buf);
    end;
    result := sha.Finalize;
  finally
    sha.Free;
  end;
end;

{$ifdef ISSMS}
class function TSQLAuthUser.ComputeRTTI: TRTTIPropInfos;
begin
  result := TRTTIPropInfos.Create(
    ['Data','Group','LogonName','DisplayName','PasswordHashHexa'],
    [sftBlob]);
end;
{$endif}

procedure TSQLAuthUser.SetPasswordPlain(const Value: string);
begin
  PasswordHashHexa := SHA256Compute(['salt',Value]);
end;


{ TSQLRestAuthentication }

constructor TSQLRestAuthentication.Create(const aUserName, aPassword: string;
  aHashedPassword: Boolean);
begin
  fUser := TSQLAuthUser.Create;
  fUser.LogonName := aUserName;
  if aHashedPassword then
    fUser.PasswordHashHexa := aPassword else
    fUser.PasswordPlain := aPassword;
end;

destructor TSQLRestAuthentication.Destroy;
begin
  fUser.Free;
  inherited;
end;

procedure TSQLRestAuthentication.SetSessionID(Value: Cardinal);
begin
  fSessionID := Value;
  fSessionIDHexa8 := LowerCase(IntToHex(Value,8));
end;

{ TSQLRestAuthenticationDefault }

function TSQLRestAuthenticationDefault.ClientComputeSessionKey(
  Sender: TSQLRestClientURI): string;
var aServerNonce, aClientNonce: string;
begin
  if fUser.LogonName='' then
    exit;
  aServerNonce := Sender.CallBackGetResult('auth',
    ['UserName',User.LogonName]);
  if aServerNonce='' then
    exit;
  aClientNonce := SHA256Compute([Copy(DateTimeToIso8601(Now),1,16)]);
  result := Sender.CallBackGetResult('auth',
     ['UserName',User.LogonName,'Password',Sha256Compute(
      [Sender.Model.Root,aServerNonce,aClientNonce,User.LogonName,User.PasswordHashHexa]),
      'ClientNonce',aClientNonce]);
  fSessionPrivateKey := crc32ascii(crc32ascii(0,result),fUser.fPasswordHashHexa);
end;

function TSQLRestAuthenticationDefault.ClientSessionComputeSignature(
  Sender: TSQLRestClientURI; const url: string): string;
var nonce: string;
begin
  nonce := LowerCase(IntToHex(trunc(Now*(24*60*60)),8));
  result := fSessionIDHexa8+nonce+LowerCase(IntToHex(
    crc32ascii(crc32ascii(fSessionPrivateKey,nonce),url),8));
end;

{ TSQLRestServerAuthenticationNone }

function TSQLRestAuthenticationNone.ClientComputeSessionKey(
  Sender: TSQLRestClientURI): string;
begin
  result := Sender.CallBackGetResult('auth',['UserName',User.LogonName]);
end;

function TSQLRestAuthenticationNone.ClientSessionComputeSignature(
  Sender: TSQLRestClientURI; const url: string): string;
begin
  result := fSessionIDHexa8;
end;

{ TSQLAuthGroup }

{$ifdef ISSMS}
class function TSQLAuthGroup.ComputeRTTI: TRTTIPropInfos;
begin
  result := TRTTIPropInfos.Create(
    ['Ident','SessionTimeOut','AccessRights'],[]);
end;
{$endif}

end.