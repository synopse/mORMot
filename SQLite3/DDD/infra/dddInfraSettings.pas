/// shared DDD Infrastructure: Application/Daemon common classes
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit dddInfraSettings;

{
    This file is part of Synopse mORMot framework.

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

  TODO:
   - store settings in database
   - allow to handle authentication via a centralized service or REST server 

}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  mORMotService,
  {$else}
  {$ifdef FPC}
  SynFPCLinux,
  {$endif}
  {$endif}
  SysUtils,
  Classes,
  SynCommons,
  SynLog,
  mORMot,
  mORMotDDD,
  SynSQLite3, SynSQLite3Static, mORMotSQLite3, // for internal SQlite3 database
  mORMotHttpServer, // for publishing a TSQLRestServer over HTTP
  mORMotHttpClient, // for consumming a TSQLRestClient over HTTP
  SynDB, mORMotDB,  // for TRestSettings on external SQL database
  mORMotMongoDB,    // for TRestSettings on external NoSQL database
  mORMotWrappers;   // for TRestSettings to publish wrapper methods


{ ----- Manage Service/Daemon settings }

type
  /// abstract class for storing application settings
  // - this class implements IAutoCreateFieldsResolve so is able to inject
  // its own values to any TInjectableAutoCreateFields instance
  // - you have to manage instance lifetime of these inherited classes with a
  // local IAutoCreateFieldsResolve variable, just like any TInterfaceObject
  TApplicationSettingsAbstract = class(TInterfacedObjectAutoCreateFields,
    IAutoCreateFieldsResolve)
  protected
    fAllProps: PPropInfoDynArray;
    fInitialJsonContent: RawUTF8;
    procedure SetProperties(Instance: TObject); virtual;
    // inherited constructors should use this method to initialize the content
    procedure SetJsonContent(JsonContent: PUTF8Char); virtual;
  public
    /// returns TRUE if the content did change according to its initial state
    // - will be used e.g. to update the file content on disk only if worth it
    function WasModified: Boolean;
    /// the JSON content, as specified when creating the instance
    // - will allow SettingsDidChange to check if has changed
    // - here the JSON content is stored with default ObjectToJSON() options,
    // so will be the normalized representation of the content, which may not
    // match the JSON supplied to SetJsonContent() protected method
    property InitialJsonContent: RawUTF8 read fInitialJsonContent;
  end;

  /// settings used to define how logging take place
  // - will map the most used TSynLogFamily parameters
  TLogSettings = class(TSynPersistent)
  protected
    fLevels: TSynLogInfos;
    fConsoleLevels: TSynLogInfos;
    fPerThread: TSynLogPerThreadMode;
    fDestinationPath: TFileName;
    fRotateFileCount: cardinal;
    fRotateFileSize: cardinal;
    fRotateFileAtHour: integer;
  public
    /// initialize the settings to their (TSynLogFamily) default values
    constructor Create; override;
  published
    /// the log levels to be used for the log file
    // - i.e. a combination of none or several logging event
    // - if "*" is serialized, unneeded sllNone won't be part of the set
    property Levels: TSynLogInfos read fLevels write fLevels;
    /// the optional log levels to be used for the console
    // - by default, only errors would be logged to the console
    // - you can specify here another set of levels, e.g. '*' for a verbose
    // console output - note that console is very slow to write, so usually
    // you should better not set a verbose definition here, unless you are
    // in debugging mode
    property ConsoleLevels: TSynLogInfos read fConsoleLevels write fConsoleLevels;
    /// the logged information about threads
    // - the default value is ptIdentifiedInOnFile, since it sounds more
    // reasonable to a multi-threaded server instance
    property PerThread: TSynLogPerThreadMode read fPerThread write fPerThread;
    /// allows to customize where the log files will be stored
    property DestinationPath: TFileName read FDestinationPath write FDestinationPath;
    /// auto-rotation of logging files
    // - set to 0 by default, meaning no rotation
    property RotateFileCount: cardinal read fRotateFileCount write fRotateFileCount;
    /// maximum size of auto-rotated logging files, in kilo-bytes (per 1024 bytes)
    property RotateFileSizeKB: cardinal read fRotateFileSize write fRotateFileSize;
    /// fixed hour of the day where logging files rotation should be performed
   property RotateFileDailyAtHour: integer read fRotateFileAtHour write fRotateFileAtHour;
  end;

  /// parent class for storing application settings as a JSON file
  TApplicationSettingsFile = class(TApplicationSettingsAbstract)
  protected
    fSettingsJsonFileName: TFileName;
    fDescription: string;
    fLog: TLogSettings;
  public
    /// initialize and read the settings from the supplied JSON file name
    // - if no file name is specified, will use the executable name with
    // '.settings' as extension
    constructor Create(const aSettingsJsonFileName: TFileName=''); reintroduce; virtual;
    /// save to file and finalize the settings
    destructor Destroy; override;
    /// save settings to file
    // - any enumerated or set published property will be commented with their
    // textual values, and 'stored false' properties would be included
    procedure UpdateFile; virtual;
    /// to be called when the application starts, to access settings
    // - will change the system current directory to SettingsJsonFileName, and
    // set the log settings as expected
    // - you can specify a default Description value
    procedure Initialize(const aDescription: string); virtual;
    /// compute a file name relative to the .settings file path
    function FileNameRelativeToSettingsFile(const aFileName: TFileName): TFileName;
    /// the .settings file name, including full path
    property SettingsJsonFileName: TFileName
      read fSettingsJsonFileName write fSettingsJsonFileName;
  published
    /// some text which will be used to describe this application
    property Description: string read FDescription write FDescription;
    /// defines how logging will be done for this application
    property Log: TLogSettings read fLog;
  end;

  /// some options to be used for TRestSettings
  TRestSettingsOption =
    (optEraseDBFileAtStartup,optStoreDBFileRelativeToSettings,
     optSQlite3FileSafeSlowMode);

  /// define options to be used for TRestSettings
  TRestSettingsOptions = set of TRestSettingsOption;

  /// how TRestSettings.NewRestInstance would create its instances
  // - riOwnModel will set ModelInstance.Owner := RestInstance
  // - riHandleAuthentication will set the corresponding parameter to true
  // - riDefaultLocalSQlite3IfNone will create a SQLite3 engine with a local
  // file, if TRestSettings.Kind is not set
  // - riCreateMissingTables will call RestInstance.CreateMissingTables
  TNewRestInstanceOptions = set of (
    riOwnModel,
    riHandleAuthentication,
    riDefaultLocalSQlite3IfNone,
    riCreateMissingTables);

  /// storage class for initializing an ORM REST class
  // - this class will contain some generic properties to initialize a TSQLRest
  // pointing to a local or remote SQL/NoSQL database, with optional wrappers
  TRestSettings = class(TSynAutoCreateFields)
  protected
    fORM: TSynConnectionDefinition;
    fRoot: RawUTF8;
    fWrapperTemplateFolder: TFileName;
    fWrapperSourceFolders: TFileName;
    fOptions: TRestSettingsOptions;
    fWrapperTemplateFolderFixed: TFileName;
    fWrapperSourceFolderFixed: TFileName;
  public
    /// is able to instantiate a REST instance according to the stored definition
    // - Definition.Kind will identify the TSQLRestServer or TSQLRestClient class
    // to be instantiated, or if equals 'MongoDB' use a full MongoDB store, or an
    // external SQL database if it matches a TSQLDBConnectionProperties classname
    // - if aDefaultLocalSQlite3 is TRUE, then if Definition.Kind is '',
    // a local SQlite3 file database will be initiated
    // - will return nil if the supplied Definition is not correct
    // - note that the supplied Model.Root is expected to be the default root
    // URI, which will be overriden with this TRestSettings.Root property
    // - will also set the TSQLRest.LogFamily.Level from LogLevels value,
    // and publish the /wrapper HTML page if WrapperTemplateFolder is set
    function NewRestInstance(aRootSettings: TApplicationSettingsFile;
      aModel: TSQLModel; aOptions: TNewRestInstanceOptions;
      aExternalDBOptions: TVirtualTableExternalRegisterOptions=[regDoNotRegisterUserGroupTables];
      aMongoDBOptions: TStaticMongoDBRegisterOptions=[mrDoNotRegisterUserGroupTables]): TSQLRest; virtual;
    /// returns the WrapperTemplateFolder property, all / chars replaced by \
    // - so that you would be able to store the paths with /, avoiding JSON escape
    function WrapperTemplateFolderFixed: TFileName;
    /// returns the WrapperSourceFolder property, all / chars replaced by \
    // - so that you would be able to store the paths with /, avoiding JSON escape
    function WrapperSourceFolderFixed: TFileName;
  published
    /// the URI Root to be used for the REST Model
    property Root: RawUTF8 read fRoot write fRoot;
    /// would let function NewRestInstance() create the expected TSQLRest
    property ORM: TSynConnectionDefinition read fORM;
    /// if set to a valid folder, the generated TSQLRest will publish a
    // '/Root/wrapper' HTML page so that client code could be generated
    property WrapperTemplateFolder: TFileName
      read fWrapperTemplateFolder write fWrapperTemplateFolder;
    /// where the source code may be searched, for comment extraction of types
    // - several folders may be defined, separated by ; (just like in Delphi IDE)
    // - only used if WrapperTemplateFolder is defined
    property WrapperSourceFolders: TFileName
      read fWrapperSourceFolders write fWrapperSourceFolders;
    /// how the REST instance is to be initialized
    property Options: TRestSettingsOptions read fOptions write fOptions;
  end;

  /// parent class for storing REST-based application settings as a JSON file
  // - this class could be used for an application with a single REST server
  // running on a given HTTP port
  TApplicationSettingsRestFile = class(TApplicationSettingsFile)
  protected
    fRest: TRestSettings;
    fServerPort: RawUTF8;
  public
    /// to be called when the application starts, to access settings
    // - will call inherited TApplicationSettingsFile.Initialize, and
    // set ServerPort to a default 888/8888 value under Windows/Linux
    procedure Initialize(const aDescription: string); override;
  published
    /// allow to instantiate a REST instance from its JSON definition
    property Rest: TRestSettings read fRest;
    /// the IP port to be used for the HTTP server associated with the application
    property ServerPort: RawUTF8 read fServerPort write fServerPort;
  end;

  /// define how an administrated service/daemon is remotely accessed via REST
  // - the IAdministratedDaemon service will be published to administrate
  // this service/daemon instance
  // - those values should match the ones used on administrative tool side
  TAdministratedDaemonSettings = class(TSynAutoCreateFields)
  protected
    FAuthRootURI: RawUTF8;
    FAuthHashedPassword: RawUTF8;
    FAuthUserName: RawUTF8;
    FAuthNamedPipeName: TFileName;
    FAuthHttp: TSQLHttpServerDefinition;
  public
    /// set default settings
    // - i.e. AuthRootURI='admin' and plain AuthHttp.WebSocketPassword=ClassName
    constructor Create; override;
  published
    /// the root URI used for the REST data model
    // - default URI is 'admin'
    property AuthRootURI: RawUTF8 read FAuthRootURI write FAuthRootURI;
    /// if set, expect authentication with this single user name
    // - that is, the TSQLRestServer will register a single TSQLAuthUser
    // instance with the supplied AuthUserName/AuthHashedPassword credentials
    property AuthUserName: RawUTF8 read FAuthUserName write FAuthUserName;
    /// the SHA-256 hashed password to authenticate AuthUserName
    // - follows the TSQLAuthUser.PasswordHashHexa expectations
    // - marked as 'stored false' so that it won't appear e.g. in the logs
    property AuthHashedPassword: RawUTF8 read FAuthHashedPassword write FAuthHashedPassword
      stored false;
    /// if defined, the following pipe name would be used for REST publishing
    // - by definition, will work only on Windows
    property AuthNamedPipeName: TFileName read FAuthNamedPipeName write FAuthNamedPipeName;
    /// if defined, these parameters would be used for REST publishing over HTTP
    property AuthHttp: TSQLHttpServerDefinition read FAuthHttp;
  end;

  /// parent class for storing a service/daemon settings as a JSON file
  // - under Windows, some Service* properties will handle installaiton as a
  // regular Windows Service, thanks to TAbstractDaemon
  TAdministratedDaemonSettingsFile = class(TApplicationSettingsFile)
  protected
    FRemoteAdmin: TAdministratedDaemonSettings;
    FServiceDisplayName: string;
    FServiceName: string;
    FServiceAutoStart: boolean;
  public
    /// to be called when the application starts, to access settings
    // - you can specify default Description and Service identifiers
    procedure Initialize(
      const aDescription,aServiceName,aServiceDisplayName: string); reintroduce; virtual;
  published
    /// define how this administrated service/daemon is accessed via REST
    property RemoteAdmin: TAdministratedDaemonSettings read FRemoteAdmin;
    /// under Windows, will define the Service internal name
    property ServiceName: string read FServiceName write FServiceName;
    /// under Windows, will define the Service displayed name
    property ServiceDisplayName: string read FServiceDisplayName write FServiceDisplayName;
    /// under Windows, will define if the Service should auto-start at boot
    // - FALSE means that it should be started on demand
    property ServiceAutoStart: boolean read FServiceAutoStart write FServiceAutoStart;
  end;

  /// abstract class to handle any kind of service/daemon executable
  // - would implement either a Windows Service, a stand-alone remotely
  // administrated daemon, or a console application, according to the command line
  // - you should inherit from this class, then override the abstract NewDaemon
  // protected method to launch and return a IAdministratedDaemon instance
  TAbstractDaemon = class
  protected
    fSettings: TAdministratedDaemonSettingsFile;
    /// the service/daemon will be stopped when this interface is set to nil
    fDaemon: IAdministratedDaemon;
    /// this abstract method should be overriden to return a new service/daemon
    // instance, using the (inherited) fSettings as parameters
    function NewDaemon: TDDDAdministratedDaemon; virtual; abstract;
    {$ifdef MSWINDOWS} // to support Windows Services
    procedure DoStart(Sender: TService);
    procedure DoStop(Sender: TService);
    {$endif}
  public
    /// initialize the service/daemon application thanks to some information
    // - actual settings would inherit from TAdministratedDaemonSettingsFile,
    // to define much more parameters, according to the service/daemon process
    // - the supplied settings will be owned by this TAbstractDaemon instance
    constructor Create(aSettings: TAdministratedDaemonSettingsFile);
    /// finalize the service/daemon application, and release its resources
    destructor Destroy; override;
    /// interprect the command line to run the application as expected
    // - under Windows, /install /uninstall /start /stop would control the
    // daemon as a Windows Service - you should run the program with
    // administrator rights to be able to change the Services settings
    // - /console or /c would run the program as a console application
    // - /verbose would run the program as a console application, in verbose mode
    // - /daemon or /d would run the program as a remotely administrated
    // daemon, using a published IAdministratedDaemon service
    // - /version would show the current revision information of the application
    // - no command line argument will run the program as a Service dispatcher
    // under Windows (as a regular service), or display the syntax
    // - any invalid switch, or no switch under Linux, will display the syntax
    // - this method will output any error or information to the console
    // - as a result, a project .dpr could look like the following:
    //!begin
    //!  with TMyApplicationDaemon.Create(TMyApplicationDaemonSettings.Create) do
    //!  try
    //!    ExecuteCommandLine;
    //!  finally
    //!    Free;
    //!  end;
    //!end.
    procedure ExecuteCommandLine;
  end;

  /// abstract class to implement a IAdministratedDaemon service via a TThread
  // - as hosted by TAbstractDaemon service/daemon application
  TAbstractThreadDaemon = class(TDDDAdministratedThreadDaemon)
  protected
    fAdministrationHTTPServer: TSQLHttpServer;
  public
    /// initialize the thread with the supplied parameters
    constructor Create(aSettings: TAdministratedDaemonSettingsFile); reintroduce;
    /// finalize the service/daemon thread
    // - will call Halt() if the associated process is still running
    destructor Destroy; override;
    /// reference to the HTTP server publishing IAdministratedDaemon service
    // - may equal nil if TAdministratedDaemonSettingsFile.AuthHttp.BindPort=''
    property AdministrationHTTPServer: TSQLHttpServer read fAdministrationHTTPServer;
  end;




implementation


{ TApplicationSettingsAbstract }

procedure TApplicationSettingsAbstract.SetJsonContent(
  JsonContent: PUTF8Char);
var valid: boolean;
begin
  if JsonContent=nil then
    exit;
  RemoveCommentsFromJSON(JsonContent);
  JSONToObject(self,JsonContent,valid);
  if valid then
    fInitialJsonContent := ObjectToJSON(self,[]);
end;

procedure TApplicationSettingsAbstract.SetProperties(Instance: TObject);
begin
  CopyObject(self,Instance);
end;

function TApplicationSettingsAbstract.WasModified: Boolean;
begin
  result := ObjectToJSON(self,[])<>fInitialJsonContent;
end;


{ TLogSettings }

constructor TLogSettings.Create;
begin
  inherited Create;
  fLevels := [low(TSynLogInfo)..high(TSynLogInfo)]; // "Levels":"*" by default
  fPerThread := ptIdentifiedInOnFile;
  fRotateFileAtHour := -1;
end;


{ TApplicationSettings }

constructor TApplicationSettingsFile.Create(
  const aSettingsJsonFileName: TFileName);
begin
  inherited Create;
  if aSettingsJsonFileName<>'' then
    fSettingsJsonFileName := aSettingsJsonFileName else
    fSettingsJsonFileName := ChangeFileExt(ExeVersion.ProgramFileName,'.settings');
  fSettingsJsonFileName := ExpandFileName(fSettingsJsonFileName);
  SetJsonContent(Pointer(AnyTextFileToRawUTF8(fSettingsJsonFileName,true)));
end;

procedure TApplicationSettingsFile.UpdateFile;
var new: RawUTF8;
begin
  if not WasModified then
    exit;
  new := ObjectToJSON(Self,[woHumanReadable,woStoreStoredFalse,
    woHumanReadableFullSetsAsStar,woHumanReadableEnumSetAsComment]);
  FileFromString(new,fSettingsJsonFileName);
end;

destructor TApplicationSettingsFile.Destroy;
begin
  UpdateFile;
  inherited Destroy;
end;

procedure TApplicationSettingsFile.Initialize(const aDescription: string);
begin
  with SQLite3Log.Family do begin
    Level := Log.Levels-[sllNone]; // '*' would include sllNone
    if Log.ConsoleLevels<>[] then
      EchoToConsole := Log.ConsoleLevels-[sllNone];
    PerThreadLog := Log.PerThread;
    if Log.DestinationPath<>'' then
     DestinationPath := Log.DestinationPath;
    RotateFileCount := Log.RotateFileCount;
    RotateFileSizeKB := Log.RotateFileSizeKB;
    RotateFileDailyAtHour := Log.RotateFileDailyAtHour;
  end;
  if fDescription='' then
    fDescription := aDescription;
  ChDir(ExtractFilePath(SettingsJsonFileName));
end;

function TApplicationSettingsFile.FileNameRelativeToSettingsFile(
  const aFileName: TFileName): TFileName;
var path,settings: TFileName;
begin
  path := ExtractFilePath(ExpandFileName(aFileName));
  settings := ExtractFilePath(ExpandFileName(SettingsJsonFileName));
  result := ExtractRelativePath(settings,path)+ExtractFileName(aFileName);
end;


{ TRestSettings }

function TRestSettings.NewRestInstance(aRootSettings: TApplicationSettingsFile;
  aModel: TSQLModel; aOptions: TNewRestInstanceOptions;
  aExternalDBOptions: TVirtualTableExternalRegisterOptions;
  aMongoDBOptions: TStaticMongoDBRegisterOptions): TSQLRest;
begin
  if aModel=nil then
     raise EDDDInfraException.CreateUTF8('%.NewRestInstance(aModel=nil)',[self]);
  if fRoot='' then // supplied TSQLModel.Root is the default root URI
    fRoot := aModel.Root else
    aModel.Root := fRoot;
  {$ifndef LINUX}
  if (fWrapperTemplateFolder='') and
     DirectoryExists('d:\dev\lib\CrossPlatform\Templates') then
    fWrapperTemplateFolder := 'd:/dev/lib/CrossPlatform/Templates';
  {$endif}
  if (fORM.Kind='') and (riDefaultLocalSQlite3IfNone in aOptions) then begin
    fORM.Kind := 'TSQLRestServerDB'; // SQlite3 engine by default
    if fORM.ServerName='' then
      fORM.ServerName := StringToUTF8(
        ChangeFileExt(ExtractFileName(ExeVersion.ProgramFileName),'.db'));
    if (aRootSettings<>nil) and (optStoreDBFileRelativeToSettings in Options) then
      fORM.ServerName := StringToUTF8(
        aRootSettings.FileNameRelativeToSettingsFile(UTF8ToString(fORM.ServerName)));
  end;
  result := nil;
  try
    if fORM.Kind='' then
      exit;
    if optEraseDBFileAtStartup in Options then
      if (fORM.Kind='TSQLRestServerDB') or
         (fORM.Kind='TSQLRestServerFullMemory') then
        DeleteFile(UTF8ToString(fORM.ServerName));
    result := TSQLRestMongoDBCreate(aModel,ORM,
      riHandleAuthentication in aOptions,aMongoDBOptions);
    if result=nil then // failed to use MongoDB -> try external or internal DB
      result := TSQLRestExternalDBCreate(aModel,ORM,
        riHandleAuthentication in aOptions,aExternalDBOptions);
    if result=nil then
      exit; // no match or wrong parameters
    if result.InheritsFrom(TSQLRestServer) then begin
      if (WrapperTemplateFolder<>'') and DirectoryExists(WrapperTemplateFolderFixed) then
        AddToServerWrapperMethod(TSQLRestServer(result),[WrapperTemplateFolderFixed],
          WrapperSourceFolderFixed);
      if result.InheritsFrom(TSQLRestServerDB) then
        with TSQLRestServerDB(result).DB do begin // tune internal SQlite3 engine
          LockingMode := lmExclusive;
          if optSQlite3FileSafeSlowMode in Options then
            Synchronous := smNormal else
            Synchronous := smOff;
        end;
      if riCreateMissingTables in aOptions then
        TSQLRestServer(result).CreateMissingTables;
    end;
  finally
    if riOwnModel in aOptions then
      if result=nil then // avoid memory leak
        aModel.Free else
        aModel.Owner := result;
  end;
end;

function TRestSettings.WrapperSourceFolderFixed: TFileName;
begin
  if fWrapperSourceFolders='' then
    result := '' else begin
    if fWrapperSourceFolderFixed='' then
      fWrapperSourceFolderFixed := StringReplace(fWrapperSourceFolders,'/','\',[rfReplaceAll]);
    result := fWrapperSourceFolders;
  end;
end;

function TRestSettings.WrapperTemplateFolderFixed: TFileName;
begin
  if fWrapperTemplateFolder='' then
    result := '' else begin
    if fWrapperTemplateFolderFixed='' then
      fWrapperTemplateFolderFixed := StringReplace(fWrapperTemplateFolder,'/','\',[rfReplaceAll]);
    result := fWrapperTemplateFolder;
  end;
end;


{ TApplicationSettingsRestFile }

procedure TApplicationSettingsRestFile.Initialize(const aDescription: string);
begin
  inherited Initialize(aDescription);
  if ServerPort='' then
    ServerPort := {$ifdef LINUX}'8888'{$else}'888'{$endif};
end;


{ TAdministratedDaemonSettings }

constructor TAdministratedDaemonSettings.Create;
begin
  inherited Create;
  AuthRootURI := 'admin';
  AuthHttp.PasswordPlain := RawUTF8(ClassName); // default WebSocketPassword
end;


{ TAdministratedDaemonSettingsFile }

procedure TAdministratedDaemonSettingsFile.Initialize(
  const aDescription,aServiceName,aServiceDisplayName: string);
begin
  inherited Initialize(aDescription);
  if FServiceName='' then
    FServiceName := aServiceName;
  if FServiceDisplayName='' then
    FServiceDisplayName := aServiceDisplayName;
end;


{ TAbstractDaemon }

constructor TAbstractDaemon.Create(aSettings: TAdministratedDaemonSettingsFile);
begin
  inherited Create;
  if aSettings=nil then
    raise EDDDInfraException.CreateUTF8('%.Create(settings=nil)',[self]);
  fSettings := aSettings;
end;

destructor TAbstractDaemon.Destroy;
begin
  inherited;
  fSettings.Free;
end;

{$ifdef MSWINDOWS} // to support Windows Services

procedure TAbstractDaemon.DoStart(Sender: TService);
begin
  fDaemon := NewDaemon;
  SQLite3Log.Enter(self);
  fDaemon.Start;
end;

procedure TAbstractDaemon.DoStop(Sender: TService);
begin
  SQLite3Log.Enter(self);
  fDaemon := nil; // will stop the daemon
end;

{$endif MSWINDOWS} // to support Windows Services

type
  TExecuteCommandLineCmd = (
    cNone,cInstall,cUninstall,cStart,cStop,cState,cVersion,cVerbose,
    cHelp,cConsole,cDaemon);

procedure TAbstractDaemon.ExecuteCommandLine;
var name,param: RawUTF8;
    cmd: TExecuteCommandLineCmd;
    daemon: TDDDAdministratedDaemon;
    {$ifdef MSWINDOWS}
    service: TServiceSingle;
    ctrl: TServiceController;
    {$endif}
{$I-} // no IO error for writeln() below
function cmdText: RawUTF8;
begin
  result := GetEnumNameTrimed(TypeInfo(TExecuteCommandLineCmd),cmd);
end;
procedure Show(Success: Boolean);
var msg: RawUTF8;
begin
  if Success then begin
    msg := 'Run';
    TextColor(ccWhite);
  end else begin
    msg := FormatUTF8('Error % "%" occured with',
      [GetLastError,SysErrorMessage(GetLastError)]);
    TextColor(ccLightRed);
  end;
  msg := FormatUTF8('% "%" (%) on Service "%"',
    [msg,param,cmdText,fSettings.ServiceName]);
  AppendToTextFile(msg,ChangeFileExt(ExeVersion.ProgramFileName,'.txt'));
  writeln(msg);
end;
procedure Syntax;
begin
  writeln('Try with one of the switches:');
  writeln(ExeVersion.ProgramName,' /console -c /verbose /daemon -d /help -h /version');
  {$ifdef MSWINDOWS}
  writeln(ExeVersion.ProgramName,' /install /uninstall /start /stop /state');
  {$endif}
end;
begin
  try
    if fSettings.ServiceDisplayName='' then begin
      fDaemon := NewDaemon; // should initialize the default .settings
      fDaemon := nil;
    end;
    TextColor(ccLightGreen);
    name := StringToUTF8(fSettings.ServiceDisplayName);
    if name='' then // perhaps the settings file is still void
      name := ExeVersion.ProgramName;
    writeln(#10' ',name);
    writeln(StringOfChar('-',length(name)+2));
    if fSettings.Description<>'' then begin
      TextColor(ccGreen);
      writeln(fSettings.Description);
    end;
    writeln;
    TextColor(ccLightCyan);
    param := trim(StringToUTF8(paramstr(1)));
    if (param='') or not(param[1] in ['/','-']) then
      cmd := cNone else
      case param[2] of
      'c','C': cmd := cConsole;
      'd','D': cmd := cDaemon;
      'h','H': cmd := cHelp;
      else byte(cmd) := 1+IdemPCharArray(@param[2],[
        'INST','UNINST','START','STOP','STAT','VERS','VERB']);
      end;
    case cmd of
    cHelp:
      Syntax;
    cVersion: begin
      if ExeVersion.Version.Version32<>0 then
        writeln(ExeVersion.ProgramName,' Version ',ExeVersion.Version.Detailed);
      TextColor(ccCyan);
      writeln('Powered by Synopse mORMot '+SYNOPSE_FRAMEWORK_VERSION);
    end;
    cConsole,cDaemon,cVerbose: begin
      writeln('Launched in ',cmdText,' mode'#10);
      TextColor(ccLightGray);
      if cmd=cVerbose then
        SQLite3Log.Family.EchoToConsole := LOG_VERBOSE else
        SQLite3Log.Family.EchoToConsole := LOG_STACKTRACE+[sllDDDInfo];
      daemon := NewDaemon;
      try
        fDaemon := daemon;
        if cmd=cDaemon then
          if (daemon.AdministrationServer=nil) or
             not ({$ifdef MSWINDOWS}
                   daemon.AdministrationServer.ExportedAsMessageOrNamedPipe or{$endif}
                  (daemon.InheritsFrom(TAbstractThreadDaemon) and
                   (TAbstractThreadDaemon(daemon).fAdministrationHTTPServer<>nil))) then
            daemon.LogClass.Add.Log(sllWarning,'ExecuteCommandLine as Daemon '+
              'without external admnistrator acccess',self);
        daemon.Execute(cmd=cDaemon);
      finally
        fDaemon := nil; // will stop the daemon
      end;
    end;
    else
    {$ifdef MSWINDOWS} // implement the daemon as a Windows Service
      with fSettings do
      if ServiceName='' then
        Syntax else
      case cmd of
      cNone:
        if param='' then begin // executed as a background service
          service := TServiceSingle.Create(ServiceName,ServiceDisplayName);
          try
            service.OnStart := DoStart;
            service.OnStop := DoStop;
            service.OnShutdown := DoStop; // sometimes, is called without Stop
            if ServicesRun then // blocking until service shutdown
              Show(true) else
              if GetLastError=1063 then
                Syntax else
                Show(false);
          finally
            service.Free;
          end;
        end else
          Syntax;
      cInstall:
        Show(TServiceController.Install(ServiceName,ServiceDisplayName,
          Description,ServiceAutoStart)<>ssNotInstalled);
      else begin
        ctrl := TServiceController.CreateOpenService('','',ServiceName);
        try
          case cmd of
          cStart:
            Show(ctrl.Start([]));
          cStop:
            Show(ctrl.Stop);
          cUninstall: begin
            ctrl.Stop;
            Show(ctrl.Delete);
          end;
          cState:
            writeln(ServiceName,' State=',ServiceStateText(ctrl.State));
          else Show(false);
          end;
        finally
          ctrl.Free;
        end;
      end;
      end;
    {$else}
      Syntax;
    {$endif MSWINDOWS}
    end;       
  except
    on E: Exception do
      ConsoleShowFatalException(E);
  end;
  TextColor(ccLightGray);
  ioresult;
end;
{$I+}


{ TAbstractThreadDaemon }

constructor TAbstractThreadDaemon.Create(
  aSettings: TAdministratedDaemonSettingsFile);
begin
  if aSettings=nil then
    raise EDDDInfraException.CreateUTF8('%.Create(settings=nil)',[self]);
  with aSettings.RemoteAdmin do begin
    inherited Create(AuthUserName,AuthHashedPassword,AuthRootURI,AuthNamedPipeName);
    fLogClass.Add.Log(sllTrace,'Create(%)',[aSettings],self);
    if AuthHttp.BindPort<>'' then
      fAdministrationHTTPServer := TSQLHttpServer.Create(fAdministrationServer,AuthHttp);
  end;
end;

destructor TAbstractThreadDaemon.Destroy;
begin
  FreeAndNil(fAdministrationHTTPServer);
  inherited;
end;


initialization
  {$ifdef EnableMemoryLeakReporting}
  {$ifdef HASFASTMM4} // FastMM4 integrated in Delphi 2006 (and up)
  ReportMemoryLeaksOnShutdown := True;
  {$endif}
  {$endif}
end.
