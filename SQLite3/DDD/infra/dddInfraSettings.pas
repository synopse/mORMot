/// shared DDD Infrastructure: Application/Daemon Settings implementation
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

}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  SysUtils,
  Classes,
  SynCommons,
  SynLog,
  mORMot,
  mORMotDDD,
  SynDB, mORMotDB, // for TRestSettings on external SQL database
  mORMotMongoDB,   // for TRestSettings on external NoSQL database
  mORMotWrappers;  // for TRestSettings to publish wrapper methods

  
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
    procedure SetProperties(Instance: TObject); virtual;
  end;

  /// parent class for storing application settings as a JSON file
  TApplicationSettingsFile = class(TApplicationSettingsAbstract)
  protected
    fSettingsJsonFileName: TFileName;
  public
    /// initialize and read the settings from the supplied JSON file name
    // - if no file name is specified, will use the executable name with
    // '.settings' as extension
    constructor Create(const aSettingsJsonFileName: TFileName=''); reintroduce; virtual;
    /// save to file and finalize the settings
    // - any enumerated or set published property will be commented with their values
    destructor Destroy; override;
    /// to be called when the application starts, to access settings
    // - will change the system current directory to SettingsJsonFileName
    procedure Initialize; virtual;
    /// compute a file name relative to the .settings file path
    function FileNameRelativeToSettingsFile(const aFileName: TFileName): TFileName;
    /// the .settings file name, including full path
    property SettingsJsonFileName: TFileName
      read fSettingsJsonFileName write fSettingsJsonFileName;
  end;

  /// some options to be used for TRestSettings
  TRestSettingsOption =
    (optEraseDBFileAtStartup,optStoreDBFileRelativeToSettings);

  /// define options to be used for TRestSettings
  TRestSettingsOptions = set of TRestSettingsOption;

  /// storage class for initializing an ORM REST class
  // - this class will contain some generic properties to initialize a TSQLRest
  // pointing to a local or remote SQL/NoSQL database, with optional wrappers
  TRestSettings = class(TSynAutoCreateFields)
  protected
    fORM: TSynConnectionDefinition;
    fRoot: RawUTF8;
    fLogLevels: TSynLogInfos;
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
    // - will return nil if the supplied Definition is not correct
    // - note that the supplied Model.Root is expected to be the default root
    // URI, which will be overriden with this TRestSettings.Root property
    // - will also set the TSQLRest.LogFamily.Level from LogLevels value,
    // and publish the /wrapper HTML page if WrapperTemplateFolder is set
    function NewRestInstance(aRootSettings: TApplicationSettingsFile;
      aModel: TSQLModel; aHandleAuthentication: boolean;
      aExternalDBOptions: TVirtualTableExternalRegisterOptions;
      aMongoDBOptions: TStaticMongoDBRegisterOptions): TSQLRest; virtual;
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
    /// the log levels to be defined
    property LogLevels: TSynLogInfos read fLogLevels write fLogLevels;
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
  TApplicationSettingsRestFile = class(TApplicationSettingsFile)
  protected
    fRest: TRestSettings;
    fServerPort: word;
  public
    /// to be called when the application starts, to access settings
    // - will call inherited TApplicationSettingsFile.Initialize, and
    // set ServerPort to a default 888/8888 value under Windows/Linux
    procedure Initialize; override;
  published
    /// allow to instantiate a REST instance from its JSON definition
    property Rest: TRestSettings read fRest;
    /// the IP port to be used for the HTTP server associated with the application
    property ServerPort: word read fServerPort write fServerPort;
  end;



implementation


{ TApplicationSettingsAbstract }

procedure TApplicationSettingsAbstract.SetProperties(Instance: TObject);
begin
  CopyObject(self,Instance);
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
  JSONFileToObject(fSettingsJsonFileName,self);
end;

destructor TApplicationSettingsFile.Destroy;
begin
  ObjectToJSONFile(Self,fSettingsJsonFileName,[woHumanReadable,
    woHumanReadableFullSetsAsStar, woHumanReadableEnumSetAsComment]);
  inherited;
end;

procedure TApplicationSettingsFile.Initialize;
begin
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
  aModel: TSQLModel; aHandleAuthentication: boolean;
  aExternalDBOptions: TVirtualTableExternalRegisterOptions;
  aMongoDBOptions: TStaticMongoDBRegisterOptions): TSQLRest;
begin
  if fRoot='' then // supplied TSQLModel.Root is the default root URI
    fRoot := aModel.Root else
    aModel.Root := fRoot;
  {$ifndef LINUX}
  if (fWrapperTemplateFolder='') and
     DirectoryExists('d:\dev\lib\CrossPlatform\Templates') then
    fWrapperTemplateFolder := 'd:/dev/lib/CrossPlatform/Templates';
  {$endif}
  if fORM.Kind='' then begin
    fORM.Kind := 'TSQLRestServerDB'; // SQlite3 engine by default
    fORM.ServerName := StringToUTF8(
      ChangeFileExt(ExtractFileName(ExeVersion.ProgramFileName),'.db'));
    if (aRootSettings<>nil) and (optStoreDBFileRelativeToSettings in Options) then
      fORM.ServerName := StringToUTF8(
        aRootSettings.FileNameRelativeToSettingsFile(UTF8ToString(fORM.ServerName)));
  end;
  if optEraseDBFileAtStartup in Options then
    if (fORM.Kind='TSQLRestServerDB') or
       (fORM.Kind='TSQLRestServerFullMemory') then
      DeleteFile(UTF8ToString(fORM.ServerName));
  result := TSQLRestMongoDBCreate(aModel,ORM,aHandleAuthentication,aMongoDBOptions);
  if result=nil then // failed to use MongoDB -> try external or internal DB
    result := TSQLRestExternalDBCreate(aModel,ORM,aHandleAuthentication,aExternalDBOptions);
  if result=nil then
    exit; // no match or wrong parameters
  result.LogFamily.Level := LogLevels-[sllNone]; // '*' would include sllNone
  if result.InheritsFrom(TSQLRestServer) then
    if (WrapperTemplateFolder<>'') and DirectoryExists(WrapperTemplateFolderFixed) then
      AddToServerWrapperMethod(TSQLRestServer(result),[WrapperTemplateFolderFixed],
        WrapperSourceFolderFixed);
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

procedure TApplicationSettingsRestFile.Initialize;
begin
  inherited Initialize;
  if ServerPort=0 then
    ServerPort := {$ifdef LINUX}8888{$else}888{$endif};
end;

end.
