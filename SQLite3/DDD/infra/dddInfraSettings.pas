/// shared DDD Infrastructure: Application/Daemon Settings implementation
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit dddInfraSettings;

{
    This file is part of Synopse mORmot framework.

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
    /// compute a file name relative to the .settings file path
    function FileNameRelativeToSettingsFile(const aFileName: TFileName): TFileName;
    /// the .settings file name, including full path
    property SettingsJsonFileName: TFileName
      read fSettingsJsonFileName write fSettingsJsonFileName;
  end;

  /// storage class for initializing an ORM REST class 
  // - this class will contain some generic properties to initialize a TSQLRest
  // pointing to a local or remote SQL/NoSQL database, with optional wrappers
  TRestSettings = class(TSynAutoCreateFields)
  protected
    fDefinition: TSynConnectionDefinition;
    fRoot: RawUTF8;
    fLogLevels: TSynLogInfos;
    fWrapperTemplateFolder: TFileName;
  public
    /// is able to instantiate a REST instance according to the stored definition
    // - Definition.Kind will identify the TSQLRestServer or TSQLRestClient class
    // to be instantiated, or if equals 'MongoDB' use a full MongoDB store, or an
    // external SQL database if it matches a TSQLDBConnectionProperties classname
    // - will return nil if the supplied Definition is not correct
    // - note that the supplied Model.Root should have been set to the Root
    // property of this class, when created
    // - will also set the TSQLRest.LogFamily.Level from LogLevels value,
    // and publish the /wrapper HTML page if WrapperTemplateFolder is set
    function NewRestInstance(aModel: TSQLModel; aHandleAuthentication: boolean;
      aExternalDBOptions: TVirtualTableExternalRegisterOptions;
      aMongoDBOptions: TStaticMongoDBRegisterOptions): TSQLRest; virtual;
  published
    /// the URI Root to be used for the REST Model
    property Root: RawUTF8 read fRoot write fRoot;
    /// would let function NewRestInstance() create the expected TSQLRest
    property Definition: TSynConnectionDefinition read fDefinition;
    /// the log levels to be defined
    property LogLevels: TSynLogInfos read fLogLevels write fLogLevels;
    /// if set to a valid folder, the generated TSQLRest will publish a
    // '/Root/wrapper' HTML page so that client code could be generated
    property WrapperTemplateFolder: TFileName
      read fWrapperTemplateFolder write fWrapperTemplateFolder;
  end;

  /// parent class for storing REST-based application settings as a JSON file
  TApplicationSettingsRestFile = class(TApplicationSettingsFile)
  protected
    fRest: TRestSettings;
  published
    /// allow to instantiate a REST instance from its JSON definition
    property Rest: TRestSettings read fRest;
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

function TApplicationSettingsFile.FileNameRelativeToSettingsFile(
  const aFileName: TFileName): TFileName;
var path,settings: TFileName;
begin
  path := ExtractFilePath(ExpandFileName(aFileName));
  settings := ExtractFilePath(ExpandFileName(SettingsJsonFileName));
  result := ExtractRelativePath(settings,path)+ExtractFileName(aFileName);
end;


{ TRestSettings }

function TRestSettings.NewRestInstance(aModel: TSQLModel; aHandleAuthentication: boolean;
  aExternalDBOptions: TVirtualTableExternalRegisterOptions;
  aMongoDBOptions: TStaticMongoDBRegisterOptions): TSQLRest;
begin
  result := TSQLRestMongoDBCreate(aModel,Definition,aHandleAuthentication,aMongoDBOptions);
  if result=nil then // failed to use MongoDB -> try external or internal DB
    result := TSQLRestExternalDBCreate(aModel,Definition,aHandleAuthentication,aExternalDBOptions);
  if result=nil then
    exit; // no match or wrong parameters
  result.LogFamily.Level := LogLevels-[sllNone]; // '*' would include sllNone
  if result.InheritsFrom(TSQLRestServer) then
    if (WrapperTemplateFolder<>'') and DirectoryExists(WrapperTemplateFolder) then
      AddToServerWrapperMethod(TSQLRestServer(result),[WrapperTemplateFolder]);
end;

end.
