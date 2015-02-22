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
  mORMotDDD;

  
{ ----- Manage Service/Daemon settings }

type
  /// abstract class for storing application settings
  TApplicationSettingsAbstract = class(TSynAutoCreateFields);

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



implementation


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

end.