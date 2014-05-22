/// fill a VCL TClientDataset from TSQLTable/TSQLTableJSON data
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit mORMotMidasVCL;

{
    This file is part of Synopse mORmot framework.

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
  - mingda

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
  - first public release, corresponding to Synopse mORMot Framework 1.18,
    which is an extraction from former SynDBVCL.pas unit (which is faster
    but read/only)


}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  Windows,
  {$ifdef ISDELPHIXE2}System.SysUtils,{$else}SysUtils,{$endif}
  Classes,
  Contnrs,
{$ifndef DELPHI5OROLDER}
  Variants,
  MidasLib,
{$endif}
  SynCommons, mORmot,
  DB, DBClient;

/// convert a TSQLTable result into a new VCL TClientDataSet
// - current implementation will return a TClientDataSet instance, created from
// the supplied TSQLTable content (a more optimized version may appear later)
// - with non-Unicode version of Delphi, you can set aForceWideString to
// force the use of WideString fields instead of AnsiString, if needed
// - for better speed with Delphi older than Delphi 2009 Update 3, it is
// recommended to use http://andy.jgknet.de/blog/bugfix-units/midas-speed-fix-12
function TSQLTableToClientDataSet(aOwner: TComponent; aTable: TSQLTable; aClient: TSQLRest=nil
  {$ifndef UNICODE}; aForceWideString: boolean=false{$endif}): TClientDataSet; overload;

/// convert a JSON result into a new VCL TClientDataSet
// - current implementation will return a TClientDataSet instance, created from
// the supplied TSQLTable content (a more optimized version may appear later)
// - with non-Unicode version of Delphi, you can set aForceWideString to
// force the use of WideString fields instead of AnsiString, if needed
// - with Unicode version of Delphi (2009+), UnicodeString will be used
// - for better speed with Delphi older than Delphi 2009 Update 3, it is
// recommended to use http://andy.jgknet.de/blog/bugfix-units/midas-speed-fix-12
function JSONToClientDataSet(aOwner: TComponent; const aJSON: RawUTF8; aClient: TSQLRest=nil
  {$ifndef UNICODE}; aForceWideString: boolean=false{$endif}): TClientDataSet; overload;



type
  /// how TSQLTableToClientDataSet/JSONToClientDataSet functions will fill
  // the TClientDataSet instance
  TClientDataSetMode = (cdsNew, cdsAppend, cdsReplace);

/// convert a TSQLTable result into an existing VCL TClientDataSet
// - current implementation will return a TClientDataSet instance, created from
// the supplied TSQLTable content (a more optimized version may appear later)
// - with non-Unicode version of Delphi, you can set aForceWideString to
// force the use of WideString fields instead of AnsiString, if needed
// - for better speed with Delphi older than Delphi 2009 Update 3, it is
// recommended to use http://andy.jgknet.de/blog/bugfix-units/midas-speed-fix-12
function TSQLTableToClientDataSet(aDataSet: TClientDataSet; aTable: TSQLTable; aClient: TSQLRest=nil;
  aMode: TClientDataSetMode=cdsReplace; aLogChange: boolean=false
  {$ifndef UNICODE}; aForceWideString: boolean=false{$endif}): boolean; overload;

/// convert a JSON result into an existing VCL TClientDataSet
// - current implementation will return a TClientDataSet instance, created from
// the supplied TSQLTable content (a more optimized version may appear later)
// - with non-Unicode version of Delphi, you can set aForceWideString to
// force the use of WideString fields instead of AnsiString, if needed
// - with Unicode version of Delphi (2009+), UnicodeString will be used
// - for better speed with Delphi older than Delphi 2009 Update 3, it is
// recommended to use http://andy.jgknet.de/blog/bugfix-units/midas-speed-fix-12
function JSONToClientDataSet(aDataSet: TClientDataSet; const aJSON: RawUTF8; aClient: TSQLRest=nil;
  aMode: TClientDataSetMode=cdsReplace; aLogChange: boolean=false
  {$ifndef UNICODE}; aForceWideString: boolean=false{$endif}): boolean; overload;


implementation

function JSONToClientDataSet(aDataSet: TClientDataSet; const aJSON: RawUTF8; aClient: TSQLRest=nil;
  aMode: TClientDataSetMode=cdsReplace; aLogChange: boolean=false
  {$ifndef UNICODE}; aForceWideString: boolean=false{$endif}): boolean; overload;
var T: TSQLTableJSON;
begin
  T := TSQLTableJSON.Create('',aJSON);
  try
    result := TSQLTableToClientDataSet(aDataSet,T,aClient,aMode,aLogChange
      {$ifndef UNICODE},aForceWideString{$endif});
  finally
    T.Free;
  end;
end;

function JSONToClientDataSet(aOwner: TComponent; const aJSON: RawUTF8; aClient: TSQLRest
  {$ifndef UNICODE}; aForceWideString: boolean{$endif}): TClientDataSet;
var T: TSQLTableJSON;
begin
  T := TSQLTableJSON.Create('',aJSON);
  try
    result := TSQLTableToClientDataSet(aOwner,T,aClient
      {$ifndef UNICODE},aForceWideString{$endif});
  finally
    T.Free;
  end;
end;

var
  GlobalDataSetCount: integer;

function TSQLTableToClientDataSet(aOwner: TComponent; aTable: TSQLTable; aClient: TSQLRest
  {$ifndef UNICODE}; aForceWideString: boolean{$endif}): TClientDataSet;
begin
  result := TClientDataSet.Create(aOwner);
  try
    result.Name := 'mORMotDS'+IntToStr(GlobalDataSetCount); // unique name
    inc(GlobalDataSetCount);
    if aTable=nil then
      exit;
    if not TSQLTableToClientDataSet(result,aTable,aClient,cdsNew,false
      {$ifndef UNICODE}, aForceWideString{$endif}) then
      FreeAndNil(result);
  except
    on Exception do
      FreeAndNil(result);
  end;
end;

function TSQLTableToClientDataSet(aDataSet: TClientDataSet; aTable: TSQLTable; aClient: TSQLRest=nil;
  aMode: TClientDataSetMode=cdsReplace; aLogChange: boolean=false
  {$ifndef UNICODE}; aForceWideString: boolean=false{$endif}): boolean; overload;
var F,i: integer;
    fDefaultStringType: TFieldType;
    Columns: array of record
      FieldName: string;
      Field: TField;
      DBType: TFieldType;
      DBSize: integer;
      SQLType: TSQLFieldType;
      EnumType: Pointer;
      WasReadOnly: boolean;
      OnChange: TFieldNotifyEvent;
    end;
    Previous: record
      Active: Boolean;
      ReadOnly: Boolean;
      LogChanges: Boolean;
      AfterScroll: TDataSetNotifyEvent;
    end;
    SourceStream,DestStream: TStream;
    blob: TSQLRawBlob;
begin
  result := false;
  if (aDataSet=nil) or (aTable=nil) then
    exit;
  fillchar(Previous,sizeof(Previous),0);
  if aDataSet.Active then begin
    Previous.Active := true;
    Previous.LogChanges := aDataSet.LogChanges;
    Previous.ReadOnly := aDataSet.ReadOnly;
    Previous.AfterScroll := aDataSet.AfterScroll;
    aDataSet.AfterScroll := nil;
    aDataSet.ReadOnly := false;
    aDataSet.DisableControls;
  end;
  if aMode=cdsReplace then begin
    if Previous.LogChanges then
      aDataSet.LogChanges := false;
    aDataSet.EmptyDataSet;
  end;
  // handle columns
  {$ifndef UNICODE}
  if not aForceWideString then
    fDefaultStringType := ftString else
  {$endif}
    fDefaultStringType := ftWideString; // means UnicodeString for Delphi 2009+
  SetLength(Columns,aTable.FieldCount);
  for F := 0 to aTable.FieldCount-1 do begin
    with Columns[F] do begin
      FieldName := aTable.GetString(0,F);
      SQLType := aTable.FieldType(F,@EnumType);
      case SQLType of
      sftBoolean:
        DBType := ftBoolean;
      sftInteger:
        DBType := ftLargeint; // LargeInt=Int64
      sftFloat, sftCurrency:
        DBType := ftFloat;
      sftID:
        DBType := ftInteger;
      sftEnumerate, sftSet:
        if EnumType=nil then
          DBType := ftInteger else
          DBType := fDefaultStringType;
      sftRecord:
        DBType := fDefaultStringType;
      sftDateTime, sftTimeLog, sftModTime, sftCreateTime:
        DBType := ftDateTime;
      sftBlob: begin
        DBType := ftBlob;
        DBSize := (aTable.FieldLengthMax(F,true)*3) shr 2;
      end;
      sftUTF8Text: begin
        DBSize := aTable.FieldLengthMax(F,true);
        DBType := fDefaultStringType;
      end else
        DBType := fDefaultStringType;
      end;
      if (DBSize=0) and (DBType=fDefaultStringType) then
        DBSize := 64;
    end;
  end;
  if aMode=cdsNew then begin
    for f := 0 to high(Columns) do
      with Columns[f] do
        aDataSet.FieldDefs.Add(FieldName,DBType,DBSize);
    aDataSet.CreateDataSet;
    for f := 0 to high(Columns) do
      Columns[f].Field := aDataSet.FieldByName(Columns[f].FieldName);
  end else
    for f := 0 to high(Columns) do
    with Columns[f] do begin
      Field := aDataSet.FieldByName(Columns[f].FieldName);
      if Field.ReadOnly then begin
        WasReadOnly := true;
        Field.ReadOnly := false;
      end;
      OnChange := Field.OnChange;
      Field.OnChange := nil;
    end;
  // append data
  try
    aDataSet.LogChanges := aLogChange;
    for i := 1 to aTable.RowCount do begin
      aDataSet.Append;
      for F := 0 to high(Columns) do
      with Columns[F] do
      if Field<>nil then
      if aTable.Get(i,F)=nil then
        Field.Clear else
      case Columns[F].SQLType of
      sftBoolean:
        Field.AsBoolean := aTable.GetAsInteger(i,F)<>0;
      sftInteger:
        if Field.DataType=ftLargeInt then // handle Int64 values directly
          TLargeintField(Field).Value := aTable.GetAsInt64(i,F) else
          Field.AsInteger := aTable.GetAsInteger(i,F);
      sftFloat, sftCurrency:
        Field.AsFloat := GetExtended(aTable.Get(i,F));
      sftID:
        Field.AsInteger := aTable.GetAsInteger(i,F);
      sftEnumerate, sftSet:
        if EnumType=nil then
          Field.AsInteger := aTable.GetAsInteger(i,F) else
          Field.AsString := aTable.GetString(i,F);
      sftDateTime:
        Field.AsDateTime := Iso8601ToDateTimePUTF8Char(aTable.Get(i,F),0);
      sftTimeLog, sftModTime, sftCreateTime:
        Field.AsDateTime := TimeLogToDateTime(aTable.GetAsInt64(i,F));
      sftBlob: begin
        blob := aTable.GetBlob(i,F);
        if blob='' then
          Field.Clear else begin
          SourceStream := TRawByteStringStream.Create(blob);
          try
            DestStream := aDataSet.CreateBlobStream(Field,bmWrite);
            try
              DestStream.CopyFrom(SourceStream,0);
            finally
              DestStream.Free;
            end;
          finally
            SourceStream.Free;
          end;
        end;
      end;
      sftUTF8Text:
        if Field.DataType=ftWideString then
          TWideStringField(Field).Value := aTable.GetSynUnicode(i,F) else
          Field.AsString := aTable.GetString(i,F);
      else
        Field.AsVariant := aTable.GetVariant(i,F,aClient);
      end;
      aDataSet.Post;
    end;
    aDataSet.First;
    result := True;
  finally
    if Previous.Active then begin
      aDataSet.LogChanges := Previous.LogChanges;
      aDataSet.ReadOnly := Previous.ReadOnly;
      aDataSet.AfterScroll := Previous.AfterScroll;
      if Assigned(Previous.AfterScroll) then
        Previous.AfterScroll(aDataSet);
      aDataSet.EnableControls;
    end;
    if aMode<>cdsNew then begin
      for f := 0 to high(Columns) do
        with Columns[f] do
        if Field<>nil then begin
          Field.ReadOnly := WasReadOnly;
          Field.OnChange := OnChange;
        end;
    end;
  end;
end;

end.