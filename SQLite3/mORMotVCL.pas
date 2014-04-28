/// DB VCL dataset using TSQLTable/TSQLTableJSON data access
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit mORmotVCL;

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

  Version 1.17
  - first public release, corresponding to Synopse mORMot Framework 1.17

  Version 1.18
  - renamed SQLite3VCL.pas to mORMotVCL.pas
  - fixed ticket [9de8be5d9e] with some types like TEnumeration or TTimeLog
  - fixed process with Unicode content
  - introduced new aForceWideString optional parameter for ticket [2970335e40]

}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  Windows,
  {$ifdef ISDELPHIXE2}System.SysUtils,{$else}SysUtils,{$endif}
  Classes,
  Contnrs,
  SynVirtualDataSet,
  SynCommons, mORMot,
  DB;


type
  /// read-only virtual TDataSet able to access a TSQLTable
  TSynSQLTableDataSet = class(TSynVirtualDataSet)
  protected
    fTable: TSQLTable;
    {$ifndef UNICODE}
    fForceWideString: boolean;
    {$endif}
    fTableShouldBeFreed: boolean;
    fTemp64: Int64;
    fTempBlob: TSQLRawBlob;
    procedure InternalInitFieldDefs; override;
    function GetRecordCount: Integer; override;
    function GetRowFieldData(Field: TField; RowIndex: integer;
      out ResultLen: Integer; OnlyCheckNull: boolean): Pointer; override;
  public
    /// initialize the virtual TDataSet from a TSQLTable
    // - WARNING: the supplied TSQLTable instance shall remain available
    // all the time the returned TSynSQLTableDataSet instance is used, unless
    // the TableShouldBeFreed property is set to true or CreateOwnedTable()
    // constructor is used instead
    // - with non-Unicode version of Delphi, you can set aForceWideString to
    // force the use of WideString fields instead of AnsiString, if needed
    // - the TDataSet will be opened once created
    constructor Create(Owner: TComponent; Table: TSQLTable
      {$ifndef UNICODE}; ForceWideString: boolean=false{$endif}); reintroduce;
    /// initialize the virtual TDataSet owning a TSQLTable
    // - this constructor will set TableShouldBeFreed to TRUE
    // - with non-Unicode version of Delphi, you can set aForceWideString to
    // force the use of WideString fields instead of AnsiString, if needed
    // - the TDataSet will be opened once created
    constructor CreateOwnedTable(Owner: TComponent; Table: TSQLTable
      {$ifndef UNICODE}; ForceWideString: boolean=false{$endif}); reintroduce;
    /// initialize the virtual TDataSet from a supplied JSON result
    // - this constructor will parse the supplied JSON content and create
    // an internal TSQLTableJSON instance to process the data
    // - with non-Unicode version of Delphi, you can set aForceWideString to
    // force the use of WideString fields instead of AnsiString, if needed
    // - the TDataSet will be opened once created
    constructor CreateFromJSON(Owner: TComponent; const JSON: RawUTF8
      {$ifndef UNICODE}; ForceWideString: boolean=false{$endif}); reintroduce; overload;
    /// initialize the virtual TDataSet from a supplied JSON result
    // - you can set the expected column types matching the results column layout
    // - this constructor will parse the supplied JSON content and create
    // an internal TSQLTableJSON instance to process the data 
    // - with non-Unicode version of Delphi, you can set aForceWideString to
    // force the use of WideString fields instead of AnsiString, if needed
    // - the TDataSet will be opened once created
    constructor CreateFromJSON(Owner: TComponent; const JSON: RawUTF8;
      const ColumnTypes: array of TSQLFieldType
      {$ifndef UNICODE}; ForceWideString: boolean=false{$endif}); reintroduce; overload;

    /// finalize the class instance
    destructor Destroy; override;
    
    /// if the supplied TSQLTable instance should be released with this class
    // - Create() will left to FALSE (meaning that the TSQLTable instance shall
    // remain available all the time the TSynSQLTableDataSet instance is used)
    // - CreateOwnedTable() will set to TRUE if you want the TSQLTable to be
    //  freed when this TSynSQLTableDataSet instance will be released
    // - you can also set it after Create(), on purpose
    property TableShouldBeFreed: boolean read fTableShouldBeFreed write fTableShouldBeFreed;
    /// read-only access to the internal TSQLTable[JSON] data
    // - you can use e.g. the SortFields() methods
    property Table: TSQLTable read fTable;
  end;


/// convert a JSON result into a VCL DataSet
// - this function is just a wrapper around TSynSQLTableDataSet.CreateFromJSON()
// - with non-Unicode version of Delphi, you can set aForceWideString to
// force the use of WideString fields instead of AnsiString, if needed
// - with Unicode version of Delphi (2009+), string/UnicodeString will be used
function JSONToDataSet(aOwner: TComponent; const aJSON: RawUTF8
  {$ifndef UNICODE}; aForceWideString: boolean=false{$endif}): TSynSQLTableDataSet; overload;
 {$ifdef HASINLINE}inline;{$endif}

/// convert a JSON result into a VCL DataSet
// - this function is just a wrapper around TSynSQLTableDataSet.CreateFromJSON()
// - with non-Unicode version of Delphi, you can set aForceWideString to
// force the use of WideString fields instead of AnsiString, if needed
// - with Unicode version of Delphi (2009+), string/UnicodeString will be used
function JSONToDataSet(aOwner: TComponent; const aJSON: RawUTF8;
  const ColumnTypes: array of TSQLFieldType
  {$ifndef UNICODE}; aForceWideString: boolean=false{$endif}): TSynSQLTableDataSet; overload;


implementation

function JSONToDataSet(aOwner: TComponent; const aJSON: RawUTF8
  {$ifndef UNICODE}; aForceWideString: boolean{$endif}): TSynSQLTableDataSet;
begin
  result := TSynSQLTableDataSet.CreateFromJSON(
    aOwner,aJSON{$ifndef UNICODE},aForceWideString{$endif});
end;

function JSONToDataSet(aOwner: TComponent; const aJSON: RawUTF8;
  const ColumnTypes: array of TSQLFieldType
  {$ifndef UNICODE}; aForceWideString: boolean{$endif}): TSynSQLTableDataSet;
begin
  result := TSynSQLTableDataSet.CreateFromJSON(
    aOwner,aJSON,ColumnTypes{$ifndef UNICODE},aForceWideString{$endif});
end;


{ TSynSQLTableDataSet }

constructor TSynSQLTableDataSet.Create(Owner: TComponent; Table: TSQLTable
  {$ifndef UNICODE}; ForceWideString: boolean{$endif});
begin
  inherited Create(Owner);
  {$ifndef UNICODE}
  fForceWideString := ForceWideString;
  {$endif}
  if Table<>nil then
    fTable := Table;
  Open;
end;

constructor TSynSQLTableDataSet.CreateOwnedTable(Owner: TComponent; Table: TSQLTable
  {$ifndef UNICODE}; ForceWideString: boolean{$endif});
begin
  Create(Owner,Table{$ifndef UNICODE},ForceWideString{$endif});
  if Table<>nil then
    TableShouldBeFreed := true;
end;

constructor TSynSQLTableDataSet.CreateFromJSON(Owner: TComponent; const JSON: RawUTF8
  {$ifndef UNICODE}; ForceWideString: boolean=false{$endif});
var T: TSQLTable;
begin
  T := TSQLTableJSON.Create('',JSON);
  try
    CreateOwnedTable(Owner,T{$ifndef UNICODE},ForceWideString{$endif});
    T := nil;
  finally
    T.Free; // release temporary instance in case of TSynSQLTableDataSet error 
  end;
end;

constructor TSynSQLTableDataSet.CreateFromJSON(Owner: TComponent; const JSON: RawUTF8;
  const ColumnTypes: array of TSQLFieldType
  {$ifndef UNICODE}; ForceWideString: boolean=false{$endif});
var T: TSQLTable;
begin
  T := TSQLTableJSON.CreateWithColumnTypes(ColumnTypes,'',JSON);
  try
    CreateOwnedTable(Owner,T{$ifndef UNICODE},ForceWideString{$endif});
    T := nil;
  finally
    T.Free; // release temporary instance in case of TSynSQLTableDataSet error 
  end;
end;

destructor TSynSQLTableDataSet.Destroy;
begin
  inherited;
  if fTableShouldBeFreed then
    FreeAndNil(fTable);
end;

function TSynSQLTableDataSet.GetRecordCount: Integer;
begin
  if fTable<>nil then
    result := fTable.RowCount else
    result := 0;
end;

function TSynSQLTableDataSet.GetRowFieldData(Field: TField; RowIndex: integer;
  out ResultLen: Integer; OnlyCheckNull: boolean): Pointer;
var SQLType: TSQLFieldType;
    EnumType: Pointer;
    F: integer;
    P: PUTF8Char;
label Txt;
begin
  result := nil;
  F := Field.Index;
  inc(RowIndex);
  if (cardinal(RowIndex)>cardinal(fTable.RowCount)) or
     (cardinal(F)>=cardinal(fTable.FieldCount)) then
    exit;
  P := fTable.Get(RowIndex,F);
  if P=nil then // null field -> result := nil
    exit;
  result := @fTemp64; // result should point to Int64,Double,Blob,UTF8 data
  if OnlyCheckNull then
    exit;
  SQLType := fTable.FieldType(F,@EnumType);
  case SQLType of
  sftBoolean, sftInteger, sftID:
    fTemp64 := GetInt64(P);
  sftFloat, sftCurrency:
    PDouble(@fTemp64)^ := GetExtended(P);
  sftEnumerate, sftSet:
    if EnumType=nil then
      fTemp64 := GetInt64(P) else
      goto Txt;
  sftDateTime:
    PDouble(@fTemp64)^ := Iso8601ToDateTimePUTF8Char(P,0);
  sftTimeLog, sftModTime, sftCreateTime:
    PDouble(@fTemp64)^ := TimeLogToDateTime(GetInt64(P));
  sftBlob: begin
    fTempBlob := BlobToTSQLRawBlob(P);
    result := pointer(fTempBlob);
    resultLen := length(fTempBlob);
  end;
  else begin // e.g. sftUTF8Text
Txt:result := P;
    resultLen := StrLen(P);
  end;
  end;
end;

procedure TSynSQLTableDataSet.InternalInitFieldDefs;
var F, DataSize: Integer;
    SQLType: TSQLFieldType;
    DBType: TFieldType;
    EnumType: Pointer;
begin
  FieldDefs.Clear;
  for F := 0 to fTable.FieldCount-1 do begin
    SQLType := fTable.FieldType(F,@EnumType);
    DataSize := 0;
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
        DBType := ftInteger else begin
        DataSize := 64;
        DBType := ftDefaultVCLString;
      end;
    sftRecord: begin
        DataSize := 64;
        DBType := ftDefaultVCLString;
      end;
    sftDateTime, sftTimeLog, sftModTime, sftCreateTime:
      DBType := ftDateTime;
    sftBlob: begin
        DataSize := (fTable.FieldLengthMax(F,true)*3) shr 2;
        DBType := ftBlob;
      end;
    sftUTF8Text: begin
      DataSize := fTable.FieldLengthMax(F,true);
      {$ifndef UNICODE} // for Delphi 2009+ TWideStringField = UnicodeString!
      if fForceWideString then
        DBType := ftWideString else
      {$endif}
        DBType := ftDefaultVCLString;
    end;
    else begin
      DBType := ftDefaultVCLString;
      DataSize := fTable.FieldLengthMax(F,true);
    end;
    end;
    FieldDefs.Add(fTable.GetString(0,F),DBType,DataSize);
  end;
end;


end.

