/// fill a VCL TClientDataset from SynDB data access
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynDBMidasVCL;

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
  SynCommons,
  SynDB, SynDBVCL,
  DB, DBClient;

/// fetch a SynDB TQuery result into a new VCL TClientDataSet
// - if aMaxRowCount>0, will return up to the specified number of rows
// - current implementation will return a TClientDataSet instance, created from
// the supplied TQuery content
// - for better speed with Delphi older than Delphi 2009 Update 3, it is
// recommended to use http://andy.jgknet.de/blog/bugfix-units/midas-speed-fix-12
// - if you need a read/only TDataSet, you should better not use this function
// but QueryToDataSet() as defined in SynDBVCL which is much faster and uses
// much less resources
function QueryToClientDataSet(aOwner: TComponent; aStatement: SynDB.TQuery;
  aMaxRowCount: integer=0): TClientDataSet; overload;

/// fetch a SynDB TSQLDBStatement result into a new VCL TClientDataSet
// - if aMaxRowCount>0, will return up to the specified number of rows
// - current implementation will return a TClientDataSet instance, created from
// the supplied TSQLDBStatement content
// - for better speed with Delphi older than Delphi 2009 Update 3, it is
// recommended to use http://andy.jgknet.de/blog/bugfix-units/midas-speed-fix-12
// - if you need a read/only TDataSet, you should better not use this function
// but StatementToDataSet() as defined in SynDBVCL which is much faster and uses
// much less resources
function StatementToClientDataSet(aOwner: TComponent; aStatement: TSQLDBStatement;
  aMaxRowCount: integer=0): TClientDataSet; overload;


type
  /// how QueryToClientDataSet/StatementToClientDataSet functions will
  // fill the TClientDataSet instance
  TClientDataSetMode = (cdsNew, cdsAppend, cdsReplace);


/// fetch a SynDB TQuery result into an existing VCL TClientDataSet
// - if aMaxRowCount>0, will return up to the specified number of rows
// - current implementation will fill an existing TClientDataSet instance, from
// the supplied TQuery content 
// - for better speed with Delphi older than Delphi 2009 Update 3, it is
// recommended to use http://andy.jgknet.de/blog/bugfix-units/midas-speed-fix-12
function QueryToClientDataSet(aDataSet: TClientDataSet; aStatement: SynDB.TQuery;
  aMaxRowCount: integer=0; aMode: TClientDataSetMode=cdsReplace; aLogChange: boolean=false): boolean; overload;

/// fetch a SynDB TSQLDBStatement result into an existing VCL TClientDataSet
// - if aMaxRowCount>0, will return up to the specified number of rows
// - current implementation will fill an existing TClientDataSet instance, from
// the supplied TSQLDBStatement content
// - for better speed with Delphi older than Delphi 2009 Update 3, it is
// recommended to use http://andy.jgknet.de/blog/bugfix-units/midas-speed-fix-12
function StatementToClientDataSet(aDataSet: TClientDataSet; aStatement: TSQLDBStatement;
  aMaxRowCount: integer=0; aMode: TClientDataSetMode=cdsReplace; aLogChange: boolean=false): boolean; overload;


implementation

var
  GlobalDataSetCount: integer;

function QueryToClientDataSet(aDataSet: TClientDataSet; aStatement: SynDB.TQuery;
  aMaxRowCount: integer; aMode: TClientDataSetMode; aLogChange: boolean): boolean;
begin
  if aStatement=nil then
    result := false else
    result := StatementToClientDataSet(aDataSet,
      aStatement.PreparedSQLDBStatement.Instance,aMaxRowCount);
end;

function QueryToClientDataSet(aOwner: TComponent; aStatement: SynDB.TQuery;
  aMaxRowCount: integer): TClientDataSet;
begin
  if aStatement=nil then
    result := nil else
    result := StatementToClientDataSet(aOwner,
      aStatement.PreparedSQLDBStatement.Instance,aMaxRowCount);
end;

function StatementToClientDataSet(aOwner: TComponent; aStatement: TSQLDBStatement;
  aMaxRowCount: integer): TClientDataSet;
begin
  result := TClientDataSet.Create(aOwner);
  try
    result.Name := 'SynDBDS'+IntToStr(GlobalDataSetCount); // unique name
    inc(GlobalDataSetCount);
    if aStatement=nil then
      exit;
    if not StatementToClientDataSet(result,aStatement,aMaxRowCount,cdsNew) then
      FreeAndNil(result);
  except
    on Exception do
      FreeAndNil(result);
  end;
end;

function StatementToClientDataSet(aDataSet: TClientDataSet; aStatement: TSQLDBStatement;
  aMaxRowCount: integer; aMode: TClientDataSetMode; aLogChange: boolean): boolean; overload;
var Source: TSynSQLStatementDataSet;
    Columns: array of record
      Field: TField;
      WasReadOnly: boolean;
      OnChange: TFieldNotifyEvent;
    end;
    Previous: record
      Active: Boolean;
      ReadOnly: Boolean;
      LogChanges: Boolean;
      AfterScroll: TDataSetNotifyEvent;
    end;
    row, f: integer;
    SourceStream,DestStream: TStream;
begin
  result := false;
  if (aDataSet=nil) or (aStatement=nil) then
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
  Source := TSynSQLStatementDataSet.Create(nil,aStatement,aMaxRowCount);
  try
    // load all data content into optimized in-memory buffer
    Source.Open;
    // handle columns
    SetLength(Columns,Source.DataAccess.ColumnCount);
    if aMode=cdsNew then begin
      for f := 0 to high(Columns) do
        with Source.Fields[f] do
          aDataSet.FieldDefs.Add(FieldName,DataType,Size);
      aDataSet.CreateDataSet;
      for f := 0 to high(Columns) do
        Columns[f].Field := aDataSet.FieldByName(Source.Fields[f].FieldName);
    end else
      for f := 0 to high(Columns) do
      with Columns[f] do begin
        Field := aDataSet.FieldByName(Source.Fields[f].FieldName);
        if Field.ReadOnly then begin
          WasReadOnly := true;
          Field.ReadOnly := false;
        end;
        OnChange := Field.OnChange;
        Field.OnChange := nil;
      end;
    // append data
    aDataSet.LogChanges := aLogChange;
    for row := 0 to Source.DataAccess.DataRowCount-1 do begin
      Source.DataAccess.GotoRow(row,true);
      aDataSet.Append;
      for f := 0 to high(Columns) do
        with Columns[f] do
        if Field<>nil then
        if Source.DataAccess.ColumnNull(f) then
          Field.Clear else
        case Field.DataType of
        ftBoolean:
          Field.AsBoolean := Source.DataAccess.ColumnInt(f)<>0;
        ftSmallint, ftInteger, ftWord:
          Field.AsInteger := Source.DataAccess.ColumnInt(f);
        ftLargeint:
          TLargeintField(Field).Value := Source.DataAccess.ColumnInt(f);
        ftFloat, ftCurrency:
          Field.AsFloat := Source.DataAccess.ColumnDouble(f);
        ftDate,ftDateTime,ftTime:
          Field.AsDateTime := Source.DataAccess.ColumnDateTime(f);
        ftString:
          Field.AsString := Source.DataAccess.ColumnString(f);
        ftWideString:
          TWideStringField(Field).Value := UTF8ToSynUnicode(Source.DataAccess.ColumnUTF8(f));
        {$ifdef ISDELPHI2007ANDUP}
        ftWideMemo,
        {$endif}
        ftMemo, ftBlob: begin
          SourceStream := Source.GetBlobStream(Source.Fields[f],row);
          if SourceStream=nil then
            Field.Clear else
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
        else raise EDatabaseError.CreateFmt('Invalid Source.ColumnType for %s)',
          [Field.FieldName]);
        end;
      aDataSet.Post;
    end;
    aDataSet.First;
    result := true;
  finally
    Source.Free;
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

