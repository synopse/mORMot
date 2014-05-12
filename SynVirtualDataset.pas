/// DB VCL read-only virtual dataset
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynVirtualDataSet;

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
  - first public release, corresponding to Synopse mORMot Framework 1.18


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
{$endif}
  SynCommons,
  DB,
  Forms;


type
  {$ifndef UNICODE} // defined as TRecordBuffer = PByte in newer DB.pas
  TRecordBuffer = PChar;
  {$endif}

  /// read-only virtual TDataSet able to access any content
  TSynVirtualDataSet = class(TDataSet)
  protected
    fCurrentRow: integer;
    fIsCursorOpen: boolean;

    // TDataSet overridden methods
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    function GetCanModify: Boolean; override;
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    procedure InternalClose; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalHandleException; override;
    procedure InternalLast; override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    function IsCursorOpen: Boolean; override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    procedure SetRecNo(Value: Integer); override;
    function GetRecNo: Integer; override;

    // classses should override all those following methods:
    // - to read the data e.g. into memory:
    procedure InternalOpen; override;
    // - to initialize FieldDefs:
    // procedure InternalInitFieldDefs; override;
    // - to return row count:
    // function GetRecordCount: Integer; override;
    // - result should point to Int64,Double,Blob,UTF8 data (if ResultLen<>nil)
    function GetRowFieldData(Field: TField; RowIndex: integer; out ResultLen: Integer;
      OnlyCheckNull: boolean): Pointer; virtual; abstract;
  public
    /// this overridden constructor will compute an unique Name property
    constructor Create(Owner: TComponent); override;
    /// get BLOB column data for the current active row
    // - handle ftBlob,ftMemo,ftWideMemo via GetRowFieldData()
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    /// get BLOB column data for a given row (may not the active row)
    // - handle ftBlob,ftMemo,ftWideMemo via GetRowFieldData()
    function GetBlobStream(Field: TField; RowIndex: integer): TStream;
    /// get column data for the current active row
    // - handle ftBoolean,ftInteger,ftLargeint,ftFloat,ftDate,ftTime,ftDateTime,
    // ftString,ftWideString kind of fields via GetRowFieldData()
    {$ifdef ISDELPHIXE3}
    {$ifdef ISDELPHIXE4}
    function GetFieldData(Field: TField; var Buffer: TValueBuffer): Boolean; override;
    {$else}
    function GetFieldData(Field: TField; Buffer: TValueBuffer): Boolean; override;
    {$endif}
    {$else}
    function GetFieldData(Field: TField; Buffer: pointer): Boolean; override;
    {$endif}
    {$ifndef UNICODE}
    function GetFieldData(Field: TField; Buffer: pointer; NativeFormat: Boolean): Boolean; override;
    {$endif}
  published
    property Active;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  end;

const
  /// map the VCL string type, depending on the Delphi compiler version
  {$ifdef UNICODE}
  ftDefaultVCLString = ftWideString;
  {$else}
  ftDefaultVCLString = ftString;
  {$endif}


/// export all rows of a TDataSet into JSON
// - will work for any kind of TDataSet
function DataSetToJSON(Data: TDataSet): RawUTF8;


implementation

var
  GlobalDataSetCount: integer;

type
  /// define how a single row is identified 
  // - for TSynVirtualDataSet, it is just the row index (starting at 0)
  TRecInfoIdentifier = integer;

  PRecInfoIdentifier = ^TRecInfoIdentifier;

  /// pointer to an internal structure used to identify a row position
  PRecInfo = ^TRecInfo;

  /// internal structure used to identify a row position
  TRecInfo = record
    /// define how a single row is identified
    RowIndentifier: TRecInfoIdentifier;
    /// any associated bookmark
    Bookmark: TRecInfoIdentifier;
    /// any associated bookmark flag
    BookmarkFlag: TBookmarkFlag;
  end;


{ TSynVirtualDataSet }

function TSynVirtualDataSet.AllocRecordBuffer: TRecordBuffer;
begin
  result := AllocMem(sizeof(TRecInfo));
end;

procedure TSynVirtualDataSet.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  FreeMem(Buffer);
  Buffer := nil;
end;

procedure TSynVirtualDataSet.GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  PRecInfoIdentifier(Data)^ := PRecInfo(Buffer)^.Bookmark;
end;

function TSynVirtualDataSet.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  result := PRecInfo(Buffer)^.BookmarkFlag;
end;

function TSynVirtualDataSet.GetCanModify: Boolean;
begin
  result := false; // we define a READ-ONLY TDataSet
end;

procedure DateTimeToNative(DataType: TFieldType; Data: TDateTime; var Output);
  {$ifdef HASINLINE}inline;{$endif}
var TimeStamp: TTimeStamp;
    result: TDateTimeRec absolute Output;
begin
  TimeStamp := DateTimeToTimeStamp(Data);
  case DataType of
    ftDate: result.Date := TimeStamp.Date;
    ftTime: result.Time := TimeStamp.Time;
    else    result.DateTime := TimeStampToMSecs(TimeStamp);
  end;
end;

{$ifndef UNICODE}
function TSynVirtualDataSet.GetFieldData(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean): Boolean;
begin
  if Field.DataType in [ftWideString] then
    NativeFormat := true; // to force Buffer as PWideString
  Result := inherited GetFieldData(Field, Buffer, NativeFormat);
end;
{$endif}

{$ifdef ISDELPHIXE3}
{$ifdef ISDELPHIXE4}
function TSynVirtualDataSet.GetFieldData(Field: TField; var Buffer: TValueBuffer): Boolean;
{$else}
function TSynVirtualDataSet.GetFieldData(Field: TField; Buffer: TValueBuffer): Boolean;
{$endif}
{$else}
function TSynVirtualDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
{$endif}
var Data, Dest: pointer;
    RowIndex, DataLen, MaxLen: integer;
    Temp: RawByteString;
    OnlyTestForNull: boolean;
begin
  result := false;
  OnlyTestForNull := (Buffer=nil);
  RowIndex := PRecInfo(ActiveBuffer).RowIndentifier;
  Data := GetRowFieldData(Field,RowIndex,DataLen,OnlyTestForNull);
  if Data=nil then // on success, points to Int64,Double,Blob,UTF8
    exit;
  result := true;
  if OnlyTestForNull then
    exit;
  Dest := pointer(Buffer); // works also if Buffer is [var] TValueBuffer
  case Field.DataType of
  ftBoolean:
    PWORDBOOL(Dest)^ := PBoolean(Data)^;
  ftInteger:
    PInteger(Dest)^ := PInteger(Data)^;
  ftLargeint, ftFloat:
    PInt64(Dest)^ := PInt64(Data)^;
  ftDate,ftTime,ftDateTime:
    DateTimeToNative(Field.DataType,PDateTime(Data)^,Dest^);
  ftString: begin
    CurrentAnsiConvert.UTF8BufferToAnsi(Data,DataLen,Temp);
    DataLen := length(Temp);
    MaxLen := Field.DataSize-1; // without trailing #0
    if DataLen>MaxLen then
      DataLen := MaxLen;
    move(pointer(Temp)^,Dest^,DataLen);
    PAnsiChar(Dest)[DataLen] := #0;
  end;
  ftWideString: begin
    {$ifdef ISDELPHI2007ANDUP} // here Dest = PWideChar[] of DataSize bytes
    UTF8ToWideChar(Dest,Data,(Field.DataSize-2)shr 1,DataLen);
    {$else}          // here Dest is PWideString
    UTF8ToWideString(Data,DataLen,WideString(Dest^));
    {$endif}
  end;
  // ftBlob,ftMemo,ftWideMemo should be retrieved by CreateBlobStream()
  else raise EDatabaseError.CreateFmt('%s.GetFieldData DataType=%d',
         [ClassName,ord(Field.DataType)]);
  end;
end;

function TSynVirtualDataSet.GetBlobStream(Field: TField; RowIndex: integer): TStream;
var Data: pointer;
    DataLen: integer;
begin
  Data := GetRowFieldData(Field,RowIndex,DataLen,false);
  if Data=nil then // should point to Blob or UTF8 data
    result := nil else
    case Field.DataType of
    ftBlob:
      result := TSynMemoryStream.Create(Data,DataLen);
    ftMemo, ftString:
      result := TRawByteStringStream.Create(CurrentAnsiConvert.UTF8BufferToAnsi(Data,DataLen));
    {$ifdef ISDELPHI2007ANDUP} ftWideMemo, {$endif} ftWideString:
      result := TRawByteStringStream.Create(Utf8DecodeToRawUnicode(Data,DataLen));
    else raise EDatabaseError.CreateFmt('%s.CreateBlobStream DataType=%d',
      [ClassName,ord(Field.DataType)]);
    end;
end;

function TSynVirtualDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  if Mode<>bmRead then
    raise EDatabaseError.CreateFmt('%s BLOB should be ReadOnly',[ClassName]);
  result := GetBlobStream(Field,PRecInfo(ActiveBuffer).RowIndentifier);
end;

function TSynVirtualDataSet.GetRecNo: Integer;
begin
  result := fCurrentRow+1;
end;

function TSynVirtualDataSet.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
begin
  result := grOK;
  case GetMode of
    gmPrior:
      if fCurrentRow>0 then
        dec(fCurrentRow) else
        result := grBOF;
    gmCurrent:
      if fCurrentRow<0 then
        result := grBOF else
      if fCurrentRow>=GetRecordCount then
        result := grEOF;
    gmNext:
      if fCurrentRow<GetRecordCount-1 then
        inc(fCurrentRow) else
        result := grEOF;
  end;
  if result=grOK then
    with PRecInfo(Buffer)^ do begin
      RowIndentifier := fCurrentRow;
      BookmarkFlag := bfCurrent;
      Bookmark := fCurrentRow;
    end;
end;

function TSynVirtualDataSet.GetRecordSize: Word;
begin
  result := SizeOf(TRecInfoIdentifier); // excluding Bookmark information
end;

procedure TSynVirtualDataSet.InternalClose;
begin
  BindFields(false);
  if DefaultFields then
    DestroyFields;
  fIsCursorOpen := False;
end;

procedure TSynVirtualDataSet.InternalFirst;
begin
  fCurrentRow := -1;
end;

procedure TSynVirtualDataSet.InternalGotoBookmark(Bookmark: Pointer);
begin
  fCurrentRow := PRecInfoIdentifier(Bookmark)^;
end;

procedure TSynVirtualDataSet.InternalHandleException;
begin
  Application.HandleException(Self);
end;

procedure TSynVirtualDataSet.InternalInitRecord(Buffer: TRecordBuffer);
begin
  fillchar(Buffer^,sizeof(TRecInfo),0);
end;

procedure TSynVirtualDataSet.InternalLast;
begin
  fCurrentRow := GetRecordCount;
end;

procedure TSynVirtualDataSet.InternalOpen;
begin
  BookmarkSize := SizeOf(TRecInfo)-sizeof(TRecInfoIdentifier);
  InternalInitFieldDefs;
  if DefaultFields then
    CreateFields;
  BindFields(true);
  fCurrentRow := -1;
  fIsCursorOpen := True;
end;

procedure TSynVirtualDataSet.InternalSetToRecord(Buffer: TRecordBuffer);
begin
  fCurrentRow := PRecInfo(Buffer).RowIndentifier;
end;

function TSynVirtualDataSet.IsCursorOpen: Boolean;
begin
  result := fIsCursorOpen;
end;

procedure TSynVirtualDataSet.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  PRecInfo(Buffer)^.Bookmark := PRecInfoIdentifier(Data)^;
end;

procedure TSynVirtualDataSet.SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);
begin
  PRecInfo(Buffer)^.BookmarkFlag := Value;
end;

procedure TSynVirtualDataSet.SetRecNo(Value: Integer);
begin
  CheckBrowseMode;
  if Value<>RecNo then begin
    dec(Value);
    if cardinal(Value)>=cardinal(GetRecordCount) then
      raise ERangeError.CreateFmt('%s.SetRecNo(%d) with Count=%d',
        [ClassName,Value+1,GetRecordCount]);
    DoBeforeScroll;
    fCurrentRow := Value;
    Resync([rmCenter]);
    DoAfterScroll;
  end;
end;

constructor TSynVirtualDataSet.Create(Owner: TComponent);
begin
  inherited Create(Owner);
  inc(GlobalDataSetCount);
  Name := ClassName+IntToStr(GlobalDataSetCount); // force unique name
end;


function DataSetToJSON(Data: TDataSet): RawUTF8;
var W: TJSONWriter;
    f: integer;
    blob: TRawByteStringStream;
begin
  result := 'null';
  if Data=nil then
    exit;
  Data.First;
  if Data.Eof then
    exit;
  W := TJSONWriter.Create(nil,true,false);
  try
    // get col names and types
    SetLength(W.ColNames,Data.FieldCount);
    for f := 0 to high(W.ColNames) do
      StringToUTF8(Data.FieldDefs[f].Name,W.ColNames[f]);
    W.AddColumns;
    W.Add('[');
    repeat
      W.Add('{');
      for f := 0 to Data.FieldCount-1 do begin
        W.AddString(W.ColNames[f]);
        with Data.Fields[f] do
        if IsNull then
          W.AddShort('null') else
        case DataType of
        ftBoolean: W.AddString(JSON_BOOLEAN[AsBoolean]);
        ftSmallint, ftInteger, ftWord, ftAutoInc: W.Add(AsInteger);
        ftLargeint: W.Add(TLargeintField(Data.Fields[f]).AsLargeInt);
        ftFloat, ftCurrency, ftBCD: W.Add(AsFloat);
        ftTimeStamp, ftDate, ftTime, ftDateTime: begin
          W.Add('"');
          W.AddDateTime(AsDateTime);
          W.Add('"');
        end;
        ftString, ftFixedChar, ftMemo: begin
          W.Add('"');
          W.AddAnsiString({$ifdef UNICODE}AsAnsiString{$else}AsString{$endif},
            twJSONEscape);
          W.Add('"');
        end;
        ftWideString: begin
          W.Add('"');
          W.AddJSONEscapeW(pointer(TWideStringField(Data.Fields[f]).Value));
          W.Add('"');
        end;
        ftVariant: W.AddVariantJSON(AsVariant);
        ftBytes, ftVarBytes, ftBlob, ftGraphic, ftOraBlob, ftOraClob: begin
          blob := TRawByteStringStream.Create;
          try
            (Data.Fields[f] as TBlobField).SaveToStream(blob);
            W.WrBase64(pointer(blob.DataString),length(blob.DataString),true);
          finally
            blob.Free;
          end;
        end;
        {$ifdef ISDELPHI2007ANDUP}
        ftWideMemo, ftFixedWideChar: begin
          W.Add('"');
          W.AddJSONEscapeW(pointer(AsWideString));
          W.Add('"');
        end;
        {$endif}
        {$ifdef UNICODE}
        ftShortint, ftByte: W.Add(AsInteger);
        ftLongWord: W.AddU(TLongWordField(Data.Fields[f]).Value);
        ftExtended, ftSingle: W.Add(AsFloat);
        {$endif}
        else W.AddShort('null'); // unhandled field type
        end;
        W.Add(',');
      end;
      W.CancelLastComma;
      W.Add('}',',');
      Data.Next;
    until Data.Eof;
    W.CancelLastComma;
    W.Add(']');
    W.SetText(result);
  finally
    W.Free;
  end;
end;

end.
