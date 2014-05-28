/// minimum standand-alone cross-platform JSON process using variants
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynCrossPlatformJSON;

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
  - FPC has some issues with working with variants: UTF-8 encoding is sometimes
    lost, and TInvokeableVariantType.SetProperty() is just broken

}

{$i SynCrossPlatform.inc} // define e.g. HASINLINE

interface

uses
  SysUtils,
  Classes,
  Variants;

type
  TStringDynArray = array of string;
  TVariantDynArray = array of variant;

  /// exception used during standand-alone cross-platform JSON process
  EJSONException = class(Exception);

  /// which kind of document the TJSONVariantData contains
  TJSONVariantKind = (jvUndefined, jvObject, jvArray);

  PJSONVariantData = ^TJSONVariantData;

  {$A-}
  /// stores any JSON object or array as variant
  // - this structure is not very optimized for speed or memory use, but is
  // simple and strong enough for our client-side purpose
  // - it is in fact already faster (and using less memory) than DBXJSON and
  // SuperObject / XSuperObject libraries - of course, mORMot's TDocVariant
  // is faster, as dwsJSON is in some cases, but those are not cross-platform
  {$ifdef UNICODE}
  TJSONVariantData = record
  private
  {$else}
  TJSONVariantData = object
  protected
  {$endif}
    VType: TVarType;
    _Align: byte;
    VKind: TJSONVariantKind;
    VCount: integer;
    function GetKind: TJSONVariantKind;
    function GetCount: integer;
    function GetVarData(const aName: string; var Dest: TVarData): boolean;
    function GetValue(const aName: string): variant;
    procedure SetValue(const aName: string; const aValue: variant);
  public
    /// names of this jvObject
    Names: TStringDynArray;
    /// values of this jvObject or jvArray
    Values: TVariantDynArray;
    /// initialize the low-level memory structure
    procedure Init; overload;
    /// initialize the low-level memory structure with a given JSON content
    procedure Init(const JSON: string); overload;
    /// access to a nested TJSONVariantData item
    // - returns nil if aName was not found
    function Data(const aName: string): PJSONVariantData;
      {$ifdef HASINLINE}inline;{$endif}
    /// add a value to the jvArray
    // - raise a ESJONException if the instance is a jvObject
    procedure AddValue(const aValue: variant);
    /// add a name/value pair to the jvObject
    // - raise a ESJONException if the instance is a jvArray
    procedure AddNameValue(const aName: string; const aValue: variant);
    /// search for a name in this jvObject
    function NameIndex(const aName: string): integer;
    /// fill this document from a JSON array or object
    function FromJSON(const JSON: string): boolean;
    /// convert this document into JSON array or object
    function ToJSON: string;
    /// kind of document this TJSONVariantData contains
    property aKind: TJSONVariantKind read GetKind;
    /// number of items in this jvObject or jvArray
    property Count: integer read GetCount;
    /// access by name to a value of this jvObject
    property Value[const aName: string]: variant read GetValue write SetValue; default;
  end;
  {$A+}

  /// low-level class used to register TJSONVariantData as custom type
  // - allows late binding to values, e.g.
  // ! jsonvar.avalue := jsonvar.avalue+1;
  // - due to an issue with FPC implementation, you can only read properties,
  // not set them, so you should write:
  // ! TJSONVariantData(jsonvar)['avalue'] := jsonvar.avalue+1;
  TJSONVariant = class(TInvokeableVariantType)
  protected
    {$ifndef FPC}
    function FixupIdent(const AText: string): string; override;
    {$endif}
  public
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
    procedure Clear(var V: TVarData); override;
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: string): Boolean; override;
    function SetProperty(const V: TVarData; const Name: string;
      const Value: TVarData): Boolean; override;
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    procedure CastTo(var Dest: TVarData; const Source: TVarData;
      const AVarType: TVarType); override;
  end;

  /// handle a JSON result table, as returned by mORMot's server
  // - handle both expanded and non expanded layout
  // - will be used on client side for ORM data parsing
  TJSONTable = class
  protected
    fJSON: string;
    fFieldNames: TStringDynArray;
    fJSONExpanded: boolean;
    fJSONIndexFirstValue: integer;
    fJSONCurrentIndex: integer;
    fRowValues: TVariantDynArray;
    function Get(const FieldName: string): variant;
  public
    /// parse the supplied JSON content
    constructor Create(const aJSON: string);
    /// case-insensitive search for a field name
    function FieldIndex(const FieldName: string): integer;
    /// to be called in a loop to iterate through all data rows
    // - if returned true, Value[] contains the fields of this row
    function Step(SeekFirst: boolean=false): boolean;
    /// after Step() returned true, can be used to retrieve a field value by name
    property Value[const FieldName: string]: variant read Get; default;
    /// after Step() returned true, can be used to retrieve a field value by index
    property RowValue: TVariantDynArray read fRowValues;
    /// the recognized field names
    property FieldNames: TStringDynArray read fFieldNames;
    /// the associated JSON content
    property JSON: string read fJSON;
  end;

  
/// create a TJSONVariant instance from a given JSON content
// - typical usage may be:
//! var doc: variant;
//!     json: string;
//! begin
//!   doc := JSONVariant('{"test":1234,"name":"Joh\"n\r"}');
//!   assert(doc.test=1234); // access via late binding
//!   assert(doc.name='Joh"n'#13);
//!   assert(doc.name2=null); // unknown properties returns null
//!   json := doc; // to convert a TJSONVariant to JSON, just assign to a string
//!   assert(json='{"test":1234,"name":"Joh\"n\r"}');
//! end;
// - note that FPC does not allow to set values by late-binding
function JSONVariant(const JSON: string): variant; overload;

/// create a TJSONVariant TJSONVariant array from a supplied array of values
function JSONVariant(const values: TVariantDynArray): variant; overload;

/// access to a TJSONVariant instance members
// - e.g. Kind, Count, Names[] or Values[]
// - will raise an exception if the supplied variant is not a TJSONVariant
// - this function is safer than TJSONVariant(JSONVariant)
function JSONVariantData(const JSONVariant: variant): PJSONVariantData;

var
  /// the custom variant type definition registered for TJSONVariant
  JSONVariantType: TInvokeableVariantType;

/// compute the quoted JSON string corresponding to the supplied text 
function StringToJSON(const Text: string): string;

/// compute the JSON representation of a floating-point value
procedure DoubleToJSON(Value: double; var result: string);

/// compute the JSON representation of a variant value
function ValueToJSON(const Value: variant): string;

/// read an UTF-8 (JSON) file into a native string
// - file should be existing, otherwise an exception is raised
function UTF8FileToString(const aFileName: TFileName): string;


implementation

{$ifdef FPC}
// assume string is UTF-8 encoded (as with Lazarus/LCL)
// note that when working with variants, FPC 2.7.1 sometimes clear the code page
type UTF8ToString = RawByteString;
{$else}
{$ifndef UNICODE}
function UTF8ToString(const utf8: UTF8String): string;
begin
  result := UTF8ToAnsi(utf8);
end;
{$endif}
{$endif}

function UTF8FileToString(const aFileName: TFileName): string;
var F: TFileStream;
    len: integer;
    utf8: UTF8String;
begin
  F := TFileStream.Create(aFileName,fmOpenRead);
  try
    len := F.Size;
    SetLength(utf8,len);
    F.Read(utf8[1],len);
    result := UTF8ToString(utf8);
  finally
    F.Free;
  end;
end;

function JSONVariant(const JSON: string): variant;
begin
  VarClear(result);
  TJSONVariantData(result).FromJSON(JSON);
end;

function JSONVariant(const values: TVariantDynArray): variant;
begin
  VarClear(result);
  TJSONVariantData(result).Init;
  TJSONVariantData(result).VKind := jvArray;
  TJSONVariantData(result).VCount := length(values);
  TJSONVariantData(result).Values := values;
end;

function JSONVariantData(const JSONVariant: variant): PJSONVariantData;
begin
  with TVarData(JSONVariant) do
    if VType=JSONVariantType.VarType then
      result := @JSONVariant else
    if VType=varByRef or varVariant then
      result := JSONVariantData(PVariant(VPointer)^) else
    raise EJSONException.CreateFmt('JSONVariantData.Data(%d<>JSONVariant)',[VType]);
end;

procedure AppendChar(var str: string; chr: Char);
  {$ifdef HASINLINE}inline;{$endif}
var len: Integer;  // this is faster than str := str+chr !
begin
  len := length(str);
  SetLength(str,len+1);
  PChar(pointer(str))[len] := chr; // SetLength() made str unique
end; // for NextGen / immutable strings, TStringBuilder could be faster 

function StringToJSON(const Text: string): string;
var len,j: integer;
procedure DoEscape;
var i: Integer;
begin
  result := '"'+copy(Text,1,j-1); // here FPC 2.7.1 erases UTF-8 encoding
  for i := j to len do begin
    case Text[i] of
    #8: result := result+'\b';
    #9: result := result+'\t';
    #10: result := result+'\n';
    #12: result := result+'\f';
    #13: result := result+'\r';
    '\': result := result+'\\';
    '"': result := result+'\"';
    else
    if Text[i]<' ' then
      result := result+'\'+IntToHex(ord(Text[i]),4) else
      AppendChar(result,Text[i]); // will be UTF-8 encoded later
    end;
  end;
  AppendChar(result,'"');
end;
begin
  len := length(Text);
  for j := 1 to len do
    case Text[j] of
    #0..#31,'\','"': begin
      DoEscape;
      exit;
    end;
    end;
  // if we reached here, no character needs to be escaped in this string
  result := '"'+Text+'"'; // here FPC 2.7.1 erases UTF-8 encoding :(
end;

procedure DoubleToJSON(Value: double; var result: string);
const
  FloatToStrDecSep: PChar  = @
{$ifdef FPC}     DefaultFormatSettings. {$else}
{$ifdef UNICODE} FormatSettings.        {$endif} {$endif}
                   DecimalSeparator;
var decsep: Char;
begin
  decsep := FloatToStrDecSep^;
  FloatToStrDecSep^ := '.';
  result := FloatToStr(Value);
  FloatToStrDecSep^ := decsep;
end;

function ValueToJSON(const Value: variant): string;
var I64: Int64;
begin
  if TVarData(Value).VType=JSONVariantType.VarType then
    result := TJSONVariantData(Value).ToJSON else
  if TVarData(Value).VType<=varNull then
    result := 'null' else
  if VarIsOrdinal(Value) then begin
    I64 := Value;
    result := IntToStr(I64);
  end else
  if VarIsFloat(Value) then
    DoubleToJSON(Value,result) else
  if VarIsStr(Value) then
    result := StringToJSON(Value) else
    result := Value;
end;


{ TJSONParser }

type
  /// the JSON node types, as recognized by TJSONParser
  TJSONParserKind = (
    kNone, kNull, kFalse, kTrue, kString, kInteger, kFloat, kObject, kArray);

  /// used to parse any JSON content
  TJSONParser = {$ifdef UNICODE}record{$else}object{$endif}
    JSON: string;
    Index: integer;
    JSONLength: integer;
    procedure Init(const aJSON: string; aIndex: integer);
    function GetNextChar: char;                {$ifdef HASINLINE}inline;{$endif}
    function GetNextNonWhiteChar: char;        {$ifdef HASINLINE}inline;{$endif}
    function GetNextString(out str: string): boolean; overload;
    function GetNextString: string; overload;  {$ifdef HASINLINE}inline;{$endif}
    function GetNextJSON(out Value: variant): TJSONParserKind;
    function CheckNextIdent(const ExpectedIdent: string): Boolean;
    function ParseJSONObject(var Data: TJSONVariantData): boolean;
    function ParseJSONArray(var Data: TJSONVariantData): boolean;
    procedure GetNextStringUnEscape(var str: string);
  end;


procedure TJSONParser.Init(const aJSON: string; aIndex: integer);
begin
  JSON := aJSON;
  JSONLength := length(JSON);
  Index := aIndex;
end;

function TJSONParser.GetNextChar: char;
begin
  if Index<=JSONLength then begin
    result := JSON[Index];
    inc(Index);
  end else
    result := #0;
end;

function TJSONParser.GetNextNonWhiteChar: char;
begin
  if Index<=JSONLength then
    repeat
      if JSON[Index]>' ' then begin
        result := JSON[Index];
        inc(Index);
        exit;
      end;
      inc(Index);
    until Index>JSONLength;
  result := #0;
end;

procedure TJSONParser.GetNextStringUnEscape(var str: string);
var c: char;
    u: string;
    unicode,err: integer;
begin
  repeat
    c := GetNextChar;
    case c of
    #0: exit;
    '"': break;
    '\': begin
      c := GetNextChar;
      case c of
      #0: exit;
      'b': AppendChar(str,#08);
      't': AppendChar(str,#09);
      'n': AppendChar(str,#$0a);
      'f': AppendChar(str,#$0c);
      'r': AppendChar(str,#$0d);
      'u': begin
        u := Copy(JSON,Index,4);
        if length(u)<>4 then
          exit;
        inc(Index,4);
        val('$'+u,unicode,err);
        if err<>0 then
          exit;
        AppendChar(str,char(unicode));
      end;
      else AppendChar(str,c);
      end;
    end;
    else AppendChar(str,c);
    end;
  until false;
end;

function TJSONParser.GetNextString(out str: string): boolean;
var i: integer;
begin
  for i := Index to JSONLength do
    case JSON[i] of
    '"': begin // end of string without escape -> direct copy
      str := copy(JSON,Index,i-Index);
      Index := i+1;
      result := true;
      exit;
    end;
    '\': begin // need unescaping
      str := copy(JSON,Index,i-Index);
      Index := i;
      GetNextStringUnEscape(str);
      result := true;
      exit;
    end;
    end;
  result := false;
end;

function TJSONParser.GetNextString: string; 
begin
  if not GetNextString(result) then
    result := '';
end;

function TJSONParser.GetNextJSON(out Value: variant): TJSONParserKind;
var str: string;
    i64: Int64;
    d: double;
    start,err: integer;
begin
  result := kNone;
  case GetNextNonWhiteChar of
  'n': if copy(JSON,Index,3)='ull' then begin
         inc(Index,3);
         result := kNull;
         Value := null;
       end;
  'f': if copy(JSON,Index,4)='alse' then begin
         inc(Index,4);
         result := kFalse;
         Value := false;
       end;
  't': if copy(JSON,Index,3)='rue' then begin
         inc(Index,3);
         result := kTrue;
         Value := true;
       end;
  '"': if GetNextString(str) then begin
         result := kString;
         Value := str;
       end;
  '{': if ParseJSONObject(TJSONVariantData(Value)) then
         result := kObject;
  '[': if ParseJSONArray(TJSONVariantData(Value)) then
         result := kArray;
  '-','0'..'9': begin
    start := Index-1;
    while true do
      case JSON[Index] of
      '-','+','0'..'9','.','E','e': inc(Index);
      else break;
      end;
    str := copy(JSON,start,Index-start);
    val(str,i64,err);
    if err=0 then begin
      Value := i64;
      result := kInteger;
    end else begin
      val(str,d,err);
      if err<>0 then
        exit;
      Value := d;
      result := kFloat;
    end;
  end;
  end;
end;

function TJSONParser.CheckNextIdent(const ExpectedIdent: string): Boolean;
begin
  result := (GetNextNonWhiteChar='"') and
            (CompareText(GetNextString,ExpectedIdent)=0) and
            (GetNextNonWhiteChar=':');
end;

function TJSONParser.ParseJSONArray(var Data: TJSONVariantData): boolean;
var item: variant;
begin
  result := false;
  Data.Init;
  repeat
    if GetNextJSON(item)=kNone then
      exit;
    Data.AddValue(item);
    case GetNextNonWhiteChar of
    ',': continue;
    ']': break;
    else exit;
    end;
  until false;
  SetLength(Data.Values,Data.VCount);
  Data.VKind := jvArray;
  result := true;
end;

function TJSONParser.ParseJSONObject(var Data: TJSONVariantData): boolean;
var key: string;
    val: variant;
begin
  result := false;
  Data.Init;
  repeat
    if (GetNextNonWhiteChar<>'"') or
       not GetNextString(key) then
      exit;
    if (GetNextNonWhiteChar<>':') or
       (GetNextJSON(val)=kNone) then
      exit; // writeln(Copy(JSON,Index-10,30));
    Data.AddNameValue(key,val);
    case GetNextNonWhiteChar of
    ',': continue;
    '}': break;
    else exit;
    end;
  until false;
  SetLength(Data.Names,Data.VCount);
  SetLength(Data.Values,Data.VCount);
  Data.VKind := jvObject;
  result := true;
end;


{ TJSONVariantData }

procedure TJSONVariantData.Init;
begin
  VType := JSONVariantType.VarType;
  {$ifdef UNICODE} // makes compiler happy
  _Align := 0;
  {$endif}
  VKind := jvUndefined;
  VCount := 0;
  pointer(Names) := nil;
  pointer(Values) := nil;
end;

procedure TJSONVariantData.Init(const JSON: string);
begin
  Init;
  FromJSON(JSON);
end;

procedure TJSONVariantData.AddNameValue(const aName: string;
  const aValue: variant);
begin
  if VKind=jvUndefined then
    VKind := jvObject else
    if VKind<>jvObject then
      raise EJSONException.CreateFmt('AddNameValue(%s) over array',[aName]);
  if VCount<=length(Values) then begin
    SetLength(Values,VCount+VCount shr 3+32);
    SetLength(Names,VCount+VCount shr 3+32);
  end;
  Values[VCount] := aValue;
  Names[VCount] := aName;
  inc(VCount);
end;

procedure TJSONVariantData.AddValue(const aValue: variant);
begin
  if VKind=jvUndefined then
    VKind := jvArray else
    if VKind<>jvArray then
      raise EJSONException.Create('AddValue() over object');
  if VCount<=length(Values) then
    SetLength(Values,VCount+VCount shr 3+32);
  Values[VCount] := aValue;
  inc(VCount);
end;

function TJSONVariantData.FromJSON(const JSON: string): boolean;
var Parser: TJSONParser;
begin
  Parser.Init(JSON,1);
  result := Parser.GetNextJSON(variant(self)) in [kObject,kArray];
end;

function TJSONVariantData.Data(const aName: string): PJSONVariantData;
var i: integer;
begin
  i := NameIndex(aName);
  if (i<0) or (TVarData(Values[i]).VType<>JSONVariantType.VarType) then
    result := nil else
    result := @Values[i];
end;

function TJSONVariantData.GetKind: TJSONVariantKind;
begin
  if (@self=nil) or (VType<>JSONVariantType.VarType) then
    result := jvUndefined else
    result := VKind;
end;

function TJSONVariantData.GetCount: integer;
begin
  if (@self=nil) or (VType<>JSONVariantType.VarType) then
    result := 0 else
    result := VCount;
end;

function TJSONVariantData.GetValue(const aName: string): variant;
begin
  VarClear(result);
  GetVarData(aName,TVarData(result));
end;

function TJSONVariantData.GetVarData(const aName: string;
  var Dest: TVarData): boolean;
var i: integer;
begin
  i := NameIndex(aName);
  if (i>=0) and (i<length(Values)) then begin
    Dest.VType := varVariant or varByRef;
    Dest.VPointer := @Values[i];
    result := true;
  end else
    result := false;
end;

function TJSONVariantData.NameIndex(const aName: string): integer;
begin
  if (@self<>nil) and (VType=JSONVariantType.VarType) and (Names<>nil) then
    for result := 0 to VCount-1 do
      if Names[result]=aName then
        exit;
  result := -1;
end;

procedure TJSONVariantData.SetValue(const aName: string;
  const aValue: variant);
var i: integer;
begin
  if @self=nil then
    raise EJSONException.Create('Unexpected Value[] access');
  i := NameIndex(aName);
  if i<0 then
    AddNameValue(aName,aValue) else
    Values[i] := aValue;
end;

function TJSONVariantData.ToJSON: string;
var i: integer;
begin
  case VKind of
  jvObject: begin
    result := '{';
    for i := 0 to VCount-1 do
      result := result+StringToJSON(Names[i])+':'+ValueToJSON(Values[i])+',';
    result[length(result)] := '}';
  end;
  jvArray: begin
    result := '[';
    for i := 0 to VCount-1 do
      result := result+ValueToJSON(Values[i])+',';
    result[length(result)] := ']';
  end;
  else result := 'null';
  end;
end;


{ TJSONVariant }

procedure TJSONVariant.Cast(var Dest: TVarData; const Source: TVarData);
begin
  CastTo(Dest,Source,VarType);
end;

procedure TJSONVariant.CastTo(var Dest: TVarData; const Source: TVarData;
  const AVarType: TVarType);
begin
  if Source.VType<>VarType then
    RaiseCastError;
  variant(Dest) := TJSONVariantData(Source).ToJSON;
end;

procedure TJSONVariant.Clear(var V: TVarData);
begin
  V.VType := varEmpty;
  Finalize(TJSONVariantData(V).Names);
  Finalize(TJSONVariantData(V).Values);
end;

procedure TJSONVariant.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: Boolean);
begin
  if Indirect then
    SimplisticCopy(Dest,Source,true) else begin
    VarClear(variant(Dest));
    TJSONVariantData(Dest).Init;
    TJSONVariantData(Dest) := TJSONVariantData(Source);
  end;
end;

{$ifndef FPC}
function TJSONVariant.FixupIdent(const AText: string): string;
begin // we expect the names to be case-sensitive
  result := AText;
end;
{$endif}

function TJSONVariant.GetProperty(var Dest: TVarData; const V: TVarData;
  const Name: string): Boolean;
begin
  if not TJSONVariantData(V).GetVarData(Name,Dest) then
    Dest.VType := varNull;
  result := true;
end;

function TJSONVariant.SetProperty(const V: TVarData; const Name: string;
  const Value: TVarData): Boolean;
begin
  {$ifdef FPC}
  raise EJSONException.Create('Setting TJSONVariant via late-binding does not'+
    ' work with FPC: use TJSONVariantData(jsonvar)[''prop''] := ... instead');
  {$else}
  TJSONVariantData(V).SetValue(Name,variant(Value));
  result := true;
  {$endif}
end;


{ TJSONTable }

constructor TJSONTable.Create(const aJSON: string);
var f,firstValue: integer;
    EndOfField: char;
    fieldCount, fieldName, dummy: variant;
    Parser: TJSONParser;
begin
  Parser.Init(aJSON,1);
  fJSON := aJSON;
  EndOfField := #0;
  if (Parser.GetNextNonWhiteChar='{') and
      Parser.CheckNextIdent('fieldCount') and
     (Parser.GetNextJSON(fieldCount)=kInteger) and
     (Parser.GetNextNonWhiteChar=',') and
      Parser.CheckNextIdent('values') and
     (Parser.GetNextNonWhiteChar='[') then begin
    // non expanded format: {"fieldCount":2,"values":["ID","Int",1,0,2,0,3,...]
    SetLength(fFieldNames,integer(fieldCount));
    for f := 0 to high(fFieldNames) do begin
      if Parser.GetNextJSON(fieldName)<>kString then
        exit;
      fFieldNames[f] := fieldName;
      EndOfField := Parser.GetNextNonWhiteChar;
      if EndOfField<>',' then
        if (EndOfField<>']') or (f<>High(FieldNames)) then
          exit
    end;
    if EndOfField=',' then
      fJSONIndexFirstValue := Parser.Index;
  end else begin
    // expanded format: [{"ID":1,"Int":0},{"ID":2,"Int":0},{"ID":3,...]
    Parser.Index := 1;
    if (Parser.GetNextNonWhiteChar='[') and
       (Parser.GetNextNonWhiteChar='{') then begin
      firstValue := Parser.Index;
      f := 0;
      repeat
        if (Parser.GetNextJSON(fieldName)<>kString) or
           (Parser.GetNextNonWhiteChar<>':') then
          exit;
        if Parser.GetNextJSON(dummy)=kNone then
          exit;
        SetLength(fFieldNames,f+1);
        fFieldNames[f] := fieldName;
        inc(f);
        EndOfField := Parser.GetNextNonWhiteChar;
        if EndOfField<>',' then
          if EndOfField='}' then
            break else
            exit;
      until false;
      fJSONIndexFirstValue := firstValue;
      fJSONExpanded := true;
    end;    
  end;
  SetLength(fRowValues,length(fFieldNames));
end;

function TJSONTable.FieldIndex(const FieldName: string): integer;
begin
  for result := 0 to high(fFieldNames) do
    if CompareText(fFieldNames[result],FieldName)=0 then
      exit;
  result := -1;
end;

function TJSONTable.Get(const FieldName: string): variant;
var ndx: integer;
begin
  ndx := FieldIndex(FieldName);
  if ndx<0 then
    result := null else
    result := fRowValues[ndx];
end;

function TJSONTable.Step(SeekFirst: boolean): boolean;
var f: integer;
    EndOfField: char;
    Parser: TJSONParser;
begin
  result := false;
  if SeekFirst or (fJSONCurrentIndex=0) then
    fJSONCurrentIndex := fJSONIndexFirstValue;
  if fJSONCurrentIndex<=0 then
    exit;
  Parser.Init(fJSON,fJSONCurrentIndex);
  fJSONCurrentIndex := -1; // indicates end of content in case of exit below
  EndOfField := #0;
  for f := 0 to high(fRowValues) do begin
    if fJSONExpanded and not Parser.CheckNextIdent(fFieldNames[f]) then
      exit;
    if Parser.GetNextJSON(fRowValues[f])=kNone then
      exit;
    EndOfField := Parser.GetNextNonWhiteChar;
    if EndOfField<>',' then
      if f<>High(fRowValues) then
        exit else
      if ((EndOfField=']') and (not fJSONExpanded)) or
         ((EndOfField='}') and fJSONExpanded) then
        break else
        exit;
  end;
  if fJSONExpanded then begin
    if EndOfField<>'}' then
      exit;
    EndOfField := Parser.GetNextNonWhiteChar;
    if (EndOfField=',') and
       (Parser.GetNextNonWhiteChar<>'{') then
      exit;
  end;
  if EndOfField=',' then
    fJSONCurrentIndex := Parser.Index; // indicates next Step() has data 
  result := true;
end;


initialization
  JSONVariantType := TJSONVariant.Create;

end.
