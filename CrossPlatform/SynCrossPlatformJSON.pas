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
 

}

{$i SynCrossPlatform.inc} // define e.g. HASINLINE

interface

uses
  SysUtils,
  Classes,
  Variants;

type
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
  // is faster, as dwsJSON is, but those are not cross-platform
  TJSONVariantData = object
  protected
    VType: TVarType;
    _Align: byte;
    VKind: TJSONVariantKind;
    VCount: integer;
    function GetKind: TJSONVariantKind;
    function GetCount: integer;
    function GetVarData(const aName: string; var Dest: TVarData): boolean;
    function GetValue(const aName: string): variant;
    procedure SetValue(const aName: string; const aValue: variant);
    function ParseJSONObject(const JSON: string; var Index: integer): boolean;
    function ParseJSONArray(const JSON: string; var Index: integer): boolean;
  public
    /// names of this jvObject
    Names: array of string;
    /// values of this jvObject or jvArray
    Values: array of variant;
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
    property Value[const aName: string]: variant read GetValue write SetValue;
  end;
  {$A+}

  /// low-level class used to register TJSONVariantData as custom type
  // - allows late binding to values
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
function JSONVariant(const JSON: string): variant;

/// access to a TJsonVariant instance members
// - e.g. Kind, Count, Names[] or Values[]
function JSONVariantData(const JSONVariant: variant): PJSONVariantData;

var
  JSONVariantType: TInvokeableVariantType;

/// compute the quoted JSON string corresponding to the supplied text 
function JSONEscape(const Text: string): string;



implementation

function JSONVariant(const JSON: string): variant;
begin
  VarClear(result);
  TJSONVariantData(result).FromJSON(JSON);
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
  len := length(str)+1;
  SetLength(str,len);
  str[len] := chr;
end; // for NextGen / immutable strings, TStringBuilder should be faster 

function JSONEscape(const Text: string): string;
var len,j: integer;
procedure DoEscape;
var i: Integer;
begin
  result := '"'+copy(Text,1,j-1);
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
  result := '"'+Text+'"';
end;

var
  SettingsUS: TFormatSettings;

function ValueToJSON(const Value: variant): string;
var I64: Int64;
begin
  if TVarData(Value).VType=JSONVariantType.VarType then
    result := TJSONVariantData(Value).ToJSON else
  if TVarData(Value).VType<=varNull then
    result := 'null' else
  if VarIsStr(Value) then
    result := JSONEscape(Value) else
  if VarIsOrdinal(Value) then begin
    I64 := Value;
    result := IntToStr(I64);
  end else
  if VarIsFloat(Value) then
    result := FloatToStr(Value,SettingsUS) else
    result := Value;
end;


{ TJSONVariantData }

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

function GetNextChar(const JSON: string; var Index: integer): char;
  {$ifdef HASINLINE}inline;{$endif}
begin
  if Index>length(JSON) then
    result := #0 else begin
    result := JSON[Index];
    inc(Index);
  end;
end;

function GetNextNonWhite(const JSON: string; var Index: integer): boolean;
  {$ifdef HASINLINE}inline;{$endif}
begin
  result := false;
  while Index<length(JSON) do begin
    if JSON[Index]=#0 then
      exit;
    if JSON[Index]>' ' then begin
      result := true;
      exit;
    end;
    inc(Index);
  end;
end;

function GetNextNonWhiteChar(const JSON: string; var Index: integer): char;
  {$ifdef HASINLINE}inline;{$endif}
begin
  result := #0;
  while Index<length(JSON) do begin
    if JSON[Index]=#0 then
      exit;
    if JSON[Index]>' ' then begin
      result := JSON[Index];
      inc(Index);
      exit;
    end;
    inc(Index);
  end;
end;

function GetNextString(const JSON: string; var Index: integer; out str: string): boolean;
procedure UnEscape;
var c: char;
    u: string;
    unicode,err: integer;
begin
  repeat
    c := GetNextChar(JSON,Index);
    case c of
    #0: exit;
    '"': break;
    '\': begin
      c := GetNextChar(JSON,Index);
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
var i: integer;
begin
  result := false;
  for i := Index to length(JSON) do
    case JSON[i] of
    #0: exit;
    '"': begin
      str := copy(JSON,Index,i-Index);
      Index := i+1;
      break;
    end;
    '\': begin
      str := copy(JSON,Index,i-Index);
      Index := i;
      UnEscape;
      break;
    end;
    end;
  result := true;
end;

type
  TJSONKind = (kNone, kNull, kFalse, kTrue, kString, kNumber, kObject, kArray);

function GetNextJSON(const JSON: string; var Index: integer; out Value: variant): TJSONKind;
var str: string;
    i64: Int64;
    d: double;
    start,err: integer;
begin
  result := kNone;
  case GetNextNonWhiteChar(JSON,Index) of
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
  '"': begin
    if not GetNextString(JSON,Index,str) then
      exit;
    result := kString;
    Value := str;
  end;
  '{': begin
    if not TJSONVariantData(Value).ParseJSONObject(JSON,Index) then
      exit;
    result := kObject;
  end;
  '[': begin
    if not TJSONVariantData(Value).ParseJSONArray(JSON,Index) then
      exit;
    result := kArray;
  end;
  '-','0'..'9': begin
    start := Index-1;
    while true do
      case JSON[Index] of
      '-','+','0'..'9','.','E','e': inc(Index);
      else break;
      end;
    str := copy(JSON,start,Index-start);
    val(str,i64,err);
    if err=0 then
      Value := i64 else begin
      val(str,d,err);
      if err<>0 then
        exit;
      Value := d;
    end;
    result := kNumber;
  end;
  end;
end;

function TJSONVariantData.FromJSON(const JSON: string): boolean;
var i: Integer;
begin
  i := 1;
  result := GetNextJSON(JSON,i,variant(self)) in [kObject,kArray];
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
  if cardinal(i)<cardinal(length(Values)) then begin
    Dest.VType := varVariant or varByRef;
    Dest.VPointer := @Values[i];
    result := true;
  end else
    result := false;
end;

procedure TJSONVariantData.Init(const JSON: string);
begin
  Init;
  FromJSON(JSON);
end;

procedure TJSONVariantData.Init;
begin
  VType := JSONVariantType.VarType;
  VKind := jvUndefined;
  VCount := 0;
  pointer(Names) := nil;
  pointer(Values) := nil;
end;

function TJSONVariantData.NameIndex(const aName: string): integer;
begin
  if (@self<>nil) and (VType=JSONVariantType.VarType) and (Names<>nil) then
    for result := 0 to VCount-1 do
      if Names[result]=aName then
        exit;
  result := -1;
end;

function TJSONVariantData.ParseJSONArray(const JSON: string;
  var Index: integer): boolean;
var item: variant;
begin
  result := false;
  Init;
  repeat
    if GetNextJSON(JSON,Index,item)=kNone then
      exit;
    AddValue(item);
    case GetNextNonWhiteChar(JSON,Index) of
    ',': continue;
    ']': break;
    else exit;
    end;
  until false;
  SetLength(Values,VCount);
  VKind := jvArray;
  result := true;
end;

function TJSONVariantData.ParseJSONObject(const JSON: string;
  var Index: integer): boolean;
var key: string;
    val: variant;
begin
  result := false;
  Init;
  repeat
    if (GetNextNonWhiteChar(JSON,Index)<>'"') or
       not GetNextString(JSON,Index,key) then
      exit;
    if (GetNextNonWhiteChar(JSON,Index)<>':') or
       (GetNextJSON(JSON,Index,val)=kNone) then
      exit; // writeln(Copy(JSON,Index-10,30));
    AddNameValue(key,val);
    case GetNextNonWhiteChar(JSON,Index) of
    ',': continue;
    '}': break;
    else exit;
    end;
  until false;
  SetLength(Names,VCount);
  SetLength(Values,VCount);
  VKind := jvObject;
  result := true;
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
      result := result+JSONEscape(Names[i])+':'+ValueToJSON(Values[i])+',';
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
  TJSONVariantData(V).SetValue(Name,variant(Value));
  result := true;
end;

initialization
  {$ifdef ISDELPHIXE}
  SettingsUS := TFormatSettings.Create($0409);
  {$else}
  GetLocaleFormatSettings($0409,SettingsUS);
  {$endif}
  JSONVariantType := TJSONVariant.Create;

end.