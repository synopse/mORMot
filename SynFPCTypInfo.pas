/// wrapper around FPC typinfo.pp unit for SynCommons.pas and mORMot.pas
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynFPCTypInfo;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2019 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

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

  The Initial Developer of the Original Code is Alfred Glaenzer.

  Portions created by the Initial Developer are Copyright (C) 2019
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Arnaud Bouchez


  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. if you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. if you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****


  Version 1.18
  - initial revision
  - unit created to avoid polluting the SynCommons.pas/mORMot.pas namespace
    with overloaded typinfo.pp types

}

interface

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

uses
  SysUtils,
  TypInfo;

const
  ptField = 0;
  ptStatic = 1;
  ptVirtual = 2;
  ptConst = 3;

{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
function AlignToPtr(p: pointer): pointer; inline;
function AlignTypeData(p : pointer): pointer; inline;
{$else FPC_REQUIRES_PROPER_ALIGNMENT}
type
  AlignToPtr = pointer;
  AlignTypeData = pointer;
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}

function GetFPCPropInfo(AClass: TClass; const PropName: string): PPropInfo; inline;
function IsStoredProp(Instance: TObject; PropInfo: pointer): boolean; inline;
function GetOrdProp(Instance: TObject; PropInfo: pointer): Int64; inline;
procedure SetOrdProp(Instance: TObject; PropInfo: pointer; Value: Int64); inline;
function GetFloatProp(Instance: TObject; PropInfo: pointer): Extended; inline;
procedure SetFloatProp(Instance: TObject; PropInfo: pointer; const Value: Extended); inline;
function GetStrProp(Instance: TObject; PropInfo: pointer): string; inline;
procedure SetStrProp(Instance: TObject; PropInfo: pointer; const Value: string); inline;
function GetWideStrProp(Instance: TObject; PropInfo: pointer): WideString; inline;
procedure SetWideStrProp(Instance: TObject; PropInfo: pointer; const Value: WideString); inline;
{$ifdef HASVARUSTRING}
function GetUnicodeStrProp(Instance: TObject; PropInfo: pointer): UnicodeString; inline;
procedure SetUnicodeStrProp(Instance: TObject; PropInfo: pointer; const Value: UnicodeString); inline;
{$endif HASVARUSTRING}
function GetVariantProp(Instance: TObject; PropInfo: pointer): Variant; inline;
procedure SetVariantProp(Instance: TObject; PropInfo: pointer; const Value: Variant); inline;

type
  /// some type definition to avoid inclusion of TypInfo in SynCommons/mORMot.pas
  PFPCInterfaceData = TypInfo.PInterfaceData;
  PFPCVmtMethodParam = TypInfo.PVmtMethodParam;
  PFPCIntfMethodTable = TypInfo.PIntfMethodTable;
  PFPCIntfMethodEntry = TypInfo.PIntfMethodEntry;
{$ifdef FPC_NEWRTTI}
  PFPCRecInitData = TypInfo.PRecInitData;

function GetFPCRecInitData(TypeData: Pointer): Pointer;
{$endif FPC_NEWRTTI}

procedure FPCDynArrayClear(var a: Pointer; typeInfo: Pointer);
procedure FPCFinalizeArray(p: Pointer; typeInfo: Pointer; elemCount: PtrUInt);
procedure FPCFinalize(Data: Pointer; TypeInfo: Pointer);
procedure FPCRecordCopy(const Source; var Dest; TypeInfo: pointer);
procedure FPCRecordAddRef(var Data; TypeInfo : pointer);


implementation

procedure FPCDynArrayClear(var a: Pointer; typeInfo: Pointer);
  [external name 'FPC_DYNARRAY_CLEAR'];
procedure FPCFinalizeArray(p: Pointer; typeInfo: Pointer; elemCount: PtrUInt);
  [external name 'FPC_FINALIZE_ARRAY'];
procedure FPCFinalize(Data: Pointer; TypeInfo: Pointer);
  [external name 'FPC_FINALIZE'];
procedure FPCRecordCopy(const Source; var Dest; TypeInfo: pointer);
  [external name 'FPC_COPY'];
procedure FPCRecordAddRef(var Data; TypeInfo : pointer);
  [external name 'FPC_ADDREF'];


{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT} // copied from latest typinfo.pp
function AlignToPtr(p: pointer): pointer;
begin
  result := align(p,sizeof(p));
end;

function AlignTypeData(p: pointer): pointer;
{$packrecords c}
  type
    TAlignCheck = record // match RTTI TTypeInfo definition
      b : byte;    // = TTypeKind
      q : qword;   // = this is where the PTypeData begins
    end;
{$packrecords default}
begin
{$ifdef VER3_0}
  result := Pointer(align(p,SizeOf(Pointer)));
{$else VER3_0}
  result := Pointer(align(p,PtrInt(@TAlignCheck(nil^).q)))
{$endif VER3_0}
end;
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}

function GetFPCPropInfo(AClass: TClass; const PropName: string): PPropInfo;
begin
  result := TypInfo.GetPropInfo(AClass,PropName);
end;

{$ifdef FPC_NEWRTTI}
function GetFPCRecInitData(TypeData: Pointer): Pointer;
begin
  if PTypeData(TypeData)^.RecInitInfo = nil then
    result := TypeData else
    result := AlignTypeData(pointer(PTypeData(TypeData)^.RecInitData));
end;
{$endif FPC_NEWRTTI}

function IsStoredProp(Instance: TObject; PropInfo: pointer): boolean;
begin
  result := TypInfo.IsStoredProp(Instance,PropInfo);
end;

function GetOrdProp(Instance: TObject; PropInfo: pointer): Int64;
begin
  result := TypInfo.GetOrdProp(Instance,PropInfo);
end;

procedure SetOrdProp(Instance: TObject; PropInfo: pointer; Value: Int64);
begin
  TypInfo.SetOrdProp(Instance,PropInfo,Value);
end;

function GetFloatProp(Instance: TObject; PropInfo: pointer): Extended;
begin
  result := TypInfo.GetFloatProp(Instance,PropInfo);
end;

procedure SetFloatProp(Instance: TObject; PropInfo: pointer; const Value: Extended);
begin
  TypInfo.SetFloatProp(Instance,PropInfo,Value);
end;

function GetVariantProp(Instance: TObject; PropInfo: pointer): Variant;
begin
  result := TypInfo.GetVariantProp(Instance,PropInfo);
end;

procedure SetVariantProp(Instance: TObject; PropInfo: pointer; const Value: Variant);
begin
  TypInfo.SetVariantProp(Instance,PropInfo,Value);
end;

function GetStrProp(Instance: TObject; PropInfo: pointer): string;
begin
  result := TypInfo.GetStrProp(Instance,PropInfo);
end;

procedure SetStrProp(Instance: TObject; PropInfo: pointer; const Value: string);
begin
  TypInfo.SetStrProp(Instance,PropInfo,Value);
end;

function GetWideStrProp(Instance: TObject; PropInfo: pointer): WideString;
begin
  result := TypInfo.GetWideStrProp(Instance,PropInfo);
end;

procedure SetWideStrProp(Instance: TObject; PropInfo: pointer; const Value: WideString);
begin
  TypInfo.SetWideStrProp(Instance,PropInfo,Value);
end;

{$ifdef HASVARUSTRING}
function GetUnicodeStrProp(Instance: TObject; PropInfo: pointer): UnicodeString;
begin
  result := TypInfo.GetUnicodeStrProp(Instance,PropInfo);
end;

procedure SetUnicodeStrProp(Instance: TObject; PropInfo: pointer; const Value: UnicodeString);
begin
  TypInfo.SetUnicodeStrProp(Instance,PropInfo,Value);
end;
{$endif HASVARUSTRING}

end.
