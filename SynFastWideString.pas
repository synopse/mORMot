// this unit will patch the System.pas RTL to use a custom NON OLE COMPATIBLE
// WideString type, NOT using the slow Windows API, but FastMM4 (without COW) 
unit SynFastWideString;

interface

{
    This file is part of Synopse Framework.

    Synopse Framework. Copyright (C) 2014 Arnaud Bouchez
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

  The Original Code is Synopse Framework.

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

  DISCLAIMER:

   Purpose of this unit is to patch the system.pas unit for older versions
   of Delphi, so that WideString memory allocation would use FastMM4 instead
   of the slow BSTR Windows API.

   It will speed up the WideString process a lot, especially when a lot of
   content is allocated, since FastMM4 is much aggresive.

   The WideString implementation pattern does NOT feature Copy-On-Write, so is
   slower than the string=UnicodeString type as implemented since Delphi 2009.
   This is the reason why this unit won't do anything on Unicode versions of
   the compiler, since the new string type is to be preferred.

  HOW TO USE:

   Just add the unit at the TOP of your .dpr uses clause, just after FastMM4
   (if you use it, and you should!) i.e. before all other units useed by your
   program. It should be initialized before any WideString is allocated.
   
   Then the patch will be applied at runtime. Nothing to recompile!

  WARNING:
      -----------------------------------------------------------------
        USING THIS UNIT WILL BREAK COMPATIBILITY WITH OLE/COM LIBRARIES !
      -----------------------------------------------------------------
   You won't be able to share WideString/BSTR variables with a dll, e.g. an
   OleDB / ADO database provider, or any COM object.
   Do not use this unit if you are calling such external call!
   It is for educational purpose only, and/or if you are 100% sure that your
   code will stay self-contained, under Delphi 7 or Delphi 2007, and need use
   of WideString instead of string=AnsiString.
   
   YOU HAVE BEEN WARNED - USE AT YOUR OWN RISK !   :)

}


implementation

{$ifndef UNICODE}
// since Delphi 2009, use string=UnicodeString type, which features CopyOnWrite

uses
  Windows;

{$RANGECHECKS OFF}
{$STACKFRAMES OFF}
{$OPTIMIZATION ON}

type // some types here since we do not want any dependency on any other units
  PByteArray = ^TByteArray;
  TByteArray = array[0..MaxInt-1] of byte;

// we need to patch the oleaut32.dll library calls as defined in System.pas
// -> retrieve CALL address from low-level funtions asm, then the API slot 

function _SysAllocStringLen: pointer;
asm
  lea eax,System.@WStrFromPWCharLen+8
end;

function _SysReAllocStringLen: pointer;
asm
  lea eax,System.@WStrAsg+8
end;

function _SysFreeString: pointer;
asm
  lea eax,System.@WStrClr+8
end;

procedure PatchAPI(source,dest: pointer);
var RestoreProtection,Ignore: DWORD;
begin
  while PByte(source)^<>$e8 do // search first CALL within the function code
    inc(PByte(source));
  inc(PByte(source));
  source := pointer(integer(source)+SizeOf(pointer)+PInteger(source)^);
  if PWord(source)^<>$25ff then // "jmp dword ptr []" API redirection
    halt;
  inc(PWord(source));
  source := PPointer(source)^;  // get address from dword ptr 
  if VirtualProtect(source,SizeOf(source),PAGE_EXECUTE_READWRITE,RestoreProtection) then begin
    PPointer(source)^ := dest;
    VirtualProtect(source,SizeOf(source),RestoreProtection,Ignore);
    FlushInstructionCache(GetCurrentProcess,source,SizeOf(source));
  end;
end;


// those are the 3 redirected API calls -> just use AnsiString for allocation

function SysAllocStringLen(P: pointer; Len: integer): pointer; stdcall;
begin
  result := nil;
  Len := Len*2;
  SetString(AnsiString(result),PAnsiChar(P),Len);
  PByteArray(result)[Len+1] := 0; // ensure finishes with a #0 WideChar
end;

function SysReAllocStringLen(var S: pointer; P: PWideChar; Len: integer): LongBool; stdcall;
begin
  Len := Len*2;
  SetString(AnsiString(S),PAnsiChar(P),Len);
  PByteArray(S)[Len+1] := 0; // ensure finishes with a #0 WideChar
  result := true;
end;

procedure SysFreeString(S: pointer); stdcall;
begin
  AnsiString(s) := '';
end;


initialization
  PatchAPI(_SysAllocStringLen,@SysAllocStringLen);
  PatchAPI(_SysReAllocStringLen,@SysReAllocStringLen);
  PatchAPI(_SysFreeString,@SysFreeString);

{$endif UNICODE}

end.