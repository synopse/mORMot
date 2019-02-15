/// implements a heap manager that uses glibc, with proper 16-bytes alignment
// - expected to be enabled with -dFPC_NO_DEFAULT_MEMORYMANAGER -dFPC_SYNCMEM
unit SynFPCCMemAligned;

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

}

interface

{$I Synopse.inc} // set proper flags, and define LINUX for BSD and ANDROID

{$ifndef FPC}
  THIS UNIT IS FOR FPC ONLY !
{$endif FPC}

implementation

const
  // default FPC's cmem unit is SizeOf(PtrUInt) which breaks memory alignment
  CMEM_ALIGN_BYTES = 16;

// low-level direct calls to the external glibc library

function  malloc(size: PtrUInt): pointer; cdecl; external 'c' name 'malloc';
procedure free(p: pointer); cdecl; external 'c' name 'free';
function  realloc(p: pointer; size: PtrUInt): pointer; cdecl; external 'c' name 'realloc';

// TMemoryManager replacement

function _GetMem(size: PtrUInt): pointer;
begin
  result := malloc(size + CMEM_ALIGN_BYTES);
  if result <> nil then begin
    PPtrUInt(result)^ := size;
    inc(result, CMEM_ALIGN_BYTES);
  end;
end;

function _FreeMem(p: pointer): PtrUInt;
begin
  if p <> nil then begin
    dec(p, CMEM_ALIGN_BYTES);
    free(p);
  end;
  result := 0;
end;

function _FreeMemSize(p: pointer; size: PtrUInt): PtrUInt;
begin
  if (size <> 0) and (p <> nil) then begin
    dec(p, CMEM_ALIGN_BYTES);
    if size <> PPtrUInt(p)^ then
      runerror(204)
    else
      free(p);
  end;
  result := 0;
end;

function _AllocMem(size: PtrUInt): pointer;
begin
  result := _GetMem(size);
  if result <> nil then
    FillChar(result^, size, 0);
end;

function _ReAllocMem(var p: pointer; size: PtrUInt): pointer;
begin
  result := p;
  if size = 0 then begin
    if result <> nil then begin
      dec(result, CMEM_ALIGN_BYTES);
      free(result);
      p := nil;
    end;
  end
  else begin
    if result = nil then
      result := malloc(size + CMEM_ALIGN_BYTES)
    else begin
      dec(result, CMEM_ALIGN_BYTES);
      result := realloc(result, size + CMEM_ALIGN_BYTES);
    end;
    if result <> nil then begin
      PPtrUInt(result)^ := size;
      inc(result, CMEM_ALIGN_BYTES);
    end;
    p := result;
  end;
end;

function _MemSize(p: pointer): PtrUInt;
begin
  if p = nil then
    result := 0
  else
    result := PPtrUInt(p - CMEM_ALIGN_BYTES)^;
end;

function _GetHeapStatus: THeapStatus;
begin
  fillchar(result, sizeof(result), 0);
end;

function _GetFPCHeapStatus: TFPCHeapStatus;
begin
  fillchar(result, sizeof(result), 0);
end;

const
  NewMM: TMemoryManager = (
    NeedLock: false;
    GetMem: @_Getmem;
    FreeMem: @_FreeMem;
    FreememSize: @_FreememSize;
    AllocMem: @_AllocMem;
    ReallocMem: @_ReAllocMem;
    MemSize: @_MemSize;
    InitThread: nil;
    DoneThread: nil;
    RelocateHeap: nil;
    GetHeapStatus: @_GetHeapStatus;
    GetFPCHeapStatus: @_GetFPCHeapStatus);

var
  OldMM: TMemoryManager;

initialization
  GetMemoryManager(OldMM);
  SetMemoryManager(NewMM);

finalization
  SetMemoryManager(OldMM);
end.

