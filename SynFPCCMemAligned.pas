/// implements a Linux FPC heap manager that uses glibc, with no overhead
// - expected to be enabled with -dFPC_NO_DEFAULT_MEMORYMANAGER -dFPC_SYNCMEM
// - with Linux glibc, alignment is 2*SizeOf(pointer) i.e. 16 bytes under x86_64
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

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2019
  the Initial Developer. All Rights Reserved.

  Contributor(s):


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
{$ifndef LINUXNOTBSD}
  THIS UNIT IS FOR FPC/LINUX ONLY !
   - requires malloc_usable_size()
{$endif LINUXNOTBSD}

implementation

// low-level direct calls to the external glibc library

function malloc(size: PtrUInt): pointer; cdecl; external 'c' name 'malloc';
function calloc(count, size: PtrUInt): pointer; cdecl; external 'c' name 'calloc';
procedure free(p: pointer); cdecl; external 'c' name 'free';
function realloc(p: pointer; size: PtrUInt): pointer; cdecl; external 'c' name 'realloc';

function malloc_usable_size(p: pointer): PtrUInt; cdecl; external 'c' name 'malloc_usable_size';
// function missing on some platforms, so this unit is enabled only for LINUXNOTBSD
// see https://www.gnu.org/software/gnulib/manual/html_node/malloc_005fusable_005fsize.html
// = Mac OS X 10.5, FreeBSD 6.0, NetBSD 5.0, OpenBSD 3.8, Minix 3.1.8, AIX 5.1, HP-UX 11.00,
// IRIX 6.5, OSF/1 5.1, Solaris 11.3, mingw, MSVC 14, Interix 3.5, BeOS, Android 4.1

// TMemoryManager replacement

function _GetMem(size: PtrUInt): pointer;
begin
  result := malloc(size);
end;

function _FreeMem(p: pointer): PtrUInt;
begin
  if p <> nil then
    free(p);
  result := 0;
end;

function _FreeMemSize(p: pointer; size: PtrUInt): PtrUInt;
begin // our library won't check the "size" value
  if p <> nil then
    free(p);
  result := 0;
end;

function _AllocMem(size: PtrUInt): pointer;
begin
  result := calloc(size, 1); // no need to call FillChar() e.g. from mmap
end;

function _ReAllocMem(var p: pointer; size: PtrUInt): pointer;
begin
  result := realloc(p, size); // is free(p) if size=0 or malloc(size) if p=nil
  p := result;
end;

function _MemSize(p: pointer): PtrUInt;
begin // AFAIK used only by fpc_AnsiStr_SetLength() in RTL
  result := malloc_usable_size(p);
end;

function _GetHeapStatus: THeapStatus;
begin
  FillChar(result, sizeof(result), 0);
end;

function _GetFPCHeapStatus: TFPCHeapStatus;
begin
  FillChar(result, sizeof(result), 0);
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

