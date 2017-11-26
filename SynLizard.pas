/// fast Lizard (LZ5) Compression routines (FPC only yet)
// - licensed under a MPL/GPL/LGPL tri-license; original Lizard is BSD 2-Clause
unit SynLizard;

{
    This file is part of Synopse Lizard Compression.

    Synopse Lizard Compression. Copyright (C) 2017 Arnaud Bouchez
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

  The Original Code is Synopse Lizard Compression.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2017
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

  *** BEGIN ORIGINAL LICENSE BLOCK *****
  Version: BSD 2-Clause License

    Lizard Library
  Copyright (C) 2011-2016, Yann Collet.
  Copyright (C) 2016-2017, Przemyslaw Skibinski <inikep@gmail.com>
  All rights reserved.

  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:

  * Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.

  * Redistributions in binary form must reproduce the above copyright notice, this
    list of conditions and the following disclaimer in the documentation and/or
    other materials provided with the distribution.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
  ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

   You can contact the author at :
    - Lizard source repository : https://github.com/inikep/lizard

  ***** END ORIGINAL LICENSE BLOCK *****

  Revision history

  Version 1.8
  - first release, associated with the main Synopse mORMot framework

  Some numbers, for a 53MB log file (taken from a production server):

  FPC Win32
       TAlgoSynLz 53 MB->5 MB: comp 563.7 MB/s decomp 815.3 MB/s
       TAlgoLizard 53 MB->3.9 MB: comp 54.6 MB/s decomp 1.1 GB/s
       TAlgoLizardFast 53 MB->6.9 MB: comp 493.8 MB/s decomp 1 GB/s
       TAlgoDeflate 53 MB->4.8 MB: comp 28.2 MB/s decomp 212 MB/s
       TAlgoDeflateFast 53 MB->7 MB: comp 58.6 MB/s decomp 182.5 MB/s
  FPC Win64
       TAlgoSynLz 53 MB->5 MB: comp 635.4 MB/s decomp 923.5 MB/s
       TAlgoLizard 53 MB->3.9 MB: comp 61 MB/s decomp 1.8 GB/s
       TAlgoLizardFast 53 MB->6.8 MB: comp 674.2 MB/s decomp 1.6 GB/s
       TAlgoDeflate 53 MB->4.8 MB: comp 40.2 MB/s decomp 255.1 MB/s
       TAlgoDeflateFast 53 MB->7 MB: comp 81.2 MB/s decomp 219.9 MB/s
  FPC Linux32
       TAlgoSynLz 53 MB->5 MB: comp 533.4 MB/s decomp 472.5 MB/s
       TAlgoLizard 53 MB->3.9 MB: comp 44.8 MB/s decomp 1.2 GB/s
       TAlgoLizardFast 53 MB->6.9 MB: comp 515.5 MB/s decomp 1 GB/s
       TAlgoDeflate 53 MB->4.8 MB: comp 60.2 MB/s decomp 413.3 MB/s
       TAlgoDeflateFast 53 MB->7 MB: comp 121.8 MB/s decomp 336.7 MB/s
  FPC Linux64
       TAlgoSynLz 53 MB->5 MB: comp 626.4 MB/s decomp 906.8 MB/s
       TAlgoLizard 53 MB->3.9 MB: comp 53.6 MB/s decomp 1.8 GB/s
       TAlgoLizardFast 53 MB->6.8 MB: comp 700.3 MB/s decomp 1.6 GB/s
       TAlgoDeflate 53 MB->4.8 MB: comp 70.5 MB/s decomp 544.5 MB/s
       TAlgoDeflateFast 53 MB->7 MB: comp 141.4 MB/s decomp 420.2 MB/s
  Conclusion: SynLZ has the best compression ratio for its compression speed,
    but Lizard is much faster at decompression

  NOTE:
  - FOR DELPHI PLEASE DOWNLOAD
    external Lizard1-32.dll/Lizard1-64.dll/liblizard.so.1
    from https://synopse.info/files/SynLizardLibs.7z

  WARNING: Lizard1-32.dll/Lizard1-64.dll files currently DON'T WORK :(
   -> use FPC for proper static linking
    
}

interface

{$I Synopse.inc}

{.$define LIZARD_EXTERNALONLY}
// will force to use an external Lizard1-32.dll/Lizard1-64.dll/liblizard.so.1
// as available from https://synopse.info/files/SynLizardLibs.7z

{$ifndef FPC}
  {$define LIZARD_EXTERNALONLY} // no static .obj for Delphi Win32/Win64 yet
{$endif}

{.$define LIZARD_STANDALONE}
/// if not defined (by default), will register Lizard as TAlgoCompress.AlgoID=4/5

uses
  {$ifndef LIZARD_STANDALONE}
  SynCommons, // TAlgoCompress definition
  {$endif LIZARD_STANDALONE}
  {$ifdef MSWINDOWS}
  Windows,
  {$endif}
  {$ifdef FPC}
  {$ifdef BSDNOTDARWIN}
  dl,
  {$else}
  DynLibs,
  {$endif BSDNOTDARWIN}
  {$endif FPC}
  SysUtils;

const
  LIZARD_MIN_CLEVEL = 10;
  LIZARD_MAX_CLEVEL = 49;
  LIZARD_DEFAULT_CLEVEL = 0; // equals level 17

{$ifdef Win32}
  LIZARD_LIB_NAME = 'Lizard1-32.dll';
{$endif Win32}
{$ifdef Win64}
  LIZARD_LIB_NAME = 'Lizard1-64.dll';
{$endif Win64}
{$ifdef Linux}
  LIZARD_LIB_NAME = 'liblizard.so.1';
{$endif Linux}

type
  /// Lizard (formerly LZ5) lossless compression algorithm
  // - provides efficient compression with very fast decompression
  TSynLizard = class
  public
    //// will initialize the library
    constructor Create; virtual;
  public
    /// version number of the linked Lizard library
    versionNumber: function: integer; cdecl;
    /// maximum size that Lizard compression may output in a "worst case" scenario
    compressBound: function(inputSize: integer): integer; cdecl;
    /// compresses srcSize bytes from src into already allocated dst buffer of
    // size maxDstSize - which should be >= Lizard_compressBound(srcSize)
    // - returns number of bytes written into dst (necessarily <= maxDstSize),
    // or 0 if compression fails due to too small maxDstSize, <0 on other failure
    // - compressionLevel is from LIZARD_MIN_CLEVEL (10) to LIZARD_MAX_CLEVEL(49),
    // any value <10 (e.g. 0) will use 17, >49 will use 49
    // $ Lev Comp     Decomp    CompSize Ratio
    // $    7332 MB/s 8719 MB/s 211947520 100.00 (move)
    // $ 10 346 MB/s  2610 MB/s 103402971 48.79
    // $ 12 103 MB/s  2458 MB/s 86232422 40.69
    // $ 15 50 MB/s   2552 MB/s 81187330 38.31
    // $ 19 3.04 MB/s 2497 MB/s 77416400 36.53
    // $ 21 157 MB/s  1795 MB/s 89239174 42.10
    // $ 23 30 MB/s   1778 MB/s 81097176 38.26
    // $ 26 6.63 MB/s 1734 MB/s 74503695 35.15
    // $ 29 1.37 MB/s 1634 MB/s 68694227 32.41
    // $ 30 246 MB/s  909 MB/s  85727429 40.45
    // $ 32 94 MB/s   1244 MB/s 76929454 36.30
    // $ 35 47 MB/s   1435 MB/s 73850400 34.84
    // $ 39 2.94 MB/s 1502 MB/s 69807522 32.94
    // $ 41 126 MB/s  961 MB/s  76100661 35.91
    // $ 43 28 MB/s   1101 MB/s 70955653 33.48
    // $ 46 6.25 MB/s 1073 MB/s 65413061 30.86
    // $ 49 1.27 MB/s 1064 MB/s 60679215 28.63
    compress: function(src, dst: pointer; srcSize, maxDstSize, compressionLevel: integer): integer; cdecl;
    /// how much memory must be allocated for compress_extState()
    sizeofState: function(compressionLevel: integer): integer; cdecl;
    /// compresses using an external pre-allocated state buffer
    compress_extState: function(state: pointer;
      src, dst: pointer; srcSize, maxDstSize, compressionLevel: integer): integer; cdecl;
    /// decompresses srcSize bytes from src into already allocated dst buffer
    // - returns number of bytes written to dst (<= maxDstSize), or <=0 on failure
    // - this function is protected against buffer overflow exploits
    decompress_safe: function(src, dst: pointer; srcSize, maxDstSize: integer): integer; cdecl;
    /// partial decompression srcSize bytes from src into already allocated dst buffer
    // - returns number of bytes written to dst (<= maxDstSize), or <=0 on failure
    // - number can be <targetDstSize should the compressed block to decode be smaller
    // - this function is protected against buffer overflow exploits
    decompress_safe_partial: function(src, dst: pointer; srcSize, targetDstSize: integer): integer; cdecl;
  end;

var
  /// direct access to the low-level Lizard (LZ5) library 
  // - is defined by default if Lizard was statically linked (under FPC)
  // - otherwise, you should execute explictly:
  // ! if Lizard = nil then
  // !   Lizard := TSynLizardDynamic.Create;
  Lizard: TSynLizard;

type
  /// try to load a LIZARD_LIB_NAME external library
  // - static linking is currently available only on FPC Win32/64 and Linux32/64
  // - this class is expected to access .dll files for Delphi - but it is NOT
  // WORKING YET, due to not good support of the Windows platform for Lizard :(
  TSynLizardDynamic = class(TSynLizard)
  protected
    {$ifdef FPC}
    fHandle: TLibHandle;
    {$else}
    fHandle: THandle;
    {$endif}
    fLibraryName: TFileName;
  public
    /// will first search in the executable folder, then within the system path
    constructor Create(const aLibraryFile: TFileName = ''); reintroduce;
    /// unload the external library
    destructor Destroy; override;
    /// the loaded library file name
    property LibraryName: TFileName read fLibraryName;
  end;

{$ifndef LIZARD_EXTERNALONLY}
type
  TSynLizardStatic = class(TSynLizard)
  public
    constructor Create;
  end;
{$endif LIZARD_EXTERNALONLY}

{$ifndef LIZARD_STANDALONE}
var
  /// implement Lizard compression in level 17 (LIZARD_DEFAULT_CLEVEL) as AlgoID=4
  // - is set by TSynLizard.Create, so available e.g. if library is statically linked
  AlgoLizard: TAlgoCompress;
  /// implement Lizard compression in level 10 (LIZARD_MIN_CLEVEL) as AlgoID=5
  // - is set by TSynLizard.Create, so available e.g. if library is statically linked
  AlgoLizardFast: TAlgoCompress;
{$endif LIZARD_STANDALONE}


implementation


{$ifndef LIZARD_EXTERNALONLY}

function Lizard_versionNumber: integer; cdecl; external; 
function Lizard_compressBound(inputSize: integer): integer; cdecl; external;
function Lizard_compress(src, dst: pointer; srcSize, maxDstSize, compressionLevel: integer): integer; cdecl; external;
function Lizard_sizeofState(compressionLevel: integer): integer; cdecl; external;
function Lizard_compress_extState(state: pointer;
  src, dst: pointer; srcSize, maxDstSize, compressionLevel: integer): integer; cdecl; external;
{
function Lizard_createStream(compressionLevel: integer): pointer; cdecl; external;
function Lizard_freeStream(streamPtr: pointer): integer; cdecl; external;
function Lizard_resetStream(streamPtr: pointer; compressionLevel: integer): pointer; cdecl; external;
function Lizard_loadDict(streamPtr, dictionary: pointer; dictSize: integer): integer; cdecl; external;
function Lizard_compress_continue(streamPtr: pointer; src, dst: pointer; srcSize, maxDstSize: integer): integer; cdecl; external;
function Lizard_saveDict(streamPtr, safeBuffer: pointer; dictSize: integer): integer; cdecl; external;
}
function Lizard_decompress_safe(src, dst: pointer; srcSize, maxDstSize: integer): integer; cdecl; external;
function Lizard_decompress_safe_partial(src, dst: pointer; srcSize, targetDstSize: integer): integer; cdecl; external;
{
function Lizard_createStreamDecode: pointer; cdecl; external;
function Lizard_freeStreamDecode(streamDec: pointer): integer; cdecl; external;
function Lizard_setStreamDecode(streamDec: pointer; dict: pointer; dictSize: integer): integer; cdecl; external;
function Lizard_decompress_safe_continue(streamDec, src, dst: pointer; srcSize, maxDstSize: integer): integer; cdecl; external;
function Lizard_decompress_safe_usingDict(src, dst: pointer; srcSize, maxDstSize: integer;
  dict: pointer; dictSize: integer): integer; cdecl; external;
}
{$ifdef CPUX64}
  {$ifdef FPC} // .o files don't work under Win64 for Delphi :(
    {$ifdef MSWINDOWS}
    {$L fpc-win64\lizard_compress.o}
    {$L fpc-win64\lizard_decompress.o}
    {$L fpc-win64\lizard_frame.o}
    {$L fpc-win64\huf_compress.o}
    {$L fpc-win64\huf_decompress.o}
    {$L fpc-win64\fse_compress.o}
    {$L fpc-win64\fse_decompress.o}
    {$L fpc-win64\entropy_common.o}
    {$L fpc-win64\xxhash.o}
    {$linklib fpc-win64\libgcc.a}
    {$linklib fpc-win64\libmsvcrt.a}
    {$else MSWINDOWS}
    {$L fpc-linux64/lizard_compress.o}
    {$L fpc-linux64/lizard_decompress.o}
    {$L fpc-linux64/lizard_frame.o}
    {$L fpc-linux64/huf_compress.o}
    {$L fpc-linux64/huf_decompress.o}
    {$L fpc-linux64/fse_compress.o}
    {$L fpc-linux64/fse_decompress.o}
    {$L fpc-linux64/entropy_common.o}
    {$L fpc-linux64/xxhash.o}
    {$endif MSWINDOWS}
  {$endif FPC}
{$endif CPUX64}

{$ifdef CPUX86}
  {$ifdef FPC}
    {$ifdef MSWINDOWS}
    {$L fpc-win32\lizard_compress.o}
    {$L fpc-win32\lizard_decompress.o}
    {$L fpc-win32\lizard_frame.o}
    {$L fpc-win32\huf_compress.o}
    {$L fpc-win32\huf_decompress.o}
    {$L fpc-win32\fse_compress.o}
    {$L fpc-win32\fse_decompress.o}
    {$L fpc-win32\entropy_common.o}
    {$L fpc-win32\xxhash.o}
    {$linklib fpc-win32\libgcc.a}
    {$linklib fpc-win32\libmsvcrt.a}
    {$else MSWINDOWS}
    {$L fpc-linux32/lizard_compress.o}
    {$L fpc-linux32/lizard_decompress.o}
    {$L fpc-linux32/lizard_frame.o}
    {$L fpc-linux32/huf_compress.o}
    {$L fpc-linux32/huf_decompress.o}
    {$L fpc-linux32/fse_compress.o}
    {$L fpc-linux32/fse_decompress.o}
    {$L fpc-linux32/entropy_common.o}
    {$L fpc-linux32/xxhash.o}
    {$linklib fpc-linux32\libgcc.a}
    {$endif MSWINDOWS}
  {$endif FPC}
{$endif CPUX86}

{ TSynLizardStatic }

constructor TSynLizardStatic.Create;
begin
  versionNumber := Lizard_versionNumber;
  compressbound := Lizard_compressbound;
  compress := Lizard_compress;
  sizeofState := Lizard_sizeofState;
  compress_extState := Lizard_compress_extState;
  decompress_safe := Lizard_decompress_safe;
  decompress_safe_partial := Lizard_decompress_safe_partial;
  inherited Create; // register AlgoLizard/AlgoLizardFast
end;

{$endif LIZARD_EXTERNALONLY}


{$ifndef LIZARD_STANDALONE}
type
  TAlgoLizard = class(TAlgoCompressWithNoDestLen)
  protected
    fCompressionLevel: integer;
    function RawProcess(src,dst: pointer; srcLen,dstLen: integer;
      process: TAlgoCompressWithNoDestLenProcess): integer;  override;
  public
    constructor Create; override;
    function AlgoID: byte; override;
    function AlgoCompressDestLen(PlainLen: integer): integer; override;
  end;

  TAlgoLizardFast = class(TAlgoLizard)
  public
    constructor Create; override;
    function AlgoID: byte; override;
  end;

{ TAlgoLizard }

function TAlgoLizard.AlgoID: byte;
begin
  result := 4;
end;

constructor TAlgoLizard.Create;
begin
  inherited Create;
  fCompressionLevel := LIZARD_DEFAULT_CLEVEL;
end;

function TAlgoLizard.AlgoCompressDestLen(PlainLen: integer): integer;
begin
  if Lizard = nil then
    result := 0
  else
    result := Lizard.compressBound(PlainLen);
end;

function TAlgoLizard.RawProcess(src, dst: pointer; srcLen, dstLen: integer;
  process: TAlgoCompressWithNoDestLenProcess): integer;
begin
  if Lizard = nil then
    result := 0
  else
  case process of
  doCompress:
    result := Lizard.compress(src,dst,srcLen,dstLen,fCompressionLevel);
  doUnCompress:
    result := Lizard.decompress_safe(src,dst,srcLen,dstLen);
  doUncompressPartial:
    result := Lizard.decompress_safe_partial(src,dst,srcLen,dstLen);
  else
    result := 0;
  end;
end;


{ TAlgoLizardFast }

constructor TAlgoLizardFast.Create;
begin
  inherited Create;
  fCompressionLevel := LIZARD_MIN_CLEVEL;
end;

function TAlgoLizardFast.AlgoID: byte;
begin
  result := 5;
end;

{$endif LIZARD_STANDALONE}

{ TSynLizard }

constructor TSynLizard.Create;
begin
  {$ifndef LIZARD_STANDALONE}
  if AlgoLizard = nil then
    AlgoLizard := TAlgoLizard.Create;
  if AlgoLizardFast = nil then
    AlgoLizardFast := TAlgoLizardFast.Create;
  {$endif LIZARD_STANDALONE}
end;


{ TSynLizardDynamic }

const
  LIZARD_ENTRIES: array[0..6] of TFileName =
  ('versionNumber', 'compressbound', 'compress', 'sizeofState',
   'compress_extState', 'decompress_safe', 'decompress_safe_partial');

constructor TSynLizardDynamic.Create(const aLibraryFile: TFileName);
var P: PPointerArray;
    i: integer;
begin
  fLibraryName := aLibraryFile;
  if fLibraryName = '' then begin
    fLibraryName := ExtractFilePath(ParamStr(0)) + LIZARD_LIB_NAME;
    if not FileExists(fLibraryName) then
      fLibraryName := LIZARD_LIB_NAME;
  end;
  {$ifdef MSWINDOWS}
  fHandle := SafeLoadLibrary(fLibraryName);
  if fHandle=0 then
  {$else}
    {$ifdef BSDNOTDARWIN}
    fHandle := dlopen(PChar(fLibraryName),0);
    if fHandle=nil then
    {$else}
    fHandle := LoadLibrary({$ifndef FPC}pointer{$endif}(fLibraryName));
    if fHandle=0 then
    {$endif}
  {$endif MSWINDOWS}
    raise Exception.CreateFmt('Unable to load %s - %s/'#13#10 +
      'Please download from https://synopse.info/files/SynLizardLibs.7z',
      [fLibraryName, {$ifdef MSWINDOWS}SysErrorMessage(GetLastError){$else}''{$endif}]);
  P := @@versionNumber;
  for i := 0 to High(LIZARD_ENTRIES) do begin
    P^[i] := {$ifdef BSDNOTDARWIN}dlsym{$else}GetProcAddress{$endif}(
      fHandle,PChar('Lizard_'+LIZARD_ENTRIES[i]));
    if not Assigned(P^[i]) then
      raise Exception.CreateFmt('Unable to load %s - Missing Lizard_%s',
        [fLibraryName,LIZARD_ENTRIES[i]]);
  end;
  inherited Create; // register AlgoLizard/AlgoLizardFast
end;

destructor TSynLizardDynamic.Destroy;
begin
  {$ifdef BSDNOTDARWIN}
  if fHandle<>nil then
    dlclose(fHandle);
  {$else}
  if fHandle<>0 then
    FreeLibrary(fHandle);
  {$endif}
  inherited;
end;

initialization
  {$ifndef LIZARD_EXTERNALONLY}
  Lizard := TSynLizardStatic.Create;
  {$endif LIZARD_EXTERNALONLY}

finalization
  Lizard.Free;

end.

