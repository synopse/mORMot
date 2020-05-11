/// Fast Memory Manager for FPC x86_64
// - this unit is a part of the freeware Synopse mORMot framework
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit SynFPCx64MM;

{
  *****************************************************************************

    A Multi-thread Friendly Memory Manager for FPC written in x86_64 assembly
    - based on FastMM4 proven algorithms by Pierre le Riche
    - targetting Windows and Linux multi-threaded Services
    - only for FPC on the x86_64 target - use the original heap on Delphi or ARM
    - code has been reduced to the only necessary featureset for production
    - huge asm refactoring for cross-platform, compactness and efficiency
    - detailed statistics gathering (also about threads contention)
    - mremap() makes large block ReallocMem a breeze on Linux :)
    - round-robin of tiny blocks (<=128 bytes) for better thread scaling

    Usage: include this unit as the very first in your FPC project uses clause

    Why another Memory Manager on FPC?
    - The built-in heap.inc is well written and cross-platform and cross-CPU,
      but its threadvar arena for small blocks tends to consume a lot of memory
      on multi-threaded servers, and has suboptimal allocation performance
    - C memory managers (glibc, Intel TBB, jemalloc) have a very high RAM
      consumption (especially Intel TBB) and panic/SIGKILL on any GPF
    - Pascal alternatives (FastMM4,ScaleMM2,BrainMM) are Windows+Delphi specific
    - It was so fun deeping into SSE2 x86_64 assembly and Pierre's insight
    - Resulting code is still easy to understand and maintain

    IMPORTANT NOTICE: only tested on-site - feedback is (very) welcome!

  *****************************************************************************

    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2020 Arnaud Bouchez
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

  Portions created by the Initial Developer are Copyright (C) 2020
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

}

interface

// if defined, includes more detailed information to WriteHeapStatus()
{.$define FPCMM_DEBUG}

// if defined, won't check the IsMultiThread global but assume it is true
// - should be enabled e.g. for a multi-threaded Server Daemon instance
{.$define FPCMM_ASSUMEMULTITHREAD}

// if defined, tiny blocks <= 128 bytes will have a bigger round-robin cycle
// - try to enable it if unexpected SmallGetmemSleepCount/SmallFreememSleepCount
// and SleepCount/SleepTime conentions are reported by CurrentHeapStatus
// - this will use 4x more arenas to share among the threads
// - warning: depending on the workload and hardware, it may actually be slower
{.$define FPCMM_BOOST}
{.$define FPCMM_BOOSTER}

  {$ifdef FPCMM_BOOST}
    {$undef FPCMM_DEBUG} // when performance matters more than stats
    {$define FPCMM_ASSUMEMULTITHREAD}
  {$endif FPCMM_BOOST}

// if defined, will export libc-like functions, and not replace the FPC MM
// - e.g. to use this unit as a stand-alone C memory allocator
{.$define FPCMM_STANDALONE}

{$ifdef FPC}
  // cut-down version of Synopse.inc to make this unit standalone
  {$mode Delphi}
  {$asmmode Intel}
  {$inline on}
  {$R-} // disable Range checking in our code
  {$S-} // disable Stack checking in our code
  {$W-} // disable stack frame generation
  {$Q-} // disable overflow checking in our code
  {$B-} // expect short circuit boolean
  {$ifdef CPUX64}
    {$define FPC_CPUX64} // this unit is for FPC + x86_64 only
  {$endif CPUX64}
{$endif FPC}


{$ifdef FPC_CPUX64}

type
  /// Arena (middle/large) heap information as returned by CurrentHeapStatus
  TMMStatusArena = record
    /// how many bytes are currently reserved (mmap) to the Operating System
    CurrentBytes: PtrUInt;
    /// how many bytes have been reserved (mmap) to the Operating System
    CumulativeBytes: PtrUInt;
    {$ifdef FPCMM_DEBUG}
    /// maximum bytes count reserved (mmap) to the Operating System
    PeakBytes: PtrUInt;
    /// how many VirtualAlloc/mmap calls to the Operating System did occur
    CumulativeAlloc: PtrUInt;
    /// how many VirtualFree/munmap calls to the Operating System did occur
    CumulativeFree: PtrUInt;
    {$endif FPCMM_DEBUG}
    /// how many times this Arena did wait from been unlocked by another thread
    SleepCount: PtrInt;
  end;

  /// heap information as returned by CurrentHeapStatus
  TMMStatus = record
    /// contain blocks up to 256KB (small and medium blocks)
    Medium: TMMStatusArena;
    /// large blocks > 256KB which are directly handled by the Operating System
    Large: TMMStatusArena;
    /// how many times the Operating System Sleep/NanoSleep API was called
    // - in a perfect world, should be as small as possible
    SleepCount: PtrUInt;
    {$ifdef FPCMM_DEBUG}
    /// how much MicroSeconds was spend within Sleep/NanoSleep API calls
    // - under Windows, is not an exact, but only indicative value
    SleepTime: PtrUInt;
    {$endif FPCMM_DEBUG}
    /// how many times Getmem() did block and wait for a small block
    // - see also GetSmallBlockContention()
    SmallGetmemSleepCount: PtrInt;
    /// how many times Freemem() did block and wait for a small block
    // - see also GetSmallBlockContention()
    SmallFreememSleepCount: PtrInt;
  end;
  PMMStatus = ^TMMStatus;


{$ifdef FPCMM_STANDALONE}

/// should be called before using any memory function
procedure InitializeMemoryManager;

/// allocate a new memory buffer
function _GetMem(size: PtrInt): pointer;

/// allocate a new zeroed memory buffer
function _AllocMem(Size: PtrInt): pointer;

/// release a memory buffer
function _FreeMem(P: pointer): PtrInt;

/// change the size of a memory buffer
function _ReallocMem(var P: pointer; Size: PtrInt): pointer;

/// retrieve the maximum size (i.e. the allocated size) of a memory buffer
function _MemSize(P: pointer): PtrUInt;

/// retrieve high-level statistics about the current memory manager state
function CurrentHeapStatus: TMMStatus;

/// should be called to finalize this memory manager process and release all RAM
procedure FreeAllMemory;

{$undef FPCMM_DEBUG} // excluded FPC-specific debugging

{$else}

  /// one GetSmallBlockContention info about unexpected multi-thread waiting
  // - a single GetmemSleepCountSmallBlockSize or
  // FreememSleepCountSmallBlockSize non 0 field is set
  TSmallBlockContention = packed record
    /// the small block size on which Getmem() has been blocked - or 0
    GetmemSleepCountSmallBlockSize: word;
    /// the small block size on which Freemem() has been blocked - or 0
    FreememSleepCountSmallBlockSize: word;
    /// how many times a small block getmem/freemem has been waiting for unlock
    SleepCount: cardinal;
  end;

  /// small blocks detailed information as returned GetSmallBlockContention
  TSmallBlockContentionDynArray = array of TSmallBlockContention;


/// retrieve high-level statistics about the current memory manager state
// - see also GetSmallBlockContention for detailed small blocks information
function CurrentHeapStatus: TMMStatus;

/// retrieve all small blocks which suffered from blocking during multi-thread
// - results are sorted by SleepCount occurence
// - optionally retrive the total contention counters for getmem/freemem
function GetSmallBlockContention(GetMemTotal: PPtrInt = nil;
  FreeMemTotal: PPtrInt = nil): TSmallBlockContentionDynArray;

/// convenient debugging function into the console
// - if smallblockcontentioncount > 0, includes GetSmallBlockContention() info
// up to the smallblockcontentioncount biggest occurences
procedure WriteHeapStatus(const context: shortstring = '';
  smallblockcontentioncount: integer = 0);

{$endif FPCMM_STANDALONE}

{$endif FPC_CPUX64}



implementation

{
   High-level Algorithms Description
  -----------------------------------

  The allocator handles the following families of memory blocks:
  - TINY   <= 128 B (or <= 256 B for FPCMM_BOOST) - not existing in FastMM4
    Round-robin distribution into several arenas, fed from medium blocks
  - SMALL  <= 2600 B
    Single arena per block size, fed from medium blocks
  - MEDIUM <= 256 KB
    Pool of bitmap-marked chunks, fed from 1MB of OS mmap/virtualalloc
  - LARGE  > 256 KB
    Directly fed from OS mmap/virtualalloc with mremap when growing

  About locking:
  - Tiny and Small blocks have their own per-size lock, in every arena
  - Medium and Large blocks have one giant lock each
  - ThreadSwitch/FpNanoSleep OS call is done after initial spinning
  - FPCMM_DEBUG / WriteHeapStatus allows to identify the lock contention

}

{$ifdef FPC_CPUX64}
// this unit is available only for FPC + X86_64 CPU


{ ********* Operating System Specific API Calls }

{$ifdef MSWINDOWS}

var
  HeapStatus: TMMStatus;

const
  kernel32 = 'kernel32.dll';

  MEM_COMMIT = $1000;
  MEM_RESERVE  = $2000;
  MEM_RELEASE = $8000;
  MEM_FREE = $10000;
  MEM_TOP_DOWN = $100000;

  PAGE_READWRITE = 4;

function VirtualAlloc(lpAddress: pointer;
   dwSize: PtrUInt; flAllocationType, flProtect: Cardinal): pointer; stdcall;
  external kernel32 name 'VirtualAlloc';
function VirtualFree(lpAddress: pointer; dwSize: PtrUInt;
   dwFreeType: Cardinal): LongBool; stdcall;
  external kernel32 name 'VirtualFree';
procedure SwitchToThread; stdcall;
  external kernel32 name 'SwitchToThread';
procedure SleepMS(dwMilliseconds: Cardinal); stdcall;
  external kernel32 name 'Sleep';

procedure ReleaseCore;
begin
  SwitchToThread;
  inc(HeapStatus.SleepCount);
  {$ifdef FPCMM_DEBUG}
  inc(HeapStatus.SleepTime, 100); // wild guess to have some debug info
  {$endif FPCMM_DEBUG}
end;

function AllocMedium(Size: PtrInt): pointer; inline;
begin
  result := VirtualAlloc(nil, Size, MEM_COMMIT, PAGE_READWRITE);
end;

function AllocLarge(Size: PtrInt): pointer; inline;
begin
  result := VirtualAlloc(nil, Size, MEM_COMMIT or MEM_TOP_DOWN, PAGE_READWRITE);
end;

function ReallocLarge(ptr: pointer; OldSize, NewSize: PtrInt): pointer;
begin
  Error(reOutOfMemory); // not available on this platform
  result := nil;
end;

procedure Free(ptr: pointer; Size: PtrInt); inline;
begin
  VirtualFree(ptr, 0, MEM_RELEASE);
end;

{$else}

uses
  {$ifndef DARWIN}
  syscall,
  {$endif DARWIN}
  BaseUnix;

var
  HeapStatus: TMMStatus;

// we directly call the Kernel, so this unit doesn't require any libc

function AllocMedium(Size: PtrInt): pointer; inline;
begin
  result := fpmmap(nil, Size, PROT_READ or PROT_WRITE,
    MAP_PRIVATE or MAP_ANONYMOUS, -1, 0);
end;

function AllocLarge(Size: PtrInt): pointer; inline;
begin
  result := fpmmap(nil, Size, PROT_READ or PROT_WRITE,
    MAP_PRIVATE or MAP_ANONYMOUS, -1, 0);
end;

procedure Free(ptr: pointer; Size: PtrInt); inline;
begin
  Size := fpmunmap(ptr, Size);
  //assert(Size = 0);
end;

{$ifdef LINUX}

const
  syscall_nr_mremap = 25;
  MREMAP_MAYMOVE = 1;
  CLOCK_MONOTONIC = 1;

function fpmremap(addr: pointer; old_len, new_len: size_t; may_move: longint): pointer; inline;
begin
  result := pointer(do_syscall(syscall_nr_mremap, TSysParam(addr),
    TSysParam(old_len), TSysParam(new_len), TSysParam(may_move)));
end;

function ReallocLarge(ptr: pointer; OldSize, NewSize: PtrInt): pointer; inline;
begin
  result := fpmremap(ptr, OldSize, NewSize, MREMAP_MAYMOVE);
  //assert(result <> pointer(-1));
end;

{$else}

const
  {$ifdef OPENBSD}
  CLOCK_MONOTONIC = 3;
  {$else}
  CLOCK_MONOTONIC = 4;
  {$endif OPENBSD}

function ReallocLarge(ptr: pointer; OldSize, NewSize: PtrInt): pointer;
begin
  Error(reOutOfMemory); // not available on this platform
  result := nil;
end;

{$endif LINUX}

procedure NSleep(nsec: PtrInt); inline;
var
  t: Ttimespec;
begin
  // note: nanosleep() adds a few dozen of microsecs for context switching
  t.tv_sec := 0;
  t.tv_nsec := nsec;
  fpnanosleep(@t, nil);
end;

{$ifdef DARWIN}

function QueryPerformanceMicroSeconds: Int64; inline;
begin
  result := 0;
end;

{$else}

function clock_gettime(clk_id: integer; tp: ptimespec): integer; inline;
begin
  // calling the libc may be slightly faster thanks to vDSO but not here
  result := do_SysCall(syscall_nr_clock_gettime, tsysparam(clk_id), tsysparam(tp));
end;

function QueryPerformanceMicroSeconds: PtrUInt; inline;
var
  r : TTimeSpec;
begin
  if clock_gettime(CLOCK_MONOTONIC, @r) = 0 then
    result := PtrUInt(r.tv_nsec) div 1000 + PtrUInt(r.tv_sec) * 1000000
  else
    result := 0;
end;

{$endif DARWIN}

const
  // empirically identified as a convenient value with a recent Linux Kernel
  NANOSLEEP = 10;

{$ifdef FPCMM_DEBUG}

procedure SleepSetTime(start: QWord); nostackframe; assembler;
asm
  push start
  call QueryPerformanceMicroSeconds
  pop rcx
  lea rdx, [rip + HeapStatus]
  sub rax, rcx
lock xadd qword ptr[rdx + TMMStatus.SleepTime], rax
lock inc qword ptr[rdx + TMMStatus.SleepCount]
end;

procedure ReleaseCore;
var
  start: QWord;
begin
  start := QueryPerformanceMicroSeconds; // is part of the wait
  NSleep(NANOSLEEP); // similar to ThreadSwitch()
  SleepSetTime(start);
end;

{$else}

procedure ReleaseCore;
begin
  NSleep(NANOSLEEP); // similar to ThreadSwitch()
  inc(HeapStatus.SleepCount); // indicative counter
end;

{$endif FPCMM_DEBUG}

{$endif MSWINDOWS}


{ ********* Some Assembly Helpers }

procedure NotifyAlloc(var Arena: TMMStatusArena; Size: PtrUInt);
  nostackframe; assembler;
asm
     mov  rax, Size
lock xadd qword ptr[Arena].TMMStatusArena.CurrentBytes, rax
lock xadd qword ptr[Arena].TMMStatusArena.CumulativeBytes, Size
     {$ifdef FPCMM_DEBUG}
lock inc  qword ptr[Arena].TMMStatusArena.CumulativeAlloc
     mov  rax, qword ptr[Arena].TMMStatusArena.CurrentBytes
     cmp  rax, qword ptr[Arena].TMMStatusArena.PeakBytes
     jbe  @s
     mov  qword ptr[Arena].TMMStatusArena.PeakBytes, rax
@s:  {$endif FPCMM_DEBUG}
end;

procedure NotifyFree(var Arena: TMMStatusArena; Size: PtrUInt);
  nostackframe; assembler;
asm
     neg Size
lock xadd qword ptr[Arena].TMMStatusArena.CurrentBytes, Size
     {$ifdef FPCMM_DEBUG}
lock inc  qword ptr[Arena].TMMStatusArena.CumulativeFree
     {$endif FPCMM_DEBUG}
end;


// Move## are called from asm with src=rcx dst=rdx len=r8 even on POSIX ABI

{$WARN 7122 off : Check size of memory operand }

procedure Move8; nostackframe; assembler;
asm
      mov rax, [rcx]
      mov [rdx], rax
end;

procedure Move24; nostackframe; assembler;
asm
      movaps xmm0, oword ptr [rcx]
      mov rax, [rcx + 16]
      movaps oword ptr [rdx], xmm0
      mov [rdx + 16], rax
end;

procedure Move40; nostackframe; assembler;
asm
      movaps xmm0, oword ptr [rcx]
      movaps xmm1, oword ptr [rcx + 16]
      mov rax, [rcx + 32]
      movaps oword ptr [rdx], xmm0
      movaps oword ptr [rdx + 16], xmm1
      mov [rdx + 32], rax
end;

procedure Move56; nostackframe; assembler;
asm
      movaps xmm0, oword ptr [rcx]
      movaps xmm1, oword ptr [rcx + 16]
      movaps xmm2, oword ptr [rcx + 32]
      mov rax, [rcx + 48]
      movaps oword ptr [rdx], xmm0
      movaps oword ptr [rdx + 16], xmm1
      movaps oword ptr [rdx + 32], xmm2
      mov [rdx + 48], rax
end;

procedure Move72; nostackframe; assembler;
asm
      movaps xmm0, oword ptr [rcx]
      movaps xmm1, oword ptr [rcx + 16]
      movaps xmm2, oword ptr [rcx + 32]
      movaps xmm3, oword ptr [rcx + 48]
      mov rax, [rcx + 64]
      movaps oword ptr [rdx], xmm0
      movaps oword ptr [rdx + 16], xmm1
      movaps oword ptr [rdx + 32], xmm2
      movaps oword ptr [rdx + 48], xmm3
      mov [rdx + 64], rax
end;

procedure Move88; nostackframe; assembler;
asm
      movaps xmm0, oword ptr [rcx]
      movaps xmm1, oword ptr [rcx + 16]
      movaps xmm2, oword ptr [rcx + 32]
      movaps xmm3, oword ptr [rcx + 48]
      movaps xmm4, oword ptr [rcx + 64]
      mov rax, [rcx + 80]
      movaps oword ptr [rdx], xmm0
      movaps oword ptr [rdx + 16], xmm1
      movaps oword ptr [rdx + 32], xmm2
      movaps oword ptr [rdx + 48], xmm3
      movaps oword ptr [rdx + 64], xmm4
      mov [rdx + 80], rax
end;

procedure Move104; nostackframe; assembler;
asm
      movaps xmm0, oword ptr [rcx]
      movaps xmm1, oword ptr [rcx + 16]
      movaps xmm2, oword ptr [rcx + 32]
      movaps xmm3, oword ptr [rcx + 48]
      movaps xmm4, oword ptr [rcx + 64]
      movaps xmm5, oword ptr [rcx + 80]
      mov rax, [rcx + 96]
      movaps oword ptr [rdx], xmm0
      movaps oword ptr [rdx + 16], xmm1
      movaps oword ptr [rdx + 32], xmm2
      movaps oword ptr [rdx + 48], xmm3
      movaps oword ptr [rdx + 64], xmm4
      movaps oword ptr [rdx + 80], xmm5
      mov [rdx + 96], rax
end;

procedure Move120; nostackframe; assembler;
asm
      movaps xmm0, oword ptr [rcx]
      movaps xmm1, oword ptr [rcx + 16]
      movaps xmm2, oword ptr [rcx + 32]
      movaps xmm3, oword ptr [rcx + 48]
      movaps xmm4, oword ptr [rcx + 64]
      movaps xmm5, oword ptr [rcx + 80]
      {$ifndef MSWINDOWS}
      movaps xmm6, oword ptr [rcx + 96] // xmm6 is non volatile on Windows
      {$endif MSWINDOWS}
      mov rax, [rcx + 112]
      movaps oword ptr [rdx], xmm0
      movaps oword ptr [rdx + 16], xmm1
      movaps oword ptr [rdx + 32], xmm2
      movaps oword ptr [rdx + 48], xmm3
      movaps oword ptr [rdx + 64], xmm4
      movaps oword ptr [rdx + 80], xmm5
      {$ifdef MSWINDOWS}
      movaps xmm0, oword ptr [rcx + 96]
      movaps oword ptr [rdx + 96], xmm0
      {$else}
      movaps oword ptr [rdx + 96], xmm6
      {$endif MSWINDOWS}
      mov [rdx + 112], rax
end;

{$ifndef MSWINDOWS} // xmm6 and up are non volatile on Windows

procedure Move136; nostackframe; assembler;
asm
      movaps xmm0, oword ptr [rcx]
      movaps xmm1, oword ptr [rcx + 16]
      movaps xmm2, oword ptr [rcx + 32]
      movaps xmm3, oword ptr [rcx + 48]
      movaps xmm4, oword ptr [rcx + 64]
      movaps xmm5, oword ptr [rcx + 80]
      movaps xmm6, oword ptr [rcx + 96]
      movaps xmm7, oword ptr [rcx + 112]
      mov rax, [rcx + 128]
      movaps oword ptr [rdx], xmm0
      movaps oword ptr [rdx + 16], xmm1
      movaps oword ptr [rdx + 32], xmm2
      movaps oword ptr [rdx + 48], xmm3
      movaps oword ptr [rdx + 64], xmm4
      movaps oword ptr [rdx + 80], xmm5
      movaps oword ptr [rdx + 96], xmm6
      movaps oword ptr [rdx + 112], xmm7
      mov [rdx + 128], rax
end;

procedure Move152; nostackframe; assembler;
asm
      movaps xmm0, oword ptr [rcx]
      movaps xmm1, oword ptr [rcx + 16]
      movaps xmm2, oword ptr [rcx + 32]
      movaps xmm3, oword ptr [rcx + 48]
      movaps xmm4, oword ptr [rcx + 64]
      movaps xmm5, oword ptr [rcx + 80]
      movaps xmm6, oword ptr [rcx + 96]
      movaps xmm7, oword ptr [rcx + 112]
      movaps xmm8, oword ptr [rcx + 128]
      mov rax, [rcx + 144]
      movaps oword ptr [rdx], xmm0
      movaps oword ptr [rdx + 16], xmm1
      movaps oword ptr [rdx + 32], xmm2
      movaps oword ptr [rdx + 48], xmm3
      movaps oword ptr [rdx + 64], xmm4
      movaps oword ptr [rdx + 80], xmm5
      movaps oword ptr [rdx + 96], xmm6
      movaps oword ptr [rdx + 112], xmm7
      movaps oword ptr [rdx + 128], xmm8
      mov [rdx + 144], rax
end;

procedure Move168; nostackframe; assembler;
asm
      movaps xmm0, oword ptr [rcx]
      movaps xmm1, oword ptr [rcx + 16]
      movaps xmm2, oword ptr [rcx + 32]
      movaps xmm3, oword ptr [rcx + 48]
      movaps xmm4, oword ptr [rcx + 64]
      movaps xmm5, oword ptr [rcx + 80]
      movaps xmm6, oword ptr [rcx + 96]
      movaps xmm7, oword ptr [rcx + 112]
      movaps xmm8, oword ptr [rcx + 128]
      movaps xmm9, oword ptr [rcx + 144]
      mov rax, [rcx + 160]
      movaps oword ptr [rdx], xmm0
      movaps oword ptr [rdx + 16], xmm1
      movaps oword ptr [rdx + 32], xmm2
      movaps oword ptr [rdx + 48], xmm3
      movaps oword ptr [rdx + 64], xmm4
      movaps oword ptr [rdx + 80], xmm5
      movaps oword ptr [rdx + 96], xmm6
      movaps oword ptr [rdx + 112], xmm7
      movaps oword ptr [rdx + 128], xmm8
      movaps oword ptr [rdx + 144], xmm9
      mov [rdx + 160], rax
end;

procedure Move184; nostackframe; assembler;
asm
      movaps xmm0, oword ptr [rcx]
      movaps xmm1, oword ptr [rcx + 16]
      movaps xmm2, oword ptr [rcx + 32]
      movaps xmm3, oword ptr [rcx + 48]
      movaps xmm4, oword ptr [rcx + 64]
      movaps xmm5, oword ptr [rcx + 80]
      movaps xmm6, oword ptr [rcx + 96]
      movaps xmm7, oword ptr [rcx + 112]
      movaps xmm8, oword ptr [rcx + 128]
      movaps xmm9, oword ptr [rcx + 144]
      movaps xmm10, oword ptr [rcx + 160]
      mov rax, [rcx + 176]
      movaps oword ptr [rdx], xmm0
      movaps oword ptr [rdx + 16], xmm1
      movaps oword ptr [rdx + 32], xmm2
      movaps oword ptr [rdx + 48], xmm3
      movaps oword ptr [rdx + 64], xmm4
      movaps oword ptr [rdx + 80], xmm5
      movaps oword ptr [rdx + 96], xmm6
      movaps oword ptr [rdx + 112], xmm7
      movaps oword ptr [rdx + 128], xmm8
      movaps oword ptr [rdx + 144], xmm9
      movaps oword ptr [rdx + 160], xmm10
      mov [rdx + 176], rax
end;

{$endif MSWINDOWS}

procedure MoveX16LP; nostackframe; assembler;
asm
      sub r8, 8
      add rcx, r8
      add rdx, r8
      neg r8
      jns @z
      align 16
@s:   movaps xmm0, oword ptr [rcx + r8]
      movaps oword ptr [rdx + r8], xmm0
      add r8, 16
      js @s
@z:   mov rax, qword ptr [rcx + r8]
      mov qword ptr [rdx + r8], rax
end;

// called from ReallocateLargeBlock with regular parameters

procedure MoveLarge(src, dst: pointer; cnt: PtrInt); nostackframe; assembler;
asm
      sub cnt, 8
      add src, cnt
      add dst, cnt
      neg cnt
      jns @z
      align 16
@s:   movaps xmm0, oword ptr [src + cnt]
      movntdq oword ptr [dst + cnt], xmm0 // non-temporal loop
      add cnt, 16
      js @s
      sfence
@z:   mov rax, qword ptr [src + cnt]
      mov qword ptr [dst + cnt], rax
end;



{ ********* Constants and Data Structures Definitions }

{$WARN 3175 off : Some fields coming before "$1" were not initialized }

const
  NumSmallBlockTypes = 46;
  MaximumSmallBlockSize = 2608;
  SmallBlockSizes: array[0..NumSmallBlockTypes - 1] of word = (
   16, 32, 48, 64, 80, 96, 112, 128, 144, 160, 176, 192, 208, 224, 240, 256,
   272, 288, 304, 320, 352, 384, 416, 448, 480, 528, 576, 624, 672, 736, 800,
   880, 960, 1056, 1152, 1264, 1376, 1504, 1648, 1808, 1984, 2176, 2384,
   MaximumSmallBlockSize, MaximumSmallBlockSize, MaximumSmallBlockSize);
  {$ifdef FPCMM_BOOST} // try if the more arenas, the better multi-threadable
  {$ifdef FPCMM_BOOSTER}
  NumTinyBlockTypesPO2 = 4;
  NumTinyBlockArenasPO2 = 5; // will probably end up with Medium lock contention
  {$else}
  NumTinyBlockTypesPO2 = 4;  // tiny are <= 256 bytes
  NumTinyBlockArenasPO2 = 4; // 16 + 1 arenas
  {$endif FPCMM_BOOSTER}
  {$else}
  NumTinyBlockTypesPO2 = 3;  // multiple arenas for tiny blocks <= 128 bytes
  NumTinyBlockArenasPO2 = 3; // 8 round-robin arenas + 1 main by default
  {$endif FPCMM_BOOST}
  NumTinyBlockTypes = 1 shl NumTinyBlockTypesPO2;
  NumTinyBlockArenas = 1 shl NumTinyBlockArenasPO2;
  NumSmallInfoBlock = NumSmallBlockTypes + NumTinyBlockArenas * NumTinyBlockTypes;
  SmallBlockGranularity = 16;
  TargetSmallBlocksPerPool = 48;
  MinimumSmallBlocksPerPool = 12;

  MediumBlockPoolSizeMem = 20 * 64 * 1024;
  MediumBlockPoolSize = MediumBlockPoolSizeMem - 16;
  MediumBlockSizeOffset = 48;
  MinimumMediumBlockSize = 11 * 256 + MediumBlockSizeOffset;
  MediumBlockBinsPerGroup = 32;
  MediumBlockBinGroupCount = 32;
  MediumBlockBinCount = MediumBlockBinGroupCount * MediumBlockBinsPerGroup;
  MediumBlockGranularity = 256;
  MaximumMediumBlockSize =
    MinimumMediumBlockSize + (MediumBlockBinCount - 1) * MediumBlockGranularity;
  OptimalSmallBlockPoolSizeLowerLimit =
    29 * 1024 - MediumBlockGranularity + MediumBlockSizeOffset;
  OptimalSmallBlockPoolSizeUpperLimit =
    64 * 1024 - MediumBlockGranularity + MediumBlockSizeOffset;
  MaximumSmallBlockPoolSize =
    OptimalSmallBlockPoolSizeUpperLimit + MinimumMediumBlockSize;
  LargeBlockGranularity = 65536;

  IsFreeBlockFlag = 1;
  IsMediumBlockFlag = 2;
  IsSmallBlockPoolInUseFlag = 4;
  IsLargeBlockFlag = 4;
  PreviousMediumBlockIsFreeFlag = 8;
  LargeBlockIsSegmented = 8;
  DropSmallFlagsMask = -8;
  ExtractSmallFlagsMask = 7;
  DropMediumAndLargeFlagsMask = -16;
  ExtractMediumAndLargeFlagsMask = 15;

  SpinSmallGetmemLockCount = 10;
  SpinSmallFreememLockCount = 2; // _freemem has more collision -> no spining
  SpinMediumLockCount = 500;
  SpinLargeLockCount = 500;

  SmallBlockDownsizeCheckAdder = 64;
  SmallBlockUpsizeAdder = 32;
  MediumInPlaceDownsizeLimit = MinimumMediumBlockSize div 4;

type
  TMoveProc = procedure; // with rcx/rdx/r8 ABI convention

  PSmallBlockPoolHeader = ^TSmallBlockPoolHeader;

  // information for each small block size - 64 bytes long = CPU cache line
  TSmallBlockType = record
    BlockTypeLocked: boolean;
    AllowedGroupsForBlockPoolBitmap: Byte;
    BlockSize: Word;
    MinimumBlockPoolSize: Word;
    OptimalBlockPoolSize: Word;
    NextPartiallyFreePool: PSmallBlockPoolHeader;
    PreviousPartiallyFreePool: PSmallBlockPoolHeader;
    NextSequentialFeedBlockAddress: pointer;
    MaxSequentialFeedBlockAddress: pointer;
    CurrentSequentialFeedPool: PSmallBlockPoolHeader;
    UpsizeMoveProcedure: TMoveProc;
    {$ifdef CPU64}
    GetmemSleepCount: integer;
    FreememSleepCount: integer;
    {$endif}
  end;
  PSmallBlockType = ^TSmallBlockType;

  TSmallBlockTypes = array[0..NumSmallBlockTypes - 1] of TSmallBlockType;
  TTinyBlockTypes = array[0..NumTinyBlockTypes - 1] of TSmallBlockType;

  TSmallBlockInfo = record
    Small: TSmallBlockTypes;
    Tiny: array[0..NumTinyBlockArenas - 1] of TTinyBlockTypes;
    GetmemLookup: array[0..
      (MaximumSmallBlockSize - 1) div SmallBlockGranularity] of byte;
    {$ifndef FPCMM_ASSUMEMULTITHREAD}
    IsMultiThreadPtr: PBoolean; // safe access to IsMultiThread global variable
    {$endif FPCMM_ASSUMEMULTITHREAD}
    TinyCurrentArena: integer;
  end;

  TSmallBlockPoolHeader = record
    BlockType: PSmallBlockType;
    {$ifdef CPU32}
    Padding32Bits: cardinal;
    {$endif}
    NextPartiallyFreePool: PSmallBlockPoolHeader;
    PreviousPartiallyFreePool: PSmallBlockPoolHeader;
    FirstFreeBlock: pointer;
    BlocksInUse: Cardinal;
    SmallBlockPoolSignature: Cardinal;
    FirstBlockPoolPointerAndFlags: PtrUInt;
  end;

  PMediumBlockPoolHeader = ^TMediumBlockPoolHeader;
  TMediumBlockPoolHeader = record
    PreviousMediumBlockPoolHeader: PMediumBlockPoolHeader;
    NextMediumBlockPoolHeader: PMediumBlockPoolHeader;
    Reserved1: PtrUInt;
    FirstMediumBlockSizeAndFlags: PtrUInt;
  end;

  PMediumFreeBlock = ^TMediumFreeBlock;
  TMediumFreeBlock = record
    PreviousFreeBlock: PMediumFreeBlock;
    NextFreeBlock: PMediumFreeBlock;
  end;

  TMediumBlockInfo = record
    Locked: boolean;
    PoolsCircularList: TMediumBlockPoolHeader;
    LastSequentiallyFed: pointer;
    SequentialFeedBytesLeft: Cardinal;
    BinGroupBitmap: Cardinal;
    BinBitmaps: array[0..MediumBlockBinGroupCount - 1] of Cardinal;
    Bins: array[0..MediumBlockBinCount - 1] of TMediumFreeBlock;
  end;

  PLargeBlockHeader = ^TLargeBlockHeader;
  TLargeBlockHeader = record
    PreviousLargeBlockHeader: PLargeBlockHeader;
    NextLargeBlockHeader: PLargeBlockHeader;
    UserAllocatedSize: PtrUInt;
    BlockSizeAndFlags: PtrUInt;
  end;

const
  BlockHeaderSize = SizeOf(pointer);
  SmallBlockPoolHeaderSize = SizeOf(TSmallBlockPoolHeader);
  MediumBlockPoolHeaderSize = SizeOf(TMediumBlockPoolHeader);
  LargeBlockHeaderSize = SizeOf(TLargeBlockHeader);

var
  SmallBlockInfo: TSmallBlockInfo;
  MediumBlockInfo: TMediumBlockInfo;

  LargeBlocksLocked: boolean;
  LargeBlocksCircularList: TLargeBlockHeader;


{ ********* Shared Routines }

{ note: pause opcode latency was 10 cycles, but is 140 cycles since SkylakeX
        see http://tiny.cc/010ioz
  -> we use pause before ReleaseCore API call when spinning locks }

procedure LockMediumBlocks; nostackframe; assembler;
asm
     // on input: rcx=MediumBlockInfo.Locked on output: r10=MediumBlockInfo
@s:  mov  edx, SpinMediumLockCount
     align 16
@sp: pause
     mov  eax, $100
     dec  edx
     jz   @rc
     cmp  byte ptr[rcx], ah // don't flush the CPU cache if Locked still true
     je   @sp
lock cmpxchg byte ptr [rcx], ah
     je   @ok
     jmp  @sp
@rc: push rsi // preserve POSIX ABI registers
     push rdi
     call ReleaseCore
     pop  rdi
     pop  rsi
     lea  r10, [rip + MediumBlockInfo]
     lea  rax, [rip + HeapStatus] // simple inc within lock
     inc  qword ptr [rax].TMMStatus.Medium.SleepCount
     lea  rcx, [r10].TMediumBlockInfo.Locked
     jmp @s
@ok:
end;

procedure InsertMediumBlockIntoBin; nostackframe; assembler;
asm
  //rcx=MediumFreeBlock edx=MediumBlockSize r10=MediumBlockInfo - even on POSIX
  mov rax, rcx
  //Get the bin number for this block size
  sub edx, MinimumMediumBlockSize
  shr edx, 8
  //Validate the bin number
  sub edx, MediumBlockBinCount - 1
  sbb ecx, ecx
  and edx, ecx
  add edx, MediumBlockBinCount - 1
  mov r9, rdx
  //Get the bin address in rcx
  shl edx, 4
  lea rcx, [r10 + rdx + TMediumBlockInfo.Bins]
  //Bins are LIFO, se we insert this block as the first free block in the bin
  mov rdx, TMediumFreeBlock[rcx].NextFreeBlock
  mov TMediumFreeBlock[rax].PreviousFreeBlock, rcx
  mov TMediumFreeBlock[rax].NextFreeBlock, rdx
  mov TMediumFreeBlock[rdx].PreviousFreeBlock, rax
  mov TMediumFreeBlock[rcx].NextFreeBlock, rax
  //Was this bin empty?
  cmp rdx, rcx
  jne @Done
  //Get the bin number in ecx
  mov rcx, r9
  //Get the group number in edx
  mov rdx, r9
  shr edx, 5
  //Flag this bin as not empty
  mov eax, 1
  shl eax, cl
  lea r8, [r10 + TMediumBlockInfo.BinBitmaps]
  or dword ptr [r8 + rdx * 4], eax
  //Flag the group as not empty
  mov eax, 1
  mov ecx, edx
  shl eax, cl
  or [r10 + TMediumBlockInfo.BinGroupBitmap], eax
@Done:
end;

procedure RemoveMediumFreeBlock; nostackframe; assembler;
asm
  //rcx=MediumFreeBlock r10=MediumBlockInfo - even on POSIX
  //Get the current previous and next blocks
  mov rdx, TMediumFreeBlock[rcx].PreviousFreeBlock
  mov rcx, TMediumFreeBlock[rcx].NextFreeBlock
  //Remove this block from the linked list
  mov TMediumFreeBlock[rcx].PreviousFreeBlock, rdx
  mov TMediumFreeBlock[rdx].NextFreeBlock, rcx
  //Is this bin now empty? If the previous and next free block pointers are
  //equal, they must point to the bin
  cmp rcx, rdx
  jne @Done
  //Get the bin number for this block size in rcx
  lea r8, [r10 + TMediumBlockInfo.Bins]
  sub rcx, r8
  mov edx, ecx
  shr ecx, 4
  //Get the group number in edx
  shr edx, 9
  //Flag this bin as empty
  mov eax, -2
  rol eax, cl
  lea r8, [r10 + TMediumBlockInfo.BinBitmaps]
  and dword ptr [r8 + rdx * 4], eax
  jnz @Done
  //Flag this group as empty
  mov eax, -2
  mov ecx, edx
  rol eax, cl
  and [r10 + TMediumBlockInfo.BinGroupBitmap], eax
@Done:
end;

procedure BinMediumSequentialFeedRemainder; nostackframe; assembler;
asm
  //r10=MediumBlockInfo - even on POSIX
  mov eax, [r10 + TMediumBlockInfo.SequentialFeedBytesLeft]
  test eax, eax
  jz @Done
  //Get a pointer to the last sequentially allocated medium block
  mov rax, [r10 + TMediumBlockInfo.LastSequentiallyFed]
  //Is the block that was last fed sequentially free?
  test byte ptr [rax - BlockHeaderSize], IsFreeBlockFlag
  jnz @LastBlockFedIsFree
  //Set the "previous block is free" flag in the last block fed
  or qword ptr [rax - BlockHeaderSize], PreviousMediumBlockIsFreeFlag
  //Get the remainder in edx
  mov edx, [r10 + TMediumBlockInfo.SequentialFeedBytesLeft]
  //Point eax to the start of the remainder
  sub rax, rdx
@BinTheRemainder:
  //rax = start of remainder, edx = size of remainder
  //Store the size of the block as well as the flags
  lea rcx, [rdx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [rax - BlockHeaderSize], rcx
  //Store the trailing size marker
  mov [rax + rdx - 16], rdx
  //Bin this medium block
  cmp edx, MinimumMediumBlockSize
  jb @Done
  mov rcx, rax
  call InsertMediumBlockIntoBin // rcx=APMediumFreeBlock, edx=AMediumBlockSize
  ret
@LastBlockFedIsFree:
  //Drop the flags
  mov rdx, DropMediumAndLargeFlagsMask
  and rdx, [rax - BlockHeaderSize]
  //Free the last block fed
  cmp edx, MinimumMediumBlockSize
  jb @DontRemoveLastFed
  //Last fed block is free - remove it from its size bin
  mov rcx, rax
  call RemoveMediumFreeBlock // rcx = APMediumFreeBlock
  //Re-read rax and rdx
  mov rax, [r10 + TMediumBlockInfo.LastSequentiallyFed]
  mov rdx, DropMediumAndLargeFlagsMask
  and rdx, [rax - BlockHeaderSize]
@DontRemoveLastFed:
  //Get the number of bytes left in ecx
  mov ecx, [r10 + TMediumBlockInfo.SequentialFeedBytesLeft]
  //rax = remainder start, rdx = remainder size
  sub rax, rcx
  add edx, ecx
  jmp @BinTheRemainder
@Done:
end;

procedure FreeMedium(ptr: PMediumBlockPoolHeader);
begin
  Free(ptr, MediumBlockPoolSizeMem);
  NotifyFree(HeapStatus.Medium, MediumBlockPoolSizeMem);
end;

function AllocNewSequentialFeedMediumPool(blocksize: Cardinal): pointer;
var
  old: PMediumBlockPoolHeader;
  new: pointer;
begin
  BinMediumSequentialFeedRemainder;
  new := AllocMedium(MediumBlockPoolSizeMem);
  with MediumblockInfo do
  if new <> nil then
  begin
    old := PoolsCircularList.NextMediumBlockPoolHeader;
    PMediumBlockPoolHeader(new).PreviousMediumBlockPoolHeader := @PoolsCircularList;
   PoolsCircularList.NextMediumBlockPoolHeader := new;
    PMediumBlockPoolHeader(new).NextMediumBlockPoolHeader := old;
    old.PreviousMediumBlockPoolHeader := new;
    PPtrUInt(PByte(new) + MediumBlockPoolSize - BlockHeaderSize)^ := IsMediumBlockFlag;
    SequentialFeedBytesLeft :=
      (MediumBlockPoolSize - MediumBlockPoolHeaderSize) - blocksize;
    result := pointer(PByte(new) + MediumBlockPoolSize - blocksize);
    LastSequentiallyFed := result;
    PPtrUInt(PByte(result) - BlockHeaderSize)^ := blocksize or IsMediumBlockFlag;
    NotifyAlloc(HeapStatus.Medium, MediumBlockPoolSizeMem);
  end
  else
  begin
    SequentialFeedBytesLeft := 0;
    result := nil;
  end;
end;

procedure LockLargeBlocks; nostackframe; assembler;
asm
@s:  mov  eax, $100
     lea  rcx, [rip + LargeBlocksLocked]
lock cmpxchg byte ptr [rcx], ah
     je   @ok
     mov  edx, SpinLargeLockCount
     align 16
@sp: pause
     mov  eax, $100
     dec  edx
     jz   @rc
     cmp  byte ptr [rcx], ah // don't flush the CPU cache if Locked still true
     je   @sp
lock cmpxchg byte ptr [rcx], ah
     je   @ok
     jmp  @sp
@rc: call ReleaseCore
     lea  rax, [rip + HeapStatus] // simple inc within lock
     inc  qword ptr [rax].TMMStatus.Large.SleepCount
     jmp @s
@ok:
end;

function AllocateLargeBlockFrom(size: PtrUInt;
   existing: pointer; oldsize: PtrUInt): pointer;
var
  blocksize: PtrUInt;
  header, old: PLargeBlockHeader;
begin
  blocksize := (size + LargeBlockHeaderSize +
    LargeBlockGranularity - 1 + BlockHeaderSize) and -LargeBlockGranularity;
  if existing = nil then
    header := AllocLarge(blocksize)
  else
    header := ReallocLarge(existing, oldsize, blocksize);
  if header <> nil then
  begin
    NotifyAlloc(HeapStatus.Large, blocksize);
    if existing <> nil then
      NotifyFree(HeapStatus.Large, oldsize);
    header.UserAllocatedSize := size;
    header.BlockSizeAndFlags := blocksize or IsLargeBlockFlag;
    LockLargeBlocks;
    old := LargeBlocksCircularList.NextLargeBlockHeader;
    header.PreviousLargeBlockHeader := @LargeBlocksCircularList;
    LargeBlocksCircularList.NextLargeBlockHeader := header;
    header.NextLargeBlockHeader := old;
    old.PreviousLargeBlockHeader := header;
    LargeBlocksLocked := False;
    inc(header);
  end;
  result := header;
end;

function AllocateLargeBlock(size: PtrUInt): pointer;
begin
  result := AllocateLargeBlockFrom(size, nil, 0);
end;

procedure FreeLarge(ptr: PLargeBlockHeader; size: PtrUInt);
begin
  NotifyFree(HeapStatus.Large, size);
  Free(ptr, size);
end;

function FreeLargeBlock(p: pointer): PtrInt;
var
  header, prev, next: PLargeBlockHeader;
begin
  header := pointer(PByte(p) - LargeBlockHeaderSize);
  LockLargeBlocks;
  prev := header.PreviousLargeBlockHeader;
  next := header.NextLargeBlockHeader;
  next.PreviousLargeBlockHeader := prev;
  prev.NextLargeBlockHeader := next;
  LargeBlocksLocked := False;
  FreeLarge(header, DropMediumAndLargeFlagsMask and header.BlockSizeAndFlags);
  result := 0; // assume success
end;

{$ifndef FPCMM_STANDALONE}

function _GetMem(size: PtrInt): pointer; forward;
function _FreeMem(P: pointer): PtrInt;   forward;

{$endif FPCMM_STANDALONE}

function ReallocateLargeBlock(p: pointer; size: PtrUInt): pointer;
var
  oldavail, minup, new: PtrUInt;
  {$ifdef LINUX} prev, next, {$endif}
  header: PLargeBlockHeader;
begin
  header := pointer(PByte(p) - LargeBlockHeaderSize);
  oldavail := (DropMediumAndLargeFlagsMask and header^.BlockSizeAndFlags) -
    (LargeBlockHeaderSize + BlockHeaderSize);
  if size > oldavail then
  begin
    // size-up with 1/8 or 1/4 overhead for future increase
    minup := oldavail + (oldavail shr {$ifdef LINUX} 3 {$else} 2 {$endif});
    if size < minup then
      new := minup
    else
      new := size;
    {$ifdef LINUX}
    // remove from current chain list
    LockLargeBlocks;
    prev := header^.PreviousLargeBlockHeader;
    next := header^.NextLargeBlockHeader;
    next.PreviousLargeBlockHeader := prev;
    prev.NextLargeBlockHeader := next;
    LargeBlocksLocked := False;
    // let the Linux Kernel mremap() the memory using its TLB magic
    size := DropMediumAndLargeFlagsMask and header^.BlockSizeAndFlags;
    result := AllocateLargeBlockFrom(new, header, size);
    {$else}
    // no mremap(): reallocate a new block, copy the existing data, free old
    result := _GetMem(new);
    if result <> nil then
    begin
      if new > (MaximumMediumBlockSize - BlockHeaderSize) then
        PLargeBlockHeader(PByte(result) - LargeBlockHeaderSize).UserAllocatedSize := size;
      MoveLarge(p, result, oldavail); // header^.UserAllocatedSize is buggy
    end;
    _FreeMem(p);
    {$endif LINUX}
  end
  else
  // size-down, or small size-up within current buffer
  if size >= (oldavail shr 1) then
  begin
    result := p;
    header.UserAllocatedSize := size;
  end
  else
  begin
    result := _GetMem(size);
    if result <> nil then
    begin
      if size > (MaximumMediumBlockSize - BlockHeaderSize) then
        PLargeBlockHeader(PByte(result) - LargeBlockHeaderSize)^.UserAllocatedSize := size;
      MoveLarge(p, result, size);
    end;
    _FreeMem(p);
  end;
end;


{ ********* Main Memory Manager Functions }

function _GetMem(size: PtrInt): pointer; nostackframe; assembler;
asm
  {$ifndef MSWINDOWS}
  mov rcx, size
  {$else}
  push rsi
  push rdi
  {$endif MSWINDOWS}
  push rbx
  //Since most allocations are for small blocks, determine the small block type
  lea rbx, [rip + SmallBlockInfo]
  lea rdx, [size + BlockHeaderSize - 1]
  shr rdx, 4 // div SmallBlockGranularity
  //Is it a tiny/small block?
  cmp rcx, (MaximumSmallBlockSize - BlockHeaderSize)
  ja @NotTinySmallBlock
  {$ifndef FPCMM_ASSUMEMULTITHREAD}
  mov rax, qword ptr [rbx].TSmallBlockInfo.IsMultiThreadPtr
  {$endif FPCMM_ASSUMEMULTITHREAD}
  //Get the tiny/small TSmallBlockType[] offset in rcx
  movzx ecx, byte ptr [rbx + rdx].TSmallBlockInfo.GetmemLookup
  mov r8, rbx
  shl ecx, 6 // *SizeOf(TSmallBlockType)
  {$ifndef FPCMM_ASSUMEMULTITHREAD}
  cmp byte ptr[rax], 0
  jne @CheckTinySmallLock
  add rbx, rcx
  mov byte ptr [rbx].TSmallBlockType.BlockTypeLocked, true
  jmp @GotLockOnSmallBlockType
@CheckTinySmallLock:
  {$endif FPCMM_ASSUMEMULTITHREAD}
  //Can use one of the several arenas reserved for tiny blocks?
  cmp ecx, SizeOf(TTinyBlockTypes)
  jae @NotTinyBlockType
  { ---------- TINY block allocation ---------- }
@LockTinyBlockTypeLoop:
  //Round-Robin trial of SmallBlockInfo.Tiny[] for size <= 128 bytes
  mov edx, NumTinyBlockArenas
@TinyBlockArenaLoop:
  mov eax, SizeOf(TTinyBlockTypes)
lock xadd dword ptr[r8 + TSmallBlockInfo.TinyCurrentArena], eax
  and eax, (NumTinyBlockArenas * Sizeof(TTinyBlockTypes)) - 1
  add rax, rcx
  lea rbx, [r8 + rax].TSmallBlockInfo.Tiny
  mov eax, $100
  cmp [rbx].TSmallBlockType.BlockTypeLocked, ah
  je @NextTinyBlockArena
lock cmpxchg [rbx].TSmallBlockType.BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
@NextTinyBlockArena:
  dec edx
  jnz @TinyBlockArenaLoop
  //Also try the default SmallBlockInfo.Small[]
  lea rbx, [r8 + rcx]
  mov eax, $100
lock cmpxchg [rbx].TSmallBlockType.BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  //Thread Contention (occurs much less than during _Freemem)
  lock inc dword ptr [rbx].TSmallBlockType.GetmemSleepCount
  mov rbx, r8
  push rcx
  call Releasecore
  pop rcx
  mov r8, rbx
  jmp @LockTinyBlockTypeLoop
  { ---------- SMALL block allocation ---------- }
@NotTinyBlockType:
  //Block size >= 128 bytes will use SmallBlockInfo.Small[]
  lea rbx, [r8 + rcx]
@LockBlockTypeLoopRetry:
  mov r9, SpinSmallGetmemLockCount
@LockBlockTypeLoop:
  //Grab the default block type
  mov eax, $100
lock cmpxchg [rbx].TSmallBlockType.BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  //Try up to two next sizes
  add rbx, SizeOf(TSmallBlockType)
  mov eax, $100
lock cmpxchg [rbx].TSmallBlockType.BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  pause
  add rbx, SizeOf(TSmallBlockType)
  mov eax, $100
lock cmpxchg [rbx].TSmallBlockType.BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  sub rbx, 2 * SizeOf(TSmallBlockType)
  pause
  dec r9
  jnz @LockBlockTypeLoop
   //Block type and two sizes larger are all locked - give up and sleep
lock inc dword ptr [rbx].TSmallBlockType.GetmemSleepCount
  call Releasecore
  jmp @LockBlockTypeLoopRetry
  { ---------- TINY/SMALL block registration ---------- }
@GotLockOnSmallBlockType:
  //Get rdx=NextPartiallyFreePool rax=FirstFreeBlock rcx=DropSmallFlagsMask
  mov rdx, [rbx].TSmallBlockType.NextPartiallyFreePool
  mov rax, [rdx].TSmallBlockPoolHeader.FirstFreeBlock
  mov rcx, DropSmallFlagsMask
  //Is there a pool with free blocks?
  cmp rdx, rbx
  je @TrySmallSequentialFeed
  add [rdx].TSmallBlockPoolHeader.BlocksInUse, 1
  //Set the new first free block and the block header
  and rcx, [rax - BlockHeaderSize]
  mov [rdx].TSmallBlockPoolHeader.FirstFreeBlock, rcx
  mov [rax - BlockHeaderSize], rdx
  //Is the chunk now full?
  jz @RemoveSmallPool
  //Unlock the block type and leave
  mov [rbx].TSmallBlockType.BlockTypeLocked, False
  pop rbx
  {$ifdef MSWINDOWS}
  pop rdi
  pop rsi
  {$endif MSWINDOWS}
  ret
@TrySmallSequentialFeed:
  //Feed a small block sequentially
  mov rdx, [rbx].TSmallBlockType.CurrentSequentialFeedPool
  movzx ecx, [rbx].TSmallBlockType.BlockSize
  add rcx, rax
  //Can another block fit?
  cmp rax, [rbx].TSmallBlockType.MaxSequentialFeedBlockAddress
  ja @AllocateSmallBlockPool
  //Adjust number of used blocks and sequential feed pool
  add [rdx].TSmallBlockPoolHeader.BlocksInUse, 1
  mov [rbx].TSmallBlockType.NextSequentialFeedBlockAddress, rcx
  //Unlock the block type, set the block header and leave
  mov [rbx].TSmallBlockType.BlockTypeLocked, False
  mov [rax - BlockHeaderSize], rdx
  pop rbx
  {$ifdef MSWINDOWS}
  pop rdi
  pop rsi
  {$endif MSWINDOWS}
  ret
@RemoveSmallPool:
  //Pool is full - remove it from the partially free list
  mov rcx, [rdx].TSmallBlockPoolHeader.NextPartiallyFreePool
  mov [rcx].TSmallBlockPoolHeader.PreviousPartiallyFreePool, rbx
  mov [rbx].TSmallBlockType.NextPartiallyFreePool, rcx
  //Unlock the block type and leave
  mov [rbx].TSmallBlockType.BlockTypeLocked, False
  pop rbx
  {$ifdef MSWINDOWS}
  pop rdi
  pop rsi
  {$endif MSWINDOWS}
  ret
@AllocateSmallBlockPool:
  //Access shared information about Medium blocks storage
  lea r10, [rip + MediumBlockInfo]
  mov eax, $100
  lea rcx, [r10 + TMediumBlockInfo.Locked]
lock cmpxchg byte ptr [rcx], ah
  je @MediumLocked1
  call LockMediumBlocks
@MediumLocked1:
  //Are there any available blocks of a suitable size?
  movsx esi, [rbx].TSmallBlockType.AllowedGroupsForBlockPoolBitmap
  and esi, [r10 + TMediumBlockInfo.BinGroupBitmap]
  jz @NoSuitableMediumBlocks
  //Compute rax = bin group number with free blocks, rcx = bin number
  bsf eax, esi
  lea r8, [r10 + TMediumBlockInfo.BinBitmaps]
  lea r9, [rax * 4]
  mov rcx, [r8 + r9]
  bsf ecx, ecx
  lea rcx, [rcx + r9 * 8]
  //Set rdi = @bin, rsi = free block
  lea rsi, [rcx * 8] // SizeOf(TMediumBlockBin) = 16
  lea rdi, [r10 + TMediumBlockInfo.Bins + rsi * 2]
  mov rsi, TMediumFreeBlock[rdi].NextFreeBlock
  //Remove the first block from the linked list (LIFO)
  mov rdx, TMediumFreeBlock[rsi].NextFreeBlock
  mov TMediumFreeBlock[rdi].NextFreeBlock, rdx
  mov TMediumFreeBlock[rdx].PreviousFreeBlock, rdi
  //Is this bin now empty?
  cmp rdi, rdx
  jne @MediumBinNotEmpty
  //rbx = block type, r8 = @MediumBlockBinBitmaps, rax = bin group number,
  //r9 = bin group number * 4, rcx = bin number, rdi = @bin, rsi = free block
  //Flag this bin as empty
  mov edx, -2
  rol edx, cl
  and [r8 + r9], edx
  jnz @MediumBinNotEmpty
  //Flag the group as empty
  btr [r10 + TMediumBlockInfo.BinGroupBitmap], eax
@MediumBinNotEmpty:
  //rsi = free block, rbx = block type
  //Get the size of the available medium block in edi
  mov rdi, DropMediumAndLargeFlagsMask
  and rdi, [rsi - BlockHeaderSize]
  cmp edi, MaximumSmallBlockPoolSize
  jb @UseWholeBlock
  //Split the block: new block size is the optimal size
  mov edx, edi
  movzx edi, [rbx].TSmallBlockType.OptimalBlockPoolSize
  sub edx, edi
  lea rcx, [rsi + rdi]
  lea rax, [rdx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [rcx - BlockHeaderSize], rax
  //Store the size of the second split as the second last pointer
  mov [rcx + rdx - 16], rdx
  //Put the remainder in a bin (it will be big enough)
  call InsertMediumBlockIntoBin // rcx=APMediumFreeBlock, edx=AMediumBlockSize
  jmp @GotMediumBlock
@NoSuitableMediumBlocks:
  //Check the sequential feed medium block pool for space
  movzx ecx, [rbx].TSmallBlockType.MinimumBlockPoolSize
  mov edi, [r10 + TMediumBlockInfo.SequentialFeedBytesLeft]
  cmp edi, ecx
  jb @AllocateNewSequentialFeed
  //Get the address of the last block that was fed
  mov rsi, [r10 + TMediumBlockInfo.LastSequentiallyFed]
  //Enough sequential feed space: Will the remainder be usable?
  movzx ecx, [rbx].TSmallBlockType.OptimalBlockPoolSize
  lea rdx, [rcx + MinimumMediumBlockSize]
  cmp edi, edx
  cmovb edi, ecx
  sub rsi, rdi
  //Update the sequential feed parameters
  sub [r10 + TMediumBlockInfo.SequentialFeedBytesLeft], edi
  mov [r10 + TMediumBlockInfo.LastSequentiallyFed], rsi
  jmp @GotMediumBlock
@AllocateNewSequentialFeed:
  //Use the optimal size for allocating this small block pool
  movzx size, word ptr [rbx].TSmallBlockType.OptimalBlockPoolSize
  push size // use "size" variable = first argument in current ABI call
  call AllocNewSequentialFeedMediumPool
  pop rdi  // restore edi=blocksize and r10=MediumBlockInfo
  lea r10, [rip + MediumBlockInfo]
  mov rsi, rax
  test rax, rax
  jnz @GotMediumBlock // rsi=freeblock rbx=blocktype edi=blocksize
  mov [r10 + TMediumBlockInfo.Locked], al
  mov [rbx].TSmallBlockType.BlockTypeLocked, al
  jmp @Done
@UseWholeBlock:
  //rsi = free block, rbx = block type, edi = block size
  //Mark this block as used in the block following it
  and byte ptr [rsi + rdi - BlockHeaderSize], not PreviousMediumBlockIsFreeFlag
@GotMediumBlock:
  //rsi = free block, rbx = block type, edi = block size
  //Set the size and flags for this block
  lea rcx, [rdi + IsMediumBlockFlag + IsSmallBlockPoolInUseFlag]
  mov [rsi - BlockHeaderSize], rcx
  //Unlock medium blocks and setup the block pool
  xor eax, eax
  mov [r10 + TMediumBlockInfo.Locked], al
  mov TSmallBlockPoolHeader[rsi].BlockType, rbx
  mov TSmallBlockPoolHeader[rsi].FirstFreeBlock, rax
  mov TSmallBlockPoolHeader[rsi].BlocksInUse, 1
  mov [rbx].TSmallBlockType.CurrentSequentialFeedPool, rsi
  //Return the pointer to the first block, compute next/last block addresses
  lea rax, [rsi + SmallBlockPoolHeaderSize]
  movzx ecx, [rbx].TSmallBlockType.BlockSize
  lea rdx, [rax + rcx]
  mov [rbx].TSmallBlockType.NextSequentialFeedBlockAddress, rdx
  add rdi, rsi
  sub rdi, rcx
  mov [rbx].TSmallBlockType.MaxSequentialFeedBlockAddress, rdi
  //Unlock the small block type, set header and leave
  mov [rbx].TSmallBlockType.BlockTypeLocked, False
  mov [rax - BlockHeaderSize], rsi
  pop rbx
  {$ifdef MSWINDOWS}
  pop rdi
  pop rsi
  {$endif MSWINDOWS}
  ret
  { ---------- MEDIUM block allocation ---------- }
@NotTinySmallBlock:
  //Do we need a Large block?
  lea r10, [rip + MediumBlockInfo]
  cmp rcx, (MaximumMediumBlockSize - BlockHeaderSize)
  ja @IsALargeBlockRequest
  //Get the bin size for this block size (rounded up to the next bin size)
  lea rbx, [rcx + MediumBlockGranularity - 1 + BlockHeaderSize - MediumBlockSizeOffset]
  lea rcx, [r10 + TMediumBlockInfo.Locked]
  and ebx, -MediumBlockGranularity
  add ebx, MediumBlockSizeOffset
  mov eax, $100
lock cmpxchg byte ptr [rcx], ah
  je @MediumLocked2
  call LockMediumBlocks
@MediumLocked2:
  //Compute ecx = bin number in ecx and edx = group number
  lea rdx, [rbx - MinimumMediumBlockSize]
  mov ecx, edx
  shr edx, 8 + 5
  shr ecx, 8
  mov eax, -1
  shl eax, cl
  lea r8, [r10 + TMediumBlockInfo.BinBitmaps]
  and eax, [r8 + rdx * 4]
  jz @GroupIsEmpty
  and ecx, -32
  bsf eax, eax
  or ecx, eax
  jmp @GotBinAndGroup
@GroupIsEmpty:
  //Try all groups greater than this group
  mov eax, -2
  mov ecx, edx
  shl eax, cl
  and eax, [r10 + TMediumBlockInfo.BinGroupBitmap]
  jz @TrySequentialFeedMedium
  //There is a suitable group with enough space
  bsf edx, eax
  mov eax, [r8 + rdx * 4]
  bsf ecx, eax
  mov eax, edx
  shl eax, 5
  or ecx, eax
  jmp @GotBinAndGroup
@TrySequentialFeedMedium:
  mov ecx, [r10 + TMediumBlockInfo.SequentialFeedBytesLeft]
  //Can block be fed sequentially?
  sub ecx, ebx
  jc @AllocateNewSequentialFeedForMedium
  //Get the block address, store remaining bytes, set the flags and unlock
  mov rax, [r10 + TMediumBlockInfo.LastSequentiallyFed]
  sub rax, rbx
  mov [r10 + TMediumBlockInfo.LastSequentiallyFed], rax
  mov [r10 + TMediumBlockInfo.SequentialFeedBytesLeft], ecx
  or rbx, IsMediumBlockFlag
  mov [rax - BlockHeaderSize], rbx
  mov byte ptr [r10 + TMediumBlockInfo.Locked], false
  jmp @Done
@AllocateNewSequentialFeedForMedium:
  mov size, rbx // 'size' variable is the first argument register in ABI call
  call AllocNewSequentialFeedMediumPool
  mov byte [rip + MediumBlockInfo.Locked], false // r10 has been overwritten
  jmp @Done
@GotBinAndGroup:
  //ebx = block size, ecx = bin number, edx = group number
  //Compute rdi = @bin, rsi = free block
  lea rax, [rcx + rcx]
  lea rdi, [r10 + TMediumBlockInfo.Bins + rax * 8]
  mov rsi, TMediumFreeBlock[rdi].NextFreeBlock
  //Remove the first block from the linked list (LIFO)
  mov rax, TMediumFreeBlock[rsi].NextFreeBlock
  mov TMediumFreeBlock[rdi].NextFreeBlock, rax
  mov TMediumFreeBlock[rax].PreviousFreeBlock, rdi
  //Is this bin now empty?
  cmp rdi, rax
  jne @MediumBinNotEmptyForMedium
  //edx = bin group number, ecx = bin number, rdi = @bin, rsi = free block, ebx = block size
  //Flag this bin and group as empty
  mov eax, -2
  rol eax, cl
  and [r10 + TMediumBlockInfo.BinBitmaps + rdx * 4], eax
  jnz @MediumBinNotEmptyForMedium
  btr [r10 + TMediumBlockInfo.BinGroupBitmap], edx
@MediumBinNotEmptyForMedium:
  //rsi = free block, ebx = block size
  //Get rdi = size of the available medium block, rdx = second split size
  mov rdi, DropMediumAndLargeFlagsMask
  and rdi, [rsi - BlockHeaderSize]
  mov edx, edi
  sub edx, ebx
  jz @UseWholeBlockForMedium
  //Split the block in two
  lea rcx, [rsi + rbx]
  lea rax, [rdx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [rcx - BlockHeaderSize], rax
  //Store the size of the second split as the second last pointer
  mov [rcx + rdx - 16], rdx
  //Put the remainder in a bin
  cmp edx, MinimumMediumBlockSize
  jb @GotMediumBlockForMedium
  call InsertMediumBlockIntoBin // rcx=APMediumFreeBlock, edx=AMediumBlockSize
  jmp @GotMediumBlockForMedium
@UseWholeBlockForMedium:
  //Mark this block as used in the block following it
  and byte ptr [rsi + rdi - BlockHeaderSize], not PreviousMediumBlockIsFreeFlag
@GotMediumBlockForMedium:
  //Set the size and flags for this block
  lea rcx, [rbx + IsMediumBlockFlag]
  mov [rsi - BlockHeaderSize], rcx
  //Unlock medium blocks and leave
  mov byte ptr[r10 + TMediumBlockInfo.Locked], false
  mov rax, rsi
  jmp @Done
  { ---------- LARGE block allocation ---------- }
@IsALargeBlockRequest:
  xor rax, rax
  test rcx, rcx
  js @Done
  //Note: size is still in the rcx/rdi first param register
  call AllocateLargeBlock
@Done:
  pop rbx
  {$ifdef MSWINDOWS}
  pop rdi
  pop rsi
  {$endif MSWINDOWS}
end;

function _FreeMem(P: pointer): PtrInt; nostackframe; assembler;
asm
  {$ifdef MSWINDOWS}
  push rsi
  {$else}
  mov rcx, P
  {$endif MSWINDOWS}
  push rbx
  test P, P
  jz @VoidPointer
  mov rdx, [P - BlockHeaderSize]
  //Is it a small block in use?
  test dl, IsFreeBlockFlag + IsMediumBlockFlag + IsLargeBlockFlag
  jnz @NotSmallBlockInUse
  //Get the small block type in rbx and try to grab it
  mov rbx, [rdx].TSmallBlockPoolHeader.BlockType
  {$ifndef FPCMM_ASSUMEMULTITHREAD}
  mov rax, qword ptr [rip + SmallBlockInfo].TSmallBlockInfo.IsMultiThreadPtr
  cmp byte ptr[rax], 0
  jne @CheckTinySmallLock
  mov byte ptr [rbx].TSmallBlockType.BlockTypeLocked, true
  jmp @GotLockOnSmallBlockType
@CheckTinySmallLock:
  {$endif FPCMM_ASSUMEMULTITHREAD}
@LockBlockTypeLoop:
  mov eax, $100
lock cmpxchg [rbx].TSmallBlockType.BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  //Spin to grab the block type (don't try long due to contention)
  mov r8d, SpinSmallFreememLockCount
@SpinLockBlockType:
  pause
  dec r8d
  jz @LockBlockTypeSleep
  mov eax, $100
  cmp [rbx].TSmallBlockType.BlockTypeLocked, ah
  je @SpinLockBlockType
  lock cmpxchg [rbx].TSmallBlockType.BlockTypeLocked, ah
  je @GotLockOnSmallBlockType
  jmp @SpinLockBlockType
@LockBlockTypeSleep:
  //Couldn't grab the block type - sleep and try again
  lock inc dword ptr [rbx].TSmallBlockType.FreeMemSleepCount
  push rcx
  call Releasecore
  pop rcx
  mov rdx, [rcx - BlockHeaderSize]
  jmp @LockBlockTypeLoop
@GotLockOnSmallBlockType:
  //rdx = @SmallBlockPoolHeader, rcx = APointer, rbx = @SmallBlockType
  //Adjust number of blocks in use, set rax = old first free block
  mov rax, [rdx].TSmallBlockPoolHeader.FirstFreeBlock
  sub [rdx].TSmallBlockPoolHeader.BlocksInUse, 1
  jz @PoolIsNowEmpty
  //Store this as the new first free block
  mov [rdx].TSmallBlockPoolHeader.FirstFreeBlock, rcx
  //Store the previous first free block as the block header
  lea r8, [rax + IsFreeBlockFlag]
  mov [rcx - BlockHeaderSize], r8
  //Was the pool full?
  test rax, rax
  jnz @SmallPoolWasNotFull
  //Insert the pool back into the linked list if it was full
  mov rcx, [rbx].TSmallBlockType.NextPartiallyFreePool
  mov [rdx].TSmallBlockPoolHeader.PreviousPartiallyFreePool, rbx
  mov [rdx].TSmallBlockPoolHeader.NextPartiallyFreePool, rcx
  mov [rcx].TSmallBlockPoolHeader.PreviousPartiallyFreePool, rdx
  mov [rbx].TSmallBlockType.NextPartiallyFreePool, rdx
@SmallPoolWasNotFull:
  //Unlock the block type and leave
  mov [rbx].TSmallBlockType.BlockTypeLocked, 0
@VoidPointer:
  xor eax, eax
  pop rbx
  {$ifdef MSWINDOWS}
  pop rsi
  {$endif MSWINDOWS}
  ret
@PoolIsNowEmpty:
  //FirstFreeBlock=nil means it is the sequential feed pool with a single block
  test rax, rax
  jz @IsSequentialFeedPool
  //Pool is now empty: Remove it from the linked list and free it
  mov rax, [rdx].TSmallBlockPoolHeader.PreviousPartiallyFreePool
  mov rcx, [rdx].TSmallBlockPoolHeader.NextPartiallyFreePool
  mov TSmallBlockPoolHeader[rax].NextPartiallyFreePool, rcx
  mov [rcx].TSmallBlockPoolHeader.PreviousPartiallyFreePool, rax
  xor eax, eax
  //Is this the sequential feed pool? If so, stop sequential feeding
  cmp [rbx].TSmallBlockType.CurrentSequentialFeedPool, rdx
  jne @NotSequentialFeedPool
@IsSequentialFeedPool:
  mov [rbx].TSmallBlockType.MaxSequentialFeedBlockAddress, rax
@NotSequentialFeedPool:
  //Unlock the small block type and release this pool
  mov [rbx].TSmallBlockType.BlockTypeLocked, al
  mov rcx, rdx
  mov rdx, [rdx - BlockHeaderSize]
  jmp @FreeMediumBlock
  {---------------------Medium blocks------------------------------}
@NotSmallBlockInUse:
  //Not a small block in use: is it a medium or large block?
  test dl, IsFreeBlockFlag + IsLargeBlockFlag
  jnz @NotASmallOrMediumBlock
@FreeMediumBlock:
  //Drop the flags, free rax=medium block, set rbx=block size
  lea r10, [rip + MediumBlockInfo]
  and rdx, DropMediumAndLargeFlagsMask
  mov rbx, rdx
  mov rsi, rcx
  lea rcx, [r10 + TMediumBlockInfo.Locked]
  mov eax, $100
lock cmpxchg byte ptr [rcx], ah
  je   @MediumBlocksLocked
  call LockMediumBlocks
@MediumBlocksLocked:
  //Get rcx = next block size and flags
  mov rcx, [rsi + rbx - BlockHeaderSize]
  //Can we combine this block with the next free block?
  test qword ptr [rsi + rbx - BlockHeaderSize], IsFreeBlockFlag
  jnz @NextBlockIsFree
  //Set the "PreviousIsFree" flag in the next block
  or rcx, PreviousMediumBlockIsFreeFlag
  mov [rsi + rbx - BlockHeaderSize], rcx
@NextBlockChecked:
  //Re-read the flags and try to combine with previous free block
  test byte ptr [rsi - BlockHeaderSize], PreviousMediumBlockIsFreeFlag
  jnz @PreviousBlockIsFree
@PreviousBlockChecked:
  //Check if entire medium block pool is free
  cmp ebx, (MediumBlockPoolSize - MediumBlockPoolHeaderSize)
  je @EntireMediumPoolFree
@BinFreeMediumBlock:
  //Store size of the block, flags and trailing size marker and insert into bin
  lea rax, [rbx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [rsi - BlockHeaderSize], rax
  mov [rsi + rbx - 16], rbx
  mov rcx, rsi
  mov rdx, rbx
  call InsertMediumBlockIntoBin // rcx=APMediumFreeBlock, edx=AMediumBlockSize
  xor eax, eax
  //Unlock medium blocks and leave
  mov [r10 + TMediumBlockInfo.Locked], al
  jmp @Done
@NextBlockIsFree:
  // Get rax = next block address, rbx = end of the block
  lea rax, [rsi + rbx]
  and rcx, DropMediumAndLargeFlagsMask
  add rbx, rcx
  //Was the block binned?
  cmp rcx, MinimumMediumBlockSize
  jb @NextBlockChecked
  mov rcx, rax
  call RemoveMediumFreeBlock // rcx = APMediumFreeBlock
  jmp @NextBlockChecked
@PreviousBlockIsFree:
  //Get rcx/rsi =  size/point of the previous free block, rbx = new block end
  mov rcx, [rsi - 16]
  sub rsi, rcx
  add rbx, rcx
  //Remove the previous block from the linked list
  cmp ecx, MinimumMediumBlockSize
  jb @PreviousBlockChecked
  mov rcx, rsi
  call RemoveMediumFreeBlock // rcx = APMediumFreeBlock
  jmp @PreviousBlockChecked
@EntireMediumPoolFree:
  //Ensure current sequential feed pool is free
  cmp dword ptr [r10 + TMediumBlockInfo.SequentialFeedBytesLeft], MediumBlockPoolSize - MediumBlockPoolHeaderSize
  jne @MakeEmptyMediumPoolSequentialFeed
  //Remove this medium block pool from the linked list stored in its header
  sub rsi, MediumBlockPoolHeaderSize
  mov rax, TMediumBlockPoolHeader[rsi].PreviousMediumBlockPoolHeader
  mov rdx, TMediumBlockPoolHeader[rsi].NextMediumBlockPoolHeader
  mov TMediumBlockPoolHeader[rax].NextMediumBlockPoolHeader, rdx
  mov TMediumBlockPoolHeader[rdx].PreviousMediumBlockPoolHeader, rax
  //Unlock medium blocks and free the block pool
  mov [r10 + TMediumBlockInfo.Locked], false
  mov P, rsi
  call FreeMedium
  xor eax, eax // success
  jmp @Done
@MakeEmptyMediumPoolSequentialFeed:
  //Get rbx = end-marker block, and recycle the current sequential feed pool
  lea rbx, [rsi + MediumBlockPoolSize - MediumBlockPoolHeaderSize]
  call BinMediumSequentialFeedRemainder
  //Set this medium pool up as the new sequential feed pool, unlock and leave
  mov qword ptr [rbx - BlockHeaderSize], IsMediumBlockFlag
  mov dword ptr [r10 + TMediumBlockInfo.SequentialFeedBytesLeft], MediumBlockPoolSize - MediumBlockPoolHeaderSize
  mov [r10 + TMediumBlockInfo.LastSequentiallyFed], rbx
  xor eax, eax
  mov [r10 + TMediumBlockInfo.Locked], al
  jmp @Done
@NotASmallOrMediumBlock:
  //If it is not an attempt to free a block twice, release as large block
  mov eax, -1
  test dl, IsFreeBlockFlag + IsMediumBlockFlag
  jnz @Done
  call FreeLargeBlock // P is still in rcx/rdi first param register
@Done:
  pop rbx
  {$ifdef MSWINDOWS}
  pop rsi
  {$endif MSWINDOWS}
end;

// warning: FPC signature is not the same than Delphi: requires "var P"
function _ReallocMem(var P: pointer; Size: PtrInt): pointer; nostackframe; assembler;
asm
  //On entry: rcx = var P, rdx = Size
  mov rdx, Size
  {$ifdef MSWINDOWS}
  push rdi
  {$endif MSWINDOWS}
  push rbx
  push r14
  push r15
  push P // for assignement in @Done
  mov r14, qword ptr[P]
  //Handle ReallocMem(r14=nil,Size) as GetMem(Size)
  test r14, r14
  jz @PlainGetMem
  mov rcx, [r14 - BlockHeaderSize]
  test cl, IsFreeBlockFlag + IsMediumBlockFlag + IsLargeBlockFlag
  jnz @NotASmallBlock
  { -------------- TINY/SMALL block ------------- }
  //Get rbx = block type, rcx = available size
  mov rbx, [rcx].TSmallBlockPoolHeader.BlockType
  movzx ecx, [rbx].TSmallBlockType.BlockSize
  sub ecx, BlockHeaderSize
  cmp rcx, rdx
  jb @SmallUpsize
  //Downsize or small growup with enough space: reallocate only if need
  lea rbx, [rdx * 4 + SmallBlockDownsizeCheckAdder]
  cmp ebx, ecx
  jb @NotSmallInPlaceDownsize
  mov rax, r14 // keep original pointer
  pop P
  pop r15
  pop r14
  pop rbx
  {$ifdef MSWINDOWS}
  pop rdi
  {$endif MSWINDOWS}
  ret
@PlainGetMem:
  mov P, rdx // P is the proper first argument register
  call _GetMem
  jmp @Done
@NotSmallInPlaceDownsize:
  //Downsize which needs to allocate a really smaller block and move data
  mov rbx, rdx
  mov P, rdx // P is the proper first argument register
  call _GetMem
  test rax, rax
  jz @Done
  push rax
  mov r8, rbx
  mov rdx, rax
  mov rcx, r14
  call MoveX16LP
  mov P, r14
  call _FreeMem
  pop rax
  jmp @Done
@SmallUpsize:
  //State: r14=pointer, rdx=NewSize, rcx=CurrentBlockSize, rbx=CurrentBlockType
  //Small blocks always grow with at least 100% + SmallBlockUpsizeAdder bytes
  lea P, qword ptr[rcx + rcx + SmallBlockUpsizeAdder]
  //New allocated size is the maximum of the requested size and the minimum upsize
  xor rax, rax
  sub P, rdx
  adc rax, -1
  and P, rax
  add P, rdx
  mov r15, rdx
  call _GetMem
  mov rdx, r15
  test rax, rax
  jz @Done
  cmp rdx, MaximumMediumBlockSize - BlockHeaderSize
  jbe @NotSmallUpsizeToLargeBlock
  //Store the user requested size for large block
  mov [rax - 16], rdx
@NotSmallUpsizeToLargeBlock:
  //Move existing data across new block and release the previous pointer
  movzx r8d, [rbx].TSmallBlockType.BlockSize
  sub r8d, BlockHeaderSize
  mov r15, rax
  mov rdx, rax
  mov rcx, r14
  call [rbx].TSmallBlockType.UpsizeMoveProcedure
  mov P, r14
  call _FreeMem
  mov rax, r15
  jmp @Done
@NotASmallBlock:
  //Is this a medium block or a large block?
  test cl, IsFreeBlockFlag + IsLargeBlockFlag
  jnz @PossibleLargeBlock
  { -------------- MEDIUM block ------------- }
  //rcx = Current Size + Flags, r14 = P, rdx = Requested Size, r10 = MediumBlockInfo
  lea r10, [rip + MediumBlockInfo]
  mov rbx, rcx
  and ecx, DropMediumAndLargeFlagsMask
  lea rdi, [r14 + rcx]
  sub ecx, BlockHeaderSize
  and ebx, ExtractMediumAndLargeFlagsMask
  //Is it an upsize or a downsize?
  cmp rdx, rcx
  ja @MediumBlockUpsize
  // rcx = Current Block Size - BlockHeaderSize, rbx = Current Block Flags,
  // rdi = @Next Block, r14 = APointer, rdx = Requested Size
  //Downsize relloacate and move data only if less than half the current size
  lea r15, [rdx + rdx]
  cmp r15, rcx
  jb @MediumMustDownsize
@MediumNoResize:
  mov rax, r14
  jmp @Done
@MediumMustDownsize:
  //In-place downsize? Ensure not smaller than MinimumMediumBlockSize
  cmp edx, MinimumMediumBlockSize - BlockHeaderSize
  jae @MediumBlockInPlaceDownsize
  //Need to move to another Medium block pool, or into a Small block?
  cmp edx, MediumInPlaceDownsizeLimit
  jb @MediumDownsizeRealloc
  //No need to realloc: resize in-place (if not already at the minimum size)
  mov edx, MinimumMediumBlockSize - BlockHeaderSize
  cmp ecx, edx
  jna @MediumNoResize
@MediumBlockInPlaceDownsize:
  //Round up to the next medium block size
  lea r15, [rdx + BlockHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset]
  and r15, -MediumBlockGranularity
  add r15, MediumBlockSizeOffset
  //Get the size of the second split
  add ecx, BlockHeaderSize
  sub ecx, r15d
  mov ebx, ecx
  //Lock the medium blocks
  lea rcx, [r10 + TMediumBlockInfo.Locked]
  mov eax, $100
lock cmpxchg byte ptr [rcx], ah
  je   @MediumBlocksLocked1
  call LockMediumBlocks
@MediumBlocksLocked1:
  mov ecx, ebx
  //Reread the flags - may have changed before medium blocks could be locked
  mov rbx, ExtractMediumAndLargeFlagsMask
  and rbx, [r14 - BlockHeaderSize]
@DoMediumInPlaceDownsize:
  //Set the new size in header, and get rbx = second split size
  or rbx, r15
  mov [r14 - BlockHeaderSize], rbx
  mov ebx, ecx
  //If the next block is used, flag its previous block as free
  mov rdx, [rdi - BlockHeaderSize]
  test dl, IsFreeBlockFlag
  jnz @MediumDownsizeNextBlockFree
  or rdx, PreviousMediumBlockIsFreeFlag
  mov [rdi - BlockHeaderSize], rdx
  jmp @MediumDownsizeDoSplit
@MediumDownsizeNextBlockFree:
  //If the next block is free, combine both
  mov rcx, rdi
  and rdx, DropMediumAndLargeFlagsMask
  add rbx, rdx
  add rdi, rdx
  cmp edx, MinimumMediumBlockSize
  jb @MediumDownsizeDoSplit
  call RemoveMediumFreeBlock // rcx=APMediumFreeBlock
@MediumDownsizeDoSplit:
  //Store the trailing size field and free part header
  mov [rdi - 16], rbx
  lea rcx, [rbx + IsMediumBlockFlag + IsFreeBlockFlag];
  mov [r14 + r15 - BlockHeaderSize], rcx
  //Bin this free block (if worth it)
  cmp rbx, MinimumMediumBlockSize
  jb @MediumBlockDownsizeDone
  lea rcx, [r14 + r15]
  mov rdx, rbx
  call InsertMediumBlockIntoBin // rcx=APMediumFreeBlock, edx=AMediumBlockSize
@MediumBlockDownsizeDone:
  //Unlock the medium blocks, and leave with the new pointer
  mov byte ptr [r10 + TMediumBlockInfo.Locked], False
  mov rax, r14
  jmp @Done
@MediumDownsizeRealloc:
  //Downsize is worth full reallocation and data move
  push rdx
  mov P, rdx
  call _GetMem
  pop r8
  test rax, rax
  jz @Done
  push rax
  mov rdx, rax
  mov rcx, r14
  call MoveX16LP
  mov P, r14
  call _FreeMem
  pop rax
  jmp @Done
@MediumBlockUpsize:
  //ecx = Current Block Size - BlockHeaderSize, bl = Current Block Flags,
  //rdi = @Next Block, r14 = APointer, rdx = Requested Size
  //Try to make in-place upsize
  mov rax, [rdi - BlockHeaderSize]
  test al, IsFreeBlockFlag
  jz @CannotUpsizeMediumBlockInPlace
  //Get rax = available size, r15 = available size with the next block
  and rax, DropMediumAndLargeFlagsMask
  lea r15, [rax + rcx]
  cmp rdx, r15
  ja @CannotUpsizeMediumBlockInPlace
  //Grow into the next block
  mov rbx, rcx
  lea rcx, [r10 + TMediumBlockInfo.Locked]
  mov eax, $100
lock cmpxchg byte ptr [rcx], ah
  je   @MediumBlocksLocked2
  mov r15, rdx
  call LockMediumBlocks
  mov rdx, r15
@MediumBlocksLocked2:
  //Re-read info once locked, and ensure next block is still free
  mov rcx, rbx
  mov rbx, ExtractMediumAndLargeFlagsMask
  and rbx, [r14 - BlockHeaderSize]
  mov rax, [rdi - BlockHeaderSize]
  test al, IsFreeBlockFlag
  jz @NextMediumBlockChanged
  and eax, DropMediumAndLargeFlagsMask
  lea r15, [rax + rcx]
  cmp rdx, r15
  ja @NextMediumBlockChanged
@DoMediumInPlaceUpsize:
  //Bin next free block (if worth it)
  cmp eax, MinimumMediumBlockSize
  jb @MediumInPlaceNoNextRemove
  push rcx
  push rdx
  mov rcx, rdi
  call RemoveMediumFreeBlock // rcx=APMediumFreeBlock
  pop rdx
  pop rcx
@MediumInPlaceNoNextRemove:
  //Medium blocks grow a minimum of 25% in in-place upsizes
  mov eax, ecx
  shr eax, 2
  add eax, ecx
  //Get the maximum of the requested size and the minimum growth size
  xor edi, edi
  sub eax, edx
  adc edi, -1
  and eax, edi
  //Round up to the nearest block size granularity
  lea rax, [rax + rdx + BlockHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset]
  and eax, -MediumBlockGranularity
  add eax, MediumBlockSizeOffset
  //Calculate the size of the second split and check if it fits
  lea rdx, [r15 + BlockHeaderSize]
  sub edx, eax
  ja @MediumInPlaceUpsizeSplit
  //Grab the whole block: Mark it as used in the next block, and adjust size
  and qword ptr [r14 + r15], not PreviousMediumBlockIsFreeFlag
  add r15, BlockHeaderSize
  jmp @MediumUpsizeInPlaceDone
@MediumInPlaceUpsizeSplit:
  //Store the size of the second split as the second last pointer
  mov [r14 + r15 - BlockHeaderSize], rdx
  //Set the second split header
  lea rdi, [rdx + IsMediumBlockFlag + IsFreeBlockFlag]
  mov [r14 + rax - BlockHeaderSize], rdi
  mov r15, rax
  cmp edx, MinimumMediumBlockSize
  jb @MediumUpsizeInPlaceDone
  lea rcx, [r14 + rax]
  call InsertMediumBlockIntoBin // rcx=APMediumFreeBlock, edx=AMediumBlockSize
@MediumUpsizeInPlaceDone:
  //No need to move data at upsize: set the size and flags for this block
  or r15, rbx
  mov [r14 - BlockHeaderSize], r15
  mov byte ptr [r10 + TMediumBlockInfo.Locked], False
  mov rax, r14
  jmp @Done
@NextMediumBlockChanged:
  //The next block changed during lock: reallocate and move data
  mov byte ptr [r10 + TMediumBlockInfo.Locked], False
@CannotUpsizeMediumBlockInPlace:
  mov eax, ecx
  shr eax, 2
  add eax, ecx
  //Get the maximum of the requested size and the minimum growth size
  xor rdi, rdi
  sub rax, rdx
  adc rdi, -1
  and rax, rdi
  add rax, rdx
  mov r15, rax
  mov edi, ecx
  push rdx
  push rdi
  mov P, rax
  call _GetMem
  pop rdi
  pop rdx
  test eax, eax
  jz @Done
  //If it's a Large block - store the actual user requested size
  cmp r15, MaximumMediumBlockSize - BlockHeaderSize
  jbe @MediumUpsizeNotLarge
  mov [rax - 16], rdx
@MediumUpsizeNotLarge:
  push rax
  mov rdx, rax
  mov rcx, r14
  mov r8, rdi
  call MoveX16LP
  mov P, r14
  call _FreeMem
  pop rax
  jmp @Done
@Error:
  xor eax, eax
  jmp @Done
@PossibleLargeBlock:
  { -------------- LARGE block ------------- }
  test cl, IsFreeBlockFlag + IsMediumBlockFlag
  jnz @Error
  {$ifdef MSWINDOWS}
  mov rcx, r14
  {$else}
  mov rdi, r14
  mov rsi, rdx
  {$endif MSWINDOWS}
  call ReallocateLargeBlock // with restored proper registers
@Done:
  pop rcx
  mov qword ptr[rcx], rax // store new pointer in var P
  pop r15
  pop r14
  pop rbx
  {$ifdef MSWINDOWS}
  pop rdi
  {$endif MSWINDOWS}
end;

function _AllocMem(Size: PtrInt): pointer; nostackframe; assembler;
asm
  push rbx
  //Get rbx = size rounded down to the previous multiple of SizeOf(pointer)
  lea rbx, [Size - 1]
  and rbx, -8
  call _GetMem
  //Could a block be allocated? rcx = 0 if yes, -1 if no
  cmp rax, 1
  sbb rcx, rcx
  //Point rdx to the last pointer
  lea rdx, [rax + rbx]
  //Compute Size (1..8 doesn't need to enter the SSE2 loop)
  or rbx, rcx
  jz @ClearLastQWord
  //Large blocks from mmap/VirtualAlloc are already zero filled
  cmp rbx, MaximumMediumBlockSize - BlockHeaderSize
  jae @Done
  neg rbx
  pxor xmm0, xmm0
  align 16
@FillLoop:
  movaps oword ptr [rdx + rbx], xmm0 // non-temporal movntdq not needed (<256KB)
  add rbx, 16
  js @FillLoop
@ClearLastQWord:
  xor rcx, rcx
  mov qword ptr [rdx], rcx
@Done:
  pop rbx
end;

function _MemSize(P: pointer): PtrUInt;
begin
  // AFAIK used only by fpc_AnsiStr_SetLength() in RTL
  P := PPointer(PByte(P) - BlockHeaderSize)^;
  if (PtrUInt(P) and (IsMediumBlockFlag or IsLargeBlockFlag)) = 0 then
    result := PSmallBlockPoolHeader(PtrUInt(P) and DropSmallFlagsMask).
      BlockType.BlockSize - BlockHeaderSize
  else
  begin
    result := (PtrUInt(P) and DropMediumAndLargeFlagsMask) - BlockHeaderSize;
    if (PtrUInt(P) and IsMediumBlockFlag) = 0 then
      dec(result, LargeBlockHeaderSize);
  end;
end;

function _FreeMemSize(P: pointer; size: PtrInt): PtrInt;
begin
  // should return the chunk size - only used by heaptrc
  if size <> 0 then
  begin
    result := _MemSize(P);
    _FreeMem(p);
  end
  else
    result := 0;
end;


{ ********* Information Gathering }

{$ifdef FPCMM_STANDALONE}

procedure Assert(flag: boolean);
begin
end;

{$else}

function _GetHeapStatus: THeapStatus;
begin
  FillChar(result, sizeof(result), 0);
end;

function _GetFPCHeapStatus: TFPCHeapStatus;
begin
  FillChar(result, sizeof(result), 0);
end;

function K(i: PtrUInt): shortstring;
var
  tmp: string[1];
begin
  if i >= 1 shl 40 then
  begin
    i := i shr 40;
    tmp := 'T';
  end
  else
  if i >= 1 shl 30 then
  begin
    i := i shr 30;
    tmp := 'G';
  end
  else
  if i >= 1 shl 20 then
  begin
    i := i shr 20;
    tmp := 'M';
  end
  else
  if i >= 1 shl 10 then
  begin
    i := i shr 10;
    tmp := 'K';
  end
  else
    tmp := '';
  str(i, result);
  result := result + tmp;
end;

{$I-}

procedure WriteHeapStatusDetail(const arena: TMMStatusArena;
  const name: shortstring);
begin
  write(name, K(arena.CurrentBytes):4,
    'B/', K(arena.CumulativeBytes), 'B ');
  {$ifdef FPCMM_DEBUG}
  write('   peak=', K(arena.PeakBytes),
    'B current=', K(arena.CumulativeAlloc - arena.CumulativeFree),
    ' alloc=', K(arena.CumulativeAlloc), ' free=', K(arena.CumulativeFree));
  {$endif FPCMM_DEBUG}
  writeln(' sleep=', K(arena.SleepCount) {$ifdef FPCMM_BOOST} , ' boost=on' {$endif});
end;

procedure WriteHeapStatus(const context: shortstring;
  smallblockcontentioncount: integer);
var
  small: TSmallBlockContentionDynArray;
  i, G, F: PtrInt;
begin
  if context[0] <> #0 then
    writeln(context);
  with HeapStatus do
  begin
    WriteHeapStatusDetail(Medium, ' Medium: ');
    WriteHeapStatusDetail(Large,  ' Large:  ');
    writeln(' Sleep:    count=', K(SleepCount)
      {$ifdef FPCMM_DEBUG} , ' microsec=', K(SleepTime) {$endif});
  end;
  if smallblockcontentioncount <= 0 then
    exit;
  small := GetSmallBlockContention(@G, @F);
  if small <> nil then
  begin
    writeln(' Small Waits: getmem=', K(G), ' freemem=', K(F));
    for i := 0 to high(small) do
      with small[i] do
      begin
        if GetmemSleepCountSmallBlockSize <> 0 then
          write(' getmem(', GetmemSleepCountSmallBlockSize)
        else
          write(' freemem(', FreememSleepCountSmallBlockSize);
        write(')=' , K(SleepCount));
        if i = smallblockcontentioncount then
          exit;
      end;
    writeln;
  end;
end;

{$I+}

procedure QuickSortRes(const Res: TSmallBlockContentionDynArray; L, R: PtrInt);
var
  I, J, P: PtrInt;
  pivot: cardinal;
  tmp: TSmallBlockContention;
begin
  if L < R then
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        pivot := Res[P].SleepCount;
        while Res[I].SleepCount > pivot do
          inc(I);
        while Res[J].SleepCount < pivot do
          dec(J);
        if I <= J then
        begin
          tmp := Res[J];
          Res[J] := Res[I];
          Res[I] := tmp;
          if P = I then
            P := J
          else if P = J then
            P := I;
          inc(I);
          dec(J);
        end;
      until I > J;
      if J - L < R - I then
      begin // use recursion only for smaller range
        if L < J then
          QuickSortRes(Res, L, J);
        L := I;
      end
      else
      begin
        if I < R then
          QuickSortRes(Res, I, R);
        R := J;
      end;
    until L >= R;
end;

function GetSmallBlockContention(GetMemTotal,
  FreeMemTotal: PPtrInt): TSmallBlockContentionDynArray;
var
  i, n: integer;
  p: PSmallBlockType;
  d: ^TSmallBlockContention;
begin
  result := nil;
  if GetMemTotal <> nil then
    GetMemTotal^ := 0;
  if FreeMemTotal <> nil then
    FreeMemTotal^ := 0;
  n := 0;
  p := @SmallBlockInfo;
  for i := 1 to NumSmallInfoBlock do
  begin
    if p^.GetmemSleepCount <> 0 then
      inc(n);
    if p^.FreememSleepCount <> 0 then
      inc(n);
    inc(p);
  end;
  if n = 0 then
    exit;
  SetLength(result, n);
  d := pointer(result);
  p := @SmallBlockInfo;
  for i := 1 to NumSmallInfoBlock do
  begin
    if p^.GetmemSleepCount <> 0 then
    begin
      d^.SleepCount := p^.GetmemSleepCount;
      if GetMemTotal <> nil then
        inc(GetMemTotal^, p^.GetmemSleepCount);
      d^.GetmemSleepCountSmallBlockSize := p^.BlockSize;
      inc(d);
    end;
    if p^.FreememSleepCount <> 0 then
    begin
      d^.SleepCount := p^.FreememSleepCount;
      if FreeMemTotal <> nil then
        inc(FreeMemTotal^, p^.FreeMemSleepCount);
      d^.FreememSleepCountSmallBlockSize := p^.BlockSize;
      inc(d);
    end;
    inc(p);
  end;
  QuickSortRes(result, 0, n - 1);
end;

{$endif FPCMM_STANDALONE}

function CurrentHeapStatus: TMMStatus;
var
  i: integer;
  p: PSmallBlockType;
begin
  result := HeapStatus;
  p := @SmallBlockInfo;
  for i := 1 to NumSmallInfoBlock do
  begin
    inc(result.SmallGetmemSleepCount,  p^.GetmemSleepCount);
    inc(result.SmallFreememSleepCount, p^.FreememSleepCount);
    inc(p);
  end;
end;


{ ********* Initialization and Finalization }

const
  _MOVES: array[1..8 {$ifndef MSWINDOWS} + 4 {$endif}] of TMoveProc = (
    Move8, Move24, Move40, Move56, Move72, Move88, Move104, Move120
    {$ifndef MSWINDOWS} , Move136, Move152, Move168, Move184 {$endif});

procedure InitializeMemoryManager;
var
  small: PSmallBlockType;
  a, i, min, poolsize, num, perpool, size, start, next: PtrInt;
  medium: PMediumFreeBlock;
begin
  small := @SmallBlockInfo;
  assert(SizeOf(small^) = 64); // as expected above asm - match CPU cache line
  for a := 0 to NumTinyBlockArenas do
    for i := 0 to NumSmallBlockTypes - 1 do
    begin
      if (i = NumTinyBlockTypes) and (a > 0) then
        break;
      size := SmallBlockSizes[i];
      assert(size and 15 = 0);
      small^.BlockSize := size;
      min := size shr 4;
      if min <= high(_MOVES) then
        small^.UpsizeMoveProcedure := _MOVES[min]
      else
        small^.UpsizeMoveProcedure := MoveX16LP;
      small^.PreviousPartiallyFreePool := pointer(small);
      small^.NextPartiallyFreePool := pointer(small);
      small^.MaxSequentialFeedBlockAddress := pointer(0);
      small^.NextSequentialFeedBlockAddress := pointer(1);
      min := ((size * MinimumSmallBlocksPerPool +
         (SmallBlockPoolHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset))
         and -MediumBlockGranularity) + MediumBlockSizeOffset;
      if min < MinimumMediumBlockSize then
        min := MinimumMediumBlockSize;
      num := (min + (- MinimumMediumBlockSize +
        MediumBlockBinsPerGroup * MediumBlockGranularity div 2)) div
        (MediumBlockBinsPerGroup * MediumBlockGranularity);
      if num > 7 then
        num := 7;
      small^.AllowedGroupsForBlockPoolBitmap := Byte(Byte(-1) shl num);
      small^.MinimumBlockPoolSize := MinimumMediumBlockSize +
        num * (MediumBlockBinsPerGroup * MediumBlockGranularity);
      poolsize := ((size * TargetSmallBlocksPerPool +
        (SmallBlockPoolHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset))
        and -MediumBlockGranularity) + MediumBlockSizeOffset;
      if poolsize < OptimalSmallBlockPoolSizeLowerLimit then
        poolsize := OptimalSmallBlockPoolSizeLowerLimit;
      if poolsize > OptimalSmallBlockPoolSizeUpperLimit then
        poolsize := OptimalSmallBlockPoolSizeUpperLimit;
      perpool := (poolsize - SmallBlockPoolHeaderSize) div size;
      small^.OptimalBlockPoolSize := ((perpool * size +
         (SmallBlockPoolHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset))
          and -MediumBlockGranularity) + MediumBlockSizeOffset;
      inc(small);
    end;
  assert(small = @SmallBlockInfo.GetmemLookup);
  {$ifndef FPCMM_ASSUMEMULTITHREAD}
  SmallBlockInfo.IsMultiThreadPtr := @IsMultiThread;
  {$endif FPCMM_ASSUMEMULTITHREAD}
  start := 0;
  with SmallBlockInfo do
    for i := 0 to NumSmallBlockTypes - 1 do
    begin
      next := PtrUInt(SmallBlockSizes[i]) div SmallBlockGranularity;
      while start < next do
      begin
        GetmemLookup[start] := i;
        inc(start);
      end;
    end;
  with MediumBlockInfo do
  begin
    PoolsCircularList.PreviousMediumBlockPoolHeader := @PoolsCircularList;
    PoolsCircularList.NextMediumBlockPoolHeader := @PoolsCircularList;
    for i := 0 to MediumBlockBinCount -1 do
    begin
      medium := @Bins[i];
      medium.PreviousFreeBlock := medium;
      medium.NextFreeBlock := medium;
    end;
  end;
  LargeBlocksCircularList.PreviousLargeBlockHeader := @LargeBlocksCircularList;
  LargeBlocksCircularList.NextLargeBlockHeader := @LargeBlocksCircularList;
end;

procedure FreeAllMemory;
var
  medium, nextmedium: PMediumBlockPoolHeader;
  bin: PMediumFreeBlock;
  large, nextlarge: PLargeBlockHeader;
  p: PSmallBlockType;
  i: PtrInt;
begin
  p := @SmallBlockInfo;
  for i := 1 to NumSmallInfoBlock do
  begin
    p^.PreviousPartiallyFreePool := pointer(p);
    p^.NextPartiallyFreePool := pointer(p);
    p^.NextSequentialFeedBlockAddress := pointer(1);
    p^.MaxSequentialFeedBlockAddress := nil;
    inc(p);
  end;
  with MediumBlockInfo do
  begin
    medium := PoolsCircularList.NextMediumBlockPoolHeader;
    while medium <> @PoolsCircularList do
    begin
      nextmedium := medium.NextMediumBlockPoolHeader;
      FreeMedium(medium);
      medium := nextmedium;
    end;
    PoolsCircularList.PreviousMediumBlockPoolHeader := @PoolsCircularList;
    PoolsCircularList.NextMediumBlockPoolHeader := @PoolsCircularList;
    for i := 0 to MediumBlockBinCount - 1 do
    begin
      bin := @Bins[i];
      bin.PreviousFreeBlock := bin;
      bin.NextFreeBlock := bin;
    end;
    BinGroupBitmap := 0;
    SequentialFeedBytesLeft := 0;
    for i := 0 to MediumBlockBinGroupCount - 1 do
      BinBitmaps[i] := 0;
  end;
  large := LargeBlocksCircularList.NextLargeBlockHeader;
  while large <> @LargeBlocksCircularList do
  begin
    nextlarge := large.NextLargeBlockHeader;
    FreeLarge(large, large.BlockSizeAndFlags and DropMediumAndLargeFlagsMask);
    large := nextlarge;
  end;
  LargeBlocksCircularList.PreviousLargeBlockHeader := @LargeBlocksCircularList;
  LargeBlocksCircularList.NextLargeBlockHeader := @LargeBlocksCircularList;
end;


{$ifndef FPCMM_STANDALONE}

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
  InitializeMemoryManager;
  GetMemoryManager(OldMM);
  SetMemoryManager(NewMM);

finalization
  SetMemoryManager(OldMM);
  FreeAllMemory;

{$endif FPCMM_STANDALONE}

{$endif FPC_CPUX64}

end.

