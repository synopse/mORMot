/// Fast Memory Manager for FPC x86_64
// - this unit is a part of the freeware Synopse mORMot framework
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit SynFPCx64MM;

{
  *****************************************************************************

    A Multi-thread Friendly Memory Manager for FPC written in x86_64 assembly
    - targetting Linux (and Windows) multi-threaded Services
    - only for FPC on the x86_64 target - use the RTL MM on Delphi or ARM
    - based on proven FastMM4 by Pierre le Riche - with tuning and enhancements
    - can report detailed statistics (with threads contention and memory leaks)
    - three app modes: default mono-thread friendly, FPCMM_SERVER or FPCMM_BOOST

    Usage: include this unit as the very first in your FPC project uses clause

    Why another Memory Manager on FPC?
    - The built-in heap.inc is well written and cross-platform and cross-CPU,
      but its threadvar arena for small blocks tends to consume a lot of memory
      on multi-threaded servers, and has suboptimal allocation performance
    - C memory managers (glibc, Intel TBB, jemalloc) have a very high RAM
      consumption (especially Intel TBB) and do panic/SIGKILL on any GPF
    - Pascal alternatives (FastMM4,ScaleMM2,BrainMM) are Windows+Delphi specific
    - Our lockess round-robin of tiny blocks and freemem bin list are unique
      algorithms among Memory Managers, and match modern CPUs and workloads
    - It was so fun diving into SSE2 x86_64 assembly and Pierre's insight
    - Resulting code is still easy to understand and maintain

    DISCLAMER: seems stable on Linux and Win64 but feedback is welcome!

  *****************************************************************************

    This file is part of Synopse framework.

    Synopse framework. Copyright (c) Arnaud Bouchez
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

  Portions created by the Initial Developer are Copyright (c)
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

{ ---- Ready-To-Use Scenarios for Memory Manager Tuning }

{
  TL;DR:
    1. default settings target LCL/console mono-threaded apps;
    2. define FPCMM_SERVER for a multi-threaded service/daemon.
}

// target a multi-threaded service on a modern CPU
// - define FPCMM_DEBUG, FPCMM_ASSUMEMULTITHREAD, FPCMM_ERMS, FPCMM_LOCKLESSFREE
// - currently mormot2tests run with no sleep when FPCMM_SERVER is set :)
// - you may try to define FPCMM_BOOST for even more aggressive settings.
{.$define FPCMM_SERVER}

// increase settings for very aggressive multi-threaded process
// - try to enable it if unexpected SmallGetmemSleepCount/SmallFreememSleepCount
// and SleepCount/SleepCycles contentions are reported by CurrentHeapStatus;
// - tiny blocks will be <= 256 bytes (instead of 128 bytes);
// - FPCMM_BOOSTER will use 2x more tiny blocks arenas - likely to be wasteful;
// - will enable FPCMM_SMALLNOTWITHMEDIUM trying to reduce medium sleeps;
// - warning: depending on the workload and hardware, it may actually be slower,
// triggering more Meidum arena contention, and consuming more RAM: consider
// FPCMM_SERVER as a fair alternative.
{.$define FPCMM_BOOST}
{.$define FPCMM_BOOSTER}


{ ---- Fine Grained Memory Manager Tuning }

// includes more detailed information to WriteHeapStatus()
{.$define FPCMM_DEBUG}

// on thread contention, don't spin executing "pause" but directly call Sleep()
// - may help on a single core CPU, or for very specific workloads
{.$define FPCMM_NOPAUSE}

// let FPCMM_DEBUG include SleepCycles information from rdtsc
// and FPCMM_PAUSE call rdtsc for its spinnning loop
// - since rdtsc is emulated so unrealiable on VM, it is disabled by default
{.$define FPCMM_SLEEPTSC}

// checks leaks and write them to the console at process shutdown
// - only basic information will be included: more debugging information (e.g.
// call stack) may be gathered using heaptrc or valgrid
{.$define FPCMM_REPORTMEMORYLEAKS}

// won't check the IsMultiThread global, but assume it is true
// - multi-threaded apps (e.g. a Server Daemon instance) will be faster with it
// - mono-threaded (console/LCL) apps are faster without this conditional
{.$define FPCMM_ASSUMEMULTITHREAD}

// let Freemem multi-thread contention use a lockless algorithm
// - on contention, Freemem won't yield the thread using an OS call, but fill
// an internal Bin list which will be released when the lock becomes available
// - beneficial from our tests on high thread contention (HTTP/REST server)
{.$define FPCMM_LOCKLESSFREE}

// won't use mremap but a regular getmem/move/freemem pattern for large blocks
// - depending on the actual system (e.g. on a VM), mremap may be slower
// - will disable Linux mremap() or Windows following block VirtualQuery/Alloc
{.$define FPCMM_NOMREMAP}

// force the tiny/small blocks to be in their own arena, not with medium blocks
// - would use a little more memory, but medium pool is less likely to sleep
// - not defined for FPCMM_SERVER because no performance difference was found
{.$define FPCMM_SMALLNOTWITHMEDIUM}

// use "rep movsb/stosd" ERMS for blocks > 256 bytes instead of SSE2 "movaps"
// - ERMS is available since Ivy Bridge, and we use "movaps" for smallest blocks
// (to not slow down older CPUs), so it is safe to enable this on FPCMM_SERVER
{.$define FPCMM_ERMS}

// try "cmp" before "lock cmpxchg" for old processors with huge lock penalty
{.$define FPCMM_CMPBEFORELOCK}

// will export libc-like functions, and not replace the FPC MM
// - e.g. to use this unit as a stand-alone C memory allocator
{.$define FPCMM_STANDALONE}

// this whole unit will compile as void
// - may be defined e.g. when compiled as Design-Time Lazarus package
{.$define FPCMM_DISABLE}

interface

{$ifdef FPC}
  // cut-down version of Synopse.inc to make this unit standalone
  {$mode Delphi}
  {$inline on}
  {$R-} // disable Range checking
  {$S-} // disable Stack checking
  {$W-} // disable stack frame generation
  {$Q-} // disable overflow checking
  {$B-} // expect short circuit boolean
  {$ifdef CPUX64}
    {$define FPCX64MM} // this unit is for FPC + x86_64 only
    {$asmmode Intel}
  {$endif CPUX64}
  {$ifdef FPCMM_BOOSTER}
    {$define FPCMM_BOOST}
  {$endif FPCMM_BOOSTER}
  {$ifdef FPCMM_BOOST}
    {$define FPCMM_SERVER}
    {$define FPCMM_SMALLNOTWITHMEDIUM}
  {$endif FPCMM_BOOST}
  {$ifdef FPCMM_SERVER}
    {$define FPCMM_DEBUG}
    {$define FPCMM_ASSUMEMULTITHREAD}
    {$define FPCMM_LOCKLESSFREE}
    {$define FPCMM_ERMS}
  {$endif FPCMM_SERVER}
  {$ifdef FPCMM_BOOSTER}
    {$undef FPCMM_DEBUG} // when performance matters more than stats
  {$endif FPCMM_BOOSTER}
{$endif FPC}

{$ifdef FPCMM_DISABLE}
  {$undef FPCX64MM} // e.g. when compiled as Design-Time Lazarus package
{$endif FPCMM_DISABLE}


{$ifdef FPCX64MM}
// this unit is available only for FPC + X86_64 CPU
// other targets would compile as a void unit

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
    SleepCount: PtrUInt;
  end;

  /// heap information as returned by CurrentHeapStatus
  TMMStatus = record
    /// how many tiny/small memory blocks (<=2600 bytes) are currently allocated
    SmallBlocks: PtrUInt;
    /// how many bytes of tiny/small memory blocks are currently allocated
    // - this size is included in Medium.CurrentBytes value, even if
    // FPCMM_SMALLNOTWITHMEDIUM has been defined
    SmallBlocksSize: PtrUInt;
    /// information about blocks up to 256KB (tiny, small and medium)
    // - includes also the memory needed for tiny/small blocks
    Medium: TMMStatusArena;
    /// information about large blocks > 256KB
    // - those blocks are directly handled by the Operating System
    Large: TMMStatusArena;
    {$ifdef FPCMM_DEBUG}
    {$ifdef FPCMM_SLEEPTSC}
    /// how much rdtsc cycles were spent within SwitchToThread/NanoSleep API
    // - we rdtsc since it is an indicative but very fast way of timing on
    // direct hardware
    // - warning: on virtual machines, the rdtsc opcode is usually emulated so
    // these SleepCycles number are non indicative anymore
    SleepCycles: PtrUInt;
    {$endif FPCMM_SLEEPTSC}
    {$ifdef FPCMM_LOCKLESSFREE}
    /// how many times Freemem() did spin to acquire its lock-less bin list
    SmallFreememLockLessSpin: PtrUInt;
    {$endif FPCMM_LOCKLESSFREE}
    {$endif FPCMM_DEBUG}
    /// how many times the Operating System Sleep/NanoSleep API was called
    // - in a perfect world, should be as small as possible: mormot2tests run as
    // SleepCount=0 when FPCMM_LOCKLESSFREE is defined (FPCMM_SERVER default)
    SleepCount: PtrUInt;
    /// how many times Getmem() did block and wait for a tiny/small block
    // - see also GetSmallBlockContention() for more detailed information
    SmallGetmemSleepCount: PtrUInt;
    /// how many times Freemem() did block and wait for a tiny/small block
    // - see also GetSmallBlockContention() for more detailed information
    SmallFreememSleepCount: PtrUInt;
  end;
  PMMStatus = ^TMMStatus;


/// allocate a new memory buffer
// - as FPC default heap, _Getmem(0) returns _Getmem(1)
function _GetMem(size: PtrUInt): pointer;

/// allocate a new zeroed memory buffer
function _AllocMem(Size: PtrUInt): pointer;

/// release a memory buffer
// - returns the allocated size of the supplied pointer (as FPC default heap)
function _FreeMem(P: pointer): PtrUInt;

/// change the size of a memory buffer
// - won't move any data if in-place reallocation is possible
// - as FPC default heap, _ReallocMem(P=nil,Size) maps P := _getmem(Size) and
// _ReallocMem(P,0) maps _Freemem(P)
function _ReallocMem(var P: pointer; Size: PtrUInt): pointer;

/// retrieve the maximum size (i.e. the allocated size) of a memory buffer
function _MemSize(P: pointer): PtrUInt; inline;

/// retrieve high-level statistics about the current memory manager state
// - see also GetSmallBlockContention for detailed small blocks information
// - standard GetHeapStatus and GetFPCHeapStatus gives less accurate information
// (only CurrHeapSize and MaxHeapSize are set), since we don't track "free" heap
// bytes: I can't figure how "free" memory is relevant nowadays - on 21th century
// Operating Systems, memory is virtual, and reserved/mapped by the OS but
// physically hosted in the HW RAM chips only when written the first time -
// GetHeapStatus information made sense on MSDOS with fixed 640KB of RAM
// - note that FPC GetHeapStatus and GetFPCHeapStatus is only about the
// current thread (irrelevant for sure) whereas CurrentHeapStatus is global
function CurrentHeapStatus: TMMStatus;


{$ifdef FPCMM_STANDALONE}

/// should be called before using any memory function
procedure InitializeMemoryManager;

/// should be called to finalize this memory manager process and release all RAM
procedure FreeAllMemory;

{$undef FPCMM_DEBUG} // excluded FPC-specific debugging

/// IsMultiThread global variable is not correct outside of the FPC RTL
{$define FPCMM_ASSUMEMULTITHREAD}
/// not supported to reduce dependencies and console writing
{$undef FPCMM_REPORTMEMORYLEAKS}

{$else}

type
  /// one GetSmallBlockContention info about unexpected multi-thread waiting
  // - a single GetmemBlockSize or FreememBlockSize non 0 field is set
  TSmallBlockContention = packed record
    /// how many times a small block getmem/freemem has been waiting for unlock
    SleepCount: cardinal;
    /// the small block size on which Getmem() has been blocked - or 0
    GetmemBlockSize: cardinal;
    /// the small block size on which Freemem() has been blocked - or 0
    FreememBlockSize: cardinal;
  end;

  /// small blocks detailed information as returned GetSmallBlockContention
  TSmallBlockContentionDynArray = array of TSmallBlockContention;

  /// one GetSmallBlockStatus information
  TSmallBlockStatus = packed record
    /// how many times a memory block of this size has been allocated
    Total: cardinal;
    /// how many memory blocks of this size are currently allocated
    Current: cardinal;
    /// the standard size of the small memory block
    BlockSize: cardinal;
  end;

  /// small blocks detailed information as returned GetSmallBlockStatus
  TSmallBlockStatusDynArray = array of TSmallBlockStatus;

  /// sort order of detailed information as returned GetSmallBlockStatus
  TSmallBlockOrderBy = (obTotal, obCurrent, obBlockSize);

/// retrieve the use counts of allocated small blocks
// - returns maxcount biggest results, sorted by "orderby" field occurence
function GetSmallBlockStatus(maxcount: integer = 10;
  orderby: TSmallBlockOrderBy = obTotal; count: PPtrUInt = nil; bytes: PPtrUInt = nil;
  small: PCardinal = nil; tiny: PCardinal = nil): TSmallBlockStatusDynArray;

/// retrieve all small blocks which suffered from blocking during multi-thread
// - returns maxcount biggest results, sorted by SleepCount occurence
function GetSmallBlockContention(
  maxcount: integer = 10): TSmallBlockContentionDynArray;


/// convenient debugging function into the console
// - if smallblockcontentioncount > 0, includes GetSmallBlockContention() info
// up to the smallblockcontentioncount biggest occurences
procedure WriteHeapStatus(const context: shortstring = '';
  smallblockstatuscount: integer = 8; smallblockcontentioncount: integer = 8;
  compilationflags: boolean = false);

/// convenient debugging function into a string
// - if smallblockcontentioncount > 0, includes GetSmallBlockContention() info
// up to the smallblockcontentioncount biggest occurences
// - warning: this function is not thread-safe
function GetHeapStatus(const context: shortstring; smallblockstatuscount,
  smallblockcontentioncount: integer; compilationflags, onsameline: boolean): string;


const
  /// human readable information about how our MM was built
  // - similar to WriteHeapStatus(compilationflags=true) output
  FPCMM_FLAGS = ' '
    {$ifdef FPCMM_BOOSTER}           + 'BOOSTER '     {$else}
      {$ifdef FPCMM_BOOST}           + 'BOOST '       {$else}
        {$ifdef FPCMM_SERVER}        + 'SERVER '      {$endif}
      {$endif}
    {$endif}
    {$ifdef FPCMM_ASSUMEMULTITHREAD} + ' assumulthrd' {$endif}
    {$ifdef FPCMM_LOCKLESSFREE}      + ' lockless'    {$endif}
    {$ifdef FPCMM_PAUSE}             + ' pause'       {$endif}
    {$ifdef FPCMM_SLEEPTSC}          + ' rdtsc'       {$endif}
    {$ifndef BSD}
      {$ifdef FPCMM_NOMREMAP}        + ' nomremap'    {$endif}
    {$endif BSD}
    {$ifdef FPCMM_SMALLNOTWITHMEDIUM}+ ' smallpool'   {$endif}
    {$ifdef FPCMM_ERMS}              + ' erms'        {$endif}
    {$ifdef FPCMM_DEBUG}             + ' debug'       {$endif}
    {$ifdef FPCMM_REPORTMEMORYLEAKS} + ' repmemleak'  {$endif};

{$endif FPCMM_STANDALONE}

{$endif FPCX64MM}



implementation

{
   High-level Allocation Strategy Description
  --------------------------------------------

  The allocator handles the following families of memory blocks:
  - TINY <= 128 B (<= 256 B for FPCMM_BOOST)
    Round-robin distribution into several arenas, fed from shared tiny/small pool
    (fair scaling from multi-threaded calls, with no threadvar nor GC involved)
  - SMALL <= 2600 B
    One arena per block size, fed from shared tiny/small pool
  - MEDIUM <= 256 KB
    Separated pool of bitmap-marked chunks, fed from 1MB of OS mmap/virtualalloc
  - LARGE  > 256 KB
    Directly fed from OS mmap/virtualalloc with mremap when growing

  The original FastMM4 was enhanced as such, especially in FPCMM_SERVER mode:
  - FPC compatibility, even on POSIX/Linux, also for FPC specific API behavior;
  - x86_64 code was refactored and tuned in regard to 2020's hardware;
  - Inlined SSE2 movaps loop or ERMS are more efficient that subfunction(s);
  - New round-robin thread-friendly arenas of tiny blocks;
  - Tiny and small blocks can fed from their own pool, not the medium pool;
  - Additional bin list to reduce small/tiny Freemem() thread contention;
  - Memory leaks and thread sleep tracked with almost no performance loss;
  - Large blocks logic has been rewritten, especially realloc;
  - On Linux, mremap is used for efficient realloc of large blocks.

  About locking:
  - Tiny and Small blocks have their own per-size lock;
  - Tiny and Small blocks have one giant lock when fedding from their pool;
  - Medium blocks have one giant lock over their own pool;
  - Large blocks have one giant lock over mmap/virtualalloc system calls;
  - SwitchToThread/FpNanoSleep OS call is done after initial spinning;
  - FPCMM_LOCKLESSFREE reduces Freemem() thread contention;
  - FPCMM_DEBUG / WriteHeapStatus helps identifying the lock contention(s).

}

{$ifdef FPCX64MM}
// this unit is available only for FPC + X86_64 CPU

{$ifndef FPCMM_NOPAUSE}
  // on contention problem, execute "pause" opcode and spin retrying the lock
  // - defined by default to follow Intel recommendatations from
  // https://software.intel.com/content/www/us/en/develop/articles/benefitting-power-and-performance-sleep-loops.html
  // - spinning loop is either using constants or rdtsc (if FPCMM_SLEEPTSC is set)
  // - on SkylakeX (Intel 7th gen), "pause" opcode went from 10-20 to 140 cycles
  // so our constants below will favor those latest CPUs with a longer pause
  {$define FPCMM_PAUSE}
{$endif FPCMM_NOPAUSE}


{ ********* Operating System Specific API Calls }

{$ifdef MSWINDOWS}

// Win64: any assembler function with sub-calls should have a stack frame
// -> nostackframe is defined only on Linux or for functions with no nested call
{$undef NOSFRAME}

const
  kernel32 = 'kernel32.dll';

  MEM_COMMIT   = $1000;
  MEM_RESERVE  = $2000;
  MEM_RELEASE  = $8000;
  MEM_FREE     = $10000;
  MEM_TOP_DOWN = $100000;

  PAGE_READWRITE = 4;
  PAGE_GUARD = $0100;
  PAGE_VALID = $00e6; // PAGE_READONLY or PAGE_READWRITE or PAGE_EXECUTE or
      // PAGE_EXECUTE_READ or PAGE_EXECUTE_READWRITE or PAGE_EXECUTE_WRITECOPY

type
  // VirtualQuery() API result structure
  TMemInfo = record
    BaseAddress, AllocationBase: pointer;
    AllocationProtect: cardinal;
    PartitionId: word;
    RegionSize: PtrUInt;
    State, Protect, MemType: cardinal;
  end;

function VirtualAlloc(lpAddress: pointer;
   dwSize: PtrUInt; flAllocationType, flProtect: cardinal): pointer;
     stdcall; external kernel32 name 'VirtualAlloc';
function VirtualFree(lpAddress: pointer; dwSize: PtrUInt;
   dwFreeType: cardinal): LongBool;
     stdcall; external kernel32 name 'VirtualFree';
procedure SwitchToThread;
     stdcall; external kernel32 name 'SwitchToThread';
function VirtualQuery(lpAddress, lpMemInfo: pointer; dwLength: PtrUInt): PtrUInt;
     stdcall; external kernel32 name 'VirtualQuery';

function AllocMedium(Size: PtrInt): pointer; inline;
begin
  // bottom-up allocation to reduce fragmentation
  result := VirtualAlloc(nil, Size, MEM_COMMIT, PAGE_READWRITE);
end;

function AllocLarge(Size: PtrInt): pointer; inline;
begin
  // FastMM4 uses top-down allocation (MEM_TOP_DOWN) of large blocks to "reduce
  // fragmentation", but on a 64-bit system I am not sure of this statement, and
  // VirtualAlloc() was reported to have a huge slowdown due to this option
  // https://randomascii.wordpress.com/2011/08/05/making-virtualalloc-arbitrarily-slower
  result := VirtualAlloc(nil, Size, MEM_COMMIT, PAGE_READWRITE);
end;

procedure FreeMediumLarge(ptr: pointer; Size: PtrInt); inline;
begin
  VirtualFree(ptr, 0, MEM_RELEASE);
end;

{$ifndef FPCMM_NOMREMAP}

function RemapLarge(addr: pointer; old_len, new_len: size_t): pointer;
var
  meminfo: TMemInfo;
  next: pointer;
  nextsize: PtrUInt;
begin
  // old_len and new_len have 64KB granularity, so match Windows page size
  nextsize := new_len - old_len;
  if PtrInt(nextsize) > 0 then
  begin
    // try to allocate the memory just after the existing one
    FillChar(meminfo, SizeOf(meminfo), 0);
    next := addr + old_len;
    if (VirtualQuery(next, @meminfo, SizeOf(meminfo)) = SizeOf(meminfo)) and
       (meminfo.State = MEM_FREE) and
       (meminfo.RegionSize >= nextsize) and // enough space?
       // reserve the address space in two steps for thread safety
       (VirtualAlloc(next, nextsize, MEM_RESERVE, PAGE_READWRITE) <> nil) and
       (VirtualAlloc(next, nextsize, MEM_COMMIT, PAGE_READWRITE) <> nil) then
      begin
        result := addr; // in-place realloc: no need to move memory :)
        exit;
      end;
  end;
  // we need to use the slower but safe Alloc/Move/Free pattern
  result := AllocLarge(new_len);
  if new_len > old_len then
    new_len := old_len; // handle size up or down
  Move(addr^, result^, new_len); // RTL non-volatile asm or our AVX MoveFast()
  FreeMediumLarge(addr, old_len);
end;

{$endif FPCMM_NOMREMAP}

// experimental VirtualQuery detection of object class - use at your own risk
{$define FPCMM_REPORTMEMORYLEAKS_EXPERIMENTAL}

{$else}

uses
  {$ifndef DARWIN}
  syscall,
  {$endif DARWIN}
  BaseUnix;

// in practice, SYSV ABI seems to not require a stack frame, as Win64 does, for
// our use case of nested calls with no local stack storage and direct kernel
// syscalls - but since it is clearly undocumented, we set it on LINUX only
// -> appears to work with no problem from our tests: feedback is welcome!
// -> see FPCMM_NOSFRAME conditional to disable it on LINUX
{$ifdef LINUX}
  {$define NOSFRAME}
{$endif LINUX}


// we directly call the OS Kernel, so this unit doesn't require any libc

function AllocMedium(Size: PtrInt): pointer; inline;
begin
  result := fpmmap(nil, Size, PROT_READ or PROT_WRITE,
    MAP_PRIVATE or MAP_ANONYMOUS, -1, 0);
end;

function AllocLarge(Size: PtrInt): pointer; inline;
begin
  result := AllocMedium(size); // same API (no MEM_TOP_DOWN option)
end;

procedure FreeMediumLarge(ptr: pointer; Size: PtrInt); inline;
begin
  fpmunmap(ptr, Size);
end;

{$ifdef LINUX}

{$ifndef FPCMM_NOMREMAP}

const
  syscall_nr_mremap = 25; // valid on x86_64 Linux and Android
  MREMAP_MAYMOVE = 1;

function RemapLarge(addr: pointer; old_len, new_len: size_t): pointer; inline;
begin
  // let the Linux Kernel mremap() the memory using its TLB magic
  result := pointer(do_syscall(syscall_nr_mremap, TSysParam(addr),
    TSysParam(old_len), TSysParam(new_len), TSysParam(MREMAP_MAYMOVE)));
end;

{$endif FPCMM_NOMREMAP}

// experimental detection of object class - use at your own risk
{$define FPCMM_REPORTMEMORYLEAKS_EXPERIMENTAL}
// (untested on BSD/DARWIN)

{$else}

  {$define FPCMM_NOMREMAP} // mremap is a Linux-specific syscall

{$endif LINUX}

procedure SwitchToThread;
var
  t: Ttimespec;
begin
  // note: nanosleep() adds a few dozen of microsecs for context switching
  t.tv_sec := 0;
  t.tv_nsec := 10; // empirically identified on a recent Linux Kernel
  fpnanosleep(@t, nil);
end;

{$endif MSWINDOWS}

// fallback to safe and simple Alloc/Move/Free pattern
{$ifdef FPCMM_NOMREMAP}

function RemapLarge(addr: pointer; old_len, new_len: size_t): pointer;
begin
  result := AllocLarge(new_len);
  if new_len > old_len then
    new_len := old_len; // resize down
  Move(addr^, result^, new_len); // RTL non-volatile asm or our AVX MoveFast()
  FreeMediumLarge(addr, old_len);
end;

{$endif FPCMM_NOMREMAP}


{ ********* Some Assembly Helpers }

// low-level conditional to disable nostackframe code on Linux
{$ifdef FPCMM_NOSFRAME}
  {$undef NOSFRAME}
{$endif FPCMM_NOSFRAME}

var
  HeapStatus: TMMStatus;

{$ifdef FPCMM_DEBUG}

procedure ReleaseCore;
  {$ifdef NOSFRAME} nostackframe; {$endif} assembler;
asm
        {$ifdef FPCMM_SLEEPTSC}
        rdtsc // returns the TSC in EDX:EAX
        shl     rdx, 32
        or      rax, rdx
        push    rax
        call    SwitchToThread
        pop     rcx
        rdtsc
        shl     rdx, 32
        or      rax, rdx
        lea     rdx, [rip + HeapStatus]
        sub     rax, rcx
   lock add     qword ptr [rdx + TMMStatus.SleepCycles], rax
        {$else}
        call    SwitchToThread
        lea     rdx, [rip + HeapStatus]
        {$endif FPCMM_SLEEPTSC}
   lock inc     qword ptr [rdx + TMMStatus.SleepCount]
end;

{$else}

procedure ReleaseCore;
begin
  SwitchToThread;
  inc(HeapStatus.SleepCount); // indicative counter
end;

{$endif FPCMM_DEBUG}

procedure NotifyArenaAlloc(var Arena: TMMStatusArena; Size: PtrUInt);
  nostackframe; assembler;
asm
        {$ifdef FPCMM_DEBUG}
   lock add     qword ptr [Arena].TMMStatusArena.CurrentBytes, Size
   lock add     qword ptr [Arena].TMMStatusArena.CumulativeBytes, Size
   lock inc     qword ptr [Arena].TMMStatusArena.CumulativeAlloc
        mov     rax, qword ptr [Arena].TMMStatusArena.CurrentBytes
        cmp     rax, qword ptr [Arena].TMMStatusArena.PeakBytes
        jbe     @s
        mov     qword ptr [Arena].TMMStatusArena.PeakBytes, rax
@s:     {$else}
        add     qword ptr [Arena].TMMStatusArena.CurrentBytes, Size
        add     qword ptr [Arena].TMMStatusArena.CumulativeBytes, Size
       {$endif FPCMM_DEBUG}
end;

procedure NotifyMediumLargeFree(var Arena: TMMStatusArena; Size: PtrUInt);
  nostackframe; assembler;
asm
        neg     Size
        {$ifdef FPCMM_DEBUG}
   lock add     qword ptr [Arena].TMMStatusArena.CurrentBytes, Size
   lock inc     qword ptr [Arena].TMMStatusArena.CumulativeFree
        {$else}
        add     qword ptr [Arena].TMMStatusArena.CurrentBytes, Size
        {$endif FPCMM_DEBUG}
end;


{ ********* Constants and Data Structures Definitions }

const
  // (sometimes) the more arenas, the better multi-threadable
  {$ifdef FPCMM_BOOSTER}
  NumTinyBlockTypesPO2  = 4;
  NumTinyBlockArenasPO2 = 4; // will probably end up with Medium lock contention
  {$else}
    {$ifdef FPCMM_BOOST}
    NumTinyBlockTypesPO2  = 4; // tiny are <= 256 bytes
    NumTinyBlockArenasPO2 = 3; // 8 arenas
    {$else}
    // default (or FPCMM_SERVER) settings
    NumTinyBlockTypesPO2  = 3; // multiple arenas for tiny blocks <= 128 bytes
    NumTinyBlockArenasPO2 = 3; // 8 round-robin arenas (including main) by default
    {$endif FPCMM_BOOST}
  {$endif FPCMM_BOOSTER}

  NumSmallBlockTypes = 46;
  MaximumSmallBlockSize = 2608;
  SmallBlockSizes: array[0..NumSmallBlockTypes - 1] of word = (
    16, 32, 48, 64, 80, 96, 112, 128, 144, 160, 176, 192, 208, 224, 240, 256,
    272, 288, 304, 320, 352, 384, 416, 448, 480, 528, 576, 624, 672, 736, 800,
    880, 960, 1056, 1152, 1264, 1376, 1504, 1648, 1808, 1984, 2176, 2384,
    MaximumSmallBlockSize, MaximumSmallBlockSize, MaximumSmallBlockSize);
  NumTinyBlockTypes = 1 shl NumTinyBlockTypesPO2;
  NumTinyBlockArenas = (1 shl NumTinyBlockArenasPO2) - 1; // -1 = main Small[]
  NumSmallInfoBlock = NumSmallBlockTypes + NumTinyBlockArenas * NumTinyBlockTypes;
  SmallBlockGranularity = 16;
  TargetSmallBlocksPerPool = 48;
  MinimumSmallBlocksPerPool = 12;
  SmallBlockDownsizeCheckAdder = 64;
  SmallBlockUpsizeAdder = 32;
  {$ifdef FPCMM_LOCKLESSFREE}
  SmallBlockTypePO2 = 8;  // SizeOf(TSmallBlockType)=256 with Bin list
  SmallBlockBinCount = (((1 shl SmallBlockTypePO2) - 64) div 8) - 1; // =23
  {$else}
  SmallBlockTypePO2 = 6;  // SizeOf(TSmallBlockType)=64
  {$endif FPCMM_LOCKLESSFREE}

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
  MediumInPlaceDownsizeLimit = MinimumMediumBlockSize div 4;

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

  {$ifdef FPCMM_SLEEPTSC}
  // pause using rdtsc (30 cycles latency on hardware but emulated on VM)
  SpinMediumLockTSC = 10000;
  SpinLargeLockTSC = 10000;
  {$ifdef FPCMM_PAUSE}
  SpinSmallGetmemLockTSC = 1000;
  SpinSmallFreememLockTSC = 1000; // _freemem has more collisions
  {$ifdef FPCMM_LOCKLESSFREE}
  SpinSmallFreememBinTSC = 2000;
  {$endif FPCMM_LOCKLESSFREE}
  {$endif FPCMM_PAUSE}
  {$else}
  // pause with constant spinning counts (empirical values from fastmm4-avx)
  SpinMediumLockCount = 2500;
  SpinLargeLockCount = 5000;
  {$ifdef FPCMM_PAUSE}
  SpinSmallGetmemLockCount = 500;
  SpinSmallFreememLockCount = 500;
  {$ifdef FPCMM_LOCKLESSFREE}
  SpinFreememBinCount = 500;
  {$endif FPCMM_LOCKLESSFREE}
  {$endif FPCMM_PAUSE}
  {$endif FPCMM_SLEEPTSC}

  {$ifdef FPCMM_ERMS}
  // pre-ERMS expects at least 256 bytes, IvyBridge+ with ERMS is good from 64
  // see https://stackoverflow.com/a/43837564/458259 for explanations and timing
  // -> "movaps" loop is used up to 256 bytes of data: good on all CPUs
  // -> "movnt" Move/MoveFast is used for large blocks: always faster than ERMS
  ErmsMinSize = 256;
  {$endif FPCMM_ERMS}

type
  PSmallBlockPoolHeader = ^TSmallBlockPoolHeader;

  // information for each small block size - 64/256 bytes long >= CPU cache line
  TSmallBlockType = record
    Locked: boolean;
    AllowedGroupsForBlockPoolBitmap: byte;
    BlockSize: Word;
    MinimumBlockPoolSize: Word;
    OptimalBlockPoolSize: Word;
    NextPartiallyFreePool: PSmallBlockPoolHeader;
    PreviousPartiallyFreePool: PSmallBlockPoolHeader;
    NextSequentialFeedBlockAddress: pointer;
    MaxSequentialFeedBlockAddress: pointer;
    CurrentSequentialFeedPool: PSmallBlockPoolHeader;
    GetmemCount: cardinal;
    FreememCount: cardinal;
    GetmemSleepCount: cardinal;
    FreememSleepCount: cardinal;
    {$ifdef FPCMM_LOCKLESSFREE} // 192 optional bytes for FreeMem Bin (= 13KB)
    BinLocked: boolean; // dedicated lock for less contention
    BinCount: byte;
    BinSpinCount: cardinal;
    BinInstance: array[0.. SmallBlockBinCount - 1] of pointer;
    {$endif FPCMM_LOCKLESSFREE}
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
    {$endif CPU32}
    NextPartiallyFreePool: PSmallBlockPoolHeader;
    PreviousPartiallyFreePool: PSmallBlockPoolHeader;
    FirstFreeBlock: pointer;
    BlocksInUse: cardinal;
    SmallBlockPoolSignature: cardinal;
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

  // medium locks occurs at getmem: freemem bin list got MaxCount=0 :(
  {.$define FPCMM_LOCKLESSFREEMEDIUM}

{$ifdef FPCMM_LOCKLESSFREEMEDIUM}
const
  MediumBlockLocklessBinCount = 255;

type
  // used by TMediumBlockInfo to reduce thread pressure
  TMediumLocklessBin = record
    Locked: boolean; // dedicated lock for less contention
    Count: byte;
    MaxCount: byte;
    Instance: array[0 .. MediumBlockLocklessBinCount - 1] of pointer;
  end;
{$endif FPCMM_LOCKLESSFREEMEDIUM}

  TMediumBlockInfo = record
    Locked: boolean;
    PoolsCircularList: TMediumBlockPoolHeader;
    LastSequentiallyFed: pointer;
    SequentialFeedBytesLeft: cardinal;
    BinGroupBitmap: cardinal;
    {$ifndef FPCMM_ASSUMEMULTITHREAD}
    IsMultiThreadPtr: PBoolean; // safe access to IsMultiThread global variable
    {$endif FPCMM_ASSUMEMULTITHREAD}
    BinBitmaps: array[0..MediumBlockBinGroupCount - 1] of cardinal;
    Bins: array[0..MediumBlockBinCount - 1] of TMediumFreeBlock;
    {$ifdef FPCMM_LOCKLESSFREEMEDIUM}
    LocklessBin: TMediumLocklessBin;
    {$endif FPCMM_LOCKLESSFREEMEDIUM}
  end;

  PLargeBlockHeader = ^TLargeBlockHeader;
  TLargeBlockHeader = record
    PreviousLargeBlockHeader: PLargeBlockHeader;
    NextLargeBlockHeader: PLargeBlockHeader;
    Reserved: PtrUInt;
    BlockSizeAndFlags: PtrUInt;
  end;

const
  BlockHeaderSize = SizeOf(pointer);
  SmallBlockPoolHeaderSize = SizeOf(TSmallBlockPoolHeader);
  MediumBlockPoolHeaderSize = SizeOf(TMediumBlockPoolHeader);
  LargeBlockHeaderSize = SizeOf(TLargeBlockHeader);
  LargeBlockGranularity = 65536;

var
  SmallBlockInfo: TSmallBlockInfo;
  MediumBlockInfo: TMediumBlockInfo;
  SmallMediumBlockInfo: TMediumBlockInfo
  {$ifdef FPCMM_SMALLNOTWITHMEDIUM} ;
  {$else} absolute MediumBlockInfo;
  {$endif FPCMM_SMALLNOTWITHMEDIUM}


  LargeBlocksLocked: boolean;
  LargeBlocksCircularList: TLargeBlockHeader;


{ ********* Shared Routines }

{$define FPCMM_CMPBEFORELOCK_SPIN}
// during spinning, there is clearly thread contention: in this case, plain
// "cmp" before "lock cmpxchg" is mandatory to leverage the CPU cores


procedure LockMediumBlocks;
  {$ifdef NOSFRAME} nostackframe; {$endif} assembler;
// on input/output: r10=TMediumBlockInfo
asm
        {$ifdef FPCMM_SLEEPTSC}
@s:     rdtsc   // tsc in edx:eax
        shl     rdx, 32
        lea     r9, [rax + rdx + SpinMediumLockTSC] // r9 = endtsc
@sp:    pause
        rdtsc
        shl     rdx, 32
        or      rax, rdx
        cmp     rax, r9
        ja      @rc // timeout
        {$else}
@s:     mov     edx, SpinMediumLockCount
@sp:    pause
        dec     edx
        jz      @rc //timeout
        {$endif FPCMM_SLEEPTSC}
        mov     rcx, r10
        mov     eax, $100
        {$ifdef FPCMM_CMPBEFORELOCK_SPIN}
        cmp     byte ptr [r10].TMediumBlockInfo.Locked, true
        je      @sp
        {$endif FPCMM_CMPBEFORELOCK_SPIN}
  lock  cmpxchg byte ptr [rcx].TMediumBlockInfo.Locked, ah
        je      @ok
        jmp     @sp
@rc:    push    rsi // preserve POSIX and Win64 ABI registers
        push    rdi
        push    r10
        push    r11
        call    ReleaseCore
        pop     r11
        pop     r10
        pop     rdi
        pop     rsi
        lea     rax, [rip + HeapStatus]
        {$ifdef FPCMM_DEBUG} lock {$endif}
        inc     qword ptr [rax].TMMStatus.Medium.SleepCount
        jmp     @s
@ok:
end;

procedure InsertMediumBlockIntoBin; nostackframe; assembler;
// rcx=P edx=blocksize r10=TMediumBlockInfo - even on POSIX
asm
        mov     rax, rcx
        // Get the bin number for this block size
        sub     edx, MinimumMediumBlockSize
        shr     edx, 8
        // Validate the bin number
        sub     edx, MediumBlockBinCount - 1
        sbb     ecx, ecx
        and     edx, ecx
        add     edx, MediumBlockBinCount - 1
        mov     r9, rdx
        // Get the bin address in rcx
        shl     edx, 4
        lea     rcx, [r10 + rdx + TMediumBlockInfo.Bins]
        // Bins are LIFO, se we insert this block as the first free block in the bin
        mov     rdx, TMediumFreeBlock[rcx].NextFreeBlock
        mov     TMediumFreeBlock[rax].PreviousFreeBlock, rcx
        mov     TMediumFreeBlock[rax].NextFreeBlock, rdx
        mov     TMediumFreeBlock[rdx].PreviousFreeBlock, rax
        mov     TMediumFreeBlock[rcx].NextFreeBlock, rax
        // Was this bin empty?
        cmp     rdx, rcx
        jne     @Done
        // Get ecx=bin number, edx=group number
        mov     rcx, r9
        mov     rdx, r9
        shr     edx, 5
        // Flag this bin as not empty
        mov     eax, 1
        shl     eax, cl
        or      dword ptr [r10 + TMediumBlockInfo.BinBitmaps + rdx * 4], eax
        // Flag the group as not empty
        mov     eax, 1
        mov     ecx, edx
        shl     eax, cl
        or      [r10 + TMediumBlockInfo.BinGroupBitmap], eax
@Done:
end;

procedure RemoveMediumFreeBlock; nostackframe; assembler;
asm
        // rcx=MediumFreeBlock r10=TMediumBlockInfo - even on POSIX
        // Get the current previous and next blocks
        mov     rdx, TMediumFreeBlock[rcx].PreviousFreeBlock
        mov     rcx, TMediumFreeBlock[rcx].NextFreeBlock
        // Remove this block from the linked list
        mov     TMediumFreeBlock[rcx].PreviousFreeBlock, rdx
        mov     TMediumFreeBlock[rdx].NextFreeBlock, rcx
        // Is this bin now empty? If the previous and next free block pointers are
        // equal, they must point to the bin
        cmp     rcx, rdx
        jne     @Done
        // Get ecx=bin number, edx=group number
        lea     r8, [r10 + TMediumBlockInfo.Bins]
        sub     rcx, r8
        mov     edx, ecx
        shr     ecx, 4
        shr     edx, 9
        // Flag this bin as empty
        mov     eax, -2
        rol     eax, cl
        and     dword ptr [r10 + TMediumBlockInfo.BinBitmaps + rdx * 4], eax
        jnz     @Done
        // Flag this group as empty
        mov     eax, -2
        mov     ecx, edx
        rol     eax, cl
        and     [r10 + TMediumBlockInfo.BinGroupBitmap], eax
@Done:
end;

procedure BinMediumSequentialFeedRemainder(
  var Info: TMediumBlockInfo); nostackframe; assembler;
asm
        mov     r10, Info
        mov     eax, [Info + TMediumBlockInfo.SequentialFeedBytesLeft]
        test    eax, eax
        jz      @Done
        // Is the last fed sequentially block free?
        mov     rax, [Info + TMediumBlockInfo.LastSequentiallyFed]
        test    byte ptr [rax - BlockHeaderSize], IsFreeBlockFlag
        jnz     @LastBlockFedIsFree
        // Set the "previous block is free" flag in the last block fed
        or      qword ptr [rax - BlockHeaderSize], PreviousMediumBlockIsFreeFlag
        // Get edx=remainder size, rax=remainder start
        mov     edx, [r10 + TMediumBlockInfo.SequentialFeedBytesLeft]
        sub     rax, rdx
@BinTheRemainder:
        // Store the size of the block as well as the flags
        lea     rcx, [rdx + IsMediumBlockFlag + IsFreeBlockFlag]
        mov     [rax - BlockHeaderSize], rcx
        // Store the trailing size marker
        mov     [rax + rdx - 16], rdx
        // Bin this medium block
        cmp     edx, MinimumMediumBlockSize
        jb      @Done
        mov     rcx, rax
        jmp     InsertMediumBlockIntoBin // rcx=APMediumFreeBlock, edx=AMediumBlockSize
@Done:  ret
@LastBlockFedIsFree:
        // Drop the flags
        mov     rdx, DropMediumAndLargeFlagsMask
        and     rdx, [rax - BlockHeaderSize]
        // Free the last block fed
        cmp     edx, MinimumMediumBlockSize
        jb      @DontRemoveLastFed
        // Last fed block is free - remove it from its size bin
        mov     rcx, rax
        call    RemoveMediumFreeBlock // rcx = APMediumFreeBlock
        // Re-read rax and rdx
        mov     rax, [r10 + TMediumBlockInfo.LastSequentiallyFed]
        mov     rdx, DropMediumAndLargeFlagsMask
        and     rdx, [rax - BlockHeaderSize]
@DontRemoveLastFed:
        // Get the number of bytes left in ecx
        mov     ecx, [r10 + TMediumBlockInfo.SequentialFeedBytesLeft]
        // rax = remainder start, rdx = remainder size
        sub    rax, rcx
        add    edx, ecx
        jmp    @BinTheRemainder
end;

procedure FreeMedium(ptr: PMediumBlockPoolHeader);
begin
  FreeMediumLarge(ptr, MediumBlockPoolSizeMem);
  NotifyMediumLargeFree(HeapStatus.Medium, MediumBlockPoolSizeMem);
end;

function AllocNewSequentialFeedMediumPool(BlockSize: cardinal;
  var Info: TMediumBlockInfo): pointer;
var
  old: PMediumBlockPoolHeader;
  new: pointer;
begin
  BinMediumSequentialFeedRemainder(Info);
  new := AllocMedium(MediumBlockPoolSizeMem);
  if new <> nil then
  begin
    old := Info.PoolsCircularList.NextMediumBlockPoolHeader;
    PMediumBlockPoolHeader(new).PreviousMediumBlockPoolHeader := @Info.PoolsCircularList;
   Info.PoolsCircularList.NextMediumBlockPoolHeader := new;
    PMediumBlockPoolHeader(new).NextMediumBlockPoolHeader := old;
    old.PreviousMediumBlockPoolHeader := new;
    PPtrUInt(PByte(new) + MediumBlockPoolSize - BlockHeaderSize)^ := IsMediumBlockFlag;
    Info.SequentialFeedBytesLeft :=
      (MediumBlockPoolSize - MediumBlockPoolHeaderSize) - BlockSize;
    result := pointer(PByte(new) + MediumBlockPoolSize - BlockSize);
    Info.LastSequentiallyFed := result;
    PPtrUInt(PByte(result) - BlockHeaderSize)^ := BlockSize or IsMediumBlockFlag;
    NotifyArenaAlloc(HeapStatus.Medium, MediumBlockPoolSizeMem);
  end
  else
  begin
    Info.SequentialFeedBytesLeft := 0;
    result := nil;
  end;
end;

procedure LockLargeBlocks;
  {$ifdef NOSFRAME} nostackframe; {$endif} assembler;
asm
@s:     mov     eax, $100
        lea     rcx, [rip + LargeBlocksLocked]
  lock  cmpxchg byte ptr [rcx], ah
        je      @ok
        {$ifdef FPCMM_SLEEPTSC}
        rdtsc
        shl     rdx, 32
        lea     r9, [rax + rdx + SpinLargeLockTSC] // r9 = endtsc
@sp:    pause
        rdtsc
        shl     rdx, 32
        or      rax, rdx
        cmp     rax, r9
        ja      @rc // timeout
        {$else}
        mov     edx, SpinLargeLockCount
@sp:    pause
        dec     edx
        jz      @rc // timeout
        {$endif FPCMM_SLEEPTSC}
        mov     eax, $100
        {$ifdef FPCMM_CMPBEFORELOCK_SPIN}
        cmp     byte ptr [rcx], true
        je      @sp
        {$endif FPCMM_CMPBEFORELOCK_SPIN}
  lock  cmpxchg byte ptr [rcx], ah
        je      @ok
        jmp     @sp
@rc:    call    ReleaseCore
        lea     rax, [rip + HeapStatus]
        {$ifdef FPCMM_DEBUG} lock {$endif}
        inc     qword ptr [rax].TMMStatus.Large.SleepCount
        jmp     @s
@ok:    // reset the stack frame before ret
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
    header := RemapLarge(existing, oldsize, blocksize);
  if header <> nil then
  begin
    NotifyArenaAlloc(HeapStatus.Large, blocksize);
    if existing <> nil then
      NotifyMediumLargeFree(HeapStatus.Large, oldsize);
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
  NotifyMediumLargeFree(HeapStatus.Large, size);
  FreeMediumLarge(ptr, size);
end;

function FreeLargeBlock(p: pointer): PtrInt;
var
  header, prev, next: PLargeBlockHeader;
begin
  header := pointer(PByte(p) - LargeBlockHeaderSize);
  if header.BlockSizeAndFlags and IsFreeBlockFlag <> 0 then
  begin
    // try to release the same pointer twice
    result := 0;
    exit;
  end;
  LockLargeBlocks;
  prev := header.PreviousLargeBlockHeader;
  next := header.NextLargeBlockHeader;
  next.PreviousLargeBlockHeader := prev;
  prev.NextLargeBlockHeader := next;
  LargeBlocksLocked := False;
  result := DropMediumAndLargeFlagsMask and header.BlockSizeAndFlags;
  FreeLarge(header, result);
end;

function ReallocateLargeBlock(p: pointer; size: PtrUInt): pointer;
var
  oldavail, minup, new: PtrUInt;
  prev, next, header: PLargeBlockHeader;
begin
  header := pointer(PByte(p) - LargeBlockHeaderSize);
  oldavail := (DropMediumAndLargeFlagsMask and header^.BlockSizeAndFlags) -
              (LargeBlockHeaderSize + BlockHeaderSize);
  new := size;
  if size > oldavail then
  begin
    // size-up with 1/8 or 1/4 overhead for any future growing realloc
    if oldavail > 128 shl 20 then
      minup := oldavail + oldavail shr 3
    else
      minup := oldavail + oldavail shr 2;
    if size < minup then
      new := minup;
  end
  else
  begin
    result := p;
    oldavail := oldavail shr 1;
    if size >= oldavail then
      // small size-up within current buffer -> no reallocate
      exit
    else
      // size-down and move just the trailing data
      oldavail := size;
  end;
  if new < MaximumMediumBlockSize then
  begin
    // size was reduced to a small/medium block: use GetMem/Move/FreeMem
    result := _GetMem(new);
    if result <> nil then
      Move(p^, result^, oldavail); // RTL non-volatile asm or our AVX MoveFast()
    _FreeMem(p);
  end
  else
  begin
    // remove large block from current chain list
    LockLargeBlocks;
    prev := header^.PreviousLargeBlockHeader;
    next := header^.NextLargeBlockHeader;
    next.PreviousLargeBlockHeader := prev;
    prev.NextLargeBlockHeader := next;
    LargeBlocksLocked := False;
    size := DropMediumAndLargeFlagsMask and header^.BlockSizeAndFlags;
    // on Linux, call Kernel mremap() and its TLB magic
    // on Windows, try to reserve the memory block just after the existing
    // otherwise, use Alloc/Move/Free pattern, with asm/AVX move
    result := AllocateLargeBlockFrom(new, header, size);
  end;
end;


{ ********* Main Memory Manager Functions }

function _GetMem(size: PtrUInt): pointer;
  {$ifdef NOSFRAME} nostackframe; {$endif} assembler;
asm
        {$ifndef MSWINDOWS}
        mov     rcx, size
        {$else}
        push    rsi
        push    rdi
        {$endif MSWINDOWS}
        push    rbx
        // Since most allocations are for small blocks, determine small block type
        lea     rbx, [rip + SmallBlockInfo]
@VoidSizeToSomething:
        lea     rdx, [size + BlockHeaderSize - 1]
        shr     rdx, 4 // div SmallBlockGranularity
        // Is it a tiny/small block?
        cmp     size, (MaximumSmallBlockSize - BlockHeaderSize)
        ja      @NotTinySmallBlock
        test    size, size
        jz      @VoidSize
        {$ifndef FPCMM_ASSUMEMULTITHREAD}
        mov     rax, qword ptr [rbx].TSmallBlockInfo.IsMultiThreadPtr
        {$endif FPCMM_ASSUMEMULTITHREAD}
        // Get the tiny/small TSmallBlockType[] offset in rcx
        movzx   ecx, byte ptr [rbx + rdx].TSmallBlockInfo.GetmemLookup
        mov     r8, rbx
        shl     ecx, SmallBlockTypePO2
        // ---------- Acquire block type lock ----------
        {$ifndef FPCMM_ASSUMEMULTITHREAD}
        cmp     byte ptr [rax], false
        je      @GotLockOnSmallBlock // no lock if IsMultiThread=false
        {$endif FPCMM_ASSUMEMULTITHREAD}
        // Can use one of the several arenas reserved for tiny blocks?
        cmp     ecx, SizeOf(TTinyBlockTypes)
        jae     @NotTinyBlockType
        // ---------- TINY (size<=128B) block lock ----------
@LockTinyBlockTypeLoop:
        // Round-Robin attempt to lock next SmallBlockInfo.Tiny[]
        // -> fair distribution among calls to reduce thread contention
        mov     dl, NumTinyBlockArenas + 1 // 8/16 arenas (including Small[])
@TinyBlockArenaLoop:
        mov     eax, SizeOf(TTinyBlockTypes)
        // note: "lock xadd" decreases the loop iterations but is slower
        xadd    dword ptr [r8 + TSmallBlockInfo.TinyCurrentArena], eax
        lea     rbx, [r8 + rcx]
        and     eax, ((NumTinyBlockArenas + 1) * SizeOf(TTinyBlockTypes)) - 1
        jz      @TinySmall // Arena 0 = TSmallBlockInfo.Small[]
	lea	rbx, [rax + rbx + TSmallBlockInfo.Tiny - SizeOf(TTinyBlockTypes)]
@TinySmall:
        mov     eax, $100
        {$ifdef FPCMM_CMPBEFORELOCK}
        cmp     byte ptr [rbx].TSmallBlockType.Locked, false
        jnz     @NextTinyBlockArena1
        {$endif FPCMM_CMPBEFORELOCK}
  lock  cmpxchg byte ptr [rbx].TSmallBlockType.Locked, ah
        je      @GotLockOnSmallBlockType
@NextTinyBlockArena1:
        dec     dl
        jnz     @TinyBlockArenaLoop
        // Fallback to SmallBlockInfo.Small[] next 2 small sizes - never occurs
        lea     rbx, [r8 + rcx + TSmallBlockInfo.Small + SizeOf(TSmallBlockType)]
        mov     eax, $100
  lock  cmpxchg byte ptr [rbx].TSmallBlockType.Locked, ah
        je      @GotLockOnSmallBlockType
        add     rbx, SizeOf(TSmallBlockType) // next two small sizes
        mov     eax, $100
  lock  cmpxchg byte ptr [rbx].TSmallBlockType.Locked, ah
        je      @GotLockOnSmallBlockType
        // Thread Contention (_Freemem is more likely)
        {$ifdef FPCMM_DEBUG} lock {$endif}
        inc     dword ptr [rbx].TSmallBlockType.GetmemSleepCount
        push    r8
        push    rcx
        call    ReleaseCore
        pop     rcx
        pop     r8
        jmp     @LockTinyBlockTypeLoop
        // ---------- SMALL (size<2600) block lock ----------
@NotTinyBlockType:
        lea     rbx, [r8 + rcx].TSmallBlockInfo.Small
@LockBlockTypeLoopRetry:
        {$ifdef FPCMM_PAUSE}
        {$ifdef FPCMM_SLEEPTSC}
        rdtsc
        shl     rdx, 32
        lea     r9, [rax + rdx + SpinSmallGetmemLockTSC] // r9 = endtsc
        {$else}
        mov    edx, SpinSmallGetmemLockCount
        {$endif FPCMM_SLEEPTSC}
        {$endif FPCMM_PAUSE}
@LockBlockTypeLoop:
        // Grab the default block type
        mov     eax, $100
        {$ifdef FPCMM_CMPBEFORELOCK}
        cmp     byte ptr [rbx].TSmallBlockType.Locked, false
        jnz     @NextLockBlockType1
        {$endif FPCMM_CMPBEFORELOCK}
  lock  cmpxchg byte ptr [rbx].TSmallBlockType.Locked, ah
        je      @GotLockOnSmallBlockType
        // Try up to two next sizes
        mov     eax, $100
@NextLockBlockType1:
        add     rbx, SizeOf(TSmallBlockType)
        {$ifdef FPCMM_CMPBEFORELOCK}
        cmp     byte ptr [rbx].TSmallBlockType.Locked, al
        jnz     @NextLockBlockType2
        {$endif FPCMM_CMPBEFORELOCK}
  lock  cmpxchg byte ptr [rbx].TSmallBlockType.Locked, ah
        je      @GotLockOnSmallBlockType
        mov     eax, $100
@NextLockBlockType2:
        add     rbx, SizeOf(TSmallBlockType)
        pause
        {$ifdef FPCMM_CMPBEFORELOCK}
        cmp     byte ptr [rbx].TSmallBlockType.Locked, al
        jnz     @NextLockBlockType3
        {$endif FPCMM_CMPBEFORELOCK}
  lock  cmpxchg byte ptr [rbx].TSmallBlockType.Locked, ah
        je      @GotLockOnSmallBlockType
@NextLockBlockType3:
        sub     rbx, 2 * SizeOf(TSmallBlockType)
        {$ifdef FPCMM_PAUSE}
        pause
        {$ifdef FPCMM_SLEEPTSC}
        rdtsc
        shl     rdx, 32
        or      rax, rdx
        cmp     rax, r9
        jb      @LockBlockTypeLoop // continue spinning until timeout
        {$else}
        dec     edx
        jnz     @LockBlockTypeLoop // continue until spin count reached
        {$endif FPCMM_SLEEPTSC}
        {$endif FPCMM_PAUSE}
        // Block type and two sizes larger are all locked - give up and sleep
        {$ifdef FPCMM_DEBUG} lock {$endif}
        inc     dword ptr [rbx].TSmallBlockType.GetmemSleepCount
        call    ReleaseCore
        jmp     @LockBlockTypeLoopRetry
        // ---------- TINY/SMALL block registration ----------
        {$ifndef FPCMM_ASSUMEMULTITHREAD}
@GotLockOnSmallBlock:
        add     rbx, rcx
        {$endif FPCMM_ASSUMEMULTITHREAD}
@GotLockOnSmallBlockType:
        // set rdx=NextPartiallyFreePool rax=FirstFreeBlock rcx=DropSmallFlagsMask
        mov     rdx, [rbx].TSmallBlockType.NextPartiallyFreePool
        add     [rbx].TSmallBlockType.GetmemCount, 1
        mov     rax, [rdx].TSmallBlockPoolHeader.FirstFreeBlock
        mov     rcx, DropSmallFlagsMask
        // Is there a pool with free blocks?
        cmp     rdx, rbx
        je      @TrySmallSequentialFeed
        add     [rdx].TSmallBlockPoolHeader.BlocksInUse, 1
        // Set the new first free block and the block header
        and     rcx, [rax - BlockHeaderSize]
        mov     [rdx].TSmallBlockPoolHeader.FirstFreeBlock, rcx
        mov     [rax - BlockHeaderSize], rdx
        // Is the chunk now full?
        jz      @RemoveSmallPool
        // Unlock the block type and leave
        mov     byte ptr [rbx].TSmallBlockType.Locked, false
        {$ifdef NOSFRAME}
        pop     rbx
        ret
        {$else}
        jmp     @Done // on Win64, a stack frame is required
        {$endif NOSFRAME}
@VoidSize:
        inc     size // "we always need to allocate something" (see RTL heap.inc)
        jmp     @VoidSizeToSomething
@TrySmallSequentialFeed:
        // Feed a small block sequentially
        movzx   ecx, [rbx].TSmallBlockType.BlockSize
        mov     rdx, [rbx].TSmallBlockType.CurrentSequentialFeedPool
        add     rcx, rax
        // Can another block fit?
        cmp     rax, [rbx].TSmallBlockType.MaxSequentialFeedBlockAddress
        ja      @AllocateSmallBlockPool
        // Adjust number of used blocks and sequential feed pool
        mov     [rbx].TSmallBlockType.NextSequentialFeedBlockAddress, rcx
        add     [rdx].TSmallBlockPoolHeader.BlocksInUse, 1
        // Unlock the block type, set the block header and leave
        mov     byte ptr [rbx].TSmallBlockType.Locked, false
        mov     [rax - BlockHeaderSize], rdx
        {$ifdef NOSFRAME}
        pop     rbx
        ret
        {$else}
        jmp     @Done // on Win64, a stack frame is required
        {$endif NOSFRAME}
@RemoveSmallPool:
        // Pool is full - remove it from the partially free list
        mov     rcx, [rdx].TSmallBlockPoolHeader.NextPartiallyFreePool
        mov     [rcx].TSmallBlockPoolHeader.PreviousPartiallyFreePool, rbx
        mov     [rbx].TSmallBlockType.NextPartiallyFreePool, rcx
        // Unlock the block type and leave
        mov     byte ptr [rbx].TSmallBlockType.Locked, false
        {$ifdef NOSFRAME}
        pop     rbx
        ret
        {$else}
        jmp     @Done // on Win64, a stack frame is required
        {$endif NOSFRAME}
@AllocateSmallBlockPool:
        // Access shared information about Medium blocks storage
        lea     rcx, [rip + SmallMediumBlockInfo]
        mov     r10, rcx
        {$ifndef FPCMM_ASSUMEMULTITHREAD}
        mov     rax, [rcx + TMediumBlockinfo.IsMultiThreadPtr]
        cmp     byte ptr [rax], false
        je      @MediumLocked1 // no lock if IsMultiThread=false
        {$endif FPCMM_ASSUMEMULTITHREAD}
        mov     eax, $100
  lock  cmpxchg byte ptr [rcx].TMediumBlockInfo.Locked, ah
        je      @MediumLocked1
        call    LockMediumBlocks
@MediumLocked1:
        // Are there any available blocks of a suitable size?
        movsx   esi, [rbx].TSmallBlockType.AllowedGroupsForBlockPoolBitmap
        and     esi, [r10 + TMediumBlockInfo.BinGroupBitmap]
        jz      @NoSuitableMediumBlocks
        // Compute rax = bin group number with free blocks, rcx = bin number
        bsf     eax, esi
        lea     r9, [rax * 4]
        mov     ecx, [r10 + TMediumBlockInfo.BinBitmaps + r9]
        bsf     ecx, ecx
        lea     rcx, [rcx + r9 * 8]
        // Set rdi = @bin, rsi = free block
        lea     rsi, [rcx * 8] // SizeOf(TMediumBlockBin) = 16
        lea     rdi, [r10 + TMediumBlockInfo.Bins + rsi * 2]
        mov     rsi, TMediumFreeBlock[rdi].NextFreeBlock
        // Remove the first block from the linked list (LIFO)
        mov     rdx, TMediumFreeBlock[rsi].NextFreeBlock
        mov     TMediumFreeBlock[rdi].NextFreeBlock, rdx
        mov     TMediumFreeBlock[rdx].PreviousFreeBlock, rdi
        // Is this bin now empty?
        cmp     rdi, rdx
        jne     @MediumBinNotEmpty
        // rbx = block type, rax = bin group number,
        // r9 = bin group number * 4, rcx = bin number, rdi = @bin, rsi = free block
        // Flag this bin (and the group if needed) as empty
        mov     edx,  - 2
        mov     r11d, [r10 + TMediumBlockInfo.BinGroupBitmap]
        rol     edx, cl
        btr     r11d, eax // btr reg,reg is faster than btr [mem],reg
        and     [r10 + TMediumBlockInfo.BinBitmaps + r9], edx
        jnz     @MediumBinNotEmpty
        mov     [r10 + TMediumBlockInfo.BinGroupBitmap], r11d
@MediumBinNotEmpty:
        // rsi = free block, rbx = block type
        // Get the size of the available medium block in edi
        mov     rdi, DropMediumAndLargeFlagsMask
        and     rdi, [rsi - BlockHeaderSize]
        cmp     edi, MaximumSmallBlockPoolSize
        jb      @UseWholeBlock
        // Split the block: new block size is the optimal size
        mov     edx, edi
        movzx   edi, [rbx].TSmallBlockType.OptimalBlockPoolSize
        sub     edx, edi
        lea     rcx, [rsi + rdi]
        lea     rax, [rdx + IsMediumBlockFlag + IsFreeBlockFlag]
        mov     [rcx - BlockHeaderSize], rax
        // Store the size of the second split as the second last pointer
        mov     [rcx + rdx - 16], rdx
        // Put the remainder in a bin (it will be big enough)
        call    InsertMediumBlockIntoBin // rcx=APMediumFreeBlock, edx=AMediumBlockSize
        jmp     @GotMediumBlock
@NoSuitableMediumBlocks:
        // Check the sequential feed medium block pool for space
        movzx   ecx, [rbx].TSmallBlockType.MinimumBlockPoolSize
        mov     edi, [r10 + TMediumBlockInfo.SequentialFeedBytesLeft]
        cmp     edi, ecx
        jb      @AllocateNewSequentialFeed
        // Get the address of the last block that was fed
        mov     rsi, [r10 + TMediumBlockInfo.LastSequentiallyFed]
        // Enough sequential feed space: Will the remainder be usable?
        movzx   ecx, [rbx].TSmallBlockType.OptimalBlockPoolSize
        lea     rdx, [rcx + MinimumMediumBlockSize]
        cmp     edi, edx
        cmovae  edi, ecx
        sub     rsi, rdi
        // Update the sequential feed parameters
        sub     [r10 + TMediumBlockInfo.SequentialFeedBytesLeft], edi
        mov     [r10 + TMediumBlockInfo.LastSequentiallyFed], rsi
        jmp     @GotMediumBlock
@AllocateNewSequentialFeed:
        // Use the optimal size for allocating this small block pool
        {$ifdef MSWINDOWS}
        movzx   ecx, word ptr [rbx].TSmallBlockType.OptimalBlockPoolSize
        lea     rdx, [rip + SmallMediumBlockInfo]
        push    rcx
        push    rdx
        {$else}
        movzx   edi, word ptr [rbx].TSmallBlockType.OptimalBlockPoolSize
        lea     rsi, [rip + SmallMediumBlockInfo]
        push    rdi
        push    rsi
        {$endif MSWINDOWS}
        call    AllocNewSequentialFeedMediumPool
        pop     r10
        pop     rdi  // restore edi=blocksize and r10=TMediumBlockInfo
        mov     rsi, rax
        test    rax, rax
        jnz     @GotMediumBlock // rsi=freeblock rbx=blocktype edi=blocksize
        mov     [r10 + TMediumBlockInfo.Locked], al
        mov     [rbx].TSmallBlockType.Locked, al
        {$ifdef NOSFRAME}
        pop     rbx
        ret
        {$else}
        jmp     @Done // on Win64, a stack frame is required
        {$endif NOSFRAME}
@UseWholeBlock:
        // rsi = free block, rbx = block type, edi = block size
        // Mark this block as used in the block following it
        and     byte ptr [rsi + rdi - BlockHeaderSize],  NOT PreviousMediumBlockIsFreeFlag
@GotMediumBlock:
        // rsi = free block, rbx = block type, edi = block size
        // Set the size and flags for this block
        lea     rcx, [rdi + IsMediumBlockFlag + IsSmallBlockPoolInUseFlag]
        mov     [rsi - BlockHeaderSize], rcx
        // Unlock medium blocks and setup the block pool
        xor     eax, eax
        mov     [r10 + TMediumBlockInfo.Locked], al
        mov     TSmallBlockPoolHeader[rsi].BlockType, rbx
        mov     TSmallBlockPoolHeader[rsi].FirstFreeBlock, rax
        mov     TSmallBlockPoolHeader[rsi].BlocksInUse, 1
        mov     [rbx].TSmallBlockType.CurrentSequentialFeedPool, rsi
        // Return the pointer to the first block, compute next/last block addresses
        lea     rax, [rsi + SmallBlockPoolHeaderSize]
        movzx   ecx, [rbx].TSmallBlockType.BlockSize
        lea     rdx, [rax + rcx]
        mov     [rbx].TSmallBlockType.NextSequentialFeedBlockAddress, rdx
        add     rdi, rsi
        sub     rdi, rcx
        mov     [rbx].TSmallBlockType.MaxSequentialFeedBlockAddress, rdi
        // Unlock the small block type, set header and leave
        mov     byte ptr [rbx].TSmallBlockType.Locked, false
        mov     [rax - BlockHeaderSize], rsi
        {$ifdef NOSFRAME}
        pop     rbx
        ret
        {$else}
        jmp     @Done // on Win64, a stack frame is required
        {$endif NOSFRAME}
        // ---------- MEDIUM block allocation ----------
@NotTinySmallBlock:
        // Do we need a Large block?
        lea     r10, [rip + MediumBlockInfo]
        cmp     rcx, MaximumMediumBlockSize - BlockHeaderSize
        ja      @IsALargeBlockRequest
        // Get the bin size for this block size (rounded up to the next bin size)
        lea     rbx, [rcx + MediumBlockGranularity - 1 + BlockHeaderSize - MediumBlockSizeOffset]
        mov     rcx, r10
        and     ebx,  - MediumBlockGranularity
        add     ebx, MediumBlockSizeOffset
        {$ifndef FPCMM_ASSUMEMULTITHREAD}
        mov     rax, [r10 + TMediumBlockinfo.IsMultiThreadPtr]
        cmp     byte ptr [rax], false
        je      @MediumLocked2 // no lock if IsMultiThread=false
        {$endif FPCMM_ASSUMEMULTITHREAD}
        mov     eax, $100
  lock  cmpxchg byte ptr [rcx].TMediumBlockInfo.Locked, ah
        je      @MediumLocked2
        call    LockMediumBlocks
@MediumLocked2:
        // Compute ecx = bin number in ecx and edx = group number
        lea     rdx, [rbx - MinimumMediumBlockSize]
        mov     ecx, edx
        shr     edx, 8 + 5
        shr     ecx, 8
        mov     eax, -1
        shl     eax, cl
        and     eax, [r10 + TMediumBlockInfo.BinBitmaps + rdx * 4]
        jz      @GroupIsEmpty
        and     ecx,  - 32
        bsf     eax, eax
        or      ecx, eax
        jmp     @GotBinAndGroup
@GroupIsEmpty:
        // Try all groups greater than this group
        mov     eax,  - 2
        mov     ecx, edx
        shl     eax, cl
        and     eax, [r10 + TMediumBlockInfo.BinGroupBitmap]
        jz      @TrySequentialFeedMedium
        // There is a suitable group with enough space
        bsf     edx, eax
        mov     eax, [r10 + TMediumBlockInfo.BinBitmaps + rdx * 4]
        bsf     ecx, eax
        mov     eax, edx
        shl     eax, 5
        or      ecx, eax
        jmp     @GotBinAndGroup
@TrySequentialFeedMedium:
        mov     ecx, [r10 + TMediumBlockInfo.SequentialFeedBytesLeft]
        // Can block be fed sequentially?
        sub     ecx, ebx
        jc      @AllocateNewSequentialFeedForMedium
        // Get the block address, store remaining bytes, set the flags and unlock
        mov     rax, [r10 + TMediumBlockInfo.LastSequentiallyFed]
        sub     rax, rbx
        mov     [r10 + TMediumBlockInfo.LastSequentiallyFed], rax
        mov     [r10 + TMediumBlockInfo.SequentialFeedBytesLeft], ecx
        or      rbx, IsMediumBlockFlag
        mov     [rax - BlockHeaderSize], rbx
        mov     byte ptr [r10 + TMediumBlockInfo.Locked], false
        {$ifdef NOSFRAME}
        pop     rbx
        ret
        {$else}
        jmp     @Done // on Win64, a stack frame is required
        {$endif NOSFRAME}
@AllocateNewSequentialFeedForMedium:
        {$ifdef MSWINDOWS}
        mov     ecx, ebx
        lea     rdx, [rip + MediumBlockInfo]
        {$else}
        mov     edi, ebx
        lea     rsi, [rip + MediumBlockInfo]
        {$endif MSWINDOWS}
        call    AllocNewSequentialFeedMediumPool
        mov     byte ptr [rip + MediumBlockInfo.Locked], false
        {$ifdef NOSFRAME}
        pop     rbx
        ret
        {$else}
        jmp     @Done // on Win64, a stack frame is required
        {$endif NOSFRAME}
@GotBinAndGroup:
        // ebx = block size, ecx = bin number, edx = group number
        // Compute rdi = @bin, rsi = free block
        lea     rax, [rcx + rcx]
        lea     rdi, [r10 + TMediumBlockInfo.Bins + rax * 8]
        mov     rsi, TMediumFreeBlock[rdi].NextFreeBlock
        // Remove the first block from the linked list (LIFO)
        mov     rax, TMediumFreeBlock[rsi].NextFreeBlock
        mov     TMediumFreeBlock[rdi].NextFreeBlock, rax
        mov     TMediumFreeBlock[rax].PreviousFreeBlock, rdi
        // Is this bin now empty?
        cmp     rdi, rax
        jne     @MediumBinNotEmptyForMedium
        // edx=bingroupnumber, ecx=binnumber, rdi=@bin, rsi=freeblock, ebx=blocksize
        // Flag this bin and group as empty
        mov     eax,  - 2
        mov     r11d, [r10 + TMediumBlockInfo.BinGroupBitmap]
        rol     eax, cl
        btr     r11d, edx // btr reg,reg is faster than btr [mem],reg
        and     [r10 + TMediumBlockInfo.BinBitmaps + rdx * 4], eax
        jnz     @MediumBinNotEmptyForMedium
        mov     [r10 + TMediumBlockInfo.BinGroupBitmap], r11d
@MediumBinNotEmptyForMedium:
        // rsi = free block, ebx = block size
        // Get rdi = size of the available medium block, rdx = second split size
        mov     rdi, DropMediumAndLargeFlagsMask
        and     rdi, [rsi - BlockHeaderSize]
        mov     edx, edi
        sub     edx, ebx
        jz      @UseWholeBlockForMedium
        // Split the block in two
        lea     rcx, [rsi + rbx]
        lea     rax, [rdx + IsMediumBlockFlag + IsFreeBlockFlag]
        mov     [rcx - BlockHeaderSize], rax
        // Store the size of the second split as the second last pointer
        mov     [rcx + rdx - 16], rdx
        // Put the remainder in a bin
        cmp     edx, MinimumMediumBlockSize
        jb      @GotMediumBlockForMedium
        call    InsertMediumBlockIntoBin // rcx=APMediumFreeBlock, edx=AMediumBlockSize
        jmp     @GotMediumBlockForMedium
@UseWholeBlockForMedium:
        // Mark this block as used in the block following it
        and     byte ptr [rsi + rdi - BlockHeaderSize],  NOT PreviousMediumBlockIsFreeFlag
@GotMediumBlockForMedium:
        // Set the size and flags for this block
        lea     rcx, [rbx + IsMediumBlockFlag]
        mov     [rsi - BlockHeaderSize], rcx
        // Unlock medium blocks and leave
        mov     byte ptr [r10 + TMediumBlockInfo.Locked], false
        mov     rax, rsi
        {$ifdef NOSFRAME}
        pop     rbx
        ret
        {$else}
        jmp     @Done // on Win64, a stack frame is required
        {$endif NOSFRAME}
        // ---------- LARGE block allocation ----------
@IsALargeBlockRequest:
        xor     rax, rax
        test    rcx, rcx
        js      @Done
        // Note: size is still in the rcx/rdi first param register
        call    AllocateLargeBlock
@Done:  // restore registers and the stack frame before ret
        pop     rbx
        {$ifdef MSWINDOWS}
        pop     rdi
        pop     rsi
        {$endif MSWINDOWS}
end;

function FreeMediumBlock(arg1: pointer): PtrUInt;
  {$ifdef NOSFRAME} nostackframe; {$endif} assembler;
// rcx=P rdx=[P-BlockHeaderSize] r10=TMediumBlockInfo
// (arg1 is used only for proper call of pascal functions below on all ABI)
asm
        // Drop the flags, and set r11=P rbx=blocksize
        and     rdx, DropMediumAndLargeFlagsMask
        push    rbx
        push    rdx // save blocksize
        mov     rbx, rdx
        mov     r11, rcx
        // Lock the Medium blocks
        mov     rcx, r10
        {$ifndef FPCMM_ASSUMEMULTITHREAD}
        mov     rax, [r10 + TMediumBlockinfo.IsMultiThreadPtr]
        cmp     byte ptr [rax], false
        je      @MediumBlocksLocked // no lock if IsMultiThread=false
        {$endif FPCMM_ASSUMEMULTITHREAD}
        mov     eax, $100
  lock  cmpxchg byte ptr [rcx].TMediumBlockInfo.Locked, ah
        je      @MediumBlocksLocked
        {$ifdef FPCMM_LOCKLESSFREEMEDIUM}
        // locked: try to put r11=P in TMediumBlockInfo.LocklessBin.Instance[]
        lea     rcx, [rcx].TMediumBlockInfo.LocklessBin
        cmp     byte ptr [rcx].TMediumLocklessBin.Count, MediumBlockLocklessBinCount
        je      @DoLock // all slots are filled
        mov     r9d, SpinFreememBinCount
@BinSp: mov     eax, $100
  lock  cmpxchg byte ptr [rcx].TMediumLocklessBin.Locked, ah
        je      @BinOk
@BinNo: pause
        dec     r9d
        jnz     @BinSp
        jmp     @DoLock
@BinOk: // we acquired TMediumLocklessBin.Locked
        movzx   eax, byte ptr [rcx].TMediumLocklessBin.Count
        cmp     al, MediumBlockLocklessBinCount
        je      @DoLoc2
        add     byte ptr [rcx].TMediumLocklessBin.Count, 1

        cmp     al, byte ptr [rcx].TMediumLocklessBin.MaxCount
        jb      @max
        mov     byte ptr [rcx].TMediumLocklessBin.MaxCount, al
@max:
        mov     [rcx + TMediumLocklessBin.Instance + rax * 8], r11
        mov     byte ptr [rcx].TMediumLocklessBin.Locked, false
        jmp     @Quit
@DoLoc2:mov     byte ptr [rcx].TMediumLocklessBin.Locked, false
@DoLock:{$endif FPCMM_LOCKLESSFREEMEDIUM}
        call    LockMediumBlocks
@MediumBlocksLocked:
        // Get rcx = next block size and flags
        mov     rcx, [r11 + rbx - BlockHeaderSize]
        // Can we combine this block with the next free block?
        test    qword ptr [r11 + rbx - BlockHeaderSize], IsFreeBlockFlag
        jnz     @NextBlockIsFree
        // Set the "PreviousIsFree" flag in the next block
        or      rcx, PreviousMediumBlockIsFreeFlag
        mov     [r11 + rbx - BlockHeaderSize], rcx
@NextBlockChecked:
        // Re-read the flags and try to combine with previous free block
        test    byte ptr [r11 - BlockHeaderSize], PreviousMediumBlockIsFreeFlag
        jnz     @PreviousBlockIsFree
@PreviousBlockChecked:
        // Check if entire medium block pool is free
        cmp     ebx, (MediumBlockPoolSize - MediumBlockPoolHeaderSize)
        je      @EntireMediumPoolFree
@Bin:   // Store size of the block, flags and trailing size marker and insert into bin
        lea     rax, [rbx + IsMediumBlockFlag + IsFreeBlockFlag]
        mov     [r11 - BlockHeaderSize], rax
        mov     [r11 + rbx - 16], rbx
        mov     rcx, r11
        mov     rdx, rbx
        call    InsertMediumBlockIntoBin // rcx=P, edx=blocksize
        {$ifdef FPCMM_LOCKLESSFREEMEDIUM}
        // recycle any pending TMediumLocklessBin.Instance[] pointer
        lea     rcx, [r10].TMediumBlockInfo.LocklessBin
        cmp     byte ptr [rcx].TMediumLocklessBin.Count, 0
        je      @Done
        mov     eax, $100
  lock  cmpxchg byte ptr [rcx].TMediumLocklessBin.Locked, ah // just try once
        jne     @Done
        // compute r11=P and rbx=blocksize from pending pointer
        movzx   eax, byte ptr [rcx].TMediumLocklessBin.Count
        dec     byte ptr [rcx].TMediumLocklessBin.Count
        mov     r11, [rcx + TMediumLocklessBin.Instance - 8 + rax * 8]
        mov     byte ptr [rcx].TMediumLocklessBin.Locked, false
        mov     rbx, qword ptr [r11 - BlockHeaderSize]
        and     rbx, DropMediumAndLargeFlagsMask
        jmp     @MediumBlocksLocked
@Done:  {$endif FPCMM_LOCKLESSFREEMEDIUM}
        // Unlock medium blocks and leave
        mov     byte ptr [r10 + TMediumBlockInfo.Locked], false
        jmp     @Quit
@NextBlockIsFree:
        // Get rax = next block address, rbx = end of the block
        lea     rax, [r11 + rbx]
        and     rcx, DropMediumAndLargeFlagsMask
        add     rbx, rcx
        // Was the block binned?
        cmp     rcx, MinimumMediumBlockSize
        jb      @NextBlockChecked
        mov     rcx, rax
        call    RemoveMediumFreeBlock // rcx = APMediumFreeBlock
        jmp     @NextBlockChecked
@PreviousBlockIsFree:
        // Get rcx =  size/point of the previous free block, rbx = new block end
        mov     rcx, [r11 - 16]
        sub     r11, rcx
        add     rbx, rcx
        // Remove the previous block from the linked list
        cmp     ecx, MinimumMediumBlockSize
        jb      @PreviousBlockChecked
        mov     rcx, r11
        call    RemoveMediumFreeBlock // rcx = APMediumFreeBlock
        jmp     @PreviousBlockChecked
@EntireMediumPoolFree:
        // Ensure current sequential feed pool is free
        cmp     dword ptr [r10 + TMediumBlockInfo.SequentialFeedBytesLeft], MediumBlockPoolSize - MediumBlockPoolHeaderSize
        jne     @MakeEmptyMediumPoolSequentialFeed
        // Remove this medium block pool from the linked list stored in its header
        sub     r11, MediumBlockPoolHeaderSize
        mov     rax, TMediumBlockPoolHeader[r11].PreviousMediumBlockPoolHeader
        mov     rdx, TMediumBlockPoolHeader[r11].NextMediumBlockPoolHeader
        mov     TMediumBlockPoolHeader[rax].NextMediumBlockPoolHeader, rdx
        mov     TMediumBlockPoolHeader[rdx].PreviousMediumBlockPoolHeader, rax
        // Unlock medium blocks and free the block pool
        mov     byte ptr [r10 + TMediumBlockInfo.Locked], false
        mov     arg1, r11
        call    FreeMedium
        jmp     @Quit
@MakeEmptyMediumPoolSequentialFeed:
        // Get rbx = end-marker block, and recycle the current sequential feed pool
        lea     rbx, [r11 + MediumBlockPoolSize - MediumBlockPoolHeaderSize]
        mov     arg1, r10
        call    BinMediumSequentialFeedRemainder
        // Set this medium pool up as the new sequential feed pool, unlock and leave
        mov     qword ptr [rbx - BlockHeaderSize], IsMediumBlockFlag
        mov     dword ptr [r10 + TMediumBlockInfo.SequentialFeedBytesLeft], MediumBlockPoolSize - MediumBlockPoolHeaderSize
        mov     [r10 + TMediumBlockInfo.LastSequentiallyFed], rbx
        mov     byte ptr [r10 + TMediumBlockInfo.Locked], false
@Quit:  // restore registers and the stack frame
        pop     rax // medium block size
        pop     rbx
end;

{$ifdef FPCMM_REPORTMEMORYLEAKS}
const
  /// mark freed blocks with 00000000 BLODLESS marker to track incorrect usage
  REPORTMEMORYLEAK_FREEDHEXSPEAK = $B10D1E55;
{$endif FPCMM_REPORTMEMORYLEAKS}

function _FreeMem(P: pointer): PtrUInt;
  {$ifdef NOSFRAME} nostackframe; {$endif} assembler;
asm
        {$ifdef FPCMM_REPORTMEMORYLEAKS}
        mov     eax, REPORTMEMORYLEAK_FREEDHEXSPEAK // 00000000 BLODLESS marker
        {$endif FPCMM_REPORTMEMORYLEAKS}
        {$ifndef MSWINDOWS}
        mov     rcx, P
        {$endif MSWINDOWS}
        test    P, P
        jz      @Quit  // void pointer
        {$ifdef FPCMM_REPORTMEMORYLEAKS}
        mov     qword ptr [P], rax // over TObject VMT or string/dynarray header
        {$endif FPCMM_REPORTMEMORYLEAKS}
        mov     rdx, qword ptr [P - BlockHeaderSize]
        {$ifdef FPCMM_ASSUMEMULTITHREAD}
        mov     eax, $100
        {$else}
        mov     rax, qword ptr [rip + SmallBlockInfo].TSmallBlockInfo.IsMultiThreadPtr
        {$endif FPCMM_ASSUMEMULTITHREAD}
        // Is it a small block in use?
        test    dl, IsFreeBlockFlag + IsMediumBlockFlag + IsLargeBlockFlag
        jnz     @NotSmallBlockInUse
        // Get the small block type in rbx and try to grab it
        push    rbx
        mov     rbx, [rdx].TSmallBlockPoolHeader.BlockType
        {$ifdef FPCMM_ASSUMEMULTITHREAD}
  lock  cmpxchg byte ptr [rbx].TSmallBlockType.Locked, ah
        jne     @CheckTinySmallLock
        {$else}
        cmp     byte ptr [rax], false
        jne     @TinySmallLockLoop // lock if IsMultiThread=true
        {$endif FPCMM_ASSUMEMULTITHREAD}
@FreeAndUnlock:
        // rbx=TSmallBlockType rcx=P rdx=TSmallBlockPoolHeader
        // Adjust number of blocks in use, set rax = old first free block
        add     [rbx].TSmallBlockType.FreememCount, 1
        mov     rax, [rdx].TSmallBlockPoolHeader.FirstFreeBlock
        sub     [rdx].TSmallBlockPoolHeader.BlocksInUse, 1
        jz      @PoolIsNowEmpty
        // Store this as the new first free block
        mov     [rdx].TSmallBlockPoolHeader.FirstFreeBlock, rcx
        // Store the previous first free block as the block header
        lea     r9, [rax + IsFreeBlockFlag]
        mov     [rcx - BlockHeaderSize], r9
        // Was the pool full?
        test    rax, rax
        jnz     @SmallPoolWasNotFull
        // Insert the pool back into the linked list if it was full
        mov     rcx, [rbx].TSmallBlockType.NextPartiallyFreePool
        mov     [rdx].TSmallBlockPoolHeader.PreviousPartiallyFreePool, rbx
        mov     [rdx].TSmallBlockPoolHeader.NextPartiallyFreePool, rcx
        mov     [rcx].TSmallBlockPoolHeader.PreviousPartiallyFreePool, rdx
        mov     [rbx].TSmallBlockType.NextPartiallyFreePool, rdx
@SmallPoolWasNotFull:
        {$ifdef FPCMM_LOCKLESSFREE}
        // Try to release all pending bin from this block while we have the lock
        cmp     byte ptr [rbx].TSmallBlockType.BinCount, 0
        jne     @ProcessPendingBin
        {$endif FPCMM_LOCKLESSFREE}
        mov     byte ptr [rbx].TSmallBlockType.Locked, false
        movzx   eax, word ptr [rbx].TSmallBlockType.BlockSize
        {$ifdef NOSFRAME}
        pop     rbx
        ret
        {$else}
        jmp     @Done // on Win64, a stack frame is required
        {$endif NOSFRAME}
@PoolIsNowEmpty:
        // FirstFreeBlock=nil means it is the sequential feed pool with a single block
        test    rax, rax
        jz      @IsSequentialFeedPool
        // Pool is now empty: Remove it from the linked list and free it
        mov     rax, [rdx].TSmallBlockPoolHeader.PreviousPartiallyFreePool
        mov     rcx, [rdx].TSmallBlockPoolHeader.NextPartiallyFreePool
        mov     TSmallBlockPoolHeader[rax].NextPartiallyFreePool, rcx
        mov     [rcx].TSmallBlockPoolHeader.PreviousPartiallyFreePool, rax
        // Is this the sequential feed pool? If so, stop sequential feeding
        xor     eax, eax
        cmp     [rbx].TSmallBlockType.CurrentSequentialFeedPool, rdx
        jne     @NotSequentialFeedPool
@IsSequentialFeedPool:
        mov     [rbx].TSmallBlockType.MaxSequentialFeedBlockAddress, rax
@NotSequentialFeedPool:
        // Unlock blocktype and release this pool
        mov     byte ptr [rbx].TSmallBlockType.Locked, false
        mov     rcx, rdx
        mov     rdx, qword ptr [rdx - BlockHeaderSize]
        lea     r10, [rip + SmallMediumBlockInfo]
        call    FreeMediumBlock // no call nor BinLocked to avoid race condition
        movzx   eax, word ptr [rbx].TSmallBlockType.BlockSize
        {$ifdef NOSFRAME}
        pop     rbx
        ret
        {$else}
        jmp     @Done // on Win64, a stack frame is required
        {$endif NOSFRAME}
{$ifdef FPCMM_LOCKLESSFREE}
@ProcessPendingBin:
        // Try once to acquire BinLocked (spinning may induce race condition)
        {$ifdef FPCMM_CMPBEFORELOCK}
        cmp     byte ptr [rbx].TSmallBlockType.BinLocked, true
        je      @BinAlreadyLocked
        {$endif FPCMM_CMPBEFORELOCK}
        mov     eax, $100
  lock  cmpxchg byte ptr [rbx].TSmallBlockType.BinLocked, ah
        jne     @BinAlreadyLocked
        movzx   eax, byte ptr [rbx].TSmallBlockType.BinCount
        test    al, al
        jz      @NoBin
        // free last pointer in TSmallBlockType.BinInstance[]
        mov     rcx, qword ptr [rbx + TSmallBlockType.BinInstance - 8 + rax * 8]
        dec     byte ptr [rbx].TSmallBlockType.BinCount
        mov     byte ptr [rbx].TSmallBlockType.BinLocked, false
        mov     rdx, [rcx - BlockHeaderSize]
        jmp     @FreeAndUnlock // loop until BinCount=0 or BinLocked
@NoBin: mov     byte ptr [rbx].TSmallBlockType.BinLocked, false
@BinAlreadyLocked:
        mov     byte ptr [rbx].TSmallBlockType.Locked, false
{$endif FPCMM_LOCKLESSFREE}
        movzx   eax, word ptr [rbx].TSmallBlockType.BlockSize
        {$ifdef NOSFRAME}
        pop     rbx
        ret
        {$else}
        jmp     @Done // on Win64, a stack frame is required
        {$endif NOSFRAME}
@NotSmallBlockInUse:
        lea     r10, [rip + MediumBlockInfo]
        test    dl, IsFreeBlockFlag + IsLargeBlockFlag
        jz      @DoFreeMedium
        call    FreeLargeBlock // P is still in rcx/rdi first param register
        jmp     @Quit
@DoFreeMedium:
        call    FreeMediumBlock
        jmp     @Quit
@TinySmallLockLoop:
        mov     eax, $100
  lock  cmpxchg byte ptr [rbx].TSmallBlockType.Locked, ah
        je      @FreeAndUnlock
@CheckTinySmallLock:
        {$ifdef FPCMM_LOCKLESSFREE}
        // Try to put rcx=P in TSmallBlockType.BinInstance[]
        cmp     byte ptr [rbx].TSmallBlockType.BinCount, SmallBlockBinCount
        je      @LockBlockTypeSleep // wait if all slots are filled
        mov     eax, $100
  lock  cmpxchg byte ptr [rbx].TSmallBlockType.BinLocked, ah
        je      @BinLocked
        {$ifdef FPCMM_PAUSE}
        {$ifdef FPCMM_SLEEPTSC}
        push    rdx
        rdtsc
        shl     rdx, 32
        lea     r9, [rax + rdx + SpinSmallFreememBinTSC] // r9 = endtsc
        {$else}
        mov     r9d, SpinFreememBinCount
        {$endif FPCMM_SLEEPTSC}
@SpinBinLock:
        pause
        {$ifdef FPCMM_SLEEPTSC}
        rdtsc
        shl     rdx, 32
        or      rax, rdx
        cmp     rax, r9
        ja      @SpinTimeout
        {$else}
        dec     r9
        jz      @SpinTimeout
        {$endif FPCMM_SLEEPTSC}
        {$ifdef FPCMM_CMPBEFORELOCK_SPIN}
        cmp     byte ptr [rbx].TSmallBlockType.BinLocked, true
        je      @SpinBinLock
        {$endif FPCMM_CMPBEFORELOCK_SPIN}
        mov     eax, $100
  lock  cmpxchg byte ptr [rbx].TSmallBlockType.BinLocked, ah
        jne     @SpinBinLock
        {$ifdef FPCMM_SLEEPTSC}
        pop     rdx
        {$endif FPCMM_SLEEPTSC}
        jmp     @BinLocked
@SpinTimeout:
        {$ifdef FPCMM_SLEEPTSC}
        pop     rdx
        {$endif FPCMM_SLEEPTSC}
        {$endif FPCMM_PAUSE}
        {$ifdef FPCMM_DEBUG} // no lock (informative only)
        inc     dword ptr [rbx].TSmallBlockType.BinSpinCount
        {$endif FPCMM_DEBUG}
        jmp     @LockBlockTypeSleep
@BinLocked:
        movzx   eax, byte ptr [rbx].TSmallBlockType.BinCount
        cmp     al, SmallBlockBinCount
        je      @LockBlockType
        add     byte ptr [rbx].TSmallBlockType.BinCount, 1
        mov     [rbx + TSmallBlockType.BinInstance + rax * 8], rcx
        mov     byte ptr [rbx].TSmallBlockType.BinLocked, false
        movzx   eax, word ptr [rbx].TSmallBlockType.BlockSize
        {$ifdef NOSFRAME}
        pop     rbx
        ret
        {$else}
        jmp     @Done // on Win64, a stack frame is required
        {$endif NOSFRAME}
@LockBlockType:
        // Fallback to main block lock if TSmallBlockType.BinInstance[] is full
        mov     byte ptr [rbx].TSmallBlockType.BinLocked, false
@LockBlockTypeSleep:
        {$endif FPCMM_LOCKLESSFREE}
        {$ifdef FPCMM_PAUSE}
        // Spin to grab the block type (don't try too long due to contention)
        {$ifdef FPCMM_SLEEPTSC}
        push    rdx
        rdtsc
        shl     rdx, 32
        lea     r9, [rax + rdx + SpinSmallFreememLockTSC] // r9 = endtsc
@SpinLockBlockType:
        pause
        rdtsc
        shl     rdx, 32
        or      rax, rdx
        cmp     rax, r9
        ja      @LockBlockTypeReleaseCore
        {$else}
        mov     r8d, SpinSmallFreememLockCount
@SpinLockBlockType:
        pause
        dec     r8d
        jz      @LockBlockTypeReleaseCore
        {$endif FPCMM_SLEEPTSC}
        mov     eax, $100
        {$ifdef FPCMM_CMPBEFORELOCK_SPIN}
        cmp     byte ptr [rbx].TSmallBlockType.Locked, true
        je      @SpinLockBlockType
        {$endif FPCMM_CMPBEFORELOCK_SPIN}
  lock  cmpxchg byte ptr [rbx].TSmallBlockType.Locked, ah
        jne     @SpinLockBlockType
        {$ifdef FPCMM_SLEEPTSC}
        pop     rdx
        {$endif FPCMM_SLEEPTSC}
        jmp     @FreeAndUnlock
@LockBlockTypeReleaseCore:
        {$ifdef FPCMM_SLEEPTSC}
        pop     rdx
        {$endif FPCMM_SLEEPTSC}
        {$endif FPCMM_PAUSE}
        // Couldn't grab the block type - sleep and try again
        {$ifdef FPCMM_DEBUG} lock {$endif}
        inc dword ptr [rbx].TSmallBlockType.FreeMemSleepCount
        push    rdx
        push    rcx
        call    ReleaseCore
        pop     rcx
        pop     rdx
        jmp     @TinySmallLockLoop
@Done:  // restore rbx and the stack frame before ret
        pop     rbx
@Quit:
end;

// warning: FPC signature is not the same than Delphi: requires "var P"
function _ReallocMem(var P: pointer; Size: PtrUInt): pointer;
  {$ifdef NOSFRAME} nostackframe; {$endif} assembler;
asm
        {$ifdef MSWINDOWS}
        push    rdi
        push    rsi
        {$else}
        mov     rdx, Size
        {$endif MSWINDOWS}
        push    rbx
        push    r14
        push    P // for assignement in @Done
        mov     r14, qword ptr [P]
        test    rdx, rdx
        jz      @VoidSize  // ReallocMem(P,0)=FreeMem(P)
        test    r14, r14
        jz      @GetMemMoveFreeMem // ReallocMem(nil,Size)=GetMem(Size)
        mov     rcx, [r14 - BlockHeaderSize]
        test    cl, IsFreeBlockFlag + IsMediumBlockFlag + IsLargeBlockFlag
        jnz     @NotASmallBlock
        // -------------- TINY/SMALL block -------------
        // Get rbx=blocktype, rcx=available size, rax=inplaceresize
        mov     rbx, [rcx].TSmallBlockPoolHeader.BlockType
        lea     rax, [rdx * 4 + SmallBlockDownsizeCheckAdder]
        movzx   ecx, [rbx].TSmallBlockType.BlockSize
        sub     ecx, BlockHeaderSize
        cmp     rcx, rdx
        jb      @SmallUpsize
        // Downsize or small growup with enough space: reallocate only if need
        cmp     eax, ecx
        jb      @GetMemMoveFreeMem // r14=P rdx=size
@NoResize:
        // branchless execution if current block is good enough for this size
        mov     rax, r14 // keep original pointer
        pop     rcx
        {$ifdef NOSFRAME}
        pop     r14
        pop     rbx
        ret
        {$else}
        jmp     @Quit // on Win64, a stack frame is required
        {$endif NOSFRAME}
@VoidSize:
        push    rdx    // to set P=nil
        jmp     @DoFree // ReallocMem(P,0)=FreeMem(P)
@SmallUpsize:
        // State: r14=pointer, rdx=NewSize, rcx=CurrentBlockSize, rbx=CurrentBlockType
        // Small blocks always grow with at least 100% + SmallBlockUpsizeAdder bytes
        lea     P, qword ptr [rcx * 2 + SmallBlockUpsizeAdder]
        movzx   ebx, [rbx].TSmallBlockType.BlockSize
        sub     ebx, BlockHeaderSize + 8
        // r14=pointer, P=NextUpBlockSize, rdx=NewSize, rbx=OldSize-8
@AdjustGetMemMoveFreeMem:
        // New allocated size is max(requestedsize, minimumupsize)
        cmp     rdx, P
        cmova   P, rdx
        push    rdx
        call    _GetMem
        pop     rdx
        test    rax, rax
        jz      @Done
        jmp     @MoveFreeMem // rax=New r14=P rbx=size-8
@GetMemMoveFreeMem:
        // reallocate copy and free: r14=P rdx=size
        mov     rbx, rdx
        mov     P, rdx // P is the proper first argument register
        call    _GetMem
        test    rax, rax
        jz      @Done
        test    r14, r14 // ReallocMem(nil,Size)=GetMem(Size)
        jz      @Done
        sub     rbx, 8
@MoveFreeMem:
        // copy and free: rax=New r14=P rbx=size-8
        push    rax
        {$ifdef FPCMM_ERMS}
        cmp     rbx, ErmsMinSize // startup cost of 0..255 bytes
        jae     @erms
        {$endif FPCMM_ERMS}
        lea     rcx, [r14 + rbx]
        lea     rdx, [rax + rbx]
        neg     rbx
        jns     @Last8
        align   16
@By16:  movaps  xmm0, oword ptr [rcx + rbx]
        movaps  oword ptr [rdx + rbx], xmm0
        add     rbx, 16
        js      @By16
@Last8: mov     rax, qword ptr [rcx + rbx]
        mov     qword ptr [rdx + rbx], rax
@DoFree:mov     P, r14
        call    _FreeMem
        pop     rax
        jmp     @Done
        {$ifdef FPCMM_ERMS}
@erms:  cld
        mov     rsi, r14
        mov     rdi, rax
        lea     rcx, [rbx + 8]
        rep movsb
        jmp     @DoFree
        {$endif FPCMM_ERMS}
@NotASmallBlock:
        // Is this a medium block or a large block?
        test    cl, IsFreeBlockFlag + IsLargeBlockFlag
        jnz     @PossibleLargeBlock
        // -------------- MEDIUM block -------------
        // rcx=CurrentSize+Flags, r14=P, rdx=RequestedSize, r10=TMediumBlockInfo
        lea     rsi, [rdx + rdx]
        lea     r10, [rip + MediumBlockInfo]
        mov     rbx, rcx
        and     ecx, DropMediumAndLargeFlagsMask
        lea     rdi, [r14 + rcx]
        sub     ecx, BlockHeaderSize
        and     ebx, ExtractMediumAndLargeFlagsMask
        // Is it an upsize or a downsize?
        cmp     rdx, rcx
        ja      @MediumBlockUpsize
        // rcx=CurrentBlockSize-BlockHeaderSize, rbx=CurrentBlockFlags,
        // rdi=@NextBlock, r14=P, rdx=RequestedSize
        // Downsize reallocate and move data only if less than half the current size
        cmp     rsi, rcx
        jae     @NoResize
        // In-place downsize? Ensure not smaller than MinimumMediumBlockSize
        cmp     edx, MinimumMediumBlockSize - BlockHeaderSize
        jae     @MediumBlockInPlaceDownsize
        // Need to move to another Medium block pool, or into a Small block?
        cmp     edx, MediumInPlaceDownsizeLimit
        jb      @GetMemMoveFreeMem
        // No need to realloc: resize in-place (if not already at the minimum size)
        mov     edx, MinimumMediumBlockSize - BlockHeaderSize
        cmp     ecx, MinimumMediumBlockSize - BlockHeaderSize
        jna     @NoResize
@MediumBlockInPlaceDownsize:
        // Round up to the next medium block size
        lea     rsi, [rdx + BlockHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset]
        and     rsi,  - MediumBlockGranularity
        add     rsi, MediumBlockSizeOffset
        // Get the size of the second split
        add     ecx, BlockHeaderSize
        sub     ecx, esi
        mov     ebx, ecx
        // Lock the medium blocks
        mov     rcx, r10
        {$ifndef FPCMM_ASSUMEMULTITHREAD}
        mov     rax, [r10 + TMediumBlockinfo.IsMultiThreadPtr]
        cmp     byte ptr [rax], false
        je      @MediumBlocksLocked1 // no lock if IsMultiThread=false
        {$endif FPCMM_ASSUMEMULTITHREAD}
        mov     eax, $100
  lock  cmpxchg byte ptr [rcx].TMediumBlockInfo.Locked, ah
        je      @MediumBlocksLocked1
        call    LockMediumBlocks
@MediumBlocksLocked1:
        mov     ecx, ebx
        // Reread the flags - may have changed before medium blocks could be locked
        mov     rbx, ExtractMediumAndLargeFlagsMask
        and     rbx, [r14 - BlockHeaderSize]
@DoMediumInPlaceDownsize:
        // Set the new size in header, and get rbx = second split size
        or      rbx, rsi
        mov     [r14 - BlockHeaderSize], rbx
        mov     ebx, ecx
        // If the next block is used, flag its previous block as free
        mov     rdx, [rdi - BlockHeaderSize]
        test    dl, IsFreeBlockFlag
        jnz     @MediumDownsizeNextBlockFree
        or      rdx, PreviousMediumBlockIsFreeFlag
        mov     [rdi - BlockHeaderSize], rdx
        jmp     @MediumDownsizeDoSplit
@MediumDownsizeNextBlockFree:
        // If the next block is free, combine both
        mov     rcx, rdi
        and     rdx, DropMediumAndLargeFlagsMask
        add     rbx, rdx
        add     rdi, rdx
        cmp     edx, MinimumMediumBlockSize
        jb      @MediumDownsizeDoSplit
        call    RemoveMediumFreeBlock // rcx=APMediumFreeBlock
@MediumDownsizeDoSplit:
        // Store the trailing size field and free part header
        mov     [rdi - 16], rbx
        lea     rcx, [rbx + IsMediumBlockFlag + IsFreeBlockFlag];
        mov     [r14 + rsi - BlockHeaderSize], rcx
        // Bin this free block (if worth it)
        cmp     rbx, MinimumMediumBlockSize
        jb      @MediumBlockDownsizeDone
        lea     rcx, [r14 + rsi]
        mov     rdx, rbx
        call    InsertMediumBlockIntoBin // rcx=APMediumFreeBlock, edx=AMediumBlockSize
@MediumBlockDownsizeDone:
        // Unlock the medium blocks, and leave with the new pointer
        mov     byte ptr [r10 + TMediumBlockInfo.Locked], false
        mov     rax, r14
        jmp     @Done
@MediumBlockUpsize:
        // ecx = Current Block Size - BlockHeaderSize, bl = Current Block Flags,
        // rdi = @Next Block, r14 = P, rdx = Requested Size
        // Try to make in-place upsize
        mov     rax, [rdi - BlockHeaderSize]
        test    al, IsFreeBlockFlag
        jz      @CannotUpsizeMediumBlockInPlace
        // Get rax = available size, rsi = available size with the next block
        and     rax, DropMediumAndLargeFlagsMask
        lea     rsi, [rax + rcx]
        cmp     rdx, rsi
        ja      @CannotUpsizeMediumBlockInPlace
        // Grow into the next block
        mov     rbx, rcx
        mov     rcx, r10
        {$ifndef FPCMM_ASSUMEMULTITHREAD}
        mov     rax, [r10 + TMediumBlockinfo.IsMultiThreadPtr]
        cmp     byte ptr [rax], false
        je      @MediumBlocksLocked2 // no lock if IsMultiThread=false
        {$endif FPCMM_ASSUMEMULTITHREAD}
        mov     eax, $100
  lock  cmpxchg byte ptr [rcx].TMediumBlockInfo.Locked, ah
        je      @MediumBlocksLocked2
        mov     rsi, rdx
        call    LockMediumBlocks
        mov     rdx, rsi
@MediumBlocksLocked2:
        // Re-read info once locked, and ensure next block is still free
        mov     rcx, rbx
        mov     rbx, ExtractMediumAndLargeFlagsMask
        and     rbx, [r14 - BlockHeaderSize]
        mov     rax, [rdi - BlockHeaderSize]
        test    al, IsFreeBlockFlag
        jz      @NextMediumBlockChanged
        and     eax, DropMediumAndLargeFlagsMask
        lea     rsi, [rax + rcx]
        cmp     rdx, rsi
        ja      @NextMediumBlockChanged
@DoMediumInPlaceUpsize:
        // Bin next free block (if worth it)
        cmp     eax, MinimumMediumBlockSize
        jb      @MediumInPlaceNoNextRemove
        push    rcx
        push    rdx
        mov     rcx, rdi
        call    RemoveMediumFreeBlock // rcx=APMediumFreeBlock
        pop     rdx
        pop     rcx
@MediumInPlaceNoNextRemove:
        // Medium blocks grow a minimum of 25% in in-place upsizes
        mov     eax, ecx
        shr     eax, 2
        add     eax, ecx
        // Get the maximum of the requested size and the minimum growth size
        xor     edi, edi
        sub     eax, edx
        adc     edi, -1
        and     eax, edi
        // Round up to the nearest block size granularity
        lea     rax, [rax + rdx + BlockHeaderSize + MediumBlockGranularity - 1 - MediumBlockSizeOffset]
        and     eax, -MediumBlockGranularity
        add     eax, MediumBlockSizeOffset
        // Calculate the size of the second split and check if it fits
        lea     rdx, [rsi + BlockHeaderSize]
        sub     edx, eax
        ja      @MediumInPlaceUpsizeSplit
        // Grab the whole block: Mark it as used in the next block, and adjust size
        and     qword ptr [r14 + rsi],  NOT PreviousMediumBlockIsFreeFlag
        add     rsi, BlockHeaderSize
        jmp     @MediumUpsizeInPlaceDone
@MediumInPlaceUpsizeSplit:
        // Store the size of the second split as the second last pointer
        mov     [r14 + rsi - BlockHeaderSize], rdx
        // Set the second split header
        lea     rdi, [rdx + IsMediumBlockFlag + IsFreeBlockFlag]
        mov     [r14 + rax - BlockHeaderSize], rdi
        mov     rsi, rax
        cmp     edx, MinimumMediumBlockSize
        jb      @MediumUpsizeInPlaceDone
        lea     rcx, [r14 + rax]
        call    InsertMediumBlockIntoBin // rcx=APMediumFreeBlock, edx=AMediumBlockSize
@MediumUpsizeInPlaceDone:
        // No need to move data at upsize: set the size and flags for this block
        or      rsi, rbx
        mov     [r14 - BlockHeaderSize], rsi
        mov     byte ptr [r10 + TMediumBlockInfo.Locked], false
        mov     rax, r14
        jmp     @Done
@NextMediumBlockChanged:
        // The next block changed during lock: reallocate and move data
        mov     byte ptr [r10 + TMediumBlockInfo.Locked], false
@CannotUpsizeMediumBlockInPlace:
        // rcx=OldSize-8, rdx=NewSize
        mov     rbx, rcx
        mov     eax, ecx
        shr     eax, 2
        lea     P, qword ptr [rcx + rax] // NextUpBlockSize = OldSize+25%
        jmp     @AdjustGetMemMoveFreeMem // P=BlockSize, rdx=NewSize, rbx=OldSize-8
@PossibleLargeBlock:
        // -------------- LARGE block -------------
        test    cl, IsFreeBlockFlag + IsMediumBlockFlag
        jnz     @Error
        {$ifdef MSWINDOWS}
        mov     rcx, r14
        {$else}
        mov     rdi, r14
        mov     rsi, rdx
        {$endif MSWINDOWS}
        call    ReallocateLargeBlock // with restored proper registers
        jmp     @Done
@Error: xor     eax, eax
@Done:  // restore registers and the stack frame before ret
        pop     rcx
        mov     qword ptr [rcx], rax // store new pointer in var P
@Quit:  pop     r14
        pop     rbx
        {$ifdef MSWINDOWS}
        pop     rsi
        pop     rdi
        {$endif MSWINDOWS}
end;

function _AllocMem(Size: PtrUInt): pointer;
  {$ifdef NOSFRAME} nostackframe; {$endif} assembler;
asm
        push    rbx
        // Compute rbx = size rounded down to the last pointer
        lea     rbx, [Size - 1]
        and     rbx,  - 8
        // Perform the memory allocation
        call    _GetMem
        // Could a block be allocated? rcx = 0 if yes, -1 if no
        cmp     rax, 1
        sbb     rcx, rcx
        // Point rdx to the last pointer
        lea     rdx, [rax + rbx]
        // Compute Size (1..8 doesn't need to enter the SSE2 loop)
        or      rbx, rcx
        jz      @LastQ
        // Large blocks from mmap/VirtualAlloc are already zero filled
        cmp     rbx, MaximumMediumBlockSize - BlockHeaderSize
        jae     @Done
        {$ifdef FPCMM_ERMS}
        cmp     rbx, ErmsMinSize // startup cost of 0..255 bytes
        jae     @erms
        {$endif FPCMM_ERMS}
        neg     rbx
        pxor    xmm0, xmm0
        align   16
@FillLoop: // non-temporal movntdq not needed with small/medium size
        movaps  oword ptr [rdx + rbx], xmm0
        add     rbx, 16
        js      @FillLoop
        // fill the last pointer
@LastQ: xor     rcx, rcx
        mov     qword ptr [rdx], rcx
        {$ifdef FPCMM_ERMS}
        {$ifdef NOSFRAME}
        pop     rbx
        ret
        {$else}
        jmp     @Done // on Win64, a stack frame is required
        {$endif NOSFRAME}
        // ERMS has a startup cost, but "rep stosd" is fast enough on all CPUs
@erms:  mov     rcx, rbx
        push    rax
        {$ifdef MSWINDOWS}
        push    rdi
        {$endif MSWINDOWS}
        cld
        mov     rdi, rdx
        xor     eax, eax
        sub     rdi, rbx
        shr     ecx, 2
        mov     qword ptr [rdx], rax
        rep stosd
        {$ifdef MSWINDOWS}
        pop     rdi
        {$endif MSWINDOWS}
        pop     rax
        {$endif FPCMM_ERMS}
@Done:  // restore rbx register and the stack frame before ret
        pop     rbx
end;

function _MemSize(P: pointer): PtrUInt;
begin
  // AFAIK used only by fpc_AnsiStr_SetLength() in FPC RTL
  // also used by our static SQLite3 for its xSize() callback
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

function _FreeMemSize(P: pointer; size: PtrUInt): PtrInt;
begin
  // should return the chunk size - only used by heaptrc AFAIK
  if (P <> nil) and
     (size <> 0) then
    result := _FreeMem(P)
  else
    result := 0;
end;


{ ********* Information Gathering }

{$ifdef FPCMM_STANDALONE}

procedure Assert(flag: boolean);
begin
end;

{$else}

function _GetFPCHeapStatus: TFPCHeapStatus;
var
  mm: PMMStatus;
begin
  mm := @HeapStatus;
  {$ifdef FPCMM_DEBUG}
  result.MaxHeapSize := mm^.Medium.PeakBytes + mm^.Large.PeakBytes;
  {$else}
  result.MaxHeapSize := 0;
  {$endif FPCMM_DEBUG}
  result.MaxHeapUsed := result.MaxHeapSize;
  result.CurrHeapSize := mm^.Medium.CurrentBytes + mm^.Large.CurrentBytes;
  result.CurrHeapUsed := result.CurrHeapSize;
  result.CurrHeapFree := 0;
end;

function _GetHeapStatus: THeapStatus;
begin
  FillChar(result, sizeof(result), 0);
  with HeapStatus do
    result.TotalAllocated := Medium.CurrentBytes + Large.CurrentBytes;
  result.TotalAddrSpace := result.TotalAllocated;
end;

type
  // match both TSmallBlockStatus and TSmallBlockContention
  TRes = array[0..2] of cardinal;
  // details are allocated on the stack, not the heap
  TResArray = array[0..(NumSmallInfoBlock * 2) - 1] of TRes;

procedure QuickSortRes(var Res: TResArray; L, R, Level: PtrInt);
var
  I, J, P: PtrInt;
  pivot: cardinal;
  tmp: TRes;
begin
  if L < R then
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        pivot := Res[P, Level];
        while Res[I, Level] > pivot do
          inc(I);
        while Res[J, Level] < pivot do
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
      begin
        // use recursion only for smaller range
        if L < J then
          QuickSortRes(Res, L, J, Level);
        L := I;
      end
      else
      begin
        if I < R then
          QuickSortRes(Res, I, R, Level);
        R := J;
      end;
    until L >= R;
end;

procedure SetSmallBlockStatus(var res: TResArray; out small, tiny: cardinal);
var
  i, a: integer;
  p: PSmallBlockType;
  d: ^TSmallBlockStatus;
begin
  small := 0;
  tiny := 0;
  d := @res;
  p := @SmallBlockInfo;
  for i := 1 to NumSmallBlockTypes do
  begin
    inc(small, ord(p^.GetmemCount <> 0));
    d^.Total := p^.GetmemCount;
    d^.Current := p^.GetmemCount - p^.FreememCount;
    d^.BlockSize := p^.BlockSize;
    inc(d);
    inc(p);
  end;
  for a := 1 to NumTinyBlockArenas do
  begin
    d := @res; // aggregate counters
    for i := 1 to NumTinyBlockTypes do
    begin
      inc(tiny, ord(p^.GetmemCount <> 0));
      inc(d^.Total, p^.GetmemCount);
      inc(d^.Current, p^.GetmemCount - p^.FreememCount);
      inc(d);
      inc(p);
    end;
  end;
  assert(p = @SmallBlockInfo.GetmemLookup);
end;

function SortSmallBlockStatus(var res: TResArray; maxcount, orderby: PtrInt;
  count, bytes: PPtrUInt): PtrInt;
var
  i: PtrInt;
begin
  QuickSortRes(res, 0, NumSmallBlockTypes - 1, orderby);
  if count <> nil then
  begin
    count^ := 0;
    for i := 0 to NumSmallBlockTypes - 1 do
      inc(count^, res[i, orderby]);
  end;
  if bytes <> nil then
  begin
    bytes^ := 0;
    for i := 0 to NumSmallBlockTypes - 1 do
      inc(bytes^, res[i, orderby] * res[i, ord(obBlockSize)]);
  end;
  result := maxcount;
  if result > NumSmallBlockTypes then
    result := NumSmallBlockTypes;
  while (result > 0) and
        (res[result - 1, orderby] = 0) do
    dec(result);
end;

function SetSmallBlockContention(var res: TResArray; maxcount: integer): integer;
var
  i: integer;
  p: PSmallBlockType;
  d: ^TSmallBlockContention;
begin
  result := 0;
  d := @res;
  p := @SmallBlockInfo;
  for i := 1 to NumSmallInfoBlock do
  begin
    if p^.GetmemSleepCount <> 0 then
    begin
      d^.SleepCount := p^.GetmemSleepCount;
      d^.GetmemBlockSize := p^.BlockSize;
      d^.FreememBlockSize := 0;
      inc(d);
      inc(result);
    end;
    if p^.FreememSleepCount <> 0 then
    begin
      d^.SleepCount := p^.FreememSleepCount;
      d^.GetmemBlockSize := 0;
      d^.FreememBlockSize := p^.BlockSize;
      inc(d);
      inc(result);
    end;
    inc(p);
  end;
  if result = 0 then
    exit;
  QuickSortRes(res, 0, result - 1, 0);
  if result > maxcount then
    result := maxcount;
end;

const
  K_: array[0..4] of string[1] = (
    'P', 'T', 'G', 'M', 'K');

function K(i: PtrUInt): ShortString;
var
  j, n: PtrUInt;
  tmp: PShortString;
begin
  tmp := nil;
  n := 1 shl 50;
  for j := 0 to high(K_) do
    if i >= n then
    begin
      i := i div n;
      tmp := @K_[j];
      break;
    end
    else
      n := n shr 10;
  str(i, result);
  if tmp <> nil then
    result := result + tmp^;
end;

function S(i: PtrUInt): ShortString;
begin
  str(i, result);
end;

type
  // allow to write into a temp string or the console
  TGetHeapStatusWrite =
    procedure(const V: array of ShortString; CRLF: boolean = true);

procedure WriteHeapStatusDetail(const arena: TMMStatusArena;
  const name: ShortString; Wr: TGetHeapStatusWrite);
begin
  Wr([name, K(arena.CurrentBytes),
    'B/', K(arena.CumulativeBytes), 'B '], {crlf=}false);
  {$ifdef FPCMM_DEBUG}
  Wr(['   peak=', K(arena.PeakBytes),
    'B current=', K(arena.CumulativeAlloc - arena.CumulativeFree),
       ' alloc=', K(arena.CumulativeAlloc),
        ' free=', K(arena.CumulativeFree)], false);
  {$endif FPCMM_DEBUG}
  Wr([' sleep=', K(arena.SleepCount)]);
end;

procedure ComputeHeapStatus(const context: ShortString; smallblockstatuscount,
  smallblockcontentioncount: integer; compilationflags: boolean;
  Wr: TGetHeapStatusWrite);
var
  res: TResArray; // no heap allocation involved
  i, n, smallcount: PtrInt;
  t, b: PtrUInt;
  small, tiny: cardinal;
begin
  if context[0] <> #0 then
    Wr([context]);
  if compilationflags then
    Wr([' Flags:' + FPCMM_FLAGS]);
  with CurrentHeapStatus do
  begin
    Wr([' Small:  blocks=', K(SmallBlocks),
                  ' size=', K(SmallBlocksSize), 'B (part of Medium arena)']);
    WriteHeapStatusDetail(Medium, ' Medium: ', Wr);
    WriteHeapStatusDetail(Large,  ' Large:  ', Wr);
    if SleepCount <> 0 then
      Wr([' Total Sleep: count=', K(SleepCount)
        {$ifdef FPCMM_SLEEPTSC} , ' rdtsc=', K(SleepCycles) {$endif}]);
    smallcount := SmallGetmemSleepCount + SmallFreememSleepCount
      {$ifdef FPCMM_LOCKLESSFREE} {$ifdef FPCMM_DEBUG}
       + SmallFreememLockLessSpin {$endif} {$endif};
    if smallcount <> 0 then
      Wr([' Small Sleep: getmem=', K(SmallGetmemSleepCount),
                      ' freemem=', K(SmallFreememSleepCount)
        {$ifdef FPCMM_LOCKLESSFREE} {$ifdef FPCMM_DEBUG} ,
        '  locklessspin=', K(SmallFreememLockLessSpin) {$endif} {$endif}]);
  end;
  if (smallblockcontentioncount > 0) and
     (smallcount <> 0) then
  begin
    n := SetSmallBlockContention(res, smallblockcontentioncount);
    for i := 0 to n - 1 do
      with TSmallBlockContention(res[i]) do
      begin
        if GetmemBlockSize <> 0 then
          Wr(['  getmem(', S(GetmemBlockSize)], {crlf=}false)
        else
          Wr(['  freemem(', S(FreememBlockSize)], false);
        Wr([')=' , K(SleepCount)], false);
        if (i and 3 = 3) or
           (i = n - 1) then
          Wr([]);
      end;
  end;
  if smallblockstatuscount > 0 then
  begin
    SetSmallBlockStatus(res, small, tiny);
    n := SortSmallBlockStatus(res, smallblockstatuscount, ord(obTotal), @t, @b) - 1;
    Wr([' Small Blocks since beginning: ', K(t), '/', K(b),
        'B (as small=', K(small), '/', S(NumSmallBlockTypes),
        ' tiny=', K(tiny), '/', S(NumTinyBlockArenas * NumTinyBlockTypes), ')']);
    for i := 0 to n do
      with TSmallBlockStatus(res[i]) do
      begin
        Wr(['  ', S(BlockSize), '=', K(Total)], false);
        if (i and 7 = 7) or
           (i = n) then
          Wr([]);
      end;
    n := SortSmallBlockStatus(res, smallblockstatuscount, ord(obCurrent), @t, @b) - 1;
    Wr([' Small Blocks current: ', K(t), '/', K(b), 'B']);
    for i := 0 to n do
      with TSmallBlockStatus(res[i]) do
      begin
        Wr(['  ', S(BlockSize), '=', K(Current)], false);
        if (i and 7 = 7) or
           (i = n) then
          Wr([]);
      end;
  end;
end;

var
  WrStrTemp: string; // we don't require thread safety here
  WrStrOnSameLine: boolean;

procedure WrStr(const V: array of ShortString; CRLF: boolean);
var
  i: PtrInt;
begin // we don't have format() nor formatutf8() -> this is good enough
  for i := 0 to high(V) do
    WrStrTemp := WrStrTemp + string(V[i]); // fast enough
  if CRLF and
     not WrStrOnSameLine then
    WrStrTemp := WrStrTemp + #13#10;
end;

function GetHeapStatus(const context: ShortString; smallblockstatuscount,
  smallblockcontentioncount: integer; compilationflags, onsameline: boolean): string;
begin
  WrStrOnSameLine := onsameline;
  ComputeHeapStatus(context, smallblockstatuscount, smallblockcontentioncount,
    compilationflags, WrStr);
  result := WrStrTemp;
  WrStrTemp := '';
end;

procedure WrConsole(const V: array of ShortString; CRLF: boolean);
var
  i: PtrInt;
begin // direct write to the console with no memory heap allocation
  {$I-}
  for i := 0 to high(V) do
    write(V[i]);
  if CRLF then
    writeln;
  ioresult;
  {$I+}
end;

procedure WriteHeapStatus(const context: ShortString; smallblockstatuscount,
  smallblockcontentioncount: integer; compilationflags: boolean);
begin
  ComputeHeapStatus(context,  smallblockstatuscount, smallblockcontentioncount,
    compilationflags, WrConsole);
end;

function GetSmallBlockStatus(maxcount: integer; orderby: TSmallBlockOrderBy;
  count, bytes: PPtrUInt; small, tiny: PCardinal): TSmallBlockStatusDynArray;
var
  res: TResArray;
  sm, ti: cardinal;
begin
  assert(SizeOf(TRes) = SizeOf(TSmallBlockStatus));
  result := nil;
  if maxcount <= 0 then
    exit;
  SetSmallBlockStatus(res, sm, ti);
  if small <> nil then
    small^ := sm;
  if tiny <> nil then
    tiny^ := ti;
  maxcount := SortSmallBlockStatus(res, maxcount, ord(orderby), count, bytes);
  if maxcount = 0 then
    exit;
  SetLength(result, maxcount);
  Move(res[0], result[0], maxcount * SizeOf(res[0]));
end;

function GetSmallBlockContention(maxcount: integer): TSmallBlockContentionDynArray;
var
  n: integer;
  res: TResArray;
begin
  result := nil;
  if maxcount <= 0 then
    exit;
  n := SetSmallBlockContention(res, maxcount);
  if n = 0 then
    exit;
  SetLength(result, n);
  Move(res[0], result[0], n * SizeOf(res[0]));
end;

{$endif FPCMM_STANDALONE}

function CurrentHeapStatus: TMMStatus;
var
  i: integer;
  small: PtrUInt;
  p: PSmallBlockType;
begin
  result := HeapStatus;
  p := @SmallBlockInfo;
  for i := 1 to NumSmallInfoBlock do
  begin
    inc(result.SmallGetmemSleepCount,  p^.GetmemSleepCount);
    inc(result.SmallFreememSleepCount, p^.FreememSleepCount);
    small := p^.GetmemCount - p^.FreememCount;
    if small <> 0 then
    begin
      inc(result.SmallBlocks, small);
      inc(result.SmallBlocksSize, small * p^.BlockSize);
    end;
    {$ifdef FPCMM_LOCKLESSFREE}
    {$ifdef FPCMM_DEBUG}
    inc(result.SmallFreememLockLessSpin, p^.BinSpinCount);
    {$endif FPCMM_LOCKLESSFREE}
    {$endif FPCMM_DEBUG}
    inc(p);
  end;
end;


{ ********* Initialization and Finalization }

procedure InitializeMediumPool(var Info: TMediumBlockInfo);
var
  i: PtrInt;
  medium: PMediumFreeBlock;
begin
  {$ifndef FPCMM_ASSUMEMULTITHREAD}
  Info.IsMultiThreadPtr := @IsMultiThread;
  {$endif FPCMM_ASSUMEMULTITHREAD}
  Info.PoolsCircularList.PreviousMediumBlockPoolHeader := @Info.PoolsCircularList;
  Info.PoolsCircularList.NextMediumBlockPoolHeader := @Info.PoolsCircularList;
  for i := 0 to MediumBlockBinCount -1 do
  begin
    medium := @Info.Bins[i];
    medium.PreviousFreeBlock := medium;
    medium.NextFreeBlock := medium;
  end;
end;

procedure InitializeMemoryManager;
var
  small: PSmallBlockType;
  a, i, min, poolsize, num, perpool, size, start, next: PtrInt;
begin
  small := @SmallBlockInfo;
  assert(SizeOf(small^) = 1 shl SmallBlockTypePO2);
  for a := 0 to NumTinyBlockArenas do
    for i := 0 to NumSmallBlockTypes - 1 do
    begin
      if (i = NumTinyBlockTypes) and
         (a > 0) then
        break;
      size := SmallBlockSizes[i];
      assert(size and 15 = 0);
      small^.BlockSize := size;
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
      small^.AllowedGroupsForBlockPoolBitmap := byte(byte(-1) shl num);
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
  SmallBlockInfo.IsMultiThreadPtr  := @IsMultiThread;
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
  InitializeMediumPool(MediumBlockInfo);
  {$ifdef FPCMM_SMALLNOTWITHMEDIUM}
  InitializeMediumPool(SmallMediumBlockInfo);
  {$endif FPCMM_SMALLNOTWITHMEDIUM}
  LargeBlocksCircularList.PreviousLargeBlockHeader := @LargeBlocksCircularList;
  LargeBlocksCircularList.NextLargeBlockHeader := @LargeBlocksCircularList;
end;

{$I-}

{$ifdef FPCMM_REPORTMEMORYLEAKS}

var
  MemoryLeakReported: boolean;

procedure StartReport;
begin
  if MemoryLeakReported then
    exit;
  writeln {$ifndef MSWINDOWS} (#27'[1;31m') {$endif}; // lightred posix console
  WriteHeapStatus('WARNING! THIS PROGRAM LEAKS MEMORY!'#13#10'Memory Status:');
  writeln('Leaks Identified:' {$ifndef MSWINDOWS} + #27'[1;37m' {$endif});
  MemoryLeakReported := true;
end;

{$ifdef FPCMM_REPORTMEMORYLEAKS_EXPERIMENTAL}
function SeemsRealPointer(p: pointer): boolean;
{$ifdef MSWINDOWS}
var
  meminfo: TMemInfo;
{$endif MSWINDOWS}
begin
  result := false;
  if PtrUInt(p) <= 65535 then
    exit;
  {$ifdef MSWINDOWS}
  // VirtualQuery API is slow but better than raising an exception
  // see https://stackoverflow.com/a/37547837/458259
  FillChar(meminfo, SizeOf(meminfo), 0);
  result := (VirtualQuery(p, @meminfo, SizeOf(meminfo)) = SizeOf(meminfo)) and
            (meminfo.RegionSize >= SizeOf(pointer)) and
            (meminfo.State = MEM_COMMIT) and
            (meminfo.Protect and PAGE_VALID <> 0) and
            (meminfo.Protect and PAGE_GUARD = 0);
  {$else}
  // let the GPF happen silently in the kernel
  result := (fpaccess(p, F_OK) <> 0) and
            (fpgeterrno <> ESysEFAULT);
  {$endif MSWINDOWS}
end;
{$endif FPCMM_REPORTMEMORYLEAKS_EXPERIMENTAL}

procedure MediumMemoryLeakReport(
  var Info: TMediumBlockInfo; p: PMediumBlockPoolHeader);
var
  block: PByte;
  header, size: PtrUInt;
  {$ifdef FPCMM_REPORTMEMORYLEAKS_EXPERIMENTAL}
  first, last: PByte;
  vmt: PAnsiChar;
  exceptcount: integer;
  {$endif FPCMM_REPORTMEMORYLEAKS_EXPERIMENTAL}
begin
  if (Info.SequentialFeedBytesLeft = 0) or
     (PtrUInt(Info.LastSequentiallyFed) < PtrUInt(p)) or
     (PtrUInt(Info.LastSequentiallyFed) > PtrUInt(p) + MediumBlockPoolSize) then
    block := Pointer(PByte(p) + MediumBlockPoolHeaderSize)
  else if Info.SequentialFeedBytesLeft <>
            MediumBlockPoolSize - MediumBlockPoolHeaderSize then
      block := Info.LastSequentiallyFed
    else
      exit;
  {$ifdef FPCMM_REPORTMEMORYLEAKS_EXPERIMENTAL}
  exceptcount := 0;
  {$endif FPCMM_REPORTMEMORYLEAKS_EXPERIMENTAL}
  repeat
    header := PPtrUInt(block - BlockHeaderSize)^;
    size := header and DropMediumAndLargeFlagsMask;
    if size = 0 then
      exit;
    if header and IsFreeBlockFlag = 0 then
      if header and IsSmallBlockPoolInUseFlag <> 0 then
      begin
        {$ifdef FPCMM_REPORTMEMORYLEAKS_EXPERIMENTAL}
        if PSmallBlockPoolHeader(block).BlocksInUse > 0 then
        begin
          first := PByte(block) + SmallBlockPoolHeaderSize;
          with PSmallBlockPoolHeader(block).BlockType^ do
            if (CurrentSequentialFeedPool <> pointer(block)) or
               (PtrUInt(NextSequentialFeedBlockAddress) >
                PtrUInt(MaxSequentialFeedBlockAddress)) then
              last := PByte(block) + (PPtrUInt(PByte(block) - BlockHeaderSize)^
                and DropMediumAndLargeFlagsMask) - BlockSize
            else
              last := Pointer(PByte(NextSequentialFeedBlockAddress) - 1);
          while (first <= last) and
                (exceptcount < 64) do
          begin
            if ((PPtrUInt(first - BlockHeaderSize)^ and IsFreeBlockFlag) = 0) then
            begin
              vmt := PPointer(first)^; // _FreeMem() ensured vmt=nil/$b10dle55
              if (vmt <> nil) and
                 {$ifdef FPCMM_REPORTMEMORYLEAKS}
                 (PtrUInt(vmt) <> REPORTMEMORYLEAK_FREEDHEXSPEAK) and
                 // FreeMem marked freed blocks with 00000000 BLOODLESS marker
                 {$endif FPCMM_REPORTMEMORYLEAKS}
                 SeemsRealPointer(vmt) then
              try
                // try to access the TObject VMT
                if (PPtrInt(vmt + vmtInstanceSize)^ >= sizeof(vmt)) and
                   (PPtrInt(vmt + vmtInstanceSize)^ <=
                    PSmallBlockPoolHeader(block).BlockType.BlockSize) and
                   SeemsRealPointer(PPointer(vmt + vmtClassName)^) then
                begin
                   StartReport;
                   writeln(' probable ', PShortString(PPointer(vmt + vmtClassName)^)^,
                     ' leak (', PPtrInt(vmt + vmtInstanceSize)^, '/',
                     PSmallBlockPoolHeader(block).BlockType.BlockSize,
                     ' bytes) at $', HexStr(first));
                end;
              except
                // intercept and ignore any GPF - SeemsRealPointer()
                inc(exceptcount);
              end;
            end;
            inc(first, PSmallBlockPoolHeader(block).BlockType.BlockSize);
          end;
        end;
        {$endif FPCMM_REPORTMEMORYLEAKS_EXPERIMENTAL}
      end
      else
      begin
        StartReport;
        writeln(' medium block leak of ', size, ' bytes (', K(size), 'B)');
      end;
    inc(block, size);
  until false;
end;

{$endif FPCMM_REPORTMEMORYLEAKS}

procedure FreeMediumPool(var Info: TMediumBlockInfo);
var
  medium, nextmedium: PMediumBlockPoolHeader;
  bin: PMediumFreeBlock;
  i: PtrInt;
begin
  medium := Info.PoolsCircularList.NextMediumBlockPoolHeader;
  while medium <> @Info.PoolsCircularList do
  begin
    {$ifdef FPCMM_REPORTMEMORYLEAKS}
    MediumMemoryLeakReport(Info, medium);
    {$endif FPCMM_REPORTMEMORYLEAKS}
    nextmedium := medium.NextMediumBlockPoolHeader;
    FreeMedium(medium);
    medium := nextmedium;
  end;
  Info.PoolsCircularList.PreviousMediumBlockPoolHeader := @Info.PoolsCircularList;
  Info.PoolsCircularList.NextMediumBlockPoolHeader := @Info.PoolsCircularList;
  for i := 0 to MediumBlockBinCount - 1 do
  begin
    bin := @Info.Bins[i];
    bin.PreviousFreeBlock := bin;
    bin.NextFreeBlock := bin;
  end;
  Info.BinGroupBitmap := 0;
  Info.SequentialFeedBytesLeft := 0;
  for i := 0 to MediumBlockBinGroupCount - 1 do
    Info.BinBitmaps[i] := 0;
  {$ifdef FPCMM_LOCKLESSFREEMEDIUM}
  with Info.LocklessBin do
    for i := 0 to Count - 1 do
      if Instance[i] <> nil then
        _FreeMem(Instance[i]); // release (unlikely) pending instances
  {$endif FPCMM_LOCKLESSFREEMEDIUM}
end;

procedure FreeAllMemory;
var
  large, nextlarge: PLargeBlockHeader;
  p: PSmallBlockType;
  i, size: PtrUInt;
  {$ifdef FPCMM_LOCKLESSFREE}
  j: PtrInt;
  {$endif FPCMM_LOCKLESSFREE}
  {$ifdef FPCMM_REPORTMEMORYLEAKS}
  leak, leaks: PtrUInt;
  {$endif FPCMM_REPORTMEMORYLEAKS}
begin
  {$ifdef FPCMM_REPORTMEMORYLEAKS}
  leaks := 0;
  {$endif FPCMM_REPORTMEMORYLEAKS}
  p := @SmallBlockInfo;
  for i := 1 to NumSmallInfoBlock do
  begin
    {$ifdef FPCMM_LOCKLESSFREE}
    {$ifdef FPCMM_REPORTMEMORYLEAKS}
    if p^.BinCount <> 0 then
      writeln('BinCount=', p^.BinCount, ' for small=', p^.BlockSize);
    {$endif FPCMM_REPORTMEMORYLEAKS}
    for j := 0 to p^.BinCount - 1 do
      if p^.BinInstance[j] <> nil then
        _FreeMem(p^.BinInstance[j]); // release (unlikely) pending instances
    {$endif FPCMM_LOCKLESSFREE}
    p^.PreviousPartiallyFreePool := pointer(p);
    p^.NextPartiallyFreePool := pointer(p);
    p^.NextSequentialFeedBlockAddress := pointer(1);
    p^.MaxSequentialFeedBlockAddress := nil;
    {$ifdef FPCMM_REPORTMEMORYLEAKS}
    leak := p^.GetmemCount - p^.FreememCount;
    if leak <> 0 then
    begin
      StartReport;
      inc(leaks, leak);
      writeln(' small block leak x', leak, ' of size=', p^.BlockSize,
        '  (getmem=', p^.GetmemCount, ' freemem=', p^.FreememCount, ')');
    end;
    {$endif FPCMM_REPORTMEMORYLEAKS}
    inc(p);
  end;
  {$ifdef FPCMM_REPORTMEMORYLEAKS}
  if leaks <> 0 then
    writeln(' Total small block leaks = ', leaks);
  {$endif FPCMM_REPORTMEMORYLEAKS}
  {$ifdef FPCMM_SMALLNOTWITHMEDIUM}
  FreeMediumPool(SmallMediumBlockInfo);
  {$endif FPCMM_SMALLNOTWITHMEDIUM}
  FreeMediumPool(MediumBlockInfo);
  large := LargeBlocksCircularList.NextLargeBlockHeader;
  while large <> @LargeBlocksCircularList do
  begin
    size := large.BlockSizeAndFlags and DropMediumAndLargeFlagsMask;
    {$ifdef FPCMM_REPORTMEMORYLEAKS}
    StartReport;
    writeln(' large block leak of ', size, ' bytes (', K(size), 'B)');
    {$endif FPCMM_REPORTMEMORYLEAKS}
    nextlarge := large.NextLargeBlockHeader;
    FreeLarge(large, size);
    large := nextlarge;
  end;
  LargeBlocksCircularList.PreviousLargeBlockHeader := @LargeBlocksCircularList;
  LargeBlocksCircularList.NextLargeBlockHeader := @LargeBlocksCircularList;
end;

{$I+}

{$ifndef FPCMM_STANDALONE}

const
  NewMM: TMemoryManager = (
    NeedLock:         false;
    GetMem:           @_Getmem;
    FreeMem:          @_FreeMem;
    FreememSize:      @_FreememSize;
    AllocMem:         @_AllocMem;
    ReallocMem:       @_ReAllocMem;
    MemSize:          @_MemSize;
    InitThread:       nil;
    DoneThread:       nil;
    RelocateHeap:     nil;
    GetHeapStatus:    @_GetHeapStatus;
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

{$endif FPCX64MM}

end.

