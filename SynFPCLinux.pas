/// wrapper for Windows functions translated to Linux for FPC
unit SynFPCLinux;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2015 Arnaud Bouchez
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

  The Initial Developer of the Original Code is Alfred Glaenzer.

  Portions created by the Initial Developer are Copyright (C) 2015
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

{$MODE objfpc}
{$inline on}
{$h+}

uses
  SysUtils
  {$ifdef Linux}
  ,UnixType
  {$endif};

const
  { HRESULT codes, delphi-like }
  NOERROR = 0;
  NO_ERROR = 0;
  INVALID_HANDLE_VALUE = THandle(-1);

  LOCALE_USER_DEFAULT = $400;
  NORM_IGNORECASE = 1;

/// compatibility function, wrapping Win32 API mutex initialization
procedure InitializeCriticalSection(var cs : TRTLCriticalSection); inline;

/// compatibility function, wrapping Win32 API mutex finalization
procedure DeleteCriticalSection(var cs : TRTLCriticalSection); inline;

{$ifdef Linux}

/// compatibility function, wrapping Win32 API high resolution timer
procedure QueryPerformanceCounter(var Value: Int64); inline;

/// compatibility function, wrapping Win32 API high resolution timer
function QueryPerformanceFrequency(var Value: Int64): boolean;

/// compatibility function, wrapping Win32 API file position change
function SetFilePointer(hFile: cInt; lDistanceToMove: TOff;
  lpDistanceToMoveHigh: Pointer; dwMoveMethod: cint): TOff; inline;

/// compatibility function, wrapping Win32 API file size retrieval
function GetFileSize(hFile: cInt; lpFileSizeHigh: PDWORD): DWORD;

/// compatibility function, wrapping Win32 API file truncate at current position
procedure SetEndOfFile(hFile: cInt); inline;

/// compatibility function, wrapping Win32 API last error code
function GetLastError: longint; inline;

/// compatibility function, wrapping Win32 API text comparison
function CompareStringW(GetThreadLocale: DWORD; dwCmpFlags: DWORD; lpString1: Pwidechar;
  cchCount1: longint; lpString2: Pwidechar; cchCount2: longint): longint; inline;

/// returns the current UTC time
function GetNowUTC: TDateTime;

/// returns the current UTC time as TSystemTime
procedure GetNowUTCSystem(var result: TSystemTime);

/// a wrapper around stat() to retrieve a file size
function GetLargeFileSize(const aFile: string): int64;

{$endif Linux}

/// compatibility function, to be implemented according to the running OS
// - expect more or less the same result as the homonymous Win32 API function
function GetTickCount64: Int64;

/// compatibility function, to be implemented according to the running OS
// - expect more or less the same result as the homonymous Win32 API function
function GetTickCount: cardinal;

/// similar to Windows sleep() API call, to be truly cross-platform
// - it should have a millisecond resolution, and handle ms=0 as a switch to
// another pending thread, i.e. call sched_yield() API
procedure SleepHiRes(ms: cardinal); 


implementation

{$ifdef Linux}
uses
  Classes, Unix, BaseUnix, linux, dynlibs;
{$endif}

procedure InitializeCriticalSection(var cs : TRTLCriticalSection);
begin
  InitCriticalSection(cs);
end;

procedure DeleteCriticalSection(var cs : TRTLCriticalSection);
begin
  DoneCriticalSection(cs);
end;

{$ifdef Linux}

const // Date Translation - see http://en.wikipedia.org/wiki/Julian_day
  HoursPerDay = 24;
  MinsPerHour = 60;
  SecsPerMin  = 60;
  MinsPerDay  = HoursPerDay*MinsPerHour;
  SecsPerDay  = MinsPerDay*SecsPerMin;
  SecsPerHour = MinsPerHour*SecsPerMin;
  C1970       = 2440588;
  D0          = 1461;
  D1          = 146097;
  D2          = 1721119;

procedure JulianToGregorian(JulianDN: integer; out Year,Month,Day: Word);
var YYear,XYear,Temp,TempMonth: integer;
begin
  Temp := ((JulianDN-D2) shl 2)-1;
  JulianDN := Temp div D1;
  XYear := (Temp mod D1) or 3;
  YYear := (XYear div D0);
  Temp := ((((XYear mod D0)+4) shr 2)*5)-3;
  Day := ((Temp mod 153)+5) div 5;
  TempMonth := Temp div 153;
  if TempMonth>=10 then begin
    inc(YYear);
    dec(TempMonth,12);
  end;
  inc(TempMonth,3);
  Month := TempMonth;
  Year := YYear+(JulianDN*100);
end;

procedure EpochToLocal(epoch: integer; out year,month,day,hour,minute,second: Word);
begin
  JulianToGregorian((Epoch div SecsPerDay)+c1970,year,month,day);
  Epoch := abs(Epoch mod SecsPerDay);
  Hour := Epoch div SecsPerHour;
  Epoch := Epoch mod SecsPerHour;
  Minute := Epoch div SecsPerMin;
  Second := Epoch mod SecsPerMin;
end;

function GetNowUTC: TDateTime;
var SystemTime: TSystemTime;
begin
  GetNowUTCSystem(SystemTime);
  result := SystemTimeToDateTime(SystemTime);
end;

procedure GetNowUTCSystem(var result: TSystemTime);
var tz: timeval;
begin
  fpgettimeofday(@tz,nil);
  EpochToLocal(tz.tv_sec,result.year,result.month,result.day,result.hour,result.Minute,result.Second);
  result.MilliSecond := tz.tv_usec div 1000;
end;

{$if not defined(GetTickCountTimeOfDay)}
function GetTickCount64: Int64;
var tp: timespec;
begin
  clock_gettime(CLOCK_MONOTONIC, @tp); // exists since Linux Kernel 2.6
  Result := (Int64(tp.tv_sec) * 1000) + (tp.tv_nsec div 1000000);
end;
{$ELSE}
function GetTickCount64: Int64;
var tp: TTimeVal;
begin
  fpgettimeofday(@tp, nil);
  Result := (Int64(tp.tv_sec) * 1000) + (tp.tv_usec div 1000);
end;
{$ENDIF}

function GetTickCount: cardinal;
begin
  result := cardinal(GetTickCount64);
end;

const
  C_THOUSAND = Int64(1000);
  C_MILLION  = Int64(C_THOUSAND * C_THOUSAND);
  C_BILLION  = Int64(C_THOUSAND * C_THOUSAND * C_THOUSAND);

procedure QueryPerformanceCounter(var Value: Int64);
var r : TTimeSpec;
begin
  clock_gettime(CLOCK_MONOTONIC,@r);
  value := r.tv_nsec+r.tv_sec*C_BILLION;
end;

function QueryPerformanceFrequency(var Value: Int64):boolean;
var r : TTimeSpec;
    FIsHighResolution : boolean;
begin
  FIsHighResolution := (clock_getres(CLOCK_MONOTONIC,@r) = 0);
  FIsHighResolution := FIsHighResolution and (r.tv_nsec <> 0);
  if (r.tv_nsec <> 0) then
    value := C_BILLION div (r.tv_nsec+(r.tv_sec*C_BILLION));
  result := FIsHighResolution;
end;

function SetFilePointer(hFile: cInt; lDistanceToMove: TOff;
  lpDistanceToMoveHigh: Pointer; dwMoveMethod: cint): TOff;
var offs: Int64;
begin
  Int64Rec(offs).Lo := lDistanceToMove;
  if lpDistanceToMoveHigh=nil then
    Int64Rec(offs).Hi := 0 else
    Int64Rec(offs).Hi := PDWord(lpDistanceToMoveHigh)^;
  offs := FpLseek(hFile,offs,dwMoveMethod);
  result := Int64Rec(offs).Lo;
  if lpDistanceToMoveHigh<>nil then
    PDWord(lpDistanceToMoveHigh)^ := Int64Rec(offs).Hi;
end;

procedure SetEndOfFile(hFile: cInt);
begin
  FpFtruncate(hFile,FPLseek(hFile,0,SEEK_CUR));
end;

function GetLastError: longint;
begin
  result := GetLastOSError;
end;

function CompareStringW(GetThreadLocale: DWORD; dwCmpFlags: DWORD; lpString1: Pwidechar;
  cchCount1: longint; lpString2: Pwidechar; cchCount2: longint): longint;
var W1,W2: WideString;
begin
  W1 := lpString1;
  W2 := lpString2;
  if dwCmpFlags and NORM_IGNORECASE<>0 then
    result := WideCompareText(W1,W2) else
    result := WideCompareStr(W1,W2);
end;

function GetFileSize(hFile: cInt; lpFileSizeHigh: PDWORD): DWORD;
var FileInfo: TStat;
begin
  if fpFstat(hFile,FileInfo)=0 then begin
    result := Int64Rec(FileInfo.st_Size).Lo;
    if lpFileSizeHigh<>nil then
      lpFileSizeHigh^ := Int64Rec(FileInfo.st_Size).Hi;
  end else
    result := 0;
end;

function GetLargeFileSize(const aFile: string): int64;
var FileInfo: TStat;
begin
  if fpStat(aFile,FileInfo)=0 then
    result := FileInfo.st_size else
    result := 0;
end;

procedure SleepHiRes(ms: cardinal);
begin
  SysUtils.Sleep(ms);
end;


{$endif}

end.
