/// wrapper for Windows functions translated to Linux
unit SynFPCLinux;

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

  The Initial Developer of the Original Code is Alfred Glaenzer.

  Portions created by the Initial Developer are Copyright (C) 2014
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
  INVALID_HANDLE_VALUE =  THandle(-1);

/// compatibility function, wrapping Win32 API mutex initialization
procedure InitializeCriticalSection(var cs : TRTLCriticalSection); inline;

/// compatibility function, wrapping Win32 API mutex finalization
procedure DeleteCriticalSection(var cs : TRTLCriticalSection); inline;

{$ifdef Linux}

/// copy one file to another place
function CopyFile(const Source, Target: string; FailIfExists:boolean): boolean;

/// compatibility function, wrapping Win32 API high resolution timer
procedure QueryPerformanceCounter(var Value: Int64); inline;

/// compatibility function, wrapping Win32 API high resolution timer
function QueryPerformanceFrequency(var Value: Int64): boolean;

/// compatibility function, wrapping Win32 API file position change
function SetFilePointer(hFile: cInt; lDistanceToMove: TOff; lpDistanceToMoveHigh: Pointer; dwMoveMethod: cint): TOff; inline;

/// compatibility function, wrapping Win32 API file truncate at current position
procedure SetEndOfFile(hFile: cInt); inline;

/// compatibility function, wrapping Win32 API last error code
function GetLastError: longint; inline;

/// compatibility function, wrapping Win32 API text comparison
function CompareStringW(GetThreadLocale: DWORD; dwCmpFlags: DWORD; lpString1: Pwidechar;
  cchCount1: longint; lpString2: Pwidechar; cchCount2: longint): longint; inline;

/// returns the current UTC time
function FPCNowUTC: TDateTime; 

/// returns the current UTC time as TSystemTime
function FPCNowUTCSystem: TSystemTime;

/// a wrapper around stat() to retrieve a file size
function GetLargeFileSize(const aFile: string): int64;

/// a wrapper around stat() to retrieve a file size
function GetFileSize(const aFile: string): int32;

{$endif Linux}

/// compatibility function, to be implemented according to the running OS
// - expect more or less the same result as the homonymous Win32 API function
function GetTickCount64: Int64;

/// compatibility function, to be implemented according to the running OS
// - expect more or less the same result as the homonymous Win32 API function
function GetTickCount: cardinal;


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

// some of these functions are copied from lazutf8sysutils to remove the dependency
// from Lazarus this allows for the use of only FPC for command line compilation
{$ifdef Linux}

const
{Date Translation}
  C1970=2440588;
  D0   =   1461;
  D1   = 146097;
  D2   =1721119;

Procedure JulianToGregorian(JulianDN:LongInt;out Year,Month,Day:Word);
Var YYear,XYear,Temp,TempMonth : LongInt;
Begin
  Temp:=((JulianDN-D2) shl 2)-1;
  JulianDN:=Temp Div D1;
  XYear:=(Temp Mod D1) or 3;
  YYear:=(XYear Div D0);
  Temp:=((((XYear mod D0)+4) shr 2)*5)-3;
  Day:=((Temp Mod 153)+5) Div 5;
  TempMonth:=Temp Div 153;
  If TempMonth>=10 Then Begin
     inc(YYear);
     dec(TempMonth,12);
   End;
  inc(TempMonth,3);
  Month := TempMonth;
  Year:=YYear+(JulianDN*100);
end;

Procedure EpochToLocal(epoch:longint;out year,month,day,hour,minute,second:Word);
{ Transforms Epoch time into local time (hour, minute,seconds) }
Var DateNum: LongInt;
Begin
  Datenum:=(Epoch Div 86400) + c1970;
  JulianToGregorian(DateNum,Year,Month,day);
  Epoch:=Abs(Epoch Mod 86400);
  Hour:=Epoch Div 3600;
  Epoch:=Epoch Mod 3600;
  Minute:=Epoch Div 60;
  Second:=Epoch Mod 60;
End;

function FPCNowUTC: TDateTime;
var tz: timeval;
    SystemTime: TSystemTime;
begin
  fpgettimeofday(@tz,nil);
  EpochToLocal(tz.tv_sec,SystemTime.year,SystemTime.month,SystemTime.day,SystemTime.hour,SystemTime.Minute,SystemTime.Second);
  SystemTime.MilliSecond:=tz.tv_usec div 1000;
  result := systemTimeToDateTime(SystemTime);
end;

function FPCNowUTCSystem: TSystemTime;
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

function CopyFile(const Source, Target: string; FailIfExists:boolean): boolean;
var SourceF, DestF: TFileStream;
begin
  result:=True;
  if FailIfExists then
    if FileExists(Target) then
      exit else
      DeleteFile(Target);
  SourceF:= TFileStream.Create(Source, fmOpenRead);
  DestF:= TFileStream.Create(Target, fmCreate);
  DestF.CopyFrom(SourceF, SourceF.Size);
  SourceF.Free;
  DestF.Free;
end;

const
  C_THOUSAND = Int64(1000);
  C_MILLION  = Int64(C_THOUSAND * C_THOUSAND);
  C_BILLION  = Int64(C_THOUSAND * C_THOUSAND * C_THOUSAND);

procedure QueryPerformanceCounter(var Value: Int64);
var
  r : TTimeSpec;
begin
  clock_gettime(CLOCK_MONOTONIC,@r);
  value := r.tv_nsec+r.tv_sec*C_BILLION;
end;

function QueryPerformanceFrequency(var Value: Int64):boolean;
var
  r : TTimeSpec;
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
begin
  result := FpLseek( hFile, lDistanceToMove, SEEK_CUR );
end;

procedure SetEndOfFile(hFile: cInt);
begin
  FileSeek(hFile,0,soFromEnd);
end;

function GetLastError: longint;
begin
  result := GetLastOSError;
end;

function CompareStringW(GetThreadLocale: DWORD; dwCmpFlags: DWORD; lpString1: Pwidechar;
  cchCount1: longint; lpString2: Pwidechar; cchCount2: longint): longint;
begin
  result := WideCompareText(Pwidechar(lpString1),Pwidechar(lpString2));
end;

function GetLargeFileSize(const aFile: string): int64;
var FileInfo:TStat;
begin
  if (fpStat(aFile,FileInfo) = 0) then
    result:= FileInfo.st_size else
    result:= -1;
end;

function GetFileSize(const aFile: string): int32;
var fsize: int64;
begin
  fsize := GetLargeFileSize(aFile);
  if fsize > high(int32) then
    // error if file too big
    result:= -1 else
    result:= fsize;
end;

{$endif}

end.
