/// wrapper for Windows functions translated to Linux for Kylix
unit SynKylix;

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

  The Initial Developer of the Original Code is Arnaud Bouchez

  Portions created by the Initial Developer are Copyright (C) 2015
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
  - initial revision, corresponding to functions not available in LibC.pas

}

interface

uses
  LibC,
  Types,
  Classes,
  SysUtils;

type
  PSystemTime = ^TSystemTime;
  /// System Time, as represented in FPC for PosIX
  TSystemTime = record
    Year: Word;
    Month: Word;
    DayOfWeek: Word;
    Day: Word;
    Hour: Word;
    Minute: Word;
    Second: Word;
    Millisecond: Word;
  end;

  cint = integer;
  cuchar = byte;
  cushort = word;

const
  NOERROR = 0;
  NO_ERROR = 0;
  INVALID_HANDLE_VALUE = THandle(-1);
  INFINITE = LongWord(-1);

  LOCALE_USER_DEFAULT = $400;
  NORM_IGNORECASE = 1;

  FILE_BEGIN = SEEK_SET;
  FILE_CURRENT = SEEK_CUR;
  FILE_END = SEEK_END;

/// compatibility function, wrapping Win32 API high resolution timer
procedure QueryPerformanceCounter(var Value: Int64);

/// compatibility function, wrapping Win32 API high resolution timer
function QueryPerformanceFrequency(var Value: Int64):boolean;

/// compatibility function, wrapping Win32 API file position change
function SetFilePointer(hFile,lDistanceToMove: integer;
  lpDistanceToMoveHigh: Pointer; dwMoveMethod: integer): dword;

/// compatibility function, wrapping Win32 API file truncate at current position
procedure SetEndOfFile(hFile: integer);

/// compatibility function, wrapping Win32 API text comparison
function CompareStringW(GetThreadLocale: DWORD; dwCmpFlags: DWORD; lpString1: Pwidechar;
  cchCount1: longint; lpString2: Pwidechar; cchCount2: longint): longint;

/// a wrapper around stat() to retrieve a file age
function GetFileAgeAsDateTime(const FileName: string): TDateTime;

/// a wrapper around stat() to retrieve a file size
function GetFileSize(hFile: Integer; lpFileSizeHigh: PDWORD): DWORD;

/// a wrapper around stat() to retrieve a file size
function GetLargeFileSize(const aFile: AnsiString): int64;

/// returns the current UTC time
function GetNowUTC: TDateTime;

/// returns the current UTC time as TSystemTime
function GetNowUTCSystem: TSystemTime;

/// compatibility function, to be implemented according to the running OS
// - expect more or less the same result as the homonymous Win32 API function
function GetTickCount64: Int64;

/// compatibility function, to be implemented according to the running OS
// - expect more or less the same result as the homonymous Win32 API function
function GetTickCount: cardinal;

/// compatibility function, to be implemented according to the running OS
procedure GetLocalTime(var result: TSystemTime);

/// compatibility function, available in SysUtils.pas only for Windows
function SystemTimeToDateTime(const SystemTime: TSystemTime): TDateTime;


implementation

const
  CLOCK_REALTIME  = 0;
  CLOCK_MONOTONIC = 1;

  C_THOUSAND = Int64(1000);
  C_MILLION  = Int64(C_THOUSAND * C_THOUSAND);
  C_BILLION  = Int64(C_THOUSAND * C_THOUSAND * C_THOUSAND);

procedure QueryPerformanceCounter(var Value: Int64);
var r: TTimeSpec;
begin
  clock_gettime(CLOCK_MONOTONIC,r);
  value := r.tv_nsec+r.tv_sec*C_BILLION;
end;

function QueryPerformanceFrequency(var Value: Int64):boolean;
var r: TTimeSpec;
    FIsHighResolution: boolean;
begin
  FIsHighResolution := (clock_getres(CLOCK_MONOTONIC,r) = 0);
  FIsHighResolution := FIsHighResolution and (r.tv_nsec <> 0);
  if (r.tv_nsec <> 0) then
    value := C_BILLION div (r.tv_nsec+(r.tv_sec*C_BILLION));
  result := FIsHighResolution;
end;

function SetFilePointer(hFile,lDistanceToMove: integer;
  lpDistanceToMoveHigh: Pointer; dwMoveMethod: integer): dword;
var Offs: Int64;
begin
  Int64Rec(Offs).Lo := lDistanceToMove;
  if lpDistanceToMoveHigh=nil then
    Int64Rec(Offs).Hi := 0 else
    Int64Rec(Offs).Hi := PDWord(lpDistanceToMoveHigh)^;
  offs := lseek64(hFile,offs,dwMoveMethod);
  result := Int64Rec(Offs).Lo;
  if lpDistanceToMoveHigh<>nil then
    PDWord(lpDistanceToMoveHigh)^ := Int64Rec(Offs).Hi;
end;

procedure SetEndOfFile(hFile: integer);
begin
  ftruncate64(hFile,lseek64(hFile,0,SEEK_CUR));
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

function GetFileAgeAsDateTime(const FileName: string): TDateTime;
var FileInfo: TStatBuf64;
begin
  if stat64(pointer(FileName),FileInfo)=0 then
    result := FileDateToDateTime(FileInfo.st_mtime) else
    result := 0;
end;

function GetFileSize(hFile: Integer; lpFileSizeHigh: PDWORD): DWORD;
var FileInfo: TStatBuf64;
begin
  if fstat64(hFile,FileInfo)=0 then begin
    result := Int64Rec(FileInfo.st_size).Lo;
    if lpFileSizeHigh<>nil then
      lpFileSizeHigh^ := Int64Rec(FileInfo.st_Size).Hi;
  end else
    result := 0;
end;

function GetLargeFileSize(const aFile: AnsiString): int64;
var FileInfo: TStatBuf64;
begin
  if stat64(pointer(aFile),FileInfo)=0 then
    result := FileInfo.st_size else
    result := 0;
end;

const { Date Translation }
  C1970=2440588;
  D0   =   1461;
  D1   = 146097;
  D2   =1721119;

Procedure JulianToGregorian(JulianDN:LongInt;out Year,Month,Day:Word);
Var YYear,XYear,Temp,TempMonth : LongInt;
Begin
  Temp := ((JulianDN-D2) shl 2)-1;
  JulianDN := Temp Div D1;
  XYear := (Temp Mod D1) or 3;
  YYear := (XYear Div D0);
  Temp := ((((XYear mod D0)+4) shr 2)*5)-3;
  Day := ((Temp Mod 153)+5) Div 5;
  TempMonth := Temp Div 153;
  If TempMonth>=10 Then Begin
     inc(YYear);
     dec(TempMonth,12);
   End;
  inc(TempMonth,3);
  Month := TempMonth;
  Year := YYear+(JulianDN*100);
end;

Procedure EpochToLocal(epoch:longint; out year,month,day,hour,minute,second:Word);
{ Transforms Epoch time into local time (hour, minute,seconds) }
Var DateNum: LongInt;
begin
  Datenum := (Epoch Div 86400) + c1970;
  JulianToGregorian(DateNum,Year,Month,day);
  Epoch := Abs(Epoch Mod 86400);
  Hour := Epoch Div 3600;
  Epoch := Epoch Mod 3600;
  Minute := Epoch Div 60;
  Second := Epoch Mod 60;
end;

function GetNowUTC: TDateTime;
var SystemTime: TSystemTime;
begin
  SystemTime := GetNowUTCSystem;
  result := SystemTimeToDateTime(SystemTime);
end;

function GetNowUTCSystem: TSystemTime;
var tz: TTimeVal;
begin
  gettimeofday(tz,nil);
  EpochToLocal(tz.tv_sec,result.Year,result.Month,result.Day,
    result.Hour,result.Minute,result.Second);
  result.MilliSecond := tz.tv_usec div 1000;
end;

procedure GetLocalTime(var result: TSystemTime);
var tv: TTimeVal;
    ut: TUnixTime;
    t: TTime_T;
begin
  gettimeofday(tv,nil);
  t := tv.tv_sec;
  localtime_r(@t,ut);
  result.Year := ut.tm_year+1900;
  result.Month := ut.tm_mon+1;
  result.Day := ut.tm_mday;
  result.Hour := ut.tm_hour;
  result.Minute := ut.tm_min;
  result.Second := ut.tm_sec;
  result.MilliSecond := tv.tv_usec div 1000;
end;

function GetTickCount64: Int64;
var tp: TTimeVal;
begin
  gettimeofday(tp, nil);
  Result := (Int64(tp.tv_sec) * 1000) + (tp.tv_usec div 1000);
end;

function GetTickCount: cardinal;
begin
  result := cardinal(GetTickCount64);
end;

function SystemTimeToDateTime(const SystemTime: TSystemTime): TDateTime;
var time: TDateTime;
begin
  with SystemTime do begin
    result := EncodeDate(Year,Month,Day);
    time := EncodeTime(Hour,Minute,Second,MilliSecond);
    if result>=0 then
      result := result+time else
      result := result-time;
  end;
end;


end.