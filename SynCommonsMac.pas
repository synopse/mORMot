unit SynCommonsMac;

interface

  uses
    SysUtils
    , Classes
    , System.Types
    , System.TypInfo
    , SynLZ
    , SyncObjs
    , DateUtils
    , Contnrs  // for TObjectList
    , Macapi.Mach // for mach_timebase_info
    ;

type
  {$undef CPUINTEL}
  {$undef USETHREADSFORBIGAESBLOCKS}

  BOOL = LongBool;
  THeapMemoryStream = TMemoryStream;
  {$ifdef UNICODE}
  PtrInt = NativeInt;
  {$else}
  PtrInt = integer;
  {$endif}
  {$ifdef UNICODE}
  PtrUInt = NativeUInt;
  {$else}
  PtrUInt = cardinal;
  {$endif}
  PPtrInt = ^PtrInt;
  {$ifdef UNICODE}
  RawUTF8 = type AnsiString(CP_UTF8); // Codepage for an UTF8 string
  {$else}
  RawUTF8 = type AnsiString;
  {$endif}
  TCardinalArray = array[0..MaxInt div SizeOf(cardinal)-1] of cardinal;
  PCardinalArray = ^TCardinalArray;

  ESynException = class(Exception)
  public
      constructor CreateUTF8(const Format: RawUTF8; const Args: array of const);
  end;
  const NTE_BAD_KEYSET = HResult($80090016);
  var MoveFast: procedure(const Source; var Dest; Count: PtrInt);
  procedure FillcharFast(var Dest; count: PtrInt; Value: byte);
  procedure XorMemory(Dest,Source: PByteArray; size: integer);
  function bswap32(a: cardinal): cardinal;
  function GetTickCount64: Int64;
  function Hash32(Data: pointer; Len: integer): cardinal; overload;
  function CompressSynLZ(var DataRawByteString; Compress: boolean): AnsiString;
  procedure BinToHex(Bin, Hex: PAnsiChar; BinBytes: integer);

  procedure InitializeCriticalSection(var ALock: TCriticalSection);
  procedure DeleteCriticalSection(var ALock: TCriticalSection);
  procedure EnterCriticalSection(var ALock: TCriticalSection);
  procedure LeaveCriticalSection(var ALock: TCriticalSection);
{
  type
  TTimebaseInfoData = record
    Numer: cardinal;
    Denom: cardinal;
  end;
  function mach_timebase_info(var TimebaseInfoData: TTimebaseInfoData): Integer;
   cdecl external 'libc' name 'mach_timebase_info';
  //function mach_timebase_info(var TimebaseInfoData: TTimebaseInfoData): integer; cdecl; external;
  function mach_absolute_time: Int64; cdecl; external 'libc.dylib';
}
  procedure QueryPerformanceCounter(var ATime: Int64);
  procedure SleepHiRes(ms: cardinal);

  function GetCurrentThreadID: TThreadID;

type
  /// the potential features, retrieved from an Intel CPU
  // - see https://en.wikipedia.org/wiki/CPUID#EAX.3D1:_Processor_Info_and_Feature_Bits
  TIntelCpuFeature =
   ( { in EDX }
   cfFPU, cfVME, cfDE, cfPSE, cfTSC, cfMSR, cfPAE, cfMCE,
   cfCX8, cfAPIC, cf_d10, cfSEP, cfMTRR, cfPGE, cfMCA, cfCMOV,
   cfPAT, cfPSE36, cfPSN, cfCLFSH, cf_d20, cfDS, cfACPI, cfMMX,
   cfFXSR, cfSSE, cfSSE2, cfSS, cfHTT, cfTM, cfIA64, cfPBE,
   { in ECX }
   cfSSE3, cfCLMUL, cfDS64, cfMON, cfDSCPL, cfVMX, cfSMX, cfEST,
   cfTM2, cfSSSE3, cfCID, cfSDBG, cfFMA, cfCX16, cfXTPR, cfPDCM,
   cf_c16, cfPCID, cfDCA, cfSSE41, cfSSE42, cfX2A, cfMOVBE, cfPOPCNT,
   cf_TSC, cfAESNI, cfXS, cfOSXS, cfAVX, cfF16C, cfRAND, cfHYP);

  /// all features, as retrieved from an Intel CPU
  TIntelCpuFeatures = set of TIntelCpuFeature;
  {$ifdef CPUARM}
  // ARM does not support 80bit extended -> 64bit double is enough for us
  TSynExtended = double;
  {$else}
  {$ifdef CPU64}
  TSynExtended = double;
  {$else}
  /// the floating-point type to be used for best precision and speed
  // - will allow to fallback to double e.g. on x64 and ARM CPUs
  TSynExtended = extended;
  {$endif}
  {$endif}
  PObject = ^TObject;
var
  CpuFeatures: TIntelCpuFeatures;
    /// a global "Garbage collector", for some classes instances which must
  // live during whole main executable process
  // - used to avoid any memory leak with e.g. 'class var RecordProps', i.e.
  // some singleton or static objects
  // - to be used, e.g. as:
  // !  Version := TFileVersion.Create(InstanceFileName,DefaultVersion32);
  // !  GarbageCollector.Add(Version);
  // - see also GarbageCollectorFreeAndNil() as an alternative
  GarbageCollector: TObjectList;

  /// set to TRUE when the global "Garbage collector" are beeing freed
  GarbageCollectorFreeing: boolean;

  /// a global "Garbage collector" for some TObject global variables which must
  // live during whole main executable process
  // - this list expects a pointer to the TObject instance variable to be
  // specified, and will be set to nil (like a FreeAndNil)
  // - this may be useful when used when targetting Delphi IDE packages,
  // to circumvent the bug of duplicated finalization of units, in the scope
  // of global variables
  // - to be used, e.g. as:
  // !  if SynAnsiConvertList=nil then
  // !    GarbageCollectorFreeAndNil(SynAnsiConvertList,TObjectList.Create);
  procedure GarbageCollectorFreeAndNil(var InstanceVariable; Instance: TObject);

  /// force the global "Garbage collector" list to be released immediately
  // - this function is called in the finalization section of this unit
  // - you should NEVER have to call this function, unless some specific cases
  // (e.g. when using Delphi packages, just before releasing the package)
  procedure GarbageCollectorFree;

  /// enter a giant lock for thread-safe shared process
  // - shall be protected as such:
  // ! GlobalLock;
  // ! try
  // !   .... do something thread-safe but as short as possible
  // ! finally
  // !  GlobalUnLock;
  // ! end;
  // - you should better not use such a giant-lock, but an instance-dedicated
  // critical section - these functions are just here to be convenient, for
  // non time critical process
  procedure GlobalLock;

  /// release the giant lock for thread-safe shared process
  // - you should better not use such a giant-lock, but an instance-dedicated
  // critical section - these functions are just here to be convenient, for
  // non time critical process
  procedure GlobalUnLock;


implementation


constructor ESynException.CreateUTF8(const Format: RawUTF8; const Args: array of const);
begin
  inherited Create(UTF8ToString(SysUtils.Format(Format,Args)));
end;


procedure FillcharFast(var Dest; count: PtrInt; Value: byte);
begin
  System.FillChar(Dest, count, Value);
end;

procedure XorMemory(Dest,Source: PByteArray; size: integer);
begin
  while size>=sizeof(PtrInt) do begin
    dec(size,sizeof(PtrInt));
    PPtrInt(Dest)^ := PPtrInt(Dest)^ xor PPtrInt(Source)^;
    inc(PPtrInt(Dest));
    inc(PPtrInt(Source));
  end;
  while size>0 do begin
    dec(size);
    Dest[size] := Dest[size] xor Source[size];
  end;
end;

function bswap32(a: cardinal): cardinal;
begin
 result := ((a and $ff)shl 24)or((a and $ff00)shl 8)or
            ((a and $ff0000)shr 8)or((a and $ff000000)shr 24);
end;
function GetTickCount64: Int64;
begin
  TThread.GetTickCount;
end;

function Hash32(Data: pointer; Len: integer): cardinal;
var s1,s2: cardinal;
    i: PtrInt;
begin
  if Data<>nil then begin
    s1 := 0;
    s2 := 0;
    for i := 1 to Len shr 4 do begin // 16 bytes (4 DWORD) by loop - aligned read
      inc(s1,PCardinalArray(Data)^[0]);
      inc(s2,s1);
      inc(s1,PCardinalArray(Data)^[1]);
      inc(s2,s1);
      inc(s1,PCardinalArray(Data)^[2]);
      inc(s2,s1);
      inc(s1,PCardinalArray(Data)^[3]);
      inc(s2,s1);
      inc(PtrUInt(Data),16);
    end;
    for i := 1 to (Len shr 2)and 3 do begin // 4 bytes (DWORD) by loop
      inc(s1,PCardinalArray(Data)^[0]);
      inc(s2,s1);
      inc(PtrUInt(Data),4);
    end;
    case Len and 3 of // remaining 0..3 bytes
    1: inc(s1,PByte(Data)^);
    2: inc(s1,PWord(Data)^);
    3: inc(s1,PWord(Data)^ or (PByteArray(Data)^[2] shl 16));
    end;
    inc(s2,s1);
    result := s1 xor (s2 shl 16);
  end else
    result := 0;
end;

function CompressSynLZ(var DataRawByteString; Compress: boolean): AnsiString;
var DataLen, len: integer;
    P: PAnsiChar;
    Data: RawByteString absolute DataRawByteString;
begin
  DataLen := length(Data);
  if DataLen<>0 then // '' is compressed and uncompressed to ''
  if Compress then begin
    len := SynLZcompressdestlen(DataLen)+8;
    SetString(result,nil,len);
    P := pointer(result);
    PCardinal(P)^ := Hash32(pointer(Data),DataLen);
    len := SynLZcompress1(pointer(Data),DataLen,P+8);
    PCardinal(P+4)^ := Hash32(pointer(P+8),len);
    SetString(Data,P,len+8);
  end else begin
    result := '';
    P := pointer(Data);
    if (DataLen<=8) or (Hash32(pointer(P+8),DataLen-8)<>PCardinal(P+4)^) then
      exit;
    len := SynLZdecompressdestlen(P+8);
    SetLength(result,len);
    if (len<>0) and
        ((SynLZdecompress1(P+8,DataLen-8,pointer(result))<>len) or
       (Hash32(pointer(result),len)<>PCardinal(P)^)) then begin
      result := '';
      exit;
    end else
      SetString(Data,PAnsiChar(pointer(result)),len);
  end;
  result := 'synlz';
end;





var
  /// fast lookup table for converting hexadecimal numbers from 0 to 15
  // into their ASCII equivalence
  // - is local for better code generation
  TwoDigitsHex: array[byte] of array[1..2] of AnsiChar;
  TwoDigitsHexW: array[AnsiChar] of word absolute TwoDigitsHex;
  TwoDigitsHexWB: array[byte] of word absolute TwoDigitsHex;

procedure BinToHex(Bin, Hex: PAnsiChar; BinBytes: integer);
var j: cardinal;
begin
  for j := 1 to BinBytes do begin
    PWord(Hex)^ := TwoDigitsHexW[Bin^];
    inc(Hex,2);
    inc(Bin);
  end;
end;



procedure InitializeCriticalSection(var ALock: TCriticalSection);
begin
  ALock := TCriticalSection.Create;
end;


procedure DeleteCriticalSection(var ALock: TCriticalSection);
begin
  ALock.Free;
end;


procedure EnterCriticalSection(var ALock: TCriticalSection);
begin
  ALock.Enter;
end;

procedure LeaveCriticalSection(var ALock: TCriticalSection);
begin
  ALock.Leave;
end;


procedure QueryPerformanceCounter(var ATime: Int64);
var info: TTimebaseInfoData;
begin // returns time in nano second resolution
  mach_timebase_info(info);
  if info.Denom=1 then
    if info.Numer=1 then
      // seems to be the case on Intel CPUs
      ATime := mach_absolute_time else
      ATime := mach_absolute_time*info.Numer else
    // use floating point to avoid potential overflow
    ATime := round(mach_absolute_time*(info.Numer/info.Denom));
end;

procedure SleepHiRes(ms: cardinal);
begin
  SysUtils.Sleep(ms);
end;


var
  GarbageCollectorFreeAndNilList: TList;

procedure GarbageCollectorFree;
var i: integer;
begin
  if GarbageCollectorFreeing then
    exit; // when already called before finalization
  GarbageCollectorFreeing := true;
  for i := GarbageCollector.Count-1 downto 0 do // last in, first out
  try
    GarbageCollector.Delete(i); // will call GarbageCollector[i].Free
  except
    on Exception do
      ; // just ignore exceptions in client code destructors
  end;
  for i := GarbageCollectorFreeAndNilList.Count-1 downto 0 do // LIFO
  try
    if PObject(GarbageCollectorFreeAndNilList.List[i])^<>nil then
      FreeAndNil(PObject(GarbageCollectorFreeAndNilList.List[i])^);
  except
    on E: Exception do
      ; // just ignore exceptions in client code destructors
  end;
  FreeAndNil(GarbageCollectorFreeAndNilList);
end;

procedure GarbageCollectorFreeAndNil(var InstanceVariable; Instance: TObject);
begin
  TObject(InstanceVariable) := Instance;
  GarbageCollectorFreeAndNilList.Add(@InstanceVariable);
end;

var
  GlobalCriticalSection: TCriticalSection;

procedure GlobalLock;
begin
  EnterCriticalSection(GlobalCriticalSection);
end;

procedure GlobalUnLock;
begin
  LeaveCriticalSection(GlobalCriticalSection);
end;

function GetCurrentThreadID: TThreadID;
begin
  Result := TThread.Current.ThreadID;
end;



initialization
  GarbageCollectorFreeAndNilList := TList.Create;
  GarbageCollectorFreeAndNil(GarbageCollector,TObjectList.Create);
  InitializeCriticalSection(GlobalCriticalSection);
  {$ifdef CPUINTEL}
  TestIntelCpuFeatures;
  {$endif}
  MoveFast := @System.Move;
  {$ifdef FPC}
  FillCharFast := @System.FillChar;
  {$else}
  {$ifdef CPUARM}
  FillCharFast := @System.FillChar;
  {$else}
  {$ifdef USEPACKAGES}
  Pointer(@FillCharFast) := SystemFillCharAddress;
  {$else}
//  InitRedirectCode;
  {$endif USEPACKAGES}
  {$endif CPUARM}
  {$endif FPC}
//  InitSynCommonsConversionTables;
//  RetrieveSystemInfo;
//  SetExecutableVersion(0,0,0,0);
//  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TFindFilesDynArray),
//    'Name string Attr Integer Size Int64 TimeStamp TDateTime');
  // some type definition assertions
//  Assert(SizeOf(TSynTableFieldType)=1); // as expected by TSynTableFieldProperties
//  Assert(SizeOf(TSynTableFieldOptions)=1);
  {$ifndef NOVARIANTS}
//  Assert(SizeOf(TSynTableData)=sizeof(TVarData));
//  Assert(SizeOf(TDocVariantData)=sizeof(TVarData));
  {$endif NOVARIANTS}
  {$warnings OFF}
//  Assert((MAX_SQLFIELDS>=64)and(MAX_SQLFIELDS<=256));
  {$warnings ON}
//  Assert(sizeof(TSynUniqueIdentifierBits)=sizeof(TSynUniqueIdentifier));
{  TypeInfoSaveRegisterKnown([
    TypeInfo(boolean),TypeInfo(byte),TypeInfo(word),TypeInfo(cardinal),TypeInfo(Int64),
    TypeInfo(single),TypeInfo(double),TypeInfo(currency),TypeInfo(extended),TypeInfo(TDateTime),
    TypeInfo(RawByteString),TypeInfo(RawJSON),TypeInfo(RawUTF8),TypeInfo(string),
    TypeInfo(SynUnicode),TypeInfo(WideString)]); }

finalization
  GarbageCollectorFree;
  DeleteCriticalSection(GlobalCriticalSection);
  //writeln('TDynArrayHashedCollisionCount=',TDynArrayHashedCollisionCount); readln;
end.
