unit ObjAutoX;

interface

uses TypInfo;

{$IFDEF VER130}
  Not Support Delphi 5
{$ENDIF}
{$IFDEF VER120}
  Not Support Delphi 4
{$ENDIF}
{$IFDEF VER100}
  Not Support Delphi 3
{$ENDIF}
{$IFDEF VER90}
  Not Support Delphi 2
{$ENDIF}
{$IFDEF VER80}
  Not Support Delphi 1
{$ENDIF}

{$IFDEF VER200}
  {$DEFINE DELPHI2009}
{$ENDIF}

const
  paEAX     = Word(0);
  paEDX     = Word(1);
  paECX     = Word(2);
  paStack   = Word(3);
  SHORT_LEN = SizeOf(ShortString) - 1;

type
  TCallingConvention = (ccRegister, ccCdecl, ccPascal, ccStdCall, ccSafeCall);
  TParamFlags = set of (pfVar, pfConst, pfArray, pfAddress, pfReference, pfOut,
    pfResult);
  PPointer = ^Pointer;
  PWord = ^Word;
  PMethodInfoHeader = ^TMethodInfoHeader;

  TMethodInfoHeader = packed record
    Len:  Word;
    Addr: Pointer;
    Name: ShortString;
  end;

  PReturnInfo = ^TReturnInfo;
  TReturnInfo = packed record
    Version:           Byte; // Must be 1
    CallingConvention: TCallingConvention;
    ReturnType:        ^PTypeInfo;
    ParamSize:         Word;
  end;

  PParamInfo = ^TParamInfo;
  TParamInfo = packed record
    Flags:     TParamFlags;
    ParamType: ^PTypeInfo;
    Access:    Word;
    Name:      ShortString;
  end;
  TMethodInfoArray = array of PMethodInfoHeader;
  TParamInfoArray  = array of PParamInfo;

function ObjectInvoke(Instance: TObject; MethodHeader: PMethodInfoHeader;
	const ParamIndexes: array of Integer; const Params: array of Variant): Variant;
function GetMethodInfo(Instance: TObject;
	const MethodName: ShortString): PMethodInfoHeader;

type
  IMethodHandler = interface
    ['{4E61C8CD-16CC-4830-B1E4-84F86FBC0D23}']
    function Execute(const Args: array of Variant): Variant;
    function InstanceToVariant(Instance: TObject): Variant;
  end;

  PParameters = ^TParameters;
  TParameters = packed record
    Registers: array[paEDX..paECX] of Cardinal;
    Stack:     array[0..1023] of Byte;
  end;
  TDynamicInvokeEvent = procedure (Params: PParameters; StackSize: Integer) of object;

function CreateMethodPointer(const MethodHandler: IMethodHandler; TypeData: PTypeData): TMethod; overload;
function CreateMethodPointer(const ADynamicInvokeEvent: TDynamicInvokeEvent; TypeData: PTypeData): TMethod; overload;
procedure ReleaseMethodPointer(MethodPointer: TMethod);
function GetMethods(ClassType: TClass): TMethodInfoArray;
function GetInvokeInstance(MethodPointer: TMethod): TObject;

function GetParams(aObj: TObject; aMethodName: string): TParamInfoArray;
function GetReturnInfo(aObj: TObject; aMethodName: string): PReturnInfo;

implementation

uses SysUtils, Variants, VarUtils, RTLConsts;

function GetTypeSize(TypeInfo: PTypeInfo): Integer;
var
  TypeData: PTypeData;
begin
  case TypeInfo^.Kind of
    tkInteger, tkEnumeration:
    begin
      TypeData := GetTypeData(TypeInfo);
      if TypeData^.MinValue >= 0 then
        if Cardinal(TypeData^.MaxValue) > $FFFF then
          Result := 4
        else if TypeData^.MaxValue > $FF then
          Result := 2
        else
          Result := 1
      else
      if (TypeData^.MaxValue > $7FFF) or (TypeData^.MinValue < -$7FFF - 1) then
        Result := 4
      else if (TypeData^.MaxValue > $7F) or (TypeData^.MinValue < -$7F - 1) then
        Result := 2
      else
        Result := 1;
    end;
    tkFloat:
    begin
      TypeData := GetTypeData(TypeInfo);
      case TypeData^.FloatType of
        ftSingle: Result                 := 4;
        ftComp, ftCurr, ftDouble: Result := 8;
        else
          Result := -1;
      end;
    end;
    tkString, tkLString,{$IFDEF DELPHI2009}tkUString,{$ENDIF} tkWString, tkInterface, tkClass:
      Result := 4;
    tkMethod, tkInt64:
      Result := 8;
    tkVariant:
      Result := 16;
    else
      Assert(False);
      Result := -1;
  end;
end;

type
  TConvertKind = (ckNone, ckConvert, ckError);

function ConvertKindOf(Source, Dest: TVarType): TConvertKind;
const
  none = ckNone;
  cvt  = ckConvert;
  err  = ckError;
  Codes: array[varEmpty..{$IFDEF DELPHI2009}varUInt64{$ELSE}varInt64{$ENDIF}, varEmpty..{$IFDEF DELPHI2009}varUInt64{$ELSE}varInt64{$ENDIF}] of TConvertKind =
    ({v From} {To >}{vt_empty} {vt_null} {vt_i2} {vt_i4} {vt_r4} {vt_r8} {vt_cy} {vt_date} {vt_bstr} {vt_dispatch} {vt_error} {vt_bool} {vt_variant} {vt_unknown} {vt_decimal} {0x0f } {vt_i1} {vt_ui1} {vt_ui2} {vt_ui4} {vt_i8} {vt_ui8}
    {vt_empty}      (none,      err,      err,    err,    err,    err,    err,    err,      err,      err,          err,       err,      none,        err,         err,         err,    err,    err,     err,     err,     err    {$IFDEF DELPHI2009},err{$ENDIF}),
    {vt_null}       (err,       none,     err,    err,    err,    err,    err,    err,      err,      err,          err,       err,      none,        err,         err,         err,    err,    err,     err,     err,     err    {$IFDEF DELPHI2009},err{$ENDIF}),
    {vt_i2}         (err,       err,      none,   cvt,    cvt,    cvt,    cvt,    cvt,      cvt,      err,          err,       cvt,      none,        err,         cvt,         err,    cvt,    cvt,     cvt,     cvt,     cvt    {$IFDEF DELPHI2009},cvt{$ENDIF}),
    {vt_i4}         (err,       err,      none,   none,   cvt,    cvt,    cvt,    cvt,      cvt,      err,          err,       cvt,      none,        err,         cvt,         err,    cvt,    cvt,     cvt,     cvt,     cvt    {$IFDEF DELPHI2009},cvt{$ENDIF}),
    {vt_r4}         (err,       err,      cvt,    cvt,    none,   cvt,    cvt,    cvt,      cvt,      err,          err,       cvt,      none,        err,         cvt,         err,    cvt,    cvt,     cvt,     cvt,     cvt    {$IFDEF DELPHI2009},cvt{$ENDIF}),
    {vt_r8}         (err,       err,      cvt,    cvt,    cvt,    none,   none,   none,     cvt,      err,          err,       cvt,      none,        err,         cvt,         err,    cvt,    cvt,     cvt,     cvt,     cvt    {$IFDEF DELPHI2009},cvt{$ENDIF}),
    {vt_cy}         (err,       err,      cvt,    cvt,    cvt,    none,   none,   none,     cvt,      err,          err,       cvt,      none,        err,         cvt,         err,    cvt,    cvt,     cvt,     cvt,     cvt    {$IFDEF DELPHI2009},cvt{$ENDIF}),
    {vt_date}       (err,       err,      cvt,    cvt,    cvt,    none,   none,   none,     cvt,      err,          err,       cvt,      none,        err,         cvt,         err,    cvt,    cvt,     cvt,     cvt,     cvt    {$IFDEF DELPHI2009},cvt{$ENDIF}),
    {vt_bstr}       (err,       err,      cvt,    cvt,    cvt,    cvt,    cvt,    cvt,      none,     err,          err,       cvt,      none,        err,         cvt,         err,    cvt,    cvt,     cvt,     cvt,     cvt    {$IFDEF DELPHI2009},cvt{$ENDIF}),
    {vt_dispatch}   (err,       err,      err,    err,    err,    err,    err,    err,      err,      none,         err,       err,      none,        none,        err,         err,    err,    err,     err,     err,     err    {$IFDEF DELPHI2009},err{$ENDIF}),
    {vt_error}      (err,       err,      err,    err,    err,    err,    err,    err,      err,      err,          none,      err,      none,        err,         err,         err,    err,    err,     err,     err,     err    {$IFDEF DELPHI2009},err{$ENDIF}),
    {vt_bool}       (err,       err,      cvt,    cvt,    err,    err,    err,    err,      cvt,      err,          err,       none,     none,        err,         cvt,         err,    cvt,    cvt,     cvt,     cvt,     cvt    {$IFDEF DELPHI2009},cvt{$ENDIF}),
    {vt_variant}    (cvt,       cvt,      cvt,    cvt,    cvt,    cvt,    cvt,    cvt,      cvt,      cvt,          cvt,       cvt,      none,        cvt,         cvt,         cvt,    cvt,    cvt,     cvt,     cvt,     cvt    {$IFDEF DELPHI2009},cvt{$ENDIF}),
    {vt_unknown}    (err,       err,      err,    err,    err,    err,    err,    err,      err,      err,          err,       err,      none,        none,        err,         err,    err,    err,     err,     err,     err    {$IFDEF DELPHI2009},err{$ENDIF}),
    {vt_decimal}    (err,       err,      cvt,    cvt,    cvt,    cvt,    cvt,    cvt,      cvt,      err,          err,       cvt,      none,        err,         none,        err,    cvt,    cvt,     cvt,     cvt,     cvt    {$IFDEF DELPHI2009},cvt{$ENDIF}),
    {0x0f }         (err,       err,      err,    err,    err,    err,    err,    err,      err,      err,          err,       err,      none,        err,         err,         none,   err,    err,     err,     err,     err    {$IFDEF DELPHI2009},err{$ENDIF}),
    {vt_i1}         (err,       err,      cvt,    cvt,    cvt,    cvt,    cvt,    cvt,      cvt,      err,          err,       cvt,      none,        err,         cvt,         err,    none,   none,    cvt,     cvt,     cvt    {$IFDEF DELPHI2009},cvt{$ENDIF}),
    {vt_ui1}        (err,       err,      cvt,    cvt,    cvt,    cvt,    cvt,    cvt,      cvt,      err,          err,       cvt,      none,        err,         cvt,         err,    none,   none,    cvt,     cvt,     cvt    {$IFDEF DELPHI2009},cvt{$ENDIF}),
    {vt_ui2}        (err,       err,      none,   cvt,    cvt,    cvt,    cvt,    cvt,      cvt,      err,          err,       cvt,      none,        err,         cvt,         err,    none,   none,    none,    cvt,     cvt    {$IFDEF DELPHI2009},cvt{$ENDIF}),
    {vt_ui4}        (err,       err,      none,   none,   cvt,    cvt,    cvt,    cvt,      cvt,      err,          err,       cvt,      none,        err,         cvt,         err,    none,   none,    none,    none,    cvt    {$IFDEF DELPHI2009},cvt{$ENDIF}),
    {vt_i8}         (err,       err,      none,   none,   cvt,    cvt,    cvt,    cvt,      cvt,      err,          err,       cvt,      none,        err,         cvt,         err,    none,   none,    none,    none,    none   {$IFDEF DELPHI2009},none{$ENDIF})
{$IFDEF DELPHI2009}
    {vt_ui8}       ,(err,       err,      none,   none,   cvt,    cvt,    cvt,    cvt,      cvt,      err,          err,       cvt,      none,        err,         cvt,         err,    none,   none,    none,    none,    none,   none)
{$ENDIF}
  );
begin
  if Source = Dest then
    Result := none
  else
    // < Low(Codes) always evaluates to false since it is zero
    if {(Source < Low(Codes)) or} (Source > High(Codes)) or
      {(Dest < Low(Codes)) or} (Dest > High(Codes)) then
      Result := cvt
    else
      Result := Codes[Source][Dest];
end;

function InterfaceDerivesFrom(TypeData: PTypeData; const GUID: TGUID): Boolean;
begin
  Result := True;
  while TypeData <> nil do
  begin
    if IsEqualGUID(TypeData^.Guid, GUID) then
      Exit;
    if (TypeData^.IntfParent <> nil) and (TypeData^.IntfParent^ <> nil) then
      TypeData := GetTypeData(TypeData^.IntfParent^)
    else
      Break;
  end;
  Result := False;
end;

function GetVariantType(TypeInfo: PTypeInfo): TVarType;
var
  TypeData: PTypeData;
begin
  case TypeInfo^.Kind of
    tkUnknown: Result := varError;

    tkInteger, tkChar, tkEnumeration, tkWChar:
      if (TypeInfo = System.TypeInfo(Boolean)) or
        (TypeInfo = System.TypeInfo(WordBool)) or
        (TypeInfo = System.TypeInfo(LongBool)) then
        Result := varBoolean
      else
      begin
        TypeData := GetTypeData(TypeInfo);
        if TypeData^.MinValue >= 0 then
          if Cardinal(TypeData^.MaxValue) > $FFFF then
            Result := varLongWord
          else if TypeData^.MaxValue > $FF then
            Result := varWord
          else
            Result := varByte
        else
        if (TypeData^.MaxValue > $7FFF) or (TypeData^.MinValue < -$7FFF - 1) then
          Result := varInteger
        else if (TypeData^.MaxValue > $7F) or (TypeData^.MinValue < -$7F - 1) then
          Result := varSmallint
        else
          Result := varShortint;
      end;
    tkFloat:
    begin
      TypeData := GetTypeData(TypeInfo);
      case TypeData^.FloatType of
        ftSingle: Result := varSingle;
        ftDouble:
          if TypeInfo = System.TypeInfo(TDateTime) then
            Result := varDate
          else
            Result := varDouble;
        ftComp, ftCurr: Result := varCurrency;
        else
          Result := varError;
      end;
    end;
    tkString:  Result := varString;
    tkLString: Result := varString;
	{$IFDEF DELPHI2009}
    tkUString: Result := varUString;
	{$ENDIF}
    tkWString: Result := varOleStr;
    tkInterface:
    begin
      TypeData := GetTypeData(TypeInfo);
      if InterfaceDerivesFrom(TypeData, IDispatch) then
        Result := varDispatch
      else
        Result := varUnknown;
    end;
    tkVariant: Result := varVariant;
    tkInt64:
	{$IFDEF DELPHI2009}
      begin
        TypeData := GetTypeData(TypeInfo);
        if TypeData^.MinInt64Value >= 0 then
          Result := varUInt64
        else
          Result := varInt64;
      end;
	{$ELSE}
		Result   := varInt64;
	{$ENDIF}

    tkClass: Result := varInteger;

    else
      Result := varError;
  end;
end;

procedure GetFloatReturn(var Ret; FloatType: TFloatType);
asm
  CMP     EDX, ftSingle
  JE      @@Single
  CMP     EDX, ftDouble
  JE      @@Double
  CMP     EDX, ftExtended
  JE      @@Extended
  CMP     EDX, ftCurr
  JE      @@Curr
  CMP     EDX, ftComp
  JE      @@Curr    // Same as Curr
        // should never get here
@@Single:
  FSTP      DWORD PTR [EAX]
  WAIT
  RET
@@Double:
  FSTP      QWORD PTR [EAX]
  WAIT
  RET
@@Extended:
  FSTP      TBYTE PTR [EAX]
  WAIT
  RET
@@Curr:
  FISTP     QWORD PTR [EAX]
  WAIT
end;

function GetMethods(ClassType: TClass): TMethodInfoArray;
var
  VMT:        Pointer;
  MethodInfo: Pointer;
  Count:      Integer;
  I:          Integer;
begin
  Count := 0;
  VMT   := ClassType;
  repeat
    MethodInfo := PPointer(Integer(VMT) + vmtMethodTable)^;
    if MethodInfo <> nil then
      Inc(Count, PWord(MethodInfo)^);
    // Find the parent VMT
    VMT := PPointer(Integer(VMT) + vmtParent)^;
    if VMT = nil then
      Break;
    VMT := PPointer(VMT)^;
  until False;
  SetLength(Result, Count);
  I   := 0;
  VMT := ClassType;
  repeat
    MethodInfo := PPointer(Integer(VMT) + vmtMethodTable)^;
    if MethodInfo <> nil then
    begin
      Count := PWord(MethodInfo)^;
      Inc(Integer(MethodInfo), SizeOf(Word));
      while Count > 0 do
      begin
        Result[I] := MethodInfo;
        Inc(I);
        Inc(Integer(MethodInfo), PMethodInfoHeader(MethodInfo)^.Len);
        Dec(Count);
      end;
    end;
    // Find the parent VMT
    VMT := PPointer(Integer(VMT) + vmtParent)^;
    if VMT = nil then
      Exit;
    VMT := PPointer(VMT)^;
  until False;
end;

function GetMethodInfo(Instance: TObject; const MethodName: ShortString): PMethodInfoHeader;
var
  VMT:        Pointer;
  MethodInfo: Pointer;
  Count:      Integer;
begin
  // Find the method
  VMT := PPointer(Instance)^;
  repeat
    MethodInfo := PPointer(Integer(VMT) + vmtMethodTable)^;
    if MethodInfo <> nil then
    begin
      // Scan method table for the method
      Count := PWord(MethodInfo)^;
      Inc(Integer(MethodInfo), 2);
      while Count > 0 do
      begin
        Result := MethodInfo;
        if {$IFDEF DELPHI2009}SamePropTypeName{$ELSE}SameText{$ENDIF}(Result^.Name, MethodName) then
          Exit;
        Inc(Integer(MethodInfo), PMethodInfoHeader(MethodInfo)^.Len);
        Dec(Count);
      end;
    end;
    // Find the parent VMT
    VMT := PPointer(Integer(VMT) + vmtParent)^;
    if VMT = nil then
    begin
      Result := nil;
      Exit;
    end;

    VMT := PPointer(VMT)^;
  until False;
end;

resourcestring
  sMethodNotFound = 'Method %s of class %s not found';
  sTypeMisMatch   = 'Type mismatch in parameter %d for method %s';
  sInvalidDispID  = 'Invalid DispID for parameter %d in method %s';
  sParamRequired  = 'Parameter %d required for method %s';
  sMethodOver     = 'Method definition for %s has over %d parameters';
  sTooManyParams  = 'Too many parameters for method %s';

 /// ObjectInvoke - function to dymically invoke a method of an object that
 /// has sufficient type information.
 ///   Instance -      the object to invoke the method on
 ///   MethodHeader -  the type information for the method obtained through
 ///                   GetMethodInfo.
 ///   ParamIndexs -   the indexs of the parameters. This assumes that the
 ///                   indexs are 1 offset. The number of indexs do not need
 ///                   to match the number of parameters. The parameters left
 ///                   over are assumed to fill in the holes left by indexs.
 ///                   Param indexs are assumed to be in lexical order, not
 ///                   inverse lexical order like Params.
 ///   Params -        the parameters for the function invocation. The
 ///                   order of the parameters is assumed to be in inverse
 ///                   lexical order, last parameter first.

function ObjectInvoke(Instance: TObject; MethodHeader: PMethodInfoHeader;
  const ParamIndexes: array of Integer;
  const Params: array of Variant): Variant;
const
  MaxParams = 32;

  procedure Swap(var A, B: PParamInfo);
  var
    T: PParamInfo;
  begin
    T := A;
    A := B;
    B := T;
  end;

var
  MethodName: string;

  procedure ParameterMismatch(I: Integer);
  begin
    raise Exception.CreateFmt(sTypeMisMatch, [I, MethodName]);
  end;

var
  MethodInfo:    Pointer;
  ReturnInfo:    PReturnInfo;
  MethodAddr:    Pointer;
  InfoEnd:       Pointer;
  Count:         Integer;
  I, K, P:       Integer;
  Param:         PParamInfo;
  Regs:          array[paEAX..paECX] of Cardinal;
  RetVal:        Variant;
  ParamType:     TVarType;
  VarType:       TVarType;
  ParamVarData:  PVarData;
  PushData:      Pointer;
  ParamBytes:    Integer;
  Size:          Integer;
  Frame:         {$IFDEF DELPHI2009}PByte{$ELSE}PChar{$ENDIF};
  ResultParam:   Pointer;
  ResultPointer: Pointer;
  ParamInfos:    array[0..MaxParams - 1] of PParamInfo;
  ParamData:     array[0..MaxParams - 1] of Pointer;
  Pointers:      array[0..MaxParams - 1] of Pointer;
  Temps:         array[0..MaxParams - 1] of Variant;
begin
  // MethodInfo now points to the method we found.
  MethodInfo := MethodHeader;
  MethodAddr := MethodHeader^.Addr;
  MethodName := {$IFDEF DELPHI2009}UTF8ToString({$ENDIF} PMethodInfoHeader(MethodInfo)^.Name{$IFDEF DELPHI2009}){$ENDIF};
  Inc(Integer(MethodInfo), SizeOf(TMethodInfoHeader) - SizeOf(ShortString) + 1 +
    Length(MethodName));
  ReturnInfo := MethodInfo;
  Inc(Integer(MethodInfo), SizeOf(TReturnInfo));

  InfoEnd := Pointer(Integer(MethodHeader) + MethodHeader^.Len);
  Count   := 0;
  while Integer(MethodInfo) < Integer(InfoEnd) do
  begin
    if Count >= MaxParams then
      raise Exception.CreateFmt(sMethodOver, [MethodName, MaxParams]);
    ParamInfos[Count] := MethodInfo;
    Inc(Count);
    Inc(Integer(MethodInfo), SizeOf(TParamInfo) - SizeOf(ShortString) + 1 +
      Length(PParamInfo(MethodInfo)^.Name));
  end;

  if High(Params) >= Count then
    raise Exception.CreateFmt(sTooManyParams, [MethodName]);

  // Fill the ParamData array, converting the type as necessary, taking
  // into account any ParamIndexes supplied
  P := 0;
  FillChar(ParamData, SizeOf(ParamData), 0);
  for I := 0 to High(Params) do
  begin
    // Figure out what parameter index this parameter refers to.
    // If it is a named parameter it will have an entry in the ParamIndexs
    // array. If not, P points to the current parameter to use for unnamed
    // parameters. K is the formal parameter number.
    // This calculation assumes Self is first and any result parameters are last
    if I <= High(ParamIndexes) then
    begin
      K := ParamIndexes[I];
      if K >= Count then
        raise Exception.CreateFmt(sInvalidDispID, [I, MethodName]);
    end
    else
      K := High(Params) - P + 1;  // Add one to account for Self
    Param := ParamInfos[K];
    ParamType    := GetVariantType(Param^.ParamType^);
    ParamVarData := @Params[I];
    VarType      := ParamVarData^.VType;
    if Param^.Flags * [pfOut, pfVar] <> [] then
    begin

      // For pfVar, the variant must be a byref and equal to the type.
      if (VarType <> ParamType or varByRef) and (ParamType <> varVariant) then
        ParameterMismatch(I);
    end
    else
      // Convert the parameter to the right type
      case ConvertKindOf(VarType and varTypeMask, ParamType) of
        ckConvert:
          try
            Temps[I] := VarAsType(Params[I], ParamType);
            // The data bytes for sizes < 4 are dirty, that is they are not
            // guarenteed to have 0's in the high bytes. We need them to be zero'ed
            if ParamType <= CMaxArrayVarType then
              case CVarTypeToElementInfo[ParamType].Size of
                1: TVarData(Temps[I]).VLongWord := TVarData(Temps[I]).VByte;
                2: TVarData(Temps[I]).VLongWord := TVarData(Temps[I]).VWord;
              end;
            ParamVarData := @Temps[I];
          except
            ParameterMismatch(I);
          end;
        ckError: ParameterMismatch(I);
      end;

    if ParamType = varVariant then
    begin
      Pointers[K]  := ParamVarData;
      ParamData[K] := @Pointers[K];
    end
    else if varByRef and VarType <> 0 then
      ParamData[K] := @ParamVarData^.VPointer
    else
      ParamData[K] := @ParamVarData^.VInteger;

    // Update P which is the pointer to the current non-named parameter.
    // This assumes that unnamed parameter fill in the holes left by
    // named parameters.
    while (P <= High(Params)) and (ParamData[High(Params) - P + 1] <> nil) do
      Inc(P);
  end;

  // Set up the call frame        RET EBP
  ParamBytes := ReturnInfo^.ParamSize - (4 + 4);
  asm
    SUB     ESP,ParamBytes
    MOV     Frame,ESP
  end;
  Dec(Integer(Frame), 4 + 4); // Access numbers include RET and EBP

  // Push the parameters on the stack (or put them into the correct register)
  ResultParam := nil;
  for I := 0 to Count - 1 do
  begin
    Param    := ParamInfos[I];
    PushData := ParamData[I];
    if PushData = nil then
      if (Param^.ParamType^.Kind = tkClass) and {$IFDEF DELPHI2009}SamePropTypeName{$ELSE}SameText{$ENDIF}(Param^.Name, 'SELF') then
        // Self is special. It doesn't appear in the ParamData array since it
        // is not represented in the Params array.
        PushData := @Instance
      else if pfResult in Param^.Flags then
      begin
        ResultParam := Param;
        VarClear(Result);
        TVarData(Result).VType := GetVariantType(Param^.ParamType^);
        if TVarData(Result).VType = varVariant then
          ResultPointer := @Result
        else
          ResultPointer := @TVarData(Result).VInteger;
        PushData := @ResultPointer;
      end
      else
        raise Exception.CreateFmt(sParamRequired, [I, MethodName]);
    if Param^.Access < Word(Ord(paStack)) then
      Regs[Param^.Access] := PCardinal(PushData)^
    else
    begin
      if [pfVar, pfOut, pfResult] * Param^.Flags <> [] then
        PCardinal(@Frame[Param^.Access])^ := PCardinal(PushData)^
      else
      begin
        Size := GetTypeSize(Param^.ParamType^);
        case Size of
          1, 2, 4:
            PCardinal(@Frame[Param^.Access])^ := PCardinal(PushData)^;
          8:
          begin
            PCardinal(@Frame[Param^.Access])^     := PCardinal(PushData)^;
            PCardinal(@Frame[Param^.Access + 4])^ :=
              PCardinal(Integer(PushData) + 4)^;
          end;
          else
            Move(PushData^, Frame[Param^.Access and not 3], Size);
        end;
      end;
    end;
  end;

  // Do the call
  asm
    MOV     EAX,DWORD PTR Regs[0]
    MOV     EDX,DWORD PTR Regs[4]
    MOV     ECX,DWORD PTR Regs[8]
    CALL    MethodAddr
    MOV     DWORD PTR Regs[0],EAX
    MOV     DWORD PTR Regs[4],EDX
  end;

  if ReturnInfo^.CallingConvention = ccCdecl then
  asm
    ADD     ESP,ParamBytes
  end;

  if (ResultParam = nil) and (ReturnInfo^.ReturnType <> nil) then
  begin
    // The result came back in registers. Otherwise a result pointer was used
    // and the return variant is already initialized (or it was a procedure)
    TVarData(RetVal).VType := GetVariantType(ReturnInfo^.ReturnType^);
    if ReturnInfo^.ReturnType^.Kind = tkFloat then
      GetFloatReturn(TVarData(RetVal).VDouble, GetTypeData(ReturnInfo^.ReturnType^)^.FloatType)
    else
    begin
      // For regular Boolean types, we must convert it to a boolean to
      // wipe the high order bytes; otherwise the caller may see a false
      // as true.
      if (TVarData(RetVal).VType = varBoolean) and
        (ReturnInfo^.ReturnType^ = System.TypeInfo(Boolean)) then
        TVarData(RetVal).VInteger := Integer(Boolean(Regs[paEAX]))
      else
        TVarData(RetVal).VInteger := Integer(Regs[paEAX]);
      PCardinal(Integer(@TVarData(RetVal).VInteger) + 4)^ := Regs[paEDX];
    end;
    Result                 := RetVal;
    TVarData(RetVal).VType := varEmpty;
  end;
end;

function GetReturnInfo(aObj: TObject; aMethodName: string): PReturnInfo;
var
  mi: PMethodInfoHeader;
begin
  mi := GetMethodInfo(aObj, ShortString(aMethodName));
  if mi.Len <= SizeOf(TMethodInfoHeader) + Length(mi.Name) - SHORT_LEN then
  begin
    Result := nil;
    Exit;
  end;
  Result := PReturnInfo(integer(mi) + SizeOf(TMethodInfoHeader) + Length(mi.Name) - SHORT_LEN);
end;

function GetParams(aObj: TObject; aMethodName: string): TParamInfoArray;
var
  mi:    PMethodInfoHeader;
  miEnd: Pointer;
  param: PParamInfo;
  Count: integer;
begin
  SetLength(Result, 0);
  mi := GetMethodInfo(aObj, ShortString(aMethodName));
  if mi.Len <= SizeOf(TMethodInfoHeader) + Length(mi.Name) - SHORT_LEN then
    Exit;
  miEnd := Pointer(integer(mi) + mi.Len);
  param := PParamInfo(integer(mi) + SizeOf(TMethodInfoHeader) + Length(mi.Name) - SHORT_LEN + SizeOf(TReturnInfo));
  Count := 0;
  while integer(param) < integer(miEnd) do
  begin
    Inc(Count);
    SetLength(Result, Count);
    Result[Count - 1] := param;
    param             := PParamInfo(integer(param) + SizeOf(TParamInfo) + Length(param.Name) - SHORT_LEN);
  end;
end;

type
  PParameterInfos = ^TParameterInfos;
  TParameterInfos = array[0..255] of ^PTypeInfo;

  TBaseMethodHandlerInstance = class
  protected
    TypeData:     PTypeData;
    ParamInfos:   PParameterInfos;
    ParamOffsets: array of Word;
    StackSize:    Integer;
    procedure InternalHandler(Params: Pointer);
    procedure Handler(Params: Pointer); virtual; abstract;
    procedure RegisterStub;
  public
    constructor Create(TypeData: PTypeData);
  end;

  TMethodHandlerInstance = class(TBaseMethodHandlerInstance)
  protected
    MethodHandler: IMethodHandler;
    procedure Handler(Params: Pointer); override;
  public
    constructor Create(const MethodHandler: IMethodHandler; TypeData: PTypeData);
  end;

  TEventHandlerInstance = class(TBaseMethodHandlerInstance)
  protected
    FDynamicInvokeEvent: TDynamicInvokeEvent;
    procedure Handler(Params: Pointer); override;
  public
    constructor Create(const ADynamicInvokeEvent: TDynamicInvokeEvent; TypeData: PTypeData);
  end;

function AdditionalInfoOf(TypeData: PTypeData): Pointer;
var
  P: {$IFDEF DELPHI2009}PByte{$ELSE}PChar{$ENDIF};
  I: Integer;
begin
  P := @TypeData^.ParamList;
  // Skip parameter names and types
  for I := 1 to TypeData^.ParamCount do
  begin
    Inc(P, 1 + {$IFNDEF DELPHI2009}Byte{$ENDIF}(P[1]) + 1);
    Inc(P, {$IFNDEF DELPHI2009}Byte{$ENDIF}(P[0]) + 1);
  end;
  if TypeData^.MethodKind = mkFunction then
    // Skip return type name and info
    Inc(P, {$IFNDEF DELPHI2009}Byte{$ENDIF}(P[0]) + 1 + 4);
  Result := P;
end;

function CreateMethodPointer(const MethodHandler: IMethodHandler; TypeData: PTypeData): TMethod;
begin
  Result.Data := TMethodHandlerInstance.Create(MethodHandler, TypeData);
  Result.Code := @TMethodHandlerInstance.RegisterStub;
end;

function CreateMethodPointer(const ADynamicInvokeEvent: TDynamicInvokeEvent; TypeData: PTypeData): TMethod; overload;
begin
  Result.Data := TEventHandlerInstance.Create(ADynamicInvokeEvent, TypeData);
  Result.Code := @TMethodHandlerInstance.RegisterStub;
end;

procedure ReleaseMethodPointer(MethodPointer: TMethod);
begin
  TObject(MethodPointer.Data).Free;
end;

function GetInvokeInstance(MethodPointer: TMethod): TObject;
begin
  if TObject(MethodPointer.Data) is TEventHandlerInstance then
    Result := TObject(TMethod(TEventHandlerInstance(MethodPointer.Data).FDynamicInvokeEvent).Data)
  else
    Result := nil;
end;

{ TBaseMethodHandlerInstance }

constructor TBaseMethodHandlerInstance.Create(TypeData: PTypeData);
var
  P:      {$IFDEF DELPHI2009}PByte{$ELSE}PChar{$ENDIF};
  Offset: Integer;
  CurReg: Integer;
  I:      Integer;
  Size:   Integer;
begin
  Self.TypeData := TypeData;

  P          := AdditionalInfoOf(TypeData);
  ParamInfos := PParameterInfos(Cardinal(P) + 1);

  // Calculate stack size
  CurReg    := paEDX;
  P         := @TypeData^.ParamList;
  StackSize := 0;
  for I := 0 to TypeData^.ParamCount - 1 do
  begin
    if TParamFlags(P[0]) * [pfVar, pfConst, pfAddress, pfReference, pfOut] <> [] then
      Size := 4
    else
      Size := GetTypeSize(ParamInfos^[I]^);
    if (Size <= 4) and (CurReg <= paECX) then
      Inc(CurReg)
    else
      Inc(StackSize, Size);
    Inc(P, 1 + {$IFNDEF DELPHI2009}Byte{$ENDIF}(P[1]) + 1);
    Inc(P, {$IFNDEF DELPHI2009}Byte{$ENDIF}(P[0]) + 1);
  end;

  // Calculate parameter offsets
  SetLength(ParamOffsets, TypeData^.PropCount);
  CurReg := paEDX;
  P      := @TypeData^.ParamList;
  Offset := StackSize;
  for I := 0 to TypeData^.ParamCount - 1 do
  begin
    if TParamFlags(P[0]) * [pfVar, pfConst, pfAddress, pfReference, pfOut] <> [] then
      Size := 4
    else
      Size := GetTypeSize(ParamInfos^[I]^);
    if (Size <= 4) and (CurReg <= paECX) then
    begin
      ParamOffsets[I] := CurReg;
      Inc(CurReg);
    end
    else
    begin
      Dec(Offset, Size);
      ParamOffsets[I] := Offset;
    end;
    Inc(P, 1 + {$IFNDEF DELPHI2009}Byte{$ENDIF}(P[1]) + 1);
    Inc(P, {$IFNDEF DELPHI2009}Byte{$ENDIF}(P[0]) + 1);
  end;
end;

procedure TBaseMethodHandlerInstance.InternalHandler(Params: Pointer);
asm
  MOV     ECX,[EAX]
  JMP     DWORD PTR [ECX] + VMTOFFSET TMethodHandlerInstance.Handler
end;

procedure TBaseMethodHandlerInstance.RegisterStub;
const
  PtrSize = SizeOf(Pointer);
asm
  PUSH    EAX
  PUSH    ECX
  PUSH    EDX
  MOV     EDX,ESP
  CALL    InternalHandler
  // Pop EDX and ECX off the stack while preserving all registers.
  MOV     [ESP+4],EAX
  POP     EAX
  POP     EAX
  POP     ECX    // Self
  MOV     ECX,[ECX].TMethodHandlerInstance.StackSize
  TEST    ECX,ECX
  JZ      @@SimpleRet
  // Jump to the actual return instruction since it is most likely not just a RET
  //JMP     ECX    // Data Exec. Prevention: Jumping into a GetMem allocated memory block

  // stack address alignment
  ADD     ECX, PtrSize - 1
  AND     ECX, NOT (PtrSize - 1)
  AND     ECX, $FFFF

  // clean up the stack
  PUSH    EAX                         // we need this register, so save it
  MOV     EAX,[ESP + 4]               // Load the return address
  MOV     [ESP + ECX + 4], EAX        // Just blast it over the first param on the stack
  POP     EAX
  ADD     ESP,ECX                     // This will move the stack back to where the moved
  // return address is now located. The next RET
  // instruction will do the final stack cleanup
@@SimpleRet:
end;

{ TMethodHandlerInstance }

constructor TMethodHandlerInstance.Create(const MethodHandler: IMethodHandler;
  TypeData: PTypeData);
begin
  inherited Create(TypeData);
  Self.MethodHandler := MethodHandler;
end;

procedure TMethodHandlerInstance.Handler(Params: Pointer);
const
  MaxParams = 10;
var
  P:           {$IFDEF DELPHI2009}PByte{$ELSE}PChar{$ENDIF};
  Parameters:  PParameters;
  ReturnValue: Variant;
  ParamValues: array[0..MaxParams - 1] of Variant;
  I:           Integer;
  Regs:        array[paEAX..paEDX] of Cardinal;
  Offset:      Integer;
  Data:        Pointer;
  Temp:        Variant;
begin
  Parameters := Params;

  // Fetch the parameters into ParamValues
  P := @TypeData^.ParamList;
  for I := 0 to TypeData^.ParamCount - 1 do
  begin
    Offset := ParamOffsets[I];
    if (Offset >= paEDX) and (Offset <= paECX) then
      Data := @Parameters^.Registers[Offset]
    else
      Data := @Parameters^.Stack[Offset];
    if ParamInfos^[I]^.Kind = tkClass then
      ParamValues[TypeData^.ParamCount - I - 1] := MethodHandler.InstanceToVariant(PPointer(Data)^)
    else if TParamFlags(P[0]) * [pfVar, pfOut] <> [] then
      with TVarData(ParamValues[TypeData^.ParamCount - I - 1]) do
      begin
        VType    := GetVariantType(ParamInfos[I]^) or varByRef;
        VPointer := Pointer(PCardinal(Data)^);
      end
    else
    begin
      TVarData(Temp).VType := GetVariantType(ParamInfos[I]^) or varByRef;
      if TParamFlags(P[0]) * [pfVar, pfOut] <> [] then
        TVarData(Temp).VPointer := Pointer(PCardinal(Data)^)
      else
        TVarData(Temp).VPointer := Data;
      ParamValues[TypeData^.ParamCount - I - 1] := Temp;
    end;
    Inc(P, 1 + {$IFNDEF DELPHI2009}Byte{$ENDIF}(P[1]) + 1);
    Inc(P, {$IFNDEF DELPHI2009}Byte{$ENDIF}(P[0]) + 1);
  end;
  // P is left pointing to the return type name if there is one.

  ReturnValue := MethodHandler.Execute(Slice(ParamValues, TypeData^.ParamCount));
  if TypeData^.MethodKind = mkFunction then
  begin
    Inc(P, {$IFNDEF DELPHI2009}Byte{$ENDIF}(P[0]) + 1);
    ReturnValue := VarAsType(ReturnValue, GetVariantType(PPTypeInfo(P)^));
    if PPTypeInfo(P)^.Kind = tkFloat then

    else
    begin
      Regs[paEAX] := TVarData(ReturnValue).VLongWord;
      Regs[paEDX] := PCardinal(Integer(@TVarData(ReturnValue).VLongWord) + 4)^;
    end;
  end;

  // Let Stub procedures know where the RET instruction is
  asm
    MOV     EAX,DWORD PTR Regs[paEAX*4]
    MOV     EDX,DWORD PTR Regs[paEDX*4]
  end;
end;

{ TEventHandlerInstance }

constructor TEventHandlerInstance.Create(const ADynamicInvokeEvent: TDynamicInvokeEvent; TypeData: PTypeData);
begin
  inherited Create(TypeData);
  Self.FDynamicInvokeEvent := ADynamicInvokeEvent;
end;

procedure TEventHandlerInstance.Handler(Params: Pointer);
begin
  if Assigned(FDynamicInvokeEvent) then
    FDynamicInvokeEvent(Params, StackSize);
end;

end.
