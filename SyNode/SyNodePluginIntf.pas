unit SyNodePluginIntf;

interface

uses
  SynCommons,
  SpiderMonkey;

type
  TSMPluginRec = {$ifdef UNICODE}record{$else}object{$endif}
    cx: PJSContext;
    Exp: PJSRootedObject;
    Req: PJSRootedObject;
    Module: PJSRootedObject;
    filename: PWideChar;
    dirname: PWideChar;
    constructor Create(aCx: PJSContext; aExports_: PJSRootedObject; aRequire: PJSRootedObject; aModule: PJSRootedObject; _filename: PWideChar; _dirname: PWideChar);
    function require(aFileName: string): PJSObject;
  end;

  TCustomSMPlugin = class
  protected
    fCx: PJSContext; 
    procedure UnInit; virtual;
    procedure Init(const rec: TSMPluginRec); virtual;
  public
    constructor Create(aCx: PJSContext; aExports_: PJSRootedObject; aRequire: PJSRootedObject; aModule: PJSRootedObject; _filename: PWideChar; _dirname: PWideChar);
    destructor Destroy; override;
  end;

  TThreadRec = record
    threadID: TThreadID;
    plugin: TCustomSMPlugin;
  end;
  TCustomSMPluginType = class of TCustomSMPlugin;

type
  TNsmFunction = function(cx: PJSContext; argc: uintN; vals: PjsvalVector; thisObj, calleeObj: PJSObject): jsval; cdecl;

const
  ptVoid = 0;
  ptInt = 1;
  ptStr = 2;
  ptObj = 3;
  ptBuffer = 100;
  ptAny = 500;

function nsmCallFunc(cx: PJSContext; argc: uintN;  var vp: JSArgRec; const Params: array of uintN; isConstructor: Boolean = false): Boolean; cdecl;

const
  StaticROAttrs = JSPROP_ENUMERATE or JSPROP_READONLY or JSPROP_PERMANENT;

implementation

uses
  SysUtils;

function isParamCorrect(paramType: uintN; val: jsval): Boolean;
begin
  case paramType of
    ptVoid: result := val.isVoid;
    ptInt: result := val.isInteger;
    ptStr: result := val.isString;
    ptObj: result := val.isObject;
    ptBuffer: result := val.isObject and val.asObject.IsArrayBufferObject;
    ptAny: result := True;
    else result := false;
  end;
end;

///////
/// Structure of params Is
/// (param_count_case1(N1),param1_case1,param2_case1...paramN1_case1,Integer(call_case1),
///  param_count_case2(N2),param1_case2,param2_case2...paramN2_case2,Integer(call_case2),...
///  param_count_caseK(NK),param1_caseK,param2_caseK...paramNK_caseK,Integer(call_caseK2)
///  )
function nsmCallFunc(cx: PJSContext; argc: uintN; var vp: JSArgRec; const Params: array of uintN; isConstructor: Boolean = false): Boolean; cdecl;
var
  thisObj, calleeObj: PJSObject;
  vals: PjsvalVector;
  i,j: uintN;
  paramsCount: uintN;
  call: TNsmFunction;
  IsCalled, IsCorrect: Boolean;
begin
  Result := False;
  try
    if isConstructor xor vp.IsConstructing then
    begin
      raise ESMException.Create('JS_IS_CONSTRUCTING');
    end;
    thisObj := vp.thisObject[cx];
    calleeObj := vp.calleObject;

    vals := vp.argv;

    IsCalled := false;

    i := Low(Params);
    while i<=uintN(High(Params)) do begin
      paramsCount := Params[i];
      Inc(i);
      if argc = paramsCount then begin
        IsCorrect := true;
        if paramsCount>0 then
          for j := 0 to paramsCount - 1 do begin
            IsCorrect := isParamCorrect(Params[i+j], vals[j]);
            if not IsCorrect then Break;
          end;
        if IsCorrect then begin
          call := TNsmFunction(Params[i+paramsCount]);

          vp.rval := call(cx, argc, vals, thisObj, calleeObj);
          IsCalled := True;
          Break;
        end;
      end;
      inc(i, paramsCount+1);
    end;

    if not IsCalled then
      raise ESMException.Create('invalid ussage');

    Result := True;
  except
    on E: Exception do
    begin
      JSError(cx, E);
    end;
  end;
end;

{ TCustomSMPlugin }


destructor TCustomSMPlugin.Destroy;
begin
  fCx.BeginRequest;
  try
    UnInit;
  finally
    fCx.EndRequest;
  end;
  inherited;
end;

procedure TCustomSMPlugin.Init(const rec: TSMPluginRec);
begin
end;

procedure TCustomSMPlugin.UnInit;
begin
end;

constructor TCustomSMPlugin.Create(aCx: PJSContext; aExports_, aRequire,
  aModule: PJSRootedObject; _filename, _dirname: PWideChar);
var
  rec: TSMPluginRec;
begin
  fCx := aCx;
  rec.Create(aCx, aExports_, aRequire, aModule, _filename, _dirname);
  fCx.BeginRequest;
  try
    Init(rec);
  finally
    fCx.EndRequest;
  end;

end;

constructor TSMPluginRec.Create(aCx: PJSContext; aExports_: PJSRootedObject; aRequire: PJSRootedObject; aModule: PJSRootedObject; _filename: PWideChar; _dirname: PWideChar);
begin
  cx := aCx;
  Exp := aExports_;
  Req := aRequire;
  Module := aModule;
  filename := _filename;
  dirname := _dirname;
end;

function TSMPluginRec.require(aFileName: string): PJSObject;
var
  arg, rval: jsval;
begin
  arg := cx.NewJSString(aFileName).ToJSVal;
  if not Module.ptr.CallFunction(cx, req.ptr, 1, @arg, rval) then
     raise ESMException.Create('Error require '+aFileName);
  Result := rval.asObject;
end;

end.
