/// `crypto` module support bindings for SyNode
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SyNodeBinding_crypto;

interface
{$I Synopse.inc}
{$I SyNode.inc}
uses
  SysUtils, SynCommons, SyNode, SpiderMonkey, SynOpenSSLWrap;

type
  {$M+}

  { Hmac }

  Hmac = class(THmac)
  private
  public
  published
    /// initialize(algo: string, secret: string|ArrayBuffer)
    function init(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean;
    /// update(data: string|ArrayBuffer; encoding: string)
    function update(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean;
    /// digest(outputEncoding: string)
    function digest(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean;
  end;
  {$M-}

implementation

uses
  SyNodeSimpleProto, SynOpenSSL, jsbutils;

type

  { THmacProtoObject }

  THmacProtoObject = class(TSMSimpleRTTIProtoObject)
  public
    function NewSMInstance(cx: PJSContext; argc: uintN; var vp: JSArgRec): TObject; override;
  end;

{ THmacProtoObject }

function THmacProtoObject.NewSMInstance(cx: PJSContext; argc: uintN; var vp: JSArgRec): TObject;
begin
  Result := Hmac.Create();
end;

{ Hmac }
function Hmac.init(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean;
var
  algo: string;
  len: uint32;
  bufData: pointer;
begin
  result := checkFuncArgs(cx, argc, vp, [atStr, atBuf]);
  if not result then exit;
  try
    algo := vp.argv^[0].asJSString.ToString(cx);
    vp.argv^[1].asObject.GetBufferDataAndLength(bufData, len);
    inherited init(algo, bufdata, len);
    result := true;
  except
    on E: Exception do begin Result := false; JSError(cx, E); end;
  end;
end;

function Hmac.update(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean;
var
  encoding: string;
  s: RawUTF8;
  len: uint32;
  bufData: pointer;
begin
  result := checkFuncArgs(cx, argc, vp, [atStr or atBuf, atStr]);
  if not result then exit;
  try
    if not vp.argv^[0].isString then // ignore encoding if buffer is passed
      vp.argv^[0].asObject.GetBufferDataAndLength(bufData, len)
    else begin // data is string
      encoding := vp.argv^[1].asJSString.ToString(cx);
      if (encoding = 'utf8') or (encoding = 'buffer')  then
        s := vp.argv^[0].asJSString.ToUTF8(cx)
      else
        raise ESMException.CreateFmt('Hmac.update. Encoding "%s" not implemented - only "utf8"', [encoding]);
      len := length(s);
      bufData := pointer(s);
    end;
    inherited update(bufdata, len);
    result := true;
  except
    on E: Exception do begin Result := false; JSError(cx, E); end;
  end;
end;

function Hmac.digest(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean;
var
  encoding: string;
  outAsBuffer: boolean;
  mdres: array[0..EVP_MAX_MD_SIZE] of byte;
  resLen: cardinal;
  bufObj: PJSObject;
  bufData: pointer;
begin
  result := checkFuncArgs(cx, argc, vp, [atStr or atVoid]);
  if not result then exit;
  try
    if (argc = 1) and vp.argv^[0].isString then begin
      encoding := vp.argv^[0].asJSString.ToString(cx);
      if encoding = 'buffer' then
        outAsBuffer := true
      else if (encoding <> 'base64') and (encoding <> 'hex') then
        raise ESMException.CreateFmt('Hmac.digest. Encoding "%s" not implemented - only "buffer/hex/base64"', [encoding])
      else
        outAsBuffer := false;
    end else
      outAsBuffer := true;
    resLen := inherited digest(@mdres);
    if (outAsBuffer) then begin
      bufObj := cx.NewArrayBuffer(resLen);
      bufData := bufObj.GetArrayBufferData;
      Move(mdres[0], bufData^, resLen);
      vp.rval := bufObj.ToJSValue;
    end else if (encoding = 'base64') then
      vp.rval := cx.NewJSString(BinToBase64(@mdres, resLen)).ToJSVal
    else // hex
      vp.rval := cx.NewJSString(BinToHexLower(@mdres, resLen)).ToJSVal;
    result := true;
  except
    on E: Exception do begin Result := false; JSError(cx, E); end;
  end;
end;

function SyNodeBindingProc_crypto(const aEngine: TSMEngine; const bindingNamespaceName: SynUnicode): jsval;
var
  obj: PJSRootedObject;
  cx: PJSContext;
const
  props = JSPROP_ENUMERATE or JSPROP_ENUMERATE or JSPROP_PERMANENT;
begin
  cx := aEngine.cx;
  obj := cx.NewRootedObject(cx.NewObject(nil));
  try
    aEngine.defineClass(Hmac, THmacProtoObject, obj);

    Result := obj.ptr.ToJSValue;
  finally
    cx.FreeRootedObject(obj);
  end;
end;

initialization
  TSMEngineManager.RegisterBinding('crypto', SyNodeBindingProc_crypto);

end.

