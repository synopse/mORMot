/// `http` module bindings for SyNode
// TODO - current implementation is not filly compatible with nodeJS
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
//
//   Contributor(s):
//    - Pavel Mashlyakovsky

unit SyNodeBinding_HTTPClient;

interface
{$I Synopse.inc}
{$I SyNode.inc}
uses
  SysUtils, SynCrtSock, SynCommons, SpiderMonkey, SyNode, SyNodeSimpleProto;

type
  {$M+}
  THTTPClient = class
  private
    fClient: THttpRequest;
    fInHeaders: RawUTF8;
    FMethod: RawUTF8;
    FWriter: TTextWriter;
    FInData: RawByteString;

    FRespHeaders: SockString;
    FRespText: SockString;
    FKeepAlive: Cardinal;
    FResponseStatus: integer;
    fConnectTimeout: integer;
    fSendTimeout: integer;
    fReceiveTimeout: integer;
    fRespAlreadyRead: boolean;
  protected
    function GetRespHeaders: RawUTF8;
    function sendFile(const URL: RawUTF8; aFileName: string): boolean;
  public
    destructor Destroy; override;
  published
    // for timeout explain see http://msdn.microsoft.com/en-us/library/windows/desktop/aa384116%28v=vs.85%29.aspx
    constructor Create();

    function initialize(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean;
    function write(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean;
    function writeEnd(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean;
    function read(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean;

    function doRequest(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean;

    property method: RawUTF8 read FMethod write FMethod;
    property keepAlive: cardinal read FKeepAlive write FKeepAlive default 1;
    property headers: RawUTF8 read fInHeaders write fInHeaders;

    property responseHeaders: RawUTF8 read GetRespHeaders;
    property responseStatus: integer read FResponseStatus;
  end;
  {$M-}

  { THTTPClientProtoObject }

  THTTPClientProtoObject = class(TSMSimpleRTTIProtoObject)
  public
    function NewSMInstance(cx: PJSContext; argc: uintN; var vp: JSArgRec): TObject; override;
  end;

implementation

uses
  SynZip, SyNodeReadWrite;

{ THTTPClientProtoObject }

function THTTPClientProtoObject.NewSMInstance(cx: PJSContext; argc: uintN; var vp: JSArgRec): TObject;
begin
  Result := THTTPClient.Create();
end;

{ THTTPClient }
constructor THTTPClient.Create();
begin
  inherited Create;
  FMethod := 'POST';
  FKeepAlive := 1;
end;

function THTTPClient.initialize(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean;
var
  in_argv: PjsvalVector;
  aServer, aPort: AnsiString;
  aHttps, aStrictSSL: boolean;
  aProxyName, aProxyByPass: RawUTF8;
begin
  in_argv := vp.argv;
  Result := true;
  try
    if (fClient <> nil) then raise ESMException.Create('already initialized');
    if (argc < 2) or (not in_argv[0].isString) or (not in_argv[1].isString) then
      raise ESMException.Create('invalid usage');

    aServer := in_argv[0].asJSString.ToAnsi(cx);
    aPort := in_argv[1].asJSString.ToAnsi(cx);

    if (argc > 2) and (in_argv[2].isBoolean)  then
      aHttps := in_argv[2].asBoolean
    else
      aHttps := false;

    if (argc > 4) and (in_argv[4].isString)  then
      aProxyName := in_argv[4].asJSString.ToUTF8(cx)
    else
      aProxyName := '';

    if (argc > 5) and (in_argv[5].isString)  then
      aProxyByPass := in_argv[5].asJSString.ToUTF8(cx)
    else
      aProxyByPass := '';

    if (argc > 6) and (in_argv[6].isInteger)  then
      fConnectTimeout := in_argv[6].asInteger
    else
      fConnectTimeout := HTTP_DEFAULT_CONNECTTIMEOUT;

    if (argc > 7) and (in_argv[7].isInteger)  then
      fSendTimeout := in_argv[7].asInteger
    else
      fSendTimeout := HTTP_DEFAULT_SENDTIMEOUT;

    if (argc > 8) and (in_argv[8].isInteger)  then
      fReceiveTimeout := in_argv[8].asInteger
    else
      fReceiveTimeout := HTTP_DEFAULT_RECEIVETIMEOUT;

    if (argc > 9) and (in_argv[9].isBoolean)  then
      aStrictSSL := in_argv[9].asBoolean
    else
      aStrictSSL := false;

    fClient := {$IFDEF MSWINDOWS}TWinHTTP{$ELSE}TCurlHTTP{$ENDIF}
      .Create(aServer, aPort, aHttps, aProxyName, aProxyByPass, fConnectTimeout, fSendTimeout, fReceiveTimeout);

    if (argc > 3) and (in_argv^[3].isBoolean) and (in_argv^[3].asBoolean) then
      fClient.RegisterCompress(CompressGZip);

    if aHttps and not aStrictSSL then
      fClient.IgnoreSSLCertificateErrors := true;
  except
    on E: Exception do begin
      Result := False;
      JSError(cx, E);
    end;
  end;
end;

destructor THTTPClient.Destroy;
begin
  fClient.Free;
  inherited Destroy;
end;

function THTTPClient.doRequest(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean;
var
  URL: SockString;
  in_argv: PjsvalVector;
begin
  FRespHeaders := '';
  FRespText := '';
  in_argv := vp.argv;
  Result := true;
  try
    if (argc <> 1) or (not in_argv[0].isString) then
      raise ESMException.Create('usage doRequest(URL: string)');
    URL := in_argv[0].asJSString.ToAnsi(cx);
    try
      fRespAlreadyRead := false;
      FResponseStatus := fClient.Request(URL, FMethod, keepAlive, fInHeaders, FInData, '', FRespHeaders, FRespText);
    except
      on E: EOSError do
        FResponseStatus := E.ErrorCode;
    end;
  except
    on E: Exception do begin
      Result := False;
      JSError(cx, E);
    end;
  end;
  FInData := '';
  FInHeaders := '';
end;

function THTTPClient.read(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean;
begin
  Result := true;
  try
    if (fRespAlreadyRead) then
      raise ESMException.Create('IncomingMessage.read() can be called only once');
    fRespAlreadyRead := true;
    vp.rval := SyNodeReadWrite.SMRead_impl(cx, argc, vp.argv, FRespText);
  except
    on E: Exception do begin
      Result := False;
      JSError(cx, E);
    end;
  end;
  FRespText := ''; // Free memory allocated for response text ASAP
end;

function THTTPClient.sendFile(const URL: RawUTF8; aFileName: string): boolean;
var
  buffer: RawByteString;
begin
  FResponseStatus := 500;
  try
    if not FileExists(aFileName) then begin
       FRespText := 'File not found';
       FResponseStatus := 404;
    end;
    try
      buffer := StringFromFile(aFileName);
      FResponseStatus := fClient.Request(URL, FMethod, FKeepAlive, fInHeaders, buffer, '', FRespHEaders, FRespText);
    except
      on E: EOSError do begin
        FResponseStatus := E.ErrorCode;
        FRespText := StringToUTF8(E.Message);
      end;
    end;
  finally
    Result := (FResponseStatus = 200);
  end;
end;

function THTTPClient.write(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean;
begin
  if (FWriter = nil) then
    FWriter := TTextWriter.CreateOwnedStream;
  Result := True;
  try
    vp.rval := SyNodeReadWrite.SMWrite_impl(cx, argc, vp.argv, FWriter);
  except
    on E: Exception do begin
      Result := False;
      JSError(cx, E);
    end;
  end;
end;

function THTTPClient.writeEnd(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean;
var
  data: RawUTF8;
begin
  Result := True;
  if (argc > 0 ) then
    Result := write(cx, argc, vp);

  if Result then
    FWriter.SetText(data);
  FInData := data;

  FreeAndNil(FWriter);
end;

function THTTPClient.GetRespHeaders: RawUTF8;
begin
  Result := FRespHeaders;
end;

function SyNodeBindingProc_synode_http(const aEngine: TSMEngine; const bindingNamespaceName: SynUnicode): jsval;
var
  obj: PJSRootedObject;
  cx: PJSContext;
begin
  cx := aEngine.cx;
  obj := cx.NewRootedObject(cx.NewObject(nil));
  try
    aEngine.defineClass(THTTPClient, THTTPClientProtoObject, obj);
    Result := obj.ptr.ToJSValue;
  finally
    cx.FreeRootedObject(obj);
  end;
end;

initialization
  TSMEngineManager.RegisterBinding('synode_http', SyNodeBindingProc_synode_http);

end.

