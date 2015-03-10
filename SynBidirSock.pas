/// implements bidirectional client and server protocol, e.g. WebSockets
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynBidirSock;

{
    This file is part of the Synopse framework.

    Synopse framework. Copyright (C) 2015 Arnaud Bouchez
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

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2015
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

  Version 1.18
  - first public release, corresponding to mORMot Framework 1.18


   TODO: add TWebSocketClientRequest with the possibility to
   - broadcast a message to several aCallingThread: THttpServerResp values
   - send asynchronously (e.g. for SOA methods sending events with no result)

   TODO: enhance server process to use events, and not threads
   - current implementation has its threads spending most time waiting in loops
   - eventually also at SynCrtSock's THttpServer class level also


}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  {$else}
  {$ifdef KYLIX3}
  LibC,
  Types,
  SynKylix,
  {$endif}
  {$ifdef FPC}
  SynFPCLinux,
  {$endif}
  {$endif}
  SysUtils,
  Classes,
  Contnrs,
  SyncObjs,
  SynLZ,
  SynCommons,
  SynCrtSock,
  SynCrypto;


type
  /// Exception raised from this unit
  ESynBidirSocket = class(ESynException);


{ -------------- WebSockets shared classes for bidirectional remote access }

type
  /// defines the interpretation of the WebSockets frame data
  TWebSocketFrameOpCode = (
    focContinuation, focText, focBinary,
    focReserved3, focReserved4, focReserved5, focReserved6, focReserved7,
    focConnectionClose, focPing, focPong,
    focReservedB, focReservedC, focReservedD, focReservedE, focReservedF);

  /// set of WebSockets frame interpretation
  TWebSocketFrameOpCodes = set of TWebSocketFrameOpCode;

  /// stores a WebSockets frame
  // - see @http://tools.ietf.org/html/rfc6455 for reference
  TWebSocketFrame = record
    /// the interpretation of the frame data
    opcode: TWebSocketFrameOpCode;
    /// the frame data itself
    // - is plain UTF-8 for focText kind of frame
    // - is raw binary for focBinary or any other frames 
    payload: RawByteString;
  end;

  {$M+}
  TWebSocketServerResp = class;
  {$M-}

  /// handle an application-level WebSockets protocol
  // - shared by TWebSocketServer and TWebSocketClient classes
  // - once upgraded to WebSockets, a HTTP link could be used e.g. to transmit our
  // proprietary 'synopsejson' or 'synopsebinary' application content, as stated
  // by this typical handshake:
  // $ GET /myservice HTTP/1.1
  // $ Host: server.example.com
  // $ Upgrade: websocket
  // $ Connection: Upgrade
  // $ Sec-WebSocket-Key: x3JJHMbDL1EzLkh9GBhXDw==
  // $ Sec-WebSocket-Protocol: synopsejson
  // $ Sec-WebSocket-Version: 13
  // $ Origin: http://example.com
  // $
  // $ HTTP/1.1 101 Switching Protocols
  // $ Upgrade: websocket
  // $ Connection: Upgrade
  // $ Sec-WebSocket-Accept: HSmrc0sMlYUkAGmm5OPpG2HaGWk=
  // $ Sec-WebSocket-Protocol: synopsejson
  // - the TWebSocketProtocolJSON inherited class will implement
  // $ Sec-WebSocket-Protocol: synopsejson
  // - the TWebSocketProtocolBinary inherited class will implement
  // $ Sec-WebSocket-Protocol: synopsebinary
  TWebSocketProtocol = class(TSynPersistent)
  protected
    fName: RawUTF8;
    fURI: RawUTF8;
    function ProcessFrame(Context: TWebSocketServerResp;
      var request,answer: TWebSocketFrame): boolean; virtual; abstract;
    function Clone: TWebSocketProtocol; virtual; abstract;
  public
    /// abstract constructor to initialize the protocol
    // - the protocol should be named, so that the client may be able to request
    // for a given protocol
    // - if aURI is '', any URI would potentially upgrade to this protocol; you can
    // specify an URI to limit the protocol upgrade to a single resource
    constructor Create(const aName,aURI: RawUTF8); reintroduce;
  published
    /// the Sec-WebSocket-Protocol application name currently involved
    // - is currently 'synopsejson' or 'synopsebinary'
    property Name: RawUTF8 read fName;
    /// the optional URI on which this protocol would be enabled
    // - leave to '' if any URI should match
    property URI: RawUTF8 read fURI;
  end;

  /// callback event triggered by TWebSocketProtocolChat for any incoming message
  // - a first call with frame.opcode=focContinuation will take place when
  // the connection will be upgrade to WebSockets
  // - then any incoming focText/focBinary events will trigger this callback
  // - eventually, a focConnectionClose will notify the connection ending 
  TOnWebSocketProtocolChatIncomingFrame =
    procedure(Sender: TWebSocketServerResp; const Frame: TWebSocketFrame) of object;

  /// simple chatting protocol, allowing to receive and send WebSocket frames
  // - you can use this protocol to implement simple asynchronous communication
  // with events expecting no answers, e.g. with AJAX applications
  // - see TWebSocketProtocolRest for bi-directional events expecting answers
  TWebSocketProtocolChat = class(TWebSocketProtocol)
  protected
    fOnIncomingFrame: TOnWebSocketProtocolChatIncomingFrame;
    function ProcessFrame(Context: TWebSocketServerResp;
      var request,answer: TWebSocketFrame): boolean; override;
    function Clone: TWebSocketProtocol; override;
  public
    /// allows to send a message over the wire to a specified connection
    function SendFrame(Sender: TWebSocketServerResp; const Frame: TWebSocketFrame): boolean;
    /// you can assign an event to this property to be notified of incoming messages
    property OnIncomingFrame: TOnWebSocketProtocolChatIncomingFrame
      read fOnIncomingFrame write fOnIncomingFrame;
  end;

  /// handle a REST application-level bi-directional WebSockets protocol
  // - will emulate a bi-directional REST process, using THttpServerRequest to
  // store and handle the request parameters: clients would be able to send
  // regular REST requests to the server, but the server could use the same
  // communication channel to push REST requests to the client
  TWebSocketProtocolRest = class(TWebSocketProtocol)
  protected
    function ProcessFrame(Context: TWebSocketServerResp;
      var request,answer: TWebSocketFrame): boolean; override;
    procedure FrameCompress(const Values: array of RawByteString;
      const Content,ContentType: RawByteString; var frame: TWebSocketFrame); virtual; abstract;
    function FrameDecompress(const frame: TWebSocketFrame; const Head: RawUTF8;
      const values: array of PRawByteString; var contentType,content: RawByteString): Boolean; virtual; abstract;
    /// convert the input information of REST request to a WebSocket frame
    procedure InputToFrame(Ctxt: THttpServerRequest; out request: TWebSocketFrame); virtual;
    /// convert a WebSocket frame to the output information of a REST request
    function FrameToOutput(var answer: TWebSocketFrame; Ctxt: THttpServerRequest): cardinal; virtual;
    /// convert the output information of REST request to a WebSocket frame
    procedure OutputToFrame(Ctxt: THttpServerRequest; Status: Cardinal;
      out answer: TWebSocketFrame); virtual;
    /// convert a WebSocket frame to the input information of a REST request
    function FrameToInput(var request: TWebSocketFrame; Ctxt: THttpServerRequest): boolean; virtual;
  end;

  /// used to store the class of a TWebSocketProtocol type
  TWebSocketProtocolClass = class of TWebSocketProtocol;

  /// handle a REST application-level WebSockets protocol using JSON for transmission
  // - could be used e.g. for AJAX or non Delphi remote access
  // - this class will implement then following application-level protocol:
  // $ Sec-WebSocket-Protocol: synopsejson
  TWebSocketProtocolJSON = class(TWebSocketProtocolRest)
  protected
    procedure FrameCompress(const Values: array of RawByteString;
      const Content,ContentType: RawByteString; var frame: TWebSocketFrame); override;
    function FrameDecompress(const frame: TWebSocketFrame; const Head: RawUTF8;
      const values: array of PRawByteString; var contentType,content: RawByteString): Boolean; override;
    function Clone: TWebSocketProtocol; override;
  public
    /// initialize the WebSockets JSON protocol
    // - if aURI is '', any URI would potentially upgrade to this protocol; you can
    // specify an URI to limit the protocol upgrade to a single resource
    constructor Create(const aURI: RawUTF8); reintroduce;
  end;

  /// handle a REST application-level WebSockets protocol using compressed and
  // optionally AES-CFB encrypted binary
  // - this class will implement then following application-level protocol:
  // $ Sec-WebSocket-Protocol: synopsebinary
  TWebSocketProtocolBinary = class(TWebSocketProtocolRest)
  protected
    fEncryption: TAESAbstract;
    fCompressed: boolean;
    procedure FrameCompress(const Values: array of RawByteString;
      const Content,ContentType: RawByteString; var frame: TWebSocketFrame); override;
    function FrameDecompress(const frame: TWebSocketFrame; const Head: RawUTF8;
      const values: array of PRawByteString; var contentType,content: RawByteString): Boolean; override;
    function Clone: TWebSocketProtocol; override;
  public
    /// finalize the encryption, if was used
    destructor Destroy; override;
    /// initialize the WebSockets binary protocol
    // - if aURI is '', any URI would potentially upgrade to this protocol; you can
    // specify an URI to limit the protocol upgrade to a single resource
    // - if aKeySize if 128, 192 or 256, AES-CFB encryption will be used on on this protocol
    constructor Create(const aURI: RawUTF8; const aKey; aKeySize: cardinal;
      aCompressed: boolean=true); reintroduce; overload;
    /// initialize the WebSockets binary protocol
    // - if aURI is '', any URI would potentially upgrade to this protocol; you can
    // specify an URI to limit the protocol upgrade to a single resource
    /// - AES-CFB 256 bit encryption will be enabled on this protocol if some
    // strings are supplied
    // - the supplied key and initialization vector will be hashed using SHA-256
    constructor Create(const aURI, aKey: RawUTF8; aCompressed: boolean=true); reintroduce; overload;
    /// defines if SynLZ compression is enabled during the transmission
    // - is set to TRUE by default
    property Compressed: boolean read fCompressed write fCompressed;
  end;

  /// used to maintain a list of websocket protocols
  TWebSocketProtocolList = class
  protected
    fProtocols: array of TWebSocketProtocol;
    function FindIndex(const aName,aURI: RawUTF8): integer;
  public
    /// add a protocol to the internal list
    function Add(aProtocol: TWebSocketProtocol): boolean;
    /// erase a protocol from the internal list, specified by its name
    function Remove(const aProtocolName,aURI: RawUTF8): boolean;
    /// finalize the list storage
    destructor Destroy; override;
    /// create a new protocol instance, from the internal list
    function CloneByName(const aProtocolName, aURI: RawUTF8): TWebSocketProtocol;
    /// create a new protocol instance, from the internal list
    function CloneByURI(const aURI: RawUTF8): TWebSocketProtocol;
    /// how many protocols are stored
    function Count: integer;
  end;

  TWebSocketServerRespProcessOne = (wspNone, wspPing, wspDone, wspError, wspClosed);

  /// an enhanced input/output structure used for HTTP and WebSockets requests
  // - this class will contain additional parameters used to maintain the
  // WebSockets execution context in overriden TWebSocketServer.Process method
  TWebSocketServerResp = class(THttpServerResp)
  protected
    fAcquireConnectionLock: TRTLCriticalSection;
    fTryAcquireConnectionCount: Integer;
    fWebSocketProtocol: TWebSocketProtocol;
    fLastPingTicks: Int64;
    /// low level WebSockets framing protocol
    function GetFrame(out Frame: TWebSocketFrame; TimeOut: cardinal): boolean;
    function SendFrame(const Frame: TWebSocketFrame): boolean;
    /// try to reserve the connection for a given process in the current thread
    // - if TRUE, should use finally LeaveCriticalSection(fAcquireConnectionLock)
    function TryAcquireConnection(TimeOutMS: cardinal): boolean;
    /// methods run by TWebSocketServerRest.WebSocketProcessLoop
    procedure ProcessStart; virtual;
    procedure ProcessStop; virtual;
    function ProcessOne: TWebSocketServerRespProcessOne; virtual;
    // OnCallback property will point to this method after upgrade to WebSockets
    function NotifyCallback(Ctxt: THttpServerRequest): cardinal; virtual;
  public
    /// initialize the context, associated to a HTTP/WebSockets server instance
    constructor Create(aServerSock: THttpServerSocket; aServer: THttpServer
       {$ifdef USETHREADPOOL}; aThreadPool: TSynThreadPoolTHttpServer{$endif}); override;
    /// finalize the context
    destructor Destroy; override;
  published
    /// the Sec-WebSocket-Protocol application protocol currently involved
    // - TWebSocketProtocolJSON or TWebSocketProtocolBinary in the mORMot context
    // - could be nil if the connection is in standard HTTP/1.1 mode
    property WebSocketProtocol: TWebSocketProtocol read fWebSocketProtocol;
  end;


{ -------------- WebSockets Server classes for bidirectional remote access }

type
  /// main HTTP/WebSockets server Thread using the standard Sockets API (e.g. WinSock)
  // - once upgraded to WebSockets from the client, this class is able to serve
  // any Sec-WebSocket-Protocol application content
  TWebSocketServer = class(THttpServer)
  protected
    fConnections: TObjectList;
    fProtocols: TWebSocketProtocolList;
    fHeartbeatDelay: Cardinal;
    /// will validate the WebSockets handshake, then call WebSocketProcessLoop()
    function WebSocketProcessUpgrade(ClientSock: THttpServerSocket;
      Context: TWebSocketServerResp): boolean; virtual;
    /// this is the main execution loop in WebSockets mode
    function WebSocketProcessLoop(ClientSock: THttpServerSocket;
      Context: TWebSocketServerResp): boolean; virtual;
    /// overriden method which will recognize the WebSocket protocol handshake,
    // then run the whole bidirectional communication in its calling thread
    // - here aCallingThread is a THttpServerResp, and ClientSock.Headers
    // and ConnectionUpgrade properties should be checked for the handshake
    procedure Process(ClientSock: THttpServerSocket;
      aCallingThread: TNotifiedThread); override;
    /// identifies an incoming THttpServerResp as a valid TWebSocketServerResp
    function IsActiveWebSocket(CallingThread: TNotifiedThread): TWebSocketServerResp; virtual;
  public
    /// create a Server Thread, binded and listening on a port
    // - this constructor will raise a EHttpServer exception if binding failed
    // - due to the way how WebSockets works, one thread will be created
    // for any incoming connection
    // - note that this constructor will not register any protocol, so is
    // useless until you execute Protocols.Add()
    constructor Create(const aPort: SockString); reintroduce; overload; virtual;
    /// close the server
    destructor Destroy; override;
    /// access to the protocol list handled by this server
    property Protocols: TWebSocketProtocolList read fProtocols;
    /// time in milli seconds between each focPing commands sent by the server
    // - default is 20000, i.e. 20 seconds
    property HeartbeatDelay: Cardinal read fHeartbeatDelay write fHeartbeatDelay;
  end;

  /// main HTTP/WebSockets server Thread using the standard Sockets API (e.g. WinSock)
  // - once upgraded to WebSockets from the client, this class is able to serve
  // our proprietary Sec-WebSocket-Protocol: 'synopsejson' or 'synopsebinary'
  // application content, managing regular REST client-side requests and
  // also server-side push notifications
  // - once in 'synopse*' mode, the Request() method will be trigerred from
  // any incoming REST request from the client, and the OnCallback event
  // will be available to push a request from the server to the client
  TWebSocketServerRest = class(TWebSocketServer)
  protected
    fCallbackAcquireTimeOutMS: cardinal;
    fCallbackAnswerTimeOutMS: cardinal;
  public
    /// create a Server Thread, binded and listening on a port, with no
    // default WebSockets protocol
    // - you should call manually Protocols.Add() to register the expected protocols
    constructor Create(const aPort: SockString); override;
    /// create a Server Thread, binded and listening on a port, with our
    // 'synopsebinary' and optionally 'synopsejson' modes
    // - if aWebSocketsURI is '', any URI would potentially upgrade; you can
    // specify an URI to limit the protocol upgrade to a single resource
    // - TWebSocketProtocolBinary will always be registered by this constructor
    // - if the encryption key text is not '', TWebSocketProtocolBinary will
    // use AES-CFB 256 bits encryption
    // - if aWebSocketsAJAX is TRUE, it will also register TWebSocketProtocolJSON
    // so that AJAX applications would be able to connect to this server
    constructor Create(const aPort: SockString;
      const aWebSocketsURI, aWebSocketsEncryptionKey: RawUTF8;
      aWebSocketsAJAX: boolean=false); reintroduce; overload;
    /// server can send a request back to the client, when the connection has
    // been upgraded to WebSocket
    // - InURL/InMethod/InContent properties are input parameters (InContentType
    // is ignored)
    // - OutContent/OutContentType/OutCustomHeader are output parameters
    // - CallingThread should be set to the client's Ctxt.CallingThread
    // value, so that the method could know which connnection is to be used -
    // it will return STATUS_NOTFOUND (404) if the connection is unknown
    // - result of the function is the HTTP error code (200 if OK, e.g.)
    function WebSocketsCallback(Ctxt: THttpServerRequest): cardinal; virtual;
  published
    /// how many milliseconds the callback notification should wait acquiring
    // the connection before failing, in WebSockets mode
    // - defaut is 5000, i.e. 5 seconds
    property CallbackAcquireTimeOutMS: cardinal
      read fCallbackAcquireTimeOutMS write fCallbackAcquireTimeOutMS;
    /// how many milliseconds the callback notification should wait for the
    // client to return its answer, in WebSockets mode
    // - defaut is 1000, i.e. 1 second
    property CallbackAnswerTimeOutMS: cardinal
      read fCallbackAnswerTimeOutMS write fCallbackAnswerTimeOutMS;
  end;


/// used to return the text corresponding to a specified WebSockets frame data
function OpcodeText(opcode: TWebSocketFrameOpCode): PShortString;


{ -------------- WebSockets Client classes for bidirectional remote access }



implementation


{ -------------- WebSockets shared classes for bidirectional remote access }


function OpcodeText(opcode: TWebSocketFrameOpCode): PShortString;
begin
  result := GetEnumName(TypeInfo(TWebSocketFrameOpCode),ord(opcode));
end;

{ TWebSocketProtocol }

constructor TWebSocketProtocol.Create(const aName,aURI: RawUTF8);
begin
  fName := aName;
  fURI := aURI;
end;


{ TWebSocketProtocolChat }

function TWebSocketProtocolChat.Clone: TWebSocketProtocol;
begin
  result := TWebSocketProtocolChat.Create(fName,fURI);
  TWebSocketProtocolChat(result).OnIncomingFrame := OnIncomingFrame;
end;

function TWebSocketProtocolChat.ProcessFrame(
  Context: TWebSocketServerResp; var request, answer: TWebSocketFrame): boolean;
begin
  if Assigned(OnInComingFrame) then
    OnIncomingFrame(Context,request);
  result := false; // no answer frame to be transmitted 
end;

function TWebSocketProtocolChat.SendFrame(Sender: TWebSocketServerResp;
  const frame: TWebSocketFrame): boolean;
begin
  result := false;
  if (self=nil) or (Sender=nil) or Sender.Terminated or
     not (Frame.opcode in [focText,focBinary]) then
    exit;
  if (Sender.fServer as TWebSocketServer).IsActiveWebSocket(Sender)=Sender then
    result := Sender.SendFrame(frame)
end;


{ TWebSocketProtocolRest }

function TWebSocketProtocolRest.ProcessFrame(Context: TWebSocketServerResp;
  var request,answer: TWebSocketFrame): boolean;
var Ctxt: THttpServerRequest;
    status: cardinal;
begin
  result := true;
  Ctxt := THttpServerRequest.Create(Context.fServer,Context);
  try
    if not FrameToInput(request,Ctxt) then
      raise ESynBidirSocket.CreateUTF8('%.ProcessOne: unexpected frame',[self]);
    status := Context.fServer.Request(Ctxt);
    if Ctxt.OutContentType=HTTP_RESP_NORESPONSE then // expects no answer
      result := false else
      OutputToFrame(Ctxt,status,answer);
  finally
    Ctxt.Free;
  end;
end;

procedure TWebSocketProtocolRest.InputToFrame(Ctxt: THttpServerRequest;
  out request: TWebSocketFrame);
begin
  FrameCompress(['request',Ctxt.Method,Ctxt.URL,Ctxt.InHeaders],
    Ctxt.InContent,Ctxt.InContentType,request);
end;

function TWebSocketProtocolRest.FrameToInput(var request: TWebSocketFrame;
  Ctxt: THttpServerRequest): boolean;
var URL,Method,InHeaders,InContentType,InContent: RawByteString;
begin
  result := FrameDecompress(request,'request',
    [@Method,@URL,@InHeaders],InContentType,InContent);
  if result then
    Ctxt.Prepare(URL,Method,InHeaders,InContent,InContentType);
end;

procedure TWebSocketProtocolRest.OutputToFrame(Ctxt: THttpServerRequest;
  Status: Cardinal; out answer: TWebSocketFrame);
begin
  FrameCompress(['answer',UInt32ToUTF8(Status),Ctxt.OutCustomHeaders],
    Ctxt.OutContent,Ctxt.OutContentType,answer);
end;

function TWebSocketProtocolRest.FrameToOutput(
  var answer: TWebSocketFrame; Ctxt: THttpServerRequest): cardinal;
var status,outHeaders,outContentType,outContent: RawByteString;
begin
  result := STATUS_NOTFOUND;
  if not FrameDecompress(answer,'ANSWER',
      [@status,@outHeaders],outContentType,outContent) then
    exit;
  result := GetInteger(pointer(status));
  Ctxt.OutCustomHeaders := outHeaders;
  Ctxt.OutContentType := outContentType;
  Ctxt.OutContent := outContent;
end;


{ TWebSocketProtocolJSON }

constructor TWebSocketProtocolJSON.Create(const aURI: RawUTF8);
begin
  inherited Create('synopsejson',aURI);
end;

function TWebSocketProtocolJSON.Clone: TWebSocketProtocol;
begin
  result := TWebSocketProtocolJSON.Create(fURI);
end;

procedure TWebSocketProtocolJSON.FrameCompress(const Values: array of RawByteString;
  const Content, ContentType: RawByteString; var frame: TWebSocketFrame);
var WR: TTextWriter;
    i: integer;
begin
  frame.opcode := focText;
  WR := TTextWriter.CreateOwnedStream;
  try
    WR.Add('{');
    WR.AddFieldName(Values[0]);
    WR.Add('[');
    for i := 1 to High(Values) do begin
      WR.Add('"');
      WR.AddString(Values[i]);
      WR.Add('"',',');
    end;
    WR.Add('"');
    WR.AddString(ContentType);
    WR.Add('"',',');
    if Content='' then
      WR.Add('"','"') else
    if (ContentType='') or
       IdemPropNameU(ContentType,JSON_CONTENT_TYPE) then
      WR.AddNoJSONEscape(pointer(Content)) else
    if IdemPChar(pointer(ContentType),'TEXT/') then
      WR.AddCSVUTF8([Content]) else
      WR.WrBase64(pointer(Content),length(Content),true);
    WR.Add(']','}');
    WR.SetText(RawUTF8(frame.payload));
  finally
    WR.Free;
  end;
end;

function TWebSocketProtocolJSON.FrameDecompress(
  const frame: TWebSocketFrame; const Head: RawUTF8;
  const values: array of PRawByteString; var contentType,
  content: RawByteString): Boolean;
var i: Integer;
    P,txt: PUTF8Char;
begin
  result := false;
  if (length(frame.payload)<10) or (frame.opcode<>focText) then
    exit;
  P := pointer(frame.payload);
  P := GotoNextNotSpace(P);
  if P^<>'{' then
    exit;
  inc(P);
  if not IdemPropNameU(GetJSONPropName(P),Head) then
    exit;
  P := GotoNextNotSpace(P);
  if P^<>'[' then
    exit;
  inc(P);
  for i := 0 to high(values) do
    values[i]^ := GetJSONField(P,P);
  contentType := GetJSONField(P,P);
  if P=nil then
    exit;
  if (contentType='') or
     IdemPropNameU(contentType,JSON_CONTENT_TYPE) then
    content := GetJSONItemAsRawJSON(P) else begin
    txt := GetJSONField(P,P);
    if IdemPChar(pointer(contentType),'TEXT/') then
      content := txt else
    if not Base64MagicCheckAndDecode(txt,content) then
      exit;
  end;
  result := true;
end;


{ TWebSocketProtocolBinary }

constructor TWebSocketProtocolBinary.Create(const aURI: RawUTF8;
  const aKey; aKeySize: cardinal; aCompressed: boolean);
begin
  inherited Create('synopsebinary',aURI);
  if aKeySize>=128 then
    fEncryption := TAESCFB.Create(aKey,aKeySize,PAESBlock(@aKey)^);
  fCompressed := aCompressed;
end;

constructor TWebSocketProtocolBinary.Create(const aURI, aKey: RawUTF8;
  aCompressed: boolean);
var key: TSHA256Digest;
    keySize: integer;
begin
  if aKey<>'' then begin
    SHA256Weak(aKey,key);
    keySize := 256;
  end else
    keySize := 0;
  Create(aURI,key,keySize,aCompressed);
end;

function TWebSocketProtocolBinary.Clone: TWebSocketProtocol;
begin
  result := TWebSocketProtocolBinary.Create(fURI,'',fCompressed);
  if fEncryption<>nil then
    TWebSocketProtocolBinary(result).fEncryption := fEncryption.Clone;
end;

destructor TWebSocketProtocolBinary.Destroy;
begin
  FreeAndNil(fEncryption);
  inherited;
end;

procedure TWebSocketProtocolBinary.FrameCompress(const Values: array of RawByteString;
  const Content, ContentType: RawByteString; var frame: TWebSocketFrame);
var tmp,value: RawByteString;
    i: integer;
begin
  frame.opcode := focBinary;
  for i := 1 to high(Values) do
    tmp := tmp+Values[i]+#1;
  tmp := tmp+ContentType+#1+Content;
  if fCompressed then
    SynLZCompress(pointer(tmp),length(tmp),value,512) else
    value := tmp;
  if fEncryption<>nil then
    value := fEncryption.EncryptPKCS7(value,true);
  frame.payload := Values[0]+#1+value;
end;

function TWebSocketProtocolBinary.FrameDecompress(
  const frame: TWebSocketFrame; const Head: RawUTF8;
  const values: array of PRawByteString; var contentType,content: RawByteString): Boolean;
var tmp,hd,value: RawByteString;
    i: integer;
    P: PUTF8Char;
begin
  result := false;
  split(frame.payload,#1,RawUTF8(hd),RawUTF8(tmp));
  if (frame.opcode<>focBinary) or
     (length(tmp)<5) or
     not IdemPropNameU(hd,Head) then
    exit;
  if fEncryption<>nil then
    tmp := fEncryption.DecryptPKCS7(tmp,true);
  if fCompressed then
    SynLZDecompress(pointer(tmp),length(tmp),value) else
    value := tmp;
  if length(value)<4 then
    exit;
  P := pointer(value);
  for i := 0 to high(values) do
    values[i]^ := GetNextItem(P,#1);
  contentType := GetNextItem(P,#1);
  if P<>nil then
    SetString(content,P,length(value)-(P-pointer(value)));
  result := true;
end;


{ TWebSocketProtocolList }

function TWebSocketProtocolList.CloneByName(const aProtocolName, aURI: RawUTF8): TWebSocketProtocol;
var i: Integer;
begin
  i := FindIndex(aProtocolName,aURI);
  if i<0 then
    result := nil else
    result := fProtocols[i].Clone;
end;

function TWebSocketProtocolList.CloneByURI(
  const aURI: RawUTF8): TWebSocketProtocol;
var i: integer;
begin
  result := nil;
  if self<>nil then
    for i := 0 to length(fProtocols)-1 do
      if IdemPropNameU(fProtocols[i].fURI,aURI) then begin
        result := fProtocols[i].Clone;
        exit;
      end;
end;

function TWebSocketProtocolList.Count: integer;
begin
  if self=nil then
    result := 0 else
    result := length(fProtocols);
end;

destructor TWebSocketProtocolList.Destroy;
begin
  ObjArrayClear(fProtocols);
  inherited;
end;

function TWebSocketProtocolList.FindIndex(const aName,aURI: RawUTF8): integer;
begin
  if aName<>'' then
    for result := 0 to high(fProtocols) do
      with fProtocols[result] do
      if IdemPropNameU(fName,aName) and
         ((fURI='') or IdemPropNameU(fURI,aURI)) then
        exit;
  result := -1;
end;

function TWebSocketProtocolList.Add(aProtocol: TWebSocketProtocol): boolean;
var i: Integer;
begin
  result := false;
  if aProtocol=nil then
    exit;
  i := FindIndex(aProtocol.Name,aProtocol.URI);
  if i<0 then begin
    ObjArrayAdd(fProtocols,aProtocol);
    result := true;
  end;
end;

function TWebSocketProtocolList.Remove(const aProtocolName,aURI: RawUTF8): Boolean;
var i: Integer;
begin
  i := FindIndex(aProtocolName,aURI);
  if i>=0 then begin
    ObjArrayDelete(fProtocols,i);
    result := true;
  end else
    result := false;
end;


{ -------------- WebSockets Server classes for bidirectional remote access }

{ TWebSocketServer }

constructor TWebSocketServer.Create(const aPort: SockString);
begin
  inherited Create(aPort{$ifdef USETHREADPOOL},0{$endif}); // no thread pool
  fThreadRespClass := TWebSocketServerResp;
  fConnections := TObjectList.Create(false);
  fProtocols := TWebSocketProtocolList.Create;
  fHeartbeatDelay := 20000;
end;

function TWebSocketServer.WebSocketProcessUpgrade(ClientSock: THttpServerSocket;
  Context: TWebSocketServerResp): boolean;
var upgrade,uri,version,protocol,prot,key,hash: RawUTF8;
    P: PUTF8Char;
    SHA: TSHA1;
    Digest: TSHA1Digest;
begin
  result := false; // quiting now will process it like a regular GET HTTP request
  if Context.fWebSocketProtocol<>nil then
    exit; // already upgraded
  upgrade := ClientSock.HeaderValue('Upgrade');
  if not IdemPropNameU(upgrade,'websocket') then
    exit;
  version := ClientSock.HeaderValue('Sec-WebSocket-Version');
  if GetInteger(pointer(version))<13 then
    exit; // we expect WebSockets protocol version 13 at least
  uri := Trim(RawUTF8(ClientSock.URL));
  protocol := ClientSock.HeaderValue('Sec-WebSocket-Protocol');
  if protocol<>'' then begin
    P := pointer(protocol);
    repeat
      prot := trim(GetNextItem(P));
      Context.fWebSocketProtocol := fProtocols.CloneByName(prot,uri);
    until (P=nil) or (Context.fWebSocketProtocol<>nil);
  end else
    // if no protocol is specified, try to match by URI
    Context.fWebSocketProtocol := fProtocols.CloneByURI(uri);
  if Context.fWebSocketProtocol=nil then
    exit;
  key := ClientSock.HeaderValue('Sec-WebSocket-Key');
  if Base64ToBinLength(pointer(key),length(key))<>16 then
    exit; // this nonce must be a Base64-encoded value of 16 bytes
  key := key+'258EAFA5-E914-47DA-95CA-C5AB0DC85B11';
  SHA.Full(pointer(key),length(key),Digest);
  hash := 'HTTP/1.1 101 Switching Protocols'#13#10+
    'Upgrade: websocket'#13#10'Connection: Upgrade'#13#10+
    'Sec-WebSocket-Accept: '+BinToBase64(@Digest,sizeof(Digest))+#13#10;
  if protocol<>'' then
    // note: we expect only a single protocol requested by the client now
    hash := hash+'Sec-WebSocket-Protocol: '+protocol+#13#10#13#10 else
    hash := hash+#13#10;
  ClientSock.SndLow(pointer(hash),length(hash));
  EnterCriticalSection(fProcessCS);
  fConnections.Add(Context);
  LeaveCriticalSection(fProcessCS);
  try
    result := WebSocketProcessLoop(ClientSock,Context);
  finally
    FreeAndNil(Context.fWebSocketProtocol); // notify end of WebSockets
    EnterCriticalSection(fProcessCS);
    fConnections.Delete(fConnections.IndexOf(Context));
    LeaveCriticalSection(fProcessCS);
  end;
end;

procedure TWebSocketServer.Process(ClientSock: THttpServerSocket;
  aCallingThread: TNotifiedThread);
begin
  if ClientSock.ConnectionUpgrade and
     ClientSock.KeepAliveClient and
     IdemPropName(ClientSock.Method,'GET') and
     aCallingThread.InheritsFrom(TWebSocketServerResp) then
    WebSocketProcessUpgrade(ClientSock,TWebSocketServerResp(aCallingThread)) else
    inherited Process(ClientSock,aCallingThread);
end;

destructor TWebSocketServer.Destroy;
begin
  inherited Destroy; // close any pending connection
  fConnections.Free;
  fProtocols.Free;
end;

function TWebSocketServer.WebSocketProcessLoop(ClientSock: THttpServerSocket;
  Context: TWebSocketServerResp): boolean;
var delay: cardinal;
    last: Int64;
begin
  result := false;
  if Context.fWebSocketProtocol=nil then
    exit;
  Context.ProcessStart;
  last := GetTickCount64;
  while Context.ServerSock.KeepAliveClient and
        not (Terminated or Context.Terminated) do
    case Context.ProcessOne of
    wspNone: begin
      case GetTickCount64-last of
      0..200:    delay := 1; // leave some time for Context.NotifyCallback()
      201..500:  delay := 5;
      501..2000: delay := 100;
      else       delay := 500;
      end;
      SleepHiRes(delay);
    end;
    wspPing:
      SleepHiRes(1);
    wspDone: begin
      SleepHiRes(0);
      last := GetTickCount64;
    end;
    wspError:
      SleepHiRes(10);
    wspClosed:
      Context.ServerSock.KeepAliveClient := false; // will close connection
    end;
  Context.ProcessStop;
  result := true;
end;

function TWebSocketServer.IsActiveWebSocket(
  CallingThread: TNotifiedThread): TWebSocketServerResp;
var connectionIndex: Integer;
begin
  result := nil;
  if Terminated or (CallingThread=nil) then
    exit;
  EnterCriticalSection(fProcessCS);
  connectionIndex := fConnections.IndexOf(CallingThread);
  LeaveCriticalSection(fProcessCS);
  if (connectionIndex>=0) and
     CallingThread.InheritsFrom(TWebSocketServerResp) then
    //  this request is a websocket, on a non broken connection
    result := TWebSocketServerResp(CallingThread);
end;


{ TWebSocketServerRest }

constructor TWebSocketServerRest.Create(const aPort: SockString;
  const aWebSocketsURI,aWebSocketsEncryptionKey: RawUTF8; aWebSocketsAJAX: boolean);
begin
  Create(aPort);
  fProtocols.Add(TWebSocketProtocolBinary.Create(aWebSocketsURI,aWebSocketsEncryptionKey));
  if aWebSocketsAJAX then
    fProtocols.Add(TWebSocketProtocolJSON.Create(aWebSocketsURI));
end;

constructor TWebSocketServerRest.Create(const aPort: SockString);
begin
  inherited Create(aPort);
  fCallbackAcquireTimeOutMS := 5000;
  fCallbackAnswerTimeOutMS := 1000;
end;

function TWebSocketServerRest.WebSocketsCallback(
  Ctxt: THttpServerRequest): cardinal;
var connection: TWebSocketServerResp;
begin
  if Ctxt=nil then
    connection := nil else
    connection := IsActiveWebSocket(Ctxt.CallingThread);
  if connection<>nil then
    //  this request is a websocket, on a non broken connection
    result := connection.NotifyCallback(Ctxt) else
    result := STATUS_NOTFOUND;
end;


{ TWebSocketServerResp }

constructor TWebSocketServerResp.Create(aServerSock: THttpServerSocket;
  aServer: THttpServer {$ifdef USETHREADPOOL}; aThreadPool: TSynThreadPoolTHttpServer{$endif});
begin
  if not aServer.InheritsFrom(TWebSocketServer) then
    raise ESynBidirSocket.CreateUTF8('%.Create(%: TWebSocketServer?)',[self,aServer]);
  InitializeCriticalSection(fAcquireConnectionLock);
  inherited Create(aServerSock,aServer{$ifdef USETHREADPOOL},aThreadPool{$endif});
end;

destructor TWebSocketServerResp.Destroy;
begin
  inherited Destroy;
  while fTryAcquireConnectionCount>0 do
    SleepHiRes(2);
  DeleteCriticalSection(fAcquireConnectionLock);
  fWebSocketProtocol.Free;
end;

procedure TWebSocketServerResp.ProcessStart;
var dummy: TWebSocketFrame;
begin
  dummy.opcode := focContinuation;
  WebSocketProtocol.ProcessFrame(self,dummy,dummy);
  fLastPingTicks := GetTickCount64;
end;

procedure TWebSocketServerResp.ProcessStop;
var dummy: TWebSocketFrame;
begin
  dummy.opcode := focConnectionClose;
  WebSocketProtocol.ProcessFrame(self,dummy,dummy);
end;

function TWebSocketServerResp.ProcessOne: TWebSocketServerRespProcessOne;
var request,answer: TWebSocketFrame;
    sendAnswer: boolean;
begin
  result := wspNone;
  if TryAcquireConnection(5) then
  try
    try // current implementation is blocking vs NotifyCallback
      if not GetFrame(request,5) then begin
        if GetTickCount64-fLastPingTicks>TWebSocketServer(fServer).fHeartbeatDelay then begin
          answer.opcode := focPing;
          SendFrame(answer);
          result := wspPing;
        end;
        exit;
      end;
      result := wspDone;
      sendAnswer := true;
      fLastPingTicks := GetTickCount64;
      case request.opcode of
      focPing: begin
        answer.opcode := focPong;
        answer.payload := request.payload;
        result := wspPing;
      end;
      focPong: begin
        result := wspPing;
        exit;
      end;
      focText,focBinary:
        sendAnswer := WebSocketProtocol.ProcessFrame(self,request,answer);
      focConnectionClose: begin
        answer := request;
        result := wspClosed;
      end;
      else exit; // reserved commands are just ignored
      end;
      if sendAnswer then
        SendFrame(answer);
    finally
      LeaveCriticalSection(fAcquireConnectionLock);
    end;
  except
    result := wspError;
  end;
end;

function TWebSocketServerResp.NotifyCallback(Ctxt: THttpServerRequest): cardinal;
var request,answer: TWebSocketFrame;
begin
  result := STATUS_NOTFOUND;
  if (fWebSocketProtocol=nil) or
     not fWebSocketProtocol.InheritsFrom(TWebSocketProtocolRest) or
     not fServer.InheritsFrom(TWebSocketServerRest) then
    exit;
  if TryAcquireConnection(TWebSocketServerRest(fServer).CallbackAcquireTimeOutMS) then
  try 
    repeat // first handle any incoming REST request
      case ProcessOne of
      wspNone:
        break;
      wspDone,wspPing:
        continue;
      wspError:
        exit;
      wspClosed: begin // force WebSocketProcessLoop to close connection
        ServerSock.KeepAliveClient := false;
        exit;
      end;
      end;
    until false;
    // now we should be alone on the wire
    TWebSocketProtocolRest(fWebSocketProtocol).InputToFrame(Ctxt,request);
    if not SendFrame(request) or
       not GetFrame(answer,TWebSocketServerRest(fServer).CallbackAnswerTimeOutMS) then
      exit;
    fLastPingTicks := GetTickCount64;
  finally
    LeaveCriticalSection(fAcquireConnectionLock);
  end;
  result := TWebSocketProtocolRest(fWebSocketProtocol).FrameToOutput(answer,Ctxt);
end;

const
  FRAME_FIN=128;
  FRAME_LEN2BYTES=126;
  FRAME_LEN8BYTES=127;

type
 TFrameHeader = packed record
   first: byte;
   len8: byte;
   len32: cardinal;
   len64: cardinal;
   mask: cardinal; // 0 indicates no payload masking
 end;

procedure ProcessMask(data: pointer; mask: cardinal; len: integer);
var i,maskCount: integer;
begin
  maskCount := len shr 2;
  for i := 0 to maskCount-1 do
    PCardinalArray(data)^[i] := PCardinalArray(data)^[i] xor mask;
  maskCount := maskCount*4;
  for i := maskCount to maskCount+(len and 3)-1 do begin
    PByteArray(data)^[i] := PByteArray(data)^[i] xor mask;
    mask := mask shr 8;
  end;
end;

function TWebSocketServerResp.GetFrame(
  out Frame: TWebSocketFrame; TimeOut: cardinal): boolean;
var hdr: TFrameHeader;
    opcode: TWebSocketFrameOpCode;
    masked: boolean;
procedure GetHeader;
begin
  fillchar(hdr,sizeof(hdr),0);
  fServerSock.SockInRead(@hdr.first,2,true);
  opcode := TWebSocketFrameOpCode(hdr.first and 15);
  masked := hdr.len8 and 128<>0;
  if masked then
    hdr.len8 := hdr.len8 and 127;
  if hdr.len8<FRAME_LEN2BYTES then
    hdr.len32 := hdr.len8 else
  if hdr.len8=FRAME_LEN2BYTES then begin
    fServerSock.SockInRead(@hdr.len32,2,true);
    hdr.len32 := swap(hdr.len32);
  end else
  if hdr.len8=FRAME_LEN8BYTES then begin
    fServerSock.SockInRead(@hdr.len32,8,true);
    if hdr.len32<>0 then // size is more than 32 bits -> reject
      hdr.len32 := maxInt else
      hdr.len32 := bswap32(hdr.len64);
    if hdr.len32>1 shl 28 then
      raise ESynBidirSocket.CreateUTF8('%.GetFrame: length should be < 256MB',[self]);
  end;
  if masked then
    fServerSock.SockInRead(@hdr.mask,4,true);
end;
procedure GetData(var data: RawByteString);
begin
  SetString(data,nil,hdr.len32);
  fServerSock.SockInRead(pointer(data),hdr.len32);
  if hdr.mask<>0 then
    ProcessMask(pointer(data),hdr.mask,hdr.len32);
end;
var data: RawByteString;
begin
  result := false;
  if fServerSock.SockInPending(TimeOut)<2 then
    exit; // no data available
  GetHeader;
  Frame.opcode := opcode;
  GetData(Frame.payload);
  while hdr.first and FRAME_FIN=0 do begin // handle partial payloads
    GetHeader;
    if (opcode<>focContinuation) and (opcode<>Frame.opcode) then
      raise ESynBidirSocket.CreateUTF8('%.GetFrame: received %, expected %',
        [self,OpcodeText(opcode)^,OpcodeText(Frame.opcode)^]);
    GetData(data);
    Frame.payload := Frame.payload+data;
  end;
  {$ifdef UNICODE}
  if opcode=focText then
    SetCodePage(Frame.payload,CP_UTF8,false); // identify text value as UTF-8
  {$endif}
  result := true;
end;

function TWebSocketServerResp.SendFrame(
  const Frame: TWebSocketFrame): boolean;
var hdr: TFrameHeader;
    len: cardinal;
begin
  try
    result := true;
    hdr.first := byte(Frame.opcode) or FRAME_FIN;
    len := Length(Frame.payload);
    if len<FRAME_LEN2BYTES then begin
      hdr.len8 := len;
      fServerSock.Snd(@hdr,2);
    end else
    if len<65536 then begin
      hdr.len8 := FRAME_LEN2BYTES;
      hdr.len32 := swap(len);
      fServerSock.Snd(@hdr,4);
    end else begin
      hdr.len8 := FRAME_LEN8BYTES;
      hdr.len64 := bswap32(len);
      hdr.len32 := 0;
      fServerSock.SndLow(@hdr,10); // huge payload sent outside TCrtSock buffers
      fServerSock.SndLow(pointer(Frame.payload),len);
      exit;
    end;
    fServerSock.Snd(pointer(Frame.payload),len);
    fServerSock.SockSendFlush; // send at once up to 64 KB
  except
    result := false;
  end;
end;

function TWebSocketServerResp.TryAcquireConnection(TimeOutMS: cardinal): boolean;
var timeoutTix: Int64;
    loopcount,delay: cardinal;
begin
  delay := 1;
  loopcount := 0;
  timeoutTix := GetTickCount64+TimeOutMS;
  InterlockedIncrement(fTryAcquireConnectionCount);
  try
    repeat
      result := boolean(TryEnterCriticalSection(fAcquireConnectionLock));
      if result or Terminated or TWebSocketServer(fServer).Terminated then
        exit;
      SleepHiRes(delay);
      inc(loopcount);
      if loopcount>5 then
        delay := 5;
    until Terminated or TWebSocketServer(fServer).Terminated or
          (TimeOutMS<=1) or (GetTickCount64>timeoutTix);
  finally
    InterlockedDecrement(fTryAcquireConnectionCount)
  end;
end;



end.