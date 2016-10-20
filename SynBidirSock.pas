/// implements bidirectional client and server protocol, e.g. WebSockets
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynBidirSock;

{
    This file is part of the Synopse framework.

    Synopse framework. Copyright (C) 2016 Arnaud Bouchez
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

  Portions created by the Initial Developer are Copyright (C) 2016
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Alfred (alf)


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


   TODO: broadcast a message to several aCallingThread: THttpServerResp values
     (add TWebSocketClientRequest? or asynch notifications may be enough, and
      the "broadcast" feature should be implementation at application level,
      using a list of callbacks)

   TODO: enhance TWebSocketServer process to use events, and not threads
   - current implementation has its threads spending most time waiting in loops
   - eventually also at SynCrtSock's THttpServer class level also


}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  SynWinSock,
  {$else}
  {$ifdef KYLIX3}
  LibC,
  Types,
  SynFPCSock, // shared with Kylix
  SynKylix,
  {$endif}
  {$ifdef FPC}
  SynFPCSock,
  SynFPCLinux,
  {$endif}
  {$endif}
  SysUtils,
  Classes,
  Contnrs,
  SyncObjs,
  SynLZ,
  SynCommons,
  SynLog,
  SynCrtSock,
  SynCrypto,
  SynEcc;

{$ifndef DELPHI5OROLDER}
  {$ifdef USELOCKERDEBUG}
    {.$define WSLOCKERDEBUGLIST}
    {.$define WSLOCKERDEBUGPROCESS}
  {$endif}
{$endif}

type
  /// Exception raised from this unit
  ESynBidirSocket = class(ESynException);


{ -------------- WebSockets shared classes for bidirectional remote access }

type
  /// defines the interpretation of the WebSockets frame data
  // - match order expected by the WebSockets RFC
  TWebSocketFrameOpCode = (
    focContinuation, focText, focBinary,
    focReserved3, focReserved4, focReserved5, focReserved6, focReserved7,
    focConnectionClose, focPing, focPong,
    focReservedB, focReservedC, focReservedD, focReservedE, focReservedF);

  /// set of WebSockets frame interpretation
  TWebSocketFrameOpCodes = set of TWebSocketFrameOpCode;

  /// define one attribute of a WebSockets frame data
  TWebSocketFramePayload = (
    fopAlreadyCompressed);
  /// define the attributes of a WebSockets frame data
  TWebSocketFramePayloads = set of TWebSocketFramePayload;

  /// stores a WebSockets frame
  // - see @http://tools.ietf.org/html/rfc6455 for reference
  TWebSocketFrame = record
    /// the interpretation of the frame data
    opcode: TWebSocketFrameOpCode;
    /// what is stored in the frame data, i.e. in payload field
    content: TWebSocketFramePayloads;
    /// the frame data itself
    // - is plain UTF-8 for focText kind of frame
    // - is raw binary for focBinary or any other frames
    payload: RawByteString;
  end;

  /// points to a WebSockets frame
  PWebSocketFrame = ^TWebSocketFrame;

  /// a dynamic list of WebSockets frames
  TWebSocketFrameDynArray = array of TWebSocketFrame;

  {$M+}
  TWebSocketProcess = class;
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
    fFramesInCount: integer;
    fFramesInBytes: QWord;
    fFramesOutCount: integer;
    fFramesOutBytes: QWord;
    fRemoteIP: SockString;
    fRemoteLocalhost: boolean;
    fLastError: string;
    fEncryption: IProtocol;
    function ProcessHandshake(const ExtIn: RawUTF8; out ExtOut: RawUTF8;
      ErrorMsg: PRawUTF8): boolean; virtual;
    procedure ProcessIncomingFrame(Sender: TWebSocketProcess;
      var request: TWebSocketFrame; const info: RawUTF8); virtual; abstract;
    function SendFrames(Owner: TWebSocketProcess;
      var Frames: TWebSocketFrameDynArray; var FramesCount: integer): boolean; virtual;
    procedure AfterGetFrame(var frame: TWebSocketFrame); virtual;
    procedure BeforeSendFrame(var frame: TWebSocketFrame); virtual;
    function FrameIs(const frame: TWebSocketFrame; const Head: RawUTF8): boolean; virtual;
    function FrameType(const frame: TWebSocketFrame): RawUTF8; virtual;
    function GetEncrypted: boolean;
  public
    /// abstract constructor to initialize the protocol
    // - the protocol should be named, so that the client may be able to request
    // for a given protocol
    // - if aURI is '', any URI would potentially upgrade to this protocol; you can
    // specify an URI to limit the protocol upgrade to a single resource
    constructor Create(const aName,aURI: RawUTF8); reintroduce;
    /// compute a new instance of the WebSockets protocol, with same parameters
    function Clone: TWebSocketProtocol; virtual; abstract;
  published
    /// the Sec-WebSocket-Protocol application name currently involved
    // - is currently 'synopsejson' or 'synopsebinary'
    property Name: RawUTF8 read fName;
    /// the optional URI on which this protocol would be enabled
    // - leave to '' if any URI should match
    property URI: RawUTF8 read fURI;
    /// the last error message, during frame processing
    property LastError: string read fLastError;
    /// returns TRUE if encryption is enabled during the transmission
    // - is only possible for TWebSocketProtocolBinary
    property Encrypted: boolean read GetEncrypted;
    /// how many frames have been received by this instance
    property FramesInCount: integer read fFramesInCount;
    /// how many frames have been sent by this instance
    property FramesOutCount: integer read fFramesOutCount;
    /// how many (uncompressed) bytes have been received by this instance
    property FramesInBytes: QWord read fFramesInBytes;
    /// how many (uncompressed) bytes have been sent by this instance
    property FramesOutBytes: QWord read fFramesOutBytes;
  end;

  /// callback event triggered by TWebSocketProtocolChat for any incoming message
  // - a first call with frame.opcode=focContinuation will take place when
  // the connection will be upgraded to WebSockets
  // - then any incoming focText/focBinary events will trigger this callback
  // - eventually, a focConnectionClose will notify the connection ending
  TOnWebSocketProtocolChatIncomingFrame =
    procedure(Sender: THttpServerResp; const Frame: TWebSocketFrame) of object;

  /// simple chatting protocol, allowing to receive and send WebSocket frames
  // - you can use this protocol to implement simple asynchronous communication
  // with events expecting no answers, e.g. with AJAX applications
  // - see TWebSocketProtocolRest for bi-directional events expecting answers
  TWebSocketProtocolChat = class(TWebSocketProtocol)
  protected
    fOnIncomingFrame: TOnWebSocketProtocolChatIncomingFrame;
    procedure ProcessIncomingFrame(Sender: TWebSocketProcess;
      var request: TWebSocketFrame; const info: RawUTF8); override;
  public
    /// compute a new instance of the WebSockets protocol, with same parameters
    function Clone: TWebSocketProtocol; override;
    /// on the server side, allows to send a message over the wire to a
    // specified client connection
    function SendFrame(Sender: THttpServerResp; const Frame: TWebSocketFrame): boolean;
    /// you can assign an event to this property to be notified of incoming messages
    property OnIncomingFrame: TOnWebSocketProtocolChatIncomingFrame
      read fOnIncomingFrame write fOnIncomingFrame;
  end;

  /// handle a REST application-level bi-directional WebSockets protocol
  // - will emulate a bi-directional REST process, using THttpServerRequest to
  // store and handle the request parameters: clients would be able to send
  // regular REST requests to the server, but the server could use the same
  // communication channel to push REST requests to the client
  // - a local THttpServerRequest will be used on both client and server sides,
  // to store REST parameters and compute the corresponding WebSockets frames
  TWebSocketProtocolRest = class(TWebSocketProtocol)
  protected
    procedure ProcessIncomingFrame(Sender: TWebSocketProcess;
      var request: TWebSocketFrame; const info: RawUTF8); override;
    procedure FrameCompress(const Head: RawUTF8; const Values: array of const;
      const Content,ContentType: RawByteString; var frame: TWebSocketFrame); virtual; abstract;
    function FrameDecompress(const frame: TWebSocketFrame; const Head: RawUTF8;
      const values: array of PRawByteString; var contentType,content: RawByteString): Boolean; virtual; abstract;
    /// convert the input information of REST request to a WebSocket frame
    procedure InputToFrame(Ctxt: THttpServerRequest; aNoAnswer: boolean;
      out request: TWebSocketFrame); virtual;
    /// convert a WebSocket frame to the input information of a REST request
    function FrameToInput(var request: TWebSocketFrame; out aNoAnswer: boolean;
      Ctxt: THttpServerRequest): boolean; virtual;
    /// convert a WebSocket frame to the output information of a REST request
    function FrameToOutput(var answer: TWebSocketFrame; Ctxt: THttpServerRequest): cardinal; virtual;
    /// convert the output information of REST request to a WebSocket frame
    procedure OutputToFrame(Ctxt: THttpServerRequest; Status: Cardinal;
      out answer: TWebSocketFrame); virtual;
  end;

  /// used to store the class of a TWebSocketProtocol type
  TWebSocketProtocolClass = class of TWebSocketProtocol;

  /// handle a REST application-level WebSockets protocol using JSON for transmission
  // - could be used e.g. for AJAX or non Delphi remote access
  // - this class will implement then following application-level protocol:
  // $ Sec-WebSocket-Protocol: synopsejson
  TWebSocketProtocolJSON = class(TWebSocketProtocolRest)
  protected
    procedure FrameCompress(const Head: RawUTF8; const Values: array of const;
      const Content,ContentType: RawByteString; var frame: TWebSocketFrame); override;
    function FrameDecompress(const frame: TWebSocketFrame; const Head: RawUTF8;
      const values: array of PRawByteString; var contentType,content: RawByteString): Boolean; override;
    function FrameIs(const frame: TWebSocketFrame; const Head: RawUTF8): boolean; override;
    function FrameType(const frame: TWebSocketFrame): RawUTF8; override;
  public
    /// compute a new instance of the WebSockets protocol, with same parameters
    function Clone: TWebSocketProtocol; override;
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
    fCompressed: boolean;
    fFramesInBytesSocket: QWord;
    fFramesOutBytesSocket: QWord;
    procedure FrameCompress(const Head: RawUTF8; const Values: array of const;
      const Content,ContentType: RawByteString; var frame: TWebSocketFrame); override;
    function FrameDecompress(const frame: TWebSocketFrame; const Head: RawUTF8;
      const values: array of PRawByteString; var contentType,content: RawByteString): Boolean; override;
    procedure AfterGetFrame(var frame: TWebSocketFrame); override;
    procedure BeforeSendFrame(var frame: TWebSocketFrame); override;
    function FrameIs(const frame: TWebSocketFrame; const Head: RawUTF8): boolean; override;
    function FrameType(const frame: TWebSocketFrame): RawUTF8; override;
    function SendFrames(Owner: TWebSocketProcess;
      var Frames: TWebSocketFrameDynArray; var FramesCount: integer): boolean; override;
    procedure ProcessIncomingFrame(Sender: TWebSocketProcess;
      var request: TWebSocketFrame; const info: RawUTF8); override;
    function GetFramesInCompression: integer;
    function GetFramesOutCompression: integer;
    function ProcessHandshake(const ExtIn: RawUTF8; out ExtOut: RawUTF8;
      ErrorMsg: PRawUTF8): boolean; override;
public
    /// compute a new instance of the WebSockets protocol, with same parameters
    function Clone: TWebSocketProtocol; override;
    /// initialize the WebSockets binary protocol with no encryption
    // - if aURI is '', any URI would potentially upgrade to this protocol; you
    // can specify an URI to limit the protocol upgrade to a single resource
    // - SynLZ compression is enabled by default, unless aCompressed is false
    constructor Create(const aURI: RawUTF8; aCompressed: boolean=true); reintroduce; overload;
    /// initialize the WebSockets binary protocol with a symmetric AES key 
    // - if aURI is '', any URI would potentially upgrade to this protocol; you
    // can specify an URI to limit the protocol upgrade to a single resource
    // - if aKeySize if 128, 192 or 256, TProtocolAES (i.e. AES-CFB encryption)
    //  will be used to secure the transmission
    // - SynLZ compression is enabled by default, unless aCompressed is false
    constructor Create(const aURI: RawUTF8; const aKey; aKeySize: cardinal;
      aCompressed: boolean=true); reintroduce; overload;
    /// initialize the WebSockets binary protocol from a textual key
    // - if aURI is '', any URI would potentially upgrade to this protocol; you
    // can specify an URI to limit the protocol upgrade to a single resource
    // - will create a TProtocolAES or TECDHEProtocol instance, corresponding to
    // the supplied aKey and aServer values, to secure the transmission using
    // a symmetric or assymetric algorithm
    // - SynLZ compression is enabled by default, unless aCompressed is false
    constructor Create(const aURI: RawUTF8; aServer: boolean; const aKey: RawUTF8;
      aCompressed: boolean=true); reintroduce; overload;
  published
    /// defines if SynLZ compression is enabled during the transmission
    // - is set to TRUE by default
    property Compressed: boolean read fCompressed write fCompressed;
    /// how many bytes have been received by this instance from the wire
    property FramesInBytesSocket: QWord read fFramesInBytesSocket;
    /// how many bytes have been sent by this instance to the wire
    property FramesOutBytesSocket: QWord read fFramesOutBytesSocket;
    /// compression ratio of frames received by this instance
    property FramesInCompression: integer read GetFramesInCompression;
    /// compression ratio of frames Sent by this instance
    property FramesOutCompression: integer read GetFramesOutCompression;
  end;

  /// used to maintain a list of websocket protocols (for the server side)
  TWebSocketProtocolList = class
  protected
    fProtocols: array of TWebSocketProtocol;
    function FindIndex(const aName,aURI: RawUTF8): integer;
  public
    /// add a protocol to the internal list
    // - returns TRUE on success
    // - if this protocol is already existing for this given name and URI,
    // returns FALSE: it is up to the caller to release aProtocol if needed
    function Add(aProtocol: TWebSocketProtocol): boolean;
    /// add once a protocol to the internal list
    // - if this protocol is already existing for this given name and URI, any
    // previous one will be released - so it may be confusing on a running server
    // - returns TRUE if the protocol was added for the first time, or FALSE
    // if the protocol has been replaced or is invalid (e.g. aProtocol=nil)
    function AddOnce(aProtocol: TWebSocketProtocol): boolean;
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

  /// indicates which kind of process did occur in the main WebSockets loop
  TWebSocketProcessOne = (
    wspNone, wspPing, wspDone, wspAnswer, wspError, wspClosed);

  /// indicates how TWebSocketProcess.NotifyCallback() will work
  TWebSocketProcessNotifyCallback = (
    wscBlockWithAnswer, wscBlockWithoutAnswer, wscNonBlockWithoutAnswer);

  /// used to manage a thread-safe list of WebSockets frames
  TWebSocketFrameList = class(TSynPersistent)
  public
    /// low-level access to the WebSocket frames list
    List: TWebSocketFrameDynArray;
    /// current number of WebSocket frames in the list
    Count: integer;
    /// the mutex associated with this resource
    // - here we use TAutoLocker and not IAutoLocker for Delphi 5 compatibility
    Safe: TAutoLocker;
    /// initialize the list
    constructor Create(const identifier: RawUTF8); reintroduce;
    /// finalize the list
    destructor Destroy; override;
    /// add a WebSocket frame in the list
    // - this method is thread-safe
    procedure Push(const frame: TWebSocketFrame);
    /// retrieve a WebSocket frame from the list, oldest first
    // - you should specify a frame type to search for, according to the
    // specified WebSockets protocl
    // - this method is thread-safe
    function Pop(protocol: TWebSocketProtocol;
      const head: RawUTF8; out frame: TWebSocketFrame): boolean;
    /// how many 'answer' frames are to be ignored
    // - this method is thread-safe
    function AnswerToIgnore(incr: integer=0): integer;
  end;

  /// parameters to be used for WebSockets process
  TWebSocketProcessSettings = object
    /// time in milli seconds between each focPing commands sent to the other end
    // - default is 0, i.e. no automatic ping sending on client side, and
    // 20000, i.e. 20 seconds, on server side
    HeartbeatDelay: cardinal;
    /// maximum period time in milli seconds when ProcessLoop thread will stay
    // idle before checking for the next pending requests
    // - default is 500 ms, but you may put a lower value, if you expects e.g.
    // REST commands or NotifyCallback(wscNonBlockWithoutAnswer) to be processed
    // with a lower delay
    LoopDelay: cardinal;
    /// will close the connection after a given number of invalid Heartbeat sent
    // - when a Hearbeat is failed to be transmitted, the class will start
    // counting how many ping/pong did fail: when this property value is
    // reached, it will release and close the connection
    // - default value is 5
    DisconnectAfterInvalidHeartbeatCount: cardinal;
    /// how many milliseconds the callback notification should wait acquiring
    // the connection before failing
    // - defaut is 5000, i.e. 5 seconds
    CallbackAcquireTimeOutMS: cardinal;
    /// how many milliseconds the callback notification should wait for the
    // client to return its answer
    // - defaut is 30000, i.e. 30 seconds
    CallbackAnswerTimeOutMS: cardinal;
    /// callback run when a WebSockets client is just connected
    // - triggerred by TWebSocketProcess.ProcessStart 
    OnClientConnected: TNotifyEvent;
    /// callback run when a WebSockets client is just disconnected
    // - triggerred by TWebSocketProcess.ProcessStop
    OnClientDisconnected: TNotifyEvent;
    /// by default, contains [] to minimize the logged information
    // - set logHeartbeat if you want the ping/pong frames to be logged
    // - set logTextFrameContent if you want the text frame content to be logged
    // - set logBinaryFrameContent if you want the binary frame content to be logged
    // - used only if WebSocketLog global variable is set to a TSynLog class
    LogDetails: set of (logHeartbeat,logTextFrameContent,logBinaryFrameContent);
    /// will set the default values
    procedure SetDefaults;
    /// will set LogDetails to its highest level of verbosity
    // - used only if WebSocketLog global variable is set
    procedure SetFullLog;
  end;
                              
  /// points to parameters to be used for WebSockets process
  // - using a pointer/reference type will allow in-place modification of
  // any TWebSocketProcess.Settings, TWebSocketServer.Settings or
  // THttpClientWebSockets.Settings property
  PWebSocketProcessSettings = ^TWebSocketProcessSettings;

  /// the current state of the WebSockets process
  TWebSocketProcessState = (wpsCreate,wpsRun,wpsClose,wpsDestroy);

  /// generic WebSockets process, used on both client or server sides
  TWebSocketProcess = class(TSynPersistentLocked)
  protected
    fSocket: TCrtSocket;
    fProcessName: RawUTF8;
    fIncoming: TWebSocketFrameList;
    fOutgoing: TWebSocketFrameList;
    fOwnerThread: TSynThread;
    fOwnerConnection: Int64;
    fState: TWebSocketProcessState;
    fProtocol: TWebSocketProtocol;
    fMaskSentFrames: byte;
    fLastSocketTicks: Int64;
    fSettings: TWebSocketProcessSettings;
    fInvalidPingSendCount: cardinal;
    fProcessCount: integer;
    {$ifdef DELPHI5OROLDER}
    fSafeIn, fSafeOut: TAutoLocker;
    {$else}
    fSafeIn, fSafeOut: IAutoLocker;
    {$endif}
    /// low level WebSockets framing protocol
    function GetFrame(out Frame: TWebSocketFrame; TimeOut: cardinal; IgnoreExceptions: boolean): boolean;
    function SendFrame(var Frame: TWebSocketFrame): boolean;
    /// methods run e.g. by TWebSocketServerRest.WebSocketProcessLoop
    procedure ProcessStart; virtual;
    procedure ProcessStop; virtual;
    procedure ProcessLoop; virtual;
    function ComputeContext(out RequestProcess: TOnHttpServerRequest): THttpServerRequest; virtual; abstract;
    procedure HiResDelay(const start: Int64);
    procedure Log(const frame: TWebSocketFrame; const aMethodName: RawUTF8;
      aEvent: TSynLogInfo=sllTrace; DisableRemoteLog: Boolean=false); virtual;
    function LastPingDelay: Int64;
    procedure SetLastPingTicks(invalidPing: boolean=false);
    procedure SendPendingOutgoingFrames;
  public
    /// initialize the WebSockets process on a given connection
    // - the supplied TWebSocketProtocol will be owned by this instance
    // - other parameters should reflect the client or server expectations
    constructor Create(aSocket: TCrtSocket; aProtocol: TWebSocketProtocol;
      aOwnerConnection: Int64; aOwnerThread: TSynThread;
      const aSettings: TWebSocketProcessSettings;
      const aProcessName: RawUTF8); reintroduce; virtual;
    /// finalize the context
    // - will release the TWebSocketProtocol associated instance
    destructor Destroy; override;
    /// will push a request or notification to the other end of the connection
    // - caller should set the aRequest with the outgoing parameters, and
    // optionally receive a response from the other end
    // - the request may be sent in blocking or non blocking mode
    // - returns the HTTP Status code (e.g. HTTP_SUCCESS=200 for success)
    function NotifyCallback(aRequest: THttpServerRequest;
      aMode: TWebSocketProcessNotifyCallback): cardinal; virtual;
    /// the settings currently used during the WebSockets process
    // - defined as a pointer so that you may be able to change the values
    function Settings: PWebSocketProcessSettings; {$ifdef HASINLINE}inline;{$endif}
    /// returns the current state of the underlying connection
    function State: TWebSocketProcessState;
    /// the associated communication socket
    // - on the server side, is a THttpServerSocket
    // - access to this instance is protected by Safe.Lock/Unlock
    property Socket: TCrtSocket read fSocket;
    /// how many frames are currently processed by this connection
    property ProcessCount: integer read fProcessCount;

  published
    /// the Sec-WebSocket-Protocol application protocol currently involved
    // - TWebSocketProtocolJSON or TWebSocketProtocolBinary in the mORMot context
    // - could be nil if the connection is in standard HTTP/1.1 mode
    property Protocol: TWebSocketProtocol read fProtocol;
    /// the associated process name
    property ProcessName: RawUTF8 read fProcessName write fProcessName;
    /// how many invalid heartbeat frames have been sent
    // -  a non 0 value indicates a connection problem
    property InvalidPingSendCount: cardinal read fInvalidPingSendCount;
  end;


{ -------------- WebSockets Server classes for bidirectional remote access }

type
  {$M+}
  TWebSocketServerResp = class;
  {$M-}

  /// implements WebSockets process as used on server side
  TWebSocketProcessServer = class(TWebSocketProcess)
  protected
    fServerResp: TWebSocketServerResp;
    function ComputeContext(out RequestProcess: TOnHttpServerRequest): THttpServerRequest; override;
    procedure ProcessStart; override;
    procedure ProcessStop; override;
  end;

  /// an enhanced input/output structure used for HTTP and WebSockets requests
  // - this class will contain additional parameters used to maintain the
  // WebSockets execution context in overriden TWebSocketServer.Process method
  TWebSocketServerResp = class(THttpServerResp)
  protected
    fProcess: TWebSocketProcessServer;
  public
    /// initialize the context, associated to a HTTP/WebSockets server instance
    constructor Create(aServerSock: THttpServerSocket; aServer: THttpServer
       {$ifdef USETHREADPOOL}; aThreadPool: TSynThreadPoolTHttpServer{$endif}); override;
    /// push a notification to the client
    function NotifyCallback(Ctxt: THttpServerRequest; aMode: TWebSocketProcessNotifyCallback): cardinal; virtual;
    /// the Sec-WebSocket-Protocol application protocol currently involved
    // - TWebSocketProtocolJSON or TWebSocketProtocolBinary in the mORMot context
    // - could be nil if the connection is in standard HTTP/1.1 mode
    function WebSocketProtocol: TWebSocketProtocol;
  end;

  /// main HTTP/WebSockets server Thread using the standard Sockets API (e.g. WinSock)
  // - once upgraded to WebSockets from the client, this class is able to serve
  // any Sec-WebSocket-Protocol application content
  TWebSocketServer = class(THttpServer)
  protected
    fWebSocketConnections: TObjectListLocked;
    fProtocols: TWebSocketProtocolList;
    fSettings: TWebSocketProcessSettings;
    /// will validate the WebSockets handshake, then call WebSocketProcessLoop()
    function WebSocketProcessUpgrade(ClientSock: THttpServerSocket;
      Context: TWebSocketServerResp): integer; virtual;
    /// overriden method which will recognize the WebSocket protocol handshake,
    // then run the whole bidirectional communication in its calling thread
    // - here aCallingThread is a THttpServerResp, and ClientSock.Headers
    // and ConnectionUpgrade properties should be checked for the handshake
    procedure Process(ClientSock: THttpServerSocket;
      ConnectionID: integer; ConnectionThread: TSynThread); override;
    /// identifies an incoming THttpServerResp as a valid TWebSocketServerResp
    function IsActiveWebSocket(ConnectionID: integer): TWebSocketServerResp; overload;
    function IsActiveWebSocket(ConnectionThread: TSynThread): TWebSocketServerResp; overload;
  public
    /// create a Server Thread, binded and listening on a port
    // - this constructor will raise a EHttpServer exception if binding failed
    // - expects the port to be specified as string, e.g. '1234'; you can
    // optionally specify a server address to bind to, e.g. '1.2.3.4:1234'
    // - due to the way how WebSockets works, one thread will be created
    // for any incoming connection
    // - note that this constructor will not register any protocol, so is
    // useless until you execute Protocols.Add()
    // - in the current implementation, the ServerThreadPoolCount parameter will
    // be ignored by this class, and one thread will be maintained per client
    constructor Create(const aPort: SockString; OnStart,OnStop: TNotifyThreadEvent;
      const ProcessName: SockString {$ifdef USETHREADPOOL}; ServerThreadPoolCount: integer=0{$endif}); override;
    /// close the server
    destructor Destroy; override;
    /// access to the protocol list handled by this server
    property WebSocketProtocols: TWebSocketProtocolList read fProtocols;
    /// the settings to be used for WebSockets process
    // - note that those parameters won't be propagated to existing connections
    // - defined as a pointer so that you may be able to change the values
    function Settings: PWebSocketProcessSettings; {$ifdef HASINLINE}inline;{$endif}
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
  public
    /// create a Server Thread, binded and listening on a port, with our
    // 'synopsebinary' and optionally 'synopsejson' modes
    // - if aWebSocketsURI is '', any URI would potentially upgrade; you can
    // specify an URI to limit the protocol upgrade to a single resource
    // - TWebSocketProtocolBinary will always be registered by this constructor
    // - if the encryption key text is not '', TWebSocketProtocolBinary will
    // use AES-CFB 256 bits encryption
    // - if aWebSocketsAJAX is TRUE, it will also register TWebSocketProtocolJSON
    // so that AJAX applications would be able to connect to this server
    constructor Create(const aPort: SockString; OnStart,OnStop: TNotifyThreadEvent;
      const aProcessName, aWebSocketsURI, aWebSocketsEncryptionKey: RawUTF8;
      aWebSocketsAJAX: boolean=false); reintroduce; overload;
    /// defines the WebSockets protocols to be used for this Server
    // - i.e. 'synopsebinary' and optionally 'synopsejson' modes
    // - if aWebSocketsURI is '', any URI would potentially upgrade; you can
    // specify an URI to limit the protocol upgrade to a single resource
    // - TWebSocketProtocolBinary will always be registered by this constructor
    // - if the encryption key text is not '', TWebSocketProtocolBinary will
    // use AES-CFB 256 bits encryption
    // - if aWebSocketsAJAX is TRUE, it will also register TWebSocketProtocolJSON
    // so that AJAX applications would be able to connect to this server
    procedure WebSocketsEnable(const aWebSocketsURI, aWebSocketsEncryptionKey: RawUTF8;
      aWebSocketsAJAX: boolean=false; aWebSocketsCompressed: boolean=true);
    /// server can send a request back to the client, when the connection has
    // been upgraded to WebSocket
    // - InURL/InMethod/InContent properties are input parameters (InContentType
    // is ignored)
    // - OutContent/OutContentType/OutCustomHeader are output parameters
    // - CallingThread should be set to the client's Ctxt.CallingThread
    // value, so that the method could know which connnection is to be used -
    // it will return STATUS_NOTFOUND (404) if the connection is unknown
    // - result of the function is the HTTP error code (200 if OK, e.g.)
    function Callback(Ctxt: THttpServerRequest; aNonBlocking: boolean): cardinal; override;
  end;


/// used to return the text corresponding to a specified WebSockets frame data
function ToText(opcode: TWebSocketFrameOpCode): PShortString; overload;


{ -------------- WebSockets Client classes for bidirectional remote access }

type
  {$M+}
  THttpClientWebSockets = class;
  TWebSocketProcessClientThread = class;        
  {$M-}

  /// implements WebSockets process as used on client side
  TWebSocketProcessClient = class(TWebSocketProcess)
  protected
    fClientThread: TWebSocketProcessClientThread;
    function ComputeContext(out RequestProcess: TOnHttpServerRequest): THttpServerRequest; override;
  public
    /// initialize the client process for a given THttpClientWebSockets
    constructor Create(aSender: THttpClientWebSockets; aProtocol: TWebSocketProtocol;
      const aProcessName: RawUTF8); reintroduce; virtual;
    /// finalize the process
    destructor Destroy; override;
  end;

  /// the current state of the client side processing thread
  TWebSocketProcessClientThreadState = (sCreate, sRun, sFinished, sClosed);

  /// WebSockets processing thread used on client side
  // - will handle any incoming callback
  TWebSocketProcessClientThread = class(TSynThread)
  protected
    fThreadState: TWebSocketProcessClientThreadState;
    fProcess: TWebSocketProcessClient;
    procedure Execute; override;
  public
    constructor Create(aProcess: TWebSocketProcessClient); reintroduce;
  end;

  /// Socket API based REST and HTTP/1.1 client, able to upgrade to WebSockets
  // - will implement regular HTTP/1.1 until WebSocketsUpgrade() is called
  THttpClientWebSockets = class(THttpClientSocket)
  protected
    fProcess: TWebSocketProcessClient;
    fSettings: TWebSocketProcessSettings;
    fOnCallbackRequestProcess: TOnHttpServerRequest;
    fOnWebSocketsClosed: TNotifyEvent;
    procedure SetInt32OptionByIndex(OptName, OptVal: integer); override;
  public
    /// common initialization of all constructors
    // - this overridden method will set the UserAgent with some default value
    constructor Create(aTimeOut: cardinal=10000); override;
    /// finalize the connection
    destructor Destroy; override;
    /// process low-level REST request, either on HTTP/1.1 or via WebSockets
    // - after WebSocketsUpgrade() call, will use WebSockets for the communication
    function Request(const url, method: SockString; KeepAlive: cardinal;
      const header, Data, DataType: SockString; retry: boolean): integer; override;
    /// upgrade the HTTP client connection to a specified WebSockets protocol
    // - i.e. 'synopsebinary' and optionally 'synopsejson' modes
    // - you may specify an URI to as expected by the server for upgrade
    // - if aWebSocketsAJAX equals default FALSE, it will register the
    // TWebSocketProtocolBinaryprotocol, with AES-CFB 256 bits encryption
    // if the encryption key text is not '' and optional SynLZ compression
    // - if aWebSocketsAJAX is TRUE, it will register the slower and less secure
    // TWebSocketProtocolJSON (to be used for AJAX debugging/test purposes only)
    // and aWebSocketsEncryptionKey/aWebSocketsCompression parameters won't be used
    // - will return '' on success, or an error message on failure
    function WebSocketsUpgrade(const aWebSocketsURI, aWebSocketsEncryptionKey: RawUTF8;
      aWebSocketsAJAX: boolean=false; aWebSocketsCompression: boolean=true): RawUTF8;
    /// the settings to be used for WebSockets process
    // - note that those parameters won't be propagated to existing connections
    // - defined as a pointer so that you may be able to change the values
    function Settings: PWebSocketProcessSettings; {$ifdef HASINLINE}inline;{$endif}
    /// this event handler will be executed for any incoming push notification
    property OnCallbackRequestProcess: TOnHttpServerRequest
      read fOnCallbackRequestProcess write fOnCallbackRequestProcess;
    /// this event handler when the WebSocket link is destroyed
    // - may happen e.g. after graceful close from the server side, or
    // after DisconnectAfterInvalidHeartbeatCount is reached
    property OnWebSocketsClosed: TNotifyEvent
      read fOnWebSocketsClosed write fOnWebSocketsClosed;
  published
    /// the current WebSockets processing class
    // - equals nil for plain HTTP/1.1 mode
    // - points to the current WebSockets process instance, after a successful
    // WebSocketsUpgrade() call, so that you could use e.g. WebSockets.Protocol
    // to retrieve the protocol currently used
    property WebSockets: TWebSocketProcessClient read fProcess write fProcess;
  end;


var
  /// if set, will log all WebSockets raw information
  // - see also TWebSocketProcessSettings.LogDetails and
  // TWebSocketProcessSettings.SetFullLog to setup even more verbose information,
  // e.g. by setting HttpServerFullWebSocketsLog and HttpClientFullWebSocketsLog
  // global variables to true (as defined in mORMotHttpServer/mORMotHttpClient)
  WebSocketLog: TSynLogClass;

  /// number of bytes above which SynLZ compression may be done
  // - when working with TWebSocketProtocolBinary
  // - it is useless to compress smaller frames, which fits in network MTU 
  WebSocketsBinarySynLzThreshold: integer = 450;

  /// how replay attacks will be handled in TWebSocketProtocolBinary encryption
  WebSocketsIVReplayAttackCheck: TAESIVReplayAttackCheck = repCheckedIfAvailable;

  /// the allowed maximum size, in MB, of a WebSockets frame
  WebSocketsMaxFrameMB: cardinal = 256;

implementation


{ -------------- WebSockets shared classes for bidirectional remote access }

function ToText(opcode: TWebSocketFrameOpCode): PShortString; overload;
begin
  result := GetEnumName(TypeInfo(TWebSocketFrameOpCode),ord(opcode));
end;

function ToText(block: TWebSocketProcessNotifyCallback): PShortString; overload;
begin
  result := GetEnumName(TypeInfo(TWebSocketProcessNotifyCallback),ord(block));
end;

function ToText(st: TWebSocketProcessClientThreadState): PShortString; overload;
begin
  result := GetEnumName(TypeInfo(TWebSocketProcessClientThreadState),ord(st));
end;


type
  TThreadHook = class(TThread);

const
  STATUS_WEBSOCKETCLOSED = 0;

procedure ComputeChallenge(const Base64: RawByteString; out Digest: TSHA1Digest);
const SALT: string[36] = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';
var SHA: TSHA1;
begin
  SHA.Init;
  SHA.Update(pointer(Base64),length(Base64));
  SHA.Update(@SALT[1],36);
  SHA.Final(Digest);
end;


{ TWebSocketProcessSettings }

procedure TWebSocketProcessSettings.SetDefaults;
begin
  HeartbeatDelay := 0;
  LoopDelay := 500;
  DisconnectAfterInvalidHeartbeatCount := 5;
  CallbackAcquireTimeOutMS := 5000;
  CallbackAnswerTimeOutMS := 5000;
  LogDetails := [];
  OnClientConnected := nil;
  OnClientDisconnected := nil;
end;

procedure TWebSocketProcessSettings.SetFullLog;
begin
  LogDetails := [logHeartbeat,logTextFrameContent,logBinaryFrameContent];
end;


{ TWebSocketProtocol }

procedure TWebSocketProtocol.AfterGetFrame(var frame: TWebSocketFrame);
begin 
  inc(fFramesInCount);
  inc(fFramesInBytes,length(frame.payload)+2);
end;

procedure TWebSocketProtocol.BeforeSendFrame(var frame: TWebSocketFrame);
begin
  inc(fFramesOutCount);
  inc(fFramesOutBytes,length(frame.payload)+2);
end;

constructor TWebSocketProtocol.Create(const aName,aURI: RawUTF8);
begin
  fName := aName;
  fURI := aURI;
end;

function TWebSocketProtocol.FrameIs(const frame: TWebSocketFrame;
  const Head: RawUTF8): boolean;
begin
  result := false; // no frame type by default
end;

function TWebSocketProtocol.FrameType(
  const frame: TWebSocketFrame): RawUTF8;
begin
  result := '*'; // no frame URI by default
end;

function TWebSocketProtocol.ProcessHandshake(const ExtIn: RawUTF8; out ExtOut: RawUTF8;
  ErrorMsg: PRawUTF8): boolean;
begin
  result := false; // do nothing by default
end;

function TWebSocketProtocol.SendFrames(Owner: TWebSocketProcess;
  var Frames: TWebSocketFrameDynArray; var FramesCount: integer): boolean;
var i,n: integer;
begin // this default implementation will send all frames one by one
  n := FramesCount;
  if (n>0) and (Owner<>nil) then begin
    result := false;
    FramesCount := 0;
    for i := 0 to n-1 do
      if Owner.SendFrame(Frames[i]) then
        Frames[i].payload := '' else
        exit;
  end;
  result := true;
end;

function TWebSocketProtocol.GetEncrypted: boolean;
begin
  result := (self<>nil) and (fEncryption<>nil);
end;


{ TWebSocketFrameList }

constructor TWebSocketFrameList.Create(const identifier: RawUTF8);
begin
  inherited Create;
  {$ifdef WSLOCKERDEBUGLIST}
  Safe := TAutoLockerDebug.Create(WebSocketLog,identifier); // more verbose
  {$else}
  Safe := TAutoLocker.Create;
  {$endif}
end;

destructor TWebSocketFrameList.Destroy;
begin
  inherited;
  Safe.Free;
end;

function TWebSocketFrameList.AnswerToIgnore(incr: integer): integer;
begin
  Safe.Enter;
  if incr<>0 then
    inc(Safe.Safe^.Padding[0].VInteger,incr);
  result := Safe.Locker.Padding[0].VInteger;
  Safe.Leave;
end;

function TWebSocketFrameList.Pop(protocol: TWebSocketProtocol; const head: RawUTF8;
  out frame: TWebSocketFrame): boolean;
var i: integer;
begin
  result := false;
  if (self=nil) or (Count=0) or (head='') or (protocol=nil) then
    exit;
  Safe.Enter;
  try
    for i := 0 to Count-1 do
      if protocol.FrameIs(List[i],head) then begin
        result := true;
        frame := List[i];
        List[i].payload := '';
        dec(Count);
        if i<Count then begin
          MoveFast(List[i+1],List[i],(Count-i)*sizeof(List[i]));
          pointer(List[Count].payload) := nil;
        end;
        exit;
      end;
  finally
    Safe.Leave;
  end;
end;

procedure TWebSocketFrameList.Push(const frame: TWebSocketFrame);
begin
  if self=nil then
    exit;
  Safe.Enter;
  try
    if Count>=length(List) then
      SetLength(List,Count+Count shr 3+8);
    List[Count] := frame;
    inc(Count);
  finally
    Safe.Leave;
  end;
end;


{ TWebSocketProtocolChat }

function TWebSocketProtocolChat.Clone: TWebSocketProtocol;
begin
  result := TWebSocketProtocolChat.Create(fName,fURI);
  TWebSocketProtocolChat(result).OnIncomingFrame := OnIncomingFrame;
end;

procedure TWebSocketProtocolChat.ProcessIncomingFrame(Sender: TWebSocketProcess;
  var request: TWebSocketFrame; const info: RawUTF8);
begin
  if Assigned(OnInComingFrame) and
     Sender.InheritsFrom(TWebSocketProcessServer) then
    OnIncomingFrame(TWebSocketProcessServer(Sender).fServerResp,request);
end;

function TWebSocketProtocolChat.SendFrame(Sender: THttpServerResp;
  const frame: TWebSocketFrame): boolean;
var tmp: TWebSocketFrame; // SendFrame() may change frame content
begin
  result := false;
  if (self=nil) or (Sender=nil) or TThreadHook(Sender).Terminated or
     not (Frame.opcode in [focText,focBinary]) then
    exit;
  if (Sender.Server as TWebSocketServer).IsActiveWebSocket(Sender)<>Sender then
    exit;
  tmp.opcode := frame.opcode;
  tmp.content := frame.content;
  SetString(tmp.payload,PAnsiChar(Pointer(frame.payload)),length(frame.payload));
  result := (Sender as TWebSocketServerResp).fProcess.SendFrame(tmp)
end;


{ TWebSocketProtocolRest }

procedure TWebSocketProtocolRest.ProcessIncomingFrame(Sender: TWebSocketProcess;
  var request: TWebSocketFrame; const info: RawUTF8);
var Ctxt: THttpServerRequest;
    onRequest: TOnHttpServerRequest;
    status: cardinal;
    noAnswer: boolean;
    answer: TWebSocketFrame;
begin
  if not (request.opcode in [focText,focBinary]) then
    exit; // ignore e.g. from TWebSocketServerResp.ProcessStart/ProcessStop
  if FrameIs(request,'request') then
  try
    Ctxt := Sender.ComputeContext(onRequest);
    try
      if not FrameToInput(request,noAnswer,Ctxt) then
        raise ESynBidirSocket.CreateUTF8('%.ProcessOne: invalid frame',[self]);
      request.payload := ''; // release memory ASAP
      if info<>'' then
        Ctxt.AddInHeader(info);
      status := onRequest(Ctxt);
      if (Ctxt.OutContentType=HTTP_RESP_NORESPONSE) or noAnswer then
        exit;
      OutputToFrame(Ctxt,status,answer);
      Sender.SendFrame(answer);
    finally
      Ctxt.Free;
    end;
  except
    on E: Exception do
      fLastError := Format('%s [%s]',[E.ClassName,E.Message]);
  end else
  if (Sender.fIncoming.AnswerToIgnore>0) and FrameIs(request,'answer') then begin
    Sender.fIncoming.AnswerToIgnore(-1);
    Sender.Log(request,'Ignored answer after NotifyCallback TIMEOUT',sllWarning);
  end else
    Sender.fIncoming.Push(request);
end;

// by convention, defaults are POST and JSON, to reduce frame size for SOA calls

procedure TWebSocketProtocolRest.InputToFrame(Ctxt: THttpServerRequest;
  aNoAnswer: boolean; out request: TWebSocketFrame);
var Method,InContentType: RawByteString;
begin
  if not IdemPropNameU(Ctxt.Method,'POST') then
    Method := Ctxt.Method;
  if (Ctxt.InContent<>'') and (Ctxt.InContentType<>'') and
     not IdemPropNameU(Ctxt.InContentType,JSON_CONTENT_TYPE) then
    InContentType := Ctxt.InContentType;
  FrameCompress('request',[Method,Ctxt.URL,Ctxt.InHeaders,ord(aNoAnswer)],
    Ctxt.InContent,InContentType,request);
end;

function TWebSocketProtocolRest.FrameToInput(var request: TWebSocketFrame;
  out aNoAnswer: boolean; Ctxt: THttpServerRequest): boolean;
var URL,Method,InHeaders,NoAnswer,InContentType,InContent: RawByteString;
begin
  result := FrameDecompress(request,'request',
    [@Method,@URL,@InHeaders,@NoAnswer],InContentType,InContent);
  if result then begin
    if (InContentType='') and (InContent<>'') then
      InContentType := JSON_CONTENT_TYPE_VAR;
    if Method='' then
      Method := 'POST';
    Ctxt.Prepare(URL,Method,InHeaders,InContent,InContentType,fRemoteIP);
    aNoAnswer := NoAnswer='1';
  end;
end;

procedure TWebSocketProtocolRest.OutputToFrame(Ctxt: THttpServerRequest;
  Status: Cardinal; out answer: TWebSocketFrame);
var OutContentType: RawByteString;
begin
  if (Ctxt.OutContent<>'') and
     not IdemPropNameU(Ctxt.OutContentType,JSON_CONTENT_TYPE) then
    OutContentType := Ctxt.OutContentType;
  FrameCompress('answer',[Status,Ctxt.OutCustomHeaders],
    Ctxt.OutContent,OutContentType,answer);
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
  if (outContentType='') and (outContent<>'') then
    Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR else
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

procedure TWebSocketProtocolJSON.FrameCompress(const Head: RawUTF8;
  const Values: array of const; const Content, ContentType: RawByteString;
  var frame: TWebSocketFrame);
var WR: TTextWriter;
    i: integer;
begin
  frame.opcode := focText;
  frame.content := [];
  WR := TTextWriter.CreateOwnedStream;
  try
    WR.Add('{');
    WR.AddFieldName(Head);
    WR.Add('[');
    for i := 0 to High(Values) do begin
      WR.AddJSONEscape(Values[i]);
      WR.Add(',');
    end;
    WR.Add('"');
    WR.AddString(ContentType);
    WR.Add('"',',');
    if Content='' then
      WR.Add('"','"') else
    if (ContentType='') or
       IdemPropNameU(ContentType,JSON_CONTENT_TYPE) then
      WR.AddNoJSONEscape(pointer(Content),length(Content)) else
    if IdemPChar(pointer(ContentType),'TEXT/') then
      WR.AddCSVUTF8([Content]) else
      WR.WrBase64(pointer(Content),length(Content),true);
    WR.Add(']','}');
    WR.SetText(RawUTF8(frame.payload));
  finally
    WR.Free;
  end;
end;

function JsonFrameIs(const frame: TWebSocketFrame; const Head: RawUTF8): PUTF8Char;
var P,txt: PUTF8Char;
begin
  result := nil;
  if (length(frame.payload)<10) or (frame.opcode<>focText) then
    exit;
  P := pointer(frame.payload);
  if not NextNotSpaceCharIs(P,'{') then
    exit;
  while P^<>'"' do begin
    inc(P);
    if P^=#0 then exit;
  end;
  txt := P+1;
  P := GotoEndOfJSONString(P); // here P^ should be '"'
  if (P^<>#0) and IdemPropNameU(Head,txt,P-txt) then
    result := P+1;
end;

function TWebSocketProtocolJSON.FrameIs(const frame: TWebSocketFrame;
  const Head: RawUTF8): boolean;
begin
  result := JsonFrameIs(frame,Head)<>nil;
end;

function TWebSocketProtocolJSON.FrameDecompress(
  const frame: TWebSocketFrame; const Head: RawUTF8;
  const values: array of PRawByteString; var contentType,
  content: RawByteString): Boolean;
var i: Integer;
    P,txt: PUTF8Char;
begin
  result := false;
  P := JsonFrameIs(frame,Head);
  if P=nil then
    exit;
  if not NextNotSpaceCharIs(P,':') or
     not NextNotSpaceCharIs(P,'[') then
    exit;
  for i := 0 to high(values) do
    values[i]^ := GetJSONField(P,P);
  contentType := GetJSONField(P,P);
  if P=nil then
    exit;
  if (contentType='') or
     IdemPropNameU(contentType,JSON_CONTENT_TYPE) then
    GetJSONItemAsRawJSON(P,RawJSON(content)) else begin
    txt := GetJSONField(P,P);
    if IdemPChar(pointer(contentType),'TEXT/') then
      SetString(content,txt,StrLen(txt)) else
    if not Base64MagicCheckAndDecode(txt,content) then
      exit;
  end;
  result := true;
end;

function TWebSocketProtocolJSON.FrameType(
  const frame: TWebSocketFrame): RawUTF8;
var P,txt: PUTF8Char;
begin
  result := '*';
  if (length(frame.payload)<10) or (frame.opcode<>focText) then
    exit;
  P := pointer(frame.payload);
  if not NextNotSpaceCharIs(P,'{') or
     not NextNotSpaceCharIs(P,'"') then
    exit;
  txt := P;
  P := GotoEndOfJSONString(P);
  SetString(result,txt,P-Txt);
end;


{ TWebSocketProtocolBinary }

constructor TWebSocketProtocolBinary.Create(const aURI: RawUTF8;
  aCompressed: boolean);
begin
  inherited Create('synopsebinary',aURI);
  fCompressed := aCompressed;
end;

constructor TWebSocketProtocolBinary.Create(const aURI: RawUTF8;
  const aKey; aKeySize: cardinal; aCompressed: boolean);
begin
  Create(aURI,aCompressed);
  if aKeySize>=128 then
    fEncryption := TProtocolAES.Create(
      TAESCFB,aKey,aKeySize,WebSocketsIVReplayAttackCheck);
end;

constructor TWebSocketProtocolBinary.Create(const aURI: RawUTF8;
  aServer: boolean; const aKey: RawUTF8; aCompressed: boolean);
var key: TSHA256Digest;
begin
  if aKey='' then
    Create(aURI,aCompressed) else begin
    fEncryption := TECDHEProtocol.FromKey(aKey,aServer);
    if fEncryption<>nil then
      Create(aURI,aCompressed) else begin
      SHA256Weak(aKey,key);
      Create(aURI,key,256,aCompressed);
    end;
  end;
end;

function TWebSocketProtocolBinary.Clone: TWebSocketProtocol;
begin
  result := TWebSocketProtocolBinary.Create(fURI,self,0,fCompressed);
  if fEncryption<>nil then
    TWebSocketProtocolBinary(result).fEncryption := fEncryption.Clone;
end;

const
  FRAME_HEAD_SEP = #1;

procedure TWebSocketProtocolBinary.FrameCompress(const Head: RawUTF8;
  const Values: array of const; const Content, ContentType: RawByteString;
  var frame: TWebSocketFrame);
var item: RawUTF8;
    i: integer;
    W: TFileBufferWriter;
begin
  frame.opcode := focBinary;
  if (ContentType<>'') and (Content<>'') and
     not IdemPChar(pointer(ContentType),'TEXT/') and
     IsContentCompressed(pointer(Content),length(Content)) then
    frame.content := [fopAlreadyCompressed] else
    frame.content := [];
  W := TFileBufferWriter.Create(TRawByteStringStream);
  try
    W.WriteBinary(Head);
    W.Write1(byte(FRAME_HEAD_SEP));
    for i := 0 to high(Values) do
    with Values[i] do begin
      VarRecToUTF8(Values[i],item);
      W.Write(item);
    end;
    W.Write(ContentType);
    W.WriteBinary(Content);
    W.Flush;
    frame.payload := TRawByteStringStream(W.Stream).DataString;
  finally
    W.Free;
  end;
end;

function TWebSocketProtocolBinary.FrameIs(const frame: TWebSocketFrame;
  const Head: RawUTF8): boolean;
var headLen: integer;
begin
  headLen := length(Head);
  result := (frame.opcode=focBinary) and
            (length(frame.payload)>=headLen+6) and
            (frame.payload[headLen+1]=FRAME_HEAD_SEP) and
            IdemPropNameU(Head,pointer(frame.payload),headLen);
end;

function TWebSocketProtocolBinary.FrameType(
  const frame: TWebSocketFrame): RawUTF8;
var i: integer;
begin
  if (length(frame.payload)<10) or (frame.opcode<>focBinary) then
    i := 0 else
    i := PosEx(FRAME_HEAD_SEP,frame.payload);
  if i=0 then
    result := '*' else
    result := copy(frame.payload,1,i-1);
end;

procedure TWebSocketProtocolBinary.BeforeSendFrame(var frame: TWebSocketFrame);
var value: RawByteString;
    threshold: integer;
begin
  inherited BeforeSendFrame(frame);
  if frame.opcode=focBinary then begin
    if fCompressed then begin
      if fRemoteLocalhost or (fopAlreadyCompressed in frame.content) then
        threshold := maxInt else // localhost or compressed -> no SynLZ
        threshold := WebSocketsBinarySynLzThreshold;
      SynLZCompress(pointer(frame.payload),length(frame.payload),value,threshold);
    end else
      value := frame.payload;
    if fEncryption<>nil then
      fEncryption.Encrypt(value,frame.payload) else
      frame.payload := value;
  end;
  inc(fFramesOutBytesSocket,length(frame.payload)+2);
end;

procedure TWebSocketProtocolBinary.AfterGetFrame(var frame: TWebSocketFrame);
var value: RawByteString;
    res: TProtocolResult;
begin
  inc(fFramesInBytesSocket,length(frame.payload)+2);
  if frame.opcode=focBinary then begin
    if fEncryption<>nil then begin
      res := fEncryption.Decrypt(frame.payload,value);
      if res<>sprSuccess then
        raise ESynBidirSocket.CreateUTF8('%.AfterGetFrame: encryption error %',
          [self,ToText(res)^]);
     end else
      value := frame.payload;
    if fCompressed then
      SynLZDecompress(pointer(value),length(value),frame.payload) else
      frame.payload := value;
  end;
  inherited AfterGetFrame(frame);
end;

function TWebSocketProtocolBinary.FrameDecompress(const frame: TWebSocketFrame;
  const Head: RawUTF8; const values: array of PRawByteString;
  var contentType,content: RawByteString): Boolean;
var i: integer;
    P: PByte;
begin
  result := false;
  if not FrameIs(frame,Head) then
    exit;
  P := pointer(frame.payload);
  inc(P,Length(Head)+1);
  for i := 0 to high(values) do
    values[i]^ := FromVarString(P);
  contentType := FromVarString(P);
  i := length(frame.payload)-(PAnsiChar(P)-pointer(frame.payload));
  if i<0 then
    exit;
  SetString(content,PAnsiChar(P),i);
  result := true;
end;

function TWebSocketProtocolBinary.SendFrames(Owner: TWebSocketProcess;
  var Frames: TWebSocketFrameDynArray; var FramesCount: integer): boolean;
var jumboFrame: TWebSocketFrame;
    i: integer;
    W: TFileBufferWriter;
begin
  if (FramesCount=0) or (Owner=nil) then begin
    result := true;
    exit;
  end;
  if FramesCount=1 then begin
    FramesCount := 0;
    result := Owner.SendFrame(Frames[0]);
    exit;
  end;
  jumboFrame.opcode := focBinary;
  jumboFrame.content := [];
  W := TFileBufferWriter.Create(TRawByteStringStream);
  try
    W.WriteBinary('frames'+FRAME_HEAD_SEP);
    dec(FramesCount);
    W.WriteVarUInt32(FramesCount);
    for i := 0 to FramesCount do
      if Frames[i].opcode=focBinary then
        W.Write(Frames[i].payload) else
        raise ESynBidirSocket.CreateUTF8('%.SendFrames[%]: Unexpected opcode=%',
          [self,i,ord(Frames[i].opcode)]);
    W.Flush;
    jumboFrame.payload := TRawByteStringStream(W.Stream).DataString;
  finally
    W.Free;
  end;
  FramesCount := 0;
  Frames := nil;
  result := Owner.SendFrame(jumboFrame); // send all frames at once
end;

procedure TWebSocketProtocolBinary.ProcessIncomingFrame(Sender: TWebSocketProcess;
  var request: TWebSocketFrame; const info: RawUTF8);
var jumboInfo: RawByteString;
    n,i: integer;
    frame: TWebSocketFrame;
    P: PByte;
begin
  if FrameIs(request,'frames') then begin
    P := pointer(request.payload);
    inc(P,7); // jump 'frames'FRAME_HEAD_SEP
    n := FromVarUInt32(P);
    for i := 0 to n do begin
      if i=0 then
        jumboInfo:= 'Sec-WebSocket-Frame: [0]' else
      if i=n then
        jumboInfo := 'Sec-WebSocket-Frame: [1]' else
        jumboInfo := '';
      frame.opcode := focBinary;
      frame.content := [];
      frame.payload := FromVarString(P);
      Sender.Log(frame,FormatUTF8('GetSubFrame(%/%)',[i+1,n+1]));
      inherited ProcessIncomingFrame(Sender,frame,jumboInfo);
    end;
  end else
    inherited ProcessIncomingFrame(Sender,request,info);
end;

function TWebSocketProtocolBinary.GetFramesInCompression: integer;
begin
  if (self=nil) or (fFramesInBytes=0) then
    result := 0 else
    result := (fFramesInBytesSocket*100) div fFramesInBytes;
  result := 100-result;
end;

function TWebSocketProtocolBinary.GetFramesOutCompression: integer;
begin
  if (self=nil) or (fFramesOutBytes=0) then
    result := 0 else
    result := (fFramesOutBytesSocket*100) div fFramesOutBytes;
  result := 100-result;
end;

function TWebSocketProtocolBinary.ProcessHandshake(const ExtIn: RawUTF8;
  out ExtOut: RawUTF8; ErrorMsg: PRawUTF8): boolean;
var res: TProtocolResult;
    msgin,msgout: RawUTF8;
begin
  result := false; // not supported
  if fEncryption=nil then
    exit;
  if ExtIn<>'' then
    if IdemPChar(pointer(ExtIn),'SYNHK; HK=') then
      msgin := copy(ExtIn,11,maxInt) else
      exit;
  res := fEncryption.ProcessHandshake(msgin,msgout);
  if res=sprUnsupported then
    exit;
  if res<>sprSuccess then begin
    WebSocketLog.Add.Log(sllWarning,'ProcessHandshake=% In=[%]',[ToText(res)^,msgin],self);
    if ErrorMsg<>nil then
      ErrorMsg^ := FormatUTF8('%: %',[ErrorMsg^,
        GetCaptionFromEnum(TypeInfo(TProtocolResult),ord(res))]);
  end else begin
    FormatUTF8('synhk; hk=%',[msgout],ExtOut);
    result := true;
  end;
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

function TWebSocketProtocolList.AddOnce(
  aProtocol: TWebSocketProtocol): boolean;
var i: Integer;
begin
  result := false;
  if aProtocol=nil then
    exit;
  i := FindIndex(aProtocol.Name,aProtocol.URI);
  if i<0 then begin
    ObjArrayAdd(fProtocols,aProtocol);
    result := true;
  end else begin
    fProtocols[i].Free;
    fProtocols[i] := aProtocol;
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


{ TWebSocketProcess }

constructor TWebSocketProcess.Create(aSocket: TCrtSocket;
  aProtocol: TWebSocketProtocol; aOwnerConnection: Int64; aOwnerThread: TSynThread;
  const aSettings: TWebSocketProcessSettings; const aProcessName: RawUTF8);
begin
  inherited Create;
  fProcessName := aProcessName;
  fSocket := aSocket;
  fProtocol := aProtocol;
  fOwnerConnection := aOwnerConnection;
  fOwnerThread := aOwnerThread;
  fSettings := aSettings;
  fIncoming := TWebSocketFrameList.Create('ws in list');
  fOutgoing := TWebSocketFrameList.Create('ws out list');
  {$ifdef WSLOCKERDEBUGPROCESS}
  fSafeIn := TAutoLockerDebug.Create(WebSocketLog,'ws in process');
  fSafeOut := TAutoLockerDebug.Create(WebSocketLog,'ws out process');
  {$else}
  fSafeIn := TAutoLocker.Create;
  fSafeOut := TAutoLocker.Create;
  {$endif}
end;

destructor TWebSocketProcess.Destroy;
var frame: TWebSocketFrame;
    timeout: Int64;
    log: ISynLog;
begin
  log := WebSocketLog.Enter(self);
  if (fState<>wpsClose) and (fInvalidPingSendCount=0) then
  try
    InterlockedIncrement(fProcessCount);
    fState := wpsDestroy;
    if fOutgoing.Count>0 then
      SendPendingOutgoingFrames;
    frame.opcode := focConnectionClose;
    if SendFrame(frame) then // notify clean closure
      GetFrame(frame,1000,true);  // expects an answer from the other side
  finally
    InterlockedDecrement(fProcessCount);
  end else
    fState := wpsDestroy;
  if fProcessCount>0 then begin
    if log<>nil then
      log.Log(sllDebug,'fProcessCount=%',[fProcessCount],self);
    timeOut := GetTickCount64+5000;
    repeat
      SleepHiRes(2);
    until (fProcessCount=0) or (GetTickCount64>timeOut);
  end;
  fProtocol.Free;
  fOutgoing.Free;
  fIncoming.Free;
  {$ifdef DELPHI5OROLDER}
  fSafeIn.Free;
  fSafeOut.Free;
  {$endif}
  inherited Destroy;
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

function TWebSocketProcess.GetFrame(out Frame: TWebSocketFrame;
  TimeOut: cardinal; IgnoreExceptions: boolean): boolean;
var hdr: TFrameHeader;
    opcode: TWebSocketFrameOpCode;
    masked: boolean;

  procedure GetHeader;
  begin // SockInRead() below raise a ESynBidirSocket error on failure
    FillCharFast(hdr,sizeof(hdr),0);
    fSocket.SockInRead(@hdr.first,2,false);
    opcode := TWebSocketFrameOpCode(hdr.first and 15);
    masked := hdr.len8 and 128<>0;
    if masked then
      hdr.len8 := hdr.len8 and 127;
    if hdr.len8<FRAME_LEN2BYTES then
      hdr.len32 := hdr.len8 else
    if hdr.len8=FRAME_LEN2BYTES then begin
      fSocket.SockInRead(@hdr.len32,2,false);
      hdr.len32 := swap(word(hdr.len32)); // FPC expects explicit word() cast
    end else
    if hdr.len8=FRAME_LEN8BYTES then begin
      fSocket.SockInRead(@hdr.len32,8,false);
      if hdr.len32<>0 then // size is more than 32 bits -> reject
        hdr.len32 := maxInt else
        hdr.len32 := bswap32(hdr.len64);
      if hdr.len32>WebSocketsMaxFrameMB shl 20 then
        raise ESynBidirSocket.CreateUTF8('%.GetFrame: length should be < % MB',
          [self,WebSocketsMaxFrameMB]);
    end;
    if masked then
      fSocket.SockInRead(@hdr.mask,4,false);
  end;

  procedure GetData(var data: RawByteString);
  begin
    SetString(data,nil,hdr.len32);
    fSocket.SockInRead(pointer(data),hdr.len32,false);
    if hdr.mask<>0 then
      ProcessMask(pointer(data),hdr.mask,hdr.len32);
  end;
  
var data: RawByteString;
    pending: integer;
begin
  result := false;
  fSafeIn.Enter;
  try
    pending := fSocket.SockInPending(TimeOut);
    if pending<0 then
      if IgnoreExceptions then
        exit else
        raise ESynBidirSocket.CreateUTF8('SockInPending() Error % on %:%',
          [fSocket.LastLowSocketError,fSocket.Server,fSocket.Port]);
    if pending<2 then
      exit; // not enough data available
    GetHeader;
    Frame.opcode := opcode;
    Frame.content := [];
    GetData(Frame.payload);
    while hdr.first and FRAME_FIN=0 do begin // handle partial payloads
      GetHeader;
      if (opcode<>focContinuation) and (opcode<>Frame.opcode) then
        if IgnoreExceptions then
          exit else
          raise ESynBidirSocket.CreateUTF8('%.GetFrame: received %, expected %',
            [self,ToText(opcode)^,ToText(Frame.opcode)^]);
      GetData(data);
      Frame.payload := Frame.payload+data;
    end;
    if (fProtocol<>nil) and (Frame.payload<>'') then
      fProtocol.AfterGetFrame(Frame);
    {$ifdef HASCODEPAGE}
    if opcode=focText then
      SetCodePage(Frame.payload,CP_UTF8,false); // identify text value as UTF-8
    {$endif}
    Log(frame,'GetFrame');
    SetLastPingTicks;
    result := true;
  finally
    fSafeIn.Leave;
  end;
end;

procedure TWebSocketProcess.ProcessStart;
begin
  if Assigned(fSettings.OnClientConnected) then
  try
    fSettings.OnClientConnected(Self);
  except
  end;
  SetLastPingTicks;
end;

procedure TWebSocketProcess.ProcessStop;
begin
  if Assigned(fSettings.OnClientDisconnected) then
  try
    fSettings.OnClientDisconnected(Self);
  except
  end;
end;

function TWebSocketProcess.SendFrame(
  var Frame: TWebSocketFrame): boolean;
var hdr: TFrameHeader;
    len: cardinal;
begin
  fSafeOut.Enter;
  try
    Log(frame,'SendFrame',sllTrace,true);
    try
      result := true;
      if (fProtocol<>nil) and (Frame.payload<>'') then
        fProtocol.BeforeSendFrame(Frame);
      len := Length(Frame.payload);
      hdr.first := byte(Frame.opcode) or FRAME_FIN;
      if fMaskSentFrames<>0 then begin
        hdr.mask := Random32;
        ProcessMask(pointer(Frame.payload),hdr.mask,len);
      end;
      if len<FRAME_LEN2BYTES then begin
        hdr.len8 := len or fMaskSentFrames;
        fSocket.SockSend(@hdr,2);
      end else
      if len<65536 then begin
        hdr.len8 := FRAME_LEN2BYTES or fMaskSentFrames;
        hdr.len32 := swap(word(len)); // FPC expects explicit word() cast
        fSocket.SockSend(@hdr,4);
      end else begin
        hdr.len8 := FRAME_LEN8BYTES or fMaskSentFrames;
        hdr.len64 := bswap32(len);
        hdr.len32 := 0;
        fSocket.SndLow(@hdr,10+fMaskSentFrames shr 5);
        // huge payload sent outside TCrtSock buffers
        fSocket.SndLow(pointer(Frame.payload),len);
        SetLastPingTicks;
        exit;
      end;
      if fMaskSentFrames<>0 then
        fSocket.SockSend(@hdr.mask,4);
      fSocket.SockSend(pointer(Frame.payload),len);
      fSocket.SockSendFlush; // send at once up to 64 KB
      SetLastPingTicks;
    except
      result := false;
    end;
  finally
    fSafeOut.Leave;
  end;
end;

procedure TWebSocketProcess.HiResDelay(const start: Int64);
var delay: cardinal;
begin
  case GetTickCount64-start of
  0..50:      delay := 0;
  51..200:    delay := 1;
  201..500:   delay := 5;
  501..2000:  delay := 50;
  2001..5000: delay := 100;
  else        delay := 500;
  end;
  if (fSettings.LoopDelay<>0) and (delay>fSettings.LoopDelay) then
    delay := fSettings.LoopDelay;
  SleepHiRes(delay);
end;

function TWebSocketProcess.Settings: PWebSocketProcessSettings;
begin
  result := @fSettings;
end;

function TWebSocketProcess.State: TWebSocketProcessState;
begin
  if self=nil then
    result := wpsCreate else
    result := fState;
end;

function TWebSocketProcess.NotifyCallback(aRequest: THttpServerRequest;
  aMode: TWebSocketProcessNotifyCallback): cardinal;
var request,answer: TWebSocketFrame;
    i: integer;
    start,max: Int64;
begin
  result := STATUS_NOTFOUND;
  if (fProtocol=nil) or (aRequest=nil) or
     not fProtocol.InheritsFrom(TWebSocketProtocolRest) then
    exit;
  if WebSocketLog<>nil then
    WebSocketLog.Add.Log(sllTrace,'NotifyCallback(%,%)',[aRequest.URL,ToText(aMode)^],self);
  TWebSocketProtocolRest(fProtocol).InputToFrame(
    aRequest,aMode in [wscBlockWithoutAnswer,wscNonBlockWithoutAnswer],request);
  case aMode of
  wscNonBlockWithoutAnswer: begin
    // add to the internal sending list for asynchronous sending
    fOutgoing.Push(request);
    result := STATUS_SUCCESS;
    exit;
  end;
  wscBlockWithAnswer:
    if fIncoming.AnswerToIgnore>0 then begin
      WebSocketLog.Add.Log(sllDebug,'NotifyCallback: Waiting for AnswerToIgnore=%',
        [fIncoming.AnswerToIgnore],self);
      max := GetTickCount64+30000;
      repeat
        SleepHiRes(1);
        if fIncoming.AnswerToIgnore=0 then
          break; // it is now safe to send a new 'request'
        if GetTickCount64<max then
          continue;
        self.Log(request,'NotifyCallback AnswerToIgnore TIMEOUT -> abort connection',sllError);
        result := STATUS_NOTIMPLEMENTED; // 501 will force recreate connection
        exit;
      until false;
    end;
  end;
  i := InterlockedIncrement(fProcessCount);
  try
    if (i>2) and (WebSocketLog<>nil) then
      WebSocketLog.Add.Log(sllWarning,'NotifyCallback with fProcessCount=%',[i],self);
    if not SendFrame(request) then
      exit;
    if aMode=wscBlockWithoutAnswer then begin
      result := STATUS_SUCCESS;
      exit;
    end;
    start := GetTickCount64;
    if fSettings.CallbackAnswerTimeOutMS=0 then
      max := start+30000 else // never wait for ever
    if fSettings.CallbackAnswerTimeOutMS<2000 then
      max := start+2000 else // 2 seconds minimal wait 
      max := start+fSettings.CallbackAnswerTimeOutMS;
    while not fIncoming.Pop(fProtocol,'answer',answer) do
      if fState in [wpsDestroy,wpsClose] then begin
        result := STATUS_WEBSOCKETCLOSED;
        exit;
      end else
      if GetTickCount64>max then begin
        WebSocketLog.Add.Log(sllWarning,'NotifyCallback TIMEOUT -> AnswerToIgnore(+1)',self);
        fIncoming.AnswerToIgnore(1); // ignore next 'answer'
        exit;
      end else
        HiResDelay(start);
  finally
    InterlockedDecrement(fProcessCount);
  end;
  result := TWebSocketProtocolRest(fProtocol).FrameToOutput(answer,aRequest);
end;

procedure TWebSocketProcess.SendPendingOutgoingFrames;
begin
  fOutgoing.Safe.Enter;
  try
    if not fProtocol.SendFrames(self,fOutgoing.List,fOutgoing.Count) then
      WebSocketLog.Add.Log(sllError,'SendPendingOutgoingFrames: SendFrames failed',self);
  finally
    fOutgoing.Safe.Leave;
  end;
end;

procedure TWebSocketProcess.ProcessLoop;
var request: TWebSocketFrame;
    elapsed: cardinal;
begin
  if fProtocol=nil then
    exit;
  ProcessStart;
  fState := wpsRun;
  while (fState<>wpsDestroy) and
        not TThreadHook(fOwnerThread).Terminated do
    try
      InterlockedIncrement(fProcessCount);
      try
        if GetFrame(request,1,false) then begin
          case request.opcode of
          focPing: begin
            request.opcode := focPong;
            SendFrame(request);
          end;
          focPong:
            continue;
          focText,focBinary:
            fProtocol.ProcessIncomingFrame(self,request,'');
          focConnectionClose: begin
            if fState=wpsRun then begin
              SendFrame(request);
              fState := wpsClose;
            end;
            break; // will close the connection
          end;
          end;
        end else
        if TThreadHook(fOwnerThread).Terminated then
          break else begin
          elapsed := LastPingDelay;
          if (elapsed>0) and (fOutgoing.Count>0) then
            SendPendingOutgoingFrames else
          if (fSettings.HeartbeatDelay<>0) and
             (elapsed>fSettings.HeartbeatDelay) then begin
            request.opcode := focPing;
            if not SendFrame(request) then
              if (fSettings.DisconnectAfterInvalidHeartbeatCount<>0) and
                 (fInvalidPingSendCount>=fSettings.DisconnectAfterInvalidHeartbeatCount) then begin
                fState := wpsClose;
                break; // will close the connection
              end else
                SetLastPingTicks(true); // mark invalid, and avoid immediate retry
          end;
        end;
      finally
        request.payload := '';
        InterlockedDecrement(fProcessCount);
      end;
      HiResDelay(fLastSocketTicks);
    except
      fState := wpsClose;
      break; // don't be optimistic: forced close connection
    end;
  ProcessStop;
end;

procedure TWebSocketProcess.SetLastPingTicks(invalidPing: boolean);
begin
  Safe.Lock;
  fLastSocketTicks := GetTickCount64;
  if invalidPing then
    inc(fInvalidPingSendCount) else
    fInvalidPingSendCount := 0;
  Safe.UnLock;
end;

function TWebSocketProcess.LastPingDelay: Int64;
begin
  Safe.Lock;
  result := GetTickCount64-fLastSocketTicks;
  Safe.UnLock;
end;

procedure LogEscape(const s: RawUTF8; var result: RawUTF8);
var i,L: integer;
const MAXLEN = 200;
begin
  with TTextWriter.CreateOwnedStream(MAXLEN*3) do
  try
    L := length(s);
    if L>MAXLEN then
      L := MAXLEN;
    for i := 1 to L do
      if s[i] in [' '..#126] then
        Add(s[i]) else begin
        Add('$');
        AddByteToHex(ord(s[i]));
      end;
    if L=MAXLEN then
      AddShort('...');
    SetText(result);
  finally
    Free;
  end;
end;

procedure TWebSocketProcess.Log(const frame: TWebSocketFrame;
  const aMethodName: RawUTF8; aEvent: TSynLogInfo; DisableRemoteLog: Boolean);
var content: RawUTF8;
    log: TSynLog;
begin
  if WebSocketLog<>nil then
  with WebSocketLog.Family do
  if aEvent in Level then
  if (logHeartbeat in fSettings.LogDetails) or
     not (frame.opcode in [focPing,focPong]) then begin
    log := SynLog;
    log.DisableRemoteLog(DisableRemoteLog);
    try
      if (frame.opcode=focText) and
         (logTextFrameContent in fSettings.LogDetails) then
        log.Log(aEvent,'% type=% focText %',[aMethodName,
          Protocol.FrameType(frame),frame.PayLoad],self) else begin
        if logBinaryFrameContent in fSettings.LogDetails then
          LogEscape(frame.PayLoad,content);
        log.Log(aEvent,'% type=% % len=% %',[aMethodName,Protocol.FrameType(frame),
          ToText(frame.opcode)^,length(frame.PayLoad),content],self);
      end;
    finally
      log.DisableRemoteLog(false);
    end;
  end;
end;


{ -------------- WebSockets Server classes for bidirectional remote access }

{ TWebSocketServer }

constructor TWebSocketServer.Create(const aPort: SockString; OnStart,OnStop: TNotifyThreadEvent;
  const ProcessName: SockString {$ifdef USETHREADPOOL}; ServerThreadPoolCount: integer{$endif});
begin
  fThreadRespClass := TWebSocketServerResp;
  fWebSocketConnections := TObjectListLocked.Create(false);
  fProtocols := TWebSocketProtocolList.Create;
  fSettings.SetDefaults;
  fSettings.HeartbeatDelay := 20000;
  fCanNotifyCallback := true;
  inherited Create(aPort,OnStart,OnStop,ProcessName{$ifdef USETHREADPOOL},0{$endif}); // NO thread pool
end;

function TWebSocketServer.WebSocketProcessUpgrade(ClientSock: THttpServerSocket;
  Context: TWebSocketServerResp): integer;
var upgrade,uri,version,protocol,key,extin,extout: RawUTF8;
    P: PUTF8Char;
    Digest: TSHA1Digest;
    prot: TWebSocketProtocol;
    i: integer;
begin
  result := STATUS_BADREQUEST;
  if Context.fProcess<>nil then
    exit; // already upgraded
  upgrade := ClientSock.HeaderValue('Upgrade');
  if not IdemPropNameU(upgrade,'websocket') then
    exit;
  version := ClientSock.HeaderValue('Sec-WebSocket-Version');
  if GetInteger(pointer(version))<13 then
    exit; // we expect WebSockets protocol version 13 at least
  uri := Trim(RawUTF8(ClientSock.URL));
  if (uri<>'') and (uri[1]='/') then
    Delete(uri,1,1);
  protocol := ClientSock.HeaderValue('Sec-WebSocket-Protocol');
  P := pointer(protocol);
  if P<>nil then
    repeat
      prot := fProtocols.CloneByName(trim(GetNextItem(P)),uri);
    until (P=nil) or (prot<>nil) else
    // if no protocol is specified, try to match by URI
    prot := fProtocols.CloneByURI(uri);
  if prot=nil then
    exit;
  prot.fRemoteIP := ClientSock.RemoteIP;
  prot.fRemoteLocalhost := ClientSock.RemoteIP='127.0.0.1';
  extin := ClientSock.HeaderValue('Sec-WebSocket-Extensions');
  if (extin<>'') and not prot.ProcessHandshake(extin,extout,nil) then begin
    prot.Free;
    result := STATUS_UNAUTHORIZED;
    exit;
  end;
  Context.fProcess := TWebSocketProcessServer.Create(
    ClientSock,prot,Context.ConnectionID,Context,fSettings,fProcessName);
  Context.fProcess.fServerResp := Context;
  key := ClientSock.HeaderValue('Sec-WebSocket-Key');
  if Base64ToBinLengthSafe(pointer(key),length(key))<>16 then begin
    prot.Free;
    exit; // this nonce must be a Base64-encoded value of 16 bytes
  end;
  ComputeChallenge(key,Digest);
  ClientSock.SockSend(['HTTP/1.1 101 Switching Protocols'#13#10+
    'Upgrade: websocket'#13#10'Connection: Upgrade'#13#10+
    'Sec-WebSocket-Protocol: ',prot.Name,#13#10+
    'Sec-WebSocket-Accept: ',BinToBase64(@Digest,sizeof(Digest))]);
  if extout<>'' then
    ClientSock.SockSend(['Sec-WebSocket-Extensions: ',extout]);
  ClientSock.SockSend;
  ClientSock.SockSendFlush;
  result := STATUS_SUCCESS; // connection upgraded: never back to HTTP/1.1
  fWebSocketConnections.Safe.Lock;
  fWebSocketConnections.Add(Context);
  fWebSocketConnections.Safe.UnLock;
  try
    Context.fProcess.ProcessLoop;
    ClientSock.KeepAliveClient := false; // always close connection
  finally
    FreeAndNil(Context.fProcess); // notify end of WebSockets
    fWebSocketConnections.Safe.Lock;
    try
      i := fWebSocketConnections.IndexOf(Context);
      if i>=0 then
        fWebSocketConnections.Delete(i);
    finally
      fWebSocketConnections.Safe.UnLock;
    end;
  end;
end;

procedure TWebSocketServer.Process(ClientSock: THttpServerSocket;
  ConnectionID: integer; ConnectionThread: TSynThread);
var err: integer;
begin
  if ClientSock.ConnectionUpgrade and ClientSock.KeepAliveClient and
     IdemPropName('GET',pointer(ClientSock.Method),length(ClientSock.Method)) and
     ConnectionThread.InheritsFrom(TWebSocketServerResp) then begin
    err := WebSocketProcessUpgrade(ClientSock,TWebSocketServerResp(ConnectionThread));
    if err<>STATUS_SUCCESS then begin
      ClientSock.SockSend(['HTTP/1.0 ',err,' WebSocket Upgrade Error'#13#10+
        'Connection: Close'#13#10]);
      ClientSock.SockSendFlush;
      ClientSock.KeepAliveClient := false;
    end;
  end else
    inherited Process(ClientSock,ConnectionID,ConnectionThread);
end;

destructor TWebSocketServer.Destroy;
begin
  inherited Destroy; // close any pending connection
  fWebSocketConnections.Free;
  fProtocols.Free;
end;

function TWebSocketServer.Settings: PWebSocketProcessSettings;
begin
  result := @fSettings;
end;

function TWebSocketServer.IsActiveWebSocket(ConnectionThread: TSynThread): TWebSocketServerResp;
var connectionIndex: Integer;
begin
  result := nil;
  if Terminated or (ConnectionThread=nil) then
    exit;
  fWebSocketConnections.Safe.Lock;
  connectionIndex := fWebSocketConnections.IndexOf(ConnectionThread);
  fWebSocketConnections.Safe.UnLock;
  if (connectionIndex>=0) and
     ConnectionThread.InheritsFrom(TWebSocketServerResp) then
    //  this request is a websocket, on a non broken connection
    result := TWebSocketServerResp(ConnectionThread);
end;

function TWebSocketServer.IsActiveWebSocket(ConnectionID: integer): TWebSocketServerResp;
var i: Integer;
begin
  result := nil;
  if Terminated or (ConnectionID<=0) then
    exit;
  fWebSocketConnections.Safe.Lock;
  try
    with fWebSocketConnections do
    for i := 0 to Count-1 do
      if TWebSocketServerResp(List[i]).ConnectionID=ConnectionID then begin
        result := TWebSocketServerResp(List[i]);
        exit;
      end;
  finally
    fWebSocketConnections.Safe.UnLock;
  end;
end;


{ TWebSocketServerRest }

constructor TWebSocketServerRest.Create(const aPort: SockString;
  OnStart,OnStop: TNotifyThreadEvent; const aProcessName,
  aWebSocketsURI, aWebSocketsEncryptionKey: RawUTF8; aWebSocketsAJAX: boolean);
begin
  Create(aPort,OnStart,OnStop,aProcessName);
  WebSocketsEnable(aWebSocketsURI,aWebSocketsEncryptionKey,aWebSocketsAJAX);
end;

procedure TWebSocketServerRest.WebSocketsEnable(const aWebSocketsURI,
  aWebSocketsEncryptionKey: RawUTF8; aWebSocketsAJAX,aWebSocketsCompressed: boolean);
begin
  if self=nil  then
    exit;
  fProtocols.AddOnce(TWebSocketProtocolBinary.Create(
    aWebSocketsURI,true,aWebSocketsEncryptionKey,aWebSocketsCompressed));
  if aWebSocketsAJAX then
    fProtocols.AddOnce(TWebSocketProtocolJSON.Create(aWebSocketsURI));
end;

function TWebSocketServerRest.Callback(
  Ctxt: THttpServerRequest; aNonBlocking: boolean): cardinal;
var connection: TWebSocketServerResp;
    mode: TWebSocketProcessNotifyCallback;
begin
  if Ctxt=nil then
    connection := nil else begin
    WebSocketLog.Add.Log(sllTrace,'Callback(%) on socket=%',
      [Ctxt.URL,Ctxt.ConnectionID],self);
    connection := IsActiveWebSocket(Ctxt.ConnectionID);
  end;
  if connection<>nil then begin
    //  this request is a websocket, on a non broken connection
    if aNonBlocking then // see TInterfacedObjectFakeServer.CallbackInvoke
      mode := wscNonBlockWithoutAnswer else
      mode := wscBlockWithAnswer;
    result := connection.NotifyCallback(Ctxt,mode);
  end else begin
    WebSocketLog.Add.Log(sllError,'Callback() on inactive socket',self);
    result := STATUS_NOTFOUND;
  end;
end;


{ TWebSocketServerResp }

constructor TWebSocketServerResp.Create(aServerSock: THttpServerSocket;
  aServer: THttpServer {$ifdef USETHREADPOOL}; aThreadPool: TSynThreadPoolTHttpServer{$endif});
begin
  if not aServer.InheritsFrom(TWebSocketServer) then
    raise ESynBidirSocket.CreateUTF8('%.Create(%: TWebSocketServer?)',[self,aServer]);
  inherited Create(aServerSock,aServer{$ifdef USETHREADPOOL},aThreadPool{$endif});
end;

function TWebSocketServerResp.NotifyCallback(Ctxt: THttpServerRequest;
  aMode: TWebSocketProcessNotifyCallback): cardinal;
begin
  if fProcess=nil then
    result := STATUS_NOTFOUND else begin
    result := fProcess.NotifyCallback(Ctxt,aMode);
    if result=STATUS_WEBSOCKETCLOSED then begin
      WebSocketLog.Add.Log(sllError,'NotifyCallback on closed connection',self);
      ServerSock.KeepAliveClient := false; // force close the connection
      result := STATUS_NOTFOUND;
    end;
  end;
end;

function TWebSocketServerResp.WebSocketProtocol: TWebSocketProtocol;
begin
  if (Self=nil) or (fProcess=nil) then
    result := nil else
    result := fProcess.Protocol;
end;


{ TWebSocketProcessServer }

function TWebSocketProcessServer.ComputeContext(
  out RequestProcess: TOnHttpServerRequest): THttpServerRequest;
begin
  result := THttpServerRequest.Create(
    (fOwnerThread as TWebSocketServerResp).fServer,fOwnerConnection,fOwnerThread);
  RequestProcess := TWebSocketServerResp(fOwnerThread).fServer.Request;
end;

procedure TWebSocketProcessServer.ProcessStart;
var frame: TWebSocketFrame;
begin // notify e.g. TOnWebSocketProtocolChatIncomingFrame
  inherited ProcessStart;
  frame.opcode := focContinuation;
  fProtocol.ProcessIncomingFrame(self,frame,'');
end;

procedure TWebSocketProcessServer.ProcessStop;
var frame: TWebSocketFrame;
begin // notify e.g. TOnWebSocketProtocolChatIncomingFrame
  frame.opcode := focConnectionClose;
  fProtocol.ProcessIncomingFrame(self,frame,'');
  inherited ProcessStop;
end;


{ -------------- WebSockets Client classes for bidirectional remote access }

{ THttpClientWebSockets }

constructor THttpClientWebSockets.Create(aTimeOut: cardinal);
begin
  inherited;
  fSettings.SetDefaults;
  fSettings.CallbackAnswerTimeOutMS := aTimeOut;
end;

destructor THttpClientWebSockets.Destroy;
begin
  FreeAndNil(fProcess);
  inherited;
end;

function THttpClientWebSockets.Request(const url, method: SockString;
  KeepAlive: cardinal; const header, Data, DataType: SockString;
  retry: boolean): integer;
var Ctxt: THttpServerRequest;
    block: TWebSocketProcessNotifyCallback;
    resthead: RawUTF8;
begin
  if fProcess<>nil then
    if fProcess.fClientThread.fThreadState>sRun then
      // WebSockets closed by server side
      result := STATUS_NOTIMPLEMENTED else begin
      // send the REST request over WebSockets
      Ctxt := THttpServerRequest.Create(
        nil,fProcess.fOwnerConnection,fProcess.fOwnerThread);
      try
        Ctxt.Prepare(url,method,header,data,dataType,'');
        resthead := FindIniNameValue(Pointer(header),'SEC-WEBSOCKET-REST: ');
        if resthead='NonBlocking' then
          block := wscNonBlockWithoutAnswer else
          block := wscBlockWithAnswer;
        result := fProcess.NotifyCallback(Ctxt,block);
        if IdemPChar(pointer(Ctxt.OutContentType), JSON_CONTENT_TYPE_UPPER) then
          HeaderSetText(Ctxt.OutCustomHeaders) else
          HeaderSetText(Ctxt.OutCustomHeaders,Ctxt.OutContentType);
        Content := Ctxt.OutContent;
        ContentType := Ctxt.OutContentType;
        ContentLength := length(Ctxt.OutContent);
      finally
        Ctxt.Free;
      end;
    end else
    // standard HTTP/1.1 REST request
    result := inherited Request(url,method,KeepAlive,header,Data,DataType,retry);
end;

procedure THttpClientWebSockets.SetInt32OptionByIndex(OptName, OptVal: integer);
begin
  inherited SetInt32OptionByIndex(OptName,OptVal);
  if OptName=SO_RCVTIMEO then
    fSettings.CallbackAnswerTimeOutMS := OptVal;
end;

function THttpClientWebSockets.Settings: PWebSocketProcessSettings;
begin
  result := @fSettings;
end;

function THttpClientWebSockets.WebSocketsUpgrade(const aWebSocketsURI,
  aWebSocketsEncryptionKey: RawUTF8; aWebSocketsAJAX,aWebSocketsCompression: boolean): RawUTF8;
var protocol: TWebSocketProtocolRest;
    key: TAESBlock;
    bin1,bin2: RawByteString;
    extin,extout: RawUTF8;
    cmd: SockString;
    digest1,digest2: TSHA1Digest;
begin
  if fProcess<>nil then begin
    result := 'Already upgraded to WebSockets';
    if IdemPropNameU(fProcess.Protocol.URI,aWebSocketsURI) then
      result := result+' on this URI' else
      result := FormatUTF8('% with URI="%" but requested "%"',
        [result,fProcess.Protocol.URI,aWebSocketsURI]);
    exit;
  end;
  try
    if aWebSocketsAJAX then
      protocol := TWebSocketProtocolJSON.Create(aWebSocketsURI) else
      protocol := TWebSocketProtocolBinary.Create(
        aWebSocketsURI,false,aWebSocketsEncryptionKey,aWebSocketsCompression);
    try
      RequestSendHeader(aWebSocketsURI,'GET');
      TAESPRNG.Main.FillRandom(key);
      bin1 := BinToBase64(@key,sizeof(key));
      SockSend(['Content-Length: 0'#13#10'Connection: Upgrade'#13#10+
        'Upgrade: websocket'#13#10'Sec-WebSocket-Key: ',bin1,#13#10+
        'Sec-WebSocket-Protocol: ',protocol.Name,#13#10+
        'Sec-WebSocket-Version: 13']);
      if protocol.ProcessHandshake('',extout,nil) then
        SockSend(['Sec-WebSocket-Extensions: ',extout]);
      SockSend;
      SockSendFlush;
      SockRecvLn(cmd);
      GetHeader;
      result := 'Invalid HTTP Upgrade Header';
      if not IdemPChar(pointer(cmd),'HTTP/1.1 101') or
         not ConnectionUpgrade or (ContentLength>0) or
         not IdemPropNameU(HeaderValue('upgrade'),'websocket') or
         not IdemPropNameU(HeaderValue('Sec-WebSocket-Protocol'),protocol.Name) then
        exit;
      result := 'Invalid HTTP Upgrade Accept Challenge';
      ComputeChallenge(bin1,digest1);
      bin2 := HeaderValue('Sec-WebSocket-Accept');
      if not Base64ToBin(pointer(bin2),@digest2,length(bin2),sizeof(digest2),false) or
         not CompareMem(@digest1,@digest2,SizeOf(digest1)) then
        exit;
      if extout<>'' then begin
        result := 'Invalid HTTP Upgrade ProcessHandshake';
        extin := HeaderValue('Sec-WebSocket-Extensions');
        if (extin='') or not protocol.ProcessHandshake(extin,extout,@result) then
          exit;
      end;
      // if we reached here, connection is successfully upgraded to WebSockets
      if (Server='localhost') or (Server='127.0.0.1') then begin
        protocol.fRemoteIP := '127.0.0.1';
        protocol.fRemoteLocalhost := true;
      end else
        protocol.fRemoteIP := Server;
      result := ''; // no error message = success
      fProcess := TWebSocketProcessClient.Create(self,protocol,fProcessName);
      protocol := nil; // protocol will be owned by fProcess now
    finally
      protocol.Free;
    end;
  except
    on E: Exception do begin
      FreeAndNil(fProcess);
      FormatUTF8('%: %',[E,E.Message],result);
    end;
  end;
end;


{ TWebSocketProcessClient }

constructor TWebSocketProcessClient.Create(aSender: THttpClientWebSockets;
  aProtocol: TWebSocketProtocol; const aProcessName: RawUTF8);
begin
  fMaskSentFrames := 128;
  inherited Create(aSender,aProtocol,0,nil,aSender.fSettings,aProcessName);
  // initialize the thread after everything is set (Execute may be instant)
  fClientThread := TWebSocketProcessClientThread.Create(self);
end;

destructor TWebSocketProcessClient.Destroy;
begin
  // focConnectionClose would be handled in this thread -> close client thread
  fClientThread.Terminate;
  while fClientThread.fThreadState<sFinished do
    SleepHiRes(1);
  fClientThread.fProcess := nil;
  // SendPendingOutgoingFrames + SendFrame/GetFrame(focConnectionClose)
  inherited Destroy;
  fClientThread.Free;
end;

function TWebSocketProcessClient.ComputeContext(
  out RequestProcess: TOnHttpServerRequest): THttpServerRequest;
begin
  result := THttpServerRequest.Create(nil,0,fOwnerThread);
  RequestProcess := (fSocket as THttpClientWebSockets).fOnCallbackRequestProcess;
end;


{ TWebSocketProcessClientThread }

constructor TWebSocketProcessClientThread.Create(
  aProcess: TWebSocketProcessClient);
begin
  fProcess := aProcess;
  fProcess.fOwnerThread := self;
  inherited Create(false);
end;

procedure TWebSocketProcessClientThread.Execute;
begin
  SetCurrentThreadName('% % %',[self,fProcess.fProcessName,fProcess.Protocol.Name]);
  fThreadState := sRun;
  if not Terminated then
    fProcess.ProcessLoop;
  if (fProcess<>nil) and (fProcess.fState=wpsClose) then
    fThreadState := sClosed else
    fThreadState := sFinished;
  WebSocketLog.Add.Log(sllDebug,'Execute finished: ThreadState=%',[ToText(fThreadState)^],self);
  if (fProcess<>nil) and (fProcess.Socket<>nil) and
     fProcess.Socket.InheritsFrom(THttpClientWebSockets) then
    with THttpClientWebSockets(fProcess.Socket) do
      if Assigned(OnWebSocketsClosed) then
        OnWebSocketsClosed(self);
end;


end.
