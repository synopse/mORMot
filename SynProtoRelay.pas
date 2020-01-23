/// implements asynchronous safe WebSockets tunnelling
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynProtoRelay;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2020 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

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

  Portions created by the Initial Developer are Copyright (C) 2020
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


  Secured WebSockets tunneling
  ----------------------------

 It will encapsulate any WebSockets duplex stream over a public server,
 allowing any remote client to connect to a local server behind a firewall,
 using a public server (e.g. a simple Linux box) as relay.

 A Private Relay client should connect to a Public Relay Server, probably
 behind a firewall. By definition, only a single Private Relay client instance
 could connect at the same time to the Public Relay server.

 ORM/SOA client nodes don't need to be modified. Their WebSockets connection
 will just point to the Public Relay Server, which will tunnel their frames to
 the Private Relay Server, which will maintain a set of local connections
 to the main business Server, behind the firewal.


                         |                               |
                  REMOTE |                               |CORPORATE
                  USERS  |                               |FIREWALL
                         |                               |                                           +------------+
                         |                               |                                           | client 3   |
                         |                               |                                           |            |
    +------------+       |                               |                                           +------------+
    | client 1   |       |                               |                                                  |
    |            |-\     |                               |     +------------------------------------------------------------+
    +------------+  -\   |        PUBLIC RELAY           |     |      PRIVATE RELAY                         |               |
                      --\|    +--------------------+     |     |  +--------------------+        +-----------v-----------+   |
                         |-\  |                    |     |     |  |                    |        |                       |   |
                         |  -\|                    |     |     |  |                ------------>|                       |   |
                         |    ->+---------------+  |     |     |  |  +-------------+   |        |    mORMot ORM/SOA     |   |
                         |    | | encapsulation |<-------------|-----|decapsulation|   |        |       SERVER          |   |
-------------------------|    ->+---------------+  |     |     |  |  +-------------+   |        |                       |   |
                         |  -/|                    |     |     |  |                ------------>|       with local DB   |   |
                         |-/  |                    |     |     |  |                    |        |                       |   |
                       --|    +--------------------+     |     |  +--------------------+        +-----------------------+   |
                     -/  |                               |     |           in-process communication         ^               |
    +------------+ -/    |                               |     +------------------------------------------------------------+
    | client 2   |       |                               |                                                  |
    |            |       |                               |                                            +------------+
    +------------+       |          internet             |        corporate local network             | client 4   |
                         |                               |                                            |            |
                         |                               |                                            +------------+
                         |                               |
                         |                               |

 In the above diagram, client 1 and client 2 just connect to the Public Relay
 server, and the mORMot ORM/SOA server see some connection coming as if they
 were local - just like client 3 and client 4. Only a higher latency and
 lower bandwidth may be noticeable, but REST is not very demanding about
 latency, and our WebSockets protocols has built-in SynLZ compression.

 One benefit of this solution is that it doesn't require to mess with the
 firewall. There is a single Public Relay process (using very few resources)
 to be setup somewhere in the Internet, and the Private Relay client could
 run in-process with the main mORMot ORM/SOA server, so nothing is to be
 changed by the corporate IT.

 The regular TWebSocketProtocolBinary encryption could be used, so no
 certificate nor security need to be setup in the Public Relay server,
 which doesn't uncipher the frames, but just work as a dummy relayer.

 All WebSockets communication starts as HTTP/1.1 regular clients to the
 Internet, before switch to some duplex encrypted stream.

 The communication between the Public Relay and the Private Relay
 is checked by validating each transmitted frames, and can be over-encrypted
 using a secret password, for even additional security.

 If any node (client, Public Relay, Private Relay, mORMot SOA/ORM server) is
 down for whatever reason (e.g. network failure), both ends will be notified
 with no dandling sockets.

 See Sample "38 - WebSockets Relay" for a stand-alone Public Relay project,
 which could be reused as plain binary - no recompilation is needed.

}

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  SynWinSock,
  {$else}
  {$ifdef KYLIX3}
  LibC,
  {$endif}
  SynFPCSock, // shared with Kylix
  {$endif}
  SysUtils,
  SynCommons,
  SynTable,
  SynCrtSock,
  SynBidirSock,
  SynCrypto,
  SynLog;

type
  ERelayProtocol = class(ESynException);

  TPublicRelay = class;
  TPrivateRelay = class;

{ ----------------- low-level WebSockets protocols involved ------------ }

  /// regular mORMot client to Public Relay connection using
  // synopsejson/synopsebin/synopsebinary protocols
  // - any incoming frame will be encapsulated with the connection ID, then
  // relayed to the Private Relay node using TRelayServerProtocol
  TSynopseServerProtocol = class(TWebSocketProtocol)
  protected
    fOwner: TPublicRelay;
    procedure ProcessIncomingFrame(Sender: TWebSocketProcess;
      var Frame: TWebSocketFrame; const info: RawUTF8); override;
    // implements SynBidirSock's TWebSocketProtocolRest variants
    function GetSubprotocols: RawUTF8; override;
    function SetSubprotocol(const aProtocolName: RawUTF8): boolean; override;
  public
    /// initialize the protocol to be processed on a given TPublicRelay
    constructor Create(aOwner: TPublicRelay); reintroduce;
    /// used server-side for any new connection
    function Clone(const aClientURI: RawUTF8): TWebSocketProtocol; override;
  end;

  /// Public Relay to Private Relay connection, encapsulating connection ID
  // - see also TRelayClientProtocol as the reversed process
  // - any incoming frame will be decapsulate the associated connection ID, then
  // relayed to the proper client node using TSynopseServerProtocol
  TRelayServerProtocol = class(TWebSocketProtocol)
  protected
    fOwner: TPublicRelay;
    procedure ProcessIncomingFrame(Sender: TWebSocketProcess;
      var Frame: TWebSocketFrame; const info: RawUTF8); override;
  public
    /// initialize the protocol to be processed on a given TPublicRelay
    constructor Create(aOwner: TPublicRelay; const aServerKey: RawUTF8); reintroduce;
    /// used server-side for any new connection
    function Clone(const aClientURI: RawUTF8): TWebSocketProtocol; override;
  end;

  /// Private Relay to Public Relay connection, decapsulating connection ID
  // - see also TRelayServerProtocol as the reversed process
  TRelayClientProtocol = class(TWebSocketProtocol)
  protected
    fOwner: TPrivateRelay;
    procedure ProcessIncomingFrame(Sender: TWebSocketProcess;
      var Frame: TWebSocketFrame; const info: RawUTF8); override;
  public
    /// initialize the protocol to be processed on a given TPrivateRelay
    constructor Create(aOwner: TPrivateRelay; const aRelayKey: RawUTF8); reintroduce;
    /// used server-side for any new connection
    function Clone(const aClientURI: RawUTF8): TWebSocketProtocol; override;
  end;

  /// Private Relay to local Server connection
  // - forward raw frames from TSynopseServerProtocol
  TSynopseClientProtocol = class(TWebSocketProtocol)
  protected
    fOwner: TPrivateRelay;
    procedure ProcessIncomingFrame(Sender: TWebSocketProcess;
      var Frame: TWebSocketFrame; const info: RawUTF8); override;
  public
    /// initialize the protocol to be processed on a given TPrivateRelay
    // - the protocol is relayed from TRelayClientProtocol.ProcessIncomingFrame
    constructor Create(aOwner: TPrivateRelay; const aProtocolName: RawUTF8); reintroduce;
    /// used server-side for any new connection
    function Clone(const aClientURI: RawUTF8): TWebSocketProtocol; override;
  end;


{ ----------------- Public and Private relay process ------------ }

  TAbstractRelay = class(TSynPersistentLock)
  protected
    fLog: TSynLogClass;
    fStarted: RawUTF8;
    fSent, fReceived: QWord;
    fFrames, fValid, fInvalid, fRejected: integer;
    function EncapsulateAndSend(Process: TWebSocketProcess; const IP: RawUTF8;
      const Frame: TWebSocketFrame; Connection: integer): boolean;
    function Decapsulate(Protocol: TWebSocketProtocol; var Frame: TWebSocketFrame): integer;
  public
  published
    property Started: RawUTF8 read fStarted;
    property Frames: integer read fFrames;
    property Sent: QWord read fSent;
    property Received: QWord read fReceived;
    property Valid: integer read fValid;
    property Invalid: integer read fInvalid;
    property Rejected: integer read fRejected;
  end;

  /// implements a Public Relay server, e.g. located on a small Linux/BSD box
  TPublicRelay = class(TAbstractRelay)
  protected
    fServerJWT: TJWTAbstract;
    fClients, fServer: TWebSocketServer;
    fServerConnected: TWebSocketProcess;
    fStatCache: RawJSON;
    fStatTix: integer;
    function OnBeforeBody(const aURL,aMethod,aInHeaders, aInContentType,
      aRemoteIP: SockString; aContentLength: integer; aUseSSL: boolean): cardinal;
    function OnRequest(Ctxt: THttpServerRequest): cardinal;
    function GetStats: RawJSON;
  public
    /// initialize the Public Relay
    // - WebSockets clients will connect to aClientsPort as usual
    // - all communication will be encapsulated and relayed to aServerPort,
    // using the optional aServerKey for TWebSocketProtocol.SetEncryptKey(),
    // and aServerJWT to authenticate the incoming connection (owned by this instance)
    constructor Create(aLog: TSynLogClass; const aClientsPort, aServerPort: SockString;
      const aServerKey: RawUTF8; aServerJWT: TJWTAbstract); reintroduce;
    /// finalize the Public Relay server
    destructor Destroy; override;
    /// access to the JWT authentication for TPrivateRelay communication
    property ServerJWT: TJWTAbstract read fServerJWT;
  published
    /// raw access to the Private Relay server instance
    property Server: TWebSocketServer read fServer;
    /// raw access to the ORM/SOA clients server instance
    property Clients: TWebSocketServer read fClients;
  end;

  /// implements a relayed link to the local ORM/SOA server instance
  // - add some internal fields for TPrivateRelay.fServers[]
  TServerClient = class(THttpClientWebSockets)
  protected
    Connection: integer;
    OriginIP: RawUTF8;
  end;
  TServerClients = array of TServerClient;

  /// implements a Private Relay client, located in a private network behind
  // a restricted firewall
  TPrivateRelay = class(TAbstractRelay)
  protected
    fRelayHost, fRelayPort, fRelayKey, fRelayCustomHeaders,
    fServerHost, fServerPort, fServerRemoteIPHeader: RawUTF8;
    fRelayClient: THttpClientWebSockets; // using TRelayClientProtocol
    fServers: TServerClients; // links to local ORM/SOA server
    fServersCount: integer;
    function FindServerClientByConnection(connection: integer; out index: integer): TServerClient;
    function FindServerClientByProcess(process: TWebSocketProcess; out index: integer): TServerClient;
    function NewServerClient(connection: integer; const ipprotocoluri: RawUTF8): TServerClient;
  public
    /// initialize the Private Relay
    // - communication will be encapsulated and relayed to aServerHost/aServerPort
    // via a new TServerClient connection, using aServerWebSocketsURI,
    // aServerWebSocketsEncryptionKey, aServerWebSocketsAJAX and
    // aServerWebSocketsCompression parameters as any regular client in the
    // local network - from the server point of view, those clients will
    // appear like local clients unless ServerRemoteIPHeader is set according
    // to the TSQLHttpServerDefinition.ServerRemoteIPHeader value (e.g. as
    // 'X-Real-IP') and the remote client IP will be used instead
    // - Connected/TryConnect should be called on a regular basis to connect to
    // the Public Relay using aRelayHost/aRelayPort/aRelayKey/aRelayBearer
    constructor Create(aLog: TSynLogClass;
      const aRelayHost, aRelayPort, aRelayKey, aRelayBearer,
      aServerHost, aServerPort, aServerRemoteIPHeader: RawUTF8); reintroduce;
    /// check if this Private Relay is actually connected to the Public Relay
    function Connected: boolean;
    /// true if this Private Relay uses encryption with the Public Relay
    function Encrypted: boolean;
    /// (re)connect to aRelayHost/aRelayPort Public Relay via a single link
    // - will first disconnect is needed
    function TryConnect: boolean;
    /// disconnect from aRelayHost/aRelayPort Public Relay
    procedure Disconnect;
    /// finalize the Private Relay server
    destructor Destroy; override;
  published
    /// Public Relay host address
    property RelayHost: RawUTF8 read fRelayHost;
    /// Public Relay host port
    property RelayPort: RawUTF8 read fRelayPort;
    /// true if the Private Relay is connected to the Public Relay
    property RelayConnected: boolean read Connected;
    /// true if the Private Relay to the Public Relay link is encrypted
    property RelayEncrypted: boolean read Encrypted;
    /// local processing server host address
    property ServerHost: RawUTF8 read fServerHost;
    /// local processing server host port
    property ServerPort: RawUTF8 read fServerPort;
    /// how many client connections are actually relayed via this instance
    property Connections: integer read fServersCount;
  end;




implementation


{ TAbstractRelay }

const
  RELAYFRAME_VER = $aa00;

type
  TRelayFrame = packed record
    revision: word;
    opcode: TWebSocketFrameOpCode;
    content: TWebSocketFramePayloads;
    connection: integer; // see TWebSocketServer.IsActiveWebSocket()
    payload: RawByteString;
  end;
  PRelayFrame = ^TRelayFrame;

function TAbstractRelay.EncapsulateAndSend(Process: TWebSocketProcess;
  const IP: RawUTF8; const Frame: TWebSocketFrame; Connection: integer): boolean;
var
  encapsulated: TRelayFrame;
  dest: TWebSocketFrame;
  trigger: integer;
begin
  result := false;
  if (Process = nil) or (Connection = 0) then
    exit;
  encapsulated.revision := RELAYFRAME_VER;
  encapsulated.opcode := Frame.opcode;
  encapsulated.content := [];
  encapsulated.connection := Connection;
  encapsulated.payload := Frame.payload;
  encapsulated.payload := RecordSave(encapsulated, TypeInfo(TRelayFrame));
  if encapsulated.opcode = focText then
    trigger := 512
  else
    trigger := maxInt; // no compression, just crc32c to avoid most attacks
  encapsulated.payload := AlgoSynLZ.Compress(encapsulated.payload, trigger);
  dest.opcode := focBinary;
  dest.content := [];
  if (Process.Protocol <> nil) and Process.Protocol.Encrypted then
    Process.Protocol.Encryption.Encrypt(encapsulated.payload, dest.payload)
  else
    dest.payload := encapsulated.payload;
  result := Process.SendFrame(dest);
  if result then begin
    inc(fSent, length(Frame.payload));
    inc(fFrames);
  end;
  fLog.Add.Log(LOG_DEBUGERROR[not result], 'Send % #% % %', [ToText(Frame.opcode)^,
    Connection, IP, KBNoSpace(length(dest.payload))], self);
end;

function TAbstractRelay.Decapsulate(Protocol: TWebSocketProtocol;
  var Frame: TWebSocketFrame): integer;
var
  encapsulated: TRelayFrame;
  plain: RawByteString;
begin
  if (Frame.opcode = focBinary) and (Protocol <> nil) and Protocol.Encrypted then
    Protocol.Encryption.Decrypt(Frame.payload, plain)
  else
    plain := Frame.payload;
  if AlgoSynLZ.TryDecompress(plain, Frame.payload) and
     (length(Frame.payload) > 8) and
     (PRelayFrame(Frame.payload)^.revision = RELAYFRAME_VER) and
     (PRelayFrame(Frame.payload)^.opcode in
       [focContinuation, focConnectionClose, focBinary, focText]) and
     RecordLoad(encapsulated, Frame.payload, TypeInfo(TRelayFrame)) and
     (encapsulated.revision = RELAYFRAME_VER) then begin // paranoid
   Frame.opcode := encapsulated.opcode;
   Frame.content := encapsulated.content;
   Frame.payload := encapsulated.payload;
   inc(fReceived, length(Frame.payload));
   inc(fValid);
   fLog.Add.Log(sllTrace, 'Decapsulate: #% % %', [encapsulated.connection,
     ToText(Frame.opcode)^, KBNoSpace(length(Frame.payload))], self);
   result := encapsulated.connection;
  end
  else begin
    inc(fInvalid);
    fLog.Add.Log(sllWarning, 'Decapsulate: incorrect % frame %',
      [ToText(Frame.opcode)^, EscapeToShort(Frame.payload)], self);
    result := 0;
  end;
end;


{ TRelayServerProtocol }

procedure TRelayServerProtocol.ProcessIncomingFrame(Sender: TWebSocketProcess;
  var Frame: TWebSocketFrame; const info: RawUTF8);
var
  log: TSynLog;
  connection: integer;
  sent: boolean;
  client: TWebSocketServerResp;
begin
  log := fOwner.fLog.Add;
  log.Log(sllTrace, 'ProcessIncomingFrame % %', [Sender.RemoteIP, ToText(Frame.opcode)^], self);
  if Frame.opcode = focBinary then
    connection := fOwner.Decapsulate(Sender.Protocol, Frame)
  else
    connection := 0;
  fOwner.Safe.Lock;
  try
    case Frame.opcode of
      focContinuation:
        if fOwner.fServerConnected <> nil then
          raise ERelayProtocol.Create('Only a single server instance is allowed')
        else
          fOwner.fServerConnected := Sender;
      focConnectionClose:
        if fOwner.fServerConnected = Sender then
          fOwner.fServerConnected := nil
        else
          log.Log(sllWarning, 'ProcessIncomingFrame: fServerConnected?', self);
      focText, focBinary: begin
        if fOwner.fServerConnected <> Sender then
          raise ERelayProtocol.CreateUTF8('Unexpected %.ProcessIncomingFrame', [self]);
        if connection = 0 then
          exit;
        client := fOwner.fClients.IsActiveWebSocket(connection);
        if client = nil then begin
          log.Log(sllWarning, 'ProcessIncomingFrame: unknown connection #%',
            [connection], self);
          if Frame.opcode <> focConnectionClose then begin
            Frame.opcode := focConnectionClose;
            Frame.payload := '';
            fOwner.EncapsulateAndSend(Sender, 'removed', Frame, connection);
          end;
          exit;
        end;
        sent := client.WebSocketProcess.SendFrame(Frame);
        log.Log(LOG_DEBUGERROR[not sent], 'ProcessIncomingFrame % #% % %',
          [ToText(Frame.opcode)^, Connection, client.WebSocketProcess.RemoteIP,
           KBNoSpace(length(Frame.payload))], self);
      end;
      else
        raise ERelayProtocol.CreateUTF8('Unexpected % in %.ProcessIncomingFrame',
          [ToText(Frame.opcode)^, self]);
    end;
  finally
    fOwner.Safe.UnLock;
  end;
end;

constructor TRelayServerProtocol.Create(aOwner: TPublicRelay;
  const aServerKey: RawUTF8);
begin
  fOwner := aOwner;
  inherited Create('synopserelay', '');
  if aServerKey <> '' then
    SetEncryptKey({aServer=}true, aServerKey);
end;

function TRelayServerProtocol.Clone(const aClientURI: RawUTF8): TWebSocketProtocol;
begin
  result := TRelayServerProtocol.Create(fOwner, '');
  if fEncryption<>nil then
    TRelayServerProtocol(result).fEncryption := fEncryption.Clone;
end;


{ TSynopseServerProtocol }

constructor TSynopseServerProtocol.Create(aOwner: TPublicRelay);
begin
  fOwner := aOwner;
  inherited Create('synopserelay', '');
end;

function TSynopseServerProtocol.Clone(const aClientURI: RawUTF8): TWebSocketProtocol;
begin
  result := TSynopseServerProtocol.Create(fOwner);
end;

function TSynopseServerProtocol.GetSubprotocols: RawUTF8;
begin // SynBidirSock's TWebSocketProtocolRest variants
  result := 'synopsejson, synopsebin, synopsebinary';
end;

function TSynopseServerProtocol.SetSubprotocol(const aProtocolName: RawUTF8): boolean;
begin
  result := FindPropName(['synopsejson', 'synopsebin', 'synopsebinary'], aProtocolName) >= 0;
end;

procedure TSynopseServerProtocol.ProcessIncomingFrame(Sender: TWebSocketProcess;
  var Frame: TWebSocketFrame; const info: RawUTF8);
var
  ip: RawUTF8;
begin
  fOwner.fLog.Add.Log(sllTrace, 'ProcessIncomingFrame % %', [Sender.RemoteIP, ToText(Frame.opcode)^], self);
  fOwner.Safe.Lock;
  try
    case Frame.opcode of
      focContinuation, focText, focBinary:
        if fOwner.fServerConnected = nil then
          raise ERelayProtocol.CreateUTF8('%.ProcessIncomingFrame: No server to relay to', [self]);
      focConnectionClose:
        if fOwner.fServerConnected = nil then
          exit;
      else
        exit; // relay meaningfull frames
    end;
    ip := Sender.RemoteIP;
    if Frame.opcode = focContinuation then
      Frame.payload := ip + #13 + Name + #13 + UpgradeURI; // propagate to Private Relay
    if not fOwner.EncapsulateAndSend(fOwner.fServerConnected, ip, Frame, Sender.OwnerConnection) and
       (Frame.opcode <> focConnectionClose) then
      raise ERelayProtocol.CreateUTF8('%.ProcessIncomingFrame: Error relaying % from #% % to server',
        [ToText(Frame.opcode)^, Sender.OwnerConnection, ip])
  finally
    fOwner.Safe.UnLock;
  end;
end;


{ TPublicRelay }

function TPublicRelay.OnBeforeBody(const aURL, aMethod, aInHeaders,
  aInContentType, aRemoteIP: SockString; aContentLength: integer;
  aUseSSL: boolean): cardinal;
var
  bearer: RawUTF8;
  res: TJWTResult;
begin
  if IdemPChar(pointer(aURL), '/STAT') then begin
    result := STATUS_SUCCESS;
    exit;
  end;
  bearer := FindIniNameValue(pointer(aInHeaders), HEADER_BEARER_UPPER);
  res := fServerJWT.Verify(bearer);
  if res = jwtValid then
    if fServerConnected <> nil then begin
      fLog.Add.Log(sllWarning, 'OnBeforeBody % already connected to %',
        [aRemoteIP, fServerConnected.RemoteIP], self);
      result := STATUS_NOTACCEPTABLE;
    end else
      result := STATUS_SUCCESS
  else begin
    inc(fRejected);
    fLog.Add.Log(sllUserAuth, 'OnBeforeBody % %', [ToText(res)^, aRemoteIP], self);
    result := STATUS_FORBIDDEN;
  end;
end;

function TPublicRelay.OnRequest(Ctxt: THttpServerRequest): cardinal;
begin
  if IdemPChar(pointer(Ctxt.URL), '/STAT') then begin
    Ctxt.OutContent := GetStats;
    Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
    fLog.Add.Log(sllTrace, 'OnRequest stats=%', [Ctxt.OutContent], self);
    result := STATUS_SUCCESS;
  end
  else
    result := STATUS_NOTFOUND;
end;

function TPublicRelay.GetStats: RawJSON;
var
  tix: integer;
  ip: RawUTF8;
begin
  tix := GetTickCount64 shr 9; // 512 ms cache to avoid DoS attacks
  if tix <> fStatTix then begin
    fStatTix := tix;
    if fServerConnected = nil then
      ip := 'not connected'
    else
      ip := fServerConnected.RemoteIP;
    fStatCache := JSONReformat(JsonEncode([
      'version',ExeVersion.Version.Detailed, 'started',Started,
      'mem',TSynMonitorMemory.ToVariant, 'disk',GetDiskPartitionsText,
      'exceptions',GetLastExceptions, 'connections',fClients.ServerConnectionCount,
      'rejected',Rejected, 'server',ip, 'sent','{','kb',KB(Sent), 'frames',Frames, '}',
      'received','{','kb',KB(Received), 'valid',Valid, 'invalid',Invalid, '}',
      'connected',fClients.ServerConnectionActive, 'clients',fClients.WebSocketConnections]));
  end;
  result := fStatCache;
end;

constructor TPublicRelay.Create(aLog: TSynLogClass; const aClientsPort,
  aServerPort: SockString; const aServerKey: RawUTF8; aServerJWT: TJWTAbstract);
begin
  if aLog = nil then
    fLog := TSynLog
  else
    fLog := aLog;
  with fLog.Enter('Create: bind clients on %, server on %, encrypted=% %',
     [aClientsPort, aServerPort, BOOL_STR[aServerKey<>''], aServerJWT], self) do begin
    inherited Create;
    fStarted := NowUTCToString;
    fServerJWT := aServerJWT;
    fServer := TWebSocketServer.Create(aServerPort, nil, nil, 'relayserver');
    if fServerJWT <> nil then
      fServer.OnBeforeBody := OnBeforeBody;
    fServer.OnRequest := OnRequest;
    fServer.WebSocketProtocols.Add(TRelayServerProtocol.Create(self, aServerKey));
    fClients := TWebSocketServer.Create(aClientsPort, nil, nil, 'relayclients');
    fClients.WebSocketProtocols.Add(TSynopseServerProtocol.Create(self));
    Log(sllDebug, 'Create: Server=% Clients=%', [fServer, fClients], self);
  end;
end;

destructor TPublicRelay.Destroy;
var
  log: ISynLog;
begin
  log := fLog.Enter(self, 'Destroy');
  fStatTix := 0; // force GetStats recomputation
  log.Log(sllDebug, 'Destroying %', [self], self);
  fClients.Free;
  fServerConnected := nil;
  fServer.Free;
  inherited Destroy;
  fServerJWT.Free;
end;


{ TRelayClientProtocol }

constructor TRelayClientProtocol.Create(aOwner: TPrivateRelay; const aRelayKey: RawUTF8);
begin
  fOwner := aOwner;
  inherited Create('synopserelay', '');
  SetEncryptKey({aServer=}false, aRelayKey);
end;

function TRelayClientProtocol.Clone(const aClientURI: RawUTF8): TWebSocketProtocol;
begin
  result := nil; // not used on this client-side protocol
end;

procedure TRelayClientProtocol.ProcessIncomingFrame(Sender: TWebSocketProcess;
  var Frame: TWebSocketFrame; const info: RawUTF8);
var
  server, tobedeleted: TServerClient;
  connection, serverindex: integer;
  log: TSynLog;
begin
  log := fOwner.fLog.Add;
  log.Log(sllTrace, 'ProcessIncomingFrame % %', [Sender.RemoteIP, ToText(Frame.opcode)^], self);
  case Frame.opcode of
    focConnectionClose: begin
      if fOwner.Connected then begin
        log.Log(sllTrace, 'ProcessIncomingFrame: Public Relay server shutdown', self);
        fOwner.Disconnect;
      end;
      log.Log(sllTrace, 'ProcessIncomingFrame: disconnected to Public Relay', self);
      exit;
    end;
    focBinary:
      if not fOwner.Connected then
        raise ERelayProtocol.CreateUTF8('%.ProcessIncomingFrame: not connected', [self]);
    else
      exit; // relay meaningfull frames
  end;
  connection := fOwner.Decapsulate(Sender.Protocol, Frame);
  if connection = 0 then
    exit;
  tobedeleted := nil;
  fOwner.Safe.Lock;
  try
    server := fOwner.FindServerClientByConnection(connection, serverindex);
    if server = nil then
      case Frame.opcode of
        focConnectionClose:
          exit; // during closure handcheck
        focContinuation: begin
          server := fOwner.NewServerClient(connection, Frame.payload);
          if server = nil then begin
            log.Log(sllWarning, 'ProcessIncomingFrame: failed to create new link', self);
            Frame.opcode := focConnectionClose;
            Frame.content := [];
            Frame.payload := '';
            fOwner.EncapsulateAndSend(Sender, 'noserver', Frame, connection);
            exit;
          end;
        end
        else
          raise ERelayProtocol.CreateUTF8('%.ProcessIncomingFrame #% %?',
            [self, connection, ToText(Frame.opcode)^]);
      end;
    case Frame.opcode of
      focBinary, focText:
        if not server.Process.SendFrame(Frame) then
          log.Log(sllWarning, 'ProcessIncomingFrame: SendFrame failed', self);
      focConnectionClose: begin
        log.Log(sllTrace, 'ProcessIncomingFrame: delete %', [server], self);
        tobedeleted := server;
        PtrArrayDelete(fOwner.fServers, serverindex, @fOwner.fServersCount);
      end;
    end;
  finally
    fOwner.Safe.UnLock;
  end;
  tobedeleted.Free; // outside fOwner.Safe.Lock to prevent deadlocks
end;


{ TSynopseClientProtocol }

constructor TSynopseClientProtocol.Create(aOwner: TPrivateRelay; const aProtocolName: RawUTF8);
begin
  fOwner := aOwner;
  inherited Create(aProtocolName, '');
end;

function TSynopseClientProtocol.Clone(const aClientURI: RawUTF8): TWebSocketProtocol;
begin
  result := nil; // not used on this client-side only protocol
end;

procedure TSynopseClientProtocol.ProcessIncomingFrame(
  Sender: TWebSocketProcess; var Frame: TWebSocketFrame; const info: RawUTF8);
var
  server, tobedeleted: TServerClient;
  serverindex: integer;
begin
  fOwner.fLog.Add.Log(sllTrace, 'ProcessIncomingFrame % %', [Sender.RemoteIP, ToText(Frame.opcode)^], self);
  if not (Frame.opcode in [focConnectionClose, focText, focBinary]) then
    exit;
  tobedeleted := nil;
  fOwner.fSafe.Lock;
  try
    if (fOwner.fRelayClient = nil) and (Frame.opcode <> focConnectionClose) then
      raise ERelayProtocol.CreateUTF8('%.ProcessIncomingFrame: Public Relay down at %:%',
        [self, fOwner.fRelayHost, fOwner.fRelayPort]);
    server := fOwner.FindServerClientByProcess(Sender, serverindex);
    if (server = nil) or (server.Connection= 0) or (server.Process <> Sender) then
      if Frame.opcode = focConnectionClose then
        exit
      else
        raise ERelayProtocol.CreateUTF8('%.ProcessIncomingFrame: Unexpected %',
          [self, ToText(Frame.opcode)^]);
    if Frame.opcode = focConnectionClose then
      tobedeleted := server;
    if not fOwner.EncapsulateAndSend(fOwner.fRelayClient.Process,
        server.OriginIP, Frame, server.Connection) and (tobedeleted = nil) then
      raise ERelayProtocol.CreateUTF8('%.ProcessIncomingFrame: Error sending to Public Relay %:%',
        [self, fOwner.fRelayHost, fOwner.fRelayPort]);
    if tobedeleted <> nil then
      PtrArrayDelete(fOwner.fServers, serverindex, @fOwner.fServersCount);
  finally
    fOwner.fSafe.UnLock;
    if tobedeleted <> nil then
      tobedeleted.Free; // outside fOwner.Safe.Lock to prevent deadlocks
  end;
end;


{ TPrivateRelay }

constructor TPrivateRelay.Create(aLog: TSynLogClass; const aRelayHost, aRelayPort,
  aRelayKey, aRelayBearer, aServerHost, aServerPort, aServerRemoteIPHeader: RawUTF8);
begin
  inherited Create;
  if aLog = nil then
    fLog := TSynLog
  else
    fLog := aLog;
  fRelayHost := aRelayHost;
  fRelayPort := aRelayPort;
  fRelayKey := aRelayKey;
  fRelayCustomHeaders := AuthorizationBearer(aRelayBearer);
  fServerHost := aServerHost;
  fServerPort := aServerPort;
  if aServerRemoteIPHeader <> '' then
    fServerRemoteIPHeader := aServerRemoteIPHeader + ': ';
  fLog.Add.Log(sllDebug, 'Create: %', [self], self);
end;

function TPrivateRelay.FindServerClientByConnection(connection: integer;
  out index: integer): TServerClient;
var
  client: ^TServerClient;
  i: integer;
begin // caller made fSafe.Lock
  result := nil;
  if connection = 0 then
    exit;
  client := pointer(fServers);
  for i := 1 to fServersCount do
    if client^.Connection = connection then begin
      index := i - 1;
      result := client^;
      exit;
    end
    else
      inc(client);
end;

function TPrivateRelay.FindServerClientByProcess(process: TWebSocketProcess; out
  index: integer): TServerClient;
var
  client: ^TServerClient;
  i: integer;
begin // caller made fSafe.Lock
  result := nil;
  if process = nil then
    exit;
  client := pointer(fServers);
  for i := 1 to fServersCount do
    if client^.WebSockets = process then begin
      index := i - 1;
      result := client^;
      exit;
    end
    else
      inc(client);
end;

function TPrivateRelay.NewServerClient(connection: integer;
  const ipprotocoluri: RawUTF8): TServerClient;
var
  ip, protocol, url, header: RawUTF8;
begin // caller made fSafe.Lock
  split(ipprotocoluri, #13, ip, protocol);
  split(protocol, #13, protocol, url);
  with fLog.Enter('NewServerClient(%:%) for #% %/% %', [fServerHost, fServerPort,
     connection, ip, url, protocol], self) do begin
    if fServerRemoteIPHeader <> '' then
      header := fServerRemoteIPHeader + ip;
    result := TServerClient(TServerClient.WebSocketsConnect(fServerHost, fServerPort,
      TSynopseClientProtocol.Create(self, protocol), fLog, 'NewServerClient', url, header));
    if result <> nil then begin
      result.Connection := connection;
      result.OriginIP := ip;
      ObjArrayAddCount(fServers, result, fServersCount);
    end;
    Log(sllTrace, 'NewServerClient = %', [result], self);
  end;
end;

procedure TPrivateRelay.Disconnect;
var
  threadsafe: TServerClients;
  i: PtrInt;
begin
  if not Connected then
    exit;
  with fLog.Enter('Disconnect %:% count=%',
      [fRelayHost, fRelayPort, fServersCount], self) do begin
    fSafe.Lock; // avoid deadlock with focConnectionClose notification
    try
      threadsafe := fServers; // shutdown all current per-client links
      SetLength(threadsafe, fServersCount + 1);
      threadsafe[fServersCount] := pointer(fRelayClient); // + PublicRelay link
      fServers := nil;
      fServersCount := 0;
      fRelayClient := nil;
    finally
      fSafe.UnLock;
    end;
    for i := 0 to high(threadsafe) do begin
      Log(sllDebug, 'Disconnect %', [threadsafe[i]], self);
      threadsafe[i].Free;
    end;
  end;
end;

function TPrivateRelay.Connected: boolean;
begin
  result := fRelayClient <> nil;
end;

function TPrivateRelay.Encrypted: boolean;
begin
  result := fRelayKey <> '';
end;

function TPrivateRelay.TryConnect: boolean;
var
  log: ISynLog;
begin
  log := fLog.Enter('TryConnect %:%', [fRelayHost, fRelayPort], self);
  if Connected then begin
    Disconnect; // will do proper Safe.Lock/UnLock
  end;
  fSafe.Lock;
  try
    fRelayClient := THttpClientWebSockets.WebSocketsConnect(
      fRelayHost, fRelayPort, TRelayClientProtocol.Create(self, fRelayKey),
      fLog, 'TPrivateRelay.TryConnect', '', fRelayCustomHeaders);
    result := fRelayClient <> nil;
  finally
    fSafe.UnLock;
  end;
  log.Log(sllDebug, 'TryConnect=%', [BOOL_STR[result]], self);
end;

destructor TPrivateRelay.Destroy;
begin
  with fLog.Enter(self, 'Destroy') do
    try
      Log(sllDebug, 'Destroying %', [self], self);
      if Connected then begin
        Disconnect;
        Log(sllDebug, 'Destroy: disconnected as %', [self], self);
      end;
    except
    end;
  inherited Destroy;
end;


initialization
end.

