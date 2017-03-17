/// implements asynchronous RTSP stream tunnelling over HTTP
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynProtoRTSPHTTP;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2017 Arnaud Bouchez
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

  Portions created by the Initial Developer are Copyright (C) 2017
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


   RTSP Stream Tunnelling Over HTTP
  ----------------------------------
  as defined by Apple at https://goo.gl/CX6VA3

 It will encapsulate a RTSP TCP/IP duplex video stream into two HTTP links,
 one POST for upgoing commands, and one GET for downloaded video.

 Thanks to TAsynchServer, it can handle thousands on concurrent streams,
 with minimal resources, in a cross-platform way.

 This unit illustrates use of TAsynchConnections, on both Windows and Linux.
 See "Sample 34 - RTSP over HTTP proxy" for some numbers on actual hardware.

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
  {$endif}
  SynFPCSock, // shared with Kylix
  {$endif}
  SysUtils,
  SynCommons,
  SynCrtSock,
  SynBidirSock,
  SynLog,
  SynTests;

{$I Synopse.inc}

type
  /// holds a POST connection for RTSP over HTTP proxy
  // - as used by the TRTSPOverHTTPServer class
  TPostConnection = class(TAsynchConnection)
  protected
    fRtspTag: TPollSocketTag;
    // redirect the POST base-64 encoded command to the RTSP socket
    function OnRead(Sender: TAsynchConnections): TPollAsynchSocketOnRead; override;
  end;

  /// holds a RTSP connection for RTSP over HTTP proxy
  // - as used by the TRTSPOverHTTPServer class
  TRtspConnection = class(TAsynchConnection)
  protected
    fGetBlocking: TCrtSocket;
    // redirect the RTSP socket input to the GET content
    function OnRead(Sender: TAsynchConnections): TPollAsynchSocketOnRead; override;
    // will release the associated blocking GET socket 
    procedure BeforeDestroy(Sender: TAsynchConnections); override;
  end;

  /// implements RTSP over HTTP asynchronous proxy
  // - the HTTP transport is built from two separate HTTP GET and POST requests
  // initiated by the client; the server then binds the connections to form a
  // virtual full-duplex connection - see https://goo.gl/CX6VA3 for reference
  // material about this horrible, but widely accepted, Apple hack
  TRTSPOverHTTPServer = class(TAsynchServer)
  protected
    fRtspServer, fRtspPort: SockString;
    fPendingGet: TRawUTF8ListLocked;
    // creates TPostConnection and TRtspConnection instances for a given stream
    function ClientAdd(aSocket: TSocket; out aConnection: TAsynchConnection): boolean; override;
  public
    /// initialize the proxy HTTP server forwarding specified RTSP server:port
    constructor Create(const aRtspServer, aRtspPort, aHttpPort: SockString;
      aLog: TSynLogClass; aOnStart, aOnStop: TNotifyThreadEvent); reintroduce;
    /// shutdown and finalize the server
    destructor Destroy; override;
    /// perform some basic regression and benchmark testing on a running server
    procedure RegressionTests(test: TSynTestCase; clientcount: integer);
  end;


implementation


{ TRtspConnection }

function TRtspConnection.OnRead(Sender: TAsynchConnections): TPollAsynchSocketOnRead;
begin
  fGetBlocking.Write(fSlot.readbuf); // synch sending to the HTTP GET client
  TSynLog.Add.Log(sllDebug, 'OnRead % RTSP forwarded % bytes to GET',
    [Handle, length(fSlot.readbuf)], self);
  fSlot.readbuf := '';
  result := sorContinue;
end;

procedure TRtspConnection.BeforeDestroy(Sender: TAsynchConnections);
begin
  fGetBlocking.Free;
  inherited BeforeDestroy(Sender);
end;


{ TPostConnection }

function TPostConnection.OnRead(Sender: TAsynchConnections): TPollAsynchSocketOnRead;
var
  decoded: RawByteString;
  rtsp: TAsynchConnection;
begin
  result := sorContinue;
  decoded := Base64ToBinSafe(fSlot.readbuf);
  if decoded = '' then
    exit; // maybe some pending command chars
  fSlot.readbuf := '';
  rtsp := Sender.ClientFindLocked(fRtspTag);
  if rtsp <> nil then
  try
    Sender.Clients.WriteString(rtsp, decoded); // asynch sending to RTSP server
    TSynLog.Add.Log(sllDebug, 'OnRead % POST forwarded RTSP command [%]',
      [Handle, decoded], self);
  finally
    Sender.Unlock;
  end
  else begin
    TSynLog.Add.Log(sllDebug, 'OnRead % POST found no rtsp=%',
      [Handle, fRtspTag], self);
    result := sorClose;
  end;
end;


{ TRTSPOverHTTPServer }

constructor TRTSPOverHTTPServer.Create(const aRtspServer, aRtspPort, aHttpPort: SockString;
  aLog: TSynLogClass; aOnStart, aOnStop: TNotifyThreadEvent);
begin
  fLog := aLog;
  fRtspServer := aRtspServer;
  fRtspPort := aRtspPort;
  fPendingGet := TRawUTF8ListLocked.Create(true);
  inherited Create(aHttpPort, aOnStart, aOnStop, TPostConnection, 'RTSP/HTTP', aLog);
end;

destructor TRTSPOverHTTPServer.Destroy;
begin
  fLog.Enter(self);
  inherited Destroy;
  fPendingGet.Free;
end;

type
  TProxySocket = class(THttpServerSocket)
  protected
    fExpires: cardinal;
  published
    property Method;
    property URL;
    property RemoteIP;
  end;

function TRTSPOverHTTPServer.ClientAdd(aSocket: TSocket;
  out aConnection: TAsynchConnection): boolean;
var
  log: ISynLog;
  sock, get, old: TProxySocket;
  cookie: RawUTF8;
  rtsp: TSocket;
  i: integer;
  postconn: TPostConnection;
  rtspconn: TRtspConnection;
  now: cardinal;

  procedure PendingDelete(i: integer; const reason: RawUTF8);
  begin
    log.Log(sllDebug, 'ClientAdd rejected %', [reason], self);
    fPendingGet.Delete(i);
  end;

begin
  aConnection := nil;
  get := nil;
  result := false;
  log := fLog.Enter('ClientAdd(%)', [aSocket], self);
  try
    sock := TProxySocket.Create(nil);
    try
      sock.InitRequest(aSocket);
      if sock.GetRequest(false) and (sock.URL <> '') then begin
        log.Log(sllTrace, 'ClientAdd received % % %', [sock.Method, sock.URL,
          sock.HeaderGetText], self);
        cookie := sock.HeaderValue('x-sessioncookie');
        if cookie = '' then
          exit;
        fPendingGet.Safe.Lock;
        try
          now := GetTickCount64 shr 10;
          for i := fPendingGet.Count - 1 downto 0 do begin
            old := fPendingGet.ObjectPtr[i];
            if now > old.fExpires then begin
              log.Log(sllTrace, 'ClientAdd deletes deprecated %', [old], self);
              fPendingGet.Delete(i);
            end;
          end;
          i := fPendingGet.IndexOf(cookie);
          if IdemPropNameU(sock.Method, 'GET') then begin
            if i >= 0 then
              PendingDelete(i, 'duplicated')
            else begin
              sock.Write(FormatUTF8('HTTP/1.0 200 OK'#13#10 +
                'Server: % %'#13#10 +
                'Connection: close'#13#10 +
                'Date: Thu, 19 Aug 1982 18:30:00 GMT'#13#10 +
                'Cache-Control: no-store'#13#10 +
                'Pragma: no-cache'#13#10 +
                'Content-Type: application/x-rtsp-tunnelled'#13#10#13#10,
                [ExeVersion.ProgramName, ExeVersion.Version.DetailedOrVoid]));
              sock.fExpires := now + 60 * 15; // deprecated after 15 minutes
              sock.CloseSockIn; // we won't use it any more
              fPendingGet.AddObject(cookie, sock);
              sock := nil; // will be in fPendingGet until POST arrives
              result := true;
            end;
          end
          else if IdemPropNameU(sock.Method, 'POST') then begin
            if i < 0 then
              log.Log(sllDebug, 'ClientAdd rejected on unknonwn %', [sock], self)
            else if not IdemPropNameU(sock.ContentType,
              'application/x-rtsp-tunnelled') then
              PendingDelete(i, sock.ContentType)
            else begin
              get := fPendingGet.Objects[i] as TProxySocket;
              fPendingGet.Objects[i] := nil; // will be owned by rtspinstance
              fPendingGet.Delete(i);
              sock.Sock := -1; // disable Close on sock.Free -> handled in pool
            end;
          end;
        finally
          fPendingGet.Safe.UnLock;
        end;
      end
      else
        log.Log(sllDebug, 'ClientAdd: ignored invalid %', [sock], self);
    finally
      sock.Free;
    end;
    if get = nil then
      exit;
    if not get.SockConnected then begin
      log.Log(sllDebug, 'ClientAdd: GET disconnected %', [get.Sock], self);
      exit;
    end;
    rtsp := CallServer(fRtspServer, fRtspPort, false, cslTCP, 1000);
    if rtsp <= 0 then
      raise ECrtSocket.CreateFmt('No RTSP server on %s:%s', [fRtspServer, fRtspPort], -1);
    postconn := TPostConnection.Create;
    rtspconn := TRtspConnection.Create;
    if not inherited ClientAddExisting(aSocket, postconn) or
       not inherited ClientAddExisting(rtsp, rtspconn) then
      raise EAsynchConnections.CreateUTF8('inherited %.ClientAdd(%) % failed',
        [self, aSocket, cookie]);
    aConnection := postconn;
    postconn.fRtspTag := rtspconn.Handle;
    rtspconn.fGetBlocking := get;
    if not fClients.Start(rtspconn) then
      exit;
    get := nil;
    result := true;
    log.Log(sllTrace, 'ClientAdd added get=% post=%/% and rtsp=%/%',
      [rtspconn.fGetBlocking.Sock, aSocket, aConnection.Handle,
       rtsp, rtspconn.Handle], self);
  except
    log.Log(sllDebug, 'ClientAdd(%) failed', [aSocket], self);
    get.Free;
  end;
end;

procedure TRTSPOverHTTPServer.RegressionTests(test: TSynTestCase;
  clientcount: integer);
type
  TReq = record
    get: THttpSocket;
    post: TCrtSocket;
    stream: TCrtSocket;
    session: RawUTF8;
  end;
var
  streamer: TCrtSocket;
  req: array of TReq;
  rmax, r, i: integer;
  text: SockString;
  log: ISynLog;
begin // here we follow the steps and content stated by https://goo.gl/CX6VA3
  log := fLog.Enter(self);
  if (self = nil) or (fRtspServer <> '127.0.0.1') then
    test.Check(false, 'expect a running proxy on 127.0.0.1')
  else
  try
    rmax := clientcount - 1;
    streamer := TCrtSocket.Bind(fRtspPort);
    try
      log.Log(sllCustom1, 'RegressionTests % GET', [clientcount], self);
      SetLength(req, clientcount);
      for r := 0 to rmax do
      with req[r] do begin
        session := TSynTestCase.RandomIdentifier(20 + r and 15);
        get := THttpSocket.Open('localhost', fServer.Port);
        get.Write(
          'GET /sw.mov HTTP/1.0'#13#10 +
          'User-Agent: QTS (qtver=4.1;cpu=PPC;os=Mac 8.6)'#13#10 +
          'x-sessioncookie: ' + session + #13#10 +
          'Accept: application/x-rtsp-tunnelled'#13#10 +
          'Pragma: no-cache'#13#10 +
          'Cache-Control: no-cache'#13#10#13#10);
        get.SockRecvLn(text);
        test.Check(text = 'HTTP/1.0 200 OK');
        get.GetHeader;
        test.Check(get.ConnectionClose);
        test.Check(get.SockConnected);
        test.Check(get.ContentType = 'application/x-rtsp-tunnelled');
      end;
      log.Log(sllCustom1, 'RegressionTests % POST', [clientcount], self);
      for r := 0 to rmax do
      with req[r] do begin
        post := TCrtSocket.Open('localhost', fServer.Port);
        post.Write(
          'POST /sw.mov HTTP/1.0'#13#10 +
          'User-Agent: QTS (qtver=4.1;cpu=PPC;os=Mac 8.6)'#13#10 +
          'x-sessioncookie: ' + session + #13#10 +
          'Content-Type: application/x-rtsp-tunnelled'#13#10 +
          'Pragma: no-cache'#13#10 +
          'Cache-Control: no-cache'#13#10 +
          'Content-Length: 32767'#13#10 +
          'Expires: Sun, 9 Jan 1972 00:00:00 GMT'#13#10#13#10);
        stream := streamer.AcceptIncoming;
        if stream = nil then begin
          test.Check(false);
          exit;
        end;
        test.Check(get.SockConnected);
        test.Check(post.SockConnected);
      end;
      for i := 0 to 10 do begin
        log.Log(sllCustom1, 'RegressionTests % RUN #%', [clientcount, i], self);
        // send a RTSP command once in a while to the POST request
        if i and 7 = 0 then begin
          for r := 0 to rmax do
            req[r].post.Write(
              'REVTQ1JJQkUgcnRzcDovL3R1Y2tydS5hcHBsZS5jb20vc3cubW92IFJUU1AvMS4w' +
              'DQpDU2VxOiAxDQpBY2NlcHQ6IGFwcGxpY2F0aW9uL3NkcA0KQmFuZHdpZHRoOiAx' +
              'NTAwMDAwDQpBY2NlcHQtTGFuZ3VhZ2U6IGVuLVVTDQpVc2VyLUFnZW50OiBRVFMg' +
              'KHF0dmVyPTQuMTtjcHU9UFBDO29zPU1hYyA4LjYpDQoNCg==');
          for r := 0 to rmax do
            test.check(req[r].stream.SockReceiveString =
              'DESCRIBE rtsp://tuckru.apple.com/sw.mov RTSP/1.0'#13#10 +
              'CSeq: 1'#13#10 +
              'Accept: application/sdp'#13#10 +
              'Bandwidth: 1500000'#13#10 +
              'Accept-Language: en-US'#13#10 +
              'User-Agent: QTS (qtver=4.1;cpu=PPC;os=Mac 8.6)'#13#10#13#10);
        end;
        // stream output should be redirected to the GET request
        for r := 0 to rmax do
          req[r].stream.Write(req[r].session);
        for r := 0 to rmax do
          with req[r] do
            test.check(get.SockReceiveString = session);
      end;
      log.Log(sllCustom1, 'RegressionTests % SHUTDOWN', [clientcount], self);
    finally
      for r := 0 to rmax do
      with req[r] do begin
        get.Free;
        post.Free;
        stream.Free;
      end;
      repeat
        sleep(10);
      until fClients.Count = 0;
      streamer.Free;
    end;
  except
    on E: Exception do
      test.Check(false, E.ClassName);
  end;
end;



end.

