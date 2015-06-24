/// HTTP/1.1 RESTful JSON Client classes for mORMot
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit mORMotHttpClient;

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

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2015
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Maciej Izak (hnb)

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



      HTTP/1.1 RESTful JSON Client for mORMot
    ******************************************

   - use internaly the JSON format for content communication
   - can be called by any JSON-aware AJAX application
   - can optionaly compress the returned data to optimize Internet bandwidth
   - speed is very high: more than 20MB/sec R/W localy on a 1.8GHz Sempron,
     i.e. 400Mb/sec of duplex raw IP data, with about 200 µs only elapsed
     by request (direct call is 50 µs, so bottle neck is the Win32 API),
     i.e. 5000 requests per second, with 113 result rows (i.e. 4803 bytes
     of JSON data each)... try to find a faster JSON HTTP server! ;)

    Initial version: 2009 May, by Arnaud Bouchez

    Version 1.1
      - code rewrite for FPC and Delphi 2009/2010 compilation

    Version 1.3 - January 22, 2010
      - some small fixes and multi-compiler enhancements

    Version 1.4 - February 08, 2010
      - whole Synopse SQLite3 database framework released under the GNU Lesser
        General Public License version 3, instead of generic "Public Domain"
      - HTTP/1.1 RESTful JSON Client and Server split into two units
        (SQLite3HttpClient and SQLite3HttpServer)

    Version 1.5 - February 12, 2010
      - test HTTP connection in both KeepAlive and with new connection for
        each request (an issue with no KeepAlive connections was detected)

    Version 1.13
      - now can compress its content using deflate or faster SynLZ algorithm:
        by default, the SynLZ algorithm will be used between a Delphi Client
        and Server over HTTP/1.1 - there will be no speed penalty on the
        server side, whereas deflate would use much more CPU
      - can make TCP/IP stream not HTTP compliant (against antivirus slowdown)
      - new TSQLite3HttpClientWinINet class, using WinINet API (very slow)
      - new TSQLite3HttpClientWinHTTP class, using WinHTTP API (fast and stable):
        this class should be considered to be used instead of TSQLite3HttpClient
        for any HTTP/1.1 client connection over a network - it is therefore
        the default TSQLite3HttpClient class since this 1.13 revision

    Version 1.16
      - fixed GPF issue at closing
      - fixed unnecessary dual URL signing (when authentication actived)

    Version 1.17
      - added optional aProxyName, aProxyByPass parameters to
        TSQLite3HttpClientWinGeneric / TSQLite3HttpClientWinINet and
        TSQLite3HttpClientWinHTTP constructors

    Version 1.18
     - unit SQLite3HttpClient.pas renamed mORMotHttpClient.pas
       (see mORMotHTTPServer.pas for the server side)
     - TSQLite3HttpClient* classes renamed as TSQLHttpClient*
     - introducing TSQLHttpClientCurl class, using cross-platform libcurl to
       connect over HTTP or HTTPS (using system OpenSSL library, if available)
     - all TSQLHttpClient* classes are now thread-safe (i.e. protected by
       a global mutex, as other TSQLRestClientURI implementations already did)
     - fixed TSQLHttpClientGeneric.InternalURI() method to raise an explicit
       exception on connection error (as expected by TSQLRestClientURI.URI)
     - TSQLHttpClient* classes will now handle properly reconnection in case
       of connection break via overridden InternalCheckOpen/InternalClose methods
     - introducing TSQLHttpClientGeneric.Compression property to set the handled
       compression schemes at runtime, i.e. SynLZ, deflate or SynLZ+SHA/AES:
       hcDeflate will in fact use gzip content encoding, since deflate/gzip is
       not consistent in practice among clients
     - added SendTimeout and ReceiveTimeout optional parameters (in ms) to
       TSQLHttpClientWinHTTP / TSQLHttpClientWinINet constructors [bfe485b678]
     - added ConnectTimeout optional parameter (thanks hnb for the patch!)
     - added TSQLHttpClientGeneric.CreateForRemoteLogging() constructor for
       easy remote logging to our LogView tool, running as server process
     - added TSQLHttpClientWinGeneric.IgnoreSSLCertificateErrors property
       to set the corresponding parameter for the underlying connection
     - added AuthScheme and AuthUserName/AuthPassword properties, for
       authentication - only implemented at TSQLHttpClientWinHTTP level yet
}

interface

{.$define USETCPPREFIX}
{ if defined, a prefix will be added to the TCP/IP stream so that it won't be
  valid HTTP content any more: it could increase the client/server speed with
  some anti-virus software, but the remote access won't work any more with
  Internet Browsers nor AJAX applications
  - not defined by default - should be set globally to the project conditionals }

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 WITHLOG

uses
{$ifdef MSWINDOWS}
  Windows,
{$else}
  {$ifdef KYLIX3}
  Types,
  LibC,
  {$endif}
{$endif}
  SysUtils,
  Classes,
  SynZip,
  SynLZ,
  SynCrtSock,
  SynBidirSock, // for WebSockets
  SynCrypto,    // for hcSynShaAes
  SynCommons,
  SynLog,
  mORMot;

type
  /// available compression algorithms for transmission
  // - SynLZ is faster then Deflate, but not standard: use hcSynLZ for Delphi
  // clients, but hcDeflate for AJAX or any HTTP clients
  // - with hcSynLZ, the 440 KB JSON for TTestClientServerAccess._TSQLHttpClient
  // is compressed into 106 KB with no speed penalty (it's even a bit faster)
  // whereas hcDeflate with its level set to 1 (fastest), is 25 % slower
  // - hcSynShaAes will use SHA-256/AES-256-CTR to encrypt the content (after
  // SynLZ compression), via SynCrypto.CompressShaAes() function
  // - here hcDeflate will use in fact gzip content encoding, since deflate
  // is inconsistent between browsers: http://stackoverflow.com/a/9186091/458259
  TSQLHttpCompression = (hcSynLZ, hcDeflate, hcSynShaAes);

  /// set of available compressions schemes
  TSQLHttpCompressions = set of TSQLHttpCompression;

  /// abstract HTTP/1.1 RESTful JSON mORMot Client class
  // - this class, and other inherited classes defined in this unit, are
  // thread-safe, since each of their URI() method is protected by a giant lock
  TSQLHttpClientGeneric = class(TSQLRestClientURI)
  protected
    fKeepAliveMS: cardinal;
    fCompression: TSQLHttpCompressions;
    /// connection parameters as set by Create()
    fServer, fPort: AnsiString;
    fHttps: boolean;
    fProxyName, fProxyByPass: AnsiString;
    fSendTimeout, fReceiveTimeout, fConnectTimeout: DWORD;
    fExtendedOptions: THttpRequestExtendedOptions;
    procedure SetCompression(Value: TSQLHttpCompressions);
    procedure SetKeepAliveMS(Value: cardinal);
    constructor RegisteredClassCreateFrom(aModel: TSQLModel;
      aDefinition: TSynConnectionDefinition); override;
    /// process low-level HTTP/1.1 request
    // - called by InternalURI(), therefore by URI() public method
    // - returns 200,202,204 if OK, http status error otherwise in result.Lo
    // - returns Server-InternalState in result.Hi
    function InternalRequest(const url, method: RawUTF8;
      var Header, Data, DataType: RawUTF8): Int64Rec; virtual; abstract;
    /// method calling the RESTful server fServer via HTTP/1.1
    // - calls the InternalRequest() protected method
    procedure InternalURI(var Call: TSQLRestURIParams); override;
  public
    /// connect to TSQLHttpServer on aServer:aPort
    // - you can customize the default client timeouts by setting appropriate
    // ConnectTimeout, SendTimeout and ReceiveTimeout parameters (in ms)
    constructor Create(const aServer, aPort: AnsiString; aModel: TSQLModel;
      SendTimeout: DWORD=HTTP_DEFAULT_SENDTIMEOUT;
      ReceiveTimeout: DWORD=HTTP_DEFAULT_RECEIVETIMEOUT;
      ConnectTimeout: DWORD=HTTP_DEFAULT_CONNECTTIMEOUT); reintroduce; overload; virtual;
    /// connect to TSQLHttpServer via 'address:port/root' URI format
    // - if port is not specified, aDefaultPort is used
    // - if root is not specified, aModel.Root is used
    constructor Create(const aServer: TSQLRestServerURIString; aModel: TSQLModel;
      aDefaultPort: integer); reintroduce; overload; 
    /// connnect to a LogView HTTP Server for remote logging
    // - will associate the EchoCustom callback of the log class to this server
    // - the aLogClass.Family will manage this TSQLHttpClientGeneric instance
    // life time, until application is closed or Family.EchoRemoteStop is called
    constructor CreateForRemoteLogging(const aServer: AnsiString;
      aLogClass: TSynLogClass; aPort: Integer=8091; const aRoot: RawUTF8='LogService');
    /// save the TSQLHttpClientGeneric properties into a persistent storage object
    // - CreateFrom() will expect Definition.ServerName to store the URI as
    // 'server:port' or 'https://server:port', Definition.User/Password to store
    // the TSQLRestClientURI.SetUser() information, and Definition.DatabaseName
    // to store the extended options as an URL-encoded string
    procedure DefinitionTo(Definition: TSynConnectionDefinition); override;
    /// the time (in milliseconds) to keep the connection alive with the
    // TSQLHttpServer
    // - default is 20000, i.e. 20 seconds
    property KeepAliveMS: cardinal read fKeepAliveMS write SetKeepAliveMS;
    /// the compression algorithms usable with this client
    // - default is [hcSynLZ], i.e. our SynLZ algorithm which will provide
    // good compression, with very low CPU use on server side: it will a bit
    // less efficient than hcDeflate, but consume much less resources
    // - if you set [hcSynShaAes], it will use SHA-256/AES-256-CTR to encrypt the
    // content (after SynLZ compression), if it is enabled on the server side:
    // ! MyServer := TSQLHttpServer.Create('888',[DataBase],'+',useHttpApi,32,secSynShaAes);
    property Compression: TSQLHttpCompressions read fCompression write SetCompression;
    /// the Server IP address
    property Server: AnsiString read fServer;
    /// the Server IP port
    property Port: AnsiString read fPort;
  end;

  TSQLHttpClientGenericClass = class of TSQLHttpClientGeneric;

  /// HTTP/1.1 RESTful JSON mORMot Client class using SynCrtSock's Sockets
  // - will give the best performance on a local computer, but has been found
  // out to be slower over a network
  // - is not able to use secure HTTPS protocol
  // - note that, in its current implementation, this class is not thread-safe:
  // you need either to lock its access via a critical section, or initialize
  // one client instance per thread
  TSQLHttpClientWinSock = class(TSQLHttpClientGeneric)
  protected
    /// internal HTTP/1.1 compatible client
    fSocketClass: THttpClientSocketClass;
    fSocket: THttpClientSocket;
    /// call fSocket.Request()
    function InternalRequest(const url, method: RawUTF8;
      var Header, Data, DataType: RawUTF8): Int64Rec; override;
    /// overridden protected method to handle HTTP connection
    function InternalCheckOpen: boolean; override;
    /// overridden protected method to close HTTP connection
    procedure InternalClose; override;
  public
    /// internal HTTP/1.1 compatible client
    // - can be used e.g. to access SendTimeout and ReceiveTimeout properties
    property Socket: THttpClientSocket read fSocket;
  end;

  /// HTTP/1.1 RESTful JSON mORMot Client able to upgrade to WebSockets
  // - in addition to TSQLHttpClientWinSock, this client class is able
  // to upgrade its HTTP connection to the WebSockets protocol, so that the
  // server may be able to notify the client via a callback
  // - the internal Socket class will be in fact a THttpClientWebSockets
  // instance, as defined in the SynBidirSock unit
  TSQLHttpClientWebsockets = class(TSQLHttpClientWinSock)
  protected
    fWebSocketParams: record
      AutoUpgrade: boolean;
      Key: RawUTF8;
      Compression: boolean;
      Ajax: boolean;
    end;
    function InternalCheckOpen: boolean; override;
    function FakeCallbackRegister(Sender: TServiceFactoryClient;
      const Method: TServiceMethod; const ParamInfo: TServiceMethodArgument;
      ParamValue: Pointer): integer; override;
    function FakeCallbackUnregister(Factory: TInterfaceFactory;
      FakeCallbackID: integer; Instance: pointer): boolean; override;
    function CallbackRequest(Ctxt: THttpServerRequest): cardinal; virtual;
  public
    /// upgrade the HTTP client connection to a specified WebSockets protocol
    // - the Model.Root URI will be used for upgrade
    // - if aWebSocketsAJAX equals default FALSE, it will use 'synopsebinary'
    // i.e. TWebSocketProtocolBinaryprotocol, with AES-CFB 256 bits encryption
    // if the encryption key text is not '' and optional SynLZ compression
    // - if aWebSocketsAJAX is TRUE, it will register the slower and less secure
    // 'synopsejson' mode, i.e. TWebSocketProtocolJSON (to be used for AJAX
    // debugging/test purposes only)
    // and aWebSocketsEncryptionKey/aWebSocketsCompression parameters won't be used
    // - will return '' on success, or an error message on failure
    function WebSocketsUpgrade(const aWebSocketsEncryptionKey: RawUTF8;
      aWebSocketsAJAX: boolean=false; aWebSocketsCompression: boolean=true): RawUTF8;
    /// internal HTTP/1.1 and WebSockets compatible client
    // - you could use its properties after upgrading the connection to WebSockets
    function WebSockets: THttpClientWebSockets;
  end;

  /// HTTP/1.1 RESTful JSON mORMot Client abstract class using either WinINet,
  // WinHTTP or libcurl API
  // - not to be called directly, but via TSQLHttpClientWinINet or (even
  // better) TSQLHttpClientWinHTTP overridden classes under Windows
  TSQLHttpClientRequest = class(TSQLHttpClientGeneric)
  protected
    fRequest: THttpRequest;
    fRequestClass: THttpRequestClass;
    /// call fWinAPI.Request()
    function InternalRequest(const url, method: RawUTF8;
      var Header, Data, DataType: RawUTF8): Int64Rec; override;
    /// overridden protected method to close HTTP connection
    procedure InternalClose; override;
    /// overridden protected method to handle HTTP connection
    function InternalCheckOpen: boolean; override;
    /// set the fWinAPI class
    // - the overridden implementation should set the expected fWinAPIClass
    procedure InternalSetClass; virtual; abstract;
  public
    /// connect to TSQLHttpServer on aServer:aPort with the default settings
    // - you can customize the default client timeouts by setting appropriate
    // ConnectTimeout, SendTimeout and ReceiveTimeout parameters (in ms) - note
    // that after creation of this instance, the connection is tied to those
    // initial parameters, so we won't publish any properties to change those
    // initial values once created
    constructor Create(const aServer, aPort: AnsiString; aModel: TSQLModel;
      SendTimeout: DWORD=HTTP_DEFAULT_SENDTIMEOUT;
      ReceiveTimeout: DWORD=HTTP_DEFAULT_RECEIVETIMEOUT;
      ConnectTimeout: DWORD=HTTP_DEFAULT_CONNECTTIMEOUT); overload; override;
    /// connect to TSQLHttpServer on aServer:aPort
    // - optional aProxyName may contain the name of the proxy server to use,
    // and aProxyByPass an optional semicolon delimited list of host names or
    // IP addresses, or both, that should not be routed through the proxy
    // - you can customize the default client timeouts by setting appropriate
    // ConnectTimeout, SendTimeout and ReceiveTimeout parameters (in ms) - note
    // that after creation of this instance, the connection is tied to those
    // initial parameters, so we won't publish any properties to change those
    // initial values once created
    constructor Create(const aServer, aPort: AnsiString; aModel: TSQLModel;
      aHttps: boolean; const aProxyName: AnsiString='';
      const aProxyByPass: AnsiString='';
      SendTimeout: DWORD=HTTP_DEFAULT_SENDTIMEOUT;
      ReceiveTimeout: DWORD=HTTP_DEFAULT_RECEIVETIMEOUT;
      ConnectTimeout: DWORD=HTTP_DEFAULT_CONNECTTIMEOUT); reintroduce; overload;
    /// internal class instance used for the connection
    // - will return either a TWinINet, a TWinHTTP or a TCurlHTTP class instance
    property Request: THttpRequest read fRequest;
    /// allows to ignore untrusted SSL certificates
    // - similar to adding a security exception for a domain in the browser
    property IgnoreSSLCertificateErrors: boolean
      read fExtendedOptions.IgnoreSSLCertificateErrors
      write fExtendedOptions.IgnoreSSLCertificateErrors;
    /// optional Authentication Scheme
    property AuthScheme: THttpRequestAuthentication
      read fExtendedOptions.Auth.Scheme write fExtendedOptions.Auth.Scheme;
    /// optional User Name for Authentication
    property AuthUserName: SynUnicode
      read fExtendedOptions.Auth.UserName write fExtendedOptions.Auth.UserName;
    /// optional Password for Authentication
    property AuthPassword: SynUnicode
      read fExtendedOptions.Auth.Password write fExtendedOptions.Auth.Password;
  end;

  TSQLHttpClientRequestClass = class of TSQLHttpClientRequest;

  {$ifdef USEWININET}
  /// HTTP/1.1 RESTful JSON mORMot Client class using WinINet API
  // - this class is 15/20 times slower than TSQLHttpClient using SynCrtSock
  // on a local machine, but was found to be faster throughout local networks
  // - this class is able to connect via the secure HTTPS protocol
  // - it will retrieve by default the Internet Explorer proxy settings, and
  // display some error messages or authentification dialog on screen
  // - you can optionaly specify manual Proxy settings at constructor level
  // - by design, the WinINet API should not be used from a service
  // - is implemented by creating a TWinINet internal class instance
  TSQLHttpClientWinINet = class(TSQLHttpClientRequest)
  protected
    procedure InternalSetClass; override;
  end;

  /// HTTP/1.1 RESTful JSON Client class using WinHTTP API
  // - has a common behavior as THttpClientSocket() but seems to be faster
  // over a network and is able to retrieve the current proxy settings
  // (if available) and handle secure HTTPS connection - so it seems to be used
  // in your client programs: TSQLHttpClient will therefore map to this class
  // - WinHTTP does not share directly any proxy settings with Internet Explorer.
  // The default WinHTTP proxy configuration is set by either
  // proxycfg.exe on Windows XP and Windows Server 2003 or earlier, either
  // netsh.exe on Windows Vista and Windows Server 2008 or later; for instance,
  // you can run "proxycfg -u" or "netsh winhttp import proxy source=ie" to use
  // the current user's proxy settings for Internet Explorer (under 64 bit
  // Vista/Seven, to configure applications using the 32 bit WinHttp settings,
  // call netsh or proxycfg bits from %SystemRoot%\SysWOW64 folder explicitely)
  // - you can optionaly specify manual Proxy settings at constructor level
  // - by design, the WinHTTP API can be used from a service or a server
  // - is implemented by creating a TWinHTTP internal class instance
  TSQLHttpClientWinHTTP = class(TSQLHttpClientRequest)
  protected
    procedure InternalSetClass; override;
  end;
  {$endif USEWININET}

  {$ifdef USELIBCURL}
  /// HTTP/1.1 RESTful JSON Client class using libculr
  // - will handle HTTP and HTTPS, if OpenSSL or similar libray is available
  TSQLHttpClientCurl = class(TSQLHttpClientRequest)
  protected
    procedure InternalSetClass; override;
  end;
  {$endif USELIBCURL}

  {$ifdef ONLYUSEHTTPSOCKET}
  /// HTTP/1.1 RESTful JSON deault mORMot Client class
  // -  maps the raw socket implementation class
  TSQLHttpClient = TSQLHttpClientWinSock;
  {$else}
  /// HTTP/1.1 RESTful JSON default mORMot Client class
  // - under Windows, maps the TSQLHttpClientWinHTTP class
  TSQLHttpClient = TSQLHttpClientWinHTTP;
  {$endif ONLYUSEHTTPSOCKET}


implementation


{ TSQLHttpClientGeneric }

procedure TSQLHttpClientGeneric.InternalURI(var Call: TSQLRestURIParams);
var Head, Content, ContentType: RawUTF8;
    P: PUTF8Char;
    res: Int64Rec;
begin
{$ifdef WITHLOG}
  fLogClass.Enter(self,nil,true);
{$endif}
  Head := Call.InHead;
  Content := Call.InBody;
  if InternalCheckOpen then begin
    if Head<>'' then begin
      P := pointer(Head);
      if IdemPChar(P,'CONTENT-TYPE:') then begin
        inc(P,14);
        if Content<>'' then begin
          ContentType := GetMimeContentType(pointer(Content),Length(Content));
          if ContentType='application/octet-stream' then
            ContentType := '';
        end;
        if ContentType='' then
          ContentType := GetNextLine(P,P);
        Head := ''; // header is processed -> no need to send Content-Type twice
      end;
    end;
    if ContentType='' then
      ContentType := JSON_CONTENT_TYPE_VAR;
    EnterCriticalSection(fMutex);
    try
      res := InternalRequest(Call.Url,Call.Method,Head,Content,ContentType);
      Call.OutStatus := res.Lo;
      Call.OutInternalState := res.Hi;
      Call.OutHead := Head;
      Call.OutBody := Content;
    finally
      LeaveCriticalSection(fMutex);
    end;
  end else
    Call.OutStatus := HTML_NOTIMPLEMENTED; // 501
{$ifdef WITHLOG}
  with Call do
    fLogFamily.SynLog.Log(sllClient,'% % status=% state=%',
      [method,url,OutStatus,OutInternalState],self);
{$endif}
end;

procedure TSQLHttpClientGeneric.SetCompression(Value: TSQLHttpCompressions);
begin
  fCompression := Value;
  InternalClose; // force re-create connection at next request
end;

procedure TSQLHttpClientGeneric.SetKeepAliveMS(Value: cardinal);
begin
  fKeepAliveMS := Value;
  InternalClose; // force re-create connection at next request
end;

constructor TSQLHttpClientGeneric.Create(const aServer, aPort: AnsiString;
  aModel: TSQLModel; SendTimeout,ReceiveTimeout,ConnectTimeout: DWORD);
begin
  inherited Create(aModel);
  fServer := aServer;
  fPort := aPort;
  fKeepAliveMS := 20000; // 20 seconds connection keep alive by default
  fCompression := [hcSynLZ];
  fConnectTimeout := ConnectTimeout;
  fSendTimeout := SendTimeout;
  fReceiveTimeout := ReceiveTimeout;
end;

constructor TSQLHttpClientGeneric.CreateForRemoteLogging(const aServer: AnsiString;
  aLogClass: TSynLogClass; aPort: Integer; const aRoot: RawUTF8);
var aModel: TSQLModel;
begin
  if not Assigned(aLogClass) then
    raise ECommunicationException.CreateUTF8('%.CreateForRemoteLogging(LogClass=nil)',[self]);
  aModel := TSQLModel.Create([],aRoot);
  Create(aServer,AnsiString(UInt32ToUtf8(aPort)),aModel);
  aModel.Owner := self;
  ServerRemoteLogStart(aLogClass,true);
  fRemoteLogClass.Log(sllTrace,
    'Echoing to remote server http://%/%/RemoteLog:%',[aServer,aRoot,aPort]);
end;

procedure TSQLHttpClientGeneric.DefinitionTo(Definition: TSynConnectionDefinition);
begin
  if Definition=nil then
    exit;
  inherited DefinitionTo(Definition); // save Kind + User/Password
  if fHttps then
    Definition.ServerName := 'https://';
  Definition.ServerName := FormatUTF8('%%:%',[Definition.ServerName,fServer,fPort]);
  Definition.DatabaseName := UrlEncode([
   'IgnoreSSLCertificateErrors',ord(fExtendedOptions.IgnoreSSLCertificateErrors),
   'ConnectTimeout',fConnectTimeout,'SendTimeout',fSendTimeout,'ReceiveTimeout',fReceiveTimeout,
   'ProxyName',fProxyName,'ProxyByPass',fProxyByPass]);
  Definition.DatabaseName := copy(Definition.DatabaseName,2,MaxInt); // trim leading '?'
end;

constructor TSQLHttpClientGeneric.RegisteredClassCreateFrom(aModel: TSQLModel;
  aDefinition: TSynConnectionDefinition);
var URI: TURI;
    P: PUTF8Char;
    V: cardinal;
    tmp: RawUTF8;
begin
  URI.From(aDefinition.ServerName);
  Create(URI.Server,URI.Port,aModel);
  fHttps := URI.Https;
  P := Pointer(aDefinition.DataBaseName);
  while P<>nil do begin
    if UrlDecodeCardinal(P,'CONNECTTIMEOUT',V) then
      fConnectTimeout := V else
    if UrlDecodeCardinal(P,'SENDTIMEOUT',V) then
      fSendTimeout := V else
    if UrlDecodeCardinal(P,'RECEIVETIMEOUT',V) then
      fReceiveTimeout := V else
    if UrlDecodeValue(P,'PROXYNAME',tmp) then
      fProxyName := CurrentAnsiConvert.UTF8ToAnsi(tmp) else
    if UrlDecodeValue(P,'PROXYBYPASS',tmp) then
      fProxyByPass := CurrentAnsiConvert.UTF8ToAnsi(tmp);
    if UrlDecodeCardinal(P,'IGNORESSLCERTIFICATEERRORS',V,@P) then
      fExtendedOptions.IgnoreSSLCertificateErrors := Boolean(V);
  end;
  inherited RegisteredClassCreateFrom(aModel,aDefinition); // call SetUser()
end;

constructor TSQLHttpClientGeneric.Create(const aServer: TSQLRestServerURIString;
  aModel: TSQLModel; aDefaultPort: integer);
var URI: TSQLRestServerURI;
begin
  URI.URI := aServer;
  if URI.Root<>'' then
    aModel.Root := URI.Root;
  if URI.Port='' then
    URI.Port := Int32ToUtf8(aDefaultPort);
  Create(SockString(URI.Address),SockString(URI.Port),aModel);
end;


{ TSQLHttpClientWinSock }

function TSQLHttpClientWinSock.InternalCheckOpen: boolean;
begin
  if fSocket<>nil then begin
    result := true; // already connected
    exit;
  end;
  EnterCriticalSection(fMutex);
  try
    try
      if fSocketClass=nil then
        fSocketClass := THttpClientSocket;
      fSocket := fSocketClass.Open(fServer,fPort,cslTCP,fConnectTimeout);
      if fSendTimeout>0 then
        fSocket.SendTimeout := fSendTimeout;
      if fReceiveTimeout>0 then
        fSocket.ReceiveTimeout := fReceiveTimeout;
      {$ifdef USETCPPREFIX}
      fSocket.TCPPrefix := 'magic';
      {$endif}
      // note that first registered algo will be the prefered one
      if hcSynShaAes in Compression then
        // global SHA-256 / AES-256-CTR encryption + SynLZ compression
        fSocket.RegisterCompress(CompressShaAes,0); // CompressMinSize=0
      if hcSynLz in Compression then
        // SynLZ is very fast and efficient, perfect for a Delphi Client
        fSocket.RegisterCompress(CompressSynLZ);
      if hcDeflate in Compression then
        // standard (slower) AJAX/HTTP gzip compression
        fSocket.RegisterCompress(CompressGZip);
      result := true;
    except
      on Exception do begin
        FreeAndNil(fSocket);
        result := false;
      end;
    end;
  finally
    LeaveCriticalSection(fMutex);
  end;
end;

procedure TSQLHttpClientWinSock.InternalClose;
begin
  try
    FreeAndNil(fSocket);
  except
    ; // ignore any error here
  end;
end;

function TSQLHttpClientWinSock.InternalRequest(const url, method: RawUTF8;
  var Header, Data, DataType: RawUTF8): Int64Rec;
begin
  result.Lo := fSocket.Request(url,method,KeepAliveMS,Header,Data,DataType,false);
  result.Hi := GetCardinal(pointer(fSocket.HeaderValue('Server-InternalState')));
  Header := fSocket.HeaderGetText;
  Data := fSocket.Content;
end;


{ TSQLHttpClientWebsockets }

function TSQLHttpClientWebsockets.InternalCheckOpen: boolean;
begin
  if fSocket<>nil then begin
    result := true; // already connected
    exit;
  end;
  if fSocketClass=nil then
    fSocketClass := THttpClientWebSockets;
  result := inherited InternalCheckOpen;
  if result then
    with fWebSocketParams do
    if AutoUpgrade then
      result := WebSocketsUpgrade(Key,Ajax,Compression)='';
end;

function TSQLHttpClientWebsockets.FakeCallbackRegister(Sender: TServiceFactoryClient;
  const Method: TServiceMethod; const ParamInfo: TServiceMethodArgument;
  ParamValue: Pointer): integer;
begin
  if WebSockets=nil then
    raise EServiceException.CreateUTF8('Missing %.WebSocketsUpgrade() call '+
      'to enable interface parameter callbacks for %.%(%: %)',
      [self,Sender.InterfaceTypeInfo^.Name,Method.URI,
       ParamInfo.ParamName^,ParamInfo.ArgTypeName^]);
  result := fFakeCallbacks.DoRegister(
    ParamValue,TInterfaceFactory.Get(ParamInfo.ArgTypeInfo));
end;

function TSQLHttpClientWebsockets.FakeCallbackUnregister(
  Factory: TInterfaceFactory; FakeCallbackID: integer;
  Instance: pointer): boolean;
var ws: THttpClientWebSockets;
    body,resp: RawUTF8;
begin
  ws := WebSockets;
  if ws=nil then
    raise EServiceException.CreateUTF8('Missing %.WebSocketsUpgrade() call',[self]);
  body := FormatUTF8('{"%":%}',[Factory.InterfaceTypeInfo^.Name,FakeCallbackID]);
  result := CallBack(mPOST,'CacheFlush/_callback_',body,resp)=HTML_SUCCESS;
end;

function TSQLHttpClientWebsockets.WebSockets: THttpClientWebSockets;
begin
  if fSocket=nil then
    if not InternalCheckOpen then begin
      result := nil;
      exit;
    end;
  result := fSocket as THttpClientWebSockets;
  if not Assigned(result.CallbackRequestProcess) then
    result.CallbackRequestProcess := CallbackRequest;
end;

function TSQLHttpClientWebsockets.WebSocketsUpgrade(
  const aWebSocketsEncryptionKey: RawUTF8; aWebSocketsAJAX,
  aWebSocketsCompression: boolean): RawUTF8;
var sockets: THttpClientWebSockets;
begin
{$ifdef WITHLOG}
  fLogFamily.SynLog.Enter(self);
{$endif}
  sockets := WebSockets;
  if sockets=nil then
    result := 'Impossible to connect to the Server' else begin
    result := sockets.WebSocketsUpgrade(Model.Root,
      aWebSocketsEncryptionKey,aWebSocketsAJAX,aWebSocketsCompression);
    if result='' then
      with fWebSocketParams do begin // store parameters for auto-reconnection
        AutoUpgrade := true;
        Key := aWebSocketsEncryptionKey;
        Compression := aWebSocketsCompression;
        Ajax := aWebSocketsAJAX;
      end;
  end;
{$ifdef WITHLOG}
  with fLogFamily.SynLog do
    if result<>'' then
      Log(sllWarning,'"%" against %',[result,sockets],self) else
      Log(sllHTTP,'HTTP link upgraded to WebSockets using %',[sockets],self);
{$endif}
end;

function TSQLHttpClientWebsockets.CallbackRequest(Ctxt: THttpServerRequest): cardinal;
var i,methodIndex,fakeCallID: integer;
    url,root,interfmethod,interf,id,method,head,frames: RawUTF8;
    instance: pointer;
    factory: TInterfaceFactory;
    WR: TTextWriter;
procedure CallCurrentFrame;
begin
  factory.Methods[factory.MethodIndexCurrentFrameCallback].InternalExecute(
    [instance],pointer(frames),nil,head,result,[],false,nil,nil);
end;
begin
  result := HTML_BADREQUEST;
  if (Ctxt=nil) or
     ((Ctxt.InContentType<>'') and
      not IdemPropNameU(Ctxt.InContentType,JSON_CONTENT_TYPE)) then
    exit;
  url := Ctxt.URL;
  if url='' then
    exit;
  if url[1]='/' then
    system.delete(url,1,1);
  Split(Split(url,'/',root),'/',interfmethod,id); // 'root/BidirCallback.AsynchEvent/1'
  if not IdemPropNameU(root,Model.Root) then
    exit;
  fakeCallID := GetInteger(pointer(id));
  if fakeCallID<=0 then
    exit;
  fFakeCallbacks.Safe.Lock;
  try
    i := fFakeCallbacks.FindIndex(fakeCallID);
    if i<0 then
      exit;
    if interfmethod='_free_' then begin
      fFakeCallbacks.List[i].ReleasedFromServer := true;
      result := HTML_SUCCESS;
      exit;
    end;
    instance := fFakeCallbacks.List[i].Instance;
    factory := fFakeCallbacks.List[i].Factory;
  finally
    fFakeCallbacks.Safe.UnLock;
  end;
  if (Ctxt.InHeaders<>'') and
     (factory.MethodIndexCurrentFrameCallback>=0) then begin
    frames := FindIniNameValue(pointer(Ctxt.InHeaders),'SEC-WEBSOCKET-FRAME: ');
  end;
  split(interfmethod,'.',interf,method);
  methodIndex := factory.FindMethodIndex(method);
  if methodIndex<0 then
    exit;
  if IdemPropNameU(interfmethod,factory.Methods[methodIndex].InterfaceDotMethodName) then
  try
    WR := TJSONSerializer.CreateOwnedStream;
    try
      WR.AddShort('{"result":[');
      if frames='[0]' then
        CallCurrentFrame; // call before the first method of the jumbo frame
      if not factory.Methods[methodIndex].InternalExecute([instance],
         pointer(Ctxt.InContent),WR,head,result,[],False,nil,nil) then
        result := HTML_SERVERERROR else begin
        if head='' then begin
          WR.Add(']','}');
          result := HTML_SUCCESS;
        end else begin
          Ctxt.OutCustomHeaders := head;
          Ctxt.OutContentType := FindIniNameValue(pointer(head),HEADER_CONTENT_TYPE_UPPER);
        end;
        if Ctxt.OutContentType='' then
          Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
        Ctxt.OutContent := WR.Text;
      end;
      if frames='[1]' then
        CallCurrentFrame; // call after the last method of the jumbo frame
    finally
      WR.Free;
    end;
  except
    on E: Exception do begin
      Ctxt.OutContent := ObjectToJSONDebug(E);
      Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
      result := HTML_SERVERERROR;
    end;
  end;
end;


{ TSQLHttpClientRequest }

constructor TSQLHttpClientRequest.Create(const aServer, aPort: AnsiString;
  aModel: TSQLModel; aHttps: boolean; const aProxyName, aProxyByPass: AnsiString;
  SendTimeout,ReceiveTimeout,ConnectTimeout: DWORD);
begin
  inherited Create(aServer,aPort,aModel);
  fHttps := aHttps;
  fProxyName := aProxyName;
  fProxyByPass := aProxyByPass;
  fSendTimeout := SendTimeout;
  fReceiveTimeout := ReceiveTimeout;
  fConnectTimeout := ConnectTimeout;
end;

constructor TSQLHttpClientRequest.Create(const aServer,
  aPort: AnsiString; aModel: TSQLModel; SendTimeout,ReceiveTimeout,ConnectTimeout: DWORD);
begin
  Create(aServer,aPort,aModel,false,'','',SendTimeout,ReceiveTimeout,ConnectTimeout);
end;

function TSQLHttpClientRequest.InternalCheckOpen: boolean;
begin
  if fRequest=nil then
  try
    result := false;
    EnterCriticalSection(fMutex);
    try
      InternalSetClass;
      if fRequestClass=nil then
        raise ECommunicationException.CreateUTF8('fRequestClass=nil for %',[self]);
      fRequest := fRequestClass.Create(fServer,fPort,fHttps,
        fProxyName,fProxyByPass,fConnectTimeout,fSendTimeout,fReceiveTimeout);
      fRequest.ExtendedOptions := fExtendedOptions;
      // note that first registered algo will be the prefered one
      if hcSynShaAes in Compression then
        // global SHA-256 / AES-256-CTR encryption + SynLZ compression
        fRequest.RegisterCompress(CompressShaAes,0); // CompressMinSize=0
      if hcSynLz in Compression then
        // SynLZ is very fast and efficient, perfect for a Delphi Client
        fRequest.RegisterCompress(CompressSynLZ);
      if hcDeflate in Compression then
        // standard (slower) AJAX/HTTP zip/deflate compression
        fRequest.RegisterCompress(CompressGZip);
      result := true;
    except
      on Exception do
        FreeAndNil(fRequest);
    end;
  finally
    LeaveCriticalSection(fMutex);
  end else
    result := true; // already connected
end;

procedure TSQLHttpClientRequest.InternalClose;
begin
  FreeAndNil(fRequest);
end;

function TSQLHttpClientRequest.InternalRequest(const url, method: RawUTF8;
  var Header, Data, DataType: RawUTF8): Int64Rec;
var OutHeader, OutData: RawByteString;
begin
  if fRequest=nil then
    result.Lo := HTML_NOTIMPLEMENTED else begin
    result.Lo := fRequest.Request(url,method,KeepAliveMS,Header,Data,DataType,
      SockString(OutHeader),SockString(OutData));
    result.Hi := GetCardinal(pointer(
      FindIniNameValue(pointer(OutHeader),'SERVER-INTERNALSTATE: ')));
    Header := OutHeader;
    Data := OutData;
  end;
end;


{$ifdef USEWININET}

{ TSQLHttpClientWinINet }

procedure TSQLHttpClientWinINet.InternalSetClass;
begin
  fRequestClass := TWinINet;
  inherited;
end;


{ TSQLHttpClientWinHTTP }

procedure TSQLHttpClientWinHTTP.InternalSetClass;
begin
  fRequestClass := TWinHTTP;
  inherited;
end;

{$endif}

{$ifdef USELIBCURL}

{ TSQLHttpClientCurl}

procedure TSQLHttpClientCurl.InternalSetClass;
begin
  fRequestClass := TCurlHTTP;
  inherited;
end;

{$endif USELIBCURL}

initialization
  TSQLHttpClientWinSock.RegisterClassNameForDefinition;
{$ifdef USELIBCURL}
  TSQLHttpClientCurl.RegisterClassNameForDefinition;
{$endif}
{$ifdef USEWININET}
  TSQLHttpClientWinINet.RegisterClassNameForDefinition;
  TSQLHttpClientWinHTTP.RegisterClassNameForDefinition;
{$endif}
end.
