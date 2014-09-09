/// HTTP/1.1 RESTFUL JSON Client classes for mORMot
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit mORMotHttpClient;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2014 Arnaud Bouchez
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

  Portions created by the Initial Developer are Copyright (C) 2014
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



      HTTP/1.1 RESTFUL JSON Client for mORMot
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
      - HTTP/1.1 RESTFUL JSON Client and Server split into two units
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
	   - classes TSQLite3HttpClient* renamed as TSQLHttpClient*
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
     - added TSQLHttpClientGeneric.CreateForRemoteLogging() constructor for
       easy remote logging to our LogView tool, running as server process
        

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
  {$define USEWININET}
{$else}
  {$undef USEWININET}
{$endif}
  SysUtils,
  Classes,
  SynCrypto, // for hcSynShaAes
  SynZip,
  SynLZ,
  SynCrtSock,
  SynCommons,
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

  /// abstract HTTP/1.1 RESTFUL JSON mORMot Client class
  // - this class, and other inherited classes defined in this unit, are 
  // thread-safe, since each of their URI() method is protected by a giant lock
  TSQLHttpClientGeneric = class(TSQLRestClientURI)
  protected
    fKeepAliveMS: cardinal;
    fCompression: TSQLHttpCompressions;
    /// connection parameters as set by Create()
    fServer, fPort: AnsiString;
    procedure SetCompression(Value: TSQLHttpCompressions);
    procedure SetKeepAliveMS(Value: cardinal);
    /// process low-level HTTP/1.1 request
    // - call by URI() public method
    // - returns 200,202,204 if OK, http status error otherwise in result.Lo
    // - returns Server-InternalState in result.Hi
    function InternalRequest(const url, method: RawUTF8;
      var Header, Data, DataType: RawUTF8): Int64Rec; virtual; abstract;
    /// method calling the RESTful server fServer via HTTP/1.1
    // - calls the InternalRequest() protected method
    procedure InternalURI(var Call: TSQLRestURIParams); override;
  public
    /// connect to TSQLHttpServer on aServer:aPort
    constructor Create(const aServer, aPort: AnsiString; aModel: TSQLModel); reintroduce; virtual;
    /// connnect to a LogView HTTP Server for remote logging
    // - will associate the EchoCustom callback of the log class to this server
    // - the aLogClass.Family will manage this TSQLHttpClientGeneric instance
    // life time, until application is closed or Family.EchoRemoteStop is called 
    constructor CreateForRemoteLogging(const aServer, aPort: AnsiString;
      aLogClass: TSynLogClass; const aRoot: RawUTF8='LogService');
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

  /// HTTP/1.1 RESTFUL JSON mORMot Client class using SynCrtSock / WinSock
  // - will give the best performance on a local computer, but has been found
  // out to be slower over a network
  // - is not able to use secure HTTPS protocol
  // - note that, in its current implementation, this class is not thread-safe:
  // you need either to lock its access via a critical section, or initialize
  // one client instance per thread  
  TSQLHttpClientWinSock = class(TSQLHttpClientGeneric)
  protected
    /// internal HTTP/1.1 compatible client
    fSocket: THttpClientSocket;
    fSocketKeptAlive: cardinal;
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

{$ifdef USEWININET}
  /// HTTP/1.1 RESTFUL JSON mORMot Client abstract class using either WinINet
  // or WinHTTP API
  // - not to be called directly, but via TSQLHttpClientWinINet or (even
  // better) TSQLHttpClientWinHTTP overridden classes
  TSQLHttpClientWinGeneric = class(TSQLHttpClientGeneric)
  protected
    fWinAPI: TWinHttpAPI;
    fWinAPIClass: TWinHttpAPIClass;
    fProxyName, fProxyByPass: AnsiString;
    fSendTimeout, fReceiveTimeout: DWORD;
    fHttps: boolean;
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
    constructor Create(const aServer, aPort: AnsiString; aModel: TSQLModel); overload; override;
    /// connect to TSQLHttpServer on aServer:aPort
    // - optional aProxyName may contain the name of the proxy server to use,
    // and aProxyByPass an optional semicolon delimited list of host names or
    // IP addresses, or both, that should not be routed through the proxy
    // - you can customize the default client timeouts by setting appropriate
    // SendTimeout and ReceiveTimeout parameters (in ms) - note that after
    // creation of this instance, the connection is tied to those initial
    // parameters, so we won't publish any properties to change those
    // initial values once created
    constructor Create(const aServer, aPort: AnsiString; aModel: TSQLModel;
      aHttps: boolean; const aProxyName: AnsiString='';
      const aProxyByPass: AnsiString='';
      SendTimeout: DWORD=HTTP_DEFAULT_SENDTIMEOUT;
      ReceiveTimeout: DWORD=HTTP_DEFAULT_RECEIVETIMEOUT); reintroduce; overload; 
    /// internal class instance used for the connection
    // - will return either a TWinINet, either a TWinHTTP class instance
    property WinAPI: TWinHttpAPI read fWinAPI;
  end;

  /// HTTP/1.1 RESTFUL JSON mORMot Client class using WinINet API
  // - this class is 15/20 times slower than TSQLHttpClient using SynCrtSock
  // on a local machine, but was found to be faster throughout local networks
  // - this class is able to connect via the secure HTTPS protocol
  // - it will retrieve by default the Internet Explorer proxy settings, and
  // display some error messages or authentification dialog on screen
  // - you can optionaly specify manual Proxy settings at constructor level
  // - by design, the WinINet API should not be used from a service
  // - is implemented by creating a TWinINet internal class instance
  TSQLHttpClientWinINet = class(TSQLHttpClientWinGeneric)
  protected
    procedure InternalSetClass; override;
  end;                

  {{ HTTP/1.1 RESTFUL JSON Client class using WinHTTP API
   - has a common behavior as THttpClientSocket() but seems to be faster
     over a network and is able to retrieve the current proxy settings
     (if available) and handle secure HTTPS connection - so it seems to be used
     in your client programs: TSQLHttpClient will therefore map to this class
   - WinHTTP does not share directly any proxy settings with Internet Explorer.
     The default WinHTTP proxy configuration is set by either
     proxycfg.exe on Windows XP and Windows Server 2003 or earlier, either
     netsh.exe on Windows Vista and Windows Server 2008 or later; for instance,
     you can run "proxycfg -u" or "netsh winhttp import proxy source=ie" to use
     the current user's proxy settings for Internet Explorer (under 64 bit
     Vista/Seven, to configure applications using the 32 bit WinHttp settings,
     call netsh or proxycfg bits from %SystemRoot%\SysWOW64 folder explicitely)
   - you can optionaly specify manual Proxy settings at constructor level
   - by design, the WinHTTP API can be used from a service or a server
   - is implemented by creating a TWinHTTP internal class instance }
  TSQLHttpClientWinHTTP = class(TSQLHttpClientWinGeneric)
  protected
    procedure InternalSetClass; override;
  end;

  /// HTTP/1.1 RESTFUL JSON default mORMot Client class
  // - under Windows, maps the TSQLHttpClientWinHTTP class 
  TSQLHttpClient = TSQLHttpClientWinHTTP;
{$else}
  /// HTTP/1.1 RESTFUL JSON deault mORMot Client class
  // - not under Windows: maps the WinSock (raw socket) implementation class
  TSQLHttpClient = TSQLHttpClientWinSock;
{$endif}


implementation


{ TSQLHttpClientGeneric }

procedure TSQLHttpClientGeneric.InternalURI(var Call: TSQLRestURIParams);
var Head, Content, ContentType: RawUTF8;
    P: PUTF8Char;
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
      ContentType := JSON_CONTENT_TYPE;
    EnterCriticalSection(fMutex);
    try
        PInt64(@Call.OutStatus)^ := Int64(
          InternalRequest(Call.Url,Call.Method,Head,Content,ContentType));
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
  aModel: TSQLModel);
begin
  inherited Create(aModel);
  fServer := aServer;
  fPort := aPort;
  fKeepAliveMS := 20000; // 20 seconds connection keep alive by default
  fCompression := [hcSynLZ];
end;

constructor TSQLHttpClientGeneric.CreateForRemoteLogging(const aServer,
  aPort: AnsiString; aLogClass: TSynLogClass; const aRoot: RawUTF8);
var aModel: TSQLModel;
begin
  if not Assigned(aLogClass) then
    raise ECommunicationException.Create('No LogClass');
  aModel := TSQLModel.Create([],aRoot);
  Create(aServer,aPort,aModel);
  aModel.Owner := self;
  ServerRemoteLogStart(aLogClass,true);
  fRemoteLogClass.Log(sllTrace,
    'Echoing to remote server http://%/%/RemoteLog:%',[aServer,aRoot,aPort]);
end;


{ TSQLHttpClientWinSock }

function TSQLHttpClientWinSock.InternalCheckOpen: boolean;
begin
  if fSocket<>nil then begin
    result := true;
    exit;
  end;
  EnterCriticalSection(fMutex);
  try
    try
      fSocket := THttpClientSocket.Open(fServer,fPort,cslTCP,60000); // 60 sec timeout
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
  if KeepAliveMS<>fSocketKeptAlive then begin
    fSocketKeptAlive := KeepAliveMS;
    fSocket.KeepAlive := KeepAliveMS;
  end;
  result.Lo := fSocket.Request(url,method,KeepAliveMS,Header,Data,DataType,false);
  result.Hi := GetCardinal(pointer(fSocket.HeaderValue('Server-InternalState')));
  Header := fSocket.HeaderGetText;
  Data := fSocket.Content;
end;


{$ifdef USEWININET}

{ TSQLHttpClientWinGeneric }

constructor TSQLHttpClientWinGeneric.Create(const aServer, aPort: AnsiString;
  aModel: TSQLModel; aHttps: boolean; const aProxyName, aProxyByPass: AnsiString;
  SendTimeout,ReceiveTimeout: DWORD);
begin
  inherited Create(aServer,aPort,aModel);
  fHttps := aHttps;
  fProxyName := aProxyName;
  fProxyByPass := aProxyByPass;
  fSendTimeout := SendTimeout;
  fReceiveTimeout := ReceiveTimeout;
end;

constructor TSQLHttpClientWinGeneric.Create(const aServer,
  aPort: AnsiString; aModel: TSQLModel);
begin
  Create(aServer,aPort,aModel,false); // will use default settings
end;

function TSQLHttpClientWinGeneric.InternalCheckOpen: boolean;
begin
  result := false;
  if fWinAPI=nil then
  try
    EnterCriticalSection(fMutex);
    try
      InternalSetClass;
      if fWinAPIClass=nil then
        raise ECommunicationException.CreateFmt('fWinAPIClass=nil for %s',[ClassName]);
      fWinAPI := fWinAPIClass.Create(fServer,fPort,fHttps,fProxyName,fProxyByPass,
        fSendTimeout,fReceiveTimeout);
      // note that first registered algo will be the prefered one
      if hcSynShaAes in Compression then
        // global SHA-256 / AES-256-CTR encryption + SynLZ compression
        fWinAPI.RegisterCompress(CompressShaAes,0); // CompressMinSize=0
      if hcSynLz in Compression then
        // SynLZ is very fast and efficient, perfect for a Delphi Client
        fWinAPI.RegisterCompress(CompressSynLZ);
      if hcDeflate in Compression then
        // standard (slower) AJAX/HTTP zip/deflate compression
        fWinAPI.RegisterCompress(CompressGZip);
      result := true;
    except
      on Exception do
        FreeAndNil(fWinAPI);
    end;
  finally
    LeaveCriticalSection(fMutex);
  end else
    result := true;
end;

procedure TSQLHttpClientWinGeneric.InternalClose;
begin
  FreeAndNil(fWinAPI);
end;

function TSQLHttpClientWinGeneric.InternalRequest(const url, method: RawUTF8;
  var Header, Data, DataType: RawUTF8): Int64Rec;
var OutHeader, OutData: RawByteString;
begin
  if fWinAPI=nil then
    result.Lo := HTML_NOTIMPLEMENTED else begin
    result.Lo := fWinAPI.Request(url,method,KeepAliveMS,Header,Data,DataType,
      OutHeader,OutData);
    result.Hi := GetCardinal(pointer(
      FindIniNameValue(pointer(OutHeader),'SERVER-INTERNALSTATE: ')));
    Header := OutHeader;
    Data := OutData;
  end;
end;


{ TSQLHttpClientWinINet }

procedure TSQLHttpClientWinINet.InternalSetClass;
begin
  fWinAPIClass := TWinINet;
  inherited;
end;


{ TSQLHttpClientWinHTTP }

procedure TSQLHttpClientWinHTTP.InternalSetClass;
begin
  fWinAPIClass := TWinHTTP;
  inherited;
end;

{$endif}

end.
