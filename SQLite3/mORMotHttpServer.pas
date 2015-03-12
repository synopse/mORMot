/// HTTP/1.1 RESTFUL JSON Server classes for mORMot
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit mORMotHttpServer;

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
  - DigDiver (for HTTPS support)
  - cheemeng
  
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


     HTTP/1.1 RESTFUL JSON Client/Server for mORMot
    ************************************************

   - use internaly the JSON format for content communication
   - can be called by TSQLHttpClient class from direct Delphi clients
     (see mORMotHttpClient unit)
   - can be called by any JSON-aware AJAX application
   - can optionaly compress the returned data to optimize Internet bandwidth
   - speed is very high: more than 20MB/sec R/W localy on a 1.8GHz Sempron,
     i.e. 400Mb/sec of duplex raw IP data, with about 200 �s only elapsed
     by request (direct call is 50 �s, so bottle neck is the Win32 API),
     i.e. 5000 requests per second, with 113 result rows (i.e. 4803 bytes
     of JSON data each)... try to find a faster JSON HTTP server! ;)

    Initial version: 2009 May, by Arnaud Bouchez

    Version 1.1
      - code rewrite for FPC and Delphi 2009/2010 compilation

    Version 1.3 - January 22, 2010
      - some small fixes and multi-compiler enhancements

    Version 1.4 - February 8, 2010
      - whole Synopse SQLite3 database framework released under the GNU Lesser
        General Public License version 3, instead of generic "Public Domain"
      - HTTP/1.1 RESTFUL JSON Client and Server split into two units
        (SQLite3HttpClient and SQLite3HttpServer)

    Version 1.5 - February 22, 2010
      - by default, the SQlite3 unit is now not included, in order to save
        some space

    Version 1.8
      - includes Unitary Testing class and functions
      - allows content-type changing for GET blob fields

    Version 1.12
      - TSQLHttpServer now handle multiple TSQLRestServer instances
        per one server (dispatching requests via the Root URI used)
      - new AddServer method, to register a TSQLRestServer after launch
      - new TSQLRestServer.OnlyJSONRequests property

    Version 1.13
      - can now use fast http.sys kernel-mode server (THttpApiServer) if
        available, and our pure Delphi THttpServer on default
      - now can compress its content using deflate or faster SynLZ algorithm
      - by default, will handle SynLZ compression for TSQLHttpClient
      - can make TCP/IP stream not HTTP compliant (for security visions)
      - TSQLHttpServer.Request now uses the new THttpServerGeneric.Request
        virtual abstract method prototype to handle THttpApiServer

    Version 1.16
      - added optional aRestAccessRights parameter in TSQLHttpServer.AddServer
        to override the default HTTP_DEFAULT_ACCESS_RIGHTS settings
      - added ServerThreadPoolCount parameter to TSQLHttpServer.Create()
        constructor, set by default to 32 - will speed up process of slow
        requests (e.g. a POST with some huge data transmitted at slow rate)
      - fixed error in case of URI similar to 'root?session_signature=...'
      - fixed incorect thread count in TSQLHttpServer.Create
      - regression tests are now extracted from this unit, in order to allow
        construction of a TSQLHttpServer instance without the need
        of linking the SQLite3 engine to the executable

    Version 1.17
      - made URI check case-insensitive (as for official RFC)
      - TSQLHttpServer will now call virtual TSQLRestServer.EndCurrentThread
        method in each of its terminating threads, to release any thread-specific
        resources (for instance, external connections in SQlite3DB)

    Version 1.18
      - renamed SQLite3HttpServer.pas unit to mORMotHttpServer.pas
      - classes TSQLHttpServer* renamed as TSQLHttpServer*
      - added TSQLHttpServer.RemoveServer() and Shutdown methods
      - added TSQLHttpServer.Port and DomainName properties
      - added TSQLHttpServer.AccessControlAllowOrigin property to handle
        cross-site AJAX requests via cross-origin resource sharing (CORS)
      - TSQLHttpServer now handles sub-domains generic matching (via
        TSQLModel.URIMatch call) at database model level (e.g. you can set
        root='/root/sub1' URIs)
      - added TSQLHttpServer.DomainHostRedirect() method for virtual-host
        redirection of Internet domains or sub-domains
      - declared TSQLHttpServer.Request method as virtual, to allow easiest
        customization, like direct file sending to the clients
      - added TSQLHttpServerOptions parameter to TSQLHttpServer.Create(),
        allowing optional auto-registration of the URI to http.sys internals
      - added aHttpServerSecurity: TSQLHttpServerSecurity parameter to
        TSQLHttpServer.Create(), allowing HTTPS secure content publishing, when
        using the http.sys kind of server, or our proprietary SHA-256 /
        AES-256-CTR encryption identified as "ACCEPT-ENCODING: synshaaes"
      - added optional aAdditionalURL parameter to TSQLHttpServer.Create(), to
        be used e.g. to registry an URI to server static file content in addition
        to TSQLRestServer instances - need to override TSQLHttpServer.Request()
      - added optional parameter to TSQLHttpServer.Create(), to specify a
        name for the THttpApiServer HTTP queue
      - COMPRESSDEFLATE conditional will use gzip (and not deflate/zip)
      - added TSQLHTTPRemoteLogServer class for easy remote log serving
      - ensure TSQLHttpServer.AddServer() will handle useHttpApiRegisteringURI
      - added TSQLHttpServer.RootRedirectToURI() method for root URI redirection
      - declared TSQLHttpServer.HttpThreadStart/HttpThreadTerminate as virtual
      - allow TSQLHttpServer.Create() without any associated TSQLRestServer

}

interface

{$define COMPRESSSYNLZ}
{ if defined, will use SynLZ for content compression
  - SynLZ is much faster than deflate/zip, so is preferred
  - can be set global for Client and Server applications
  - with SynLZ, the 440 KB JSON for TTestClientServerAccess._TSQLHttpClient
    is compressed into 106 KB with no speed penalty (it's even a bit faster)
    whereas deflate, even with its level set to 1 (fastest), is 25 % slower
  - TSQLHttpClientGeneric.Compression shall contain hcSynLZ to handle it }

{$define COMPRESSDEFLATE}
{ if defined, will use gzip (and not deflate/zip) for content compression
  - can be set global for Client and Server applications
  - deflate/zip is just broken between browsers and client, and should be
    avoided: see http://stackoverflow.com/a/9186091/458259
  - SynLZ is faster but only known by Delphi clients: you can enable deflate
    when the server is connected an AJAX application (not defined by default)
  - if you define both COMPRESSSYNLZ and COMPRESSDEFLATE, the server will use
    SynLZ if available, and deflate if not called from a Delphi client
  - TSQLHttpClientGeneric.Compression shall contain hcDeflate to handle it }

{.$define USETCPPREFIX}
{ if defined, a prefix will be added to the TCP/IP stream so that it won't be
  valid HTTP content any more: it could increase the client/server speed with
  some anti-virus software, but the remote access won't work any more with
  Internet Browsers nor AJAX applications
  - will work only with our THttpServer pure Delphi code, of course
  - not defined by default - should be set globally to the project conditionals,
  to be defined in both mORMotHttpClient and mORMotHttpServer units }

{$I Synopse.inc} // define HASINLINE WITHLOG USETHREADPOOL ONLYUSEHTTPSOCKET


uses
{$ifdef MSWINDOWS}
  Windows,
{$endif}
{$ifdef KYLIX3}
  Types,
  LibC,
{$endif}
  SysUtils,
  Classes,
{$ifdef COMPRESSDEFLATE}
  SynZip,
{$endif}
{$ifdef COMPRESSSYNLZ}
  SynLZ,
{$endif}
  SynCrtSock,
  SynCrypto, // for CompressShaAes()
  SynCommons,
  SynLog,
  mORMot;

type
  /// available running options for TSQLHttpServer.Create() constructor
  // - useHttpApi to use kernel-mode HTTP.SYS server (THttpApiServer) with an
  // already registered URI (default way, similar to IIS/WCF security policy
  // as specified by Microsoft) - you would need to register the URI by hand,
  // e.g. in the Setup program, via code similar to this one:
  // ! THttpApiServer.AddUrlAuthorize('root','888',false,'+'))
  // - useHttpApiRegisteringURI will first registry the given URI, then use
  // kernel-mode HTTP.SYS server (THttpApiServer) - will need Administrator
  // execution rights at least one time (e.g. during setup); note that if
  // the URI is already registered, the server will still be launched, even if
  // the program does not run as Administrator - it is therefore sufficient
  // to run such a program once as Administrator to register the URI, when this
  // useHttpApiRegisteringURI option is set
  // - useHttpSocket will use the standard Sockets library (i.e. socket-based
  // THttpServer) - it will trigger the Windows firewall popup UAC window at
  // first execution
  // - the first item should be the preferred one (see HTTP_DEFAULT_MODE)
  TSQLHttpServerOptions =
    ({$ifndef ONLYUSEHTTPSOCKET}useHttpApi, useHttpApiRegisteringURI, {$endif}
     useHttpSocket, useBidirSocket);

  /// available security options for TSQLHttpServer.Create() constructor
  // - default secNone will use plain HTTP connection
  // - secSSL will use HTTPS secure connection
  // - secSynShaAes will use our proprietary SHA-256 / AES-256-CTR encryption
  // identified as 'synshaaes' as ACCEPT-ENCODING: header parameter
  TSQLHttpServerSecurity =
    (secNone, secSSL, secSynShaAes);

const
  /// the default access rights used by the HTTP server if none is specified
  HTTP_DEFAULT_ACCESS_RIGHTS: PSQLAccessRights = @SUPERVISOR_ACCESS_RIGHTS;

  /// the kind of HTTP server to be used by default
  // - will define the best available server class, depending on the platform
  HTTP_DEFAULT_MODE = low(TSQLHttpServerOptions);

type
  /// HTTP/1.1 RESTFUL JSON mORMot Server class
  // - this server is multi-threaded and not blocking
  // - will first try to use fastest http.sys kernel-mode server (i.e. create a
  // THttpApiServer instance); it should work OK under XP or WS 2K3 - but
  // you need to have administrator rights under Vista or Seven: if http.sys
  // fails to initialize, it will use a pure Delphi THttpServer instance; a
  // solution is to call the THttpApiServer.AddUrlAuthorize class method during
  // program setup for the desired port, in order to allow it for every user
  // - just create it and it will serve SQL statements as UTF-8 JSON
  // - for a true AJAX server, expanded data is prefered - your code may contain:
  // ! DBServer.NoAJAXJSON := false;
  TSQLHttpServer = class
  protected
    fOnlyJSONRequests: boolean;
    fHttpServer: THttpServerGeneric;
    fPort, fDomainName: AnsiString;
    /// internal servers to compute responses
    fDBServers: array of record
      Server: TSQLRestServer;
      RestAccessRights: PSQLAccessRights;
      Security: TSQLHttpServerSecurity;
    end;
    fHosts: TSynNameValue;
    fAccessControlAllowOrigin: RawUTF8;
    fAccessControlAllowOriginHeader: RawUTF8;
    fRootRedirectToURI: array[boolean] of RawUTF8;
    fHttpServerKind: TSQLHttpServerOptions;
    fLog: TSynLogClass;
    procedure SetAccessControlAllowOrigin(const Value: RawUTF8);
    // assigned to fHttpServer.OnHttpThreadStart/Terminate e.g. to handle connections
    procedure HttpThreadStart(Sender: TThread); virtual;
    procedure HttpThreadTerminate(Sender: TThread); virtual;
    /// implement the server response - must be thread-safe
    function Request(Ctxt: THttpServerRequest): cardinal; virtual;
    function GetDBServerCount: integer;
    function GetDBServer(Index: Integer): TSQLRestServer;
    procedure SetDBServerAccessRight(Index: integer; Value: PSQLAccessRights);
    function HttpApiAddUri(const aRoot,aDomainName: RawByteString;
      aSecurity: TSQLHttpServerSecurity; aRegisterURI,aRaiseExceptionOnError: boolean): RawUTF8;
  public
    /// create a Server Thread, binded and listening on a TCP port to HTTP JSON requests
    // - raise a EHttpServer exception if binding failed
    // - specify one or more TSQLRestServer server class to be used: each
    // class must have an unique Model.Root value, to identify which TSQLRestServer
    // instance must handle a particular request from its URI
    // - port is an AnsiString, as expected by the WinSock API
    // - aDomainName is the URLprefix to be used for HttpAddUrl API call:
    // it could be either a fully qualified case-insensitive domain name
    // an IPv4 or IPv6 literal string, or a wildcard ('+' will bound
    // to all domain names for the specified port, '*' will accept the request
    // when no other listening hostnames match the request for that port) - this
    // parameter is ignored by the TSQLHttpApiServer instance
    // - aHttpServerKind defines how the HTTP server itself will be implemented:
    // it will use by default optimized useHttpApi kernel-based http.sys server,
    // optionally registering the URI (useHttpApiRegisteringURI) if needed,
    // or using the standard Sockets library (useHttpSocket)
    // - by default, the PSQLAccessRights will be set to nil
    // - the ServerThreadPoolCount parameter will set the number of threads
    // to be initialized to handle incoming connections (default is 32, which
    // may be sufficient for most cases, maximum is 256)
    // - the aHttpServerSecurity can be set to secSSL to initialize a HTTPS
    // instance (after proper certificate installation as explained in the SAD
    // pdf), or to secSynShaAes if you want our proprietary SHA-256 /
    // AES-256-CTR encryption identified as "ACCEPT-ENCODING: synshaaes"
    // - optional aAdditionalURL parameter can be used e.g. to registry an URI
    // to server static file content, by overriding TSQLHttpServer.Request
    // - for THttpApiServer, you can specify an optional name for the HTTP queue
    constructor Create(const aPort: AnsiString;
      const aServers: array of TSQLRestServer; const aDomainName: AnsiString='+';
      aHttpServerKind: TSQLHttpServerOptions=HTTP_DEFAULT_MODE; ServerThreadPoolCount: Integer=32;
      aHttpServerSecurity: TSQLHttpServerSecurity=secNone;
      const aAdditionalURL: AnsiString=''; const aQueueName: SynUnicode=''); reintroduce; overload;
    /// create a Server Thread, binded and listening on a TCP port to HTTP JSON requests
    // - raise a EHttpServer exception if binding failed
    // - specify one TSQLRestServer server class to be used
    // - port is an AnsiString, as expected by the WinSock API
    // - aDomainName is the URLprefix to be used for HttpAddUrl API call
    // - the aHttpServerSecurity can be set to secSSL to initialize a HTTPS
    // instance (after proper certificate installation as explained in the SAD
    // pdf), or to secSynShaAes if you want our proprietary SHA-256 /
    // AES-256-CTR encryption identified as "ACCEPT-ENCODING: synshaaes"
    // - optional aAdditionalURL parameter can be used e.g. to registry an URI
    // to server static file content, by overriding TSQLHttpServer.Request
    // - for THttpApiServer, you can specify an optional name for the HTTP queue
    constructor Create(const aPort: AnsiString; aServer: TSQLRestServer;
      const aDomainName: AnsiString='+';
      aHttpServerKind: TSQLHttpServerOptions=HTTP_DEFAULT_MODE; aRestAccessRights: PSQLAccessRights=nil;
      ServerThreadPoolCount: Integer=32; aHttpServerSecurity: TSQLHttpServerSecurity=secNone;
      const aAdditionalURL: AnsiString=''; const aQueueName: SynUnicode=''); reintroduce; overload;
    /// release all memory, internal mORMot server and HTTP handlers
    destructor Destroy; override;
    /// you can call this method to prepare the HTTP server for shutting down
    // - it will call all associated TSQLRestServer.Shutdown methods
    // - note that Destroy won't call this method on its own, since the
    // TSQLRestServer instances may have a life-time uncoupled from HTTP process
    procedure Shutdown;
    /// try to register another TSQLRestServer instance to the HTTP server
    // - each TSQLRestServer class must have an unique Model.Root value, to
    // identify which instance must handle a particular request from its URI
    // - an optional aRestAccessRights parameter is available to override the
    // default HTTP_DEFAULT_ACCESS_RIGHTS access right setting - but you shall
    // better rely on the authentication feature included in the framework
    // - the aHttpServerSecurity can be set to secSSL to initialize a HTTPS
    // instance (after proper certificate installation as explained in the SAD
    // pdf), or to secSynShaAes if you want our proprietary SHA-256 /
    // AES-256-CTR encryption identified as "ACCEPT-ENCODING: synshaaes"
    // - return true on success, false on error (e.g. duplicated Root value)
    function AddServer(aServer: TSQLRestServer; aRestAccessRights: PSQLAccessRights=nil;
      aHttpServerSecurity: TSQLHttpServerSecurity=secNone): boolean;
    /// un-register a TSQLRestServer from the HTTP server
    // - each TSQLRestServer class must have an unique Model.Root value, to
    // identify which instance must handle a particular request from its URI
    // - return true on success, false on error (e.g. specified server not found)
    function RemoveServer(aServer: TSQLRestServer): boolean;
    /// register a domain name to be redirected to a given Model.Root
    // - i.e. can be used to support some kind of virtual hosting
    // - by default, the URI would be used to identify which TSQLRestServer
    // instance to use, and the incoming HOST value would just be ignored
    // - you can specify here domain names which would be checked against
    // the incoming HOST header, to redirect to a given URI, as such:
    // ! DomainHostRedirect('project1.com','root1');
    // ! DomainHostRedirect('project2.com','root2');
    // ! DomainHostRedirect('blog.project2.com','root2/blog');
    // for the last entry, you may have for instance initialized a MVC web
    // server on the 'blog' sub-URI of the 'root2' TSQLRestServer via:
    // !constructor TMyMVCApplication.Create(aRestModel: TSQLRest; aInterface: PTypeInfo);
    // ! ...
    // ! fMainRunner := TMVCRunOnRestServer.Create(self,nil,'blog');
    // ! ...
    // - if aURI='' is given, the corresponding host redirection will be disabled
    procedure DomainHostRedirect(const aDomain,aURI: RawUTF8);
    /// allow to temporarly redirect ip:port root URI to a given sub-URI
    // - by default, only sub-URI, as defined by TSQLRestServer.Model.Root, are
    // registered - you can define here a sub-URI to reach when the main server
    // is directly accessed from a browser, e.g. localhost:port will redirect to
    // localhost:port/RedirectedURI
    // - for http.sys server, would try to register '/' if aRegisterURI is TRUE
    // - by default, will redirect http://localhost:port unless you set
    // aHttpServerSecurity=secSSL so that it would redirect https://localhost:port
    procedure RootRedirectToURI(const aRedirectedURI: RawUTF8;
      aRegisterURI: boolean=true; aHttps: boolean=false);
    /// the associated running HTTP server instance
    // - either THttpApiServer, either THttpServer
    property HttpServer: THttpServerGeneric read fHttpServer;
    /// the TCP/IP port on which this server is listening to
    property Port: AnsiString read fPort;
    /// the URLprefix used for internal HttpAddUrl API call
    property DomainName: AnsiString read fDomainName;
    /// read-only access to the number of registered internal servers
    property DBServerCount: integer read GetDBServerCount;
    /// read-only access to all internal servers
    property DBServer[Index: integer]: TSQLRestServer read GetDBServer;
    /// write-only access to all internal servers access right
    // - can be used to override the default HTTP_DEFAULT_ACCESS_RIGHTS setting
    property DBServerAccessRight[Index: integer]: PSQLAccessRights write SetDBServerAccessRight;
    /// set this property to TRUE if the server must only respond to
    // request of MIME type APPLICATION/JSON
    // - the default is false, in order to allow direct view of JSON from
    // any browser
    property OnlyJSONRequests: boolean read fOnlyJSONRequests write fOnlyJSONRequests;
    /// enable cross-origin resource sharing (CORS) for proper AJAX process
    // - see @https://developer.mozilla.org/en-US/docs/HTTP/Access_control_CORS
    // - can be set e.g. to '*' to allow requests from any sites
    // - or specify an URI to be allowed as origin (e.g. 'http://foo.example')
    // - current implementation is pretty basic, and does not check the incoming
    // "Origin: " header value
    property AccessControlAllowOrigin: RawUTF8 read fAccessControlAllowOrigin write SetAccessControlAllowOrigin;
  end;

  /// callback expected by TSQLHTTPRemoteLogServer to notify about a received log
  TRemoteLogReceivedOne = procedure(const Text: RawUTF8) of object;

  {$M+}
  /// limited HTTP server which is will receive remote log notifications
  // - this will create a simple in-memory mORMot server, which will trigger
  // a supplied callback when a remote log is received
  // - see TSQLHttpClientWinGeneric.CreateForRemoteLogging() for the client side
  // - used e.g. by the LogView tool
  TSQLHTTPRemoteLogServer = class(TSQLHttpServer)
  protected
    fServer: TSQLRestServerFullMemory;
    fEvent: TRemoteLogReceivedOne;
  public
    /// initialize the HTTP server and an internal mORMot server
    // - you can share several HTTP log servers on the same port, if you use
    // a dedicated root URI and use the http.sys server (which is the default)
    constructor Create(const aRoot: RawUTF8; aPort: integer;
      const aEvent: TRemoteLogReceivedOne);
    /// release the HTTP server and its internal mORMot server
    destructor Destroy; override;
    /// the associated mORMot server instance running with this HTTP server
    property Server: TSQLRestServerFullMemory read fServer;
  published
    /// this HTTP server will publish a 'RemoteLog' method-based service
    // - expecting PUT with text as body, at http://server/root/RemoteLog
    procedure RemoteLog(Ctxt: TSQLRestServerURIContext);
  end;
  {$M-}


implementation


{ TSQLHttpServer }

function TSQLHttpServer.AddServer(aServer: TSQLRestServer;
  aRestAccessRights: PSQLAccessRights; aHttpServerSecurity: TSQLHttpServerSecurity): boolean;
var i,n: integer;
begin
  result := False;
  if (self=nil) or (aServer=nil) or (aServer.Model=nil) then
    exit;
  fLog.Enter(self);
  try
    n := length(fDBServers);
    for i := 0 to n-1 do
      if (fDBServers[i].Server.Model.URIMatch(aServer.Model.Root)) and
         (fDBServers[i].Security=aHttpServerSecurity) then
        exit; // register only once per URI Root address and per protocol
    {$ifndef ONLYUSEHTTPSOCKET}
    if HttpApiAddUri(aServer.Model.Root,fDomainName,aHttpServerSecurity,
       fHttpServerKind=useHttpApiRegisteringURI,false)<>'' then
      exit;
    {$endif}
    SetLength(fDBServers,n+1);
    with fDBServers[n] do begin
      Server := aServer;
      Security := aHttpServerSecurity;
      if aRestAccessRights=nil then
        RestAccessRights := HTTP_DEFAULT_ACCESS_RIGHTS else
        RestAccessRights := aRestAccessRights;
    end;
    result := true;
  finally
    fLog.Add.Log(sllDebug,'%.AddServer(%,Root=%,Port=%)=%',
      [self,aServer,aServer.Model.Root,fPort,JSON_BOOLEAN[Result]],self);
  end;
end;

function TSQLHttpServer.RemoveServer(aServer: TSQLRestServer): boolean;
var i,j,n: integer;
begin
  result := False;
  if (self=nil) or (aServer=nil) or (aServer.Model=nil) then
    exit;
  fLog.Enter(self);
  try
  n := high(fDBServers);
  for i := 0 to n do
    if fDBServers[i].Server=aServer then begin
      {$ifndef ONLYUSEHTTPSOCKET}
      if fHttpServer.InheritsFrom(THttpApiServer) then
        if THttpApiServer(fHttpServer).RemoveUrl(aServer.Model.Root,fPort,
           fDBServers[i].Security=secSSL,fDomainName)<>NO_ERROR then
          fLog.Add.Log(sllLastError,'%.RemoveUrl(%)',[self,aServer.Model.Root],self);
      {$endif}
      for j := i to n-1 do
        fDBServers[j] := fDBServers[j+1];
      SetLength(fDBServers,n);
      result := true; // don't break here: may appear with another Security  
    end;
  finally
    fLog.Add.Log(sllDebug,'%.RemoveServer(Root=%)=%',
      [self,aServer.Model.Root,JSON_BOOLEAN[Result]],self);
  end;
end;

procedure TSQLHttpServer.DomainHostRedirect(const aDomain,aURI: RawUTF8);
begin
  if aURI='' then
    fHosts.Delete(aDomain) else
    fHosts.Add(aDomain,aURI);
end;

constructor TSQLHttpServer.Create(const aPort: AnsiString;
  const aServers: array of TSQLRestServer; const aDomainName: AnsiString;
  aHttpServerKind: TSQLHttpServerOptions; ServerThreadPoolCount: Integer;
  aHttpServerSecurity: TSQLHttpServerSecurity; const aAdditionalURL: AnsiString;
  const aQueueName: SynUnicode);
var i,j: integer;
    ServersRoot: RawUTF8;
    ErrMsg: RawUTF8;
begin
  {$ifdef WITHLOG} // this is the only place where we check for WITHLOG
  if high(aServers)<0 then
    fLog := TSQLLog else
    fLog := aServers[0].LogClass;
  fLog.Enter(self);
  {$endif}
  inherited Create;
  SetAccessControlAllowOrigin(''); // deny CORS by default
  fHosts.Init(false);
  fDomainName := aDomainName;
  fPort := aPort;
  fHttpServerKind := aHttpServerKind;
  if high(aServers)>=0 then begin
    for i := 0 to high(aServers) do
      if (aServers[i]=nil) or (aServers[i].Model=nil) then
        ErrMsg := 'Invalid TSQLRestServer';
    if ErrMsg='' then
      for i := 0 to high(aServers) do
      with aServers[i].Model do begin
        ServersRoot := ServersRoot+' '+Root;
        for j := i+1 to high(aServers) do
          if aServers[j].Model.URIMatch(Root) then
            ErrMsg:= FormatUTF8('Duplicated Root URI: % and %',[Root,aServers[j].Model.Root]);
      end;
    if ErrMsg<>'' then
       raise EModelException.CreateUTF8('%.Create(% ): %',[self,ServersRoot,ErrMsg]);
    SetLength(fDBServers,length(aServers));
    for i := 0 to high(aServers) do
    with fDBServers[i] do begin
      Server := aServers[i];
      RestAccessRights := HTTP_DEFAULT_ACCESS_RIGHTS;
      Security := aHttpServerSecurity;
    end;
  end;
  {$ifndef USETCPPREFIX}
  {$ifndef ONLYUSEHTTPSOCKET}
  if aHttpServerKind in [useHttpApi,useHttpApiRegisteringURI] then
  try
    // first try to use fastest http.sys
    fHttpServer := THttpApiServer.Create(false,aQueueName);
    for i := 0 to high(aServers) do
      HttpApiAddUri(aServers[i].Model.Root,fDomainName,aHttpServerSecurity,
        fHttpServerKind=useHttpApiRegisteringURI,true);
    if aAdditionalURL<>'' then
      HttpApiAddUri(aAdditionalURL,fDomainName,aHttpServerSecurity,
        fHttpServerKind=useHttpApiRegisteringURI,true);
  except
    on E: Exception do begin
      fLog.Add.Log(sllError,'% for % at%  -> fallback to socket-based server',
        [E,fHttpServer,ServersRoot],self);
      FreeAndNil(fHttpServer); // if http.sys initialization failed
    end;
  end;
  {$endif}
  {$endif}
  if fHttpServer=nil then begin
    // http.sys failed -> create one instance of our pure Delphi server
    fHttpServer := THttpServer.Create(aPort
      {$ifdef USETHREADPOOL},ServerThreadPoolCount{$endif});
    {$ifdef USETCPPREFIX}
    THttpServer(fHttpServer).TCPPrefix := 'magic';
    {$endif}
  end;
  fHttpServer.OnHttpThreadStart := HttpThreadStart;
  fHttpServer.OnRequest := Request;
  fHttpServer.OnHttpThreadTerminate := HttpThreadTerminate;
  if aHttpServerSecurity=secSynShaAes then
    fHttpServer.RegisterCompress(CompressShaAes,0); // CompressMinSize=0
{$ifdef COMPRESSSYNLZ} // SynLZ registered first, since will be prefered
  fHttpServer.RegisterCompress(CompressSynLZ);
{$endif}
{$ifdef COMPRESSDEFLATE}
  fHttpServer.RegisterCompress(CompressGZip);
{$endif}
{$ifndef ONLYUSEHTTPSOCKET}
  if fHttpServer.InheritsFrom(THttpApiServer) then
    // allow fast multi-threaded requests
    if ServerThreadPoolCount>1 then
      THttpApiServer(fHttpServer).Clone(ServerThreadPoolCount-1);
{$endif}
  fLog.Add.Log(sllInfo,'% initialized for%',[fHttpServer,ServersRoot],self);
end;

constructor TSQLHttpServer.Create(const aPort: AnsiString;
  aServer: TSQLRestServer; const aDomainName: AnsiString;
  aHttpServerKind: TSQLHttpServerOptions; aRestAccessRights: PSQLAccessRights;
  ServerThreadPoolCount: integer; aHttpServerSecurity: TSQLHttpServerSecurity;
  const aAdditionalURL: AnsiString; const aQueueName: SynUnicode);
begin
  Create(aPort,[aServer],aDomainName,aHttpServerKind,ServerThreadPoolCount,
    aHttpServerSecurity,aAdditionalURL,aQueueName);
  if aRestAccessRights<>nil then
    DBServerAccessRight[0] := aRestAccessRights;
end;

destructor TSQLHttpServer.Destroy;
begin
  fLog.Enter(self).Log(sllInfo,'% finalized for % server(s)',
    [fHttpServer,length(fDBServers)],self);
  FreeAndNil(fHttpServer);
  inherited Destroy;
end;

procedure TSQLHttpServer.Shutdown;
var i: integer;
begin
  if self<>nil then
    for i := 0 to high(fDBServers) do
      fDBServers[i].Server.Shutdown;
end;

function TSQLHttpServer.GetDBServer(Index: Integer): TSQLRestServer;
begin
  if (Self<>nil) and (cardinal(Index)<cardinal(length(fDBServers))) then
    result := fDBServers[Index].Server else
    result := nil;
end;

function TSQLHttpServer.GetDBServerCount: integer;
begin
  result := length(fDBServers);
end;

procedure TSQLHttpServer.SetDBServerAccessRight(Index: integer;
  Value: PSQLAccessRights);
begin
  if Value=nil then
    Value := HTTP_DEFAULT_ACCESS_RIGHTS;
  if (Self<>nil) and (cardinal(Index)<cardinal(length(fDBServers))) then
    fDBServers[Index].RestAccessRights := Value;
end;

const
  HTTPS_TEXT: array[boolean] of string[1] = ('','s');
  HTTPS_SECURITY: array[boolean] of TSQLHttpServerSecurity = (secNone, secSSL);

procedure TSQLHttpServer.RootRedirectToURI(const aRedirectedURI: RawUTF8;
  aRegisterURI,aHttps: boolean);
begin
  if fRootRedirectToURI[aHttps]=aRedirectedURI then
    exit;
  fLog.Add.Log(sllDebug,'Redirect http%://localhost:% to http%://localhost:%/%',
    [HTTPS_TEXT[aHttps],Port,HTTPS_TEXT[aHttps],Port,aRedirectedURI],self);
  fRootRedirectToURI[aHttps] := aRedirectedURI;
  if aRedirectedURI<>'' then
    HttpApiAddUri('/','+',HTTPS_SECURITY[aHttps],aRegisterURI,true);
end;

function TSQLHttpServer.HttpApiAddUri(const aRoot,aDomainName: RawByteString;
  aSecurity: TSQLHttpServerSecurity; aRegisterURI,aRaiseExceptionOnError: boolean): RawUTF8;
{$ifndef ONLYUSEHTTPSOCKET}
var err: integer;
    https: boolean;
{$endif}
begin
  result := ''; // no error
  {$ifndef ONLYUSEHTTPSOCKET}
  if not fHttpServer.InheritsFrom(THttpApiServer) then
    exit;
  https := aSecurity=secSSL;
  fLog.Add.Log(sllInfo,'http.sys registration of http%://%:%/%',
    [HTTPS_TEXT[https],aDomainName,fPort,aRoot],self);
  // try to register the URL to http.sys
  err := THttpApiServer(fHttpServer).AddUrl(aRoot,fPort,https,aDomainName,aRegisterURI);
  if err=NO_ERROR then
    exit;
  result := FormatUTF8('http.sys URI registration error #% for http%://%:%/%',
    [err,HTTPS_TEXT[https],aDomainName,fPort,aRoot]);
  if err=ERROR_ACCESS_DENIED then
    if aRegisterURI then
      result := result+' (administrator rights needed)' else
      result := result+' (you need to register the URI )';
  fLog.Add.Log(sllLastError,result,self);
  if aRaiseExceptionOnError then
    raise ECommunicationException.CreateUTF8('%: %',[self,result]);
  {$endif}
end;

function TSQLHttpServer.Request(Ctxt: THttpServerRequest): cardinal;
var call: TSQLRestURIParams;
    i,len: integer;
    P: PUTF8Char;
    host,redirect: RawUTF8;
begin
  if ((Ctxt.URL='') or (Ctxt.URL='/')) and (Ctxt.Method='GET') then
    if fRootRedirectToURI[Ctxt.UseSSL]<>'' then begin
      Ctxt.OutCustomHeaders := 'Location: '+fRootRedirectToURI[Ctxt.UseSSL];
      result := HTML_TEMPORARYREDIRECT;
    end else
      result := HTML_BADREQUEST else
  if (Ctxt.Method='') or (OnlyJSONRequests and
     not IdemPChar(pointer(Ctxt.InContentType),'APPLICATION/JSON')) then
    // wrong Input parameters or not JSON request: 400 BAD REQUEST
    result := HTML_BADREQUEST else
  if Ctxt.Method='OPTIONS' then begin
    // handle CORS headers control
    Ctxt.OutCustomHeaders := 'Access-Control-Allow-Headers: '+
      FindIniNameValue(pointer(Ctxt.InHeaders),'ACCESS-CONTROL-REQUEST-HEADERS: ')+
      fAccessControlAllowOriginHeader;
    result := HTML_SUCCESS;
  end else begin
    // compute URI, handling any virtual host domain
    fillchar(call,sizeof(call),0);
    call.LowLevelRequest := Ctxt;
    if Ctxt.UseSSL then
      include(call.LowLevelFlags,llfSSL);
    if fHosts.Count>0 then begin
      host := FindIniNameValue(pointer(Ctxt.InHeaders),'HOST: ');
      i := PosEx(':',host);
      if i>0 then
        SetLength(host,i-1); // trim any port
      if host<>'' then
        host := fHosts.Value(host);
    end;
    if host<>'' then
      if (Ctxt.URL='') or (Ctxt.URL='/') then
        call.Url := host else
        if Ctxt.URL[1]='/' then
          call.Url := host+Ctxt.URL else
          call.Url := host+'/'+Ctxt.URL else
      if Ctxt.URL[1]='/' then
        call.Url := copy(Ctxt.URL,2,maxInt) else
        call.Url := Ctxt.URL;
    // search and call any matching TSQLRestServer instance
    result := HTML_NOTFOUND; // page not found by default (in case of wrong URL)
    for i := 0 to high(fDBServers) do
    with fDBServers[i] do
      if Ctxt.UseSSL=(Security=secSSL) then // registered for http or https
      if Server.Model.URIMatch(call.Url) then begin
        call.Method := Ctxt.Method;
        call.InHead := Ctxt.InHeaders;
        call.InBody := Ctxt.InContent;
        call.RestAccessRights := RestAccessRights;
        Server.URI(call);
        // set output content
        result := call.OutStatus;
        Ctxt.OutContent := call.OutBody;
        P := pointer(call.OutHead);
        if IdemPChar(P,'CONTENT-TYPE: ') then begin
          // change mime type if modified in HTTP header (e.g. GET blob fields)
          Ctxt.OutContentType := GetNextLine(P+14,P);
          call.OutHead := P;
        end else
          // default content type is JSON
          Ctxt.OutContentType := JSON_CONTENT_TYPE;
        // handle HTTP redirection over virtual hosts
        if (host<>'') and
           ((result=HTML_MOVEDPERMANENTLY) or (result=HTML_TEMPORARYREDIRECT)) then begin
          redirect := FindIniNameValue(P,'LOCATION: ');
          len := length(host);
          if (length(redirect)>len) and (redirect[len+1]='/') and
             IdemPropNameU(host,pointer(redirect),len) then
            // host/method -> method on same domain
            call.OutHead := 'Location: '+copy(redirect,len+1,maxInt);
        end;
        // handle optional CORS origin
        Ctxt.OutCustomHeaders := Trim(call.OutHead)+
          #13#10'Server-InternalState: '+Int32ToUtf8(call.OutInternalState);
        if ExistsIniName(pointer(call.InHead),'ORIGIN:') then
          Ctxt.OutCustomHeaders := Trim(Ctxt.OutCustomHeaders+fAccessControlAllowOriginHeader) else
          Ctxt.OutCustomHeaders := Trim(Ctxt.OutCustomHeaders);
        break;
      end;
  end;
end;

procedure TSQLHttpServer.HttpThreadTerminate(Sender: TThread);
var i: integer;
begin
  if self<>nil then
    for i := 0 to high(fDBServers) do
      fDBServers[i].Server.EndCurrentThread(Sender);
end;

procedure TSQLHttpServer.HttpThreadStart(Sender: TThread);
var i: integer;
begin
  if self=nil then
    exit;
  SetCurrentThreadName('% worker',[self]);
  for i := 0 to high(fDBServers) do
    fDBServers[i].Server.BeginCurrentThread(Sender);
end;

procedure TSQLHttpServer.SetAccessControlAllowOrigin(const Value: RawUTF8);
begin
  fAccessControlAllowOrigin := Value;
  if Value='' then
    fAccessControlAllowOriginHeader :=
      #13#10'Access-Control-Allow-Origin: ' else
    fAccessControlAllowOriginHeader :=
      #13#10'Access-Control-Allow-Methods: POST, PUT, GET, DELETE, LOCK, OPTIONS'+
      #13#10'Access-Control-Max-Age: 1728000'+
      // see http://blog.import.io/tech-blog/exposing-headers-over-cors-with-access-control-expose-headers
      #13#10'Access-Control-Expose-Headers: content-length,location,server-internalstate'+
      #13#10'Access-Control-Allow-Origin: '+Value;
end;


{ TSQLHTTPRemoteLogServer }

constructor TSQLHTTPRemoteLogServer.Create(const aRoot: RawUTF8;
  aPort: integer; const aEvent: TRemoteLogReceivedOne);
var aModel: TSQLModel;
begin
  aModel := TSQLModel.Create([],aRoot);
  fServer := TSQLRestServerFullMemory.Create(aModel);
  aModel.Owner := fServer;
  fServer.ServiceMethodRegisterPublishedMethods('',self);
  fServer.AcquireExecutionMode[execSOAByMethod] := amLocked; // protect aEvent
  inherited Create(AnsiString(UInt32ToUtf8(aPort)),fServer,'+',HTTP_DEFAULT_MODE,nil,1);
  fEvent := aEvent;
  AccessControlAllowOrigin := '*'; // e.g. when called from AJAX/SMS 
end;

destructor TSQLHTTPRemoteLogServer.Destroy;
begin
  try
    inherited Destroy;
  finally
    fServer.Free;
  end;
end;

procedure TSQLHTTPRemoteLogServer.RemoteLog(Ctxt: TSQLRestServerURIContext);
begin
  if Assigned(fEvent) and (Ctxt.Method=mPUT) then begin
    fEvent(Ctxt.Call^.InBody);
    Ctxt.Success;
  end;
end;

end.
