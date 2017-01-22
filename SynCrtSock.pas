/// classes implementing HTTP/1.1 client and server protocol
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynCrtSock;

{
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2017 Arnaud Bouchez
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
  - Alfred Glaenzer (alf)
  - EMartin
  - Eric Grange
  - EvaF
  - Maciej Izak (hnb)
  - Marius Maximus
  - Pavel (mpv)
  - Willo vd Merwe

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


   TCP/IP and HTTP/1.1 Client and Server
  ***************************************

  Initial version: 2009 May, by Arnaud Bouchez

  Version 1.4 - February 8, 2010
  - whole Synopse SQLite3 database framework released under the GNU Lesser
    General Public License version 3, instead of generic "Public Domain"
  - fix a bug happening when multiple HTTP connections were opened and
    closed in the same program

  Version 1.5 - March 1, 2010
  - new generic unix implementation, using libc sockets, in SynLibcSock.pas

  Version 1.9
  - avoid some GPF during client deconnection when the server shut down
  - rewrite HTTP Server handle request loop keep alive timing
  - HTTP Server now use a Thread Pool to speed up multi-connections: this
    speed up a lot HTTP/1.0 requests, by creating a Thread only if
    necessary

  Version 1.9.2
  - deleted deprecated DOS related code (formerly used with DWPL Dos Extender)
  - a dedicated thread is now used if the incoming HTTP request has
    POSTed a body content of more than 16 KB (to avoid Deny Of Service, and
    preserve the Thread Pool to only real small processes)
  - new CROnly parameter for TCrtSocket.SockRecvLn, to handle #13 as
    line delimiter: by default, #10 or #13#10 are line delimiters
    (as for normal Window/Linux text files)

  Version 1.12
  - added connection check and exception handling in
    THttpServerSocket.GetRequest, which now is a function returning a boolean
  - added DOS / TCP SYN Flood detection if THttpServerSocket.GetRequest
    spent more than 2 seconds to get header from HTTP Client

  Version 1.13
  - code modifications to compile with Delphi 5 compiler
  - new THttpApiServer class, using fast http.sys kernel-mode server
    for better performance and less resource usage
  - DOS / TCP SYN Flood detection time enhanced to 5 seconds
  - fixed HTTP client stream layout (to be more RFC compliant)
  - new generic compression handling mechanism: can handle gzip, deflate
    or custom synlz / synlzo algorithms via THttpSocketCompress functions
  - new THttpServerGeneric.Request virtual abstract method prototype
  - new TWinINet class, using WinINet API (very slow, do not use)
  - new TWinHTTP class, using WinHTTP API (faster than THttpClientSocket):
    this is the class to be used

  Version 1.15
  - unit now tested with Delphi XE2 (32 Bit)
  - fixed issue in HTTP_RESPONSE.SetHeaders()

  Version 1.16
  - fixed issue in case of wrong void parameter e.g. in THttpApiServer.AddUrl
  - circumvent some bugs of Delphi XE2 background compiler (main compiler is OK)
  - added 'RemoteIP: 127.0.0.1' to the retrieved HTTP headers
  - major speed up of THttpApiServer for Windows Vista and up, by processing
    huge content in chunks: upload of 100Mb file take 25 sec before and 6 sec
    after changes, according to feedback by MPV - ticket 711247b998
  - new THttpServerGeneric.OnHttpThreadTerminate event, available to clean-up
    any process in the thread context, when it is terminated (to call e.g.
    TSQLDBConnectionPropertiesThreadSafe.EndCurrentThread in order to call
    CoUnInitialize from thread in which CoInitialize was initialy made) - see
    https://synopse.info/fossil/tktview?name=213544b2f5

  Version 1.17
  - replaced TSockData string type to the generic RawByteString type (and
    the default AnsiString for non-Unicode version of Delphi)
  - added optional aProxyName, aProxyByPass parameters to TWinHttpAPI /
    TWinInet and TWinHTTP constructors
  - added THttpServerGeneric.OnHttpThreadStart property, and associated
    TNotifyThreadEvent event prototype
  - handle 'Range: bytes=***-***' request in THttpApiServer

  Version 1.18
  - replaced RawByteString type (defined locally for non-Unicode version of
    Delphi) by a dedicated SockString type, used for data storage and for
    simple ASCII content (like URIs or port number)
  - added and tested Linux support (FPC/CrossKylix), via sockets or libcurl API
  - introducing THttpServerRequest class for HTTP server context and THttpRequest
    (replacing TWinHttpAPI) as abstract parent for HTTP client classes
  - http.sys kernel-mode server now handles HTTP API 2.0 (available since
    Windows Vista / Server 2008), or fall back to HTTP API 1.0 (for Windows XP
    or Server 2003) - thanks pavel for the feedback and initial patch!
  - deep code refactoring of thread process, especially for TSynThreadPool as
    used by THttpServer: introducing TSynThread and TSynThreadPoolSubThread;
    as a result, it fixes OnHttpThreadStart and OnHttpThreadTerminate to be
    triggered from every thread, and propagated from THttpApiServer's clones
  - added TCrtSocket.TCPNoDelay/SendTimeout/ReceiveTimeout/KeepAlive properties
  - added optional parameter to set buffer size for TCrtSocket.CreateSockIn
    and TCrtSocket.CreateSockOut methods
  - renamed misleading TCrtSocket.Snd method as overloaded SockSend, and
    refactored public fields as read/only properties
  - fixed long-standing random bug occuring when a TCrtSocket connection is
    closed then reopened (e.g. when THttpClientSocket.Request does its retrial);
    thanks hnb for the patch!
  - added THttpServerRequest.UseSSL property to check if connection is secured
  - added optional queue name for THttpApiServer.Create constructor [149cf42383]
  - added THttpApiServer.RemoveUrl() method
  - introduced THttpApiServer.ReceiveBufferSize property
  - added THttpApiServer.HTTPQueueLength property (for HTTP API 2.0 only)
  - added THttpApiServer.MaxBandwidth and THttpApiServer.MaxConnections
    properties (for HTTP API 2.0 only) - thanks mpv for the proposal!
  - added THttpApiServer.LogStart/LogStop for HTTP API 2.0 integrated logging
  - introducing new THttpApiServer.SetAuthenticationSchemes() method for HTTP
    API 2.0, and the corresponding THttpServerRequest.AuthenticationStatus and
    AuthenticatedUser properties
  - added THttpApiServer.SetTimeOutLimits() method for HTTP API 2.0
  - added THttpApiServer.ServerSessionID and UrlGroupID read-only properties
  - let HTTP_RESPONSE.AddCustomHeader() recognize all known headers
  - THttpApiServer won't try to send an error message when connection is broken
  - added error check for HttpSendHttpResponse() API call
  - added EWinHTTP exception, raised when TWinHttp client fails to connect
  - added aTimeOut optional parameter to TCrtSocket.Open() constructor
  - added function HtmlEncode()
  - some code cleaning about 64 bit compilation (including [540628f498])
  - refactored HTTP_DATA_CHUNK record definition into HTTP_DATA_CHUNK_* records
    to circumvent XE3 alignemnt issue
  - WinSock-based THttpServer will avoid creating a thread per connection,
    when the maximum of 64 threads is reached in the pool, with an exception
    of kept-alife or huge body requets (avoiding DoS attacks by limiting the
    total number of created threads)
  - allow WinSock-based THttpServer to set a server address ('1.2.3.4:1234')
  - let WinSock-based THttpServer.Process() handle HTTP_RESP_STATICFILE
  - force disable range checking and other compiler options for this unit
  - included more detailed information to HTTP client User-Agent header
  - added ConnectionTimeOut, SendTimeout and ReceiveTimeout optional parameters
    to THttpRequest constructors - feature request [bfe485b678]
  - added optional aCompressMinSize parameter to RegisterCompress() methods
  - added THttpRequest.Get/Post/Put/Delete() class functions for easy remote
    resource retrieval using either WinHTTP or WinINet APIs
  - added TURI structure, ready to parse a supplied HTTP URI
  - added ConnectionID the HTTP server context - see request [0636eeec54]
  - added THttpRequest.IgnoreSSLCertificateErrors property (proposal by EMartin)
  - added THttpRequest AuthScheme and AuthUserName/AuthPassword properties, for
    authentication - only implemented at TWinHttp level (thanks Eric Grange)
  - fixed TCrtSocket.BytesIn and TCrtSocket.BytesOut properties
  - fixed ticket [82df275784] THttpRequest with responses without Content-Length
  - fixed ticket [f0749956af] TWinINet does not work with HTTPS servers
  - fixed ticket [842a5ae15a] THttpApiServer.Execute/SendError message
  - fixed ticket [f2ae4022a4] EWinINet error handling
  - fixed ticket [73da2c17b1] about Accept-Encoding header in THttpApiServer
  - fixed ticket [cbcbb3b2fc] about PtrInt definition
  - fixed ticket [91f8f3ec6f] about error retrieving unknown headers
  - fixed ticket [f79ff5714b] about potential finalization issues as .bpl in IDE
  - fixed ticket [2d53fc43e3] about unneeded port 80
  - fixed ticket [11b327bd77] about TCrtSocket not working with Delphi 2009+
  - fixed ticket [0f6ecdaf55] for better compatibility with HTTP/1.1 cache
  - fixed ticket [814f6bd65a] about missing OnHttpThreadStart in CreateClone
  - fixed ticket [51a9c086f3] about THttpApiServer.SetHTTPQueueLength()
  - fixed potential Access Violation error at THttpServerResp shutdown
  - removed several compilation hints when assertions are set to off
  - added aRegisterURI optional parameter to THttpApiServer.AddUrl() method
  - made exception error messages more explicit (tuned per module)
  - fixed several issues when releasing THttpApiServer and THttpServer instances
  - allow to use any Unicode content for SendEmail() - also includes
    SendEmailSubject() function, for feature request [0a5fdf9129]
  - added support for TLS1.1 & TLS1.2 for TWinHTTP
  - added advanced exception text if case of HTTPS connection problems

}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

{.$define DEBUGAPI}
{.$define DEBUG23}
{$ifdef DEBUG2}
{.$define DEBUG}
{$endif}


uses
{$ifdef MSWINDOWS}
  Windows,
  SynWinSock,
  {$ifdef USEWININET}
  WinInet,
  {$endif}
{$else MSWINDOWS}
  {$undef USEWININET}
  {$ifdef FPC}
  Sockets,
  SynFPCSock,
  SynFPCLinux,
  {$else}
  {$ifndef DELPHI5OROLDER}
  Types,
  {$endif}
  {$endif}
  {$ifdef KYLIX3}
  LibC,
  KernelIoctl,
  SynFPCSock, // shared with Kylix
  SynKylix,
  {$endif}
{$endif MSWINDOWS}
{$ifndef LVCL}
  Contnrs,
{$endif}
  SysUtils,
  Classes;

const
  /// the full text of the current Synopse mORMot framework version
  // - match the value defined in SynCommons.pas and SynopseCommit.inc
  XPOWEREDPROGRAM = 'mORMot 1.18';

  /// the running Operating System
  XPOWEREDOS = {$ifdef MSWINDOWS} 'Windows' {$else}
                 {$ifdef LINUX} 'Linux' {$else} 'Posix' {$endif LINUX}
               {$endif MSWINDOWS};

  /// used by THttpApiServer.Request for http.sys to send a static file
  // - the OutCustomHeader should contain the proper 'Content-type: ....'
  // corresponding to the file (e.g. by calling GetMimeContentType() function
  // from SynCommons supplyings the file name)
  // - should match HTML_CONTENT_STATICFILE constant defined in mORMot.pas unit
  HTTP_RESP_STATICFILE = '!STATICFILE';
  /// used to notify e.g. the THttpServerRequest not to wait for any response
  // from the client
  // - is not to be used in normal HTTP process, but may be used e.g. by
  // TWebSocketProtocolRest.ProcessFrame() to avoid to wait for an incoming
  // response from the other endpoint
  // - should match NORESPONSE_CONTENT_TYPE constant defined in mORMot.pas unit
  HTTP_RESP_NORESPONSE = '!NORESPONSE';

var
  /// THttpRequest timeout default value for DNS resolution
  // - leaving to 0 will let system default value be used
  HTTP_DEFAULT_RESOLVETIMEOUT: integer = 0;
  /// THttpRequest timeout default value for remote connection
  // - default is 60 seconds
  // - used e.g. by THttpRequest, TSQLHttpClientRequest and TSQLHttpClientGeneric
  HTTP_DEFAULT_CONNECTTIMEOUT: integer = 60000;
  /// THttpRequest timeout default value for data sending
  // - default is 30 seconds
  // - used e.g. by THttpRequest, TSQLHttpClientRequest and TSQLHttpClientGeneric
  // - you can override this value by setting the corresponding parameter in
  // THttpRequest.Create() constructor
  HTTP_DEFAULT_SENDTIMEOUT: integer = 30000;
  /// THttpRequest timeout default value for data receiving
  // - default is 30 seconds
  // - used e.g. by THttpRequest, TSQLHttpClientRequest and TSQLHttpClientGeneric
  // - you can override this value by setting the corresponding parameter in
  // THttpRequest.Create() constructor
  HTTP_DEFAULT_RECEIVETIMEOUT: integer = 30000;

type
{$ifdef UNICODE}
  /// define the fastest Unicode string type of the compiler
  SynUnicode = UnicodeString;
  /// define a raw 8-bit storage string type, used for data buffer management
  SockString = type RawByteString;
{$else}
  /// define the fastest 16-bit Unicode string type of the compiler
  SynUnicode = WideString;
  {$ifdef HASCODEPAGE} // FPC may expect a CP, e.g. to compare two string constants
  SockString = type RawByteString;
  {$else}
  /// define a 8-bit raw storage string type, used for data buffer management
  SockString = type AnsiString;
  {$endif}
{$endif}
  /// points to a 8-bit raw storage variable, used for data buffer management
  PSockString = ^SockString;

  /// defines a dynamic array of SockString
  TSockStringDynArray = array of SockString;

{$ifdef DELPHI5OROLDER}
  // not defined in Delphi 5 or older
  PPointer = ^Pointer;
  TTextLineBreakStyle = (tlbsLF, tlbsCRLF);
  UTF8String = AnsiString;
  UTF8Encode = AnsiString;
{$endif}

{$ifndef FPC}
  /// FPC 64 compatibility integer type
  {$ifdef UNICODE}
  PtrInt = NativeInt;
  PtrUInt = NativeUInt;
  {$else}
  PtrInt = integer;
  PtrUInt = cardinal;
  {$endif}
  /// FPC 64 compatibility pointer type
  PPtrInt = ^PtrInt;
  PPtrUInt = ^PtrUInt;
{$endif}

  {$M+}
  /// exception thrown by the classes of this unit
  ECrtSocket = class(Exception)
  protected
    fLastError: integer;
  public
    /// will concat the message with the WSAGetLastError information
    constructor Create(const Msg: string); overload;
    /// will concat the message with the supplied WSAGetLastError information
    constructor Create(const Msg: string; Error: integer); overload;
  published
    /// the associated WSAGetLastError value
    property LastError: integer read fLastError;
  end;
  {$M-}

  TCrtSocketClass = class of TCrtSocket;

  /// the available available network transport layer
  // - either TCP/IP, UDP/IP or Unix sockets
  TCrtSocketLayer = (cslTCP, cslUDP, cslUNIX);

  /// identify the incoming data availability in TCrtSocket.SockReceivePending
  TCrtSocketPending = (cspSocketError, cspNoData, cspDataAvailable);

  PTextFile = ^TextFile;

  {$M+}
  /// Fast low-level Socket implementation
  // - direct access to the OS (Windows, Linux) network layer API
  // - use Open constructor to create a client to be connected to a server
  // - use Bind constructor to initialize a server
  // - use SockIn and SockOut (after CreateSock*) to read/readln or write/writeln
  //  as with standard Delphi text files (see SendEmail implementation)
  // - even if you do not use read(SockIn^), you may call CreateSockIn then
  // read the (binary) content via SockInRead/SockInPending methods, which would
  // benefit of the SockIn^ input buffer to maximize reading speed
  // - to write data, CreateSockOut and write(SockOut^) is not mandatory: you
  // rather may use SockSend() overloaded methods, followed by a SockFlush call
  // - in fact, you can decide whatever to use none, one or both SockIn/SockOut
  // - since this class rely on its internal optimized buffering system,
  // TCP_NODELAY is set to disable the Nagle algorithm
  // - our classes are (much) faster than the Indy or Synapse implementation
  TCrtSocket = class
  protected
    fSock: TSocket;
    fServer: SockString;
    fPort: SockString;
    fSockIn: PTextFile;
    fSockOut: PTextFile;
    fTimeOut: cardinal;
    fBytesIn: Int64;
    fBytesOut: Int64;
    fSocketLayer: TCrtSocketLayer;
    fSockInEof: boolean;
    // updated by every SockSend() call
    fSndBuf: SockString;
    fSndBufLen: integer;
    // updated during UDP connection, accessed via PeerAddress/PeerPort
    fPeerAddr: TSockAddr;
    /// close and shutdown the connection (called from Destroy)
    procedure Close;
    procedure SetInt32OptionByIndex(OptName, OptVal: integer); virtual;
  public
    /// common initialization of all constructors
    // - do not call directly, but use Open / Bind constructors instead
    constructor Create(aTimeOut: cardinal=10000); reintroduce; virtual;
    /// connect to aServer:aPort
    constructor Open(const aServer, aPort: SockString; aLayer: TCrtSocketLayer=cslTCP;
      aTimeOut: cardinal=10000);
    /// bind to a Port
    // - expects the port to be specified as Ansi string, e.g. '1234'
    // - you can optionally specify a server address to bind to, e.g.
    // '1.2.3.4:1234'
    constructor Bind(const aPort: SockString; aLayer: TCrtSocketLayer=cslTCP);
    /// low-level internal method called by Open() and Bind() constructors
    // - raise an ECrtSocket exception on error
    procedure OpenBind(const aServer, aPort: SockString; doBind: boolean;
      aSock: integer=-1; aLayer: TCrtSocketLayer=cslTCP);
    /// initialize SockIn for receiving with read[ln](SockIn^,...)
    // - data is buffered, filled as the data is available
    // - read(char) or readln() is indeed very fast
    // - multithread applications would also use this SockIn pseudo-text file
    // - by default, expect CR+LF as line feed (i.e. the HTTP way)
    procedure CreateSockIn(LineBreak: TTextLineBreakStyle=tlbsCRLF;
      InputBufferSize: Integer=1024);
    /// initialize SockOut for sending with write[ln](SockOut^,....)
    // - data is sent (flushed) after each writeln() - it's a compiler feature
    // - use rather SockSend() + SockSendFlush to send headers at once e.g.
    // since writeln(SockOut^,..) flush buffer each time
    procedure CreateSockOut(OutputBufferSize: Integer=1024);
    /// close the opened socket, and corresponding SockIn/SockOut
    destructor Destroy; override;
    /// read Length bytes from SockIn buffer + Sock if necessary
    // - if SockIn is available, it first gets data from SockIn^.Buffer,
    // then directly receive data from socket if UseOnlySockIn=false
    // - if UseOnlySockIn=true, it will return the data available in SockIn^,
    // and returns the number of bytes
    // - can be used also without SockIn: it will call directly SockRecv()
    // in such case (assuming UseOnlySockin=false)
    function SockInRead(Content: PAnsiChar; Length: integer;
      UseOnlySockIn: boolean=false): integer;
    /// returns the number of bytes in SockIn buffer or pending in Sock
    // - if SockIn is available, it first check from any data in SockIn^.Buffer,
    // then call InputSock to try to receive any pending data
    // - will wait up to the specified aTimeOut value (in milliseconds) for
    // incoming data
    // - returns -1 in case of a socket error (e.g. broken connection); you
    // can raise a ECrtSocket exception to propagate the error
    function SockInPending(aTimeOut: integer): integer;
    /// check the connection status of the socket
    function SockConnected: boolean;
    /// simulate writeln() with direct use of Send(Sock, ..)
    // - useful on multi-treaded environnement (as in THttpServer.Process)
    // - no temp buffer is used
    // - handle SockString, ShortString, Char, Integer parameters
    // - raise ECrtSocket exception on socket error
    procedure SockSend(const Values: array of const); overload;
    /// simulate writeln() with a single line
    procedure SockSend(const Line: SockString=''); overload;
    /// append P^ data into SndBuf (used by SockSend(), e.g.)
    // - call SockSendFlush to send it through the network via SndLow()
    procedure SockSend(P: pointer; Len: integer); overload;
    /// flush all pending data to be sent
    procedure SockSendFlush;
    /// fill the Buffer with Length bytes
    // - use TimeOut milliseconds wait for incoming data
    // - bypass the SockIn^ buffers
    // - raise ECrtSocket exception on socket error
    procedure SockRecv(Buffer: pointer; Length: integer);
    /// check if there are some pending bytes in the input sockets API buffer
    function SockReceivePending(TimeOut: cardinal): TCrtSocketPending;
    /// returns the socket input stream as a string
    function SockReceiveString: SockString;
    /// fill the Buffer with Length bytes
    // - use TimeOut milliseconds wait for incoming data
    // - bypass the SockIn^ buffers
    // - return false on any error, true on success
    function TrySockRecv(Buffer: pointer; Length: integer): boolean;
    /// call readln(SockIn^,Line) or simulate it with direct use of Recv(Sock, ..)
    // - char are read one by one
    // - use TimeOut milliseconds wait for incoming data
    // - raise ECrtSocket exception on socket error
    // - by default, will handle #10 or #13#10 as line delimiter (as normal text
    // files), but you can delimit lines using #13 if CROnly is TRUE
    procedure SockRecvLn(out Line: SockString; CROnly: boolean=false); overload;
    /// call readln(SockIn^) or simulate it with direct use of Recv(Sock, ..)
    // - char are read one by one
    // - use TimeOut milliseconds wait for incoming data
    // - raise ECrtSocket exception on socket error
    // - line content is ignored
    procedure SockRecvLn; overload;
    /// direct send data through network
    // - raise a ECrtSocket exception on any error
    // - bypass the SockSend() or SockOut^ buffers
    procedure SndLow(P: pointer; Len: integer);
    /// direct send data through network
    // - return false on any error, true on success
    // - bypass the SndBuf or SockOut^ buffers
    function TrySndLow(P: pointer; Len: integer): boolean;
    /// returns the low-level error number
    // - i.e. returns WSAGetLastError
    function LastLowSocketError: Integer;
    /// direct send data through network
    // - raise a ECrtSocket exception on any error
    // - bypass the SndBuf or SockOut^ buffers
    // - raw Data is sent directly to OS: no CR/CRLF is appened to the block
    procedure Write(const Data: SockString);
    /// remote IP address of the last packet received, set only for SocketLayer=slUDP
    function PeerAddress: SockString;
    /// remote IP port of the last packet received, set only for SocketLayer=slUDP
    function PeerPort: integer;
    /// set the TCP_NODELAY option for the connection
    // - default 1 (true) will disable the Nagle buffering algorithm; it should
    // only be set for applications that send frequent small bursts of information
    // without getting an immediate response, where timely delivery of data
    // is required - so it expects buffering before calling Write() or SndLow()
    // - you can set 0 (false) here to enable the Nagle algorithm, if needed
    // - see http://www.unixguide.net/network/socketfaq/2.16.shtml
    property TCPNoDelay: Integer index TCP_NODELAY write SetInt32OptionByIndex;
    /// set the SO_SNDTIMEO option for the connection
    // - i.e. the timeout, in milliseconds, for blocking send calls
    // - see http://msdn.microsoft.com/en-us/library/windows/desktop/ms740476
    property SendTimeout: Integer index SO_SNDTIMEO write SetInt32OptionByIndex;
    /// set the SO_RCVTIMEO option for the connection
    // - i.e. the timeout, in milliseconds, for blocking receive calls
    // - see http://msdn.microsoft.com/en-us/library/windows/desktop/ms740476
    property ReceiveTimeout: Integer index SO_RCVTIMEO write SetInt32OptionByIndex;
    /// set the SO_KEEPALIVE option for the connection
    // - 1 (true) will enable keep-alive packets for the connection
    // - see http://msdn.microsoft.com/en-us/library/windows/desktop/ee470551
    property KeepAlive: Integer index SO_KEEPALIVE write SetInt32OptionByIndex;
    /// set the SO_LINGER option for the connection, to control its shutdown
    // - by default (or Linger<0), Close will return immediately to the caller,
    // and any pending data will be delivered if possible
    // - Linger > 0  represents the time in seconds for the timeout period
    // to be applied at Close; under Linux, will also set SO_REUSEADDR; under
    // Darwin, set SO_NOSIGPIPE
    // - Linger = 0 causes the connection to be aborted and any pending data
    // is immediately discarded at Close
    property Linger: Integer index SO_LINGER write SetInt32OptionByIndex;
    /// after CreateSockIn, use Readln(SockIn^,s) to read a line from the opened socket
    property SockIn: PTextFile read fSockIn;
    /// after CreateSockOut, use Writeln(SockOut^,s) to send a line to the opened socket
    property SockOut: PTextFile read fSockOut;
  published
    /// low-level socket handle, initialized after Open() with socket
    property Sock: TSocket read fSock;
    /// low-level socket type, initialized after Open() with socket
    property SocketLayer: TCrtSocketLayer read fSocketLayer;
    /// IP address, initialized after Open() with Server name
    property Server: SockString read fServer;
    /// IP port, initialized after Open() with port number
    property Port: SockString read fPort;
    /// if higher than 0, read loop will wait for incoming data till
    // TimeOut milliseconds (default value is 10000) - used also in SockSend()
    property TimeOut: cardinal read fTimeOut;
    /// total bytes received
    property BytesIn: Int64 read fBytesIn;
    /// total bytes sent
    property BytesOut: Int64 read fBytesOut;
  end;
  {$M-}

  /// event used to compress or uncompress some data during HTTP protocol
  // - should always return the protocol name for ACCEPT-ENCODING: header
  // e.g. 'gzip' or 'deflate' for standard HTTP format, but you can add
  // your own (like 'synlzo' or 'synlz')
  // - the data is compressed (if Compress=TRUE) or uncompressed (if
  // Compress=FALSE) in the Data variable (i.e. it is modified in-place)
  // - to be used with THttpSocket.RegisterCompress method
  // - DataRawByteStringtype should be a generic AnsiString/RawByteString, which
  // should be in practice a SockString or a RawByteString
  THttpSocketCompress = function(var DataRawByteString; Compress: boolean): AnsiString;

  /// used to maintain a list of known compression algorithms
  THttpSocketCompressRec = record
    /// the compression name, as in ACCEPT-ENCODING: header (gzip,deflate,synlz)
    Name: SockString;
    /// the function handling compression and decompression
    Func: THttpSocketCompress;
    /// the size in bytes after which compress will take place
    // - will be 1024 e.g. for 'zip' or 'deflate'
    // - could be 0 e.g. when encrypting the content, meaning "always compress"
    CompressMinSize: integer;
  end;

  /// list of known compression algorithms
  THttpSocketCompressRecDynArray = array of THttpSocketCompressRec;

  /// identify some items in a list of known compression algorithms
  THttpSocketCompressSet = set of 0..31;

  /// parent of THttpClientSocket and THttpServerSocket classes
  // - contain properties for implementing HTTP/1.1 using the Socket API
  // - handle chunking of body content
  // - can optionaly compress and uncompress on the fly the data, with
  // standard gzip/deflate or custom (synlzo/synlz) protocols
  THttpSocket = class(TCrtSocket)
  protected
    /// true if the TRANSFER-ENCODING: CHUNKED was set in headers
    Chunked: boolean;
    /// to call GetBody only once
    fBodyRetrieved: boolean;
    /// used by RegisterCompress method
    fCompress: THttpSocketCompressRecDynArray;
    /// set by RegisterCompress method
    fCompressAcceptEncoding: SockString;
    /// GetHeader set index of protocol in fCompress[], from ACCEPT-ENCODING:
    fCompressHeader: THttpSocketCompressSet;
    /// same as HeaderValue('Content-Encoding'), but retrieved during Request
    // and mapped into the fCompress[] array
    fContentCompress: integer;
    /// retrieve the HTTP headers into Headers[] and fill most properties below
    procedure GetHeader;
    /// retrieve the HTTP body (after uncompression if necessary) into Content
    procedure GetBody;
    /// compress the data, adding corresponding headers via SockSend()
    // - always add a 'Content-Length: ' header entry (even if length=0)
    // - e.g. 'Content-Encoding: synlz' header if compressed using synlz
    // - and if Data is not '', will add 'Content-Type: ' header
    procedure CompressDataAndWriteHeaders(const OutContentType: SockString;
      var OutContent: SockString);
  public
    /// TCP/IP prefix to mask HTTP protocol
    // - if not set, will create full HTTP/1.0 or HTTP/1.1 compliant content
    // - in order to make the TCP/IP stream not HTTP compliant, you can specify
    // a prefix which will be put before the first header line: in this case,
    // the TCP/IP stream won't be recognized as HTTP, and will be ignored by
    // most AntiVirus programs, and increase security - but you won't be able
    // to use an Internet Browser nor AJAX application for remote access any more
    TCPPrefix: SockString;
    /// will contain the first header line:
    // - 'GET /path HTTP/1.1' for a GET request with THttpServer, e.g.
    // - 'HTTP/1.0 200 OK' for a GET response after Get() e.g.
    Command: SockString;
    /// will contain the header lines after a Request - use HeaderValue() to get one
    Headers: array of SockString;
    /// will contain the data retrieved from the server, after the Request
    Content: SockString;
    /// same as HeaderValue('Content-Length'), but retrieved during Request
    // - is overridden with real Content length during HTTP body retrieval
    ContentLength: integer;
    /// same as HeaderValue('Content-Type'), but retrieved during Request
    ContentType: SockString;
    /// same as HeaderValue('Connection')='Close', but retrieved during Request
    ConnectionClose: boolean;
    /// same as HeaderValue('Connection')='Upgrade', but retrieved during Request
    ConnectionUpgrade: boolean;
    /// add an header entry, returning the just entered entry index in Headers[]
    function HeaderAdd(const aValue: SockString): integer;
    /// set all Header values at once, from CRLF delimited text
    procedure HeaderSetText(const aText: SockString;
      const aForcedContentType: SockString='');
    /// get all Header values at once, as CRLF delimited text
    function HeaderGetText: SockString; virtual;
    /// HeaderValue('Content-Type')='text/html', e.g.
    function HeaderValue(aName: SockString): SockString;
    /// will register a compression algorithm
    // - used e.g. to compress on the fly the data, with standard gzip/deflate
    // or custom (synlzo/synlz) protocols
    // - returns true on success, false if this function or this
    // ACCEPT-ENCODING: header was already registered
    // - you can specify a minimal size (in bytes) before which the content won't
    // be compressed (1024 by default, corresponding to a MTU of 1500 bytes)
    // - the first registered algorithm will be the prefered one for compression
    function RegisterCompress(aFunction: THttpSocketCompress;
      aCompressMinSize: integer=1024): boolean;
  end;

  THttpServer = class;

  /// Socket API based HTTP/1.1 server class used by THttpServer Threads
  THttpServerSocket = class(THttpSocket)
  private
  public
    /// contains the method ('GET','POST'.. e.g.) after GetRequest()
    Method: SockString;
    /// contains the URL ('/' e.g.) after GetRequest()
    URL: SockString;
    /// true if the client is HTTP/1.1 and 'Connection: Close' is not set
    // - default HTTP/1.1 behavior is "keep alive", unless 'Connection: Close'
    // is specified, cf. RFC 2068 page 108: "HTTP/1.1 applications that do not
    // support persistent connections MUST include the "close" connection option
    // in every message"
    KeepAliveClient: boolean;
    /// the recognized client IP, after a call to InitRequest()
    RemoteIP: SockString;
    /// create the socket according to a server
    // - will register the THttpSocketCompress functions from the server
    constructor Create(aServer: THttpServer); reintroduce;
    /// main object function called after aClientSock := Accept + Create:
    // - get initialize the socket with the supplied accepted socket
    // - caller will then use the GetRequest method below to
    // get the request
    procedure InitRequest(aClientSock: TSocket);
    /// main object function called after aClientSock := Accept + Create:
    // - get Command, Method, URL, Headers and Body (if withBody is TRUE)
    // - get sent data in Content (if ContentLength<>0)
    // - return false if the socket was not connected any more, or if
    // any exception occured during the process
    function GetRequest(withBody: boolean=true): boolean;
    /// get all Header values at once, as CRLF delimited text
    // - this overridden version will add the 'RemoteIP: 1.2.3.4' header
    function HeaderGetText: SockString; override;
  end;

  /// Socket API based REST and HTTP/1.1 compatible client class
  // - this component is HTTP/1.1 compatible, according to RFC 2068 document
  // - the REST commands (GET/POST/PUT/DELETE) are directly available
  // - open connection with the server with inherited Open(server,port) function
  // - if KeepAlive>0, the connection is not broken: a further request (within
  // KeepAlive milliseconds) will use the existing connection if available,
  // or recreate a new one if the former is outdated or reset by server
  // (will retry only once); this is faster, uses less resources (especialy
  // under Windows), and is the recommended way to implement a HTTP/1.1 server
  // - on any error (timeout, connection closed) will retry once to get the value
  // - don't forget to use Free procedure when you are finished
  THttpClientSocket = class(THttpSocket)
  protected
    fUserAgent: SockString;
    fProcessName: SockString;
    procedure RequestSendHeader(const url, method: SockString); virtual;
  public
    /// common initialization of all constructors
    // - this overridden method will set the UserAgent with some default value
    // - you can customize the default client timeouts by setting appropriate
    // aTimeout parameters (in ms) if you left the 0 default parameters,
    // it would use global HTTP_DEFAULT_RECEIVETIMEOUT variable values
    constructor Create(aTimeOut: cardinal=0); override;
    /// low-level HTTP/1.1 request
    // - called by all Get/Head/Post/Put/Delete REST methods
    // - after an Open(server,port), return 200,202,204 if OK, http status error otherwise
    // - retry is false by caller, and will be recursively called with true to retry once
    function Request(const url, method: SockString; KeepAlive: cardinal;
      const header, Data, DataType: SockString; retry: boolean): integer; virtual;

    /// after an Open(server,port), return 200 if OK, http status error otherwise
    // - get the page data in Content
    function Get(const url: SockString; KeepAlive: cardinal=0; const header: SockString=''): integer;
    /// after an Open(server,port), return 200 if OK, http status error otherwise
    // - get the page data in Content
    // - if AuthToken<>'', will add an header with 'Authorization: Bearer '+AuthToken
    function GetAuth(const url, AuthToken: SockString; KeepAlive: cardinal=0): integer;
    /// after an Open(server,port), return 200 if OK, http status error otherwise - only
    // header is read from server: Content is always '', but Headers are set
    function Head(const url: SockString; KeepAlive: cardinal=0; const header: SockString=''): integer;
    /// after an Open(server,port), return 200,201,204 if OK, http status error otherwise
    function Post(const url, Data, DataType: SockString; KeepAlive: cardinal=0;
      const header: SockString=''): integer;
    /// after an Open(server,port), return 200,201,204 if OK, http status error otherwise
    function Put(const url, Data, DataType: SockString; KeepAlive: cardinal=0;
      const header: SockString=''): integer;
    /// after an Open(server,port), return 200,202,204 if OK, http status error otherwise
    function Delete(const url: SockString; KeepAlive: cardinal=0; const header: SockString=''): integer;

    /// by default, the client is identified as IE 5.5, which is very
    // friendly welcome by most servers :(
    // - you can specify a custom value here
    property UserAgent: SockString read fUserAgent write fUserAgent;
    /// the associated process name
    property ProcessName: SockString read fProcessName write fProcessName;
  end;

  /// class-reference type (metaclass) of a HTTP client socket access
  // - may be either THttpClientSocket or THttpClientWebSockets (from
  // SynBidirSock unit)
  THttpClientSocketClass = class of THttpClientSocket;

  {$ifndef LVCL}
  /// event prototype used e.g. by THttpServerGeneric.OnHttpThreadStart
  TNotifyThreadEvent = procedure(Sender: TThread) of object;
  {$endif}

  {$M+}
  /// a simple TThread with a "Terminate" event run in the thread context
  // - the TThread.OnTerminate event is run within Synchronize() so did not
  // match our expectations to be able to release the resources in the thread
  // context which created them (e.g. for COM objects, or some DB drivers)
  // - used internally by THttpServerGeneric.NotifyThreadStart() - you should
  // not have to use the protected fOnTerminate event handler
  // - also define a Start method for compatibility with older versions of Delphi

  { TSynThread }

  TSynThread = class(TThread)
  protected
    // ensure fOnTerminate is called only if NotifyThreadStart has been done
    fStartNotified: TObject;
    {$ifndef LVCL} // already available in LVCL
    // we re-defined an fOnTerminate event which would be run in the terminated
    // thread context (whereas TThread.OnTerminate is called in the main thread)
    // -> see THttpServerGeneric.OnHttpThreadTerminate event property
    fOnTerminate: TNotifyThreadEvent;
    procedure DoTerminate; override;
    {$endif}
  public
    /// initialize the server instance, in non suspended state
    constructor Create(CreateSuspended: Boolean); reintroduce; virtual;
    {$ifndef HASTTHREADSTART}
    /// method to be called when the thread was created as suspended
    // - Resume is deprecated in the newest RTL, since some OS - e.g. Linux -
    // do not implement this pause/resume feature
    // - we define here this method for older versions of Delphi
    procedure Start;
    {$endif}
    {$ifdef FPC}
    /// under FPC, would call Terminate and WaitFor just with Delphi RTL
    destructor Destroy; override;
    {$endif}
    /// safe version of Sleep() which won't break the thread process
    // - returns TRUE if the thread was Terminated
    // - returns FALSE if successfully waited up to MS milliseconds
    function SleepOrTerminated(MS: cardinal): boolean;
  end;
  {$M-}

  {$ifdef USETHREADPOOL}
  TSynThreadPoolTHttpServer = class;
  {$endif}

  /// HTTP response Thread as used by THttpServer Socket API based class
  // - Execute procedure get the request and calculate the answer
  // - you don't have to overload the protected THttpServerResp Execute method:
  // override THttpServer.Request() function or, if you need a lower-level access
  // (change the protocol, e.g.) THttpServer.Process() method itself
  THttpServerResp = class(TSynThread)
  protected
    fServer: THttpServer;
    fServerSock: THttpServerSocket;
    {$ifdef USETHREADPOOL}
    fThreadPool: TSynThreadPoolTHttpServer;
    {$endif}
    fClientSock: TSocket;
    fConnectionID: integer;
    /// main thread loop: read request from socket, send back answer
    procedure Execute; override;
  public
    /// initialize the response thread for the corresponding incoming socket
    // - this version will get the request directly from an incoming socket
    constructor Create(aSock: TSocket; aServer: THttpServer); reintroduce; overload; 
    /// initialize the response thread for the corresponding incoming socket
    // - this version will handle KeepAlive, for such an incoming request
    constructor Create(aServerSock: THttpServerSocket; aServer: THttpServer
      {$ifdef USETHREADPOOL}; aThreadPool: TSynThreadPoolTHttpServer{$endif});
      reintroduce; overload; virtual;
    /// the associated socket to communicate with the client
    property ServerSock: THttpServerSocket read fServerSock;
    /// the associated main HTTP server instance
    property Server: THttpServer read fServer;
    /// the unique identifier of this connection
    property ConnectionID: integer read fConnectionID;
  end;

  THttpServerRespClass = class of THttpServerResp;

  /// an event handler for implementing a socked-based Thread Pool
  // - matches TSynThreadPoolTHttpServer.Push method signature
  TOnThreadPoolSocketPush = function(aClientSock: TSocket): boolean of object; 

{$ifdef USETHREADPOOL} { currently only available under Windows } 

  TSynThreadPool = class;

  /// defines the sub-threads used by TSynThreadPool
  TSynThreadPoolSubThread = class(TSynThread)
  protected
    fOwner: TSynThreadPool;
  public
    /// initialize the thread
    constructor Create(Owner: TSynThreadPool); reintroduce;
    /// will loop for any pending IOCP commands, and execute fOwner.Task()
    procedure Execute; override;
  end;

  /// a simple Thread Pool, used for fast handling HTTP requests
  // - will handle multi-connection with less overhead than creating a thread
  // for each incoming request
  // - this Thread Pool is implemented over I/O Completion Ports, which is a faster
  // method than keeping a TThread list, and resume them on request: I/O completion
  // just has the thread running while there is pending tasks, with no pause/resume
  TSynThreadPool = class
  protected
    FRequestQueue: THandle;
    FThread: TObjectList; // of TSynThreadPoolSubThread
    FThreadID: array[0..63] of THandle; // WaitForMultipleObjects() limit=64
    FGeneratedThreadCount: integer;
    FOnHttpThreadTerminate: TNotifyThreadEvent;
    /// process to be executed after notification
    procedure Task(aCaller: TSynThreadPoolSubThread; aContext: Pointer); virtual; abstract;
  public
    /// initialize a thread pool with the supplied number of threads
    // - abstract Task() virtual method will be called by one of the threads
    // - up to 64 threads can be associated to a Thread Pool
    constructor Create(NumberOfThreads: Integer=32);
    /// shut down the Thread pool, releasing all associated threads
    destructor Destroy; override;
  end;

  /// a simple Thread Pool, used for fast handling HTTP requests of a THttpServer
  // - will create a THttpServerResp response thread, if the incoming request
  // is identified as HTTP/1.1 keep alive
  TSynThreadPoolTHttpServer = class(TSynThreadPool)
  protected
    fServer: THttpServer;
    procedure Task(aCaller: TSynThreadPoolSubThread; aContext: Pointer); override;
  public
    /// initialize a thread pool with the supplied number of threads
    // - Task() overridden method processs the HTTP request set by Push()
    // - up to 64 threads can be associated to a Thread Pool
    constructor Create(Server: THttpServer; NumberOfThreads: Integer=32); reintroduce;
    /// add an incoming HTTP request to the Thread Pool
    // - matches TOnThreadPoolSocketPush event handler signature
    function Push(aClientSock: TSocket): Boolean;
  end;

{$endif USETHREADPOOL}

{$M+} // to have existing RTTI for published properties
  THttpServerGeneric = class;
{$M-}

  /// the server-side available authentication schemes
  // - as used by THttpServerRequest.AuthenticationStatus
  // - hraNone..hraKerberos will match low-level HTTP_REQUEST_AUTH_TYPE enum as
  // defined in HTTP 2.0 API and
  THttpServerRequestAuthentication = (
    hraNone, hraFailed, hraBasic, hraDigest, hraNtlm, hraNegotiate, hraKerberos);

  /// a generic input/output structure used for HTTP server requests
  // - URL/Method/InHeaders/InContent properties are input parameters
  // - OutContent/OutContentType/OutCustomHeader are output parameters
  THttpServerRequest = class
  protected
    fURL, fMethod, fInHeaders, fInContent, fInContentType: SockString;
    fOutContent, fOutContentType, fOutCustomHeaders: SockString;
    fServer: THttpServerGeneric;
    fConnectionID: Int64;
    fConnectionThread: TSynThread;
    fUseSSL: boolean;
    fAuthenticationStatus: THttpServerRequestAuthentication;
    fAuthenticatedUser: SockString;
  public
    /// initialize the context, associated to a HTTP server instance
    constructor Create(aServer: THttpServerGeneric; aConnectionID: Int64;
      aConnectionThread: TSynThread); virtual;
    /// prepare an incoming request
    // - will set input parameters URL/Method/InHeaders/InContent/InContentType
    // - will reset output parameters
    procedure Prepare(const aURL,aMethod,aInHeaders,aInContent,aInContentType,
      aRemoteIP: SockString);
    /// append some lines to the InHeaders input parameter
    procedure AddInHeader(additionalHeader: SockString);
    /// input parameter containing the caller URI
    property URL: SockString read fURL;
    /// input parameter containing the caller method (GET/POST...)
    property Method: SockString read fMethod;
    /// input parameter containing the caller message headers
    property InHeaders: SockString read fInHeaders;
    /// input parameter containing the caller message body
    // - e.g. some GET/POST/PUT JSON data can be specified here
    property InContent: SockString read fInContent;
    // input parameter defining the caller message body content type
    property InContentType: SockString read fInContentType;
    /// output parameter to be set to the response message body
    property OutContent: SockString read fOutContent write fOutContent ;
    /// output parameter to define the reponse message body content type
    // - if OutContentType is HTTP_RESP_STATICFILE (i.e. '!STATICFILE', defined
    // as STATICFILE_CONTENT_TYPE in mORMot.pas), then OutContent is the UTF-8
    // file name of a file which must be sent to the client via http.sys (much
    // faster than manual buffering/sending)
    // - if OutContentType is HTTP_RESP_NORESPONSE (i.e. '!NORESPONSE', defined
    // as NORESPONSE_CONTENT_TYPE in mORMot.pas), then the actual transmission
    // protocol may not wait for any answer - used e.g. for WebSockets
    property OutContentType: SockString read fOutContentType write fOutContentType;
    /// output parameter to be sent back as the response message header
    // - e.g. to set Content-Type/Location
    property OutCustomHeaders: SockString read fOutCustomHeaders write fOutCustomHeaders;
    /// the associated server instance
    // - may be a THttpServer or a THttpApiServer class
    property Server: THttpServerGeneric read fServer;
    /// the ID of the connection which called this execution context
    // - e.g. SynCrtSock's TWebSocketProcess.NotifyCallback method would use
    // this property to specify the client connection to be notified
    // - is set as an Int64 to match http.sys ID type, but will be an
    // increasing integer sequence for (web)socket-based servers
    property ConnectionID: Int64 read fConnectionID;
    /// the thread which owns the connection of this execution context
    // - depending on the HTTP server used, may not follow ConnectionID
    property ConnectionThread: TSynThread read fConnectionThread;
    /// is TRUE if the caller is connected via HTTPS
    // - only set for THttpApiServer class yet
    property UseSSL: boolean read fUseSSL;
    /// contains the THttpServer-side authentication status
    // - e.g. when using http.sys authentication with HTTP API 2.0
    property AuthenticationStatus: THttpServerRequestAuthentication
      read fAuthenticationStatus;
    /// contains the THttpServer-side authenticated user name, UTF-8 encoded
    // - e.g. when using http.sys authentication with HTTP API 2.0, the
    // domain user name is retrieved from the supplied AccessToken
    // - could also be set by the THttpServerGeneric.Request() method, after
    // proper authentication, so that it would be logged as expected
    property AuthenticatedUser: SockString read fAuthenticatedUser;
  end;

  /// event handler used by THttpServerGeneric.OnRequest property
  // - Ctxt defines both input and output parameters
  // - result of the function is the HTTP error code (200 if OK, e.g.)
  // - OutCustomHeader will handle Content-Type/Location
  // - if OutContentType is HTTP_RESP_STATICFILE (i.e. '!STATICFILE' aka
  // STATICFILE_CONTENT_TYPE in mORMot.pas), then OutContent is the UTF-8 file
  // name of a file which must be sent to the client via http.sys (much faster
  // than manual buffering/sending) and  the OutCustomHeader should
  // contain the proper 'Content-type: ....'
  TOnHttpServerRequest = function(Ctxt: THttpServerRequest): cardinal of object;

{$M+} { to have existing RTTI for published properties }
  /// abstract class to implement a HTTP server
  // - do not use this class, but rather the THttpServer or THttpApiServer
  THttpServerGeneric = class(TSynThread)
  protected
    /// optional event handler for the virtual Request method
    fOnRequest: TOnHttpServerRequest;
    /// list of all registered compression algorithms
    fCompress: THttpSocketCompressRecDynArray;
    /// set by RegisterCompress method
    fCompressAcceptEncoding: SockString;
    fOnHttpThreadStart: TNotifyThreadEvent;
    fServerName: SockString;
    fProcessName: SockString;
    fCurrentConnectionID: integer;
    fCanNotifyCallback: boolean;
    procedure SetOnTerminate(const Event: TNotifyThreadEvent); virtual;
    function GetAPIVersion: string; virtual; abstract;
    procedure NotifyThreadStart(Sender: TSynThread);
    procedure SetServerName(const aName: SockString); virtual;
    function NextConnectionID: integer;
  public
    /// initialize the server instance, in non suspended state
    constructor Create(CreateSuspended: Boolean; OnStart,OnStop: TNotifyThreadEvent;
      const ProcessName: SockString); reintroduce;
    /// override this function to customize your http server
    // - InURL/InMethod/InContent properties are input parameters
    // - OutContent/OutContentType/OutCustomHeader are output parameters
    // - result of the function is the HTTP error code (200 if OK, e.g.),
    // - OutCustomHeader is available to handle Content-Type/Location
    // - if OutContentType is HTTP_RESP_STATICFILE (i.e. '!STATICFILE' or
    // STATICFILE_CONTENT_TYPE defined in mORMot.pas), then OutContent is the
    // UTF-8 file name of a file which must be sent to the client via http.sys
    // (much faster than manual buffering/sending) and  the OutCustomHeader should
    // contain the proper 'Content-type: ....'
    // - default implementation is to call the OnRequest event (if existing),
    // and will return STATUS_NOTFOUND if OnRequest was not set
    // - warning: this process must be thread-safe (can be called by several
    // threads simultaneously, but with a given Ctxt instance for each)
    function Request(Ctxt: THttpServerRequest): cardinal; virtual;
    /// server can send a request back to the client, when the connection has
    // been upgraded e.g. to WebSockets
    // - InURL/InMethod/InContent properties are input parameters (InContentType
    // is ignored)
    // - OutContent/OutContentType/OutCustomHeader are output parameters
    // - CallingThread should be set to the client's Ctxt.CallingThread
    // value, so that the method could know which connnection is to be used -
    // it will return STATUS_NOTFOUND (404) if the connection is unknown
    // - result of the function is the HTTP error code (200 if OK, e.g.)
    // - warning: this void implementation will raise an ECrtSocket exception -
    // inherited classes should override it, e.g. as in TWebSocketServerRest
    function Callback(Ctxt: THttpServerRequest; aNonBlocking: boolean): cardinal; virtual;
    /// will register a compression algorithm
    // - used e.g. to compress on the fly the data, with standard gzip/deflate
    // or custom (synlzo/synlz) protocols
    // - you can specify a minimal size (in bytes) before which the content won't
    // be compressed (1024 by default, corresponding to a MTU of 1500 bytes)
    // - the first registered algorithm will be the prefered one for compression
    procedure RegisterCompress(aFunction: THttpSocketCompress;
      aCompressMinSize: integer=1024); virtual;
    /// event handler called by the default implementation of the
    // virtual Request method
    // - warning: this process must be thread-safe (can be called by several
    // threads simultaneously)
    property OnRequest: TOnHttpServerRequest read fOnRequest write fOnRequest;
    /// event handler called after each working Thread is just initiated
    // - called in the thread context at first place in THttpServerGeneric.Execute
    property OnHttpThreadStart: TNotifyThreadEvent
      read fOnHttpThreadStart write fOnHttpThreadStart;
    /// event handler called when a working Thread is terminating
    // - called in the corresponding thread context
    // - the TThread.OnTerminate event will be called within a Synchronize()
    // wrapper, so it won't fit our purpose
    // - to be used e.g. to call CoUnInitialize from thread in which CoInitialize
    // was made, for instance via a method defined as such:
    // ! procedure TMyServer.OnHttpThreadTerminate(Sender: TObject);
    // ! begin // TSQLDBConnectionPropertiesThreadSafe
    // !   fMyConnectionProps.EndCurrentThread;
    // ! end;
    // - is used e.g. by TSQLRest.EndCurrentThread for proper multi-threading
    property OnHttpThreadTerminate: TNotifyThreadEvent read fOnTerminate write SetOnTerminate;
    /// TRUE if the inherited class is able to handle callbacks
    // - only TWebSocketServer has this ability by now
    property CanNotifyCallback: boolean read fCanNotifyCallback;
  published
    /// returns the API version used by the inherited implementation
    property APIVersion: string read GetAPIVersion;
    /// the Server name, UTF-8 encoded, e.g. 'mORMot/1.18 (Linux)'
    // - will be served as "Server: ..." HTTP header
    // - for THttpApiServer, when called from the main instance, will propagate
    // the change to all cloned instances, and included in any HTTP API 2.0 log
    property ServerName: SockString read fServerName write SetServerName;
    /// the associated process name
    property ProcessName: SockString read fProcessName write fProcessName;
  end;

  {$ifdef MSWINDOWS}

  ULONGLONG = Int64;
  HTTP_OPAQUE_ID = ULONGLONG;
  HTTP_URL_GROUP_ID = HTTP_OPAQUE_ID;
  HTTP_SERVER_SESSION_ID = HTTP_OPAQUE_ID;

  /// http.sys API 2.0 logging file supported layouts
  // - match low-level HTTP_LOGGING_TYPE as defined in HTTP 2.0 API
  THttpApiLoggingType = (
    hltW3C, hltIIS, hltNCSA, hltRaw);

  /// http.sys API 2.0 logging file rollover types
  // - match low-level HTTP_LOGGING_ROLLOVER_TYPE as defined in HTTP 2.0 API
  THttpApiLoggingRollOver = (
    hlrSize, hlrDaily, hlrWeekly, hlrMonthly, hlrHourly);

  /// http.sys API 2.0 logging option flags
  // - used to alter the default logging behavior
  // - hlfLocalTimeRollover would force the log file rollovers by local time,
  // instead of the default GMT time
  // - hlfUseUTF8Conversion will use UTF-8 instead of default local code page
  // - only one of hlfLogErrorsOnly and hlfLogSuccessOnly flag could be set
  // at a time: if neither of them are present, both errors and success will
  // be logged, otherwise mutually exclusive flags could be set to force only
  // errors or success logging
  // - match low-level HTTP_LOGGING_FLAG_* constants as defined in HTTP 2.0 API
  THttpApiLoggingFlags = set of (
    hlfLocalTimeRollover, hlfUseUTF8Conversion,
    hlfLogErrorsOnly, hlfLogSuccessOnly);

  /// http.sys API 2.0 fields used for W3C logging
  // - match low-level HTTP_LOG_FIELD_* constants as defined in HTTP 2.0 API
  THttpApiLogFields = set of (
    hlfDate, hlfTime, hlfClientIP, hlfUserName, hlfSiteName, hlfComputerName,
    hlfServerIP, hlfMethod, hlfURIStem, hlfURIQuery, hlfStatus, hlfWIN32Status,
    hlfBytesSent, hlfBytesRecv, hlfTimeTaken, hlfServerPort, hlfUserAgent,
    hlfCookie, hlfReferer, hlfVersion, hlfHost, hlfSubStatus);

  /// http.sys API 2.0 fields used for server-side authentication
  // - as used by
  // - will match low-level HTTP_AUTH_ENABLE_* constants as defined in HTTP 2.0 API
  THttpApiRequestAuthentications = set of (
    haBasic, haDigest, haNtlm, haNegotiate, haKerberos);

  /// HTTP server using fast http.sys kernel-mode server
  // - The HTTP Server API enables applications to communicate over HTTP without
  // using Microsoft Internet Information Server (IIS). Applications can register
  // to receive HTTP requests for particular URLs, receive HTTP requests, and send
  // HTTP responses. The HTTP Server API includes SSL support so that applications
  // can exchange data over secure HTTP connections without IIS. It is also
  // designed to work with I/O completion ports.
  // - The HTTP Server API is supported on Windows Server 2003 operating systems
  // and on Windows XP with Service Pack 2 (SP2). Be aware that Microsoft IIS 5
  // running on Windows XP with SP2 is not able to share port 80 with other HTTP
  // applications running simultaneously.
  THttpApiServer = class(THttpServerGeneric)
  protected
    /// the internal request queue
		fReqQueue: THandle;
    /// contain list of THttpApiServer cloned instances
    fClones: TObjectList;
    // if fClones=nil, fOwner contains the main THttpApiServer instance
    fOwner: THttpApiServer;
    /// list of all registered URL
    fRegisteredUnicodeUrl: array of SynUnicode;
    fServerSessionID: HTTP_SERVER_SESSION_ID;
    fUrlGroupID: HTTP_URL_GROUP_ID;
    fExecuteFinished: boolean;
    fLogData: pointer;
    fLogDataStorage: array of byte;
    fLoggingServiceName: SockString;
    fAuthenticationSchemes: THttpApiRequestAuthentications;
    fReceiveBufferSize: cardinal;
    procedure SetReceiveBufferSize(Value: cardinal);
    function GetRegisteredUrl: SynUnicode;
    function GetCloned: boolean;
    function GetHTTPQueueLength: Cardinal;
    procedure SetHTTPQueueLength(aValue: Cardinal);
    function GetMaxBandwidth: Cardinal;
    procedure SetMaxBandwidth(aValue: Cardinal);
    function GetMaxConnections: Cardinal;
    procedure SetMaxConnections(aValue: Cardinal);
    procedure SetOnTerminate(const Event: TNotifyThreadEvent); override;
    function GetAPIVersion: string; override;
    function GetLogging: boolean;
    procedure SetServerName(const aName: SockString); override;
    procedure SetLoggingServiceName(const aName: SockString);
    /// server main loop - don't change directly
    // - will call the Request public virtual method with the appropriate
    // parameters to retrive the content
    procedure Execute; override;
    /// create a clone
    constructor CreateClone(From: THttpApiServer);
  public
    /// initialize the HTTP Service
    // - will raise an exception if http.sys is not available (e.g. before
    // Windows XP SP2) or if the request queue creation failed
    // - if you override this contructor, put the AddUrl() methods within,
    // and you can set CreateSuspended to FALSE
    // - if you will call AddUrl() methods later, set CreateSuspended to TRUE,
    // then call explicitely the Resume method, after all AddUrl() calls, in
    // order to start the server
    constructor Create(CreateSuspended: Boolean; QueueName: SynUnicode='';
      OnStart: TNotifyThreadEvent=nil; OnStop: TNotifyThreadEvent=nil;
      const ProcessName: SockString=''); reintroduce;
    /// release all associated memory and handles
    destructor Destroy; override;
    /// will clone this thread into multiple other threads
    // - could speed up the process on multi-core CPU
    // - will work only if the OnProcess property was set (this is the case
    // e.g. in TSQLHttpServer.Create() constructor)
    // - maximum value is 256 - higher should not be worth it
    procedure Clone(ChildThreadCount: integer);
    /// register the URLs to Listen On
    // - e.g. AddUrl('root','888')
    // - aDomainName could be either a fully qualified case-insensitive domain
    // name, an IPv4 or IPv6 literal string, or a wildcard ('+' will bound
    // to all domain names for the specified port, '*' will accept the request
    // when no other listening hostnames match the request for that port)
    // - return 0 (NO_ERROR) on success, an error code if failed: under Vista
    // and Seven, you could have ERROR_ACCESS_DENIED if the process is not
    // running with enough rights (by default, UAC requires administrator rights
    // for adding an URL to http.sys registration list) - solution is to call
    // the THttpApiServer.AddUrlAuthorize class method during program setup
    // - if this method is not used within an overridden constructor, default
    // Create must have be called with CreateSuspended = TRUE and then call the
    // Resume method after all Url have been added
    // - if aRegisterURI is TRUE, the URI will be registered (need adminitrator
    // rights) - default is FALSE, as defined by Windows security policy
    function AddUrl(const aRoot, aPort: SockString; Https: boolean=false;
      const aDomainName: SockString='*'; aRegisterURI: boolean=false): integer;
    /// un-register the URLs to Listen On
    // - this method expect the same parameters as specified to AddUrl()
    // - return 0 (NO_ERROR) on success, an error code if failed (e.g.
    // -1 if the corresponding parameters do not match any previous AddUrl)
    function RemoveUrl(const aRoot, aPort: SockString; Https: boolean=false;
      const aDomainName: SockString='*'): integer;
    /// will authorize a specified URL prefix
    // - will allow to call AddUrl() later for any user on the computer
    // - if aRoot is left '', it will authorize any root for this port
    // - must be called with Administrator rights: this class function is to be
    // used in a Setup program for instance, especially under Vista or Seven,
    // to reserve the Url for the server
    // - add a new record to the http.sys URL reservation store
    // - return '' on success, an error message otherwise
    // - will first delete any matching rule for this URL prefix
    // - if OnlyDelete is true, will delete but won't add the new authorization;
    // in this case, any error message at deletion will be returned
    class function AddUrlAuthorize(const aRoot, aPort: SockString; Https: boolean=false;
      const aDomainName: SockString='*'; OnlyDelete: boolean=false): string;
    /// will register a compression algorithm
    // - overridden method which will handle any cloned instances
    procedure RegisterCompress(aFunction: THttpSocketCompress;
      aCompressMinSize: integer=1024); override;
    /// access to the internal THttpApiServer list cloned by this main instance
    // - as created by Clone() method
    property Clones: TObjectList read fClones;
  public { HTTP API 2.0 methods and properties }
    /// can be used to check if the HTTP API 2.0 is available
    function HasAPI2: boolean;
    /// enable HTTP API 2.0 advanced timeout settings
    // - all those settings are set for the current URL group
    // - will raise an EHttpApiServer exception if the old HTTP API 1.x is used
    // so you should better test the availability of the method first:
    // ! if aServer.HasAPI2 then
    // !   SetTimeOutLimits(....);
    // - aEntityBody is the time, in seconds, allowed for the request entity
    // body to arrive - default value is 2 minutes
    // - aDrainEntityBody is the time, in seconds, allowed for the HTTP Server
    // API to drain the entity body on a Keep-Alive connection - default value
    // is 2 minutes
    // - aRequestQueue is the time, in seconds, allowed for the request to
    // remain in the request queue before the application picks it up - default
    // value is 2 minutes
    // - aIdleConnection is the time, in seconds, allowed for an idle connection;
    // is similar to THttpServer.ServerKeepAliveTimeOut - default value is
    // 2 minutes
    // - aHeaderWait is the time, in seconds, allowed for the HTTP Server API
    // to parse the request header - default value is 2 minutes
    // - aMinSendRate is the minimum send rate, in bytes-per-second, for the
    // response - default value is 150 bytes-per-second
    // - any value set to 0 will set the HTTP Server API default value
    procedure SetTimeOutLimits(aEntityBody, aDrainEntityBody,
      aRequestQueue, aIdleConnection, aHeaderWait, aMinSendRate: cardinal);
    /// enable HTTP API 2.0 logging
    // - will raise an EHttpApiServer exception if the old HTTP API 1.x is used
    // so you should better test the availability of the method first:
    // ! if aServer.HasAPI2 then
    // !   LogStart(....);
    // - this method won't do anything on the cloned instances, but the main
    // instance logging state will be replicated to all cloned instances
    // - you can select the output folder and the expected logging layout
    // - aSoftwareName will set the optional W3C-only software name string
    // - aRolloverSize will be used only when aRolloverType is hlrSize
    procedure LogStart(const aLogFolder: TFileName;
      aType: THttpApiLoggingType=hltW3C;
      const aSoftwareName: TFileName='';
      aRolloverType: THttpApiLoggingRollOver=hlrDaily;
      aRolloverSize: cardinal=0;
      aLogFields: THttpApiLogFields=[hlfDate..hlfSubStatus];
      aFlags: THttpApiLoggingFlags=[hlfUseUTF8Conversion]);
    /// disable HTTP API 2.0 logging
    // - this method won't do anything on the cloned instances, but the main
    // instance logging state will be replicated to all cloned instances
    procedure LogStop;
    /// enable HTTP API 2.0 server-side authentication
    // - once enabled, the client sends an unauthenticated request: it is up to
    // the server application to generate the initial 401 challenge with proper
    // WWW-Authenticate headers; any further authentication steps will be
    // perform in kernel mode, until the authentication handshake is finalized;
    //  later on, the application can check the AuthenticationStatus property
    // of THttpServerRequest and its associated AuthenticatedUser value
    // see https://msdn.microsoft.com/en-us/library/windows/desktop/aa364452
    // - will raise an EHttpApiServer exception if the old HTTP API 1.x is used
    // so you should better test the availability of the method first:
    // ! if aServer.HasAPI2 then
    // !   SetAuthenticationSchemes(....);
    // - this method will work on the current group, for all instances
    // - see HTTPAPI_AUTH_ENABLE_ALL constant to set all available schemes
    // - optional Realm parameters can be used when haBasic scheme is defined
    // - optional DomainName and Realm parameters can be used for haDigest
    procedure SetAuthenticationSchemes(schemes: THttpApiRequestAuthentications;
      const DomainName: SynUnicode=''; const Realm: SynUnicode='');
    /// read-only access to HTTP API 2.0 server-side enabled authentication schemes
    property AuthenticationSchemes: THttpApiRequestAuthentications
      read fAuthenticationSchemes;
    /// read-only access to check if the HTTP API 2.0 logging is enabled
    // - use LogStart/LogStop methods to change this property value
    property Logging: boolean read GetLogging;
    /// the current HTTP API 2.0 logging Service name
    // - should be UTF-8 encoded, if LogStart(aFlags=[hlfUseUTF8Conversion])
    // - this value is dedicated to one instance, so the main instance won't
    // propagate the change to all cloned instances
    property LoggingServiceName: SockString
      read fLoggingServiceName write SetLoggingServiceName;
    /// read-only access to the low-level HTTP API 2.0 Session ID
    property ServerSessionID: HTTP_SERVER_SESSION_ID read fServerSessionID;
    /// read-only access to the low-level HTTP API 2.0 URI Group ID
    property UrlGroupID: HTTP_URL_GROUP_ID read fUrlGroupID;
    /// how many bytes are retrieved in a single call to ReceiveRequestEntityBody
    // - set by default to 1048576, i.e. 1 MB - practical limit is around 20 MB
    // - you may customize this value if you encounter HTTP error 406 from client,
    // corresponding to an ERROR_NO_SYSTEM_RESOURCES (1450) exception on server
    // side, when uploading huge data content
    property ReceiveBufferSize: cardinal read fReceiveBufferSize write SetReceiveBufferSize;
  published
    /// TRUE if this instance is in fact a cloned instance for the thread pool
    property Cloned: boolean read GetCloned;
    /// return the list of registered URL on this server instance
    property RegisteredUrl: SynUnicode read GetRegisteredUrl;
    /// HTTP.sys requers/responce queue length (via HTTP API 2.0)
    // - default value if 1000, which sounds fine for most use cases
    // - increase this value in case of many 503 HTTP answers or if many
    // "QueueFull" messages appear in HTTP.sys log files (normaly in
    // C:\Windows\System32\LogFiles\HTTPERR\httperr*.log) - may appear with
    // thousands of concurrent clients accessing at once the same server
  	// - see @http://msdn.microsoft.com/en-us/library/windows/desktop/aa364501
    // - will return 0 if the system does not support HTTP API 2.0 (i.e.
    // under Windows XP or Server 2003)
    // - this method will also handle any cloned instances, so you can write e.g.
    // ! if aSQLHttpServer.HttpServer.InheritsFrom(THttpApiServer) then
    // !   THttpApiServer(aSQLHttpServer.HttpServer).HTTPQueueLength := 5000;
    property HTTPQueueLength: Cardinal read GetHTTPQueueLength write SetHTTPQueueLength;
    /// the maximum allowed bandwidth rate in bytes per second (via HTTP API 2.0)
    // - Setting this value to 0 allows an unlimited bandwidth
    // - by default Windows not limit bandwidth (actually limited to 4 Gbit/sec).
    // - will return 0 if the system does not support HTTP API 2.0 (i.e.
    // under Windows XP or Server 2003)
    property MaxBandwidth: Cardinal read GetMaxBandwidth write SetMaxBandwidth;
    /// the maximum number of HTTP connections allowed (via HTTP API 2.0)
    // - Setting this value to 0 allows an unlimited number of connections
    // - by default Windows does not limit number of allowed connections
    // - will return 0 if the system does not support HTTP API 2.0 (i.e.
    // under Windows XP or Server 2003)
    property MaxConnections: Cardinal read GetMaxConnections write SetMaxConnections;
  end;
  {$endif MSWINDOWS}


  /// main HTTP server Thread using the standard Sockets API (e.g. WinSock)
  // - bind to a port and listen to incoming requests
  // - assign this requests to THttpServerResp threads
  // - it implements a HTTP/1.1 compatible server, according to RFC 2068 specifications
  // - if the client is also HTTP/1.1 compatible, KeepAlive connection is handled:
  //  multiple requests will use the existing connection and thread;
  //  this is faster and uses less resources, especialy under Windows
  // - a Thread Pool is used internaly to speed up HTTP/1.0 connections
  // - it will trigger the Windows firewall popup UAC window at first run
  // - don't forget to use Free procedure when you are finished
  THttpServer = class(THttpServerGeneric)
  protected
    /// used to protect Process() call
    fProcessCS: TRTLCriticalSection;
    fThreadPoolPush: TOnThreadPoolSocketPush;
    fThreadPoolContentionCount: cardinal;
    fThreadPoolContentionAbortCount: cardinal;
    {$ifdef USETHREADPOOL}
    fThreadPool: TSynThreadPoolTHttpServer;
    {$endif}
    fInternalHttpServerRespList: TList;
    fServerConnectionCount: cardinal;
    fServerKeepAliveTimeOut: cardinal;
    fTCPPrefix: SockString;
    fSock: TCrtSocket;
    fThreadRespClass: THttpServerRespClass;
    // this overridden version will return e.g. 'Winsock 2.514'
    function GetAPIVersion: string; override;
    /// server main loop - don't change directly
    procedure Execute; override;
    /// this method is called on every new client connection, i.e. every time
    // a THttpServerResp thread is created with a new incoming socket
    procedure OnConnect; virtual;
    /// this method is called on every client disconnection to update stats
    procedure OnDisconnect; virtual;
    /// override this function in order to low-level process the request;
    // default process is to get headers, and call public function Request
    procedure Process(ClientSock: THttpServerSocket;
      ConnectionID: integer; ConnectionThread: TSynThread); virtual;
  public
    /// create a Server Thread, binded and listening on a port
    // - this constructor will raise a EHttpServer exception if binding failed
    // - expects the port to be specified as string, e.g. '1234'; you can
    // optionally specify a server address to bind to, e.g. '1.2.3.4:1234'
    // - you can specify a number of threads to be initialized to handle
    // incoming connections (default is 32, which may be sufficient for most
    // cases, maximum is 64) - if you set 0, the thread pool will be disabled
    // and one thread will be created for any incoming connection
    constructor Create(const aPort: SockString; OnStart,OnStop: TNotifyThreadEvent;
      const ProcessName: SockString {$ifdef USETHREADPOOL};
        ServerThreadPoolCount: integer=32{$endif});
      reintroduce; virtual;
    /// release all memory and handlers
    destructor Destroy; override;
    /// access to the main server low-level Socket
    // - it's a raw TCrtSocket, which only need a socket to be bound, listening
    // and accept incoming request
    // - THttpServerSocket are created on the fly for every request, then
    // a THttpServerResp thread is created for handling this THttpServerSocket
    property Sock: TCrtSocket read fSock;
  published
    /// will contain the total number of connection to the server
    // - it's the global count since the server started
    property ServerConnectionCount: cardinal
      read fServerConnectionCount write fServerConnectionCount;
    /// time, in milliseconds, for the HTTP.1/1 connections to be kept alive;
    // default is 3000 ms
    // - see THttpApiServer.SetTimeOutLimits(aIdleConnection) parameter
    property ServerKeepAliveTimeOut: cardinal
      read fServerKeepAliveTimeOut write fServerKeepAliveTimeOut;
    /// TCP/IP prefix to mask HTTP protocol
    // - if not set, will create full HTTP/1.0 or HTTP/1.1 compliant content
    // - in order to make the TCP/IP stream not HTTP compliant, you can specify
    // a prefix which will be put before the first header line: in this case,
    // the TCP/IP stream won't be recognized as HTTP, and will be ignored by
    // most AntiVirus programs, and increase security - but you won't be able
    // to use an Internet Browser nor AJAX application for remote access any more
    property TCPPrefix: SockString read fTCPPrefix write fTCPPrefix;
    /// number of times there was no availibility in the internal thread pool
    // to handle an incoming request
    // - this won't make any error, but just delay for 20 ms and try again
    property ThreadPoolContentionCount: cardinal read fThreadPoolContentionCount;
    /// number of times there an incoming request is rejected due to overload
    // - this is an error after 30 seconds of not any process availability
    property ThreadPoolContentionAbortCount: cardinal read fThreadPoolContentionAbortCount;
  end;
{$M-}

  /// structure used to parse an URI into its components
  // - ready to be supplied e.g. to a THttpRequest sub-class
  // - used e.g. by class function THttpRequest.Get()
  TURI = {$ifdef UNICODE}record{$else}object{$endif}
  public
    /// if the server is accessible via http:// or https://
    Https: boolean;
    /// the server name
    // - e.g. 'www.somewebsite.com'
    Server: SockString;
    /// the server port
    // - e.g. '80'
    Port: SockString;
    /// the resource address
    // - e.g. '/category/name/10?param=1'
    Address: SockString;
    /// fill the members from a supplied URI
    function From(aURI: SockString): boolean;
    /// compute the whole normalized URI
    function URI: SockString;
    /// the server port, as integer value
    function PortInt: integer;
  end;

  /// the supported authentication schemes which may be used by HTTP clients
  // - supported only by TWinHTTP class yet
  THttpRequestAuthentication = (wraNone,wraBasic,wraDigest,wraNegotiate);

  /// a record to set some extended options for HTTP clients
  // - allow easy propagation e.g. from a TSQLHttpClient* wrapper class to
  // the actual SynCrtSock's THttpRequest implementation class
  THttpRequestExtendedOptions = record
    /// let HTTPS be less paranoid about SSL certificates
    // - IgnoreSSLCertificateErrors is handled by TWinHttp and TCurlHTTP
    IgnoreSSLCertificateErrors: Boolean;
    /// allow HTTP authentication to take place at connection
    // - Auth.Scheme and UserName/Password properties are handled
    // by the TWinHttp class only by now
    Auth: record
      UserName: SynUnicode;
      Password: SynUnicode;
      Scheme: THttpRequestAuthentication;
    end;
  end;

  {$M+}
  /// abstract class to handle HTTP/1.1 request
  // - never instantiate this class, but inherited TWinHTTP, TWinINet or TCurlHTTP
  THttpRequest = class
  protected
    fServer: SockString;
    fProxyName: SockString;
    fProxyByPass: SockString;
    fPort: cardinal;
    fHttps: boolean;
    fKeepAlive: cardinal;
    fUserAgent: SockString;
    fExtendedOptions: THttpRequestExtendedOptions;
    /// used by RegisterCompress method
    fCompress: THttpSocketCompressRecDynArray;
    /// set by RegisterCompress method
    fCompressAcceptEncoding: SockString;
    /// set index of protocol in fCompress[], from ACCEPT-ENCODING: header
    fCompressHeader: THttpSocketCompressSet;
    fTag: PtrInt;
    class function InternalREST(const url,method,data,header: SockString;
      aIgnoreSSLCertificateErrors: boolean; outHeaders: PSockString=nil): SockString;
    // inherited class should override those abstract methods
    procedure InternalConnect(ConnectionTimeOut,SendTimeout,ReceiveTimeout: DWORD); virtual; abstract;
    procedure InternalCreateRequest(const method, aURL: SockString); virtual; abstract;
    procedure InternalSendRequest(const aData: SockString); virtual; abstract;
    function InternalRetrieveAnswer(var Header, Encoding, AcceptEncoding,
      Data: SockString): integer; virtual; abstract;
    procedure InternalCloseRequest; virtual; abstract;
    procedure InternalAddHeader(const hdr: SockString); virtual; abstract;
  public
    /// connect to http://aServer:aPort or https://aServer:aPort
    // - optional aProxyName may contain the name of the proxy server to use,
    // and aProxyByPass an optional semicolon delimited list of host names or
    // IP addresses, or both, that should not be routed through the proxy
    // (works only with TWinHTTP and TWinINet yet)
    // - you can customize the default client timeouts by setting appropriate
    // SendTimeout and ReceiveTimeout parameters (in ms) - note that after
    // creation of this instance, the connection is tied to the initial
    // parameters, so we won't publish any properties to change those
    // initial values once created - if you left the 0 default parameters, it
    // would use global HTTP_DEFAULT_CONNECTTIMEOUT, HTTP_DEFAULT_SENDTIMEOUT
    // and HTTP_DEFAULT_RECEIVETIMEOUT variable values
    // - aProxyName and *TimeOut parameters are currently ignored by TCurlHttp
    constructor Create(const aServer, aPort: SockString; aHttps: boolean;
      const aProxyName: SockString=''; const aProxyByPass: SockString='';
      ConnectionTimeOut: DWORD=0; SendTimeout: DWORD=0; ReceiveTimeout: DWORD=0); virtual;

    /// low-level HTTP/1.1 request
    // - after an Create(server,port), return 200,202,204 if OK,
    // http status error otherwise
    function Request(const url, method: SockString; KeepAlive: cardinal;
      const InHeader, InData, InDataType: SockString;
      out OutHeader, OutData: SockString): integer; virtual;

    /// wrapper method to retrieve a resource via an HTTP GET
    // - will parse the supplied URI to check for the http protocol (HTTP/HTTPS),
    // server name and port, and resource name
    // - aIgnoreSSLCerticateErrors will ignore the error when using untrusted certificates
    // - it will internally create a THttpRequest inherited instance: do not use
    // THttpRequest.Get() but either TWinHTTP.Get(), TWinINet.Get() or
    // TCurlHTTP.Get() methods
    class function Get(const aURI: SockString; const aHeader: SockString='';
      aIgnoreSSLCertificateErrors: Boolean=true; outHeaders: PSockString=nil): SockString;
    /// wrapper method to create a resource via an HTTP POST
    // - will parse the supplied URI to check for the http protocol (HTTP/HTTPS),
    // server name and port, and resource name
    // - aIgnoreSSLCerticateErrors will ignore the error when using untrusted certificates
    // - the supplied aData content is POSTed to the server, with an optional
    // aHeader content
    // - it will internally create a THttpRequest inherited instance: do not use
    // THttpRequest.Post() but either TWinHTTP.Post(), TWinINet.Post() or
    // TCurlHTTP.Post() methods
    class function Post(const aURI, aData: SockString; const aHeader: SockString='';
      aIgnoreSSLCertificateErrors: Boolean=true; outHeaders: PSockString=nil): SockString;
    /// wrapper method to update a resource via an HTTP PUT
    // - will parse the supplied URI to check for the http protocol (HTTP/HTTPS),
    // server name and port, and resource name
    // - aIgnoreSSLCerticateErrors will ignore the error when using untrusted certificates
    // - the supplied aData content is PUT to the server, with an optional
    // aHeader content
    // - it will internally create a THttpRequest inherited instance: do not use
    // THttpRequest.Put() but either TWinHTTP.Put(), TWinINet.Put() or
    // TCurlHTTP.Put() methods
    class function Put(const aURI, aData: SockString; const aHeader: SockString='';
      aIgnoreSSLCertificateErrors: Boolean=true; outHeaders: PSockString=nil): SockString;
    /// wrapper method to delete a resource via an HTTP DELETE
    // - will parse the supplied URI to check for the http protocol (HTTP/HTTPS),
    // server name and port, and resource name
    // - aIgnoreSSLCerticateErrors will ignore the error when using untrusted certificates
    // - it will internally create a THttpRequest inherited instance: do not use
    // THttpRequest.Delete() but either TWinHTTP.Delete(), TWinINet.Delete() or
    // TCurlHTTP.Delete() methods
    class function Delete(const aURI: SockString; const aHeader: SockString='';
      aIgnoreSSLCertificateErrors: Boolean=true; outHeaders: PSockString=nil): SockString;

    /// will register a compression algorithm
    // - used e.g. to compress on the fly the data, with standard gzip/deflate
    // or custom (synlzo/synlz) protocols
    // - returns true on success, false if this function or this
    // ACCEPT-ENCODING: header was already registered
    // - you can specify a minimal size (in bytes) before which the content won't
    // be compressed (1024 by default, corresponding to a MTU of 1500 bytes)
    // - the first registered algorithm will be the prefered one for compression
    function RegisterCompress(aFunction: THttpSocketCompress;
      aCompressMinSize: integer=1024): boolean;

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
    /// internal structure used to store extended options
    // - will be replicated by IgnoreSSLCertificateErrors and Auth* properties
    property ExtendedOptions: THttpRequestExtendedOptions
      read fExtendedOptions write fExtendedOptions;
    /// some internal field, which may be used by end-user code
    property Tag: PtrInt read fTag write fTag;
  published
    /// the remote server host name, as stated specified to the class constructor
    property Server: SockString read fServer;
    /// the remote server port number, as specified to the class constructor
    property Port: cardinal read fPort;
    /// if the remote server uses HTTPS, as specified to the class constructor
    property Https: boolean read fHttps;
    /// the remote server optional proxy, as specified to the class constructor
    property ProxyName: SockString read fProxyName;
    /// the remote server optional proxy by-pass list, as specified to the class
    // constructor
    property ProxyByPass: SockString read fProxyByPass;
    /// the HTTP "User Agent:" header value
    // - you can customize this value from its default content
    // (only for TCurlHTTP class)
    property UserAgent: SockString read fUserAgent write fUserAgent;
  end;
  {$M-}

  /// store the actual class of a HTTP/1.1 client instance
  // - may be used to define at runtime which API to be used (e.g. WinHTTP,
  // WinINet or LibCurl), following the Liskov substitution principle
  THttpRequestClass = class of THttpRequest;

{$ifdef USEWININET}
  TWinHttpAPI = class;

  /// event callback to track download progress, e.g. in the UI
  // - used in TWinHttpAPI.OnProgress property
  // - CurrentSize is the current total number of downloaded bytes
  // - ContentLength is retrieved from HTTP headers, but may be 0 if not set
  TWinHttpProgress = procedure(Sender: TWinHttpAPI;
    CurrentSize, ContentLength: DWORD) of object;
  /// event callback to process the download by chunks, not in memory
  // - used in TWinHttpAPI.OnDownload property
  // - CurrentSize is the current total number of downloaded bytes
  // - ContentLength is retrieved from HTTP headers, but may be 0 if not set
  // - ChunkSize is the size of the latest downloaded chunk, available in
  // the untyped ChunkData memory buffer
  // - implementation should return TRUE to continue the download, or FALSE
  // to abort the download process
  TWinHttpDownload = function(Sender: TWinHttpAPI;
    CurrentSize, ContentLength, ChunkSize: DWORD; const ChunkData): boolean of object;

  /// a class to handle HTTP/1.1 request using either WinINet or WinHTTP API
  // - both APIs have a common logic, which is encapsulated by this parent class
  // - this abstract class defined some abstract methods which will be
  // implemented by TWinINet or TWinHttp with the proper API calls
  TWinHttpAPI = class(THttpRequest)
  protected
    fOnProgress: TWinHttpProgress;
    fOnDownload: TWinHttpDownload;
    fOnDownloadChunkSize: cardinal;
    /// used for internal connection
    fSession, fConnection, fRequest: HINTERNET;
    function InternalGetInfo(Info: DWORD): SockString; virtual; abstract;
    function InternalGetInfo32(Info: DWORD): DWORD; virtual; abstract;
    function InternalReadData(var Data: SockString; Read: integer): cardinal; virtual; abstract;
    function InternalRetrieveAnswer(
      var Header, Encoding, AcceptEncoding, Data: SockString): integer; override;
  public
    /// download would call this method to notify progress
    property OnProgress: TWinHttpProgress read fOnProgress write fOnProgress;
    /// download would call this method instead of filling Data: SockString value
    // - may be used e.g. when downloading huge content, and saving directly
    // the incoming data on disk or database
    // - if this property is set, raw TCP/IP incoming data would be supplied:
    // compression and encoding won't be handled by the class
    property OnDownload: TWinHttpDownload read fOnDownload write fOnDownload;
    /// how many bytes should be retrieved for each OnDownload event chunk
    // - if default 0 value is left, would use 65536, i.e. 64KB
    property OnDownloadChunkSize: cardinal
      read fOnDownloadChunkSize write fOnDownloadChunkSize;
  end;

  /// a class to handle HTTP/1.1 request using the WinINet API
  // - The Microsoft Windows Internet (WinINet) application programming interface
  // (API) enables applications to access standard Internet protocols, such as
  // FTP and HTTP/HTTPS, similar to what IE offers
  // - by design, the WinINet API should not be used from a service, since this
  // API may require end-user GUI interaction
  // - note: WinINet is MUCH slower than THttpClientSocket or TWinHttp: do not
  // use this, only if you find some configuration benefit on some old networks
  // (e.g. to diaplay the dialup popup window for a GUI client application)
  TWinINet = class(TWinHttpAPI)
  protected
    // those internal methods will raise an EWinINet exception on error
    procedure InternalConnect(ConnectionTimeOut,SendTimeout,ReceiveTimeout: DWORD); override;
    procedure InternalCreateRequest(const method, aURL: SockString); override;
    procedure InternalCloseRequest; override;
    procedure InternalAddHeader(const hdr: SockString); override;
    procedure InternalSendRequest(const aData: SockString); override;
    function InternalGetInfo(Info: DWORD): SockString; override;
    function InternalGetInfo32(Info: DWORD): DWORD; override;
    function InternalReadData(var Data: SockString; Read: integer): cardinal; override;
  public
    /// relase the connection
    destructor Destroy; override;
  end;

  /// WinINet exception type
  EWinINet = class(ECrtSocket)
  public
    /// create a WinINet exception, with the error message as text
    constructor Create;
  end;

  /// a class to handle HTTP/1.1 request using the WinHTTP API
  // - has a common behavior as THttpClientSocket() but seems to be faster
  // over a network and is able to retrieve the current proxy settings
  // (if available) and handle secure https connection - so it seems to be the
  // class to use in your client programs
  // - WinHTTP does not share any proxy settings with Internet Explorer.
  // The WinHTTP proxy configuration is set by either
  // $ proxycfg.exe
  // on Windows XP and Windows Server 2003 or earlier, either
  // $ netsh.exe
  // on Windows Vista and Windows Server 2008 or later; for instance,
  // you can run either:
  // $ proxycfg -u
  // $ netsh winhttp import proxy source=ie
  // to use the current user's proxy settings for Internet Explorer (under 64 bit
  // Vista/Seven, to configure applications using the 32 bit WinHttp settings,
  // call netsh or proxycfg bits from %SystemRoot%\SysWOW64 folder explicitely)
  // - Microsoft Windows HTTP Services (WinHTTP) is targeted at middle-tier and
  // back-end server applications that require access to an HTTP client stack
  TWinHTTP = class(TWinHttpAPI)
  protected
    // those internal methods will raise an EOSError exception on error
    procedure InternalConnect(ConnectionTimeOut,SendTimeout,ReceiveTimeout: DWORD); override;
    procedure InternalCreateRequest(const method, aURL: SockString); override;
    procedure InternalCloseRequest; override;
    procedure InternalAddHeader(const hdr: SockString); override;
    procedure InternalSendRequest(const aData: SockString); override;
    function InternalGetInfo(Info: DWORD): SockString; override;
    function InternalGetInfo32(Info: DWORD): DWORD; override;
    function InternalReadData(var Data: SockString; Read: integer): cardinal; override;
  public
    /// relase the connection
    destructor Destroy; override;
  end;

  /// WinHTTP exception type
  EWinHTTP = class(Exception);

{$endif USEWININET}

{$ifdef USELIBCURL}
type
  /// a class to handle HTTP/1.1 request using the libcurl library
  // - libcurl is a free and easy-to-use cross-platform URL transfer library,
  // able to directly connect via HTTP or HTTPS on most Linux systems
  // - under a 32 bit Linux system, the libcurl library (and its dependencies,
  // like OpenSSL) may not be installed - you can add it via your package
  // manager, e.g. on Ubuntu:
  // $ sudo apt-get install libcurl3
  // - under a 64 bit Linux system, you should install the 32 bit flavor of
  // libcurl, e.g. on Ubuntu:
  // $ sudo apt-get install libcurl3:i386
  // - will use in fact libcurl.so, so either libcurl.so.3 or libcurl.so.4,
  // depending on the default version installation on the system
  TCurlHTTP = class(THttpRequest)
  protected
    fHandle: pointer;
    fRootURL: SockString;
    fIn: record
      Headers: pointer;
      Method: SockString;
      Data: SockString;
      DataOffset: integer;
    end;
    fOut: record
      Header, Encoding, AcceptEncoding, Data: SockString;
    end;
    fSSL: record
      CertFile: SockString;
      CACertFile: SockString;
      KeyName: SockString;
      PassPhrase: SockString;
    end;
    procedure InternalConnect(ConnectionTimeOut,SendTimeout,ReceiveTimeout: DWORD); override;
    procedure InternalCreateRequest(const method, aURL: SockString); override;
    procedure InternalSendRequest(const aData: SockString); override;
    function InternalRetrieveAnswer(var Header, Encoding, AcceptEncoding,
      Data: SockString): integer; override;
    procedure InternalCloseRequest; override;
    procedure InternalAddHeader(const hdr: SockString); override;
  public
    /// release the connection
    destructor Destroy; override;
    /// set the client SSL certification details
    // - used e.g. as
    // ! UseClientCertificate('testcert.pem','cacert.pem','testkey.pem','pass');
    procedure UseClientCertificate(
      const aCertFile, aCACertFile, aKeyName, aPassPhrase: SockString);
  end;

{$endif USELIBCURL}

/// create a TCrtSocket, returning nil on error
// (useful to easily catch socket error exception ECrtSocket)
function Open(const aServer, aPort: SockString): TCrtSocket;

/// create a THttpClientSocket, returning nil on error
// - useful to easily catch socket error exception ECrtSocket
function OpenHttp(const aServer, aPort: SockString): THttpClientSocket; overload;

/// create a THttpClientSocket, returning nil on error
// - useful to easily catch socket error exception ECrtSocket
function OpenHttp(const aURI: SockString; aAddress: PSockString=nil): THttpClientSocket; overload;

/// retrieve the content of a web page, using the HTTP/1.1 protocol and GET method
// - this method will use a low-level THttpClientSock socket: if you want
// something able to use your computer proxy, take a look at TWinINet.Get()
function HttpGet(const server, port: SockString; const url: SockString;
  const inHeaders: SockString; outHeaders: PSockString=nil): SockString; overload;

/// retrieve the content of a web page, using the HTTP/1.1 protocol and GET method
// - this method will use a low-level THttpClientSock socket: if you want
// something able to use your computer proxy, take a look at TWinINet.Get()
function HttpGet(const aURI: SockString; outHeaders: PSockString=nil): SockString; overload;

/// retrieve the content of a web page, using the HTTP/1.1 protocol and GET method
// - this method will use a low-level THttpClientSock socket: if you want
// something able to use your computer proxy, take a look at TWinINet.Get()
function HttpGet(const aURI: SockString; const inHeaders: SockString; 
  outHeaders: PSockString=nil): SockString; overload;

/// send some data to a remote web server, using the HTTP/1.1 protocol and POST method
function HttpPost(const server, port: SockString; const url, Data, DataType: SockString): boolean;

const
  /// the layout of TSMTPConnection.FromText method
  SMTP_DEFAULT = 'user:password@smtpserver:port';

type
  /// may be used to store a connection to a SMTP server
  // - see SendEmail() overloaded function
  TSMTPConnection = {$ifdef UNICODE}record{$else}object{$endif}
  public
    /// the SMTP server IP or host name
    Host: SockString;
    /// the SMTP server port (25 by default)
    Port: SockString;
    /// the SMTP user login (if any)
    User: SockString;
    /// the SMTP user password (if any)
    Pass: SockString;
    /// fill the STMP server information from a single text field
    // - expects 'user:password@smtpserver:port' format
    // - if aText equals SMTP_DEFAULT ('user:password@smtpserver:port'),
    // does nothing
    function FromText(const aText: SockString): boolean;
  end;

/// send an email using the SMTP protocol
// - retry true on success
// - the Subject is expected to be in plain 7 bit ASCII, so you could use
// SendEmailSubject() to encode it as Unicode, if needed
// - you can optionally set the encoding charset to be used for the Text body
function SendEmail(const Server, From, CSVDest, Subject, Text: SockString;
  const Headers: SockString=''; const User: SockString=''; const Pass: SockString='';
  const Port: SockString='25'; const TextCharSet: SockString = 'ISO-8859-1'): boolean; overload;

/// send an email using the SMTP protocol
// - retry true on success
// - the Subject is expected to be in plain 7 bit ASCII, so you could use
// SendEmailSubject() to encode it as Unicode, if needed
// - you can optionally set the encoding charset to be used for the Text body
function SendEmail(const Server: TSMTPConnection;
  const From, CSVDest, Subject, Text: SockString; const Headers: SockString='';
  const TextCharSet: SockString = 'ISO-8859-1'): boolean; overload;

/// convert a supplied subject text into an Unicode encoding
// - will convert the text into UTF-8 and append '=?UTF-8?B?'
// - for pre-Unicode versions of Delphi, Text is expected to be already UTF-8
// encoded - since Delphi 2010, it will be converted from UnicodeString
function SendEmailSubject(const Text: string): SockString;

const
  /// HTTP Status Code for "Success"
  STATUS_SUCCESS = 200;
  /// HTTP Status Code for "Created"
  STATUS_CREATED = 201;
  /// HTTP Status Code for "No Content"
  STATUS_NOCONTENT = 204;
  /// HTTP Status Code for "Bad Request"
  STATUS_BADREQUEST = 400;
  /// HTTP Status Code for "Unauthorized"
  STATUS_UNAUTHORIZED = 401;
  /// HTTP Status Code for "Forbidden"
  STATUS_FORBIDDEN = 403;
  /// HTTP Status Code for "Not Found"
  STATUS_NOTFOUND = 404;
  /// HTTP Status Code for "Internal Server Error"
  STATUS_SERVERERROR = 500;
  /// HTTP Status Code for "Not Implemented"
  STATUS_NOTIMPLEMENTED = 501;

  {$ifdef MSWINDOWS}
  /// can be used with THttpApiServer.AuthenticationSchemes to enable all schemes
  HTTPAPI_AUTH_ENABLE_ALL = [hraBasic..hraKerberos];
  {$endif}


/// retrieve the HTTP reason text from a code
// - e.g. StatusCodeToReason(200)='OK'
function StatusCodeToReason(Code: integer): SockString;

/// retrieve the IP address from a computer name
function ResolveName(const Name: SockString;
  Family: Integer=AF_INET; SockProtocol: Integer=IPPROTO_TCP;
  SockType: integer=SOCK_STREAM): SockString;

/// Base64 encoding of a string
function Base64Encode(const s: SockString): SockString;

/// Base64 decoding of a string
function Base64Decode(const s: SockString): SockString;

/// escaping of HTML codes like < > & "
function HtmlEncode(const s: SockString): SockString;

{$ifdef MSWINDOWS}

/// remotly get the MAC address of a computer, from its IP Address
// - only works under Win2K and later
// - return the MAC address as a 12 hexa chars ('0050C204C80A' e.g.)
function GetRemoteMacAddress(const IP: SockString): SockString;

/// enumerate all IP addresses of the current computer
// - may be used to enumerate all adapters
function GetIPAddresses: TSockStringDynArray;

/// returns all IP addresses of the current computer as a single CSV text 
// - may be used to enumerate all adapters
function GetIPAddressesText(const Sep: SockString = ' '): SockString;

{$endif MSWINDOWS}

/// low-level text description of  Socket error code
function SocketErrorMessage(Error: integer): string;


implementation

{$ifdef FPC}
uses
  dynlibs;
{$endif}

{ ************ some shared helper functions and classes }

function StatusCodeToReason(Code: integer): SockString;
begin
  case Code of
    100: result := 'Continue';
    200: result := 'OK';
    201: result := 'Created';
    202: result := 'Accepted';
    203: result := 'Non-Authoritative Information';
    204: result := 'No Content';
    207: result := 'Multi-Status';
    300: result := 'Multiple Choices';
    301: result := 'Moved Permanently';
    302: result := 'Found';
    303: result := 'See Other';
    304: result := 'Not Modified';
    307: result := 'Temporary Redirect';
    400: result := 'Bad Request';
    401: result := 'Unauthorized';
    403: result := 'Forbidden';
    404: result := 'Not Found';
    405: result := 'Method Not Allowed';
    406: result := 'Not Acceptable';
    500: result := 'Internal Server Error';
    503: result := 'Service Unavailable';
    else str(Code,result);
  end;
end;

function Hex2Dec(c: AnsiChar): byte;
begin
  case c of
  'A'..'Z': result := Ord(c) - (Ord('A') - 10);
  'a'..'z': result := Ord(c) - (Ord('a') - 10);
  '0'..'9': result := Ord(c) - Ord('0');
  else result := 255;
  end;
end;

// Base64 string encoding
function Base64Encode(const s: SockString): SockString;
procedure Encode(rp, sp: PAnsiChar; len: integer);
const
  b64: array[0..63] of AnsiChar =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
var i: integer;
    c: cardinal;
begin
  for i := 1 to len div 3 do begin
    c := ord(sp[0]) shl 16 + ord(sp[1]) shl 8 + ord(sp[2]);
    rp[0] := b64[(c shr 18) and $3f];
    rp[1] := b64[(c shr 12) and $3f];
    rp[2] := b64[(c shr 6) and $3f];
    rp[3] := b64[c and $3f];
    inc(rp,4);
    inc(sp,3);
  end;
  case len mod 3 of
    1: begin
      c := ord(sp[0]) shl 16;
      rp[0] := b64[(c shr 18) and $3f];
      rp[1] := b64[(c shr 12) and $3f];
      rp[2] := '=';
      rp[3] := '=';
    end;
    2: begin
      c := ord(sp[0]) shl 16 + ord(sp[1]) shl 8;
      rp[0] := b64[(c shr 18) and $3f];
      rp[1] := b64[(c shr 12) and $3f];
      rp[2] := b64[(c shr 6) and $3f];
      rp[3] := '=';
    end;
  end;
end;
var len: integer;
begin
  result:='';
  len := length(s);
  if len = 0 then exit;
  SetLength(result, ((len + 2) div 3) * 4);
  Encode(pointer(result),pointer(s),len);
end;

function Base64Decode(const s: SockString): SockString;
var i, j, len: integer;
    sp, rp: PAnsiChar;
    c, ch: integer;
begin
  result:= '';
  len := length(s);
  if (len = 0) or (len mod 4 <> 0) then
    exit;
  len := len shr 2;
  SetLength(result, len * 3);
  sp := pointer(s);
  rp := pointer(result);
  for i := 1 to len do begin
    c := 0;
    j := 0;
    while true do begin
      ch := ord(sp[j]);
      case chr(ch) of
        'A'..'Z': c := c or (ch - ord('A'));
        'a'..'z': c := c or (ch - (ord('a')-26));
        '0'..'9': c := c or (ch - (ord('0')-52));
        '+': c := c or 62;
        '/': c := c or 63;
        else
        if j=3 then begin
          rp[0] := AnsiChar(c shr 16);
          rp[1] := AnsiChar(c shr 8);
          SetLength(result, len*3-1);
          exit;
        end else begin
          rp[0] := AnsiChar(c shr 10);
          SetLength(result, len*3-2);
          exit;
        end;
      end;
      if j=3 then break;
      inc(j);
      c := c shl 6;
    end;
    rp[2] := AnsiChar(c);
    c := c shr 8;
    rp[1] := AnsiChar(c);
    c := c shr 8;
    rp[0] := AnsiChar(c);
    inc(rp,3);
    inc(sp,4);
  end;
end;

function HtmlEncode(const s: SockString): SockString;
var i: integer;
begin // not very fast, but working
  result := '';
  for i := 1 to length(s) do
    case s[i] of
      '<': result := result+'&lt;';
      '>': result := result+'&gt;';
      '&': result := result+'&amp;';
      '"': result := result+'&quot;';
      else result := result+s[i];
    end;
end;

function HtmlEncodeString(const s: string): string;
var i: integer;
begin // not very fast, but working
  result := '';
  for i := 1 to length(s) do
    case s[i] of
      '<': result := result+'&lt;';
      '>': result := result+'&gt;';
      '&': result := result+'&amp;';
      '"': result := result+'&quot;';
      else result := result+s[i];
    end;
end;

const
  CRLF: array[0..1] of AnsiChar = (#13,#10);

function StrLen(S: PAnsiChar): integer;
begin
  result := 0;
  if S<>nil then
  while true do
    if S[0]<>#0 then
    if S[1]<>#0 then
    if S[2]<>#0 then
    if S[3]<>#0 then begin
      inc(S,4);
      inc(result,4);
    end else begin
      inc(result,3);
      exit;
    end else begin
      inc(result,2);
      exit;
    end else begin
      inc(result);
      exit;
    end else
      exit;
end;

function IdemPChar(p, up: pAnsiChar): boolean;
// if the beginning of p^ is same as up^ (ignore case - up^ must be already Upper)
var c: AnsiChar;
begin
  result := false;
  if p=nil then
    exit;
  if (up<>nil) and (up^<>#0) then
    repeat
      c := p^;
      if up^<>c then
        if c in ['a'..'z'] then begin
          dec(c,32);
          if up^<>c then
            exit;
        end else exit;
      inc(up);
      inc(p);
    until up^=#0;
  result := true;
end;

function IdemPCharArray(p: PAnsiChar; const upArray: array of PAnsiChar): integer;
var w: word;
begin
  if p<>nil then begin
    w := ord(p[0])+ord(p[1])shl 8;
    if p[0] in ['a'..'z'] then
      dec(w,32);
    if p[1] in ['a'..'z'] then
      dec(w,32 shl 8);
    for result := 0 to high(upArray) do
      if (PWord(upArray[result])^=w) and IdemPChar(p+2,upArray[result]+2) then
        exit;
  end;
  result := -1;
end;

function GetNextItem(var P: PAnsiChar; Sep: AnsiChar = ','): SockString;
// return next CSV string in P, nil if no more
var S: PAnsiChar;
begin
  if P=nil then
    result := '' else begin
    S := P;
    while (S^<>#0) and (S^<>Sep) do
      inc(S);
    SetString(result,P,S-P);
    if S^<>#0 then
     P := S+1 else
     P := nil;
  end;
end;

function GetNextItemUInt64(var P: PAnsiChar): Int64;
var c: PtrInt;
begin
  if P=nil then begin
    result := 0;
    exit;
  end;
  result := byte(P^)-48;  // caller ensured that P^ in ['0'..'9']
  inc(P);
  repeat
    c := byte(P^)-48;
    if c>9 then
      break else
      result := result*10+c;
    inc(P);
  until false;
end; // P^ will point to the first non digit char

function GetNextLine(var P: PAnsiChar): SockString;
var S: PAnsiChar;
begin
  if P=nil then
    result := '' else begin
    S := P;
    while S^>=' ' do
      inc(S);
    SetString(result,P,S-P);
    while (S^<>#0) and (S^<' ') do inc(S); // ignore e.g. #13 or #10
    if S^<>#0 then
      P := S else
      P := nil;
  end;
end;

function GetHeaderValue(var headers: SockString; const upname: SockString;
  deleteInHeaders: boolean): SockString;
var i,j,k: integer;
begin
  result := '';
  if (headers='') or (upname='') then
    exit;
  i := 1;
  repeat
    k := length(headers)+1;
    for j := i to k-1 do
      if headers[j]<' ' then begin
        k := j;
        break;
      end;
    if IdemPChar(@headers[i],pointer(upname)) then begin
      j := i;
      inc(i,length(upname));
      while headers[i]=' ' do inc(i);
      result := copy(headers,i,k-i);
      if deleteInHeaders then begin
        while true do
          if (headers[k]=#0) or (headers[k]>=' ') then
            break else
            inc(k);
        delete(headers,j,k-j);
      end;
      exit;
    end;
    i := k;
    while headers[i]<' ' do
      if headers[i]=#0 then
        exit else
        inc(i);
  until false;
end;

function PosChar(Str: PAnsiChar; Chr: AnsiChar): PAnsiChar;
begin
  result := Str;
  while result^<>Chr do begin
    if result^=#0 then begin
      result := nil;
      exit;
    end;
    Inc(result);
  end;
end;

{$ifdef HASCODEPAGE}
// rewrite some functions to avoid unattempted ansi<->unicode conversion

function Trim(const S: SockString): SockString;
{$ifdef FPC_OR_PUREPASCAL}
var I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I<=L) and (S[i]<=' ') do Inc(I);
  if I>L then
    Result := '' else
  if (I=1) and (S[L]>' ') then
    Result := S else begin
    while S[L]<=' ' do Dec(L);
    Result := Copy(S, I, L-I+1);
  end;
end;
{$else}
asm  // fast implementation by John O'Harrow
  test eax,eax                   {S = nil?}
  xchg eax,edx
  jz   System.@LStrClr           {Yes, Return Empty String}
  mov  ecx,[edx-4]               {Length(S)}
  cmp  byte ptr [edx],' '        {S[1] <= ' '?}
  jbe  @@TrimLeft                {Yes, Trim Leading Spaces}
  cmp  byte ptr [edx+ecx-1],' '  {S[Length(S)] <= ' '?}
  jbe  @@TrimRight               {Yes, Trim Trailing Spaces}
  jmp  System.@LStrLAsg          {No, Result := S (which occurs most time)}
@@TrimLeft:                      {Strip Leading Whitespace}
  dec  ecx
  jle  System.@LStrClr           {All Whitespace}
  inc  edx
  cmp  byte ptr [edx],' '
  jbe  @@TrimLeft
@@CheckDone:
  cmp  byte ptr [edx+ecx-1],' '
{$ifdef UNICODE}
  jbe  @@TrimRight
  push 65535 // SockString code page for Delphi 2009 and up
  call  System.@LStrFromPCharLen // we need a call, not a direct jmp
  ret
{$else}
  ja   System.@LStrFromPCharLen
{$endif}
@@TrimRight:                     {Strip Trailing Whitespace}
  dec  ecx
  jmp  @@CheckDone
end;
{$endif}

function UpperCase(const S: SockString): SockString;
procedure Upper(Source, Dest: PAnsiChar; L: cardinal);
var Ch: AnsiChar; // this sub-call is shorter and faster than 1 plain proc
begin
  repeat
    Ch := Source^;
    if (Ch>='a') and (Ch<='z') then
      dec(Ch,32);
    Dest^ := Ch;
    dec(L);
    inc(Source);
    inc(Dest);
  until L=0;
end;
var L: cardinal;
begin
  result := '';
  L := Length(S);
  if L=0 then
    exit;
  SetLength(result, L);
  Upper(pointer(S),pointer(result),L);
end;

{$endif HASCODEPAGE}

function GetCardinal(P: PAnsiChar): cardinal; overload;
var c: cardinal;
begin
  if P=nil then begin
    result := 0;
    exit;
  end;
  if P^=' ' then repeat inc(P) until P^<>' ';
  c := byte(P^)-48;
  if c>9 then
    result := 0 else begin
    result := c;
    inc(P);
    repeat
      c := byte(P^)-48;
      if c>9 then
        break else
        result := result*10+c;
      inc(P);
    until false;
  end;
end;

function GetCardinal(P,PEnd: PAnsiChar): cardinal; overload;
var c: cardinal;
begin
  result := 0;
  if (P=nil) or (P>=PEnd) then
    exit;
  if P^=' ' then repeat
    inc(P);
    if P=PEnd then exit;
  until P^<>' ';
  c := byte(P^)-48;
  if c>9 then
    exit;
  result := c;
  inc(P);
  while P<PEnd do begin
    c := byte(P^)-48;
    if c>9 then
      break else
      result := result*10+c;
    inc(P);
  end;
end;

function PCharToHex32(p: PAnsiChar): cardinal;
var v0,v1: byte;
begin
  result := 0;
  if p<>nil then begin
    while p^=' ' do inc(p);
    repeat
      v0 := Hex2Dec(p[0]);
      if v0=255 then break; // not in '0'..'9','a'..'f'
      v1 := Hex2Dec(p[1]);
      inc(p);
      if v1=255 then begin
        result := (result shl 4)+v0; // only one char left
        break;
      end;
      v0 := v0 shl 4;
      result := result shl 8;
      inc(v0,v1);
      inc(p);
      inc(result,v0);
    until false;
  end;
end;

{$ifdef DELPHI5OROLDER}
function Utf8ToAnsi(const UTF8: SockString): SockString;
begin
  result := UTF8; // no conversion
end;
{$endif}

const
  ENGLISH_LANGID = $0409;
  // see http://msdn.microsoft.com/en-us/library/windows/desktop/aa383770
  ERROR_WINHTTP_CANNOT_CONNECT = 12029;
  ERROR_WINHTTP_TIMEOUT = 12002;
  ERROR_WINHTTP_INVALID_SERVER_RESPONSE = 12152;


function SysErrorMessagePerModule(Code: DWORD; ModuleName: PChar): string;
{$ifdef MSWINDOWS}
var tmpLen: DWORD;
    err: PChar;
{$endif}
begin
  if Code=NO_ERROR then begin
    result := '';
    exit;
  end;
  {$ifdef MSWINDOWS}
  tmpLen := FormatMessage(
    FORMAT_MESSAGE_FROM_HMODULE or FORMAT_MESSAGE_ALLOCATE_BUFFER,
    pointer(GetModuleHandle(ModuleName)),Code,ENGLISH_LANGID,@err,0,nil);
  try
    while (tmpLen>0) and (ord(err[tmpLen-1]) in [0..32,ord('.')]) do
      dec(tmpLen);
    SetString(result,err,tmpLen);
  finally
    LocalFree(HLOCAL(err));
  end;
  {$endif}
  if result='' then begin
    result := SysErrorMessage(Code);
    if result='' then
      if Code=ERROR_WINHTTP_CANNOT_CONNECT then
        result := 'cannot connect' else
      if Code=ERROR_WINHTTP_TIMEOUT then
        result := 'timeout' else
      if Code=ERROR_WINHTTP_INVALID_SERVER_RESPONSE then
        result := 'invalid server response' else
        result := IntToHex(Code,8);
  end;
end;

procedure RaiseLastModuleError(ModuleName: PChar; ModuleException: ExceptClass);
var LastError: Integer;
    Error: Exception;
begin
  LastError := GetLastError;
  if LastError<>NO_ERROR then
    Error := ModuleException.CreateFmt('%s error %d (%s)',
      [ModuleName,LastError,SysErrorMessagePerModule(LastError,ModuleName)]) else
    Error := ModuleException.CreateFmt('Undefined %s error',[ModuleName]);
  raise Error;
end;

const
  HexChars: array[0..15] of AnsiChar = '0123456789ABCDEF';

procedure BinToHexDisplay(Bin: PByte; BinBytes: integer; var result: shortstring);
var j: cardinal;
begin
  result[0] := AnsiChar(BinBytes*2);
  for j := BinBytes-1 downto 0 do begin
    result[j*2+1] := HexChars[Bin^ shr 4];
    result[j*2+2] := HexChars[Bin^ and $F];
    inc(Bin);
  end;
end;

procedure BinToHexDisplayW(Bin: PByte; BinBytes: integer; var result: SynUnicode);
var j: cardinal;
    P: PWideChar;
begin
  SetString(Result,nil,BinBytes*2);
  P := pointer(Result);
  for j := BinBytes-1 downto 0 do begin
    P[j*2] := WideChar(HexChars[Bin^ shr 4]);
    P[j*2+1] := WideChar(HexChars[Bin^ and $F]);
    inc(Bin);
  end;
end;

function Ansi7ToUnicode(const Ansi: SockString): SockString;
var n, i: integer;
begin  // fast ANSI 7 bit conversion
  if Ansi='' then
    result := '' else begin
    n := length(Ansi);
    SetLength(result,n*2+1);
    for i := 0 to n do // to n = including last #0
      PWordArray(pointer(result))^[i] := PByteArray(pointer(Ansi))^[i];
  end;
end;

function DefaultUserAgent(Instance: TObject): SockString;
begin
  // note: some part of mORMot.pas would identify 'mORMot' pattern in the
  // agent header to enable advanced behavior e.g. about JSON transmission
  result := 'Mozilla/5.0 ('+XPOWEREDOS+'; '+XPOWEREDPROGRAM+' '+
    SockString(Instance.ClassName)+')';
end;

/// decode 'CONTENT-ENCODING: ' parameter from registered compression list
function ComputeContentEncoding(const Compress: THttpSocketCompressRecDynArray;
  P: PAnsiChar): THttpSocketCompressSet;
var i: integer;
    aName: SockString;
    Beg: PAnsiChar;
begin
  integer(result) := 0;
  if P<>nil then
    repeat
      while P^ in [' ',','] do inc(P);
      Beg := P; // 'gzip;q=1.0, deflate' -> aName='gzip' then 'deflate'
      while not (P^ in [';',',',#0]) do inc(P);
      SetString(aName,Beg,P-Beg);
      for i := 0 to high(Compress) do
        if aName=Compress[i].Name then
          include(result,i);
      while not (P^ in [',',#0]) do inc(P);
    until P^=#0;
end;

function RegisterCompressFunc(var Compress: THttpSocketCompressRecDynArray;
  aFunction: THttpSocketCompress; var aAcceptEncoding: SockString;
  aCompressMinSize: integer): SockString;
var i, n: integer;
    dummy, aName: SockString;
begin
  result := '';
  if @aFunction=nil then
    exit;
  n := length(Compress);
  aName := aFunction(dummy,true);
  for i := 0 to n-1 do
    with Compress[i] do
      if Name=aName then begin // already set
        if @Func=@aFunction then // update min. compress size value
          CompressMinSize := aCompressMinSize;
        exit;
      end;
  if n=sizeof(integer)*8 then
    exit; // fCompressHeader is 0..31 (casted as integer)
  SetLength(Compress,n+1);
  with Compress[n] do begin
    Name := aName;
    @Func := @aFunction;
    CompressMinSize := aCompressMinSize;
  end;
  if aAcceptEncoding='' then
    aAcceptEncoding := 'Accept-Encoding: '+aName else
    aAcceptEncoding := aAcceptEncoding+','+aName;
  result := aName;
end;

function CompressDataAndGetHeaders(Accepted: THttpSocketCompressSet;
  var Handled: THttpSocketCompressRecDynArray; const OutContentType: SockString;
  var OutContent: SockString): SockString;
var i, OutContentLen: integer;
    compressible: boolean;
    OutContentTypeP: PAnsiChar absolute OutContentType;
begin
  if (integer(Accepted)<>0) and (OutContentType<>'') and (Handled<>nil) then begin
    OutContentLen := length(OutContent);
    if IdemPChar(OutContentTypeP,'TEXT/') then
      compressible := true else
    if IdemPChar(OutContentTypeP,'IMAGE/') then
      compressible := IdemPCharArray(OutContentTypeP+6,['SVG','X-ICO'])>=0 else
    if IdemPChar(OutContentTypeP,'APPLICATION/') then 
      compressible := IdemPCharArray(OutContentTypeP+12,['JSON','XML','JAVASCRIPT'])>=0 else
      compressible := false;
    for i := 0 to high(Handled) do
    if i in Accepted then
    with Handled[i] do
    if (CompressMinSize=0) or // 0 here means "always" (e.g. for encryption)
       (compressible and (OutContentLen>=CompressMinSize)) then begin
      // compression of the OutContent + update header
      result := Func(OutContent,true);
      exit; // first in fCompress[] is prefered
    end;
  end;
  result := '';
end;

procedure IP4Text(addr: TInAddr; var result: SockString);
var b: array[0..3] of byte absolute addr;
begin
  if cardinal(addr)=0 then
    result := '' else
  if cardinal(addr)=$0100007f then
    result := '127.0.0.1' else
    result := SockString(Format('%d.%d.%d.%d',[b[0],b[1],b[2],b[3]]))
end;

{$ifdef MSWINDOWS}

function GetRemoteMacAddress(const IP: SockString): SockString;
// implements http://msdn.microsoft.com/en-us/library/aa366358
type
  TSendARP = function(DestIp: DWORD; srcIP: DWORD; pMacAddr: pointer; PhyAddrLen: Pointer): DWORD; stdcall;
var dwRemoteIP: DWORD;
    PhyAddrLen: Longword;
    pMacAddr: array [0..7] of byte;
    I: integer;
    P: PAnsiChar;
    SendARPLibHandle: THandle;
    SendARP: TSendARP;
begin
  result := '';
  SendARPLibHandle := LoadLibrary('iphlpapi.dll');
  if SendARPLibHandle<>0 then
  try
    SendARP := TSendARP(GetProcAddress(SendARPLibHandle,'SendARP'));
    if @SendARP=nil then
      exit; // we are not under 2K or later
    dwremoteIP := inet_addr(pointer(IP));
    if dwremoteIP<>0 then begin
      PhyAddrLen := 8;
      if SendARP(dwremoteIP, 0, @pMacAddr, @PhyAddrLen)=NO_ERROR then begin
        if PhyAddrLen=6 then begin
          SetLength(result,12);
          P := pointer(result);
          for i := 0 to 5 do begin
            P[0] := HexChars[pMacAddr[i] shr 4];
            P[1] := HexChars[pMacAddr[i] and $F];
            inc(P,2);
          end;
        end;
      end;
    end;
  finally
    FreeLibrary(SendARPLibHandle);
  end;
end;

type
  PMIB_IPADDRTABLE = ^MIB_IPADDRTABLE;
  MIB_IPADDRTABLE = record
    dwNumEntries: DWORD;
    ip: array[0..200] of record
      dwAddr: DWORD;
      dwIndex: DWORD;
      dwMask: DWORD;
      dwBCastAddr: DWORD;
      dwReasmSize: DWORD;
      unused1: Word;
      wType: Word;
    end;
  end;

function GetIpAddrTable(pIpAddrTable: PMIB_IPADDRTABLE;
  var pdwSize: DWORD; bOrder: BOOL): DWORD; stdcall; external 'iphlpapi.dll';

function GetIPAddresses: TSockStringDynArray;
var Table: MIB_IPADDRTABLE;
    Size: DWORD;
    i: integer;
    n: cardinal;
begin
  result := nil;
  Size := SizeOf(Table);
  if GetIpAddrTable(@Table,Size,false)<>NO_ERROR then
    exit;
  SetLength(result,Table.dwNumEntries);
  n := 0;
  for i := 0 to Table.dwNumEntries-1 do
    with Table.ip[i] do
    if (dwAddr <> $0100007f) and (dwAddr <> 0) then begin
      IP4Text(TInAddr(dwAddr),result[n]);
      inc(n);
    end;
  if n<>Table.dwNumEntries then
    SetLength(result,n);
end;

var
  // should not change during process livetime
  IPAddressesText: SockString;
  
function GetIPAddressesText(const Sep: SockString = ' '): SockString;
var ip: TSockStringDynArray;
    i: integer;
begin
  result := IPAddressesText;
  if result<>'' then
    exit;
  ip := GetIPAddresses;
  if ip=nil then
    exit;
  result := ip[0];
  for i := 1 to high(ip) do
    result := result+Sep+ip[i];
  IPAddressesText := result;
end;

{$endif MSWINDOWS}

{$ifndef NOXPOWEREDNAME}
const
  XPOWEREDNAME = 'X-Powered-By';
  XPOWEREDVALUE = XPOWEREDPROGRAM + ' synopse.info';
{$endif}

{ TURI }

const
  DEFAULT_PORT: array[boolean] of SockString = ('80','443');

function TURI.From(aURI: SockString): boolean;
var P,S: PAnsiChar;
begin
  Https := false;
  Finalize(self);
  result := false;
  aURI := Trim(aURI);
  if aURI='' then
    exit;
  P := pointer(aURI);
  if IdemPChar(P,'HTTP://') then
    inc(P,7) else
  if IdemPChar(P,'HTTPS://') then begin
    inc(P,8);
    Https := true;
  end;
  S := P;
  while not (S^ in [#0,':','/']) do inc(S);
  SetString(Server,P,S-P);
  if S^=':' then begin
    inc(S);
    P := S;
    while not (S^ in [#0,'/']) do inc(S);
    SetString(Port,P,S-P);
  end else
    Port := DEFAULT_PORT[Https];
  if S^<>#0 then // ':' or '/'
    inc(S);
  Address := S;
  if Server<>'' then
    result := true;
end;

function TURI.URI: SockString;
const Prefix: array[boolean] of SockString = ('http://','https://');
begin
  if (Port='') or (Port='0') or (Port=DEFAULT_PORT[Https]) then
    result := Prefix[Https]+Server+'/'+Address else
    result := Prefix[Https]+Server+':'+Port+'/'+Address;
end;

function TURI.PortInt: integer;
var err: integer;
begin
  Val(string(Port),result,err);
  if err<>0 then
    result := 0;
end;


{ ************ Socket API access - TCrtSocket and THttp*Socket }

var
  WsaDataOnce: TWSADATA;
  SO_TRUE: integer = ord(true);

function ResolveName(const Name: SockString;
  Family, SockProtocol, SockType: integer): SockString;
var l: TStringList;
begin
  l := TStringList.Create;
  try
    ResolveNameToIP(Name, Family, SockProtocol, SockType, l);
    if l.Count=0 then
      result := Name else
      result := SockString(l[0]);
  finally
    l.Free;
  end;
end;

procedure SetInt32Option(Sock: TSocket; OptName, OptVal: integer);
var li: TLinger;
    {$ifndef MSWINDOWS}
    timeval: TTimeval;
    {$endif}
begin
  if Sock<=0 then
    raise ECrtSocket.CreateFmt('Unexpected SetOption(%d,%d)',[OptName,OptVal]);
  case OptName of
  SO_SNDTIMEO, SO_RCVTIMEO: begin
    {$ifndef MSWINDOWS} // POSIX expects a timeval parameter for time out values
    timeval.tv_sec := OptVal div 1000;
    timeval.tv_usec := (OptVal mod 1000)*1000;
    if SetSockOpt(Sock,SOL_SOCKET,OptName,@timeval,sizeof(timeval))=0 then
    {$else}             // WinAPI expects the time out directly as ms integer
    if SetSockOpt(Sock,SOL_SOCKET,OptName,pointer(@OptVal),sizeof(OptVal))=0 then
    {$endif}
      exit;
  end;
  SO_KEEPALIVE: // boolean (0/1) value
    if SetSockOpt(Sock,SOL_SOCKET,OptName,pointer(@OptVal),sizeof(OptVal))=0 then
      exit;
  SO_LINGER: begin // not available on UDP
    if OptVal<0 then
      li.l_onoff := Ord(false) else begin
      li.l_onoff := Ord(true);
      li.l_linger := OptVal;
    end;
    SetSockOpt(Sock,SOL_SOCKET, SO_LINGER, @li, SizeOf(li));
    if OptVal>0 then begin
      {$ifdef LINUX}
      SetSockOpt(Sock,SOL_SOCKET, SO_REUSEADDR,@SO_TRUE,SizeOf(SO_TRUE));
      {$endif}
      {$ifdef BSD}
      SetSockOpt(Sock,SOL_SOCKET,SO_NOSIGPIPE,@SO_TRUE,SizeOf(SO_TRUE));
      {$endif}
    end;
    exit;
  end;
  TCP_NODELAY: // boolean (0/1) value
    if SetSockOpt(Sock,IPPROTO_TCP,OptName,@OptVal,sizeof(OptVal))=0 then
      exit;
  end;
  raise ECrtSocket.CreateFmt('Error %d for SetOption(%d,%d)',
    [WSAGetLastError,OptName,OptVal]);
end;

function CallServer(const Server, Port: SockString; doBind: boolean;
   aLayer: TCrtSocketLayer; ConnectTimeout: DWORD): TSocket;
var Sin: TVarSin;
    IP: SockString;
    SOCK_TYPE, IPPROTO: integer;
    {$ifndef MSWINDOWS}
    serveraddr: sockaddr;
    {$endif}
begin
  result := -1;
  case aLayer of
    cslTCP: begin
      SOCK_TYPE := SOCK_STREAM;
      IPPROTO := IPPROTO_TCP;
    end;
    cslUDP: begin
      SOCK_TYPE := SOCK_DGRAM;
      IPPROTO := IPPROTO_UDP;
    end;
    cslUNIX: begin
      {$ifdef MSWINDOWS}
      exit; // not handled under Win32
      {$else} // special version for UNIX sockets
      result := socket(AF_UNIX,SOCK_STREAM,0);
      if result<0 then
        exit;
      if doBind then begin
        fillchar(serveraddr,sizeof(serveraddr),0);
        //http://publib.boulder.ibm.com/infocenter/iseries/v5r3/index.jsp?topic=/rzab6/rzab6uafunix.htm
        {$ifdef KYLIX3}
        if (Libc.bind(result,serveraddr,sizeof(serveraddr))<0) or
           (Libc.listen(result,SOMAXCONN)<0) then begin
        {$else}
        if (fpbind(result,@serveraddr,sizeof(serveraddr))<0) or
           (fplisten(result,SOMAXCONN)<0) then begin
        {$endif}
          result := -1;
        end;
      end;
      exit;
      {$endif}
    end;
    else exit; // make this stupid compiler happy
  end;
  {$ifndef MSWINDOWS}
  if (Server='') and not doBind then
    IP := cLocalHost else
  {$endif}
    IP := ResolveName(Server, AF_INET, IPPROTO, SOCK_TYPE);
  // use AF_INET instead of AF_UNSPEC: IP6 is buggy!
  if SetVarSin(Sin, IP, Port, AF_INET, IPPROTO, SOCK_TYPE, false)<>0 then
    exit;
  result := Socket(integer(Sin.AddressFamily), SOCK_TYPE, IPPROTO);
  if result=-1 then
    exit;
  if doBind then begin
    // Socket should remain open for 5 seconds after a closesocket() call
    SetInt32Option(result, SO_LINGER, 5);
    // bind and listen to this port
    if (Bind(result, Sin)<>0) or
       ((aLayer<>cslUDP) and (Listen(result, SOMAXCONN)<>0)) then begin
      CloseSocket(result);
      result := -1;
    end;
  end else begin
    if ConnectTimeout>0 then begin
      SetInt32Option(result, SO_RCVTIMEO, ConnectTimeout);
      SetInt32Option(result, SO_SNDTIMEO, ConnectTimeout);
    end;
    if Connect(result,Sin)<>0 then begin
       CloseSocket(result);
       result := -1;
    end;
  end;
end;

type
  PCrtSocket = ^TCrtSocket;

function OutputSock(var F: TTextRec): integer;
begin
  if F.BufPos=0 then
    result := 0 else
    if PCrtSocket(@F.UserData)^.TrySndLow(F.BufPtr,F.BufPos) then begin
      F.BufPos := 0;
      result := 0;
    end else
      result := -1; // on socket error -> raise ioresult error
end;

function InputSock(var F: TTextRec): Integer;
// SockIn pseudo text file fill its internal buffer only with available data
// -> no unwanted wait time is added
// -> very optimized use for readln() in HTTP stream
var Size: integer;
    Sock: TCRTSocket;
    {$ifdef MSWINDOWS}
    iSize: integer;
    {$else}
    sin: TVarSin;
    {$endif}
begin
  F.BufEnd := 0;
  F.BufPos := 0;
  result := -1; // on socket error -> raise ioresult error
  Sock := PCrtSocket(@F.UserData)^;
  if (Sock=nil) or (Sock.Sock=-1) then
    exit; // file closed = no socket -> error
  if Sock.TimeOut<>0 then begin // will wait for pending data?
    IOCtlSocket(Sock.Sock, FIONREAD, Size); // get exact count
    if (Size<=0) or (Size>integer(F.BufSize)) then
      Size := F.BufSize;
  end else
    Size := F.BufSize;
  case Sock.SocketLayer of
  cslTCP:
    Size := Recv(Sock.Sock, F.BufPtr, Size, 0
      {$ifndef MSWINDOWS}{$ifdef FPC_OR_KYLIX},Sock.TimeOut{$endif}{$endif});
  else begin
    {$ifdef MSWINDOWS}
    iSize := SizeOf(TSockAddr);
    Size := RecvFrom(Sock.Sock, F.BufPtr, Size, 0, @Sock.fPeerAddr, @iSize);
    {$else}
    Size := RecvFrom(Sock.Sock, F.BufPtr, Size, 0, sin);
    Sock.fPeerAddr.sin_port := Sin.sin_port;
    Sock.fPeerAddr.sin_addr := Sin.sin_addr;
    {$endif}
  end;
  end;
  // Recv() may return Size=0 if no data is pending, but no TCP/IP error
  if Size>=0 then begin
    F.BufEnd := Size;
    inc(Sock.fBytesIn,Size);
    result := 0; // no error
  end else begin
    Sock.fSockInEof := true; // error -> mark end of SockIn
    result := -WSAGetLastError();
    // result <0 will update ioresult and raise an exception if {$I+}
  end;
end;

function CloseSock(var F: TTextRec): integer;
begin
  if PCrtSocket(@F.UserData)^<>nil then
    PCrtSocket(@F.UserData)^.Close;
  PCrtSocket(@F.UserData)^ := nil;
  Result := 0;
end;

function OpenSock(var F: TTextRec): integer;
begin
  F.BufPos := 0;
  F.BufEnd := 0;
  if F.Mode=fmInput then begin // ReadLn
    F.InOutFunc := @InputSock;
    F.FlushFunc := nil;
  end else begin               // WriteLn
    F.Mode := fmOutput;
    F.InOutFunc := @OutputSock;
    F.FlushFunc := @OutputSock;
  end;
  F.CloseFunc := @CloseSock;
  Result := 0;
end;


{ TCrtSocket }

function Split(const Text: SockString; Sep: AnsiChar; var Before,After: SockString): boolean;
var i: integer;
begin
  for i := length(Text)-1 downto 2 do
    if Text[i]=Sep then begin
      Before := trim(copy(Text,1,i-1));
      After := trim(copy(Text,i+1,maxInt));
      result := true;
      exit;
    end;
  result := false;
end;

constructor TCrtSocket.Bind(const aPort: SockString; aLayer: TCrtSocketLayer=cslTCP);
var s,p: SockString;
begin
  // on Linux, Accept() blocks even after Shutdown() -> use 0.5 second timeout
  Create({$ifdef LINUX}500{$else}5000{$endif});
  if not Split(aPort,':',s,p) then begin
    s := '0.0.0.0';
    p := aPort;
  end;
  OpenBind(s,p,true,-1,aLayer); // raise an ECrtSocket exception on error
end;

constructor TCrtSocket.Open(const aServer, aPort: SockString; aLayer: TCrtSocketLayer;
  aTimeOut: cardinal);
begin
  Create(aTimeOut); // default read timeout is 10 seconds
  OpenBind(aServer,aPort,false,-1,aLayer); // raise an ECrtSocket exception on error
end;

type
  PTextRec = ^TTextRec;

procedure TCrtSocket.Close;
begin
  fSndBufLen := 0; // always reset (e.g. in case of further Open)
  if (SockIn<>nil) or (SockOut<>nil) then begin
    ioresult; // reset ioresult value if SockIn/SockOut were used
    if SockIn<>nil then begin
      PTextRec(SockIn)^.BufPos := 0;  // reset input buffer
      PTextRec(SockIn)^.BufEnd := 0;
    end;
    if SockOut<>nil then begin
      PTextRec(SockIn)^.BufPos := 0; // reset output buffer
      PTextRec(SockIn)^.BufEnd := 0;
    end;
  end;
  if Sock=-1 then
    exit; // no opened connection to close
  Shutdown(Sock,SHUT_WR);
  CloseSocket(Sock); // SO_LINGER usually set to 5 or 10 seconds
  fSock := -1; // don't change Server or Port, since may try to reconnect
end;

constructor TCrtSocket.Create(aTimeOut: cardinal);
begin
  fTimeOut := aTimeOut;
end;

procedure TCrtSocket.SetInt32OptionByIndex(OptName, OptVal: integer);
begin
  SetInt32Option(Sock,OptName,OptVal);
end;

procedure TCrtSocket.OpenBind(const aServer, aPort: SockString;
  doBind: boolean; aSock: integer=-1; aLayer: TCrtSocketLayer=cslTCP);
const BINDTXT: array[boolean] of string = ('open','bind');
begin
  fSocketLayer := aLayer;
  if aSock<0 then begin
    if aPort='' then
      fPort := '80' else // default port is 80 (HTTP)
      fPort := aPort;
    fServer := aServer;
    fSock := CallServer(aServer,Port,doBind,aLayer,Timeout); // OPEN or BIND
    if fSock<0 then
      raise ECrtSocket.CreateFmt('Socket %s creation error on %s:%s (%d)',
        [BINDTXT[doBind],aServer,Port,WSAGetLastError]);
  end else
    fSock := aSock; // ACCEPT mode -> socket is already created by caller
  if TimeOut>0 then begin // set timout values for both directions
    ReceiveTimeout := TimeOut;           
    SendTimeout := TimeOut;
  end;
  if aLayer = cslTCP then begin
    TCPNoDelay := 1; // disable Nagle algorithm since we use our own buffers
    KeepAlive := 1; // enable TCP keepalive (even if we rely on transport layer)
  end;
end;

procedure TCrtSocket.SockSend(const Values: array of const);
var i: integer;
    tmp: shortstring;
begin
  for i := 0 to high(Values) do
  with Values[i] do
  case VType of
    vtString:
      SockSend(@VString^[1],pByte(VString)^);
    vtAnsiString:
      SockSend(VAnsiString,length(SockString(VAnsiString)));
    {$ifdef HASVARUSTRING}
    vtUnicodeString: begin
      tmp := ShortString(UnicodeString(VUnicodeString)); // convert into ansi
      SockSend(@tmp[1],length(tmp));
    end;
    {$endif}
    vtPChar:
      SockSend(VPChar,StrLen(VPChar));
    vtChar:
      SockSend(@VChar,1);
    vtWideChar:
      SockSend(@VWideChar,1); // only ansi part of the character
    vtInteger: begin
      Str(VInteger,tmp);
      SockSend(@tmp[1],length(tmp));
    end;
    vtInt64: begin
      Str(VInt64^,tmp);
      SockSend(@tmp[1],length(tmp));
    end;
  end;
  SockSend(@CRLF,2);
end;

procedure TCrtSocket.SockSend(const Line: SockString);
begin
  if Line<>'' then
    SockSend(pointer(Line),length(Line));
  SockSend(@CRLF,2);
end;

procedure TCrtSocket.SockSendFlush;
begin
  if fSndBufLen=0 then
    exit;
  SndLow(pointer(fSndBuf),fSndBufLen);
  fSndBufLen := 0;
end;

procedure TCrtSocket.SndLow(P: pointer; Len: integer);
begin
  if not TrySndLow(P,Len) then
    raise ECrtSocket.Create('SndLow');
end;

function TCrtSocket.TrySndLow(P: pointer; Len: integer): boolean;
var SentLen: integer;
begin
  result := false;
  if (self=nil) or (Sock=-1) or (Len<0) or (P=nil) then
    exit;
  repeat
    SentLen := Send(Sock, P, Len, MSG_NOSIGNAL
      {$ifndef MSWINDOWS}{$ifdef FPC_OR_KYLIX},TimeOut{$endif}{$endif});
    if SentLen<0 then
      exit;
    dec(Len,SentLen);
    inc(fBytesOut,SentLen);
    if Len<=0 then break;
    inc(PtrUInt(P),SentLen);
  until false;
  result := true;
end;

procedure TCrtSocket.Write(const Data: SockString);
begin
  SndLow(pointer(Data),length(Data));
end;

function TCrtSocket.SockInRead(Content: PAnsiChar; Length: integer;
  UseOnlySockIn: boolean): integer;
var len,res: integer;
// read Length bytes from SockIn^ buffer + Sock if necessary
begin
  // get data from SockIn buffer, if any (faster than ReadChar)
  result := 0;
  if Length=0 then
    exit;
  if SockIn<>nil then
    with PTextRec(SockIn)^ do
    repeat
      len := BufEnd-BufPos;
      if len>0 then begin
        if len>Length then
          len := Length;
        move(BufPtr[BufPos],Content^,len);
        inc(BufPos,len);
        inc(Content,len);
        dec(Length,len);
        inc(result,len);
      end;
      if Length=0 then
        exit; // we got everything we wanted
      if not UseOnlySockIn then
        break;
      res := InputSock(PTextRec(SockIn)^);
      if res<0 then
        raise ECrtSocket.CreateFmt('SockInRead InputSock=%d',[res]);
    until Timeout=0;
  // direct receiving of the remaining bytes from socket
  if Length>0 then begin
    SockRecv(Content,Length);
    inc(result,Length);
  end;
end;

function TCrtSocket.SockInPending(aTimeOut: integer): integer;
var backup: cardinal;
begin
  if SockIn=nil then
    raise ECrtSocket.Create('SockInPending without SockIn');
  with PTextRec(SockIn)^ do begin
    result := BufEnd-BufPos;
    if result=0 then
      // no data in SockIn^.Buffer, but some at socket level -> retrieve now
      case SockReceivePending(aTimeOut) of
      cspDataAvailable: begin
        backup := TimeOut;
        fTimeOut := 0;
        if InputSock(PTextRec(SockIn)^)=NO_ERROR then
          result := BufEnd-BufPos else
          result := -1; // indicates broken socket
        fTimeOut := backup;
      end;
      cspSocketError:
        result := -1; // indicates broken socket
      end; // cspNoData will leave result=0
  end;
end;

destructor TCrtSocket.Destroy;
begin
  Close;
  if SockIn<>nil then
    Freemem(SockIn);
  if SockOut<>nil then
    Freemem(SockOut);
  inherited;
end;

procedure TCrtSocket.SockSend(P: pointer; Len: integer);
var cap: integer;
begin
  if Len<=0 then
    exit;
  cap := Length(fSndBuf);
  if Len+fSndBufLen>cap then
    SetLength(fSndBuf,len+cap+cap shr 3+2048);
  move(P^,PAnsiChar(pointer(fSndBuf))[fSndBufLen],Len);
  inc(fSndBufLen,Len);
end;

const
  SOCKMINBUFSIZE = 1024; // big enough for headers (content will be read directly)

{$ifdef FPC}
procedure SetLineBreakStyle(var T: Text; Style: TTextLineBreakStyle);
begin
  case Style Of
    tlbsLF: TextRec(T).LineEnd := #10;
    tlbsCRLF: TextRec(T).LineEnd := #13#10;
    tlbsCR: TextRec(T).LineEnd := #13;
  end;
end;
{$endif FPC}

procedure TCrtSocket.CreateSockIn(LineBreak: TTextLineBreakStyle;
  InputBufferSize: Integer);
begin
  if (Self=nil) or (SockIn<>nil) then
    exit; // initialization already occured
  if InputBufferSize<SOCKMINBUFSIZE then
    InputBufferSize := SOCKMINBUFSIZE;
  GetMem(fSockIn,sizeof(TTextRec)+InputBufferSize);
  fillchar(SockIn^,sizeof(TTextRec),0);
  with TTextRec(SockIn^) do begin
    PCrtSocket(@UserData)^ := self;
    Mode := fmClosed;
    BufSize := InputBufferSize;
    BufPtr := pointer(PAnsiChar(SockIn)+sizeof(TTextRec)); // ignore Buffer[] (Delphi 2009+)
    OpenFunc := @OpenSock;
    Handle := -1;
  end;
  {$ifndef DELPHI5OROLDER}
  SetLineBreakStyle(SockIn^,LineBreak); // http does break lines with #13#10
  {$endif}
  Reset(SockIn^);
end;

procedure TCrtSocket.CreateSockOut(OutputBufferSize: Integer);
begin
  if SockOut<>nil then
    exit; // initialization already occured
  if OutputBufferSize<SOCKMINBUFSIZE then
    OutputBufferSize := SOCKMINBUFSIZE;
  GetMem(fSockOut,sizeof(TTextRec)+OutputBufferSize);
  fillchar(SockOut^,sizeof(TTextRec),0);
  with TTextRec(SockOut^) do begin
    PCrtSocket(@UserData)^ := self;
    Mode := fmClosed;
    BufSize := OutputBufferSize;
    BufPtr := pointer(PAnsiChar(SockIn)+sizeof(TTextRec)); // ignore Buffer[] (Delphi 2009+)
    OpenFunc := @OpenSock;
    Handle := -1;
  end;
  {$ifndef DELPHI5OROLDER}
  SetLineBreakStyle(SockOut^,tlbsCRLF); // force e.g. for Linux platforms
  {$endif}
  Rewrite(SockOut^);
end;

procedure TCrtSocket.SockRecv(Buffer: pointer; Length: integer);
begin
  if not TrySockRecv(Buffer,Length) then
    raise ECrtSocket.Create('SockRecv');
end;

function TCrtSocket.TrySockRecv(Buffer: pointer; Length: integer): boolean;
var Size: PtrInt;
    endtime: cardinal;
begin
  result := false;
  if self=nil then
    exit;
  if (Buffer<>nil) and (Length>0) then begin
    endtime := GetTickCount+TimeOut;
    repeat                                         
      Size := Recv(Sock, Buffer, Length, MSG_NOSIGNAL
        {$ifndef MSWINDOWS}{$ifdef FPC_OR_KYLIX},TimeOut{$endif}{$endif});
      if Size<=0 then begin
        if Size=0 then
          Close; // socket closed gracefully (otherwise SOCKET_ERROR=-1) 
        exit;
      end;
      inc(fBytesIn,Size);
      dec(Length,Size);
      inc(PByte(Buffer),Size);
      if Length=0 then
        break;
      if GetTickCount>endtime then
        exit; // identify read time out as error
    until false;
  end;
  result := true;
end;

function TCrtSocket.SockReceivePending(TimeOut: cardinal): TCrtSocketPending;
var tv: TTimeVal;
    fdset: TFDSet;
    res: integer;
begin
  {$ifdef MSWINDOWS}
  fdset.fd_array[0] := Sock;
  fdset.fd_count := 1;
  {$else}
  FD_ZERO(fdset);
  FD_SET(sock,fdset);
  {$endif}
  tv.tv_usec := TimeOut*1000;
  tv.tv_sec := 0;
  res := Select(Sock+1,@fdset,nil,nil,@tv);
  if res=0 then
    result := cspNoData else
  if res>0 then
    result := cspDataAvailable else
    {$ifdef LINUX}
    if (WSAGetLastError=WSATRY_AGAIN) or (WSAGetLastError=WSAEWOULDBLOCK) then
      result := cspNoData else
    {$endif}
      result := cspSocketError;
end;

function TCrtSocket.LastLowSocketError: Integer;
begin
  result := WSAGetLastError; // retrieved directly from Sockets API
end;

procedure TCrtSocket.SockRecvLn(out Line: SockString; CROnly: boolean=false);
procedure RecvLn(var Line: SockString);
var P: PAnsiChar;
    LP, L: PtrInt;
    tmp: array[0..1023] of AnsiChar; // avoid ReallocMem() every char
begin
  P := @tmp;
  Line := '';
  repeat
    SockRecv(P,1); // this is very slow under Windows -> use SockIn^ instead
    if P^<>#13 then // at least NCSA 1.3 does send a #10 only -> ignore #13
      if P^=#10 then begin
        if Line='' then // get line
          SetString(Line,tmp,P-tmp) else begin
          LP := P-tmp; // append to already read chars
          L := length(Line);
          Setlength(Line,L+LP);
          move(tmp,(PAnsiChar(pointer(Line))+L)^,LP);
        end;
        exit;
      end else
      if P=@tmp[1023] then begin // tmp[] buffer full?
        L := length(Line); // -> append to already read chars
        Setlength(Line,L+1024);
        move(tmp,(PAnsiChar(pointer(Line))+L)^,1024);
        P := tmp;
      end else
        inc(P);
  until false;
end;
var c: AnsiChar;
   Error: integer;
begin
  if CROnly then begin // slow but accurate version which expect #13 as line end
    // SockIn^ expect either #10, either #13#10 -> a dedicated version is needed
    repeat
      SockRecv(@c,1); // this is slow but works
      if c in [#0,#13] then
        exit; // end of line
      Line := Line+c; // will do the work anyway
    until false;
  end else
  if SockIn<>nil then begin
    {$I-}
    readln(SockIn^,Line); // example: HTTP/1.0 200 OK
    Error := ioresult;
    if Error<>0 then
      raise ECrtSocket.Create('SockRecvLn',Error);
    {$I+}
  end else
    RecvLn(Line); // slow under Windows -> use SockIn^ instead
end;

procedure TCrtSocket.SockRecvLn;
var c: AnsiChar;
    Error: integer;
begin
  if SockIn<>nil then begin
    {$I-}
    readln(SockIn^);
    Error := ioresult;
    if Error<>0 then
      raise ECrtSocket.Create('SockRecvLn',Error);
    {$I+}
  end else
    repeat
      SockRecv(@c,1);
    until c=#10;
end;

function TCrtSocket.SockConnected: boolean;
var Sin: TVarSin;
begin
  result := GetPeerName(Sock,Sin)=0;
end;

function TCrtSocket.PeerAddress: SockString;
begin
  IP4Text(fPeerAddr.sin_addr,result);
end;

function TCrtSocket.PeerPort: integer;
begin
  result := fPeerAddr.sin_port;
end;

{$ifdef MSWINDOWS}
procedure SleepHiRes(ms: cardinal);
begin
  {$ifndef FPC} // function SwitchToThread oddly not defined in fpc\rtl\win
  if (ms<>0) or not SwitchToThread then
  {$endif}
    Windows.Sleep(ms);
end;
{$endif}

function TCrtSocket.SockReceiveString: SockString;
var Size, L, Read: integer;
begin
  result := '';
  if self=nil then
    exit;
  L := 0;
  repeat
    SleepHiRes(0);
    if IOCtlSocket(Sock, FIONREAD, Size)<>0 then // get exact count
      exit;
    if Size=0 then // connection broken
      if result='' then begin // wait till something
        SleepHiRes(10); // 10 ms delay in infinite loop
        continue;
      end else
        break;
    SetLength(result,L+Size); // append to result
    Read := recv(Sock,PAnsiChar(pointer(result))+L,Size,MSG_NOSIGNAL
      {$ifndef MSWINDOWS}{$ifdef FPC_OR_KYLIX},TimeOut{$endif}{$endif});
    if Read<0 then
      exit;
    inc(L,Read);
    if Read<Size then
      SetLength(result,L); // e.g. Read=0 may happen
  until false;
end;


{ THttpClientSocket }

constructor THttpClientSocket.Create(aTimeOut: cardinal);
begin
  if aTimeOut=0 then
    aTimeOut := HTTP_DEFAULT_RECEIVETIMEOUT;
  inherited Create(aTimeOut);
  UserAgent := DefaultUserAgent(self);
end;

function THttpClientSocket.Delete(const url: SockString; KeepAlive: cardinal;
  const header: SockString): integer;
begin
  result := Request(url,'DELETE',KeepAlive,header,'','',false);
end;

function THttpClientSocket.Get(const url: SockString; KeepAlive: cardinal=0; const header: SockString=''): integer;
begin
  result := Request(url,'GET',KeepAlive,header,'','',false);
end;

function THttpClientSocket.GetAuth(const url, AuthToken: SockString; KeepAlive: cardinal=0): integer;
begin
  if AuthToken='' then
    result := Get(url,KeepAlive) else
    result := Get(url,KeepAlive,'Authorization: Bearer '+AuthToken);
end;

function THttpClientSocket.Head(const url: SockString; KeepAlive: cardinal;
  const header: SockString): integer;
begin
  result := Request(url,'HEAD',KeepAlive,header,'','',false);
end;

function THttpClientSocket.Post(const url, Data, DataType: SockString; KeepAlive: cardinal;
  const header: SockString): integer;
begin
  result := Request(url,'POST',KeepAlive,header,Data,DataType,false);
end;

function THttpClientSocket.Put(const url, Data, DataType: SockString;
  KeepAlive: cardinal; const header: SockString): integer;
begin
  result := Request(url,'PUT',KeepAlive,header,Data,DataType,false);
end;

procedure THttpClientSocket.RequestSendHeader(const url, method: SockString);
var aURL: SockString;
begin
  if Sock<0 then
    exit;
  if SockIn=nil then // done once
    CreateSockIn; // use SockIn by default if not already initialized: 2x faster
  if TCPPrefix<>'' then
    SockSend(TCPPrefix);
  if (url='') or (url[1]<>'/') then
    aURL := '/'+url else // need valid url according to the HTTP/1.1 RFC
    aURL := url;
  SockSend([method,' ',aURL,' HTTP/1.1']);
  if Port='80' then
    SockSend(['Host: ',Server]) else
    SockSend(['Host: ',Server,':',Port]);
  SockSend(['Accept: */*'#13#10'User-Agent: ',UserAgent]);
end;

function THttpClientSocket.Request(const url, method: SockString;
  KeepAlive: cardinal; const Header, Data, DataType: SockString; retry: boolean): integer;
procedure DoRetry(Error: integer);
begin
  if retry then // retry once -> return error only if failed after retrial
    result := Error else begin
    Close; // close this connection
    try
      OpenBind(Server,Port,false); // then retry this request with a new socket
      result := Request(url,method,KeepAlive,Header,Data,DataType,true);
    except
      on Exception do
        result := Error;
    end;
  end;
end;
var P: PAnsiChar;
    aData: SockString;
begin
  if SockIn=nil then // done once
    CreateSockIn; // use SockIn by default if not already initialized: 2x faster
  Content := '';
  if Sock<0 then
    DoRetry(STATUS_NOTFOUND) else // socket closed (e.g. KeepAlive=0) -> reconnect
  try
  try
    // send request - we use SockSend because writeln() is calling flush()
    // -> all header will be sent at once
    RequestSendHeader(url,method);
    if KeepAlive>0 then
      SockSend(['Keep-Alive: ',KeepAlive,#13#10'Connection: Keep-Alive']) else
      SockSend('Connection: Close');
    aData := Data; // need var for Data to be eventually compressed
    CompressDataAndWriteHeaders(DataType,aData);
    if header<>'' then
      SockSend(header);
    if fCompressAcceptEncoding<>'' then
      SockSend(fCompressAcceptEncoding);
    SockSend; // send CRLF
    SockSendFlush; // flush all pending data (i.e. headers) to network
    if aData<>'' then // for POST and PUT methods: content to be sent
      SndLow(pointer(aData),length(aData)); // no CRLF at the end of data
    // get headers
    SockRecvLn(Command); // will raise ECrtSocket on any error
    if TCPPrefix<>'' then
      if Command<>TCPPrefix then begin
        result :=  505;
        exit;
      end else
      SockRecvLn(Command);
    P := pointer(Command);
    if IdemPChar(P,'HTTP/1.') then begin
      result := GetCardinal(P+9); // get http numeric status code
      if result=0 then begin
        result :=  505;
        exit;
      end;
      while result=100 do begin
        repeat // 100 CONTINUE will just be ignored client side
          SockRecvLn(Command);
          P := pointer(Command);
        until IdemPChar(P,'HTTP/1.');  // ignore up to next command
        result := GetCardinal(P+9);
      end;
      if P[7]='0' then
        KeepAlive := 0; // HTTP/1.0 -> force connection close
    end else begin // error on reading answer
      DoRetry(505); // 505=wrong format
      exit;
    end;
    GetHeader; // read all other headers
    if not IdemPChar(pointer(method),'HEAD') then
      GetBody; // get content if necessary (not HEAD method)
{$ifdef DEBUGAPI}writeln('? ',Command,' ContentLength=',length(Content));
    if result<>STATUS_SUCCESS then writeln('? ',Content,#13#10,HeaderGetText); {$endif}
  except
    on Exception do
      DoRetry(STATUS_NOTFOUND);
  end;
  finally
    if KeepAlive=0 then
      Close;
  end;
end;

function Open(const aServer, aPort: SockString): TCrtSocket;
begin
  try
    result := TCrtSocket.Open(aServer,aPort);
  except
    on ECrtSocket do
      result := nil;
  end;
end;

function OpenHttp(const aServer, aPort: SockString): THttpClientSocket;
begin
  try
    result := THttpClientSocket.Open(aServer,aPort,cslTCP,0); // HTTP_DEFAULT_RECEIVETIMEOUT
  except
    on ECrtSocket do
      result := nil;
  end;
end;

function OpenHttp(const aURI: SockString; aAddress: PSockString): THttpClientSocket;
var URI: TURI;
begin
  result := nil;
  if URI.From(aURI) then begin
    if not URI.Https then
      result := OpenHttp(URI.Server,URI.Port);
    if aAddress <> nil then
      aAddress^ := URI.Address;
  end;
end;

function HttpGet(const server, port: SockString; const url: SockString;
  const inHeaders: SockString; outHeaders: PSockString): SockString;
var Http: THttpClientSocket;
begin
  result := '';
  Http := OpenHttp(server,port);
  if Http<>nil then
  try
    if Http.Get(url,0,inHeaders)=STATUS_SUCCESS then begin
      result := Http.Content;
      if outHeaders<>nil then
        outHeaders^ := Http.HeaderGetText;
    end;
  finally
    Http.Free;
  end;
end;

function HttpGet(const aURI: SockString; outHeaders: PSockString): SockString;
begin
  result := HttpGet(aURI,'',outHeaders);
end;

function HttpGet(const aURI: SockString; const inHeaders: SockString; outHeaders: PSockString): SockString;
var URI: TURI;
begin
  if URI.From(aURI) then
    if URI.Https then
      {$ifdef MSWINDOWS}
      result := TWinHTTP.Get(aURI,inHeaders,true,outHeaders) else
      {$else}
      {$ifdef USELIBCURL}
      result := TCurlHTTP.Get(aURI,inHeaders,true,outHeaders) else
      {$else}
      raise ECrtSocket.CreateFmt('https is not supported by HttpGet(%s)',[aURI]) else
      {$endif}
      {$endif}
      result := HttpGet(URI.Server,URI.Port,URI.Address,inHeaders,outHeaders) else
    result := '';
  {$ifdef LINUX}
  if result='' then
    writeln('HttpGet returned VOID for ',URI.server,':',URI.Port,' ',URI.Address);
  {$endif}
end;

function HttpPost(const server, port: SockString; const url, Data, DataType: SockString): boolean;
var Http: THttpClientSocket;
begin
  result := false;
  Http := OpenHttp(server,port);
  if Http<>nil then
  try
    result := Http.Post(url,Data,DataType) in
      [STATUS_SUCCESS,STATUS_CREATED,STATUS_NOCONTENT];
  finally
    Http.Free;
  end;
end;

function TSMTPConnection.FromText(const aText: SockString): boolean;
var u,h: SockString;
begin
  if aText=SMTP_DEFAULT then begin
    result := false;
    exit;
  end;
  if Split(aText,'@',u,h) then begin
    if not Split(u,':',User,Pass) then
      User := u;
  end else
    h := aText;
  if not Split(h,':',Host,Port) then begin
    Host := h;
    Port := '25';
  end;
  if (Host<>'') and (Host[1]='?') then
    Host := '';
  result := Host<>'';
end;

function SendEmail(const Server: TSMTPConnection;
  const From, CSVDest, Subject, Text, Headers, TextCharSet: SockString): boolean;
begin
  result := SendEmail(Server.Host, From, CSVDest, Subject, Text, Headers,
    Server.User, Server.Pass, Server.Port, TextCharSet);
end;

function SendEmail(const Server, From, CSVDest, Subject, Text, Headers,
  User, Pass, Port, TextCharSet: SockString): boolean;
var TCP: TCrtSocket;
procedure Expect(const Answer: SockString);
var Res: SockString;
begin
  repeat
    readln(TCP.SockIn^,Res);
  until (Length(Res)<4)or(Res[4]<>'-');
  if not IdemPChar(pointer(Res),pointer(Answer)) then
    raise ECrtSocket.Create(string(Res));
end;
procedure Exec(const Command, Answer: SockString);
begin
  writeln(TCP.SockOut^,Command);
  Expect(Answer)
end;
var P: PAnsiChar;
    rec, ToList, head: SockString;
begin
  result := false;
  P := pointer(CSVDest);
  if P=nil then exit;
  TCP := Open(Server, Port);
  if TCP<>nil then
  try
    TCP.CreateSockIn; // we use SockIn and SockOut here
    TCP.CreateSockOut;
    Expect('220');
    if (User<>'') and (Pass<>'') then begin
      Exec('EHLO '+Server,'25');
      Exec('AUTH LOGIN','334');
      Exec(Base64Encode(User),'334');
      Exec(Base64Encode(Pass),'235');
    end else
      Exec('HELO '+Server,'25');
    writeln(TCP.SockOut^,'MAIL FROM:<',From,'>'); Expect('250');
    repeat
      rec := trim(GetNextItem(P));
      if rec='' then continue;
      if pos({$ifdef HASCODEPAGE}SockString{$endif}('<'),rec)=0 then
        rec := '<'+rec+'>';
      Exec('RCPT TO:'+rec,'25');
      if ToList='' then
        ToList := #13#10'To: '+rec else
        ToList := ToList+', '+rec;
    until P=nil;
    Exec('DATA','354');
    head := trim(Headers);
    if head<>'' then
      head := head+#13#10;
    writeln(TCP.SockOut^,'Subject: ',Subject,#13#10'From: ',From,
      ToList,#13#10'Content-Type: text/plain; charset=',TextCharSet,
      #13#10'Content-Transfer-Encoding: 8bit'#13#10,head,#13#10,Text);
    Exec('.','25');
    writeln(TCP.SockOut^,'QUIT');
    result := true;
  finally
    TCP.Free;
  end;
end;

function IsAnsi7(const s: string): boolean;
var i: integer;
begin
  result := false;
  for i := 1 to length(s) do
    if ord(s[i])>126 then
      exit;
  result := true;
end;

function SendEmailSubject(const Text: string): SockString;
var utf8: UTF8String;
begin
  if IsAnsi7(Text) then
    result := SockString(Text) else begin
    utf8 := UTF8String(Text);
    result := '=?UTF-8?B?'+Base64Encode(utf8);
  end;
end;


{ THttpServerRequest }

constructor THttpServerRequest.Create(aServer: THttpServerGeneric;
  aConnectionID: Int64; aConnectionThread: TSynThread);
begin
  inherited Create;
  fServer := aServer;
  fConnectionID := aConnectionID;
  fConnectionThread := aConnectionThread;
end;

procedure THttpServerRequest.Prepare(const aURL, aMethod,
  aInHeaders, aInContent, aInContentType, aRemoteIP: SockString);
begin
  fURL := aURL;
  fMethod := aMethod;
  if aRemoteIP<>'' then
    if aInHeaders='' then
      fInHeaders := 'RemoteIP: '+aRemoteIP else
      fInHeaders := aInHeaders+#13#10'RemoteIP: '+aRemoteIP else
    fInHeaders := aInHeaders;
  fInContent := aInContent;
  fInContentType := aInContentType;
  fOutContent := '';
  fOutContentType := '';
  fOutCustomHeaders := '';
  fUseSSL := false;
end;

procedure THttpServerRequest.AddInHeader(additionalHeader: SockString);
begin
  additionalHeader := Trim(additionalHeader);
  if additionalHeader<>'' then
    if fInHeaders='' then
      fInHeaders := additionalHeader else
      fInHeaders := fInHeaders+#13#10+additionalHeader;
end;


{ THttpServerGeneric }

constructor THttpServerGeneric.Create(CreateSuspended: Boolean;
  OnStart,OnStop: TNotifyThreadEvent; const ProcessName: SockString);
begin
  fProcessName := ProcessName;
  SetServerName('mORMot ('+XPOWEREDOS+')');
  fOnHttpThreadStart := OnStart;
  SetOnTerminate(OnStop);
  inherited Create(CreateSuspended);
end;

procedure THttpServerGeneric.RegisterCompress(aFunction: THttpSocketCompress;
  aCompressMinSize: integer=1024);
begin
  RegisterCompressFunc(fCompress,aFunction,fCompressAcceptEncoding,aCompressMinSize);
end;

function THttpServerGeneric.Request(Ctxt: THttpServerRequest): cardinal;
begin
  NotifyThreadStart(Ctxt.ConnectionThread);
  if Assigned(OnRequest) then
    result := OnRequest(Ctxt) else
    result := STATUS_NOTFOUND;
end;

function THttpServerGeneric.Callback(Ctxt: THttpServerRequest; aNonBlocking: boolean): cardinal;
begin
  raise ECrtSocket.CreateFmt('%s.Callback is not implemented: try to use '+
    'another communication protocol, e.g. WebSockets',[ClassName]);
end;

procedure THttpServerGeneric.NotifyThreadStart(Sender: TSynThread);
begin
  if Sender=nil then
    raise ECrtSocket.Create('NotifyThreadStart(nil)');
  if Assigned(fOnHttpThreadStart) and not Assigned(Sender.fStartNotified) then begin
    fOnHttpThreadStart(Sender);
    Sender.fStartNotified := self;
  end;
end;

procedure THttpServerGeneric.SetOnTerminate(const Event: TNotifyThreadEvent);
begin
  fOnTerminate := Event;
end;

procedure THttpServerGeneric.SetServerName(const aName: SockString);
begin
  fServerName := aName;
end;

function THttpServerGeneric.NextConnectionID: integer;
begin
  result := InterlockedIncrement(fCurrentConnectionID);
end;


{ THttpServer }

constructor THttpServer.Create(const aPort: SockString; OnStart,OnStop: TNotifyThreadEvent;
  const ProcessName: SockString {$ifdef USETHREADPOOL}; ServerThreadPoolCount: integer{$endif});
begin
  InitializeCriticalSection(fProcessCS);
  fSock := TCrtSocket.Bind(aPort); // BIND + LISTEN
  ServerKeepAliveTimeOut := 3000; // HTTP.1/1 KeepAlive is 3 seconds by default
  fInternalHttpServerRespList := TList.Create;
  if fThreadRespClass=nil then
    fThreadRespClass := THttpServerResp;
  {$ifdef USETHREADPOOL}
  if ServerThreadPoolCount>0 then begin
    fThreadPool := TSynThreadPoolTHttpServer.Create(self,ServerThreadPoolCount);
    fThreadPoolPush := fThreadPool.Push;
  end;
  {$endif}
  inherited Create(false,OnStart,OnStop,ProcessName);
end;

function THttpServer.GetAPIVersion: string;
begin
  result := Format('%s.%d',[WsaDataOnce.szDescription,WsaDataOnce.wVersion]);
end;

destructor THttpServer.Destroy;
var StartTick, StopTick: Cardinal;
    i: integer;
begin
  Terminate; // set Terminated := true for THttpServerResp.Execute
  StartTick := GetTickCount;
  StopTick := StartTick+20000;
  EnterCriticalSection(fProcessCS);
  if fInternalHttpServerRespList<>nil then begin
    for i := 0 to fInternalHttpServerRespList.Count-1 do
      THttpServerResp(fInternalHttpServerRespList.List[i]).Terminate;
    repeat // wait for all THttpServerResp.Execute to be finished
      if fInternalHttpServerRespList.Count=0 then
        break;
      LeaveCriticalSection(fProcessCS);
      SleepHiRes(100);
      EnterCriticalSection(fProcessCS);
    until (GetTickCount>StopTick) or (GetTickCount<StartTick);
    FreeAndNil(fInternalHttpServerRespList);
  end;
  LeaveCriticalSection(fProcessCS);
  fThreadPoolPush := nil;
  {$ifdef USETHREADPOOL}
  FreeAndNil(fThreadPool); // release all associated threads and I/O completion
  {$endif}
  {$ifdef LINUX}
  {$ifdef FPC}
  KillThread(ThreadID);  // manualy do it here
  {$endif}
  {$endif}
  FreeAndNil(fSock);
  inherited Destroy;     // direct Thread abort, no wait till ended
  DeleteCriticalSection(fProcessCS);
end;

{$ifndef MSWINDOWS}
  {.$define MONOTHREAD}
  // define this not to create a thread at every connection (not recommended)
{$endif}

procedure THttpServer.Execute;
var ClientSock: TSocket;
    ClientSin: TVarSin;
{$ifdef MONOTHREAD}
    ClientCrtSock: THttpServerSocket;
{$endif}
    i: integer;
label abort;
begin
  // THttpServerGeneric thread preparation: launch any OnHttpThreadStart event
  NotifyThreadStart(self);
  // main server process loop
  if Sock.Sock>0 then
  try
    while not Terminated do begin
      ClientSock := Accept(Sock.Sock,ClientSin);
      if ClientSock<0 then
        if Terminated then
          break else begin
        SleepHiRes(0);
        continue;
      end;
      if Terminated or (Sock=nil) then begin
abort:  Shutdown(ClientSock,1);
        CloseSocket(ClientSock);
        break; // don't accept input if server is down
      end;
      OnConnect;
      {$ifdef MONOTHREAD}
      ClientCrtSock := THttpServerSocket.Create(self);
      try
        ClientCrtSock.InitRequest(ClientSock);
        if ClientCrtSock.GetRequest then
          Process(ClientCrtSock,self);
        OnDisconnect;
        Shutdown(ClientSock,1);
        CloseSocket(ClientSock)
      finally
        ClientCrtSock.Free;
      end;
      {$else}
      if Assigned(fThreadPoolPush) then begin
        if not fThreadPoolPush(ClientSock) then begin
          for i := 1 to 1500 do begin
            inc(fThreadPoolContentionCount);
            SleepHiRes(20); // wait a little until a thread is available
            if Terminated then
              break;
            if fThreadPoolPush(ClientSock) then
              exit; // the thread pool acquired the client sock
          end;
          inc(fThreadPoolContentionAbortCount);
          goto abort; // 1500*20 = 30 seconds timeout
        end
      end else
        // default implementation creates one thread for each incoming socket
        fThreadRespClass.Create(ClientSock, self);
      {$endif MONOTHREAD}
      end;
  except
    on Exception do
      ; // any exception would break and release the thread
  end;
end;

procedure THttpServer.OnConnect;
begin
  inc(fServerConnectionCount);
end;

procedure THttpServer.OnDisconnect;
begin
  // nothing to do by default
end;

procedure THttpServer.Process(ClientSock: THttpServerSocket;
  ConnectionID: integer; ConnectionThread: TSynThread);
var Context: THttpServerRequest;
    P: PAnsiChar;
    Code: cardinal;
    s: SockString;
    staticfn, ErrorMsg: string;
    FileToSend: TFileStream;
begin
  if (ClientSock=nil) or (ClientSock.Headers=nil) then
    // we didn't get the request = socket read error
    exit; // -> send will probably fail -> nothing to send back
  if Terminated then
    exit;
  Context := THttpServerRequest.Create(self,ConnectionID,ConnectionThread);
  try
    // calc answer
    with ClientSock do
      Context.Prepare(URL,Method,HeaderGetText,Content,ContentType,'');
    try
      Code := Request(Context);
    except
      on E: Exception do begin
        ErrorMsg := E.ClassName+': '+E.Message;
        Code := STATUS_SERVERERROR;
      end;
    end;
    if Terminated then
      exit;
    // handle case of direct sending of static file (as with http.sys)
    if (Context.OutContent<>'') and (Context.OutContentType=HTTP_RESP_STATICFILE) then
      try
        staticfn := {$ifdef UNICODE}UTF8ToUnicodeString{$else}Utf8ToAnsi{$endif}(Context.OutContent);
        FileToSend := TFileStream.Create(staticfn,fmOpenRead or fmShareDenyNone);
        try
          SetString(Context.fOutContent,nil,FileToSend.Size);
          FileToSend.Read(Pointer(Context.fOutContent)^,length(Context.fOutContent));
          Context.OutContentType := GetHeaderValue(Context.fOutCustomHeaders,'CONTENT-TYPE:',true);
       finally
          FileToSend.Free;
        end;
      except
        on E: Exception do begin // error reading or sending file
         ErrorMsg := E.ClassName+': '+E.Message;
         Code := STATUS_NOTFOUND;
        end;
      end;
    if Context.OutContentType=HTTP_RESP_NORESPONSE then
      Context.OutContentType := ''; // true HTTP always expects a response
    // send response (multi-thread OK) at once
    if (Code<STATUS_SUCCESS) or (ClientSock.Headers=nil) then
      Code := STATUS_NOTFOUND;
    if ErrorMsg<>'' then begin
      Context.OutCustomHeaders := '';
      Context.OutContentType := 'text/html; charset=utf-8'; // create message to display
      Context.OutContent := {$ifdef UNICODE}UTF8String{$else}UTF8Encode{$endif}(
        format('<body style="font-family:verdana">'#13+
        '<h1>%s Server Error %d</h1><hr><p>HTTP %d %s<p>%s<p><small>%s',
        [ClassName,Code,Code,StatusCodeToReason(Code),HtmlEncodeString(ErrorMsg),fServerName]));
    end;
    // 1. send HTTP status command
    if ClientSock.TCPPrefix<>'' then
      ClientSock.SockSend(ClientSock.TCPPrefix);
    if ClientSock.KeepAliveClient then
      ClientSock.SockSend(['HTTP/1.1 ',Code,' OK']) else
      ClientSock.SockSend(['HTTP/1.0 ',Code,' OK']);
    // 2. send headers
    // 2.1. custom headers from Request() method
    P := pointer(Context.fOutCustomHeaders);
    while P<>nil do begin
      s := GetNextLine(P);
      if s<>'' then begin // no void line (means headers ending)
        ClientSock.SockSend(s);
        if IdemPChar(pointer(s),'CONTENT-ENCODING:') then
          integer(ClientSock.fCompressHeader) := 0; // custom encoding: don't compress
      end;
    end;
    // 2.2. generic headers
    ClientSock.SockSend([
      {$ifndef NOXPOWEREDNAME}XPOWEREDNAME+': '+XPOWEREDVALUE+#13#10+{$endif}
      'Server: ',fServerName]);
    ClientSock.CompressDataAndWriteHeaders(Context.OutContentType,Context.fOutContent);
    if ClientSock.KeepAliveClient then begin
      if ClientSock.fCompressAcceptEncoding<>'' then
        ClientSock.SockSend(ClientSock.fCompressAcceptEncoding);
      ClientSock.SockSend('Connection: Keep-Alive'#13#10); // #13#10 -> end headers
    end else
      ClientSock.SockSend; // headers must end with a void line
    ClientSock.SockSendFlush; // flush all pending data (i.e. headers) to network
    // 3. sent HTTP body content (if any)
    if Context.OutContent<>'' then
      // direct send to socket (no CRLF at the end of data)
      ClientSock.SndLow(pointer(Context.OutContent),length(Context.OutContent));
  finally
    if Sock<>nil then begin // add transfert stats to main socket
      EnterCriticalSection(fProcessCS);
      inc(Sock.fBytesIn,ClientSock.BytesIn);
      inc(Sock.fBytesOut,ClientSock.BytesOut);
      LeaveCriticalSection(fProcessCS);
      ClientSock.fBytesIn := 0;
      ClientSock.fBytesOut := 0;
    end;
    Context.Free;
  end;
end;


{ TSynThread }

constructor TSynThread.Create(CreateSuspended: Boolean);
begin
  {$ifdef FPC}
  inherited Create(CreateSuspended,512*1024); // DefaultSizeStack=512KB
  {$else}
  inherited Create(CreateSuspended);
  {$endif}
end;

function TSynThread.SleepOrTerminated(MS: cardinal): boolean;
var endtix, tix, lasttix: cardinal;
begin
  result := true; // notify Terminated
  if Terminated then
    exit;
  if MS<32 then begin // smaller than GetTickCount resolution (under Windows)
    sleep(MS);
    if Terminated then
      exit;
  end else begin
    tix := GetTickCount;
    endtix := tix+MS;
    repeat
      sleep(10);
      if Terminated then
        exit;
      lasttix := tix; // handle GetTickCount 32-bit overflow
      tix := GetTickCount;
    until (tix>endtix) or (tix<lasttix);
  end;
  result := false; // normal delay expiration
end;

{$ifdef FPC}
destructor TSynThread.Destroy;
begin
  if not Terminated then begin
    Terminate;
    WaitFor;
  end;
  inherited Destroy;
end;
{$endif}

{$ifndef LVCL}
procedure TSynThread.DoTerminate;
begin
  if Assigned(fStartNotified) and Assigned(fOnTerminate) then begin
    fOnTerminate(self);
    fStartNotified := nil;
  end;
  inherited DoTerminate;
end;
{$endif}

{$ifndef HASTTHREADSTART}
procedure TSynThread.Start;
begin
  Resume;
end;
{$endif}


{ THttpServerResp }

constructor THttpServerResp.Create(aSock: TSocket; aServer: THttpServer);
begin
  fClientSock := aSock; // ensure it is set ASAP: on Linux, Execute raises immediately
  Create(THttpServerSocket.Create(aServer),aServer{$ifdef USETHREADPOOL},nil{$endif});
end;

constructor THttpServerResp.Create(aServerSock: THttpServerSocket;
  aServer: THttpServer{$ifdef USETHREADPOOL}; aThreadPool: TSynThreadPoolTHttpServer{$endif});
begin
  fServer := aServer;
  fServerSock := aServerSock;
  {$ifdef USETHREADPOOL}
  fThreadPool := aThreadPool;
  {$endif}
  fOnTerminate := fServer.fOnTerminate;
  EnterCriticalSection(fServer.fProcessCS);
  try
    fServer.fInternalHttpServerRespList.Add(self);
  finally
    LeaveCriticalSection(fServer.fProcessCS);
  end;
  fConnectionID := fServer.NextConnectionID;
  FreeOnTerminate := true;
  inherited Create(false);
end;

procedure THttpServerResp.Execute;

  procedure HandleRequestsProcess;
  var StartTick, StopTick, Tick: cardinal;
      pending: TCrtSocketPending;
  begin
    {$ifdef USETHREADPOOL}
    if fThreadPool<>nil then
      InterlockedIncrement(fThreadPool.FGeneratedThreadCount);
    {$endif}
    try
    try
      repeat
        StartTick := GetTickCount;
        StopTick := StartTick+fServer.ServerKeepAliveTimeOut;
        repeat // within this loop, break=wait for next command, exit=quit
          if (fServer=nil) or fServer.Terminated or (fServerSock=nil) then
            exit; // server is down -> close connection
          pending := fServerSock.SockReceivePending(50); // 50 ms timeout
          if (fServer=nil) or fServer.Terminated then
            exit; // server is down -> disconnect the client
          case pending of
          cspSocketError:
            exit; // socket error -> disconnect the client
          cspNoData: begin
            Tick := GetTickCount;  // wait for keep alive timeout
            if Tick<StartTick then // time wrap after continuous run for 49.7 days
              break; // reset Ticks count + retry
            if Tick>=StopTick then
              exit; // reached time out -> close connection
          end;
          cspDataAvailable: begin
            // get request and headers
            if not fServerSock.GetRequest(True) then
              // fServerSock connection was down or headers are not correct
              exit;
            // calc answer and send response
            fServer.Process(fServerSock,ConnectionID,self);
            // keep connection only if necessary
            if fServerSock.KeepAliveClient then
              break else
              exit;
          end;
          end;
         until false;
      until false;
    finally
      {$ifdef USETHREADPOOL}
      if fThreadPool<>nil then
        InterlockedDecrement(fThreadPool.FGeneratedThreadCount);
      {$endif}
    end;
    except
      on E: Exception do
        ; // any exception will silently disconnect the client
    end;
  end;

var aSock: TSocket;
    i: integer;
begin
  fServer.NotifyThreadStart(self);
  try
    try
      if fClientSock<>0 then begin
        // direct call from incoming socket
        aSock := fClientSock;
        fClientSock := 0; // mark no need to Shutdown and close fClientSock
        fServerSock.InitRequest(aSock); // now fClientSock is in fServerSock
        if fServer<>nil then
          HandleRequestsProcess;
      end else begin
        // call from TSynThreadPoolTHttpServer -> handle first request
        if not fServerSock.fBodyRetrieved then
          fServerSock.GetBody;
        fServer.Process(fServerSock,ConnectionID,self);
        if (fServer<>nil) and fServerSock.KeepAliveClient then
          HandleRequestsProcess; // process further kept alive requests
      end;
    finally
      try
        if fServer<>nil then
        try
          EnterCriticalSection(fServer.fProcessCS);
          fServer.OnDisconnect;
          if (fServer.fInternalHttpServerRespList<>nil) then begin
            i := fServer.fInternalHttpServerRespList.IndexOf(self);
            if i>=0 then
              fServer.fInternalHttpServerRespList.Delete(i);
          end;
        finally
          LeaveCriticalSection(fServer.fProcessCS);
          fServer := nil;
        end;
      finally
        FreeAndNil(fServerSock);
        if fClientSock<>0 then begin
          // if Destroy happens before fServerSock.GetRequest() in Execute below
          Shutdown(fClientSock,1);
          CloseSocket(fClientSock);
        end;
      end;
    end;
  except
    on Exception do
      ; // just ignore unexpected exceptions here, especially during clean-up
  end;
end;


{ THttpSocket }

procedure THttpSocket.GetBody;
var Line: SockString; // 32 bits chunk length in hexa
    LinePChar: array[0..31] of AnsiChar;
    Len, LContent, Error: integer;
begin
  fBodyRetrieved := true;
{$ifdef DEBUG23}system.writeln('GetBody ContentLength=',ContentLength);{$endif}
  Content := '';
  {$I-}
  // direct read bytes, as indicated by Content-Length or Chunked
  if Chunked then begin // we ignore the Length
    LContent := 0; // current read position in Content
    repeat
      if SockIn<>nil then begin
        readln(SockIn^,LinePChar);      // use of a static PChar is faster
        Error := ioresult;
        if Error<>0 then
          raise ECrtSocket.Create('GetBody1',Error);
        Len := PCharToHex32(LinePChar); // get chunk length in hexa
      end else begin
        SockRecvLn(Line);
        Len := PCharToHex32(pointer(Line)); // get chunk length in hexa
      end;
      if Len=0 then begin // ignore next line (normaly void)
        SockRecvLn;
        break;
      end;
      SetLength(Content,LContent+Len); // reserve memory space for this chunk
      SockInRead(pointer(PAnsiChar(pointer(Content))+LContent),Len) ; // append chunk data
      inc(LContent,Len);
      SockRecvLn; // ignore next #13#10
    until false;
  end else
  if ContentLength>0 then begin
    SetLength(Content,ContentLength); // not chuncked: direct read
    SockInRead(pointer(Content),ContentLength); // works with SockIn=nil or not
  end else
  if ContentLength<0 then begin // ContentLength=-1 if no Content-Length
    // no Content-Length nor Chunked header -> read until eof()
    if SockIn<>nil then
      while not eof(SockIn^) do begin
        readln(SockIn^,Line);
        if Content='' then
          Content := Line else
          Content := Content+#13#10+Line;
      end;
    ContentLength := length(Content); // update Content-Length
    exit;
  end;
  // optionaly uncompress content
  if cardinal(fContentCompress)<cardinal(length(fCompress)) then
    if fCompress[fContentCompress].Func(Content,false)='' then
      // invalid content
      raise ECrtSocket.CreateFmt('%s uncompress',[fCompress[fContentCompress].Name]);
  ContentLength := length(Content); // update Content-Length
  if SockIn<>nil then begin
    Error := ioresult;
    if Error<>0 then
      raise ECrtSocket.Create('GetBody2',Error);
  end;
  {$I+}
end;

procedure THttpSocket.GetHeader;
var s: SockString;
    i, n: integer;
    P: PAnsiChar;
begin
  fBodyRetrieved := false;
  ContentType := '';
  ContentLength := -1;
  fContentCompress := -1;
  ConnectionClose := false;
  ConnectionUpgrade := false;
  Chunked := false;
  n := 0;
  repeat
    SockRecvLn(s);
    if s='' then
      break; // headers end with a void line
    if length(Headers)<=n then
      SetLength(Headers,n+10);
    Headers[n] := s;
    inc(n);
    {$ifdef DEBUG23}system.Writeln(ClassName,'.HeaderIn ',s);{$endif}
    P := pointer(s);
    if IdemPChar(P,'CONTENT-') then begin
      if IdemPChar(P+8,'LENGTH:') then
        ContentLength := GetCardinal(pointer(PAnsiChar(pointer(s))+16)) else
      if IdemPChar(P+8,'TYPE:') then
        ContentType := trim(copy(s,14,128));
    end else
    if IdemPChar(P,'TRANSFER-ENCODING: CHUNKED') then
      Chunked := true else
    if IdemPChar(P,'CONNECTION: ') then
       if IdemPChar(P+12,'CLOSE') then
          ConnectionClose := true else
       if IdemPChar(P+12,'UPGRADE') or IdemPChar(P+12,'KEEP-ALIVE, UPGRADE') then
          ConnectionUpgrade := true;
    if fCompress<>nil then
      if IdemPChar(P,'ACCEPT-ENCODING:') then
        fCompressHeader := ComputeContentEncoding(fCompress,P+16) else
      if IdemPChar(P,'CONTENT-ENCODING: ') then begin
        i := 18;
        while s[i+1]=' ' do inc(i);
        delete(s,1,i);
        for i := 0 to high(fCompress) do
          if fCompress[i].Name=s then begin
            fContentCompress := i;
            break;
          end;
      end;
  until false;
  SetLength(Headers,n);
end;

function THttpSocket.HeaderAdd(const aValue: SockString): integer;
begin
  result := length(Headers);
  SetLength(Headers,result+1);
  Headers[result] := aValue;
end;

procedure THttpSocket.HeaderSetText(const aText, aForcedContentType: SockString);
var P, PDeb: PAnsiChar;
    n: integer;
begin
  P := pointer(aText);
  n := 0;
  if P<>nil then
    repeat
      PDeb := P;
      while P^>#13 do inc(P);
      if PDeb<>P then begin // add any not void line
        if length(Headers)<=n then
          SetLength(Headers,n+n shr 3+8);
        SetString(Headers[n],PDeb,P-PDeb);
        inc(n);
      end;
      while (P^=#13) or (P^=#10) do inc(P);
    until P^=#0;
  SetLength(Headers,n);
  if (aForcedContentType='') or (HeaderValue('Content-Type')<>'') then
    exit;
  SetLength(Headers,n+1);
  Headers[n] := 'Content-Type: '+aForcedContentType;
end;

function THttpSocket.HeaderGetText: SockString;
var i,L,n: integer;
    P: PAnsiChar;
begin // faster than for i := 0 to Count-1 do result := result+Headers[i]+#13#10
  result := '';
  n := length(Headers);
  if n=0 then
    exit;
  L := n*2; // #13#10 size
  dec(n);
  for i := 0 to n do
    inc(L,length(Headers[i]));
  SetLength(result,L);
  P := pointer(result);
  for i := 0 to n do begin
    L := length(Headers[i]);
    if L>0 then begin
      move(pointer(Headers[i])^,P^,L);
      inc(P,L);
    end;
    PWord(P)^ := 13+10 shl 8;
    inc(P,2);
  end;
end;

function THttpSocket.HeaderValue(aName: SockString): SockString;
var i: integer;
begin
  if Headers<>nil then begin
    aName := UpperCase(aName)+':';
    for i := 0 to high(Headers) do
      if IdemPChar(pointer(Headers[i]),pointer(aName)) then begin
        result := trim(copy(Headers[i],length(aName)+1,maxInt));
        exit;
      end;
  end;
  result := '';
end;

function THttpSocket.RegisterCompress(aFunction: THttpSocketCompress;
  aCompressMinSize: integer): boolean;
begin
  result := RegisterCompressFunc(fCompress,aFunction,fCompressAcceptEncoding,aCompressMinSize)<>'';
end;

procedure THttpSocket.CompressDataAndWriteHeaders(const OutContentType: SockString;
  var OutContent: SockString);
var OutContentEncoding: SockString;
begin
  if integer(fCompressHeader)<>0 then begin
    OutContentEncoding := CompressDataAndGetHeaders(fCompressHeader,fCompress,
      OutContentType,OutContent);
    if OutContentEncoding<>'' then
        SockSend(['Content-Encoding: ',OutContentEncoding]);
  end;
  SockSend(['Content-Length: ',length(OutContent)]); // needed even 0
  if (OutContent<>'') and (OutContentType<>'') then
    SockSend(['Content-Type: ',OutContentType]);
end;

procedure GetSinIPFromCache(const Sin: TVarSin; var result: SockString);
begin
  if Sin.sin_family=AF_INET then
    IP4Text(Sin.sin_addr,result) else begin
    result := GetSinIP(Sin); // AF_INET6 may be optimized in a future revision
    if result='::1' then
      result := '127.0.0.1'; // IP6 localhost loopback benefits of matching IP4
  end;
end;


{ THttpServerSocket }

constructor THttpServerSocket.Create(aServer: THttpServer);
begin
  inherited Create(5000);
  if aServer<>nil then begin
    fCompress := aServer.fCompress;
    fCompressAcceptEncoding := aServer.fCompressAcceptEncoding;
    TCPPrefix := aServer.TCPPrefix;
  end;
end;

procedure THttpServerSocket.InitRequest(aClientSock: TSocket);
var Name: TVarSin;
begin
  CreateSockIn; // use SockIn by default if not already initialized: 2x faster
  OpenBind('','',false,aClientSock); // set the ACCEPTed aClientSock
  Linger := 5; // should remain open for 5 seconds after a closesocket() call
  if GetPeerName(aClientSock,Name)=0 then
    GetSinIPFromCache(Name,RemoteIP);
end;

function THttpServerSocket.HeaderGetText: SockString;
begin
  result := inherited HeaderGetText;
  if RemoteIP<>'' then
    result := result+'RemoteIP: '+RemoteIP+#13#10;
end;

function THttpServerSocket.GetRequest(withBody: boolean=true): boolean;
var P: PAnsiChar;
    StartTix, EndTix: cardinal;
begin
  try
    StartTix := GetTickCount;
    // 1st line is command: 'GET /path HTTP/1.1' e.g.
    SockRecvLn(Command);
    if TCPPrefix<>'' then
      if TCPPrefix<>Command then begin
        result := false;
        exit
      end else
      SockRecvLn(Command);
    P := pointer(Command);
    Method := GetNextItem(P,' '); // 'GET'
    URL := GetNextItem(P,' ');    // '/path'
    KeepAliveClient := IdemPChar(P,'HTTP/1.1');
    Content := '';
    // get headers and content
    GetHeader;
    if ConnectionClose then
      KeepAliveClient := false;
    if (ContentLength<0) and KeepAliveClient then
      ContentLength := 0; // HTTP/1.1 and no content length -> no eof
    EndTix := GetTickCount;
    result := EndTix<StartTix+10000; // 10 sec for header -> DOS / TCP SYN Flood
    // if time wrap after 49.7 days -> EndTix<StartTix -> always accepted
    if result and withBody and not ConnectionUpgrade then
      GetBody;
  except
    on E: Exception do
      result := false; // mark error
  end;
end;


{ ECrtSocket }

function SocketErrorMessage(Error: integer): string;
begin
  case Error of
  WSAETIMEDOUT:    result := 'WSAETIMEDOUT';
  WSAENETDOWN:     result := 'WSAENETDOWN';
  WSAEWOULDBLOCK:  result := 'WSAEWOULDBLOCK';
  WSAECONNABORTED: result := 'WSAECONNABORTED';
  WSAECONNRESET:   result := 'WSAECONNRESET';
  else result := '';
  end;
  result := Format('%d %s [%s]',[Error,result,SysErrorMessage(Error)]);
end;

constructor ECrtSocket.Create(const Msg: string);
begin
  Create(Msg,WSAGetLastError());
end;

constructor ECrtSocket.Create(const Msg: string; Error: integer);
begin
  if Error=0 then
    fLastError := WSAEWOULDBLOCK else // if unknown, probably a timeout
    fLastError := abs(Error);
  inherited Create(Msg+' '+SocketErrorMessage(fLastError));
end;


{$ifdef USETHREADPOOL}

{ TSynThreadPool }

const
  // Posted to the completion port when shutting down
  SHUTDOWN_FLAG = POverlapped(-1);

constructor TSynThreadPool.Create(NumberOfThreads: Integer);
var i: integer;
    Thread: TSynThreadPoolSubThread;
begin
  if NumberOfThreads=0 then
    NumberOfThreads := 1 else
  if cardinal(NumberOfThreads)>cardinal(length(FThreadID)) then
    NumberOfThreads := length(FThreadID); // maximum count for WaitForMultipleObjects()
  // Create IO completion port to queue the HTTP requests
  FRequestQueue := CreateIoCompletionPort(INVALID_HANDLE_VALUE, 0, 0, NumberOfThreads);
  if FRequestQueue=INVALID_HANDLE_VALUE then begin
    FRequestQueue := 0;
    exit;
  end;
  // Now create the worker threads
  FThread := TObjectList.Create;
  for i := 0 to NumberOfThreads-1 do begin
    Thread := TSynThreadPoolSubThread.Create(Self);
    FThread.Add(Thread);
    FThreadID[i] := Thread.ThreadID;
  end;
  FGeneratedThreadCount := NumberOfThreads;
end;

destructor TSynThreadPool.Destroy;
var i: integer;
begin
  if FRequestQueue<>0 then begin
    // Tell the threads we're shutting down
    for i := 1 to fThread.Count do
      PostQueuedCompletionStatus(FRequestQueue, 0, 0, SHUTDOWN_FLAG);
    // Wait for threads to finish, with 30 seconds TimeOut
    WaitForMultipleObjects(FThread.Count,@FThreadID,True,30000);
    // Close the request queue handle
    CloseHandle(FRequestQueue);
    FRequestQueue := 0;
  end;
  FreeAndNil(fThread);
end;

{ TSynThreadPoolSubThread }

const
  // if HTTP body length is bigger than 1 MB, creates a dedicated THttpServerResp
  THREADPOOL_BIGBODYSIZE = 1024*1024;

  // kept-alive or big HTTP requests will create a dedicated THttpServerResp
  // - each thread reserves 2 MB of memory so it may break the server
  // - keep the value to a decent number, to let resources be constrained
  THREADPOOL_MAXCREATEDTHREADS = 100;

constructor TSynThreadPoolSubThread.Create(Owner: TSynThreadPool);
begin
  fOwner := Owner; // ensure it is set ASAP: on Linux, Execute raises immediately
  fOnTerminate := Owner.FOnHttpThreadTerminate;
  inherited Create(false);
end;

function GetQueuedCompletionStatus(CompletionPort: THandle;
  var lpNumberOfBytesTransferred: pointer; var lpCompletionKey: PtrUInt;
  var lpOverlapped: POverlapped; dwMilliseconds: DWORD): BOOL; stdcall;
  external kernel32; // redefine with an unique signature for all Delphi/FPC

procedure TSynThreadPoolSubThread.Execute;
var Context: pointer;
    Key: PtrUInt;
    Overlapped: POverlapped;
begin
  if fOwner=nil then
    exit;
  while GetQueuedCompletionStatus(fOwner.FRequestQueue,Context,Key,OverLapped,INFINITE) do
  try
    if OverLapped=SHUTDOWN_FLAG then
      break; // exit thread
    if Context<>nil then
      fOwner.Task(Self,Context);
  except
    on Exception do
      ; // we should handle all exceptions in this loop
  end;
end;


{ TSynThreadPoolTHttpServer }

constructor TSynThreadPoolTHttpServer.Create(Server: THttpServer; NumberOfThreads: Integer=32);
begin
  inherited Create(NumberOfThreads);
  fServer := Server;
end;

function TSynThreadPoolTHttpServer.Push(aClientSock: TSocket): boolean;
begin
  result := false;
  if (Self=nil) or (FRequestQueue=0) then
    exit;
  result := PostQueuedCompletionStatus(FRequestQueue,PtrUInt(aClientSock),0,nil);
end;

procedure TSynThreadPoolTHttpServer.Task(aCaller: TSynThreadPoolSubThread;
  aContext: Pointer);
var ServerSock: THttpServerSocket;
begin
  ServerSock := THttpServerSocket.Create(fServer);
  try
    ServerSock.InitRequest(TSocket(aContext));
    // get Header of incoming request
    if ServerSock.GetRequest(false) then
      // connection and header seem valid -> process request further
      if (FGeneratedThreadCount<THREADPOOL_MAXCREATEDTHREADS) and
         (ServerSock.KeepAliveClient or
          (ServerSock.ContentLength>THREADPOOL_BIGBODYSIZE)) then begin
        // HTTP/1.1 Keep Alive -> process in background thread
        // or posted data > 1 MB -> get Body in background thread
        fServer.fThreadRespClass.Create(ServerSock,fServer,self);
        ServerSock := nil; // THttpServerResp will do ServerSock.Free
      end else begin
        // no Keep Alive = multi-connection -> process in the Thread Pool
        ServerSock.GetBody; // we need to get it now
        fServer.Process(ServerSock,0,aCaller); // multi-connection -> ID=0
        fServer.OnDisconnect;
        // no Shutdown here: will be done client-side
      end;
  finally
    FreeAndNil(ServerSock);
  end;
end;

{$endif USETHREADPOOL}

{$ifdef MSWINDOWS}

{ ************  http.sys / HTTP API low-level direct access }

{$MINENUMSIZE 4}
{$A+}

{$ifdef FPC}
{$PACKRECORDS C}
{$endif}

type
  // HTTP version used
  HTTP_VERSION = packed record
    MajorVersion: word;
    MinorVersion: word;
  end;

  // the req* values identify Request Headers, and resp* Response Headers
  THttpHeader = (
    reqCacheControl,
    reqConnection,
    reqDate,
    reqKeepAlive,
    reqPragma,
    reqTrailer,
    reqTransferEncoding,
    reqUpgrade,
    reqVia,
    reqWarning,
    reqAllow,
    reqContentLength,
    reqContentType,
    reqContentEncoding,
    reqContentLanguage,
    reqContentLocation,
    reqContentMd5,
    reqContentRange,
    reqExpires,
    reqLastModified,
    reqAccept,
    reqAcceptCharset,
    reqAcceptEncoding,
    reqAcceptLanguage,
    reqAuthorization,
    reqCookie,
    reqExpect,
    reqFrom,
    reqHost,
    reqIfMatch,
    reqIfModifiedSince,
    reqIfNoneMatch,
    reqIfRange,
    reqIfUnmodifiedSince,
    reqMaxForwards,
    reqProxyAuthorization,
    reqReferrer,
    reqRange,
    reqTe,
    reqTranslate,
    reqUserAgent
{$ifdef DELPHI5OROLDER}
   );
const // Delphi 5 does not support values overlapping for enums
  respAcceptRanges = THttpHeader(20);
  respAge = THttpHeader(21);
  respEtag = THttpHeader(22);
  respLocation = THttpHeader(23);
  respProxyAuthenticate = THttpHeader(24);
  respRetryAfter = THttpHeader(25);
  respServer = THttpHeader(26);
  respSetCookie = THttpHeader(27);
  respVary = THttpHeader(28);
  respWwwAuthenticate = THttpHeader(29);
type
{$else}  ,
    respAcceptRanges = 20,
    respAge,
    respEtag,
    respLocation,
    respProxyAuthenticate,
    respRetryAfter,
    respServer,
    respSetCookie,
    respVary,
    respWwwAuthenticate);
{$endif}

  THttpVerb = (
    hvUnparsed,
    hvUnknown,
    hvInvalid,
    hvOPTIONS,
    hvGET,
    hvHEAD,
    hvPOST,
    hvPUT,
    hvDELETE,
    hvTRACE,
    hvCONNECT,
    hvTRACK,  // used by Microsoft Cluster Server for a non-logged trace
    hvMOVE,
    hvCOPY,
    hvPROPFIND,
    hvPROPPATCH,
    hvMKCOL,
    hvLOCK,
    hvUNLOCK,
    hvSEARCH,
    hvMaximum );

  THttpChunkType = (
    hctFromMemory,
    hctFromFileHandle,
    hctFromFragmentCache);

  THttpServiceConfigID = (
    hscIPListenList,
    hscSSLCertInfo,
    hscUrlAclInfo,
    hscMax);
  THttpServiceConfigQueryType = (
    hscQueryExact,
    hscQueryNext,
    hscQueryMax);

  HTTP_URL_CONTEXT = HTTP_OPAQUE_ID;
  HTTP_REQUEST_ID = HTTP_OPAQUE_ID;
  HTTP_CONNECTION_ID = HTTP_OPAQUE_ID;
  HTTP_RAW_CONNECTION_ID = HTTP_OPAQUE_ID;

  // Pointers overlap and point into pFullUrl. nil if not present.
  HTTP_COOKED_URL = record
    FullUrlLength: word;     // in bytes not including the #0
    HostLength: word;        // in bytes not including the #0
    AbsPathLength: word;     // in bytes not including the #0
    QueryStringLength: word; // in bytes not including the #0
    pFullUrl: PWideChar;     // points to "http://hostname:port/abs/.../path?query"
    pHost: PWideChar;        // points to the first char in the hostname
    pAbsPath: PWideChar;     // Points to the 3rd '/' char
    pQueryString: PWideChar; // Points to the 1st '?' char or #0
  end;

  HTTP_TRANSPORT_ADDRESS = record
    pRemoteAddress: PSOCKADDR;
    pLocalAddress: PSOCKADDR;
  end;

  HTTP_UNKNOWN_HEADER = record
    NameLength: word;          // in bytes not including the #0
    RawValueLength: word;      // in bytes not including the n#0
    pName: PAnsiChar;          // The header name (minus the ':' character)
    pRawValue: PAnsiChar;      // The header value
  end;
  PHTTP_UNKNOWN_HEADER = ^HTTP_UNKNOWN_HEADER;
  HTTP_UNKNOWN_HEADERs = array of HTTP_UNKNOWN_HEADER;

  HTTP_KNOWN_HEADER = record
    RawValueLength: word;     // in bytes not including the #0
    pRawValue: PAnsiChar;
  end;
  PHTTP_KNOWN_HEADER = ^HTTP_KNOWN_HEADER;

  HTTP_RESPONSE_HEADERS = record
    // number of entries in the unknown HTTP headers array
    UnknownHeaderCount: word;
    // array of unknown HTTP headers
    pUnknownHeaders: pointer;
    // Reserved, must be 0
    TrailerCount: word;
    // Reserved, must be nil
    pTrailers: pointer;
    // Known headers
    KnownHeaders: array[low(THttpHeader)..respWwwAuthenticate] of HTTP_KNOWN_HEADER;
  end;

  HTTP_REQUEST_HEADERS = record
    // number of entries in the unknown HTTP headers array
    UnknownHeaderCount: word;
    // array of unknown HTTP headers
    pUnknownHeaders: PHTTP_UNKNOWN_HEADER;
    // Reserved, must be 0
    TrailerCount: word;
    // Reserved, must be nil
    pTrailers: pointer;
    // Known headers
    KnownHeaders: array[low(THttpHeader)..reqUserAgent] of HTTP_KNOWN_HEADER;
  end;

  HTTP_BYTE_RANGE = record
    StartingOffset: ULARGE_INTEGER;
    Length: ULARGE_INTEGER;
  end;

  // we use 3 distinct HTTP_DATA_CHUNK_* records since variable records
  // alignment is buggy/non compatible under Delphi XE3
  HTTP_DATA_CHUNK_INMEMORY = record
    DataChunkType: THttpChunkType; // always hctFromMemory
    Reserved1: ULONG;
    pBuffer: pointer;
    BufferLength: ULONG;
    Reserved2: ULONG;
    Reserved3: ULONG;
  end;
  PHTTP_DATA_CHUNK_INMEMORY = ^HTTP_DATA_CHUNK_INMEMORY;
  HTTP_DATA_CHUNK_FILEHANDLE = record
    DataChunkType: THttpChunkType; // always hctFromFileHandle
    ByteRange: HTTP_BYTE_RANGE;
    FileHandle: THandle;
  end;
  HTTP_DATA_CHUNK_FRAGMENTCACHE = record
    DataChunkType: THttpChunkType; // always hctFromFragmentCache
    FragmentNameLength: word;      // in bytes not including the #0
    pFragmentName: PWideChar;
  end;

  HTTP_SSL_CLIENT_CERT_INFO = record
    CertFlags: ULONG;
    CertEncodedSize: ULONG;
    pCertEncoded: PUCHAR;
    Token: THandle;
    CertDeniedByMapper: boolean;
  end;
  PHTTP_SSL_CLIENT_CERT_INFO = ^HTTP_SSL_CLIENT_CERT_INFO;

  HTTP_SSL_INFO = record
    ServerCertKeySize: word;
    ConnectionKeySize: word;
    ServerCertIssuerSize: ULONG;
    ServerCertSubjectSize: ULONG;
    pServerCertIssuer: PAnsiChar;
    pServerCertSubject: PAnsiChar;
    pClientCertInfo: PHTTP_SSL_CLIENT_CERT_INFO;
    SslClientCertNegotiated: ULONG;
  end;
  PHTTP_SSL_INFO = ^HTTP_SSL_INFO;

  HTTP_SERVICE_CONFIG_URLACL_KEY = record
    pUrlPrefix: PWideChar;
  end;
  HTTP_SERVICE_CONFIG_URLACL_PARAM = record
    pStringSecurityDescriptor: PWideChar;
  end;
  HTTP_SERVICE_CONFIG_URLACL_SET = record
    KeyDesc: HTTP_SERVICE_CONFIG_URLACL_KEY;
    ParamDesc: HTTP_SERVICE_CONFIG_URLACL_PARAM;
  end;
  HTTP_SERVICE_CONFIG_URLACL_QUERY = record
    QueryDesc: THttpServiceConfigQueryType;
    KeyDesc: HTTP_SERVICE_CONFIG_URLACL_KEY;
    dwToken: DWORD;
  end;

  HTTP_REQUEST_INFO_TYPE = (
    HttpRequestInfoTypeAuth
    );

  // about Authentication in HTTP Version 2.0
  // see https://msdn.microsoft.com/en-us/library/windows/desktop/aa364452

  HTTP_AUTH_STATUS = (
    HttpAuthStatusSuccess,
    HttpAuthStatusNotAuthenticated,
    HttpAuthStatusFailure
    );

  HTTP_REQUEST_AUTH_TYPE = (
    HttpRequestAuthTypeNone,
    HttpRequestAuthTypeBasic,
    HttpRequestAuthTypeDigest,
    HttpRequestAuthTypeNTLM,
    HttpRequestAuthTypeNegotiate,
    HttpRequestAuthTypeKerberos
    );

  SECURITY_STATUS = ULONG;

  HTTP_REQUEST_AUTH_INFO = record
    AuthStatus: HTTP_AUTH_STATUS;
    SecStatus: SECURITY_STATUS;
    Flags: ULONG;
    AuthType: HTTP_REQUEST_AUTH_TYPE;
    AccessToken: THandle;
    ContextAttributes: ULONG;
    PackedContextLength: ULONG;
    PackedContextType: ULONG;
    PackedContext: pointer;
    MutualAuthDataLength: ULONG;
    pMutualAuthData: PCHAR;
  end;
  PHTTP_REQUEST_AUTH_INFO = ^HTTP_REQUEST_AUTH_INFO;

  HTTP_REQUEST_INFO = record
    InfoType: HTTP_REQUEST_INFO_TYPE;
    InfoLength: ULONG;
    pInfo: pointer;
  end;
  HTTP_REQUEST_INFOS = array[0..1000] of HTTP_REQUEST_INFO;
  PHTTP_REQUEST_INFOS = ^HTTP_REQUEST_INFOS;

  /// structure used to handle data associated with a specific request
  HTTP_REQUEST = record
    // either 0 (Only Header), either HTTP_RECEIVE_REQUEST_FLAG_COPY_BODY
    Flags: cardinal;
    // An identifier for the connection on which the request was received
    ConnectionId: HTTP_CONNECTION_ID;
    // A value used to identify the request when calling
    // HttpReceiveRequestEntityBody, HttpSendHttpResponse, and/or
    // HttpSendResponseEntityBody
    RequestId: HTTP_REQUEST_ID;
    // The context associated with the URL prefix
    UrlContext: HTTP_URL_CONTEXT;
    // The HTTP version number
    Version: HTTP_VERSION;
    // An HTTP verb associated with this request
    Verb: THttpVerb;
    // The length of the verb string if the Verb field is hvUnknown
    // (in bytes not including the last #0)
    UnknownVerbLength: word;
    // The length of the raw (uncooked) URL (in bytes not including the last #0)
    RawUrlLength: word;
     // Pointer to the verb string if the Verb field is hvUnknown
    pUnknownVerb: PAnsiChar;
    // Pointer to the raw (uncooked) URL
    pRawUrl: PAnsiChar;
    // The canonicalized Unicode URL
    CookedUrl: HTTP_COOKED_URL;
    // Local and remote transport addresses for the connection
    Address: HTTP_TRANSPORT_ADDRESS;
    // The request headers.
    Headers: HTTP_REQUEST_HEADERS;
    // The total number of bytes received from network for this request
    BytesReceived: ULONGLONG;
    EntityChunkCount: word;
    pEntityChunks: pointer;
    RawConnectionId: HTTP_RAW_CONNECTION_ID;
    // SSL connection information
    pSslInfo: PHTTP_SSL_INFO;
    // beginning of HTTP_REQUEST_V2 structure
    xxxPadding: DWORD;
    RequestInfoCount: word;
    pRequestInfo: PHTTP_REQUEST_INFOS;
  end;
  PHTTP_REQUEST = ^HTTP_REQUEST;

  HTTP_RESPONSE_INFO_TYPE = (
    HttpResponseInfoTypeMultipleKnownHeaders,
    HttpResponseInfoTypeAuthenticationProperty,
    HttpResponseInfoTypeQosProperty,
    HttpResponseInfoTypeChannelBind
    );

  HTTP_RESPONSE_INFO = record
    Typ: HTTP_RESPONSE_INFO_TYPE;
    Length: ULONG;
    pInfo: Pointer;
  end;
  PHTTP_RESPONSE_INFO = ^HTTP_RESPONSE_INFO;

  /// structure as expected by HttpSendHttpResponse() API
  HTTP_RESPONSE = object
  public
    Flags: cardinal;
    // The raw HTTP protocol version number
    Version: HTTP_VERSION;
    // The HTTP status code (e.g., 200)
    StatusCode: word;
    // in bytes not including the '\0'
    ReasonLength: word;
    // The HTTP reason (e.g., "OK"). This MUST not contain non-ASCII characters
    // (i.e., all chars must be in range 0x20-0x7E).
    pReason: PAnsiChar;
    // The response headers
    Headers: HTTP_RESPONSE_HEADERS;
    // number of elements in pEntityChunks[] array
    EntityChunkCount: word;
    // pEntityChunks points to an array of EntityChunkCount HTTP_DATA_CHUNK_*
    pEntityChunks: pointer;
    // contains the number of HTTP API 2.0 extended information
    ResponseInfoCount: word;
    // map the HTTP API 2.0 extended information
    pResponseInfo: PHTTP_RESPONSE_INFO;
    // will set both StatusCode and Reason
    // - OutStatus is a temporary variable which will be field with the
    // corresponding text
    procedure SetStatus(code: integer; var OutStatus: SockString);
    // will set the content of the reponse, and ContentType header
    procedure SetContent(var DataChunk: HTTP_DATA_CHUNK_INMEMORY;
      const Content: SockString; const ContentType: SockString='text/html');
    /// will set all header values from lines
    // - Content-Type/Content-Encoding/Location will be set in KnownHeaders[]
    // - all other headers will be set in temp UnknownHeaders[]
    procedure SetHeaders(P: PAnsiChar; var UnknownHeaders: HTTP_UNKNOWN_HEADERs);
    /// add one header value to the internal headers
    // - SetHeaders() method should have been called before to initialize the
    // internal UnknownHeaders[] array
    function AddCustomHeader(P: PAnsiChar; var UnknownHeaders: HTTP_UNKNOWN_HEADERs): PAnsiChar;
  end;
  PHTTP_RESPONSE = ^HTTP_RESPONSE;

  HTTP_PROPERTY_FLAGS = ULONG;

  HTTP_ENABLED_STATE = (
    HttpEnabledStateActive,
    HttpEnabledStateInactive
    );
  PHTTP_ENABLED_STATE = ^HTTP_ENABLED_STATE;

  HTTP_STATE_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    State: HTTP_ENABLED_STATE;
  end;
  PHTTP_STATE_INFO = ^HTTP_STATE_INFO;

  THTTP_503_RESPONSE_VERBOSITY = (
    Http503ResponseVerbosityBasic,
    Http503ResponseVerbosityLimited,
    Http503ResponseVerbosityFull
    );
  PHTTP_503_RESPONSE_VERBOSITY = ^ THTTP_503_RESPONSE_VERBOSITY;

  HTTP_QOS_SETTING_TYPE = (
    HttpQosSettingTypeBandwidth,
    HttpQosSettingTypeConnectionLimit,
    HttpQosSettingTypeFlowRate // Windows Server 2008 R2 and Windows 7 only.
    );
  PHTTP_QOS_SETTING_TYPE = ^HTTP_QOS_SETTING_TYPE;

  HTTP_QOS_SETTING_INFO = record
    QosType: HTTP_QOS_SETTING_TYPE;
    QosSetting: Pointer;
  end;
  PHTTP_QOS_SETTING_INFO = ^HTTP_QOS_SETTING_INFO;

  HTTP_CONNECTION_LIMIT_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    MaxConnections: ULONG;
  end;
  PHTTP_CONNECTION_LIMIT_INFO = ^HTTP_CONNECTION_LIMIT_INFO;

  HTTP_BANDWIDTH_LIMIT_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    MaxBandwidth: ULONG;
  end;
  PHTTP_BANDWIDTH_LIMIT_INFO = ^HTTP_BANDWIDTH_LIMIT_INFO;

  HTTP_FLOWRATE_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    MaxBandwidth: ULONG;
    MaxPeakBandwidth: ULONG;
    BurstSize: ULONG;
  end;
  PHTTP_FLOWRATE_INFO = ^HTTP_FLOWRATE_INFO;

const
   HTTP_MIN_ALLOWED_BANDWIDTH_THROTTLING_RATE {:ULONG} = 1024;
   HTTP_LIMIT_INFINITE {:ULONG} = ULONG(-1);

type
  HTTP_SERVICE_CONFIG_TIMEOUT_KEY = (
    IdleConnectionTimeout,
    HeaderWaitTimeout
    );
  PHTTP_SERVICE_CONFIG_TIMEOUT_KEY = ^HTTP_SERVICE_CONFIG_TIMEOUT_KEY;

  HTTP_SERVICE_CONFIG_TIMEOUT_PARAM = word;
  PHTTP_SERVICE_CONFIG_TIMEOUT_PARAM = ^HTTP_SERVICE_CONFIG_TIMEOUT_PARAM;

  HTTP_SERVICE_CONFIG_TIMEOUT_SET = record
    KeyDesc: HTTP_SERVICE_CONFIG_TIMEOUT_KEY;
    ParamDesc: HTTP_SERVICE_CONFIG_TIMEOUT_PARAM;
  end;
  PHTTP_SERVICE_CONFIG_TIMEOUT_SET = ^HTTP_SERVICE_CONFIG_TIMEOUT_SET;

  HTTP_TIMEOUT_LIMIT_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    EntityBody: word;
    DrainEntityBody: word;
    RequestQueue: word;
    IdleConnection: word;
    HeaderWait: word;
    MinSendRate: cardinal;
  end;
  PHTTP_TIMEOUT_LIMIT_INFO = ^HTTP_TIMEOUT_LIMIT_INFO;

  HTTP_LISTEN_ENDPOINT_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    EnableSharing: BOOLEAN;
  end;
  PHTTP_LISTEN_ENDPOINT_INFO = ^HTTP_LISTEN_ENDPOINT_INFO;

  HTTP_SERVER_AUTHENTICATION_DIGEST_PARAMS = record
    DomainNameLength: word;
    DomainName: PWideChar;
    RealmLength: word;
    Realm: PWideChar;
  end;
  PHTTP_SERVER_AUTHENTICATION_DIGEST_PARAMS = ^HTTP_SERVER_AUTHENTICATION_DIGEST_PARAMS;

  HTTP_SERVER_AUTHENTICATION_BASIC_PARAMS = record
    RealmLength: word;
    Realm: PWideChar;
  end;
  PHTTP_SERVER_AUTHENTICATION_BASIC_PARAMS = ^HTTP_SERVER_AUTHENTICATION_BASIC_PARAMS;

const
  HTTP_AUTH_ENABLE_BASIC        = $00000001;
  HTTP_AUTH_ENABLE_DIGEST       = $00000002;
  HTTP_AUTH_ENABLE_NTLM         = $00000004;
  HTTP_AUTH_ENABLE_NEGOTIATE    = $00000008;
  HTTP_AUTH_ENABLE_KERBEROS     = $00000010;
  HTTP_AUTH_ENABLE_ALL          = $0000001F;

  HTTP_AUTH_EX_FLAG_ENABLE_KERBEROS_CREDENTIAL_CACHING  = $01;
  HTTP_AUTH_EX_FLAG_CAPTURE_CREDENTIAL                  = $02;

type
  HTTP_SERVER_AUTHENTICATION_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    AuthSchemes: ULONG;
    ReceiveMutualAuth: BYTEBOOL;
    ReceiveContextHandle: BYTEBOOL;
    DisableNTLMCredentialCaching: BYTEBOOL;
    ExFlags: BYTE;
    DigestParams: HTTP_SERVER_AUTHENTICATION_DIGEST_PARAMS;
    BasicParams: HTTP_SERVER_AUTHENTICATION_BASIC_PARAMS;
  end;
  PHTTP_SERVER_AUTHENTICATION_INFO = ^HTTP_SERVER_AUTHENTICATION_INFO;


  HTTP_SERVICE_BINDING_TYPE=(
    HttpServiceBindingTypeNone,
    HttpServiceBindingTypeW,
    HttpServiceBindingTypeA
    );

  HTTP_SERVICE_BINDING_BASE = record
    BindingType: HTTP_SERVICE_BINDING_TYPE;
  end;
  PHTTP_SERVICE_BINDING_BASE = ^HTTP_SERVICE_BINDING_BASE;

  HTTP_SERVICE_BINDING_A = record
    Base: HTTP_SERVICE_BINDING_BASE;
    Buffer: PAnsiChar;
    BufferSize: ULONG;
  end;
  PHTTP_SERVICE_BINDING_A = HTTP_SERVICE_BINDING_A;

  HTTP_SERVICE_BINDING_W = record
    Base: HTTP_SERVICE_BINDING_BASE;
    Buffer: PWCHAR;
    BufferSize: ULONG;
  end;
  PHTTP_SERVICE_BINDING_W = ^HTTP_SERVICE_BINDING_W;

  HTTP_AUTHENTICATION_HARDENING_LEVELS = (
    HttpAuthenticationHardeningLegacy,
    HttpAuthenticationHardeningMedium,
    HttpAuthenticationHardeningStrict
  );

const
  HTTP_CHANNEL_BIND_PROXY = $1;
  HTTP_CHANNEL_BIND_PROXY_COHOSTING = $20;

  HTTP_CHANNEL_BIND_NO_SERVICE_NAME_CHECK = $2;
  HTTP_CHANNEL_BIND_DOTLESS_SERVICE = $4;
  HTTP_CHANNEL_BIND_SECURE_CHANNEL_TOKEN = $8;
  HTTP_CHANNEL_BIND_CLIENT_SERVICE = $10;

type
  HTTP_CHANNEL_BIND_INFO = record
    Hardening: HTTP_AUTHENTICATION_HARDENING_LEVELS;
    Flags: ULONG;
    ServiceNames: PHTTP_SERVICE_BINDING_BASE;
    NumberOfServiceNames: ULONG;
  end;
  PHTTP_CHANNEL_BIND_INFO = ^HTTP_CHANNEL_BIND_INFO;

  HTTP_REQUEST_CHANNEL_BIND_STATUS = record
    ServiceName: PHTTP_SERVICE_BINDING_BASE;
    ChannelToken: PUCHAR;
    ChannelTokenSize: ULONG;
    Flags: ULONG;
  end;
  PHTTP_REQUEST_CHANNEL_BIND_STATUS = ^HTTP_REQUEST_CHANNEL_BIND_STATUS;

const
   // Logging option flags. When used in the logging configuration alters
   // some default logging behaviour.

   // HTTP_LOGGING_FLAG_LOCAL_TIME_ROLLOVER - This flag is used to change
   //      the log file rollover to happen by local time based. By default
   //      log file rollovers happen by GMT time.
   HTTP_LOGGING_FLAG_LOCAL_TIME_ROLLOVER = 1;

   // HTTP_LOGGING_FLAG_USE_UTF8_CONVERSION - When set the unicode fields
   //      will be converted to UTF8 multibytes when writting to the log
   //      files. When this flag is not present, the local code page
   //      conversion happens.
   HTTP_LOGGING_FLAG_USE_UTF8_CONVERSION = 2;

   // HTTP_LOGGING_FLAG_LOG_ERRORS_ONLY -
   // HTTP_LOGGING_FLAG_LOG_SUCCESS_ONLY - These two flags are used to
   //      to do selective logging. If neither of them are present both
   //      types of requests will be logged. Only one these flags can be
   //      set at a time. They are mutually exclusive.
   HTTP_LOGGING_FLAG_LOG_ERRORS_ONLY = 4;
   HTTP_LOGGING_FLAG_LOG_SUCCESS_ONLY = 8;

   // The known log fields recognized/supported by HTTPAPI. Following fields
   // are used for W3C logging. Subset of them are also used for error logging
   HTTP_LOG_FIELD_DATE              = $00000001;
   HTTP_LOG_FIELD_TIME              = $00000002;
   HTTP_LOG_FIELD_CLIENT_IP         = $00000004;
   HTTP_LOG_FIELD_USER_NAME         = $00000008;
   HTTP_LOG_FIELD_SITE_NAME         = $00000010;
   HTTP_LOG_FIELD_COMPUTER_NAME     = $00000020;
   HTTP_LOG_FIELD_SERVER_IP         = $00000040;
   HTTP_LOG_FIELD_METHOD            = $00000080;
   HTTP_LOG_FIELD_URI_STEM          = $00000100;
   HTTP_LOG_FIELD_URI_QUERY         = $00000200;
   HTTP_LOG_FIELD_STATUS            = $00000400;
   HTTP_LOG_FIELD_WIN32_STATUS      = $00000800;
   HTTP_LOG_FIELD_BYTES_SENT        = $00001000;
   HTTP_LOG_FIELD_BYTES_RECV        = $00002000;
   HTTP_LOG_FIELD_TIME_TAKEN        = $00004000;
   HTTP_LOG_FIELD_SERVER_PORT       = $00008000;
   HTTP_LOG_FIELD_USER_AGENT        = $00010000;
   HTTP_LOG_FIELD_COOKIE            = $00020000;
   HTTP_LOG_FIELD_REFERER           = $00040000;
   HTTP_LOG_FIELD_VERSION           = $00080000;
   HTTP_LOG_FIELD_HOST              = $00100000;
   HTTP_LOG_FIELD_SUB_STATUS        = $00200000;

   HTTP_ALL_NON_ERROR_LOG_FIELDS = HTTP_LOG_FIELD_SUB_STATUS*2-1;

   // Fields that are used only for error logging
   HTTP_LOG_FIELD_CLIENT_PORT    = $00400000;
   HTTP_LOG_FIELD_URI            = $00800000;
   HTTP_LOG_FIELD_SITE_ID        = $01000000;
   HTTP_LOG_FIELD_REASON         = $02000000;
   HTTP_LOG_FIELD_QUEUE_NAME     = $04000000;

type
  HTTP_LOGGING_TYPE = (
    HttpLoggingTypeW3C,
    HttpLoggingTypeIIS,
    HttpLoggingTypeNCSA,
    HttpLoggingTypeRaw
    );

  HTTP_LOGGING_ROLLOVER_TYPE = (
    HttpLoggingRolloverSize,
    HttpLoggingRolloverDaily,
    HttpLoggingRolloverWeekly,
    HttpLoggingRolloverMonthly,
    HttpLoggingRolloverHourly
    );

  HTTP_LOGGING_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    LoggingFlags: ULONG;
    SoftwareName: PWideChar;
    SoftwareNameLength: word;
    DirectoryNameLength: word;
    DirectoryName: PWideChar;
    Format: HTTP_LOGGING_TYPE;
    Fields: ULONG;
    pExtFields: pointer;
    NumOfExtFields: word;
    MaxRecordSize: word;
    RolloverType: HTTP_LOGGING_ROLLOVER_TYPE;
    RolloverSize: ULONG;
    pSecurityDescriptor: PSECURITY_DESCRIPTOR;
  end;
  PHTTP_LOGGING_INFO = ^HTTP_LOGGING_INFO;

  HTTP_LOG_DATA_TYPE = (
    HttpLogDataTypeFields
    );

  HTTP_LOG_DATA = record
    Typ: HTTP_LOG_DATA_TYPE
  end;
  PHTTP_LOG_DATA = ^HTTP_LOG_DATA;

  HTTP_LOG_FIELDS_DATA = record
    Base: HTTP_LOG_DATA;
    UserNameLength: word;
    UriStemLength: word;
    ClientIpLength: word;
    ServerNameLength: word;
    ServiceNameLength: word;
    ServerIpLength: word;
    MethodLength: word;
    UriQueryLength: word;
    HostLength: word;
    UserAgentLength: word;
    CookieLength: word;
    ReferrerLength: word;
    UserName: PWideChar;
    UriStem: PWideChar;
    ClientIp: PAnsiChar;
    ServerName: PAnsiChar;
    ServiceName: PAnsiChar;
    ServerIp: PAnsiChar;
    Method: PAnsiChar;
    UriQuery: PAnsiChar;
    Host: PAnsiChar;
    UserAgent: PAnsiChar;
    Cookie: PAnsiChar;
    Referrer: PAnsiChar;
    ServerPort: word;
    ProtocolStatus: word;
    Win32Status: ULONG;
    MethodNum: THttpVerb;
    SubStatus: word;
  end;
  PHTTP_LOG_FIELDS_DATA = ^HTTP_LOG_FIELDS_DATA;

  HTTP_BINDING_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    RequestQueueHandle: THandle;
  end;

  HTTP_PROTECTION_LEVEL_TYPE=(
    HttpProtectionLevelUnrestricted,
    HttpProtectionLevelEdgeRestricted,
    HttpProtectionLevelRestricted
    );

  HTTP_PROTECTION_LEVEL_INFO = record
    Flags: HTTP_PROPERTY_FLAGS;
    Level: HTTP_PROTECTION_LEVEL_TYPE;
  end;
  PHTTP_PROTECTION_LEVEL_INFO = ^HTTP_PROTECTION_LEVEL_INFO;

const
  HTTP_VERSION_UNKNOWN: HTTP_VERSION = (MajorVersion: 0; MinorVersion: 0);
  HTTP_VERSION_0_9: HTTP_VERSION = (MajorVersion: 0; MinorVersion: 9);
  HTTP_VERSION_1_0: HTTP_VERSION = (MajorVersion: 1; MinorVersion: 0);
  HTTP_VERSION_1_1: HTTP_VERSION = (MajorVersion: 1; MinorVersion: 1);
  /// error raised by HTTP API when the client disconnected (e.g. after timeout)
  HTTPAPI_ERROR_NONEXISTENTCONNECTION = 1229;
  // if set, available entity body is copied along with the request headers
  // into pEntityChunks
  HTTP_RECEIVE_REQUEST_FLAG_COPY_BODY = 1;
  // there is more entity body to be read for this request
  HTTP_REQUEST_FLAG_MORE_ENTITY_BODY_EXISTS = 1;
  // initialization for applications that use the HTTP Server API
  HTTP_INITIALIZE_SERVER = 1;
  // initialization for applications that use the HTTP configuration functions
  HTTP_INITIALIZE_CONFIG = 2;
  // see http://msdn.microsoft.com/en-us/library/windows/desktop/aa364496
  HTTP_RECEIVE_REQUEST_ENTITY_BODY_FLAG_FILL_BUFFER = 1;
  // see http://msdn.microsoft.com/en-us/library/windows/desktop/aa364499
  HTTP_SEND_RESPONSE_FLAG_PROCESS_RANGES = 1;
  // flag which can be used by HttpRemoveUrlFromUrlGroup()
  HTTP_URL_FLAG_REMOVE_ALL = 1;

function RetrieveHeaders(const Request: HTTP_REQUEST;
  out RemoteIP: SockString): SockString;
const
  KNOWNHEADERS: array[reqCacheControl..reqUserAgent] of string[19] = (
    'Cache-Control','Connection','Date','Keep-Alive','Pragma','Trailer',
    'Transfer-Encoding','Upgrade','Via','Warning','Allow','Content-Length',
    'Content-Type','Content-Encoding','Content-Language','Content-Location',
    'Content-MD5','Content-Range','Expires','Last-Modified','Accept',
    'Accept-Charset','Accept-Encoding','Accept-Language','Authorization',
    'Cookie','Expect','From','Host','If-Match','If-Modified-Since',
    'If-None-Match','If-Range','If-Unmodified-Since','Max-Forwards',
    'Proxy-Authorization','Referer','Range','TE','Translate','User-Agent');
  REMOTEIP_HEADERLEN = 10;
  REMOTEIP_HEADER: string[REMOTEIP_HEADERLEN] = 'RemoteIP: ';
var i, L: integer;
    H: THttpHeader;
    P: PHTTP_UNKNOWN_HEADER;
    D: PAnsiChar;
begin
  assert(low(KNOWNHEADERS)=low(Request.Headers.KnownHeaders));
  assert(high(KNOWNHEADERS)=high(Request.Headers.KnownHeaders));
  if Request.Address.pRemoteAddress<>nil then
    GetSinIPFromCache(PVarSin(Request.Address.pRemoteAddress)^,RemoteIP);
  // compute headers length
  if RemoteIP<>'' then
    L := (REMOTEIP_HEADERLEN+2)+length(RemoteIP) else
    L := 0;
  for H := low(KNOWNHEADERS) to high(KNOWNHEADERS) do
    if Request.Headers.KnownHeaders[h].RawValueLength<>0 then
      inc(L,Request.Headers.KnownHeaders[h].RawValueLength+ord(KNOWNHEADERS[h][0])+4);
  P := Request.Headers.pUnknownHeaders;
  if P<>nil then
    for i := 1 to Request.Headers.UnknownHeaderCount do begin
      inc(L,P^.NameLength+P^.RawValueLength+4); // +4 for each ': '+#13#10
      inc(P);
    end;
  // set headers content
  SetString(result,nil,L);
  D := pointer(result);
  for H := low(KNOWNHEADERS) to high(KNOWNHEADERS) do
    if Request.Headers.KnownHeaders[h].RawValueLength<>0 then begin
      move(KNOWNHEADERS[h][1],D^,ord(KNOWNHEADERS[h][0]));
      inc(D,ord(KNOWNHEADERS[h][0]));
      PWord(D)^ := ord(':')+ord(' ')shl 8;
      inc(D,2);
      move(Request.Headers.KnownHeaders[h].pRawValue^,D^,
        Request.Headers.KnownHeaders[h].RawValueLength);
      inc(D,Request.Headers.KnownHeaders[h].RawValueLength);
      PWord(D)^ := 13+10 shl 8;
      inc(D,2);
    end;
  P := Request.Headers.pUnknownHeaders;
  if P<>nil then
    for i := 1 to Request.Headers.UnknownHeaderCount do begin
      move(P^.pName^,D^,P^.NameLength);
      inc(D,P^.NameLength);
      PWord(D)^ := ord(':')+ord(' ')shl 8;
      inc(D,2);
      move(P^.pRawValue^,D^,P^.RawValueLength);
      inc(D,P^.RawValueLength);
      inc(P);
      PWord(D)^ := 13+10 shl 8;
      inc(D,2);
    end;
  if RemoteIP<>'' then begin
    move(REMOTEIP_HEADER[1],D^,REMOTEIP_HEADERLEN);
    inc(D,REMOTEIP_HEADERLEN);
    move(pointer(RemoteIP)^,D^,length(RemoteIP));
    inc(D,length(RemoteIP));
    PWord(D)^ := 13+10 shl 8;
  {$ifopt C+}
    inc(D,2);
  end;
  assert(D-pointer(result)=L);
  {$else}
  end;
  {$endif}
end;

type
  HTTP_SERVER_PROPERTY = (
    HttpServerAuthenticationProperty,
    HttpServerLoggingProperty,
    HttpServerQosProperty,
    HttpServerTimeoutsProperty,
    HttpServerQueueLengthProperty,
    HttpServerStateProperty,
    HttpServer503VerbosityProperty,
    HttpServerBindingProperty,
    HttpServerExtendedAuthenticationProperty,
    HttpServerListenEndpointProperty,
    HttpServerChannelBindProperty,
    HttpServerProtectionLevelProperty
    );

  /// direct late-binding access to the HTTP API server 1.0 or 2.0
  THttpAPI = packed record
    /// access to the httpapi.dll loaded library
    Module: THandle;
    /// will be either 1.0 or 2.0, depending on the published .dll functions
    Version: HTTP_VERSION;
    /// The HttpInitialize function initializes the HTTP Server API driver, starts it,
    // if it has not already been started, and allocates data structures for the
    // calling application to support response-queue creation and other operations.
    // Call this function before calling any other functions in the HTTP Server API.
    Initialize: function(Version: HTTP_VERSION; Flags: cardinal;
      pReserved: pointer=nil): HRESULT; stdcall;
    /// The HttpTerminate function cleans up resources used by the HTTP Server API
    // to process calls by an application. An application should call HttpTerminate
    // once for every time it called HttpInitialize, with matching flag settings.
    Terminate: function(Flags: cardinal;
      Reserved: integer=0): HRESULT; stdcall;
    /// The HttpCreateHttpHandle function creates an HTTP request queue for the
    // calling application and returns a handle to it.
    CreateHttpHandle: function(var ReqQueueHandle: THandle;
      Reserved: integer=0): HRESULT; stdcall;
    /// The HttpAddUrl function registers a given URL so that requests that match
    // it are routed to a specified HTTP Server API request queue. An application
    // can register multiple URLs to a single request queue using repeated calls to
    // HttpAddUrl
    // - a typical url prefix is 'http://+:80/vroot/', 'https://+:80/vroot/' or
    // 'https://adatum.com:443/secure/database/' - here the '+' is called a
    // Strong wildcard, i.e. will match every IP or server name
    AddUrl: function(ReqQueueHandle: THandle; UrlPrefix: PWideChar;
      Reserved: integer=0): HRESULT; stdcall;
    /// Unregisters a specified URL, so that requests for it are no longer
    // routed to a specified queue.
    RemoveUrl: function(ReqQueueHandle: THandle; UrlPrefix: PWideChar): HRESULT; stdcall;
    /// retrieves the next available HTTP request from the specified request queue
    ReceiveHttpRequest: function(ReqQueueHandle: THandle; RequestId: HTTP_REQUEST_ID;
      Flags: cardinal; var pRequestBuffer: HTTP_REQUEST; RequestBufferLength: ULONG;
      var pBytesReceived: ULONG; pOverlapped: pointer=nil): HRESULT; stdcall;
    /// sent the response to a specified HTTP request
    // - pLogData optional parameter is handled since HTTP API 2.0
    SendHttpResponse: function(ReqQueueHandle: THandle; RequestId: HTTP_REQUEST_ID;
      Flags: integer; var pHttpResponse: HTTP_RESPONSE; pReserved1: pointer;
      var pBytesSent: cardinal; pReserved2: pointer=nil; Reserved3: ULONG=0;
      pOverlapped: pointer=nil; pLogData: PHTTP_LOG_DATA=nil): HRESULT; stdcall;
    /// receives additional entity body data for a specified HTTP request
    ReceiveRequestEntityBody: function(ReqQueueHandle: THandle; RequestId: HTTP_REQUEST_ID;
      Flags: ULONG; pBuffer: pointer; BufferLength: cardinal; var pBytesReceived: cardinal;
      pOverlapped: pointer=nil): HRESULT; stdcall;
    /// set specified data, such as IP addresses or SSL Certificates, from the
    // HTTP Server API configuration store
    SetServiceConfiguration: function(ServiceHandle: THandle;
      ConfigId: THttpServiceConfigID; pConfigInformation: pointer;
      ConfigInformationLength: ULONG; pOverlapped: pointer=nil): HRESULT; stdcall;
    /// deletes specified data, such as IP addresses or SSL Certificates, from the
    // HTTP Server API configuration store
    DeleteServiceConfiguration: function(ServiceHandle: THandle;
      ConfigId: THttpServiceConfigID; pConfigInformation: pointer;
      ConfigInformationLength: ULONG; pOverlapped: pointer=nil): HRESULT; stdcall;
    /// removes from the HTTP Server API cache associated with a given request
    // queue all response fragments that have a name whose site portion matches
    // a specified UrlPrefix
    FlushResponseCache: function(ReqQueueHandle: THandle; pUrlPrefix: PWideChar; Flags: ULONG;
      pOverlapped: POverlapped): ULONG; stdcall;
    /// cancels a specified request
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    CancelHttpRequest: function(ReqQueueHandle: THandle; RequestId: HTTP_REQUEST_ID;
      pOverlapped: pointer = nil): HRESULT; stdcall;
    /// creates a server session for the specified HTTP API version
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    CreateServerSession: function(Version: HTTP_VERSION;
      var ServerSessionId: HTTP_SERVER_SESSION_ID; Reserved: ULONG = 0): HRESULT; stdcall;
    /// deletes the server session identified by the server session ID
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    CloseServerSession: function(ServerSessionId: HTTP_SERVER_SESSION_ID): HRESULT; stdcall;
    ///  creates a new request queue or opens an existing request queue
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    // - replaces the HTTP version 1.0 CreateHttpHandle() function
    CreateRequestQueue: function(Version: HTTP_VERSION;
      pName: PWideChar; pSecurityAttributes: Pointer;
      Flags: ULONG; var ReqQueueHandle: THandle): HRESULT; stdcall;
    /// sets a new server session property or modifies an existing property
    // on the specified server session
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    SetServerSessionProperty: function(ServerSessionId: HTTP_SERVER_SESSION_ID;
      aProperty: HTTP_SERVER_PROPERTY; pPropertyInformation: Pointer;
      PropertyInformationLength: ULONG): HRESULT; stdcall;
    /// queries a server property on the specified server session
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    QueryServerSessionProperty: function(ServerSessionId: HTTP_SERVER_SESSION_ID;
      aProperty: HTTP_SERVER_PROPERTY; pPropertyInformation: Pointer;
      PropertyInformationLength: ULONG; pReturnLength: PULONG = nil): HRESULT; stdcall;
    /// creates a URL Group under the specified server session
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    CreateUrlGroup: function(ServerSessionId: HTTP_SERVER_SESSION_ID;
      var UrlGroupId: HTTP_URL_GROUP_ID; Reserved: ULONG = 0): HRESULT; stdcall;
    /// closes the URL Group identified by the URL Group ID
    // - this call also removes all of the URLs that are associated with
    // the URL Group
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    CloseUrlGroup: function(UrlGroupId: HTTP_URL_GROUP_ID): HRESULT; stdcall;
    /// adds the specified URL to the URL Group identified by the URL Group ID
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    // - this function replaces the HTTP version 1.0 AddUrl() function
    AddUrlToUrlGroup: function(UrlGroupId: HTTP_URL_GROUP_ID;
      pFullyQualifiedUrl: PWideChar; UrlContext: HTTP_URL_CONTEXT = 0;
      Reserved: ULONG = 0): HRESULT; stdcall;
    /// removes the specified URL from the group identified by the URL Group ID
    // - this function removes one, or all, of the URLs from the group
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    // - it replaces the HTTP version 1.0 RemoveUrl() function
    RemoveUrlFromUrlGroup: function(UrlGroupId: HTTP_URL_GROUP_ID;
      pFullyQualifiedUrl: PWideChar; Flags: ULONG): HRESULT; stdcall;
    /// sets a new property or modifies an existing property on the specified
    // URL Group
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    SetUrlGroupProperty: function(UrlGroupId: HTTP_URL_GROUP_ID;
      aProperty: HTTP_SERVER_PROPERTY; pPropertyInformation: Pointer;
      PropertyInformationLength: ULONG): HRESULT; stdcall;
    /// queries a property on the specified URL Group
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    QueryUrlGroupProperty: function(UrlGroupId: HTTP_URL_GROUP_ID;
      aProperty: HTTP_SERVER_PROPERTY; pPropertyInformation: Pointer;
      PropertyInformationLength: ULONG; pReturnLength: PULONG = nil): HRESULT; stdcall;
    /// sets a new property or modifies an existing property on the request
    // queue identified by the specified handle
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    SetRequestQueueProperty: function(ReqQueueHandle: THandle;
      aProperty: HTTP_SERVER_PROPERTY; pPropertyInformation: Pointer;
      PropertyInformationLength: ULONG; Reserved: ULONG; pReserved: Pointer): HRESULT; stdcall;
    ///  queries a property of the request queue identified by the
    // specified handle
    // - available only for HTTP API 2.0 (since Windows Vista / Server 2008)
    QueryRequestQueueProperty: function(ReqQueueHandle: THandle;
      aProperty: HTTP_SERVER_PROPERTY; pPropertyInformation: Pointer;
      PropertyInformationLength: ULONG; Reserved: ULONG; pReturnLength: PULONG; pReserved: Pointer): HRESULT; stdcall;
  end;

var
  Http: THttpAPI;

type
  THttpAPIs = (hInitialize,hTerminate,hCreateHttpHandle,
    hAddUrl, hRemoveUrl, hReceiveHttpRequest,
    hSendHttpResponse, hReceiveRequestEntityBody,
    hSetServiceConfiguration, hDeleteServiceConfiguration, hFlushResponseCache,
    hCancelHttpRequest,
    hCreateServerSession, hCloseServerSession,
    hCreateRequestQueue,
    hSetServerSessionProperty, hQueryServerSessionProperty,
    hCreateUrlGroup, hCloseUrlGroup,
    hAddUrlToUrlGroup, hRemoveUrlFromUrlGroup,
    hSetUrlGroupProperty, hQueryUrlGroupProperty,
    hSetRequestQueueProperty, hQueryRequestQueueProperty
    );
const
  hHttpApi2First = hCancelHttpRequest;

  HttpNames: array[THttpAPIs] of PChar = (
    'HttpInitialize','HttpTerminate','HttpCreateHttpHandle',
    'HttpAddUrl', 'HttpRemoveUrl', 'HttpReceiveHttpRequest',
    'HttpSendHttpResponse', 'HttpReceiveRequestEntityBody',
    'HttpSetServiceConfiguration', 'HttpDeleteServiceConfiguration',
    'HttpFlushResponseCache',
    'HttpCancelHttpRequest',
    'HttpCreateServerSession', 'HttpCloseServerSession',
    'HttpCreateRequestQueue',
    'HttpSetServerSessionProperty', 'HttpQueryServerSessionProperty',
    'HttpCreateUrlGroup', 'HttpCloseUrlGroup',
    'HttpAddUrlToUrlGroup', 'HttpRemoveUrlFromUrlGroup',
    'HttpSetUrlGroupProperty', 'HttpQueryUrlGroupProperty',
    'HttpSetRequestQueueProperty', 'HttpQueryRequestQueueProperty'
    );

function RegURL(aRoot, aPort: SockString; Https: boolean;
  aDomainName: SockString): SynUnicode;
const Prefix: array[boolean] of SockString = ('http://','https://');
begin
  if aPort='' then
    aPort := '80';
  aRoot := trim(aRoot);
  aDomainName := trim(aDomainName);
  if aDomainName='' then begin
    result := '';
    exit;
  end;
  if aRoot<>'' then begin
    if aRoot[1]<>'/' then
      insert('/',aRoot,1);
    if aRoot[length(aRoot)]<>'/' then
      aRoot := aRoot+'/';
  end else
    aRoot := '/'; // allow for instance 'http://*:2869/'
  aRoot := Prefix[Https]+aDomainName+':'+aPort+aRoot;
  result := SynUnicode(aRoot);
end;

const
  HTTPAPI_DLL = 'httpapi.dll';

procedure HttpApiInitialize;
var api: THttpAPIs;
    P: PPointer;
begin
  if Http.Module<>0 then
    exit; // already loaded
  try
    Http.Module := LoadLibrary(HTTPAPI_DLL);
    Http.Version.MajorVersion := 2; // API 2.0 if all functions are available
    if Http.Module<=255 then
      raise ECrtSocket.CreateFmt('Unable to find %s',[HTTPAPI_DLL]);
    P := @@Http.Initialize;
    for api := low(api) to high(api) do begin
      P^ := GetProcAddress(Http.Module,HttpNames[api]);
      if P^=nil then
        if api<hHttpApi2First then
          raise ECrtSocket.CreateFmt('Unable to find %s() in %s',[HttpNames[api],HTTPAPI_DLL]) else
          Http.Version.MajorVersion := 1; // e.g. Windows XP or Server 2003
      inc(P);
    end;
  except
    on E: Exception do begin
      if Http.Module>255 then begin
        FreeLibrary(Http.Module);
        Http.Module := 0;
      end;
      raise;
    end;
  end;
end;


{ EHttpApiServer }

type
  EHttpApiServer = class(ECrtSocket)
  protected
    fLastApi: THttpAPIs;
  public
    class procedure RaiseOnError(api: THttpAPIs; Error: integer);
    constructor Create(api: THttpAPIs; Error: integer); reintroduce;
  published
    property LastApi: THttpAPIs read fLastApi;
  end;

class procedure EHttpApiServer.RaiseOnError(api: THttpAPIs; Error: integer);
begin
  if Error<>NO_ERROR then
    raise self.Create(api,Error);
end;

constructor EHttpApiServer.Create(api: THttpAPIs; Error: integer);
begin
  fLastError := Error;
  fLastApi := api;
  inherited CreateFmt('%s failed: %s (%d)',
    [HttpNames[api],SysErrorMessagePerModule(Error,HTTPAPI_DLL),Error])
end;


{ THttpApiServer }

function THttpApiServer.AddUrl(const aRoot, aPort: SockString; Https: boolean;
  const aDomainName: SockString; aRegisterURI: boolean): integer;
var uri: SynUnicode;
    n: integer;
begin
  result := -1;
  if (Self=nil) or (fReqQueue=0) or (Http.Module=0) then
    exit;
  uri := RegURL(aRoot, aPort, Https, aDomainName);
  if uri='' then
    exit; // invalid parameters
  if aRegisterURI then
    AddUrlAuthorize(aRoot,aPort,Https,aDomainName);
  if Http.Version.MajorVersion>1 then
    result := Http.AddUrlToUrlGroup(fUrlGroupID,pointer(uri)) else
    result := Http.AddUrl(fReqQueue,pointer(uri));
  if result=NO_ERROR then begin
    n := length(fRegisteredUnicodeUrl);
    SetLength(fRegisteredUnicodeUrl,n+1);
    fRegisteredUnicodeUrl[n] := uri;
  end;
end;

function THttpApiServer.RemoveUrl(const aRoot, aPort: SockString; Https: boolean;
  const aDomainName: SockString): integer;
var uri: SynUnicode;
    i,j,n: integer;
begin
  result := -1;
  if (Self=nil) or (fReqQueue=0) or (Http.Module=0) then
    exit;
  uri := RegURL(aRoot, aPort, Https, aDomainName);
  if uri='' then
    exit; // invalid parameters
  n := High(fRegisteredUnicodeUrl);
  for i := 0 to n do
    if fRegisteredUnicodeUrl[i]=uri then begin
      if Http.Version.MajorVersion>1 then
        result := Http.RemoveUrlFromUrlGroup(fUrlGroupID,pointer(uri),0) else
        result := Http.RemoveUrl(fReqQueue,pointer(uri));
      if result<>0 then
        exit; // shall be handled by caller
      for j := i to n-1 do
        fRegisteredUnicodeUrl[j] := fRegisteredUnicodeUrl[j+1];
      SetLength(fRegisteredUnicodeUrl,n);
      exit;
    end;
end;

class function THttpApiServer.AddUrlAuthorize(const aRoot, aPort: SockString;
  Https: boolean; const aDomainName: SockString; OnlyDelete: boolean): string;
const
  /// will allow AddUrl() registration to everyone
  // - 'GA' (GENERIC_ALL) to grant all access
  // - 'S-1-1-0'	defines a group that includes all users
  HTTPADDURLSECDESC: PWideChar = 'D:(A;;GA;;;S-1-1-0)';
var prefix: SynUnicode;
    Error: HRESULT;
    Config: HTTP_SERVICE_CONFIG_URLACL_SET;
begin
  try
    HttpApiInitialize;
    prefix := RegURL(aRoot, aPort, Https, aDomainName);
    if prefix='' then
      result := 'Invalid parameters' else begin
      EHttpApiServer.RaiseOnError(hInitialize,Http.Initialize(
        Http.Version,HTTP_INITIALIZE_CONFIG));
      try
        fillchar(Config,sizeof(Config),0);
        Config.KeyDesc.pUrlPrefix := pointer(prefix);
        // first delete any existing information
        Error := Http.DeleteServiceConfiguration(0,hscUrlAclInfo,@Config,Sizeof(Config));
        // then add authorization rule
        if not OnlyDelete then begin
          Config.KeyDesc.pUrlPrefix := pointer(prefix);
          Config.ParamDesc.pStringSecurityDescriptor := HTTPADDURLSECDESC;
          Error := Http.SetServiceConfiguration(0,hscUrlAclInfo,@Config,Sizeof(Config));
        end;
        if (Error<>NO_ERROR) and (Error<>ERROR_ALREADY_EXISTS) then
          raise EHttpApiServer.Create(hSetServiceConfiguration,Error);
        result := ''; // success
      finally
        Http.Terminate(HTTP_INITIALIZE_CONFIG);
      end;
    end;
  except
    on E: Exception do
      result := E.Message;
  end;
end;

procedure THttpApiServer.Clone(ChildThreadCount: integer);
var i: integer;
begin
  if (fReqQueue=0) or not Assigned(OnRequest) or (ChildThreadCount<=0) then
    exit; // nothing to clone (need a queue and a process event)
  if ChildThreadCount>256 then
    ChildThreadCount := 256; // not worth adding
  for i := 1 to ChildThreadCount do
    fClones.Add(THttpApiServer.CreateClone(self));
end;

function THttpApiServer.GetAPIVersion: string;
begin
  result := Format('HTTP API %d.%d',[Http.Version.MajorVersion,Http.Version.MinorVersion]);
end;

procedure THttpApiServer.SetOnTerminate(const Event: TNotifyThreadEvent);
var i: integer;
begin
  inherited SetOnTerminate(Event);
  if (Clones<>nil) and (fOwner=nil) then
    for i := 0 to Clones.Count-1 do
      THttpApiServer(Clones[i]).OnHttpThreadTerminate := Event;
end;

constructor THttpApiServer.Create(CreateSuspended: Boolean; QueueName: SynUnicode;
  OnStart,OnStop: TNotifyThreadEvent; const ProcessName: SockString);
var bindInfo: HTTP_BINDING_INFO;
begin
  SetLength(fLogDataStorage,sizeof(HTTP_LOG_FIELDS_DATA)); // should be done 1st
  inherited Create(true,OnStart,OnStop,ProcessName);
  HttpApiInitialize; // will raise an exception in case of failure
  EHttpApiServer.RaiseOnError(hInitialize,
    Http.Initialize(Http.Version,HTTP_INITIALIZE_SERVER));
  if Http.Version.MajorVersion>1 then begin
    EHttpApiServer.RaiseOnError(hCreateServerSession,Http.CreateServerSession(
      Http.Version,fServerSessionID));
    EHttpApiServer.RaiseOnError(hCreateUrlGroup,Http.CreateUrlGroup(
      fServerSessionID,fUrlGroupID));
    if QueueName='' then
      BinToHexDisplayW(@fServerSessionID,SizeOf(fServerSessionID),QueueName);
    EHttpApiServer.RaiseOnError(hCreateRequestQueue,Http.CreateRequestQueue(
      Http.Version,pointer(QueueName),nil,0,fReqQueue));
    bindInfo.Flags := 1;
    bindInfo.RequestQueueHandle := FReqQueue;
    EHttpApiServer.RaiseOnError(hSetUrlGroupProperty,Http.SetUrlGroupProperty(
      fUrlGroupID,HttpServerBindingProperty,@bindInfo,SizeOf(bindInfo)));
  end else
    EHttpApiServer.RaiseOnError(hCreateHttpHandle,Http.CreateHttpHandle(fReqQueue));
  fClones := TObjectList.Create;
  fReceiveBufferSize := 1048576; // i.e. 1 MB
  if not CreateSuspended then
    Suspended := False;
end;

constructor THttpApiServer.CreateClone(From: THttpApiServer);
begin
  SetLength(fLogDataStorage,sizeof(HTTP_LOG_FIELDS_DATA));
  inherited Create(false,From.fOnHttpThreadStart,From.fOnTerminate,From.ProcessName);
  fOwner := From;
  fReqQueue := From.fReqQueue;
  fOnRequest := From.fOnRequest;
  fCompress := From.fCompress;
  fCompressAcceptEncoding := From.fCompressAcceptEncoding;
  fReceiveBufferSize := From.fReceiveBufferSize;
  if From.fLogData<>nil then
    fLogData := pointer(fLogDataStorage);
  SetServerName(From.fServerName);
  fLoggingServiceName := From.fLoggingServiceName;
end;

destructor THttpApiServer.Destroy;
var i: integer;
begin
  {$ifdef LVCL}
  Terminate; // for Execute to be notified about end of process
  {$endif}
  try
    if (fClones<>nil) and (Http.Module<>0) then begin  // fClones=nil for clone threads
      if fReqQueue<>0 then begin
        if Http.Version.MajorVersion>1 then begin
         if fUrlGroupID<>0 then begin
           Http.RemoveUrlFromUrlGroup(fUrlGroupID,nil,HTTP_URL_FLAG_REMOVE_ALL);
           Http.CloseUrlGroup(fUrlGroupID);
           fUrlGroupID := 0;
         end;
         CloseHandle(FReqQueue);
         if fServerSessionID<>0 then begin
           Http.CloseServerSession(fServerSessionID);
           fServerSessionID := 0;
         end;
        end else begin
          for i := 0 to high(fRegisteredUnicodeUrl) do
            Http.RemoveUrl(fReqQueue,pointer(fRegisteredUnicodeUrl[i]));
          CloseHandle(fReqQueue); // will break all THttpApiServer.Execute
        end;
        fReqQueue := 0;
        FreeAndNil(fClones);
        Http.Terminate(HTTP_INITIALIZE_SERVER);
      end;
    end;
    {$ifdef LVCL}
    for i := 1 to 500 do
      if fExecuteFinished then
        break else
        SleepHiRes(10);
    {$endif}
  finally
    inherited Destroy;
  end;
end;

procedure GetDomainUserNameFromToken(UserToken: THandle; var result: SockString);
var Buffer: array[0..511] of byte;
    BufferSize, UserSize, DomainSize: DWORD;
    UserInfo: PSIDAndAttributes;
    NameUse: {$ifdef FPC}SID_NAME_USE{$else}Cardinal{$endif};
    tmp: SynUnicode;
    P: PWideChar;
begin
   if not GetTokenInformation(UserToken,TokenUser,@Buffer,SizeOf(Buffer),BufferSize) then
     exit;
   UserInfo := @Buffer;
   UserSize := 0;
   DomainSize := 0;
   LookupAccountSidW(nil,UserInfo^.Sid,nil,UserSize,nil,DomainSize,NameUse);
   if (UserSize=0) or (DomainSize=0) then
     exit;
   SetLength(tmp,UserSize+DomainSize-1);
   P := pointer(tmp);
   if not LookupAccountSidW(nil,UserInfo^.Sid,P+DomainSize,UserSize,P,DomainSize,NameUse) then
     exit;
   P[DomainSize] := '\';
   result := {$ifdef UNICODE}UTF8String{$else}UTF8Encode{$endif}(tmp);
end;


procedure THttpApiServer.Execute;
type
  TVerbText = array[hvOPTIONS..hvSEARCH] of SockString;
const
  VERB_TEXT: TVerbText = (
    'OPTIONS','GET','HEAD','POST','PUT','DELETE','TRACE','CONNECT','TRACK',
    'MOVE','COPY','PROPFIND','PROPPATCH','MKCOL','LOCK','UNLOCK','SEARCH');
var Req: PHTTP_REQUEST;
    ReqID: HTTP_REQUEST_ID;
    ReqBuf, RespBuf, RemoteIP: SockString;
    i: integer;
    flags, bytesRead, bytesSent: cardinal;
    Err: HRESULT;
    InCompressAccept: THttpSocketCompressSet;
    InContentLength, InContentLengthChunk, InContentLengthRead: cardinal;
    InContentEncoding, InAcceptEncoding, Range: SockString;
    OutContentEncoding, OutStatus: SockString;
    Context: THttpServerRequest;
    FileHandle: THandle;
    Resp: PHTTP_RESPONSE;
    BufRead, R: PAnsiChar;
    Heads: HTTP_UNKNOWN_HEADERs;
    RangeStart, RangeLength: Int64;
    DataChunkInMemory: HTTP_DATA_CHUNK_INMEMORY;
    DataChunkFile: HTTP_DATA_CHUNK_FILEHANDLE;
    CurrentLog: PHTTP_LOG_FIELDS_DATA;
    Verbs: TVerbText; // to avoid memory allocation

  procedure SendError(StatusCode: cardinal; const ErrorMsg: string; E: Exception=nil);
  var Msg: string;
  begin
    try
      Resp^.SetStatus(StatusCode,OutStatus);
      CurrentLog^.ProtocolStatus := StatusCode;
      Msg := format(
        '<html><body style="font-family:verdana;"><h1>Server Error %d: %s</h1><p>',
        [StatusCode,OutStatus]);
      if E<>nil then
        Msg := Msg+string(E.ClassName)+' Exception raised:<br>';
      Resp^.SetContent(DataChunkInMemory,UTF8String(Msg)+HtmlEncode(
        {$ifdef UNICODE}UTF8String{$else}UTF8Encode{$endif}(ErrorMsg))
        {$ifndef NOXPOWEREDNAME}+'</p><p><small>'+XPOWEREDVALUE{$endif},
        'text/html; charset=utf-8');
      Http.SendHttpResponse(fReqQueue,
        Req^.RequestId,0,Resp^,nil,bytesSent,nil,0,nil,fLogData);
    except
      on Exception do
        ; // ignore any HttpApi level errors here (client may crashed)
    end;
  end;

begin
  // THttpServerGeneric thread preparation: launch any OnHttpThreadStart event
  NotifyThreadStart(self);
  // reserve working buffers
  SetLength(Heads,64);
  SetLength(RespBuf,sizeof(Resp^));
  Resp := pointer(RespBuf);
  SetLength(ReqBuf,16384+sizeof(HTTP_REQUEST)); // space for Req^ + 16 KB of headers
  Req := pointer(ReqBuf);
  CurrentLog := pointer(fLogDataStorage);
  Verbs := VERB_TEXT;
  Context := THttpServerRequest.Create(self,0,self);
  try
    // main loop
    ReqID := 0;
    Context.fServer := self;
    repeat
      Context.fInContent := ''; // release input/output body buffers ASAP
      Context.fOutContent := '';
      // retrieve next pending request, and read its headers
      fillchar(Req^,sizeof(HTTP_REQUEST),0);
      Err := Http.ReceiveHttpRequest(fReqQueue,ReqID,0,Req^,length(ReqBuf),bytesRead);
      if Terminated then
        break;
      case Err of
      NO_ERROR:
      try
        // parse method and headers
        Context.fConnectionID := Req^.ConnectionId;
        Context.fURL := Req^.pRawUrl;
        if Req^.Verb in [low(Verbs)..high(Verbs)] then
          Context.fMethod := Verbs[Req^.Verb] else
          SetString(Context.fMethod,Req^.pUnknownVerb,Req^.UnknownVerbLength);
        with Req^.Headers.KnownHeaders[reqContentType] do
          SetString(Context.fInContentType,pRawValue,RawValueLength);
        with Req^.Headers.KnownHeaders[reqAcceptEncoding] do
          SetString(InAcceptEncoding,pRawValue,RawValueLength);
        InCompressAccept := ComputeContentEncoding(fCompress,pointer(InAcceptEncoding));
        Context.fUseSSL := Req^.pSslInfo<>nil;
        Context.fInHeaders := RetrieveHeaders(Req^,RemoteIP);
        // retrieve any SetAuthenticationSchemes() information
        if byte(fAuthenticationSchemes)<>0 then // set only with HTTP API 2.0
          for i := 0 to Req^.RequestInfoCount-1 do
          if Req^.pRequestInfo^[i].InfoType=HttpRequestInfoTypeAuth then
            with PHTTP_REQUEST_AUTH_INFO(Req^.pRequestInfo^[i].pInfo)^ do
            case AuthStatus of
            HttpAuthStatusSuccess:
            if AuthType>HttpRequestAuthTypeNone then begin
              byte(Context.fAuthenticationStatus) := ord(AuthType)+1;
              if AccessToken<>0 then
                GetDomainUserNameFromToken(AccessToken,Context.fAuthenticatedUser);
            end;
            HttpAuthStatusFailure:
              Context.fAuthenticationStatus := hraFailed;
            end;
        // retrieve body
        if HTTP_REQUEST_FLAG_MORE_ENTITY_BODY_EXISTS and Req^.Flags<>0 then begin
          with Req^.Headers.KnownHeaders[reqContentLength] do
            InContentLength := GetCardinal(pRawValue,pRawValue+RawValueLength);
          if InContentLength<>0 then begin
            SetLength(Context.fInContent,InContentLength);
            BufRead := pointer(Context.InContent);
            InContentLengthRead := 0;
            repeat
              BytesRead := 0;
              if Http.Version.MajorVersion>1 then // speed optimization for Vista+
                flags := HTTP_RECEIVE_REQUEST_ENTITY_BODY_FLAG_FILL_BUFFER else
                flags := 0;
              InContentLengthChunk := InContentLength-InContentLengthRead;
              if (fReceiveBufferSize>=1024) and (InContentLengthChunk>fReceiveBufferSize) then
                InContentLengthChunk := fReceiveBufferSize;
              Err := Http.ReceiveRequestEntityBody(fReqQueue,Req^.RequestId,flags,
                BufRead,InContentLengthChunk,BytesRead);
              if Terminated then
                exit;
              inc(InContentLengthRead,BytesRead);
              if Err=ERROR_HANDLE_EOF then begin
                if InContentLengthRead<InContentLength then
                  SetLength(Context.fInContent,InContentLengthRead);
                Err := NO_ERROR;
                break; // should loop until returns ERROR_HANDLE_EOF
              end;
              if Err<>NO_ERROR then
                break;
              inc(BufRead,BytesRead);
            until InContentLengthRead=InContentLength;
            if Err<>NO_ERROR then begin
              SendError(406,SysErrorMessagePerModule(Err,HTTPAPI_DLL));
              continue;
            end;
            with Req^.Headers.KnownHeaders[reqContentEncoding] do
            if RawValueLength<>0 then begin
              SetString(InContentEncoding,pRawValue,RawValueLength);
              for i := 0 to high(fCompress) do
                if fCompress[i].Name=InContentEncoding then begin
                  fCompress[i].Func(Context.fInContent,false); // uncompress
                  break;
                end;
            end;
          end;
        end;
        try
          // compute response
          Context.OutContent := '';
          Context.OutContentType := '';
          Context.OutCustomHeaders := '';
          fillchar(Resp^,sizeof(Resp^),0);
          Resp^.SetStatus(Request(Context),OutStatus);
          if Terminated then
            exit;
          // update log information
          if Http.Version.MajorVersion>=2 then
            with Req^,CurrentLog^ do begin
              MethodNum := Verb;
              UriStemLength := CookedUrl.AbsPathLength;
              UriStem := CookedUrl.pAbsPath;
              with Headers.KnownHeaders[reqUserAgent] do begin
                UserAgentLength := RawValueLength;
                UserAgent := pRawValue;
              end;
              with Headers.KnownHeaders[reqHost] do begin
                HostLength := RawValueLength;
                Host := pRawValue;
              end;
              with Headers.KnownHeaders[reqReferrer] do begin
                ReferrerLength := RawValueLength;
                Referrer := pRawValue;
              end;
              ProtocolStatus := Resp^.StatusCode;
              ClientIp := pointer(RemoteIP);
              ClientIpLength := length(RemoteIP);
              Method := pointer(Context.fMethod);
              MethodLength := length(Context.fMethod);
              UserName := pointer(Context.fAuthenticatedUser);
              UserNameLength := Length(Context.fAuthenticatedUser);
            end;
          // send response
          Resp^.Version := Req^.Version;
          Resp^.SetHeaders(pointer(Context.OutCustomHeaders),Heads);
          if fCompressAcceptEncoding<>'' then
            Resp^.AddCustomHeader(pointer(fCompressAcceptEncoding),Heads);
          with Resp^.Headers.KnownHeaders[respServer], CurrentLog^ do begin
            pRawValue := ServerName;
            RawValueLength := ServerNameLength;
          end;
          if Context.OutContentType=HTTP_RESP_STATICFILE then begin
            // response is file -> OutContent is UTF-8 file name to be served
            FileHandle := FileOpen(
              {$ifdef UNICODE}UTF8ToUnicodeString{$else}Utf8ToAnsi{$endif}(Context.OutContent),
              fmOpenRead or fmShareDenyNone);
            if PtrInt(FileHandle)<0 then begin
              SendError(STATUS_NOTFOUND,SysErrorMessage(GetLastError));
              continue;
            end;
            try // http.sys will serve then close the file from kernel
              DataChunkFile.DataChunkType := hctFromFileHandle;
              DataChunkFile.FileHandle := FileHandle;
              flags := 0;
              DataChunkFile.ByteRange.StartingOffset.QuadPart := 0;
              Int64(DataChunkFile.ByteRange.Length.QuadPart) := -1; // to eof
              with Req^.Headers.KnownHeaders[reqRange] do
                if (RawValueLength>6) and IdemPChar(pRawValue,'BYTES=') and
                   (pRawValue[6] in ['0'..'9']) then begin
                  SetString(Range,pRawValue+6,RawValueLength-6); // need #0 end
                  R := pointer(Range);
                  RangeStart := GetNextItemUInt64(R);
                  if R^='-' then begin
                    inc(R);
                    flags := HTTP_SEND_RESPONSE_FLAG_PROCESS_RANGES;
                    DataChunkFile.ByteRange.StartingOffset := ULARGE_INTEGER(RangeStart);
                    if R^ in ['0'..'9'] then begin
                      RangeLength := GetNextItemUInt64(R)-RangeStart+1;
                      if RangeLength>=0 then // "bytes=0-499" -> start=0, len=500
                        DataChunkFile.ByteRange.Length := ULARGE_INTEGER(RangeLength);
                    end; // "bytes=1000-" -> start=1000, len=-1 (to eof)
                  end;
                end;
              Resp^.EntityChunkCount := 1;
              Resp^.pEntityChunks := @DataChunkFile;
              Http.SendHttpResponse(fReqQueue,
                Req^.RequestId,flags,Resp^,nil,bytesSent,nil,0,nil,fLogData);
            finally
              FileClose(FileHandle);
            end;
          end else begin
            if Context.OutContentType=HTTP_RESP_NORESPONSE then
              Context.OutContentType := ''; // true HTTP always expects a response
            // response is in OutContent -> send it from memory
            if fCompress<>nil then begin
              with Resp^.Headers.KnownHeaders[reqContentEncoding] do
              if RawValueLength=0 then begin
                // no previous encoding -> try if any compression
                OutContentEncoding := CompressDataAndGetHeaders(InCompressAccept,
                  fCompress,Context.OutContentType,Context.fOutContent);
                pRawValue := pointer(OutContentEncoding);
                RawValueLength := length(OutContentEncoding);
              end;
            end;
            Resp^.SetContent(DataChunkInMemory,Context.OutContent,Context.OutContentType);
            EHttpApiServer.RaiseOnError(hSendHttpResponse,Http.SendHttpResponse(
              fReqQueue,Req^.RequestId,0,Resp^,nil,bytesSent,nil,0,nil,fLogData));
          end;
        except
          on E: Exception do
            // handle any exception raised during process: show must go on!
            if not E.InheritsFrom(EHttpApiServer) or // ensure still connected
               (EHttpApiServer(E).LastError<>HTTPAPI_ERROR_NONEXISTENTCONNECTION) then
              SendError(STATUS_SERVERERROR,E.Message,E);
        end;
      finally
        ReqId := 0; // reset Request ID to handle the next pending request
      end;
      ERROR_MORE_DATA: begin
        // input buffer was too small to hold the request headers
        // -> increase buffer size and call the API again
        ReqID := Req^.RequestId;
        SetLength(ReqBuf,bytesRead);
        Req := pointer(ReqBuf);
      end;
      ERROR_CONNECTION_INVALID:
        if ReqID=0 then
          break else
          // TCP connection was corrupted by the peer -> ignore + next request
          ReqID := 0;
      else break; // unhandled Err value
      end;
    until Terminated;
  finally
    Context.Free;
    fExecuteFinished := true;
  end;
end;

procedure THttpApiServer.RegisterCompress(aFunction: THttpSocketCompress;
  aCompressMinSize: integer=1024);
var i: integer;
begin
  inherited;
  if fClones<>nil then
    for i := 0 to fClones.Count-1 do
      THttpApiServer(fClones.List{$ifdef FPC}^{$endif}[i]).
        RegisterCompress(aFunction,aCompressMinSize);
end;

function THttpApiServer.GetHTTPQueueLength: Cardinal;
var returnLength: ULONG;
begin
  if (Http.Version.MajorVersion<2) or (self=nil) then
    result := 0 else begin
    if fOwner<>nil then
      self := fOwner;
    if fReqQueue=0 then
      result := 0 else
      EHttpApiServer.RaiseOnError(hQueryRequestQueueProperty,
        Http.QueryRequestQueueProperty(fReqQueue,HttpServerQueueLengthProperty,
          @Result, sizeof(Result), 0, @returnLength, nil));
  end;
end;

procedure THttpApiServer.SetHTTPQueueLength(aValue: Cardinal);
begin
  if Http.Version.MajorVersion<2 then
    raise EHttpApiServer.Create(hSetRequestQueueProperty, ERROR_OLD_WIN_VERSION);
  if (self<>nil) and (fReqQueue<>0) then
    EHttpApiServer.RaiseOnError(hSetRequestQueueProperty,
      Http.SetRequestQueueProperty(fReqQueue,HttpServerQueueLengthProperty,
        @aValue, sizeof(aValue), 0, nil));
end;

function THttpApiServer.GetRegisteredUrl: SynUnicode;
var i: integer;
begin
  if fRegisteredUnicodeUrl=nil then
    result := '' else
    result := fRegisteredUnicodeUrl[0];
  for i := 1 to high(fRegisteredUnicodeUrl) do
    result := result+','+fRegisteredUnicodeUrl[i];
end;

function THttpApiServer.GetCloned: boolean;
begin
  result := (fOwner<>nil);
end;

procedure THttpApiServer.SetMaxBandwidth(aValue: Cardinal);
var qosInfo: HTTP_QOS_SETTING_INFO;
    limitInfo: HTTP_BANDWIDTH_LIMIT_INFO;
begin
  if Http.Version.MajorVersion<2 then
    raise EHttpApiServer.Create(hSetUrlGroupProperty,ERROR_OLD_WIN_VERSION);
  if (self<>nil) and (fUrlGroupID<>0) then begin
    if AValue=0 then
      limitInfo.MaxBandwidth := HTTP_LIMIT_INFINITE else
    if AValue<HTTP_MIN_ALLOWED_BANDWIDTH_THROTTLING_RATE then
      limitInfo.MaxBandwidth := HTTP_MIN_ALLOWED_BANDWIDTH_THROTTLING_RATE else
      limitInfo.MaxBandwidth := aValue;
    limitInfo.Flags := 1;
    qosInfo.QosType := HttpQosSettingTypeBandwidth;
    qosInfo.QosSetting := @limitInfo;
    EHttpApiServer.RaiseOnError(hSetServerSessionProperty,
      Http.SetServerSessionProperty(fServerSessionID, HttpServerQosProperty,
        @qosInfo, SizeOf(qosInfo)));
    EHttpApiServer.RaiseOnError(hSetUrlGroupProperty,
      Http.SetUrlGroupProperty(fUrlGroupID, HttpServerQosProperty,
        @qosInfo, SizeOf(qosInfo)));
  end;
end;

function THttpApiServer.GetMaxBandwidth: Cardinal;
var qosInfoGet: record
      qosInfo: HTTP_QOS_SETTING_INFO;
      limitInfo: HTTP_BANDWIDTH_LIMIT_INFO;
    end;
begin
  if (Http.Version.MajorVersion<2) or (self=nil) then begin
    result := 0;
    exit;
  end;
  if fOwner<>nil then
    self := fOwner;
  if fUrlGroupID=0 then begin
    result := 0;
    exit;
  end;
  qosInfoGet.qosInfo.QosType := HttpQosSettingTypeBandwidth;
  qosInfoGet.qosInfo.QosSetting := @qosInfoGet.limitInfo;
  EHttpApiServer.RaiseOnError(hQueryUrlGroupProperty,
    Http.QueryUrlGroupProperty(fUrlGroupID, HttpServerQosProperty,
      @qosInfoGet, SizeOf(qosInfoGet)));
  Result := qosInfoGet.limitInfo.MaxBandwidth;
end;

function THttpApiServer.GetMaxConnections: Cardinal;
var qosInfoGet: record
      qosInfo: HTTP_QOS_SETTING_INFO;
      limitInfo: HTTP_CONNECTION_LIMIT_INFO;
    end;
    returnLength: ULONG;
begin
  if (Http.Version.MajorVersion<2) or (self=nil) then begin
    result := 0;
    exit;
  end;
  if fOwner<>nil then
    self := fOwner;
  if fUrlGroupID=0 then begin
    result := 0;
    exit;
  end;
  qosInfoGet.qosInfo.QosType := HttpQosSettingTypeConnectionLimit;
  qosInfoGet.qosInfo.QosSetting := @qosInfoGet.limitInfo;
  EHttpApiServer.RaiseOnError(hQueryUrlGroupProperty,
    Http.QueryUrlGroupProperty(fUrlGroupID, HttpServerQosProperty,
      @qosInfoGet, SizeOf(qosInfoGet), @returnLength));
  Result := qosInfoGet.limitInfo.MaxConnections;
end;

procedure THttpApiServer.SetMaxConnections(aValue: Cardinal);
var qosInfo: HTTP_QOS_SETTING_INFO;
    limitInfo: HTTP_CONNECTION_LIMIT_INFO;
begin
  if Http.Version.MajorVersion<2 then
    raise EHttpApiServer.Create(hSetUrlGroupProperty, ERROR_OLD_WIN_VERSION);
  if (self<>nil) and (fUrlGroupID<>0) then begin
    if AValue = 0 then
      limitInfo.MaxConnections := HTTP_LIMIT_INFINITE else
      limitInfo.MaxConnections := aValue;
    limitInfo.Flags := 1;
    qosInfo.QosType := HttpQosSettingTypeConnectionLimit;
    qosInfo.QosSetting := @limitInfo;
    EHttpApiServer.RaiseOnError(hSetUrlGroupProperty,
      Http.SetUrlGroupProperty(fUrlGroupID, HttpServerQosProperty,
        @qosInfo, SizeOf(qosInfo)));
  end;
end;

function THttpApiServer.HasAPI2: boolean;
begin
  result := Http.Version.MajorVersion>=2;
end;

function THttpApiServer.GetLogging: boolean;
begin
  result := (fLogData<>nil);
end;

procedure THttpApiServer.LogStart(const aLogFolder: TFileName;
  aType: THttpApiLoggingType; const aSoftwareName: TFileName;
  aRolloverType: THttpApiLoggingRollOver; aRolloverSize: cardinal;
  aLogFields: THttpApiLogFields; aFlags: THttpApiLoggingFlags);
var logInfo : HTTP_LOGGING_INFO;
    folder,software: SynUnicode;
begin
  if (self=nil) or (fOwner<>nil) then
    exit;
  if Http.Version.MajorVersion<2 then
    raise EHttpApiServer.Create(hSetUrlGroupProperty,ERROR_OLD_WIN_VERSION);
  fLogData := nil; // disable any previous logging
  fillchar(logInfo,SizeOf(logInfo),0);
  logInfo.Flags := 1;
  logInfo.LoggingFlags := byte(aFlags);
  if aLogFolder='' then
    raise EHttpApiServer.CreateFmt('LogStart(aLogFolder="")',[]);
  if length(aLogFolder)>212 then
    // http://msdn.microsoft.com/en-us/library/windows/desktop/aa364532
    raise EHttpApiServer.CreateFmt('aLogFolder is too long for LogStart(%s)',[aLogFolder]);
  folder := SynUnicode(aLogFolder);
  software := SynUnicode(aSoftwareName);
  logInfo.SoftwareNameLength := length(software)*2;
  logInfo.SoftwareName := pointer(software);
  logInfo.DirectoryNameLength := length(folder)*2;
  logInfo.DirectoryName := pointer(folder);
  logInfo.Format := HTTP_LOGGING_TYPE(aType);
  if aType=hltNCSA then
    aLogFields := [hlfDate..hlfSubStatus];
  logInfo.Fields := integer(aLogFields);
  logInfo.RolloverType := HTTP_LOGGING_ROLLOVER_TYPE(aRolloverType);
  if aRolloverType=hlrSize then
    logInfo.RolloverSize := aRolloverSize;
  EHttpApiServer.RaiseOnError(hSetUrlGroupProperty,
    Http.SetUrlGroupProperty(fUrlGroupID, HttpServerLoggingProperty,
      @logInfo, SizeOf(logInfo)));
  // on success, update the acutal
  fLogData := pointer(fLogDataStorage);
end;

procedure THttpApiServer.LogStop;
var i: integer;
begin
  if (self=nil) or (fClones=nil) or (fLogData=nil) then
    exit;
  fLogData := nil;
  for i := 0 to fClones.Count-1 do
    THttpApiServer(fClones.List{$ifdef FPC}^{$endif}[i]).fLogData := nil;
end;

procedure THttpApiServer.SetReceiveBufferSize(Value: cardinal);
var i: integer;
begin
  fReceiveBufferSize := Value;
  if fClones<>nil then // parameter shared by all clones
    for i := 0 to fClones.Count-1 do
      THttpApiServer(fClones.List{$ifdef FPC}^{$endif}[i]).fReceiveBufferSize := Value;
end;

procedure THttpApiServer.SetServerName(const aName: SockString);
var i: integer;
begin
  inherited SetServerName(aName);
  PHTTP_LOG_FIELDS_DATA(fLogDataStorage)^.ServerNameLength := Length(fServerName);
  PHTTP_LOG_FIELDS_DATA(fLogDataStorage)^.ServerName := pointer(fServerName);
  if fClones<>nil then // server name is shared by all clones
    for i := 0 to fClones.Count-1 do
      THttpApiServer(fClones.List{$ifdef FPC}^{$endif}[i]).ServerName := aName;
end;

procedure THttpApiServer.SetLoggingServiceName(const aName: SockString);
begin
  if self=nil then
    exit;
  fLoggingServiceName := aName;
  PHTTP_LOG_FIELDS_DATA(fLogDataStorage)^.ServiceNameLength := Length(fLoggingServiceName);
  PHTTP_LOG_FIELDS_DATA(fLogDataStorage)^.ServiceName := pointer(fLoggingServiceName);
end;

procedure THttpApiServer.SetAuthenticationSchemes(schemes: THttpApiRequestAuthentications;
  const DomainName, Realm: SynUnicode);
var authInfo: HTTP_SERVER_AUTHENTICATION_INFO;
begin
  if (self=nil) or (fOwner<>nil) then
    exit;
  if Http.Version.MajorVersion<2 then
    raise EHttpApiServer.Create(hSetUrlGroupProperty,ERROR_OLD_WIN_VERSION);
  fAuthenticationSchemes := schemes;
  FillChar(authInfo,SizeOf(authInfo),0);
  authInfo.Flags := 1;
  authInfo.AuthSchemes := byte(schemes);
  authInfo.ReceiveMutualAuth := true;
  if haBasic in schemes then
    with authInfo.BasicParams do begin
      RealmLength := Length(Realm);
      Realm := pointer(Realm);
    end;
  if haDigest in schemes then
    with authInfo.DigestParams do begin
      DomainNameLength := Length(DomainName);
      DomainName := pointer(DomainName);
      RealmLength := Length(Realm);
      Realm := pointer(Realm);
    end;
  EHttpApiServer.RaiseOnError(hSetUrlGroupProperty,
    Http.SetUrlGroupProperty(fUrlGroupID, HttpServerAuthenticationProperty,
      @authInfo, SizeOf(authInfo)));
end;

procedure THttpApiServer.SetTimeOutLimits(aEntityBody, aDrainEntityBody,
  aRequestQueue, aIdleConnection, aHeaderWait, aMinSendRate: cardinal);
var timeoutInfo: HTTP_TIMEOUT_LIMIT_INFO;
begin
  if (self=nil) or (fOwner<>nil) then
    exit;
  if Http.Version.MajorVersion<2 then
    raise EHttpApiServer.Create(hSetUrlGroupProperty,ERROR_OLD_WIN_VERSION);
  FillChar(timeOutInfo,SizeOf(timeOutInfo),0);
  timeoutInfo.Flags := 1;
  timeoutInfo.EntityBody := aEntityBody;
  timeoutInfo.DrainEntityBody := aDrainEntityBody;
  timeoutInfo.RequestQueue := aRequestQueue;
  timeoutInfo.IdleConnection := aIdleConnection;
  timeoutInfo.HeaderWait := aHeaderWait;
  timeoutInfo.MinSendRate := aMinSendRate;
  EHttpApiServer.RaiseOnError(hSetUrlGroupProperty,
    Http.SetUrlGroupProperty(fUrlGroupID, HttpServerTimeoutsProperty,
      @timeoutInfo, SizeOf(timeoutInfo)));
end;


{ HTTP_RESPONSE }

procedure HTTP_RESPONSE.SetContent(var DataChunk: HTTP_DATA_CHUNK_INMEMORY;
  const Content, ContentType: SockString);
begin
  fillchar(DataChunk,sizeof(DataChunk),0);
  if Content='' then
    exit;
  DataChunk.DataChunkType := hctFromMemory;
  DataChunk.pBuffer := pointer(Content);
  DataChunk.BufferLength := length(Content);
  EntityChunkCount := 1;
  pEntityChunks := @DataChunk;
  Headers.KnownHeaders[reqContentType].RawValueLength := length(ContentType);
  Headers.KnownHeaders[reqContentType].pRawValue := pointer(ContentType);
end;

function HTTP_RESPONSE.AddCustomHeader(P: PAnsiChar; var UnknownHeaders: HTTP_UNKNOWN_HEADERs): PAnsiChar;
const KNOWNHEADERS: array[reqCacheControl..respWwwAuthenticate] of PAnsiChar = (
    'CACHE-CONTROL:','CONNECTION:','DATE:','KEEP-ALIVE:','PRAGMA:','TRAILER:',
    'TRANSFER-ENCODING:','UPGRADE:','VIA:','WARNING:','ALLOW:','CONTENT-LENGTH:',
    'CONTENT-TYPE:','CONTENT-ENCODING:','CONTENT-LANGUAGE:','CONTENT-LOCATION:',
    'CONTENT-MD5:','CONTENT-RANGE:','EXPIRES:','LAST-MODIFIED:',
    'ACCEPT-RANGES:','AGE:','ETAG:','LOCATION:','PROXY-AUTHENTICATE:',
    'RETRY-AFTER:','SERVER:','SET-COOKIE:','VARY:','WWW-AUTHENTICATE:');
var UnknownName: PAnsiChar;
    i: integer;
begin
  i := IdemPCharArray(P,KNOWNHEADERS);
  if i>=0 then
  with Headers.KnownHeaders[THttpHeader(i)] do begin
    while P^<>':' do inc(P);
    inc(P); // jump ':'
    while P^=' ' do inc(P);
    pRawValue := P;
    while P^>=' ' do inc(P);
    RawValueLength := P-pRawValue;
  end else begin
    UnknownName := P;
    while (P^>=' ') and (P^<>':') do inc(P);
    if P^=':' then
      with UnknownHeaders[Headers.UnknownHeaderCount] do begin
        pName := UnknownName;
        NameLength := P-pName;
        repeat inc(P) until P^<>' ';
        pRawValue := P;
        while P^>=' ' do inc(P);
        RawValueLength := P-pRawValue;
        if Headers.UnknownHeaderCount=high(UnknownHeaders) then begin
          SetLength(UnknownHeaders,Headers.UnknownHeaderCount+32);
          Headers.pUnknownHeaders := pointer(UnknownHeaders);
        end;
        inc(Headers.UnknownHeaderCount);
      end else
      while P^>=' ' do inc(P);
  end;
  result := P;
end;

procedure HTTP_RESPONSE.SetHeaders(P: PAnsiChar; var UnknownHeaders: HTTP_UNKNOWN_HEADERs);
{$ifndef NOXPOWEREDNAME}
const XPN: PAnsiChar = XPOWEREDNAME;
      XPV: PAnsiChar = XPOWEREDVALUE;
{$endif}
begin
  Headers.pUnknownHeaders := pointer(UnknownHeaders);
  {$ifdef NOXPOWEREDNAME}
  Headers.UnknownHeaderCount := 0;
  {$else}
  with UnknownHeaders[0] do begin
    pName := XPN;
    NameLength := length(XPOWEREDNAME);
    pRawValue := XPV;
    RawValueLength := length(XPOWEREDVALUE);
  end;
  Headers.UnknownHeaderCount := 1;
  {$endif}
  if P<>nil then
  repeat
    while P^ in [#13,#10] do inc(P);
    if P^=#0 then
      break;
    P := AddCustomHeader(P,UnknownHeaders);
  until false;
end;

procedure HTTP_RESPONSE.SetStatus(code: integer; var OutStatus: SockString);
begin
  StatusCode := code;
  OutStatus := StatusCodeToReason(code);
  ReasonLength := length(OutStatus);
  pReason := pointer(OutStatus);
end;

const
  HTTP_LOG_FIELD_TEST_SUB_STATUS: THttpApiLogFields = [hlfSubStatus];

{$endif MSWINDOWS}

{ THttpRequest }

function THttpRequest.RegisterCompress(aFunction: THttpSocketCompress;
  aCompressMinSize: integer): boolean;
begin
  result := RegisterCompressFunc(fCompress,aFunction,fCompressAcceptEncoding,aCompressMinSize)<>'';
end;

constructor THttpRequest.Create(const aServer, aPort: SockString;
  aHttps: boolean; const aProxyName,aProxyByPass: SockString;
  ConnectionTimeOut,SendTimeout,ReceiveTimeout: DWORD);
begin
  fPort := GetCardinal(pointer(aPort));
  if fPort=0 then
    if aHttps then
      fPort := 443 else
      fPort := 80;
  fServer := aServer;
  fHttps := aHttps;
  fProxyName := aProxyName;
  fProxyByPass := aProxyByPass;
  fUserAgent := DefaultUserAgent(self);
  if ConnectionTimeOut=0 then
    ConnectionTimeOut := HTTP_DEFAULT_CONNECTTIMEOUT;
  if SendTimeout=0 then
    SendTimeout := HTTP_DEFAULT_SENDTIMEOUT;
  if ReceiveTimeout=0 then
    ReceiveTimeout := HTTP_DEFAULT_RECEIVETIMEOUT;
  InternalConnect(ConnectionTimeOut,SendTimeout,ReceiveTimeout); // raise an exception on error
end;

class function THttpRequest.InternalREST(const url,method,data,header: SockString;
  aIgnoreSSLCertificateErrors: Boolean; outHeaders: PSockString): SockString;
var URI: TURI;
    oh: SockString;
begin
  result := '';
  with URI do
  if From(url) then
  try
    with self.Create(Server,Port,Https,'','') do
    try
      IgnoreSSLCertificateErrors := aIgnoreSSLCertificateErrors;
      Request(Address,method,0,header,data,'',oh,result);
      if outHeaders<>nil then
        outHeaders^ := oh;
    finally
      Free;
    end;
  except
    result := '';
  end;
end;

class function THttpRequest.Get(const aURI,aHeader: SockString;
  aIgnoreSSLCertificateErrors: Boolean; outHeaders: PSockString): SockString;
begin
  result := InternalREST(aURI,'GET','',aHeader,aIgnoreSSLCertificateErrors,outHeaders);
end;

class function THttpRequest.Post(const aURI, aData, aHeader: SockString;
  aIgnoreSSLCertificateErrors: Boolean; outHeaders: PSockString): SockString;
begin
  result := InternalREST(aURI,'POST',aData,aHeader,aIgnoreSSLCertificateErrors,outHeaders);
end;

class function THttpRequest.Put(const aURI, aData, aHeader: SockString;
  aIgnoreSSLCertificateErrors: Boolean; outHeaders: PSockString): SockString;
begin
  result := InternalREST(aURI,'PUT',aData,aHeader,aIgnoreSSLCertificateErrors,outHeaders);
end;

class function THttpRequest.Delete(const aURI, aHeader: SockString;
  aIgnoreSSLCertificateErrors: Boolean; outHeaders: PSockString): SockString;
begin
  result := InternalREST(aURI,'DELETE','',aHeader,aIgnoreSSLCertificateErrors,outHeaders);
end;

function THttpRequest.Request(const url, method: SockString;
  KeepAlive: cardinal; const InHeader, InData, InDataType: SockString;
  out OutHeader, OutData: SockString): integer;
var aData, aDataEncoding, aAcceptEncoding, aURL: SockString;
    i: integer;
begin
  if (url='') or (url[1]<>'/') then
    aURL := '/'+url else // need valid url according to the HTTP/1.1 RFC
    aURL := url;
  fKeepAlive := KeepAlive;
  InternalCreateRequest(method,aURL); // should raise an exception on error
  try
    // common headers
    InternalAddHeader(InHeader);
    if InDataType<>'' then
      InternalAddHeader(SockString('Content-Type: ')+InDataType);
    // handle custom compression
    aData := InData;
    if integer(fCompressHeader)<>0 then begin
      aDataEncoding := CompressDataAndGetHeaders(fCompressHeader,fCompress,
        InDataType,aData);
      if aDataEncoding<>'' then
        InternalAddHeader(SockString('Content-Encoding: ')+aDataEncoding);
    end;
    if fCompressAcceptEncoding<>'' then
      InternalAddHeader(fCompressAcceptEncoding);
    // send request to remote server
    InternalSendRequest(aData);
    // retrieve status and headers
    result := InternalRetrieveAnswer(OutHeader,aDataEncoding,aAcceptEncoding,OutData);
    // handle incoming answer compression
    if OutData<>'' then begin
      if aDataEncoding<>'' then
        for i := 0 to high(fCompress) do
          with fCompress[i] do
          if Name=aDataEncoding then
            if Func(OutData,false)='' then
              raise ECrtSocket.CreateFmt('%s uncompress',[Name]) else
              break; // successfully uncompressed content
      if aAcceptEncoding<>'' then
        fCompressHeader := ComputeContentEncoding(fCompress,pointer(aAcceptEncoding));
    end;
  finally
    InternalCloseRequest;
  end;
end;


{$ifdef USEWININET}

{ ************ WinHttp / WinINet HTTP clients }

{ TWinHttpAPI }

const
  // while reading an HTTP response, read it in blocks of this size. 8K for now
  HTTP_RESP_BLOCK_SIZE = 8*1024;

function TWinHttpAPI.InternalRetrieveAnswer(
  var Header, Encoding, AcceptEncoding, Data: SockString): integer;
var Bytes, ContentLength, Read: DWORD;
    tmp: SockString;
begin // HTTP_QUERY* and WINHTTP_QUERY* do match -> common to TWinINet + TWinHTTP
    result := InternalGetInfo32(HTTP_QUERY_STATUS_CODE);
    Header := InternalGetInfo(HTTP_QUERY_RAW_HEADERS_CRLF);
    Encoding := InternalGetInfo(HTTP_QUERY_CONTENT_ENCODING);
    AcceptEncoding := InternalGetInfo(HTTP_QUERY_ACCEPT_ENCODING);
    // retrieve received content (if any)
    Read := 0;
    ContentLength := InternalGetInfo32(HTTP_QUERY_CONTENT_LENGTH);
    if Assigned(fOnDownload) then begin
      // download per-chunk using calback event
      Bytes := fOnDownloadChunkSize;
      if Bytes<=0 then
        Bytes := 65536; // 64KB seems fair enough by default
      SetLength(tmp,Bytes);
      repeat
        Bytes := InternalReadData(tmp,0);
        if Bytes=0 then
          break;
        inc(Read,Bytes);
        if not fOnDownload(self,Read,ContentLength,Bytes,pointer(tmp)^) then
          break; // returned false = aborted 
        if Assigned(fOnProgress) then
          fOnProgress(self,Read,ContentLength);
      until false;
    end else
    if ContentLength<>0 then begin
      // optimized version reading "Content-Length: xxx" bytes
      SetLength(Data,ContentLength);
      repeat
        Bytes := InternalReadData(Data,Read);
        if Bytes=0 then begin
          SetLength(Data,Read); // truncated content
          break;
        end;
        inc(Read,Bytes);
        if Assigned(fOnProgress) then
          fOnProgress(self,Read,ContentLength);
      until Read=ContentLength;
    end else begin
      // Content-Length not set: read response in blocks of HTTP_RESP_BLOCK_SIZE
      repeat
        SetLength(Data,Read+HTTP_RESP_BLOCK_SIZE);
        Bytes := InternalReadData(Data,Read);
        if Bytes=0 then
          break;
        inc(Read,Bytes);
        if Assigned(fOnProgress) then
          fOnProgress(self,Read,ContentLength);
      until false;
      SetLength(Data,Read);
    end;
end;


{ EWinINet }

constructor EWinINet.Create;
var dwError, tmpLen: DWORD;
    msg, tmp: string;
begin // see http://msdn.microsoft.com/en-us/library/windows/desktop/aa383884
  fLastError := GetLastError;
  msg := SysErrorMessagePerModule(fLastError,'wininet.dll');
  if fLastError=ERROR_INTERNET_EXTENDED_ERROR then begin
    InternetGetLastResponseInfo({$ifdef FPC}@{$endif}dwError,nil,tmpLen);
    if tmpLen > 0 then begin
      SetLength(tmp,tmpLen);
      InternetGetLastResponseInfo({$ifdef FPC}@{$endif}dwError,PChar(tmp),tmpLen);
      msg := msg+' ['+tmp+']';
    end;
  end;
  inherited CreateFmt('%s (%d)',[msg,fLastError]);
end;


{ TWinINet }

destructor TWinINet.Destroy;
begin
  if fConnection<>nil then
    InternetCloseHandle(FConnection);
  if fSession<>nil then
    InternetCloseHandle(FSession);
  inherited;
end;

procedure TWinINet.InternalAddHeader(const hdr: SockString);
begin
  if (hdr<>'') and not HttpAddRequestHeadersA(fRequest,
     Pointer(hdr), length(hdr), HTTP_ADDREQ_FLAG_COALESCE) then
    raise EWinINet.Create;
end;

procedure TWinINet.InternalCloseRequest;
begin
  if fRequest<>nil then begin
    InternetCloseHandle(fRequest);
    fRequest := nil;
  end;
end;

procedure TWinINet.InternalConnect(ConnectionTimeOut,SendTimeout,ReceiveTimeout: DWORD);
var OpenType: integer;
begin
  if fProxyName='' then
   OpenType := INTERNET_OPEN_TYPE_PRECONFIG else
   OpenType := INTERNET_OPEN_TYPE_PROXY;
  fSession := InternetOpenA(Pointer(fUserAgent), OpenType,
    pointer(fProxyName), pointer(fProxyByPass), 0);
  if fSession=nil then
    raise EWinINet.Create;
  InternetSetOption(fConnection,INTERNET_OPTION_CONNECT_TIMEOUT,
    @ConnectionTimeOut,SizeOf(ConnectionTimeOut));
  InternetSetOption(fConnection,INTERNET_OPTION_SEND_TIMEOUT,
    @SendTimeout,SizeOf(SendTimeout));
  InternetSetOption(fConnection,INTERNET_OPTION_RECEIVE_TIMEOUT,
    @ReceiveTimeout,SizeOf(ReceiveTimeout));
  fConnection := InternetConnectA(fSession, pointer(fServer), fPort, nil, nil,
    INTERNET_SERVICE_HTTP, 0, 0);
  if fConnection=nil then
    raise EWinINet.Create;
end;

function TWinINet.InternalGetInfo(Info: DWORD): SockString;
var dwSize, dwIndex: DWORD;
begin
  result := '';
  dwSize := 0;
  dwIndex := 0;
  if not HttpQueryInfoA(fRequest, Info, nil, dwSize, dwIndex) and
     (GetLastError=ERROR_INSUFFICIENT_BUFFER) then begin
    SetLength(result,dwSize-1);
    if not HttpQueryInfoA(fRequest, Info, pointer(result), dwSize, dwIndex) then
      result := '';
  end;
end;

function TWinINet.InternalGetInfo32(Info: DWORD): DWORD;
var dwSize, dwIndex: DWORD;
begin
  dwSize := sizeof(result);
  dwIndex := 0;
  Info := Info or HTTP_QUERY_FLAG_NUMBER;
  if not HttpQueryInfoA(fRequest, Info, @result, dwSize, dwIndex) then
    result := 0;
end;

function TWinINet.InternalReadData(var Data: SockString; Read: integer): cardinal;
begin
  if not InternetReadFile(fRequest, @PByteArray(Data)[Read], length(Data)-Read, result) then
    raise EWinINet.Create;
end;

procedure TWinINet.InternalCreateRequest(const method, aURL: SockString);
const ALL_ACCEPT: array[0..1] of PAnsiChar = ('*/*',nil);
var Flags: DWORD;
begin
  Flags := INTERNET_FLAG_HYPERLINK or INTERNET_FLAG_PRAGMA_NOCACHE or
    INTERNET_FLAG_RESYNCHRONIZE; // options for a true RESTful request
  if fKeepAlive<>0 then
    Flags := Flags or INTERNET_FLAG_KEEP_CONNECTION;
  if fHttps then
    Flags := Flags or INTERNET_FLAG_SECURE;
  FRequest := HttpOpenRequestA(FConnection, Pointer(method), Pointer(aURL), nil,
    nil, @ALL_ACCEPT, Flags,0);
  if FRequest=nil then
    raise EWinINet.Create;
end;

procedure TWinINet.InternalSendRequest(const aData: SockString);
begin
  if not HttpSendRequestA(fRequest, nil, 0, pointer(aData), length(aData)) then
    raise EWinINet.Create;
end;


{ TWinHTTP }

const
  winhttpdll = 'winhttp.dll';

  WINHTTP_ACCESS_TYPE_DEFAULT_PROXY = 0;
  WINHTTP_ACCESS_TYPE_NAMED_PROXY = 3;
  WINHTTP_FLAG_REFRESH = $00000100;
  WINHTTP_FLAG_SECURE = $00800000;
  WINHTTP_ADDREQ_FLAG_COALESCE = $40000000;
  WINHTTP_QUERY_FLAG_NUMBER = $20000000;

  // taken from http://www.tek-tips.com/faqs.cfm?fid=7493
  // status manifests for WinHttp status callback
    WINHTTP_CALLBACK_STATUS_RESOLVING_NAME = $00000001;
    WINHTTP_CALLBACK_STATUS_NAME_RESOLVED = $00000002;
    WINHTTP_CALLBACK_STATUS_CONNECTING_TO_SERVER = $00000004;
    WINHTTP_CALLBACK_STATUS_CONNECTED_TO_SERVER = $00000008;
    WINHTTP_CALLBACK_STATUS_SENDING_REQUEST = $00000010;
    WINHTTP_CALLBACK_STATUS_REQUEST_SENT = $00000020;
    WINHTTP_CALLBACK_STATUS_RECEIVING_RESPONSE = $00000040;
    WINHTTP_CALLBACK_STATUS_RESPONSE_RECEIVED = $00000080;
    WINHTTP_CALLBACK_STATUS_CLOSING_CONNECTION = $00000100;
    WINHTTP_CALLBACK_STATUS_CONNECTION_CLOSED = $00000200;
    WINHTTP_CALLBACK_STATUS_HANDLE_CREATED = $00000400;
    WINHTTP_CALLBACK_STATUS_HANDLE_CLOSING = $00000800;
    WINHTTP_CALLBACK_STATUS_DETECTING_PROXY = $00001000;
    WINHTTP_CALLBACK_STATUS_REDIRECT = $00004000;
    WINHTTP_CALLBACK_STATUS_INTERMEDIATE_RESPONSE = $00008000;
    WINHTTP_CALLBACK_STATUS_SECURE_FAILURE = $00010000;
    WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE = $00020000;
    WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE = $00040000;
    WINHTTP_CALLBACK_STATUS_READ_COMPLETE = $00080000;
    WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE = $00100000;
    WINHTTP_CALLBACK_STATUS_REQUEST_ERROR = $00200000;
    WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE = $00400000;

    WINHTTP_CALLBACK_FLAG_RESOLVE_NAME =
     (WINHTTP_CALLBACK_STATUS_RESOLVING_NAME or WINHTTP_CALLBACK_STATUS_NAME_RESOLVED);
    WINHTTP_CALLBACK_FLAG_CONNECT_TO_SERVER =
     (WINHTTP_CALLBACK_STATUS_CONNECTING_TO_SERVER or
      WINHTTP_CALLBACK_STATUS_CONNECTED_TO_SERVER);
    WINHTTP_CALLBACK_FLAG_SEND_REQUEST =
     (WINHTTP_CALLBACK_STATUS_SENDING_REQUEST or
      WINHTTP_CALLBACK_STATUS_REQUEST_SENT);
    WINHTTP_CALLBACK_FLAG_RECEIVE_RESPONSE =
     (WINHTTP_CALLBACK_STATUS_RECEIVING_RESPONSE or
      WINHTTP_CALLBACK_STATUS_RESPONSE_RECEIVED);
    WINHTTP_CALLBACK_FLAG_CLOSE_CONNECTION =
     (WINHTTP_CALLBACK_STATUS_CLOSING_CONNECTION or
      WINHTTP_CALLBACK_STATUS_CONNECTION_CLOSED);
    WINHTTP_CALLBACK_FLAG_HANDLES =
     (WINHTTP_CALLBACK_STATUS_HANDLE_CREATED or
      WINHTTP_CALLBACK_STATUS_HANDLE_CLOSING);
    WINHTTP_CALLBACK_FLAG_DETECTING_PROXY = WINHTTP_CALLBACK_STATUS_DETECTING_PROXY;
    WINHTTP_CALLBACK_FLAG_REDIRECT = WINHTTP_CALLBACK_STATUS_REDIRECT;
    WINHTTP_CALLBACK_FLAG_INTERMEDIATE_RESPONSE =  WINHTTP_CALLBACK_STATUS_INTERMEDIATE_RESPONSE;
    WINHTTP_CALLBACK_FLAG_SECURE_FAILURE = WINHTTP_CALLBACK_STATUS_SECURE_FAILURE;
    WINHTTP_CALLBACK_FLAG_SENDREQUEST_COMPLETE = WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE;
    WINHTTP_CALLBACK_FLAG_HEADERS_AVAILABLE = WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE;
    WINHTTP_CALLBACK_FLAG_DATA_AVAILABLE = WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE;
    WINHTTP_CALLBACK_FLAG_READ_COMPLETE = WINHTTP_CALLBACK_STATUS_READ_COMPLETE;
    WINHTTP_CALLBACK_FLAG_WRITE_COMPLETE = WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE;
    WINHTTP_CALLBACK_FLAG_REQUEST_ERROR = WINHTTP_CALLBACK_STATUS_REQUEST_ERROR;

    WINHTTP_CALLBACK_FLAG_ALL_COMPLETIONS =
        (WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE
       or WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE
       or WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE
       or WINHTTP_CALLBACK_STATUS_READ_COMPLETE
       or WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE
       or WINHTTP_CALLBACK_STATUS_REQUEST_ERROR);
    WINHTTP_CALLBACK_FLAG_ALL_NOTIFICATIONS = $ffffffff;

    WINHTTP_FLAG_SECURE_PROTOCOL_SSL2   = $00000008;
    WINHTTP_FLAG_SECURE_PROTOCOL_SSL3   = $00000020;
    WINHTTP_FLAG_SECURE_PROTOCOL_TLS1   = $00000080;
    // tls 1.1 & 1.2 const from here:
    // https://github.com/nihon-tc/Rtest/blob/master/header/Microsoft%20SDKs/Windows/v7.0A/Include/winhttp.h
    WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_1 = $00000200;
    WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2 = $00000800;
    // MPV - don't know why, but if I pass WINHTTP_FLAG_SECURE_PROTOCOL_SSL2
    // flag also, TLS1.2 do not work
    WINHTTP_FLAG_SECURE_PROTOCOL_MODERN: DWORD =
      WINHTTP_FLAG_SECURE_PROTOCOL_SSL3
         or WINHTTP_FLAG_SECURE_PROTOCOL_TLS1
         or WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_1 or WINHTTP_FLAG_SECURE_PROTOCOL_TLS1_2;

   WINHTTP_OPTION_SECURE_PROTOCOLS = 84;

    // if the following value is returned by WinHttpSetStatusCallback, then
    // probably an invalid (non-code) address was supplied for the callback
    WINHTTP_INVALID_STATUS_CALLBACK = -1;

type
  WINHTTP_STATUS_CALLBACK = procedure(hInternet: HINTERNET; dwContext: PDWORD;
    dwInternetStatus: DWORD; lpvStatusInformation: pointer; dwStatusInformationLength: DWORD); stdcall;
  PWINHTTP_STATUS_CALLBACK = ^WINHTTP_STATUS_CALLBACK;

function WinHttpOpen(pwszUserAgent: PWideChar; dwAccessType: DWORD;
  pwszProxyName, pwszProxyBypass: PWideChar; dwFlags: DWORD): HINTERNET; stdcall; external winhttpdll;
function WinHttpSetStatusCallback(hSession: HINTERNET; lpfnInternetCallback: WINHTTP_STATUS_CALLBACK;
  dwNotificationFlags: DWORD; dwReserved: PDWORD): WINHTTP_STATUS_CALLBACK; stdcall; external winhttpdll;

function WinHttpConnect(hSession: HINTERNET; pswzServerName: PWideChar;
  nServerPort: INTERNET_PORT; dwReserved: DWORD): HINTERNET; stdcall; external winhttpdll;
function WinHttpOpenRequest(hConnect: HINTERNET; pwszVerb: PWideChar;
  pwszObjectName: PWideChar; pwszVersion: PWideChar; pwszReferer: PWideChar;
  ppwszAcceptTypes: PLPWSTR; dwFlags: DWORD): HINTERNET; stdcall; external winhttpdll;
function WinHttpCloseHandle(hInternet: HINTERNET): BOOL; stdcall; external winhttpdll;
function WinHttpAddRequestHeaders(hRequest: HINTERNET; pwszHeaders: PWideChar; dwHeadersLength: DWORD;
  dwModifiers: DWORD): BOOL; stdcall; external winhttpdll;
function WinHttpSendRequest(hRequest: HINTERNET; pwszHeaders: PWideChar;
  dwHeadersLength: DWORD; lpOptional: Pointer; dwOptionalLength: DWORD; dwTotalLength: DWORD;
  dwContext: DWORD): BOOL; stdcall; external winhttpdll;
function WinHttpReceiveResponse(hRequest: HINTERNET;
  lpReserved: Pointer): BOOL; stdcall; external winhttpdll;
function WinHttpQueryHeaders(hRequest: HINTERNET; dwInfoLevel: DWORD; pwszName: PWideChar;
  lpBuffer: Pointer; var lpdwBufferLength, lpdwIndex: DWORD): BOOL; stdcall; external winhttpdll;
function WinHttpReadData(hRequest: HINTERNET; lpBuffer: Pointer;
  dwNumberOfBytesToRead: DWORD; var lpdwNumberOfBytesRead: DWORD): BOOL; stdcall; external winhttpdll;
function WinHttpSetTimeouts(hInternet: HINTERNET; dwResolveTimeout: DWORD;
  dwConnectTimeout: DWORD; dwSendTimeout: DWORD; dwReceiveTimeout: DWORD): BOOL; stdcall; external winhttpdll;
function WinHttpSetOption(hInternet: HINTERNET; dwOption: DWORD;
  lpBuffer: Pointer; dwBufferLength: DWORD): BOOL; stdcall; external winhttpdll;
function WinHttpSetCredentials(hRequest: HINTERNET; AuthTargets: DWORD; AuthScheme: DWORD;
  pwszUserName: PWideChar; pwszPassword: PWideChar; pAuthParams: Pointer) : BOOL; stdcall; external winhttpdll;

destructor TWinHTTP.Destroy;
begin
  if fConnection<>nil then
    WinHttpCloseHandle(fConnection);
  if fSession<>nil then
    WinHttpCloseHandle(fSession);
  inherited;
end;

procedure TWinHTTP.InternalAddHeader(const hdr: SockString);
begin
  if (hdr<>'') and
    not WinHttpAddRequestHeaders(FRequest, Pointer(Ansi7ToUnicode(hdr)), length(hdr),
      WINHTTP_ADDREQ_FLAG_COALESCE) then
    RaiseLastModuleError(winhttpdll,EWinHTTP);
end;

procedure TWinHTTP.InternalCloseRequest;
begin
  if fRequest<>nil then begin
    WinHttpCloseHandle(fRequest);
    FRequest := nil;
  end;
end;

procedure WinHTTPSecurityErrorCallback(hInternet: HINTERNET; dwContext: PDWORD;
  dwInternetStatus: DWORD; lpvStatusInformation: pointer; dwStatusInformationLength: DWORD); stdcall;
begin
  // in case lpvStatusInformation^=-2147483648 this is attempt to connect to
  // non-https socket wrong port - may be must be 443?
  raise EWinHTTP.CreateFmt('WinHTTP security error. Status %d, statusInfo: %d',
    [dwInternetStatus, pdword(lpvStatusInformation)^]);
end;

procedure TWinHTTP.InternalConnect(ConnectionTimeOut,SendTimeout,ReceiveTimeout: DWORD);
var OpenType: integer;
    Callback: WINHTTP_STATUS_CALLBACK;
    CallbackRes: PtrInt absolute Callback; // for FPC compatibility
begin
  if fProxyName='' then
    OpenType := WINHTTP_ACCESS_TYPE_DEFAULT_PROXY else
    OpenType := WINHTTP_ACCESS_TYPE_NAMED_PROXY;
  fSession := WinHttpOpen(pointer(Ansi7ToUnicode(fUserAgent)), OpenType,
    pointer(Ansi7ToUnicode(fProxyName)), pointer(Ansi7ToUnicode(fProxyByPass)), 0);
  if fSession=nil then
    RaiseLastModuleError(winhttpdll,EWinHTTP);
  // cf. http://msdn.microsoft.com/en-us/library/windows/desktop/aa384116
  if not WinHttpSetTimeouts(fSession,HTTP_DEFAULT_RESOLVETIMEOUT,
     ConnectionTimeOut,SendTimeout,ReceiveTimeout) then
    RaiseLastModuleError(winhttpdll,EWinHTTP);
  if fHTTPS then begin
    if not WinHttpSetOption(fSession, WINHTTP_OPTION_SECURE_PROTOCOLS,
       @WINHTTP_FLAG_SECURE_PROTOCOL_MODERN, SizeOf(WINHTTP_FLAG_SECURE_PROTOCOL_MODERN)) then
      RaiseLastModuleError(winhttpdll,EWinHTTP);
    Callback := WinHttpSetStatusCallback(fSession, WinHTTPSecurityErrorCallback,
       WINHTTP_CALLBACK_FLAG_SECURE_FAILURE, nil);
    if CallbackRes=WINHTTP_INVALID_STATUS_CALLBACK then
      RaiseLastModuleError(winhttpdll,EWinHTTP);
  end;
  fConnection := WinHttpConnect(fSession, pointer(Ansi7ToUnicode(FServer)), fPort, 0);
  if fConnection=nil then
    RaiseLastModuleError(winhttpdll,EWinHTTP);
end;

function TWinHTTP.InternalGetInfo(Info: DWORD): SockString;
var dwSize, dwIndex: DWORD;
    tmp: SockString;
    i: integer;
begin
  result := '';
  dwSize := 0;
  dwIndex := 0;
  if not WinHttpQueryHeaders(fRequest, Info, nil, nil, dwSize, dwIndex) and
     (GetLastError=ERROR_INSUFFICIENT_BUFFER) then begin
    SetLength(tmp,dwSize);
    if WinHttpQueryHeaders(fRequest, Info, nil, pointer(tmp), dwSize, dwIndex) then begin
      dwSize := dwSize shr 1;
      SetLength(result,dwSize);
      for i := 0 to dwSize-1 do // fast ANSI 7 bit conversion
        PByteArray(result)^[i] := PWordArray(tmp)^[i];
    end;
  end;
end;

function TWinHTTP.InternalGetInfo32(Info: DWORD): DWORD;
var dwSize, dwIndex: DWORD;
begin
  dwSize := sizeof(result);
  dwIndex := 0;
  Info := Info or WINHTTP_QUERY_FLAG_NUMBER;
  if not WinHttpQueryHeaders(fRequest, Info, nil, @result, dwSize, dwIndex) then
    result := 0;
end;

function TWinHTTP.InternalReadData(var Data: SockString; Read: integer): cardinal;
begin
  if not WinHttpReadData(fRequest, @PByteArray(Data)[Read], length(Data)-Read, result) then
    RaiseLastModuleError(winhttpdll,EWinHTTP);
end;

procedure TWinHTTP.InternalCreateRequest(const method, aURL: SockString);
const ALL_ACCEPT: array[0..1] of PWideChar = ('*/*',nil);
var Flags: DWORD;
begin
  Flags := WINHTTP_FLAG_REFRESH; // options for a true RESTful request
  if fHttps then
    Flags := Flags or WINHTTP_FLAG_SECURE;
  fRequest := WinHttpOpenRequest(fConnection, pointer(Ansi7ToUnicode(method)),
    pointer(Ansi7ToUnicode(aURL)), nil, nil, @ALL_ACCEPT, Flags);
  if fRequest=nil then
    RaiseLastModuleError(winhttpdll,EWinHTTP);
end;

const
  // from http://www.tek-tips.com/faqs.cfm?fid=7493
  WINHTTP_OPTION_SECURITY_FLAGS = 31;
  SECURITY_FLAG_IGNORE_UNKNOWN_CA = $00000100;
  SECURITY_FLAG_IGNORE_CERT_DATE_INVALID = $00002000; // expired X509 Cert.
  SECURITY_FLAG_IGNORE_CERT_CN_INVALID = $00001000; // bad common name in X509 Cert.
  SECURITY_FLAG_IGNORE_CERT_WRONG_USAGE = $00000200;
  SECURITY_FLAT_IGNORE_CERTIFICATES: DWORD =
    SECURITY_FLAG_IGNORE_UNKNOWN_CA or
    SECURITY_FLAG_IGNORE_CERT_DATE_INVALID or
    SECURITY_FLAG_IGNORE_CERT_CN_INVALID or
    SECURITY_FLAG_IGNORE_CERT_WRONG_USAGE;

  WINHTTP_AUTH_TARGET_SERVER = 0;
  WINHTTP_AUTH_TARGET_PROXY = 1;
  WINHTTP_AUTH_SCHEME_BASIC = $00000001;
  WINHTTP_AUTH_SCHEME_NTLM = $00000002;
  WINHTTP_AUTH_SCHEME_PASSPORT = $00000004;
  WINHTTP_AUTH_SCHEME_DIGEST = $00000008;
  WINHTTP_AUTH_SCHEME_NEGOTIATE = $00000010;

procedure TWinHTTP.InternalSendRequest(const aData: SockString);
var L: integer;
    winAuth: DWORD;
begin
  with fExtendedOptions do
  if AuthScheme<>wraNone then begin
    case AuthScheme of
    wraBasic: winAuth := WINHTTP_AUTH_SCHEME_BASIC;
    wraDigest: winAuth := WINHTTP_AUTH_SCHEME_DIGEST;
    wraNegotiate: winAuth := WINHTTP_AUTH_SCHEME_NEGOTIATE;
    else raise EWinHTTP.CreateFmt('Unsupported AuthScheme=%d',[ord(AuthScheme)]);
    end;
    if not WinHttpSetCredentials(fRequest,WINHTTP_AUTH_TARGET_SERVER,
       winAuth,pointer(AuthUserName),pointer(AuthPassword),nil) then
      RaiseLastModuleError(winhttpdll,EWinHTTP);
  end;
  if fHTTPS and IgnoreSSLCertificateErrors then
    if not WinHttpSetOption(fRequest, WINHTTP_OPTION_SECURITY_FLAGS,
       @SECURITY_FLAT_IGNORE_CERTIFICATES, SizeOf(SECURITY_FLAT_IGNORE_CERTIFICATES)) then
      RaiseLastModuleError(winhttpdll,EWinHTTP);
  L := length(aData);
  if not WinHttpSendRequest(fRequest, nil, 0, pointer(aData), L, L, 0) or
     not WinHttpReceiveResponse(fRequest,nil) then
    RaiseLastModuleError(winhttpdll,EWinHTTP);
end;

{$endif USEWININET}

{$ifdef USELIBCURL}

{ ************ libcurl implementation }

const
  LIBCURL_DLL = {$IFDEF LINUX} 'libcurl.so' {$ELSE} 'libcurl.dll' {$ENDIF};

type
  TCurlOption = (
    coPort                 = 3,
    coTimeout              = 13,
    coInFileSize           = 14,
    coLowSpeedLimit        = 19,
    coLowSpeedTime         = 20,
    coResumeFrom           = 21,
    coCRLF                 = 27,
    coSSLVersion           = 32,
    coTimeCondition        = 33,
    coTimeValue            = 34,
    coVerbose              = 41,
    coHeader               = 42,
    coNoProgress           = 43,
    coNoBody               = 44,
    coFailOnError          = 45,
    coUpload               = 46,
    coPost                 = 47,
    coFTPListOnly          = 48,
    coFTPAppend            = 50,
    coNetRC                = 51,
    coFollowLocation       = 52,
    coTransferText         = 53,
    coPut                  = 54,
    coAutoReferer          = 58,
    coProxyPort            = 59,
    coPostFieldSize        = 60,
    coHTTPProxyTunnel      = 61,
    coSSLVerifyPeer        = 64,
    coMaxRedirs            = 68,
    coFileTime             = 69,
    coMaxConnects          = 71,
    coClosePolicy          = 72,
    coFreshConnect         = 74,
    coForbidResue          = 75,
    coConnectTimeout       = 78,
    coHTTPGet              = 80,
    coSSLVerifyHost        = 81,
    coHTTPVersion          = 84,
    coFTPUseEPSV           = 85,
    coSSLEngineDefault     = 90,
    coDNSUseGlobalCache    = 91,
    coDNSCacheTimeout      = 92,
    coCookieSession        = 96,
    coBufferSize           = 98,
    coNoSignal             = 99,
    coProxyType            = 101,
    coUnrestrictedAuth     = 105,
    coFTPUseEPRT           = 106,
    coHTTPAuth             = 107,
    coFTPCreateMissingDirs = 110,
    coProxyAuth            = 111,
    coFTPResponseTimeout   = 112,
    coIPResolve            = 113,
    coMaxFileSize          = 114,
    coFTPSSL               = 119,
    coTCPNoDelay           = 121,
    coFTPSSLAuth           = 129,
    coIgnoreContentLength  = 136,
    coFTPSkipPasvIp        = 137,
    coFile                 = 10001,
    coURL                  = 10002,
    coProxy                = 10004,
    coUserPwd              = 10005,
    coProxyUserPwd         = 10006,
    coRange                = 10007,
    coInFile               = 10009,
    coErrorBuffer          = 10010,
    coPostFields           = 10015,
    coReferer              = 10016,
    coFTPPort              = 10017,
    coUserAgent            = 10018,
    coCookie               = 10022,
    coHTTPHeader           = 10023,
    coHTTPPost             = 10024,
    coSSLCert              = 10025,
    coSSLCertPasswd        = 10026,
    coQuote                = 10028,
    coWriteHeader          = 10029,
    coCookieFile           = 10031,
    coCustomRequest        = 10036,
    coStdErr               = 10037,
    coPostQuote            = 10039,
    coWriteInfo            = 10040,
    coProgressData         = 10057,
    coInterface            = 10062,
    coKRB4Level            = 10063,
    coCAInfo               = 10065,
    coTelnetOptions        = 10070,
    coRandomFile           = 10076,
    coEGDSocket            = 10077,
    coCookieJar            = 10082,
    coSSLCipherList        = 10083,
    coSSLCertType          = 10086,
    coSSLKey               = 10087,
    coSSLKeyType           = 10088,
    coSSLEngine            = 10089,
    coPreQuote             = 10093,
    coDebugData            = 10095,
    coCAPath               = 10097,
    coShare                = 10100,
    coEncoding             = 10102,
    coPrivate              = 10103,
    coHTTP200Aliases       = 10104,
    coSSLCtxData           = 10109,
    coNetRCFile            = 10118,
    coSourceUserPwd        = 10123,
    coSourcePreQuote       = 10127,
    coSourcePostQuote      = 10128,
    coIOCTLData            = 10131,
    coSourceURL            = 10132,
    coSourceQuote          = 10133,
    coFTPAccount           = 10134,
    coCookieList           = 10135,
    coWriteFunction        = 20011,
    coReadFunction         = 20012,
    coProgressFunction     = 20056,
    coHeaderFunction       = 20079,
    coDebugFunction        = 20094,
    coSSLCtxtFunction      = 20108,
    coIOCTLFunction        = 20130,
    coInFileSizeLarge      = 30115,
    coResumeFromLarge      = 30116,
    coMaxFileSizeLarge     = 30117,
    coPostFieldSizeLarge   = 30120
  );
  TCurlResult = (
    crOK, crUnsupportedProtocol, crFailedInit, crURLMalformat, crURLMalformatUser,
    crCouldntResolveProxy, crCouldntResolveHost, crCouldntConnect,
    crFTPWeirdServerReply, crFTPAccessDenied, crFTPUserPasswordIncorrect,
    crFTPWeirdPassReply, crFTPWeirdUserReply, crFTPWeirdPASVReply,
    crFTPWeird227Format, crFTPCantGetHost, crFTPCantReconnect, crFTPCouldntSetBINARY,
    crPartialFile, crFTPCouldntRetrFile, crFTPWriteError, crFTPQuoteError,
    crHTTPReturnedError, crWriteError, crMalFormatUser, crFTPCouldntStorFile,
    crReadError, crOutOfMemory, crOperationTimeouted,
    crFTPCouldntSetASCII, crFTPPortFailed, crFTPCouldntUseREST, crFTPCouldntGetSize,
    crHTTPRangeError, crHTTPPostError, crSSLConnectError, crBadDownloadResume,
    crFileCouldntReadFile, crLDAPCannotBind, crLDAPSearchFailed,
    crLibraryNotFound, crFunctionNotFound, crAbortedByCallback,
    crBadFunctionArgument, crBadCallingOrder, crInterfaceFailed,
    crBadPasswordEntered, crTooManyRedirects, crUnknownTelnetOption,
    crTelnetOptionSyntax, crObsolete, crSSLPeerCertificate, crGotNothing,
    crSSLEngineNotFound, crSSLEngineSetFailed, crSendError, crRecvError,
    crShareInUse, crSSLCertProblem, crSSLCipher, crSSLCACert, crBadContentEncoding,
    crLDAPInvalidURL, crFileSizeExceeded, crFTPSSLFailed, crSendFailRewind,
    crSSLEngineInitFailed, crLoginDenied, crTFTPNotFound, crTFTPPerm,
    crTFTPDiskFull, crTFTPIllegal, crTFTPUnknownID, crTFTPExists, crTFTPNoSuchUser
  );
  TCurlInfo = (
    ciNone,
    ciLastOne               = 28,
    ciEffectiveURL          = 1048577,
    ciContentType           = 1048594,
    ciPrivate               = 1048597,
    ciResponseCode          = 2097154,
    ciHeaderSize            = 2097163,
    ciRequestSize           = 2097164,
    ciSSLVerifyResult       = 2097165,
    ciFileTime              = 2097166,
    ciRedirectCount         = 2097172,
    ciHTTPConnectCode       = 2097174,
    ciHTTPAuthAvail         = 2097175,
    ciProxyAuthAvail        = 2097176,
    ciOS_Errno              = 2097177,
    ciNumConnects           = 2097178,
    ciTotalTime             = 3145731,
    ciNameLookupTime        = 3145732,
    ciConnectTime           = 3145733,
    ciPreTRansferTime       = 3145734,
    ciSizeUpload            = 3145735,
    ciSizeDownload          = 3145736,
    ciSpeedDownload         = 3145737,
    ciSpeedUpload           = 3145738,
    ciContentLengthDownload = 3145743,
    ciContentLengthUpload   = 3145744,
    ciStartTransferTime     = 3145745,
    ciRedirectTime          = 3145747,
    ciSSLEngines            = 4194331,
    ciCookieList            = 4194332
  );

  TCurlVersion = (cvFirst,cvSecond,cvThird,cvFour);
  TCurlGlobalInit = set of (giSSL,giWin32);
  PAnsiCharArray = array[0..1023] of PAnsiChar;

  TCurlVersionInfo = record
    age: TCurlVersion;
    version: PAnsiChar;
    version_num: cardinal;
    host: PAnsiChar;
    features: longint;
    ssl_version: PAnsiChar;
    ssl_version_num: PAnsiChar;
    libz_version: PAnsiChar;
    protocols: ^PAnsiCharArray;
    ares: PAnsiChar;
    ares_num: longint;
    libidn: PAnsiChar;
  end;
  PCurlVersionInfo = ^TCurlVersionInfo;

  TCurl = type pointer;
  TCurlList = type pointer;

  curl_write_callback = function (buffer: PAnsiChar; size,nitems: integer;
    outstream: pointer): integer; cdecl;
  curl_read_callback = function (buffer: PAnsiChar; size,nitems: integer;
    instream: pointer): integer; cdecl;

var
  curl: packed record
    Module: THandle;
    global_init: function(flags: TCurlGlobalInit): TCurlResult; cdecl;
    global_cleanup: procedure; cdecl;
    version_info: function(age: TCurlVersion): PCurlVersionInfo; cdecl;
    easy_init: function: pointer; cdecl;
    easy_setopt: function(curl: TCurl; option: TCurlOption): TCurlResult; cdecl varargs;
    easy_perform: function(curl: TCurl): TCurlResult; cdecl;
    easy_cleanup: procedure(curl: TCurl); cdecl;
    easy_getinfo: function(curl: TCurl; info: TCurlInfo; out value): TCurlResult; cdecl;
    easy_duphandle: function(curl: TCurl): pointer; cdecl;
    easy_reset: procedure(curl: TCurl); cdecl;
    easy_strerror: function(code: TCurlResult): PAnsiChar; cdecl;
    slist_append: function(list: TCurlList; s: PAnsiChar): TCurlList; cdecl;
    slist_free_all: procedure(list: TCurlList); cdecl;
    info: TCurlVersionInfo;
    infoText: string;
  end;

procedure LibCurlInitialize;
var P: PPointer;
    api: integer;
const NAMES: array[0..12] of string = (
  'global_init','global_cleanup','version_info',
  'easy_init','easy_setopt','easy_perform','easy_cleanup','easy_getinfo',
  'easy_duphandle','easy_reset','easy_strerror',
  'slist_append','slist_free_all');
begin
  if curl.Module=0 then
  try
    curl.Module := LoadLibrary(LIBCURL_DLL);
    {$ifdef LINUX}
    if curl.Module=0 then
      curl.Module := LoadLibrary('libcurl.so.3');
    {$endif}
    if curl.Module=0 then
      raise ECrtSocket.CreateFmt('Unable to find %s'{$ifdef LINUX}
        +': try e.g. sudo apt-get install libcurl3:i386'{$endif},[LIBCURL_DLL]);
    P := @@curl.global_init;
    for api := low(NAMES) to high(NAMES) do begin
      P^ := GetProcAddress(curl.Module,{$ifndef FPC}PChar{$endif}('curl_'+NAMES[api]));
      if P^=nil then
        raise ECrtSocket.CreateFmt('Unable to find %s() in %s',[NAMES[api],LIBCURL_DLL]);
      inc(P);
    end;
    curl.global_init([giSSL]);
    curl.info := curl.version_info(cvFour)^;
    curl.infoText := format('%s version %s',[LIBCURL_DLL,curl.info.version]);
    if curl.info.ssl_version<>nil then
      curl.infoText := format('%s using %s',[curl.infoText,curl.info.ssl_version]);
//   api := 0; with curl.info do while protocols[api]<>nil do begin
//     write(protocols[api], ' '); inc(api); end; writeln(#13#10,curl.infoText);
  except
    on E: Exception do begin
      if curl.Module<>0 then begin
        FreeLibrary(curl.Module);
        curl.Module := 0;
      end;
      {$ifdef LINUX}
      writeln(E.Message);
      {$else}
      raise;
      {$endif}
    end;
  end;
end;

function CurlWriteRawByteString(buffer: PAnsiChar; size,nitems: integer;
  opaque: pointer): integer; cdecl;
var storage: ^SockString absolute opaque;
    n: integer;
begin
  if storage=nil then
    result := 0 else begin
    n := length(storage^);
    result := size*nitems;
    SetLength(storage^,n+result);
    Move(buffer^,PPAnsiChar(opaque)^[n],result);
  end;
end;

function CurlReadData(buffer: PAnsiChar; size,nitems: integer;
  opaque: pointer): integer; cdecl;
var instance: TCurlHTTP absolute opaque;
    dataLen: integer;
begin
  dataLen := length(instance.fIn.Data)-instance.fIn.DataOffset;
  result := size*nitems;
  if result>dataLen then
    result := dataLen;
  move(PAnsiChar(pointer(instance.fIn.Data))[instance.fIn.DataOffset],buffer^,result);
  inc(instance.fIn.DataOffset,result);
end;


{ TCurlHTTP }

procedure TCurlHTTP.InternalConnect(ConnectionTimeOut,SendTimeout,ReceiveTimeout: DWORD);
const HTTPS: array[boolean] of string = ('','s');
begin
  inherited;
  if curl.Module=0 then
    LibCurlInitialize;
  fHandle := curl.easy_init;
  fRootURL := AnsiString(Format('http%s://%s:%d',[HTTPS[fHttps],fServer,fPort]));
end;

destructor TCurlHTTP.Destroy;
begin
  if fHandle<>nil then
    curl.easy_cleanup(fHandle);
  inherited;
end;

procedure TCurlHTTP.UseClientCertificate(
  const aCertFile, aCACertFile, aKeyName, aPassPhrase: SockString);
begin
  fSSL.CertFile := aCertFile;
  fSSL.CACertFile := aCACertFile;
  fSSL.KeyName := aKeyName;
  fSSL.PassPhrase := aPassPhrase;
end;

procedure TCurlHTTP.InternalCreateRequest(const method, aURL: SockString);
const CERT_PEM: SockString = 'PEM';
var url: SockString;
begin
  url := fRootURL+aURL;
  curl.easy_setopt(fHandle,coURL,pointer(url));
  if fHttps then
    if IgnoreSSLCertificateErrors then begin
      curl.easy_setopt(fHandle,coSSLVerifyPeer,0);
      curl.easy_setopt(fHandle,coSSLVerifyHost,0);
    end else begin
      // see https://curl.haxx.se/libcurl/c/simplessl.html
      if fSSL.CertFile<>'' then begin
        curl.easy_setopt(fHandle,coSSLCertType,pointer(CERT_PEM));
        curl.easy_setopt(fHandle,coSSLCert,pointer(fSSL.CertFile));
        if fSSL.PassPhrase<>'' then
          curl.easy_setopt(fHandle,coSSLCertPasswd,pointer(fSSL.PassPhrase));
        curl.easy_setopt(fHandle,coSSLKeyType,nil);
        curl.easy_setopt(fHandle,coSSLKey,pointer(fSSL.KeyName));
        curl.easy_setopt(fHandle,coCAInfo,pointer(fSSL.CACertFile));
        curl.easy_setopt(fHandle,coSSLVerifyPeer,1);
      end;
    end;
  curl.easy_setopt(fHandle,coUserAgent,pointer(fUserAgent));
  fIn.Method := UpperCase(method);
  fIn.Headers := nil;
  Finalize(fOut);
end;

procedure TCurlHTTP.InternalAddHeader(const hdr: SockString);
var P,H: PAnsiChar;
begin
  P := pointer(hdr);
  while P<>nil do begin
    H := pointer(GetNextLine(P));
    if H<>nil then // nil would reset the whole list
      fIn.Headers := curl.slist_append(fIn.Headers,H);
  end;
end;

procedure TCurlHTTP.InternalSendRequest(const aData: SockString);
begin // see http://curl.haxx.se/libcurl/c/CURLOPT_CUSTOMREQUEST.html
  // libcurl has dedicated options for GET,HEAD verbs
  if (fIn.Method='') or (fIn.Method='GET') then begin
    curl.easy_setopt(fHandle,coHTTPGet,1);
    if (aData<>'') and (fIn.Method='GET') then begin // e.g. GET with body
      curl.easy_setopt(fHandle,coCustomRequest,pointer(fIn.Method));
      curl.easy_setopt(fHandle,coNoBody,0);
      curl.easy_setopt(fHandle,coUpload,1);
      fIn.Data := aData;
      fIn.DataOffset := 0;
      curl.easy_setopt(fHandle,coInFile,pointer(self));
      curl.easy_setopt(fHandle,coReadFunction,@CurlReadData);
      curl.easy_setopt(fHandle,coInFileSize,length(aData));
    end;
  end else
  if fIn.Method='HEAD' then
    curl.easy_setopt(fHandle,coNoBody,1) else begin
    // handle other HTTP verbs
    curl.easy_setopt(fHandle,coCustomRequest,pointer(fIn.Method));
    if aData='' then begin // e.g. DELETE or LOCK
      curl.easy_setopt(fHandle,coNoBody,1);
      curl.easy_setopt(fHandle,coUpload,0);
    end else begin // e.g. POST or PUT
      curl.easy_setopt(fHandle,coNoBody,0);
      curl.easy_setopt(fHandle,coUpload,1);
      fIn.Data := aData;
      fIn.DataOffset := 0;
      curl.easy_setopt(fHandle,coInFile,pointer(self));
      curl.easy_setopt(fHandle,coReadFunction,@CurlReadData);
      curl.easy_setopt(fHandle,coInFileSize,length(aData));
    end;
    InternalAddHeader('Expect:'); // disable 'Expect: 100 Continue'
  end;
  curl.easy_setopt(fHandle,coWriteFunction,@CurlWriteRawByteString);
  curl.easy_setopt(fHandle,coFile,@fOut.Data);
  curl.easy_setopt(fHandle,coHeaderFunction,@CurlWriteRawByteString);
  curl.easy_setopt(fHandle,coWriteHeader,@fOut.Header);
end;

function TCurlHTTP.InternalRetrieveAnswer(var Header, Encoding, AcceptEncoding,
  Data: SockString): integer;
var res: TCurlResult;
    P: PAnsiChar;
    s: SockString;
    i: integer;
    rc: longint; // needed on Linux x86-64
begin
  curl.easy_setopt(fHandle,coHTTPHeader,fIn.Headers);
  res := curl.easy_perform(fHandle);
  if res<>crOK then begin
    result := STATUS_NOTFOUND;
    Data := SockString(format('libcurl easy_perform=%d', [ord(res)]));
  end else begin
    curl.easy_getinfo(fHandle,ciResponseCode,rc);
    result := rc;
    Header := Trim(fOut.Header);
    if IdemPChar(pointer(Header),'HTTP/') then begin
      i := 6;
      while Header[i]>=' ' do inc(i);
      while Header[i] in [#13,#10] do inc(i);
      system.Delete(Header,1,i-1); // trim leading 'HTTP/1.1 200 OK'#$D#$A
    end;
    P := pointer(Header);
    while P<>nil do begin
      s := GetNextLine(P);
      if IdemPChar(pointer(s),'ACCEPT-ENCODING:') then
        AcceptEncoding := trim(copy(s,17,100)) else
      if IdemPChar(pointer(s),'CONTENT-ENCODING:') then
        Encoding := trim(copy(s,19,100))
    end;
    Data := fOut.Data;
  end;
end;

procedure TCurlHTTP.InternalCloseRequest;
begin
  if fIn.Headers<>nil then begin
    curl.slist_free_all(fIn.Headers);
    fIn.Headers := nil;
  end;
  Finalize(fIn);
  fIn.DataOffset := 0;
  Finalize(fOut);
end;


{$endif USELIBCURL}


initialization
  {$ifdef DEBUGAPI}{$ifdef MSWINDOWS}AllocConsole;{$endif}{$endif}
  {$ifdef MSWINDOWS}
  Assert(
    {$ifdef CPU64}
    (sizeof(HTTP_REQUEST)=864) and
    (sizeof(HTTP_SSL_INFO)=48) and
    (sizeof(HTTP_DATA_CHUNK_INMEMORY)=32) and
    (sizeof(HTTP_DATA_CHUNK_FILEHANDLE)=32) and
    (sizeof(HTTP_REQUEST_HEADERS)=688) and
    (sizeof(HTTP_RESPONSE_HEADERS)=512) and
    (sizeof(HTTP_COOKED_URL)=40) and
    (sizeof(HTTP_RESPONSE)=568) and
    {$else}
    (sizeof(HTTP_REQUEST)=472) and
    (sizeof(HTTP_SSL_INFO)=28) and
    (sizeof(HTTP_DATA_CHUNK_INMEMORY)=24) and
    (sizeof(HTTP_DATA_CHUNK_FILEHANDLE)=32) and
    (sizeof(HTTP_RESPONSE)=288) and
    (sizeof(HTTP_REQUEST_HEADERS)=344) and
    (sizeof(HTTP_RESPONSE_HEADERS)=256) and
    (sizeof(HTTP_COOKED_URL)=24) and
    {$endif CPU64}
    (ord(reqUserAgent)=40) and
    (ord(respLocation)=23) and (sizeof(THttpHeader)=4) and
    (integer(HTTP_LOG_FIELD_TEST_SUB_STATUS)=HTTP_LOG_FIELD_SUB_STATUS));
  {$endif MSWINDOWS}
  if InitSocketInterface then
    WSAStartup(WinsockLevel, WsaDataOnce) else
    fillchar(WsaDataOnce,sizeof(WsaDataOnce),0);

finalization
  if WsaDataOnce.wVersion<>0 then
  try
    {$ifdef MSWINDOWS}
    if Assigned(WSACleanup) then
      WSACleanup;
    {$endif}
  finally
    fillchar(WsaDataOnce,sizeof(WsaDataOnce),0);
  end;
  {$ifdef MSWINDOWS}
  if Http.Module<>0 then begin
    FreeLibrary(Http.Module);
    Http.Module := 0;
  end;
  {$endif}
  DestroySocketInterface;
  {$ifdef USELIBCURL}
  if curl.Module<>0 then begin
    curl.global_cleanup;
    FreeLibrary(curl.Module);
  end;
  {$endif USELIBCURL}
end.
