/// system-specific cross-platform units
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynCrossPlatformSpecific;

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

  
  Version 1.18
  - first public release, corresponding to mORMot Framework 1.18
  - each operating system will have its own API calls in this single unit
  - would compile with Delphi for any platform, or with FPC or Kylix

}

{$i SynCrossPlatform.inc} // define e.g. HASINLINE

{$ifdef MSWINDOWS}
  {$ifdef FPC}
    {$define USEFCL}  // for debugging the FCL within Lazarus
  {$else}
  {$ifdef UNICODE}    // for debugging Indy within the IDE
    {$define USEINDY}
  {$else}
    {$define USESYNCRT}
  {$endif}
  {$endif}
{$else}
  {$ifdef FPC}
    {$define USEFCL}
  {$else}
    {$define USEINDY}
  {$endif}
{$endif}

interface

uses
  SysUtils,
  Classes;

type
  /// HTTP body may not match the string type, and could be binary 
  THttpBody = array of byte;
  
  /// used to store the request of a REST call
  TSQLRestURIParams = record
    /// input parameter containing the caller URI
    Url: string;
    /// input parameter containing the caller method
    Method: string;
    /// input parameter containing the caller message headers
    InHead: string;
    /// input parameter containing the caller message body
    InBody: THttpBody;
    /// output parameter to be set to the response message header
    OutHead: string;
    /// output parameter to be set to the response message body
    OutBody: THttpBody;
    /// output parameter to be set to the HTTP status integer code
    OutStatus: cardinal;
    /// output parameter to be set to the database internal state
    // - this is the only mORMot-specific parameter
    OutInternalState: cardinal;
  end;

  /// the connection parameters 
  TSQLRestConnectionParams = record
    Server: string;
    Port: integer;
    Https: boolean;
    ProxyName: string;
    ProxyByPass: string;
    SendTimeout: cardinal;
    ReceiveTimeout: cardinal
  end;

  /// abstract class for HTTP client connection
  TAbstractHttpConnection = class
  protected
    fParameters: TSQLRestConnectionParams;
    fURL: string;
  public
    /// this is the main entry point for all HTTP clients
    // - connect to http://aServer:aPort or https://aServer:aPort
    // - optional aProxyName may contain the name of the proxy server to use,
    // and aProxyByPass an optional semicolon delimited list of host names or
    // IP addresses, or both, that should not be routed through the proxy
    constructor Create(const aParameters: TSQLRestConnectionParams); virtual;
    /// perform the request
    procedure URI(var Call: TSQLRestURIParams;
      const InDataType: string; KeepAlive: integer); virtual; abstract;

    /// the remote server URI
    property Server: string read fURL;
    /// the connection parameters
    property Parameters: TSQLRestConnectionParams read fParameters;
  end;

  TAbstractHttpConnectionClass = class of TAbstractHttpConnection;


/// gives access to the class type to implement a HTTP connection
// - will use WinHTTP API (from our SynCrtSock) under Windows
// - will use Indy for Delphi on other platforms
// - will use fcl-web (fphttpclient) with FreePascal
function HttpConnectionClass: TAbstractHttpConnectionClass;

  
/// convert a text into UTF-8 binary buffer
procedure TextToHttpBody(const Text: string; var Body: THttpBody);

/// convert a UTF-8 binary buffer into texts
procedure HttpBodyToText(const Body: THttpBody; var Text: string);


implementation

{$ifdef USEFCL}
uses
  fphttpclient;
{$endif}

{$ifdef USEINDY}
uses
  IdHTTP;
{$endif}

{$ifdef USESYNCRT}
uses
  Windows,
  SynCrtSock;
{$endif}


procedure TextToHttpBody(const Text: string; var Body: THttpBody);
var utf8: UTF8String;
    n: integer;
begin
  utf8 := UTF8Encode(Text);
  n := length(utf8);
  SetLength(Body,n);
  move(pointer(utf8)^,pointer(Body)^,n);
end;

procedure HttpBodyToText(const Body: THttpBody; var Text: string);
var utf8: UTF8String;
begin
  SetLength(utf8,length(Body));
  move(pointer(Body)^,pointer(utf8)^,length(Body));
  {$ifdef UNICODE}
  Text := UTF8ToString(utf8);
  {$else}
  Text := UTF8Decode(utf8);
  {$endif}
end;


{ TAbstractHttpConnection }

const
  INTERNET_DEFAULT_HTTP_PORT = 80;
  INTERNET_DEFAULT_HTTPS_PORT = 443; 

constructor TAbstractHttpConnection.Create(
  const aParameters: TSQLRestConnectionParams);
begin
  inherited Create;
  fParameters := aParameters;
  if fParameters.Port=0 then
    if fParameters.Https then
      fParameters.Port := INTERNET_DEFAULT_HTTPS_PORT else
      fParameters.Port := INTERNET_DEFAULT_HTTP_PORT;
  if fParameters.Https then
    fURL := 'https://' else
    fURL := 'http://';
  fURL := fURL+fParameters.Server+':'+IntToStr(fParameters.Port)+'/';
end;


{$ifdef USEFCL}

type
  TFclHttpConnectionClass = class(TAbstractHttpConnection)
  protected
    fConnection: TFPHttpClient;
  public
    constructor Create(const aParameters: TSQLRestConnectionParams); override;
    procedure URI(var Call: TSQLRestURIParams; const InDataType: string;
      KeepAlive: integer); override;
    destructor Destroy; override;
  end;

{ TFclHttpConnectionClass }

constructor TFclHttpConnectionClass.Create(
  const aParameters: TSQLRestConnectionParams);
begin
  inherited Create(aParameters);
  fConnection := TFPHttpClient.Create(nil);
end;

procedure TFclHttpConnectionClass.URI(var Call: TSQLRestURIParams;
  const InDataType: string; KeepAlive: integer);
var InStr,OutStr: TBytesStream;
begin
  InStr := TBytesStream.Create(Call.InBody);
  OutStr := TBytesStream.Create;
  try
    fConnection.RequestHeaders.Text := Call.InHead;
    if Call.InBody<>nil then
      fConnection.RequestBody := InStr;
    fConnection.HTTPMethod(Call.Method,fURL+Call.Url,OutStr,[]);
    Call.OutHead := fConnection.ResponseHeaders.Text;
    Call.OutBody := OutStr.Bytes;
  finally
    OutStr.Free;
    InStr.Free;
  end;
end;

destructor TFclHttpConnectionClass.Destroy;
begin
  fConnection.Free;
  inherited Destroy;
end;

function HttpConnectionClass: TAbstractHttpConnectionClass;
begin
  result := TFclHttpConnectionClass;
end;

{$endif}

{$ifdef USEINDY}

type
  TIndyHttpConnectionClass = class(TAbstractHttpConnection)
  protected
    fConnection: TIdHTTP;
  public
    constructor Create(const aParameters: TSQLRestConnectionParams); override;
    procedure URI(var Call: TSQLRestURIParams; const InDataType: string;
      KeepAlive: integer); override;
    destructor Destroy; override;
  end;

{ TIndyHttpConnectionClass }

constructor TIndyHttpConnectionClass.Create(
  const aParameters: TSQLRestConnectionParams);
begin
  inherited;
  fConnection := TIdHTTP.Create(nil);
  if fParameters.ProxyName<>'' then
    fConnection.ProxyParams.ProxyServer := fParameters.ProxyName;
end;

destructor TIndyHttpConnectionClass.Destroy;
begin
  fConnection.Free;
  inherited;
end;

procedure TIndyHttpConnectionClass.URI(var Call: TSQLRestURIParams;
  const InDataType: string; KeepAlive: integer);
var InStr, OutStr: TStream;
    OutLen: integer;
begin
  InStr := TMemoryStream.Create;
  OutStr := TMemoryStream.Create;
  try
    fConnection.Request.RawHeaders.Text := Call.InHead;
    if Call.InBody<>nil then begin
      InStr.Write(Call.InBody,length(Call.InBody));
      InStr.Seek(0,soBeginning);
      fConnection.Request.Source := InStr;
    end;
    if Call.Method='GET' then
      fConnection.Get(fURL+Call.Url,OutStr) else
    if Call.Method='POST' then
      fConnection.Post(fURL+Call.Url,InStr,OutStr) else
    if Call.Method='PUT' then
      fConnection.Put(fURL+Call.Url,InStr) else
    if Call.Method='DELETE' then
      fConnection.Delete(fURL+Call.Url,OutStr) else
      raise Exception.CreateFmt('Indy does not know method %s',[Call.Method]);
    Call.OutStatus := fConnection.Response.ResponseCode;
    Call.OutHead := fConnection.Response.RawHeaders.Text;
    OutLen := OutStr.Size;
    if OutLen>0 then begin
      SetLength(Call.OutBody,OutLen);
      OutStr.Seek(0,soBeginning);
      OutStr.Read(Call.OutBody,OutLen);
    end;
  finally
    OutStr.Free;
    InStr.Free;
  end;
end;

function HttpConnectionClass: TAbstractHttpConnectionClass;
begin
  result := TIndyHttpConnectionClass;
end;


{$endif}

{$ifdef USESYNCRT}

type
  TWinHttpConnectionClass = class(TAbstractHttpConnection)
  protected
    fConnection: TWinHttpAPI;
    fLock: TRTLCriticalSection;
  public
    constructor Create(const aParameters: TSQLRestConnectionParams); override;
    procedure URI(var Call: TSQLRestURIParams; const InDataType: string;
      KeepAlive: integer); override;
    destructor Destroy; override;
  end;

{ TWinHttpConnectionClass }

constructor TWinHttpConnectionClass.Create(
  const aParameters: TSQLRestConnectionParams);
begin
  inherited;
  InitializeCriticalSection(fLock);
  fConnection := TWinHTTP.Create(RawByteString(fParameters.Server),
    RawByteString(IntToStr(fParameters.Port)),fParameters.Https,
    RawByteString(fParameters.ProxyName),RawByteString(fParameters.ProxyByPass),
    fParameters.SendTimeout,fParameters.ReceiveTimeout);
end;

destructor TWinHttpConnectionClass.Destroy;
begin
  fConnection.Free;
  DeleteCriticalSection(fLock);
  inherited;
end;

procedure TWinHttpConnectionClass.URI(var Call: TSQLRestURIParams;
  const InDataType: string; KeepAlive: integer);
var inb,outb,outh: RawByteString;
    n: integer;
begin
  EnterCriticalSection(fLock);
  try
    SetString(inb,PAnsiChar(Call.InBody),length(Call.InBody));
    fConnection.Request(RawByteString(Call.Url),RawByteString(Call.Method),
      KeepAlive,RawByteString(Call.InHead),inb,RawByteString(InDataType),
      outh,outb);
    Call.OutHead := string(outh);
    n := length(outb);
    SetLength(Call.OutBody,n);
    Move(pointer(outb)^,pointer(Call.OutBody)^,n);
  finally
    LeaveCriticalSection(fLock);
  end;
end;

function HttpConnectionClass: TAbstractHttpConnectionClass;
begin
  result := TWinHttpConnectionClass;
end;

{$endif}

end.
