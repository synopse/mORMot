/// low level access to Windows Authentication for the Win32/Win64 platform
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynSSPIAuth;
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

  The Initial Developer of the Original Code is Chaa.

  Portions created by the Initial Developer are Copyright (C) 2014
  the Initial Developer. All Rights Reserved.

  Contributor(s):
   Arnaud Bouchez

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


  This unit has been contributed by Chaa.
  See http://synopse.info/forum/viewtopic.php?pid=5619#p5619

  Thanks A LOT for this great contribution to the framework!


  Version 1.18
  - initial release, with code submitted by Chaa
  - code refactored to conform MSDN sample code, see
    http://msdn.microsoft.com/en-us/library/windows/desktop/aa379449.aspx
  - SECURITY_NETWORK_DREP changed to SECURITY_NATIVE_DREP to conform
    browser authentication requirements
  - SecPkgName moved to unit interface section, to be used in
    browser authentication code
  - added support for Kerberos authentication method, to use Kerberos set
    SecKerberosSPN variable on client-side to the value of SPN for service,
    see documentation for details
  - added SecPackageName() to see active Kerberos or NTLM authentication scheme
  - added SecEncrypt() and SecDecrypt() functions
  
}

interface

uses
    SysUtils, SynCommons;

{$I Synopse.inc} // define HASINLINE

type
  /// Windows Authentication context handle
  TSecHandle = record
    dwLower: PtrInt;
    dwUpper: PtrInt;
  end;
  PSecHandle = ^TSecHandle;

  /// Windows Authentication context
  TSecContext = record
    ID: RawUTF8;
    CredHandle: TSecHandle;
    CtxHandle: TSecHandle;
    CreatedTick64: Int64;
  end;
  PSecContext = ^TSecContext;

  /// dynamic array of Windows Authentication contexts
  // - used to hold information between calls to ServerSSPIAuth
  TSecContextDynArray = array of TSecContext;

  
/// Sets aSecHandle fields to empty state for a given connection ID
procedure InvalidateSecContext(var aSecContext: TSecContext; const aConnectionID: RawUTF8);

/// Client-side authentication procedure
// - aSecContext holds information between function calls
// - aInData contains data received from server
// - aSecKerberosSPN is the optional SPN domain name, e.g.
// 'mymormotservice/myserver.mydomain.tld'
// - aOutData contains data that must be sent to server
// - if function returns True, client must send aOutData to server
// and call function again width data, returned from servsr
function ClientSSPIAuth(var aSecContext: TSecContext;
    const aInData: RawByteString; const aSecKerberosSPN: RawUTF8; 
    out aOutData: RawByteString): Boolean;

/// Server-side authentication procedure
// - aSecContext holds information between function calls
// - aInData contains data recieved from client
// - aOutData contains data that must be sent to client
// - if function returns True, server must send aOutData to client
// and call function again width data, returned from client
function ServerSSPIAuth(var aSecContext: TSecContext;
    const aInData: RawByteString; out aOutData: RawByteString): Boolean;

/// Server-side function that returns authenticated user name
// - aSecContext must be received from previos success call to ServerSSPIAuth
// - aUserName contains authenticated user name
procedure ServerSSPIAuthUser(var aSecContext: TSecContext; out aUserName: RawUTF8);

/// Returns name of the security package that has been used with the negotiation process
// - aSecContext must be received from previos success call to ServerSSPIAuth
// or ClientSSPIAuth
function SecPackageName(var aSecContext: TSecContext): RawUTF8;

/// Encrypts a message
// - aSecContext must be received from previos success call to ServerSSPIAuth
// or ClientSSPIAuth
// - aPlain contains data that must be encrypted
// - returns encrypted message
function SecEncrypt(var aSecContext: TSecContext; const aPlain: RawByteString): RawByteString;

/// Decrypts  a message
// - aSecContext must be received from previos success call to ServerSSPIAuth
// or ClientSSPIAuth
// - aEncrypted contains data that must be decrypted
// - returns decrypted message
function SecDecrypt(var aSecContext: TSecContext; const aEncrypted: RawByteString): RawByteString;

/// Free aSecContext on client or server side
procedure FreeSecContext(var aSecContext: TSecContext);

const
  /// identification name used for SSPI authentication
  SECPKGNAMEINTERNAL = 'Negotiate';
  /// HTTP header to be set for SSPI authentication
  SECPKGNAMEHTTPWWWAUTHENTICATE = 'WWW-Authenticate: '+SECPKGNAMEINTERNAL;
  /// HTTP header pattern received for SSPI authentication
  SECPKGNAMEHTTPAUTHORIZATION = 'AUTHORIZATION: NEGOTIATE ';


implementation

uses
  Windows;

type
  TSecBuffer = record
    cbBuffer: Cardinal;   // Size of the buffer, in bytes
    BufferType: Cardinal; // Type of the buffer
    pvBuffer: Pointer;    // Pointer to the buffer
  end;
  PSecBuffer = ^TSecBuffer;

  TSecBufferDesc = record
    ulVersion: Cardinal;  // Version number
    cBuffers: Cardinal;   // Number of buffers
    pBuffers: PSecBuffer; // Pointer to array of buffers
  end;
  PSecBufferDesc = ^TSecBufferDesc;

  TSecPkgInfoW = record
    fCapabilities: Cardinal; // Capability bitmask
    wVersion: Word;       // Version of driver
    wRPCID: Word;         // ID for RPC Runtime
    cbMaxToken: Cardinal; // Size of authentication token (max)
    Name: PWideChar;      // Text name
    Comment: PWideChar;   // Comment
  end;
  PSecPkgInfoW = ^TSecPkgInfoW;

  SecPkgContext_NegotiationInfo = record
    PackageInfo: PSecPkgInfoW;
    NegotiationState: Cardinal;
  end;

  SecPkgContext_Sizes = record
    cbMaxToken: Cardinal;
    cbMaxSignature: Cardinal;
    cbBlockSize: Cardinal;
    cbSecurityTrailer: Cardinal;
  end;

const
  SECBUFFER_VERSION = 0;
  SECBUFFER_DATA = 1;
  SECBUFFER_TOKEN = 2;
  SECPKG_CRED_INBOUND  = $00000001;
  SECPKG_CRED_OUTBOUND = $00000002;
  SECPKG_ATTR_SIZES = 0;
  SECPKG_ATTR_NEGOTIATION_INFO = 12;
  SECURITY_NETWORK_DREP = 0;
  SECURITY_NATIVE_DREP = $10;
  ISC_REQ_MUTUAL_AUTH = $00000002;
  ISC_REQ_CONFIDENTIALITY = $00000010;
  ISC_REQ_ALLOCATE_MEMORY = $00000100;
  ASC_REQ_CONFIDENTIALITY = $00000010;
  ASC_REQ_ALLOCATE_MEMORY = $00000100;
  SEC_I_CONTINUE_NEEDED = $00090312;
  SEC_I_COMPLETE_NEEDED = $00090313;
  SEC_I_COMPLETE_AND_CONTINUE = $00090314;
  secur32 = 'secur32.dll';

function QuerySecurityPackageInfoW(pszPackageName: PWideChar;
  var ppPackageInfo: PSecPkgInfoW): Integer; stdcall;
  external secur32 name 'QuerySecurityPackageInfoW';

function AcquireCredentialsHandleW(pszPrincipal, pszPackage: PWideChar;
  fCredentialUse: Cardinal; pvLogonId, pAuthData: Pointer;
  pGetKeyFn: Pointer; pvGetKeyArgument: Pointer; phCredential: PSecHandle;
  var ptsExpiry: LARGE_INTEGER): Integer; stdcall;
  external secur32 name 'AcquireCredentialsHandleW';

function InitializeSecurityContextW(phCredential: PSecHandle; phContext: PSecHandle;
  pszTargetName: PWideChar; fContextReq, Reserved1, TargetDataRep: Cardinal;
  pInput: PSecBufferDesc; Reserved2: Cardinal; phNewContext: PSecHandle;
  pOutput: PSecBufferDesc; var pfContextAttr: Cardinal; var ptsExpiry: LARGE_INTEGER): Integer; stdcall;
  external secur32 name 'InitializeSecurityContextW';

function AcceptSecurityContext(phCredential: PSecHandle; phContext: PSecHandle;
  pInput: PSecBufferDesc; fContextReq, TargetDataRep: Cardinal;
  phNewContext: PSecHandle; pOutput: PSecBufferDesc; var pfContextAttr: Cardinal;
  var ptsExpiry: LARGE_INTEGER): Integer; stdcall;
  external secur32 name 'AcceptSecurityContext';

function CompleteAuthToken(phContext: PSecHandle; pToken: PSecBufferDesc): Integer; stdcall;
  external secur32 name 'CompleteAuthToken';

function QueryContextAttributesW(phContext: PSecHandle; ulAttribute: Cardinal;
  pBuffer: Pointer): Integer; stdcall;
  external secur32 name 'QueryContextAttributesW';

function QuerySecurityContextToken(phContext: PSecHandle; var Token: THandle): Integer; stdcall;
  external secur32 name 'QuerySecurityContextToken';

function EncryptMessage(phContext: PSecHandle; fQOP: Cardinal;
  pToken: PSecBufferDesc; MessageSeqNo: Cardinal): Integer; stdcall;
  external secur32 name 'EncryptMessage';

function DecryptMessage(phContext: PSecHandle; pToken: PSecBufferDesc;
  MessageSeqNo: Cardinal; var fQOP: Cardinal): Integer; stdcall;
  external secur32 name 'DecryptMessage';

function FreeContextBuffer(pvContextBuffer: Pointer): Integer; stdcall;
  external secur32 name 'FreeContextBuffer';

function DeleteSecurityContext(phContext: PSecHandle): Integer; stdcall;
  external secur32 name 'DeleteSecurityContext';

function FreeCredentialsHandle(phCredential: PSecHandle): Integer; stdcall;
  external secur32 name 'FreeCredentialsHandle';

procedure InvalidateSecContext(var aSecContext: TSecContext; const aConnectionID: RawUTF8);
begin
  aSecContext.ID := aConnectionID;
  aSecContext.CredHandle.dwLower := -1;
  aSecContext.CredHandle.dwUpper := -1;
  aSecContext.CtxHandle.dwLower := -1;
  aSecContext.CtxHandle.dwUpper := -1;
  aSecContext.CreatedTick64 := 0;
end;

function ClientSSPIAuth(var aSecContext: TSecContext;
    const aInData: RawByteString; const aSecKerberosSPN: RawUTF8;
    out aOutData: RawByteString): Boolean;
var InBuf: TSecBuffer;
    InDesc: TSecBufferDesc;
    InDescPtr: PSecBufferDesc;
    SecPkgInfo: PSecPkgInfoW;
    Expiry: LARGE_INTEGER;
    LInCtxPtr: PSecHandle;
    OutBuf: TSecBuffer;
    OutDesc: TSecBufferDesc;
    CtxReqAttr: Cardinal;
    CtxAttr: Cardinal;
    Status: Integer;
begin
  InBuf.BufferType := SECBUFFER_TOKEN;
  InBuf.cbBuffer := Length(aInData);
  InBuf.pvBuffer := PByte(aInData);

  if (aSecContext.CredHandle.dwLower = -1) and (aSecContext.CredHandle.dwUpper = -1) then begin
    aSecContext.CreatedTick64 := GetTickCount64;
    if QuerySecurityPackageInfoW(SECPKGNAMEINTERNAL, SecPkgInfo) <> 0 then
      RaiseLastOSError;
    try
      if AcquireCredentialsHandleW(nil, SecPkgInfo.Name, SECPKG_CRED_OUTBOUND, nil, nil, nil, nil, @aSecContext.CredHandle, Expiry) <> 0 then
        RaiseLastOSError;
    finally
      FreeContextBuffer(SecPkgInfo);
    end;
    InDescPtr := nil;
    LInCtxPtr := nil;
  end
  else begin
    InDesc.ulVersion := SECBUFFER_VERSION;
    InDesc.cBuffers := 1;
    InDesc.pBuffers := @InBuf;
    InDescPtr := @InDesc;
    LInCtxPtr := @aSecContext.CtxHandle;
  end;

  CtxReqAttr := ISC_REQ_ALLOCATE_MEMORY or ASC_REQ_CONFIDENTIALITY;
  if aSecKerberosSPN <> '' then
    CtxReqAttr := CtxReqAttr or ISC_REQ_MUTUAL_AUTH;

  OutBuf.BufferType := SECBUFFER_TOKEN;
  OutBuf.cbBuffer := 0;
  OutBuf.pvBuffer := nil;
  OutDesc.ulVersion := SECBUFFER_VERSION;
  OutDesc.cBuffers := 1;
  OutDesc.pBuffers := @OutBuf;

  Status := InitializeSecurityContextW(@aSecContext.CredHandle, LInCtxPtr,
    pointer(UTF8ToSynUnicode(aSecKerberosSPN)), CtxReqAttr,
    0, SECURITY_NATIVE_DREP, InDescPtr, 0, @aSecContext.CtxHandle, @OutDesc, CtxAttr, Expiry);

  Result := (Status = SEC_I_CONTINUE_NEEDED) or (Status = SEC_I_COMPLETE_AND_CONTINUE);

  if (Status = SEC_I_COMPLETE_NEEDED) or (Status = SEC_I_COMPLETE_AND_CONTINUE) then
    Status := CompleteAuthToken(@aSecContext.CtxHandle, @OutDesc);
  if Status < 0 then
    RaiseLastOSError;

  SetString(aOutData, PAnsiChar(OutBuf.pvBuffer), OutBuf.cbBuffer);
  FreeContextBuffer(OutBuf.pvBuffer);
end;

function ServerSSPIAuth(var aSecContext: TSecContext;
  const aInData: RawByteString; out aOutData: RawByteString): Boolean;
var InBuf: TSecBuffer;
    InDesc: TSecBufferDesc;
    SecPkgInfo: PSecPkgInfoW;
    Expiry: LARGE_INTEGER;
    LInCtxPtr: PSecHandle;
    OutBuf: TSecBuffer;
    OutDesc: TSecBufferDesc;
    CtxAttr: Cardinal;
    Status: Integer;
begin
  InBuf.BufferType := SECBUFFER_TOKEN;
  InBuf.cbBuffer := Length(aInData);
  InBuf.pvBuffer := PByte(aInData);
  InDesc.ulVersion := SECBUFFER_VERSION;
  InDesc.cBuffers := 1;
  InDesc.pBuffers := @InBuf;

  if (aSecContext.CredHandle.dwLower = -1) and (aSecContext.CredHandle.dwUpper = -1) then begin
    aSecContext.CreatedTick64 := GetTickCount64;
    if QuerySecurityPackageInfoW(SECPKGNAMEINTERNAL, SecPkgInfo) <> 0 then
      RaiseLastOSError;
    try
      if AcquireCredentialsHandleW(nil, SecPkgInfo.Name, SECPKG_CRED_INBOUND, nil, nil, nil, nil, @aSecContext.CredHandle, Expiry) <> 0 then
          RaiseLastOSError;
    finally
      FreeContextBuffer(SecPkgInfo);
    end;
    LInCtxPtr := nil;
  end
  else
    LInCtxPtr := @aSecContext.CtxHandle;

  OutBuf.BufferType := SECBUFFER_TOKEN;
  OutBuf.cbBuffer := 0;
  OutBuf.pvBuffer := nil;
  OutDesc.ulVersion := SECBUFFER_VERSION;
  OutDesc.cBuffers := 1;
  OutDesc.pBuffers := @OutBuf;

  Status := AcceptSecurityContext(@aSecContext.CredHandle, LInCtxPtr, @InDesc,
      ASC_REQ_ALLOCATE_MEMORY or ASC_REQ_CONFIDENTIALITY,
      SECURITY_NATIVE_DREP, @aSecContext.CtxHandle, @OutDesc, CtxAttr, Expiry);

  Result := (Status = SEC_I_CONTINUE_NEEDED) or (Status = SEC_I_COMPLETE_AND_CONTINUE);

  if (Status = SEC_I_COMPLETE_NEEDED) or (Status = SEC_I_COMPLETE_AND_CONTINUE) then
    Status := CompleteAuthToken(@aSecContext.CtxHandle, @OutDesc);
  if Status < 0 then
      RaiseLastOSError;

  SetString(aOutData, PAnsiChar(OutBuf.pvBuffer), OutBuf.cbBuffer);
  FreeContextBuffer(OutBuf.pvBuffer);
end;

procedure ServerSSPIAuthUser(var aSecContext: TSecContext; out aUserName: RawUTF8);
var UserToken: THandle;
    UserInfo: PSIDAndAttributes;
    Size: Cardinal;
    NameBuf: array[byte] of Char;
    NameLen: Cardinal;
    DomainBuf: array[byte] of Char;
    DomainLen: Cardinal;
    NameType: Cardinal;
begin
  aUserName := '';
  if QuerySecurityContextToken(@aSecContext.CtxHandle, UserToken) <> 0 then
    RaiseLastOSError;
  try
    Size := 0;
    GetTokenInformation(UserToken, TokenUser, nil, 0, Size);
    UserInfo := AllocMem(Size);
    try
      if not GetTokenInformation(Cardinal(UserToken), Windows.TokenUser, UserInfo, Size, Size) then
        RaiseLastOSError;
      FillChar(NameBuf[0], SizeOf(NameBuf), 0);
      NameLen := 256;
      FillChar(DomainBuf[0], SizeOf(DomainBuf), 0);
      DomainLen := 256;
      if not LookupAccountSid(nil, UserInfo.Sid, NameBuf, NameLen, DomainBuf, DomainLen, NameType) then
        RaiseLastOSError;
      if NameType = SidTypeUser then
        aUserName := FormatUTF8('%\%', [DomainBuf, NameBuf]);
    finally
      FreeMem(UserInfo);
    end;
  finally
    CloseHandle(UserToken);
  end;
end;

function SecPackageName(var aSecContext: TSecContext): RawUTF8;
var NegotiationInfo: SecPkgContext_NegotiationInfo;
begin
  if QueryContextAttributesW(@aSecContext.CtxHandle, SECPKG_ATTR_NEGOTIATION_INFO, @NegotiationInfo) <> 0 then
    RaiseLastOSError;
  Result := RawUnicodeToUtf8(NegotiationInfo.PackageInfo.Name, StrLenW(NegotiationInfo.PackageInfo.Name));
end;

function SecEncrypt(var aSecContext: TSecContext; const aPlain: RawByteString): RawByteString;
var Sizes: SecPkgContext_Sizes;
    SrcLen, EncLen: Cardinal;
    EncBuffer: RawByteString;
    InBuf: array[0..1] of TSecBuffer;
    InDesc: TSecBufferDesc;
    Status: Integer;
    BufPtr: PByte;
begin
  // Sizes.cbSecurityTrailer is size of the trailer (signature + padding) block
  if QueryContextAttributesW(@aSecContext.CtxHandle, SECPKG_ATTR_SIZES, @Sizes) <> 0 then
    RaiseLastOSError;

  SrcLen := Length(aPlain);
  EncLen := SizeOf(Cardinal) + Sizes.cbSecurityTrailer + SrcLen;
  SetLength(Result, EncLen);

  InBuf[0].BufferType := SECBUFFER_TOKEN;
  InBuf[0].cbBuffer := Sizes.cbSecurityTrailer;
  InBuf[0].pvBuffer := pointer(PtrInt(Result) + SizeOf(Cardinal));

  // Encoding done in-place, so we copy the data
  EncBuffer := aPlain;

  InBuf[1].BufferType := SECBUFFER_DATA;
  InBuf[1].cbBuffer := SrcLen;
  InBuf[1].pvBuffer := @EncBuffer[1]; // call UniqueString

  InDesc.ulVersion := SECBUFFER_VERSION;
  InDesc.cBuffers := 2;
  InDesc.pBuffers := @InBuf;

  Status := EncryptMessage(@aSecContext.CtxHandle, 0, @InDesc, 0);
  if Status < 0 then
      RaiseLastOSError;

  // Encrypted data buffer structure:
  //
  //   4 bytes  SigLen bytes     Remaining bytes
  // +--------+----------------+-----------------+
  // | SigLen | Trailer        | Data            |
  // +--------+----------------+-----------------+

  BufPtr := PByte(Result);
  PCardinal(BufPtr)^ := InBuf[0].cbBuffer;
  Inc(BufPtr, SizeOf(Cardinal));
  Inc(BufPtr, InBuf[0].cbBuffer);
  Move(PByte(InBuf[1].pvBuffer)^, BufPtr^, SrcLen);
  SetLength(Result, SizeOf(Cardinal) + InBuf[0].cbBuffer + SrcLen);
end;

function SecDecrypt(var aSecContext: TSecContext; const aEncrypted: RawByteString): RawByteString;
var EncLen, SigLen, SrcLen: Cardinal;
    BufPtr: PByte;
    InBuf: array [0..1] of TSecBuffer;
    InDesc: TSecBufferDesc;
    Status: Integer;
    QOP: Cardinal;
begin
  EncLen := Length(aEncrypted);
  BufPtr := PByte(aEncrypted);
  if EncLen < SizeOf(Cardinal) then  begin
    SetLastError(ERROR_INVALID_PARAMETER);
    RaiseLastOSError;
  end;

  SigLen := PCardinal(BufPtr)^;
  Inc(BufPtr, SizeOf(Cardinal));
  if EncLen < (SizeOf(Cardinal) + SigLen) then begin
    SetLastError(ERROR_INVALID_PARAMETER);
    RaiseLastOSError;
  end;

  SrcLen := EncLen - SizeOf(Cardinal) - SigLen;
  SetLength(Result, SrcLen);

  InBuf[0].BufferType := SECBUFFER_TOKEN;
  InBuf[0].cbBuffer := SigLen;
  InBuf[0].pvBuffer := BufPtr;
  Inc(BufPtr, SigLen);

  Move(BufPtr^, PByte(Result)^, SrcLen);
  InBuf[1].BufferType := SECBUFFER_DATA;
  InBuf[1].cbBuffer := SrcLen;
  InBuf[1].pvBuffer := PByte(Result);

  InDesc.ulVersion := SECBUFFER_VERSION;
  InDesc.cBuffers := 2;
  InDesc.pBuffers := @InBuf;

  Status := DecryptMessage(@aSecContext.CtxHandle, @InDesc, 0, QOP);
  if Status < 0 then
    RaiseLastOSError;
end;

procedure FreeSecContext(var aSecContext: TSecContext);
begin
  if (aSecContext.CtxHandle.dwLower <> -1) or (aSecContext.CtxHandle.dwUpper <> -1) then begin
    DeleteSecurityContext(@aSecContext.CtxHandle);
    aSecContext.CtxHandle.dwLower := -1;
    aSecContext.CtxHandle.dwUpper := -1;
  end;
  if (aSecContext.CredHandle.dwLower <> -1) or (aSecContext.CredHandle.dwUpper <> -1) then begin
    FreeCredentialsHandle(@aSecContext.CredHandle);
    aSecContext.CredHandle.dwLower := -1;
    aSecContext.CredHandle.dwUpper := -1;
  end;
end;

end.
