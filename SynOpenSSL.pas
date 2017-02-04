/// OpenSSL library direct access classes
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynOpenSSL;

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
  - initial revision

}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  {$else}
  {$ifdef FPC}
  dynlibs,
  {$endif}
  {$ifdef KYLIX3}
  LibC,
  SynKylix,
  {$endif}
  {$endif}
  SysUtils,
  Classes;
  // we tried to avoid any dependency to SynCommons or SynCrtSock units


{ -------------- OpenSSL library low-level interfaces, constants and types }

const
  SSL_ERROR_NONE = 0;
  SSL_ERROR_SSL = 1;
  SSL_ERROR_WANT_READ = 2;
  SSL_ERROR_WANT_WRITE = 3;
  SSL_ERROR_WANT_X509_LOOKUP = 4;
  SSL_ERROR_SYSCALL = 5;
  SSL_ERROR_ZERO_RETURN = 6;
  SSL_ERROR_WANT_CONNECT = 7;
  SSL_ERROR_WANT_ACCEPT = 8;

  SSL_ERROR_NOT_FATAL = [SSL_ERROR_NONE,
    SSL_ERROR_WANT_READ, SSL_ERROR_WANT_WRITE,
		SSL_ERROR_WANT_CONNECT, SSL_ERROR_WANT_ACCEPT];

  SSL_ST_CONNECT = $1000;
  SSL_ST_ACCEPT = $2000;
  SSL_ST_MASK = $0FFF;
  SSL_ST_INIT = (SSL_ST_CONNECT or SSL_ST_ACCEPT);
  SSL_ST_BEFORE = $4000;
  SSL_ST_OK = $03;
  SSL_ST_RENEGOTIATE = ($04 or SSL_ST_INIT);

  SSL_OP_ALL = $000FFFFF;
  SSL_OP_NO_SSLv2 = $01000000;
  SSL_OP_NO_SSLv3 = $02000000;
  SSL_OP_NO_COMPRESSION = $00020000;

  SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS = $00000800;

  BIO_CTRL_INFO = 3;
  BIO_CTRL_PENDING = 10;
  SSL_CTRL_OPTIONS = 32;
  SSL_VERIFY_NONE = $00;

  CRYPTO_LOCK = 1;
  CRYPTO_UNLOCK = 2;
  CRYPTO_READ = 4;
  CRYPTO_WRITE = 8;

  BIO_FLAGS_READ = 1;
  BIO_FLAGS_WRITE = 2;
  BIO_FLAGS_IO_SPECIAL = 4;
  BIO_FLAGS_RWS = (BIO_FLAGS_READ or BIO_FLAGS_WRITE or BIO_FLAGS_IO_SPECIAL);
  BIO_FLAGS_SHOULD_RETRY = 8;
  BIO_NOCLOSE = 0;
  BIO_CLOSE = 1;

  EVP_OK = 1;

type
  {$ifdef CPU64}
  size_t = UInt64;
  {$else}
  size_t = cardinal;
  {$endif}

  TSSL_METHOD = packed record
  end;
  PSSL_METHOD = ^TSSL_METHOD;

  TSSL_CTX = packed record
  end;
  PSSL_CTX = ^TSSL_CTX;

  TBIO = packed record
  end;
  PBIO = ^TBIO;
  PPBIO = ^PBIO;

  TSSL = packed record
  end;
  PSSL = ^TSSL;

  TX509_STORE = packed record
  end;
  PX509_STORE = ^TX509_STORE;

  TEVP_PKEY = packed record
  end;
  PEVP_PKEY = ^TEVP_PKEY;
  PPEVP_PKEY = ^PEVP_PKEY;
  PEVP_PKEY_CTX = pointer;

  PEVP_MD_CTX = pointer;
  PEVP_MD = pointer;

  ENGINE = pointer;

  TX509 = packed record
  end;
  PX509 = ^TX509;
  PPX509 = ^PX509;

  TASN1_STRING = record
    length: integer;
    type_: integer;
    data: PAnsiChar;
    flags: Longword;
  end;
  PASN1_STRING = ^TASN1_STRING;
  TASN1_OCTET_STRING = TASN1_STRING;
  PASN1_OCTET_STRING = ^TASN1_OCTET_STRING;
  TASN1_BIT_STRING = TASN1_STRING;
  PASN1_BIT_STRING = ^TASN1_BIT_STRING;

  TSetVerify_cb = function(Ok: integer; StoreCtx: PX509_STORE): integer; cdecl;

  TCRYPTO_THREADID = packed record
  end;
  PCRYPTO_THREADID = ^TCRYPTO_THREADID;

  TCRYPTO_dynlock_value = record
    Mutex: TRTLCriticalSection;
    // see https://www.delphitools.info/2011/11/30/fixing-tcriticalsection
    _padding: array[0..95] of Byte;
  end;
  PCRYPTO_dynlock_value = ^TCRYPTO_dynlock_value;
  CRYPTO_dynlock_value  = TCRYPTO_dynlock_value;

  TBIO_METHOD = packed record
  end;
  PBIO_METHOD = ^TBIO_METHOD;

  TX509_NAME = packed record
  end;
  PX509_NAME = ^TX509_NAME;

  TSTACK = packed record
  end;
  PSTACK = ^TSTACK;

  TASN1_OBJECT = packed record
  end;
  PASN1_OBJECT = ^TASN1_OBJECT;

  TStatLockLockCallback = procedure(Mode: integer; N: integer; _file: PAnsiChar; Line: integer); cdecl;
  TStatLockIDCallback = function: Longword; cdecl;
  TCryptoThreadIDCallback = procedure(ID: PCRYPTO_THREADID); cdecl;

  TDynLockCreateCallback = function(_file: PAnsiChar; Line: integer): PCRYPTO_dynlock_value; cdecl;
  TDynLockLockCallback = procedure(Mode: integer; L: PCRYPTO_dynlock_value; _file: PAnsiChar; Line: integer); cdecl;
  TDynLockDestroyCallback = procedure(L: PCRYPTO_dynlock_value; _file: PAnsiChar; Line: integer); cdecl;
  pem_password_cb = function(buf: pointer; size: integer; rwflag: integer; userdata: pointer): integer; cdecl;

type
  /// low-level exception raised during OpenSSL library access
  EOpenSSL = class(Exception);

  {$ifdef HASCODEPAGE}
  TOpenSSLBytes = RawByteString;
  {$else}
  TOpenSSLBytes = AnsiString;
  {$endif}

  /// direct access to the OpenSSL library
  // - this wrapper will initialize both libcrypto and libssl libraries
  TOpenSSLLib = class
  public
  protected
    fLibCrypto, fLibSSL: HMODULE;
    fLibVersion: AnsiString;
    fLibPath: TFileName;
    fAPLNNotSupported: boolean;
  public
    // libcrypto API functions
    CRYPTO_num_locks: function: integer; cdecl;
    CRYPTO_set_locking_callback: procedure(callback: TStatLockLockCallback); cdecl;
    CRYPTO_set_dynlock_create_callback: procedure(callback: TDynLockCreateCallBack); cdecl;
    CRYPTO_set_dynlock_lock_callback: procedure(callback: TDynLockLockCallBack); cdecl;
    CRYPTO_set_dynlock_destroy_callback: procedure(callback: TDynLockDestroyCallBack); cdecl;
    CRYPTO_cleanup_all_ex_data: procedure; cdecl;
    ERR_remove_state: procedure(tid: cardinal); cdecl;
    ERR_free_strings: procedure; cdecl;
    ERR_error_string_n: procedure(err: cardinal; buf: PAnsiChar; len: size_t); cdecl;
    ERR_get_error: function: cardinal; cdecl;
    ERR_remove_thread_state: procedure(pid: cardinal); cdecl;
    ERR_load_BIO_strings: function: cardinal; cdecl;
    EVP_cleanup: procedure; cdecl;
    EVP_PKEY_free: procedure(pkey: PEVP_PKEY); cdecl;
    BIO_new: function(BioMethods: PBIO_METHOD): PBIO; cdecl;
    BIO_ctrl: function(bp: PBIO; cmd: integer; larg: longint; parg: pointer): longint; cdecl;
    BIO_set_flags: procedure(bp: PBIO; flags: longint); cdecl;
    BIO_test_flags: function(bp: PBIO; flags: longint): longint; cdecl;
    BIO_clear_flags: procedure(bp: PBIO; flags: longint); cdecl;
    BIO_new_mem_buf: function(buf: pointer; len: integer): PBIO; cdecl;
    BIO_free: function(b: PBIO): integer; cdecl;
    BIO_s_mem: function: PBIO_METHOD; cdecl;
    BIO_read: function(b: PBIO; buf: pointer; Len: integer): integer; cdecl;
    BIO_write: function(b: PBIO; buf: pointer; Len: integer): integer; cdecl;
    BIO_new_socket: function(sock: integer; close_flag: integer): PBIO; cdecl;
    X509_get_issuer_name: function(cert: PX509): PX509_NAME; cdecl;
    X509_get_subject_name: function(cert: PX509): PX509_NAME; cdecl;
    X509_free: procedure(cert: PX509); cdecl;
    X509_NAME_print_ex: function(bout: PBIO; nm: PX509_NAME; indent: integer; flags: cardinal): integer; cdecl;
    sk_num: function(stack: PSTACK): integer; cdecl;
    sk_pop: function(stack: PSTACK): pointer; cdecl;
    ASN1_BIT_STRING_get_bit: function(a: PASN1_BIT_STRING; n: integer): integer; cdecl;
    OBJ_obj2nid: function(o: PASN1_OBJECT): integer; cdecl;
    OBJ_nid2sn: function(n: integer): PAnsiChar; cdecl;
    ASN1_STRING_data: function(x: PASN1_STRING): pointer; cdecl;
    PEM_read_bio_X509: function(bp: PBIO; x: PX509; cb: pem_password_cb; u: pointer): PX509; cdecl;
    PEM_read_bio_PrivateKey: function(bp: PBIO; x: PPEVP_PKEY; cb: pem_password_cb; u: pointer): PEVP_PKEY; cdecl;
    PEM_read_bio_RSAPrivateKey: function(bp: PBIO; x: PPEVP_PKEY; cb: pem_password_cb; u: pointer): PEVP_PKEY; cdecl;
    PEM_read_bio_PUBKEY: function(bp: PBIO; x: PPEVP_PKEY; cb: pem_password_cb; u: pointer): PEVP_PKEY; cdecl;
    EVP_MD_CTX_create: function: PEVP_MD_CTX; cdecl;
    EVP_MD_CTX_destroy: procedure(ctx: PEVP_MD_CTX); cdecl;
    EVP_sha256: function: PEVP_MD; cdecl;
    EVP_PKEY_size: function(key: PEVP_PKEY): integer; cdecl;
    EVP_DigestSignInit: function(aCtx: PEVP_MD_CTX; aPCtx: PEVP_PKEY_CTX; aType: PEVP_MD; aEngine: ENGINE; aKey: PEVP_PKEY): integer; cdecl;
    EVP_DigestUpdate: function(ctx: PEVP_MD_CTX; d: pointer; cnt: cardinal): integer; cdecl;
    EVP_DigestSignFinal: function(ctx: PEVP_MD_CTX; d: PByte; var cnt: cardinal): integer; cdecl;
    EVP_DigestVerifyInit: function(aCtx: PEVP_MD_CTX; aPCtx: PEVP_PKEY_CTX; aType: PEVP_MD; aEngine: ENGINE; aKey: pEVP_PKEY): integer; cdecl;
    EVP_DigestVerifyFinal: function(ctx: pEVP_MD_CTX; d: PByte; cnt: cardinal): integer; cdecl;
    CRYPTO_malloc: function(aLength: longint; f: PAnsiChar; aLine: integer): pointer; cdecl;
    CRYPTO_free: procedure(str: pointer); cdecl;
    SSLeay_version: function(t: integer): PAnsiChar; cdecl;
    // libssl API functions
    SSL_library_init: function: integer; cdecl;
    SSL_load_error_strings: procedure; cdecl;
    SSLv3_method: function: PSSL_METHOD; cdecl;
    SSLv23_method: function: PSSL_METHOD; cdecl;
    TLSv1_method: function: PSSL_METHOD; cdecl;
    TLSv1_1_method: function: PSSL_METHOD; cdecl;
    TLSv1_2_method: function: PSSL_METHOD; cdecl;
    SSL_CTX_new: function(meth: PSSL_METHOD): PSSL_CTX; cdecl;
    SSL_CTX_free: procedure(ctx: PSSL_CTX); cdecl;
    SSL_CTX_set_verify: procedure(ctx: PSSL_CTX; mode: integer; callback: TSetVerify_cb); cdecl;
    SSL_CTX_use_PrivateKey: function(ctx: PSSL_CTX; pkey: PEVP_PKEY): integer; cdecl;
    SSL_CTX_use_RSAPrivateKey: function(ctx: PSSL_CTX; pkey: PEVP_PKEY): integer; cdecl;
    SSL_CTX_use_certificate: function(ctx: PSSL_CTX; x: PX509): integer; cdecl;
    SSL_CTX_check_private_key: function(ctx: PSSL_CTX): integer; cdecl;
    SSL_CTX_use_certificate_file: function(ctx: PSSL_CTX; f: PAnsiChar; t: integer): integer; cdecl;
    SSL_CTX_use_RSAPrivateKey_file: function(ctx: PSSL_CTX; f: PAnsiChar; t: integer): integer; cdecl;
    SSL_CTX_get_cert_store: function(ctx: PSSL_CTX): PX509_STORE; cdecl;
    SSL_CTX_ctrl: function(ctx: PSSL_CTX; cmd, i: integer; p: pointer): integer; cdecl;
    SSL_CTX_load_verify_locations: function(ctx: PSSL_CTX; CAFile: PAnsiChar; CAPath: PAnsiChar): integer; cdecl;
    SSL_CTX_use_certificate_chain_file: function(ctx: PSSL_CTX; CAFile: PAnsiChar): integer; cdecl;
    SSL_CTX_set_alpn_protos: function(ctx: PSSL_CTX; protos: PAnsiChar; protos_len: integer): integer; cdecl;
    SSL_new: function(ctx: PSSL_CTX): PSSL; cdecl;
    SSL_get_version: function(ssl: PSSL): PAnsiChar; cdecl;
    SSL_set_bio: procedure(s: PSSL; rbio, wbio: PBIO); cdecl;
    SSL_get_peer_certificate: function(s: PSSL): PX509; cdecl;
    SSL_get_error: function(s: PSSL; ret_code: integer): integer; cdecl;
    SSL_shutdown: function(s: PSSL): integer; cdecl;
    SSL_free: procedure(s: PSSL); cdecl;
    SSL_connect: function(s: PSSL): integer; cdecl;
    SSL_set_connect_state: procedure(s: PSSL); cdecl;
    SSL_set_accept_state: procedure(s: PSSL); cdecl;
    SSL_read: function(s: PSSL; buf: pointer; num: integer): integer; cdecl;
    SSL_write: function(s: PSSL; buf: pointer; num: integer): integer; cdecl;
    SSL_state: function(s: PSSL): integer; cdecl;
    SSL_pending: function(s: PSSL): integer; cdecl;
    SSL_set_cipher_list: function(s: PSSL; ciphers: PAnsiChar): integer; cdecl;
    SSL_get0_alpn_selected: procedure (s: PSSL; out data: PAnsiChar; out len: integer); cdecl;
    SSL_clear: function(s: PSSL): integer; cdecl;
    // aliases
    EVP_DigestVerifyUpdate: function(ctx: PEVP_MD_CTX; d: pointer; cnt: cardinal): integer; cdecl;
    sk_ASN1_OBJECT_num: function(stack: PSTACK): integer; cdecl;  // = sk_num
    sk_GENERAL_NAME_num: function(stack: PSTACK): integer; cdecl; // = sk_num
    sk_GENERAL_NAME_pop: function(stack: PSTACK): pointer; cdecl; // = sk_pop
    // helper functions
    function BIO_pending(bp: PBIO): integer; {$ifdef HASINLINE}inline;{$endif}
    function BIO_get_mem_data(bp: PBIO; parg: pointer): integer; {$ifdef HASINLINE}inline;{$endif}
    function BIO_get_flags(b: PBIO): integer; {$ifdef HASINLINE}inline;{$endif}
    function BIO_should_retry(b: PBIO): boolean; {$ifdef HASINLINE}inline;{$endif}
    function SSL_CTX_set_options(ctx: pointer; op: integer): integer;
    function SSL_is_init_finished(s: PSSL): boolean; {$ifdef HASINLINE}inline;{$endif}
    function SSL_is_fatal_error(s: PSSL; ret_code: integer; raiseexception: boolean;
      last_err: PCardinal = nil): boolean;
    function ERR_error_string(err: cardinal): string;
    function SSL_error(ssl: PSSL; ret_code: integer; out errormsg: string): integer;
    procedure SetCertificate(ctx: PSSL_CTX; const certificate, privatekey: TOpenSSLBytes;
      const password: string = ''); overload;
    procedure SetCertificate(ctx: PSSL_CTX; certificate, privatekey: pointer;
      certlen, privlen: integer; const password: string = ''); overload;
    function EVP_sha256_sign(msg, privkey: pointer; msglen, privkeylen: integer;
      const password: string = ''): TOpenSSLBytes;
    function EVP_sha256_verify(msg, pubkey, sign: pointer;
      msglen, pubkeylen, signlen: integer; const password: string = ''): boolean;
  public
    /// load the OpenSSL libraries
    // - and retrieve all needed procedure addresses for libcrypto/libssl
    // - raise a EOpenSSL in case of missing or invalid .dll / .so files
    constructor Create(const aFolderName: TFileName = '');
    /// release associated memory and linked library
    destructor Destroy; override;
    /// the associated libcrypto library handle
    property LibCrypto: HMODULE read fLibCrypto write fLibCrypto;
    /// the associated libssl library handle
    property LibSSL: HMODULE read fLibSSL write fLibSSL;
    /// we allow some APLN-related missing entries in the loaded API 
    property APLNNotSupported: boolean read fAPLNNotSupported;
  published
    /// the version information about the loaded library
    property LibVersion: AnsiString read fLibVersion;
    /// the loaded libray path name
    property LibPath: TFileName read fLibPath;
  end;

var
  /// global expected location of the OpenSSL .dll / .so files
  // - as used by OpenSSL global function
  OpenSSLFolderName: TFileName;

  /// global variable used to inline OpenSSL function
  // - do not use directly
  SharedOpenSSL: TOpenSSLLib;

  /// global variable used to inline OpenSSL function
  // - do not use directly
  TryLoadOpenSSLState: (ossNotTested, ossAvailable, ossNotAvailable);

/// global function used to inline OpenSSL function
// - do not call directly
procedure TryLoadOpenSSL;

/// access to a shared OpenSSL library functions
// - will load and initialize it, if necessary, looking in the OpenSSLFolderName
// - raises a EOpenSSL if the library is not available
function OpenSSL: TOpenSSLLib; {$ifdef HASINLINE}inline;{$endif}

/// return TRUE if a shared OpenSSL library functions is available
// - will load and initialize it, if necessary, looking in the OpenSSLFolderName
function OpenSSLAvailable: boolean;


{ -------------- TOpenSSL* high-level wrapper classes and types }



implementation

{ -------------- OpenSSL library low-level interfaces, constants and types }

procedure TryLoadOpenSSL;
begin
  case TryLoadOpenSSLState of
  ossNotTested: begin
    TryLoadOpenSSLState := ossNotAvailable;
    SharedOpenSSL := TOpenSSLLib.Create(OpenSSLFolderName);
    TryLoadOpenSSLState := ossAvailable;
  end;
  ossNotAvailable:
    raise EOpenSSL.Create('No OpenSSL available'); // test once
  end;
end;

function OpenSSLAvailable: boolean;
begin
  if TryLoadOpenSSLState = ossNotTested then
    try
      TryLoadOpenSSL;
      result := true;
    except
      result := false;
    end
  else
    result := TryLoadOpenSSLState = ossAvailable;
end;

function OpenSSL: TOpenSSLLib;
begin
  if TryLoadOpenSSLState <> ossAvailable then
    TryLoadOpenSSL;
  result := SharedOpenSSL;
end;

const
  {$ifdef MSWINDOWS}
  LIBSSL_NAME = 'ssleay32.dll';
  LIBCRYPTO_NAME = 'libeay32.dll';
  {$else}
  LIBSSL_NAME = 'libssl.so.1.0.0';
  LIBCRYPTO_NAME = 'libcrypto.so.1.0.0';
  {$endif}

  LIBCRYPTO_ENTRIES: array[0..50] of PChar =
    ('CRYPTO_num_locks', 'CRYPTO_set_locking_callback', 'CRYPTO_set_dynlock_create_callback',
     'CRYPTO_set_dynlock_lock_callback', 'CRYPTO_set_dynlock_destroy_callback',
     'CRYPTO_cleanup_all_ex_data', 'ERR_remove_state', 'ERR_free_strings',
     'ERR_error_string_n', 'ERR_get_error', 'ERR_remove_thread_state', 'ERR_load_BIO_strings',
     'EVP_cleanup', 'EVP_PKEY_free', 'BIO_new', 'BIO_ctrl', 'BIO_set_flags', 'BIO_test_flags',
     'BIO_clear_flags', 'BIO_new_mem_buf', 'BIO_free', 'BIO_s_mem', 'BIO_read', 'BIO_write',
     'BIO_new_socket', 'X509_get_issuer_name', 'X509_get_subject_name', 'X509_free',
     'X509_NAME_print_ex', 'sk_num', 'sk_pop', 'ASN1_BIT_STRING_get_bit',
     'OBJ_obj2nid', 'OBJ_nid2sn', 'ASN1_STRING_data', 'PEM_read_bio_X509',
     'PEM_read_bio_PrivateKey', 'PEM_read_bio_RSAPrivateKey', 'PEM_read_bio_PUBKEY',
     'EVP_MD_CTX_create', 'EVP_MD_CTX_destroy',
     'EVP_sha256', 'EVP_PKEY_size', 'EVP_DigestSignInit',
     'EVP_DigestUpdate', 'EVP_DigestSignFinal', 'EVP_DigestVerifyInit',
     'EVP_DigestVerifyFinal', 'CRYPTO_malloc', 'CRYPTO_free', 'SSLeay_version');
  LIBSSL_ENTRIES: array[0..37] of PChar =
    ('SSL_library_init', 'SSL_load_error_strings', 'SSLv3_method', 'SSLv23_method',
     'TLSv1_method', 'TLSv1_1_method', 'TLSv1_2_method', 'SSL_CTX_new',
     'SSL_CTX_free', 'SSL_CTX_set_verify', 'SSL_CTX_use_PrivateKey',
     'SSL_CTX_use_RSAPrivateKey', 'SSL_CTX_use_certificate',
     'SSL_CTX_check_private_key', 'SSL_CTX_use_certificate_file',
     'SSL_CTX_use_RSAPrivateKey_file', 'SSL_CTX_get_cert_store',
     'SSL_CTX_ctrl', 'SSL_CTX_load_verify_locations',
     'SSL_CTX_use_certificate_chain_file', 'SSL_CTX_set_alpn_protos',
     'SSL_new', 'SSL_get_version', 'SSL_set_bio',
     'SSL_get_peer_certificate', 'SSL_get_error', 'SSL_shutdown',
     'SSL_free', 'SSL_connect', 'SSL_set_connect_state', 'SSL_set_accept_state',
     'SSL_read', 'SSL_write', 'SSL_state', 'SSL_pending', 'SSL_set_cipher_list',
     'SSL_get0_alpn_selected', 'SSL_clear');

var
  SharedMutex: array of TCRYPTO_dynlock_value;

procedure ssl_lock_callback(Mode, N: Integer; _file: PAnsiChar; Line: Integer); cdecl;
begin
	if Mode and CRYPTO_LOCK <> 0 then
    EnterCriticalSection(SharedMutex[N].Mutex)
	else
    LeaveCriticalSection(SharedMutex[N].Mutex);
end;

procedure ssl_lock_dyn_callback(Mode: Integer; L: PCRYPTO_dynlock_value;
  _file: PAnsiChar; Line: Integer); cdecl;
begin
  if Mode and CRYPTO_LOCK <> 0 then
    EnterCriticalSection(L^.Mutex)
  else
    LeaveCriticalSection(L^.Mutex)
end;

function ssl_lock_dyn_create_callback(_file: PAnsiChar; Line: Integer): PCRYPTO_dynlock_value; cdecl;
begin
  Getmem(result, sizeof(result^));
  InitializeCriticalSection(result^.Mutex);
end;

procedure ssl_lock_dyn_destroy_callback(L: PCRYPTO_dynlock_value;
  _file: PAnsiChar; Line: Integer); cdecl;
begin
  DeleteCriticalSection(L^.Mutex);
  Freemem(L);
end;


{ TOpenSSLLib }

constructor TOpenSSLLib.Create(const aFolderName: TFileName);

  function LoadLib(api,name: PPointer; last: integer; var h: HMODULE;
    const lib: TFileName): TFileName;
  var i: integer;
  begin
    if aFolderName <> '' then
      result := IncludeTrailingPathDelimiter(aFolderName) + lib
    else
      result := lib;
    h := SafeLoadLibrary(result);
    if h = 0 then
      raise EOpenSSL.CreateFmt('%s not found', [result]);
    for i := 0 to last do begin
      api^ := GetProcAddress(h, name^);
      if api^ = nil then
        if (api = @@SSL_CTX_set_alpn_protos) or (api = @@SSL_get0_alpn_selected) then
          fAPLNNotSupported := true
        else begin
          FreeLibrary(h);
          h := 0;
          if @h = @fLibSSL then begin
            FreeLibrary(fLibCrypto);
            fLibCrypto := 0;
          end;
          raise EOpenSSL.CreateFmt('Missing %s in %s', [PAnsiChar(name^), result]);
        end;
      inc(api);
      inc(name);
    end;
  end;

var i: integer;
begin
  LoadLib(@@CRYPTO_num_locks, @LIBCRYPTO_ENTRIES, high(LIBCRYPTO_ENTRIES),
    fLibCrypto, LIBCRYPTO_NAME);
  LoadLib(@@SSL_library_init, @LIBSSL_ENTRIES, high(LIBSSL_ENTRIES),
    fLibSSL, LIBSSL_NAME);
  EVP_DigestVerifyUpdate := @EVP_DigestUpdate;
  sk_ASN1_OBJECT_num := @sk_num;
  sk_GENERAL_NAME_num := @sk_num;
  sk_GENERAL_NAME_pop := @sk_pop;
  if SharedMutex = nil then begin
    SetLength(SharedMutex, CRYPTO_num_locks);
    for i := 0 to high(SharedMutex) do
      InitializeCriticalSection(SharedMutex[i].Mutex);
  end;
	CRYPTO_set_locking_callback(ssl_lock_callback);
  CRYPTO_set_dynlock_create_callback(ssl_lock_dyn_create_callback);
	CRYPTO_set_dynlock_lock_callback(ssl_lock_dyn_callback);
  CRYPTO_set_dynlock_destroy_callback(ssl_lock_dyn_destroy_callback);
  SSL_load_error_strings;
  SSL_library_init;
  fLibVersion := SSLeay_version(0);
  fLibPath := ExtractFilePath(GetModuleName(flibCrypto));
end;

destructor TOpenSSLLib.Destroy;
var i: integer;
begin
  if fLibCrypto<>0 then begin
    CRYPTO_set_locking_callback(nil);
    CRYPTO_set_dynlock_create_callback(nil);
    CRYPTO_set_dynlock_lock_callback(nil);
    CRYPTO_set_dynlock_destroy_callback(nil);
    for i := 0 to high(SharedMutex) do
      DeleteCriticalSection(SharedMutex[i].Mutex);
    SharedMutex := nil;
    EVP_cleanup;
    CRYPTO_cleanup_all_ex_data();
    ERR_remove_state(0);
    ERR_free_strings;
    FreeLibrary(fLibCrypto);
    FreeLibrary(fLibSSL);
  end;
  inherited Destroy;
end;

function TOpenSSLLib.BIO_get_flags(b: PBIO): integer;
begin
  result := BIO_test_flags(b, -1);
end;

function TOpenSSLLib.BIO_get_mem_data(bp: PBIO; parg: pointer): integer;
begin
  result := BIO_ctrl(bp, BIO_CTRL_INFO, 0, parg);
end;

function TOpenSSLLib.BIO_pending(bp: PBIO): integer;
begin
  result := BIO_ctrl(bp, BIO_CTRL_PENDING, 0, nil);
end;

function TOpenSSLLib.BIO_should_retry(b: PBIO): boolean;
begin
  result := BIO_test_flags(b, BIO_FLAGS_SHOULD_RETRY) <> 0;
end;

function TOpenSSLLib.SSL_CTX_set_options(ctx: pointer; op: integer): integer;
begin
  result := SSL_CTX_ctrl(ctx, SSL_CTRL_OPTIONS, op, nil);
end;

function TOpenSSLLib.ERR_error_string(err: cardinal): string;
var tmp: array[0..511] of AnsiChar;
begin
  ERR_error_string_n(err, @tmp, sizeof(tmp));
  result := string(tmp);
end;

function TOpenSSLLib.SSL_error(ssl: PSSL; ret_code: integer;
  out errormsg: string): integer;
var err: cardinal;
begin
  err := SSL_get_error(ssl, ret_code);
  result := err;
  while err <> SSL_ERROR_NONE do begin
    if not (err in SSL_ERROR_NOT_FATAL) then
      errormsg := ERR_error_string(err);
    err := ERR_get_error;
  end;
end;

function TOpenSSLLib.SSL_is_fatal_error(s: PSSL; ret_code: integer;
  raiseexception: boolean; last_err: PCardinal): boolean;
var err: cardinal;
begin
  if ret_code >= 0 then
    result := false
  else begin
    err := SSL_get_error(s, ret_code);
    if last_err <> nil then
      last_err^ := err;
    if err in SSL_ERROR_NOT_FATAL then
      result := false
    else if raiseexception then
      raise EOpenSSL.CreateFmt('SSL_get_error %d [%s]', [err, ERR_error_string(err)])
    else
      result := true;
  end;
end;

function TOpenSSLLib.SSL_is_init_finished(s: PSSL): boolean;
begin
  result := SSL_state(s) = SSL_ST_OK;
end;

procedure TOpenSSLLib.SetCertificate(ctx: PSSL_CTX; certificate, privatekey: pointer;
  certlen, privlen: integer; const password: string);
var cert, priv: PBIO;
    x509: PX509;
    pkey: PEVP_PKEY;
begin
  cert := BIO_new_mem_buf(certificate, certlen);
  x509 := PEM_read_bio_X509(cert, nil, nil, nil);
  priv := BIO_new_mem_buf(privatekey, privlen);
  pkey := PEM_read_bio_PrivateKey(priv, nil, nil, pointer(AnsiString(password)));
  try
    SSL_CTX_use_certificate(ctx, x509);
    SSL_CTX_use_privatekey(ctx, pkey);
    if SSL_CTX_check_private_key(ctx) = 0 then
      raise EOpenSSL.Create('SetCertificate: private key does''nt match certificate');
  finally
    EVP_PKEY_free(pkey);
    BIO_free(priv);
    X509_free(x509);
    BIO_free(cert);
  end;
end;

procedure TOpenSSLLib.SetCertificate(ctx: PSSL_CTX;
  const certificate, privatekey: TOpenSSLBytes; const password: string);
begin
  SetCertificate(ctx, pointer(certificate), pointer(privatekey),
    length(certificate), length(privatekey), password);
end;

function TOpenSSLLib.EVP_sha256_sign(msg, privkey: pointer; msglen, privkeylen: integer;
  const password: string): TOpenSSLBytes;
var priv: PBIO;
    pkey: PEVP_PKEY;
    ctx: PEVP_MD_CTX;
    size: cardinal;
begin
  result := '';
  if (privkey = nil) or (privkeylen = 0) then begin
    priv := nil;
    pkey := nil;
  end
  else begin
    priv := BIO_new_mem_buf(privkey, privkeylen);
    pkey := PEM_read_bio_PrivateKey(priv, nil, nil, pointer(AnsiString(password)));
  end;
  try
    ctx := EVP_MD_CTX_create;
    try
      if EVP_DigestSignInit(ctx, nil, EVP_sha256, nil, pkey) = EVP_OK then
        if EVP_DigestUpdate(ctx, msg, msglen) = EVP_OK then
          if EVP_DigestSignFinal(ctx, nil, size) = EVP_OK then begin
            SetLength(result, size);
            if EVP_DigestSignFinal(ctx, pointer(result), size) <> EVP_OK then
              result := '';
          end;
    finally
      EVP_MD_CTX_destroy(ctx);
    end;
  finally
    if pkey <> nil then
      EVP_PKEY_free(pkey);
    if priv <> nil then
      BIO_free(priv);
  end;
end;

function TOpenSSLLib.EVP_sha256_verify(msg, pubkey, sign: pointer;
  msglen, pubkeylen, signlen: integer; const password: string): boolean;
var pub: PBIO;
    pkey: PEVP_PKEY;
    ctx: PEVP_MD_CTX;
begin
  result := false;
  if (pubkey = nil) or (pubkeylen <= 0) or (sign = nil) or (signlen <= 0) then
    exit;
  pub := BIO_new_mem_buf(pubkey, pubkeylen);
  pkey := PEM_read_bio_PUBKEY(pub, nil, nil, pointer(AnsiString(password)));
  ctx := EVP_MD_CTX_create;
  try
    if EVP_DigestVerifyInit(ctx, nil, EVP_sha256, nil, pkey) = EVP_OK then
      if EVP_DigestVerifyUpdate(ctx, msg, msglen) = EVP_OK then
        result := EVP_DigestVerifyFinal(ctx, sign, signlen) = EVP_OK;
  finally
    EVP_MD_CTX_destroy(ctx);
  end;
  if pkey <> nil then begin
  	EVP_PKEY_free(pkey);
    BIO_free(pub);
  end;
end;


{ -------------- TOpenSSL* high-level wrapper classes and types }


initialization

finalization
  SharedOpenSSL.Free;
end.
