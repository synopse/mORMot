unit SynGSSAPI;

{
  Server side Kerberos authentication support using GSSAPI
  Both MIT and Heimdal implementation libraries are supported
}

interface

uses
  SysUtils,
  Classes;

type
  EGSSError = class(Exception)
  private
    FMajorStatus: Cardinal;
    FMinorStatus: Cardinal;
  public
    constructor Create(AMajorStatus, AMinorStatus: Cardinal; const APrefix: String);
    property MajorStatus: Cardinal read FMajorStatus;
    property MinorStatus: Cardinal read FMinorStatus;
  end;

/// Call this function to check whether gssapi library found or not
function GSSLibraryFound: Boolean;
/// Accepts security context provided by client
// For this method to work the following conditions should be met:
// - KRB5_KTNAME environment variable should be set and point to valid readable keytab file
// - the keytab file should contain SvcName in the form HTTP/<host_FQDN>[:port]@REALM
function GSSAcceptSecurityContext(const InputToken: RawByteString;
  const SPN: AnsiString; var GSSContext: Pointer; out ClientName: AnsiString;
  out OutputToken: RawByteString): Boolean;
/// Releases previously accepted security context
procedure GSSReleaseContext(var GSSContext: Pointer);
/// Lists supported security mechanisms in form
// sasl:name:description
// Not all mechanisms provide human readable name and description
procedure GSSEnlistMechsSupported(MechList: TStringList);

const // This is here to be more compatible with SynSSPIAuth
  /// HTTP header to be set for SSPI authentication
  SECPKGNAMEHTTPWWWAUTHENTICATE = 'WWW-Authenticate: Negotiate';
  /// HTTP header pattern received for SSPI authentication
  SECPKGNAMEHTTPAUTHORIZATION = 'AUTHORIZATION: NEGOTIATE ';

implementation

const
  GSSLib_MIT = 'libgssapi_krb5.so.2';
  GSSLib_Heimdal = 'libgssapi.so.3';

  GSS_C_NO_NAME = nil;

  // Some "helper" definitions to make the status code macros obvious.
  GSS_C_CALLING_ERROR_OFFSET = 24;
  GSS_C_ROUTINE_ERROR_OFFSET = 16;
  GSS_C_SUPPLEMENTARY_OFFSET =  0;
  GSS_C_CALLING_ERROR_MASK = &0000377;
  GSS_C_ROUTINE_ERROR_MASK = &0000377;
  GSS_C_SUPPLEMENTARY_MASK = &0177777;

  // Supplementary info bits:
  GSS_S_CONTINUE_NEEDED = 1 shl (GSS_C_SUPPLEMENTARY_OFFSET + 0);
  GSS_S_DUPLICATE_TOKEN = 1 shl (GSS_C_SUPPLEMENTARY_OFFSET + 1);
  GSS_S_OLD_TOKEN       = 1 shl (GSS_C_SUPPLEMENTARY_OFFSET + 2);
  GSS_S_UNSEQ_TOKEN     = 1 shl (GSS_C_SUPPLEMENTARY_OFFSET + 3);
  GSS_S_GAP_TOKEN       = 1 shl (GSS_C_SUPPLEMENTARY_OFFSET + 4);

  // Status code types for gss_display_status
  GSS_C_GSS_CODE  = 1;
  GSS_C_MECH_CODE = 2;

  // Expiration time of 2^32-1 seconds means infinite lifetime for a
  // credential or security context
  GSS_C_INDEFINITE = $FFFFFFFF;

  // Credential usage options
  GSS_C_BOTH      = 0;
  GSS_C_INITIATE  = 1;
  GSS_C_ACCEPT    = 2;

type
  gss_name_t = Pointer;

  gss_OID_desc = record
    length: SizeInt;
    elements: Pointer;
  end;
  gss_OID = ^gss_OID_desc;
  gss_OID_ptr = ^gss_OID;
  gss_OID_array = array [0..0] of gss_OID_desc;
  gss_OID_descs = ^gss_OID_array;

  gss_OID_set_desc = record
    count: SizeInt;
    elements: gss_OID_descs;
  end;
  gss_OID_set = ^gss_OID_set_desc;
  gss_OID_set_ptr = ^gss_OID_set;

  gss_buffer_desc = record
    length: SizeInt;
    value: Pointer;
  end;
  gss_buffer_t = ^gss_buffer_desc;

  gss_channel_bindings_struct = record
    initiator_addrtype: Cardinal;
    initiator_address: gss_buffer_desc;
    acceptor_addrtype: Cardinal;
    acceptor_address: gss_buffer_desc;
    application_data: gss_buffer_desc;
  end;
  gss_channel_bindings_t = ^gss_channel_bindings_struct;

  PGSSAPIFunctions = ^TGSSAPIFunctions;
  TGSSAPIFunctions = record
    gsslib: TLibHandle;
    gss_indicate_mechs: function (
      out minor_status: Cardinal;
      out mech_set: gss_OID_set): Cardinal;
    gss_release_oid_set: function (
      out minor_status: Cardinal;
      out mech_set: gss_OID_set): Cardinal;
    gss_inquire_saslname_for_mech: function (
      out minor_status: Cardinal;
      const desired_mech: gss_OID;
      out sasl_mech_name: gss_buffer_desc;
      out mech_name: gss_buffer_desc;
      out mech_description: gss_buffer_desc): Cardinal;
    gss_display_status: function (
      out minor_status: Cardinal;
      status: Cardinal;
      status_type: Integer;
      mech_type: gss_OID;
      out message_context: Cardinal;
      out status_string: gss_buffer_desc): Cardinal;
    gss_release_buffer: function (
      out minor_status: Cardinal;
      var buffer: gss_buffer_desc): Cardinal;
    gss_import_name: function (
      out minor_status: Cardinal;
      input_name_buffer: gss_buffer_t;
      input_name_type: gss_OID; // (used to be const)
      out output_name: Pointer): Cardinal;
    gss_acquire_cred: function (
      out minor_status: Cardinal;
      desired_name: Pointer;
      time_req: Cardinal;
      desired_mechs: gss_OID_set;
      cred_usage: Integer;
      out output_cred_handle: Pointer;
      actual_mechs: gss_OID_set_ptr;
      time_rec: PCardinal): Cardinal;
    gss_release_cred: function (
      out minor_status: Cardinal;
      var cred_handle: Pointer): Cardinal;
    gss_accept_sec_context: function (
      out minor_status: Cardinal;
      out context_handle: Pointer;
      acceptor_cred_handle: Pointer;
      var input_token_buffer: gss_buffer_desc;
      input_chan_bindings: gss_channel_bindings_t;
      out src_name: Pointer;
      out mech_type: gss_OID;
      out output_token: gss_buffer_desc;
      {out} ret_flags: PCardinal;
      {out} time_rec: PCardinal;
      {out} delegated_cred_handle: PPointer): Cardinal;
    gss_display_name: function (
      out minor_status: Cardinal;
      input_name: Pointer;
      out output_name_buffer: gss_buffer_desc;
      output_name_type: gss_OID_ptr): Cardinal;
    gss_release_name: function (
      out minor_status: Cardinal;
      var name: Pointer): Cardinal;
    gss_delete_sec_context: function (
      out minor_status: Cardinal;
      gss_context: PPointer;
      buffer: gss_buffer_t): Cardinal;
  end;

var
  GSS_C_NT_USER_NAME: gss_OID;
  GSS_KRB5_NT_PRINCIPAL_NAME: gss_OID;

  GSSAPI: PGSSAPIFunctions = nil;

  GSSAPINotFound: Boolean = False;

{*
 * The macros that test status codes for error conditions.  Note that the
 * GSS_ERROR() macro has changed slightly from the V1 GSSAPI so that it now
 * evaluates its argument only once.
 *}
function GSS_CALLING_ERROR(x: Cardinal): Cardinal; inline;
begin
  Result := x and (GSS_C_CALLING_ERROR_MASK shl GSS_C_CALLING_ERROR_OFFSET);
end;

function GSS_ROUTINE_ERROR(x: Cardinal): Cardinal; inline;
begin
  Result := x and (GSS_C_ROUTINE_ERROR_MASK shl GSS_C_ROUTINE_ERROR_OFFSET);
end;

function GSS_SUPPLEMENTARY_INFO(x: Cardinal): Cardinal; inline;
begin
  Result := x and (GSS_C_SUPPLEMENTARY_MASK shl GSS_C_SUPPLEMENTARY_OFFSET);
end;

function GSS_ERROR(x: Cardinal): Cardinal; inline;
begin
  Result := x and
    ((GSS_C_CALLING_ERROR_MASK shl GSS_C_CALLING_ERROR_OFFSET) or
     (GSS_C_ROUTINE_ERROR_MASK shl GSS_C_ROUTINE_ERROR_OFFSET));
end;

procedure GSSCheck(AMajorStatus, AMinorStatus: Cardinal; const APrefix: String = ''); inline;
begin
  if GSS_ERROR(AMajorStatus) <> 0 then
    raise EGSSError.Create(AMajorStatus, AMinorStatus, APrefix);
end;

function GSSAcquireCredentials(const SPN: AnsiString): Pointer;
var
  MajSt, MinSt: Cardinal;
  SvcNameBuf: gss_buffer_desc;
  GSSNameHandle: Pointer;
begin
  GSSNameHandle := nil;
  SvcNameBuf.length := Length(SPN) + 1;
  SvcNameBuf.value := PAnsiChar(SPN);
  MajSt := GSSAPI^.gss_import_name(MinSt, @SvcNameBuf, GSS_KRB5_NT_PRINCIPAL_NAME{GSS_C_NT_USER_NAME}, GSSNameHandle);
  GSSCheck(MajSt, MinSt, 'gss_import_name() failed');
  try
    GSSCheck(
      GSSAPI^.gss_acquire_cred(MinSt, GSSNameHandle, GSS_C_INDEFINITE, nil, GSS_C_ACCEPT, Result, nil, nil),
      MinSt, 'gss_acquire_cred() failed');
  finally
    GSSAPI^.gss_release_name(MinSt, GSSNameHandle);
  end;
end;

procedure TryLoadGSSAPI;
var
  handle: TLibHandle;
  UseHeimdal: Boolean;
begin
  if not GSSAPINotFound then begin
    handle := SafeLoadLibrary(GSSLib_MIT);
    UseHeimdal := handle=0;
    if UseHeimdal then
      handle := SafeLoadLibrary(GSSLib_Heimdal);
    GSSAPINotFound := handle=0;
    if not GSSAPINotFound then begin
      New(GSSAPI);
      with GSSAPI^ do begin
        gsslib := handle;
        PPointer(@gss_indicate_mechs)^ := GetProcAddress(handle, 'gss_indicate_mechs');
        PPointer(@gss_release_oid_set)^ := GetProcAddress(handle, 'gss_release_oid_set');
        PPointer(@gss_inquire_saslname_for_mech)^ := GetProcAddress(handle, 'gss_inquire_saslname_for_mech');
        PPointer(@gss_display_status)^ := GetProcAddress(handle, 'gss_display_status');
        PPointer(@gss_release_buffer)^ := GetProcAddress(handle, 'gss_release_buffer');
        PPointer(@gss_import_name)^ := GetProcAddress(handle, 'gss_import_name');
        PPointer(@gss_acquire_cred)^ := GetProcAddress(handle, 'gss_acquire_cred');
        PPointer(@gss_release_cred)^ := GetProcAddress(handle, 'gss_release_cred');
        PPointer(@gss_accept_sec_context)^ := GetProcAddress(handle, 'gss_accept_sec_context');
        PPointer(@gss_display_name)^ := GetProcAddress(handle, 'gss_display_name');
        PPointer(@gss_release_name)^ := GetProcAddress(handle, 'gss_release_name');
        PPointer(@gss_delete_sec_context)^ := GetProcAddress(handle, 'gss_delete_sec_context');
        if UseHeimdal then begin
          Pointer(GSS_C_NT_USER_NAME) := GetProcAddress(handle, '__gss_c_nt_user_name_oid_desc');
          Pointer(GSS_KRB5_NT_PRINCIPAL_NAME) := GetProcAddress(handle, '__gss_krb5_nt_principal_name_oid_desc');
        end else begin
          Pointer(GSS_C_NT_USER_NAME) := PPointer(GetProcAddress(handle, 'GSS_C_NT_USER_NAME'))^;
          Pointer(GSS_KRB5_NT_PRINCIPAL_NAME) := PPointer(GetProcAddress(handle, 'GSS_KRB5_NT_PRINCIPAL_NAME'))^;
        end;
      end;
    end;
  end;
end;

procedure RequireGSSAPI;
begin
  if not GSSLibraryFound then
    raise ENotSupportedException.Create('No GSSAPI library found - please install either MIT or Heimdal GSSAPI implementation');
end;

// Public API

function GSSLibraryFound: Boolean;
begin
  if (GSSAPI=nil) and not GSSAPINotFound then
    TryLoadGSSAPI;
  Result := GSSAPI<>nil;
end;

function GSSAcceptSecurityContext(const InputToken: RawByteString;
  const SPN: AnsiString; var GSSContext: Pointer; out ClientName: String;
  out OutputToken: RawByteString): Boolean;
var
  MajSt, MinSt, Flags, Secs: Cardinal;
  InBuf, OutBuf, NameBuf: gss_buffer_desc;
  SelfCreds, ClientNameHandle: Pointer;
  NameType: gss_OID;
begin
  RequireGSSAPI;
  if SPN <> '' then
    SelfCreds := GSSAcquireCredentials(SPN)
  else
    SelfCreds := nil;
  try
    InBuf.length := Length(InputToken);
    InBuf.value := PChar(InputToken);
    GSSContext := nil; // It is important to clean GSSContext before call
    OutBuf.length := 0; // This indicates no panding output exists
    MajSt := gssapi^.gss_accept_sec_context(
      MinSt, GSSContext, SelfCreds, InBuf, nil,
      ClientNameHandle, NameType, OutBuf, @Flags, @Secs, nil);
    GSSCheck(MajSt, MinSt, 'Failed accepting security context');
    Result := (MajSt and GSS_S_CONTINUE_NEEDED) = 0;
    SetString(OutputToken, OutBuf.value, OutBuf.length);
    GSSAPI^.gss_release_buffer(MinSt, OutBuf);
    if ClientNameHandle <> nil then
      try
        GSSCheck(
          GSSAPI^.gss_display_name(MinSt, ClientNameHandle, NameBuf, @NameType),
          MinSt, 'Error retrieving client name');
        SetString(ClientName, NameBuf.value, NameBuf.length);
        GSSAPI^.gss_release_buffer(MinSt, NameBuf);
      finally
        GSSAPI^.gss_release_name(MinSt, ClientNameHandle);
      end;
  finally
    GSSAPI^.gss_release_cred(MinSt, SelfCreds);
  end;
end;

procedure GSSReleaseContext(var GSSContext: Pointer);
var
  MinSt: Cardinal;
begin
  RequireGSSAPI;
  GSSAPI^.gss_delete_sec_context(MinSt, @GSSContext, nil);
end;

procedure GSSEnlistMechsSupported(MechList: TStringList);
var
  i, MajSt, MinSt: Cardinal;
  Mechs: gss_OID_set;
  Buf_sasl, Buf_name, Buf_desc: gss_buffer_desc;
  Sasl, Name, Desc: String;
begin
  RequireGSSAPI;
  if MechList <> nil then begin
    MajSt := GSSAPI^.gss_indicate_mechs(MinSt, Mechs);
    for i := 0 to Pred(Mechs^.count) do begin
      MajSt := GSSAPI^.gss_inquire_saslname_for_mech(MinSt, @Mechs^.elements[i], Buf_sasl, Buf_name, Buf_desc);
      SetString(Sasl, Buf_sasl.value, Buf_sasl.length);
      SetString(Name, Buf_name.value, Buf_name.length);
      SetString(Desc, Buf_desc.value, Buf_desc.length);
      MechList.Add(Format('%s:%s:%s', [Sasl, Name, Desc]));
      GSSAPI^.gss_release_buffer(MinSt, Buf_sasl);
      GSSAPI^.gss_release_buffer(MinSt, Buf_name);
      GSSAPI^.gss_release_buffer(MinSt, Buf_desc);
    end;
    MajSt := GSSAPI^.gss_release_oid_set(MinSt, Mechs);
  end;
end;

{ EGSSError }

constructor EGSSError.Create(AMajorStatus, AMinorStatus: Cardinal; const APrefix: String);

  procedure GetDisplayStatus(var Msg: String; AErrorStatus: Cardinal; StatusType: Integer);
  var
    Str: String;
    MsgCtx: Cardinal;
    MsgBuf: gss_buffer_desc;
    MajSt, MinSt: Cardinal;
  begin
    repeat
      MajSt := GSSAPI^.gss_display_status(
        MinSt, AErrorStatus, StatusType, nil, MsgCtx, MsgBuf);
      SetString(Str, MsgBuf.value, MsgBuf.length);
      GSSAPI^.gss_release_buffer(MinSt, MsgBuf);
      if Msg <> '' then
        Msg := Msg + ': ' + Str
      else
        Msg := Str;
    until (GSS_ERROR(MajSt) <> 0) or (MsgCtx = 0);
  end;

var
  Msg: String;
begin
  Msg := APrefix;
  GetDisplayStatus(Msg, AMajorStatus, GSS_C_GSS_CODE);
  if AMinorStatus <> 0 then
    GetDisplayStatus(Msg, AMinorStatus, GSS_C_MECH_CODE);
  inherited Create(Msg);
  FMajorStatus := AMajorStatus;
  FMinorStatus := AMinorStatus;
end;

finalization
  if GSSAPI<>nil then begin
    FreeLibrary(GSSAPI^.gsslib);
    FreeAndNil(GSSAPI);
  end;
end.

