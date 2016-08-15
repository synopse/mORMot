unit ECCProcess;

interface

{$I Synopse.inc}

uses
  SysUtils,
  Classes,
  SynCommons,
  SynECC,
  SynCrypto,
  mORMot;

type
  ICommandLine = interface
    ['{77AB427C-1025-488B-8E04-3E62C8100E62}']
    function AsUTF8(const Switch, Default: RawUTF8; const Prompt: string): RawUTF8;
    function AsString(const Switch: RawUTF8; const Default, Prompt: string): string;
    function AsInt(const Switch: RawUTF8; Default: Int64; const Prompt: string): Int64;
    function AsDate(const Switch: RawUTF8; Default: TDateTime; const Prompt: string): TDateTime;
  end;

  TCommandLine = class(TInterfacedObjectWithCustomCreate, ICommandLine)
  private
    Values: TDocVariantData;
  public
    constructor Create; override;
    function AsUTF8(const Switch, Default: RawUTF8; const Prompt: string): RawUTF8;
    function AsString(const Switch: RawUTF8; const Default, Prompt: string): string;
    function AsInt(const Switch: RawUTF8; Default: Int64; const Prompt: string): Int64;
    function AsDate(const Switch: RawUTF8; Default: TDateTime; const Prompt: string): TDateTime;
    function AsJSON(Format: TTextWriterJSONFormat=jsonCompact): RawUTF8;
  end;

  
/// end-user command to create a new private/public key file
// - as used in the ECC.dpr command-line sample project
function ECCCommandNew(const AuthPrivKey: TFileName;
  const AuthPassword: RawUTF8; AuthPasswordRounds: integer;
  const Issuer: RawUTF8; StartDate: TDateTime; ExpirationDays: integer;
  const SavePassword: RawUTF8; const SavePassordRounds: integer): TFileName;

/// end-user command to sign a file using a private key file
// - as used in the ECC.dpr command-line sample project
function ECCCommandSignFile(const FileToSign, AuthPrivKey: TFileName;
  const AuthPassword: RawUTF8; AuthPasswordRounds: integer): TFileName;

/// end-user command to verify a file signature
// - as used in the ECC.dpr command-line sample project
function ECCCommandVerifyFile(const FileToVerify, AuthPubKey: TFileName;
  const AuthBase64: RawUTF8): TECCCertificateValidity;


implementation


{ TCommandLine }

constructor TCommandLine.Create;
var i: integer;
    p, sw: RawUTF8;
begin
  inherited Create;
  Values.InitFast(10,dvObject);
  for i := 1 to ParamCount do begin
    p := StringToUTF8(ParamStr(i));
    if p='' then
      continue;
    if p[1] in ['-','/'] then
      sw := LowerCase(copy(p,2,100)) else
      if sw<>'' then begin
        Values.AddValueFromText(sw,p,true);
        sw := '';
      end;
  end;
end;

function TCommandLine.AsUTF8(const Switch, Default: RawUTF8;
  const Prompt: string): RawUTF8;
var i: integer;
begin
  i := Values.GetValueIndex(Switch);
  if i>=0 then begin
    VariantToUTF8(Values.Values[i],result);
    Values.Delete(i);
    exit;
  end;
  result := '';
  if Prompt='' then
    exit;
  TextColor(ccLightGray);
  {$I-}
  writeln(Prompt);
  TextColor(ccCyan);
  write(Switch);
  if Default<>'' then
    write(' [',Default,'] ');
  write(': ');
  TextColor(ccWhite);
  readln(result);
  writeln;
  ioresult;
  {$I+}
  TextColor(ccLightGray);
  result := trim(result);
  if result='' then
    result := Default;
end;

function TCommandLine.AsInt(const Switch: RawUTF8; Default: Int64;
  const Prompt: string): Int64;
var res: RawUTF8;
begin
  res := AsUTF8(Switch, Int64ToUtf8(Default), Prompt);
  result := GetInt64Def(pointer(res),Default);
end;

function TCommandLine.AsDate(const Switch: RawUTF8; Default: TDateTime;
  const Prompt: string): TDateTime;
var res: RawUTF8;
begin
  res := AsUTF8(Switch, DateToIso8601Text(Default), Prompt);
  if res='0' then begin
    result := 0;
    exit;
  end;
  result := Iso8601ToDateTime(res);
  if result=0 then
    result := Default;
end;

function TCommandLine.AsJSON(Format: TTextWriterJSONFormat): RawUTF8;
begin
  result := Values.ToJSON('','',Format);
end;

function TCommandLine.AsString(const Switch: RawUTF8; const Default, Prompt: string): string;
begin
  result := UTF8ToString(AsUTF8(Switch,StringToUTF8(Default),Prompt));
end;


function ECCCommandNew(const AuthPrivKey: TFileName;
  const AuthPassword: RawUTF8; AuthPasswordRounds: integer;
  const Issuer: RawUTF8; StartDate: TDateTime; ExpirationDays: integer;
  const SavePassword: RawUTF8; const SavePassordRounds: integer): TFileName;
var auth,new: TECCCertificateSecret;
begin
  if AuthPrivKey='' then
    auth := nil else 
    auth := TECCCertificateSecret.CreateFromSecureFile(AuthPrivKey,AuthPassword,AuthPasswordRounds);
  try
    new := TECCCertificateSecret.CreateNew(auth,Issuer,ExpirationDays,StartDate);
    try
      new.SaveToSecureFile(SavePassword,'.',64,SavePassordRounds);
      result := new.SaveToSecureFileName;
      ObjectToJSONFile(new,ChangeFileExt(result,ECCCERTIFICATEPUBLIC_FILEEXT));
    finally
      new.Free;
    end;
  finally
    auth.Free;
  end;
end;

function ECCCommandSignFile(const FileToSign, AuthPrivKey: TFileName;
  const AuthPassword: RawUTF8; AuthPasswordRounds: integer): TFileName;
var auth: TECCCertificateSecret;
    content: RawByteString;
    sign: RawUTF8;
    json: TDocVariantData;
    sha: TSHA256Digest;
begin
  content := StringFromFile(FileToSign);
  if content='' then
    raise EECCException.CreateUTF8('File not found: %',[FileToSign]);
  auth := TECCCertificateSecret.CreateFromSecureFile(AuthPrivKey,AuthPassword,AuthPasswordRounds);
  try
    sha := SHA256Digest(pointer(content),length(content));
    sign := auth.SignToBase64(sha);
    json.InitObject([
      'meta',_ObjFast(['name',ExtractFileName(FileToSign),
        'date',DateToIso8601Text(FileAgeToDateTime(FileToSign))]),
      'md5',MD5(content),'sha256',SHA256DigestToString(sha),
      'sign',sign],JSON_OPTIONS_FAST);
    result := FileToSign+ECCCERTIFICATESIGN_FILEEXT;
    FileFromString(json.ToJSON('','',jsonHumanReadable),result);
  finally
    auth.Free;
  end;
end;

function ECCCommandVerifyFile(const FileToVerify, AuthPubKey: TFileName;
  const AuthBase64: RawUTF8): TECCCertificateValidity;
var auth: TECCCertificate;
    content: RawByteString;
    json: TDocVariantData;
    cert: TECCSignatureCertified;
    sign: RawUTF8;
    authfilename: TFileName;
begin
  content := StringFromFile(FileToVerify);
  if content='' then
    raise EECCException.CreateUTF8('File not found: %',[FileToVerify]);
  auth := TECCCertificate.Create;
  try
    json.InitJSONFromFile(FileToVerify+ECCCERTIFICATESIGN_FILEEXT);
    result := ecvInvalidSignature;
    if not json.GetAsRawUTF8('sign',sign) then
      exit;
    cert := TECCSignatureCertified.Create;
    try
      if not cert.FromBase64(sign) then
        exit;
      result := ecvUnknownAuthority;
      if not auth.FromBase64(AuthBase64) then 
        if not JSONFileToObject(AuthPubKey,auth) then begin
          authfilename := UTF8ToString(cert.AuthoritySerial);
          if ECCKeyFileFind(authfilename,false) then begin
            if not JSONFileToObject(authfilename,auth) then
              exit;
          end;
        end;
      result := cert.Verify(auth,pointer(content),length(content));
    finally
      cert.Free;
    end;
  finally
    auth.Free;
  end;
end;

end.
