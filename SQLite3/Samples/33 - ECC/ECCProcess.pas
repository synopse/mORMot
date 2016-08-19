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
  const AuthBase64: RawUTF8): TECCValidity;

/// end-user command to create a .inc pascal source file from a private key file
// - ready to be included within the executable binary as private secret
// - as used in the ECC.dpr command-line sample project
function ECCCommandSourceFile(const AuthPrivKey: TFileName;
  const AuthPassword: RawUTF8; AuthPasswordRounds: integer;
  const ConstName, Comment, PassWord: RawUTF8): TFileName;

/// end-user command to create a .json base-64 text array from a set of public key files
// - ready to be included e.g. as settings of any server
// - ECCCommandChainCertificates(['*']) will create a 'chain.certif' of all
// public key files in the current folder
// - as used in the ECC.dpr command-line sample project
function ECCCommandChainCertificates(const CertFiles: array of string): TFileName;

/// end-user command to display the json information from a private key file
// - as used in the ECC.dpr command-line sample project
function ECCCommandInfoFile(const AuthPrivKey: TFileName;
  const AuthPassword: RawUTF8; AuthPasswordRounds: integer): RawUTF8;


implementation

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
begin
  auth := TECCCertificateSecret.CreateFromSecureFile(AuthPrivKey,AuthPassword,AuthPasswordRounds);
  try
    result := auth.SignFile(FileToSign,[]);
  finally
    auth.Free;
  end;
end;

function ECCCommandSourceFile(const AuthPrivKey: TFileName;
  const AuthPassword: RawUTF8; AuthPasswordRounds: integer;
  const ConstName, Comment, PassWord: RawUTF8): TFileName;
var auth: TECCCertificateSecret;
begin
  auth := TECCCertificateSecret.CreateFromSecureFile(AuthPrivKey,AuthPassword,AuthPasswordRounds);
  try
    result := AuthPrivKey+'.inc';
    FileFromString(auth.SaveToSource(ConstName,Comment,Password),result);
  finally
    auth.Free;
  end;
end;

function ECCCommandVerifyFile(const FileToVerify, AuthPubKey: TFileName;
  const AuthBase64: RawUTF8): TECCValidity;
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

function ECCCommandChainCertificates(const CertFiles: array of string): TFileName;
var n,i: integer;
    files: TFileNameDynArray;
begin
  result := '';
  n := length(CertFiles);
  if n=0 then
    exit;
  if (n=1) and (CertFiles[0]='*') then begin
    files := FindFilesDynArrayToFileNames(
      FindFiles('.','*'+ECCCERTIFICATEPUBLIC_FILEEXT));
    result := 'chain.certif';
  end else begin
    SetLength(files,n);
    for i := 0 to n-1 do begin
      files[i] := CertFiles[i];
      if not ECCKeyFileFind(files[i],false) then
        exit;
     end;
    result := format('chain%d.certif',[GetTickCount64]);
  end;
  with TECCCertificateChainFile.CreateFromFiles(files) do
  try
    if ValidateItems<>nil then begin
      result := '';
      raise EECCException.Create('Some of the certificates are invalid');
    end;
    SaveToFile(result);
  finally
    Free;
  end;
end;

function ECCCommandInfoFile(const AuthPrivKey: TFileName;
  const AuthPassword: RawUTF8; AuthPasswordRounds: integer): RawUTF8;
var auth: TECCCertificateSecret;
begin
  auth := TECCCertificateSecret.CreateFromSecureFile(AuthPrivKey,AuthPassword,AuthPasswordRounds);
  try
    result := ObjectToJSON(auth,[woHumanReadable]);
  finally
    auth.Free;
  end;
end;

end.
