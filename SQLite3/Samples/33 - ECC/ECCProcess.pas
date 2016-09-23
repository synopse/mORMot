unit ECCProcess;

interface

{$I Synopse.inc}

uses
  SysUtils,
  Classes,
  SynCommons,
  SynEcc,
  SynCrypto,
  mORMot;

/// end-user command to create a new private/public key file
// - as used in the ECC.dpr command-line sample project
function ECCCommandNew(const AuthPrivKey: TFileName;
  const AuthPassword: RawUTF8; AuthPasswordRounds: integer;
  const Issuer: RawUTF8; StartDate: TDateTime; ExpirationDays: integer;
  const SavePassword: RawUTF8; SavePassordRounds, SplitFiles: integer): TFileName;

/// end-user command to create a renew a .privkey file password
// - as used in the ECC.dpr command-line sample project
function ECCCommandRekey(const AuthPrivKey: TFileName;
  const AuthPassword: RawUTF8; AuthPasswordRounds: integer;
  const SavePassword: RawUTF8; SavePassordRounds: integer): TFileName;

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
function ECCCommandChainCertificates(const CertFiles: array of RawUTF8): TFileName;

/// end-user command to display the json information from a .privkey file
// - as used in the ECC.dpr command-line sample project
function ECCCommandInfoPrivFile(const AuthPrivKey: TFileName;
  const AuthPassword: RawUTF8; AuthPasswordRounds: integer): RawUTF8;

/// end-user command to encrypt a file with the .synecc format 
// - as used in the ECC.dpr command-line sample project
procedure ECCCommandCryptFile(const FileToCrypt, DestFile, AuthPubKey: TFileName;
  const AuthBase64, AuthSerial, Password: RawUTF8; PasswordRounds: integer;
  Algo: TECIESAlgo=ecaUnknown);

/// end-user command to decrypt a .synecc file
// - as used in the ECC.dpr command-line sample project
// - if AuthPrivKey is not set, it will search for the stored TECCCertificate.Serial
function ECCCommandDecryptFile(const FileToDecrypt, DestFile, AuthPrivKey: TFileName;
  const AuthPassword: RawUTF8; AuthPasswordRounds: integer;
  const DecryptPassword: RawUTF8; DecryptPasswordRounds: integer;
  Signature: PECCSignatureCertifiedContent): TECCDecrypt;

/// end-user command to verify a .synecc file signature, after decryption
// - as used in the ECC.dpr command-line sample project
// - the supplied signature can be retrieved from ECCCommandDecryptFile()
function ECCCommandVerifyDecryptedFile(const FileToVerify: TFileName;
  const Signature: TECCSignatureCertifiedContent): TECCValidity;

type
  /// the actions implemented by ECCCommand()
  // - as used in the ECC.dpr command-line sample project
  // - retrieved from the command line as first parameter
  TECCCommand = (
    ecHelp, ecNew, ecRekey, ecSign, ecVerify, ecSource, ecInfoPriv,
    ecChain, ecChainAll,
    ecCrypt, ecDecrypt, ecInfoCrypt);
  /// the result code returned by ECCCommand()
  // - as used in the ECC.dpr command-line sample project
  TECCCommandError = (
    eccSuccess, eccUnknownCommand, eccValidationError, eccError, eccException);


/// execute the encryption process corresponding to the command line options
// - as used in the ECC.dpr sample project
// - returns the ExitCode expected value (0=eccSuccess)
function ECCCommand(cmd: TECCCommand; const sw: ICommandLine): TECCCommandError;


implementation

function ECCCommandNew(const AuthPrivKey: TFileName;
  const AuthPassword: RawUTF8; AuthPasswordRounds: integer;
  const Issuer: RawUTF8; StartDate: TDateTime; ExpirationDays: integer;
  const SavePassword: RawUTF8; SavePassordRounds, SplitFiles: integer): TFileName;
var auth,new: TECCCertificateSecret;
begin
  if AuthPrivKey='' then
    auth := nil else
    auth := TECCCertificateSecret.CreateFromSecureFile(AuthPrivKey,AuthPassword,AuthPasswordRounds);
  try
    // generate pair
    new := TECCCertificateSecret.CreateNew(auth,Issuer,ExpirationDays,StartDate);
    try
      // save private key as .privkey password-protected binary file
      new.SaveToSecureFiles(SavePassword,'.',SplitFiles,64,SavePassordRounds);
      // save public key as .pubkey JSON file
      result := ChangeFileExt(new.SaveToSecureFileName,ECCCERTIFICATEPUBLIC_FILEEXT);
      ObjectToJSONFile(new,result);
    finally
      new.Free;
    end;
  finally
    auth.Free;
  end;
end;

function ECCCommandRekey(const AuthPrivKey: TFileName;
  const AuthPassword: RawUTF8; AuthPasswordRounds: integer;
  const SavePassword: RawUTF8; SavePassordRounds: integer): TFileName;
var auth: TECCCertificateSecret;
begin
  auth := TECCCertificateSecret.CreateFromSecureFile(AuthPrivKey,AuthPassword,AuthPasswordRounds);
  try
    auth.SaveToSecureFile(SavePassword,'.',64,SavePassordRounds);
    result := auth.SaveToSecureFileName;
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
var content: RawByteString;
    auth: TECCCertificate;
    cert: TECCSignatureCertified;
begin
  content := StringFromFile(FileToVerify);
  if content='' then
    raise EECCException.CreateUTF8('File not found: %',[FileToVerify]);
  cert := TECCSignatureCertified.CreateFromFile(FileToVerify);
  try
    if not cert.Check then begin
      result := ecvInvalidSignature;
      exit;
    end;
    auth := TECCCertificate.Create;
    try
      if auth.FromAuth(AuthPubKey,AuthBase64,cert.AuthoritySerial) then
        result := cert.Verify(auth,pointer(content),length(content)) else
        result := ecvUnknownAuthority;
    finally
      auth.Free;
    end;
  finally
    cert.Free;
  end;
end;

function ECCCommandVerifyDecryptedFile(const FileToVerify: TFileName;
  const Signature: TECCSignatureCertifiedContent): TECCValidity;
var content: RawByteString;
    auth: TECCCertificate;
    cert: TECCSignatureCertified;
begin
  content := StringFromFile(FileToVerify);
  if content='' then
    raise EECCException.CreateUTF8('File not found: %',[FileToVerify]);
  cert := TECCSignatureCertified.CreateFrom(Signature);
  try
    auth := TECCCertificate.Create;
    try
      result := ecvUnknownAuthority;
      if auth.FromAuth('','',cert.AuthoritySerial) then
        result := cert.Verify(auth,pointer(content),length(content));
    finally
      auth.Free;
    end;
  finally
    cert.Free;
  end;
end;

procedure ECCCommandCryptFile(const FileToCrypt, DestFile, AuthPubKey: TFileName;
  const AuthBase64, AuthSerial, Password: RawUTF8; PasswordRounds: integer;
  Algo: TECIESAlgo);
var content: RawByteString;
    auth: TECCCertificate;
begin
  content := StringFromFile(FileToCrypt);
  if content='' then
    raise EECCException.CreateUTF8('File not found: %',[FileToCrypt]);
  auth := TECCCertificate.Create;
  try
    if auth.FromAuth(AuthPubKey,AuthBase64,AuthSerial) then begin
      auth.EncryptFile(FileToCrypt,DestFile,Password,PasswordRounds,Algo,true);
    end;
  finally
    auth.Free;
    FillZero(content);
  end;
end;

function ECCCommandDecryptFile(const FileToDecrypt, DestFile, AuthPrivKey: TFileName;
  const AuthPassword: RawUTF8; AuthPasswordRounds: integer;
  const DecryptPassword: RawUTF8; DecryptPasswordRounds: integer;
  Signature: PECCSignatureCertifiedContent): TECCDecrypt;
var auth: TECCCertificateSecret;
    head: TECIESHeader;
    priv: TFileName;
begin
  auth := TECCCertificateSecret.Create;
  try
    result := ecdNoPrivateKey;
    if FileExists(AuthPrivKey) then
      priv := AuthPrivKey else begin
      if not ECIESHeaderFile(FileToDecrypt,head) then
        exit;
      priv := UTF8ToString(ECCText(head.recid));
      if not ECCKeyFileFind(priv,true) then
        exit;
    end;
    if not auth.LoadFromSecureFile(priv,AuthPassword,AuthPasswordRounds) then
      exit;
    result := auth.DecryptFile(FileToDecrypt,DestFile,
      DecryptPassword,DecryptPasswordRounds,Signature);
  finally
    auth.Free;
  end;
end;

function ECCCommandChainCertificates(const CertFiles: array of RawUTF8): TFileName;
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
      files[i] := UTF8ToString(CertFiles[i]);
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

function ECCCommandInfoPrivFile(const AuthPrivKey: TFileName;
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


function ECCCommand(cmd: TECCCommand; const sw: ICommandLine): TECCCommandError;

  procedure WriteVerif(verif: TECCValidity; const filename: TFileName; const sw: ICommandLine);
  var res: string;
  begin
    res := SysUtils.LowerCase(GetEnumCaption(TypeInfo(TECCValidity),verif));
    if verif in ECC_VALIDSIGN then
      sw.Text(' % file verified as %.',[filename,res],ccLightGreen) else begin
      sw.Text(' % file verification failure: % (%).',[filename,res,ord(verif)],ccLightRed);
      result := eccValidationError;
    end;
  end;

var issuer, authpass, savepass, constname, comment: RawUTF8;
    start: TDateTime;
    authrounds, days, saverounds, splitfiles: integer;
    msg: string;
    origfile,auth,newfile: TFileName;
    decrypt: TECCDecrypt;
    decryptsign: TECCSignatureCertifiedContent;
begin
  result := eccSuccess;
  if sw=nil then
    raise EECCException.Create('ECCCommand(nil)');
  try
  try
    if not ecc_available then
      raise EECCException.Create('ECC is not implemented on this platform');
    case cmd of
    ecNew: begin
      repeat
        auth := sw.AsString('Auth','',
          'Enter the first chars of the .privkey file name of the signing authority.'#13#10+
          'Will create a self-signed certificate if left void.');
      until (auth='') or ECCKeyFileFind(auth,true) or sw.NoPrompt;
      if auth<>'' then begin
        sw.Text('Will use: %'#13#10,[ExtractFileName(auth)]);
        authpass := sw.AsUTF8('AuthPass','',
          'Enter the PassPhrase of this .privkey file.');
        authrounds := sw.AsInt('AuthRounds',60000,
          'Enter the PassPhrase iteration rounds of this .privkey file.');
      end else
        authrounds := 0;
      issuer := sw.AsUTF8('Issuer',ExeVersion.User,
        'Enter Issuer identifier text.'#13#10'Will be truncated to 15-20 ascii-7 chars.');
      start := sw.AsDate('Start',NowUTC,
        'Enter the YYYY-MM-DD start date of its validity.'#13#10+
        '0 will create a never-expiring certificate.');
      if start<=0 then
        days := 0 else
        days := sw.AsInt('Days',365,'Enter the number of days of its validity.');
      repeat
        savepass := sw.AsUTF8('NewPass',TAESPRNG.Main.RandomPassword(12),
          'Enter a private PassPhrase for the new key (at least 8 chars long).'#13#10+
          'Save this in a safe place: if you forget it, the key will be useless!');
      until (length(savepass)>=8) or sw.NoPrompt;
      sw.Text('Corresponding TSynPersistentWithPassword.ComputePassword:',[]);
      sw.Text(' %'#13#10,[TSynPersistentWithPassword.ComputePassword(savepass)],ccWhite);
      repeat
        saverounds := sw.AsInt('NewRounds',60000,
          'Enter the PassPhrase iteration round for the new key (at least 1000).'#13#10+
          'The higher, the safer, but will demand more computation time.');
      until (saverounds>=1000) or sw.NoPrompt;
      splitfiles := 1;
      {repeat
        splitfiles := sw.AsInt('SplitFiles',1,
          'Into how many files the private key should be parceled out.');
      until (splitfiles>0) or sw.NoPrompt;}
      newfile := EccCommandNew(
        auth,authpass,authrounds,issuer,start,days,savepass,saverounds,splitfiles);
      if newfile<>'' then
        newfile := newfile+'/.privkey';
    end;
    ecRekey: begin
      repeat
        auth := sw.AsString('Auth','',
          'Enter the first chars of the .privkey certificate file name.');
      until ECCKeyFileFind(auth,true) or sw.NoPrompt;
      sw.Text('Will use: %'#13#10,[ExtractFileName(auth)]);
      authpass := sw.AsUTF8('AuthPass','',
        'Enter the PassPhrase of this .privkey file.');
      authrounds := sw.AsInt('AuthRounds',60000,
        'Enter the PassPhrase iteration rounds of this .privkey file.');
      repeat
        savepass := sw.AsUTF8('NewPass',TAESPRNG.Main.RandomPassword(12),
          'Enter a NEW private PassPhrase for the key (at least 8 chars long).'#13#10+
          'Save this in a safe place: if you forget it, the key will be useless!');
      until (length(savepass)>=8) or sw.NoPrompt;
      sw.Text('Corresponding TSynPersistentWithPassword.ComputePassword:',[]);
      sw.Text(' %'#13#10,[TSynPersistentWithPassword.ComputePassword(savepass)],ccWhite);
      repeat
        saverounds := sw.AsInt('NewRounds',60000,
          'Enter the NEW PassPhrase iteration round for the key (at least 1000).'#13#10+
          'The higher, the safer, but will demand more computation time.');
      until (saverounds>=1000) or sw.NoPrompt;
      newfile := EccCommandRekey(auth,authpass,authrounds,savepass,saverounds);
    end;
    ecSign: begin
      repeat
        origfile := sw.AsString('File','','Enter the name of the file to be signed.');
      until FileExists(origfile) or sw.NoPrompt;
      repeat
        auth := sw.AsString('Auth','',
          'Enter the first chars of the .privkey file name of the signing authority.');
      until ECCKeyFileFind(auth,true) or sw.NoPrompt;
      sw.Text('Will use: %'#13#10,[ExtractFileName(auth)]);
      authpass := sw.AsUTF8('Pass','',
        'Enter the PassPhrase of this .privkey file.');
      authrounds := sw.AsInt('Rounds',60000,
        'Enter the PassPhrase iteration rounds of this .privkey file.');
      newfile := EccCommandSignFile(origfile,auth,authpass,authrounds);
    end;
    ecVerify: begin
      repeat
        origfile := sw.AsString('File','','Enter the name of the file to be verified.');
      until sw.NoPrompt or
        (FileExists(origfile) and FileExists(origfile+ECCCERTIFICATESIGN_FILEEXT));
      WriteVerif(ECCCommandVerifyFile(origfile,sw.AsString('auth','',''),''),origfile,sw);
    end;
    ecSource: begin
      repeat
        auth := sw.AsString('Auth','',
          'Enter the first chars of the .privkey certificate file name.');
      until ECCKeyFileFind(auth,true) or sw.NoPrompt;
      sw.Text('Will use: %'#13#10,[ExtractFileName(auth)]);
      authpass := sw.AsUTF8('Pass','',
        'Enter the PassPhrase of this .privkey file.');
      authrounds := sw.AsInt('Rounds',60000,
        'Enter the PassPhrase iteration rounds of this .privkey file.');
      constname := sw.AsUTF8('Const','',
        'Enter the variable name to define the const in source.');
      comment := sw.AsUTF8('Comment','',
        'Enter some optional comment to identify this private key.');
      newfile := EccCommandSourceFile(auth,authpass,authrounds,constname,comment,
        TAESPRNG.Main.RandomPassword(24));
    end;
    ecInfoPriv: begin
      repeat
        auth := sw.AsString('Auth','',
          'Enter the first chars of the .privkey certificate file name.');
      until ECCKeyFileFind(auth,true) or sw.NoPrompt;
      if not sw.NoPrompt then
        sw.Text('Will use: %'#13#10,[ExtractFileName(auth)]);
      authpass := sw.AsUTF8('Pass','',
        'Enter the PassPhrase of this .privkey file.');
      authrounds := sw.AsInt('Rounds',60000,
        'Enter the PassPhrase iteration rounds of this .privkey file.');
      sw.Text('%',[ECCCommandInfoPrivFile(auth,authpass,authrounds)]);
    end;
    ecChain:
      newfile := ECCCommandChainCertificates(sw.AsArray);
    ecChainAll:
      newfile := ECCCommandChainCertificates(['*']);
    ecCrypt: begin
      repeat
        origfile := sw.AsString('File','','Enter the name of the file to be encrypted.');
      until FileExists(origfile) or sw.NoPrompt;
      repeat
        newfile := SysUtils.Trim(sw.AsString('Out',origfile+ENCRYPTED_FILEEXT,
          'Enter the name of the encrypted file'));
      until (newfile <> '') or sw.NoPrompt;
      repeat
        auth := sw.AsString('Auth','',
          'Enter the first chars of the .pubkey file name of the encryption authority.');
      until ECCKeyFileFind(auth,false) or sw.NoPrompt;
      sw.Text('Will use: %'#13#10,[ExtractFileName(auth)]);
      authpass := sw.AsUTF8('SaltPass','salt','Enter the optional PassPhrase to be used for encryption.');
      authrounds := sw.AsInt('SaltRounds',60000, 'Enter the PassPhrase iteration rounds.');
      ECCCommandCryptFile(origfile,newfile,auth,'','',authpass,authrounds);
    end;
    ecInfoCrypt: begin
      repeat
        origfile := sw.AsString('File','','Enter the name of the encrypted file.');
      until FileExists(origfile) or sw.NoPrompt;
      sw.Text('%',[JSONReformat(ECIESHeaderText(origfile))]);
    end;
    ecDecrypt: begin
      repeat
        origfile := sw.AsString('File','','Enter the name of the file to be decrypted.');
      until FileExists(origfile) or sw.NoPrompt;
      repeat
        newfile := SysUtils.Trim(sw.AsString('Out',GetFileNameWithoutExt(origfile)+'.2',
          'Enter the name of the decrypted file'));
      until (newfile <> '') or sw.NoPrompt;
      authpass := sw.AsUTF8('AuthPass','',
        'Enter the PassPhrase of the associated .privkey file.');
      authrounds := sw.AsInt('AuthRounds',60000,
        'Enter the PassPhrase iteration rounds of this .privkey file.');
      savepass := sw.AsUTF8('SaltPass','salt','Enter the optional PassPhrase to be used for decryption.');
      saverounds := sw.AsInt('SaltRounds',60000, 'Enter the PassPhrase iteration rounds.');
      decrypt := ECCCommandDecryptFile(origfile,newfile,
        sw.AsString('Auth','',''),authpass,authrounds,savepass,saverounds,@decryptsign);
      msg := SysUtils.LowerCase(GetEnumCaption(TypeInfo(TECCDecrypt),decrypt));
      if decrypt in ECC_VALIDDECRYPT then begin
        if decrypt=ecdDecryptedWithSignature then
          WriteVerif(ECCCommandVerifyDecryptedFile(newfile,decryptsign),newfile,sw);
        sw.Text(' % file %.',[origfile,msg],ccLightGreen);
      end else begin
        sw.Text(' % file decryption failure: % (%).',[origfile,msg,ord(decrypt)],ccLightRed);
        result := eccError;
      end;
    end;
    else
      result := eccUnknownCommand;
    end;
  except
    on E: Exception do begin
      if not sw.NoPrompt then
        ConsoleShowFatalException(E,false);
      newfile := '';
      result := eccException;
    end;
  end;
  finally
    if not sw.NoPrompt then begin
      FillcharFast(pointer(authpass)^,length(authpass),0);
      FillcharFast(pointer(savepass)^,length(savepass),0);
    end;
  end;
  if (newfile<>'') and (result=eccSuccess) then
    sw.Text(' % file created.',[newfile],ccWhite);
  sw.TextColor(ccLightGray);
end;

end.
