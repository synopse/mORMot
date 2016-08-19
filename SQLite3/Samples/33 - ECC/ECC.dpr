program ECC;

{$APPTYPE CONSOLE}

(*

  Synopse mORMot framework

  Sample 33 - ECC command line tool
    Manage certificate-based public-key cryptography using ECC-secp256r1,
    i.e. public/private key pairs, certificates and digital signatures,
    as implemented in SynECC.


  Version 1.18
  - Initial Release


  first line of uses clause below must be {$I SynDprUses.inc} to enable FastMM4
  conditional define should contain INCLUDE_FTS3 to handle FTS3/FTS4 in SQLite3
*)

uses
  {$I SynDprUses.inc}
  SysUtils,
  SynCommons,
  mORMot,
  SynCrypto,
  SynEcc,
  ECCProcess;

{$R *.res}

type
  TSwitches = (
    swHelp, swNew, swSign, swVerify, swSource, swInfo, swChain, swChainAll);

procedure ProcessCommandLine;
var cmd, issuer, authpass, savepass, constname, comment: RawUTF8;
    start: TDateTime;
    authrounds, days, saverounds: integer;
    sw: ICommandLine;
    signfile,auth,newfile: TFileName;
    verif: TECCValidity;
    certs: TStringDynArray;
    i: integer;
begin
  try
  try
    TextColor(ccLightGreen);
    writeln(#13#10'Synopse ECC certificate-based public-key cryptography'+
            #13#10'-----------------------------------------------------');
    TextColor(ccGreen);
    writeln('Using mORMot''s SynECC rev. ' + {$I SynopseCommit.inc} + #13#10);
    TextColor(ccLightGray);
    if not ecc_available then
      raise EECCException.Create('ECC is not implemented on this platform');
    sw := TCommandLine.Create;
    cmd := StringToUTF8(ParamStr(1));
    case GetEnumNameValueTrimmed(TypeInfo(TSwitches),pointer(cmd),length(cmd)) of
    ord(swNew): begin
      repeat
        auth := sw.AsString('Auth','',
          'Enter the first chars of the .privkey file name of the signing authority.'#13#10+
          'Will create a self-signed certificate if left void.');
      until (auth='') or ECCKeyFileFind(auth,true);
      if auth<>'' then begin
        writeln('Will use: ',ExtractFileName(auth),#13#10);
        authpass := sw.AsUTF8('Pass','',
          'Enter the PassPhrase of this .privkey file.');
        authrounds := sw.AsInt('Rounds',60000,
          'Enter the PassPhrase iteration rounds of this .privkey file.');
      end else
        authrounds := 0;
      issuer := sw.AsUTF8('Issuer',ExeVersion.User,
        'Enter Issuer identifier text.'#13#10'Will be truncated to 15-20 ascii-7 chars.');
      start := sw.AsDate('Start',NowUTC,
        'Enter the YYYY-MM-DD start date of its validity.'#13#10+
        '0 will create a never-expiring certificate');
      if start<=0 then
        days := 0 else
        days := sw.AsInt('Days',365,'Enter the number of days of its validity');
      repeat
        savepass := sw.AsUTF8('NewPass',TAESPRNG.Main.RandomPassword(12),
          'Enter a private PassPhrase for the new key (at least 8 chars long).'#13#10+
          'Save this in a safe place: if you forget it, the key will be useless!');
      until length(savepass)>=8;
      TextColor(ccLightGray);
      writeln('Corresponding TSynPersistentWithPassword.ComputePassword:');
      TextColor(ccWhite);
      writeln(' ',TSynPersistentWithPassword.ComputePassword(savepass),#13#10);
      repeat
        saverounds := sw.AsInt('NewRounds',60000,
          'Enter the PassPhrase iteration round for the new key (at least 1000).'#13#10+
          'The higher, the safer, but will demand more computation time.');
      until saverounds>=1000;
      newfile := EccCommandNew(auth,authpass,authrounds,issuer,start,days,savepass,saverounds);
      if newfile<>'' then
        newfile := newfile+'/.pubkey';
    end;
    ord(swSign): begin
      repeat
        signfile := sw.AsString('File','','Enter the name of the file to be signed');
      until FileExists(signfile);
      repeat
        auth := sw.AsString('Auth','',
          'Enter the first chars of the .privkey file name of the signing authority.');
      until ECCKeyFileFind(auth,true);
      writeln('Will use: ',ExtractFileName(auth),#13#10);
      authpass := sw.AsUTF8('Pass','',
        'Enter the PassPhrase of this .privkey file.');
      authrounds := sw.AsInt('Rounds',60000,
        'Enter the PassPhrase iteration rounds of this .privkey file.');
      newfile := EccCommandSignFile(signfile,auth,authpass,authrounds);
    end;
    ord(swVerify): begin
      repeat
        signfile := sw.AsString('File','','Enter the name of the file to be verified.');
      until FileExists(signfile) and FileExists(signfile+ECCCERTIFICATESIGN_FILEEXT);
      verif := ECCCommandVerifyFile(signfile,sw.AsString('auth','',''),'');
      if verif in ECC_VALIDSIGN then begin
        TextColor(ccLightGreen);
        write(signfile,' file verified as ');
      end else begin
        TextColor(ccLightRed);
        write(signfile,' file verification failure: ');
        ExitCode := 1;
      end;
      writeln(sysutils.lowercase(GetEnumCaption(TypeInfo(TECCValidity),verif)),'.');
    end;
    ord(swSource): begin
      repeat
        auth := sw.AsString('Auth','',
          'Enter the first chars of the .privkey certificate file name.');
      until ECCKeyFileFind(auth,true);
      writeln('Will use: ',ExtractFileName(auth),#13#10);
      authpass := sw.AsUTF8('Pass','',
        'Enter the PassPhrase of this .privkey file.');
      authrounds := sw.AsInt('Rounds',60000,
        'Enter the PassPhrase iteration rounds of this .privkey file.');
      constname := sw.AsUTF8('Const','',
        'Enter the variable name to define the const in source.');
      comment := sw.AsUTF8('Comment','',
        'Enter some optional comment to identify this private key');
      newfile := EccCommandSourceFile(auth,authpass,authrounds,constname,comment,
        TAESPRNG.Main.RandomPassword(24));
    end;
    ord(swInfo): begin
      repeat
        auth := sw.AsString('Auth','',
          'Enter the first chars of the .privkey certificate file name.');
      until ECCKeyFileFind(auth,true);
      writeln('Will use: ',ExtractFileName(auth),#13#10);
      authpass := sw.AsUTF8('Pass','',
        'Enter the PassPhrase of this .privkey file.');
      authrounds := sw.AsInt('Rounds',60000,
        'Enter the PassPhrase iteration rounds of this .privkey file.');
      writeln(ECCCommandInfoFile(auth,authpass,authrounds));
    end;
    ord(swChain): begin
      SetLength(certs,paramcount-1);
      for i := 2 to paramcount do
        certs[i-2] := paramstr(i);
      newfile := ECCCommandChainCertificates(certs);
    end;
    ord(swChainAll): begin
      newfile := ECCCommandChainCertificates(['*']);
    end;
    else begin
      writeln(ExeVersion.ProgramName,
        ' help');
      writeln(ExeVersion.ProgramName,
        ' new -auth file.privkey -pass P@ssW0rd -rounds 60000'#13#10+
        '        -issuer toto@toto.com -start 2016-10-30 -days 30'#13#10+
        '        -newpass newP@ssw0RD@ -newrounds 60000');
      writeln(ExeVersion.ProgramName,
        ' sign -file some.doc -auth file.privkey -pass P@ssW0rd -rounds 60000');
      writeln(ExeVersion.ProgramName,
        ' verify -file some.doc -auth file.pubkey');
      writeln(ExeVersion.ProgramName,
        ' source -auth file.privkey -pass P@ssW0rd -rounds 60000'#13#10+
        '           -const MY_PRIVKEY -comment "My Private Key"');
      writeln(ExeVersion.ProgramName,
        ' info -auth file.privkey -pass P@ssW0rd -rounds 60000');
      writeln(ExeVersion.ProgramName,
        ' chain file1.pubkey file2.pubkey file3.pubkey ...');
      writeln(ExeVersion.ProgramName,
        ' chainall');
      ExitCode:= 2;
    end;
    end;
  except
    on E: Exception do begin
      ConsoleShowFatalException(E,false);
      ExitCode := 3;
    end;
  end;
  finally
    FillcharFast(pointer(authpass),length(authpass),0);
    FillcharFast(pointer(savepass),length(savepass),0);
  end;
  TextColor(ccWhite);
  if newfile<>'' then
    writeln(' ',newfile,' file created.');
  writeln;
  TextColor(ccLightGray);
  {$ifndef FPC}
  {$WARNINGS OFF}
  {$ifdef MSWINDOWS}
  if DebugHook<>0 then
    readln;
  {$endif}
  {$WARNINGS ON}
  {$endif}
end;

begin
   //if ParamCount > 0 then
   ProcessCommandLine;
end.
