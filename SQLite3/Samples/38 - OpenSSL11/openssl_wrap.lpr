program openssl_wrap;

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 USELIBCURL

uses
  {$I SynDprUses.inc}
  SysUtils,
  SynCrypto,
  SynCommons,
  SynOpenSSL,
  SynOpenSSLWrap,
  SynTests;

var
  O: TOpenSSLLib;
  sha256md: PEVP_MD;

/// creating cryptographic HMAC digests ('SHA256', 'SHA3-512', etc.)
// - digestName is an algorithm name from `openssl list -digest-algorithms`
function HMAC(const digestName, key, dat: RawByteString): RawUTF8;
var
  ctx: PHMAC_CTX;
  md: PEVP_MD;
  mdres: array[0..EVP_MAX_MD_SIZE] of byte;
  resLen: cardinal;
begin
  md := O.EVP_get_digestbyname(PChar(digestName));
  if md = nil then
    raise EOpenSSL.CreateFmt('Unknown message digest "%s"', [digestName]);

  ctx := O.HMAC_CTX_new();
  if ctx = nil then
    raise EOpenSSL.Create('HMAC_CTX_new');

  if O.HMAC_Init_ex(ctx, pointer(key), length(key), md, nil) <> EVP_OK then
    raise EOpenSSL.CreateFmt('HMAC_Init_ex: %d', [O.ERR_get_error()]);

  if O.HMAC_Update(ctx, pointer(dat),length(dat)) <> EVP_OK then
    raise EOpenSSL.CreateFmt('HMAC_Update: %d', [O.ERR_get_error()]);

  if O.HMAC_Final(ctx, @mdres, resLen) <> EVP_OK then
    raise EOpenSSL.CreateFmt('HMAC_Final: %d', [O.ERR_get_error()]);

  BinToHexLower(@mdres, resLen, Result);
  O.HMAC_CTX_free(ctx);
end;

function HashSha256(const key, dat: AnsiString): RawUTF8;
var
  ctx: PHMAC_CTX;
  mdres: array[0..EVP_MAX_MD_SIZE] of byte;
  resLen: cardinal;
begin
  ctx := O.HMAC_CTX_new();
  if ctx = nil then
    raise EOpenSSL.Create('HMAC_CTX_new');

  if O.HMAC_Init_ex(ctx, PByte(key), length(key), sha256md, nil) <> EVP_OK then
    raise EOpenSSL.CreateFmt('HMAC_Init_ex: %d', [O.ERR_get_error()]);

  if O.HMAC_Update(ctx, PByte(dat),length(dat)) <> EVP_OK then
    raise EOpenSSL.CreateFmt('HMAC_Update: %d', [O.ERR_get_error()]);

  if O.HMAC_Final(ctx, @mdres, resLen) <> EVP_OK then
    raise EOpenSSL.CreateFmt('HMAC_Final: %d', [O.ERR_get_error()]);

  BinToHexLower(@mdres, resLen, Result);
  O.HMAC_CTX_free(ctx);
end;


function Hash256Syn(const key, dat: AnsiString): RawUTF8;
var
  d: TSHA256Digest;
begin
  HMAC_SHA256(PByte(key), PByte(dat), length(key), length(dat), d);
  Result := SHA256DigestToString(d);
end;

var
  T: TPrecisionTimer;
  i, k: integer;
  data: RawByteString;
const
  CNT=10000;
  SIZ: array[0..4] of integer = (8, 50, 100, 1000, 10000);
  SECRET = 'secret string';

begin
  O := OpenSSL;
  sha256md := O.EVP_get_digestbyname('SHA256');
  WriteLn('HMAC SHA256');
  for k := 0 to 4 do begin
    data := TSynTestCase.RandomString(SIZ[k]);
    Writeln('Data length ', SIZ[k]);

    T.Start;
    for i := 1 to CNT do
      HMAC('SHA256', 'secret', data);
    Writeln(#9'OpenSSL md    ', T.PerSec(CNT), ' op/sec');

    T.Start;
    for i := 1 to CNT do
      HashSha256(SECRET, data);
    Writeln(#9'OpenSSL nomd  ', T.PerSec(CNT), ' op/sec');

    T.Start;
    for i := 1 to CNT do
      THmac.hmac('sha256', SECRET, data);
    Writeln(#9'OpenSSL THmac ', T.PerSec(CNT), ' op/sec');

    T.Start;
    for i := 1 to CNT do
      Hash256Syn(SECRET, data);
    Writeln(#9'Syn           ', T.PerSec(CNT), ' op/sec');
  end;

  WriteLn(#10'hash SHA256');
  for k := 0 to 4 do begin
    data := TSynTestCase.RandomString(SIZ[k]);
    Writeln('Data length ', SIZ[k]);
    T.Start;
    for i := 1 to CNT do
      THash.hash('sha256', data);
    Writeln(#9'OpenSSL THash ', T.PerSec(CNT), ' op/sec');

    T.Start;
    for i := 1 to CNT do
      SHA256(data);
    Writeln(#9'Syn SHA256    ', T.PerSec(CNT), ' op/sec');
  end;

  Writeln('hmac("SHA256", "secret", "data")= ', HMAC('SHA3-224', 'secret', data));
  Writeln('hmac("SHA_MD", "secret", "data")= ', HashSha256('secret', data));
  Writeln('hmac("SYNCRY", "secret", "data")= ', Hash256Syn('secret', data));
  Writeln('hmac sha512                     = ', THmac.hmac('sha512', 'secret', data));
  Writeln('SHAKE256 hash                   = ', THash.hash('SHAKE256', data));
  Writeln('SHAKE256 hash 128 byte          = ', THash.hash('SHAKE256', data, 64));
  Writeln('SynHash = OpenSSL hash: ', SHA256(data) = THash.hash('sha256', data));
end.

