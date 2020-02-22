/// Hi level OpenSSL 1.1 library wrapper.
// - OPENSSL1.1 should be defined in Synopse.inc to use this unit
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynOpenSSLWrap;

(*
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2020 Arnaud Bouchez
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

  The Original Code is Synopse framework.

  The Initial Developer of the Original Code is Pavel Mashlyakovsky.

  Portions created by the Initial Developer are Copyright (C) 2020
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
  - old version history has been cut down to maintain this huge unit under
    65,000 lines, as required by Delphi 5 to avoid internal error PRO-3006
*)

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER OPENSSL1.1

interface

uses
  SynCommons,
  SynOpenSSL;

type
  /// convenient multi-algorithm hashing wrapper
  THash = class
  private
    ctx: PEVP_MD_CTX;
    md_len: cardinal;
    md_value_: pointer;
  public
    /// initialize the internal hashing structure for a specific algorithm
    // - algorithm is one of `openssl list -digest-algorithms`
    // - for XOF hash functions such as 'shake256', the hashSize option can be used to specify the desired output length in bytes
    // - throws on unknown/unsupported algorithm
    procedure init(const algorithm: string; const hashSize: size_t = 0);
    /// call this method for each continuous message block
    // - iterate over all message blocks, then call digest to retrieve the HMAC
    procedure update(const data: pointer; const dataLength: integer); overload;
    procedure update(const data: RawByteString); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// computes the hash of all supplied messages and store it internally in mdValue buffer
    // - buffer allocated and free's internally
    // - returns actual mdValue buffer length
    function digest: size_t;
    /// computes the HMAC of all supplied message according to the key as lowercase hexa chars
    function digestHex: RawUTF8;
    /// compute the HMAC message authentication code using `algorithm` as hash function
    class function hash(const algorithm: string; const data: RawByteString;
      const hashSize: size_t = 0): RawUTF8;
    constructor Create;
    // release digest context
    destructor Destroy; override;
    property mdValue: pointer read md_value_;
  end;

  /// compute the HMAC message authentication code using `algorithm` as hash function
  // - you may use `hmac` class function for one-step process
  THmac = class
  private
    ctx: PHMAC_CTX;
  public
    /// init cryptographic HMAC digests
    // - algorithm is any of `openssl list -digest-algorithms`
    procedure init(const algorithm: string; const key: pointer; const keyLength: cardinal); overload;
    procedure init(const algorithm: string; const key: RawByteString); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// call this method for each continuous message block
    // - iterate over all message blocks, then call digest to retrieve the HMAC
    procedure update(const data: pointer; const dataLength: integer); overload;
    procedure update(const data: RawByteString); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// computes the HMAC of all supplied messages according to the key
    // - buffer must be last EVP_MAX_MD_SIZE bytes length
    // - returns actual digest length
    function digest(buffer: pointer): integer;
    /// computes the HMAC of all supplied message according to the key as lowercase hexa chars
    function digestHex: RawUTF8;
    /// compute the HMAC message authentication code using `algorithm` as hash function
    class function hmac(const algorithm: string; const key, data: RawByteString): RawUTF8;
    constructor Create;
    // release digest context
    destructor Destroy; override;
  end;

implementation

var
  O: TOpenSSLLib;

{ THash }
procedure THash.init(const algorithm: string; const hashSize: size_t);
var
  md: PEVP_MD;
  r: integer;
begin
  md := O.EVP_get_digestbyname(PChar(algorithm));
  if md = nil then
    raise EOpenSSL.CreateFmt('Unknown hash algorithm "%s"', [algorithm]);

  ctx := O.EVP_MD_CTX_new();
  r := O.EVP_DigestInit_ex(ctx, md, nil);
  if r <> EVP_OK then
    raise EOpenSSL.Create(O.ERR_error_string(r));
  md_len := O.EVP_MD_size(md);
  if (hashSize <> 0)  and (md_len <> hashSize) then begin
    if (O.EVP_MD_flags(md) and EVP_MD_FLAG_XOF) = 0 then
      raise EOpenSSL.CreateFmt('incorrect hashSize option was passed for a non-XOF hash function "%s"', [algorithm]);
    md_len := hashSize;
  end;
end;

procedure THash.update(const data: pointer; const dataLength: integer);
var r: integer;
begin
  r := O.EVP_DigestUpdate(ctx, data, dataLength);
  if r <> EVP_OK then
    raise EOpenSSL.Create(O.ERR_error_string(r));
end;

procedure THash.update(const data: RawByteString);
begin
  update(pointer(data), length(data))
end;

function THash.digest: size_t;
var
  default_len: size_t;
  r: integer;
begin
  if (md_value_ <> nil) then // already called
    exit(md_len);

  md_value_ := O.OPENSSL_malloc(md_len);

  default_len := O.EVP_MD_CTX_size(ctx);
  if md_len = default_len then
    r := O.EVP_DigestFinal_ex(ctx, md_value_, @md_len)
  else
    r := O.EVP_DigestFinalXOF(ctx, md_value_, md_len);

  if r <> EVP_OK then begin
    O.OPENSSL_free(md_value_);
    md_value_ := nil;
    raise EOpenSSL.Create(O.ERR_error_string(r));
  end;
  Result := md_len;
end;

function THash.digestHex: RawUTF8;
begin
  if md_value_ = nil then digest();
  BinToHexLower(md_value_, md_len, Result);
end;

class function THash.hash(const algorithm: string; const data: RawByteString;
  const hashSize: size_t): RawUTF8;
var
  H: THash;
begin
  H := THash.Create();
  try
    H.init(algorithm, hashSize);
    H.update(data);
    Result := H.digestHex;
  finally
    H.Free;
  end;
end;

constructor THash.Create;
begin
 if O = nil then
   O := OpenSSL;
 inherited Create();
end;

destructor THash.Destroy;
begin
  if md_value_ <> nil then
    O.OPENSSL_free(md_value_);
  if ctx <> nil then
    O.EVP_MD_CTX_free(ctx);
  inherited Destroy;
end;

{ _Hmac }

procedure THmac.init(const algorithm: string; const key: pointer; const keyLength: cardinal);
var
  md: PEVP_MD;
  r: integer;
begin
  md := O.EVP_get_digestbyname(PChar(algorithm));
  if md = nil then
    raise EOpenSSL.CreateFmt('Unknown message digest "%s"', [algorithm]);

  ctx := O.HMAC_CTX_new();
  if ctx = nil then
    EOpenSSL.Create('HMAC_CTX_new');
  r := O.HMAC_Init_ex(ctx, key, keyLength, md, nil);
  if r <> EVP_OK then
    raise EOpenSSL.Create(O.ERR_error_string(r));
end;

procedure THmac.init(const algorithm: string; const key: RawByteString);
begin
  init(algorithm, pointer(key), length(key));
end;

procedure THmac.update(const data: pointer; const dataLength: integer);
var r: integer;
begin
  r := O.HMAC_Update(ctx, data, dataLength);
  if r <> EVP_OK then
    raise EOpenSSL.Create(O.ERR_error_string(r));
end;

procedure THmac.update(const data: RawByteString);
begin
  update(pointer(data), length(data));
end;

function THmac.digest(buffer: pointer): integer;
var r: integer;
begin
  r := O.HMAC_Final(ctx, buffer, Result);
  if r <> EVP_OK then
    raise EOpenSSL.Create(O.ERR_error_string(r));
end;

function THmac.digestHex: RawUTF8;
var
  mdres: array[0..EVP_MAX_MD_SIZE] of byte;
  resLen: cardinal;
begin
  resLen := digest(@mdres);
  BinToHexLower(@mdres, resLen, Result);
end;

class function THmac.hmac(const algorithm: string; const key, data: RawByteString): RawUTF8;
var
  H: THmac;
begin
  H := THmac.Create;
  try
    H.init(algorithm, key);
    H.update(data);
    Result := H.digestHex;
  finally
    H.Free;
  end;
end;

constructor THmac.Create;
begin
  if O = nil then
    O := OpenSSL;
  inherited Create();
end;

destructor THmac.Destroy;
begin
  if ctx <> nil then O.HMAC_CTX_free(ctx);
  inherited Destroy;
end;

initialization
  O := nil;

end.

