/// ECDH and ECDSA cryptography for secp256r1 digital signature and key derivation
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynEcc;

(*
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2016 Arnaud Bouchez
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

  The Original Code is Synopse framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2016
  the Initial Developer. All Rights Reserved.

  Contributor(s):
   - Kenneth MacKay (easy-ecc source code)

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

  Using secp256r1 curve from "simple and secure ECDH and ECDSA library"
  Copyright (c) 2013, Kenneth MacKay - BSD 2-clause license
  https://github.com/esxgx/easy-ecc

  Version 1.18
  - first public release, corresponding to SQLite3 Framework 1.18

*)

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  SynCommons,
  SynCrypto;

const
  ECC_BYTES = 32;

type
  TECCPublicKey = array[0..ECC_BYTES] of byte;
  TECCPrivateKey = array[0..ECC_BYTES-1] of byte;
  TECCHash = TSHA256Digest;
  TECCSignature = array[0..(ECC_BYTES*2)-1] of byte;
  TECCSecretKey = TSHA256Digest;

{$ifdef CPUINTEL}

  {$define ECC_AVAILABLE}

  {$ifdef CPUX86}
    {$ifdef FPC}
      {$define ECC_32ASM}       // gcc -g -O1 -c ecc.c
    {$else}
      {$ifdef KYLIX3}
        {$define ECC_32ASM}     // gcc -g -O1 -c ecc.c
      {$else}
        {.$define ECC_32ASM}    // gcc -g -O1 -c ecc.c
        {.$define ECC_O1}       // gcc -g -O1 -c ecc.c
        {$define ECC_O2}        // gcc -g -O2 -c ecc.c
        {.$define ECC_O3}       // gcc -g -O3 -c ecc.c
      {$endif KYLIX}
    {$endif FPC}
  {$endif CPUX86}

  {$ifdef CPUX64}
      {.$define ECC_O1}       // gcc -g -O1 -c ecc.c
      {$define ECC_O2}        // gcc -g -O2 -c ecc.c
      {.$define ECC_O3}       // gcc -g -O3 -c ecc.c
  {$endif CPUX64}

{$endif}


{$ifdef ECC_32ASM}
var
  /// create a public/private key pair for further ECC cryptographic process
  // - using secp256r1 curve, i.e. NIST P-256, or OpenSSL prime256v1
  // - returns true if the key pair was generated successfully in pub/priv
  // - returns false if an error occurred
  ecc_make_key: function(out pub: TECCPublicKey; out priv: TECCPrivateKey): boolean; cdecl;

  /// compute a shared secret given your secret key and someone else's public key
  // - using secp256r1 curve, i.e. NIST P-256, or OpenSSL prime256v1
  // - Note: it is recommended that you hash the result of ecdh_shared_secret
  // before using it for symmetric encryption or HMAC
  // - returns true if the shared secret was generated successfully in secret
  // - returns false if an error occurred
  ecdh_shared_secret: function(const pub: TECCPublicKey; const priv: TECCPrivateKey;
    out secret: TECCSecretKey): boolean; cdecl;

  /// generate an ECDSA signature for a given hash value
  // - using secp256r1 curve, i.e. NIST P-256, or OpenSSL prime256v1
  // - returns true if the signature generated successfully in sign
  // - returns false if an error occurred
  ecdsa_sign: function(const priv: TECCPrivateKey; const hash: TECCHash;
    out sign: TECCSignature): boolean; cdecl;

  /// verify an ECDSA signature
  // - using secp256r1 curve, i.e. NIST P-256, or OpenSSL prime256v1
  // - returns true if the signature is valid
  // - returns false if it is invalid
  ecdsa_verify: function(const pub: TECCPublicKey; const hash: TECCHash;
    const sign: TECCSignature): boolean; cdecl;

{$else}

/// create a public/private key pair
// - using secp256r1 curve, i.e. NIST P-256, or OpenSSL prime256v1
// - returns true if the key pair was generated successfully in pub/priv
// - returns false if an error occurred, or if ecc_available=false
function ecc_make_key(out pub: TECCPublicKey; out priv: TECCPrivateKey): boolean; cdecl;

/// compute a shared secret given your secret key and someone else's public key
// - using secp256r1 curve, i.e. NIST P-256, or OpenSSL prime256v1
// - note: it is recommended that you hash the result of ecdh_shared_secret
// before using it for symmetric encryption or HMAC
// - returns true if the shared secret was generated successfully in secret
// - returns false if an error occurred, or if ecc_available=false
function ecdh_shared_secret(const pub: TECCPublicKey; const priv: TECCPrivateKey;
  out secret: TECCSecretKey): boolean; cdecl;

/// generate an ECDSA signature for a given hash value
// - using secp256r1 curve, i.e. NIST P-256, or OpenSSL prime256v1
// - returns true if the signature generated successfully in sign
// - returns false if an error occurred, or if ecc_available=false
function ecdsa_sign(const priv: TECCPrivateKey; const hash: TECCHash;
  out sign: TECCSignature): boolean; cdecl;

/// verify an ECDSA signature
// - using secp256r1 curve, i.e. NIST P-256, or OpenSSL prime256v1
// - returns true if the signature is valid
// - returns false if it is invalid, or if ecc_available=false
function ecdsa_verify(const pub: TECCPublicKey; const hash: TECCHash;
  const sign: TECCSignature): boolean; cdecl;

{$ifdef FPC}
{ **** latest FPC trunk expect those definitions to be part of the unit interface **** }
function getRandomNumber(dest: pointer): integer; cdecl;
{$endif FPC}

{$endif ECC_32ASM}

const
  /// contains true if the ECDSA/ECDH cryptographic functions are available
  // - only CPUINTEL is supported by now, i.e. x86/x64
  // - other CPUs, like ARM, would have false here, as all ECC functions
  ecc_available = {$ifdef ECC_AVAILABLE}true{$else}false{$endif};


implementation

{
  Benchmark of all available x86/32-bit variants, compiled with MinGW-W64 5.2.0

  gcc -g -O2 -c ecc.c
  d:\dev\tools\objconv.exe -fomf -nd -nu- ecc.o
  del eccwin32O2.o
  ren ecc.o SynEccWin32O2.o
  del eccwin32O2.obj
  ren ecc.obj SynEccWin32O2.obj

  Win32 ECC_32ASM
  - ecc_make_key: 1,000 assertions passed  2.38s
  - ecdsa_sign: 1,000 assertions passed  2.44s
  - ecdsa_verify: 1,000 assertions passed  2.96s
  - ecdh_shared_secret: 2,997 assertions passed  5.08s
  Total failed: 0 / 5,997  - ECC cryptography PASSED  12.88s

  Linux32 (Kylix) ECC_32ASM
  - ecc_make_key: 1,000 assertions passed  2.36s
  - ecdsa_sign: 1,000 assertions passed  2.44s
  - ecdsa_verify: 1,000 assertions passed  2.95s
  - ecdh_shared_secret: 2,997 assertions passed  5.07s
  Total failed: 0 / 5,997  - ECC cryptography PASSED  12.84s

  Win32 ECC_O1 (eccwin32O1.obj = 10480 bytes)
  - ecc_make_key: 1,000 assertions passed  2.34s
  - ecdsa_sign: 1,000 assertions passed  2.42s
  - ecdsa_verify: 1,000 assertions passed  2.91s
  - ecdh_shared_secret: 2,997 assertions passed  4.98s
  Total failed: 0 / 5,997  - ECC cryptography PASSED  12.67s

  Win32 ECC_O2 (eccwin32O2.obj = 16700 bytes)
  - ecc_make_key: 1,000 assertions passed  2.16s
  - ecdsa_sign: 1,000 assertions passed  2.20s
  - ecdsa_verify: 1,000 assertions passed  2.66s
  - ecdh_shared_secret: 2,997 assertions passed  4.58s
  Total failed: 0 / 5,997  - ECC cryptography PASSED  11.63s

  Win32 ECC_O3 (eccwin32O3.obj = 66798 bytes)
  - ecc_make_key: 1,000 assertions passed  2.17s
  - ecdsa_sign: 1,000 assertions passed  2.20s
  - ecdsa_verify: 1,000 assertions passed  2.65s
  - ecdh_shared_secret: 2,997 assertions passed  4.59s
  Total failed: 0 / 5,997  - ECC cryptography PASSED  11.64s

  -> conclusion: under Win32, ECC_O2 is used, and ECC_32ASM for Kylix+FPC
     time is around 2-3 ms for each operation


  Benchmark of all available x64/64-bit variants, compiled with MinGW-W64 5.2.0

  Win64 ECC_O1 (eccwin64O1.o = 45765 bytes)
  - ecc_make_key: 1,000 assertions passed  601.37ms
  - ecdsa_sign: 1,000 assertions passed  622.23ms
  - ecdsa_verify: 1,000 assertions passed  758.28ms
  - ecdh_shared_secret: 2,997 assertions passed  1.26s
  Total failed: 0 / 5,997  - ECC cryptography PASSED  3.32s

  Win64 ECC_O2 (eccwin64O2.o = 84779 bytes)
  - ecc_make_key: 1,000 assertions passed  573.09ms
  - ecdsa_sign: 1,000 assertions passed  588.86ms
  - ecdsa_verify: 1,000 assertions passed  712.31ms
  - ecdh_shared_secret: 2,997 assertions passed  1.20s
  Total failed: 0 / 5,997  - ECC cryptography PASSED  3.16s

  Win64 ECC_O3 (eccwin64O3.o = 204775 bytes)
  - access violation at startup (due to .o linking error by Delphi)

  -> conclusion: under Win64, ECC_O2 is used
     x64 is four time faster than x86 for such huge arithmetic tasks :)

}

{$ifdef ECC_AVAILABLE}

function getRandomNumber(dest: pointer): integer; cdecl;
{$ifdef FPC}{$ifdef CPU64}alias: 'getRandomNumber'{$else}alias: '_getRandomNumber'{$endif};{$endif}
begin
  TAESPRNG.Main.FillRandom(dest, ECC_BYTES);
  result := 1;
end;

{$ifdef ECC_32ASM}

{$I SynEcc32asm.inc}

{$else}

{$ifdef CPUX86}
  {$ifdef ECC_O1}
    {$L SynEcc32O1.obj}
  {$endif}
  {$ifdef ECC_O2}
    {$L SynEcc32O2.obj}
  {$endif}
  {$ifdef ECC_O3}
    {$L SynEcc32O3.obj}
  {$endif}
{$endif CPUX86}

{$ifdef CPUX64}
  {$ifdef ECC_O1}
    {$L SynEcc64O1.o}
  {$endif}
  {$ifdef ECC_O2}
    {$L SynEcc64O2.o}
  {$endif}
  {$ifdef ECC_O3}
    {$L SynEcc64O3.o}
  {$endif}
{$endif CPUX64}

function ecc_make_key; external;
function ecdh_shared_secret; external;
function ecdsa_sign; external;
function ecdsa_verify; external;

{$endif ECC_32ASM}

{$else ECC_AVAILABLE}

// currently no .o file available under ARM -> stub functions returning 0 (error)

function ecc_make_key(out pub: TECCPublicKey; out priv: TECCPrivateKey): boolean;
begin
  result := false;
end;

function ecdh_shared_secret(const pub: TECCPublicKey; const priv: TECCPrivateKey;
  out secret: TECCSecretKey): boolean;
begin
  result := false;
end;

function ecdsa_sign(const priv: TECCPrivateKey; const hash: TECCHash;
  out sign: TECCSignature): boolean;
begin
  result := false;
end;

function ecdsa_verify(const pub: TECCPublicKey; const hash: TECCHash;
  const sign: TECCSignature): boolean;
begin
  result := false;
end;

{$endif ECC_AVAILABLE}

initialization
{$ifdef ECC_32ASM} 
  pointer(@ecc_make_key) := pointer(@_ecc_make_key);
  pointer(@ecdh_shared_secret) := pointer(@_ecdh_shared_secret);
  pointer(@ecdsa_sign) := pointer(@_ecdsa_sign);
  pointer(@ecdsa_verify) := pointer(@_ecdsa_verify);
{$endif ECC_32ASM}
end.

