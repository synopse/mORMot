/// fast cryptographic routines (hashing and cypher)
// - implements AES, XOR, ADLER32, MD5, RC4, SHA1, SHA256 algorithms
// - optimized for speed (tuned assembler and AES-NI / PADLOCK support)
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynCrypto;

(*
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2015 Arnaud Bouchez
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

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2015
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Wolfgang Ehrhardt under zlib license for AES "pure pascal" versions
  - Intel's sha256_sse4.asm under under a three-clause Open Software license

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


      Synopse Cryptographic routines
      ==============================

    - fastest ever 100% Delphi (and asm ;) code
    - AES Crypto(128,192,256 bits key) with optimized asm version
      and multi-threaded code for multi-core CPU for blocks > 512 KB
    - XOR Crypto (32 bits key) - very fast with variable or fixed key
    - RC4 Crypto - weak, but simple and standard (used e.g. by SynPdf)
    - ADLER32 - 32 bits fast Hash with optimized asm version
    - MD5 - standard fast 128 bits Hash
    - SHA1 - 160 bits Secure Hash
    - SHA256 - 256 bits Secure Hash with optimized asm version
    - hardware AES-NI and SHA-SSE4 support for latest CPU
    - VIA PADLOCK optional support - native .o code on linux or .dll (Win32)
     (tested on a Dedibox C7 (rev1) linux server - need validation for Win32)
    - Microsoft AES Cryptographic Provider optional support via CryptoAPI

    Source code licenced under the MPL:
      see http://www.mozilla.org/MPL/MPL-1.1.html


    Benchmark on my AMD-64 TL-56 dualcore-CPU:
    ==========================================
    Testing with blocks of 16KB each
           crc32  624 MB/s
     adler32 pas  571 MB/s              asm 1304 MB/s
             MD5  176 MB/s
            SHA1  101 MB/s
          SHA256   63 MB/s
   AES128 cypher   84 MB/s         uncypher   81 MB/s  asm version
   AES128 cypher   57 MB/s         uncypher   57 MB/s  pascal version
   AES192 cypher   72 MB/s         uncypher   70 MB/s  asm version
   AES192 cypher   48 MB/s         uncypher   48 MB/s  pascal version
   AES256 cypher   62 MB/s         uncypher   61 MB/s  asm version
   AES256 cypher   42 MB/s         uncypher   42 MB/s  pascal version
        XorBlock 3463 MB/s  (very fast, since with 16KB data remain in L2 cache)
       XorOffset 3425 MB/s
        XorConst 5940 MB/s  (even faster, since no table used -> all in L1 cache)

    Testing with blocks of 1024KB each (for AES: block >512KB -> uses dualcore)
           crc32  577 MB/s
     adler32 pas  529 MB/s              asm 1003 MB/s
             MD5  176 MB/s
            SHA1  100 MB/s
          SHA256   63 MB/s
   AES128 cypher  129 MB/s         uncypher  130 MB/s  asm version
   AES128 cypher   96 MB/s         uncypher   95 MB/s  pascal version
   AES192 cypher  107 MB/s         uncypher  114 MB/s  asm version
   AES192 cypher   83 MB/s         uncypher   85 MB/s  pascal version
   AES256 cypher   98 MB/s         uncypher  105 MB/s  asm version
   AES256 cypher   76 MB/s         uncypher   76 MB/s  pascal version
        XorBlock 1423 MB/s   (we reach the memory control bandwidth)
       XorOffset 1325 MB/s
        XorConst 1506 MB/s

    Testing with blocks of 4096KB each (for AES: block >512KB -> uses dualcore)
           crc32  578 MB/s
     adler32 pas  525 MB/s              asm  984 MB/s
             MD5  175 MB/s
            SHA1  100 MB/s
          SHA256   63 MB/s
   AES128 cypher  159 MB/s         uncypher  147 MB/s  asm version
   AES128 cypher  107 MB/s         uncypher  109 MB/s  pascal version
   AES192 cypher  134 MB/s         uncypher  128 MB/s  asm version
   AES192 cypher   90 MB/s         uncypher   92 MB/s  pascal version
   AES256 cypher  118 MB/s         uncypher  113 MB/s  asm version
   AES256 cypher   80 MB/s         uncypher   81 MB/s  pascal version
        XorBlock 1385 MB/s
       XorOffset 1292 MB/s
        XorConst 1479 MB/s

   Benchmark on a C7 Dedibox (USEPADLOCK version):
   ===============================================
   Testing with blocks of 16KB each
           crc32  402 MB/s
     adler32 pas  274 MB/s        asm  542 MB/s       libz.so 414 MB/s
             MD5  126 MB/s
            SHA1  480 MB/s
          SHA256  458 MB/s
   AES128 cypher 1566 MB/s         uncypher 1560 MB/s
   AES192 cypher 1421 MB/s         uncypher 1422 MB/s
   AES256 cypher 1237 MB/s         uncypher 1247 MB/s
        XorBlock 2336 MB/s
       XorOffset 1807 MB/s
        XorConst 3154 MB/s

    Testing with blocks of 1024KB each
           crc32  352 MB/s
     adler32 pas  256 MB/s         asm  395 MB/s      libz.so 361 MB/s
             MD5  123 MB/s
            SHA1  324 MB/s
          SHA256  324 MB/s
   AES128 cypher  552 MB/s         uncypher  552 MB/s
   AES192 cypher  552 MB/s         uncypher  552 MB/s
   AES256 cypher  552 MB/s         uncypher  552 MB/s
        XorBlock  354 MB/s
       XorOffset  373 MB/s
        XorConst  511 MB/s

    Testing with blocks of 4096KB each
           crc32  352 MB/s
     adler32 pas  255 MB/s         asm  395 MB/s      libz.so 361 MB/s
             MD5  124 MB/s
            SHA1  324 MB/s
          SHA256  326 MB/s
   AES128 cypher  552 MB/s         uncypher  552 MB/s
   AES192 cypher  552 MB/s         uncypher  552 MB/s
   AES256 cypher  552 MB/s         uncypher  552 MB/s
        XorBlock  352 MB/s
       XorOffset  368 MB/s
        XorConst  510 MB/s

   Conclusion:
   - USETHREADSFORBIGAESBLOCKS will help on modern multi-threaded CPU
   - AES speed: W.Ehrhardt's pascal is 55MB/s, A.Bouchez's asm is 84MB/s
   - AES-256 is faster than a simple XOR() on a dedibox with a C7 cpu ;)
   - see below for benchmarks using AES-NI or SHA-256-SSE4, which induce
     a huge performance boost

   Initial version (C) 2008-2009 Arnaud Bouchez http://bouchez.info

   Revision History:

   Version 1.0
    - initial release on Internet, with MyCrypto unit name

   Version 1.1
    - updated release, with new optimized AES i386 assembler implementation
      and no FastCode dependency (CpuCount is taken from Windows API)

   Version 1.4 - February 8, 2010
   - whole Synopse SQLite3 database framework released under the GNU Lesser
     General Public License version 3, instead of generic "Public Domain"

   Version 1.8
   - mostly code review for Delphi 2009/2010 integration (unit uses now
     SynCommons string types definitions)

   Version 1.9
   - now use direct Windows threads, since we don't need any exception handling
     nor memory usage inside the AES encryption Thread handler
     -> avoid classes.TThread and system.BeginThread() use
     -> application is still "officialy" mono-threaded (i.e. IsMultiThread=false),
     for faster System.pas and FastMM4 (prevent CPU locking - see
     http://synopse.info/forum/viewtopic.php?id=57 about Delphi & multi-core)
   - some other minor fixes and enhancements

   Version 1.10
   - code modifications to compile with Delphi 6 compiler

   Version 1.13
   - code modifications to compile with Delphi 5 compiler

   Version 1.15
   - unit now tested with Delphi XE2 (32 Bit)

   Version 1.16
   - added TAESECB, TAESCBC, TAESCFB, TAESOFB and TAESCTR classes to handle AES
     encryption of memory buffers in ECB, CBC, CFB, OFB and CTR mode (including
     PKCS7 padding)
   - added pure pascal version (for XE2 64 compilation) of all algorithms

   Version 1.18
   - added AES-NI hardware support on newer CPUs, for huge performance boost
     and enhanced security
   - AES encryption will compute its own tables, to get rid of 4KB of const
   - tested compilation for Win64 platform
   - run with FPC under Win32 and Linux (including AES-NI support), and Kylix
   - added Intel's SSE4 x64 optimized asm for SHA-256 on Win64
   - added overloaded procedure TMD5.Final() and function SHA256()
   - introduce ESynCrypto exception class dedicated to this unit
   - added AES encryption using official Microsoft AES Cryptographic Provider
     (CryptoAPI) via TAESECB_API, TAESCBC_API, TAESCFB_API and TAESOFB_API -
     our optimized asm version is faster, so is still our default/preferred
   - added optional IVAtBeginning parameter to EncryptPKCS7/DecryptPKC7 methods
   - get rid of the unsafe IV parameter for TAES* classes constructors
   - added CompressShaAes() and global CompressShaAesKey and CompressShaAesClass
     variables to be used by THttpSocket.RegisterCompress
   - introduce new TRC4 object for RC4 encryption algorithm
   - removed several compilation hints when assertions are set to off

*)

interface

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

{.$define USEPADLOCK}

{.$define PUREPASCAL} // for debug

{$ifdef Linux}
  {$undef USETHREADSFORBIGAESBLOCKS} // uses low-level WinAPI threading
  {.$define USEPADLOCK} // dedibox linux tested only, but may be OK on Windows
{$else}
  {$ifdef CONDITIONALEXPRESSIONS}
    // on Windows: enable Microsoft AES Cryptographic Provider (XP SP3 and up)
    {$define USE_PROV_RSA_AES}
  {$endif}
  // on Windows: will use Threads for very big blocks (>512KB) if multi-CPU
  {$define USETHREADSFORBIGAESBLOCKS}
{$endif}

{$ifdef USEPADLOCK}
{$ifdef MSWINDOWS}
  {$define USEPADLOCKDLL}   // Win32: we can use LibPadlock.dll
{$else}
  {.$define PADLOCKDEBUG}   // display message before using padlock
  {.$define USEPADLOCKDLL}  // Linux: use fast .o linked code
{$endif}
{$endif}

uses
{$ifdef MSWINDOWS}
  Windows,
{$endif}
  SysUtils,
{$ifndef LVCL}
  {$ifdef CONDITIONALEXPRESSIONS}
  RTLConsts,
  {$endif}
{$endif}
  Classes,
  SynLZ, // already included in SynCommons, and used by CompressShaAes()
  SynCommons;


const
  /// hide all AES Context complex code
  AESContextSize = 275 {$ifdef USEPADLOCK}+sizeof(pointer){$endif};
  /// hide all SHA Context complex code
  SHAContextSize = 108;
  /// standard AES block size (in bytes) during cypher/uncypher
  AESBlockSize = 16;
  /// maximum AES key size (in bytes)
  AESKeySize = 256 div 8;

type
  /// class of Exceptions raised by this unit
  ESynCrypto = class(Exception);

  PAESBlock = ^TAESBlock;

  /// 128 bits memory block for AES data cypher/uncypher
  TAESBlock = packed array[0..AESBlockSize-1] of byte;

  /// 256 bits memory block for maximum AES key storage
  TAESKey = packed array[0..AESKeySize-1] of byte;

  PAES = ^TAES;
  /// handle AES cypher/uncypher
  // - this is the default Electronic codebook (ECB) mode
  // - this class will use AES-NI hardware instructions, if available
  {$ifdef USEPADLOCK}
  // - this class will use VIA PadLock instructions, if available
  {$endif}
  TAES = {$ifndef UNICODE}object{$else}record{$endif}
  private
    Context: packed array[1..AESContextSize] of byte;
{$ifdef USEPADLOCK}
    function DoPadlockInit(const Key; KeySize: cardinal): boolean;
{$endif}
  public
    /// true if the context was initialized
    Initialized: boolean;
    /// Initialize AES contexts for cypher
    // - first method to call before using this class
    // - KeySize is in bits, i.e. 128,192,256
    function EncryptInit(const Key; KeySize: cardinal): boolean;
    /// encrypt an AES data block into another data block
    procedure Encrypt(const BI: TAESBlock; var BO: TAESBlock); overload;
    /// encrypt an AES data block
    procedure Encrypt(var B: TAESBlock); overload;

    /// Initialize AES contexts for uncypher
    function DecryptInit(const Key; KeySize: cardinal): boolean;
    /// decrypt an AES data block
    procedure Decrypt(var B: TAESBlock); overload;
    /// decrypt an AES data block into another data block
    procedure Decrypt(const BI: TAESBlock; var BO: TAESBlock); overload;

    /// Finalize AES contexts for both cypher and uncypher
    // - only used with Padlock
    procedure Done;

    /// generic initialization method for AES contexts
    // - call either EncryptInit() either DecryptInit() method
    function DoInit(const Key; KeySize: cardinal; doEncrypt: boolean): boolean;
    /// perform the AES cypher or uncypher to continuous memory blocks
    // - call either Encrypt() either Decrypt() method
    procedure DoBlocks(pIn, pOut: PAESBlock; out oIn, oOut: PAESBLock; Count: integer; doEncrypt: boolean); overload;
    /// perform the AES cypher or uncypher to continuous memory blocks
    // - call either Encrypt() either Decrypt() method
    procedure DoBlocks(pIn, pOut: PAESBlock; Count: integer; doEncrypt: boolean); overload;
{$ifdef USETHREADSFORBIGAESBLOCKS}
    /// perform the AES cypher or uncypher to continuous memory blocks
    // - this special method will use Threads for bigs blocks (>512KB) if multi-CPU
    // - call either Encrypt() either Decrypt() method
    procedure DoBlocksThread(var bIn, bOut: PAESBlock; Count: integer; doEncrypt: boolean);
{$endif}
    /// return TRUE if the AES-NI instruction sets are available on this CPU
    function UsesAESNI: boolean;
  end;

  TAESAbstractClass = class of TAESAbstract;

  /// handle AES cypher/uncypher with chaining
  // - use any of the inherited implementation, corresponding to the chaining
  // mode required - TAESECB, TAESCBC, TAESCFB, TAESOFB and TAESCTR classes to
  // handle in ECB, CBC, CFB, OFB and CTR mode (including PKCS7 padding)
  TAESAbstract = class
  protected
    fKeySize: cardinal;
    fKeySizeBytes: cardinal;
    fKey: TAESKey;
    fIV: TAESBlock;
  public
    /// Initialize AES contexts for cypher
    // - first method to call before using this class
    // - KeySize is in bits, i.e. 128,192,256
    constructor Create(const aKey; aKeySize: cardinal); virtual;
    /// compute a class instance similar to this one
    function Clone: TAESAbstract; virtual;

    /// perform the AES cypher in the corresponding mode
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); virtual; abstract;
    /// perform the AES un-cypher in the corresponding mode
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); virtual; abstract;

    /// encrypt a memory buffer using a PKCS7 padding pattern
    // - PKCS7 is described in RFC 5652 - it will add up to 16 bytes to
    // the input buffer
    // - if IVAtBeginning is TRUE, a random Initialization Vector will be computed,
    // and stored at the beginning of the output binary buffer
    function EncryptPKCS7(const Input: RawByteString; IVAtBeginning: boolean=false): RawByteString;
    /// decrypt a memory buffer using a PKCS7 padding pattern
    // - PKCS7 is described in RFC 5652 - it will trim up to 16 bytes from
    // the input buffer
    // - if IVAtBeginning is TRUE, the Initialization Vector will be taken
    // from the beginning of the input binary buffer
    function DecryptPKCS7(const Input: RawByteString; IVAtBeginning: boolean=false): RawByteString;

    /// simple wrapper able to cypher/decypher any content
    // - here all data variable could be text or binary
    // - if IVAtBeginning is TRUE, a random Initialization Vector will be computed,
    // and stored at the beginning of the output binary buffer
    class function SimpleEncrypt(const Input,Key: RawByteString; Encrypt: boolean;
      IVAtBeginning: boolean=false): RawByteString;

    /// associated Key Size, in bits (i.e. 128,192,256)
    property KeySize: cardinal read fKeySize;
    /// associated Initialization Vector
    // - you should better use PKCS7 encoding with IVAtBeginning option than
    // a fixed Initialization Vector, especially in ECB mode
    property IV: TAESBlock read fIV write fIV;
  end;

  /// handle AES cypher/uncypher with chaining
  // - use any of the inherited implementation, corresponding to the chaining
  // mode required - TAESECB, TAESCBC, TAESCFB, TAESOFB and TAESCTR classes to
  // handle in ECB, CBC, CFB, OFB and CTR mode (including PKCS7 padding)
  // - this class will use AES-NI hardware instructions, if available
  // - those classes are re-entrant, i.e. that you can call the Encrypt*
  // or Decrypt* methods on the same instance several times
  TAESAbstractSyn = class(TAESAbstract)
  protected
    fIn, fOut: PAESBlock;
    AES: TAES;
    fCount: Cardinal;
    fCV: TAESBlock;
    procedure EncryptInit;
    procedure DecryptInit;
    procedure EncryptTrailer;
    procedure DecryptTrailer;
  public
    /// perform the AES cypher in the corresponding mode
    // - this abstract method will set CV from AES.Context, and fIn/fOut
    // from BufIn/BufOut
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the corresponding mode
    // - this abstract method will set CV from AES.Context, and fIn/fOut
    // from BufIn/BufOut
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

  /// handle AES cypher/uncypher without chaining (ECB)
  // - this mode is known to be less secure than the others
  // - IV property should be set to a fixed value to encode the trailing bytes
  // of the buffer by a simple XOR - but you should better use the PKC7 pattern
  // - this class will use AES-NI hardware instructions, if available, e.g.
  // ! ECB128: 19.70ms in x86 optimized code, 6.97ms with AES-NI
  TAESECB = class(TAESAbstractSyn)
  public
    /// perform the AES cypher in the ECB mode
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the ECB mode
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

  /// handle AES cypher/uncypher with Cipher-block chaining (CBC)
  // - this class will use AES-NI hardware instructions, if available, e.g.
  // ! CBC192: 24.91ms in x86 optimized code, 9.75ms with AES-NI
  TAESCBC = class(TAESAbstractSyn)
  public
    /// perform the AES cypher in the CBC mode
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the CBC mode
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

  /// handle AES cypher/uncypher with Cipher feedback (CFB)
  // - this class will use AES-NI hardware instructions, if available, e.g.
  // ! CFB128: 22.25ms in x86 optimized code, 9.29ms with AES-NI
  TAESCFB = class(TAESAbstractSyn)
  public
    /// perform the AES cypher in the CFB mode
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the CFB mode
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

  /// handle AES cypher/uncypher with Output feedback (OFB)
  // - this class will use AES-NI hardware instructions, if available, e.g.
  // ! OFB256: 27.69ms in x86 optimized code, 9.94ms with AES-NI
  TAESOFB = class(TAESAbstractSyn)
  public
    /// perform the AES cypher in the OFB mode
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the OFB mode
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

  /// handle AES cypher/uncypher with Counter mode (CTR)
  // - this class will use AES-NI hardware instructions, e.g.
  // ! CTR256: 28.13ms in x86 optimized code, 10.63ms with AES-NI
  TAESCTR = class(TAESAbstractSyn)
  public
    /// perform the AES cypher in the CTR mode
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the CTR mode
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

{$ifdef USE_PROV_RSA_AES}

  /// handle AES cypher/uncypher using Windows CryptoAPI and the
  // official Microsoft AES Cryptographic Provider (PROV_RSA_AES)
  // - see @http://msdn.microsoft.com/en-us/library/windows/desktop/aa386979
  // - timing of our optimized asm versions, for small (<=8KB) block processing
  // (similar to standard web pages or most typical JSON/XML content),
  // benchmarked on a Core i7 notebook and compiled as Win32 platform:
  // ! AES128 - ECB:79.33ms CBC:83.37ms CFB:80.75ms OFB:78.98ms CTR:80.45ms
  // ! AES192 - ECB:91.16ms CBC:96.06ms CFB:96.45ms OFB:92.12ms CTR:93.38ms
  // ! AES256 - ECB:103.22ms CBC:119.14ms CFB:111.59ms OFB:107.00ms CTR:110.13ms
  // - timing of the same process, using CryptoAPI official PROV_RSA_AES provider:
  // ! AES128 - ECB_API:102.88ms CBC_API:124.91ms
  // ! AES192 - ECB_API:115.75ms CBC_API:129.95ms
  // ! AES256 - ECB_API:139.50ms CBC_API:154.02ms
  // - but the CryptoAPI does not supports AES-NI, whereas our classes do on Win32,
  // with a huge speed benefit
  // - under Win64, the official CryptoAPI is faster than our PUREPASCAL version,
  // and the Win32 version of CryptoAPI itself:
  // ! AES128 - ECB:107.95ms CBC:112.65ms CFB:109.62ms OFB:107.23ms CTR:109.42ms
  // ! AES192 - ECB:130.30ms CBC:133.04ms CFB:128.78ms OFB:127.25ms CTR:130.22ms
  // ! AES256 - ECB:145.33ms CBC:147.01ms CFB:148.36ms OFB:145.96ms CTR:149.67ms
  // ! AES128 - ECB_API:89.64ms CBC_API:100.84ms
  // ! AES192 - ECB_API:99.05ms CBC_API:105.85ms
  // ! AES256 - ECB_API:107.11ms CBC_API:118.04ms
  TAESAbstract_API = class(TAESAbstract)
  protected
    fKeyHeader: packed record
      bType: byte;
      bVersion: byte;
      reserved: word;
      aiKeyAlg: cardinal;
      dwKeyLength: cardinal;
    end;
    fKeyHeaderKey: TAESKey;
    fKeyCryptoAPI: pointer;
    fInternalMode: cardinal;
    procedure InternalSetMode; virtual; abstract;
    procedure EncryptDecrypt(BufIn, BufOut: pointer; Count: cardinal; DoEncrypt: boolean);
  public
    /// Initialize AES context for cypher
    // - first method to call before using this class
    // - KeySize is in bits, i.e. 128,192,256
    constructor Create(const aKey; aKeySize: cardinal); override;
    /// release the AES execution context
    destructor Destroy; override;
    /// perform the AES cypher in the ECB mode
    // - if Count is not a multiple of a 16 bytes block, the IV will be used
    // to XOR the trailing bytes - so it won't be compatible with our
    // TAESAbstractSyn classes: you should better use PKC7 padding instead
    procedure Encrypt(BufIn, BufOut: pointer; Count: cardinal); override;
    /// perform the AES un-cypher in the ECB mode
    // - if Count is not a multiple of a 16 bytes block, the IV will be used
    // to XOR the trailing bytes - so it won't be compatible with our
    // TAESAbstractSyn classes: you should better use PKC7 padding instead
    procedure Decrypt(BufIn, BufOut: pointer; Count: cardinal); override;
  end;

  /// handle AES cypher/uncypher without chaining (ECB) using Windows CryptoAPI
  TAESECB_API = class(TAESAbstract_API)
  protected
    /// will set fInternalMode := CRYPT_MODE_ECB
    procedure InternalSetMode; override;
  end;

  /// handle AES cypher/uncypher Cipher-block chaining (CBC) using Windows CryptoAPI
  TAESCBC_API = class(TAESAbstract_API)
  protected
    /// will set fInternalMode := CRYPT_MODE_CBC
    procedure InternalSetMode; override;
  end;

  /// handle AES cypher/uncypher Cipher feedback (CFB) using Windows CryptoAPI
  // - NOT TO BE USED: the current PROV_RSA_AES provider does not return
  // expected values for CFB
  TAESCFB_API = class(TAESAbstract_API)
  protected
    /// will set fInternalMode := CRYPT_MODE_CFB
    procedure InternalSetMode; override;
  end;

  /// handle AES cypher/uncypher Output feedback (OFB) using Windows CryptoAPI
  // - NOT TO BE USED: the current PROV_RSA_AES provider does not implement
  // this mode, and returns a NTE_BAD_ALGID error
  TAESOFB_API = class(TAESAbstract_API)
  protected
    /// will set fInternalMode := CRYPT_MODE_OFB
    procedure InternalSetMode; override;
  end;

{$endif USE_PROV_RSA_AES}



  PSHA1Digest = ^TSHA1Digest;
  /// 160 bits memory block for SHA1 hash digest storage
  TSHA1Digest   = packed array[0..19] of byte;

  PSHA1 = ^TSHA1;
  /// handle SHA1 hashing
  TSHA1 = {$ifndef UNICODE}object{$else}record{$endif}
  private
    Context: packed array[1..SHAContextSize div 4] of cardinal;
    procedure Compress; // used by Update and Final
  public
    /// initialize SHA1 context for hashing
    procedure Init;
    /// update the SHA1 context with some data
    procedure Update(Buffer: pointer; Len: integer);
    /// finalize and compute the resulting SHA1 hash Digest of all data
    // affected to Update() method
    procedure Final(out Digest: TSHA1Digest);
    /// one method to rule them all
    // - call Init, then Update(), then Final()
    // - only Full() is Padlock-implemented - use this rather than Update()
    procedure Full(Buffer: pointer; Len: integer; out Digest: TSHA1Digest);
  end;

  PSHA256Digest = ^TSHA256Digest;
  /// 256 bits memory block for SHA256 hash digest storage
  TSHA256Digest   = packed array[0..31] of byte;

  PSHA256 = ^TSHA256;
  /// handle SHA256 hashing
  TSHA256 = {$ifndef UNICODE}object{$else}record{$endif}
  private
    Context: packed array[1..SHAContextSize] of byte;
    procedure Compress; // used by Update and Final
  public
    /// initialize SHA256 context for hashing
    procedure Init;
    /// update the SHA256 context with some data
    procedure Update(Buffer: pointer; Len: integer);
    /// finalize and compute the resulting SHA256 hash Digest of all data
    // affected to Update() method
    procedure Final(out Digest: TSHA256Digest);
    /// one method to rule them all
    // - call Init, then Update(), then Final()
    // - only Full() is Padlock-implemented - use this rather than Update()
    procedure Full(Buffer: pointer; Len: integer; out Digest: TSHA256Digest);
  end;

  TMD5In = array[0..15] of cardinal;
  /// 128 bits memory block for MD5 hash digest storage
  TMD5Digest = array[0..15] of Byte;
  PMD5 = ^TMD5;
  TMD5Buf = array[0..3] of cardinal;

  /// handle MD5 hashing
  TMD5 = {$ifndef UNICODE}object{$else}record{$endif}
  private
    buf: TMD5Buf;
    bytes: array[0..1] of cardinal;
    in_: TMD5In;
    procedure Finalize;
  public
    /// initialize MD5 context for hashing
    procedure Init;
    /// update the MD5 context with some data
    procedure Update(const buffer; Len: cardinal);
    /// finalize and compute the resulting MD5 hash Digest of all data
    // affected to Update() method
    procedure Final(out result: TMD5Digest); overload;
    /// finalize and compute the resulting MD5 hash Digest of all data
    // affected to Update() method
    function Final: TMD5Digest; overload;
    /// one method to rule them all
    // - call Init, then Update(), then Final()
    procedure Full(Buffer: pointer; Len: integer; out Digest: TMD5Digest);
  end;

  /// internal key permutation buffer, as used by TRC4
  TRC4InternalKey = array[byte] of byte;

  /// handle RC4 encryption/decryption
  TRC4 = {$ifndef UNICODE}object{$else}record{$endif}
  private
    key: TRC4InternalKey;
  public
    /// initialize the RC4 encryption/decryption
    // - KeyLen is in bytes, and should be within 1..255 range
    procedure Init(const aKey; aKeyLen: integer);
    /// perform the RC4 cypher encryption/decryption on a buffer
    // - each call to this method shall be preceded with an Init() call,
    // or a RestoreKey() from a previous SaveKey(), since it will change
    // the internal key[] during its process
    // - RC4 is a symetrical algorithm: use this Encrypt() method for both
    // encryption and decryption of any buffer
    procedure Encrypt(const BufIn; var BufOut; Count: cardinal);
    /// save the internal key computed by Init()
    procedure SaveKey(out Backup: TRC4InternalKey);
    /// restore the internal key as computed by Init()
    procedure RestoreKey(const Backup: TRC4InternalKey);
  end;

{$A-} { packed memory structure }
  /// internal header for storing our AES data with salt and CRC
  TAESFullHeader = {$ifndef UNICODE}object{$else}record{$endif}
  public
    /// Len before compression (if any)
    OriginalLen,
    /// Len before AES encoding
    SourceLen,
    /// Random Salt for better encryption
    SomeSalt,
    /// CRC from header
    HeaderCheck: cardinal;
    function Calc(const Key; KeySize: cardinal): cardinal;
  end;
{$A+}

  PAESFull = ^TAESFull;
  /// AES and XOR encryption object for easy direct memory or stream access
  // - calls internaly TAES objet methods, and handle memory and streams for best speed
  // - a TAESFullHeader is encrypted at the begining, allowing fast Key validation,
  // but the resulting stream is not compatible with raw TAES object
  TAESFull = {$ifndef UNICODE}object{$else}record{$endif}
  public
    /// header, stored at the beginning of struct -> 16-byte aligned
    Head: TAESFullHeader;
    /// this memory stream is used in case of EncodeDecode(outStream=bOut=nil)
    // method call
    outStreamCreated: TMemoryStream;
    /// main method of AES or XOR cypher/uncypher
    // - return out size, -1 if error on decoding (Key not correct)
    // - valid KeySize: 0=nothing, 32=xor, 128,192,256=AES
    // - if outStream is TMemoryStream -> auto-reserve space (no Realloc:)
    // - for normal usage, you just have to Assign one In and one Out
    // - if outStream AND bOut are both nil, an outStream is created via
    // THeapMemoryStream.Create
    // - if Padlock is used, 16-byte alignment is forced (via tmp buffer if necessary)
    // - if Encrypt -> OriginalLen can be used to store unCompressed Len
    function EncodeDecode(const Key; KeySize, inLen: cardinal; Encrypt: boolean;
      inStream, outStream: TStream; bIn, bOut: pointer; OriginalLen: cardinal=0): integer;
  end;

  /// AES encryption stream
  // - encrypt the Data on the fly, in a compatible way with AES() - last bytes
  // are coded with XOR (not compatible with TAESFull format)
  // - not optimized for small blocks -> ok if used AFTER TBZCompressor/TZipCompressor
  // - warning: Write() will crypt Buffer memory in place -> use AFTER T*Compressor
  TAESWriteStream = class(TStream)
  public
    Adler, // CRC from uncrypted compressed data - for Key check
    DestSize: cardinal;
  private
    Dest: TStream;
    Buf: TAESBlock; // very small buffer for remainging 0..15 bytes
    BufCount: integer; // number of pending bytes (0..15) in Buf
    AES: TAES;
    NoCrypt: boolean; // if KeySize=0
  public
    /// initialize the AES encryption stream for an output stream (e.g.
    // a TMemoryStream or a TFileStream)
    constructor Create(outStream: TStream; const Key; KeySize: cardinal);
    /// finalize the AES encryption stream
    // - internaly call the Finish method
    destructor Destroy; override;
    /// read some data is not allowed -> this method will raise an exception on call
    function Read(var Buffer; Count: Longint): Longint; override;
    /// append some data to the outStream, after encryption
    function Write(const Buffer; Count: Longint): Longint; override;
    /// read some data is not allowed -> this method will raise an exception on call
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    /// write pending data
    // - should always be called before closeing the outStream (some data may
    // still be in the internal buffers)
    procedure Finish;
  end;


/// direct MD5 hash calculation of some data
function MD5Buf(const Buffer; Len: Cardinal): TMD5Digest;

/// direct MD5 hash calculation of some data (string-encoded)
// - result is returned in hexadecimal format
function MD5(const s: RawByteString): RawUTF8;

/// direct SHA1 hash calculation of some data (string-encoded)
// - result is returned in hexadecimal format
function SHA1(const s: RawByteString): RawUTF8;

/// direct SHA256 hash calculation of some data (string-encoded)
// - result is returned in hexadecimal format
function SHA256(const s: RawByteString): RawUTF8; overload;

/// direct SHA256 hash calculation of some binary data
// - result is returned in hexadecimal format
function SHA256(Data: pointer; Len: integer): RawUTF8; overload;

/// direct SHA256 hash calculation of some data (string-encoded)
// - result is returned in hexadecimal format
// - this procedure has a weak password protection: small incoming data
// is append to some salt, in order to have at least a 256 bytes long hash:
// such a feature improve security for small passwords, e.g.
procedure SHA256Weak(const s: RawByteString; out Digest: TSHA256Digest); overload;


/// direct Encrypt/Decrypt of data using the TAES class
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
procedure AES(const Key; KeySize: cardinal; buffer: pointer; Len: Integer; Encrypt: boolean); overload;

/// direct Encrypt/Decrypt of data using the TAES class
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
procedure AES(const Key; KeySize: cardinal; bIn, bOut: pointer; Len: Integer; Encrypt: boolean); overload;

/// direct Encrypt/Decrypt of data using the TAES class
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
function  AES(const Key; KeySize: cardinal; const s: RawByteString; Encrypt: boolean): RawByteString; overload;

/// direct Encrypt/Decrypt of data using the TAES class
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
function  AES(const Key; KeySize: cardinal; buffer: pointer; Len: cardinal; Stream: TStream; Encrypt: boolean): boolean; overload;

/// AES and XOR encryption using the TAESFull format
// - outStream will be larger/smaller than Len (full AES encrypted)
// - returns true if OK
function AESFull(const Key; KeySize: cardinal; bIn: pointer; Len: Integer;
  outStream: TStream; Encrypt: boolean; OriginalLen: Cardinal=0): boolean; overload;

/// AES and XOR encryption using the TAESFull format
// - bOut must be at least bIn+32/Encrypt bIn-16/Decrypt
// - returns outLength, -1 if error
function AESFull(const Key; KeySize: cardinal; bIn, bOut: pointer; Len: Integer;
  Encrypt: boolean; OriginalLen: Cardinal=0): integer; overload;

/// AES and XOR decryption check using the TAESFull format
// - return true if begining of buff contains true AESFull encrypted data with this Key
// - if not KeySize in [128,192,256] -> use fast and efficient Xor Cypher
function AESFullKeyOK(const Key; KeySize: cardinal; buff: pointer): boolean;

/// AES encryption using the TAES format with a supplied SHA256 password
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
procedure AESSHA256(Buffer: pointer; Len: integer; const Password: RawByteString; Encrypt: boolean); overload;

/// AES encryption using the TAES format with a supplied SHA256 password
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
procedure AESSHA256(bIn, bOut: pointer; Len: integer; const Password: RawByteString; Encrypt: boolean); overload;

/// AES encryption using the TAES format with a supplied SHA256 password
// - last bytes (not part of 16 bytes blocks) are not crypted by AES, but with XOR
function AESSHA256(const s, Password: RawByteString; Encrypt: boolean): RawByteString; overload;

/// AES encryption using the TAESFull format with a supplied SHA256 password
// - outStream will be larger/smaller than Len: this is a full AES version with
// a triming TAESFullHeader at the beginning
procedure AESSHA256Full(bIn: pointer; Len: Integer; outStream: TStream; const Password: RawByteString; Encrypt: boolean); overload;

const
  SHA1DIGESTSTRLEN = sizeof(TSHA1Digest)*2;
  SHA256DIGESTSTRLEN = sizeof(TSHA256Digest)*2;
  MD5DIGESTSTRLEN = sizeof(TMD5Digest)*2;

/// compute the hexadecimal representation of a SHA1 digest
function SHA1DigestToString(const D: TSHA1Digest): RawUTF8;

/// compute the hexadecimal representation of a SHA256 digest
function SHA256DigestToString(const D: TSHA256Digest): RawUTF8;

/// compare two supplied MD5 digests
function MD5DigestsEqual(const A, B: TMD5Digest): Boolean;

/// compute the hexadecimal representation of a MD5 digest
function MD5DigestToString(const D: TMD5Digest): RawUTF8;

/// fill a block of 16 bytes with some random values
// - is used internally by this unit to compute an AES Initialization Vector
procedure FillRandom(var IV: TAESBlock);

/// compute the HTDigest for a user and a realm, according to a supplied password
// - apache-compatible: 'agent007:download area:8364d0044ef57b3defcfa141e8f77b65'
function htdigest(const user, realm, pass: RawByteString): RawUTF8;

/// self test of Adler32 routines
function Adler32SelfTest: boolean;

/// self test of MD5 routines
function MD5SelfTest: boolean;

/// self test of SHA1 routines
function SHA1SelfTest: boolean;

/// self test of SHA256 routines
function SHA256SelfTest: boolean;

/// self test of AES routines
function AESSelfTest(onlytables: Boolean): boolean;

/// self test of RC4 routines
function RC4SelfTest: boolean;

// little endian fast conversion
// - 160 bits = 5 integers
// - use fast bswap asm in x86/x64 mode
procedure bswap160(s,d: PIntegerArray);

// little endian fast conversion
// - 256 bits = 8 integers
// - use fast bswap asm in x86/x64 mode
procedure bswap256(s,d: PIntegerArray);

/// simple Adler32 implementation
// - a bit slower than Adler32Asm() version below, but shorter code size
function Adler32Pas(Adler: cardinal; p: pointer; Count: Integer): cardinal;

/// fast Adler32 implementation
// - 16-bytes-chunck unrolled asm version
function Adler32Asm(Adler: cardinal; p: pointer; Count: Integer): cardinal;
  {$ifdef PUREPASCAL}{$ifdef HASINLINE}inline;{$endif}{$endif}

// - very fast XOR according to Cod - not Compression or Stream compatible
// - used in AESFull() for KeySize=32
procedure XorBlock(p: PIntegerArray; Count, Cod: integer);

/// fast and simple XOR Cypher using Index (=Position in Dest Stream)
// - Compression not compatible with this function: should be applied after
// compress (e.g. as outStream for TAESWriteStream)
// - Stream compatible (with updated Index)
// - used in AES() and TAESWriteStream
procedure XorOffset(p: pByte; Index,Count: integer);

/// fast XOR Cypher changing by Count value
// - Compression compatible, since the XOR value is always the same, the
// compression rate will not change a lot
procedure XorConst(p: PIntegerArray; Count: integer);

var
  /// the encryption key used by CompressShaAes() global function
  // - the key is global to the whole process
  // - use CompressShaAesSetKey() procedure to set this Key from text 
  CompressShaAesKey: TSHA256Digest;

  /// the AES-256 encoding class used by CompressShaAes() global function
  // - use any of the implementation classes, corresponding to the chaining
  // mode required - TAESECB, TAESCBC, TAESCFB, TAESOFB and TAESCTR classes to
  // handle in ECB, CBC, CFB, OFB and CTR mode (including PKCS7 padding)
  // - set to the secure and efficient CFB mode by default
  CompressShaAesClass: TAESAbstractClass = TAESCFB;

/// set an text-based encryption key for CompressShaAes() global function
// - will compute the key via SHA256Weak() and set CompressShaAesKey
// - the key is global to the whole process
procedure CompressShaAesSetKey(const Key: RawByteString; AesClass: TAESAbstractClass=nil);

/// encrypt data content using the AES-256/CTR algorithm, after SynLZ compression
// - as expected by THttpSocket.RegisterCompress()
// - will return 'synshaaes' as ACCEPT-ENCODING: header parameter
// - will use global CompressShaAesKey / CompressShaAesClass variables to be set
// according to the expected algorithm and Key e.g. via a call to CompressShaAesSetKey() 
// - if you want to change the chaining mode, you can customize the global
// CompressShaAesClass variable to the expected TAES* class name
// - will store a hash of both cyphered and clear stream: if the
// data is corrupted during transmission, will instantly return ''
function CompressShaAes(var DataRawByteString; Compress: boolean): AnsiString;


{$ifdef USEPADLOCK}
var
  /// if dll/so and VIA padlock compatible CPU are present
  padlock_available: boolean = false;
{$endif}


implementation

{$ifdef USEPADLOCK}

const
  AES_SUCCEEDED = 0;
  KEY_128BITS = 0;
  KEY_192BITS = 1;
  KEY_256BITS = 2;
  ACE_AES_ECB = 0;
  ACE_AES_CBC = 1;

{$ifdef USEPADLOCKDLL}
type
  tpadlock_phe_available = function: boolean; cdecl;
  tpadlock_phe_sha = function(
    buffer: pointer; nbytes: integer; var Digest): integer; cdecl;

  tpadlock_ace_available = function: boolean; cdecl;
  tpadlock_aes_begin = function: pointer; cdecl;
  tpadlock_aes_setkey = function(
    ctx: pointer; const key; key_len: integer): integer; cdecl;
  tpadlock_aes_setmodeiv = function(
    ctx: pointer; mode: integer; var iv): integer; cdecl;
  tpadlock_aes_encrypt = function(
    ctx, bIn, bOut: pointer; nbytes: integer): integer; cdecl;
  tpadlock_aes_decrypt = function(
    ctx, bIn, bOut: pointer; nbytes: integer): integer; cdecl;
  tpadlock_aes_close = function(
    ctx: pointer): integer; cdecl;

var
  padlock_phe_available: tpadlock_phe_available = nil;
  padlock_phe_sha1: tpadlock_phe_sha = nil;
  padlock_phe_sha256: tpadlock_phe_sha = nil;

  padlock_ace_available: tpadlock_ace_available = nil;
  padlock_aes_begin: tpadlock_aes_begin = nil;
  padlock_aes_setkey: tpadlock_aes_setkey = nil;
  padlock_aes_setmodeiv: tpadlock_aes_setmodeiv = nil;
  padlock_aes_encrypt: tpadlock_aes_encrypt = nil;
  padlock_aes_decrypt: tpadlock_aes_decrypt = nil;
  padlock_aes_close: tpadlock_aes_close = nil;

{$ifdef MSWINDOWS}
  PadLockLibHandle: THandle = 0;
{$else} // Linux:
  PadLockLibHandle: HMODULE = 0;
{$endif}


procedure PadlockInit;
begin
{$ifdef MSWINDOWS}
  PadLockLibHandle := LoadLibrary('LibPadlock');
{$else} // Linux:
  PadLockLibHandle := LoadLibrary('libvia_padlock.so');
  if PadLockLibHandle=0 then
    PadLockLibHandle := LoadLibrary('libvia_padlock.so.1.0.0');
{$endif}
  if PadLockLibHandle=0 then
    exit;
  padlock_phe_available := GetProcAddress(PadLockLibHandle,'padlock_phe_available');
  padlock_phe_sha1 := GetProcAddress(PadLockLibHandle,'padlock_phe_sha1');
  padlock_phe_sha256 := GetProcAddress(PadLockLibHandle,'padlock_phe_sha256');
  padlock_ace_available := GetProcAddress(PadLockLibHandle,'padlock_ace_available');
  padlock_aes_begin := GetProcAddress(PadLockLibHandle,'padlock_aes_begin');
  padlock_aes_setkey := GetProcAddress(PadLockLibHandle,'padlock_aes_setkey');
  padlock_aes_setmodeiv := GetProcAddress(PadLockLibHandle,'padlock_aes_setmodeiv');
  padlock_aes_encrypt := GetProcAddress(PadLockLibHandle,'padlock_aes_encrypt');
  padlock_aes_decrypt := GetProcAddress(PadLockLibHandle,'padlock_aes_decrypt');
  padlock_aes_close := GetProcAddress(PadLockLibHandle,'padlock_aes_close');
  if @padlock_phe_available=nil then exit;
  if @padlock_phe_sha1=nil then exit;
  if @padlock_phe_sha256=nil then exit;
  if @padlock_ace_available=nil then exit;
  if @padlock_aes_begin=nil then exit;
  if @padlock_aes_setkey=nil then exit;
  if @padlock_aes_setmodeiv=nil then exit;
  if @padlock_aes_encrypt=nil then exit;
  if @padlock_aes_decrypt=nil then exit;
  if @padlock_aes_close=nil then exit;
  if padlock_phe_available and padlock_ace_available then
    padlock_available := true;
end;
{$else} // not USEPADLOCKDLL:

{$ifdef MSWINDOWS}
{$L padlock.obj}
{$L padlock_sha.obj}
{$L padlock_aes.obj}

function memcpy(dest, src: Pointer; count: integer): Pointer; cdecl;
begin
  Move(src^, dest^, count);
  Result := dest;
end;

function memset(dest: Pointer; val: Integer; count: integer): Pointer; cdecl;
begin
  FillChar(dest^, count, val);
  Result := dest;
end;

function malloc(size: integer): Pointer; cdecl;
begin
  GetMem(Result, size);
end;

procedure free(pBlock: Pointer); cdecl;
begin
  FreeMem(pBlock);
end;

function printf(format:PAnsiChar; args:array of const): PAnsiChar; cdecl;
begin
  result := format;
  // called on error -> do nothing
end;

{$else}
{$L padlock.o}
{$L padlock_sha.o}
{$L padlock_aes.o}
{$endif}

{ this .o files have been generated from the sdk sources with
    gcc-2.95 -c -O2 padlock*.c -I../include
}
  function padlock_phe_available: boolean;                                         cdecl; external;
  function padlock_phe_sha1(buf: pointer; nbytes: integer; var Digest): integer;   cdecl; external;
  function padlock_phe_sha256(buf: pointer; nbytes: integer; var Digest): integer; cdecl; external;

  function padlock_ace_available: boolean;                                         cdecl; external;
  function padlock_aes_begin: pointer;                                             cdecl; external;
  function padlock_aes_setkey(ctx: pointer; const key; key_len: integer): integer; cdecl; external;
  function padlock_aes_setmodeiv (ctx: pointer; mode: integer; var iv): integer;   cdecl; external;
  function padlock_aes_encrypt(ctx, bIn, bOut: pointer; nbytes: integer): integer; cdecl; external;
  function padlock_aes_decrypt(ctx, bIn, bOut: pointer; nbytes: integer): integer; cdecl; external;
  function padlock_aes_close(ctx: pointer): integer;                               cdecl; external;

procedure PadlockInit;
begin
  if padlock_phe_available and padlock_ace_available then
    padlock_available := true;
{$ifdef PADLOCKDEBUG}if padlock_available then writeln('PADLOCK available'); {$endif}
end;
{$endif USEPADLOCKDLL}
{$endif USEPADLOCK}

const
  AESMaxRounds = 14;

type
  TKeyArray   = packed array[0..AESMaxRounds] of TAESBlock;

  TAESContext = packed record
    // don't change the structure below: it is fixed in the asm code
    // -> use PUREPASCAL if you really have to change it
    RK: TKeyArray;   // Key (encr. or decr.)
    IV: TAESBlock;   // IV or CTR
    buf: TAESBlock;  // Work buffer
    {$ifdef USEPADLOCK}
    ViaCtx: pointer; // padlock_*() context
    {$endif}
    AesNi: boolean;  // if the CPU supports AES-NI new asm instructions
    Rounds: byte;    // Number of rounds
    KeyBits: byte;   // Number of bits in key
  end;


// helper types for better code generation
type
  TWA4  = packed array[0..3] of cardinal;     // AES block as array of cardinal
  TAWk  = packed array[0..4*(AESMaxRounds+1)-1] of cardinal; // Key as array of cardinal
  PWA4  = ^TWA4;
  PAWk  = ^TAWk;

const
  RCon: array[0..9] of cardinal= ($01,$02,$04,$08,$10,$20,$40,$80,$1b,$36);

// AES computed tables
var
  SBox, InvSBox: array[byte] of byte;
  Td0, Td1, Td2, Td3, Te0, Te1, Te2, Te3: array[byte] of cardinal;

procedure ComputeAesStaticTables; // will compute 4.5 KB of constant tables
var i, x,y: byte;
    pow,log: array of byte;
begin
  SetLength(pow,256);
  SetLength(log,256);
  x := 1;
  for i := 0 to 255 do begin
    pow[i] := x;
    log[x] := i;
    if x and $80<>0 then
      x := x xor (x shl 1) xor $1B else
      x := x xor (x shl 1);
  end;
  SBox[0] := $63;
  InvSBox[$63] := 0;
  for i := 1 to 255 do begin
    x := pow[255-log[i]]; y := (x shl 1)+(x shr 7);
    x := x xor y; y := (y shl 1)+(y shr 7);
    x := x xor y; y := (y shl 1)+(y shr 7);
    x := x xor y; y := (y shl 1)+(y shr 7);
    x := x xor y xor $63;
    SBox[i] := x;
    InvSBox[x] := i;
  end;
  for i := 0 to 255 do begin
    x := SBox[i];
    y := x shl 1;
    if x and $80<>0 then
      y := y xor $1B;
    Te0[i] := y+x shl 8+x shl 16+(y xor x)shl 24;
    Te1[i] := Te0[i] shl 8+Te0[i] shr 24;
    Te2[i] := Te1[i] shl 8+Te1[i] shr 24;
    Te3[i] := Te2[i] shl 8+Te2[i] shr 24;
    x := InvSBox[i];
    if x=0 then
      continue;
    x := log[x]; // Td0[x] = Si[x].[0e,09,0d,0b] -> e.g. log[$0e]=223 below
    Td0[i] := pow[(x+223)mod 255]+pow[(x+199)mod 255]shl 8+
        pow[(x+238)mod 255]shl 16+pow[(x+104)mod 255]shl 24;
    Td1[i] := Td0[i] shl 8+Td0[i] shr 24;
    Td2[i] := Td1[i] shl 8+Td1[i] shr 24;
    Td3[i] := Td2[i] shl 8+Td2[i] shr 24;
  end;
end;

type
  TSHAHash  = packed record
    A,B,C,D,E,F,G,H: cardinal; // will use A..E with TSHA1, A..H with TSHA256
  end;

  TSHAContext = packed record
    // Working hash
    Hash  : TSHAHash;
    // 64bit msg length
    MLen  : Int64;
    // Block buffer
    Buffer: array[0..63] of byte;
    // Index in buffer
    Index : integer;
  end;

{$ifdef PUREPASCAL}

procedure bswap256(s,d: PIntegerArray);
begin
  d[0] := bswap32(s[0]);
  d[1] := bswap32(s[1]);
  d[2] := bswap32(s[2]);
  d[3] := bswap32(s[3]);
  d[4] := bswap32(s[4]);
  d[5] := bswap32(s[5]);
  d[6] := bswap32(s[6]);
  d[7] := bswap32(s[7]);
end;

procedure bswap160(s,d: PIntegerArray);
begin
  d[0] := bswap32(s[0]);
  d[1] := bswap32(s[1]);
  d[2] := bswap32(s[2]);
  d[3] := bswap32(s[3]);
  d[4] := bswap32(s[4]);
end;

{$else}

procedure bswap256(s,d: PIntegerArray);
asm
  push ebx
  mov ecx,eax // ecx=s, edx=d
  mov eax,[ecx]; mov ebx,[ecx+4]; bswap eax; bswap ebx; mov [edx],eax; mov [edx+4],ebx
  mov eax,[ecx+8]; mov ebx,[ecx+12]; bswap eax; bswap ebx; mov [edx+8],eax; mov [edx+12],ebx
  mov eax,[ecx+16]; mov ebx,[ecx+20]; bswap eax; bswap ebx; mov [edx+16],eax; mov [edx+20],ebx
  mov eax,[ecx+24]; mov ebx,[ecx+28]; bswap eax; bswap ebx; mov [edx+24],eax; mov [edx+28],ebx
  pop ebx
end;

procedure bswap160(s,d: PIntegerArray);
asm
  push ebx
  mov ecx,eax // ecx=s, edx=d
  mov eax,[ecx]; mov ebx,[ecx+4]; bswap eax; bswap ebx; mov [edx],eax; mov [edx+4],ebx
  mov eax,[ecx+8]; mov ebx,[ecx+12]; bswap eax; bswap ebx; mov [edx+8],eax; mov [edx+12],ebx
  mov eax,[ecx+16]; bswap eax; mov [edx+16],eax
  pop ebx
end;

{$endif PUREPASCAL}

function SHA256SelfTest: boolean;
function SingleTest(const s: RawByteString; const TDig: TSHA256Digest): boolean;
var SHA: TSHA256;
  Digest: TSHA256Digest;
  i: integer;
begin
  // 1. Hash complete RawByteString
  SHA.Full(pointer(s),length(s),Digest);
  result := CompareMem(@Digest,@TDig,sizeof(Digest));
  if not result then exit;
  // 2. one update call for all chars
  SHA.Init;
  for i := 1 to length(s) do
    SHA.Update(@s[i],1);
  SHA.Final(Digest);
  result := CompareMem(@Digest,@TDig,sizeof(Digest));
  // 3. test consistency with Padlock engine down results
{$ifdef USEPADLOCK}
  if not result or not padlock_available then exit;
  padlock_available := false;  // force PadLock engine down
  SHA.Full(pointer(s),length(s),Digest);
  result := CompareMem(@Digest,@TDig,sizeof(Digest));
{$ifdef PADLOCKDEBUG} write('=padlock '); {$endif}
  padlock_available := true;
{$endif}
end;
var Digest: TSHA256Digest;
const
  D1: TSHA256Digest = ($ba,$78,$16,$bf,$8f,$01,$cf,$ea,$41,$41,$40,$de,$5d,$ae,$22,$23,
     $b0,$03,$61,$a3,$96,$17,$7a,$9c,$b4,$10,$ff,$61,$f2,$00,$15,$ad);
  D2: TSHA256Digest = ($24,$8d,$6a,$61,$d2,$06,$38,$b8,$e5,$c0,$26,$93,$0c,$3e,$60,$39,
     $a3,$3c,$e4,$59,$64,$ff,$21,$67,$f6,$ec,$ed,$d4,$19,$db,$06,$c1);
  D3: TSHA256Digest =
    ($94,$E4,$A9,$D9,$05,$31,$23,$1D,$BE,$D8,$7E,$D2,$E4,$F3,$5E,$4A,
     $0B,$F4,$B3,$BC,$CE,$EB,$17,$16,$D5,$77,$B1,$E0,$8B,$A9,$BA,$A3);
begin
//  result := true; exit;
  result := SingleTest('abc', D1) and
     SingleTest('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq', D2);
  if not result then exit;
  SHA256Weak('lagrangehommage',Digest); // test with len=256>64
  result := Comparemem(@Digest,@D3,sizeof(Digest));
  {$ifdef CPUX64}
  if cfSSE41 in CpuFeatures then begin
    Exclude(CpuFeatures,cfSSE41);
    result := result and SingleTest('abc', D1) and
       SingleTest('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq', D2);
    Include(CpuFeatures,cfSSE41);
  end
  {$endif}
end;

function MD5(const s: RawByteString): RawUTF8;
begin
  result := MD5DigestToString(MD5Buf(s[1],length(s)));
end;

function SHA1(const s: RawByteString): RawUTF8;
var SHA: TSHA1;
    Digest: TSHA1Digest;
begin
  SHA.Full(pointer(s),length(s),Digest);
  result := SHA1DigestToString(Digest);
end;

function SHA256(const s: RawByteString): RawUTF8;
var SHA: TSHA256;
    Digest: TSHA256Digest;
begin
  SHA.Full(pointer(s),length(s),Digest);
  result := SHA256DigestToString(Digest);
end;

function SHA256(Data: pointer; Len: integer): RawUTF8;
var SHA: TSHA256;
    Digest: TSHA256Digest;
begin
  SHA.Full(Data,Len,Digest);
  result := SHA256DigestToString(Digest);
end;

function SHA1SelfTest: boolean;
function SingleTest(const s: RawByteString; TDig: TSHA1Digest): boolean;
var SHA: TSHA1;
    Digest: TSHA1Digest;
    i: integer;
begin
  // 1. Hash complete RawByteString
  SHA.Full(pointer(s),length(s),Digest);
  result := CompareMem(@Digest,@TDig,sizeof(Digest));
  if not result then exit;
  // 2. one update call for all chars
  for i := 1 to length(s) do
    SHA.Update(@s[i],1);
  SHA.Final(Digest);
  result := CompareMem(@Digest,@TDig,sizeof(Digest));
  // 3. test consistency with Padlock engine down results
{$ifdef USEPADLOCK}
  if not result or not padlock_available then exit;
  padlock_available := false;  // force PadLock engine down
  SHA.Full(pointer(s),length(s),Digest);
  result := CompareMem(@Digest,@TDig,sizeof(Digest));
{$ifdef PADLOCKDEBUG} write('=padlock '); {$endif}
  padlock_available := true;
{$endif}
end;
const
  Test1Out: TSHA1Digest=
    ($A9,$99,$3E,$36,$47,$06,$81,$6A,$BA,$3E,$25,$71,$78,$50,$C2,$6C,$9C,$D0,$D8,$9D);
  Test2Out: TSHA1Digest=
    ($84,$98,$3E,$44,$1C,$3B,$D2,$6E,$BA,$AE,$4A,$A1,$F9,$51,$29,$E5,$E5,$46,$70,$F1);
var
  s: RawByteString;
  SHA: TSHA1;
  Digest: TSHA1Digest;
begin
  result := SingleTest('abc',Test1Out) and
    SingleTest('abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq',Test2Out);
  if not result then exit;
  s := 'Wikipedia, l''encyclopedie libre et gratuite';
  SHA.Full(pointer(s),length(s),Digest);
  result := SHA1DigestToString(Digest)='c18cc65028bbdc147288a2d136313287782b9c73';
end;


{ TAES }

{$define AES_ROLLED}
// if defined, use rolled version, which is faster (at least on my AMD CPU)

{$ifdef DELPHI5OROLDER}
  {$define PURE_PASCAL} // Delphi 5 internal asm is buggy :(
{$else}
  {$ifdef PUREPASCAL} // e.g. for x64 (CPU64) or ARM
    {$define PURE_PASCAL} // AES128 unrolled pascal(Delphi7)=57MB/s rolled asm=84MB/s :)
  {$else}
    {$define USEAESNI}
    {$ifdef ISDELPHIXE4}
      {$define HASAESNI}
    {$endif}
    {$ifdef FPC}
      {$define HASAESNI}
    {$endif}
  {$endif PUREPASCAL}
{$endif}

function AESSelfTest(onlytables: Boolean): boolean;
var A: TAES;
    st: RawByteString;
    Key: TSHA256Digest;
    s,b,p: TAESBlock;
    i,k,ks: integer;
begin
  // ensure that we have $2000 bytes of contiguous XOR tables ;)
  result := (PtrUInt(@TD0)+$400=PtrUInt(@TD1))and(PtrUInt(@TD0)+$800=PtrUInt(@TD2))
    and(PtrUInt(@TD0)+$C00=PtrUInt(@TD3))and(PtrUInt(@TD0)+$1000=PtrUInt(@TE0))
    and(PtrUInt(@TD0)+$1400=PtrUInt(@TE1))and(PtrUInt(@TD0)+$1800=PtrUInt(@TE2))
    and(PtrUInt(@TD0)+$1C00=PtrUInt(@TE3))and
    (SBox[255]=$16)and(InvSBox[0]=$52)and
    (Te0[0]=$a56363c6)and(Te0[255]=$3a16162c)and
    (Te1[0]=$6363c6a5)and(Te1[255]=$16162c3a)and
    (Te3[0]=$c6a56363)and(Te3[255]=$2c3a1616)and
    (Td0[0]=$50a7f451)and(Td0[99]=0)and(Td0[255]=$4257b8d0)and
    (Td3[0]=$5150a7f4)and(Td3[255]=$d04257b8);
  if onlytables or not result then
    exit;
  // test
  result := false;
  Randomize;
  st := '1234essai';
  PInteger(@st[1])^ := Random(MaxInt);
  for k := 0 to 2 do begin
    ks := 128+k*64; // test keysize of 128,192 and 256 bits
//    write('Test AES ',ks);
    for i := 1 to 100 do begin
      SHA256Weak(st,Key);
      move(Key,s,16);
      A.EncryptInit(Key,ks);
      A.Encrypt(s,b);
      A.Done;
      A.DecryptInit(Key,ks);
      A.Decrypt(b,p);
      A.Done;
      if not CompareMem(@p,@s,AESBLockSize) then begin
        writeln('AESSelfTest compareError with keysize=',ks);
        exit;
      end;
      st := st+AnsiChar(Random(255));
    end;
  end;
  result := true;
end;

procedure TAES.Encrypt(var B: TAESBlock);
begin
  Encrypt(B,B);
end;

{$ifdef USEAESNI}
procedure AesNiEncryptXmm7;
asm // input: eax=TAESContext, xmm7=data; output: xmm7=data
  mov dl,[eax].TAESContext.Rounds
  movdqu xmm0,[eax+16*0]
  movdqu xmm1,[eax+16*1]
  movdqu xmm2,[eax+16*2]
  movdqu xmm3,[eax+16*3]
  movdqu xmm4,[eax+16*4]
  movdqu xmm5,[eax+16*5]
  movdqu xmm6,[eax+16*6]
  pxor xmm7,xmm0
  cmp dl,10
  {$ifdef HASAESNI}
  aesenc xmm7,xmm1
  aesenc xmm7,xmm2
  aesenc xmm7,xmm3
  aesenc xmm7,xmm4
  {$else}
  db $66,$0F,$38,$DC,$F9
  db $66,$0F,$38,$DC,$FA
  db $66,$0F,$38,$DC,$FB
  db $66,$0F,$38,$DC,$FC
  {$endif}
  movdqu xmm0,[eax+16*7]
  movdqu xmm1,[eax+16*8]
  movdqu xmm2,[eax+16*9]
  movdqu xmm3,[eax+16*10]
  je @128
  cmp dl,12
  {$ifdef HASAESNI}
  aesenc xmm7,xmm5
  aesenc xmm7,xmm6
  {$else}
  db $66,$0F,$38,$DC,$FD
  db $66,$0F,$38,$DC,$FE
  {$endif}
  movdqu xmm4,[eax+16*11]
  movdqu xmm5,[eax+16*12]
  je @192
@256:
  movdqu xmm6,[eax+16*13]
  {$ifdef HASAESNI}
  aesenc xmm7,xmm0
  aesenc xmm7,xmm1
  {$else}
  db $66,$0F,$38,$DC,$F8
  db $66,$0F,$38,$DC,$F9
  {$endif}
  movdqu xmm1,[eax+16*14]
  {$ifdef HASAESNI}
  aesenc xmm7,xmm2
  aesenc xmm7,xmm3
  aesenc xmm7,xmm4
  aesenc xmm7,xmm5
  aesenc xmm7,xmm6
  aesenclast xmm7,xmm1
  {$else}
  db $66,$0F,$38,$DC,$FA
  db $66,$0F,$38,$DC,$FB
  db $66,$0F,$38,$DC,$FC
  db $66,$0F,$38,$DC,$FD
  db $66,$0F,$38,$DC,$FE
  db $66,$0F,$38,$DD,$F9
  {$endif}
  ret
@128:
  {$ifdef HASAESNI}
  aesenc xmm7,xmm5
  aesenc xmm7,xmm6
  aesenc xmm7,xmm0
  aesenc xmm7,xmm1
  aesenc xmm7,xmm2
  aesenclast xmm7,xmm3
  {$else}
  db $66,$0F,$38,$DC,$FD
  db $66,$0F,$38,$DC,$FE
  db $66,$0F,$38,$DC,$F8
  db $66,$0F,$38,$DC,$F9
  db $66,$0F,$38,$DC,$FA
  db $66,$0F,$38,$DD,$FB
  {$endif}
  ret
@192:
  {$ifdef HASAESNI}
  aesenc xmm7,xmm0
  aesenc xmm7,xmm1
  aesenc xmm7,xmm2
  aesenc xmm7,xmm3
  aesenc xmm7,xmm4
  aesenclast xmm7,xmm5
  {$else}
  db $66,$0F,$38,$DC,$F8
  db $66,$0F,$38,$DC,$F9
  db $66,$0F,$38,$DC,$FA
  db $66,$0F,$38,$DC,$FB
  db $66,$0F,$38,$DC,$FC
  db $66,$0F,$38,$DD,$FD
  {$endif}
end;
{$endif}

procedure TAES.Encrypt(const BI: TAESBlock; var BO: TAESBlock);
// encrypt one block: Context contains encryption key
{$ifdef PURE_PASCAL}
{ PURE_PASCAL version (c) Wolfgang Ehrhardt under zlib license:
 Permission is granted to anyone to use this software for any purpose,
 including commercial applications, and to alter it and redistribute it
 freely, subject to the following restrictions:
 1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software in
    a product, an acknowledgment in the product documentation would be
    appreciated but is not required.
 2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.
 3. This notice may not be removed or altered from any source distribution. }
var
  ctx: TAESContext absolute Context;
  s0,s1,s2,s3: cardinal; // TAESBlock s as separate variables
  t0,t1,t2: cardinal;    // TAESBlock t as separate variables
{$ifdef AES_ROLLED}
  i: integer;
  pK: PWA4;
{$else}
  t3: cardinal;
  pK: PAWk;              // pointer to loop rount key
{$endif}
begin
{$ifdef USEPADLOCK}
  if ctx.ViaCtx<>nil then begin
    padlock_aes_encrypt(ctx.ViaCtx,@BI,@BO,16);
    exit;
  end;
{$endif}
  // Setup key pointer
  pK := PWA4(@ctx.RK);
  // Initialize with input block
  s0 := TWA4(BI)[0] xor pK^[0];
  s1 := TWA4(BI)[1] xor pK^[1];
  s2 := TWA4(BI)[2] xor pK^[2];
  s3 := TWA4(BI)[3] xor pK^[3];
{$ifdef AES_ROLLED}
  // Wolfgang Ehrhardt rolled version - faster on modern CPU than unrolled one below
  Inc(PK);
  for I := 1 to ctx.Rounds-1 do begin
    t0 := Te0[s0 and $ff] xor Te1[s1 shr 8 and $ff] xor Te2[s2 shr 16 and $ff] xor Te3[s3 shr 24];
    t1 := Te0[s1 and $ff] xor Te1[s2 shr 8 and $ff] xor Te2[s3 shr 16 and $ff] xor Te3[s0 shr 24];
    t2 := Te0[s2 and $ff] xor Te1[s3 shr 8 and $ff] xor Te2[s0 shr 16 and $ff] xor Te3[s1 shr 24];
    s3 := Te0[s3 and $ff] xor Te1[s0 shr 8 and $ff] xor Te2[s1 shr 16 and $ff] xor Te3[s2 shr 24] xor PK[3];
    s0 := t0 xor PK[0];
    s1 := t1 xor PK[1];
    s2 := t2 xor PK[2];
    Inc(pK);
  end;
  TWA4(BO)[0] := ((SBox[s0        and $ff])        xor
                  (SBox[s1 shr  8 and $ff]) shl  8 xor
                  (SBox[s2 shr 16 and $ff]) shl 16 xor
                  (SBox[s3 shr 24])         shl 24    ) xor pK^[0];
  TWA4(BO)[1] := ((SBox[s1        and $ff])        xor
                  (SBox[s2 shr  8 and $ff]) shl  8 xor
                  (SBox[s3 shr 16 and $ff]) shl 16 xor
                  (SBox[s0 shr 24])         shl 24    ) xor pK^[1];
  TWA4(BO)[2] := ((SBox[s2        and $ff])        xor
                  (SBox[s3 shr  8 and $ff]) shl  8 xor
                  (SBox[s0 shr 16 and $ff]) shl 16 xor
                  (SBox[s1 shr 24])         shl 24    ) xor pK^[2];
  TWA4(BO)[3] := ((SBox[s3        and $ff])        xor
                  (SBox[s0 shr  8 and $ff]) shl  8 xor
                  (SBox[s1 shr 16 and $ff]) shl 16 xor
                  (SBox[s2 shr 24])         shl 24    ) xor pK^[3];
{$else} // unrolled version (WE6) from Wolfgang Ehrhardt - slower
  // Round 1
  t0 := Te0[s0 and $ff] xor Te1[s1 shr 8 and $ff] xor Te2[s2 shr 16 and $ff] xor Te3[s3 shr 24] xor pK^[4];
  t1 := Te0[s1 and $ff] xor Te1[s2 shr 8 and $ff] xor Te2[s3 shr 16 and $ff] xor Te3[s0 shr 24] xor pK^[5];
  t2 := Te0[s2 and $ff] xor Te1[s3 shr 8 and $ff] xor Te2[s0 shr 16 and $ff] xor Te3[s1 shr 24] xor pK^[6];
  t3 := Te0[s3 and $ff] xor Te1[s0 shr 8 and $ff] xor Te2[s1 shr 16 and $ff] xor Te3[s2 shr 24] xor pK^[7];
  // Round 2
  s0 := Te0[t0 and $ff] xor Te1[t1 shr 8 and $ff] xor Te2[t2 shr 16 and $ff] xor Te3[t3 shr 24] xor pK^[8];
  s1 := Te0[t1 and $ff] xor Te1[t2 shr 8 and $ff] xor Te2[t3 shr 16 and $ff] xor Te3[t0 shr 24] xor pK^[9];
  s2 := Te0[t2 and $ff] xor Te1[t3 shr 8 and $ff] xor Te2[t0 shr 16 and $ff] xor Te3[t1 shr 24] xor pK^[10];
  s3 := Te0[t3 and $ff] xor Te1[t0 shr 8 and $ff] xor Te2[t1 shr 16 and $ff] xor Te3[t2 shr 24] xor pK^[11];
  // Round 3
  t0 := Te0[s0 and $ff] xor Te1[s1 shr 8 and $ff] xor Te2[s2 shr 16 and $ff] xor Te3[s3 shr 24] xor pK^[12];
  t1 := Te0[s1 and $ff] xor Te1[s2 shr 8 and $ff] xor Te2[s3 shr 16 and $ff] xor Te3[s0 shr 24] xor pK^[13];
  t2 := Te0[s2 and $ff] xor Te1[s3 shr 8 and $ff] xor Te2[s0 shr 16 and $ff] xor Te3[s1 shr 24] xor pK^[14];
  t3 := Te0[s3 and $ff] xor Te1[s0 shr 8 and $ff] xor Te2[s1 shr 16 and $ff] xor Te3[s2 shr 24] xor pK^[15];
  // Round 4
  s0 := Te0[t0 and $ff] xor Te1[t1 shr 8 and $ff] xor Te2[t2 shr 16 and $ff] xor Te3[t3 shr 24] xor pK^[16];
  s1 := Te0[t1 and $ff] xor Te1[t2 shr 8 and $ff] xor Te2[t3 shr 16 and $ff] xor Te3[t0 shr 24] xor pK^[17];
  s2 := Te0[t2 and $ff] xor Te1[t3 shr 8 and $ff] xor Te2[t0 shr 16 and $ff] xor Te3[t1 shr 24] xor pK^[18];
  s3 := Te0[t3 and $ff] xor Te1[t0 shr 8 and $ff] xor Te2[t1 shr 16 and $ff] xor Te3[t2 shr 24] xor pK^[19];
  // Round 5
  t0 := Te0[s0 and $ff] xor Te1[s1 shr 8 and $ff] xor Te2[s2 shr 16 and $ff] xor Te3[s3 shr 24] xor pK^[20];
  t1 := Te0[s1 and $ff] xor Te1[s2 shr 8 and $ff] xor Te2[s3 shr 16 and $ff] xor Te3[s0 shr 24] xor pK^[21];
  t2 := Te0[s2 and $ff] xor Te1[s3 shr 8 and $ff] xor Te2[s0 shr 16 and $ff] xor Te3[s1 shr 24] xor pK^[22];
  t3 := Te0[s3 and $ff] xor Te1[s0 shr 8 and $ff] xor Te2[s1 shr 16 and $ff] xor Te3[s2 shr 24] xor pK^[23];
  // Round 6
  s0 := Te0[t0 and $ff] xor Te1[t1 shr 8 and $ff] xor Te2[t2 shr 16 and $ff] xor Te3[t3 shr 24] xor pK^[24];
  s1 := Te0[t1 and $ff] xor Te1[t2 shr 8 and $ff] xor Te2[t3 shr 16 and $ff] xor Te3[t0 shr 24] xor pK^[25];
  s2 := Te0[t2 and $ff] xor Te1[t3 shr 8 and $ff] xor Te2[t0 shr 16 and $ff] xor Te3[t1 shr 24] xor pK^[26];
  s3 := Te0[t3 and $ff] xor Te1[t0 shr 8 and $ff] xor Te2[t1 shr 16 and $ff] xor Te3[t2 shr 24] xor pK^[27];
  // Round 7
  t0 := Te0[s0 and $ff] xor Te1[s1 shr 8 and $ff] xor Te2[s2 shr 16 and $ff] xor Te3[s3 shr 24] xor pK^[28];
  t1 := Te0[s1 and $ff] xor Te1[s2 shr 8 and $ff] xor Te2[s3 shr 16 and $ff] xor Te3[s0 shr 24] xor pK^[29];
  t2 := Te0[s2 and $ff] xor Te1[s3 shr 8 and $ff] xor Te2[s0 shr 16 and $ff] xor Te3[s1 shr 24] xor pK^[30];
  t3 := Te0[s3 and $ff] xor Te1[s0 shr 8 and $ff] xor Te2[s1 shr 16 and $ff] xor Te3[s2 shr 24] xor pK^[31];
  // Round 8
  s0 := Te0[t0 and $ff] xor Te1[t1 shr 8 and $ff] xor Te2[t2 shr 16 and $ff] xor Te3[t3 shr 24] xor pK^[32];
  s1 := Te0[t1 and $ff] xor Te1[t2 shr 8 and $ff] xor Te2[t3 shr 16 and $ff] xor Te3[t0 shr 24] xor pK^[33];
  s2 := Te0[t2 and $ff] xor Te1[t3 shr 8 and $ff] xor Te2[t0 shr 16 and $ff] xor Te3[t1 shr 24] xor pK^[34];
  s3 := Te0[t3 and $ff] xor Te1[t0 shr 8 and $ff] xor Te2[t1 shr 16 and $ff] xor Te3[t2 shr 24] xor pK^[35];
  // Round 9
  t0 := Te0[s0 and $ff] xor Te1[s1 shr 8 and $ff] xor Te2[s2 shr 16 and $ff] xor Te3[s3 shr 24] xor pK^[36];
  t1 := Te0[s1 and $ff] xor Te1[s2 shr 8 and $ff] xor Te2[s3 shr 16 and $ff] xor Te3[s0 shr 24] xor pK^[37];
  t2 := Te0[s2 and $ff] xor Te1[s3 shr 8 and $ff] xor Te2[s0 shr 16 and $ff] xor Te3[s1 shr 24] xor pK^[38];
  t3 := Te0[s3 and $ff] xor Te1[s0 shr 8 and $ff] xor Te2[s1 shr 16 and $ff] xor Te3[s2 shr 24] xor pK^[39];
  if ctx.rounds>10 then begin
    // Round 10
    s0 := Te0[t0 and $ff] xor Te1[t1 shr 8 and $ff] xor Te2[t2 shr 16 and $ff] xor Te3[t3 shr 24] xor pK^[40];
    s1 := Te0[t1 and $ff] xor Te1[t2 shr 8 and $ff] xor Te2[t3 shr 16 and $ff] xor Te3[t0 shr 24] xor pK^[41];
    s2 := Te0[t2 and $ff] xor Te1[t3 shr 8 and $ff] xor Te2[t0 shr 16 and $ff] xor Te3[t1 shr 24] xor pK^[42];
    s3 := Te0[t3 and $ff] xor Te1[t0 shr 8 and $ff] xor Te2[t1 shr 16 and $ff] xor Te3[t2 shr 24] xor pK^[43];
    // Round 11
    t0 := Te0[s0 and $ff] xor Te1[s1 shr 8 and $ff] xor Te2[s2 shr 16 and $ff] xor Te3[s3 shr 24] xor pK^[44];
    t1 := Te0[s1 and $ff] xor Te1[s2 shr 8 and $ff] xor Te2[s3 shr 16 and $ff] xor Te3[s0 shr 24] xor pK^[45];
    t2 := Te0[s2 and $ff] xor Te1[s3 shr 8 and $ff] xor Te2[s0 shr 16 and $ff] xor Te3[s1 shr 24] xor pK^[46];
    t3 := Te0[s3 and $ff] xor Te1[s0 shr 8 and $ff] xor Te2[s1 shr 16 and $ff] xor Te3[s2 shr 24] xor pK^[47];
    if ctx.rounds>12 then begin
      // Round 12
      s0 := Te0[t0 and $ff] xor Te1[t1 shr 8 and $ff] xor Te2[t2 shr 16 and $ff] xor Te3[t3 shr 24] xor pK^[48];
      s1 := Te0[t1 and $ff] xor Te1[t2 shr 8 and $ff] xor Te2[t3 shr 16 and $ff] xor Te3[t0 shr 24] xor pK^[49];
      s2 := Te0[t2 and $ff] xor Te1[t3 shr 8 and $ff] xor Te2[t0 shr 16 and $ff] xor Te3[t1 shr 24] xor pK^[50];
      s3 := Te0[t3 and $ff] xor Te1[t0 shr 8 and $ff] xor Te2[t1 shr 16 and $ff] xor Te3[t2 shr 24] xor pK^[51];
      // Round 13
      t0 := Te0[s0 and $ff] xor Te1[s1 shr 8 and $ff] xor Te2[s2 shr 16 and $ff] xor Te3[s3 shr 24] xor pK^[52];
      t1 := Te0[s1 and $ff] xor Te1[s2 shr 8 and $ff] xor Te2[s3 shr 16 and $ff] xor Te3[s0 shr 24] xor pK^[53];
      t2 := Te0[s2 and $ff] xor Te1[s3 shr 8 and $ff] xor Te2[s0 shr 16 and $ff] xor Te3[s1 shr 24] xor pK^[54];
      t3 := Te0[s3 and $ff] xor Te1[s0 shr 8 and $ff] xor Te2[s1 shr 16 and $ff] xor Te3[s2 shr 24] xor pK^[55];
    end;
  end;
  inc(PtrUInt(pK), (ctx.rounds shl 4));

  TWA4(BO)[0] := ((SBox[t0        and $ff])        xor
                  (SBox[t1 shr  8 and $ff]) shl  8 xor
                  (SBox[t2 shr 16 and $ff]) shl 16 xor
                  (SBox[t3 shr 24])         shl 24    ) xor pK^[0];
  TWA4(BO)[1] := ((SBox[t1        and $ff])        xor
                  (SBox[t2 shr  8 and $ff]) shl  8 xor
                  (SBox[t3 shr 16 and $ff]) shl 16 xor
                  (SBox[t0 shr 24])         shl 24    ) xor pK^[1];
  TWA4(BO)[2] := ((SBox[t2        and $ff])        xor
                  (SBox[t3 shr  8 and $ff]) shl  8 xor
                  (SBox[t0 shr 16 and $ff]) shl 16 xor
                  (SBox[t1 shr 24])         shl 24    ) xor pK^[2];
  TWA4(BO)[3] := ((SBox[t3        and $ff])        xor
                  (SBox[t0 shr  8 and $ff]) shl  8 xor
                  (SBox[t1 shr 16 and $ff]) shl 16 xor
                  (SBox[t2 shr 24])         shl 24    ) xor pK^[3];
{$endif}
end;
{$else}
asm // eax=TAES(self)=TAESContext edx=BI ecx=BO
{$ifdef USEAESNI}
  // AES-NI hardware accelerated version by A. Bouchez
  cmp byte ptr [eax].TAESContext.AesNi,0
  je @noAesNi
  movdqu xmm7,[edx]
  call AesNiEncryptXmm7
  movdqu [ecx],xmm7
  ret
@noAesNi:
{$endif USEAESNI}
  // rolled optimized encryption asm version by A. Bouchez
{$ifdef USEPADLOCK}
  cmp dword [eax].TAESContext.ViaCtx,0
  jz @DoAsm
  mov eax,[eax].TAESContext.ViaCtx
  push 16
  push ecx
  push edx
  push eax           // padlock_aes_encrypt(ctx.ViaCtx,@BI,@BO,16);
{$ifdef USEPADLOCKDLL}
  call dword ptr [padlock_aes_encrypt] {$else}
  call padlock_aes_encrypt
{$endif}
  add esp,16 // padlock_aes_encrypt is cdecl -> caller must restore stack
  ret
@DoAsm: {$endif}
  push ebx
  push esi
  push edi
  push ebp
  add esp,-24
  mov [esp+4],ecx
  mov ecx,eax // ecx=pk
  movzx eax,byte ptr [eax].TAESContext.Rounds
  dec eax
  mov [esp+20],eax
  mov ebx,[edx]
  xor ebx,[ecx]
  mov esi,[edx+4]
  xor esi,[ecx+4]
  mov eax,[edx+8]
  xor eax,[ecx+8]
  mov edx,[edx+12]
  xor edx,[ecx+12]
  lea ecx,[ecx+16]
@1: // pK=ecx s0=ebx s1=esi s2=eax s3=edx
  movzx edi,bl
  mov edi,[4*edi+te0]
  movzx ebp,si
  shr ebp,$08
  xor edi,[4*ebp+te1]
  mov ebp,eax
  shr ebp,$10
  and ebp,$ff
  xor edi,[4*ebp+te2]
  mov ebp,edx
  shr ebp,$18
  xor edi,[4*ebp+te3]
  mov [esp+8],edi
  mov edi,esi
  and edi,255
  mov edi,[4*edi+te0]
  movzx ebp,ax
  shr ebp,$08
  xor edi,[4*ebp+te1]
  mov ebp,edx
  shr ebp,$10
  and ebp,255
  xor edi,[4*ebp+te2]
  mov ebp,ebx
  shr ebp,$18
  xor edi,[4*ebp+te3]
  mov [esp+12],edi
  movzx edi,al
  mov edi,[4*edi+te0]
  movzx ebp,dh
  xor edi,[4*ebp+te1]
  mov ebp,ebx
  shr ebp,$10
  and ebp,255
  xor edi,[4*ebp+te2]
  mov ebp,esi
  shr ebp,$18
  xor edi,[4*ebp+te3]
  mov [esp+16],edi
  and edx,255
  mov edx,[4*edx+te0]
  shr ebx,$08
  and ebx,255
  xor edx,[4*ebx+te1]
  shr esi,$10
  and esi,255
  xor edx,[4*esi+te2]
  shr eax,$18
  xor edx,[4*eax+te3]
  mov ebx,[ecx]
  xor ebx,[esp+8]
  mov esi,[ecx+4]
  xor esi,[esp+12]
  mov eax,[ecx+8]
  xor eax,[esp+16]
  xor edx,[ecx+12]
  lea ecx,[ecx+16]
  dec byte ptr [esp+20]
  jne @1

  mov ebp,ecx // ebp=pk
  movzx ecx,bl
  mov edi,esi
  movzx ecx,byte ptr [ecx+SBox]
  shr edi,$08
  and edi,255
  movzx edi,byte ptr [edi+SBox]
  shl edi,$08
  xor ecx,edi
  mov edi,eax
  shr edi,$10
  and edi,255
  movzx edi,byte ptr [edi+SBox]
  shl edi,$10
  xor ecx,edi
  mov edi,edx
  shr edi,$18
  movzx edi,byte ptr [edi+SBox]
  shl edi,$18
  xor ecx,edi
  xor ecx,[ebp]
  mov edi,[esp+4]
  mov [edi],ecx
  mov ecx,esi
  and ecx,255
  movzx ecx,byte ptr [ecx+SBox]
  movzx edi,ah
  movzx edi,byte ptr [edi+SBox]
  shl edi,$08
  xor ecx,edi
  mov edi,edx
  shr edi,$10
  and edi,255
  movzx edi,byte ptr [edi+SBox]
  shl edi,$10
  xor ecx,edi
  mov edi,ebx
  shr edi,$18
  movzx edi,byte ptr [edi+SBox]
  shl edi,$18
  xor ecx,edi
  xor ecx,[ebp+4]
  mov edi,[esp+4]
  mov [edi+4],ecx
  mov ecx,eax
  and ecx,255
  movzx ecx,byte ptr [ecx+SBox]
  movzx edi,dh
  movzx edi,byte ptr [edi+SBox]
  shl edi,$08
  xor ecx,edi
  mov edi,ebx
  shr edi,$10
  and edi,255
  movzx edi,byte ptr [edi+SBox]
  shl edi,$10
  xor ecx,edi
  mov edi,esi
  shr edi,$18
  movzx edi,byte ptr [edi+SBox]
  shl edi,$18
  xor ecx,edi
  xor ecx,[ebp+8]
  mov edi,[esp+4]
  mov [edi+8],ecx
  and edx,255
  movzx edx,byte ptr [edx+SBox]
  shr ebx,$08
  and ebx,255
  xor ecx,ecx
  mov cl,byte ptr [ebx+SBox]
  shl ecx,$08
  xor edx,ecx
  shr esi,$10
  and esi,255
  xor ecx,ecx
  mov cl,byte ptr [esi+SBox]
  shl ecx,$10
  xor edx,ecx
  shr eax,$18
  movzx eax,byte ptr [eax+SBox]
  shl eax,$18
  xor edx,eax
  xor edx,[ebp+12]
  mov eax,[esp+4]
  mov [eax+12],edx
  add esp,24
  pop ebp
  pop edi
  pop esi
  pop ebx
end;
{$endif}

{$ifdef USEPADLOCK}
function TAES.DoPadlockInit(const Key; KeySize: cardinal): boolean;
var ctx: TAESContext absolute Context;
  KL: integer;
begin
  result := false;
  ctx.ViaCtx := nil; // object has not set it to nil
  if not padlock_available then // dll/so and CPU are present ?
    exit;
  case KeySize of
    128: KL := KEY_128BITS;
    192: KL := KEY_192BITS;
    256: KL := KEY_256BITS;
    else exit;
  end;
  ctx.ViaCtx := padlock_aes_begin;
  if ctx.ViaCtx<>nil then
  if (padlock_aes_setkey(ctx.ViaCtx,Key,KL)=AES_SUCCEEDED) and
     (padlock_aes_setmodeiv(ctx.ViaCtx,ACE_AES_ECB,ctx.IV)=AES_SUCCEEDED) then
    result := true else begin
    padlock_aes_close(ctx.ViaCtx);
    ctx.ViaCtx := nil;
  end;
end;
{$endif}

{$ifdef USEAESNI} // should be put outside the main method for FPC :(
procedure ShiftAesNi(KeySize: cardinal; pk: pointer);
asm // eax=KeySize edx=pk
  movdqu xmm1,[edx]
  movdqu xmm5,dqword ptr [@shuffle_mask]
  cmp al,128
  je @128
  cmp al,192
  je @end // 192 bits is very complicated -> skip by now (we mostly use 128+256)
@256:
  movdqu xmm3,[edx+16]
  add edx,32
  db $66,$0F,$3A,$DF,$D3,$01 // aeskeygenassist xmm2,xmm3,1
  call @key_expansion256
  db $66,$0F,$3A,$DF,$D3,$02 // aeskeygenassist xmm2,xmm3,2
  call @key_expansion256
  db $66,$0F,$3A,$DF,$D3,$04 // aeskeygenassist xmm2,xmm3,4
  call @key_expansion256
  db $66,$0F,$3A,$DF,$D3,$08 // aeskeygenassist xmm2,xmm3,8
  call @key_expansion256
  db $66,$0F,$3A,$DF,$D3,$10 // aeskeygenassist xmm2,xmm3,$10
  call @key_expansion256
  db $66,$0F,$3A,$DF,$D3,$20 // aeskeygenassist xmm2,xmm3,$20
  call @key_expansion256
  db $66,$0F,$3A,$DF,$D3,$40 // aeskeygenassist xmm2,xmm3,$40
  pshufd xmm2,xmm2,$FF
  movdqu xmm4,xmm1
  db $66,$0F,$38,$00,$E5 // pshufb xmm4,xmm5
  pxor xmm1,xmm4
  db $66,$0F,$38,$00,$E5 // pshufb xmm4,xmm5
  pxor xmm1,xmm4
  db $66,$0F,$38,$00,$E5 // pshufb xmm4,xmm5
  pxor xmm1,xmm4
  pxor xmm1,xmm2
  movdqu [edx],xmm1
  jmp @end
@shuffle_mask:
  dd $ffffffff
  dd $03020100
  dd $07060504
  dd $0b0a0908
@key_expansion256:
  pshufd xmm2,xmm2,$ff
  movdqu xmm4,xmm1
  db $66,$0F,$38,$00,$E5 // pshufb xmm4,xmm5
  pxor xmm1,xmm4
  db $66,$0F,$38,$00,$E5 // pshufb xmm4,xmm5
  pxor xmm1,xmm4
  db $66,$0F,$38,$00,$E5 // pshufb xmm4,xmm5
  pxor xmm1,xmm4
  pxor xmm1,xmm2
  movdqu [edx],xmm1
  add edx,$10
  db $66,$0F,$3A,$DF,$E1,$00 // aeskeygenassist xmm4,xmm1,0
  pshufd xmm2,xmm4,$AA
  movdqu xmm4,xmm3
  db $66,$0F,$38,$00,$E5 // pshufb xmm4,xmm5
  pxor xmm3,xmm4
  db $66,$0F,$38,$00,$E5 // pshufb xmm4,xmm5
  pxor xmm3,xmm4
  db $66,$0F,$38,$00,$E5 // pshufb xmm4,xmm5
  pxor xmm3,xmm4
  pxor xmm3,xmm2
  movdqu [edx],xmm3
  add edx,$10
  ret
@key_expansion128:
  pshufd xmm2,xmm2,$FF
  movdqu xmm3,xmm1
  db $66,$0F,$38,$00,$DD // pshufb xmm3,xmm5
  pxor xmm1,xmm3
  db $66,$0F,$38,$00,$DD // pshufb xmm3,xmm5
  pxor xmm1,xmm3
  db $66,$0F,$38,$00,$DD // pshufb xmm3,xmm5
  pxor xmm1,xmm3
  pxor xmm1,xmm2
  movdqu [edx],xmm1
  add edx,$10
  ret
@128:
  add edx,16
  db $66,$0F,$3A,$DF,$D1,$01 // aeskeygenassist xmm2,xmm1,1
  call @key_expansion128
  db $66,$0F,$3A,$DF,$D1,$02 // aeskeygenassist xmm2,xmm1,2
  call @key_expansion128
  db $66,$0F,$3A,$DF,$D1,$04 // aeskeygenassist xmm2,xmm1,4
  call @key_expansion128
  db $66,$0F,$3A,$DF,$D1,$08 // aeskeygenassist xmm2,xmm1,8
  call @key_expansion128
  db $66,$0F,$3A,$DF,$D1,$10 // aeskeygenassist xmm2,xmm1,$10
  call @key_expansion128
  db $66,$0F,$3A,$DF,$D1,$20 // aeskeygenassist xmm2,xmm1,$20
  call @key_expansion128
  db $66,$0F,$3A,$DF,$D1,$40 // aeskeygenassist xmm2,xmm1,$40
  call @key_expansion128
  db $66,$0F,$3A,$DF,$D1,$80 // aeskeygenassist xmm2,xmm1,$80
  call @key_expansion128
  db $66,$0F,$3A,$DF,$D1,$1b // aeskeygenassist xmm2,xmm1,$1b
  call @key_expansion128
  db $66,$0F,$3A,$DF,$D1,$36 // aeskeygenassist xmm2,xmm1,$36
  call @key_expansion128
@end:
end;
{$endif USEAESNI}

function TAES.EncryptInit(const Key; KeySize: cardinal): boolean;
procedure Shift(KeySize: cardinal; pk: PAWK);
var i: integer;
    temp: cardinal;
begin
  // 32 bit use shift and mask
  case KeySize of
  128:
    for i := 0 to 9 do begin
      temp := pK^[3];
      // SubWord(RotWord(temp)) if "word" count mod 4 = 0
      pK^[4] := ((SBox[(temp shr  8) and $ff])       ) xor
                ((SBox[(temp shr 16) and $ff]) shl  8) xor
                ((SBox[(temp shr 24)        ]) shl 16) xor
                ((SBox[(temp       ) and $ff]) shl 24) xor
                pK^[0] xor RCon[i];
      pK^[5] := pK^[1] xor pK^[4];
      pK^[6] := pK^[2] xor pK^[5];
      pK^[7] := pK^[3] xor pK^[6];
      inc(PtrUInt(pK),4*4);
    end;
  192:
    for i := 0 to 7 do begin
      temp := pK^[5];
      // SubWord(RotWord(temp)) if "word" count mod 6 = 0
      pK^[ 6] := ((SBox[(temp shr  8) and $ff])       ) xor
                 ((SBox[(temp shr 16) and $ff]) shl  8) xor
                 ((SBox[(temp shr 24)        ]) shl 16) xor
                 ((SBox[(temp       ) and $ff]) shl 24) xor
                 pK^[0] xor RCon[i];
      pK^[ 7] := pK^[1] xor pK^[6];
      pK^[ 8] := pK^[2] xor pK^[7];
      pK^[ 9] := pK^[3] xor pK^[8];
      if i=7 then exit;
      pK^[10] := pK^[4] xor pK^[ 9];
      pK^[11] := pK^[5] xor pK^[10];
      inc(PtrUInt(pK),6*4);
    end;
  else // 256:
    for i := 0 to 6 do begin
      temp := pK^[7];
      // SubWord(RotWord(temp)) if "word" count mod 8 = 0
      pK^[ 8] := ((SBox[(temp shr  8) and $ff])       ) xor
                 ((SBox[(temp shr 16) and $ff]) shl  8) xor
                 ((SBox[(temp shr 24)        ]) shl 16) xor
                 ((SBox[(temp       ) and $ff]) shl 24) xor
                 pK^[0] xor RCon[i];
      pK^[ 9] := pK^[1] xor pK^[ 8];
      pK^[10] := pK^[2] xor pK^[ 9];
      pK^[11] := pK^[3] xor pK^[10];
      if i=6 then exit;
      temp := pK^[11];
      // SubWord(temp) if "word" count mod 8 = 4
      pK^[12] := ((SBox[(temp       ) and $ff])       ) xor
                 ((SBox[(temp shr  8) and $ff]) shl  8) xor
                 ((SBox[(temp shr 16) and $ff]) shl 16) xor
                 ((SBox[(temp shr 24)        ]) shl 24) xor
                 pK^[4];
      pK^[13] := pK^[5] xor pK^[12];
      pK^[14] := pK^[6] xor pK^[13];
      pK^[15] := pK^[7] xor pK^[14];
      inc(PtrUInt(pK),8*4);
    end;
  end;
end;
var Nk: integer;
    ctx: TAESContext absolute Context;
begin
  result := true;
  Initialized := true;
{$ifdef USEPADLOCK}
  if DoPadlockInit(Key,KeySize) then
    exit; // Init OK
{$endif}
  with ctx do begin
    // Clear only the necessary context data at init. IV and buf
    // remain uninitialized, other fields are initialized below.
{$ifdef USEPADLOCK}
    ctx.ViaCtx := nil;
{$endif}
  end;
  if (KeySize<>128) and (KeySize<>192) and (KeySize<>256) then begin
    result := false;
    Initialized := false;
    exit;
  end;
  Nk := KeySize div 32;
  Move(Key, ctx.RK, 4*Nk);
  {$ifdef NOTPUREPASCALNORCPU64DELPHI}
  ctx.AesNi := cfAESNI in CpuFeatures;
  {$else}
  ctx.AesNi := false;
  {$endif}
  ctx.Rounds  := 6+Nk;
  ctx.KeyBits := KeySize;
  // Calculate encryption round keys
  {$ifdef USEAESNI} // 192 is more complex -> skip by now
  if (KeySize<>192) and ctx.AESNI then
    ShiftAesNi(KeySize,@ctx.RK) else
  {$endif}
    Shift(KeySize,pointer(@ctx.RK));
end;

{$ifndef PURE_PASCAL}
  {$define AES_ROLLED} // asm version is rolled
{$endif}

{$ifdef USEAESNI} // should be put outside the main method for FPC :(
procedure MakeDecrKeyAesNi(Rounds: integer; RK: Pointer);
asm // eax=Rounds edx=RK
  sub eax,9
  movdqu xmm0,[edx+$10]
  movdqu xmm1,[edx+$20]
  movdqu xmm2,[edx+$30]
  movdqu xmm3,[edx+$40]
  movdqu xmm4,[edx+$50]
  movdqu xmm5,[edx+$60]
  movdqu xmm6,[edx+$70]
  movdqu xmm7,[edx+$80]
  {$ifdef HASAESNI}
  aesimc xmm0,xmm0
  aesimc xmm1,xmm1
  aesimc xmm2,xmm2
  aesimc xmm3,xmm3
  aesimc xmm4,xmm4
  aesimc xmm5,xmm5
  aesimc xmm6,xmm6
  aesimc xmm7,xmm7
  {$else}
  db $66,$0F,$38,$DB,$C0
  db $66,$0F,$38,$DB,$C9
  db $66,$0F,$38,$DB,$D2
  db $66,$0F,$38,$DB,$DB
  db $66,$0F,$38,$DB,$E4
  db $66,$0F,$38,$DB,$ED
  db $66,$0F,$38,$DB,$F6
  db $66,$0F,$38,$DB,$FF
  {$endif}
  movdqu [edx+$10],xmm0
  movdqu [edx+$20],xmm1
  movdqu [edx+$30],xmm2
  movdqu [edx+$40],xmm3
  movdqu [edx+$50],xmm4
  movdqu [edx+$60],xmm5
  movdqu [edx+$70],xmm6
  movdqu [edx+$80],xmm7
  lea edx,[edx+$90]
@loop:
  movdqu xmm0,[edx]
  db $66,$0F,$38,$DB,$C0 // aesimc xmm0,xmm0
  movdqu [edx],xmm0
  dec eax
  lea edx,[edx+16]
  jnz @loop
end;
{$endif}

function TAES.DecryptInit(const Key; KeySize: cardinal): boolean;
procedure MakeDecrKey(var ctx: TAESContext);
// Calculate decryption key from encryption key
var i: integer;
    x: cardinal;
{$ifndef AES_ROLLED}
    j: integer;
{$endif}
begin
{$ifndef AES_ROLLED} // inversion is needed only for fully unrolled version
  // invert the order of the round keys
  i := 0;
  j := 4*ctx.Rounds;
  while i<j do begin
    x:=TAWk(ctx.RK)[i  ];  TAWk(ctx.RK)[i  ]:=TAWk(ctx.RK)[j  ];  TAWk(ctx.RK)[j  ]:=x;
    x:=TAWk(ctx.RK)[i+1];  TAWk(ctx.RK)[i+1]:=TAWk(ctx.RK)[j+1];  TAWk(ctx.RK)[j+1]:=x;
    x:=TAWk(ctx.RK)[i+2];  TAWk(ctx.RK)[i+2]:=TAWk(ctx.RK)[j+2];  TAWk(ctx.RK)[j+2]:=x;
    x:=TAWk(ctx.RK)[i+3];  TAWk(ctx.RK)[i+3]:=TAWk(ctx.RK)[j+3];  TAWk(ctx.RK)[j+3]:=x;
    inc(i,4);
    dec(j,4);
  end;
{$endif}
  for i := 1 to ctx.Rounds-1 do begin
    x  := TAWk(ctx.RK)[i*4  ];
    TAWk(ctx.RK)[i*4  ] := Td3[SBox[x shr 24]] xor Td2[SBox[x shr 16 and $ff]]
      xor Td1[SBox[x shr 8 and $ff]] xor Td0[SBox[x and $ff]];
    x  := TAWk(ctx.RK)[i*4+1];
    TAWk(ctx.RK)[i*4+1] := Td3[SBox[x shr 24]] xor Td2[SBox[x shr 16 and $ff]]
      xor Td1[SBox[x shr 8 and $ff]] xor Td0[SBox[x and $ff]];
    x  := TAWk(ctx.RK)[i*4+2];
    TAWk(ctx.RK)[i*4+2] := Td3[SBox[x shr 24]] xor Td2[SBox[x shr 16 and $ff]]
      xor Td1[SBox[x shr 8 and $ff]] xor Td0[SBox[x and $ff]];
    x  := TAWk(ctx.RK)[i*4+3];
    TAWk(ctx.RK)[i*4+3] := Td3[SBox[x shr 24]] xor Td2[SBox[x shr 16 and $ff]]
      xor Td1[SBox[x shr 8 and $ff]] xor Td0[SBox[x and $ff]];
  end;
end;
var ctx: TAESContext absolute Context;
begin
  {$ifdef USEPADLOCK}
  if DoPadlockInit(Key,KeySize) then begin
    result := true;
    Initialized := true;
    exit; // Init OK
  end;
  {$endif}
  result := EncryptInit(Key, KeySize); // contains Initialized := true
  if not result then
    exit;
  {$ifdef USEAESNI}
  if ctx.AESNI then
    MakeDecrKeyAesNi(ctx.Rounds,@ctx.RK) else
  {$endif}
    MakeDecrKey(ctx);
end;

procedure TAES.Decrypt(var B: TAESBlock);
begin
  Decrypt(B,B);
end;

procedure TAES.Decrypt(const BI: TAESBlock; var BO: TAESBlock);
// decrypt one block (in ECB mode)
{$ifdef PURE_PASCAL}
var
  ctx: TAESContext absolute Context;
  s0,s1,s2,s3: cardinal;    {TAESBlock s as separate variables}
  t0,t1,t2: cardinal;    {TAESBlock t as separate variables}
{$ifdef AES_ROLLED}
  i: integer;
  pK: PWA4;
{$else}
  t3: cardinal;
  pK: PAWk;                 {pointer to loop rount key   }
{$endif}
begin
{$ifdef USEPADLOCK}
  if ctx.ViaCtx<>nil then begin
    padlock_aes_decrypt(ctx.ViaCtx,@BI,@BO,16);
    exit;
  end;
{$endif}
{$ifdef AES_ROLLED}
  // Wolfgang Ehrhardt rolled version - faster on modern CPU than unrolled one below
  // Setup key pointer
  pK := PWA4(@ctx.RK[ctx.Rounds]);
  // Initialize with input block
  s0 := TWA4(BI)[0] xor pK^[0];
  s1 := TWA4(BI)[1] xor pK^[1];
  s2 := TWA4(BI)[2] xor pK^[2];
  s3 := TWA4(BI)[3] xor pK^[3];
  dec(pK);
  for I := 1 to ctx.Rounds-1 do begin
      t0 := Td0[s0 and $ff] xor Td1[s3 shr 8 and $ff] xor Td2[s2 shr 16 and $ff] xor Td3[s1 shr 24];
      t1 := Td0[s1 and $ff] xor Td1[s0 shr 8 and $ff] xor Td2[s3 shr 16 and $ff] xor Td3[s2 shr 24];
      t2 := Td0[s2 and $ff] xor Td1[s1 shr 8 and $ff] xor Td2[s0 shr 16 and $ff] xor Td3[s3 shr 24];
      s3 := Td0[s3 and $ff] xor Td1[s2 shr 8 and $ff] xor Td2[s1 shr 16 and $ff] xor Td3[s0 shr 24] xor PK[3];
      s0 := t0 xor PK[0];
      s1 := t1 xor PK[1];
      s2 := t2 xor PK[2];
      dec(pK);
    end;
  TWA4(BO)[0] := ((InvSBox[s0        and $ff])        xor
                  (InvSBox[s3 shr  8 and $ff]) shl  8 xor
                  (InvSBox[s2 shr 16 and $ff]) shl 16 xor
                  (InvSBox[s1 shr 24])         shl 24    ) xor pK^[0];
  TWA4(BO)[1] := ((InvSBox[s1        and $ff])        xor
                  (InvSBox[s0 shr  8 and $ff]) shl  8 xor
                  (InvSBox[s3 shr 16 and $ff]) shl 16 xor
                  (InvSBox[s2 shr 24])         shl 24    ) xor pK^[1];
  TWA4(BO)[2] := ((InvSBox[s2        and $ff])        xor
                  (InvSBox[s1 shr  8 and $ff]) shl  8 xor
                  (InvSBox[s0 shr 16 and $ff]) shl 16 xor
                  (InvSBox[s3 shr 24])         shl 24    ) xor pK^[2];
  TWA4(BO)[3] := ((InvSBox[s3        and $ff])        xor
                  (InvSBox[s2 shr  8 and $ff]) shl  8 xor
                  (InvSBox[s1 shr 16 and $ff]) shl 16 xor
                  (InvSBox[s0 shr 24])         shl 24    ) xor pK^[3];
{$else} // unrolled version (WE6) from Wolfgang Ehrhardt - slower
  // Setup key pointer
  pK := PAWk(@ctx.RK);
  // Initialize with input block
  s0 := TWA4(BI)[0] xor pK^[0];
  s1 := TWA4(BI)[1] xor pK^[1];
  s2 := TWA4(BI)[2] xor pK^[2];
  s3 := TWA4(BI)[3] xor pK^[3];

  // Round 1
  t0 := Td0[s0 and $ff] xor Td1[s3 shr 8 and $ff] xor Td2[s2 shr 16 and $ff] xor Td3[s1 shr 24] xor pK^[4];
  t1 := Td0[s1 and $ff] xor Td1[s0 shr 8 and $ff] xor Td2[s3 shr 16 and $ff] xor Td3[s2 shr 24] xor pK^[5];
  t2 := Td0[s2 and $ff] xor Td1[s1 shr 8 and $ff] xor Td2[s0 shr 16 and $ff] xor Td3[s3 shr 24] xor pK^[6];
  t3 := Td0[s3 and $ff] xor Td1[s2 shr 8 and $ff] xor Td2[s1 shr 16 and $ff] xor Td3[s0 shr 24] xor pK^[7];
  // Round 2
  s0 := Td0[t0 and $ff] xor Td1[t3 shr 8 and $ff] xor Td2[t2 shr 16 and $ff] xor Td3[t1 shr 24] xor pK^[8];
  s1 := Td0[t1 and $ff] xor Td1[t0 shr 8 and $ff] xor Td2[t3 shr 16 and $ff] xor Td3[t2 shr 24] xor pK^[9];
  s2 := Td0[t2 and $ff] xor Td1[t1 shr 8 and $ff] xor Td2[t0 shr 16 and $ff] xor Td3[t3 shr 24] xor pK^[10];
  s3 := Td0[t3 and $ff] xor Td1[t2 shr 8 and $ff] xor Td2[t1 shr 16 and $ff] xor Td3[t0 shr 24] xor pK^[11];
  // Round 3
  t0 := Td0[s0 and $ff] xor Td1[s3 shr 8 and $ff] xor Td2[s2 shr 16 and $ff] xor Td3[s1 shr 24] xor pK^[12];
  t1 := Td0[s1 and $ff] xor Td1[s0 shr 8 and $ff] xor Td2[s3 shr 16 and $ff] xor Td3[s2 shr 24] xor pK^[13];
  t2 := Td0[s2 and $ff] xor Td1[s1 shr 8 and $ff] xor Td2[s0 shr 16 and $ff] xor Td3[s3 shr 24] xor pK^[14];
  t3 := Td0[s3 and $ff] xor Td1[s2 shr 8 and $ff] xor Td2[s1 shr 16 and $ff] xor Td3[s0 shr 24] xor pK^[15];
  // Round 4
  s0 := Td0[t0 and $ff] xor Td1[t3 shr 8 and $ff] xor Td2[t2 shr 16 and $ff] xor Td3[t1 shr 24] xor pK^[16];
  s1 := Td0[t1 and $ff] xor Td1[t0 shr 8 and $ff] xor Td2[t3 shr 16 and $ff] xor Td3[t2 shr 24] xor pK^[17];
  s2 := Td0[t2 and $ff] xor Td1[t1 shr 8 and $ff] xor Td2[t0 shr 16 and $ff] xor Td3[t3 shr 24] xor pK^[18];
  s3 := Td0[t3 and $ff] xor Td1[t2 shr 8 and $ff] xor Td2[t1 shr 16 and $ff] xor Td3[t0 shr 24] xor pK^[19];
  // Round 5
  t0 := Td0[s0 and $ff] xor Td1[s3 shr 8 and $ff] xor Td2[s2 shr 16 and $ff] xor Td3[s1 shr 24] xor pK^[20];
  t1 := Td0[s1 and $ff] xor Td1[s0 shr 8 and $ff] xor Td2[s3 shr 16 and $ff] xor Td3[s2 shr 24] xor pK^[21];
  t2 := Td0[s2 and $ff] xor Td1[s1 shr 8 and $ff] xor Td2[s0 shr 16 and $ff] xor Td3[s3 shr 24] xor pK^[22];
  t3 := Td0[s3 and $ff] xor Td1[s2 shr 8 and $ff] xor Td2[s1 shr 16 and $ff] xor Td3[s0 shr 24] xor pK^[23];
  // Round 6
  s0 := Td0[t0 and $ff] xor Td1[t3 shr 8 and $ff] xor Td2[t2 shr 16 and $ff] xor Td3[t1 shr 24] xor pK^[24];
  s1 := Td0[t1 and $ff] xor Td1[t0 shr 8 and $ff] xor Td2[t3 shr 16 and $ff] xor Td3[t2 shr 24] xor pK^[25];
  s2 := Td0[t2 and $ff] xor Td1[t1 shr 8 and $ff] xor Td2[t0 shr 16 and $ff] xor Td3[t3 shr 24] xor pK^[26];
  s3 := Td0[t3 and $ff] xor Td1[t2 shr 8 and $ff] xor Td2[t1 shr 16 and $ff] xor Td3[t0 shr 24] xor pK^[27];
  // Round 7
  t0 := Td0[s0 and $ff] xor Td1[s3 shr 8 and $ff] xor Td2[s2 shr 16 and $ff] xor Td3[s1 shr 24] xor pK^[28];
  t1 := Td0[s1 and $ff] xor Td1[s0 shr 8 and $ff] xor Td2[s3 shr 16 and $ff] xor Td3[s2 shr 24] xor pK^[29];
  t2 := Td0[s2 and $ff] xor Td1[s1 shr 8 and $ff] xor Td2[s0 shr 16 and $ff] xor Td3[s3 shr 24] xor pK^[30];
  t3 := Td0[s3 and $ff] xor Td1[s2 shr 8 and $ff] xor Td2[s1 shr 16 and $ff] xor Td3[s0 shr 24] xor pK^[31];
  // Round 8
  s0 := Td0[t0 and $ff] xor Td1[t3 shr 8 and $ff] xor Td2[t2 shr 16 and $ff] xor Td3[t1 shr 24] xor pK^[32];
  s1 := Td0[t1 and $ff] xor Td1[t0 shr 8 and $ff] xor Td2[t3 shr 16 and $ff] xor Td3[t2 shr 24] xor pK^[33];
  s2 := Td0[t2 and $ff] xor Td1[t1 shr 8 and $ff] xor Td2[t0 shr 16 and $ff] xor Td3[t3 shr 24] xor pK^[34];
  s3 := Td0[t3 and $ff] xor Td1[t2 shr 8 and $ff] xor Td2[t1 shr 16 and $ff] xor Td3[t0 shr 24] xor pK^[35];
  // Round 9
  t0 := Td0[s0 and $ff] xor Td1[s3 shr 8 and $ff] xor Td2[s2 shr 16 and $ff] xor Td3[s1 shr 24] xor pK^[36];
  t1 := Td0[s1 and $ff] xor Td1[s0 shr 8 and $ff] xor Td2[s3 shr 16 and $ff] xor Td3[s2 shr 24] xor pK^[37];
  t2 := Td0[s2 and $ff] xor Td1[s1 shr 8 and $ff] xor Td2[s0 shr 16 and $ff] xor Td3[s3 shr 24] xor pK^[38];
  t3 := Td0[s3 and $ff] xor Td1[s2 shr 8 and $ff] xor Td2[s1 shr 16 and $ff] xor Td3[s0 shr 24] xor pK^[39];
  if ctx.rounds>10 then begin
    // Round 10
    s0 := Td0[t0 and $ff] xor Td1[t3 shr 8 and $ff] xor Td2[t2 shr 16 and $ff] xor Td3[t1 shr 24] xor pK^[40];
    s1 := Td0[t1 and $ff] xor Td1[t0 shr 8 and $ff] xor Td2[t3 shr 16 and $ff] xor Td3[t2 shr 24] xor pK^[41];
    s2 := Td0[t2 and $ff] xor Td1[t1 shr 8 and $ff] xor Td2[t0 shr 16 and $ff] xor Td3[t3 shr 24] xor pK^[42];
    s3 := Td0[t3 and $ff] xor Td1[t2 shr 8 and $ff] xor Td2[t1 shr 16 and $ff] xor Td3[t0 shr 24] xor pK^[43];
    // Round 11
    t0 := Td0[s0 and $ff] xor Td1[s3 shr 8 and $ff] xor Td2[s2 shr 16 and $ff] xor Td3[s1 shr 24] xor pK^[44];
    t1 := Td0[s1 and $ff] xor Td1[s0 shr 8 and $ff] xor Td2[s3 shr 16 and $ff] xor Td3[s2 shr 24] xor pK^[45];
    t2 := Td0[s2 and $ff] xor Td1[s1 shr 8 and $ff] xor Td2[s0 shr 16 and $ff] xor Td3[s3 shr 24] xor pK^[46];
    t3 := Td0[s3 and $ff] xor Td1[s2 shr 8 and $ff] xor Td2[s1 shr 16 and $ff] xor Td3[s0 shr 24] xor pK^[47];
    if ctx.rounds>12 then begin
      // Round 12
      s0 := Td0[t0 and $ff] xor Td1[t3 shr 8 and $ff] xor Td2[t2 shr 16 and $ff] xor Td3[t1 shr 24] xor pK^[48];
      s1 := Td0[t1 and $ff] xor Td1[t0 shr 8 and $ff] xor Td2[t3 shr 16 and $ff] xor Td3[t2 shr 24] xor pK^[49];
      s2 := Td0[t2 and $ff] xor Td1[t1 shr 8 and $ff] xor Td2[t0 shr 16 and $ff] xor Td3[t3 shr 24] xor pK^[50];
      s3 := Td0[t3 and $ff] xor Td1[t2 shr 8 and $ff] xor Td2[t1 shr 16 and $ff] xor Td3[t0 shr 24] xor pK^[51];
      // Round 13
      t0 := Td0[s0 and $ff] xor Td1[s3 shr 8 and $ff] xor Td2[s2 shr 16 and $ff] xor Td3[s1 shr 24] xor pK^[52];
      t1 := Td0[s1 and $ff] xor Td1[s0 shr 8 and $ff] xor Td2[s3 shr 16 and $ff] xor Td3[s2 shr 24] xor pK^[53];
      t2 := Td0[s2 and $ff] xor Td1[s1 shr 8 and $ff] xor Td2[s0 shr 16 and $ff] xor Td3[s3 shr 24] xor pK^[54];
      t3 := Td0[s3 and $ff] xor Td1[s2 shr 8 and $ff] xor Td2[s1 shr 16 and $ff] xor Td3[s0 shr 24] xor pK^[55];
    end;
  end;
  inc(PtrUInt(pK), (ctx.rounds shl 4));

  // Uses InvSbox and shl, needs type cast cardinal() for
  // 16 bit compilers: here InvSbox is byte, Td4 is cardinal
  TWA4(BO)[0] := ((InvSBox[t0 and $ff]) xor
    (InvSBox[t3 shr  8 and $ff]) shl  8 xor
    (InvSBox[t2 shr 16 and $ff]) shl 16 xor
    (InvSBox[t1 shr 24]) shl 24) xor pK^[0];
  TWA4(BO)[1] := ((InvSBox[t1 and $ff]) xor
    (InvSBox[t0 shr  8 and $ff]) shl  8 xor
    (InvSBox[t3 shr 16 and $ff]) shl 16 xor
    (InvSBox[t2 shr 24]) shl 24) xor pK^[1];
  TWA4(BO)[2] := ((InvSBox[t2 and $ff]) xor
    (InvSBox[t1 shr  8 and $ff]) shl  8 xor
    (InvSBox[t0 shr 16 and $ff]) shl 16 xor
    (InvSBox[t3 shr 24]) shl 24) xor pK^[2];
  TWA4(BO)[3] := ((InvSBox[t3 and $ff]) xor
    (InvSBox[t2 shr  8 and $ff]) shl  8 xor
    (InvSBox[t1 shr 16 and $ff]) shl 16 xor
    (InvSBox[t0 shr 24]) shl 24) xor pK^[3];
{$endif}
end;
{$else}
asm // eax=TAES(self)=TAESContext edx=BI ecx=BO
{$ifdef USEAESNI}
  // AES-NI hardware accelerated version by A. Bouchez
  cmp byte ptr [eax].TAESContext.AesNi,0
  je @noAesNi
  movdqu xmm7,[edx]
  mov dl,[eax].TAESContext.Rounds
  cmp dl,10
  je @128
  cmp dl,12
  je @192
@256:
  movdqu xmm0,[eax+16*14]
  movdqu xmm1,[eax+16*13]
  movdqu xmm2,[eax+16*12]
  movdqu xmm3,[eax+16*11]
  movdqu xmm4,[eax+16*10]
  movdqu xmm5,[eax+16*9]
  movdqu xmm6,[eax+16*8]
  pxor xmm7,xmm0
  {$ifdef HASAESNI}
  aesdec xmm7,xmm1
  aesdec xmm7,xmm2
  aesdec xmm7,xmm3
  aesdec xmm7,xmm4
  aesdec xmm7,xmm5
  aesdec xmm7,xmm6
  {$else}
  db $66,$0F,$38,$DE,$F9
  db $66,$0F,$38,$DE,$FA
  db $66,$0F,$38,$DE,$FB
  db $66,$0F,$38,$DE,$FC
  db $66,$0F,$38,$DE,$FD
  db $66,$0F,$38,$DE,$FE
  {$endif}
  movdqu xmm0,[eax+16*7]
  movdqu xmm1,[eax+16*6]
  movdqu xmm2,[eax+16*5]
  movdqu xmm3,[eax+16*4]
  movdqu xmm4,[eax+16*3]
  movdqu xmm5,[eax+16*2]
  movdqu xmm6,[eax+16*1]
  {$ifdef HASAESNI}
  aesdec xmm7,xmm0
  aesdec xmm7,xmm1
  aesdec xmm7,xmm2
  aesdec xmm7,xmm3
  aesdec xmm7,xmm4
  aesdec xmm7,xmm5
  aesdec xmm7,xmm6
  {$else}
  db $66,$0F,$38,$DE,$F8
  db $66,$0F,$38,$DE,$F9
  db $66,$0F,$38,$DE,$FA
  db $66,$0F,$38,$DE,$FB
  db $66,$0F,$38,$DE,$FC
  db $66,$0F,$38,$DE,$FD
  db $66,$0F,$38,$DE,$FE
  {$endif}
  movdqu xmm0,[eax+16*0]
  {$ifdef HASAESNI}
  aesdeclast xmm7,xmm0
  {$else}
  db $66,$0F,$38,$DF,$F8
  {$endif}
  movdqu [ecx],xmm7
  ret
@192:
  movdqu xmm0,[eax+16*12]
  movdqu xmm1,[eax+16*11]
  movdqu xmm2,[eax+16*10]
  movdqu xmm3,[eax+16*9]
  movdqu xmm4,[eax+16*8]
  movdqu xmm5,[eax+16*7]
  movdqu xmm6,[eax+16*6]
  pxor xmm7,xmm0
  {$ifdef HASAESNI}
  aesdec xmm7,xmm1
  aesdec xmm7,xmm2
  aesdec xmm7,xmm3
  aesdec xmm7,xmm4
  aesdec xmm7,xmm5
  aesdec xmm7,xmm6
  {$else}
  db $66,$0F,$38,$DE,$F9
  db $66,$0F,$38,$DE,$FA
  db $66,$0F,$38,$DE,$FB
  db $66,$0F,$38,$DE,$FC
  db $66,$0F,$38,$DE,$FD
  db $66,$0F,$38,$DE,$FE
  {$endif}
  movdqu xmm0,[eax+16*5]
  movdqu xmm1,[eax+16*4]
  movdqu xmm2,[eax+16*3]
  movdqu xmm3,[eax+16*2]
  movdqu xmm4,[eax+16*1]
  movdqu xmm5,[eax+16*0]
  {$ifdef HASAESNI}
  aesdec xmm7,xmm0
  aesdec xmm7,xmm1
  aesdec xmm7,xmm2
  aesdec xmm7,xmm3
  aesdec xmm7,xmm4
  aesdeclast xmm7,xmm5
  {$else}
  db $66,$0F,$38,$DE,$F8
  db $66,$0F,$38,$DE,$F9
  db $66,$0F,$38,$DE,$FA
  db $66,$0F,$38,$DE,$FB
  db $66,$0F,$38,$DE,$FC
  db $66,$0F,$38,$DF,$FD
  {$endif}
  movdqu [ecx],xmm7
  ret
@128:
  movdqu xmm0,[eax+16*10]
  movdqu xmm1,[eax+16*9]
  movdqu xmm2,[eax+16*8]
  movdqu xmm3,[eax+16*7]
  movdqu xmm4,[eax+16*6]
  movdqu xmm5,[eax+16*5]
  movdqu xmm6,[eax+16*4]
  pxor xmm7,xmm0
  {$ifdef HASAESNI}
  aesdec xmm7,xmm1
  aesdec xmm7,xmm2
  aesdec xmm7,xmm3
  aesdec xmm7,xmm4
  {$else}
  db $66,$0F,$38,$DE,$F9
  db $66,$0F,$38,$DE,$FA
  db $66,$0F,$38,$DE,$FB
  db $66,$0F,$38,$DE,$FC
  {$endif}
  movdqu xmm0,[eax+16*3]
  movdqu xmm1,[eax+16*2]
  movdqu xmm2,[eax+16*1]
  movdqu xmm3,[eax+16*0]
  {$ifdef HASAESNI}
  aesdec xmm7,xmm5
  aesdec xmm7,xmm6
  aesdec xmm7,xmm0
  aesdec xmm7,xmm1
  aesdec xmm7,xmm2
  aesdeclast xmm7,xmm3
  {$else}
  db $66,$0F,$38,$DE,$FD
  db $66,$0F,$38,$DE,$FE
  db $66,$0F,$38,$DE,$F8
  db $66,$0F,$38,$DE,$F9
  db $66,$0F,$38,$DE,$FA
  db $66,$0F,$38,$DF,$FB
  {$endif}
  movdqu [ecx],xmm7
  ret
@noAesNi:
{$endif USEAESNI}
// rolled optimized decryption asm version by A. Bouchez
{$ifdef USEPADLOCK}
  cmp dword [eax].TAESContext.ViaCtx,0
  jz @DoAsm
  mov eax,[eax].TAESContext.ViaCtx
  push 16
  push ecx
  push edx
  push eax           // padlock_aes_decrypt(ctx.ViaCtx,@BI,@BO,16);
{$ifdef USEPADLOCKDLL}
  call dword ptr [padlock_aes_decrypt] {$else}
  call padlock_aes_decrypt
{$endif}
  add esp,16 // padlock_aes_decrypt is cdecl -> caller must restore stack
  ret
@DoAsm:
{$endif}
  push ebx
  push esi
  push edi
  push ebp
  add esp,-20
  mov [esp],ecx
  movzx ecx,byte ptr [eax].TAESContext.Rounds
  lea esi,[4*ecx]
  lea ecx,[ecx-1]
  lea eax,[eax+4*esi] // eax=@ctx.rk[ctx.rounds]=pk
  mov [esp+16],ecx // [esp+16]=ctx.round
  mov ebx,[edx]
  xor ebx,[eax]
  mov esi,[edx+4]
  xor esi,[eax+4]
  mov ecx,[edx+8]
  xor ecx,[eax+8]
  mov edx,[edx+12]
  xor edx,[eax+12]
  lea eax,[eax-16]
@1: // pk=eax s0=ebx s1=esi s2=ecx s3=edx
  movzx edi,bl
  mov edi,[4*edi+td0]
  movzx ebp,dh
  xor edi,[4*ebp+td1]
  mov ebp,ecx
  shr ebp,$10
  and ebp,255
  xor edi,[4*ebp+td2]
  mov ebp,esi
  shr ebp,$18
  xor edi,[4*ebp+td3]
  mov [esp+4],edi
  mov edi,esi
  and edi,255
  mov edi,[4*edi+td0]
  movzx ebp,bh
  xor edi,[4*ebp+td1]
  mov ebp,edx
  shr ebp,$10
  and ebp,255
  xor edi,[4*ebp+td2]
  mov ebp,ecx
  shr ebp,$18
  xor edi,[4*ebp+td3]
  mov [esp+8],edi
  movzx edi,cl
  mov edi,[4*edi+td0]
  movzx ebp,si
  shr ebp,$08
  xor edi,[4*ebp+td1]
  mov ebp,ebx
  shr ebp,$10
  and ebp,255
  xor edi,[4*ebp+td2]
  mov ebp,edx
  shr ebp,$18
  xor edi,[4*ebp+td3]
  mov [esp+12],edi
  and edx,255
  mov edx,[4*edx+td0]
  movzx ecx,ch
  xor edx,[4*ecx+td1]
  shr esi,$10
  and esi,255
  xor edx,[4*esi+td2]
  shr ebx,$18
  xor edx,[4*ebx+td3]
  xor edx,[eax+12]
  mov ebx,[eax]
  xor ebx,[esp+4]
  mov esi,[eax+4]
  xor esi,[esp+8]
  mov ecx,[eax+8]
  xor ecx,[esp+12]
  lea eax,[eax-16]
  dec byte ptr [esp+16]
  jnz @1

  mov ebp,eax
  movzx eax,bl
  movzx eax,byte ptr [eax+InvSBox]
  movzx edi,dh
  movzx edi,byte ptr [edi+InvSBox]
  shl edi,$08
  xor eax,edi
  mov edi,ecx
  shr edi,$10
  and edi,255
  movzx edi,byte ptr [edi+InvSBox]
  shl edi,$10
  xor eax,edi
  mov edi,esi
  shr edi,$18
  movzx edi,byte ptr [edi+InvSBox]
  shl edi,$18
  xor eax,edi
  xor eax,[ebp]
  mov edi,[esp]
  mov [edi],eax
  mov eax,esi
  and eax,255
  movzx eax,byte ptr [eax+InvSBox]
  movzx edi,bh
  movzx edi,byte ptr [edi+InvSBox]
  shl edi,$08
  xor eax,edi
  mov edi,edx
  shr edi,$10
  and edi,255
  movzx edi,byte ptr [edi+InvSBox]
  shl edi,$10
  xor eax,edi
  mov edi,ecx
  shr edi,$18
  movzx edi,byte ptr [edi+InvSBox]
  shl edi,$18
  xor eax,edi
  xor eax,[ebp+4]
  mov edi,[esp]
  mov [edi+4],eax
  movzx eax,cl
  movzx eax,byte ptr [eax+InvSBox]
  movzx edi,si
  shr edi,$08
  movzx edi,byte ptr [edi+InvSBox]
  shl edi,$08
  xor eax,edi
  mov edi,ebx
  shr edi,$10
  and edi,255
  movzx edi,byte ptr [edi+InvSBox]
  shl edi,$10
  xor eax,edi
  mov edi,edx
  shr edi,$18
  movzx edi,byte ptr [edi+InvSBox]
  shl edi,$18
  xor eax,edi
  xor eax,[ebp+8]
  mov edi,[esp]
  mov [edi+8],eax
  and edx,255
  movzx eax,byte ptr [edx+InvSBox]
  shr ecx,$08
  and ecx,255
  movzx edx,byte ptr [ecx+InvSBox]
  shl edx,$08
  xor eax,edx
  shr esi,$10
  and esi,255
  movzx edx,byte ptr [esi+InvSBox]
  shl edx,$10
  xor eax,edx
  shr ebx,$18
  movzx edx,byte ptr [ebx+InvSBox]
  shl edx,$18
  xor eax,edx
  xor eax,[ebp+12]
  mov [edi+12],eax
  add esp,20
  pop ebp
  pop edi
  pop esi
  pop ebx
end;
{$endif}

procedure TAES.DoBlocks(pIn, pOut: PAESBlock;
  out oIn, oOut: PAESBLock; Count: integer; doEncrypt: Boolean);
var i: integer;
    ctx: TAESContext absolute Context;
begin
{$ifdef USEPADLOCK}
//  assert(PtrUInt(pIn) and $F=0); // must be 16 bytes aligned
  if ctx.ViaCtx<>nil then begin
    if Count<>0 then begin
      Count := Count*AESBlockSize;
      if doEncrypt then
        padlock_aes_encrypt(ctx.ViaCtx,pIn,pOut,Count) else
        padlock_aes_decrypt(ctx.ViaCtx,pIn,pOut,Count);
    end;
    oIn := pointer(PtrUInt(pIn)+Count);
    oOut := pointer(PtrUInt(pOut)+Count);
    exit;
  end;
{$endif}
  if doEncrypt then
  for i := 1 to Count do begin
    Encrypt(pIn^,pOut^);
    inc(pIn);
    inc(pOut);
  end else
  for i := 1 to Count do begin
    Decrypt(pIn^,pOut^);
    inc(pIn);
    inc(pOut);
  end;
  oIn := pIn;
  oOut := pOut;
end;

function TAES.DoInit(const Key; KeySize: cardinal; doEncrypt: boolean): boolean;
begin
  if doEncrypt then
    result := EncryptInit(Key, KeySize) else
    result := DecryptInit(Key,KeySize);
end;

procedure TAES.DoBlocks(pIn, pOut: PAESBlock; Count: integer; doEncrypt: boolean);
begin
  DoBlocks(pIn,pOut,pIn,pOut,Count,doEncrypt);
end;

function TAES.UsesAESNI: boolean;
begin
  {$ifdef USEAESNI}
  result := TAESContext(Context).AesNi;
  {$else}
  result := False;
  {$endif}
end;

{$ifdef USEPADLOCK}
procedure TAES.Done;
var ctx: TAESContext absolute Context;
begin
  if initialized and padlock_available and (ctx.ViaCtx<>nil) then begin
    padlock_aes_close(ctx.ViaCtx);
    initialized := false;
    ctx.ViaCtx := nil;
  end;
end;
{$else}
procedure TAES.Done;
begin
end;
{$endif}

{$ifdef USETHREADSFORBIGAESBLOCKS}
type
  TThreadParams = record
    bIn, bOut: pAESBlock;
    BlockCount,BlockIndex: integer;
    Encrypt: boolean;
    ID: DWORD;
    AES: TAES;
  end;

{ we use direct Windows threads, since we don't need any exception handling
  nor memory usage inside the Thread handler
   -> avoid classes.TThread and system.BeginThread() use
   -> application is still "officialy" mono-threaded (i.e. IsMultiThread=false),
     for faster System.pas and FastMM4 (no locking)
   -> code is even shorter then original one using TThread }
function ThreadWrapper(var P: TThreadParams): Integer; stdcall;
begin
  SetCurrentThreadName('AES #%',[P.BlockIndex]);
  with P do
    AES.DoBlocks(bIn,bOut,bIn,bOut,BlockCount,Encrypt);
  ExitThread(0);
  result := 0; // make the compiler happy, but won't never be called
end;

procedure TAES.DoBlocksThread(var bIn, bOut: PAESBlock; Count: integer; doEncrypt: boolean);
var Thread: array[0..3] of TThreadParams; // faster than dynamic array
    Handle: array[0..3] of THandle; // high(Thread) is not compiled by XE2
    nThread, i, nOne: integer;
    pIn, pOut: PAESBlock;
begin
  if Count=0 then exit;
  if {$ifdef USEPADLOCK} padlock_available or {$endif}
     {$ifdef USEAESNI} (cfAESNI in CpuFeatures) or {$endif}
    (SystemInfo.dwNumberOfProcessors<=1) or // (DebugHook<>0) or
    (Count<((512*1024) div AESBlockSize)) then begin // not needed below 512 KB
    DoBlocks(bIn,bOut,bIn,bOut,Count,doEncrypt);
    exit;
  end;
  nThread := SystemInfo.dwNumberOfProcessors;
  if nThread>length(Thread) then // a quad-core is enough ;)
    nThread := length(Thread);
  nOne := Count div nThread;
  pIn := bIn;
  pOut := bOut;
  for i := 0 to nThread-1 do
  with Thread[i] do begin // create threads parameters
    bIn := pIn;
    bOut := pOut;
    BlockCount := nOne;
    BlockIndex := i+1;
    Encrypt := doEncrypt;
    AES := self; // local copy of the AES context for every thread
    Handle[i] := CreateThread(nil,0,@ThreadWrapper,@Thread[i],0,ID);
    inc(pIn,nOne);
    inc(pOut,nOne);
    dec(Count,nOne);
  end;
  if Count>0 then
    DoBlocks(pIn,pOut,pIn,pOut,Count,doEncrypt); // remaining blocks
  {$ifopt C+}
  inc(Count,nOne*nThread);
  assert(PtrUInt(pIn)-PtrUInt(bIn)=cardinal(Count)*AESBlockSize);
  assert(PtrUInt(pOut)-PtrUInt(bOut)=cardinal(Count)*AESBlockSize);
  {$endif}
  bIn := pIn;
  bOut := pOut;
  WaitForMultipleObjects(nThread,@Handle[0],True,INFINITE);
  for i := 0 to nThread-1 do
    CloseHandle(Handle[i]);
end;
{$endif USETHREADSFORBIGAESBLOCKS}


{ TSHA256 }

// under Win32, with a Core i7 CPU: pure pascal: 152ms - x86: 112ms
// under Win64, with a Core i7 CPU: pure pascal: 202ms - SSE4: 78ms

procedure Sha256ExpandMessageBlocks(W, Buf: PIntegerArray);
// Calculate "expanded message blocks"
{$ifdef PUREPASCAL}
var i: integer;
begin
  for i := 0 to 15 do
    W[i]:= bswap32(Buf[i]);
  for i := 16 to 63 do
    W[i] := (((W[i-2]shr 17)or(W[i-2]shl 15))xor((W[i-2]shr 19)or(W[i-2]shl 13))
      xor (W[i-2]shr 10))+W[i-7]+(((W[i-15]shr 7)or(W[i-15]shl 25))
      xor ((W[i-15]shr 18)or(W[i-15]shl 14))xor(W[i-15]shr 3))+W[i-16];
end;
{$else}
asm // W=eax Buf=edx
     push  esi
     push  edi
     push  ebx
     mov   esi,eax
     // part 1: W[i]:= RB(TW32Buf(Buf)[i])
     mov eax,[edx];    mov ebx,[edx+ 4]; bswap eax; bswap ebx; mov [esi  ],eax;  mov [esi+ 4],ebx
     mov eax,[edx+8];  mov ebx,[edx+12]; bswap eax; bswap ebx; mov [esi+8],eax;  mov [esi+12],ebx
     mov eax,[edx+16]; mov ebx,[edx+20]; bswap eax; bswap ebx; mov [esi+16],eax; mov [esi+20],ebx
     mov eax,[edx+24]; mov ebx,[edx+28]; bswap eax; bswap ebx; mov [esi+24],eax; mov [esi+28],ebx
     mov eax,[edx+32]; mov ebx,[edx+36]; bswap eax; bswap ebx; mov [esi+32],eax; mov [esi+36],ebx
     mov eax,[edx+40]; mov ebx,[edx+44]; bswap eax; bswap ebx; mov [esi+40],eax; mov [esi+44],ebx
     mov eax,[edx+48]; mov ebx,[edx+52]; bswap eax; bswap ebx; mov [esi+48],eax; mov [esi+52],ebx
     mov eax,[edx+56]; mov ebx,[edx+60]; bswap eax; bswap ebx; mov [esi+56],eax; mov [esi+60],ebx
     lea esi,[esi+64]
     // part2: W[i]:= LRot_1(W[i-3] xor W[i-8] xor W[i-14] xor W[i-16]);
     mov   ecx,48
@@2: mov   eax,[esi-2*4]    // W[i-2]
     mov   edi,[esi-7*4]    // W[i-7]
     mov   edx,eax
     mov   ebx,eax          // Sig1: RR17 xor RR19 xor SRx,10
     ror   eax,17
     ror   edx,19
     shr   ebx,10
     xor   eax,edx
     xor   eax,ebx
     add   edi,eax
     mov   eax,[esi-15*4]   // W[i-15]
     mov   ebx,eax          // Sig0: RR7 xor RR18 xor SR3
     mov   edx,eax
     ror   eax,7
     ror   edx,18
     shr   ebx,3
     xor   eax,edx
     xor   eax,ebx
     add   eax,edi
     add   eax,[esi-16*4]   // W[i-16]
     mov   [esi],eax
     add   esi,4
     dec   ecx
     jnz   @@2
     pop   ebx
     pop   edi
     pop   esi
end;
{$endif}

const
  K256: array[0..63] of cardinal = (
   $428a2f98, $71374491, $b5c0fbcf, $e9b5dba5, $3956c25b, $59f111f1,
   $923f82a4, $ab1c5ed5, $d807aa98, $12835b01, $243185be, $550c7dc3,
   $72be5d74, $80deb1fe, $9bdc06a7, $c19bf174, $e49b69c1, $efbe4786,
   $0fc19dc6, $240ca1cc, $2de92c6f, $4a7484aa, $5cb0a9dc, $76f988da,
   $983e5152, $a831c66d, $b00327c8, $bf597fc7, $c6e00bf3, $d5a79147,
   $06ca6351, $14292967, $27b70a85, $2e1b2138, $4d2c6dfc, $53380d13,
   $650a7354, $766a0abb, $81c2c92e, $92722c85, $a2bfe8a1, $a81a664b,
   $c24b8b70, $c76c51a3, $d192e819, $d6990624, $f40e3585, $106aa070,
   $19a4c116, $1e376c08, $2748774c, $34b0bcb5, $391c0cb3, $4ed8aa4a,
   $5b9cca4f, $682e6ff3, $748f82ee, $78a5636f, $84c87814, $8cc70208,
   $90befffa, $a4506ceb, $bef9a3f7, $c67178f2);

{$ifdef CPU64}
// optimized unrolled version from Intel's sha256_sse4.asm
//  Original code is released as Copyright (c) 2012, Intel Corporation
var
  K256Aligned: RawByteString; // movdqa + paddd do expect 16 bytes alignment

const
  PSHUFFLE_BYTE_FLIP_MASK: array[0..1] of QWord =
    ($0405060700010203,$0C0D0E0F08090A0B);
  _SHUF_00BA: array[0..1] of QWord =
    ($B0A090803020100, $FFFFFFFFFFFFFFFF);
  _SHUF_DC00: array[0..1] of QWord =
    ($FFFFFFFFFFFFFFFF,$B0A090803020100);
  STACK_SIZE = 32{$ifndef LINUX}+7*16{$endif};

procedure sha256_sse4(var input_data; var digest; num_blks: PtrUInt);
  {$ifdef FPC}nostackframe; assembler;{$endif}
asm // rcx=input_data rdx=digest r8=num_blks
        {$ifdef CPUX64}
        .NOFRAME
        {$endif}
        push    rbx
        {$ifndef LINUX}
        push    rsi
        push    rdi
        {$endif}
        push    rbp
        push    r13
        push    r14
        push    r15
        sub     rsp,STACK_SIZE
        {$ifndef LINUX}
        movdqa  [rsp+20H],xmm6    // manual .PUSHNV for FPC compatibility
        movdqa  [rsp+30H],xmm7
        movdqa  [rsp+40H],xmm8
        movdqa  [rsp+50H],xmm9
        movdqa  [rsp+60H],xmm10
        movdqa  [rsp+70H],xmm11
        movdqa  [rsp+80H],xmm12
        {$endif}
        shl     r8,6
        je      @done
        add     r8,rcx
        mov     [rsp],r8
        mov     eax,[rdx]
        mov     ebx,[rdx+4H]
        mov     edi,[rdx+8H]
        mov     esi,[rdx+0CH]
        mov     r8d,[rdx+10H]
        mov     r9d,[rdx+14H]
        mov     r10d,[rdx+18H]
        mov     r11d,[rdx+1CH]
        movdqu  xmm12,[PSHUFFLE_BYTE_FLIP_MASK]
        movdqu  xmm10,[_SHUF_00BA]
        movdqu  xmm11,[_SHUF_DC00]
@loop0: mov     rbp,[K256Aligned]
        movdqu  xmm4,[rcx]
        pshufb  xmm4,xmm12
        movdqu  xmm5,[rcx+10H]
        pshufb  xmm5,xmm12
        movdqu  xmm6,[rcx+20H]
        pshufb  xmm6,xmm12
        movdqu  xmm7,[rcx+30H]
        pshufb  xmm7,xmm12
        mov     [rsp+8H],rcx
        mov     rcx,3
        nop; nop; nop; nop; nop // manual align 16
@loop1: movdqa  xmm9,[rbp]
        paddd   xmm9,xmm4
        movdqa  [rsp+10H],xmm9
        movdqa  xmm0,xmm7
        mov     r13d,r8d
        ror     r13d,14
        mov     r14d,eax
        palignr xmm0,xmm6,04H
        ror     r14d,9
        xor     r13d,r8d
        mov     r15d,r9d
        ror     r13d,5
        movdqa  xmm1,xmm5
        xor     r14d,eax
        xor     r15d,r10d
        paddd   xmm0,xmm4
        xor     r13d,r8d
        and     r15d,r8d
        ror     r14d,11
        palignr xmm1,xmm4,04H
        xor     r14d,eax
        ror     r13d,6
        xor     r15d,r10d
        movdqa  xmm2,xmm1
        ror     r14d,2
        add     r15d,r13d
        add     r15d,[rsp+10H]
        movdqa  xmm3,xmm1
        mov     r13d,eax
        add     r11d,r15d
        mov     r15d,eax
        pslld   xmm1,25
        or      r13d,edi
        add     esi,r11d
        and     r15d,edi
        psrld   xmm2,7
        and     r13d,ebx
        add     r11d,r14d
        por     xmm1,xmm2
        or      r13d,r15d
        add     r11d,r13d
        movdqa  xmm2,xmm3
        mov     r13d,esi
        mov     r14d,r11d
        movdqa  xmm8,xmm3
        ror     r13d,14
        xor     r13d,esi
        mov     r15d,r8d
        ror     r14d,9
        pslld   xmm3,14
        xor     r14d,r11d
        ror     r13d,5
        xor     r15d,r9d
        psrld   xmm2,18
        ror     r14d,11
        xor     r13d,esi
        and     r15d,esi
        ror     r13d,6
        pxor    xmm1,xmm3
        xor     r14d,r11d
        xor     r15d,r9d
        psrld   xmm8,3
        add     r15d,r13d
        add     r15d,[rsp+14H]
        ror     r14d,2
        pxor    xmm1,xmm2
        mov     r13d,r11d
        add     r10d,r15d
        mov     r15d,r11d
        pxor    xmm1,xmm8
        or      r13d,ebx
        add     edi,r10d
        and     r15d,ebx
        pshufd  xmm2,xmm7,0FAH
        and     r13d,eax
        add     r10d,r14d
        paddd   xmm0,xmm1
        or      r13d,r15d
        add     r10d,r13d
        movdqa  xmm3,xmm2
        mov     r13d,edi
        mov     r14d,r10d
        ror     r13d,14
        movdqa  xmm8,xmm2
        xor     r13d,edi
        ror     r14d,9
        mov     r15d,esi
        xor     r14d,r10d
        ror     r13d,5
        psrlq   xmm2,17
        xor     r15d,r8d
        psrlq   xmm3,19
        xor     r13d,edi
        and     r15d,edi
        psrld   xmm8,10
        ror     r14d,11
        xor     r14d,r10d
        xor     r15d,r8d
        ror     r13d,6
        pxor    xmm2,xmm3
        add     r15d,r13d
        ror     r14d,2
        add     r15d,[rsp+18H]
        pxor    xmm8,xmm2
        mov     r13d,r10d
        add     r9d,r15d
        mov     r15d,r10d
        pshufb  xmm8,xmm10
        or      r13d,eax
        add     ebx,r9d
        and     r15d,eax
        paddd   xmm0,xmm8
        and     r13d,r11d
        add     r9d,r14d
        pshufd  xmm2,xmm0,50H
        or      r13d,r15d
        add     r9d,r13d
        movdqa  xmm3,xmm2
        mov     r13d,ebx
        ror     r13d,14
        mov     r14d,r9d
        movdqa  xmm4,xmm2
        ror     r14d,9
        xor     r13d,ebx
        mov     r15d,edi
        ror     r13d,5
        psrlq   xmm2,17
        xor     r14d,r9d
        xor     r15d,esi
        psrlq   xmm3,19
        xor     r13d,ebx
        and     r15d,ebx
        ror     r14d,11
        psrld   xmm4,10
        xor     r14d,r9d
        ror     r13d,6
        xor     r15d,esi
        pxor    xmm2,xmm3
        ror     r14d,2
        add     r15d,r13d
        add     r15d,[rsp+1CH]
        pxor    xmm4,xmm2
        mov     r13d,r9d
        add     r8d,r15d
        mov     r15d,r9d
        pshufb  xmm4,xmm11
        or      r13d,r11d
        add     eax,r8d
        and     r15d,r11d
        paddd   xmm4,xmm0
        and     r13d,r10d
        add     r8d,r14d
        or      r13d,r15d
        add     r8d,r13d
        movdqa  xmm9,[rbp+10H]
        paddd   xmm9,xmm5
        movdqa  [rsp+10H],xmm9
        movdqa  xmm0,xmm4
        mov     r13d,eax
        ror     r13d,14
        mov     r14d,r8d
        palignr xmm0,xmm7,04H
        ror     r14d,9
        xor     r13d,eax
        mov     r15d,ebx
        ror     r13d,5
        movdqa  xmm1,xmm6
        xor     r14d,r8d
        xor     r15d,edi
        paddd   xmm0,xmm5
        xor     r13d,eax
        and     r15d,eax
        ror     r14d,11
        palignr xmm1,xmm5,04H
        xor     r14d,r8d
        ror     r13d,6
        xor     r15d,edi
        movdqa  xmm2,xmm1
        ror     r14d,2
        add     r15d,r13d
        add     r15d,[rsp+10H]
        movdqa  xmm3,xmm1
        mov     r13d,r8d
        add     esi,r15d
        mov     r15d,r8d
        pslld   xmm1,25
        or      r13d,r10d
        add     r11d,esi
        and     r15d,r10d
        psrld   xmm2,7
        and     r13d,r9d
        add     esi,r14d
        por     xmm1,xmm2
        or      r13d,r15d
        add     esi,r13d
        movdqa  xmm2,xmm3
        mov     r13d,r11d
        mov     r14d,esi
        movdqa  xmm8,xmm3
        ror     r13d,14
        xor     r13d,r11d
        mov     r15d,eax
        ror     r14d,9
        pslld   xmm3,14
        xor     r14d,esi
        ror     r13d,5
        xor     r15d,ebx
        psrld   xmm2,18
        ror     r14d,11
        xor     r13d,r11d
        and     r15d,r11d
        ror     r13d,6
        pxor    xmm1,xmm3
        xor     r14d,esi
        xor     r15d,ebx
        psrld   xmm8,3
        add     r15d,r13d
        add     r15d,[rsp+14H]
        ror     r14d,2
        pxor    xmm1,xmm2
        mov     r13d,esi
        add     edi,r15d
        mov     r15d,esi
        pxor    xmm1,xmm8
        or      r13d,r9d
        add     r10d,edi
        and     r15d,r9d
        pshufd  xmm2,xmm4,0FAH
        and     r13d,r8d
        add     edi,r14d
        paddd   xmm0,xmm1
        or      r13d,r15d
        add     edi,r13d
        movdqa  xmm3,xmm2
        mov     r13d,r10d
        mov     r14d,edi
        ror     r13d,14
        movdqa  xmm8,xmm2
        xor     r13d,r10d
        ror     r14d,9
        mov     r15d,r11d
        xor     r14d,edi
        ror     r13d,5
        psrlq   xmm2,17
        xor     r15d,eax
        psrlq   xmm3,19
        xor     r13d,r10d
        and     r15d,r10d
        psrld   xmm8,10
        ror     r14d,11
        xor     r14d,edi
        xor     r15d,eax
        ror     r13d,6
        pxor    xmm2,xmm3
        add     r15d,r13d
        ror     r14d,2
        add     r15d,[rsp+18H]
        pxor    xmm8,xmm2
        mov     r13d,edi
        add     ebx,r15d
        mov     r15d,edi
        pshufb  xmm8,xmm10
        or      r13d,r8d
        add     r9d,ebx
        and     r15d,r8d
        paddd   xmm0,xmm8
        and     r13d,esi
        add     ebx,r14d
        pshufd  xmm2,xmm0,50H
        or      r13d,r15d
        add     ebx,r13d
        movdqa  xmm3,xmm2
        mov     r13d,r9d
        ror     r13d,14
        mov     r14d,ebx
        movdqa  xmm5,xmm2
        ror     r14d,9
        xor     r13d,r9d
        mov     r15d,r10d
        ror     r13d,5
        psrlq   xmm2,17
        xor     r14d,ebx
        xor     r15d,r11d
        psrlq   xmm3,19
        xor     r13d,r9d
        and     r15d,r9d
        ror     r14d,11
        psrld   xmm5,10
        xor     r14d,ebx
        ror     r13d,6
        xor     r15d,r11d
        pxor    xmm2,xmm3
        ror     r14d,2
        add     r15d,r13d
        add     r15d,[rsp+1CH]
        pxor    xmm5,xmm2
        mov     r13d,ebx
        add     eax,r15d
        mov     r15d,ebx
        pshufb  xmm5,xmm11
        or      r13d,esi
        add     r8d,eax
        and     r15d,esi
        paddd   xmm5,xmm0
        and     r13d,edi
        add     eax,r14d
        or      r13d,r15d
        add     eax,r13d
        movdqa  xmm9,[rbp+20H]
        paddd   xmm9,xmm6
        movdqa  [rsp+10H],xmm9
        movdqa  xmm0,xmm5
        mov     r13d,r8d
        ror     r13d,14
        mov     r14d,eax
        palignr xmm0,xmm4,04H
        ror     r14d,9
        xor     r13d,r8d
        mov     r15d,r9d
        ror     r13d,5
        movdqa  xmm1,xmm7
        xor     r14d,eax
        xor     r15d,r10d
        paddd   xmm0,xmm6
        xor     r13d,r8d
        and     r15d,r8d
        ror     r14d,11
        palignr xmm1,xmm6,04H
        xor     r14d,eax
        ror     r13d,6
        xor     r15d,r10d
        movdqa  xmm2,xmm1
        ror     r14d,2
        add     r15d,r13d
        add     r15d,[rsp+10H]
        movdqa  xmm3,xmm1
        mov     r13d,eax
        add     r11d,r15d
        mov     r15d,eax
        pslld   xmm1,25
        or      r13d,edi
        add     esi,r11d
        and     r15d,edi
        psrld   xmm2,7
        and     r13d,ebx
        add     r11d,r14d
        por     xmm1,xmm2
        or      r13d,r15d
        add     r11d,r13d
        movdqa  xmm2,xmm3
        mov     r13d,esi
        mov     r14d,r11d
        movdqa  xmm8,xmm3
        ror     r13d,14
        xor     r13d,esi
        mov     r15d,r8d
        ror     r14d,9
        pslld   xmm3,14
        xor     r14d,r11d
        ror     r13d,5
        xor     r15d,r9d
        psrld   xmm2,18
        ror     r14d,11
        xor     r13d,esi
        and     r15d,esi
        ror     r13d,6
        pxor    xmm1,xmm3
        xor     r14d,r11d
        xor     r15d,r9d
        psrld   xmm8,3
        add     r15d,r13d
        add     r15d,[rsp+14H]
        ror     r14d,2
        pxor    xmm1,xmm2
        mov     r13d,r11d
        add     r10d,r15d
        mov     r15d,r11d
        pxor    xmm1,xmm8
        or      r13d,ebx
        add     edi,r10d
        and     r15d,ebx
        pshufd  xmm2,xmm5,0FAH
        and     r13d,eax
        add     r10d,r14d
        paddd   xmm0,xmm1
        or      r13d,r15d
        add     r10d,r13d
        movdqa  xmm3,xmm2
        mov     r13d,edi
        mov     r14d,r10d
        ror     r13d,14
        movdqa  xmm8,xmm2
        xor     r13d,edi
        ror     r14d,9
        mov     r15d,esi
        xor     r14d,r10d
        ror     r13d,5
        psrlq   xmm2,17
        xor     r15d,r8d
        psrlq   xmm3,19
        xor     r13d,edi
        and     r15d,edi
        psrld   xmm8,10
        ror     r14d,11
        xor     r14d,r10d
        xor     r15d,r8d
        ror     r13d,6
        pxor    xmm2,xmm3
        add     r15d,r13d
        ror     r14d,2
        add     r15d,[rsp+18H]
        pxor    xmm8,xmm2
        mov     r13d,r10d
        add     r9d,r15d
        mov     r15d,r10d
        pshufb  xmm8,xmm10
        or      r13d,eax
        add     ebx,r9d
        and     r15d,eax
        paddd   xmm0,xmm8
        and     r13d,r11d
        add     r9d,r14d
        pshufd  xmm2,xmm0,50H
        or      r13d,r15d
        add     r9d,r13d
        movdqa  xmm3,xmm2
        mov     r13d,ebx
        ror     r13d,14
        mov     r14d,r9d
        movdqa  xmm6,xmm2
        ror     r14d,9
        xor     r13d,ebx
        mov     r15d,edi
        ror     r13d,5
        psrlq   xmm2,17
        xor     r14d,r9d
        xor     r15d,esi
        psrlq   xmm3,19
        xor     r13d,ebx
        and     r15d,ebx
        ror     r14d,11
        psrld   xmm6,10
        xor     r14d,r9d
        ror     r13d,6
        xor     r15d,esi
        pxor    xmm2,xmm3
        ror     r14d,2
        add     r15d,r13d
        add     r15d,[rsp+1CH]
        pxor    xmm6,xmm2
        mov     r13d,r9d
        add     r8d,r15d
        mov     r15d,r9d
        pshufb  xmm6,xmm11
        or      r13d,r11d
        add     eax,r8d
        and     r15d,r11d
        paddd   xmm6,xmm0
        and     r13d,r10d
        add     r8d,r14d
        or      r13d,r15d
        add     r8d,r13d
        movdqa  xmm9,[rbp+30H]
        paddd   xmm9,xmm7
        movdqa  [rsp+10H],xmm9
        add     rbp,64
        movdqa  xmm0,xmm6
        mov     r13d,eax
        ror     r13d,14
        mov     r14d,r8d
        palignr xmm0,xmm5,04H
        ror     r14d,9
        xor     r13d,eax
        mov     r15d,ebx
        ror     r13d,5
        movdqa  xmm1,xmm4
        xor     r14d,r8d
        xor     r15d,edi
        paddd   xmm0,xmm7
        xor     r13d,eax
        and     r15d,eax
        ror     r14d,11
        palignr xmm1,xmm7,04H
        xor     r14d,r8d
        ror     r13d,6
        xor     r15d,edi
        movdqa  xmm2,xmm1
        ror     r14d,2
        add     r15d,r13d
        add     r15d,[rsp+10H]
        movdqa  xmm3,xmm1
        mov     r13d,r8d
        add     esi,r15d
        mov     r15d,r8d
        pslld   xmm1,25
        or      r13d,r10d
        add     r11d,esi
        and     r15d,r10d
        psrld   xmm2,7
        and     r13d,r9d
        add     esi,r14d
        por     xmm1,xmm2
        or      r13d,r15d
        add     esi,r13d
        movdqa  xmm2,xmm3
        mov     r13d,r11d
        mov     r14d,esi
        movdqa  xmm8,xmm3
        ror     r13d,14
        xor     r13d,r11d
        mov     r15d,eax
        ror     r14d,9
        pslld   xmm3,14
        xor     r14d,esi
        ror     r13d,5
        xor     r15d,ebx
        psrld   xmm2,18
        ror     r14d,11
        xor     r13d,r11d
        and     r15d,r11d
        ror     r13d,6
        pxor    xmm1,xmm3
        xor     r14d,esi
        xor     r15d,ebx
        psrld   xmm8,3
        add     r15d,r13d
        add     r15d,[rsp+14H]
        ror     r14d,2
        pxor    xmm1,xmm2
        mov     r13d,esi
        add     edi,r15d
        mov     r15d,esi
        pxor    xmm1,xmm8
        or      r13d,r9d
        add     r10d,edi
        and     r15d,r9d
        pshufd  xmm2,xmm6,0FAH
        and     r13d,r8d
        add     edi,r14d
        paddd   xmm0,xmm1
        or      r13d,r15d
        add     edi,r13d
        movdqa  xmm3,xmm2
        mov     r13d,r10d
        mov     r14d,edi
        ror     r13d,14
        movdqa  xmm8,xmm2
        xor     r13d,r10d
        ror     r14d,9
        mov     r15d,r11d
        xor     r14d,edi
        ror     r13d,5
        psrlq   xmm2,17
        xor     r15d,eax
        psrlq   xmm3,19
        xor     r13d,r10d
        and     r15d,r10d
        psrld   xmm8,10
        ror     r14d,11
        xor     r14d,edi
        xor     r15d,eax
        ror     r13d,6
        pxor    xmm2,xmm3
        add     r15d,r13d
        ror     r14d,2
        add     r15d,[rsp+18H]
        pxor    xmm8,xmm2
        mov     r13d,edi
        add     ebx,r15d
        mov     r15d,edi
        pshufb  xmm8,xmm10
        or      r13d,r8d
        add     r9d,ebx
        and     r15d,r8d
        paddd   xmm0,xmm8
        and     r13d,esi
        add     ebx,r14d
        pshufd  xmm2,xmm0,50H
        or      r13d,r15d
        add     ebx,r13d
        movdqa  xmm3,xmm2
        mov     r13d,r9d
        ror     r13d,14
        mov     r14d,ebx
        movdqa  xmm7,xmm2
        ror     r14d,9
        xor     r13d,r9d
        mov     r15d,r10d
        ror     r13d,5
        psrlq   xmm2,17
        xor     r14d,ebx
        xor     r15d,r11d
        psrlq   xmm3,19
        xor     r13d,r9d
        and     r15d,r9d
        ror     r14d,11
        psrld   xmm7,10
        xor     r14d,ebx
        ror     r13d,6
        xor     r15d,r11d
        pxor    xmm2,xmm3
        ror     r14d,2
        add     r15d,r13d
        add     r15d,[rsp+1CH]
        pxor    xmm7,xmm2
        mov     r13d,ebx
        add     eax,r15d
        mov     r15d,ebx
        pshufb  xmm7,xmm11
        or      r13d,esi
        add     r8d,eax
        and     r15d,esi
        paddd   xmm7,xmm0
        and     r13d,edi
        add     eax,r14d
        or      r13d,r15d
        add     eax,r13d
        sub     rcx,1
        jne     @loop1
        mov     rcx,2
@loop2: paddd   xmm4,[rbp]
        movdqa  [rsp+10H],xmm4
        mov     r13d,r8d
        ror     r13d,14
        mov     r14d,eax
        xor     r13d,r8d
        ror     r14d,9
        mov     r15d,r9d
        xor     r14d,eax
        ror     r13d,5
        xor     r15d,r10d
        xor     r13d,r8d
        ror     r14d,11
        and     r15d,r8d
        xor     r14d,eax
        ror     r13d,6
        xor     r15d,r10d
        add     r15d,r13d
        ror     r14d,2
        add     r15d,[rsp+10H]
        mov     r13d,eax
        add     r11d,r15d
        mov     r15d,eax
        or      r13d,edi
        add     esi,r11d
        and     r15d,edi
        and     r13d,ebx
        add     r11d,r14d
        or      r13d,r15d
        add     r11d,r13d
        mov     r13d,esi
        ror     r13d,14
        mov     r14d,r11d
        xor     r13d,esi
        ror     r14d,9
        mov     r15d,r8d
        xor     r14d,r11d
        ror     r13d,5
        xor     r15d,r9d
        xor     r13d,esi
        ror     r14d,11
        and     r15d,esi
        xor     r14d,r11d
        ror     r13d,6
        xor     r15d,r9d
        add     r15d,r13d
        ror     r14d,2
        add     r15d,[rsp+14H]
        mov     r13d,r11d
        add     r10d,r15d
        mov     r15d,r11d
        or      r13d,ebx
        add     edi,r10d
        and     r15d,ebx
        and     r13d,eax
        add     r10d,r14d
        or      r13d,r15d
        add     r10d,r13d
        mov     r13d,edi
        ror     r13d,14
        mov     r14d,r10d
        xor     r13d,edi
        ror     r14d,9
        mov     r15d,esi
        xor     r14d,r10d
        ror     r13d,5
        xor     r15d,r8d
        xor     r13d,edi
        ror     r14d,11
        and     r15d,edi
        xor     r14d,r10d
        ror     r13d,6
        xor     r15d,r8d
        add     r15d,r13d
        ror     r14d,2
        add     r15d,[rsp+18H]
        mov     r13d,r10d
        add     r9d,r15d
        mov     r15d,r10d
        or      r13d,eax
        add     ebx,r9d
        and     r15d,eax
        and     r13d,r11d
        add     r9d,r14d
        or      r13d,r15d
        add     r9d,r13d
        mov     r13d,ebx
        ror     r13d,14
        mov     r14d,r9d
        xor     r13d,ebx
        ror     r14d,9
        mov     r15d,edi
        xor     r14d,r9d
        ror     r13d,5
        xor     r15d,esi
        xor     r13d,ebx
        ror     r14d,11
        and     r15d,ebx
        xor     r14d,r9d
        ror     r13d,6
        xor     r15d,esi
        add     r15d,r13d
        ror     r14d,2
        add     r15d,[rsp+1CH]
        mov     r13d,r9d
        add     r8d,r15d
        mov     r15d,r9d
        or      r13d,r11d
        add     eax,r8d
        and     r15d,r11d
        and     r13d,r10d
        add     r8d,r14d
        or      r13d,r15d
        add     r8d,r13d
        paddd   xmm5,[rbp+10H]
        movdqa  [rsp+10H],xmm5
        add     rbp,32
        mov     r13d,eax
        ror     r13d,14
        mov     r14d,r8d
        xor     r13d,eax
        ror     r14d,9
        mov     r15d,ebx
        xor     r14d,r8d
        ror     r13d,5
        xor     r15d,edi
        xor     r13d,eax
        ror     r14d,11
        and     r15d,eax
        xor     r14d,r8d
        ror     r13d,6
        xor     r15d,edi
        add     r15d,r13d
        ror     r14d,2
        add     r15d,[rsp+10H]
        mov     r13d,r8d
        add     esi,r15d
        mov     r15d,r8d
        or      r13d,r10d
        add     r11d,esi
        and     r15d,r10d
        and     r13d,r9d
        add     esi,r14d
        or      r13d,r15d
        add     esi,r13d
        mov     r13d,r11d
        ror     r13d,14
        mov     r14d,esi
        xor     r13d,r11d
        ror     r14d,9
        mov     r15d,eax
        xor     r14d,esi
        ror     r13d,5
        xor     r15d,ebx
        xor     r13d,r11d
        ror     r14d,11
        and     r15d,r11d
        xor     r14d,esi
        ror     r13d,6
        xor     r15d,ebx
        add     r15d,r13d
        ror     r14d,2
        add     r15d,[rsp+14H]
        mov     r13d,esi
        add     edi,r15d
        mov     r15d,esi
        or      r13d,r9d
        add     r10d,edi
        and     r15d,r9d
        and     r13d,r8d
        add     edi,r14d
        or      r13d,r15d
        add     edi,r13d
        mov     r13d,r10d
        ror     r13d,14
        mov     r14d,edi
        xor     r13d,r10d
        ror     r14d,9
        mov     r15d,r11d
        xor     r14d,edi
        ror     r13d,5
        xor     r15d,eax
        xor     r13d,r10d
        ror     r14d,11
        and     r15d,r10d
        xor     r14d,edi
        ror     r13d,6
        xor     r15d,eax
        add     r15d,r13d
        ror     r14d,2
        add     r15d,[rsp+18H]
        mov     r13d,edi
        add     ebx,r15d
        mov     r15d,edi
        or      r13d,r8d
        add     r9d,ebx
        and     r15d,r8d
        and     r13d,esi
        add     ebx,r14d
        or      r13d,r15d
        add     ebx,r13d
        mov     r13d,r9d
        ror     r13d,14
        mov     r14d,ebx
        xor     r13d,r9d
        ror     r14d,9
        mov     r15d,r10d
        xor     r14d,ebx
        ror     r13d,5
        xor     r15d,r11d
        xor     r13d,r9d
        ror     r14d,11
        and     r15d,r9d
        xor     r14d,ebx
        ror     r13d,6
        xor     r15d,r11d
        add     r15d,r13d
        ror     r14d,2
        add     r15d,[rsp+1CH]
        mov     r13d,ebx
        add     eax,r15d
        mov     r15d,ebx
        or      r13d,esi
        add     r8d,eax
        and     r15d,esi
        and     r13d,edi
        add     eax,r14d
        or      r13d,r15d
        add     eax,r13d
        movdqa  xmm4,xmm6
        movdqa  xmm5,xmm7
        dec     rcx
        jne     @loop2
        add     eax,[rdx]
        mov     [rdx],eax
        add     ebx,[rdx+4H]
        add     edi,[rdx+8H]
        add     esi,[rdx+0CH]
        add     r8d,[rdx+10H]
        add     r9d,[rdx+14H]
        add     r10d,[rdx+18H]
        add     r11d,[rdx+1CH]
        mov     [rdx+4H],ebx
        mov     [rdx+8H],edi
        mov     [rdx+0CH],esi
        mov     [rdx+10H],r8d
        mov     [rdx+14H],r9d
        mov     [rdx+18H],r10d
        mov     [rdx+1CH],r11d
        mov     rcx,[rsp+8H]
        add     rcx,64
        cmp     rcx,[rsp]
        jne     @loop0
@done: {$ifndef LINUX}
        movdqa  xmm6,[rsp+20H]
        movdqa  xmm7,[rsp+30H]
        movdqa  xmm8,[rsp+40H]
        movdqa  xmm9,[rsp+50H]
        movdqa  xmm10,[rsp+60H]
        movdqa  xmm11,[rsp+70H]
        movdqa  xmm12,[rsp+80H]
        {$endif}
        add     rsp,STACK_SIZE
        pop     r15
        pop     r14
        pop     r13
        pop     rbp
        {$ifndef LINUX}
        pop     rdi
        pop     rsi
        {$endif}
        pop     rbx
end;
{$endif CPU64}

procedure TSHA256.Compress;
// Actual hashing function
var H: TSHAHash;
    W: array[0..63] of cardinal;
    {$ifdef PUREPASCAL}
    i: integer;
    t1, t2: cardinal;
    {$endif}
begin
  {$ifdef CPUX64}
  if cfSSE41 in CpuFeatures then begin
    if K256Aligned='' then 
      SetString(K256Aligned,PAnsiChar(@K256),SizeOf(K256));
    if PtrUInt(K256ALigned)and 15=0 then begin
      sha256_sse4(TSHAContext(Context).Buffer,TSHAContext(Context).Hash,1);
      exit;
    end; // if K256Aligned[] is not properly aligned -> fallback to pascal
  end;
  {$endif CPU64}

  // Calculate "expanded message blocks"
  Sha256ExpandMessageBlocks(@W,@TSHAContext(Context).Buffer);

  // Assign old working hash to local variables A..H
  with TSHAContext(Context) do begin
    H.A := Hash.A;
    H.B := Hash.B;
    H.C := Hash.C;
    H.D := Hash.D;
    H.E := Hash.E;
    H.F := Hash.F;
    H.G := Hash.G;
    H.H := Hash.H;
  end;

{$ifdef PUREPASCAL}
  // SHA256 compression function
  for i := 0 to high(W) do begin
    t1 := H.H+(((H.E shr 6)or(H.E shl 26))xor((H.E shr 11)or(H.E shl 21))xor
      ((H.E shr 25)or(H.E shl 7)))+((H.E and H.F)xor(not H.E and H.G))+K256[i]+W[i];
    t2 := (((H.A shr 2)or(H.A shl 30))xor((H.A shr 13)or(H.A shl 19))xor
      ((H.A shr 22)xor(H.A shl 10)))+((H.A and H.B)xor(H.A and H.C)xor(H.B and H.C));
    H.H := H.G; H.G := H.F; H.F := H.E; H.E := H.D+t1;
    H.D := H.C; H.C := H.B; H.B := H.A; H.A := t1+t2;
  end;
{$else}
  // SHA256 compression function - optimized by A.B. for pipelined CPU
  asm
    push ebx
    push esi
    push edi
    xor  edi,edi // edi=i
    // rolled version faster than the unrolled one (good pipelining work :)
@s: mov  eax,[H].TSHAHash.E
    mov  ecx,eax
    mov  edx,eax
    mov  ebx,eax // ebx=E
    ror  eax,6
    ror  edx,11
    ror  ecx,25
    xor  eax,edx
    mov  edx,[H].TSHAHash.G
    xor  eax,ecx
    mov  ecx,[H].TSHAHash.H
    add  ecx,eax // T1=ecx
    mov  eax,[H].TSHAHash.F
    mov  [H].TSHAHash.H,edx
    mov  [H].TSHAHash.G,eax
    xor  eax,edx
    mov  [H].TSHAHash.F,ebx
    and  eax,ebx
    xor  eax,edx
    add  eax,dword ptr [K256+edi*4]
    add  eax,ecx
    mov  ecx,[H].TSHAHash.D
    add  eax,dword ptr [W+edi*4]
    mov  ebx,[H].TSHAHash.A
    //  eax= T1 := H + Sum1(E) +(((F xor G) and E) xor G)+K256[i]+W[i];
    add  ecx,eax
    mov  esi,eax  // esi = T1
    mov  [H].TSHAHash.E,ecx // E := D + T1;
    mov  eax,ebx // Sum0(A)
    mov  edx,ebx
    ror  eax,2
    mov  ecx,ebx
    ror  edx,13
    ror  ecx,22
    xor  eax,edx
    xor  eax,ecx // eax = Sum0(A)
    mov  ecx,[H].TSHAHash.B
    add  esi,eax
    mov  eax,ebx // ebx=A
    mov  edx,ebx // eax=edx=A
    or   eax,ecx
    and  eax,[H].TSHAHash.C   // eax = (A or B)and C
    and  edx,ecx
    or   eax,edx // eax = ((A or B)and C) or (A and B)
    inc  edi
    add  esi,eax  // esi= T1+T2
    mov  [H].TSHAHash.A,esi // all these instructions are pipelined -> roll OK
    mov  eax,[H].TSHAHash.C // eax=C ecx=B ebx=A
    mov  [H].TSHAHash.B,ebx
    mov  [H].TSHAHash.C,ecx
    mov  [H].TSHAHash.D,eax
    cmp  edi,64
    jnz  @s
    pop  edi
    pop  esi
    pop  ebx
  end;
{$endif PUREPASCAL}

  // Calculate new working hash
  with TSHAContext(Context) do begin
    inc(Hash.A,H.A);
    inc(Hash.B,H.B);
    inc(Hash.C,H.C);
    inc(Hash.D,H.D);
    inc(Hash.E,H.E);
    inc(Hash.F,H.F);
    inc(Hash.G,H.G);
    inc(Hash.H,H.H);
  end;
end;

procedure TSHA256.Final(out Digest: TSHA256Digest);
// finalize SHA256 calculation, clear context
var Data: TSHAContext absolute Context;
begin
  // Message padding
  // 1. append bit '1' after Buffer
  Data.Buffer[Data.Index]:= $80;
  fillchar(Data.Buffer[Data.Index+1],63-Data.Index,0);
  // 2. Compress if more than 448 bits, (no room for 64 bit length
  if Data.Index>=56 then begin
    Compress;
    fillchar(Data.Buffer,56,0);
  end;
  // Write 64 bit Buffer length into the last bits of the last block
  // (in big endian format) and do a final compress
  PInteger(@Data.Buffer[56])^ := bswap32(Int64Rec(Data.MLen).Hi);
  PInteger(@Data.Buffer[60])^ := bswap32(Int64Rec(Data.MLen).Lo);
  Compress;
  // Hash -> Digest to little endian format
  bswap256(@Data.Hash,@Digest);
  // Clear Data
  Init;
end;

procedure TSHA256.Full(Buffer: pointer; Len: integer; out Digest: TSHA256Digest);
begin
{$ifdef USEPADLOCK}
  // Padlock need all data once -> Full() is OK, not successive Update()
  if padlock_available then begin
    Init; // for later Update use
    {$ifdef PADLOCKDEBUG}write('padlock_phe_sha256 ');{$endif}
    if padlock_phe_sha256(buffer,Len,Digest)=0 then
      exit else
    {$ifdef PADLOCKDEBUG}write(':ERROR ');{$endif}
  end;
{$endif}
  Init;
  Update(Buffer,Len);
  Final(Digest);
end;

procedure TSHA256.Init;
// initialize context
var Data: TSHAContext absolute Context;
begin
  fillchar(Data,sizeof(Data),0);
  Data.Hash.A := $6a09e667;
  Data.Hash.B := $bb67ae85;
  Data.Hash.C := $3c6ef372;
  Data.Hash.D := $a54ff53a;
  Data.Hash.E := $510e527f;
  Data.Hash.F := $9b05688c;
  Data.Hash.G := $1f83d9ab;
  Data.Hash.H := $5be0cd19;
end;

procedure TSHA256.Update(Buffer: pointer; Len: integer);
var Data: TSHAContext absolute Context;
    aLen: integer;
begin
  if Buffer=nil then exit; // avoid GPF
  inc(Data.MLen,Int64(cardinal(Len)) shl 3);
  while Len>0 do begin
    aLen := 64-Data.Index;
    if aLen<=Len then begin
      move(buffer^,Data.Buffer[Data.Index],aLen);
      dec(Len,aLen);
      inc(PtrInt(buffer),aLen);
      Compress;
      Data.Index := 0;
    end else begin
      move(buffer^,Data.Buffer[Data.Index],Len);
      inc(Data.Index,Len);
      break;
    end;
  end;
end;

procedure SHA256Weak(const s: RawByteString; out Digest: TSHA256Digest);
var L: integer;
    SHA: TSHA256;
    p: PAnsiChar;
    tmp: array[0..255] of byte;
begin
  L := length(s);
  p := pointer(s);
  if L<sizeof(tmp) then begin
    fillchar(tmp,sizeof(tmp),L); // add some salt to unweak password
    if L>0 then
      move(p^,tmp,L);
    SHA.Full(@tmp,sizeof(tmp),Digest);
  end else
    SHA.Full(p,L,Digest);
end;

procedure AES(const Key; KeySize: cardinal; buffer: pointer; Len: Integer; Encrypt: boolean);
begin
  AES(Key,KeySize,buffer,buffer,Len,Encrypt);
end;

procedure AES(const Key; KeySize: cardinal; bIn, bOut: pointer; Len: Integer; Encrypt: boolean);
var n: integer;
    pIn, pOut: PAESBlock;
    Crypt: TAES;
begin
  if (bIn=nil) or (bOut=nil) then exit;
  // 1. Init
  n := Len div AESBlockSize;
  if n<0 then exit else
  if n>0 then
    if (KeySize>4) and not Crypt.DoInit(Key,KeySize,Encrypt) then
      KeySize := 4; // if error in KeySize, use default fast XorOffset()
  if KeySize=0 then begin // KeySize=0 -> no encryption -> direct copy
    move(bIn^, bOut^, Len);
    exit;
  end;
  if n<1 then begin // too small for AES -> XorOffset() remaining 0..15 bytes
    move(bIn^, bOut^, Len);
    XorOffset(bOut,0,Len);
    exit;
  end;
  // 2. All full blocks, with AES
{$ifdef USETHREADSFORBIGAESBLOCKS}
  pIn := bIn;
  pOut := bOut;
  Crypt.DoBlocksThread(pIn,pOut,n,Encrypt);
{$else}
  Crypt.DoBlocks(bIn,bOut,pIn,pOut,n,Encrypt);
{$endif}
  // 3. Last block, just XORed from Key
//  assert(KeySize div 8>=AESBlockSize);
  n := cardinal(Len) mod AESBlockSize;
  move(pIn^,pOut^,n); // pIn=pOut is tested in move()
  XorOffset(pointer(pOut),Len-n,n);
{$ifdef USEPADLOCK}
  Crypt.Done; // use for Padlock support
{$endif}
end;

const TmpSize = 65536;
  // Tmp buffer for AESFull -> Xor Crypt is TmpSize-dependent / use XorBlock()
      TmpSizeBlock = TmpSize div AESBlockSize;
type
  TTmp = array[0..TmpSizeBlock-1] of TAESBlock;

function AES(const Key; KeySize: cardinal; const s: RawByteString; Encrypt: boolean): RawByteString;
begin
  SetString(result,nil,length(s));
  if s<>'' then
    AES(Key,KeySize,pointer(s),pointer(result),length(s),Encrypt);
end;

function AES(const Key; KeySize: cardinal; buffer: pointer; Len: cardinal; Stream: TStream; Encrypt: boolean): boolean; overload;
var buf: pointer;
    last, b, n, i: cardinal;
    Crypt: TAES;
begin
  result := false;
  if buffer=nil then exit;
  if (KeySize>4) and not Crypt.DoInit(Key,KeySize,Encrypt) then
    KeySize := 4; // if error in KeySize, use default fast XorOffset()
  if KeySize=0 then begin // no Crypt -> direct write to dest Stream
    Stream.Write(buffer^,Len);
    result := true;
    exit;
  end;
  getmem(buf,TmpSize);
  try
    Last := Len mod AESBlockSize;
    n := Len-Last;
    i := 0;
    while n>0 do begin // crypt/uncrypt all AESBlocks
      if n>TmpSize then
        b := TmpSize else
        b := n;
      assert(b mod AESBlockSize=0);
      if KeySize=4 then begin
        move(buffer^,buf^,b);
        XorOffset(pointer(buf),i,b);
        inc(i,b);
      end else
        Crypt.DoBlocks(buffer,buf,b div AESBlockSize,Encrypt);
      Stream.Write(buf^,b);
      inc(PtrUInt(buffer),b);
      dec(n,b);
    end;
    assert((KeySize>4)or(i=Len-Last));
    if last>0 then begin // crypt/uncrypt (Xor) last 0..15 bytes
      move(buffer^,buf^,Last);
      XorOffset(pointer(buf),Len-Last,Last);
      Stream.Write(buf^,Last);
    end;
    result := true;
  finally
    freemem(buf);
  end;
end;

function KeyFrom(const Key; KeySize: cardinal): cardinal;
begin
  case KeySize div 8 of
  0:   result := 0;
  1:   result := pByte(@Key)^;
  2,3: result := pWord(@Key)^;
  else result := PInteger(@Key)^;
  end;
end;

function TAESFullHeader.Calc(const Key; KeySize: cardinal): cardinal;
begin
  result := Adler32Asm(KeySize,@Key,KeySize shr 3) xor Te0[OriginalLen and $FF]
    xor Te1[SourceLen and $FF] xor Td0[SomeSalt and $7FF];
end;

function TAESFull.EncodeDecode(const Key; KeySize, inLen: cardinal; Encrypt: boolean;
  inStream, outStream: TStream; bIn, bOut: pointer; OriginalLen: Cardinal=0): integer;
var Tmp: ^TTmp;
    pIn, pOut: PAESBlock;
    Crypt: TAES;
    nBlock,
    XorCod: cardinal;
procedure Read(Tmp: pointer; ByteCount: cardinal);
begin
  if pIn=nil then
    InStream.Read(Tmp^,ByteCount) else begin
    move(pIn^,Tmp^,ByteCount);
    inc(PtrUInt(pIn),ByteCount);
  end;
end;
procedure Write(Tmp: pointer; ByteCount: cardinal);
begin
  if pOut=nil then
    OutStream.Write(Tmp^,ByteCount) else begin
    move(Tmp^,pOut^,ByteCount);
    inc(PtrUInt(pOut),ByteCount);
  end;
end;
procedure SetOutLen(Len: cardinal);
var P: cardinal;
begin
  result := Len; // global EncodeDecode() result
  if OutStream<>nil then begin
    if OutStream.InheritsFrom(TMemoryStream) then
      with TMemoryStream(OutStream) do begin
        P := Seek(0,soFromCurrent);
        Size := P+Len; // auto-reserve space (no Realloc:)
        Seek(P+Len,soBeginning);
        bOut := PAnsiChar(Memory)+P;
        pOut := bOut;
        OutStream := nil; //  OutStream is slower and use no thread
      end;
  end else
  if bOut=nil then begin
    outStreamCreated := THeapMemoryStream.Create; // faster than TMemoryStream
    outStreamCreated.Size := Len; // auto-reserve space (no Realloc:)
    bOut := outStreamCreated.Memory;
    pOut := bOut; // OutStream is slower and use no thread
  end;
  if KeySize=0 then exit; // no Tmp to be allocated on direct copy
{$ifdef USEPADLOCK} // PADLOCK prefers 16-bytes alignment
  if (KeySize=32) or (InStream<>nil) or (OutStream<>nil) or
     (PtrUInt(bIn) and $f<>0) or (PtrUInt(bOut) and $f<>0) then begin
    New(Tmp);
//    assert(PtrUInt(Tmp) and $F=0);
  end;
{$else}
  if (KeySize=32) or (InStream<>nil) or (OutStream<>nil) then
    New(Tmp);
{$endif}
end;
procedure DoBlock(BlockCount: integer);
begin
  if BlockCount=0 then
    exit;
  Read(Tmp,BlockCount*AESBlockSize);
  Crypt.DoBlocks(PAESBLock(Tmp),PAESBLock(Tmp),BlockCount,Encrypt);
  Write(Tmp,BlockCount*AESBlockSize);
end;
var n, LastLen: cardinal;
    i: integer;
    Last: TAESBlock;
begin
  Tmp := nil;
  outStreamCreated := nil;
  Head.SourceLen := InLen;
  nBlock := Head.SourceLen div AESBlockSize;
  if Encrypt and (OriginalLen<>0) then
    Head.OriginalLen := OriginalLen else
    Head.OriginalLen := InLen;
  KeySize := KeySize div 8;
  if not (KeySize in [0,4,16,24,32]) then
    KeySize := 0 else  // valid KeySize: 0=nothing, 32=xor, 128,192,256=AES
    KeySize := KeySize*8;
  XorCod := inLen;
  if (inStream<>nil) and inStream.InheritsFrom(TMemoryStream) then begin
    bIn := TMemoryStream(inStream).Memory;
    inStream := nil;
   end;
  pIn := bIn;
  pOut := bOut;
  if (KeySize>=128) and not Crypt.DoInit(Key,KeySize,Encrypt) then
    KeySize := 32;
  if KeySize=32 then
     XorCod := KeyFrom(Key,KeySize) xor XorCod else
  if (KeySize=0) and (InStream=nil) then begin
    SetOutLen(inLen);
    Write(bIn,inLen);  // no encryption -> direct write
    exit;
  end;
  try
    // 0. KeySize = 0:direct copy 32:XorBlock
    if KeySize<128 then begin
      SetOutLen(inLen);
      assert(Tmp<>nil);
      LastLen := inLen;
      while LastLen<>0 do begin
        if LastLen>TmpSize then
          n := TmpSize else
          n := LastLen;
        Read(Tmp,n);
        if KeySize>0 then
          XorBlock(pointer(Tmp),n,XorCod);
        Write(Tmp,n);
        dec(LastLen,n);
      end;
    end else begin // no we do AES encryption:
      // 1. Header process
      if Encrypt then begin
        // encrypt data
        if (pIn=pOut) and (pIn<>nil) then begin
          assert(false); // Head in pOut^ will overflow data in pIn^
          result := 0;
          exit;
        end;
        LastLen := inLen mod AESBlockSize;
        if LastLen=0 then
          SetOutLen(inLen+AESBlockSize) else
          SetOutLen((nBlock+2)*AESBlockSize);
        Head.SomeSalt := random(MaxInt);
        Head.HeaderCheck := Head.Calc(Key,KeySize);
        Crypt.Encrypt(TAESBlock(Head));
        Write(@Head,sizeof(Head));
      end else begin
        // uncrypt data
        dec(nBlock); // Header is already done
        Read(@Head,sizeof(Head));
        Crypt.Decrypt(TAESBlock(Head));
        with Head do begin
          if HeaderCheck<>Head.Calc(Key,KeySize) then begin
            result := -1;
            exit; // wrong key
          end;
          SetOutLen(SourceLen);
          LastLen := SourceLen mod AESBlockSize;
        end;
        if LastLen<>0 then
          dec(nBlock); // the very last block is for the very last bytes
      end;
      // 2. All full blocks, with AES
      if Tmp=nil then begin
      {$ifdef USETHREADSFORBIGAESBLOCKS} // Tmp is 64KB -> helpless Threads
        Crypt.DoBlocksThread(pIn,pOut,nBlock,Encrypt);
      {$else}
        Crypt.DoBlocks(pIn,pOut,pIn,pOut,nBlock,Encrypt);
      {$endif}
      end else begin
        for i := 1 to nBlock div TmpSizeBlock do
          DoBlock(TmpSizeBlock);
        DoBlock(nBlock mod TmpSizeBlock);
      end;
      // 3. Last block
      if LastLen<>0 then
      if Encrypt then begin
        fillchar(Last,AESBlockSize,0);
        Read(@Last,LastLen);
        Crypt.Encrypt(Last);
        Write(@Last,AESBlockSize);
      end else begin
        Read(@Last,AESBlockSize);
        Crypt.Decrypt(Last);
        Write(@Last,LastLen);
      end;
{$ifdef USEPADLOCK}
      Crypt.Done; // used for Padlock only
{$endif}
    end;
  finally
    if Tmp<>nil then
      Freemem(Tmp);
  end;
end;


function AESFullKeyOK(const Key; KeySize: cardinal; buff: pointer): boolean;
// true if begining of buff contains true AESFull encrypted data with this Key
var Crypt: TAES;
    Head: TAESFullHeader;
begin
  if KeySize<128 then
    result := true else
  if not Crypt.DecryptInit(Key,KeySize) then
    result := false else begin
    Crypt.Decrypt(PAESBlock(buff)^,TAESBlock(Head));
    result := Head.Calc(Key,KeySize)=Head.HeaderCheck;
{$ifdef USEPADLOCK}
    Crypt.Done; // for Padlock support
{$endif}
  end;
end;

function AESFull(const Key; KeySize: cardinal; bIn, bOut: pointer; Len: integer;
  Encrypt: boolean; OriginalLen: Cardinal=0): integer; overload;
// bOut must be at least bIn+32/Encrypt bIn-16/Decrypt -> returns outLength, <0 if error
var A: TAESFull;
begin
  result := A.EncodeDecode(Key,KeySize,Len,Encrypt,nil,nil,bIn,bOut,OriginalLen);
end;

function AESFull(const Key; KeySize: cardinal; bIn: pointer; Len: Integer;
   outStream: TStream; Encrypt: boolean; OriginalLen: Cardinal=0): boolean; // true is Key OK
// outStream will be larger/smaller than Len: this is a full AES version
// if not KeySize in [128,192,256] -> use very fast and Simple Xor Cypher
var A: TAESFull;
begin
  result := A.EncodeDecode(Key,KeySize,
    Len,Encrypt,nil,outStream,bIn,nil,OriginalLen)>=0;
end;

procedure AESSHA256(bIn, bOut: pointer; Len: integer; const Password: RawByteString; Encrypt: boolean);
var Digest: TSHA256Digest;
begin
  SHA256Weak(Password,Digest);
  AES(Digest,sizeof(Digest)*8,bIn,bOut,Len,Encrypt);
end;

function AESSHA256(const s, Password: RawByteString; Encrypt: boolean): RawByteString;
begin
  SetString(result,nil,length(s));
  AESSHA256(pointer(s),pointer(result),length(s),Password,Encrypt);
end;

procedure AESSHA256(Buffer: pointer; Len: integer; const Password: RawByteString; Encrypt: boolean);
// Encrypt/Decrypt Buffer with AES and SHA256 password
begin
  AESSHA256(Buffer,Buffer,Len,Password,Encrypt);
end;

procedure AESSHA256Full(bIn: pointer; Len: Integer; outStream: TStream; const Password: RawByteString; Encrypt: boolean);
// outStream will be larger/smaller than Len: this is a full AES version
var Digest: TSHA256Digest;
begin
  SHA256Weak(Password,Digest);
  AESFull(Digest,sizeof(Digest)*8,bIn,Len,outStream,Encrypt);
end;


function Adler32Pas(Adler: cardinal; p: pointer; Count: Integer): cardinal;
// simple Adler32 implementation (twice slower than Asm, but shorter code size)
var s1, s2: cardinal;
    i, n: integer;
begin
  s1 := LongRec(Adler).Lo;
  s2 := LongRec(Adler).Hi;
  while Count>0 do begin
    if Count<5552 then
      n := Count else
      n := 5552;
    for i := 1 to n do begin
      inc(s1,pByte(p)^);
      inc(PtrUInt(p));
      inc(s2,s1);
    end;
    s1 := s1 mod 65521;
    s2 := s2 mod 65521;
    dec(Count,n);
  end;
  result := word(s1)+cardinal(word(s2)) shl 16;
end;

function Adler32Asm(Adler: cardinal; p: pointer; Count: Integer): cardinal;
{$ifdef PUREPASCAL}
begin
  result := Adler32Pas(Adler,p,Count);
end;
{$else}
asm
    push  ebx
    push  esi
    push  edi
    mov   edi,eax
    shr   edi,16
    movzx ebx,ax
    push  ebp
    mov   esi,edx
    test  esi,esi
    mov   ebp,ecx
    jne   @31
    mov   eax,1
    jmp   @32
@31:test  ebp,ebp
  	jbe   @34
@33:cmp   ebp,5552
    jae    @35
    mov   eax,ebp
    jmp    @36
@35:mov   eax,5552
@36:sub   ebp,eax
    cmp   eax,16
    jl    @38
    xor   edx,edx
    xor   ecx,ecx
@39:sub   eax,16
    mov   dl,[esi]
    mov   cl,[esi+1]
    add   ebx,edx
    add   edi,ebx
    add   ebx,ecx
    mov   dl,[esi+2]
    add   edi,ebx
    add   ebx,edx
    mov   cl,[esi+3]
    add   edi,ebx
    add   ebx,ecx
    mov   dl,[esi+4]
    add   edi,ebx
    add   ebx,edx
    mov   cl,[esi+5]
    add   edi,ebx
    add   ebx,ecx
    mov   dl,[esi+6]
    add   edi,ebx
    add   ebx,edx
    mov   cl,[esi+7]
    add   edi,ebx
    add   ebx,ecx
    mov   dl,[esi+8]
    add   edi,ebx
    add   ebx,edx
    mov   cl,[esi+9]
    add   edi,ebx
    add   ebx,ecx
    mov   dl,[esi+10]
    add   edi,ebx
    add   ebx,edx
    mov   cl,[esi+11]
    add   edi,ebx
    add   ebx,ecx
    mov   dl,[esi+12]
    add   edi,ebx
    add   ebx,edx
    mov   cl,[esi+13]
    add   edi,ebx
    add   ebx,ecx
    mov   dl,[esi+14]
    add   edi,ebx
    add   ebx,edx
    mov   cl,[esi+15]
    add   edi,ebx
    add   ebx,ecx
    cmp   eax,16
    lea   esi,[esi+16]
    lea   edi,[edi+ebx]
    jge   @39
@38:test  eax,eax
  	je    @42
@43:movzx edx,byte ptr [esi]
    add   ebx,edx
    dec   eax
    lea   esi,[esi+1]
    lea   edi,[edi+ebx]
    jg    @43
@42:mov   ecx,65521
    mov   eax,ebx
    xor   edx,edx
    div   ecx
    mov   ebx,edx
    mov   ecx,65521
    mov   eax,edi
    xor   edx,edx
    div   ecx
    test  ebp,ebp
    mov   edi,edx
    ja    @33
@34:mov   eax,edi
    shl   eax,16
    or    eax,ebx
@32:pop   ebp
	  pop   edi
  	pop   esi
  	pop   ebx
end;
{$endif}

function Adler32SelfTest: boolean;
begin
  result :=
  {$ifndef PUREPASCAL}
    (Adler32Asm(1,@Te0,sizeof(Te0))=$BCBEFE10) and
    (Adler32Asm(7,@Te1,sizeof(Te1)-3)=$DA91FDBE) and
  {$endif}
    (Adler32Pas(1,@Te0,sizeof(Te0))=$BCBEFE10) and
    (Adler32Pas(7,@Te1,sizeof(Te1)-3)=$DA91FDBE);
end;


{ TAESWriteStream }

constructor TAESWriteStream.Create(outStream: TStream; const Key; KeySize: cardinal);
begin
  inherited Create;
  if KeySize=0 then
    NoCrypt := true else
    AES.EncryptInit(Key,KeySize);
  Dest := outStream;
end;

destructor TAESWriteStream.Destroy;
begin
  Finish;
{$ifdef USEPADLOCK}
  AES.Done; // useful for padlock only
{$endif}
  inherited;
end;

procedure TAESWriteStream.Finish;
begin
  if BufCount=0 then exit;
  assert((BufCount<AESBlockSize) and AES.Initialized and not NoCrypt);
  XorOffset(@Buf,DestSize,BufCount);
  Dest.Write(Buf,BufCount);
  BufCount := 0;
end;

function TAESWriteStream.Read(var Buffer; Count: Integer): Longint;
begin
  raise ESynCrypto.Create(ClassName);
end;

function TAESWriteStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  raise ESynCrypto.Create(ClassName);
end;

function TAESWriteStream.Write(const Buffer; Count: Integer): Longint;
// most of the time, a 64KB-buffered compressor have BufCount=0
// will crypt 'const Buffer' memory in place -> use AFTER T*Compressor
var B: TByteArray absolute Buffer;
    Len: integer;
begin
  result := Count;
  Adler := Adler32Asm(Adler,@Buffer,Count);
  if not NoCrypt then // KeySize=0 -> save as-is
  if not AES.Initialized then // if error in KeySize -> default fast XorOffset()
    XorOffset(@B,DestSize,Count) else begin
    if BufCount>0 then begin
      Len := AESBlockSize-BufCount;
      if Len>Count then
        Len := Count;
      move(Buffer,Buf[BufCount],Len);
      inc(BufCount,Len);
      if BufCount<AESBlockSize then
        exit;
      AES.Encrypt(Buf);
      Dest.Write(Buf,AESBlockSize);
      inc(DestSize,AESBlockSize);
      Dec(Count,Len);
      AES.DoBlocks(@B[Len],@B[Len],cardinal(Count) div AESBlockSize,true);
    end else
      AES.DoBlocks(@B,@B,cardinal(Count) div AESBlockSize,true);
    BufCount := cardinal(Count) mod AESBlockSize;
    if BufCount<>0 then begin
      dec(Count,BufCount);
      move(B[Count],Buf[0],BufCount);
    end;
  end;
  Dest.Write(Buffer,Count);
  inc(DestSize,Count);
end;


procedure XorBlock(p: PIntegerArray; Count, Cod: integer);
// very fast Xor() according to Cod - not Compression or Stream compatible
var i: integer;
begin
  for i := 1 to Count shr 4 do begin // proceed through 16 bytes blocs
    Cod := (Cod shl 11) xor integer(Td0[cod shr 21]); // shr 21 -> 8*[byte] of cardinal
    p^[0] := p^[0] xor Cod;
    p^[1] := p^[1] xor Cod;
    p^[2] := p^[2] xor Cod;
    p^[3] := p^[3] xor Cod;
    inc(PtrUInt(p),16);
  end;
  Cod := (Cod shl 11) xor integer(Td0[cod shr 21]);
  for i := 1 to (Count and 15)shr 2 do begin // last 4 bytes blocs
    p^[0] := p^[0] xor Cod;
    inc(PtrUInt(p),4);
  end;
  for i := 1 to Count and 3 do begin
    pByte(p)^ := pByte(p)^ xor byte(Cod);
    inc(PtrUInt(p));
  end;
end;

var
  Xor32Byte: TByteArray absolute Td0;  // $2000=8192 bytes of XOR tables ;)

{$ifndef PUREPASCAL} // should be put outside XorOffset() for FPC :(
procedure Xor64(PI: PIntegerArray; P: pByte; Count: integer);
asm // eax=PI edx=P ecx=Count64
  push ebx
  push esi
  shr ecx,3
  jz @z
@1:
  mov ebx,[eax]
  mov esi,[eax+4]
  xor [edx],ebx
  xor [edx+4],esi
  dec ecx
  lea eax,[eax+8]
  lea edx,[edx+8]
  jnz @1
@z:
  pop esi
  pop ebx
end;
{$endif PURE_PASCAL}

procedure XorOffset(p: pByte; Index,Count: integer);
// XorOffset: fast and simple Cypher using Index (=Position in Dest Stream):
// Compression not OK -> apply after compress (e.g. TBZCompressor.withXor=true)
var i, Len: integer;
begin
  if Count>0 then
  repeat
    Index := Index and $1FFF;
    Len := $2000-Index;
    if Len>Count then
      Len := Count;
{$ifdef PURE_PASCAL}
    for i := 1 to Len do begin
      p^ := p^ xor Xor32Byte[Index];
      inc(p); inc(Index);
    end;
{$else}
    Xor64(@Xor32Byte[Index],p,Len);
    inc(p,Len and -8); // -8=$FFFFFFF8
    inc(Index,Len and -8);
    for i := 1 to Len and 7 do begin
      p^ := p^ xor Xor32Byte[Index];
      inc(p); inc(Index);
    end;
{$endif}
    Dec(Count,Len);
  until Count=0;
end;


procedure XorConst(p: PIntegerArray; Count: integer);
// XorConst: fast Cypher changing by Count value
// (compression OK):
var i: integer;
    Code: integer;
begin // 1 to 3 bytes may stay unencrypted: not relevant
  Code := integer(Td0[Count and $3FF]);
  for i := 1 to (Count shr 4) do begin
     p^[0] := p^[0] xor Code;
     p^[1] := p^[1] xor Code;
     p^[2] := p^[2] xor Code;
     p^[3] := p^[3] xor Code;
     inc(PtrUInt(p),16);
  end;
  for i := 0 to ((Count and 15)shr 2)-1 do // last 4 bytes blocs
    p^[i] := p^[i] xor Code;
end;


{ TMD5 }

procedure MD5Transform(var buf: TMD5Buf; const in_: TMD5In);
var a,b,c,d: cardinal; // unrolled -> compiler will only use cpu registers :)
// the code below is very fast, and can be compared proudly against C or ASM
begin
  a := buf[0];
  b := buf[1];
  c := buf[2];
  d := buf[3];
  inc(a,in_[0]+$d76aa478+(d xor(b and(c xor d)))); a := ((a shl 7)or(a shr(32-7)))+b;
  inc(d,in_[1]+$e8c7b756+(c xor(a and(b xor c)))); d := ((d shl 12)or(d shr(32-12)))+a;
  inc(c,in_[2]+$242070db+(b xor(d and(a xor b)))); c := ((c shl 17)or(c shr(32-17)))+d;
  inc(b,in_[3]+$c1bdceee+(a xor(c and(d xor a)))); b := ((b shl 22)or(b shr(32-22)))+c;
  inc(a,in_[4]+$f57c0faf+(d xor(b and(c xor d)))); a := ((a shl 7)or(a shr(32-7)))+b;
  inc(d,in_[5]+$4787c62a+(c xor(a and(b xor c)))); d := ((d shl 12)or(d shr(32-12)))+a;
  inc(c,in_[6]+$a8304613+(b xor(d and(a xor b)))); c := ((c shl 17)or(c shr(32-17)))+d;
  inc(b,in_[7]+$fd469501+(a xor(c and(d xor a)))); b := ((b shl 22)or(b shr(32-22)))+c;
  inc(a,in_[8]+$698098d8+(d xor(b and(c xor d)))); a := ((a shl 7)or(a shr(32-7)))+b;
  inc(d,in_[9]+$8b44f7af+(c xor(a and(b xor c)))); d := ((d shl 12)or(d shr(32-12)))+a;
  inc(c,in_[10]+$ffff5bb1+(b xor(d and(a xor b)))); c := ((c shl 17)or(c shr(32-17)))+d;
  inc(b,in_[11]+$895cd7be+(a xor(c and(d xor a)))); b := ((b shl 22)or(b shr(32-22)))+c;
  inc(a,in_[12]+$6b901122+(d xor(b and(c xor d)))); a := ((a shl 7)or(a shr(32-7)))+b;
  inc(d,in_[13]+$fd987193+(c xor(a and(b xor c)))); d := ((d shl 12)or(d shr(32-12)))+a;
  inc(c,in_[14]+$a679438e+(b xor(d and(a xor b)))); c := ((c shl 17)or(c shr(32-17)))+d;
  inc(b,in_[15]+$49b40821+(a xor(c and(d xor a)))); b := ((b shl 22)or(b shr(32-22)))+c;
  inc(a,in_[1]+$f61e2562+(c xor(d and(b xor c))));  a := ((a shl 5)or(a shr(32-5)))+b;
  inc(d,in_[6]+$c040b340+(b xor(c and(a xor b))));  d := ((d shl 9)or(d shr(32-9)))+a;
  inc(c,in_[11]+$265e5a51+(a xor(b and(d xor a)))); c := ((c shl 14)or(c shr(32-14)))+d;
  inc(b,in_[0]+$e9b6c7aa+(d xor(a and(c xor d))));  b := ((b shl 20)or(b shr(32-20)))+c;
  inc(a,in_[5]+$d62f105d+(c xor(d and(b xor c))));  a := ((a shl 5)or(a shr(32-5)))+b;
  inc(d,in_[10]+$02441453+(b xor(c and(a xor b)))); d := ((d shl 9)or(d shr(32-9)))+a;
  inc(c,in_[15]+$d8a1e681+(a xor(b and(d xor a)))); c := ((c shl 14)or(c shr(32-14)))+d;
  inc(b,in_[4]+$e7d3fbc8+(d xor(a and(c xor d))));  b := ((b shl 20)or(b shr(32-20)))+c;
  inc(a,in_[9]+$21e1cde6+(c xor(d and(b xor c))));  a := ((a shl 5)or(a shr(32-5)))+b;
  inc(d,in_[14]+$c33707d6+(b xor(c and(a xor b)))); d := ((d shl 9)or(d shr(32-9)))+a;
  inc(c,in_[3]+$f4d50d87+(a xor(b and(d xor a))));  c := ((c shl 14)or(c shr(32-14)))+d;
  inc(b,in_[8]+$455a14ed+(d xor(a and(c xor d))));  b := ((b shl 20)or(b shr(32-20)))+c;
  inc(a,in_[13]+$a9e3e905+(c xor(d and(b xor c)))); a := ((a shl 5)or(a shr(32-5)))+b;
  inc(d,in_[2]+$fcefa3f8+(b xor(c and(a xor b))));  d := ((d shl 9)or(d shr(32-9)))+a;
  inc(c,in_[7]+$676f02d9+(a xor(b and(d xor a))));  c := ((c shl 14)or(c shr(32-14)))+d;
  inc(b,in_[12]+$8d2a4c8a+(d xor(a and(c xor d)))); b := ((b shl 20)or(b shr(32-20)))+c;
  inc(a,in_[5]+$fffa3942+(b xor c xor d));  a := ((a shl 4)or(a shr(32-4)))+b;
  inc(d,in_[8]+$8771f681+(a xor b xor c));  d := ((d shl 11)or(d shr(32-11)))+a;
  inc(c,in_[11]+$6d9d6122+(d xor a xor b)); c := ((c shl 16)or(c shr(32-16)))+d;
  inc(b,in_[14]+$fde5380c+(c xor d xor a)); b := ((b shl 23)or(b shr(32-23)))+c;
  inc(a,in_[1]+$a4beea44+(b xor c xor d));  a := ((a shl 4)or(a shr(32-4)))+b;
  inc(d,in_[4]+$4bdecfa9+(a xor b xor c));  d := ((d shl 11)or(d shr(32-11)))+a;
  inc(c,in_[7]+$f6bb4b60+(d xor a xor b));  c := ((c shl 16)or(c shr(32-16)))+d;
  inc(b,in_[10]+$bebfbc70+(c xor d xor a)); b := ((b shl 23)or(b shr(32-23)))+c;
  inc(a,in_[13]+$289b7ec6+(b xor c xor d)); a := ((a shl 4)or(a shr(32-4)))+b;
  inc(d,in_[0]+$eaa127fa+(a xor b xor c));  d := ((d shl 11)or(d shr(32-11)))+a;
  inc(c,in_[3]+$d4ef3085+(d xor a xor b));  c := ((c shl 16)or(c shr(32-16)))+d;
  inc(b,in_[6]+$04881d05+(c xor d xor a));  b := ((b shl 23)or(b shr(32-23)))+c;
  inc(a,in_[9]+$d9d4d039+(b xor c xor d));  a := ((a shl 4)or(a shr(32-4)))+b;
  inc(d,in_[12]+$e6db99e5+(a xor b xor c)); d := ((d shl 11)or(d shr(32-11)))+a;
  inc(c,in_[15]+$1fa27cf8+(d xor a xor b)); c := ((c shl 16)or(c shr(32-16)))+d;
  inc(b,in_[2]+$c4ac5665+(c xor d xor a));   b := ((b shl 23)or(b shr(32-23)))+c;
  inc(a,in_[0]+$f4292244+(c xor(b or(not d))));  a := ((a shl 6)or(a shr(32-6)))+b;
  inc(d,in_[7]+$432aff97+(b xor(a or(not c))));  d := ((d shl 10)or(d shr(32-10)))+a;
  inc(c,in_[14]+$ab9423a7+(a xor(d or(not b)))); c := ((c shl 15)or(c shr(32-15)))+d;
  inc(b,in_[5]+$fc93a039+(d xor(c or(not a))));  b := ((b shl 21)or(b shr(32-21)))+c;
  inc(a,in_[12]+$655b59c3+(c xor(b or(not d)))); a := ((a shl 6)or(a shr(32-6)))+b;
  inc(d,in_[3]+$8f0ccc92+(b xor(a or(not c))));  d := ((d shl 10)or(d shr(32-10)))+a;
  inc(c,in_[10]+$ffeff47d+(a xor(d or(not b)))); c := ((c shl 15)or(c shr(32-15)))+d;
  inc(b,in_[1]+$85845dd1+(d xor(c or(not a))));  b := ((b shl 21)or(b shr(32-21)))+c;
  inc(a,in_[8]+$6fa87e4f+(c xor(b or(not d))));  a := ((a shl 6)or(a shr(32-6)))+b;
  inc(d,in_[15]+$fe2ce6e0+(b xor(a or(not c)))); d := ((d shl 10)or(d shr(32-10)))+a;
  inc(c,in_[6]+$a3014314+(a xor(d or(not b))));  c := ((c shl 15)or(c shr(32-15)))+d;
  inc(b,in_[13]+$4e0811a1+(d xor(c or(not a)))); b := ((b shl 21)or(b shr(32-21)))+c;
  inc(a,in_[4]+$f7537e82+(c xor(b or(not d))));  a := ((a shl 6)or(a shr(32-6)))+b;
  inc(d,in_[11]+$bd3af235+(b xor(a or(not c)))); d := ((d shl 10)or(d shr(32-10)))+a;
  inc(c,in_[2]+$2ad7d2bb+(a xor(d or(not b))));  c := ((c shl 15)or(c shr(32-15)))+d;
  inc(b,in_[9]+$eb86d391+(d xor(c or(not a))));  b := ((b shl 21)or(b shr(32-21)))+c;
  inc(buf[0],a);
  inc(buf[1],b);
  inc(buf[2],c);
  inc(buf[3],d);
end;

function TMD5.Final: TMD5Digest;
begin
  Finalize;
  Move(buf,result,sizeof(result));
end;

procedure TMD5.Final(out result: TMD5Digest);
begin
  Finalize;
  Move(buf,result,sizeof(result));
end;

procedure TMD5.Finalize;
var count: Integer;
    p: ^Byte;
begin
  count := bytes[0] and $3f;  // Number of bytes in
  p := @in_;
  Inc(p, count);
  // Set the first char of padding to 0x80.  There is always room
  p^ := $80;
  Inc(p);
  // Bytes of padding needed to make 56 bytes (-8..55)
  count := 56 - 1 - count;
  if count < 0 then begin  //  Padding forces an extra block
    FillChar(p^,count + 8, 0);
    MD5Transform(buf,in_);
    p := @in_;
    count := 56;
  end;
  FillChar(p^, count, 0);
  // Append length in bits and transform
  in_[14] := bytes[0] shl 3;
  in_[15] := (bytes[1] shl 3) or (bytes[0] shr 29);
  MD5Transform(buf,in_);
end;

procedure TMD5.Full(Buffer: pointer; Len: integer; out Digest: TMD5Digest);
begin
  Init;
  Update(Buffer^,Len);
  Finalize;
  Move(buf,Digest,sizeof(Digest));
end;

procedure TMD5.Init;
begin
  buf[0] := $67452301;
  buf[1] := $efcdab89;
  buf[2] := $98badcfe;
  buf[3] := $10325476;
  bytes[0] := 0;
  bytes[1] := 0;
end;

procedure TMD5.Update(const buffer; len: Cardinal);
var p: ^TMD5In;
    t: cardinal;
    i: integer;
begin
  p := @buffer;
  // Update byte count
  t := bytes[0];
  Inc(bytes[0], len);
  if bytes[0]<t then
    Inc(bytes[1]);  // Carry from low to high
  t := 64 - (t and $3f);  // Space available in in_ (at least 1)
  if t>len then begin
    Move(p^, Pointer(PtrUInt(@in_) + 64 - t)^, len);
    Exit;
  end;
  // First chunk is an odd size
  Move(p^, Pointer(PtrUInt(@in_) + 64 - t)^, t);
  MD5Transform(buf, in_);
  inc(PtrUInt(p), t);
  dec(len, t);
  // Process data in 64-byte chunks
  for i := 1 to len div 64 do begin
    MD5Transform(buf, p^);
    inc(p);
  end;
  // Handle any remaining bytes of data.
  Move(p^, in_, len mod 64);
end;

function MD5Buf(const Buffer; Len: Cardinal): TMD5Digest;
var MD5: TMD5;
begin
  MD5.Full(@Buffer,Len,result);
end;

function MD5DigestsEqual(const A, B: TMD5Digest): Boolean;
begin
  result := CompareMem(@A,@B,sizeof(TMD5Digest));
end;

const Digits: array[0..15] of AnsiChar = '0123456789abcdef';

function MD5DigestToString(const D: TMD5Digest): RawUTF8;
var P: PAnsiChar;
    I: Integer;
begin
  SetString(result,nil,sizeof(D)*2);
  P := pointer(result);
  for I := 0 to sizeof(D)-1 do begin
    P[0] := Digits[D[I] shr 4];
    P[1] := Digits[D[I] and 15];
    Inc(P,2);
  end;
end;

function SHA1DigestToString(const D: TSHA1Digest): RawUTF8;
var P: PAnsiChar;
    I: Integer;
begin
  SetString(result,nil,sizeof(D)*2);
  P := pointer(result);
  for I := 0 to sizeof(D)-1 do begin
    P[0] := Digits[D[I] shr 4];
    P[1] := Digits[D[I] and 15];
    Inc(P,2);
  end;
end;

function SHA256DigestToString(const D: TSHA256Digest): RawUTF8;
var P: PAnsiChar;
    I: Integer;
begin
  SetString(result,nil,sizeof(D)*2);
  P := pointer(result);
  for I := 0 to sizeof(D)-1 do begin
    P[0] := Digits[D[I] shr 4];
    P[1] := Digits[D[I] and 15];
    Inc(P,2);
  end;
end;

function htdigest(const user, realm, pass: RawByteString): RawUTF8;
// apache-compatible: agent007:download area:8364d0044ef57b3defcfa141e8f77b65
//    hash=`echo -n "$user:$realm:$pass" | md5sum | cut -b -32`
//    echo "$user:$realm:$hash"
var tmp: RawByteString;
begin
  tmp := user+':'+realm+':';
  result := tmp+MD5(tmp+pass);
end;

function MD5SelfTest: boolean;
begin
  result := htdigest('agent007','download area','secret')=
    'agent007:download area:8364d0044ef57b3defcfa141e8f77b65';
end;


{ TSHA1 }

// TSHAContext = Hash,MLen,Buffer,Index
procedure TSHA1.Compress;
var
  A, B, C, D, E: cardinal;
  X: cardinal;
  W: array[0..79] of cardinal;
  i: integer;
begin
  // init W[] + A..E
  bswap256(@TSHAContext(Context).Buffer[0],@W[0]);
  bswap256(@TSHAContext(Context).Buffer[32],@W[8]);
  for i := 16 to 79 do begin
    X  := W[i-3] xor W[i-8] xor W[i-14] xor W[i-16];
    W[i]:= (X shl 1) or (X shr 31);
  end;
  with TSHAContext(Context) do begin
    A := Hash.A;
    B := Hash.B;
    C := Hash.C;
    D := Hash.D;
    E := Hash.E;
  end;
  // unrolled loop -> all is computed in cpu registers
  Inc(E,((A shl 5) or (A shr 27)) + (D xor (B and (C xor D))) + $5A827999 + W[ 0]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (C xor (A and (B xor C))) + $5A827999 + W[ 1]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (B xor (E and (A xor B))) + $5A827999 + W[ 2]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (A xor (D and (E xor A))) + $5A827999 + W[ 3]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (E xor (C and (D xor E))) + $5A827999 + W[ 4]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (D xor (B and (C xor D))) + $5A827999 + W[ 5]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (C xor (A and (B xor C))) + $5A827999 + W[ 6]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (B xor (E and (A xor B))) + $5A827999 + W[ 7]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (A xor (D and (E xor A))) + $5A827999 + W[ 8]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (E xor (C and (D xor E))) + $5A827999 + W[ 9]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (D xor (B and (C xor D))) + $5A827999 + W[10]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (C xor (A and (B xor C))) + $5A827999 + W[11]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (B xor (E and (A xor B))) + $5A827999 + W[12]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (A xor (D and (E xor A))) + $5A827999 + W[13]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (E xor (C and (D xor E))) + $5A827999 + W[14]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (D xor (B and (C xor D))) + $5A827999 + W[15]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (C xor (A and (B xor C))) + $5A827999 + W[16]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (B xor (E and (A xor B))) + $5A827999 + W[17]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (A xor (D and (E xor A))) + $5A827999 + W[18]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (E xor (C and (D xor E))) + $5A827999 + W[19]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (B xor C xor D) + $6ED9EBA1 + W[20]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (A xor B xor C) + $6ED9EBA1 + W[21]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (E xor A xor B) + $6ED9EBA1 + W[22]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (D xor E xor A) + $6ED9EBA1 + W[23]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (C xor D xor E) + $6ED9EBA1 + W[24]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (B xor C xor D) + $6ED9EBA1 + W[25]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (A xor B xor C) + $6ED9EBA1 + W[26]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (E xor A xor B) + $6ED9EBA1 + W[27]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (D xor E xor A) + $6ED9EBA1 + W[28]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (C xor D xor E) + $6ED9EBA1 + W[29]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (B xor C xor D) + $6ED9EBA1 + W[30]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (A xor B xor C) + $6ED9EBA1 + W[31]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (E xor A xor B) + $6ED9EBA1 + W[32]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (D xor E xor A) + $6ED9EBA1 + W[33]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (C xor D xor E) + $6ED9EBA1 + W[34]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (B xor C xor D) + $6ED9EBA1 + W[35]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (A xor B xor C) + $6ED9EBA1 + W[36]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (E xor A xor B) + $6ED9EBA1 + W[37]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (D xor E xor A) + $6ED9EBA1 + W[38]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (C xor D xor E) + $6ED9EBA1 + W[39]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + ((B and C) or (D and (B or C))) + $8F1BBCDC + W[40]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + ((A and B) or (C and (A or B))) + $8F1BBCDC + W[41]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + ((E and A) or (B and (E or A))) + $8F1BBCDC + W[42]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + ((D and E) or (A and (D or E))) + $8F1BBCDC + W[43]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + ((C and D) or (E and (C or D))) + $8F1BBCDC + W[44]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + ((B and C) or (D and (B or C))) + $8F1BBCDC + W[45]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + ((A and B) or (C and (A or B))) + $8F1BBCDC + W[46]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + ((E and A) or (B and (E or A))) + $8F1BBCDC + W[47]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + ((D and E) or (A and (D or E))) + $8F1BBCDC + W[48]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + ((C and D) or (E and (C or D))) + $8F1BBCDC + W[49]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + ((B and C) or (D and (B or C))) + $8F1BBCDC + W[50]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + ((A and B) or (C and (A or B))) + $8F1BBCDC + W[51]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + ((E and A) or (B and (E or A))) + $8F1BBCDC + W[52]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + ((D and E) or (A and (D or E))) + $8F1BBCDC + W[53]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + ((C and D) or (E and (C or D))) + $8F1BBCDC + W[54]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + ((B and C) or (D and (B or C))) + $8F1BBCDC + W[55]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + ((A and B) or (C and (A or B))) + $8F1BBCDC + W[56]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + ((E and A) or (B and (E or A))) + $8F1BBCDC + W[57]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + ((D and E) or (A and (D or E))) + $8F1BBCDC + W[58]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + ((C and D) or (E and (C or D))) + $8F1BBCDC + W[59]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (B xor C xor D) + $CA62C1D6 + W[60]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (A xor B xor C) + $CA62C1D6 + W[61]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (E xor A xor B) + $CA62C1D6 + W[62]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (D xor E xor A) + $CA62C1D6 + W[63]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (C xor D xor E) + $CA62C1D6 + W[64]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (B xor C xor D) + $CA62C1D6 + W[65]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (A xor B xor C) + $CA62C1D6 + W[66]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (E xor A xor B) + $CA62C1D6 + W[67]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (D xor E xor A) + $CA62C1D6 + W[68]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (C xor D xor E) + $CA62C1D6 + W[69]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (B xor C xor D) + $CA62C1D6 + W[70]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (A xor B xor C) + $CA62C1D6 + W[71]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (E xor A xor B) + $CA62C1D6 + W[72]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (D xor E xor A) + $CA62C1D6 + W[73]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (C xor D xor E) + $CA62C1D6 + W[74]); C:= (C shl 30) or (C shr 2);
  Inc(E,((A shl 5) or (A shr 27)) + (B xor C xor D) + $CA62C1D6 + W[75]); B:= (B shl 30) or (B shr 2);
  Inc(D,((E shl 5) or (E shr 27)) + (A xor B xor C) + $CA62C1D6 + W[76]); A:= (A shl 30) or (A shr 2);
  Inc(C,((D shl 5) or (D shr 27)) + (E xor A xor B) + $CA62C1D6 + W[77]); E:= (E shl 30) or (E shr 2);
  Inc(B,((C shl 5) or (C shr 27)) + (D xor E xor A) + $CA62C1D6 + W[78]); D:= (D shl 30) or (D shr 2);
  Inc(A,((B shl 5) or (B shr 27)) + (C xor D xor E) + $CA62C1D6 + W[79]); C:= (C shl 30) or (C shr 2);
  // Calculate new working hash
  with TSHAContext(Context) do begin
    inc(Hash.A,A);
    inc(Hash.B,B);
    inc(Hash.C,C);
    inc(Hash.D,D);
    inc(Hash.E,E);
  end;
end;

procedure TSHA1.Final(out Digest: TSHA1Digest);
var Data: TSHAContext absolute Context;
begin
  // 1. append bit '1' after Buffer
  Data.Buffer[Data.Index]:= $80;
  fillchar(Data.Buffer[Data.Index+1],63-Data.Index,0);
  // 2. Compress if more than 448 bits, (no room for 64 bit length
  if Data.Index>=56 then begin
    Compress;
    fillchar(Data.Buffer,56,0);
  end;
  // Write 64 bit Buffer length into the last bits of the last block
  // (in big endian format) and do a final compress
  PInteger(@Data.Buffer[56])^ := bswap32(Int64Rec(Data.MLen).Hi);
  PInteger(@Data.Buffer[60])^ := bswap32(Int64Rec(Data.MLen).Lo);
  Compress;
  // Hash -> Digest to little endian format
  bswap160(@Data.Hash,@Digest);
  // Clear Data
  Init;
end;

procedure TSHA1.Full(Buffer: pointer; Len: integer; out Digest: TSHA1Digest);
begin
{$ifdef USEPADLOCK}
  // Padlock need all data once -> Full() is OK, not successive Update()
  if padlock_available then begin
    Init; // for later Update use
    {$ifdef PADLOCKDEBUG}write('padlock_phe_sha1 ');{$endif}
    if padlock_phe_sha1(buffer,Len,Digest)=0 then
      exit else
    {$ifdef PADLOCKDEBUG}write(':ERROR ');{$endif}
  end;
{$endif}
  Init;
  Update(Buffer,Len);
  Final(Digest);
end;

procedure TSHA1.Init;
// initialize context
var Data: TSHAContext absolute Context;
begin
  fillchar(Data,sizeof(Data),0);
  Data.Hash.A := $67452301;
  Data.Hash.B := $EFCDAB89;
  Data.Hash.C := $98BADCFE;
  Data.Hash.D := $10325476;
  Data.Hash.E := $C3D2E1F0;
end;

procedure TSHA1.Update(Buffer: pointer; Len: integer);
var Data: TSHAContext absolute Context;
    aLen: integer;
begin
  if Buffer=nil then exit; // avoid GPF
  inc(Data.MLen, Int64(Cardinal(Len)) shl 3);
  while Len > 0 do begin
    aLen := sizeof(Data.Buffer)-Data.Index;
    if aLen<=Len then begin
      move(buffer^,Data.Buffer[Data.Index],aLen);
      dec(Len,aLen);
      inc(PtrUInt(buffer),aLen);
      Compress;
      Data.Index := 0;
    end else begin
      move(buffer^,Data.Buffer[Data.Index],Len);
      inc(Data.Index,Len);
      break;
    end;
  end;
end;


{ TAESAbstract }

{$ifdef CPU64}
procedure XorBlock16(A,B: PInt64Array); inline; overload;
begin
  A[0] := A[0] xor B[0];
  A[1] := A[1] xor B[1];
end;

procedure XorBlock16(A,B,C: PInt64Array); inline; overload;
begin
  B[0] := A[0] xor C[0];
  B[1] := A[1] xor C[1];
end;
{$else}
procedure XorBlock16(A,B: PCardinalArray); {$ifdef HASINLINE}inline;{$endif} overload;
begin
  A[0] := A[0] xor B[0];
  A[1] := A[1] xor B[1];
  A[2] := A[2] xor B[2];
  A[3] := A[3] xor B[3];
end;

procedure XorBlock16(A,B,C: PCardinalArray); {$ifdef HASINLINE}inline;{$endif} overload;
begin
  B[0] := A[0] xor C[0];
  B[1] := A[1] xor C[1];
  B[2] := A[2] xor C[2];
  B[3] := A[3] xor C[3];
end;
{$endif}

procedure XorBlockN(A,B,C: PByteArray; Count: integer); {$ifdef HASINLINE}inline;{$endif}
var i: integer;
begin
  for i := 0 to Count-1 do
    B[i] := A[i] xor C[i];
end;

const
  sAESException = 'AES engine initialization failure';

procedure FillRandom(var IV: TAESBlock);
var i,rnd: cardinal;
begin
  rnd := (GetTickCount64 xor PtrUInt(@IV))*Random(MaxInt);
  for i := 0 to 3 do
    PCardinalArray(@IV)[i] := PCardinalArray(@IV)[i]
      xor rnd xor TD0[(rnd shr i)and 2047];
end;

constructor TAESAbstract.Create(const aKey; aKeySize: cardinal);
begin
   if (aKeySize<>128) and (aKeySize<>192) and (aKeySize<>256) then
    raise ESynCrypto.CreateFmt(
      '%s.Create key size = %d; should be either 128, 192 or 256',
      [ClassName,aKeySize]);
  fKeySize := aKeySize;
  fKeySizeBytes := fKeySize shr 3;
  move(aKey,fKey,fKeySizeBytes);
end;

function TAESAbstract.DecryptPKCS7(const Input: RawByteString;
  IVAtBeginning: boolean): RawByteString;
var len,iv: integer;
begin
  // validate input
  len := length(Input);
  if (len<AESBlockSize) or (len and (AESBlockSize-1)<>0) then
    raise ESynCrypto.Create('Invalid content');
  // decrypt
  if IVAtBeginning then begin
    fIV := PAESBlock(Input)^;
    dec(len,AESBlockSize);
    iv := AESBlockSize;
  end else
    iv := 0;
  SetString(result,nil,len);
  Decrypt(@PByteArray(Input)^[iv],pointer(result),len);
  // delete right padding
  if ord(result[len])>AESBlockSize then
    raise ESynCrypto.Create('Invalid content');
  SetLength(result,len-ord(result[len]));
end;

function TAESAbstract.EncryptPKCS7(const Input: RawByteString;
  IVAtBeginning: boolean): RawByteString;
var len, padding, iv: cardinal;
    P: Pointer;
begin
  // use PKCS7 padding, so expects AESBlockSize=16 bytes blocks
  len := length(Input);
  padding := AESBlockSize-(len and (AESBlockSize-1));
  if IVAtBeginning then
    iv := AESBlockSize else
    iv := 0;
  SetString(result,nil,iv+len+padding);
  if IVAtBeginning then begin
    FillRandom(fIV);
    PAESBlock(result)^ := fIV;
  end;
  move(Pointer(Input)^,PByteArray(result)^[iv],len);
  FillChar(PByteArray(result)^[iv+len],padding,padding);
  // encryption
  P := @PByteArray(result)^[iv];
  Encrypt(P,P,len+padding);
end;

class function TAESAbstract.SimpleEncrypt(const Input,Key: RawByteString;
  Encrypt, IVAtBeginning: boolean): RawByteString;
var instance: TAESAbstract;
    digest: TSHA256Digest;
begin
  SHA256Weak(Key,digest);
  instance := Create(digest,256);
  try
    if Encrypt then
      result := instance.EncryptPKCS7(Input,IVAtBeginning) else
      result := instance.DecryptPKCS7(Input,IVAtBeginning);
  finally
    instance.Free;
  end;
end;

function TAESAbstract.Clone: TAESAbstract;
begin
  result := TAESAbstractClass(ClassType).Create(fKey,fKeySize);
end;


{ TAESAbstractSyn }

procedure TAESAbstractSyn.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  fIn := BufIn;
  fOut := BufOut;
  fCount := Count;
  fCV := fIV;
end;

procedure TAESAbstractSyn.DecryptInit;
begin
  if not AES.DecryptInit(fKey,fKeySize) then
    raise ESynCrypto.Create(sAESException);
end;

procedure TAESAbstractSyn.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  fIn := BufIn;
  fOut := BufOut;
  fCount := Count;
  fCV := fIV;
end;

procedure TAESAbstractSyn.EncryptInit;
begin
  if not AES.EncryptInit(fKey,fKeySize) then
    raise ESynCrypto.Create(sAESException);
end;

procedure TAESAbstractSyn.EncryptTrailer;
var len: Cardinal;
begin
  len := fCount and (AESBlockSize-1);
  if len<>0 then begin
    AES.Encrypt(fCV,fCV);
    XorBlockN(pointer(fIn),pointer(fOut),@fCV,len);
  end;
end;

procedure TAESAbstractSyn.DecryptTrailer;
var len: Cardinal;
begin
  len := fCount and (AESBlockSize-1);
  if len<>0 then begin
    EncryptInit; 
    AES.Encrypt(fCV,fCV);
    XorBlockN(pointer(fIn),pointer(fOut),@fCV,len);
  end;
end;


{ TAESECB }

procedure TAESECB.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
var i: integer;
begin
  inherited; // CV := IV + set fIn,fOut,fCount
  DecryptInit;
  for i := 1 to Count shr 4 do begin
    AES.Decrypt(fIn^,fOut^);
    inc(fIn);
    inc(fOut);
  end;
  DecryptTrailer;
end;

procedure TAESECB.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
var i: integer;
begin
  inherited; // CV := IV + set fIn,fOut,fCount
  EncryptInit;
  for i := 1 to Count shr 4 do begin
    AES.Encrypt(fIn^,fOut^);
    inc(fIn);
    inc(fOut);
  end;
  EncryptTrailer;
end;


{ TAESCBC }

procedure TAESCBC.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
var i: integer;
    tmp: TAESBlock;
begin
  inherited; // CV := IV + set fIn,fOut,fCount
  if Count>=AESBlockSize then begin
    DecryptInit;
    for i := 1 to Count shr 4 do begin
      tmp := fIn^;
      AES.Decrypt(fIn^,fOut^);
      XorBlock16(pointer(fOut),pointer(@fCV));
      fCV := tmp;
      inc(fIn);
      inc(fOut);
    end;
  end;
  DecryptTrailer;
end;

procedure TAESCBC.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
var i: integer;
begin
  inherited; // CV := IV + set fIn,fOut,fCount
  EncryptInit;
  for i := 1 to Count shr 4 do begin
    XorBlock16(pointer(fIn),pointer(fOut),pointer(@fCV));
    AES.Encrypt(fOut^,fOut^);
    fCV := fOut^;
    inc(fIn);
    inc(fOut);
  end;
  EncryptTrailer;
end;


{ TAESCFB }

{$ifdef USEAESNI}
procedure AesNiTrailer;
asm
    and    ecx,15
    jz     @0
    call   AesNiEncryptXmm7
    lea    edx,[eax].TAESContext.buf
    movdqu [edx],xmm7
    cld
@s: lodsb
    xor    al,[edx]
    inc    edx
    stosb
    loop   @s
@0:
end;
{$endif}

procedure TAESCFB.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
var i: integer;
    tmp: TAESBlock;
begin
  if not AES.Initialized then
    EncryptInit; // CFB mode = only Encrypt -> allow prepare the key once
  {$ifdef USEAESNI}
  if TAESContext(AES.Context).AesNi then
  asm
    push   esi
    push   edi
    mov    eax,self
    mov    ecx,Count
    mov    esi,BufIn
    mov    edi,BufOut
    movdqu xmm7,dqword ptr [eax].TAESCFB.fIV
    lea    eax,[eax].TAESCFB.AES
    push   ecx
    shr    ecx,4
    jz     @z
@s: call   AesNiEncryptXmm7
    movdqu xmm0,dqword ptr [esi]
    pxor   xmm0,xmm7
    movdqu xmm7,dqword ptr [esi]
    movdqu dqword ptr [edi],xmm0
    dec    ecx
    lea    esi,[esi+16]
    lea    edi,[edi+16]
    jnz    @s
@z: pop    ecx
    call   AesNiTrailer
    pop    edi
    pop    esi
  end else
  {$endif} begin
    inherited; // CV := IV + set fIn,fOut,fCount
    for i := 1 to Count shr 4 do begin
      tmp := fIn^;
      AES.Encrypt(fCV,fCV);
      XorBlock16(pointer(fIn),pointer(fOut),pointer(@fCV));
      fCV := tmp;
      inc(fIn);
      inc(fOut);
    end;
    EncryptTrailer;
  end;
end;

procedure TAESCFB.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
var i: integer;
begin
  if not AES.Initialized then
    EncryptInit; // CFB mode = only Encrypt -> allow prepare the key once
  {$ifdef USEAESNI}
  if TAESContext(AES.Context).AesNi then
  asm
    push   esi
    push   edi
    mov    eax,self
    mov    ecx,Count
    mov    esi,BufIn
    mov    edi,BufOut
    movdqu xmm7,dqword ptr [eax].TAESCFB.fIV
    lea    eax,[eax].TAESCFB.AES
    push   ecx
    shr    ecx,4
    jz     @z
@s: call   AesNiEncryptXmm7
    movdqu xmm0,dqword ptr [esi]
    pxor   xmm7,xmm0
    movdqu dqword ptr [edi],xmm7
    dec    ecx
    lea    esi,[esi+16]
    lea    edi,[edi+16]
    jnz    @s
@z: pop    ecx
    call   AesNiTrailer
    pop    edi
    pop    esi
  end else
  {$endif} begin
    inherited; // CV := IV + set fIn,fOut,fCount
    for i := 1 to Count shr 4 do begin
      AES.Encrypt(fCV,fCV);
      XorBlock16(pointer(fIn),pointer(fOut),pointer(@fCV));
      fCV := fOut^;
      inc(fIn);
      inc(fOut);
    end;
    EncryptTrailer;
  end;
end;


{ TAESOFB }

procedure TAESOFB.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  Encrypt(BufIn, BufOut, Count); // by definition
end;

procedure TAESOFB.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
var i: integer;
begin
  if not AES.Initialized then
    EncryptInit; // OFB mode = only Encrypt -> allow prepare the key once
  {$ifdef USEAESNI}
  if TAESContext(AES.Context).AesNi then
  asm
    push   esi
    push   edi
    mov    eax,self
    mov    ecx,Count
    mov    esi,BufIn
    mov    edi,BufOut
    movdqu xmm7,dqword ptr [eax].TAESCFB.fIV
    lea    eax,[eax].TAESCFB.AES
    push   ecx
    shr    ecx,4
    jz     @z
@s: call   AesNiEncryptXmm7
    movdqu xmm0,dqword ptr [esi]
    pxor   xmm0,xmm7
    movdqu dqword ptr [edi],xmm0
    dec    ecx
    lea    esi,[esi+16]
    lea    edi,[edi+16]
    jnz    @s
@z: pop    ecx
    call   AesNiTrailer
    pop    edi
    pop    esi
  end else
  {$endif} begin
    inherited; // CV := IV + set fIn,fOut,fCount
    for i := 1 to Count shr 4 do begin
      AES.Encrypt(fCV,fCV);
      XorBlock16(pointer(fIn),pointer(fOut),pointer(@fCV));
      inc(fIn);
      inc(fOut);
    end;
    EncryptTrailer;
  end;
end;


{ TAESCTR }

procedure TAESCTR.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  Encrypt(BufIn, BufOut, Count); // by definition
end;

procedure TAESCTR.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
var i,j: integer;
    tmp: TAESBlock;
begin
  if not AES.Initialized then
    EncryptInit; // CTR mode = only Encrypt -> allow prepare the key once
  inherited; // CV := IV + set fIn,fOut,fCount
  for i := 1 to Count shr 4 do begin
    AES.Encrypt(fCV,tmp);
    inc(fCV[7]);
    j := 7;
    while (j>0) and (fCV[j]=0) do begin
      dec(j);
      inc(fCV[j]);
    end;
    XorBlock16(pointer(fIn),pointer(fOut),pointer(@tmp));
    inc(fIn);
    inc(fOut);
  end;
  Count := Count and (AESBlockSize-1);
  if Count<>0 then begin
    AES.Encrypt(fCV,tmp);
    XorBlockN(pointer(fIn),pointer(fOut),@tmp,Count);
  end;
end;


{$ifdef USE_PROV_RSA_AES}

const
  PROV_RSA_AES = 24;
  CRYPT_NEWKEYSET = 8;
  PLAINTEXTKEYBLOB = 8;
  CUR_BLOB_VERSION = 2;
  KP_IV = 1;
  KP_MODE = 4;

  CALG_AES_128  = $660E;
  CALG_AES_192  = $660F;
  CALG_AES_256  = $6610;

  CRYPT_MODE_CBC = 1;
  CRYPT_MODE_ECB = 2;
  CRYPT_MODE_OFB = 3;
  CRYPT_MODE_CFB = 4;
  CRYPT_MODE_CTS = 5;

type
  HCRYPTPROV = pointer;
  HCRYPTKEY = pointer;
  HCRYPTHASH = pointer;

  TCryptLibrary = object
  public
    AcquireContextA: function(var phProv: HCRYPTPROV; pszContainer: PAnsiChar;
      pszProvider: PAnsiChar; dwProvType: DWORD; dwFlags: DWORD): BOOL; stdcall;
    ReleaseContext: function(hProv: HCRYPTPROV; dwFlags: PtrUInt): BOOL; stdcall;
    ImportKey: function(hProv: HCRYPTPROV; pbData: pointer; dwDataLen: DWORD;
      hPubKey: HCRYPTKEY; dwFlags: DWORD; var phKey: HCRYPTKEY): BOOL; stdcall;
    SetKeyParam: function(hKey: HCRYPTKEY; dwParam: DWORD; pbData: pointer;
      dwFlags: DWORD): BOOL; stdcall;
    DestroyKey: function(hKey: HCRYPTKEY): BOOL; stdcall;
    Encrypt: function(hKey: HCRYPTKEY; hHash: HCRYPTHASH; Final: BOOL;
      dwFlags: DWORD; pbData: pointer; var pdwDataLen: DWORD; dwBufLen: DWORD): BOOL; stdcall;
    Decrypt: function(hKey: HCRYPTKEY; hHash: HCRYPTHASH; Final: BOOL;
      dwFlags: DWORD; pbData: pointer; var pdwDataLen: DWORD): BOOL; stdcall;
    function Available: boolean;
  protected
    Tested: boolean;
    Handle: THandle;
  end;

const
  HCRYPTPROV_NOTTESTED = HCRYPTPROV(-1);

var
  CryptoAPI: TCryptLibrary;
  CryptoAPIAESProvider: HCRYPTPROV = HCRYPTPROV_NOTTESTED;

function TCryptLibrary.Available: boolean;
  procedure Acquire;
  const NAMES: array[0..6] of PChar = (
    'CryptAcquireContextA','CryptReleaseContext',
    'CryptImportKey','CryptSetKeyParam','CryptDestroyKey',
    'CryptEncrypt','CryptDecrypt');
  var P: PPointer;
      i: integer;
  begin
    Tested := true;
    Handle := GetModuleHandle('advapi32.dll');
    if Handle<>0 then begin
      P := @@AcquireContextA;
      for i := 0 to high(NAMES) do begin
        P^ := GetProcAddress(Handle,NAMES[i]);
        if P^=nil then begin
          PPointer(@@AcquireContextA)^ := nil;
          break;
        end;
        inc(P);
      end;
    end;
  end;
begin
  if not Tested then
    Acquire;
  result := Assigned(AcquireContextA);
end;

procedure EnsureCryptoAPIAESProviderAvailable;
begin
  if CryptoAPIAESProvider=nil then
    raise ESynCrypto.Create('PROV_RSA_AES provider not installed') else
  if CryptoAPIAESProvider=HCRYPTPROV_NOTTESTED then begin
    CryptoAPIAESProvider := nil;
    if CryptoAPI.Available then begin
      if not CryptoAPI.AcquireContextA(CryptoAPIAESProvider,nil,nil,PROV_RSA_AES,0) then
        if (HRESULT(GetLastError)<>NTE_BAD_KEYSET) or not CryptoAPI.AcquireContextA(
           CryptoAPIAESProvider,nil,nil,PROV_RSA_AES,CRYPT_NEWKEYSET) then
          RaiseLastOSError;
    end;
  end;
end;


{ TAESAbstract_API }

constructor TAESAbstract_API.Create(const aKey; aKeySize: cardinal);
begin
  EnsureCryptoAPIAESProviderAvailable;
  inherited Create(aKey,aKeySize); // check and set fKeySize[Bytes]
  InternalSetMode;
  fKeyHeader.bType := PLAINTEXTKEYBLOB;
  fKeyHeader.bVersion := CUR_BLOB_VERSION;
  case fKeySize of
  128: fKeyHeader.aiKeyAlg := CALG_AES_128;
  192: fKeyHeader.aiKeyAlg := CALG_AES_192;
  256: fKeyHeader.aiKeyAlg := CALG_AES_256;
  end;
  fKeyHeader.dwKeyLength := fKeySizeBytes;
  fKeyHeaderKey := fKey;
end;

destructor TAESAbstract_API.Destroy;
begin
  if fKeyCryptoAPI<>nil then
    CryptoAPI.DestroyKey(fKeyCryptoAPI);
  inherited;
end;

procedure TAESAbstract_API.EncryptDecrypt(BufIn, BufOut: pointer; Count: cardinal;
  DoEncrypt: boolean);
var n: Cardinal;
begin
  if Count=0 then
    exit; // nothing to do
  if fKeyCryptoAPI<>nil then begin
    CryptoAPI.DestroyKey(fKeyCryptoAPI);
    fKeyCryptoAPI := nil;
  end;
  if not CryptoAPI.ImportKey(CryptoAPIAESProvider,
     @fKeyHeader,sizeof(fKeyHeader)+fKeySizeBytes,nil,0,fKeyCryptoAPI) then
    raise ESynCrypto.CreateFmt('Error $%x in CryptImportKey',[GetLastError]);
  if not CryptoAPI.SetKeyParam(fKeyCryptoAPI,KP_IV,@fIV,0) then
    raise ESynCrypto.CreateFmt('Error $%x in CryptSetKeyParam(KP_IV)',[GetLastError]);
  if not CryptoAPI.SetKeyParam(fKeyCryptoAPI,KP_MODE,@fInternalMode,0) then
    raise ESynCrypto.CreateFmt('Error $%x in CryptSetKeyParam(KP_MODE,%d)',[GetLastError,fInternalMode]);
  if BufOut<>BufIn then
    Move(BufIn^,BufOut^,Count);
  n := Count and not (AESBlockSize-1);
  if DoEncrypt then begin
    if not CryptoAPI.Encrypt(fKeyCryptoAPI,nil,false,0,BufOut,n,Count) then
      raise ESynCrypto.CreateFmt('Error $%x in CryptEncrypt() for %s',[GetLastError,ClassName]);
  end else
    if not CryptoAPI.Decrypt(fKeyCryptoAPI,nil,false,0,BufOut,n) then
      raise ESynCrypto.CreateFmt('Error $%x in CryptDecrypt() for %s',[GetLastError,ClassName]);
  dec(Count,n);
  if Count>0 then // remaining bytes will be XORed with the supplied IV
    XorBlockN(@PByteArray(BufIn)[n],@PByteArray(BufOut)[n],@fIV,Count);
end;

procedure TAESAbstract_API.Encrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  EncryptDecrypt(BufIn,BufOut,Count,true);
end;

procedure TAESAbstract_API.Decrypt(BufIn, BufOut: pointer; Count: cardinal);
begin
  EncryptDecrypt(BufIn,BufOut,Count,false);
end;

{ TAESECB_API }

procedure TAESECB_API.InternalSetMode;
begin
  fInternalMode := CRYPT_MODE_ECB;
end;

{ TAESCBC_API }

procedure TAESCBC_API.InternalSetMode;
begin
  fInternalMode := CRYPT_MODE_CBC;
end;

{ TAESCFB_API }

procedure TAESCFB_API.InternalSetMode;
begin
  raise ESynCrypto.Create('CRYPT_MODE_CFB does not work');
  fInternalMode := CRYPT_MODE_CFB;
end;

{ TAESOFB_API }

procedure TAESOFB_API.InternalSetMode;
begin
  raise ESynCrypto.Create('CRYPT_MODE_OFB not implemented by PROV_RSA_AES');
  fInternalMode := CRYPT_MODE_OFB;
end;

{$endif USE_PROV_RSA_AES}


{ TRC4 }

procedure TRC4.Init(const aKey; aKeyLen: integer);
var i,k: integer;
    j,tmp: byte;
begin
  if aKeyLen<=0 then
    raise ESynCrypto.CreateFmt('TRC4.Init(invalid aKeyLen=%d)',[aKeyLen]);
  dec(aKeyLen);
  for i := 0 to high(key) do
    key[i] := i;
  j := 0;
  k := 0;
  for i := 0 to high(key) do begin
    inc(j,key[i]+TByteArray(aKey)[k]);
    tmp := key[i];
    key[i] := key[j];
    key[j] := tmp;
    if k>=aKeyLen then // avoid slow mod operation within loop
      k := 0 else
      inc(k);
  end;
end;

procedure TRC4.Encrypt(const BufIn; var BufOut; Count: cardinal);
var ndx: cardinal;
    i,j,ki,kj: byte;
begin
  i := 0;
  j := 0;
  for ndx := 0 to Count-1 do begin
    inc(i);
    ki := key[i];
    inc(j,ki);
    kj := key[j];
    key[i] := kj;
    inc(kj,ki);
    key[j] := ki;
    TByteArray(BufOut)[ndx] := TByteArray(BufIn)[ndx] xor key[kj];
  end;
end;

procedure TRC4.RestoreKey(const Backup: TRC4InternalKey);
begin
  move(Backup,key,sizeof(key));
end;

procedure TRC4.SaveKey(out Backup: TRC4InternalKey);
begin
  move(key,Backup,sizeof(key));
end;

function RC4SelfTest: boolean;
const
  Key: array[0..4] of byte = ($61,$8A,$63,$D2,$FB);
  InDat: array[0..4] of byte = ($DC,$EE,$4C,$F9,$2C);
  OutDat: array[0..4] of byte = ($F1,$38,$29,$C9,$DE);
  Test1: array[0..7] of byte = ($01,$23,$45,$67,$89,$ab,$cd,$ef);
  Res1: array[0..7] of byte = ($75,$b7,$87,$80,$99,$e0,$c5,$96);
  Key2: array[0..3] of byte = ($ef,$01,$23,$45);
  Test2: array[0..9] of byte = (0,0,0,0,0,0,0,0,0,0);
  Res2: array[0..9] of byte = ($d6,$a1,$41,$a7,$ec,$3c,$38,$df,$bd,$61);
var RC4: TRC4;
    Dat: array[0..9] of byte;
    Backup: TRC4InternalKey;
begin
  RC4.Init(Test1,8);
  RC4.Encrypt(Test1,Dat,8);
  result := CompareMem(@Dat,@Res1,sizeof(Res1));
  RC4.Init(Key2,4);
  RC4.Encrypt(Test2,Dat,10);
  result := result and CompareMem(@Dat,@Res2,sizeof(Res2));
  RC4.Init(Key,sizeof(Key));
  RC4.Encrypt(InDat,Dat,sizeof(InDat));
  result := result and CompareMem(@Dat,@OutDat,sizeof(OutDat));
  RC4.Init(Key,sizeof(Key));
  RC4.SaveKey(Backup);
  RC4.Encrypt(InDat,Dat,sizeof(InDat));
  result := result and CompareMem(@Dat,@OutDat,sizeof(OutDat));
  RC4.RestoreKey(Backup);
  RC4.Encrypt(InDat,Dat,sizeof(InDat));
  result := result and CompareMem(@Dat,@OutDat,sizeof(OutDat));
  RC4.RestoreKey(Backup);
  RC4.Encrypt(OutDat,Dat,sizeof(InDat));
  result := result and CompareMem(@Dat,@InDat,sizeof(OutDat));
end;


procedure CompressShaAesSetKey(const Key: RawByteString; AesClass: TAESAbstractClass);
begin
  if Key='' then
    FillChar(CompressShaAesKey,sizeof(CompressShaAesKey),0) else
    SHA256Weak(Key,CompressShaAesKey);
end;

function CompressShaAes(var DataRawByteString; Compress: boolean): AnsiString;
var Data: RawByteString absolute DataRawByteString;
begin
  if (Data<>'') and (CompressShaAesClass<>nil) then
  try
    with CompressShaAesClass.Create(CompressShaAesKey,256) do
    try
      if Compress then begin
        CompressSynLZ(Data,true);
        Data := EncryptPKCS7(Data,true);
      end else begin
        Data := DecryptPKCS7(Data,true);
        if CompressSynLZ(Data,false)='' then begin
          result := '';
          exit; // invalid content
        end;
      end;
    finally
      Free;
    end;
  except
    on Exception do begin // e.g. ESynCrypto in DecryptPKCS7(Data)
      result := '';
      exit; // invalid content
    end;
  end;
  result := 'synshaaes'; // mark success
end;


initialization
  ComputeAesStaticTables;
{$ifdef USEPADLOCK}
  PadlockInit;
{$endif}
  assert(sizeof(TMD5Buf)=sizeof(TMD5Digest));
  assert(sizeof(TAESContext)=AESContextSize);
  assert(sizeof(TSHAContext)=SHAContextSize);
  assert(sizeof(TAESFullHeader)=AESBlockSize);

finalization
{$ifdef USEPADLOCKDLL}
  if PadLockLibHandle<>0 then
    FreeLibrary(PadLockLibHandle); // same on Win+Linux, thanks to SysUtils
{$endif}
{$ifdef USE_PROV_RSA_AES}
  if CryptoAPI.Handle<>0 then begin
    if (CryptoAPIAESProvider<>nil) and (CryptoAPIAESProvider<>HCRYPTPROV_NOTTESTED) then
      CryptoAPI.ReleaseContext(CryptoAPIAESProvider,0);
    FreeLibrary(CryptoAPI.Handle);
  end;
{$endif}
end.
