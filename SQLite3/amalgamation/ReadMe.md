# SQLite3 Database Engine With Encryption

## Reference Only - Do Not Use

This source code is included as reference.

**You should not have to compile the SQLite3 c code by yourself.**

We supply and validate the proper static `.o` `.obj` files within our https://github/synopse repository, or directly from https://synopse.info/files/sqlite3fpc.7z (for FPC) or https://synopse.info/files/sqlite3obj.7z (for Delphi).

## How To Compile The SQlite3 Engine

1. Copy here the latest amalgamation files from  https://www.sqlite.org/download.html

2. The `objconv.exe` tool should be downloaded from https://agner.org/optimize/objconv.zip and copied into this folder.

3. Run the `patch-and-compile.sh` script to patch main `sqlite3.c` and cross-compile it for FPC

4. Run `c*.bat` to generate the `sqlite3.o` and `sqlite3.obj` for Delphi Win32/Win64

5. Don't forget to tune the expected *SQLite3* version text in `SynSQLite3Static.pas`


## Cross-Compile

Scripts are supplied to cross-compile from Linux to other systems.

It will use either the cross-compiler as installed by `fpcupdeluxe` or you should manually add some packages.

Here are some instructions for Debian/Ubuntu.

### Cross-Compile to Linux i386 from Linux x86_64

Install the following package:

    sudo apt install libc6-dev:i386

granted the following has been run beforehand:

    dpkg --add-architecture i386

You may also try the `gcc-multilib` package as alternative.

### Cross-Compile To Win32 And Win64

Install the following package:

    sudo apt install mingw-w64

### Cross-Compile to Darwin / Linux ARM/AARCH64

Ensure you installed the latest version of the corresponding cross-compilers in `fpcupdeluxe` (in the *Cross* tab), and modify the `*.sh` path if necessary, from its default value:

    CROSS=/home/ab/fpcup/cross/bin/$ARCH 

## Technical Notes

Our SQlite3 static files use a cut-down, deeply adapted, VFS wrapper from https://github.com/utelle/SQLite3MultipleCiphers calling our `mormot.crypt.core.pas` units for ciphering, and enhanced to ensure compatibily with *bcc32/bcc64* Embarcadero compilers. You will find the patched in this folder, as required by the *Original MIT License - (c) 2006-2020 Ulrich Telle*.
The SQLite3 source is not patched to implement the VFS itself (which is just linked to the static file), but is patched to add some optional key-related high-level features - see https://utelle.github.io/SQLite3MultipleCiphers/docs/architecture/arch_patch - VFS `codecext.c` and `sqlite3mc.c` wrappers redirect to some pascal functions in `mormot.db.raw.sqlite3.static.pas`.

Encryption is done in `CodecAESProcess()` using AES-CTR or AES-OFB. It uses mORMot optimized asm, which is faster than OpenSSL on x86_64 (our main server target).
Each page is encrypted with an IV derived from its page number using primes and AES ciphering. By default with mORMot a page size is 4KB. No overhead is used for IV or HMAC storage. The first bytes of the files are not encrypted, because it is mandatory for proper work with most SQLite3 tools. This is what all other libraries do, including the official (but not free) SSE extension from SQLite3 authors - see https://utelle.github.io/SQLite3MultipleCiphers/docs/ciphers/cipher_legacy_mode

Key derivation from password is done in `CodecGenerateKey()` using PBKDF2 safe iterative key derivation over the SHA-3 (SHAKE_128) algorithm, reduced into 128-bit. There is no benefit of using AES-256 in practice with a password-derivated key, because a 128-bit password using a 80-character alphabet (i.e. a very strong computer generated password) would require at least 21 chars, which is very unlikely in practice. It uses 1000 rounds by default, and you can customize the password derivation using overridden parameters in JSON format instead of the plain password, following `TSynSignerParams` fields.
