
This folder contains all raw binary files needed for FPC static linking.

Mainly needed for SynSQLite3Static, SynLizard, SynCrypto and SynEcc units compilation.

Note that such external files are not mandatory to compile the framework source code. There is always a "pure pascal" fallback code available, or use e.g. the official external sqlite3 library. Those .o files were compiled from optimized C/asm, for the best performance, and reduce dependencies or version problems.

Ensure that "Libraries -fFl" in your FPC project options is defined as:
  ..\static\$(TargetCPU)-$(TargetOS)
(replace ..\static by an absolute/relative path to this folder)

If this folder is void (e.g. when retrieved from https://synopse.info/fossil), you can download all the needed sub-folders from http://synopse.info/files/sqlite3fpc.7z

Ensure you keep in synch these binaries with the main framework source code.
Otherwise, SynSQLite3Static will complain about invalid versions, and some random/unexpected errors may occur.

See SQlite3/amalgamation/ReadMe.md for instructions about how to compile the SQlite3 static files after a release from https://sqlite.org