
This folder contains all raw binary files needed for FPC static linking.

Mainly needed for SynSQLite3Static, SynLizard, SynCrypto and SynEcc units compilation.

Ensure that "Libraries -fFl" in your FPC project options is defined as:
  ..\static\$(TargetCPU)-$(TargetOS)
(replace ..\static by an absolute/relative path to this folder)

If this folder is void (e.g. when retrieved from https://synopse.info/fossil), you can download all the needed sub-folders from http://synopse.info/files/sqlite3fpc.7z

Ensure you keep in synch these binaries with the main framework source code.
Otherwise, SynSQLite3Static will complain about invalid versions, and some random/unexpected errors may occur.