type
  {$ifdef MSWINDOWS}
  TSQLFile = packed record // see struct winFile in sqlite3.c
    pMethods: pointer;     // sqlite3.io_methods_ptr
    pVfs: pointer;         // The VFS used to open this file (new in version 3.7)
    h: THandle;            // Handle for accessing the file
    bulk1: cardinal;       // lockType+sharedLockByte are word-aligned
    bulk2: cardinal;       // ctrlFlags (with DWORD alignment)
    lastErrno: cardinal;   // The Windows errno from the last I/O error
    // asm code generated from c is [esi+20] for lastErrNo -> OK
    pShm: pointer; // not there if SQLITE_OMIT_WAL is defined
    zPath: PAnsiChar;
    szChunk, nFetchOut: integer;
    hMap: THANDLE;
    pMapRegion: PAnsiChar;
    mmapSize, mmapSizeActual, mmapSizeMax: Int64Rec;
  end;
  {$else}
  TSQLFile = record             // see struct unixFile in sqlite3.c
    pMethods: pointer;          // sqlite3.io_methods_ptr
    pVfs: pointer;              // The VFS used to open this file (new in version 3.7)
    unixInodeInfo: pointer;     // Info about locks on this inode
    h: THandle;                 // Handle for accessing the file
    eFileLock: cuchar;          // The type of lock held on this fd
    ctrlFlags: cushort;         // Behavioral bits.  UNIXFILE_* flags
    lastErrno: cint;            // The unix errno from the last I/O error
    lockingContext : PAnsiChar; // Locking style specific state
    UnixUnusedFd : pointer;     // unused
    zPath: PAnsiChar;           // Name of the file
    pShm: pointer; // not there if SQLITE_OMIT_WAL is defined
    szChunk: cint;
    nFetchOut: cint;
    mmapSize, mmapSizeActual, mmapSizeMax: Int64Rec;
    pMapRegion: PAnsiChar;
  end;
  {$endif}

  // qsort() is used if SQLITE_ENABLE_FTS3 is defined
  // this function type is defined for calling termDataCmp() in sqlite3.c
  qsort_compare_func = function(P1,P2: pointer): integer; cdecl; { always cdecl }

