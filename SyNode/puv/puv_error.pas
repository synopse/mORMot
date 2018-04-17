unit puv_error;

interface

const
  // Standard Unix errors from FPC's errno.inc
  PUV_EPERM           = 1;    { Operation not permitted }
  PUV_ENOENT          = 2;    { No such file or directory }
  PUV_ESRCH           = 3;    { No such process }
  PUV_EINTR           = 4;    { Interrupted system call }
  PUV_EIO             = 5;    { I/O error }
  PUV_ENXIO           = 6;    { No such device or address }
  PUV_E2BIG           = 7;    { Arg list too long }
  PUV_ENOEXEC         = 8;    { Exec format error }
  PUV_EBADF           = 9;    { Bad file number }
  PUV_ECHILD          = 10;   { No child processes }
  PUV_EAGAIN          = 11;   { Try again }
  PUV_ENOMEM          = 12;   { Out of memory }
  PUV_EACCES          = 13;   { Permission denied }
  PUV_EFAULT          = 14;   { Bad address }
  PUV_ENOTBLK         = 15;   { Block device required, NOT POSIX! }
  PUV_EBUSY           = 16;   { Device or resource busy }
  PUV_EEXIST          = 17;   { File exists }
  PUV_EXDEV           = 18;   { Cross-device link }
  PUV_ENODEV          = 19;   { No such device }
  PUV_ENOTDIR         = 20;   { Not a directory }
  PUV_EISDIR          = 21;   { Is a directory }
  PUV_EINVAL          = 22;   { Invalid argument }
  PUV_ENFILE          = 23;   { File table overflow }
  PUV_EMFILE          = 24;   { Too many open files }
  PUV_ENOTTY          = 25;   { Not a typewriter }
  PUV_ETXTBSY         = 26;   { Text file busy. The new process was
                                a pure procedure (shared text) file which was
                                open for writing by another process, or file
                                which was open for writing by another process,
                                or while the pure procedure file was being
                                executed an open(2) call requested write access
                                requested write access.}
  PUV_EFBIG           = 27;   { File too large }
  PUV_ENOSPC          = 28;   { No space left on device }
  PUV_ESPIPE          = 29;   { Illegal seek }
  PUV_EROFS           = 30;   { Read-only file system }
  PUV_EMLINK          = 31;   { Too many links }
  PUV_EPIPE           = 32;   { Broken pipe }
  PUV_EDOM            = 33;   { Math argument out of domain of func }
  PUV_ERANGE          = 34;   { Math result not representable }


  PUV_EDEADLK         = 35;   { Resource deadlock would occur }
  PUV_ENAMETOOLONG    = 36;   { File name too long }
  PUV_ENOLCK          = 37;   { No record locks available }
  PUV_ENOSYS          = 38;   { Function not implemented }
  PUV_ENOTEMPTY       = 39;   { Directory not empty }
  PUV_ELOOP           = 40;   { Too many symbolic links encountered }
  PUV_EWOULDBLOCK     = PUV_EAGAIN;   { Operation would block }
  PUV_ENOMSG          = 42;   { No message of desired type }
  PUV_EIDRM           = 43;   { Identifier removed }
  PUV_ECHRNG          = 44;   { Channel number out of range }
  PUV_EL2NSYNC        = 45;   { Level 2 not synchronized }
  PUV_EL3HLT          = 46;   { Level 3 halted }
  PUV_EL3RST          = 47;   { Level 3 reset }
  PUV_ELNRNG          = 48;   { Link number out of range }
  PUV_EUNATCH         = 49;   { Protocol driver not attached }
  PUV_ENOCSI          = 50;   { No CSI structure available }
  PUV_EL2HLT          = 51;   { Level 2 halted }
  PUV_EBADE           = 52;   { Invalid exchange }
  PUV_EBADR           = 53;   { Invalid request descriptor }
  PUV_EXFULL          = 54;   { Exchange full }
  PUV_ENOANO          = 55;   { No anode }
  PUV_EBADRQC         = 56;   { Invalid request code }
  PUV_EBADSLT         = 57;   { Invalid slot }
  PUV_EDEADLOCK       = PUV_EDEADLK; { number 58 is missing }
  PUV_EBFONT          = 59;   { Bad font file format }
  PUV_ENOSTR          = 60;   { Device not a stream }
  PUV_ENODATA         = 61;   { No data available }
  PUV_ETIME           = 62;   { Timer expired }
  PUV_ENOSR           = 63;   { Out of streams resources }
  PUV_ENONET          = 64;   { Machine is not on the network }
  PUV_ENOPKG          = 65;   { Package not installed }
  PUV_EREMOTE         = 66;   { Object is remote }
  PUV_ENOLINK         = 67;   { Link has been severed }
  PUV_EADV            = 68;   { Advertise error }
  PUV_ESRMNT          = 69;   { Srmount error }
  PUV_ECOMM           = 70;   { Communication error on send }
  PUV_EPROTO          = 71;   { Protocol error }
  PUV_EMULTIHOP       = 72;   { Multihop attempted }
  PUV_EDOTDOT         = 73;   { RFS specific error }
  PUV_EBADMSG         = 74;   { Not a data message }
  PUV_EOVERFLOW       = 75;   { Value too large for defined data type }
  PUV_ENOTUNIQ        = 76;   { Name not unique on network }
  PUV_EBADFD          = 77;   { File descriptor in bad state }
  PUV_EREMCHG         = 78;   { Remote address changed }
  PUV_ELIBACC         = 79;   { Can not access a needed shared library }
  PUV_ELIBBAD         = 80;   { Accessing a corrupted shared library }
  PUV_ELIBSCN         = 81;   { .lib section in a.out corrupted }
  PUV_ELIBMAX         = 82;   { Attempting to link in too many shared libraries }
  PUV_ELIBEXEC        = 83;   { Cannot exec a shared library directly }
  PUV_EILSEQ          = 84;   { Illegal byte sequence }
  PUV_ERESTART        = 85;   { Interrupted system call should be restarted }
  PUV_ESTRPIPE        = 86;   { Streams pipe error }
  PUV_EUSERS          = 87;   { Too many users }
  PUV_ENOTSOCK        = 88;   { Socket operation on non-socket }
  PUV_EDESTADDRREQ    = 89;   { Destination address required }
  PUV_EMSGSIZE        = 90;   { Message too long }
  PUV_EPROTOTYPE      = 91;   { Protocol wrong type for socket }
  PUV_ENOPROTOOPT     = 92;   { Protocol not available }
  PUV_EPROTONOSUPPORT = 93;   { Protocol not supported }
  PUV_ESOCKTNOSUPPORT = 94;   { Socket type not supported }
  PUV_EOPNOTSUPP      = 95;   { Operation not supported on transport endpoint }
  PUV_EPFNOSUPPORT    = 96;   { Protocol family not supported }
  PUV_EAFNOSUPPORT    = 97;   { Address family not supported by protocol }
  PUV_EADDRINUSE      = 98;   { Address already in use }
  PUV_EADDRNOTAVAIL   = 99;   { Cannot assign requested address }
  PUV_ENETDOWN        = 100;  { Network is down }
  PUV_ENETUNREACH     = 101;  { Network is unreachable }
  PUV_ENETRESET       = 102;  { Network dropped connection because of reset }
  PUV_ECONNABORTED    = 103;  { Software caused connection abort }
  PUV_ECONNRESET      = 104;  { Connection reset by peer }
  PUV_ENOBUFS         = 105;  { No buffer space available }
  PUV_EISCONN         = 106;  { Transport endpoint is already connected }
  PUV_ENOTCONN        = 107;  { Transport endpoint is not connected }
  PUV_ESHUTDOWN       = 108;  { Cannot send after transport endpoint shutdown }
  PUV_ETOOMANYREFS    = 109;  { Too many references: cannot splice }
  PUV_ETIMEDOUT       = 110;  { Connection timed out }
  PUV_ECONNREFUSED    = 111;  { Connection refused }
  PUV_EHOSTDOWN       = 112;  { Host is down }
  PUV_EHOSTUNREACH    = 113;  { No route to host }
  PUV_EALREADY        = 114;  { Operation already in progress }
  PUV_EINPROGRESS     = 115;  { Operation now in progress }
  PUV_ESTALE          = 116;  { Stale NFS file handle }
  PUV_EUCLEAN         = 117;  { Structure needs cleaning }
  PUV_ENOTNAM         = 118;  { Not a XENIX named type file }
  PUV_ENAVAIL         = 119;  { No XENIX semaphores available }
  PUV_EISNAM          = 120;  { Is a named type file }
  PUV_EREMOTEIO       = 121;  { Remote I/O error }
  PUV_EDQUOT          = 122;  { Quota exceeded }

  PUV_ENOMEDIUM       = 123;
  PUV_EMEDIUMTYPE     = 124;
  PUV_ECANCELED       = 125;
  PUV_ENOKEY          = 126;
  PUV_EKEYEXPIRED     = 127;
  PUV_EKEYREVOKED     = 128;
  PUV_EKEYREJECTED    = 129;
  PUV_EOWNERDEAD      = 130;
  PUV_ENOTRECOVERABLE = 131;
  PUV_ERFKILL         = 132;

  // Specific codes. Not sure is these needed
  PUV_ENOTSUP         = -4049;
  PUV_ECHARSET        = -4080;
  PUV_UNKNOWN         = -4094;
  PUV_EOF             = -4095;

{$IFDEF MSWINDOWS}
// Omitted standard constants
  ERROR_NOT_SUPPORTED             =   50;
  ERROR_BROKEN_PIPE               =  109;
  ERROR_ELEVATION_REQUIRED        =  740;
  ERROR_NO_UNICODE_TRANSLATION    = 1113;
  ERROR_DEVICE_REQUIRES_CLEANING  = 1165;
  ERROR_DEVICE_DOOR_OPEN          = 1166;
  ERROR_SYMLINK_NOT_SUPPORTED     = 1464;
  ERROR_CANT_RESOLVE_FILENAME     = 1921;
  ERROR_INVALID_REPARSE_DATA      = 4392;
{$ENDIF}

function puv_get_errno: Integer;
function puv_errno_str(errno: Integer): String;
function puv_translate_sys_error(sys_errno: Integer): Integer;

implementation
{$IFDEF MSWINDOWS}
uses
  Windows,
  winsock;

function _get_errno(out errno: LongInt): LongInt; cdecl;
  external 'msvcrt';

function puv_get_errno: Integer;
begin
  _get_errno(Result);
end;

function puv_translate_sys_error(sys_errno: Integer): Integer;
begin
  if (sys_errno <= 0) then
    Result := sys_errno  // If < 0 then it's already a puv error.
  else
    case (sys_errno) of
      ERROR_NOACCESS:                    Result := PUV_EACCES;
      WSAEACCES:                         Result := PUV_EACCES;
      ERROR_ELEVATION_REQUIRED:          Result := PUV_EACCES;
      ERROR_ADDRESS_ALREADY_ASSOCIATED:  Result := PUV_EADDRINUSE;
      WSAEADDRINUSE:                     Result := PUV_EADDRINUSE;
      WSAEADDRNOTAVAIL:                  Result := PUV_EADDRNOTAVAIL;
      WSAEAFNOSUPPORT:                   Result := PUV_EAFNOSUPPORT;
      WSAEWOULDBLOCK:                    Result := PUV_EAGAIN;
      WSAEALREADY:                       Result := PUV_EALREADY;
      ERROR_INVALID_FLAGS:               Result := PUV_EBADF;
      ERROR_INVALID_HANDLE:              Result := PUV_EBADF;
      ERROR_LOCK_VIOLATION:              Result := PUV_EBUSY;
      ERROR_PIPE_BUSY:                   Result := PUV_EBUSY;
      ERROR_SHARING_VIOLATION:           Result := PUV_EBUSY;
      ERROR_OPERATION_ABORTED:           Result := PUV_ECANCELED;
      WSAEINTR:                          Result := PUV_ECANCELED;
      ERROR_NO_UNICODE_TRANSLATION:      Result := PUV_ECHARSET;
      ERROR_CONNECTION_ABORTED:          Result := PUV_ECONNABORTED;
      WSAECONNABORTED:                   Result := PUV_ECONNABORTED;
      ERROR_CONNECTION_REFUSED:          Result := PUV_ECONNREFUSED;
      WSAECONNREFUSED:                   Result := PUV_ECONNREFUSED;
      ERROR_NETNAME_DELETED:             Result := PUV_ECONNRESET;
      WSAECONNRESET:                     Result := PUV_ECONNRESET;
      ERROR_ALREADY_EXISTS:              Result := PUV_EEXIST;
      ERROR_FILE_EXISTS:                 Result := PUV_EEXIST;
      ERROR_BUFFER_OVERFLOW:             Result := PUV_EFAULT;
      WSAEFAULT:                         Result := PUV_EFAULT;
      ERROR_HOST_UNREACHABLE:            Result := PUV_EHOSTUNREACH;
      WSAEHOSTUNREACH:                   Result := PUV_EHOSTUNREACH;
      ERROR_INSUFFICIENT_BUFFER:         Result := PUV_EINVAL;
      ERROR_INVALID_DATA:                Result := PUV_EINVAL;
      ERROR_INVALID_PARAMETER:           Result := PUV_EINVAL;
      ERROR_SYMLINK_NOT_SUPPORTED:       Result := PUV_EINVAL;
      WSAEINVAL:                         Result := PUV_EINVAL;
      WSAEPFNOSUPPORT:                   Result := PUV_EINVAL;
      WSAESOCKTNOSUPPORT:                Result := PUV_EINVAL;
      ERROR_BEGINNING_OF_MEDIA:          Result := PUV_EIO;
      ERROR_BUS_RESET:                   Result := PUV_EIO;
      ERROR_CRC:                         Result := PUV_EIO;
      ERROR_DEVICE_DOOR_OPEN:            Result := PUV_EIO;
      ERROR_DEVICE_REQUIRES_CLEANING:    Result := PUV_EIO;
      ERROR_DISK_CORRUPT:                Result := PUV_EIO;
      ERROR_EOM_OVERFLOW:                Result := PUV_EIO;
      ERROR_FILEMARK_DETECTED:           Result := PUV_EIO;
      ERROR_GEN_FAILURE:                 Result := PUV_EIO;
      ERROR_INVALID_BLOCK_LENGTH:        Result := PUV_EIO;
      ERROR_IO_DEVICE:                   Result := PUV_EIO;
      ERROR_NO_DATA_DETECTED:            Result := PUV_EIO;
      ERROR_NO_SIGNAL_SENT:              Result := PUV_EIO;
      ERROR_OPEN_FAILED:                 Result := PUV_EIO;
      ERROR_SETMARK_DETECTED:            Result := PUV_EIO;
      ERROR_SIGNAL_REFUSED:              Result := PUV_EIO;
      WSAEISCONN:                        Result := PUV_EISCONN;
      ERROR_CANT_RESOLVE_FILENAME:       Result := PUV_ELOOP;
      ERROR_TOO_MANY_OPEN_FILES:         Result := PUV_EMFILE;
      WSAEMFILE:                         Result := PUV_EMFILE;
      WSAEMSGSIZE:                       Result := PUV_EMSGSIZE;
      ERROR_FILENAME_EXCED_RANGE:        Result := PUV_ENAMETOOLONG;
      ERROR_NETWORK_UNREACHABLE:         Result := PUV_ENETUNREACH;
      WSAENETUNREACH:                    Result := PUV_ENETUNREACH;
      WSAENOBUFS:                        Result := PUV_ENOBUFS;
      ERROR_BAD_PATHNAME:                Result := PUV_ENOENT;
      ERROR_DIRECTORY:                   Result := PUV_ENOENT;
      ERROR_FILE_NOT_FOUND:              Result := PUV_ENOENT;
      ERROR_INVALID_NAME:                Result := PUV_ENOENT;
      ERROR_INVALID_DRIVE:               Result := PUV_ENOENT;
      ERROR_INVALID_REPARSE_DATA:        Result := PUV_ENOENT;
      ERROR_MOD_NOT_FOUND:               Result := PUV_ENOENT;
      ERROR_PATH_NOT_FOUND:              Result := PUV_ENOENT;
      WSAHOST_NOT_FOUND:                 Result := PUV_ENOENT;
      WSANO_DATA:                        Result := PUV_ENOENT;
      ERROR_NOT_ENOUGH_MEMORY:           Result := PUV_ENOMEM;
      ERROR_OUTOFMEMORY:                 Result := PUV_ENOMEM;
      ERROR_CANNOT_MAKE:                 Result := PUV_ENOSPC;
      ERROR_DISK_FULL:                   Result := PUV_ENOSPC;
      ERROR_EA_TABLE_FULL:               Result := PUV_ENOSPC;
      ERROR_END_OF_MEDIA:                Result := PUV_ENOSPC;
      ERROR_HANDLE_DISK_FULL:            Result := PUV_ENOSPC;
      ERROR_NOT_CONNECTED:               Result := PUV_ENOTCONN;
      WSAENOTCONN:                       Result := PUV_ENOTCONN;
      ERROR_DIR_NOT_EMPTY:               Result := PUV_ENOTEMPTY;
      WSAENOTSOCK:                       Result := PUV_ENOTSOCK;
      ERROR_NOT_SUPPORTED:               Result := PUV_ENOTSUP;
      ERROR_BROKEN_PIPE:                 Result := PUV_EOF;
      ERROR_ACCESS_DENIED:               Result := PUV_EPERM;
      ERROR_PRIVILEGE_NOT_HELD:          Result := PUV_EPERM;
      ERROR_BAD_PIPE:                    Result := PUV_EPIPE;
      ERROR_NO_DATA:                     Result := PUV_EPIPE;
      ERROR_PIPE_NOT_CONNECTED:          Result := PUV_EPIPE;
      WSAESHUTDOWN:                      Result := PUV_EPIPE;
      WSAEPROTONOSUPPORT:                Result := PUV_EPROTONOSUPPORT;
      ERROR_WRITE_PROTECT:               Result := PUV_EROFS;
      ERROR_SEM_TIMEOUT:                 Result := PUV_ETIMEDOUT;
      WSAETIMEDOUT:                      Result := PUV_ETIMEDOUT;
      ERROR_NOT_SAME_DEVICE:             Result := PUV_EXDEV;
      ERROR_INVALID_FUNCTION:            Result := PUV_EISDIR;
      ERROR_META_EXPANSION_TOO_LONG:     Result := PUV_E2BIG;
    else
      Result := PUV_UNKNOWN;
  end;
end;
{$ELSE}
uses
  BaseUnix;

function puv_get_errno: Integer;
begin
  Result := fpgeterrno;
end;

function puv_translate_sys_error(sys_errno: Integer): Integer;
begin
  Result := sys_errno; // Under Unix this is the same
end;
{$ENDIF}
function puv_errno_str(errno: Integer): String;
const
  err_strs: array [1..132] of String = (
  'EPERM',
  'ENOENT',
  'ESRCH',
  'EINTR',
  'EIO',
  'ENXIO',
  'E2BIG',
  'ENOEXEC',
  'EBADF',
  'ECHILD',
  'EAGAIN',
  'ENOMEM',
  'EACCES',
  'EFAULT',
  'ENOTBLK',
  'EBUSY',
  'EEXIST',
  'EXDEV',
  'ENODEV',
  'ENOTDIR',
  'EISDIR',
  'EINVAL',
  'ENFILE',
  'EMFILE',
  'ENOTTY',
  'ETXTBSY',
  'EFBIG',
  'ENOSPC',
  'ESPIPE',
  'EROFS',
  'EMLINK',
  'EPIPE',
  'EDOM',
  'ERANGE',
  'EDEADLK',
  'ENAMETOOLONG',
  'ENOLCK',
  'ENOSYS',
  'ENOTEMPTY',
  'ELOOP',
  '',
  'ENOMSG',
  'EIDRM',
  'ECHRNG',
  'EL2NSYNC',
  'EL3HLT',
  'EL3RST',
  'ELNRNG',
  'EUNATCH',
  'ENOCSI',
  'EL2HLT',
  'EBADE',
  'EBADR',
  'EXFULL',
  'ENOANO',
  'EBADRQC',
  'EBADSLT',
  '',
  'EBFONT',
  'ENOSTR',
  'ENODATA',
  'ETIME',
  'ENOSR',
  'ENONET',
  'ENOPKG',
  'EREMOTE',
  'ENOLINK',
  'EADV',
  'ESRMNT',
  'ECOMM',
  'EPROTO',
  'EMULTIHOP',
  'EDOTDOT',
  'EBADMSG',
  'EOVERFLOW',
  'ENOTUNIQ',
  'EBADFD',
  'EREMCHG',
  'ELIBACC',
  'ELIBBAD',
  'ELIBSCN',
  'ELIBMAX',
  'ELIBEXEC',
  'EILSEQ',
  'ERESTART',
  'ESTRPIPE',
  'EUSERS',
  'ENOTSOCK',
  'EDESTADDRREQ',
  'EMSGSIZE',
  'EPROTOTYPE',
  'ENOPROTOOPT',
  'EPROTONOSUPPORT',
  'ESOCKTNOSUPPORT',
  'EOPNOTSUPP',
  'EPFNOSUPPORT',
  'EAFNOSUPPORT',
  'EADDRINUSE',
  'EADDRNOTAVAIL',
  'ENETDOWN',
  'ENETUNREACH',
  'ENETRESET',
  'ECONNABORTED',
  'ECONNRESET',
  'ENOBUFS',
  'EISCONN',
  'ENOTCONN',
  'ESHUTDOWN',
  'ETOOMANYREFS',
  'ETIMEDOUT',
  'ECONNREFUSED',
  'EHOSTDOWN',
  'EHOSTUNREACH',
  'EALREADY',
  'EINPROGRESS',
  'ESTALE',
  'EUCLEAN',
  'ENOTNAM',
  'ENAVAIL',
  'EISNAM',
  'EREMOTEIO',
  'EDQUOT',
  'ENOMEDIUM',
  'EMEDIUMTYPE',
  'ECANCELED',
  'ENOKEY',
  'EKEYEXPIRED',
  'EKEYREVOKED',
  'EKEYREJECTED',
  'EOWNERDEAD',
  'ENOTRECOVERABLE',
  'ERFKILL');
begin
  if (errno >= Low(err_strs)) and (errno <= High(err_strs)) then
    Result := err_strs[errno]
  else
    Result := '';
end;

end.

