unit puv_error;

interface

const
  PUV_EOF     = -4095;
  PUV_UNKNOWN = -4094;

  PUV_EAI_ADDRFAMILY  = -3000;
  PUV_EAI_AGAIN       = -3001;
  PUV_EAI_BADFLAGS    = -3002;
  PUV_EAI_CANCELED    = -3003;
  PUV_EAI_FAIL        = -3004;
  PUV_EAI_FAMILY      = -3005;
  PUV_EAI_MEMORY      = -3006;
  PUV_EAI_NODATA      = -3007;
  PUV_EAI_NONAME      = -3008;
  PUV_EAI_OVERFLOW    = -3009;
  PUV_EAI_SERVICE     = -3010;
  PUV_EAI_SOCKTYPE    = -3011;
  PUV_EAI_BADHINTS    = -3013;
  PUV_EAI_PROTOCOL    = -3014;

  // Only map to the system errno on non-Windows platforms. It's apparently
  // a fairly common practice for Windows programmers to redefine errno codes.
  {$IFNDEF MSWINDOWS}
  PUV_E2BIG = -ESysE2BIG;
  {$ELSE}
  PUV_E2BIG = -4093;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_EACCES = -ESysEACCES;
  {$ELSE}
  PUV_EACCES = -4092;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_EADDRINUSE = -ESysEADDRINUSE;
  {$ELSE}
  PUV_EADDRINUSE = -4091;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_EADDRNOTAVAIL = -ESysEADDRNOTAVAIL;
  {$ELSE}
  PUV_EADDRNOTAVAIL = -4090;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_EAFNOSUPPORT = -ESysEAFNOSUPPORT;
  {$ELSE}
  PUV_EAFNOSUPPORT = -4089;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_EAGAIN = -ESysEAGAIN;
  {$ELSE}
  PUV_EAGAIN = -4088;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_EALREADY = -ESysEALREADY;
  {$ELSE}
  PUV_EALREADY = -4084;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_EBADF = -ESysEBADF;
  {$ELSE}
  PUV_EBADF = -4083;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_EBUSY = -ESysEBUSY;
  {$ELSE}
  PUV_EBUSY = -4082;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ECANCELED = -ESysECANCELED;
  {$ELSE}
  PUV_ECANCELED = -4081;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ECHARSET = -ESysECHARSET;
  {$ELSE}
  PUV_ECHARSET = -4080;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ECONNABORTED = -ESysECONNABORTED;
  {$ELSE}
  PUV_ECONNABORTED = -4079;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ECONNREFUSED = -ESysECONNREFUSED;
  {$ELSE}
  PUV_ECONNREFUSED = -4078;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ECONNRESET = -ESysECONNRESET;
  {$ELSE}
  PUV_ECONNRESET = -4077;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_EDESTADDRREQ = -ESysEDESTADDRREQ;
  {$ELSE}
  PUV_EDESTADDRREQ = -4076;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_EEXIST = -ESysEEXIST;
  {$ELSE}
  PUV_EEXIST = -4075;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_EFAULT = -ESysEFAULT;
  {$ELSE}
  PUV_EFAULT = -4074;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_EHOSTUNREACH = -ESysEHOSTUNREACH;
  {$ELSE}
  PUV_EHOSTUNREACH = -4073;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_EINTR = -ESysEINTR;
  {$ELSE}
  PUV_EINTR = -4072;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_EINVAL = -ESysEINVAL;
  {$ELSE}
  PUV_EINVAL = -4071;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_EIO = -ESysEIO;
  {$ELSE}
  PUV_EIO = -4070;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_EISCONN = -ESysEISCONN;
  {$ELSE}
  PUV_EISCONN = -4069;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_EISDIR = -ESysEISDIR;
  {$ELSE}
  PUV_EISDIR = -4068;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ELOOP = -ESysELOOP;
  {$ELSE}
  PUV_ELOOP = -4067;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_EMFILE = -ESysEMFILE;
  {$ELSE}
  PUV_EMFILE = -4066;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_EMSGSIZE = -ESysEMSGSIZE;
  {$ELSE}
  PUV_EMSGSIZE = -4065;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ENAMETOOLONG = -ESysENAMETOOLONG;
  {$ELSE}
  PUV_ENAMETOOLONG = -4064;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ENETDOWN = -ESysENETDOWN;
  {$ELSE}
  PUV_ENETDOWN = -4063;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ENETUNREACH = -ESysENETUNREACH;
  {$ELSE}
  PUV_ENETUNREACH = -4062;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ENFILE = -ESysENFILE;
  {$ELSE}
  PUV_ENFILE = -4061;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ENOBUFS = -ESysENOBUFS;
  {$ELSE}
  PUV_ENOBUFS = -4060;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ENODEV = -ESysENODEV;
  {$ELSE}
  PUV_ENODEV = -4059;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ENOENT = -ESysENOENT;
  {$ELSE}
  PUV_ENOENT = -4058;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ENOMEM = -ESysENOMEM;
  {$ELSE}
  PUV_ENOMEM = -4057;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ENONET = -ESysENONET;
  {$ELSE}
  PUV_ENONET = -4056;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ENOSPC = -ESysENOSPC;
  {$ELSE}
  PUV_ENOSPC = -4055;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ENOSYS = -ESysENOSYS;
  {$ELSE}
  PUV_ENOSYS = -4054;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ENOTCONN = -ESysENOTCONN;
  {$ELSE}
  PUV_ENOTCONN = -4053;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ENOTDIR = -ESysENOTDIR;
  {$ELSE}
  PUV_ENOTDIR = -4052;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ENOTEMPTY = -ESysENOTEMPTY;
  {$ELSE}
  PUV_ENOTEMPTY = -4051;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ENOTSOCK = -ESysENOTSOCK;
  {$ELSE}
  PUV_ENOTSOCK = -4050;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ENOTSUP = -ESysENOTSUP;
  {$ELSE}
  PUV_ENOTSUP = -4049;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_EPERM = -ESysEPERM;
  {$ELSE}
  PUV_EPERM = -4048;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_EPIPE = -ESysEPIPE;
  {$ELSE}
  PUV_EPIPE = -4047;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_EPROTO = -ESysEPROTO;
  {$ELSE}
  PUV_EPROTO = -4046;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_EPROTONOSUPPORT = -ESysEPROTONOSUPPORT;
  {$ELSE}
  PUV_EPROTONOSUPPORT = -4045;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_EPROTOTYPE = -ESysEPROTOTYPE;
  {$ELSE}
  PUV_EPROTOTYPE = -4044;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_EROFS = -ESysEROFS;
  {$ELSE}
  PUV_EROFS = -4043;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ESHUTDOWN = -ESysESHUTDOWN;
  {$ELSE}
  PUV_ESHUTDOWN = -4042;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ESPIPE = -ESysESPIPE;
  {$ELSE}
  PUV_ESPIPE = -4041;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ESRCH = -ESysESRCH;
  {$ELSE}
  PUV_ESRCH = -4040;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ETIMEDOUT = -ESysETIMEDOUT;
  {$ELSE}
  PUV_ETIMEDOUT = -4039;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ETXTBSY = -ESysETXTBSY;
  {$ELSE}
  PUV_ETXTBSY = -4038;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_EXDEV = -ESysEXDEV;
  {$ELSE}
  PUV_EXDEV = -4037;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_EFBIG = -ESysEFBIG;
  {$ELSE}
  PUV_EFBIG = -4036;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ENOPROTOOPT = -ESysENOPROTOOPT;
  {$ELSE}
  PUV_ENOPROTOOPT = -4035;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ERANGE = -ESysERANGE;
  {$ELSE}
  PUV_ERANGE = -4034;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ENXIO = -ESysENXIO;
  {$ELSE}
  PUV_ENXIO = -4033;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_EMLINK = -ESysEMLINK;
  {$ELSE}
  PUV_EMLINK = -4032;
  {$ENDIF}

  // EHOSTDOWN is not visible on BSD-like systems when _POSIX_C_SOURCE is
  // defined. Fortunately, its value is always 64 so it's possible albeit
  // icky to hard-code it.
  {$IFNDEF MSWINDOWS}
  PUV_EHOSTDOWN = -ESysEHOSTDOWN;
  PUV_EHOSTDOWN = -64;
  {$ELSE}
  PUV_EHOSTDOWN = -4031;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_EREMOTEIO = -ESysEREMOTEIO;
  {$ELSE}
  PUV_EREMOTEIO = -4030;
  {$ENDIF}

  {$IFNDEF MSWINDOWS}
  PUV_ENOTTY = -ESysENOTTY;
  {$ELSE}
  PUV_ENOTTY = -4029;
  {$ENDIF}

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

function puv_errno_str(errno: Integer): String;
function puv_errno_codestr(errno: Integer): String;
{$IFDEF MSWINDOWS}
function puv_translate_sys_error(sys_errno: Integer): Integer;
{$ENDIF}

implementation

uses
  SysUtils,
{$IFDEF MSWINDOWS}
  Windows,
  winsock;

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
{$ENDIF}

function puv_errno_str(errno: Integer): String;
begin
  case errno of
    PUV_E2BIG: Result := 'argument list too long';
    PUV_EACCES: Result := 'permission denied';
    PUV_EADDRINUSE: Result := 'address already in use';
    PUV_EADDRNOTAVAIL: Result := 'address not available';
    PUV_EAFNOSUPPORT: Result := 'address family not supported';
    PUV_EAGAIN: Result := 'resource temporarily unavailable';
    PUV_EAI_ADDRFAMILY: Result := 'address family not supported';
    PUV_EAI_AGAIN: Result := 'temporary failure';
    PUV_EAI_BADFLAGS: Result := 'bad ai_flags value';
    PUV_EAI_BADHINTS: Result := 'invalid value for hints';
    PUV_EAI_CANCELED: Result := 'request canceled';
    PUV_EAI_FAIL: Result := 'permanent failure';
    PUV_EAI_FAMILY: Result := 'ai_family not supported';
    PUV_EAI_MEMORY: Result := 'out of memory';
    PUV_EAI_NODATA: Result := 'no address';
    PUV_EAI_NONAME: Result := 'unknown node or service';
    PUV_EAI_OVERFLOW: Result := 'argument buffer overflow';
    PUV_EAI_PROTOCOL: Result := 'resolved protocol is unknown';
    PUV_EAI_SERVICE: Result := 'service not available for socket type';
    PUV_EAI_SOCKTYPE: Result := 'socket type not supported';
    PUV_EALREADY: Result := 'connection already in progress';
    PUV_EBADF: Result := 'bad file descriptor';
    PUV_EBUSY: Result := 'resource busy or locked';
    PUV_ECANCELED: Result := 'operation canceled';
    PUV_ECHARSET: Result := 'invalid Unicode character';
    PUV_ECONNABORTED: Result := 'software caused connection abort';
    PUV_ECONNREFUSED: Result := 'connection refused';
    PUV_ECONNRESET: Result := 'connection reset by peer';
    PUV_EDESTADDRREQ: Result := 'destination address required';
    PUV_EEXIST: Result := 'file already exists';
    PUV_EFAULT: Result := 'bad address in system call argument';
    PUV_EFBIG: Result := 'file too large';
    PUV_EHOSTUNREACH: Result := 'host is unreachable';
    PUV_EINTR: Result := 'interrupted system call';
    PUV_EINVAL: Result := 'invalid argument';
    PUV_EIO: Result := 'i/o error';
    PUV_EISCONN: Result := 'socket is already connected';
    PUV_EISDIR: Result := 'illegal operation on a directory';
    PUV_ELOOP: Result := 'too many symbolic links encountered';
    PUV_EMFILE: Result := 'too many open files';
    PUV_EMSGSIZE: Result := 'message too long';
    PUV_ENAMETOOLONG: Result := 'name too long';
    PUV_ENETDOWN: Result := 'network is down';
    PUV_ENETUNREACH: Result := 'network is unreachable';
    PUV_ENFILE: Result := 'file table overflow';
    PUV_ENOBUFS: Result := 'no buffer space available';
    PUV_ENODEV: Result := 'no such device';
    PUV_ENOENT: Result := 'no such file or directory';
    PUV_ENOMEM: Result := 'not enough memory';
    PUV_ENONET: Result := 'machine is not on the network';
    PUV_ENOPROTOOPT: Result := 'protocol not available';
    PUV_ENOSPC: Result := 'no space left on device';
    PUV_ENOSYS: Result := 'function not implemented';
    PUV_ENOTCONN: Result := 'socket is not connected';
    PUV_ENOTDIR: Result := 'not a directory';
    PUV_ENOTEMPTY: Result := 'directory not empty';
    PUV_ENOTSOCK: Result := 'socket operation on non-socket';
    PUV_ENOTSUP: Result := 'operation not supported on socket';
    PUV_EPERM: Result := 'operation not permitted';
    PUV_EPIPE: Result := 'broken pipe';
    PUV_EPROTO: Result := 'protocol error';
    PUV_EPROTONOSUPPORT: Result := 'protocol not supported';
    PUV_EPROTOTYPE: Result := 'protocol wrong type for socket';
    PUV_ERANGE: Result := 'result too large';
    PUV_EROFS: Result := 'read-only file system';
    PUV_ESHUTDOWN: Result := 'cannot send after transport endpoint shutdown';
    PUV_ESPIPE: Result := 'invalid seek';
    PUV_ESRCH: Result := 'no such process';
    PUV_ETIMEDOUT: Result := 'connection timed out';
    PUV_ETXTBSY: Result := 'text file is busy';
    PUV_EXDEV: Result := 'cross-device link not permitted';
    PUV_UNKNOWN: Result := 'unknown error';
    PUV_EOF: Result := 'end of file';
    PUV_ENXIO: Result := 'no such device or address';
    PUV_EMLINK: Result := 'too many links';
    PUV_EHOSTDOWN: Result := 'host is down';
    PUV_EREMOTEIO: Result := 'remote I/O error';
    PUV_ENOTTY: Result := 'inappropriate ioctl for device';
    else
      Result := Format('Unknown system error %d', [errno]);
  end;
end;

function puv_errno_codestr(errno: Integer): String;
begin
  case errno of
    PUV_E2BIG: Result := 'E2BIG';
    PUV_EACCES: Result := 'EACCES';
    PUV_EADDRINUSE: Result := 'EADDRINUSE';
    PUV_EADDRNOTAVAIL: Result := 'EADDRNOTAVAIL';
    PUV_EAFNOSUPPORT: Result := 'EAFNOSUPPORT';
    PUV_EAGAIN: Result := 'EAGAIN';
    PUV_EAI_ADDRFAMILY: Result := 'EAI_ADDRFAMILY';
    PUV_EAI_AGAIN: Result := 'EAI_AGAIN';
    PUV_EAI_BADFLAGS: Result := 'EAI_BADFLAGS';
    PUV_EAI_BADHINTS: Result := 'EAI_BADHINTS';
    PUV_EAI_CANCELED: Result := 'EAI_CANCELED';
    PUV_EAI_FAIL: Result := 'EAI_FAIL';
    PUV_EAI_FAMILY: Result := 'EAI_FAMILY';
    PUV_EAI_MEMORY: Result := 'EAI_MEMORY';
    PUV_EAI_NODATA: Result := 'EAI_NODATA';
    PUV_EAI_NONAME: Result := 'EAI_NONAME';
    PUV_EAI_OVERFLOW: Result := 'EAI_OVERFLOW';
    PUV_EAI_PROTOCOL: Result := 'EAI_PROTOCOL';
    PUV_EAI_SERVICE: Result := 'EAI_SERVICE';
    PUV_EAI_SOCKTYPE: Result := 'EAI_SOCKTYPE';
    PUV_EALREADY: Result := 'EALREADY';
    PUV_EBADF: Result := 'EBADF';
    PUV_EBUSY: Result := 'EBUSY';
    PUV_ECANCELED: Result := 'ECANCELED';
    PUV_ECHARSET: Result := 'ECHARSET';
    PUV_ECONNABORTED: Result := 'ECONNABORTED';
    PUV_ECONNREFUSED: Result := 'ECONNREFUSED';
    PUV_ECONNRESET: Result := 'ECONNRESET';
    PUV_EDESTADDRREQ: Result := 'EDESTADDRREQ';
    PUV_EEXIST: Result := 'EEXIST';
    PUV_EFAULT: Result := 'EFAULT';
    PUV_EFBIG: Result := 'EFBIG';
    PUV_EHOSTUNREACH: Result := 'EHOSTUNREACH';
    PUV_EINTR: Result := 'EINTR';
    PUV_EINVAL: Result := 'EINVAL';
    PUV_EIO: Result := 'EIO';
    PUV_EISCONN: Result := 'EISCONN';
    PUV_EISDIR: Result := 'EISDIR';
    PUV_ELOOP: Result := 'ELOOP';
    PUV_EMFILE: Result := 'EMFILE';
    PUV_EMSGSIZE: Result := 'EMSGSIZE';
    PUV_ENAMETOOLONG: Result := 'ENAMETOOLONG';
    PUV_ENETDOWN: Result := 'ENETDOWN';
    PUV_ENETUNREACH: Result := 'ENETUNREACH';
    PUV_ENFILE: Result := 'ENFILE';
    PUV_ENOBUFS: Result := 'ENOBUFS';
    PUV_ENODEV: Result := 'ENODEV';
    PUV_ENOENT: Result := 'ENOENT';
    PUV_ENOMEM: Result := 'ENOMEM';
    PUV_ENONET: Result := 'ENONET';
    PUV_ENOPROTOOPT: Result := 'ENOPROTOOPT';
    PUV_ENOSPC: Result := 'ENOSPC';
    PUV_ENOSYS: Result := 'ENOSYS';
    PUV_ENOTCONN: Result := 'ENOTCONN';
    PUV_ENOTDIR: Result := 'ENOTDIR';
    PUV_ENOTEMPTY: Result := 'ENOTEMPTY';
    PUV_ENOTSOCK: Result := 'ENOTSOCK';
    PUV_ENOTSUP: Result := 'ENOTSUP';
    PUV_EPERM: Result := 'EPERM';
    PUV_EPIPE: Result := 'EPIPE';
    PUV_EPROTO: Result := 'EPROTO';
    PUV_EPROTONOSUPPORT: Result := 'EPROTONOSUPPORT';
    PUV_EPROTOTYPE: Result := 'EPROTOTYPE';
    PUV_ERANGE: Result := 'ERANGE';
    PUV_EROFS: Result := 'EROFS';
    PUV_ESHUTDOWN: Result := 'ESHUTDOWN';
    PUV_ESPIPE: Result := 'ESPIPE';
    PUV_ESRCH: Result := 'ESRCH';
    PUV_ETIMEDOUT: Result := 'ETIMEDOUT';
    PUV_ETXTBSY: Result := 'ETXTBSY';
    PUV_EXDEV: Result := 'EXDEV';
    PUV_UNKNOWN: Result := 'UNKNOWN';
    PUV_EOF: Result := 'EOF';
    PUV_ENXIO: Result := 'ENXIO';
    PUV_EMLINK: Result := 'EMLINK';
    PUV_EHOSTDOWN: Result := 'EHOSTDOWN';
    PUV_EREMOTEIO: Result := 'EREMOTEIO';
    PUV_ENOTTY: Result := 'ENOTTY';
    else
      Result := Format('Unknown system error %d', [errno]);
  end;
end;

end.

