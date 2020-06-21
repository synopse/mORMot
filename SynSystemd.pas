/// curl library direct access classes
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynSystemd;

{
  *****************************************************************************
   Access to SystemD features for modern Linux using libsystemd

   Features:
    - a building block for THTTPServer socket activation

   Limitations:
    - Linux only

  *****************************************************************************
}

interface

{$I Synopse.inc} // define HASINLINE CPU32 CPU64 OWNNORMTOUPPER

uses
  {$ifndef LINUXNOTBSD}
  // SynSystemD is for Linux only
  {$endif}
  SysUtils,
  SynCommons;

const
  /// The first passed file descriptor is fd 3
  SD_LISTEN_FDS_START = 3;
  /// low-level libcurl library file name, depending on the running OS
  LIBSYSTEMD_PATH = 'libsystemd.so.0';
  ENV_INVOCATION_ID = 'INVOCATION_ID';

type
  /// low-level exception raised during systemd library access
  ESystemd = class(Exception);

var
  /// low-level late binding functions access to the systemd library API
  // - ensure you called LibSystemdInitialize or SystemdIsAvailable functions to
  // setup this global instance before using any of its internal functions
  // - see also https://www.freedesktop.org/wiki/Software/systemd/
  // and http://0pointer.de/blog/projects/socket-activation.html
  // - to get a headers on debian - `sudo apt install libsystemd-dev && cd /usr/include/systemd`
  sd: packed record
    /// hold a reference to the loaded library
    // - PtrInt(Module)=0 before initialization, or PtrInt(Module) = -1
    // on initialization failure, or PtrInt(Module)>0 if loaded
    {$ifdef FPC}
    Module: TLibHandle;
    {$else}
    Module: THandle;
    {$endif FPC}
    /// returns how many file descriptors have been passed to process.
    // If result=1 then socket for accepting connection is SD_LISTEN_FDS_START
    listen_fds: function(unset_environment: integer): integer; cdecl;
    /// returns 1 if the file descriptor is an AF_UNIX socket of the specified type and path
    is_socket_unix: function(fd, typr, listening: integer;
      var path: TFileName; pathLength: PtrUInt): integer; cdecl;
    /// submit simple, plain text log entries to the system journal
    // priority can be obtained using longint(LOG_TO_SYSLOG[logLevel])
    journal_print: function(priority: longint; args: array of const): longint; cdecl;
    /// sends notification to systemd. See https://www.freedesktop.org/software/systemd/man/sd_notify.html
    // status notification sample: sd.notify(0, 'READY=1');
    // watchdog notification: sd.notify(0, 'WATCHDOG=1');
    notify: function(unset_environment: longint; state: PUTF8Char): longint; cdecl;
    /// check whether the service manager expects watchdog keep-alive notifications from a service
    // If result > 0 then usec contains notification interval (app should notify every usec\2 uniseconds)
    watchdog_enabled: function(unset_environment: longint; usec: Puint64): longint; cdecl;
  end;

/// return TRUE if a systemd library is available
// - will load and initialize it, calling LibSystemdInitialize if necessary,
// catching any exception during the process
function SystemdIsAvailable: boolean;

/// Return true in case process is started by systemd
// For systemd v232+
function ProcessIsStartedBySystemd: boolean;

/// initialize the libsystemd API
// - do nothing if the library has already been loaded
// - will raise ESsytemd exception on any loading issue
procedure LibSystemdInitialize(const libname: TFileName=LIBSYSTEMD_PATH);

implementation

uses
  BaseUnix,
  SynFPCSock; // SynSockCS

function SystemdIsAvailable: boolean;
begin
  try
    if sd.Module=0 then
      LibSystemdInitialize;
    result := PtrInt(sd.Module)>0;
  except
    result := false;
  end;
end;

function ProcessIsStartedBySystemd: boolean;
begin
  if not SystemdIsAvailable then
    exit(false);
  Result := fpGetenv(ENV_INVOCATION_ID) <> nil;
end;

procedure LibSystemdInitialize(const libname: TFileName);
var P: PPointer;
    api: integer;
    h: {$ifdef FPC}TLibHandle{$else}THandle{$endif FPC};
const NAMES: array[0..4] of string = (
  'sd_listen_fds', 'sd_is_socket_unix', 'sd_journal_print', 'sd_notify', 'sd_watchdog_enabled');
begin
  EnterCriticalSection(SynSockCS);
  try
    if sd.Module=0 then // try to load once
    try
      h := LoadLibrary(libname);
      P := @@sd.listen_fds;
      for api := low(NAMES) to high(NAMES) do begin
        P^ := GetProcAddress(h,PChar(NAMES[api]));
        if P^=nil then
          raise ESystemd.CreateFmt('Unable to find %s() in %s',[NAMES[api],libname]);
        inc(P);
      end;
      sd.Module := h;
    except
      on E: Exception do begin
        if h<>0 then
          FreeLibrary(h);
        PtrInt(sd.Module) := -1; // <>0 so that won't try to load any more
        raise;
      end;
    end;
  finally
    LeaveCriticalSection(SynSockCS);
  end;
end;

finalization
  if PtrInt(sd.Module) > 0 then begin
    FreeLibrary(sd.Module);
  end;
end.

