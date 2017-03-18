program Project34RTSPproxy;

{$APPTYPE CONSOLE}

(*

  Synopse mORMot framework

  Sample 34 - RTSP over HTTP proxy
    Implements RTSP stream tunnelling over HTTP.

 Purpose of this sample is to illustrate low-level use of TAsynchConnections,
 on both Windows and Linux.

 The HTTP transport is built from two separate HTTP GET and POST requests
 initiated by the client. The server then binds the connections to form a
 virtual full-duplex connection.

 See https://goo.gl/CX6VA3 for reference material about this horrible, but
 widely accepted, Apple's hack.


  Version 1.18
  - Initial Release

  first line of uses clause below must be {$I SynDprUses.inc} to enable FastMM4

*)

uses
  {$I SynDprUses.inc}
  SysUtils,
  SynCommons,
  SynLog,
  SynCrtSock,
  SynBidirSock,
  mORMot, // just for object serialization in logs
  SynProtoRTSPHTTP;

{$R *.res}

const
  CONCURRENT = 500;
  // one socket consumes two file descriptors, so each stream opens 6 files;
  // under Linux with "ulimit -H -n" = 4096, maximum is 4096/6 = 680 streams:
  // add "* hard nofiles 65535" to /etc/security/limits.conf to make it higher

{
  Some rough numbers, on various Operating Systems:

  CONCURRENT  OS             API      Time      Sockets  Polled
  100         Windows XP     select   190 ms    300      200
  100         Windows Seven  select   190 ms    300      200
  100         Linux          poll     200 ms    300      200
  100         Linux          epoll    190 ms    300      200
  500         Windows XP     select   544 ms    1500     1000
  500         Windows Seven  select   990 ms    1500     1000
  500         Linux          poll     380 ms    1500     1000
  500         Linux          epoll    370 ms    1500     1000
  5000        Windows XP               N/A
  5000        Windows Seven  select   27.61 s   15000    10000
  5000        Windows Seven  WSAPoll  33.70 s   15000    10000
  5000        Linux          epoll    2.71 s    15000    10000
  10000       Windows XP               N/A
  10000       Windows Seven  select   116.32 s  30000    20000
  10000       Windows Seven  WSAPoll  118.23 s  30000    20000
  10000       Linux          epoll    9.42 s    30000    20000

  All process did take place with logs enabled, on the same physical PC.
  Note that the Windows Seven native system (not a VM) may be slow down by its
  AntiVirus software, whereas the XP VM did not have any AntiVirus installed.
  WSAPoll API was very disapointing: it is slightly slower than plain Select!
  In the future, we will eventually uses the IOCP API on Windows, which is told
  to be much faster (but also much more difficult to implement).
  Memory consumption was similar on all OS and API methods.
  In all cases, the Linux VM with epoll did show much better scaling abilities.
}

var
  server: TRTSPOverHTTPServer;
  timer: TPrecisionTimer;
  clients: integer;
begin
  if (paramcount = 0) or not TryStrToInt(paramstr(1), clients) then
    clients := CONCURRENT;
  TSynLog.Family.HighResolutionTimeStamp := true;
  TSynLog.Family.PerThreadLog := ptIdentifiedInOnFile;
  TSynLog.Family.Level := LOG_VERBOSE;
  TSynLog.Family.EchoToConsole := LOG_STACKTRACE + [sllCustom1];
  server := TRTSPOverHTTPServer.Create('127.0.0.1', '4999', '4998', TSynLog, nil, nil);
  try
    //server.Clients.Options := [paoWritePollOnly];
    //server.Options := [acoVerboseLog];
    writeln(server.ClassName, ' running');
    writeln('  performing tests with ', clients, ' concurrent streams using ',
      server.Clients.PollRead.PollClass.ClassName, #10);
    timer.Start;
    server.RegressionTests(nil, clients);
    writeln(#10'  tests finished in ', timer.Stop);
    {$ifdef MSWINDOWS}
    writeln('Press [Enter] to close server.');
    Readln;
    {$endif}
  finally
    server.Free;
  end;
end.
