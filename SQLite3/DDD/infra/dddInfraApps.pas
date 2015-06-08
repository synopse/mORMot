/// shared DDD Infrastructure: Application/Daemon implementation classes
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit dddInfraApps;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2015 Arnaud Bouchez
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

  Version 1.18
  - first public release, corresponding to Synopse mORMot Framework 1.18

  TODO:
   - store settings in database
   - allow to handle authentication via a centralized service or REST server

}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  {$ifdef MSWINDOWS}
  Windows,
  mORMotService, // for running the daemon as a regular Windows Service
  {$else}
  {$ifdef FPC}
  SynFPCLinux,
  {$endif}
  {$endif}
  SysUtils,
  Classes,
  SynCommons,
  SynLog,
  mORMot,
  mORMotDDD,
  dddInfraSettings,
  SynCrtSock,
  mORMotHttpServer, // for publishing a TSQLRestServer over HTTP
  mORMotHttpClient; // for publishing a TSQLRestClientURI over HTTP


{ ----- Implements Service/Daemon Applications }

type
  /// abstract class to handle any kind of service/daemon executable
  // - would implement either a Windows Service, a stand-alone remotely
  // administrated daemon, or a console application, according to the command line
  // - you should inherit from this class, then override the abstract NewDaemon
  // protected method to launch and return a IAdministratedDaemon instance
  TDDDDaemon = class
  protected
    fSettings: TDDDAdministratedDaemonSettingsFile;
    /// the service/daemon will be stopped when this interface is set to nil
    fDaemon: IAdministratedDaemon;
    /// this abstract method should be overriden to return a new service/daemon
    // instance, using the (inherited) fSettings as parameters
    function NewDaemon: TDDDAdministratedDaemon; virtual; abstract;
    {$ifdef MSWINDOWS} // to support Windows Services
    procedure DoStart(Sender: TService);
    procedure DoStop(Sender: TService);
    {$endif}
  public
    /// initialize the service/daemon application thanks to some information
    // - actual settings would inherit from TDDDAdministratedDaemonSettingsFile,
    // to define much more parameters, according to the service/daemon process
    // - the supplied settings will be owned by this TDDDDaemon instance
    constructor Create(aSettings: TDDDAdministratedDaemonSettingsFile); virtual;
    /// finalize the service/daemon application, and release its resources
    destructor Destroy; override;
    /// interprect the command line to run the application as expected
    // - under Windows, /install /uninstall /start /stop would control the
    // daemon as a Windows Service - you should run the program with
    // administrator rights to be able to change the Services settings
    // - /console or /c would run the program as a console application
    // - /verbose would run the program as a console application, in verbose mode
    // - /daemon or /d would run the program as a remotely administrated
    // daemon, using a published IAdministratedDaemon service
    // - /version would show the current revision information of the application
    // - no command line argument will run the program as a Service dispatcher
    // under Windows (as a regular service), or display the syntax
    // - any invalid switch, or no switch under Linux, will display the syntax
    // - this method will output any error or information to the console
    // - as a result, a project .dpr could look like the following:
    //!begin
    //!  with TMyApplicationDaemon.Create(TMyApplicationDaemonSettings.Create) do
    //!  try
    //!    ExecuteCommandLine;
    //!  finally
    //!    Free;
    //!  end;
    //!end.
    procedure ExecuteCommandLine;
  end;

  /// abstract class to implement a IAdministratedDaemon service via a TThread
  // - as hosted by TDDDDaemon service/daemon application
  TDDDThreadDaemon = class(TDDDAdministratedThreadDaemon)
  protected
    fAdministrationHTTPServer: TSQLHttpServer;
  public
    /// initialize the thread with the supplied parameters
    constructor Create(aSettings: TDDDAdministratedDaemonSettingsFile); reintroduce;
    /// finalize the service/daemon thread
    // - will call Halt() if the associated process is still running
    destructor Destroy; override;
    /// reference to the HTTP server publishing IAdministratedDaemon service
    // - may equal nil if TDDDAdministratedDaemonSettingsFile.AuthHttp.BindPort=''
    property AdministrationHTTPServer: TSQLHttpServer read fAdministrationHTTPServer;
  end;

  /// abstract class to implement a IAdministratedDaemon service via a TSQLRestServer
  // - as hosted by TDDDDaemon service/daemon application
  TDDDRestDaemon = class(TDDDAdministratedRestDaemon)
  protected
    fAdministrationHTTPServer: TSQLHttpServer;
    // returns the current state from fRest.Stat() + T
    function InternalRetrieveState(var Status: variant): boolean; override;
  public
    /// initialize the thread with the supplied parameters
    constructor Create(aSettings: TDDDAdministratedDaemonSettingsFile); reintroduce;
    /// finalize the service/daemon thread
    // - will call Halt() if the associated process is still running
    destructor Destroy; override;
    /// reference to the HTTP server publishing IAdministratedDaemon service
    // - may equal nil if TDDDAdministratedDaemonSettingsFile.AuthHttp.BindPort=''
    property AdministrationHTTPServer: TSQLHttpServer read fAdministrationHTTPServer;
  end;


{ ----- Implements Thread Processing to access a TCP server }

type
  /// the current connection state of the TCP client associated to a
  // TDDDSocketThread thread
  TDDDSocketThreadState = (tpsDisconnected, tpsConnecting, tpsConnected);
  
  /// the monitoring information of a TDDDSocketThread thread
  TDDDSocketThreadMonitoring = class(TDDDAdministratedDaemonMonitor)
  private
    FState: TDDDSocketThreadState;
  published
    /// how this thread is currently connected to its associated TCP server
    property State: TDDDSocketThreadState read FState write FState;
  end;

  /// a generic TThread able to connect and reconnect to a TCP server
  // - initialize and own a TCrtSocket instance for TCP transmission
  // - allow automatic reconnection
  // - inherit from TSQLRestThread, so should be associated with a REST instance
  TDDDSocketThread = class(TSQLRestThread)
  protected
    fSettings: TDDDSocketThreadSettings;
    fMonitoring: TDDDSocketThreadMonitoring;
    fSocket: TCrtSocket;
    fPerformConnection: boolean;
    fHost, fPort: SockString;
    fSocketInputBuffer: RawByteString;
    fExecuteSocketLoopPeriod: integer;
    fShouldDisconnect: boolean;
    procedure InternalExecute; override;
    procedure ExecuteConnect;
    procedure ExecuteDisconnect;
    procedure ExecuteDisconnectAfterError;
    procedure ExecuteSocket;
    function TrySend(const aFrame: RawByteString;
      ImmediateDisconnectAfterError: boolean=true): Boolean;
    // inherited classes could override those methods for process customization
    procedure InternalExecuteConnected; virtual;
    procedure InternalExecuteDisconnect; virtual;
    procedure InternalExecuteIdle; virtual;
    procedure InternalExecuteSocket; virtual; abstract; // process FSocketInputBuffer
  public
    /// initialize the thread for a given REST instance
    constructor Create(aSettings: TDDDSocketThreadSettings; aRest: TSQLRest;
      aMonitoring: TDDDSocketThreadMonitoring;
      const aDefaultHost,aDefaultPort: SockString);
    /// finalize the thread process, and its associted REST instance
    destructor Destroy; override;
    /// returns the Monitoring and Rest statistics as a JSON object
    // - resulting format is
    // $ {...MonitoringProperties...,"Rest":{...RestStats...}}
    function StatsAsJson: RawUTF8;
    /// the parameters used to setup this thread process
    property Settings: TDDDSocketThreadSettings read fSettings;
  published
    /// the IP Host name used to connect with TCP
    property Host: SockString read fHost;
    /// the IP Port value used to connect with TCP
    property Port: SockString read fPort;
  end;



implementation

{ TDDDDaemon }

constructor TDDDDaemon.Create(aSettings: TDDDAdministratedDaemonSettingsFile);
begin
  inherited Create;
  if aSettings=nil then
    raise EDDDInfraException.CreateUTF8('%.Create(settings=nil)',[self]);
  fSettings := aSettings;
end;

destructor TDDDDaemon.Destroy;
begin
  inherited;
  fSettings.Free;
end;

{$ifdef MSWINDOWS} // to support Windows Services

procedure TDDDDaemon.DoStart(Sender: TService);
begin
  fDaemon := NewDaemon;
  SQLite3Log.Enter(self);
  fDaemon.Start;
end;

procedure TDDDDaemon.DoStop(Sender: TService);
begin
  SQLite3Log.Enter(self);
  fDaemon := nil; // will stop the daemon
end;

{$endif MSWINDOWS} // to support Windows Services

type
  TExecuteCommandLineCmd = (
    cNone,cInstall,cUninstall,cStart,cStop,cState,cVersion,cVerbose,
    cHelp,cConsole,cDaemon);

procedure TDDDDaemon.ExecuteCommandLine;
var name,param: RawUTF8;
    cmd: TExecuteCommandLineCmd;
    daemon: TDDDAdministratedDaemon;
    {$ifdef MSWINDOWS}
    service: TServiceSingle;
    ctrl: TServiceController;
    {$endif}
{$I-} // no IO error for writeln() below
function cmdText: RawUTF8;
begin
  result := GetEnumNameTrimed(TypeInfo(TExecuteCommandLineCmd),cmd);
end;
procedure Show(Success: Boolean);
var msg: RawUTF8;
begin
  if Success then begin
    msg := 'Run';
    TextColor(ccWhite);
  end else begin
    msg := FormatUTF8('Error % "%" occured with',
      [GetLastError,SysErrorMessage(GetLastError)]);
    TextColor(ccLightRed);
  end;
  msg := FormatUTF8('% "%" (%) on Service "%"',
    [msg,param,cmdText,fSettings.ServiceName]);
  AppendToTextFile(msg,ChangeFileExt(ExeVersion.ProgramFileName,'.txt'));
  writeln(msg);
end;
procedure Syntax;
begin
  writeln('Try with one of the switches:');
  writeln(ExeVersion.ProgramName,' /console -c /verbose /daemon -d /help -h /version');
  {$ifdef MSWINDOWS}
  writeln(ExeVersion.ProgramName,' /install /uninstall /start /stop /state');
  {$endif}
end;
begin
  try
    if fSettings.ServiceDisplayName='' then begin
      fDaemon := NewDaemon; // should initialize the default .settings
      fDaemon := nil;
    end;
    TextColor(ccLightGreen);
    name := StringToUTF8(fSettings.ServiceDisplayName);
    if name='' then // perhaps the settings file is still void
      name := ExeVersion.ProgramName;
    writeln(#10' ',name);
    writeln(StringOfChar('-',length(name)+2));
    if fSettings.Description<>'' then begin
      TextColor(ccGreen);
      writeln(fSettings.Description);
    end;
    writeln;
    TextColor(ccLightCyan);
    param := trim(StringToUTF8(paramstr(1)));
    if (param='') or not(param[1] in ['/','-']) then
      cmd := cNone else
      case param[2] of
      'c','C': cmd := cConsole;
      'd','D': cmd := cDaemon;
      'h','H': cmd := cHelp;
      else byte(cmd) := 1+IdemPCharArray(@param[2],[
        'INST','UNINST','START','STOP','STAT','VERS','VERB']);
      end;
    case cmd of
    cHelp:
      Syntax;
    cVersion: begin
      if ExeVersion.Version.Version32<>0 then
        writeln(ExeVersion.ProgramName,' Version ',ExeVersion.Version.Detailed);
      TextColor(ccCyan);
      writeln('Powered by Synopse mORMot '+SYNOPSE_FRAMEWORK_VERSION);
    end;
    cConsole,cDaemon,cVerbose: begin
      writeln('Launched in ',cmdText,' mode'#10);
      TextColor(ccLightGray);
      case cmd of
      cConsole:
        SQLite3Log.Family.EchoToConsole := LOG_STACKTRACE+[sllDDDInfo];
      cVerbose:
        SQLite3Log.Family.EchoToConsole := LOG_VERBOSE;
      end;
      daemon := NewDaemon;
      try
        fDaemon := daemon;
        if cmd=cDaemon then
          if (daemon.AdministrationServer=nil) or
             not ({$ifdef MSWINDOWS}
                   daemon.AdministrationServer.ExportedAsMessageOrNamedPipe or{$endif}
                  (daemon.InheritsFrom(TDDDThreadDaemon) and
                   (TDDDThreadDaemon(daemon).fAdministrationHTTPServer<>nil))) then
            daemon.LogClass.Add.Log(sllWarning,'ExecuteCommandLine as Daemon '+
              'without external admnistrator acccess',self);
        daemon.Execute(cmd=cDaemon);
      finally
        fDaemon := nil; // will stop the daemon
      end;
    end;
    else
    {$ifdef MSWINDOWS} // implement the daemon as a Windows Service
      with fSettings do
      if ServiceName='' then
        if cmd=cNone then
          Syntax else begin
          TextColor(ccLightRed);
          writeln('No ServiceName specified - please fix the code');
        end else
      case cmd of
      cNone:
        if param='' then begin // executed as a background service
          service := TServiceSingle.Create(ServiceName,ServiceDisplayName);
          try
            service.OnStart := DoStart;
            service.OnStop := DoStop;
            service.OnShutdown := DoStop; // sometimes, is called without Stop
            if ServicesRun then // blocking until service shutdown
              Show(true) else
              if GetLastError=1063 then
                Syntax else
                Show(false);
          finally
            service.Free;
          end;
        end else
          Syntax;
      cInstall:
        Show(TServiceController.Install(ServiceName,ServiceDisplayName,
          Description,ServiceAutoStart)<>ssNotInstalled);
      else begin
        ctrl := TServiceController.CreateOpenService('','',ServiceName);
        try
          case cmd of
          cStart:
            Show(ctrl.Start([]));
          cStop:
            Show(ctrl.Stop);
          cUninstall: begin
            ctrl.Stop;
            Show(ctrl.Delete);
          end;
          cState:
            writeln(ServiceName,' State=',ServiceStateText(ctrl.State));
          else Show(false);
          end;
        finally
          ctrl.Free;
        end;
      end;
      end;
    {$else}
      Syntax;
    {$endif MSWINDOWS}
    end;       
  except
    on E: Exception do
      ConsoleShowFatalException(E);
  end;
  TextColor(ccLightGray);
  ioresult;
end;
{$I+}


{ TDDDThreadDaemon }

constructor TDDDThreadDaemon.Create(
  aSettings: TDDDAdministratedDaemonSettingsFile);
begin
  if aSettings=nil then
    raise EDDDInfraException.CreateUTF8('%.Create(settings=nil)',[self]);
  with aSettings.RemoteAdmin do
    inherited Create(AuthUserName,AuthHashedPassword,AuthRootURI,AuthNamedPipeName);
  fLogClass.Add.Log(sllTrace,'Create(%)',[aSettings],self);
  with aSettings.RemoteAdmin do
    if AuthHttp.BindPort<>'' then
      fAdministrationHTTPServer := TSQLHttpServer.Create(fAdministrationServer,AuthHttp);
end;

destructor TDDDThreadDaemon.Destroy;
begin
  FreeAndNil(fAdministrationHTTPServer);
  inherited;
end;


{ TDDDRestDaemon }

constructor TDDDRestDaemon.Create(
  aSettings: TDDDAdministratedDaemonSettingsFile);
begin
  if aSettings=nil then
    raise EDDDInfraException.CreateUTF8('%.Create(settings=nil)',[self]);
  with aSettings.RemoteAdmin do
    inherited Create(AuthUserName,AuthHashedPassword,AuthRootURI,AuthNamedPipeName);
  fLogClass.Add.Log(sllTrace,'Create(%)',[aSettings],self);
  with aSettings.RemoteAdmin do
    if AuthHttp.BindPort<>'' then
      fAdministrationHTTPServer := TSQLHttpServer.Create(fAdministrationServer,AuthHttp);
end;

destructor TDDDRestDaemon.Destroy;
begin
  FreeAndNil(fAdministrationHTTPServer);
  inherited Destroy;
end;

function TDDDRestDaemon.InternalRetrieveState(
  var Status: variant): boolean;
var mem: TSynMonitorMemory;
begin                                     
  if fRest<>nil then begin
    mem := TSynMonitorMemory.Create;
    try
      Status := _ObjFast([
        'Rest',fRest.FullStatsAsDocVariant,'SystemMemory',ObjectToVariant(mem)]);
    finally
      mem.Free;
    end;
    result := true;
  end else
    result := false;
end;


{ TDDDSocketThread }

constructor TDDDSocketThread.Create(
  aSettings: TDDDSocketThreadSettings; aRest: TSQLRest;
  aMonitoring: TDDDSocketThreadMonitoring; const aDefaultHost, aDefaultPort: SockString);
begin
  if aSettings=nil then
    raise EDDDInfraException.CreateUTF8('%.Create(Settings=nil)',[self]);
  fSettings := aSettings;
  if aMonitoring=nil then
    raise EDDDInfraException.CreateUTF8('%.Create(aMonitoring=nil)',[self]);
  fMonitoring := aMonitoring;
  if fSettings.Host='' then
    if aDefaultHost='' then
      fHost := '127.0.0.1' else
      fHost := aDefaultHost else
    fHost := fSettings.Host;
  if fSettings.Port=0 then
    fPort := aDefaultPort else
    fPort := UInt32ToUtf8(fSettings.Port);
  fExecuteSocketLoopPeriod := 300;
  if fSettings.SocketTimeout<fExecuteSocketLoopPeriod then
    fSettings.SocketTimeout := 2000;
  fPerformConnection := true;
  inherited Create(aRest,true); // aOwnRest=true
end;

destructor TDDDSocketThread.Destroy;
var timeOut: Int64;
begin
  FLog.Enter(self);
  Terminate;
  timeOut := GetTickCount64+10000;
  repeat // wait until properly disconnected from remote TCP server
    Sleep(10);
  until (FMonitoring.State=tpsDisconnected) or (GetTickCount64>timeOut);
  inherited Destroy;
  FreeAndNil(fMonitoring);
end;

procedure TDDDSocketThread.ExecuteConnect;
var tix: Int64;
begin
  FLog.Enter(self);
  if fSocket<>nil then
    raise EDDDInfraException.CreateUTF8('%.ExecuteConnect: FSocket<>nil',[self]);
  if FMonitoring.State<>tpsDisconnected then
    raise EDDDInfraException.CreateUTF8('%.ExecuteConnect: State=%',[self,ord(FMonitoring.State)]);
  fMonitoring.State := tpsConnecting;
  FLog.Log(sllTrace,'ExecuteConnect: Connecting to %:%',[Host,Port],self);
  try
    FSocket := TCrtSocket.Open(Host,Port,cslTCP,fSettings.SocketTimeout);
    FSocket.CreateSockIn(tlbsCRLF,65536); // use SockIn safe buffer
    InternalExecuteConnected;
    FMonitoring.State := tpsConnected;
    FLog.Log(sllTrace,'ExecuteConnect: Connected via Socket % - %',
      [FSocket.Sock,FMonitoring],self);
  except
    on E: Exception do begin
      FLog.Log(sllTrace,'ExecuteConnect: Impossible to Connect to %:% (%) %',
        [Host,Port,E.ClassType,FMonitoring],self);
      FreeAndNil(FSocket);
      FMonitoring.State := tpsDisconnected;
    end;
  end;
  if (FMonitoring.State<>tpsConnected) and not Terminated then
    if fSettings.ConnectionAttemptsInterval>0 then begin // on error, retry
      tix := GetTickCount64+fSettings.ConnectionAttemptsInterval*1000;
      repeat
        sleep(50);
      until Terminated or (GetTickCount64>tix);
      if Terminated then
        FLog.Log(sllTrace,'ExecuteConnect: thread terminated',self) else
        FLog.Log(sllTrace,'ExecuteConnect: wait finished -> retry connect',self);
    end;
end;

procedure TDDDSocketThread.ExecuteDisconnect;
begin
  FLog.Enter(self);
  try
    fLock.Acquire;
    try
      fShouldDisconnect := false;
      FMonitoring.State := tpsDisconnected;
      try
        InternalExecuteDisconnect;
      finally
        FreeAndNil(FSocket);
      end;
      FLog.Log(sllTrace,'Socket disconnected %',[fMonitoring],self);
    finally
      fLock.Release;
    end;
  except
    on E: Exception do
      FLog.Log(sllTrace,'Socket disconnection error (%)',[E.ClassType],self);
  end;
end;

procedure TDDDSocketThread.ExecuteDisconnectAfterError;
begin
  FLog.Log(sllError,'%.ExecuteDisconnectAfterError: Sock=% LastLowSocketError=%',
    [ClassType,FSocket.Sock,FSocket.LastLowSocketError],self);
  ExecuteDisconnect;
  FSocketInputBuffer := '';
  if fSettings.AutoReconnectAfterSocketError then
    FPerformConnection := true;
end;

procedure TDDDSocketThread.ExecuteSocket;
var pending, len: integer;
begin
  pending := FSocket.SockInPending(fExecuteSocketLoopPeriod);
  if Terminated or (pending=0) then
    exit;
  if pending<0 then begin
    ExecuteDisconnectAfterError;
    exit;
  end;
  len := length(FSocketInputBuffer);
  SetLength(FSocketInputBuffer,len+pending);
  if FSocket.SockInRead(@PByteArray(FSocketInputBuffer)[len],pending)<>pending then begin
    ExecuteDisconnectAfterError;
    exit;
  end;
  FMonitoring.AddSize(pending,0);
  InternalExecuteSocket;
end;

procedure TDDDSocketThread.InternalExecute;
var PreviousMonitorTix: Int64;
begin
  PreviousMonitorTix := GetTickCount64;
  try
    repeat
      if fMonitoring.State=tpsConnected then
        ExecuteSocket else
        if fPerformConnection then
          ExecuteConnect else
          sleep(200);
      if Terminated then
        break;
      try
        if Elapsed(PreviousMonitorTix,fSettings.MonitoringLogInterval) then
          FLog.Log(sllMonitoring,'%',[FMonitoring],Self);
        InternalExecuteIdle;
      except
        on E: Exception do
          FLog.Log(sllWarning,'Skipped % exception in %.InternalExecuteIdle',[E,ClassType],self);
      end;
    until Terminated;
  finally
    ExecuteDisconnect;
  end;
end;

procedure TDDDSocketThread.InternalExecuteConnected;
begin
end;

procedure TDDDSocketThread.InternalExecuteDisconnect;
begin
end;

procedure TDDDSocketThread.InternalExecuteIdle;
begin
  fLock.Acquire;
  try
    if fShouldDisconnect then
      ExecuteDisconnectAfterError;
  finally
    fLock.Release;
  end;
end;

function TDDDSocketThread.StatsAsJson: RawUTF8;
begin
  with TJSONSerializer.CreateOwnedStream do
  try
    WriteObject(FMonitoring);
    if FRest.InheritsFrom(TSQLRestServer) then begin
      CancelLastChar('}');
      AddShort(',"Rest":');
      AddNoJSONEscapeUTF8(TSQLRestServer(FRest).FullStatsAsJson);
      Add('}');
    end;
    SetText(result);
  finally
    Free;
  end;
end;

function TDDDSocketThread.TrySend(
  const aFrame: RawByteString; ImmediateDisconnectAfterError: boolean): Boolean;
begin
  fLock.Acquire;
  result := (fSocket<>nil) and (fMonitoring.State=tpsConnected) and
            not fShouldDisconnect;
  fLock.Release;
  if not result then
    exit;
  // here a GPF may occur if FSocket=nil after fLock.Release (very unlikely)
  result := FSocket.TrySndLow(pointer(aFrame),length(aFrame));
  if result then
    FMonitoring.AddSize(0,length(aFrame)) else
    if ImmediateDisconnectAfterError then
      ExecuteDisconnectAfterError else begin
      fLock.Acquire;
      fShouldDisconnect := true; // notify for InternalExecuteIdle
      fLock.Release;
    end;
end;


initialization
  {$ifdef EnableMemoryLeakReporting}
  {$ifdef HASFASTMM4} // FastMM4 integrated in Delphi 2006 (and up)
  ReportMemoryLeaksOnShutdown := True;
  {$endif}
  {$endif}
end.
