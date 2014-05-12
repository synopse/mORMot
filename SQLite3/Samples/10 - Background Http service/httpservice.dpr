/// implements a background Service serving HTTP pages
program HttpService;

uses
  {$I SynDprUses.inc}
  Windows,
  Classes,
  SysUtils,
  WinSvc,
  SynCommons,
  mORMotService,
  mORMot,
  mORMotSQLite3, SynSQLite3Static,
  mORMotHTTPServer,
  SampleData in '..\01 - In Memory ORM\SampleData.pas';

// define this conditional if you want the GDI messages to be accessible
// from the background service
{.$define USEMESSAGES}


/// if we will run the service with administrator rights
// - otherwise, ensure you registered the URI /root:8080
{$R VistaAdm.res}

type
  /// class implementing the background Service
  TSQLite3HttpService = class(TServiceSingle)
  public
    /// the associated database model
    Model: TSQLModel;
    /// the associated DB
    DB: TSQLRestServerDB;
    /// the background Server processing all requests
    Server: TSQLHttpServer;

    /// event triggered to start the service
    // - e.g. create the Server instance
    procedure DoStart(Sender: TService);
    /// event triggered to stop the service
    // - e.g. destroy the Server instance
    procedure DoStop(Sender: TService);

    /// initialize the background Service
    constructor Create; reintroduce;
    /// launch as Console application
    constructor CreateAsConsole; reintroduce;
    /// release memory
    destructor Destroy; override;
  end;


const
  HTTPSERVICENAME = 'mORMotHttpServerService';
  HTTPSERVICEDISPLAYNAME = 'mORMot Http Server Service';


{ TSQLite3HttpService }

constructor TSQLite3HttpService.Create;
begin
  inherited Create(HTTPSERVICENAME,HTTPSERVICEDISPLAYNAME);
  TSQLLog.Family.Level := LOG_VERBOSE;
  TSQLLog.Family.PerThreadLog := ptIdentifiedInOnFile;
  TSQLLog.Enter(self);
  OnStart := DoStart;
  OnStop := DoStop;
  OnResume := DoStart; // trivial Pause/Resume actions
  OnPause := DoStop;
end;

constructor TSQLite3HttpService.CreateAsConsole;
begin
  // manual switch to console mode
  AllocConsole;
  // define the log level
  with TSQLLog.Family do begin
    Level := LOG_VERBOSE;
    EchoToConsole := LOG_STACKTRACE;
  end;
end;

destructor TSQLite3HttpService.Destroy;
begin
  TSQLLog.Enter(self);
  if Server<>nil then
    DoStop(nil); // should not happen
  inherited Destroy;
end;

procedure TSQLite3HttpService.DoStart(Sender: TService);
begin
  TSQLLog.Enter(self);
  if Server<>nil then
    DoStop(nil); // should never happen
  Model := CreateSampleModel;
  DB := TSQLRestServerDB.Create(Model,ChangeFileExt(paramstr(0),'.db3'));
  DB.CreateMissingTables(0);
  Server := TSQLHttpServer.Create('8080',[DB],'+',useHttpApiRegisteringURI);
  TSQLLog.Add.Log(sllInfo,'Server % started by %',[Server.HttpServer,Server]);
end;

procedure TSQLite3HttpService.DoStop(Sender: TService);
begin
  TSQLLog.Enter(self);
  if Server=nil then
    exit;
  TSQLLog.Add.Log(sllInfo,'Server % stopped by %',[Server.HttpServer,Server]);
  FreeAndNil(Server);
  FreeAndNil(DB);
  FreeAndNil(Model);
end;

procedure CheckParameters;
begin
  if SameText(ParamStr(1),'-c') or SameText(ParamStr(1),'/c') then
    with TSQLite3HttpService.CreateAsConsole do
    try
      DoStart(nil);
      TextColor(ccLightGray);
      writeln(#10'Background server is running.'#10);
      writeln('Press [Enter] to close the server.'#10);
      ConsoleWaitForEnterKey;
      exit;
    finally
      Free;
    end;
  TSQLLog.Family.Level := LOG_VERBOSE;
  with TServiceController.CreateOpenService('','',HTTPSERVICENAME) do
  try
    CheckParameters(HTTPSERVICEDISPLAYNAME);
  finally
    Free;
  end;
  TSQLLog.Add.Log(sllTrace,'Quitting command line');
  with TServiceController.CreateOpenService('','',HTTPSERVICENAME) do
  try
    State; // just to log the service state after handling the /parameters
  finally
    Free;
  end;
end;

begin
  if ParamCount<>0 then
    CheckParameters else
    with TSQLite3HttpService.Create do
    try
      // launches the registered Services execution = do all the magic
      ServicesRun;
    finally
      Free;
    end;
end.
