/// manage (install/start/stop/uninstall) HttpService sample
program HttpServiceSetup;

uses
  {$I SynDprUses.inc}
  Windows,
  Classes,
  SysUtils,
  WinSvc,
  SynCommons,
  SynLog,
  mORMot,
  mORMotService;

/// if we will run the service with administrator rights
// - otherwise, ensure you registered the URI /root:8080
{$R VistaAdm.res}

const
  HTTPSERVICENAME = 'mORMotHttpServerService';
  HTTPSERVICEDISPLAYNAME = 'mORMot Http Server Service';

begin
  TSQLLog.Family.Level := LOG_VERBOSE;
  with TServiceController.CreateOpenService('','',HTTPSERVICENAME) do
  try
    CheckParameters(ExtractFilePath(paramstr(0))+'HttpService.exe',
      HTTPSERVICEDISPLAYNAME);
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
end.
