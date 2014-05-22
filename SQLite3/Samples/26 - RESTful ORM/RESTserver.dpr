/// RESTful ORM server
program RESTserver;

// first line after uses clause should be  {$I SynDprUses.inc}  for FastMM4
uses
  {$I SynDprUses.inc}
  Windows,
  Classes,
  SysUtils,
  WinSvc,
  SynCommons,
  mORMot,
  SynCrtSock,
  mORMotHTTPServer,
  RESTData,
  RESTServerClass in 'RESTServerClass.pas';

var ORMServer: TNoteServer;
    HTTPServer: TSQLHttpServer;
begin
  ORMServer := TNoteServer.Create(ExtractFilePath(paramstr(0))+'data','root');
  try
    HTTPServer := TSQLHttpServer.Create(HTTP_PORT,[ORMServer]);
    try
      AllocConsole;
      TSQLLog.Family.EchoToConsole := LOG_VERBOSE;
      writeln(#13#10'Background server is running at http://localhost:888'#13#10+
              #13#10'Press [Enter] to close the server.');
      ConsoleWaitForEnterKey;
    finally
      HTTPServer.Free;
    end;
  finally
    ORMServer.Free;
  end;
end.
