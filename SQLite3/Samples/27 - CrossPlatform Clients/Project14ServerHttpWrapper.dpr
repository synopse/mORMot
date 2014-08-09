/// this server will demonstrate how to publish code generation wrappers 
program Project14ServerHttpWrapper;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  SynCommons,
  mORMot,
  mORMotHttpServer,
  mORMotWrappers,
  Project14Interface in '..\14 - Interface based services\Project14Interface.pas';

type
  TServiceCalculator = class(TInterfacedObject, ICalculator)
  public
    function Add(n1,n2: integer): integer;
  end;

function TServiceCalculator.Add(n1, n2: integer): integer;
begin
  result := n1+n2;
end;

{ TMyServer }

type
  TMyServer = class(TSQLRestServerFullMemory)
  published
    procedure Wrapper(Ctxt: TSQLRestServerURIContext);
  end;

procedure TMyServer.Wrapper(Ctxt: TSQLRestServerURIContext);
begin // search in current and parent (may be compiled in an 'exe' sub-folder)
  WrapperMethod(Ctxt,['..\..\..\CrossPlatform\templates',
                      '..\..\..\..\CrossPlatform\templates']);
end;

var
  aModel: TSQLModel;
  aServer: TSQLRestServer;
  aHTTPServer: TSQLHttpServer;
begin
  // define the log level
  with TSQLLog.Family do begin
    Level := LOG_VERBOSE;
    //EchoToConsole := LOG_VERBOSE; // log all events to the console
  end;
  // create a Data Model
  aModel := TSQLModel.Create([],ROOT_NAME);
  try
    // initialize a TObjectList-based database engine
    aServer := TMyServer.Create(aModel,'test.json',false,true);
    try
      aServer.ServiceMethodByPassAuthentication('wrapper');
      // register our ICalculator service on the server side
      aServer.ServiceRegister(TServiceCalculator,[TypeInfo(ICalculator)],sicShared);
      // launch the HTTP server
      aHTTPServer := TSQLHttpServer.Create(PORT_NAME,[aServer],'+',useHttpApiRegisteringURI);
      try
        aHTTPServer.AccessControlAllowOrigin := '*'; // for AJAX requests to work
        writeln(#10'Background server is running.');
        writeln('You can test http://localhost:',PORT_NAME,'/wrapper');
        writeln(#10'Press [Enter] to close the server.'#10);
        readln;
      finally
        aHTTPServer.Free;
      end;
    finally
      aServer.Free;
    end;
  finally
    aModel.Free;
  end;
end.
