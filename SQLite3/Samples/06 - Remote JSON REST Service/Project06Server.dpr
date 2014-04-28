program Project06Server;

{$APPTYPE CONSOLE}

uses
  SynCommons,
  mORMot,
  SysUtils;

type
  // TSQLRestServerFullMemory kind of server is light and enough for our purpose
  TServiceServer = class(TSQLRestServerFullMemory)
  published
    procedure Sum(Ctxt: TSQLRestServerURIContext);
  end;


{ TServiceServer }

procedure TServiceServer.Sum(Ctxt: TSQLRestServerURIContext);
var a,b: Extended;
begin
  if UrlDecodeNeedParameters(Ctxt.Parameters,'A,B') then begin
    while Ctxt.Parameters<>nil do begin
      UrlDecodeExtended(Ctxt.Parameters,'A=',a);
      UrlDecodeExtended(Ctxt.Parameters,'B=',b,@Ctxt.Parameters);
    end;
    Ctxt.Results([a+b]);
  end else
    Ctxt.Error('Missing Parameter');
end;

var
  aModel: TSQLModel;
begin
  aModel := TSQLModel.Create([],'service');
  try
    with TServiceServer.Create(aModel) do
    try
      if ExportServerNamedPipe('RestService') then
        writeln('Background server is running.'#10) else
        writeln('Error launching the server'#10);
      write('Press [Enter] to close the server.');
      readln;
    finally
      Free;
    end;
  finally
    aModel.Free;
  end;
end.
