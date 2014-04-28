program Project20Client;

//   first line of uses clause must be   {$I SynDprUses.inc}
uses
  {$I SynDprUses.inc}
  Forms,
  Project20ClientMain in 'Project20ClientMain.pas' {Form1},
  Project20Interface in 'Project20Interface.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
