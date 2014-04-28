program Project16Client;

//   first line of uses clause must be   {$I SynDprUses.inc}
uses
  {$I SynDprUses.inc}
  Forms,
  Project16ClientMain in 'Project16ClientMain.pas' {MainForm},
  Project16Interface in 'Project16Interface.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
