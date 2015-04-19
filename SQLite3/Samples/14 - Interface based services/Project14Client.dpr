program Project14Client;

uses
  {$I SynDprUses.inc} // use FastMM4 on older Delphi, or set FPC threads
  Forms,
  Project14ClientMain in 'Project14ClientMain.pas' {Form1},
  Project14Interface in 'Project14Interface.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
