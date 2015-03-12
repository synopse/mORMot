program RemoteLoggingTest;

uses
  Forms,
  RemoteLogMain in 'RemoteLogMain.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
