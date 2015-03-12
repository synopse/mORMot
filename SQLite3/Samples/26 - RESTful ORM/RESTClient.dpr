program RESTClient;

uses
  Forms,
  RestClientMain in 'RestClientMain.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
