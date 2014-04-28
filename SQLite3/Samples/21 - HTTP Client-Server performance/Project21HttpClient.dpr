/// this client will stress a remote TSQLRestServerDB over HTTP
program Project21HttpClient;

// first line of uses clause must be   {$I SynDprUses.inc}
uses
  {$I SynDprUses.inc}
  Forms,
  Project21HttpClientMain in 'Project21HttpClientMain.pas' {MainForm};

{$R *.res}

{$R Vista.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
