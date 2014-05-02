program SourceCodeRep;

uses
  Forms,
  SourceCodeRepMain in 'SourceCodeRepMain.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
