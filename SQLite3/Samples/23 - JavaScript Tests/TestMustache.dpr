/// test Mustache libraries: native SynMustache vs JavaScript/SpiderMonkey
program TestMustache;

{
  -------------------------------------------------------------------------
   Download the SpiderMonkey library at http://synopse.info/files/synsm.7z
   and put mozjs-24.dll and libnspr4.dll files with your TestMustache.exe
  -------------------------------------------------------------------------
}

// first line of uses clause must be   {$I SynDprUses.inc}
uses
  {$I SynDprUses.inc}
  Forms,
  TestMustacheUnit in 'TestMustacheUnit.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
