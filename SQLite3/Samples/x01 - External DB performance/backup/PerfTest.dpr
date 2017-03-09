program PerfTest;

//   first line of uses clause must be   {$I SynDprUses.inc}
{$ifdef Linux}
  {$ifdef FPC_CROSSCOMPILING}
    {$linklib libc_nonshared.a}
  {$endif}
{$endif}

uses
  {$I SynDprUses.inc}
  // SynFastWideString, // still works with fast WideString, and slightly faster
//  Forms,
  {$ifdef FPC}
//  Interfaces,
  {$endif}
  PerfMain in 'PerfMain.pas' {MainForm};

begin
//    RequireDerivedFormResource:=True;
    PerfConsoleTest;
//  Application.Initialize;
//  Application.CreateForm(TMainForm, MainForm);
//  Application.Run;
end.
