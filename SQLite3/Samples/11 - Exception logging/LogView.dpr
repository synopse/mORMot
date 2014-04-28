/// sample program able to visualize .log files as created by TSynLog
program LogView;

{ Revision History:

  Version 1.18
  - Introducing thread identification
  - Added "Search Previous" button
  - Incremental search will now remain on the same line if it matches the entry 
  
}
// first line of uses clause must be   {$I SynDprUses.inc}
uses
  {$I SynDprUses.inc}
  Forms,
  LogViewMain in 'LogViewMain.pas' {MainLogView};

{$R *.res}
{$R Vista.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainLogView, MainLogView);
  Application.Run;
end.
