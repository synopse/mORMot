program RegressionTests;

{$APPTYPE CONSOLE}

uses
  //FastMM4,
  SynCrossPlatformJSON,
  SynCrossPlatformREST,
  SynCrossPlatformTests,
  SysUtils;

begin
  with TSynCrossPlatformTests.Create('Cross Platform Client for mORMot') do
  try
    Run(true);
  finally
    Free;
  end;
  write(' Press [Enter] to quit');
  readln;
end.
 
