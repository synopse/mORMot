/// test access to a local MongoDB instance
program MongoDBTests;

{$APPTYPE CONSOLE}

uses
  {$I SynDprUses.inc}
  SynCommons,
  mORMot,
  MongoDBTestCases;

begin
  //SQLite3Log.Family.Level := LOG_VERBOSE;
  TSynLogTestLog := SQLite3Log;
  with TTestMongoDB.Create do
  try
    Run;
    readln;
  finally
    Free;
  end;
end.
