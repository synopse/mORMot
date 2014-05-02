/// test access to a local MongoDB instance
program MongoDBTests;

{$APPTYPE CONSOLE}

uses
  {$I SynDprUses.inc}
  MongoDBTestCases;

begin
  with TTestMongoDB.Create do
  try
    Run;
    readln;
  finally
    Free;
  end;
end.
