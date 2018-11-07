program FishShopDaemon;

{$APPTYPE CONSOLE}

uses
  {$I SynDprUses.inc}  // use FastMM4 on older Delphi, or set FPC threads
  SysUtils,
  SynCommons,          // framework core
  SynLog,              // logging features
  mORMot,              // RESTful server & ORM
  mORMotSQLite3,       // SQLite3 engine as ORM core
  SynSQLite3Static,    // staticaly linked SQLite3 engine
  mORMotHttpServer,    // HTTP server for RESTful server
  ServFishShopTypes,   // definitions shared by Server and Clients
  ServFishShopMain;    // the main implementation unit of our daemon

begin
  // set logging abilities
  SQLite3Log.Family.Level := LOG_VERBOSE;
  //SQLite3Log.Family.EchoToConsole := LOG_VERBOSE;
  SQLite3Log.Family.PerThreadLog := ptIdentifiedInOnFile;
  
end.

