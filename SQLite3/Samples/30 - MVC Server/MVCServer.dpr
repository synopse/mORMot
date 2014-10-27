/// MVC sample web application, publishing a simple BLOG
program MVCServer;

{$APPTYPE CONSOLE}


{.$define USEZEOSPOSTGRESQL}
{.$define USEFIREDACPOSTGRESQL}

// direct ZDBC/FireDAC driver needs only libpq.dll and libintl.dll e.g. from
// http://www.enterprisedb.com/products-services-training/pgbindownload

{$ifdef USEZEOSPOSTGRESQL}    {$define USESYNDB} {$endif}
{$ifdef USEFIREDACPOSTGRESQL} {$define USESYNDB} {$endif}

uses
  {$I SynDprUses.inc}    // will enable FastMM4 prior to Delphi 2006
  SynCrtSock,
  SynCommons,
  mORMot,
  SynSQLite3,
  SynSQLite3Static,
  mORMotSQLite3,
  mORMotHttpServer,
  mORMotMVC,
  {$ifdef USESYNDB}
  SynDB,
  mORMotDB,
  {$ifdef USEZEOSPOSTGRESQL}
  SynDBZeos,
  {$endif}
  {$ifdef USEFIREDACPOSTGRESQL}
  SynDBFireDAC,
  {$ifdef ISDELPHIXE5} FireDAC.Phys.PG, {$else} uADPhysPG, {$endif}
  {$endif}
  {$endif}
  MVCModel,
  MVCViewModel,
  SysUtils;

var aModel: TSQLModel;
    {$ifdef USESYNDB}
    aExternalDB: TSQLDBConnectionProperties;
    {$endif}
    aServer: TSQLRestServerDB;
    aApplication: TBlogApplication;
    aHTTPServer: TSQLHttpServer;
begin
  aModel := CreateModel;
  try
    {$ifdef USESYNDB}
    {$ifdef USEZEOSPOSTGRESQL}
    aExternalDB := TSQLDBZEOSConnectionProperties.Create(
      TSQLDBZEOSConnectionProperties.URI(dPostgreSQL,'localhost:5432'),
    {$endif}
    {$ifdef USEFIREDACPOSTGRESQL}
    aExternalDB := TSQLDBFireDACConnectionProperties.Create(
      'PG?Server=localhost;Port=5432',
    {$endif}
      'postgres','postgres','postgresPassword');
    VirtualTableExternalRegisterAll(aModel,aExternalDB);
    aServer := TSQLRestServerDB.Create(aModel,SQLITE_MEMORY_DATABASE_NAME);
    try // PostgreSQL uses one fork per connection -> better only two threads
      aServer.AcquireExecutionMode[execORMGet] := amBackgroundThread;
      aServer.AcquireExecutionMode[execORMWrite] := amBackgroundThread;
    {$else}
    aServer := TSQLRestServerDB.Create(aModel,ChangeFileExt(paramstr(0),'.db'));
    try
      aServer.DB.Synchronous := smNormal;
      aServer.DB.LockingMode := lmExclusive;
    {$endif}
      aServer.CreateMissingTables;
      aApplication := TBlogApplication.Create(aServer);
      try
        aHTTPServer := TSQLHttpServer.Create('8092',aServer,'+',useHttpApiRegisteringURI);
        try
          aHTTPServer.RootRedirectToURI('blog/default'); // redirect localhost:8092
          writeln('"MVC Blog Server" launched on port 8092 using ',aHttpServer.HttpServer.ClassName);
          writeln(#10'You can check http://localhost:8092/blog/mvc-info for information');
          writeln('or point to http://localhost:8092 to access the web app.');
          writeln(#10'Press [Enter] to close the server.'#10);
          readln;
        finally
          aHTTPServer.Free;
        end;
      finally
        aApplication.Free;
      end;
    finally
      aServer.Free;
      {$ifdef USESYNDB}
      aExternalDB.Free;
      {$endif}
    end;
  finally
    aModel.Free;
  end;
end.
