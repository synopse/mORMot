program RegressionTests;

{$APPTYPE CONSOLE}

{$ifdef MSWINDOWS}
{$ifndef FPC}
  {$define RUNSERVER}
{$endif}
{$endif}

uses
  {$ifndef FPC}
  {$ifndef HASINLINE}
  FastMM4,
  {$endif}
  {$endif}
  SynCrossPlatformJSON,
  SynCrossPlatformREST,
  SynCrossPlatformCrypto,
  SynCrossPlatformTests,
  {$ifdef RUNSERVER}
  SynCommons,
  mORMot,
  mORMotHttpServer,
  {$endif}
  SysUtils;

// for testing FPC, run locally RegressionTests.exe compiled with Delphi on Windows :)

{$ifdef RUNSERVER}
type
  TSQLRecordPeople = class(mORMot.TSQLRecord)
  private
    fData: TSQLRawBlob;
    fFirstName: RawUTF8;
    fLastName: RawUTF8;
    fYearOfBirth: integer;
    fYearOfDeath: word;
  published
    property FirstName: RawUTF8 read fFirstName write fFirstName;
    property LastName: RawUTF8 read fLastName write fLastName;
    property Data: TSQLRawBlob read fData write fData;
    property YearOfBirth: integer read fYearOfBirth write fYearOfBirth;
    property YearOfDeath: word read fYearOfDeath write fYearOfDeath;
  end;

  TCustomServer = class(TSQLRestServerFullMemory)
  published
    procedure DropWholeDatabase(Ctxt: TSQLRestServerURIContext);
  end;

procedure TCustomServer.DropWholeDatabase(Ctxt: TSQLRestServerURIContext);
begin
  if Ctxt.Method=mGET then begin
    DropDatabase;
    Ctxt.Success;
  end;
end;
{$endif}

procedure TestWithAuth(aAuth: TSQLRestAuthenticationClass; waitEnterKey: boolean);
{$ifdef RUNSERVER}
var Model: TSQLModel;
    DB: TCustomServer;
    Server: TSQLHttpServer;
{$endif}
begin
  with TSynCrossPlatformClient.Create(aAuth) do
  try
    Ident := 'Cross Platform Client for mORMot';
    if aAuth=nil then
      Ident := Ident+' without authentication' else
      Ident := Ident+' using '+string(aAuth.ClassName);
{$ifdef RUNSERVER}
    Model := TSQLModel.Create([mORMot.TSQLAuthUser,mORMot.TSQLAuthGroup,
      TSQLRecordPeople]);
    DB := TCustomServer.Create(Model);
    Server := TSQLHttpServer.Create('888',DB);
    try
{$endif}
      Run(true);
      if waitEnterKey then begin
        write(' Press [Enter] to quit');
        readln;
      end;
{$ifdef RUNSERVER}
    finally
      Server.Free;
      DB.Free;
      Model.Free;
    end;
{$endif}
  finally
    Free;
  end;
end;

begin
  with TSynCrossPlatformTests.Create('Cross Platform Units for mORMot') do
  try
    Run(true);
  finally
    Free;
  end;
  writeln;
  TestWithAuth(nil,true);
end.

