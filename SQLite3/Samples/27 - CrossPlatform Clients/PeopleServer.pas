unit PeopleServer;

interface

uses
  SynCommons,
  mORMot,
  mORMotHttpServer,
  mORMotWrappers,
  SysUtils;

type
  TSQLRecordPeople = class(TSQLRecord)
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
    procedure DropTable(Ctxt: TSQLRestServerURIContext);
  end;

  TPeopleServerAuthentication = (psaNone,psaWeak,psaDefault);


procedure StartServer(auth: TPeopleServerAuthentication);

procedure StopServer;


implementation


procedure TCustomServer.DropTable(Ctxt: TSQLRestServerURIContext);
begin
  if (Ctxt.Method=mGET) and (Ctxt.TableIndex>=0) then begin
    TSQLRestStorageInMemory(fStaticData[Ctxt.TableIndex]).DropValues;
    Ctxt.Success;
  end;
end;

var Model: TSQLModel;
    DB: TCustomServer;
    Server: TSQLHttpServer;

procedure StartServer(auth: TPeopleServerAuthentication);
begin
  StopServer;
  Model := TSQLModel.Create([TSQLAuthUser,TSQLAuthGroup,TSQLRecordPeople]);
  DB := TCustomServer.Create(Model);
  Server := TSQLHttpServer.Create('888',DB);
  Server.AccessControlAllowOrigin := '*';
  case auth of
  psaDefault:
    DB.AuthenticationRegister(TSQLRestServerAuthenticationDefault);
  psaWeak:
    DB.AuthenticationRegister(TSQLRestServerAuthenticationNone);
  end;
  if DB.TableRowCount(TSQLRecordPeople)=0 then
    // we expect at least one record
    DB.Add(TSQLRecordPeople,['First1','Last1',1801,1826]);
    // in all cases, client will call DropTable method-based service
end;

procedure StopServer;
begin
  FreeAndNil(Server);
  FreeAndNil(DB);
  FreeAndNil(Model);
end;

end.
