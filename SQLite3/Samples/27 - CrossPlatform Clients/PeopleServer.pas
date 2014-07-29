unit PeopleServer;

interface

{.$define TESTRECORD}

uses
  SynCommons,
  mORMot,
  mORMotHttpServer,
  mORMotWrappers,
  SynMustache,
  SysUtils;

type
  {$ifdef TESTRECORD}
  TTestCustomJSONArraySimpleArray = packed record
    F: RawUTF8;
    G: array of RawUTF8;
  end;
  {$endif TESTRECORD}

  TSQLRecordPeople = class(TSQLRecord)
  protected
    fData: TSQLRawBlob;
    fFirstName: RawUTF8;
    fLastName: RawUTF8;
    fYearOfBirth: integer;
    fYearOfDeath: word;
    {$ifdef TESTRECORD}
    fSimple: TTestCustomJSONArraySimpleArray;
    class procedure InternalRegisterCustomProperties(Props: TSQLRecordProperties); override;
    {$endif}
  published
    property FirstName: RawUTF8 read fFirstName write fFirstName;
    property LastName: RawUTF8 read fLastName write fLastName;
    property Data: TSQLRawBlob read fData write fData;
    property YearOfBirth: integer read fYearOfBirth write fYearOfBirth;
    property YearOfDeath: word read fYearOfDeath write fYearOfDeath;
  {$ifdef TESTRECORD}
  public
    property Simple: TTestCustomJSONArraySimpleArray read fSimple;
  {$endif}
  end;

  TCustomServer = class(TSQLRestServerFullMemory)
  published
    procedure DropTable(Ctxt: TSQLRestServerURIContext);
    procedure Wrapper(Ctxt: TSQLRestServerURIContext);
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

procedure TCustomServer.Wrapper(Ctxt: TSQLRestServerURIContext);
begin // search in current and parent (may be compiled in an 'exe' sub-folder)
  WrapperMethod(Ctxt,['..\..\..\CrossPlatform\templates',
                      '..\..\..\..\CrossPlatform\templates']);
end;


var Model: TSQLModel;
    DB: TCustomServer;
    Server: TSQLHttpServer;

procedure StartServer(auth: TPeopleServerAuthentication);
begin
  StopServer;
  //TSQLLog.Family.Level := LOG_VERBOSE;
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
  DB.ServiceMethodByPassAuthentication('wrapper'); // for our testing purpose
end;

procedure StopServer;
begin
  FreeAndNil(Server);
  FreeAndNil(DB);
  FreeAndNil(Model);
end;

{$ifdef TESTRECORD}

{ TSQLRecordPeople }

const
  __TTestCustomJSONArraySimpleArray =
  'F RawUTF8 G array of RawUTF8';

class procedure TSQLRecordPeople.InternalRegisterCustomProperties(
  Props: TSQLRecordProperties);
begin
  Props.RegisterCustomPropertyFromRTTI(Self,TypeInfo(TTestCustomJSONArraySimpleArray),
    'Simple',@TSQLRecordPeople(nil).fSimple);
end;

initialization
  TTextWriter.RegisterCustomJSONSerializerFromText(
    TypeInfo(TTestCustomJSONArraySimpleArray),__TTestCustomJSONArraySimpleArray);
    
{$endif TESTRECORD}

end.
