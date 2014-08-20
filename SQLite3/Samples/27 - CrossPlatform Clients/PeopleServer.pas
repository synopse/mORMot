unit PeopleServer;

interface

{$define TESTRECORD}

uses
  SynCommons,
  mORMot, 
  mORMotHttpServer,
  mORMotWrappers,
  SynMustache,
  SysUtils;

type
  {$ifdef TESTRECORD}
  TRecordEnum = (reOne, reTwo, reLast);

  TTestCustomJSONArraySimpleArray = packed record
    F: RawUTF8;
    G: array of RawUTF8;
    H: record
      H1: integer;
      H2: WideString;
      H3: record
        H3a: boolean;
        H3b: RawByteString;
      end;
    end;
    I: TDateTime;
    J: array of packed record
      J1: byte;
      J2: TGUID;
      J3: TRecordEnum;
    end;
  end;
  {$endif TESTRECORD}

  TPeopleSexe = (sFemale, sMale);

  ICalculator = interface(IInvokable)
    ['{9A60C8ED-CEB2-4E09-87D4-4A16F496E5FE}']
    function Add(n1,n2: integer): integer;
    procedure ToText(Value: Currency; const Curr: RawUTF8;
      var Sexe: TPeopleSexe; var Name: RawUTF8);
    {$ifdef TESTRECORD}
    function RecordToText(var Rec: TTestCustomJSONArraySimpleArray): string;
    {$endif}
  end;

  TSQLRecordPeople = class(TSQLRecord)
  protected
    fData: TSQLRawBlob;
    fFirstName: RawUTF8;
    fLastName: RawUTF8;
    fYearOfBirth: integer;
    fYearOfDeath: word;
    {$ifdef TESTRECORD}
    fSexe: TPeopleSexe;
    fSimple: TTestCustomJSONArraySimpleArray;
  public
    class procedure InternalRegisterCustomProperties(Props: TSQLRecordProperties); override;
    {$endif}
  published
    property FirstName: RawUTF8 read fFirstName write fFirstName;
    property LastName: RawUTF8 read fLastName write fLastName;
    property Data: TSQLRawBlob read fData write fData;
    property YearOfBirth: integer read fYearOfBirth write fYearOfBirth;
    property YearOfDeath: word read fYearOfDeath write fYearOfDeath;
  {$ifdef TESTRECORD}
    property Sexe: TPeopleSexe read fSexe write fSexe;
  public
    property Simple: TTestCustomJSONArraySimpleArray read fSimple;
  {$endif}
  end;

  TCustomServer = class(TSQLRestServerFullMemory)
  published
    procedure DropTable(Ctxt: TSQLRestServerURIContext);
  end;

  TPeopleServerAuthentication = (psaNone,psaWeak,psaDefault);


procedure StartServer(auth: TPeopleServerAuthentication);

procedure StopServer;


implementation

{ TServiceCalculator }

type
  TServiceCalculator = class(TInterfacedObject, ICalculator)
  public
    function Add(n1,n2: integer): integer;
    procedure ToText(Value: Currency; const Curr: RawUTF8;
      var Sexe: TPeopleSexe; var Name: RawUTF8);
    {$ifdef TESTRECORD}
    function RecordToText(var Rec: TTestCustomJSONArraySimpleArray): string;
    {$endif}
  end;

function TServiceCalculator.Add(n1, n2: integer): integer;
begin
  result := n1+n2;
end;

procedure TServiceCalculator.ToText(Value: Currency; const Curr: RawUTF8;
  var Sexe: TPeopleSexe; var Name: RawUTF8);
const SEX_TEXT: array[TPeopleSexe] of RawUTF8 = ('Miss','Mister');
begin
  Name := FormatUTF8('% % for % %',[Curr,Value,SEX_TEXT[Sexe],Name]);
  Sexe := sFemale;
end;

{$ifdef TESTRECORD}

function TServiceCalculator.RecordToText(var Rec: TTestCustomJSONArraySimpleArray): string;
var n: integer;
begin
  result := UTF8ToString(RecordSaveJSON(Rec,TypeInfo(TTestCustomJSONArraySimpleArray)));
  Rec.F := Rec.F+'!';
  n := length(Rec.G);
  SetLength(Rec.G,n+1);
  Rec.G[n] := UInt32ToUtf8(n+1);
  inc(Rec.H.H1);
  n := length(Rec.J);
  SetLength(Rec.J,n+1);
  Rec.J[n].J1 := n;
  Rec.J[n].J2.D2 := n;
  Rec.J[n].J3 := TRecordEnum(n mod (ord(high(TRecordEnum))+1));
end;

{$endif}

{ TCustomServer }

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
    if DB.Add(TSQLRecordPeople,['First1','Last1',1801,1826{$ifdef TESTRECORD},0,''{$endif}])=0 then
      writeln('TSQLRecordPeople Error');
    // in all cases, client will call DropTable method-based service
  AddToServerWrapperMethod(DB,['..\..\..\CrossPlatform\templates',
                                '..\..\..\..\CrossPlatform\templates']);
  DB.ServiceRegister(TServiceCalculator,[TypeInfo(ICalculator)],sicShared);
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
  'F RawUTF8 G array of RawUTF8 '+
  'H {H1 integer H2 WideString H3{H3a boolean H3b RawByteString}} I TDateTime '+
  'J [J1 byte J2 TGUID J3 TRecordEnum]';

class procedure TSQLRecordPeople.InternalRegisterCustomProperties(
  Props: TSQLRecordProperties);
begin
  Props.RegisterCustomPropertyFromRTTI(Self,TypeInfo(TTestCustomJSONArraySimpleArray),
    'Simple',@TSQLRecordPeople(nil).fSimple);
end;

initialization
  TTextWriter.RegisterCustomJSONSerializerFromTextSimpleType(TypeInfo(TRecordEnum));
  TTextWriter.RegisterCustomJSONSerializerFromText(
    TypeInfo(TTestCustomJSONArraySimpleArray),__TTestCustomJSONArraySimpleArray);

{$endif TESTRECORD}

end.
