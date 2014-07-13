unit LoginForm;

interface

uses 
  SmartCL.System, SmartCL.Graphics, SmartCL.Components, SmartCL.Forms,
  SmartCL.Fonts, SmartCL.Borders, SmartCL.Application,
  SynCrossPlatformREST, SmartTests,
  SmartCL.Controls.Label, SmartCL.Controls.Editbox, SmartCL.Controls.Panel,
  SmartCL.Controls.Button, w3c.date;

type
  TLoginForm=class(TW3form)
    procedure W3Button1Click(Sender: TObject);
    procedure ConnectClick(Sender: TObject);
  private
    {$I 'LoginForm:intf'}
  protected
    procedure InitializeForm; override;
    procedure InitializeObject; override;
    procedure Resize; override;
  end;

implementation

{ TLoginForm }

procedure TLoginForm.InitializeForm;
begin
  inherited;
  // this is a good place to initialize components
end;

procedure TLoginForm.InitializeObject;
begin
  inherited;
  {$I 'LoginForm:impl'}
  LogonPassword.InputType := itPassword;
end;

procedure TLoginForm.ConnectClick(Sender: TObject);
var client: TSQLRestClientHTTP;
    model: TSQLModel;
begin
  BrowserAPI.console.time('ORM');
  writeln('Creating Data Model');
  model := TSQLModel.Create([TSQLAuthUser,TSQLAuthGroup,TSQLRecordPeople],'root');
  model.GetTableIndexExisting(TSQLRecordPeople);
  var people := new TSQLRecordPeople;
  var s := model.InfoExisting(people.RecordClass).ToJSONAdd(client,people,true,'');
  assert(s='{"RowID":0,"FirstName":"","LastName":"","YearOfBirth":0,"YearOfDeath":0}');
  s := '{"RowID":10,"FirstName":"ab\"c","LastName":"def","YearOfBirth":20,"YearOfDeath":30}';
  assert(people.FromJSON(s));
  assert(people.ID=10);
  assert(people.FirstName='ab"c');
  assert(people.LastName='def');
  assert(people.YearOfBirth=20);
  assert(people.YearOfDeath=30);
  writeln('Connecting to the server at '+ServerAddress.Text+':888');
  client.Free;
  client := TSQLRestClientHTTP.Create(ServerAddress.Text,888,model,false);
  client.Connect(
  lambda
    if client.ServerTimeStamp=0 then
      ShowMessage('Impossible to retrieve server time stamp') else
      writeln('ServerTimeStamp='+IntToStr(client.ServerTimeStamp));
    client.SetUser(TSQLRestAuthenticationDefault,LogonName.Text,LogonPassWord.Text);
    if client.Authentication=nil then
      ShowMessage('Authentication Error');
    writeln('Safely connected with SessionID='+IntToStr(client.Authentication.SessionID));
    people := TSQLRecordPeople.Create(client,1);
    assert(people.ID=1);
    writeln(people.ToJSON(client.Model,'*'));
    writeln('Testing remote CRUD methods');
    ORMTest(client);
    writeln('Disconnect from server');
    client.Free;
    BrowserAPI.console.timeEnd('ORM');
  end,
  lambda
    ShowMessage('Impossible to connect to the server');
  end);
end;

procedure TLoginForm.W3Button1Click(Sender: TObject);
begin
  BrowserAPI.console.time('LowLevel');
  TestSMS;
  BrowserAPI.console.timeEnd('LowLevel');
end;

procedure TLoginForm.Resize;
begin
  inherited;
end;
 
end.
