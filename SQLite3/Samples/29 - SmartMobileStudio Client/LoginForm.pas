unit LoginForm;

interface

uses 
  W3System, W3Graphics, W3Components, W3Forms, W3Fonts, W3Borders, W3Application,
  W3Label, W3Editbox, W3Panel, W3Button, SynCrossPlatformREST;

type
  TLoginForm=class(TW3form)
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
  model := TSQLModel.Create([TSQLAuthUser],'root');
  model.Add(TSQLAuthGroup); // circumvent requests/43
  client := TSQLRestClientHTTP.Create(ServerAddress.Text,888,model,false);
  client.Connect;
  if client.ServerTimeStamp=0 then
    ShowMessage('Impossible to retrieve server time stamp');

end;

procedure TLoginForm.Resize;
begin
  inherited;
end;
 
end.
