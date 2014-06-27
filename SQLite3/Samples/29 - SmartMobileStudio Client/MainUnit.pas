unit MainUnit;

interface

uses 
  W3System, W3Components, W3Forms, W3Application,
  SynCrossPlatformCrypto, SynCrossPlatformSpecific, SynCrossPlatformREST,
  SmartTests,
  LoginForm;

type
  TApplication = class(TW3CustomApplication)
  private
    FForm1: TLoginForm;
  protected
    procedure ApplicationStarting; override;
  public

  end;

implementation

{ TApplication}

procedure TApplication.ApplicationStarting;
begin
  FForm1 := TLoginForm.Create(Display.View);
  FForm1.Name := 'Form1';
  RegisterFormInstance(FForm1, True);
  // register other forms here

  inherited;
end;


end.
