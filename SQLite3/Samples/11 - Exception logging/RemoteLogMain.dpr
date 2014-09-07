unit RemoteLogMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SynCommons, mORMot, mORMotHttpClient;

type
  TMainForm = class(TForm)
    grpEvent: TGroupBox;
    cbbEvent: TComboBox;
    edtText: TEdit;
    btnEventSend: TButton;
    grpConnection: TGroupBox;
    edtServer: TEdit;
    lblServer: TLabel;
    lblPort: TLabel;
    edtPort: TEdit;
    lblInfoConnect: TLabel;
    btnConnect: TButton;
    procedure btnConnectClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnEventSendClick(Sender: TObject);
  private
    fClient: TSQLHttpClientWinGeneric;
    fNumber: integer;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R Vista.res}

{$R *.dfm}

procedure TMainForm.btnConnectClick(Sender: TObject);
begin
  TSynLog.Family.Level := LOG_VERBOSE;
  try
    Screen.Cursor := crHourGlass;
    try
      fClient := TSQLHttpClient.CreateForRemoteLogging(
        edtServer.Text,edtPort.Text,TSynLog);
    finally
      Screen.Cursor := crDefault;
    end;
  except
    on E: Exception do begin
      FreeAndNil(fClient);
      MessageDlg(E.Message,mtError,[mbOk],0);
      exit;
    end;
  end;
  grpConnection.Enabled := false;
  PTypeInfo(TypeInfo(TSynLogInfo))^.EnumBaseType^.AddCaptionStrings(cbbEvent.Items);
  cbbEvent.ItemIndex := Ord(sllInfo);
  grpEvent.Show;
  btnEventSend.SetFocus;   
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  fClient.Free;
end;

procedure TMainForm.btnEventSendClick(Sender: TObject);
begin
  TSynLog.Add.Log(TSynLogInfo(cbbEvent.ItemIndex),
    FormatUTF8('% - %',[edtText.Text,fNumber]));
  inc(fNumber);
end;

end.
