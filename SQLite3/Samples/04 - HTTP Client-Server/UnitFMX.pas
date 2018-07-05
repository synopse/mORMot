unit UnitFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  FMX.Grid.Style, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Grid,
  FMX.StdCtrls, Data.Bind.Components, Data.Bind.ObjectScope, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Fmx.Bind.Grid, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.Grid,

  Generics.Collections,
  SynCommons,
  mORMot,
  mORMotHttpClient,
  SampleData, FMX.Memo, FMX.Edit;

type
  TForm1 = class(TForm)
    StringGrid1: TStringGrid;
    btnFillGrid: TButton;
    AdapterBindSource1: TAdapterBindSource;
    BindingsList1: TBindingsList;
    LinkGridToDataSourceAdapterBindSource1: TLinkGridToDataSource;
    Label1: TLabel;
    NameEdit: TEdit;
    FindButton: TButton;
    Label2: TLabel;
    QuestionMemo: TMemo;
    AddButton: TButton;
    QuitButton: TButton;
    procedure btnFillGridClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FindButtonClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
  private
    MyPeople: TObjectList<TSQLRecord>;
  public
    Database: TSQLRest;
    Model: TSQLModel;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.AddButtonClick(Sender: TObject);
var Rec: TSQLSampleRecord;
begin
  Rec := TSQLSampleRecord.Create;
  try
    // we use explicit StringToUTF8() for conversion below
    // a real application should use TLanguageFile.StringToUTF8() in mORMoti18n
    Rec.Name := StringToUTF8(NameEdit.Text);
    Rec.Question := StringToUTF8(QuestionMemo.Text);
    if Database.Add(Rec,true)=0 then
      ShowMessage('Error adding the data') else begin
      NameEdit.Text := '';
      QuestionMemo.Text := '';
      NameEdit.SetFocus;
    end;
  finally
    Rec.Free;
  end;
end;

procedure TForm1.btnFillGridClick(Sender: TObject);
begin
  MyPeople := Database.RetrieveList(TSQLSampleRecord, '', [], 'ID,Time,Name,Question');

  AdapterBindSource1.Adapter := TListBindSourceAdapter<TSQLSampleRecord>.Create(self, TObjectList<TSQLSampleRecord>(MyPeople), True);
  AdapterBindSource1.Active := True;
end;

procedure TForm1.FindButtonClick(Sender: TObject);
var Rec: TSQLSampleRecord;
begin
  Rec := TSQLSampleRecord.Create(Database,'Name=?',[StringToUTF8(NameEdit.Text)]);
  try
    if Rec.ID=0 then
      QuestionMemo.Text := 'Not found' else
      QuestionMemo.Text := UTF8ToString(Rec.Question);
  finally
    Rec.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var Server: AnsiString;
begin
  Form1.Caption := 'Sample 04 - HTTP Client FMX';
  Model := CreateSampleModel; // from SampleData unit
  if ParamCount=0 then
    Server := 'localhost' else
    Server := AnsiString(Paramstr(1));
  Form1.Database := TSQLHttpClient.Create(Server,'8080',Form1.Model);
  TSQLHttpClient(Form1.Database).SetUser('User','synopse');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Database.Free;
  Model.Free;
end;

end.
