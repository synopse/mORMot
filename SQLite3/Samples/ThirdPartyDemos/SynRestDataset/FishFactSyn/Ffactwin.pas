unit Ffactwin;

{ This application shows how to display TSynRestDataset style memo and graphic
 fields in a form.

 - This application use TImage for display the image from Project19Server.db3.
 - Originally Implemented by EMartin
 - Modified by HOUDW2006 2016-05-09
 - (2017-08-02)
   * added calculated field example
   * added field Modified: TModTime in SampleData unit and Project19Server.db3.BIOLIFE table
   * added example display format in TModTime field 
}

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, StdCtrls, DBCtrls, DBGrids, DB, Buttons, ExtCtrls,
  SynRestMidasVCL, DBClient,
  SynCommons, mORMot, mORMotHttpClient,
  OleCtrls, Dialogs, ExtDlgs,
  SynGdiPlus, Grids, SampleData, ImageLoader;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    DBMemo1: TDBMemo;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    dbnvgr1: TDBNavigator;
    btnUpload: TButton;
    dlgOpenPic1: TOpenPictureDialog;
    img: TImage;
    DBLabel1: TDBText;
    pnl1: TPanel;
    lbl1: TLabel;
    dbtxtCommon_Name: TDBText;
    procedure FormCreate(Sender: TObject);
    procedure dbnvgr1Click(Sender: TObject; Button: TNavigateBtn);
    procedure btnUploadClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    fImageLoader: TImageLoader;
    fServer, fPort, fRoot: RawUTF8;
    fSQLModel: TSQLModel;
    fRestClient: TSQLRestClientURI;
    SynRestDataset: TSynRestDataset;

    procedure DoOnAfterScroll(Dataset: TDataset);
    procedure DoOnCalcFields(Dataset: TDataset);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  SynRestVCL;
  
{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  fImageLoader := TImageLoader.Create;	// used to load image from blob fields

  fServer := 'LocalHost';
  fPort := '8080';
  fRoot := 'root';
  fSQLModel := CreateSampleModel(fRoot);
  fRestClient := TSQLHttpClient.Create(fServer, fPort, fSQLModel);

  SynRestDataset := TSynRestDataset.Create(Nil);
  SynRestDataset.RestClient := fRestClient;
  SynRestDataset.CommandText := 'SELECT Species_No, Category, Common_Name, Species_Name, Length_cm, Length_in, Notes, '
      + 'Modified, Graphic, Som FROM BioLife ORDER BY Species_No';
  // calculated field
  with TStringField.Create(SynRestDataset) do begin
    FieldName := 'Full_Name';
    Calculated:= True;
    Size := 50;
    Dataset := SynRestDataset;
  end;
  SynRestDataset.OnCalcFields := DoOnCalcFields;

  // WHERE and/or ORDER BY clauses, and Parameters can be used as well.
  //SynRestDataset.CommandText := 'SELECT * FROM BioLife '
  //    + 'WHERE Species_No < :Species_No '
  //    + 'ORDER BY Species_No ';
  //SynRestDataset.Params.ParamByName('SPecies_No').Value := 100;
  SynRestDataset.AfterScroll := DoOnAfterScroll;
  SynRestDataset.Open;
  // set display format in TTimeLogField
  TTimeLogField(SynRestDataset.FieldByName('Modified')).DisplayFormat := 'YYYY-MM-DD HH:NN:SS';

  DataSource1.DataSet := SynRestDataset;
  // show the first record image
  DoOnAfterScroll(Nil);
  // hide blob and ID fields in the grid
  for I := 0 to DBGrid1.Columns.Count-1 do
  begin
    if (DBGrid1.Columns[I].Field.DataType = DB.ftBlob) then
      DBGrid1.Columns[I].Visible := False
    else if (DBGrid1.Columns[I].Field.FieldName = 'ID') then  // Hide the ID column
      DBGrid1.Columns[I].Visible := False;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fImageLoader);
  FreeAndNil(SynRestDataset);
  FreeAndNil(fRestClient);
  FreeAndNil(fSQLModel);
end;

procedure TForm1.dbnvgr1Click(Sender: TObject; Button: TNavigateBtn);
begin
  case Button of
    nbDelete, nbPost:
      SynRestDataset.ApplyUpdates(0);
  end;
end;

procedure TForm1.btnUploadClick(Sender: TObject);
var
  fID: Integer;
  fStream: TMemoryStream;
begin
  fStream := TMemoryStream.Create;
  try
    if dlgOpenPic1.Execute then
    begin
      fStream.LoadFromFile(dlgOpenPic1.FileName);
      fStream.Position := 0;
      fID := SynRestDataset.FieldByName('Species_No').AsInteger;
      SynRestDataset.RestClient.UpdateBlob(TSQLBiolife, fID, 'Graphic', fStream);

      fImageLoader.LoadImage(img.Picture, fStream);
    end;
  finally
    fStream.Free;
  end;
end;

procedure TForm1.DoOnAfterScroll(Dataset: TDataset);
var
  fID: TID;
  fBlobData: TSQLRawBlob;
begin
  fID := SynRestDataset.FieldByName('Species_No').AsInteger;
  if (SynRestDataset.RestClient.RetrieveBlob(TSQLBiolife, fID, 'Graphic', fBlobData)) then
  begin
    fImageLoader.LoadImage(img.Picture, fBlobData);
  end;
end;

procedure TForm1.DoOnCalcFields(Dataset: TDataset);
begin
  with Dataset do begin
    FieldByName('Full_Name').AsString :=
      FieldByName('Common_Name').AsString + ' - ' + FieldByName('Species_Name').AsString;
  end;
end;

end.