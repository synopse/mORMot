unit LCLMain;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SynCrossPlatformJSON, StdCtrls, FileUtil;

type
  TForm1 = class(TForm)
    lbl1: TLabel;
    edtValue: TEdit;
    lbl2: TLabel;
    mmoJSON: TMemo;
    btnRewind: TButton;
    btnNext: TButton;
    procedure FormCreate(Sender: TObject);
    procedure edtValueChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
  private
  public
    doc: variant;
    table: TJSONTable;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
var json: string;
    FN: TFileName;
    level: integer;
begin
  {$ASSERTIONS ON}
  doc := JSONVariant('{"test":1234,"name":"Joh\"n\r","zero":0.0}');
  assert(doc.test=1234);
  assert(doc.name='Joh"n'#13);
  assert(doc.name2=null);
  assert(doc.zero=0);
  json := doc;
  assert(json='{"test":1234,"name":"Joh\"n\r","zero":0}');
  TJSONVariantData(doc)['name2'] := 3.1415926;
  TJSONVariantData(doc)['name'] := 'John';
  json := doc;
  assert(json='{"test":1234,"name":"John","zero":0,"name2":3.1415926}');
  FN := 'people.json';
  for level := 1 to 4 do
    if FileExists(FN) then
      break else
      FN := IncludeTrailingPathDelimiter('..')+FN;
  table := TJSONTable.Create(UTF8FileToString(FN));
  assert(length(table.FieldNames)=6);
end;

procedure TForm1.edtValueChange(Sender: TObject);
begin
  TJSONVariantData(doc)['value'] := edtValue.Text;
  mmoJSON.Text := doc;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  table.Free;
end;

procedure TForm1.btnNextClick(Sender: TObject);
var json: string;
begin
  if table.Step(Sender=btnRewind) then
    json := JSONVariant(table.RowValue) else
    json := 'null';
  mmoJSON.Text := json;
end;

end.
