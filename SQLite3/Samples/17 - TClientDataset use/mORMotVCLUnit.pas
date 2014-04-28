unit mORMotVCLUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Grids, DBGrids,
  SynCommons, mORMot, mORMotMidasVCL, mORMotVCL,
  SynDB, SynDBSQLite3, SynSQLite3Static,
  SynVirtualDataset, SynDBMidasVCL, SynDBVCL,
  DB, ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    dbgrdData: TDBGrid;
    ds1: TDataSource;
    pnl1: TPanel;
    chkFromSQL: TCheckBox;
    chkViaTClientDataSet: TCheckBox;
    lblTiming: TLabel;
    procedure FormDestroy(Sender: TObject);
    procedure chkFromSQLClick(Sender: TObject);
  private
    fDataSet: TDataSet;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fDataSet);
end;

procedure TForm1.chkFromSQLClick(Sender: TObject);
var JSON: RawUTF8;
    DBFileName: TFileName;
    props: TSQLDBConnectionProperties;
    stmt: TSQLDBStatement;
    Timer: TPrecisionTimer;
begin
  ds1.DataSet := nil;
  FreeAndNil(fDataSet);
  Timer.Start;
  if chkFromSQL.Checked then begin
    // test TSynSQLStatementDataSet: reading from SynDB database
    DBFileName :=  '..\..\exe\test.db3';
    if not FileExists(DBFileName) then
      DBFileName :=  '..\..\test.db3';
    if not FileExists(DBFileName) then
      DBFileName :=  'i:\mORMotTemp\exe\test.db3';
    props := TSQLDBSQLite3ConnectionProperties.Create(StringToUTF8(DBFileName),'','','');
    try
      stmt := props.NewThreadSafeStatement;
      try
        stmt.Execute('select * from People',true);
        if chkViaTClientDataSet.Checked then
          fDataSet := StatementToClientDataSet(self,stmt) else
          fDataSet := StatementToDataSet(self,stmt);
      finally
        stmt.Free;
      end;
    finally
      props.Free;
    end;
  end else begin
    // test TSynSQLTableDataSet: reading from JSON content
    JSON := StringFromFile('..\..\exe\People.json');
    if JSON='' then
      JSON := StringFromFile('..\..\People.json');
    if JSON='' then
      JSON := StringFromFile('i:\mORMotTemp\exe\People.json');
    if chkViaTClientDataSet.Checked then
      fDataSet := JSONToClientDataSet(self,JSON) else
      fDataSet := JSONToDataSet(self,JSON, // demo client-side column definition
        [sftInteger,sftUTF8Text,sftUTF8Text,sftBlob,sftInteger,sftInteger]);
    //Assert(DataSetToJSON(fDataSet)<>'');
  end;
  ds1.DataSet := fDataSet;
  lblTiming.Caption := 'Processed in '+Ansi7ToString(Timer.Stop);
end;

end.
