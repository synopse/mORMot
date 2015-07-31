unit dddToolsAdminDB;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls, ExtCtrls,
  SynCommons, mORMot, mORMotDDD, mORMotUI;

type
  TDBFrame = class(TFrame)
    lstTables: TListBox;
    pnlRight: TPanel;
    pnlTop: TPanel;
    mmoSQL: TMemo;
    btnExec: TButton;
    mmoResult: TMemo;
    drwgrdResult: TDrawGrid;
    spl1: TSplitter;
    spl2: TSplitter;
    btnHistory: TButton;
    procedure lstTablesDblClick(Sender: TObject);
    procedure btnExecClick(Sender: TObject);
    procedure drwgrdResultClick(Sender: TObject);
    procedure btnHistoryClick(Sender: TObject);
  protected
    fmmoResultRow: integer;
    fGrid: TSQLTableToGrid;
    fJson: RawJSON;
    fPreviousSQL: RawUTF8;
    fSQLLogFile: TFileName;
    procedure AddSQL(SQL: string; AndExec: boolean);
    function OnText(Sender: TSQLTable; FieldIndex, RowIndex: Integer; var Text: string): boolean;
    procedure LogClick(Sender: TObject);
    procedure LogDblClick(Sender: TObject);
    procedure LogSearch(Sender: TObject);
  public
    Admin: IAdministratedDaemon;
    DatabaseName: RawUTF8;
    procedure Open;
    destructor Destroy; override;
  end;

  TDBFrameClass = class of TDBFrame;


implementation

{$R *.dfm}

{ TDBFrame }

procedure TDBFrame.Open;
var tables: TRawUTF8DynArray;
    i: integer;
begin
  fSQLLogFile := ChangeFileExt(ExeVersion.ProgramFileName,'.history');
  drwgrdResult.Align := alClient;
  with lstTables.Items do
  try
    BeginUpdate;
    Clear;
    tables := Admin.DatabaseTables(DatabaseName);
    for i := 0 to high(tables) do
      Add(UTF8ToString(tables[i]));
  finally
    EndUpdate;
  end;
end;

procedure TDBFrame.lstTablesDblClick(Sender: TObject);
var i: integer;
begin
  i := lstTables.ItemIndex;
  if i>=0 then
    AddSQL('select * from '+lstTables.Items[i]+' limit 1000',true);
end;

procedure TDBFrame.btnExecClick(Sender: TObject);
var sql,res: RawUTF8;
    mmo: string;
    SelStart, SelLength,i : integer;
    table: TSQLTable;
begin
  SelStart := mmoSQL.SelStart;
  SelLength := mmoSQL.SelLength;
  if SelLength>10 then
    mmo := mmoSQL.SelText else
    mmo := mmoSQL.Lines.Text;
  sql := Trim(StringToUTF8(mmo));
  if sql='' then
    exit;
  Screen.Cursor := crHourGlass;
  try
    try
      fJson := Admin.DatabaseExecute(DatabaseName,sql);
    except
      on E: Exception do
        fJSON := ObjectToJSON(E);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
  FreeAndNil(fGrid);
  fmmoResultRow := 0;
  if sql[1]='#' then begin
    drwgrdResult.Hide;
    mmoResult.Align := alClient;
    if fJson<>'' then
      if IdemPropNameU(sql,'#help') then begin
        fJson := UnQuoteSQLString(fJson);
        res := StringReplaceAll(fJson,'|',#13#10' ');
        with TRawUTF8List.Create do
        try
          SetText(fJson);
          for i := 0 to Count-1 do
            if (ListPtr[i]<>nil) and (ListPtr[i][0]='#') then
              
        finally
          Free;
        end;
      end else
        JSONBufferReformat(pointer(fJson),res);
    mmoResult.Text := UTF8ToString(res);
    fJson := '';
  end else begin
    mmoResult.Text := '';
    mmoResult.Align := alBottom;
    mmoResult.Height := 100;
    table := TSQLTableJSON.Create('',pointer(fJson),length(fJSON));
    fGrid := TSQLTableToGrid.Create(drwgrdResult,table,nil);
    fGrid.SetAlignedByType(sftCurrency,alRight);
    fGrid.SetFieldFixedWidth(100);
    fGrid.FieldTitleTruncatedNotShownAsHint := true;
    fGrid.OnValueText := OnText;
    drwgrdResult.Options := drwgrdResult.Options-[goRowSelect];
    drwgrdResult.Show;
    if table.RowCount>0 then
      drwgrdResultClick(nil);
  end;
  if Sender<>nil then begin
    mmoSQL.SelStart := SelStart;
    mmoSQL.SelLength := SelLength;
  end;
  mmoSQL.SetFocus;
  if ((fJson<>'') or ((sql[1]='#') and (PosEx(' ',sql)>0))) and
     (sql<>fPreviousSQL) then begin
    AppendToTextFile(sql,fSQLLogFile);
    fPreviousSQL := sql;
  end;
end;

destructor TDBFrame.Destroy;
begin
  FreeAndNil(fGrid);
  inherited;
end;

function TDBFrame.OnText(Sender: TSQLTable; FieldIndex, RowIndex: Integer;
  var Text: string): boolean;
begin
  if RowIndex=0 then begin
    Text := UTF8ToString(Sender.GetU(RowIndex,FieldIndex)); // display true column name
    result := true;
  end else
    result := false;
end;

procedure TDBFrame.drwgrdResultClick(Sender: TObject);
var R: integer;
    row: variant;
begin
  R := drwgrdResult.Row;
  if (R>0) and (R<>fmmoResultRow) and (fGrid<>nil) then begin
    fmmoResultRow := R;
    fGrid.Table.ToDocVariant(R,row);
    mmoResult.Text := UTF8ToString(VariantToUTF8(row));
  end;
end;

procedure TDBFrame.btnHistoryClick(Sender: TObject);
var F: TForm;
    List: TListBox;
    Search: TEdit;
    Details: TMemo;
begin
  F := TForm.Create(Application);
  try
    F.Caption := ' '+btnHistory.Hint;
    F.Font := Font;
    F.Width := 800;
    F.Height := 600;
    F.Position := poMainFormCenter;
    Search := TEdit.Create(F);
    Search.Parent := F;
    Search.Align := alTop;
    Search.Height := 24;
    Search.OnChange := LogSearch;
    Details := TMemo.Create(F);
    Details.Parent := F;
    Details.Align := alBottom;
    Details.Height := 200;
    Details.ReadOnly := true;
    Details.Font.Name := 'Consolas';
    List := TListBox.Create(F);
    with List do begin
      Parent := F;
      Align := alClient;
      Tag := PtrInt(Details);
      OnClick := LogClick;
      OnDblClick := LogDblClick;
    end;
    Search.Tag := PtrInt(List);
    LogSearch(Search);
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TDBFrame.LogClick(Sender: TObject);
var List: TListBox absolute Sender;
    ndx: integer;
begin
  ndx := cardinal(List.ItemIndex);
  if ndx>=0 then
    TMemo(List.Tag).Text := copy(List.Items[ndx],21,maxInt) else
    TMemo(List.Tag).Clear;
end;

procedure TDBFrame.LogDblClick(Sender: TObject);
var List: TListBox absolute Sender;
    SQL: string;
    ndx: integer;
begin
  ndx := cardinal(List.ItemIndex);
  if ndx>=0 then begin
    SQL := copy(List.Items[ndx],21,maxInt);
    AddSQL(SQL,IsSelect(pointer(StringToAnsi7(SQL))));
    TForm(List.Owner).Close;
  end;
end;

procedure TDBFrame.LogSearch(Sender: TObject);
const MAX_LINES_IN_HISTORY = 500;
var Edit: TEdit absolute Sender;
    List: TListBox;
    i: integer;
    s: RawUTF8;
begin
  s := SynCommons.UpperCase(StringToUTF8(Edit.Text));
  List := pointer(Edit.Tag);
  with TMemoryMapText.Create(fSQLLogFile) do
  try
    List.Items.BeginUpdate;
    List.Items.Clear;
    for i := Count-1 downto 0 do
      if (s='') or LineContains(s,i) then
        if List.Items.Add(Strings[i])>MAX_LINES_IN_HISTORY then
          break; // read last 500 lines from UTF-8 file
  finally
    Free;
    List.Items.EndUpdate;
  end;
  List.ItemIndex := 0;
  LogClick(List);
end;

procedure TDBFrame.AddSQL(SQL: string; AndExec: boolean);
var len: integer;
    orig: string;
begin
  SQL := SysUtils.Trim(SQL);
  len := Length(SQL);
  if len=0 then
    exit;
  orig := mmoSQL.Lines.Text;
  if orig<>'' then
    SQL := #13#10#13#10+SQL;
  SQL := orig+SQL;
  mmoSQL.Lines.Text := SQL;
  mmoSQL.SelStart := length(SQL)-len;
  mmoSQL.SelLength := len;
  if AndExec then
    btnExecClick(btnExec) else
    mmoSQL.SetFocus;
end;

end.

