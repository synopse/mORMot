unit dddToolsAdminDB;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Grids,
  StdCtrls,
  ExtCtrls,
  Menus,
  SynMemoEx,
  SynCommons,
  mORMot,
  mORMotDDD,
  mORMotHttpClient,
  mORMotUI,
  SynMustache;

type
  TDBFrame = class(TFrame)
    lstTables: TListBox;
    pnlRight: TPanel;
    pnlTop: TPanel;
    mmoSQL: TMemo;
    btnExec: TButton;
    drwgrdResult: TDrawGrid;
    spl1: TSplitter;
    spl2: TSplitter;
    btnHistory: TButton;
    btnCmd: TButton;
    pmCmd: TPopupMenu;
    procedure lstTablesDblClick(Sender: TObject); virtual;
    procedure btnExecClick(Sender: TObject); virtual;
    procedure drwgrdResultClick(Sender: TObject); virtual;
    procedure btnHistoryClick(Sender: TObject); virtual;
    procedure btnCmdClick(Sender: TObject); virtual;
  protected
    fmmoResultRow: integer;
    fJson: RawJSON;
    fSQL, fPreviousSQL: RawUTF8;
    fSQLLogFile: TFileName;
    function ExecSQL(const SQL: RawUTF8): RawUTF8;
    procedure SetResult(const JSON: RawUTF8); virtual;
    function OnText(Sender: TSQLTable; FieldIndex, RowIndex: Integer; var Text:
      string): boolean;
    procedure LogClick(Sender: TObject);
    procedure LogDblClick(Sender: TObject);
    procedure LogSearch(Sender: TObject);
    procedure GridToVariant(var result: variant); virtual;
  public
    DatabaseName: RawUTF8;
    mmoResult: TMemoEx;
    Grid: TSQLTableToGrid;
    GridLastTableName: RawUTF8;
    Client: TSQLHttpClientWebsockets;
    Admin: IAdministratedDaemon;
    Tables: TRawUTF8DynArray;
    AssociatedModel: TSQLModel;
    AssociatedTables: TSQLRecordClassDynArray;
    TableDblClickOrderByIdDesc: boolean;
    TableDblClickOrderByIdDescCSV: string;
    OnAfterExecute: TNotifyEvent;
    constructor Create(AOwner: TComponent); override;
    procedure Open; virtual;
    procedure AddSQL(SQL: string; AndExec: boolean);
    destructor Destroy; override;
  end;

  TDBFrameClass = class of TDBFrame;

  TDBFrameDynArray = array of TDBFrame;

implementation

{$R *.dfm}

const
  WRAPPER_TEMPLATE = '{{#soa.services}}'#13#10'{{#methods}}'#13#10 +
    '#get {{uri}}/{{methodName}}{{#hasInParams}}?{{#args}}{{#dirInput}}{{argName}}={{typeSource}}' +
    '{{#commaInSingle}}&{{/commaInSingle}}{{/dirInput}}{{/args}}{{/hasInParams}}'#13#10 +
    '{{#hasOutParams}}'#13#10' { {{#args}}{{#dirOutput}}{{jsonQuote argName}}: {{typeSource}}' +
    '{{#commaOutResult}},{{/commaOutResult}} {{/dirOutput}}{{/args}} }'#13#10 +
    '{{/hasOutParams}}{{/methods}}'#13#10'{{/soa.services}}'#13#10'{{#enumerates}}{{name}}: ' +
    '{{#values}}{{EnumTrim .}}={{-index0}}{{^-last}}, {{/-last}}{{/values}}'#13#10'{{/enumerates}}';

{ TDBFrame }

constructor TDBFrame.Create(AOwner: TComponent);
begin
  inherited;
  mmoResult := TMemoEx.Create(self);
  mmoResult.Name := 'mmoResult';
  mmoResult.Parent := pnlRight;
  mmoResult.Align := alClient;
  mmoResult.Font.Height := -11;
  mmoResult.Font.Name := 'Consolas';
  mmoResult.readonly := True;
  mmoResult.ScrollBars := ssVertical;
  mmoResult.Text := '';
  mmoResult.RightMargin := 130;
  mmoResult.RightMarginVisible := true;
  mmoResult.OnGetLineAttr := mmoResult.JSONLineAttr;
end;

procedure TDBFrame.Open;
var
  i: integer;
begin
  fSQLLogFile := ChangeFileExt(ExeVersion.ProgramFileName, '.history');
  drwgrdResult.Align := alClient;
  with lstTables.Items do
  try
    BeginUpdate;
    Clear;
    Tables := Admin.DatabaseTables(DatabaseName);
    for i := 0 to high(Tables) do
      Add(UTF8ToString(Tables[i]));
  finally
    EndUpdate;
  end;
  mmoSQL.Text := '#help';
  btnExecClick(nil);
  mmoSQL.Text := '';
end;

procedure TDBFrame.lstTablesDblClick(Sender: TObject);
var
  i: integer;
  table, sql: string;
begin
  i := lstTables.ItemIndex;
  if i < 0 then
    exit;
  table := lstTables.Items[i];
  sql := 'select * from ' + table;
  if TableDblClickOrderByIdDesc or
     ((TableDblClickOrderByIdDescCSV<>'') and
      (Pos(table + ',', TableDblClickOrderByIdDescCSV + ',') > 0)) then
    sql := sql + ' order by id desc';
  sql := sql + ' limit 1000';
  AddSQL(sql, true);
end;

procedure TDBFrame.SetResult(const JSON: RawUTF8);
begin
  FreeAndNil(Grid);
  drwgrdResult.Hide;
  mmoResult.Align := alClient;
  mmoResult.WordWrap := false;
  mmoResult.ScrollBars := ssBoth;
  mmoResult.RightMarginVisible := false;
  if (JSON = '') or (JSON[1] in ['A'..'Z', '#']) then
    mmoResult.OnGetLineAttr := nil
  else
    mmoResult.OnGetLineAttr := mmoResult.JSONLineAttr;
  mmoResult.TopRow := 0;
  mmoResult.Text := UTF8ToString(StringReplaceTabs(JSON, '    '));
  fJson := '';
end;

procedure TDBFrame.btnExecClick(Sender: TObject);

  function NewPopup(const c: string): TMenuItem;
  var
    cmd: string;
    i: integer;
  begin
    result := TMenuItem.Create(pmCmd);
    result.Caption := c;
    i := Pos(' ', c);
    if i > 0 then
      cmd := copy(c, 1, i) + '*'
    else begin
      i := Pos('(', c);
      if i > 0 then
        cmd := copy(c, 1, i) + '*)'
      else
        cmd := c;
    end;
    result.Hint := cmd;
    result.OnClick := btnExecClick;
  end;

var
  res, ctyp: RawUTF8;
  mmo, cmd: string;
  SelStart, SelLength, i: integer;
  table: TSQLTable;
  tables: TSQLRecordClassDynArray;
  P: PUTF8Char;
  exec: TServiceCustomAnswer;
  ctxt: variant;
begin
  if (Sender <> nil) and Sender.InheritsFrom(TMenuItem) then begin
    mmo := TMenuItem(Sender).Hint;
    mmoSQL.Text := mmo;
    i := Pos('*', mmo);
    if i > 0 then begin
      mmoSQL.SelStart := i - 1;
      mmoSQL.SelLength := 1;
      mmoSQL.SetFocus;
      exit;
    end;
  end;
  SelStart := mmoSQL.SelStart;
  SelLength := mmoSQL.SelLength;
  if SelLength > 10 then
    mmo := mmoSQL.SelText
  else
    mmo := mmoSQL.Lines.Text;
  fSQL := Trim(StringToUTF8(mmo));
  if fSQL = '' then
    exit;
  if IdemPropNameU(fSQL, '#client') then begin
    fJSON := ObjectToJSON(Client);
  end
  else begin
    Screen.Cursor := crHourGlass;
    try
      try
        exec := Admin.DatabaseExecute(DatabaseName, fSQL);
        ctyp := FindIniNameValue(pointer(exec.Header), HEADER_CONTENT_TYPE_UPPER);
        if ctyp = '' then
          fJSON := exec.Content
        else
          fJSON := '';
      except
        on E: Exception do
          fJSON := ObjectToJSON(E);
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
  FreeAndNil(Grid);
  GridLastTableName := '';
  fmmoResultRow := 0;
  if fSQL[1] = '#' then begin
    if fJson <> '' then
      if IdemPropNameU(fSQL, '#help') then begin
        fJson := Trim(UnQuoteSQLString(fJson)) + '|#client'#13#10;
        res := StringReplaceAll(fJson, '|', #13#10' ');
        if pmCmd.Items.Count = 0 then begin
          P := pointer(res);
          while P <> nil do begin
            cmd := UTF8ToString(Trim(GetNextLine(P, P)));
            if (cmd <> '') and (cmd[1] = '#') then
              pmCmd.Items.Add(NewPopup(cmd));
          end;
        end;
      end
      else if IdemPropNameU(fSQL, '#wrapper') then begin
        ctxt := _JsonFast(fJson);
        res := TSynMustache.Parse(WRAPPER_TEMPLATE).Render(ctxt, nil,
          TSynMustache.HelpersGetStandardList, nil, true);
      end
      else
        JSONBufferReformat(pointer(fJson), res, jsonUnquotedPropName);
    if Assigned(OnAfterExecute) then
      OnAfterExecute(self);
    SetResult(res);
  end
  else begin
    GridLastTableName := GetTableNameFromSQLSelect(fSQL, false);
    mmoResult.Text := '';
    mmoResult.Align := alBottom;
    mmoResult.WordWrap := true;
    mmoResult.ScrollBars := ssVertical;
    mmoResult.Height := 100;
    if AssociatedModel <> nil then
      tables := AssociatedModel.Tables
    else
      tables := AssociatedTables;
    table := TSQLTableJSON.CreateFromTables(tables, '', pointer(fJson), length(fJSON));
    Grid := TSQLTableToGrid.Create(drwgrdResult, table, nil);
    Grid.SetAlignedByType(sftCurrency, alRight);
    Grid.SetFieldFixedWidth(100);
    Grid.FieldTitleTruncatedNotShownAsHint := true;
    Grid.OnValueText := OnText;
    if Assigned(OnAfterExecute) then
      OnAfterExecute(self);
    drwgrdResult.Options := drwgrdResult.Options - [goRowSelect];
    drwgrdResult.Show;
    if table.RowCount > 0 then
      drwgrdResultClick(nil);
  end;
  if Sender <> nil then begin
    mmoSQL.SelStart := SelStart;
    mmoSQL.SelLength := SelLength;
    mmoSQL.SetFocus;
  end;
  if ((fJson <> '') or ((fSQL[1] = '#') and (PosEx(' ', fSQL) > 0))) and (fSQL
    <> fPreviousSQL) then begin
    AppendToTextFile(fSQL, fSQLLogFile);
    fPreviousSQL := fSQL;
  end;
end;

destructor TDBFrame.Destroy;
begin
  FreeAndNil(Grid);
  FreeAndNil(AssociatedModel);
  inherited;
end;

function TDBFrame.OnText(Sender: TSQLTable; FieldIndex, RowIndex: Integer; var
  Text: string): boolean;
begin
  if Sender.FieldType(FieldIndex) in [sftBoolean] then
    result := false
  else begin
    Text := Sender.GetString(RowIndex, FieldIndex); // display the value as such
    result := true;
  end;
end;

procedure TDBFrame.GridToVariant(var result: variant);
begin
  Grid.Table.ToDocVariant(fmmoResultRow, result, JSON_OPTIONS_NAMEVALUE[true], true);
end;

procedure TDBFrame.drwgrdResultClick(Sender: TObject);
var
  R: integer;
  row: variant;
  json: RawUTF8;
begin
  R := drwgrdResult.Row;
  if (R > 0) and (R <> fmmoResultRow) and (Grid <> nil) then begin
    fmmoResultRow := R;
    GridToVariant(row);
    mmoResult.OnGetLineAttr := mmoResult.JSONLineAttr;
    JSONBufferReformat(pointer(VariantToUTF8(row)), json, jsonUnquotedPropNameCompact);
    mmoResult.Text := UTF8ToString(json);
  end;
end;

procedure TDBFrame.btnHistoryClick(Sender: TObject);
var
  F: TForm;
  List: TListBox;
  Search: TEdit;
  Details: TMemo;
begin
  F := TForm.Create(Application);
  try
    F.Caption := ' ' + btnHistory.Hint;
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
    Details.readonly := true;
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
var
  List: TListBox absolute Sender;
  ndx: integer;
begin
  ndx := cardinal(List.ItemIndex);
  if ndx >= 0 then
    TMemo(List.Tag).Text := copy(List.Items[ndx], 21, maxInt)
  else
    TMemo(List.Tag).Clear;
end;

procedure TDBFrame.LogDblClick(Sender: TObject);
var
  List: TListBox absolute Sender;
  SQL: string;
  ndx: integer;
begin
  ndx := cardinal(List.ItemIndex);
  if ndx >= 0 then begin
    SQL := copy(List.Items[ndx], 21, maxInt);
    AddSQL(SQL, IsSelect(pointer(StringToAnsi7(SQL))));
    TForm(List.Owner).Close;
  end;
end;

procedure TDBFrame.LogSearch(Sender: TObject);
const
  MAX_LINES_IN_HISTORY = 500;
var
  Edit: TEdit absolute Sender;
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
    for i := Count - 1 downto 0 do
      if (s = '') or LineContains(s, i) then
        if List.Items.Add(Strings[i]) > MAX_LINES_IN_HISTORY then
          break; // read last 500 lines from UTF-8 file
  finally
    Free;
    List.Items.EndUpdate;
  end;
  List.ItemIndex := 0;
  LogClick(List);
end;

procedure TDBFrame.AddSQL(SQL: string; AndExec: boolean);
var
  len: integer;
  orig: string;
begin
  SQL := SysUtils.Trim(SQL);
  len := Length(SQL);
  if len = 0 then
    exit;
  orig := mmoSQL.Lines.Text;
  if orig <> '' then
    SQL := #13#10#13#10 + SQL;
  SQL := orig + SQL;
  mmoSQL.Lines.Text := SQL;
  mmoSQL.SelStart := length(SQL) - len;
  mmoSQL.SelLength := len;
  if AndExec then
    btnExecClick(btnExec)
  else
    mmoSQL.SetFocus;
end;

procedure TDBFrame.btnCmdClick(Sender: TObject);
begin
  with ClientToScreen(btnCmd.BoundsRect.TopLeft) do
    pmCmd.Popup(X, Y + btnCmd.Height);
end;

function TDBFrame.ExecSQL(const SQL: RawUTF8): RawUTF8;
var
  exec: TServiceCustomAnswer;
begin
  exec := Admin.DatabaseExecute(DatabaseName, sql);
  result := exec.Content;
end;

end.

