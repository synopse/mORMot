/// main form of the TSynLog .log file vizualizer
unit LogViewMain;

interface


uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SynCommons, ImgList, StdCtrls, CheckLst, Menus, ExtCtrls, ShellAPI,
  Grids,
{$WARN UNIT_PLATFORM OFF}
  FileCtrl,
{$WARN UNIT_PLATFORM ON}
  Clipbrd;

type
  TMainLogView = class(TForm)
    PanelLeft: TPanel;
    BtnBrowse: TButton;
    EventsList: TCheckListBox;
    FilterMenu: TPopupMenu;
    EditSearch: TEdit;
    BtnSearchNext: TButton;
    MemoBottom: TMemo;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    BtnStats: TButton;
    OpenDialog: TOpenDialog;
    BtnMapSearch: TButton;
    MergedProfile: TCheckBox;
    ProfileGroup: TRadioGroup;
    ImageLogo: TImage;
    List: TDrawGrid;
    ProfileList: TDrawGrid;
    ThreadGroup: TGroupBox;
    BtnThreadNext: TButton;
    ThreadListBox: TCheckListBox;
    Splitter1: TSplitter;
    BtnThreadShow: TButton;
    ThreadListMenu: TPopupMenu;
    ThreadListMenuAll: TMenuItem;
    ThreadListMenuNone: TMenuItem;
    PanelBrowse: TPanel;
    Drive: TDriveComboBox;
    Directory: TDirectoryListBox;
    Files: TFileListBox;
    Splitter4: TSplitter;
    ListMenu: TPopupMenu;
    ListMenuCopy: TMenuItem;
    BtnSearchPrevious: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtnFilterClick(Sender: TObject);
    procedure EventsListClickCheck(Sender: TObject);
    procedure ListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure BtnSearchNextClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListClick(Sender: TObject);
    procedure ProfileListClick(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
    procedure BtnStatsClick(Sender: TObject);
    procedure BtnOpenClick(Sender: TObject);
    procedure BtnMapSearchClick(Sender: TObject);
    procedure MergedProfileClick(Sender: TObject);
    procedure ProfileGroupClick(Sender: TObject);
    procedure ImageLogoClick(Sender: TObject);
    procedure EventsListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ListDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure ProfileListDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure EventsListDblClick(Sender: TObject);
    procedure BtnThreadNextClick(Sender: TObject);
    procedure BtnThreadShowClick(Sender: TObject);
    procedure ThreadListBoxDblClick(Sender: TObject);
    procedure ThreadListMenuClick(Sender: TObject);
    procedure ThreadListBoxClick(Sender: TObject);
    procedure BtnBrowseClick(Sender: TObject);
    procedure FilesClick(Sender: TObject);
    procedure ListMenuCopyClick(Sender: TObject);
    procedure BtnSearchPreviousClick(Sender: TObject);
  protected
    FLog: TSynLogFile;
    FLogSelected: TIntegerDynArray;
    FLogSelectedCount: integer;
    FMainCaption: string;
    FMenuFilterAll: TMenuItem;
    FLogUncompressed: TMemoryStream;
    FEventCaption: array[TSynLogInfo] of string;
    FThreadSelected: TByteDynArray;
    FLastSearch: RawUTF8;
    FLastSearchSender: TObject;
    procedure SetLogFileName(const Value: TFileName);
    procedure SetListItem(Index: integer; const search: RawUTF8='');
    procedure BtnFilterMenu(Sender: TObject);
    procedure ThreadListCheckRefresh;
  public
    destructor Destroy; override;
    property LogFileName: TFileName write SetLogFileName;
  end;


var
  MainLogView: TMainLogView;


implementation

{$R *.dfm}

type
  TLogFilter = (
    lfNone,lfAll,lfErrors,lfExceptions,lfProfile,lfDatabase,lfClientServer,
    lfDebug,lfCustom);

const
  LOG_FILTER: array[TLogFilter] of TSynLogInfos = (
    [], [succ(sllNone)..high(TSynLogInfo)],
    [sllError,sllLastError,sllException,sllExceptionOS],
    [sllException,sllExceptionOS], [sllEnter,sllLeave],
    [sllSQL,sllCache,sllDB], [sllClient,sllServer,sllServiceCall, sllServiceReturn],
    [sllDebug,sllTrace,sllEnter], [sllCustom1..sllCustom4]);
  LOG_COLORS: array[Boolean,TSynLogInfo] of TColor = (
    (clWhite,$DCC0C0,$DCDCDC,clSilver,$8080C0,$8080FF,$C0DCC0,$DCDCC0,
//  sllNone, sllInfo, sllDebug, sllTrace, sllWarning, sllError, sllEnter, sllLeave,
     $C0C0F0, $C080FF, $C080F0, $C080C0, $C080C0,
//  sllLastError, sllException, sllExceptionOS, sllMemory, sllStackTrace,
     $4040FF, $B08080, $B0B080, $8080DC, $80DC80, $DC8080, $DCFF00, $DCD000,
//  sllFail, sllSQL, sllCache, sllResult, sllDB, sllHTTP, sllClient, sllServer,
     $DCDC80, $DC80DC, $DCDCDC,
//  sllServiceCall, sllServiceReturn, sllUserAuth,
     $D0D0D0, $D0D0DC, $D0D0C0, $D0D0E0, $20E0D0),
//  sllCustom1, sllCustom2, sllCustom3, sllCustom4, sllNewRun
    (clBlack,clBlack,clBlack,clBlack,clBlack,clWhite,clBlack,clBlack,
     clWhite,clWhite,clWhite,clBlack,clBlack,
     clWhite,clWhite,clBlack,clWhite,clBlack,clBlack,clBlack,clBlack,
     clBlack,clBlack,clBlack,
     clBlack,clBlack,clBlack,clBlack,clBlack));

     
{ TMainLogView }

procedure TMainLogView.SetLogFileName(const Value: TFileName);
var E: TSynLogInfo;
    i: integer;
begin
  FreeAndNil(FLog);
  FreeAndNil(FLogUncompressed);
  Finalize(FLogSelected);
  FLogSelectedCount := 0;
  ThreadListBox.Clear;
  Caption := FMainCaption+ExpandFileName(Value);
  Screen.Cursor := crHourGlass;
  try
    List.RowCount := 0;
    if SameText(ExtractFileExt(Value),'.synlz') then begin
      FLogUncompressed := StreamUnSynLZ(Value,LOG_MAGIC);
      if FLogUncompressed=nil then
        exit; // invalid file content
      FLog := TSynLogFile.Create(FLogUncompressed.Memory,FLogUncompressed.Size);
      FLog.FileName := Value;
    end else
      FLog := TSynLogFile.Create(Value);
    EventsList.Items.BeginUpdate;
    EventsList.Items.Clear;
    if FLog.EventLevel=nil then begin // if not a TSynLog file -> open as plain text
      List.ColCount := 1;
      List.ColWidths[0] := 2000;
    end else begin
      if FLog.EventThread<>nil then begin
        List.ColCount := 4;
        List.ColWidths[0] := 70;
        List.ColWidths[1] := 60;
        case FLog.ThreadsCount of
        0..9:     List.ColWidths[2] := 15;
        10..99:   List.ColWidths[2] := 20;
        100..999: List.ColWidths[2] := 30;
             else List.ColWidths[2] := 40;
        end;
        List.ColWidths[3] := 2000;
      end else begin
        List.ColCount := 3;
        List.ColWidths[0] := 70;
        List.ColWidths[1] := 60;
        List.ColWidths[2] := 2000;
      end;
      SetLength(FLogSelected,FLog.Count);
      for E := succ(sllNone) to high(E) do begin
        FEventCaption[E] := GetCaptionFromEnum(TypeInfo(TSynLogInfo),ord(E));
        if E in FLog.EventLevelUsed then
          EventsList.Items.AddObject(FEventCaption[E],pointer(ord(E)));
      end;
      for i := 1 to FilterMenu.Items.Count-1 do
        FilterMenu.Items[i].Visible :=
          LOG_FILTER[TLogFilter(FilterMenu.Items[i].Tag)]*FLog.EventLevelUsed<>[];
    end;
  finally
    Screen.Cursor := crDefault;
  end;
  EventsList.Items.EndUpdate;
  EventsList.Height := 8+EventsList.Count*EventsList.ItemHeight;
  ProfileGroup.Top := EventsList.Top+EventsList.Height+12;
  MergedProfile.Top := ProfileGroup.Top+ProfileGroup.Height+2;
  BtnStats.Top := MergedProfile.Top+32;
  BtnMapSearch.Top := BtnStats.Top;
  ThreadGroup.Visible := (FLog<>nil) and (FLog.EventThread<>nil);
  if ThreadGroup.Visible then begin
    SetLength(FThreadSelected,(FLog.ThreadsCount shr 3)+1);
    fillchar(FThreadSelected[0],Length(FThreadSelected),255);
    ThreadGroup.Top := BtnStats.Top+32;
    with ThreadListBox.Items do begin
      BeginUpdate;
      try
        for i := 1 to FLog.ThreadsCount do begin
          Add(Format('Thread %d (%d rows)',[i,FLog.ThreadsRows[i]]));
          ThreadListBox.Checked[i-1] := true;
        end;
      finally
        EndUpdate;
      end;
    end;
  end;
  ProfileGroup.ItemIndex := 0;
  MergedProfile.Checked := false;
  BtnFilterMenu(FMenuFilterAll);
  EventsList.Enabled := FLog<>nil;
  ProfileGroup.Enabled := (FLog<>nil) and (FLog.LogProcCount<>0);
  MergedProfile.Enabled := ProfileGroup.Enabled;
  BtnStats.Enabled := (FLog<>nil) and (FLog.EventLevel<>nil);
  BtnSearchNext.Enabled := FLog<>nil;
  BtnSearchPrevious.Enabled := FLog<>nil;
  EditSearch.Enabled := FLog<>nil;
  List.Visible := FLog<>nil;
  EventsListClickCheck(nil);
end;

destructor TMainLogView.Destroy;
begin
  FLog.Free;
  FLogUncompressed.Free;
  inherited;
end;

procedure TMainLogView.FormCreate(Sender: TObject);
var F: TLogFilter;
    O: TLogProcSortOrder;
    M: TMenuItem;
begin
  FMainCaption := Caption;
  for F := low(F) to high(F) do begin
    M := TMenuItem.Create(self);
    M.Caption := GetCaptionFromEnum(TypeInfo(TLogFilter),Ord(F));
    M.Tag := ord(F);
    M.OnClick := BtnFilterMenu;
    if F=lfAll then
      FMenuFilterAll := M;
    FilterMenu.Items.Add(M);
  end;
  for O := low(O) to high(O) do
    ProfileGroup.Items.AddObject(
      GetCaptionFromEnum(TypeInfo(TLogProcSortOrder),Ord(O)),TObject(O));
  ProfileList.ColWidths[0] := 60;
  ProfileList.ColWidths[1] := 1000;
  ProfileList.Hide;
end;

procedure TMainLogView.FormShow(Sender: TObject);
var CmdLine: TFileName;
begin
  if ParamCount>0 then begin
    CmdLine := ParamStr(1);
    if SysUtils.DirectoryExists(CmdLine) then begin
      BtnBrowseClick(nil);
      Directory.Directory := CmdLine;
    end else
      LogFileName := CmdLine;
    end;
  WindowState := wsMaximized;
  EditSearch.SetFocus;
end;

procedure TMainLogView.BtnFilterClick(Sender: TObject);
var SenderBtn: TButton absolute Sender;
begin
  if Sender.InheritsFrom(TButton) then
    with ClientToScreen(SenderBtn.BoundsRect.TopLeft) do
      SenderBtn.PopupMenu.Popup(X,Y+SenderBtn.Height);
end;

procedure TMainLogView.BtnFilterMenu(Sender: TObject);
var F: TLogFilter;
    i: integer;
begin
  if not Sender.InheritsFrom(TMenuItem) then exit;
  F := TLogFilter(TMenuItem(Sender).Tag);
  for i := 0 to EventsList.Count-1 do
    EventsList.Checked[i] := TSynLogInfo(EventsList.Items.Objects[i]) in LOG_FILTER[F];
  EventsListClickCheck(nil);
end;

procedure TMainLogView.EventsListDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var E: TSynLogInfo;
begin
  if Index<0 then
    exit;
  E := TSynLogInfo(EventsList.Items.Objects[Index]);
  with EventsList.Canvas do begin
    Brush.Color := LOG_COLORS[false,E];
    Font.Color  := LOG_COLORS[true,E];
    TextRect(Rect,Rect.Left+4,Rect.Top,FEventCaption[E]);
  end;
end;

procedure TMainLogView.EventsListClickCheck(Sender: TObject);
var i, ndx: integer;
    Sets: TSynLogInfos;
    AllChecked: boolean;
begin
  FLogSelectedCount := 0;
  ndx := -1;
  if FLog=nil then
    List.RowCount := 0 else
  if FLog.EventLevel<>nil then begin
    ndx := List.Row;
    if ndx>=0 then
      ndx := FLogSelected[ndx];
    integer(Sets) := 0;
    AllChecked := true;
    for i := 0 to EventsList.Count-1 do
      if EventsList.Checked[i] then
        Include(Sets,TSynLogInfo(EventsList.Items.Objects[i])) else
        AllChecked := false;
    if AllChecked and (FLog.EventThread<>nil) then
      for i := 0 to length(FThreadSelected)-1 do
        if FThreadSelected[i]<>255 then 
          AllChecked := false;
    if AllChecked then begin
      FLogSelectedCount := FLog.Count;
      FillIncreasing(pointer(FlogSelected),0,FLogSelectedCount);
    end else
    if integer(Sets)=0 then
      ndx := -1 else begin
      for i := 0 to FLog.Count-1 do
      if FLog.EventLevel[i] in Sets then
        if (FLog.EventThread=nil) or
           GetBit(FThreadSelected[0],FLog.EventThread[i]-1) then begin
          FLogSelected[FLogSelectedCount] := i;
          inc(FLogSelectedCount);
        end;
      if ndx>=0 then
        ndx := IntegerScanIndex(pointer(FLogSelected),FLogSelectedCount,ndx);
    end;
    List.RowCount := FLogSelectedCount;
  end else
    List.RowCount := FLog.Count;
  SetListItem(ndx);
  if List.Visible then begin
    List.Repaint;
    ListClick(nil);
  end;
end;

procedure TMainLogView.EventsListDblClick(Sender: TObject);
var i: integer;
    E: TSynLogInfo;
begin
  if FLog.EventLevel=nil then // plain text file does not handle this
    exit;
  i := EventsList.ItemIndex;
  if i<0 then
    exit;
  E := TSynLogInfo(EventsList.Items.Objects[i]);
  // search from next item
  for i := List.Row+1 to FLogSelectedCount-1 do
    if FLog.EventLevel[FLogSelected[i]]=E then begin
      SetListItem(i);
      exit;
    end;
  // search from beginning
  for i := 0 to List.Row-1 do
    if FLog.EventLevel[FLogSelected[i]]=E then begin
      SetListItem(i);
      exit;
    end;
end;

procedure TMainLogView.ListDrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
var txt: string;
    b: boolean;
    Index: integer;
begin
  with List.Canvas do
    if FLog=nil then
      FillRect(Rect) else
    if FLog.EventLevel<>nil then begin
      Brush.Style := bsClear;
      if cardinal(ARow)<cardinal(FLogSelectedCount) then begin
        Index := FLogSelected[ARow];
        b := (gdFocused in State) or (gdSelected in State);
        if b then
          Brush.Color := clBlack else
          Brush.Color := LOG_COLORS[b,FLog.EventLevel[Index]];
        Font.Color  := LOG_COLORS[not b,FLog.EventLevel[Index]];
        FillRect(Rect);
        case ACol of
        0: txt := TimeToStr(FLog.EventDateTime(Index));
        1: txt := FEventCaption[FLog.EventLevel[Index]];
        2: if FLog.EventThread<>nil then
             txt := IntToString(cardinal(FLog.EventThread[Index])) else
             txt := UTF8ToString(StringReplaceAll(FLog.EventText[Index],#9,'   '));
        3: txt := UTF8ToString(StringReplaceAll(FLog.EventText[Index],#9,'   '));
        end;
        TextOut(Rect.Left+4,Rect.Top,txt);
      end else begin
        Brush.Color := clLtGray;
        FillRect(Rect);
      end;
    end else
      TextRect(Rect,Rect.Left+4,Rect.Top,FLog.Strings[ARow]);
end;

procedure TMainLogView.ListDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var s: RawUTF8;
    b: boolean;
begin
  with List.Canvas do 
    if FLog=nil then
      FillRect(Rect) else
    if FLog.EventLevel<>nil then begin
      Brush.Style := bsClear;
      if cardinal(Index)<cardinal(FLogSelectedCount) then begin
        Index := FLogSelected[Index];
        b := (odFocused in State) or (odSelected in State);
        Brush.Color := LOG_COLORS[b,FLog.EventLevel[Index]];
        Font.Color  := LOG_COLORS[not b,FLog.EventLevel[Index]];
        FillRect(Rect);
        s := FLog.Lines[Index];
        if Length(s)>400 then
          SetLength(s,400);
        s := StringReplaceAll(s,#9,'    ');
        TextOut(Rect.Left+4,Rect.Top,UTF8ToString(s));
      end else begin
        Brush.Color := clLtGray;
        FillRect(Rect);
      end;
    end else
      TextRect(Rect,Rect.Left+4,Rect.Top,FLog.Strings[Index]);
end;

procedure TMainLogView.BtnSearchNextClick(Sender: TObject);
var s: RawUTF8;
    ndx, i, searchnext: integer;
begin
  s := UpperCase(StringToUTF8(EditSearch.Text));
  if (FLog=nil) or (s='') then
    exit;
  if (FLastSearchSender=Sender) and (FLastSearch=s) then
    searchnext := 1 else begin
    FLastSearch := s;
    FLastSearchSender := Sender;
    searchnext := 0;
  end;
  Screen.Cursor := crHourGlass;
  try
    ndx := List.Row;
    if FLog.EventLevel=nil then begin // plain text search
      // search from next item
      for i := ndx+searchnext to FLog.Count-1 do
        if FLog.LineContains(s,i) then begin
          SetListItem(i,s);
          exit;
        end;
      // not found -> search from beginning
      for i := 0 to ndx-1 do
        if FLog.LineContains(s,i) then begin
          SetListItem(i,s);
          exit;
        end;
    end else begin
      // search from next item
      for i := ndx+searchnext to FLogSelectedCount-1 do
        if FLog.LineContains(s,FLogSelected[i]) then begin
          SetListItem(i,s);
          exit;
        end;
      // not found -> search from beginning
      for i := 0 to ndx-1 do
        if FLog.LineContains(s,FLogSelected[i]) then begin
          SetListItem(i,s);
          exit;
        end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainLogView.BtnSearchPreviousClick(Sender: TObject);
var s: RawUTF8;
    ndx, i, searchnext: integer;
begin
  s := UpperCase(StringToUTF8(EditSearch.Text));
  if s='' then
    exit;
  if (FLastSearchSender=Sender) and (FLastSearch=s) then
    searchnext := 1 else begin
    FLastSearch := s;
    FLastSearchSender := Sender;
    searchnext := 0;
  end;
  Screen.Cursor := crHourGlass;
  try
    ndx := List.Row;
    if FLog.EventLevel=nil then begin // plain text search
      // search from previous item
      for i := ndx-searchnext downto 0 do
        if FLog.LineContains(s,i) then begin
          SetListItem(i,s);
          exit;
        end;
      // not found -> search from end
      for i := FLog.Count-1 downto ndx+1 do
        if FLog.LineContains(s,i) then begin
          SetListItem(i,s);
          exit;
        end;
    end else begin
      // search from previous item
      for i := ndx-searchnext downto 0 do
        if FLog.LineContains(s,FLogSelected[i]) then begin
          SetListItem(i,s);
          exit;
        end;
      // not found -> search from end
      for i := FLogSelectedCount-1 downto ndx+1 do
        if FLog.LineContains(s,FLogSelected[i]) then begin
          SetListItem(i,s);
          exit;
        end;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;


procedure TMainLogView.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_F3 then begin
    if Shift=[] then
      BtnSearchNextClick(nil) else
    if ssShift in Shift then
      BtnSearchPreviousClick(nil) else
      exit;
    List.SetFocus;
  end else
  if (Shift=[ssCtrl]) and (Key=ord('F')) then begin
    EditSearch.SetFocus;
  end;
end;

procedure TMainLogView.ProfileListClick(Sender: TObject);
var ndx,i: integer;
begin
  i := ProfileList.Row;
  if (FLog<>Nil) and (cardinal(i)<=cardinal(FLog.LogProcCount)) then begin
    ndx := FLog.LogProc[i].Index;
    i := IntegerScanIndex(pointer(FLogSelected),FLogSelectedCount,ndx);
    if i>=0 then begin
      SetListItem(i);
      List.Row := i;
      List.SetFocus;
    end;
  end;
end;

procedure TMainLogView.ProfileListDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
type TProfileListCol = (colTime, colName);
var Tim: integer;
    s: string;
begin
  if (FLog<>Nil) and (cardinal(ARow)<cardinal(FLog.LogProcCount)) then
  with FLog.LogProc[ARow] do begin
    case TProfileListCol(ACol) of
    colTime: begin
      if FLog.LogProcOrder=soByProperTime then
        Tim := ProperTime else
        Tim := Time;
      s := Ansi7ToString(MicroSecToString(Tim));
    end;
    colName:
      s := UTF8ToString(trim(FLog.EventText[Index]));
    end;
    ProfileList.Canvas.TextRect(Rect,Rect.Left+4,Rect.Top,s);
  end;
end;

procedure TMainLogView.ListClick(Sender: TObject);
var i: integer;
begin
  i := List.Row;
  if cardinal(i)>=cardinal(FLogSelectedCount) then
    if FLog<>nil then
      MemoBottom.Text := FLog.Strings[i] else
      MemoBottom.Text := '' else begin
    MemoBottom.Text := FLog.Strings[FLogSelected[i]];
    if FLog.EventThread<>nil then
      ThreadListBox.ItemIndex := FLog.EventThread[FLogSelected[i]]-1;
  end;
end;

procedure TMainLogView.ListDblClick(Sender: TObject);
var i, Level: integer;
begin
  i := List.Row;
  if (FLog<>Nil) and (cardinal(i)<=cardinal(FLogSelectedCount)) then begin
    Level := 0;
    case FLog.EventLevel[FLogSelected[i]] of
      sllEnter: // retrieve corresponding Leave event
        repeat
          inc(i);
          if i>=FLogSelectedCount then
            exit;
          case FLog.EventLevel[FLogSelected[i]] of
          sllEnter: Inc(Level);
          sllLeave: if Level=0 then begin
            SetListItem(i);
            exit;
          end else Dec(Level);
          end;
        until false;
      sllLeave: // retrieve corresponding Enter event
        repeat
          dec(i);
          if i<0 then
            exit;
          case FLog.EventLevel[FLogSelected[i]] of
          sllLeave: Inc(Level);
          sllEnter: if Level=0 then begin
            SetListItem(i);
            exit;
          end else Dec(Level);
          end;
        until false;
    end;
  end;
end;

procedure TMainLogView.BtnThreadNextClick(Sender: TObject);
var i: integer;
    ID: word;
begin
  i := List.Row;
  if (FLog=nil) or (FLog.EventThread=nil) or
     (cardinal(i)>=cardinal(FLogSelectedCount)) then
    exit;
  ID := FLog.EventThread[FLogSelected[i]];
  repeat
    inc(i);
    if i=FLogSelectedCount then
      exit;
  until FLog.EventThread[FLogSelected[i]]<>ID;
  SetListItem(i);
end;

procedure TMainLogView.ThreadListBoxDblClick(Sender: TObject);
var ID: cardinal;
    i: integer;
begin
  ID := ThreadListBox.ItemIndex;
  if ID>=FLog.ThreadsCount then
    exit;
  inc(ID);
  for i := List.Row+1 to FLogSelectedCount-1 do
    if FLog.EventThread[FLogSelected[i]]=ID then begin
      SetListItem(i);
      exit;
    end;
  for i := 0 to List.Row-1 do
    if FLog.EventThread[FLogSelected[i]]=ID then begin
      SetListItem(i);
      exit;
    end;
end;

procedure TMainLogView.BtnThreadShowClick(Sender: TObject);
begin
  ThreadListBox.Visible := not ThreadListBox.Visible;
  Splitter3.Visible := ThreadListBox.Visible;
  if ThreadListBox.Visible then begin
    ThreadListBox.Left := ProfileList.Left+ProfileList.Width;
    Splitter3.Left := ThreadListBox.Left+ThreadListBox.Width;
  end;
end;

resourcestring
  sEnterAddress = 'Enter an hexadecimal address:';
  sStats = #13#10'Log'#13#10'---'#13#10#13#10'Name: %s'#13#10'Size: %s'#13#10#13#10+
    'Executable'#13#10'----------'#13#10#13#10'Name: %s%s'#13#10'Version: %s'#13#10+
    'Date: %s'#13#10#13#10'Host'#13#10'----'#13#10#13#10'Computer: %s'#13#10+
    'User: %s'#13#10'CPU: %s'#13#10'OS: Windows %s (service pack %d)'#13#10+
    'Wow64: %d'#13#10#13#10'Log content'#13#10'-----------'#13#10#13#10+
    'Log started at: %s'#13#10'Events count: %d'#13#10'Methods count: %d'#13#10+
    'Threads count: %d'#13#10'Time elapsed: %s'#13#10#13#10+
    'Per event stats'#13#10'---------------'#13#10#13#10;

procedure TMainLogView.BtnStatsClick(Sender: TObject);
var M: TMemo;
    F: TForm;
    s: string;
    sets: array[TSynLogInfo] of integer;
    i: integer;
    P: PUTF8Char;
    line,name,value: RawUTF8;
begin
  F := TForm.Create(Application);
  try
    F.Caption := FMainCaption+BtnStats.Caption;
    if Screen.Fonts.IndexOf('Consolas')>=0 then
      F.Font.Name := 'Consolas' else
      F.Font.Name := 'Courier New';
    F.Position := poScreenCenter;
    F.Width := 700;
    F.Height := 600;
    M := TMemo.Create(F);
    M.Parent := F;
    M.Align := alClient;
    M.ScrollBars := ssVertical;
    M.WordWrap := true;
    M.ReadOnly := true;
    if (FLog<>nil) and (FLog.EventLevel<>nil) then
      with FLog do begin
        if InstanceName<>'' then
          s := ' / '+UTF8ToString(InstanceName);
        s := format(sStats,
          [FileName,Ansi7ToString(KB(Map.Size)),
           UTF8ToString(ExecutableName),s,Ansi7ToString(ExecutableVersion),
           DateTimeToStr(ExecutableDate),UTF8ToString(ComputerHost),
           UTF8ToString(RunningUser),Ansi7ToString(CPU),
           GetCaptionFromEnum(TypeInfo(TWindowsVersion),ord(OS)),ServicePack,
           Integer(Wow64),DateTimeToStr(StartDateTime),Count,LogProcCount,
           ThreadsCount,FormatDateTime('hh:mm:ss',EventDateTime(Count-1)-StartDateTime)]);
        fillchar(sets,sizeof(sets),0);
        for i := 0 to Count-1 do
          inc(sets[EventLevel[i]]);
        for i := 0 to EventsList.Count-1 do
          s := s+EventsList.Items[i]+': '+
            IntToStr(sets[TSynLogInfo(EventsList.Items.Objects[i])])+#13#10;
        P := pointer(Headers);
        while (P<>nil) and (P^<>#0) do begin
          line := GetNextLine(P,P);
          Split(line,'=',name,value);
          s := s+#13#10+UTF8ToString(name)+#13#10+StringOfChar('-',length(name))+
            #13#10#13#10+UTF8ToString(StringReplaceAll(value,#9,#13#10))+#13#10;
        end;
      end;
    M.Text := s;
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TMainLogView.BtnOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    LogFileName := OpenDialog.FileName;
end;

procedure TMainLogView.SetListItem(Index: integer; const search: RawUTF8='');
var i: integer;
    s,ss: string;
begin
  if (Index<0) or (FLog=nil) then
    MemoBottom.Text := '' else begin
    List.Row := Index;
    if (search='') and List.Visible then
      List.SetFocus;
    if FLog.EventLevel=nil then
      s := FLog.Strings[Index] else
      s := FLog.Strings[FLogSelected[Index]];
    MemoBottom.Text := s;
    if search<>'' then begin
      ss := UTF8ToString(search);
      i := Pos(ss,SysUtils.UpperCase(s));
      if i>0 then begin
        MemoBottom.SelStart := i-1;
        MemoBottom.SelLength := length(ss);
      end;
    end;
  end;
end;

procedure TMainLogView.BtnMapSearchClick(Sender: TObject);
var FN: TFileName;
    Addr: string;
    AddrInt, err: integer;
    Loc: RawUTF8;
    Map: TSynMapFile;
begin
  if FLog<>nil then begin
    FN := ChangeFileExt(ExtractFileName(UTF8ToString(FLog.ExecutableName)),'.map');
    FN := FN+';'+ChangeFileExt(FN,'.mab');
  end else
    FN := '*.map;*.mab';
  with TOpenDialog.Create(Application) do
  try
    DefaultExt := '.map';
    Filter := FN+'|'+FN;
    Options := [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing];
    if not Execute then
      exit;
    Map := TSynMapFile.Create(FileName);
    try
      repeat
        if not InputQuery(BtnMapSearch.Hint,sEnterAddress,Addr) then
          Exit;
        Addr := SysUtils.Trim(Addr);
        if Addr='' then continue;
        if Addr[1]<>'$' then
          Addr := '$'+Addr;
        val(Addr,AddrInt,err);
        if err<>0 then
          continue;
        Loc := Map.FindLocation(AddrInt);
        if Loc<>'' then
          ShowMessage(Addr+#13#10+UTF8ToString(Loc));
      until false;
    finally
      Map.Free;
    end;
  finally
    Free;
  end;
end;

procedure TMainLogView.MergedProfileClick(Sender: TObject);
begin
  if FLog=nil then
    Exit;
  Screen.Cursor := crHourGlass;
  try
    FLog.LogProcMerged := MergedProfile.Checked;
    ProfileGroupClick(nil);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainLogView.ProfileGroupClick(Sender: TObject);
var O: TLogProcSortOrder;
begin
  O := TLogProcSortOrder(ProfileGroup.ItemIndex);
  if O<low(O) then
    O := low(O);
  if (FLog=nil) or (O=soNone) then begin
    Splitter1.Hide;
    ProfileList.Hide;
  end else begin
    Screen.Cursor := crHourGlass;
    FLog.LogProcSort(O);
    Screen.Cursor := crDefault;
    ProfileList.RowCount := FLog.LogProcCount;
    ProfileList.Show;
    Splitter1.Left := ProfileList.Left+ProfileList.Width;
    Splitter1.Show;
    ProfileList.Repaint;
  end;
end;

procedure TMainLogView.ImageLogoClick(Sender: TObject);
begin
{$WARNINGS OFF}
  if DebugHook=0 then
    ShellExecute(0,'open','http://synopse.info',nil,nil,SW_SHOWNORMAL);
{$WARNINGS ON}
end;

procedure TMainLogView.ThreadListMenuClick(Sender: TObject);
begin
  fillchar(FThreadSelected[0],Length(FThreadSelected),byte((Sender=ThreadListMenuAll))*255);
  ThreadListCheckRefresh;
  EventsListClickCheck(nil);
end;

procedure TMainLogView.ThreadListCheckRefresh;
var i: integer;
begin
  for i := 0 to ThreadListBox.Count-1 do
    ThreadListBox.Checked[i] := GetBit(FThreadSelected[0],i);
end;

procedure TMainLogView.ThreadListBoxClick(Sender: TObject);
var i: integer;
begin
  i := ThreadListBox.ItemIndex;
  if i>=0 then begin
    if ThreadListBox.Checked[i] then
      SetBit(FThreadSelected[0],i) else
      UnSetBit(FThreadSelected[0],i);
    EventsListClickCheck(nil);
  end;
end;

procedure TMainLogView.BtnBrowseClick(Sender: TObject);
begin
  PanelBrowse.Visible := (Sender=nil) or not PanelBrowse.Visible;
  Splitter4.Visible := PanelBrowse.Visible;
  if PanelBrowse.Visible then
    Splitter4.Left := PanelBrowse.Left+PanelBrowse.Width;
end;

procedure TMainLogView.FilesClick(Sender: TObject);
begin
  if (FLog=nil) or (FLog.FileName<>Files.FileName) then
    LogFileName := Files.FileName;
end;

procedure TMainLogView.ListMenuCopyClick(Sender: TObject);
var s: string;
    Selection: TGridRect;
    i: integer;
begin
  Selection := List.Selection;
  for i := Selection.Top to Selection.Bottom do begin
    s := s+TimeToStr(FLog.EventDateTime(i))+#9+FEventCaption[FLog.EventLevel[i]]+#9;
    if FLog.EventThread<>nil then
      s := s+IntToString(cardinal(FLog.EventThread[i]))+#9;
    s := s+UTF8ToString(StringReplaceAll(FLog.EventText[i],#9,'   '))+sLineBreak;
  end;
  Clipboard.AsText := s;
end;

end.

