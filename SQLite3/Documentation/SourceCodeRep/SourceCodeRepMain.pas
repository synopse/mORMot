unit SourceCodeRepMain;

interface

uses
  SynCommons,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Clipbrd;

const
  VERSION = '1.18';

type
  TMainForm = class(TForm)
    mmoStatus: TMemo;
    lbl1: TLabel;
    lbl2: TLabel;
    mmoDescription: TMemo;
    btnFossilSynch: TButton;
    btnFullSynch: TButton;
    btnGitSynch: TButton;
    btnRefreshStatus: TButton;
    btnGitShell: TButton;
    btnFossilShell: TButton;
    btnTests: TButton;
    btnCopyLink: TButton;
    btnGitAll: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnFullSynchClick(Sender: TObject);
    procedure btnFossilSynchClick(Sender: TObject);
    procedure btnGitSynchClick(Sender: TObject);
    procedure btnRefreshStatusClick(Sender: TObject);
    procedure btnGitShellClick(Sender: TObject);
    procedure btnFossilShellClick(Sender: TObject);
    procedure btnTestsClick(Sender: TObject);
    procedure btnCopyLinkClick(Sender: TObject);
  private
    fBatPath: TFileName;
    fFossilRepository: TFileName;
    fDevPath: TFileName;
    fGitExe: TFileName;
    fGitRepository: TFileName;
    procedure ReadStatus;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

{$R Vista.res}

function WinExecAndWait32(const Command,CurrentDir: TFileName;
  Visibility: Word; Timeout : DWORD): integer;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  FillChar(StartupInfo,SizeOf(TStartupInfo),0);
  with StartupInfo do begin
    cb := SizeOf(TStartupInfo);
    dwFlags := STARTF_USESHOWWINDOW or STARTF_FORCEONFEEDBACK;
    wShowWindow := visibility;
  end;
  if CreateProcess(nil,pointer(Command),nil, nil, False, NORMAL_PRIORITY_CLASS,
      nil, pointer(CurrentDir), StartupInfo, ProcessInfo) then
    { timeout is in miliseconds or INFINITE if you want to wait forever }
    result := Integer(WaitForSingleObject(ProcessInfo.hProcess, timeout)) else
    result := GetLastError;
end;

procedure TMainForm.ReadStatus;
begin
  WinExecAndWait32(fBatPath+'FossilStatus.bat "'+fBatPath+'status.txt"',fFossilRepository,SW_HIDE,10000);
  mmoStatus.Text := StringFromFile(fBatPath+'status.txt');
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  fBatPath := ExtractFilePath(paramstr(0));
  if not FileExists(fBatPath+'FossilStatus.bat') then // from exe sub-folder?
    fBatPath := ExtractFilePath(ExcludeTrailingPathDelimiter(fBatPath));
  if not FileExists(fBatPath+'FossilStatus.bat') then
    ShowMessage('Missing .bat files');
  fDevPath := 'd:\dev\lib';
  fFossilRepository := 'c:\progs\fossil\lib';
  fGitExe := 'c:\Program Files (x86)\Git\bin\git.exe';
  fGitRepository := 'd:\dev\github\mORMot';
  if not DirectoryExists(fFossilRepository) then begin
    ShowMessage('Please set Fossil Repository Name');
    Close;
  end else
    ReadStatus;
end;

procedure TMainForm.btnFullSynchClick(Sender: TObject);
begin
  btnFossilSynch.Click;
  btnGitSynch.Click;
end;

procedure TMainForm.btnFossilSynchClick(Sender: TObject);
var Desc: string;
    DescFile: TFileName;
    VersionNumber: integer;
    VersionText: RawUTF8;
begin
  Desc := trim(mmoDescription.Text);
  if Desc='' then begin
    ShowMessage('Missing description');
    mmoDescription.SetFocus;
    exit;
  end;
  VersionText := UnQuoteSQLString(StringFromFile(fDevPath+'\SynopseCommit.inc'));
  VersionText := GetCSVItem(pointer(VersionText),2,'.');
  VersionNumber := GetCardinalDef(pointer(VersionText),255);
  inc(VersionNumber);
  VersionText := ''''+VERSION+'.'+UInt32ToUtf8(VersionNumber)+'''';
  FileFromString(VersionText,fDevPath+'\SynopseCommit.inc');
  FileFromString(VersionText,fFossilRepository+'\SynopseCommit.inc');
  DescFile := fBatPath+'desc.txt';
  FileFromString('{'+IntToStr(VersionNumber)+'} '+Desc,DescFile);
  WinExecAndWait32(fBatPath+'FossilCommit.bat "'+DescFile+'"',fFossilRepository,SW_SHOWNORMAL,INFINITE);
  ReadStatus;
end;

procedure TMainForm.btnGitSynchClick(Sender: TObject);
var Desc,status: string;
    DescFile, BatchFile: TFileName;
    i: integer;
begin
  Desc := trim(mmoDescription.Text);
  if Desc='' then begin
    status := mmoStatus.Text;
    i := pos('comment:',status);
    if i>0 then begin
      delete(status,1,i+8);
      with TStringList.Create do
      try
        Text := trim(status);
        status := Strings[0];
        for i := 1 to Count-1 do
          if copy(Strings[i],1,3)='   ' then
            status := status+' '+trim(Strings[i]) else
            break;
      finally
        Free;
      end;
      i := pos('(user: ',status);
      if i>0 then
        SetLength(status,i-1);
      i := pos('} ',status);
      if (i>0) and (i<10) then
        delete(status,1,i+1); // trim left '{256} '
      mmoDescription.Text := trim(status);
    end else begin
      ShowMessage('Missing description');
      mmoDescription.SetFocus;
    end;
    exit;
  end;
  if not FileExists(fGitExe) then begin
    ShowMessage('git.exe not found');
    exit;
  end;
  if not DirectoryExists(fGitRepository) then begin
    ShowMessage('Please set Git Repository Name');
    exit;
  end;
  DescFile := fBatPath+'desc.txt';
  FileFromString(Desc,DescFile);
  if Sender=btnGitAll then
    BatchFile := 'GitCommitAll.bat' else
    BatchFile := 'GitCommit.bat';
  WinExecAndWait32(format('%s%s "%s" "%s" "%s" "%s"',
      [fBatPath,BatchFile,fFossilRepository,fGitRepository,fGitExe,DescFile]),
     fGitRepository,SW_SHOWNORMAL,INFINITE);
end;

procedure TMainForm.btnRefreshStatusClick(Sender: TObject);
begin
  ReadStatus;
  mmoDescription.SetFocus;
  mmoDescription.SelectAll;
end;

procedure TMainForm.btnGitShellClick(Sender: TObject);
begin
  WinExecAndWait32(format('%sGitShell.bat  "%s"',[fBatPath,ExtractFilePath(fGitExe)]),
    fGitRepository,SW_SHOWNORMAL,INFINITE);
end;

procedure TMainForm.btnFossilShellClick(Sender: TObject);
begin
  WinExecAndWait32('cmd.exe',fFossilRepository,SW_SHOWNORMAL,INFINITE);
end;

procedure TMainForm.btnTestsClick(Sender: TObject);
begin
  WinExecAndWait32(fDevPath+'\compilpil.bat',fDevPath,SW_SHOWNORMAL,INFINITE);
end;

procedure TMainForm.btnCopyLinkClick(Sender: TObject);
var i: integer;
    status: string;
begin
  status := mmoStatus.Lines.Text;
  i := pos('checkout:',status);
  if i<0 then
    exit;
  inc(i,10);
  while (i<length(status)) and (status[i]<=' ') do inc(i);
  Clipboard.AsText := 'http://synopse.info/fossil/info/'+copy(status,i,10);
end;


end.
