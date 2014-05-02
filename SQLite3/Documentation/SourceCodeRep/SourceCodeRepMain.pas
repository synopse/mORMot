unit SourceCodeRepMain;

interface

uses
  SynCommons,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

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
    procedure FormCreate(Sender: TObject);
    procedure btnFullSynchClick(Sender: TObject);
    procedure btnFossilSynchClick(Sender: TObject);
    procedure btnGitSynchClick(Sender: TObject);
    procedure btnRefreshStatusClick(Sender: TObject);
    procedure btnGitShellClick(Sender: TObject);
    procedure btnFossilShellClick(Sender: TObject);
    procedure btnTestsClick(Sender: TObject);
  private
    fBatPath: TFileName;
    fFossilRepository: TFileName;
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
  FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
  with StartupInfo do
  begin
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
  if not FileExists(fBatPath+'FossilStatus.bat') then
    fBatPath := ExtractFilePath(ExcludeTrailingPathDelimiter(fBatPath));
  if not FileExists(fBatPath+'FossilStatus.bat') then
    ShowMessage('Missing .bat files');
  fFossilRepository := 'c:\progs\fossil\lib';
  fGitExe := 'C:\Users\User\AppData\Local\GitHub\PortableGit_054f2e797ebafd44a30203088cd3d58663c627ef\bin\git.exe';
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
begin
  Desc := trim(mmoDescription.Text);
  if Desc='' then begin
    ShowMessage('Missing description');
    mmoDescription.SetFocus;
    exit;
  end;
  DescFile := fBatPath+'desc.txt';
  FileFromString(Desc,DescFile);
  WinExecAndWait32(fBatPath+'FossilCommit.bat "'+DescFile+'"',fFossilRepository,SW_SHOWNORMAL,INFINITE);
  ReadStatus;
end;

procedure TMainForm.btnGitSynchClick(Sender: TObject);
var Desc,status: string;
    DescFile: TFileName;
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
  WinExecAndWait32(format('%sGitCommit.bat "%s" "%s" "%s" "%s"',
      [fBatPath,fFossilRepository,fGitRepository,fGitExe,DescFile]),
     fGitRepository,SW_SHOWNORMAL,INFINITE);
end;

procedure TMainForm.btnRefreshStatusClick(Sender: TObject);
begin
  ReadStatus;
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
  WinExecAndWait32('d:\dev\lib\compilpil.bat','d:\dev\lib',SW_SHOWNORMAL,INFINITE);
end;

end.
