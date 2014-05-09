unit TestMustacheUnit;

interface

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  SynCrtSock,
  {$ifndef CPU64} // SpiderMonkey library is not available yet in 64 bit
  SynSM, SynSMAPI,
  {$endif}
  SynCommons, SynMustache;


type
  TMainForm = class(TForm)
    mmoTemplate: TMemo;
    lblTemplate: TLabel;
    mmoContext: TMemo;
    lblContext: TLabel;
    btnExecSynMustache: TButton;
    btnExecSpiderMonkey: TButton;
    mmoResult: TMemo;
    btnOpenBrowser: TButton;
    lblIteration: TLabel;
    edtIteration: TEdit;
    procedure Render(Sender: TObject);
    procedure btnOpenBrowserClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    {$ifndef CPU64} // SpiderMonkey library is not available yet in 64 bit
    fEngineManager: TSMEngineManager;
    fEngine: TSMEngine;
    {$endif}
  end;

var
  MainForm: TMainForm;

implementation

uses
  ShellAPI;

{$R *.dfm}

{$R Vista.res}

procedure TMainForm.Render(Sender: TObject);
var Template, Context, Result: RawUTF8;
    data: variant;
    i,n: Integer;
    Timer: TPrecisionTimer;
    SynMustacheTemplate: TSynMustache;
    {$ifndef CPU64} // SpiderMonkey library is not available yet in 64 bit
    person: RawUTF8;
    partial: variant;
    {$endif}
begin
  Template := StringToUTF8(mmoTemplate.Lines.Text);
  Context := StringToUTF8(mmoContext.Lines.Text);
  data := _JsonFast(Context);
  n := StrToIntDef(edtIteration.Text,1000);
  if Sender=btnExecSynMustache then begin
    SynMustacheTemplate := TSynMustache.Parse(Template);
    Timer.Start;
    for i := 1 to n do
      result := SynMustacheTemplate.Render(data);
  end else
  {$ifndef CPU64} // SpiderMonkey library is not available yet in 64 bit
  if Sender=btnExecSpiderMonkey then begin
    fEngine.MaybeGarbageCollect;
    i := PosEx('{{<person}}',Template);
    if i>0 then begin // extract any inlined partial (should be at the end)
      person := Copy(Template,i+12,maxInt);
      SetLength(Template,i-1);
      i := PosEx('{{/person}}',person);
      if i>0 then
        SetLength(person,i-1);
      partial := _ObjFast(['person',person]);
    end;
    Timer.Start;
    for i := 1 to n do
      result := VariantToUTF8(fEngine.Global.Mustache.render(Template, data, partial));
  end else
  {$endif}
    exit;
  mmoResult.Lines.Text :=
    Format('Rendered %d times in %s (%d/sec):'#13#10#13#10'%s',
      [n,Timer.Stop,Timer.PerSec(n),result]);
  FileFromString(Result,ChangeFileExt(paramstr(0),'.html'));
end;

procedure TMainForm.btnOpenBrowserClick(Sender: TObject);
begin
  ShellExecute(0,'open',Pointer(ChangeFileExt(paramstr(0),'.html')),nil,nil,SW_SHOWNORMAL);
end;

procedure TMainForm.FormShow(Sender: TObject);
{$ifdef CPU64} // SpiderMonkey library is not available yet in 64 bit
begin
  btnExecSpiderMonkey.Hide;
end;
{$else}
var mustacheFN: TFileName;
    mSource: SynUnicode;
    mustache: RawByteString;
    i: integer;
begin
  fEngineManager := TSMEngineManager.Create;
  fEngine := fEngineManager.ThreadSafeEngine;
  mustacheFN := ExtractFilePath(ParamStr(0)) + 'js\mustache.js';
  mSource := AnyTextFileToSynUnicode(mustacheFN);
  if mSource='' then begin
    mustache := TWinINet.Get('https://github.com/janl/mustache.js/raw/master/mustache.js');
    if PosEx('return send(result);',mustache)=0 then begin
      i := PosEx('send(result);',mustache);
      if i>0 then
        insert('return ',mustache,i); // fix syntax error in official libary! :)
    end;
    FileFromString(mustache,mustacheFN);
    mSource := SynUnicode(mustache);
  end;
  fEngine.Evaluate(mSource,'mustache.js');
end;
{$endif}

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  {$ifndef CPU64} // SpiderMonkey library is not available yet in 64 bit
  fEngineManager.Free;
  {$endif}
end;

end.
