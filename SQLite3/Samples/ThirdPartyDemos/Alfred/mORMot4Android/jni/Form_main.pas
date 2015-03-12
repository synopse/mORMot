Unit Form_Main;

{$mode delphi}

interface

uses
  SysUtils,Classes,math,
  And_jni,And_jni_Bridge,And_Controls_Types,And_Controls,
  Form_Splash
  ,SynCommons
  ,mORMot
  ,mORMotSQLite3, SynSQLite3Static
  ,SampleData
  ;

Type
 jForm_Main = class(jForm)
 private
  ivImageBG    : jImageView; // Image - BackGround
  lblTitle     : jTextView;
  lblName      : jTextView;
  lblMessage   : jTextView;
  AddBtn       : jButton;
  FindBtn      : jButton;
  NameInput    : jEditText;
  MessageInput : jEditText;
 private
  Database: TSQLRest;
  Model: TSQLModel;
 protected
  Procedure Form_OnActive    (Sender : TObject);
  Procedure Form_OnCloseQuery(Sender : TObject; Var CanClose : Boolean);
  Procedure Form_OnClose     (Sender : TObject);
  //
  Procedure AddBtn_OnClick (Sender : TObject);
  Procedure FindBtn_OnClick (Sender : TObject);
 public
  Form_Splash : jForm;
  Constructor Create(Owner : jForm) ; override;
 end;

implementation

//
Constructor jForm_Main.Create(Owner : jForm) ;
 begin
  inherited;

  Dbg('MainForm.OnCreate');
  Self.Color := clBlack;

  // Resource Extract
  Asset_SaveToFile('mormotbw.jpg'  ,App.Paths.Dat+'/mormotbw.jpg');
  // Create Controls ----------------------------------------------------------
  ivImageBG            := jImageView.Create(Self);
  ivImageBG.xyWH       := xyWH( 0,0,App.Device.ScreenWH.width,App.Device.ScreenWH.height); // x,y,w,h
  ivImageBG.Parent     := Self.View;
  ivImageBG.ImageName  := App.Paths.Dat+'/mormotbw.jpg';

  lblTitle             := jTextView.Create (Self);
  lblTitle.xyWH        := xyWH(30,10,600,80,1.4);
  lblTitle.Parent      := Self.View;
  lblTitle.Text        := 'mORMot for Android';
  lblTitle.FontSize    := 60;
  lblTitle.FontColor   := clBlue;
  lblTitle.Enabled     := True;
  //
  lblName            := jTextView.Create (Self);
  lblName.xyWH       := xyWH(30,90,200,40,1.2);
  lblName.Parent     := Self.View;
  lblName.Text       := 'Your name' ;
  lblName.FontColor  := clBlack;
  lblName.FontSize   := 30;

  NameInput                := jEditText.Create (Self);
  NameInput.xyWH           := xyWH( 30,150,350, 80);
  NameInput.Parent         := Self.View;
  NameInput.FontColor      := clBlack;               // ARGB , Black
  NameInput.FontSize       := 40;                    // Pixel
  NameInput.EditType       := Edit_Type_Text;        //
  //NameInput.OnEnter        := edInput_OnEnter;
  //NameInput.OnChange       := edInput_OnChange;
  NameInput.Text           := utf8encode('Name');

  FindBtn              := jButton.Create(Self);
  FindBtn.xyWH         := xyWH( 400,150,220,80); // x,y,w,h
  FindBtn.Parent       := Self.View;
  FindBtn.Text         := 'Find';
  FindBtn.FontColor    := clBlack;
  FindBtn.FontSize     := 40;
  FindBtn.OnClick      := FindBtn_OnClick;

  lblMessage            := jTextView.Create (Self);
  lblMessage.xyWH       := xyWH(30,200,200,40,1.2);
  lblMessage.Parent     := Self.View;
  lblMessage.Text       := 'Your message' ;
  lblMessage.FontColor  := clBlack;
  lblMessage.FontSize   := 30;

  MessageInput                := jEditText.Create(Self);
  MessageInput.xyWH           := xyWH( 30, 280, 600, 400);
  MessageInput.Parent         := Self.View;
  MessageInput.FontColor      := clBlack;               // ARGB , Black
  MessageInput.FontSize       := 40;                    // Pixel
  MessageInput.EditType       := Edit_Type_Text_Multi;  //
  //MessageInput.OnEnter        := edInput_OnEnter;
  //MessageInput.OnChange       := edInput_OnChange;
  MessageInput.Text           := utf8encode('Message');

  AddBtn              := jButton.Create(Self);
  AddBtn.xyWH         := xyWH(30,700,600,80); // x,y,w,h
  AddBtn.Parent       := Self.View;
  AddBtn.Text         := 'Add the message';
  AddBtn.OnClick      := AddBtn_OnClick;
  FindBtn.FontSize    := 40;

  Self.Name            := 'Form_Main';
  Self.OnActive        := Form_OnActive;
  Self.OnCloseQuery    := Form_OnCloseQuery;
  Self.OnClose         := Form_OnClose;
  //
  Model := CreateSampleModel; // from SampleData unit
  Database := TSQLRestServerDB.Create(Model,'/storage/emulated/0/Download/mORMot.db3');
  TSQLRestServerDB(Database).CreateMissingTables;

  Form_Splash          := jForm_Splash.Create(Self);
 end;

Procedure jForm_Main.Form_OnActive(Sender : TObject);
begin
  Dbg('OnActive Event');
  Form_Splash.Show;
end;

Procedure jForm_Main.Form_OnCloseQuery(Sender : TObject; Var CanClose : Boolean);
begin
  Dbg('CanClose:Main');
  CanClose := True;
  if Assigned(Form_Splash.View) then Form_Splash.Visible := False;
 end;

Procedure jForm_Main.Form_OnClose(Sender : TObject);
begin
  Dbg('OnClose Event:Main');
  Database.Free;
  Model.Free;
  Self.Free;
  App.Finish;
end;

Procedure jForm_Main.AddBtn_OnClick (Sender : TObject);
var Rec: TSQLSampleRecord;
begin
  Rec := TSQLSampleRecord.Create;
  try
    // we use explicit StringToUTF8() for conversion below
    // a real application should use TLanguageFile.StringToUTF8() in mORMoti18n
    Rec.Name := StringToUTF8(NameInput.Text);
    Rec.Question := StringToUTF8(MessageInput.Text);
    if Database.Add(Rec,true)=0 then
      ShowMessage('Error adding the data') else begin
      NameInput.Text := '';
      MessageInput.Text := '';
      NameInput.SetFocus;
    end;
  finally
    Rec.Free;
  end;
end;

Procedure jForm_Main.FindBtn_OnClick (Sender : TObject);
var Rec: TSQLSampleRecord;
begin
  Rec := TSQLSampleRecord.Create(Database,'Name=?',[StringToUTF8(NameInput.Text)]);
  try
    if Rec.ID=0 then
      MessageInput.Text := 'Not found' else
      MessageInput.Text := UTF8ToString(Rec.Question);
  finally
    Rec.Free;
  end;
end;


end.
