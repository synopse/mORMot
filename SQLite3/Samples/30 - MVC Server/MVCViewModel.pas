/// ViewModel/Control interfaces for the MVCServer BLOG sample
unit MVCViewModel;

interface

uses
  SysUtils,
  Contnrs,
  Variants,
  SynCommons,
  mORMot,
  mORMotMVC,
  MVCModel;

type
  /// defines the main ViewModel/Controller commands of the BLOG web site
  IBlogApplication = interface(IMVCApplication)
    /// blog/main/article?id=12 -> view one article
    // - here article details are returned as out parameters values
    procedure ArticleView(
      ID: integer; WithComments: boolean;
      out Article: TSQLArticle; out Author: TSQLAuthor;
      out Comments: TObjectList);
    /// blog/main/login?name=...&plainpassword=... -> log as author
    function Login(
      const LogonName,PlainPassword: RawUTF8): TMVCAction;
    /// blog/main/logout -> disconnect author
    function Logout: TMVCAction;
    /// blog/main/editarticle -> article edition (ID=0 for new)
    procedure ArticleEdit(
      ID: integer; const Title,Content: RawUTF8;
      out Article: TSQLArticle);
    /// blog/main/editarticle -> article edition commit (ID=0 for new)
    function ArticleCommit(
      ID: integer; const Title,Content: RawUTF8): TMVCAction;
  end;

  /// session information which will be stored on client side within a cookie
  // - before Delphi 2010, TTextWriter.RegisterCustomJSONSerializerFromText()
  // is set in initialization block below, to allow proper JSON serialization
  TCookieData = packed record
    AuthorName: RawUTF8;
    AuthorID: cardinal;
    AuthorRights: TSQLAuthorRights;
  end;

  /// implements the ViewModel/Controller of this BLOG web site
  TBlogApplication = class(TMVCApplication,IBlogApplication)
  protected
    fBlogMainInfo: variant;
    fCachedMainArticles: variant;
    function GetViewInfo(MethodIndex: integer): variant; override;
    function GetLoggedAuthorID(Rights: TSQLAuthorRights): integer;
  public
    constructor Create(aServer: TSQLRestServer); reintroduce;
  public
    function Default: variant;
    procedure ArticleView(ID: integer; WithComments: boolean;
      out Article: TSQLArticle; out Author: TSQLAuthor;
      out Comments: TObjectList);
    function Login(const LogonName,PlainPassword: RawUTF8): TMVCAction;
    function Logout: TMVCAction;
    procedure ArticleEdit(ID: integer; const Title,Content: RawUTF8;
      out Article: TSQLArticle);
    function ArticleCommit(ID: integer; const Title,Content: RawUTF8): TMVCAction;
  end;


implementation

resourcestring
  sErrorInvalidLogin = 'Wrong logging information';
  sErrorNeedValidAuthorSession = 'You need to be logged as a valid Author to perform this action';
  sErrorWriting = 'An error occured during saving the information to the database';


{ TBlogApplication }

constructor TBlogApplication.Create(aServer: TSQLRestServer);
var info: TSQLBlogInfo;
begin
  inherited Create(aServer,TypeInfo(IBlogApplication));
  info := TSQLBlogInfo.Create;
  try
    if not RestModel.Retrieve('',info) then begin // retrieve first item
      info.Title := 'mORMot BLOG';
      info.Copyright := '(c)2014 <a href=http://synopse.info>Synopse Informatique</a>';
      RestModel.Add(info,true);
    end;
    fBlogMainInfo := info.GetSimpleFieldsAsDocVariant(false);
  finally
    info.Free;
  end;
  AssociateWithRestServer; // publish IBlogApplication using SynMustache Views
end;

function TBlogApplication.GetLoggedAuthorID(Rights: TSQLAuthorRights): integer;
var SessionInfo: TCookieData;
begin
  result := CurrentSession.CheckAndRetrieve(@SessionInfo,TypeInfo(TCookieData));
  if result>0 then
    result := SessionInfo.AuthorID;
end;

function TBlogApplication.GetViewInfo(MethodIndex: integer): variant;
begin
  result := inherited GetViewInfo(MethodIndex);
  result.blog := fBlogMainInfo;
  result.session := CurrentSession.CheckAndRetrieveInfo(TypeInfo(TCookieData));
end;

procedure TBlogApplication.ArticleEdit(ID: integer; const Title,Content: RawUTF8;
  out Article: TSQLArticle);
var AuthorID: integer;
begin
  AuthorID := GetLoggedAuthorID([canPost]);
  if AuthorID=0 then
    raise EMVCApplication.CreateGotoError(sErrorNeedValidAuthorSession);
  if ID=0 then
    Article.Author := pointer(AuthorID) else
    if not RestModel.Retrieve(ID,Article) then
      raise EMVCApplication.CreateGotoError(HTML_UNAVAILABLE) else
    if Article.Author<>pointer(AuthorID) then
      raise EMVCApplication.CreateGotoError(sErrorNeedValidAuthorSession);
  if Title<>'' then
    Article.Title := Title;
  if Content<>'' then
    Article.Content := Content;
end;

function TBlogApplication.ArticleCommit(ID: integer; const Title,Content: RawUTF8): TMVCAction;
var AuthorID: integer;
    Article: TSQLArticle;
    error: string;
begin
  AuthorID := GetLoggedAuthorID([canPost]);
  if AuthorID=0 then begin
    GotoError(result,sErrorNeedValidAuthorSession);
    exit;
  end;
  VarClear(fCachedMainArticles);
  Article := TSQLArticle.Create(RestModel,ID);
  try
    Article.Title := Title;
    Article.Content := Content;
    Article.Author := pointer(AuthorID);
    Article.Filter;
    error := Article.Validate(RestModel);
    if error<>'' then
      GotoView(result,'ArticleEdit',
        ['ValidationError',error,'ID',ID,
         'Title',Article.Title,'Content',Article.Content]) else
      if Article.ID=0 then
        if RestModel.Add(Article,true)<>0 then
          GotoView(result,'Article',['ID',Article.ID]) else
          GotoError(result,sErrorWriting);
  finally
    Article.Free;
  end;
end;

procedure TBlogApplication.ArticleView(ID: integer; WithComments: boolean;
  out Article: TSQLArticle; out Author: TSQLAuthor; out Comments: TObjectList);
begin
  RestModel.Retrieve(ID,Article);
  if Article.ID<>0 then begin
    RestModel.Retrieve(Article.Author.ID,Author);
    if WithComments then begin
      Comments.Free; // we will override the TObjectList created at input
      Comments := RestModel.RetrieveList(TSQLComment,'Article=?',[Article.ID]);
    end;
  end;
end;

function TBlogApplication.Default: variant;
begin
  if VarIsEmpty(fCachedMainArticles) then
    fCachedMainArticles := RestModel.RetrieveDocVariantArray(
      TSQLArticle,'','order by ID desc limit 40',[],'ID,Title,CreatedAt');
  result := _ObjFast(['articles',fCachedMainArticles]);
end;

function TBlogApplication.Login(const LogonName, PlainPassword: RawUTF8): TMVCAction;
var Author: TSQLAuthor;
    SessionInfo: TCookieData;
begin
  if CurrentSession.CheckAndRetrieve<>0 then begin
    GotoError(result,HTML_BADREQUEST);
    exit;
  end;
  Author := TSQLAuthor.Create(RestModel,'LogonName=?',[LogonName]);
  try
    if (Author.ID<>0) and Author.CheckPlainPassword(PlainPassword) then begin
      SessionInfo.AuthorName := Author.LogonName;
      SessionInfo.AuthorID := Author.ID;
      SessionInfo.AuthorRights := Author.Rights;
      CurrentSession.Initialize(@SessionInfo,TypeInfo(TCookieData));
      GotoDefault(result);
    end else
      GotoError(result,sErrorInvalidLogin);
  finally
    Author.Free;
  end;
end;

function TBlogApplication.Logout: TMVCAction;
begin
  CurrentSession.Finalize;
  GotoDefault(result);
end;


{$ifndef ISDELPHI2010}
initialization
  TTextWriter.RegisterCustomJSONSerializerFromTextSimpleType(TypeInfo(TSQLAuthorRights));
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TCookieData),
    'AuthorName RawUTF8 AuthorID cardinal AuthorRights TSQLAuthorRights');
{$endif}
end.
