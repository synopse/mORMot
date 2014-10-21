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
    /// blog/main/articleView?id=12 -> view one article
    // - here article details are returned as out parameters values
    procedure ArticleView(
      ID: integer; WithComments: boolean;
      out Article: TSQLArticle; out Author: TSQLAuthor;
      out Comments: TObjectList);
    /// blog/main/authorView?id=12 -> information about one author
    procedure AuthorView(
      ID: integer; out Author: TSQLAuthor; out Articles: RawJSON);
    /// blog/main/login?name=...&plainpassword=... -> log as author
    function Login(
      const LogonName,PlainPassword: RawUTF8): TMVCAction;
    /// blog/main/logout -> disconnect author
    function Logout: TMVCAction;
    /// blog/main/articleedit -> article edition (ID=0 for new)
    procedure ArticleEdit(
      ID: integer; const Title,Content, ValidationError: RawUTF8;
      out Article: TSQLArticle);
    /// blog/main/articlecommit -> article edition commit (ID=0 for new)
    function ArticleCommit(
      ID: integer; const Title,Content: RawUTF8): TMVCAction;
  end;

  /// session information which will be stored on client side within a cookie
  // - before Delphi 2010, TTextWriter.RegisterCustomJSONSerializerFromText() is
  // called in initialization block below, to allow proper JSON serialization
  // as needed for proper injection into the Mustache rendering data context
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
    procedure FlushAnyCache; override;
    function GetViewInfo(MethodIndex: integer): variant; override;
    function GetLoggedAuthorID(Rights: TSQLAuthorRights): integer;
  public
    constructor Create(aServer: TSQLRestServer); reintroduce;
  public
    procedure Default(var Scope: variant);
    procedure ArticleView(ID: integer; WithComments: boolean;
      out Article: TSQLArticle; out Author: TSQLAuthor;
      out Comments: TObjectList);
    procedure AuthorView(
      ID: integer; out Author: TSQLAuthor; out Articles: RawJSON);
    function Login(const LogonName,PlainPassword: RawUTF8): TMVCAction;
    function Logout: TMVCAction;
    procedure ArticleEdit(ID: integer; const Title,Content, ValidationError: RawUTF8;
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
      info.Language := 'en';
      info.Description := 'Sample Blog Web Application using Synopse mORMot MVC'; 
      info.Copyright := '&copy;2014 <a href=http://synopse.info>Synopse Informatique</a>';
      RestModel.Add(info,true);
    end;
    fBlogMainInfo := info.GetSimpleFieldsAsDocVariant(false);
  finally
    info.Free;
  end;
  // publish IBlogApplication using Mustache Views (TMVCRunOnRestServer default)
  fMainRunner := TMVCRunOnRestServer.Create(Self).
    SetCache('Default',cacheRootIfNoSession,15). 
    SetCache('ArticleView',cacheWithParametersIfNoSession,60).
    SetCache('AuthorView',cacheWithParametersIgnoringSession,60);
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
  _ObjAddProps(['blog',fBlogMainInfo,
    'session',CurrentSession.CheckAndRetrieveInfo(TypeInfo(TCookieData))],result);
end;

procedure TBlogApplication.FlushAnyCache;
begin
  inherited FlushAnyCache; // call fMainRunner.NotifyContentChanged
  VarClear(fCachedMainArticles);
end;


{ TBlogApplication - Commands }

const
  ARTICLE_FIELDS = 'ID,Title,Abstract,Author,AuthorName,CreatedAt';

procedure TBlogApplication.Default(var Scope: variant);
begin
  if VarIsEmpty(fCachedMainArticles) then
    fCachedMainArticles := RestModel.RetrieveDocVariantArray(
      TSQLArticle,'','order by ID desc limit 40',[],ARTICLE_FIELDS);
  _ObjAddProps(['articles',fCachedMainArticles],Scope);
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
  end else
    raise EMVCApplication.CreateGotoError(HTML_NOTFOUND);
end;

procedure TBlogApplication.AuthorView(ID: integer; out Author: TSQLAuthor;
  out Articles: RawJSON);
begin
  RestModel.Retrieve(ID,Author);
  if Author.ID<>0 then
    Articles := RestModel.RetrieveListJSON(TSQLArticle,'Author=?',[ID],ARTICLE_FIELDS) else
    raise EMVCApplication.CreateGotoError(HTML_NOTFOUND);
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

procedure TBlogApplication.ArticleEdit(ID: integer;
  const Title,Content, ValidationError: RawUTF8; out Article: TSQLArticle);
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
  FlushAnyCache;
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

{$ifndef ISDELPHI2010}

initialization
  TTextWriter.RegisterCustomJSONSerializerFromTextSimpleType(TypeInfo(TSQLAuthorRights));
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TCookieData),
    'AuthorName RawUTF8 AuthorID cardinal AuthorRights TSQLAuthorRights');
{$endif}
end.
