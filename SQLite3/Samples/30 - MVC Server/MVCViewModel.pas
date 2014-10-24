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
  // - typical URI are:
  // ! blog/main/articleView?id=12 -> view one article
  // ! blog/main/authorView?id=12 -> information about one author
  // ! blog/main/login?name=...&plainpassword=... -> log as author
  // ! blog/main/articlecommit -> article edition commit (ID=0 for new)
  IBlogApplication = interface(IMVCApplication)
    procedure ArticleView(
      ID: integer; var WithComments: boolean; Direction: integer;
      out Article: TSQLArticle; out Author: variant;
      out Comments: TObjectList);
    procedure AuthorView(
      var ID: integer; out Author: TSQLAuthor; out Articles: RawJSON);
    function Login(
      const LogonName,PlainPassword: RawUTF8): TMVCAction;
    function Logout: TMVCAction;
    procedure ArticleEdit(var ID: integer; const Title,Content: RawUTF8;
      const ValidationError: variant;
      out Article: TSQLArticle);
    function ArticleCommit(
      ID: integer; const Title,Content: RawUTF8): TMVCAction;
  end;

  /// session information which will be stored on client side within a cookie
  // - TMVCSessionWithCookies is able to store any record on the client side,
  // as optimized base64 encoded binary data, without any storage on the server
  // - before Delphi 2010, TTextWriter.RegisterCustomJSONSerializerFromText() is
  // called in initialization block below, to allow proper JSON serialization
  // as needed for fields injection into the Mustache rendering data context
  TCookieData = packed record
    AuthorName: RawUTF8;
    AuthorID: cardinal;
    AuthorRights: TSQLAuthorRights;
  end;

  /// implements the ViewModel/Controller of this BLOG web site
  TBlogApplication = class(TMVCApplication,IBlogApplication)
  protected
    fBlogMainInfo: variant;
    fDefaultData: ILockedDocVariant;
    fDefaultLastID: integer;
    procedure ComputeMinimalData; virtual;
    procedure FlushAnyCache; override;
    function GetViewInfo(MethodIndex: integer): variant; override;
    function GetLoggedAuthorID(Rights: TSQLAuthorRights): integer;
    procedure MonthToText(const Value: variant; out result: variant);
  public
    constructor Create(aServer: TSQLRestServer); reintroduce;
  public
    procedure Default(var Scope: variant);
    procedure ArticleView(ID: integer; var WithComments: boolean;
      Direction: integer;
      out Article: TSQLArticle; out Author: variant;
      out Comments: TObjectList);
    procedure AuthorView(
      var ID: integer; out Author: TSQLAuthor; out Articles: RawJSON);
    function Login(const LogonName,PlainPassword: RawUTF8): TMVCAction;
    function Logout: TMVCAction;
    procedure ArticleEdit(var ID: integer; const Title,Content: RawUTF8;
      const ValidationError: variant;
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
begin
  fDefaultData := TLockedDocVariant.Create;
  inherited Create(aServer,TypeInfo(IBlogApplication));
  ComputeMinimalData;
  // publish IBlogApplication using Mustache Views (TMVCRunOnRestServer default)
  fMainRunner := TMVCRunOnRestServer.Create(Self).
    SetCache('Default',cacheRootIfNoSession,15).
    SetCache('ArticleView',cacheWithParametersIfNoSession,60).
    SetCache('AuthorView',cacheWithParametersIgnoringSession,60);
  (TMVCRunOnRestServer(fMainRunner).Views as TMVCViewsMustache).
    RegisterExpressionHelpers(['MonthToText'],[MonthToText]);
end;

procedure TBlogApplication.MonthToText(const Value: variant;
  out result: variant);
const MONTHS: array[0..11] of RawUTF8 = (
  'January','February','March','April','May','June','July','August',
  'September','October','November','December');
var month: integer;
    text: RawUTF8;
begin
  if VariantToInteger(Value,month) and (month>0) then
    text := MONTHS[month mod 12]+' '+UInt32ToUTF8(month div 12);
  RawUTF8ToVariant(text,result);
end;

procedure TBlogApplication.ComputeMinimalData;
var info: TSQLBlogInfo;
    article: TSQLArticle;
    comment: TSQLComment;
    n: integer;
    res: TIntegerDynArray;
begin
  info := TSQLBlogInfo.Create;
  try
    if not RestModel.Retrieve('',info) then begin // retrieve first item
      info.Title := 'mORMot BLOG';
      info.Language := 'en';
      info.Description := 'Sample Blog Web Application using Synopse mORMot MVC';
      info.Copyright := '&copy;2014 <a href=http://synopse.info>Synopse Informatique</a>';
      info.About := TSynTestCase.RandomTextParagraph(30,'!');
      RestModel.Add(info,true);
    end;
    fBlogMainInfo := info.GetSimpleFieldsAsDocVariant(false);
  finally
    info.Free;
  end;
  if not RestModel.TableHasRows(TSQLArticle) then begin
    RestModel.BatchStart(TSQLArticle,1000);
    article := TSQLArticle.Create;
    try
      article.Author := TSQLAuthor(1);
      article.AuthorName := 'synopse';
      for n := 1 to 100 do begin
        article.PublishedMonth := 2014*12+(n div 10);
        article.Title := TSynTestCase.RandomTextParagraph(5,' ');
        article.Abstract := TSynTestCase.RandomTextParagraph(30,'!');
        article.Content := TSynTestCase.RandomTextParagraph(200,'.','http://synopse.info');
        RestModel.BatchAdd(article,true);
      end;
      if RestModel.BatchSend(res)=HTML_SUCCESS then begin
        comment := TSQLComment.Create;
        try
          comment.Author := article.Author;
          comment.AuthorName := article.AuthorName;
          RestModel.BatchStart(TSQLComment,1000);
          for n := 1 to 200 do begin
            comment.Article := Pointer(res[random(length(res))]);
            comment.Title := TSynTestCase.RandomTextParagraph(5,' ');
            comment.Content := TSynTestCase.RandomTextParagraph(30,'.','http://mormot.net');
            RestModel.BatchAdd(Comment,true);
          end;
          RestModel.BatchSend(res)
        finally
          comment.Free;
        end;
      end;
    finally
      article.Free;
    end;
  end;
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
  fDefaultData.Clear;
end;


{ TBlogApplication - Commands }

const
  ARTICLE_FIELDS = 'ID,Title,Abstract,Author,AuthorName,CreatedAt';

procedure TBlogApplication.Default(var Scope: variant);
var lastID: integer;
begin
  if VariantToInteger(Scope,lastID) and (lastID>0) then begin
    _ObjAddProps(['articles',RestModel.RetrieveDocVariantArray(
        TSQLArticle,'','ID<? order by ID desc limit 20',[lastID],ARTICLE_FIELDS,
        nil,@lastID)],Scope);
    if lastID>1 then
      _ObjAddProps(['lastID',lastID],Scope);
  end else begin
    if not fDefaultData.AddExistingProp('Articles',Scope) then
      fDefaultData.AddNewProp('Articles',RestModel.RetrieveDocVariantArray(
        TSQLArticle,'','order by ID desc limit 20',[],ARTICLE_FIELDS,
        nil,@fDefaultLastID),Scope);
    _ObjAddProps(['lastID',fDefaultLastID],Scope);
  end;
  if not fDefaultData.AddExistingProp('Archives',Scope) then
    fDefaultData.AddNewProp('Archives',RestModel.RetrieveDocVariantArray(
      TSQLArticle,'','group by PublishedMonth order by PublishedMonth desc limit 12',[],
      'distinct(PublishedMonth),max(ID)+1 as FirstID'),Scope);
end;

procedure TBlogApplication.ArticleView(
  ID: integer; var WithComments: boolean; Direction: integer;
  out Article: TSQLArticle; out Author: variant; out Comments: TObjectList);
var newID: integer;
const WHERE: array[1..2] of PUTF8Char = (
  'ID<? order by id desc','ID>? order by id');
begin
  if Direction in [1,2] then // allows fast paging using index on ID
    if RestModel.OneFieldValue(TSQLArticle,'ID',WHERE[Direction],[],[ID],newID) and
      (newID<>0) then
      ID := newID;
  RestModel.Retrieve(ID,Article);
  if Article.ID<>0 then begin
    Author := RestModel.RetrieveDocVariant(
      TSQLAuthor,'ID=?',[Article.Author.ID],'FirstName,FamilyName');
    if WithComments then begin
      Comments.Free; // we will override the TObjectList created at input
      Comments := RestModel.RetrieveList(TSQLComment,'Article=?',[Article.ID]);
    end;
  end else
    raise EMVCApplication.CreateGotoError(HTML_NOTFOUND);
end;

procedure TBlogApplication.AuthorView(var ID: integer; out Author: TSQLAuthor;
  out Articles: RawJSON);
begin
  RestModel.Retrieve(ID,Author);
  Author.HashedPassword := ''; // no need to publish it
  if Author.ID<>0 then
    Articles := RestModel.RetrieveListJSON(
      TSQLArticle,'Author=? order by id desc limit 50',[ID],ARTICLE_FIELDS) else
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

procedure TBlogApplication.ArticleEdit(var ID: integer;
  const Title,Content: RawUTF8; const ValidationError: variant;
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
      if Article.ID=0 then begin
        Article.PublishedMonth := TSQLArticle.CurrentPublishedMonth;
        if RestModel.Add(Article,true)<>0 then
          GotoView(result,'Article',['ID',Article.ID]) else
          GotoError(result,sErrorWriting);
      end else
        RestModel.Update(Article);
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
