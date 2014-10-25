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
    procedure ArticleView(ID: integer;
      var WithComments: boolean; Direction: integer; var Scope: variant;
      out Article: TSQLArticle; out Author: variant;
      out Comments: TObjectList);
    procedure AuthorView(
      var ID: integer; out Author: TSQLAuthor; out Articles: variant);
    function Login(
      const LogonName,PlainPassword: RawUTF8): TMVCAction;
    function Logout: TMVCAction;
    function ArticleComment(ID: integer; const Title,Comment: RawUTF8): TMVCAction;
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
    fTagsLookupLock: IAutoLocker;
    fTagsLookup: TRawUTF8DynArray;
    fTagsLookupOrder: TCardinalDynArray;
    fDefaultData: ILockedDocVariant;
    fDefaultLastID: integer;
    procedure ComputeMinimalData; virtual;
    procedure FlushAnyCache; override;
    function GetViewInfo(MethodIndex: integer): variant; override;
    function GetLoggedAuthorID(Right: TSQLAuthorRight; out AuthorName: RawUTF8): integer;
    procedure MonthToText(const Value: variant; out result: variant);
    procedure TagToText(const Value: variant; out result: variant);
  public
    constructor Create(aServer: TSQLRestServer); reintroduce;
  public
    procedure Default(var Scope: variant);
    procedure ArticleView(ID: integer;
      var WithComments: boolean; Direction: integer; var Scope: variant;
      out Article: TSQLArticle; out Author: variant;
      out Comments: TObjectList);
    procedure AuthorView(
      var ID: integer; out Author: TSQLAuthor; out Articles: variant);
    function Login(const LogonName,PlainPassword: RawUTF8): TMVCAction;
    function Logout: TMVCAction;
    function ArticleComment(ID: integer; const Title,Comment: RawUTF8): TMVCAction;
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
  with TSQLBlogInfo.Create(RestModel,'') do
  try
    fBlogMainInfo := GetSimpleFieldsAsDocVariant(false);
  finally
    Free;
  end;
  fTagsLookupLock := TAutoLocker.Create;
  fTagsLookup := TSQLTag.ComputeTagIdentPerIDArray(RestModel,fTagsLookupOrder);
  // publish IBlogApplication using Mustache Views (TMVCRunOnRestServer default)
  fMainRunner := TMVCRunOnRestServer.Create(Self).
    SetCache('Default',cacheRootIfNoSession,15).
    SetCache('ArticleView',cacheWithParametersIfNoSession,60).
    SetCache('AuthorView',cacheWithParametersIgnoringSession,60);
  (TMVCRunOnRestServer(fMainRunner).Views as TMVCViewsMustache).
    RegisterExpressionHelpers(['MonthToText'],[MonthToText]).
    RegisterExpressionHelpers(['TagToText'],[TagToText]);
end;

procedure TBlogApplication.MonthToText(const Value: variant;
  out result: variant);
const MONTHS: array[0..11] of RawUTF8 = (
  'January','February','March','April','May','June','July','August',
  'September','October','November','December');
var month: integer;
begin
  if VariantToInteger(Value,month) and (month>0) then
    RawUTF8ToVariant(MONTHS[month mod 12]+' '+UInt32ToUTF8(month div 12),result) else
    SetVariantNull(result);
end;

procedure TBlogApplication.TagToText(const Value: variant;
  out result: variant);
var tag: integer;
begin
  fTagsLookupLock.Enter;
  try
    if VariantToInteger(Value,tag) and
       (tag>0) and (tag<=length(fTagsLookup)) then
      RawUTF8ToVariant(fTagsLookup[tag-1],result) else
      SetVariantNull(result);
  finally
    fTagsLookupLock.Leave;
  end;
end;

const
  // just try with 200000 - and let your WordPress blog engine start to cry...
  FAKEDATA_ARTICLESCOUNT = 200;
  
procedure TBlogApplication.ComputeMinimalData;
var info: TSQLBlogInfo;
    article: TSQLArticle;
    comment: TSQLComment;
    tag: TSQLTag;
    batch: TSQLRestBatch;
    n,t: integer;
    articles,tags,comments: TIntegerDynArray;
begin
  TSQLRecord.AutoFree([ // avoid several try..finally
    @info,TSQLBlogInfo, @article,TSQLArticle, @comment,TSQLComment, @tag,TSQLTag]);
  if not RestModel.Retrieve('',info) then begin // retrieve first item
    info.Title := 'mORMot BLOG';
    info.Language := 'en';
    info.Description := 'Sample Blog Web Application using Synopse mORMot MVC';
    info.Copyright := '&copy;2014 <a href=http://synopse.info>Synopse Informatique</a>';
    info.About := TSynTestCase.RandomTextParagraph(30,'!');
    RestModel.Add(info,true);
  end;
  if RestModel.TableHasRows(TSQLArticle) then
    exit;
  batch := TSQLRestBatch.Create(RestModel,TSQLTag,100);
  try
    for n := 1 to 32 do begin
      tag.Ident := 'Tag'+UInt32ToUtf8(n);
      tag.ID := n*2; // force ID to test TSQLTag.ComputeTagIdentPerIDArray
      batch.Add(tag,true,true);
    end;
    RestModel.BatchSend(batch,tags);
    fTagsLookup := TSQLTag.ComputeTagIdentPerIDArray(RestModel,fTagsLookupOrder);
    batch.Reset(TSQLArticle,20000);
    article.Author := TSQLAuthor(1);
    article.AuthorName := 'synopse';
    for n := 1 to FAKEDATA_ARTICLESCOUNT do begin
      article.PublishedMonth := 2014*12+(n div 10);
      article.Title := TSynTestCase.RandomTextParagraph(5,' ');
      article.Abstract := TSynTestCase.RandomTextParagraph(30,'!');
      article.Content := TSynTestCase.RandomTextParagraph(200,'.','http://synopse.info');
      article.Tags := nil;
      for t := 1 to Random(6) do
        article.TagsAddOrdered(tags[random(length(tags))],fTagsLookupOrder);
      batch.Add(article,true);
    end;
    if RestModel.BatchSend(batch,articles)=HTML_SUCCESS then begin
      comment.Author := article.Author;
      comment.AuthorName := article.AuthorName;
      batch.Reset(TSQLComment,20000);
      for n := 1 to FAKEDATA_ARTICLESCOUNT*2 do begin
        comment.Article := Pointer(articles[random(length(articles))]);
        comment.Title := TSynTestCase.RandomTextParagraph(5,' ');
        comment.Content := TSynTestCase.RandomTextParagraph(30,'.','http://mormot.net');
        batch.Add(Comment,true);
      end;
      RestModel.BatchSend(batch,comments)
    end;
  finally
    batch.Free;
  end;
end;

function TBlogApplication.GetLoggedAuthorID(Right: TSQLAuthorRight;
  out AuthorName: RawUTF8): integer;
var SessionInfo: TCookieData;
    author: TSQLAuthor;
begin
  result := 0;
  if (CurrentSession.CheckAndRetrieve(@SessionInfo,TypeInfo(TCookieData))=0) or
     not(Right in SessionInfo.AuthorRights) then
    exit;
  TSQLAuthor.AutoFree(author,RestModel,SessionInfo.AuthorID);
  if Right in author.Rights then begin
    result := SessionInfo.AuthorID;
    AuthorName := author.LogonName;
  end;
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
  ARTICLE_FIELDS = 'ID,Title,Tags,Abstract,Author,AuthorName,CreatedAt';
  ARTICLE_DEFAULT_ORDER: RawUTF8 = 'order by ID desc limit 20';

procedure TBlogApplication.Default(var Scope: variant);
var lastID,tag: integer;
    whereClause: RawUTF8;
begin
  lastID := 0;
  tag := 0;
  with DocVariantDataSafe(Scope)^ do begin
    if GetAsInteger('lastID',lastID) then
      whereClause := 'ID<?' else
      whereClause := 'ID>?'; // will search ID>0 so always true
    if GetAsInteger('tag',tag) then // uses custom function to search in BLOB
      whereClause := whereClause+' and IntegerDynArrayContains(Tags,?)';
  end;
  if (lastID=0) and (tag=0) then begin // use simple cache if no parameters
    SetVariantNull(Scope);
    if not fDefaultData.AddExistingProp('Articles',Scope) then
      fDefaultData.AddNewProp('Articles',RestModel.RetrieveDocVariantArray(
        TSQLArticle,'',pointer(ARTICLE_DEFAULT_ORDER),[],
        ARTICLE_FIELDS,nil,@fDefaultLastID),Scope);
    lastID := fDefaultLastID;
  end else // use more complex request using lastID + tag parameters
    scope := _ObjFast(['Articles',RestModel.RetrieveDocVariantArray(
        TSQLArticle,'',Pointer(whereClause+ARTICLE_DEFAULT_ORDER),[lastID,tag],
        ARTICLE_FIELDS,nil,@lastID)]);
  if lastID>1 then
    _ObjAddProps(['lastID',lastID],Scope);
  if tag>0 then
    _ObjAddProps(['tag',tag],Scope);
  if not fDefaultData.AddExistingProp('Archives',Scope) then
    fDefaultData.AddNewProp('Archives',RestModel.RetrieveDocVariantArray(
      TSQLArticle,'','group by PublishedMonth order by PublishedMonth desc limit 12',[],
      'distinct(PublishedMonth),max(ID)+1 as FirstID'),Scope);
end;

procedure TBlogApplication.ArticleView(ID: integer;
  var WithComments: boolean; Direction: integer; var Scope: variant;
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
  out Articles: variant);
begin
  RestModel.Retrieve(ID,Author);
  Author.HashedPassword := ''; // no need to publish it
  if Author.ID<>0 then
    Articles := RestModel.RetrieveDocVariantArray(
      TSQLArticle,'','Author=? order by id desc limit 50',[ID],ARTICLE_FIELDS) else
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

function TBlogApplication.ArticleComment(ID: integer;
  const Title,Comment: RawUTF8): TMVCAction;
var AuthorID: integer;
    AuthorName: RawUTF8;
    comm: TSQLComment;
    error: string;
begin
  AuthorID := GetLoggedAuthorID(canComment,AuthorName);
  if AuthorID=0 then
    raise EMVCApplication.CreateGotoError(sErrorNeedValidAuthorSession);
  if not RestModel.MemberExists(TSQLArticle,ID) then
    raise EMVCApplication.CreateGotoError(HTML_UNAVAILABLE);
  TSQLComment.AutoFree(comm);
  comm.Title := Title;
  comm.Content := Comment;
  comm.Article := TSQLArticle(ID);
  comm.Author := TSQLAuthor(AuthorID);
  comm.AuthorName := AuthorName;
  if comm.FilterAndValidate(RestModel,error) and (RestModel.Add(comm,true)<>0) then
    GotoView(result,'ArticleView',['ID',ID,'withComments',true]) else
    GotoView(result,'ArticleView',['ID',ID,'withComments',true,
      'Scope',_ObjFast([
        'CommentError',error,'CommentTitle',comm.Title,'CommentContent',comm.Content])],
      HTML_NOTMODIFIED);
end;

procedure TBlogApplication.ArticleEdit(var ID: integer;
  const Title,Content: RawUTF8; const ValidationError: variant;
  out Article: TSQLArticle);
var AuthorID: integer;
    AuthorName: RawUTF8;
begin
  AuthorID := GetLoggedAuthorID(canPost,AuthorName);
  if AuthorID=0 then
    raise EMVCApplication.CreateGotoError(sErrorNeedValidAuthorSession);
  if ID=0 then begin
    Article.Author := pointer(AuthorID);
    Article.AuthorName := AuthorName;
  end else
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
    AuthorName: RawUTF8;
    error: string;
begin
  AuthorID := GetLoggedAuthorID(canPost,AuthorName);
  if AuthorID=0 then begin
    GotoError(result,sErrorNeedValidAuthorSession);
    exit;
  end;
  FlushAnyCache;
  TSQLArticle.AutoFree(Article,RestModel,ID);
  Article.Title := Title;
  Article.Content := Content;
  Article.Author := pointer(AuthorID);
  Article.AuthorName := AuthorName;
  if not Article.FilterAndValidate(RestModel,error) then
    GotoView(result,'ArticleEdit',
      ['ValidationError',error,'ID',ID,
       'Title',Article.Title,'Content',Article.Content],HTML_NOTMODIFIED) else
    if Article.ID=0 then begin
      Article.PublishedMonth := TSQLArticle.CurrentPublishedMonth;
      if RestModel.Add(Article,true)<>0 then
        GotoView(result,'ArticleView',['ID',Article.ID],HTML_SUCCESS) else
        GotoError(result,sErrorWriting);
    end else
      RestModel.Update(Article);
end;

{$ifndef ISDELPHI2010}


initialization
  TTextWriter.RegisterCustomJSONSerializerFromTextSimpleType(TypeInfo(TSQLAuthorRights));
  TTextWriter.RegisterCustomJSONSerializerFromText(TypeInfo(TCookieData),
    'AuthorName RawUTF8 AuthorID cardinal AuthorRights TSQLAuthorRights');
{$endif}
end.
