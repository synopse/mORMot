/// database Model for the MVCServer BLOG sample
unit MVCModel;

interface

uses
  SysUtils,
  SynCommons,
  SynCrypto,
  mORMot;

type
  TSQLBlogInfo = class(TSQLRecord)
  private
    fCopyright: RawUTF8;
    fDescription: RawUTF8;
    fTitle: RawUTF8;
    fLanguage: RawUTF8;
    fAbout: RawUTF8;
  published
    property Title: RawUTF8 index 80 read fTitle write fTitle;
    property Language: RawUTF8 index 3 read fLanguage write fLanguage;
    property Description: RawUTF8 index 120 read fDescription write fDescription;
    property Copyright: RawUTF8 index 80 read fCopyright write fCopyright;
    property About: RawUTF8 read fAbout write fAbout;
  end;

  TSQLRecordTimeStamped = class(TSQLRecord)
  private
    fCreatedAt: TCreateTime;
    fModifiedAt: TModTime;
  published
    property CreatedAt: TCreateTime read fCreatedAt write fCreatedAt;
    property ModifiedAt: TModTime read fModifiedAt write fModifiedAt;
  end;

  TSQLSomeone = class(TSQLRecordTimeStamped)
  private
    fFirstName: RawUTF8;
    fFamilyName: RawUTF8;
    fBirthDate: TDateTime;
    fEmail: RawUTF8;
    fVerified: boolean;
    fHashedPassword: RawUTF8;
    fLogonName: RawUTF8;
  public
    procedure SetPlainPassword(const PlainPassword: RawUTF8);
    function CheckPlainPassword(const PlainPassword: RawUTF8): boolean;
    function Name: RawUTF8; 
  published
    property LogonName: RawUTF8 index 30 read fLogonName write fLogonName stored AS_UNIQUE;
    property FirstName: RawUTF8 index 50 read fFirstName write fFirstName;
    property FamilyName: RawUTF8 index 50 read fFamilyName write fFamilyName;
    property BirthDate: TDateTime read fBirthDate write fBirthDate;
    property Email: RawUTF8 index 40 read fEmail write fEmail;
    property HashedPassword: RawUTF8 index 64 read fHashedPassword write fHashedPassword;
    property Verified: boolean read fVerified write fVerified;
  end;
          
  TSQLAuthorRight = (canComment, canPost, canDelete, canAdministrate);
  TSQLAuthorRights = set of TSQLAuthorRight;

  TSQLAuthor = class(TSQLSomeone)
  private
    fRights: TSQLAuthorRights;
  public
    class procedure InitializeTable(Server: TSQLRestServer; const FieldName: RawUTF8;
      Options: TSQLInitializeTableOptions); override;
  published
    property Rights: TSQLAuthorRights read fRights write fRights;
  end;

  TSQLContent = class(TSQLRecordTimeStamped)
  private
    fContent: RawUTF8;
    fTitle: RawUTF8;
    fAuthor: TSQLAuthor;
    fAuthorName: RawUTF8;
  published
    property Title: RawUTF8 index 80 read fTitle write fTitle;
    property Content: RawUTF8 read fContent write fContent;
    property Author: TSQLAuthor read fAuthor write fAuthor;
    property AuthorName: RawUTF8 index 50 read fAuthorName write fAuthorName;
  end;

  TSQLTags = object
    Lock: IAutoLocker;
    Lookup: array of record
      Ident: RawUTF8;
      Occurence: integer;
    end;
    OrderID: TIntegerDynArray;
    procedure Init(aRest: TSQLRest);
    function Get(tagID: integer): RawUTF8;
    procedure SaveOccurence(aRest: TSQLRest);
    procedure SortTagsByIdent(var Tags: TIntegerDynArray);
    function GetAsDocVariantArray: Variant;
  end;

  TSQLArticle = class(TSQLContent)
  private
    fAbstract: RawUTF8;
    fPublishedMonth: Integer;
    fTags: TIntegerDynArray;
  public
    class function CurrentPublishedMonth: Integer;
    class procedure InitializeTable(Server: TSQLRestServer; const FieldName: RawUTF8;
      Options: TSQLInitializeTableOptions); override;
    // note: caller should call Tags.SaveOccurence() to update the DB
    procedure TagsAddOrdered(aTagID: Integer; var aTags: TSQLTags);
  published
    property PublishedMonth: Integer read fPublishedMonth write fPublishedMonth;
    property Abstract: RawUTF8 index 1024 read fAbstract write fAbstract;
    // "index 1" below to allow writing e.g. aArticle.DynArray(1).Delete(aIndex)
    property Tags: TIntegerDynArray index 1 read fTags write fTags;
  end;

  TSQLComment = class(TSQLContent)
  private
    fArticle: TSQLArticle;
  published
    property Article: TSQLArticle read fArticle write fArticle;
  end;

  TSQLTag = class(TSQLRecord)
  private
    fIdent: RawUTF8;
    fOccurence: integer;
    fCreatedAt: TCreateTime;
  published
    property Ident: RawUTF8 read fIdent write fIdent;
    property Occurence: Integer read fOccurence write fOccurence;
    property CreatedAt: TCreateTime read fCreatedAt write fCreatedAt;
  end;



function CreateModel: TSQLModel;


implementation

function CreateModel: TSQLModel;
begin
  result := TSQLModel.Create([TSQLBlogInfo,TSQLAuthor,
    TSQLTag,TSQLArticle,TSQLComment],'blog');
  TSQLArticle.AddFilterNotVoidText(['Title','Content']);
  TSQLComment.AddFilterNotVoidText(['Title','Content']);
  TSQLTag.AddFilterNotVoidText(['Ident']);
end;


{ TSQLSomeone }

const
  SALT = 'mORMot';

function TSQLSomeone.CheckPlainPassword(const PlainPassword: RawUTF8): boolean;
begin
  result := fHashedPassword=SHA256(SALT+LogonName+PlainPassword);
end;

function TSQLSomeone.Name: RawUTF8;
begin
  result := FirstName+' '+FamilyName;
end;

procedure TSQLSomeone.SetPlainPassword(const PlainPassword: RawUTF8);
begin
  fHashedPassword := SHA256(SALT+LogonName+PlainPassword);
end;


{ TSQLAuthor }

class procedure TSQLAuthor.InitializeTable(Server: TSQLRestServer;
  const FieldName: RawUTF8; Options: TSQLInitializeTableOptions);
var Auth: TSQLAuthor;
begin
  inherited;
  if FieldName='' then begin // new table -> create default Author
    Auth := TSQLAuthor.Create;
    try
      Auth.LogonName := 'synopse';
      Auth.SetPlainPassword('synopse');
      Auth.FamilyName := 'Synopse';
      Auth.Verified := true;
      Auth.Rights := [Low(TSQLAuthorRight)..High(TSQLAuthorRight)];
      Server.Add(Auth,true);
    finally
      Auth.Free;
    end;
  end;
end;


{ TSQLArticle }

class function TSQLArticle.CurrentPublishedMonth: Integer;
var Y,M,D: word;
begin
  DecodeDate(NowUTC,Y,M,D);
  result := integer(Y)*12+integer(M)-1;
end;

class procedure TSQLArticle.InitializeTable(Server: TSQLRestServer;
  const FieldName: RawUTF8; Options: TSQLInitializeTableOptions);
begin
  inherited;
  if (FieldName='') or (FieldName='PublishedMonth') then
    Server.CreateSQLIndex(TSQLArticle,'PublishedMonth',false);
end;

procedure TSQLArticle.TagsAddOrdered(aTagID: Integer; var aTags: TSQLTags);
begin
  if aTagID>=length(aTags.Lookup) then
    exit;
  if not AddInteger(fTags,aTagID,true) then
    exit; // already there
  aTags.Lock.ProtectMethod;
  inc(aTags.Lookup[aTagID-1].Occurence);
  aTags.SortTagsByIdent(fTags);
end;


{ TSQLTags }

function TSQLTags.Get(tagID: integer): RawUTF8;
begin
  if (tagID>0) and (tagID<=Length(Lookup)) then
    result := Lookup[tagID-1].Ident else
    result := '';
end;

function TSQLTags.GetAsDocVariantArray: Variant;
var i,ndx: Integer;
begin
  TDocVariant.NewFast(result);
  for i := 0 to length(OrderID)-1 do begin
    ndx := OrderID[i]-1;
    with Lookup[ndx] do
      if Occurence>0 then
        TDocVariantData(result).AddItem(
          _ObjFast(['tagID',ndx+1,'ident',Ident,'occurence',Occurence]));
  end;
end;

procedure TSQLTags.Init(aRest: TSQLRest);
var tag: TSQLTag;
    ID,count,maxID: integer;
begin
  Finalize(Lookup);
  if Lock=nil then
    Lock := TAutoLocker.Create;
  Lock.ProtectMethod;
  TAutoFree.One(
    tag,TSQLTag.CreateAndFillPrepare(aRest,'order by Ident','ID,Ident,Occurence'));
  count := tag.FillTable.RowCount;
  if count=0 then
    exit;
  SetLength(OrderID,count);
  count := 0;
  maxID := 0;
  while tag.FillOne do begin
    ID := tag.ID;
    OrderID[count] := ID;
    inc(count);
    if ID>maxID then
      maxID := ID;
  end;
  SetLength(Lookup,maxID);
  tag.FillRewind;
  while tag.FillOne do
  with Lookup[tag.ID-1] do begin
    Ident := tag.Ident;
    Occurence := tag.Occurence;
  end;
end;

procedure TSQLTags.SaveOccurence(aRest: TSQLRest);
var tag: TSQLTag;
    batch: TSQLRestBatch;
begin
  Lock.ProtectMethod;
  TAutoFree.Several([
    @tag,TSQLTag.CreateAndFillPrepare(aRest,'','ID,Occurence'),
    @batch,TSQLRestBatch.Create(aRest,TSQLTag,1000)]);
  while tag.FillOne do begin
    if tag.ID<=length(Lookup) then
      if Lookup[tag.ID-1].Occurence<>tag.Occurence then begin
        tag.Occurence := Lookup[tag.ID-1].Occurence;
        batch.Update(tag); // will update only Occurence field
      end;
  end;
  aRest.BatchSend(batch);
end;

procedure TSQLTags.SortTagsByIdent(var Tags: TIntegerDynArray);
var new: TIntegerDynArray;
    i,n: integer;
begin
  n := length(Tags);
  if n=1 then
    exit;
  SetLength(new,n);
  QuickSortInteger(pointer(Tags),0,n-1);
  n := 0;
  for i := 0 to length(OrderID)-1 do
    if FastFindIntegerSorted(Tags,OrderID[i])>=0 then begin
      new[n] := OrderID[i];
      inc(n);
    end;
  assert(n=length(Tags));
  Tags := new;
end;

end.
