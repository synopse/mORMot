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

  TSQLArticle = class(TSQLContent)
  private
    fAbstract: RawUTF8;
    fPublishedMonth: Integer;
    fTags: TIntegerDynArray;
  public
    class function CurrentPublishedMonth: Integer;
    class procedure InitializeTable(Server: TSQLRestServer; const FieldName: RawUTF8;
      Options: TSQLInitializeTableOptions); override;
    procedure TagsAddOrdered(aTagID: Integer; const AlphabeticalOrder: TCardinalDynArray);
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
    fCreatedAt: TCreateTime;
  public
    class function ComputeTagIdentPerIDArray(aRest: TSQLRest;
      out AlphabeticalOrder: TCardinalDynArray): TRawUTF8DynArray;
  published
    property Ident: RawUTF8 read fIdent write fIdent;
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

procedure TSQLArticle.TagsAddOrdered(aTagID: Integer;
  const AlphabeticalOrder: TCardinalDynArray);
var sets: TByteDynArray;
    i,n,max: integer;
begin // add tag ID per alphabetic order - a bit complicated but works
  max := length(AlphabeticalOrder);
  if (aTagID=0) or (aTagID>max) then
    exit;
  n := length(fTags);
  if n=0 then begin
    SetLength(fTags,1);
    fTags[0] := aTagID;
    exit;
  end;
  SetLength(sets,(max shr 3)+1);
  for i := 0 to n-1 do
    SetBit(sets[0],fTags[i]-1);
  if GetBit(sets[0],aTagID-1) then
    exit; // duplicated aTagID
  SetBit(sets[0],aTagID-1);
  SetLength(fTags,n+1);
  n := 0;
  for i := 0 to max-1 do
    if GetBit(sets[0],AlphabeticalOrder[i]) then begin
      fTags[n] := AlphabeticalOrder[i]+1;
      inc(n);
    end;
  assert(n=length(fTags));
end;


{ TSQLTag }

class function TSQLTag.ComputeTagIdentPerIDArray(aRest: TSQLRest;
  out AlphabeticalOrder: TCardinalDynArray): TRawUTF8DynArray;
var tag: TSQLTag;
    max: integer;
begin
  result := nil;
  tag := TSQLTag.CreateAndFillPrepare(aRest,'','ID,Ident');
  try
    if tag.FillTable.RowCount=0 then
      exit;
    max := 0;
    while tag.FillOne do
      if tag.ID>max then
        max := tag.ID;
    SetLength(result,max);
    tag.FillRewind;
    while tag.FillOne do
      result[tag.ID-1] := tag.Ident;
    SetLength(AlphabeticalOrder,max);
    FillIncreasing(pointer(AlphabeticalOrder),0,max);
    QuickSortIndexedPUTF8Char(pointer(result),max,AlphabeticalOrder);
  finally
    tag.Free;
  end;
end;

end.
