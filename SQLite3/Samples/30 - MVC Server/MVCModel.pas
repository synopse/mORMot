/// database Model for the MVCServer BLOG sample
unit MVCModel;

interface

uses
  SynCommons,
  SynCrypto,
  mORMot;

type
  TSQLBlogInfo = class(TSQLRecord)
  private
    fCopyright: RawUTF8;
    fTitle: RawUTF8;
  published
    property Title: RawUTF8 index 80 read fTitle write fTitle;
    property Copyright: RawUTF8 index 80 read fCopyright write fCopyright;
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
  published
    property Title: RawUTF8 index 80 read fTitle write fTitle;
    property Content: RawUTF8 read fContent write fContent;
  end;

  TSQLCategory = class(TSQLRecord)
  private
    fIdent: RawUTF8;
  published
    property Ident: RawUTF8 read fIdent write fIdent;
  end;

  TSQLArticle = class(TSQLContent)
  private
    fAuthor: TSQLAuthor;
  published
    property Author: TSQLAuthor read fAuthor write fAuthor;
  end;

  TSQLComment = class(TSQLContent)
  private
    fCommentor: TSQLAuthor;
    fArticle: TSQLArticle;
  published
    property Commentor: TSQLAuthor read fCommentor write fCommentor;
    property Article: TSQLArticle read fArticle write fArticle;
  end;

function CreateModel: TSQLModel;


implementation

function CreateModel: TSQLModel;
begin
  result := TSQLModel.Create([TSQLBlogInfo,TSQLCategory,TSQLAuthor,
    TSQLArticle,TSQLComment],'blog');
  TSQLArticle.AddFilterOrValidate('Title',TSynFilterTrim.Create);
  TSQLArticle.AddFilterOrValidate('Title',TSynValidateText.Create);
  TSQLArticle.AddFilterOrValidate('Content',TSynFilterTrim.Create);
  TSQLArticle.AddFilterOrValidate('Content',TSynValidateText.Create);
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


end.
