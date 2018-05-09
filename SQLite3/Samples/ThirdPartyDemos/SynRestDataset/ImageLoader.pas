unit ImageLoader;

// Image Loader: a helpler class for loading Image from stream into TPicture.
// Implemented by Dewen HOU (HOUDW2006) and it is open source.

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  Classes, ExtCtrls, SysUtils,
  Graphics,
  {$ifdef ISDELPHIXE2}
  vcl.Imaging.jpeg, vcl.Imaging.pngImage, Vcl.Imaging.GIFImg,
  {$ELSE}
  jpeg,
  {$ENDIF}
  SynCommons, mORMot;  // TSQLRawBlob

type
  TImageType = (itNone=0, itBmp, itJpg, itPng, itGif,
    itPcx, itTiff, itRas, itPsd, itSgi);

  TImageLoader = class(TObject)
  private
    aBmp: TBitmap;
    aJpg: TJPEGImage;
    {$ifdef ISDELPHIXE2}
    aGif: TGifImage;
    aPng: TPngImage;
    {$ENDIF}
  protected
    function GuessImageType(aStream: TStream): TImageType;
    function TryLoadGraphic(aImage: TPicture; aStream: TStream; aGraphic: TGraphic): Boolean;

    function TryLoadBmp(aImage: TPicture; aStream: TStream): Boolean;
    function TryLoadJpg(aImage: TPicture; aStream: TStream): Boolean;
    {$ifdef ISDELPHIXE2}
    function TryLoadGif(aImage: TPicture; aStream: TStream): Boolean;
    function TryLoadPng(aImage: TPicture; aStream: TStream): Boolean;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;

    function LoadImage(aImage: TPicture; aStream: TStream): Boolean; overload;
    function LoadImage(aImage: TPicture; const aBlobData: TSQLRawBlob): Boolean; overload;
  end;

implementation

{ TImageLoader }

constructor TImageLoader.Create;
begin
  inherited;

  aBmp := TBitmap.Create;
  aJpg := TJPEGImage.Create;
  {$ifdef ISDELPHIXE2}
  aGif := TGifImage.Create;
  aPng := TPngImage.Create;
  {$ENDIF}
end;

destructor TImageLoader.Destroy;
begin
  aBmp.Free;
  aJpg.Free;
  {$ifdef ISDELPHIXE2}
  aGif.Free;
  aPng.Free;
  {$ENDIF}

  inherited;
end;

function TImageLoader.GuessImageType(aStream: TStream): TImageType;
var
  Buffer: Word;
begin
  Result := itNone;

  if aStream.Size >= 2 then
  try
    // guess the Image type from the first 2 bytes of image header
    aStream.Position := 0;
    aStream.ReadBuffer(Buffer, 2);
    case Buffer of
      $4D42: Result := itBmp;
      $D8FF: Result := itJpg;
      $4947: Result := itGif;
      $5089: Result := itPng;
      $4949: Result := itTiff;
      $050A: Result := itPcx;
      $4238: Result := itPsd;
      $A659: Result := itRas;
      $DA01: Result := itSgi;
    end;
  finally
    aStream.Position := 0;  // restore the position of stream
  end;
end;

function TImageLoader.LoadImage(aImage: TPicture; const aBlobData: TSQLRawBlob): Boolean;
var
  fStream: TMemoryStream;
begin
  fStream := TMemoryStream.Create;
  try
    fStream.Write(Pointer(aBlobData)^, Length(aBlobData));

    Result := LoadImage(aImage, fStream);
  finally
    fStream.Free;
  end;
end;

function TImageLoader.LoadImage(aImage: TPicture; aStream: TStream): Boolean;
begin
  if Assigned(aImage.Graphic) then
    aImage.Graphic := nil;

  case GuessImageType(aStream) of
    itBmp: Result := TryLoadBmp(aImage, aStream);
    itJpg: Result := TryLoadJpg(aImage, aStream);
    {$ifdef ISDELPHIXE2}
    itGif: Result := TryLoadGif(aImage, aStream);
    itPng: Result := TryLoadPng(aImage, aStream);
    {$ENDIF}
    else Result := False;
  end;
end;

function TImageLoader.TryLoadBmp(aImage: TPicture; aStream: TStream): Boolean;
begin
  Result := TryLoadGraphic(aImage, aStream, aBmp);
end;

function TImageLoader.TryLoadJpg(aImage: TPicture; aStream: TStream): Boolean;
begin
  Result := TryLoadGraphic(aImage, aStream, aJpg);
end;

{$ifdef ISDELPHIXE2}
function TImageLoader.TryLoadGif(aImage: TPicture; aStream: TStream): Boolean;
begin
  Result := TryLoadGraphic(aImage, aStream, aGif);
end;

function TImageLoader.TryLoadPng(aImage: TPicture; aStream: TStream): Boolean;
begin
  Result := TryLoadGraphic(aImage, aStream, aPng);
end;
{$ENDIF}

function TImageLoader.TryLoadGraphic(aImage: TPicture; aStream: TStream;
  aGraphic: TGraphic): Boolean;
begin
  Result := True;
  try
    aStream.Position := 0;
    aGraphic.LoadFromStream(aStream);
    aImage.Assign(aGraphic);
  except on E: Exception  do
    Result := False;
  end;
end;

end.