unit ServFishShopMain;

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  SynCommons,
  mORMot,
  mORMotService;

type
  TFishShopSettings = class(TSynDaemonSettings)
  published
  end;

  TFishShopDaemon = class(TSynDaemon)
  public
    procedure Start; override;
    procedure Stop; override;
  end;


implementation

{ TFishShopDaemon }

procedure TFishShopDaemon.Start;
begin

end;

procedure TFishShopDaemon.Stop;
begin

end;

end.
