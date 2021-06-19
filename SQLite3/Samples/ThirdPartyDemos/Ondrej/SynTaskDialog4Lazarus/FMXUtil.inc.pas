{$IF (CompilerVersion <= 25)}
type
    //other name of constants in XE4
    TStyledSettingHelper = record helper for TStyledSetting
        const Family = TStyledSetting.ssFamily;
        const Size = TStyledSetting.ssSize;
        const Style = TStyledSetting.ssStyle;
        const FontColor = TStyledSetting.ssFontColor;
        const Other = TStyledSetting.ssOther;
    end;

    TTextAlignHelper = record helper for TTextAlign
        const Center = TTextAlign.taCenter;
        const Leading = TTextAlign.taLeading;
        const Trailing = TTextAlign.taTrailing;
    end;

    TFmxFormBorderStyleHelper = record helper for TFmxFormBorderStyle
        const None = TFmxFormBorderStyle.bsNone;
        const Single = TFmxFormBorderStyle.bsSingle;
        const Sizeable = TFmxFormBorderStyle.bsSizeable;
        const ToolWindow = TFmxFormBorderStyle.bsToolWindow;
        const SizeToolWin = TFmxFormBorderStyle.bsSizeToolWin;
    end;

    TFormPositionHelper = record helper for TFormPosition
        //(poDesigned, poDefault, poDefaultPosOnly, poDefaultSizeOnly,
        //poScreenCenter, poDesktopCenter, poMainFormCenter, poOwnerFormCenter);
        const OwnerFormCenter = TFormPosition.poOwnerFormCenter;
        const ScreenCenter = TFormPosition.poScreenCenter;
    end;

    TBrushKindHelper = record helper for TBrushKind
        //(bkNone, bkSolid, bkGradient, bkBitmap, bkResource);
        const None = TBrushKind.bkNone;
    end;

    TAlignLayoutHelper = record helper for TAlignLayout
        //(alNone, alTop, alLeft, alRight, alBottom, alMostTop, alMostBottom, alMostLeft, alMostRight, alClient,
        //alContents, alCenter, alVertCenter, alHorzCenter, alHorizontal, alVertical, alScale, alFit, alFitLeft, alFitRight);
        const Top = TAlignLayout.alTop;
    end;

{$IFEND}

var
  _ScreenDPI_X : Single = 0;

function ScalingByScreenDPI_N( F:TForm = NIL ):Single;
var
  p : TPointF;
  {$IF (CompilerVersion >= 28)}
  M : TDeviceDisplayMetrics;
  {$IFEND}
  i : integer;
  h : THandle;
begin
  if _ScreenDPI_X > 0 then
  begin
    Result := _ScreenDPI_X;
    Exit;
  end else
    Result := 1;

  {$IFDEF MSWINDOWS}
  if F <> NIL then
  begin
    h := GetWindowDC( WindowHandleToPlatform( F.Handle ).Wnd );
    i := GetDeviceCaps( h, LOGPIXELSX );
    if i >= 72 then
    begin
      Result := i / 96;
      _ScreenDPI_X := Result;
      Exit;
    end;
  end;
 {$ENDIF}

  {$IF (CompilerVersion >= 28)}   //TDeviceDisplayMetrics is available since XE8
  if TPlatformServices.Current.SupportsPlatformService( IFMXDeviceMetricsService ) then
  begin
    M := (TPlatformServices.Current.GetPlatformService(
        IFMXDeviceMetricsService) as IFMXDeviceMetricsService).GetDisplayMetrics;
    if M.PixelsPerInch >= 72 then
      Result := M.PixelsPerInch / {$IFDEF MACOS}110{$ENDIF}
                                  {$IFDEF MSWINDOWS}96{$ENDIF}
                                  ;
  end;
  {$IFEND}
end;

function ScalingByScreenDPI( F:TForm = NIL ):TPointF;
begin
  Result.X := ScalingByScreenDPI_N(F);
  Result.Y := Result.X;
end;

procedure inc( var F:Single; D:Single ); overload;
begin
  F := F+D;
end;

procedure dec( var F:Single; D:Single ); overload;
begin
  F := F-D;
end;

function FMXMeasureText( s:string; C : TTextControl; MaxWidth : Single; WordWrap : boolean ):TRectF;
var
  R : TRectF;
begin
  if MaxWidth = 0 then
    MaxWidth := 2000;
  R := RectF( 0,0,MaxWidth,2000 );
  C.StyledSettings := C.StyledSettings - [ TStyledSetting.Size ];
  C.Canvas.Font.Assign( C.Font );
  C.Canvas.MeasureText( R, s, WordWrap, [], TTextAlign.Leading, TTextAlign.Leading );
  Result := R;
end;
