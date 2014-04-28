{ ComPort Library global definitions }

{$B-}
{$X+}
{$H+}
{$R-}
{$Q-}

{$IFDEF VER110}     { C++ Builder 3 }
  {$ObjExportAll On}
{$ENDIF}

{$IFDEF VER120}     { Delphi 4 }
  {$DEFINE DELPHI_4_OR_HIGHER}
  {$DEFINE DELPHI_4}
{$ENDIF}

{$IFDEF VER125}     { C++ Builder 4 }
  {$DEFINE DELPHI_4_OR_HIGHER}
  {$DEFINE DELPHI_4}
  {$ObjExportAll On}
{$ENDIF}

{$IFDEF VER130}     { Delphi 5 and C++ Builder 5 }
  {$DEFINE DELPHI_4_OR_HIGHER}
  {$DEFINE DELPHI_5_OR_HIGHER}
  {$DEFINE DELPHI_5}
  {$IFDEF BCBNOTDELPHI}
    {$ObjExportAll On}
  {$ENDIF}
{$ENDIF}

{$IFDEF CONDITIONALEXPRESSIONS}  // Delphi 6 or newer
  {$DEFINE VER140}
  {$if CompilerVersion >= 17}
    {$define HASINLINE}     // Delphi 2005 or newer
  {$ifend}
{$ENDIF}

{$IFDEF VER140}     { Delphi 6 }
  {$DEFINE DELPHI_4_OR_HIGHER}
  {$DEFINE DELPHI_5_OR_HIGHER}
  {$DEFINE DELPHI_6_OR_HIGHER}
  {$DEFINE DELPHI_6}
  {$IFDEF BCBNOTDELPHI}
    {$ObjExportAll On}
  {$ENDIF}
{$ENDIF}
