{$ifndef FPC}
  {$IFDEF CONDITIONALEXPRESSIONS}
    {$if CompilerVersion <= 17} // Delphi 2006 (and up) have FastMM4 integrated
      FastMM4,
    {$ifend}
  {$ELSE}
    FastMM4, // no conditional expressions -> versions older than Delphi 6
  {$ENDIF}
{$endif}

{$WARN DUPLICATE_CTOR_DTOR OFF} // global at .dpr level - see QC 100815

