{$ifndef LVCL}
{$ifdef FPC} // we may be on Kylix or upcoming Delphi for Linux
  {$ifdef Unix} // we may also be on Darwin / OSX  
  cthreads, // if you use threads: always needed for servers
  cwstring, // optional WideString manager, but won't hurt
  {$endif}
{$else}  // enable FastMM4 on older versions of Delphi
  {$IFDEF CONDITIONALEXPRESSIONS}
    {$if CompilerVersion <= 17} // Delphi 2006 (and up) have FastMM4 integrated
      FastMM4,
    {$ifend}
    {$ifdef UNICODE}
    {$WARN DUPLICATE_CTOR_DTOR OFF} // global at .dpr level - see QC 100815
    {$endif}
  {$ELSE}
    FastMM4, // no conditional expressions -> versions older than Delphi 6
  {$ENDIF}
{$endif FPC}
{$endif LVCL}

