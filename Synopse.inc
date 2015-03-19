{
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2011 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2015
  the Initial Developer. All Rights Reserved.

  Contributor(s):
   Alfred Glaenzer (alf)
  
  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

  Version 1.7
  - first public release, corresponding to SQLite3 Framework 1.7

  Version 1.8
  - force no Range Checking and other compilation settings

  Version 1.10
  - code modifications to support Delphi 5 / Delphi 6 compilers

  Version 1.11
  - allow to fix Delphi 2009 specific compilation issues :(

  Version 1.13
  - updated conditionals for FPC
  - code modifications for Delphi 5 compiler
  - new WITHLOG conditional (defined by default)

  Version 1.15
  - now handles Delphi XE2 (32 Bit)

  Version 1.16
  - added USEPACKAGES conditional to help compiling the unit within packages
  - added ISDELPHIXE conditional for fixing some compilation warnings
  - added DOPATCHTRTL conditional (not set by default, for compatibility)

  Version 1.18
  - added NOX64PATCHRTL conditional (unset by default, for better speed)
  - added SQLITE3_FASTCALL conditional (shared by SQLite3 related units)
  - added NEWRTTINOTUSED conditional (unset by default, for compatibility) - see
    http://synopse.info/forum/viewtopic.php?id=1394
  - enhanced FPC compatibility
  - now handles Delphi XE3, XE4, XE5, XE6 and XE7 (32 and 64 bit)

}

{.$define PUREPASCAL}
// define this if your compiler doesn't support Delphi's x86 asm
// - is set automaticaly in case of a 64 bits compiler (only FPC exists now)

{$define USENORMTOUPPER}
// if defined, text functions will use the NormToUpper[] array, as defined
// in our custom SysUtils.pas (not the LVCL version) -> when using LVCL,
// define the global LVCL compiler directive, and this unit will initialize
// its own NormToUpper[] array
// -> define ENHANCEDRTL conditional below if our Enhanced RTL IS installed
// -> in practice, this conditional is ALWAYS DEFINED, since needed by SQLite3

{.$define ENHANCEDRTL}
{ define this if you DID install our Enhanced Runtime library or the LVCL:
  - it's better to define this conditional globaly in the Project/Options window
  - we need to hack the "legacy" LoadResString() procedure and add a
    LoadResStringTranslate() function, for on the fly resourcestring i18n
  - it will also define the TwoDigitLookup[] array and some very fast x86 asm
    IntToStr() and other functions, available in our Enhanced Runtime library
    (and our LVCL library)
  - it will be unset automaticaly (see below) for Delphi 2009 and up
  - this conditional must be defined in both SQLite3Commons and SQLite3i18n units,
    or (even better) globally in the Project options }

{.$define USEPACKAGES}
{ define this if you compile the unit within a Delphi package
  - it will avoid error like "[DCC Error] E2201 Need imported data reference ($G)
    to access 'VarCopyProc' from unit 'SynCommons'"
  - shall be set at the package options level, and left untouched by default }

{$define WITHLOG}
// if defined, logging will be supported via the TSQLLog family
// - by default, should be left defined: undefine this only if you have
// performance issue (but TSQLog.Family.Level default setting won't log nothing,
// so there could be a very unnoticable performance penalty having this defined)

{.$define DOPATCHTRTL}
// if defined, the low-level patches made to RecordCopy() low-level function
// as defined in SynCommons.pas will be applied (if applicable to your Delphi
// version) - you should better use it, but we have unset it by default

{.$define NOX64PATCHRTL}
// if defined, with Win64 Delphi XE2+ compilers, FillChar() and Move() WON'T use
// optimized x64 assembler implementation instead of slower purepascal version
// - is not defined by default, since speed gain is noticeable

{.$define NEWRTTINOTUSED}
// if defined, the new RTTI (available since Delphi 2010) won't be linked to
// the executable: resulting file size will be much smaller, and mORMot won't
// be affected (unless you use the enhanced RTTI for record/dynamic array JSON
// serialization) - left undefined by default to ensure minimal impact

{.$define NOSETTHREADNAME}
// if defined, SetThreadName() would not raise the exception used to set the
// thread name: to be defined if you have issues when debugging your application

{.$define OLDTEXTWRITERFORMAT}
// force TTextWriter.Add(Format) to handle the alternate $ % tags

{$ifdef LVCL}
   // NormToUpper[] exists only in our enhanced RTL
  {$define OWNNORMTOUPPER}
  // LVCL does not support variants
  {$define NOVARIANTS}
{$endif}

{$ifdef UNICODE}
  {$undef ENHANCEDRTL} // Delphi 2009 and up don't have our Enhanced Runtime library
  {$define HASVARUSTRING}
{$endif}


{$define INCLUDE_FTS3}
{ define this if you want to include the FTS3/FTS4 feature into the library
  - FTS3 is an SQLite module implementing full-text search
  - will include also FTS4 extension module since 3.7.4
  - see http://www.sqlite.org/fts3.html for documentation
  - is defined by default, but can be unset by defining EXCLUDE_FTS3 conditional
    to save about 50 KB of code size (is it worth it nowdays?)
  - should be defined for SynSQLite3, SynSQLite3Static and mORMotSQLite3 units,
    so a global condition in this Synopse.inc does make sense }

{$ifdef EXCLUDE_FTS3}
  {$undef INCLUDE_FTS3}
{$endif}

{ Free Pascal adaptation notes:
  - we use the Delphi compatibility mode
  - from system.pp use these constants (Win32/64 values listed):
      LineEnding = #13#10;
      DirectorySeparator = '\';
  - for Cross-Platform and all CPU:
      integer is NOT CPU-dependent (thanks to objpas), i.e. always 32 bits
      cardinal is NOT CPU-dependent (thanks to objpas), i.e. always 32 bits
      PtrUInt is an unsigned integer type of same size as a pointer / register
        -> to be used in loops
        -> to be used for pointer arithmetic
      PtrInt is a signed integer type of same size as a pointer / register
        -> to be used in loops
      all 32 bits x86 asm code can be replaced by a pascal only version, with
      if the conditional define PUREPASCAL is set (defined below e.g. for CPUX64)
}

{$ifdef FPC}

  {$MODE DELPHI} // e.g. for asm syntax - disabled for FPC 2.6 compatibility
  {$INLINE ON}
  {$MINENUMSIZE 1}
  {$PACKSET 1}
  {$PACKENUM 1}
  {$undef ENHANCEDRTL}    // there is no version of our Enhanced RTL for FPC
  {$undef DOPATCHTRTL}
  {$define USETYPEINFO}  // will use SynFPCTypInfo.pas wrapper
  {$define HASINLINE}
  {$define NODELPHIASM}   // ignore low-level System.@LStrFromPCharLen calls
  {$define NOX64PATCHRTL} // official FPC's RTL already includes this x64 asm

  {$ifdef ANDROID}
    {$define LINUX}
  {$endif}
  {$ifdef DARWIN}
    {$define LINUX}
    {$define PUREPASCAL} // e.g. low-level stack layout differs  
  {$endif}

  {$ifdef CPU64}
    {$define PUREPASCAL}
  {$else}
    {$ifdef CPUARM}
    {$define PUREPASCAL}
    {$else}
    {$ASMMODE INTEL} // as Delphi expects, and I use to write
    {$endif}
    // FPC has its own RTTI layout only since late 3.x
    // when http://bugs.freepascal.org/view.php?id=26774 has been fixed
    {.$define HASINTERFACERTTI}
  {$endif CPU64}

  {$define FPC_OR_PUREPASCAL}
  {$define FPC_OR_KYLIX}
  // exceptions interception code in FPC differs from Delphi
  {$define NOEXCEPTIONINTERCEPT}

  {$ifdef VER2_7}
    {$define ISFPC27}
  {$endif}
  {$ifdef VER3_0}
    {$define ISFPC27}
  {$endif}
  {$ifdef VER3_1}
    {$define ISFPC27}
  {$endif}
  {$ifdef ISFPC27}
    {$define ISFPC271}
    {$define HASVARUSTRING}
    {$define HASVARUSTRARG}
    // defined if the http://mantis.freepascal.org/view.php?id=26773 bug is fixed
    // you should use 2.7.1/trunk branch in revision 28995 from 2014-11-05T22:17:54
    // => this will change the TInvokeableVariantType.SetProperty() signature
    {$define FPC_VARIANTSETVAR}
  {$endif}

{$else FPC}

  {$ifdef CPUX64}
    {$define CPU64} // Delphi compiler for 64 bit CPU
    {$define CPU64DELPHI}
    {$undef CPU32}
    {$define PUREPASCAL}   // no x86 32 bit asm to be used
    {$warn DUPLICATE_CTOR_DTOR OFF} // avoid W1029 unneeded hints
  {$else CPUX64}
    {$define CPU32} // Delphi compiler for 32 bit CPU
    {$undef CPU64}
  {$endif CPUX64}

  // defines if exceptions shall not be intercepted
  {.$define NOEXCEPTIONINTERCEPT}

  {$IFDEF CONDITIONALEXPRESSIONS}  // Delphi 6 or newer
    {$define HASINTERFACERTTI} // interface RTTI (not FPC)
    {$ifdef LINUX}
      {$if RTLVersion = 14.5}
        {$define KYLIX3}
        {$define FPC_OR_KYLIX}
        // Kylix 3 will be handled just like Delphi 7
        {$undef ENHANCEDRTL}   // Enhanced Runtime library not fully tested yet
        {$define DOPATCHTRTL}  // nice speed up for server apps
        {$define NOVARCOPYPROC}
        {$define NOSQLITE3STATIC} // Kylix will use external sqlite3.so
      {$else}
      Kylix1/2 are unsupported
      {$ifend}
    {$else}
      {$ifdef VER140} // Delphi 6
        {$define DELPHI6OROLDER}
        {$define NOVARCOPYPROC}
        {$undef ENHANCEDRTL} // Delphi 6 doesn't have our Enhanced Runtime library
      {$else}
        {$define WITHUXTHEME} // Delphi 7 and up handle UI theming
      {$endif}
    {$endif LINUX}
    {$if CompilerVersion >= 17}
    {$ifend}
    {$if CompilerVersion >= 18}
      {$define HASNEWFILEAGE} // Delphi 2006 or newer
      {$define HASINLINE}
      {$define HASREGION}
    {$ifend}
    {$if CompilerVersion > 18}
      {$define ISDELPHI2007ANDUP} // Delphi 2007 or newer
    {$ifend}
    {$if CompilerVersion = 20}
      {$define ISDELPHI2009} // Delphi 2009 has specific compilation issues :(
      // for Delphi 2009 and up, use UNICODE conditional :)
    {$ifend}
    {$if CompilerVersion >= 21.0}
      // Delphi 2010/XE: Reduce EXE size by disabling much RTTI
      {$define ISDELPHI2010}
      {$ifdef NEWRTTINOTUSED}
        {$WEAKLINKRTTI ON}
        {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
      {$endif NEWRTTINOTUSED}
    {$ifend}
    {$if CompilerVersion >= 22.0}
      {$define ISDELPHIXE}
    {$ifend}
    {$if CompilerVersion >= 23.0}
      // Delphi XE2 has some cross-platform features
      // e.g. {$ifdef ISDELPHIXE2}VCL.Graphics{$else}Graphics{$endif}
      {$define ISDELPHIXE2}
      {$define HASVARUSTRARG}
    {$ifend}
    {$if CompilerVersion >= 24.0}
      {$define ISDELPHIXE3}
    {$ifend}
    {$if CompilerVersion >= 25.0}
      {$define ISDELPHIXE4}
    {$ifend}
    {$if CompilerVersion >= 26.0}
      {$define ISDELPHIXE5}
      {$define PUBLISHRECORD}
      // if defined, will handle RTTI available only since Delphi XE5 for
      // record published properties 
    {$ifend}
    {$if CompilerVersion >= 27.0}
      {$define ISDELPHIXE6}
    {$ifend}
    {$if CompilerVersion >= 28.0}
      {$define ISDELPHIXE7}
    {$ifend}
    {$if CompilerVersion >= 29.0}
      {$define ISDELPHIXE8}
    {$ifend}
  {$ELSE}
    // Delphi 5 or older
    {$define DELPHI6OROLDER}
    {$define DELPHI5OROLDER}
    {$define MSWINDOWS}
    {$define NOVARIANTS}
    {$define NOVARCOPYPROC}
    {$undef ENHANCEDRTL} // Delphi 5 doesn't have our Enhanced Runtime library
    {$undef DOPATCHTRTL}
  {$ENDIF}

{$endif FPC}

{$ifdef PUREPASCAL}
  {$define NODELPHIASM}
  {$define FPC_OR_PUREPASCAL}
  {$undef DOPATCHTRTL}
  {$ifdef CPU64DELPHI}
    {$define NOTPUREPASCALNORCPU64DELPHI}
  {$endif CPU64DELPHI}
{$else}
  {$define NOTPUREPASCALNORCPU64DELPHI}
{$endif PUREPASCAL}

{$R-} // disable Range checking in our code
{$S-} // disable Stack checking in our code
{$X+} // expect extended syntax
{$W-} // disable stack frame generation
{$Q-} // disable overflow checking in our code
{$B-} // expect short circuit boolean
{$V-} // disable Var-String Checking
{$T-} // Typed @ operator
{$Z1} // enumerators stored as byte by default
{$IFNDEF FPC}
  {$P+} // Open string params
{$ENDIF FPC}

{$ifdef VER150}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_CAST OFF}
{$ENDIF}

{$ifdef CONDITIONALEXPRESSIONS}  // Delphi 6 or newer
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$endif}

// see http://synopse.info/fossil/tktview?name=6593f0fbd1
{$ifndef WITHUXTHEME}
  {$define EXTENDEDTOSTRING_USESTR} // no TFormatSettings before Delphi 6
{$endif}
{$ifdef LVCL}
  {$define EXTENDEDTOSTRING_USESTR} // no FloatToText implemented in LVCL
{$endif}
{$ifdef FPC}
  {$define EXTENDEDTOSTRING_USESTR} // FloatToText uses str() in FPC
{$endif}
{$ifdef CPU64}
  {$define EXTENDEDTOSTRING_USESTR} // FloatToText() much slower in x64 mode
{$endif}

// global conditional to use SQLite3 with fastcall calling convention
// used by SynSQLite3, SynSQLite3Static, SynSQlite3RegEx and mORMotSQLite3 units
{$ifdef CPU64}
  // only one calling convention in the Win64 world
  {$define SQLITE3_FASTCALL}
{$else}
  // undefined by default: BCC32 -pr fastcall (=Delphi resgister) is broken
  // because of issues with BCC32 itself, or some obfuscated calls in source?
  // -> allow to use external SQlite3 libraries in addition to static version
  {.$define SQLITE3_FASTCALL}
{$endif}

// Only Win32+Linux32 do support our SQLite3 static linked library yet  
{$ifdef CPU64}
  {$define NOSQLITE3STATIC}
{$endif}
{$ifdef ANDROID}
  {$define NOSQLITE3STATIC}
{$endif}
{$ifdef DARWIN}
  {$define NOSQLITE3STATIC}
{$endif}

{$ifdef NOSQLITE3STATIC}
  // our proprietary crypto expects a patched SQlite3.c statically linked
  {$define NOSQLITE3ENCRYPT}
{$endif}

{$ifdef MSWINDOWS}
  /// define this to publish TWinINet / TWinHttp / TWinHttpAPI classes
  // and TSQLHttpClientWinINet / TSQLHttpClientWinHTTP classes 
  {$define USEWININET}
  // our current IOCP pattern is Windows-specific:
  //  with Thread Pool: 3394 requests / second (each request with 4 KB of data)
  //  without the Pool: 140/s in the IDE (i.e. one core), 2637/s on a dual core
  // but less needed under Linux, since thread creation sounds much cheaper
  // see http://www.akkadia.org/drepper/nptl-design.pdf
  {$define USETHREADPOOL}
  {.$define ONLYUSEHTTPSOCKET} // for testing (no benefit vs http.sys)
  {.$define USELIBCURL}     // for testing under Windows (no benefit vs WinHTTP)
{$else}
  // http.sys server is Windows-specific
  {$define ONLYUSEHTTPSOCKET}
  {$ifndef ANDROID}
  // cross-platform libcurl has a great API -> TCurlHttp and TSQLHttpClientCurl
  {$define USELIBCURL}
  {$endif}
{$endif}

