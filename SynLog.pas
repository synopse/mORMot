/// logging functions used by Synopse projects
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynLog;

(*
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2015 Arnaud Bouchez
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

  Version 1.18
  - first public release, extracted from SynCommons.pas unit
  - BREAKING CHANGE: PWinAnsiChar type for constant text format parameters has
    been changed into a RawUTF8, to please all supported platforms and compilers  
  - Delphi XE4/XE5/XE6/XE7/XE8 compatibility (Windows target platform only)
  - unit fixed and tested with Delphi XE2 (and up) 64-bit compiler under Windows
  - Exception logging and Stack trace do work now on Linux with Kylix/CrossKylix
  - added TSynLogFile.Freq read-only property
  - added DefaultSynLogExceptionToStr() function and TSynLogExceptionToStrCustom
    variable, and ESynException.CustomLog() method to customize how raised
    exception are logged when intercepted - feature request [495720e0b9]
  - added new sllDDDError, sllDDDInfo log levels
  - added TSynLogFamily.EndOfLineCRLF properties
  - added TSynLogFamily's NoFile and EchoCustom properties - see [91a114d2f6]
  - TSynLog will now append only the execution time when leaving a method,
    without the class/method name (smaller log file, and less resource use)
  - TSynLog header now contains system environment variables
  - added overloaded ISynLog.Log(string) method for Unicode Delphi
  - added TSynLog.DebuggerNotify() and TSynLog.CloseLogFile / Release methods
  - protected the global TSynLog instances list against potential race condition
  - introducing TSynLogFamily.StackTraceUse: TSynLogStackTraceUse property
    (set to stManualAndAPI by default, but stOnlyAPI within the Delphi IDE)
  - introducing TSynLogFamily.EchoToConsole: TSynLogInfos property, able to
    optionally echo the process log to the current console window, using colors
  - added TSynLogFamily.EchoRemoteStart() and EchoRemoteStop methods
  - added TSynLog.Void class function
  - if new property TSynLogFamily.PerThreadLog is set to ptIdentifiedInOnFile,
    a new column will be added for each logged row - LogViewer has been updated
    to allow easy and efficient multi-thread process logging
  - introducing TSynLogFamily.RotateFileCount and associated RotateFileSizeKB,
    RotateFileDailyAtHour and OnRotate properties, to enable log file rotation
    by size or at given hour - request [72feb66d45] + [b3e8cc8424]
  - added TSynLog.CustomFileName property - see [d8fbc10bf8]
  - added TSynLog.ComputeFileName virtual method and TSynLogFamily.FileExistsAction
    property for feature request [d029051dcb]
  - added TSynLog/ISynLog.LogLines() method for direct multi-line text logging
  - added optional TextTruncateAtLength parameter for TSynLog/ISynLog.Log()
  - declared TSynLog.LogInternal() methods as virtual - request [e47c64fb2c]
  - .NET/CLR external exceptions will now be logged with their C# type name
  - special 'SetThreadName' exception will now be ignored by TSynLog hook
  - fixed ticket [19e567b8ca] about TSynLog issue in heavily concurrent mode:
    now a per-thread context will be stored, e.g. for Enter/Leave tracking
  - fixed ticket [a516b1a954] about ptOneFilePerThread log file rotation
  - introduced clear distinction between absolute and relative memory address
    values, and TSynMapFile.AbsoluteToOffset(), as reported by [0aeaa1353149]
  - introduced ISynLogCallback and TSynLogCallbacks types for easy integration
    with mORMot's interface-based services real-time notification

*)


{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
{$ifdef MSWINDOWS}
  Windows,
  Messages,
{$endif}
{$ifdef KYLIX3}
  Types,
  LibC,
  SynKylix,
{$endif}
  Classes,
{$ifndef LVCL}
  SyncObjs, // for TEvent
  Contnrs,  // for TObjectList
{$ifdef HASINLINE}
  Types,
{$endif}
{$endif}
{$ifndef NOVARIANTS}
  Variants,
{$endif}
  SysUtils,
  SynLZ, // needed e.g. for TSynMapFile .mab format
  SynCommons;


{ ************ Logging classes and functions }

type
  /// a debugger symbol, as decoded by TSynMapFile from a .map file
  TSynMapSymbol = record
    /// symbol internal name
    Name: RawUTF8;
    /// starting offset of this symbol in the executable
    // - addresses are integer, since map be <0 in Kylix .map files
    Start: integer;
    /// end offset of this symbol in the executable
    // - addresses are integer, since map be <0 in Kylix .map files
    Stop: integer;
  end;
  PSynMapSymbol = ^TSynMapSymbol;
  /// a dynamic array of symbols, as decoded by TSynMapFile from a .map file
  TSynMapSymbolDynArray = array of TSynMapSymbol;

  /// a debugger unit, as decoded by TSynMapFile from a .map file
  TSynMapUnit = record
    /// Name, Start and Stop of this Unit
    Symbol: TSynMapSymbol;
    /// associated source file name
    FileName: RawUTF8;
    /// list of all mapped source code lines of this unit
    Line: TIntegerDynArray;
    /// start code address of each source code lin
    Addr: TIntegerDynArray;
  end;
  /// a dynamic array of units, as decoded by TSynMapFile from a .map file
  TSynMapUnitDynArray = array of TSynMapUnit;

  {$M+}
  /// retrieve a .map file content, to be used e.g. with TSynLog to provide
  // additional debugging information
  // - original .map content can be saved as .mab file in a more optimized format
  TSynMapFile = class
  protected
    fMapFile: TFileName;
    fSymbol: TSynMapSymbolDynArray;
    fUnit: TSynMapUnitDynArray;
    fSymbols: TDynArray;
    fUnits: TDynArrayHashed;
    fUnitSynLogIndex,fUnitSystemIndex: integer;
    fCodeOffset: PtrUInt;
    fHasDebugInfo: boolean;
  public
    /// get the available debugging information
    // - if aExeName is specified, will use it in its search for .map/.mab
    // - if aExeName is not specified, will use the currently running .exe/.dll
    // - it will first search for a .map matching the file name: if found,
    // will be read to retrieve all necessary debugging information - a .mab
    // file will be also created in the same directory (if MabCreate is TRUE)
    // - if .map is not not available, will search for the .mab file
    // - if no .mab is available, will search for a .mab appended to the .exe/.dll
    // - if nothing is available, will log as hexadecimal pointers, without
    // debugging information
    constructor Create(const aExeName: TFileName=''; MabCreate: boolean=true);
    /// save all debugging information in the .mab custom binary format
    // - if no file name is specified, it will be saved as ExeName.mab or DllName.mab
    // - this file content can be appended to the executable via SaveToExe method
    // - this function returns the created file name
    function SaveToFile(const aFileName: TFileName=''): TFileName;
    /// save all debugging informat in our custom binary format
    procedure SaveToStream(aStream: TStream);
    /// append all debugging information to an executable (or library)
    // - the executable name must be specified, because it's impossible to
    // write to the executable of a running process
    // - this method will work for .exe and for .dll (or .ocx)
    procedure SaveToExe(const aExeName: TFileName);
    /// add some debugging information about the supplied absolute memory address
    // - will create a global TSynMapFile instance for the current process, if
    // necessary
    // - if no debugging information is available (.map or .mab), will write
    // the raw address pointer as hexadecimal
    class procedure Log(W: TTextWriter; aAddressAbsolute: PtrUInt;
      AllowNotCodeAddr: boolean);
    /// compute the relative memory address from its absolute (pointer) value
    function AbsoluteToOffset(aAddressAbsolute: PtrUInt): integer;
    /// retrieve a symbol according to a relative code address
    function FindSymbol(aAddressOffset: integer): integer;
    /// retrieve an unit and source line, according to a relative code address
    function FindUnit(aAddressOffset: integer; out LineNumber: integer): integer;
    /// return the symbol location according to the supplied absolute address
    // - i.e. unit name, symbol name and line number (if any), as plain text
    // - returns '' if no match found
    function FindLocation(aAddressAbsolute: PtrUInt): RawUTF8;
    /// all symbols associated to the executable
    property Symbols: TSynMapSymbolDynArray read fSymbol;
    /// all units, including line numbers, associated to the executable
    property Units: TSynMapUnitDynArray read fUnit;
  published
    /// the associated file name
    property FileName: TFileName read fMapFile;
    /// equals true if a .map or .mab debugging information has been loaded
    property HasDebugInfo: boolean read fHasDebugInfo;
  end;
  {$M-}

  {$M+} { we need the RTTI for the published methods of the logging classes }

  TSynLog = class;

  /// class-reference type (metaclass) of a TSynLog family
  // - since TSynLog classes store their information per type, you usually
  // will store a reference to a logging family (i.e. logging settings) using
  // a TSynLogClass variable, whereas TSynLog would point to the active logging
  // instance
  TSynLogClass = class of TSynLog;

  TSynLogFamily = class;
  TSynLogFile = class;
  
  {$M-}

  /// a generic interface used for logging a method
  // - you should create one TSynLog instance at the beginning of a block code
  // using TSynLog.Enter: the ISynLog will be released automaticaly by the
  // compiler at the end of the method block, marking it's executation end
  // - all logging expect UTF-8 encoded text, i.e. usualy English text
  ISynLog = interface(IUnknown)
    ['{527AC81F-BC41-4717-B089-3F74DE56F1AE}']
    /// call this method to add some information to the log at a specified level
    // - see the format in TSynLog.Log() method description
    // (not compatible with default SysUtils.Format function)
    // - if Instance is set, it will log the corresponding class name and address
    // (to be used if you didn't call TSynLog.Enter() method first)
    // - note that cardinal values should be type-casted to Int64() (otherwise
    // the integer mapped value will be transmitted, therefore wrongly)
    procedure Log(Level: TSynLogInfo; const TextFmt: RawUTF8; const TextArgs: array of const;
      Instance: TObject=nil); overload;
    /// call this method to add some information to the log at a specified level
    // - if Instance is set and Text is not '', it will log the corresponding
    // class name and address (to be used e.g. if you didn't call TSynLog.Enter()
    // method first)
    // - if Instance is set and Text is '', will behave the same as
    // Log(Level,Instance), i.e. write the Instance as JSON content
    procedure Log(Level: TSynLogInfo; const Text: RawUTF8;
      Instance: TObject=nil; TextTruncateAtLength: integer=maxInt); overload;
    {$ifdef UNICODE}
    /// call this method to add some VCL string to the log at a specified level
    // - this overloaded version will avoid a call to StringToUTF8()
    procedure Log(Level: TSynLogInfo; const Text: string; Instance: TObject=nil); overload;
    {$endif}
    /// call this method to add the content of an object to the log at a
    // specified level
    // - TSynLog will write the class and hexa address - TSQLLog will write the
    // object JSON content
    procedure Log(Level: TSynLogInfo; Instance: TObject); overload;
    /// call this method to add the content of most low-level types to the log
    // at a specified level
    // - TSynLog will handle enumerations and dynamic array; TSQLLog will be
    // able to write TObject/TSQLRecord and sets content as JSON
    procedure Log(Level: TSynLogInfo; const aName: RawUTF8;
      aTypeInfo: pointer; var aValue; Instance: TObject=nil); overload;
    /// call this method to add the caller address to the log at the specified level
    // - if the debugging info is available from TSynMapFile, will log the
    // unit name, associated symbol and source code line
    procedure Log(Level: TSynLogInfo=sllTrace); overload;
    /// call this method to add some multi-line information to the log at a
    // specified level
    // - LinesToLog content will be added, one line per one line, delimited
    // by #13#10 (CRLF)
    // - if a line starts with IgnoreWhenStartWith (already uppercase), it won't
    // be added to the log content (to be used e.g. with '--' for SQL statements)
    procedure LogLines(Level: TSynLogInfo; LinesToLog: PUTF8Char;
      aInstance: TObject=nil; const IgnoreWhenStartWith: PAnsiChar=nil);
    /// retrieve the associated logging instance
    function Instance: TSynLog;
  end;

  /// a mORMot-compatible calback definition
  // - used to notify a remote mORMot server via interface-based serivces
  // for any incoming event
  ISynLogCallback = interface(IInvokable)
    ['{9BC218CD-A7CD-47EC-9893-97B7392C37CF}']
    /// each line of the TTextWriter internal instance will trigger this method
    // - the format is similar to TOnTextWriterEcho, as defined in SynCommons
    procedure Log(Level: TSynLogInfo; const Text: RawUTF8);
  end;

  /// store a subscribe to ISynLogCallback
  TSynLogCallback = record
    Levels: TSynLogInfos;
    Callback: ISynLogCallback;
  end;
  /// store the all subscribed ISynLogCallback
  TSynLogCallbackDynArray = array of TSynLogCallback;

  /// can manage a list of ISynLogCallback registrations
  TSynLogCallbacks = class(TSynPersistentLocked)
  protected
    fCount: integer;
  public
    /// direct access to the registration storage
    Registration: TSynLogCallbackDynArray;
    /// high-level access to the registration storage
    Registrations: TDynArray;
    /// the TSynLog family actually associated with those callbacks
    TrackedLog: TSynLogFamily;
    /// initialize the registration storage for a given TSynLogFamily instance
    constructor Create(aTrackedLog: TSynLogFamily); reintroduce;
    /// finalize the registration storage for a given TSynLogFamily instance
    destructor Destroy; override;
    /// register a callback for a given set of log levels
    procedure Subscribe(const Levels: TSynLogInfos;
      const Callback: ISynLogCallback); virtual;
    /// unregister a callback previously registered by Subscribe()
    procedure Unsubscribe(const Callback: ISynLogCallback); virtual;
    /// notify a given log event
    // - matches the TOnTextWriterEcho signature
    function OnEcho(Sender: TTextWriter; Level: TSynLogInfo;
      const Text: RawUTF8): boolean;
  published
    /// how many registrations are currently defined
    property Count: integer read fCount;
  end;

  /// this event can be set for a TSynLogFamily to archive any deprecated log
  // into a custom compressed format
  // - will be called by TSynLogFamily when TSynLogFamily.Destroy identify
  // some outdated files
  // - the aOldLogFileName will contain the .log file with full path
  // - the aDestinationPath parameter will contain 'ArchivePath\log\YYYYMM\'
  // - should return true on success, false on error
  // - example of matching event handler are EventArchiveDelete/EventArchiveSynLZ
  // or EventArchiveZip in SynZip.pas
  // - this event handler will be called one time per .log file to archive,
  // then one last time with aOldLogFileName='' in order to close any pending
  // archive (used e.g. by EventArchiveZip to open the .zip only once)
  TSynLogArchiveEvent = function(const aOldLogFileName, aDestinationPath: TFileName): boolean;

  /// this event can be set for a TSynLogFamily to customize the file rotation
  // - will be called by TSynLog.PerformRotation
  // - should return TRUE if the function did process the file name
  // - should return FALSE if the function did not do anything, so that the
  // caller should perform the rotation as usual  
  TSynLogRotateEvent = function(aLog: TSynLog; const aOldLogFileName: TFileName): boolean;

  /// how threading is handled by the TSynLogFamily
  // - by default, ptMergedInOneFile will indicate that all threads are logged
  // in the same file, in occurence order
  // - if set to ptOneFilePerThread, it will create one .log file per thread
  // - if set to ptIdentifiedInOnFile, a new column will be added for each
  // log row, with the corresponding ThreadID - LogView tool will be able to
  // display per-thread logging, if needed - note that your application shall
  // use a thread pool (just like all mORMot servers classes do), otherwise
  // some random hash collision may occur if Thread IDs are not recycled enough
  TSynLogPerThreadMode = (
    ptMergedInOneFile, ptOneFilePerThread, ptIdentifiedInOnFile);

  /// how stack trace shall be computed during logging
  TSynLogStackTraceUse = (stManualAndAPI,stOnlyAPI,stOnlyManual);

  /// how file existing shall be handled during logging
  TSynLogExistsAction = (acOverwrite, acAppend, acAppendWithHeader);

  {/ regroup several logs under an unique family name
   - you should usualy use one family per application or per architectural
     module: e.g. a server application may want to log in separate files the
     low-level Communication, the DB access, and the high-level process
   - initialize the family settings before using them, like in this code:
     ! with TSynLogDB.Family do begin
     !   Level := LOG_VERBOSE;
     !   PerThreadLog := ptOneFilePerThread;
     !   DestinationPath := 'C:\Logs';
     ! end;
   - then use the logging system inside a method:
     ! procedure TMyDB.MyMethod;
     ! var ILog: ISynLog;
     ! begin
     !   ILog := TSynLogDB.Enter(self,'MyMethod');
     !   // do some stuff
     !   ILog.Log(sllInfo,'method called');
     ! end; }
  TSynLogFamily = class
  protected
    fLevel, fLevelStackTrace: TSynLogInfos;
    fArchiveAfterDays: Integer;
    fArchivePath: TFileName;
    fOnArchive: TSynLogArchiveEvent;
    fOnRotate: TSynLogRotateEvent;
    fPerThreadLog: TSynLogPerThreadMode;
    fIncludeComputerNameInFileName: boolean;
    fCustomFileName: TFileName;
    fGlobalLog: TSynLog;
    fSynLogClass: TSynLogClass;
    fIdent: integer;
    fDestinationPath: TFileName;
    fDefaultExtension: TFileName;
    fBufferSize: integer;
    fHRTimeStamp: boolean;
    fWithUnitName: boolean;
    fNoFile: boolean;
    {$ifdef MSWINDOWS}
    fAutoFlush: cardinal;
    {$endif}
    {$ifndef NOEXCEPTIONINTERCEPT}
    fHandleExceptions: boolean;
    {$endif}
    fStackTraceLevel: byte;
    fStackTraceUse: TSynLogStackTraceUse;
    fFileExistsAction: TSynLogExistsAction;
    fExceptionIgnore: TList;
    fEchoToConsole: TSynLogInfos;
    fEchoCustom: TOnTextWriterEcho;
    fEchoRemoteClient: TObject;
    fEchoRemoteClientOwned: boolean;
    fEchoRemoteEvent: TOnTextWriterEcho;
    fEndOfLineCRLF: boolean;
    fDestroying: boolean;
    fRotateFileCurrent: cardinal;
    fRotateFileCount: cardinal;
    fRotateFileSize: cardinal;
    fRotateFileAtHour: integer;
    function CreateSynLog: TSynLog;
    {$ifdef MSWINDOWS}
    procedure SetAutoFlush(TimeOut: cardinal);
    {$endif}
    procedure SetDestinationPath(const value: TFileName);
    procedure SetLevel(aLevel: TSynLogInfos);
    procedure SetEchoToConsole(aEnabled: TSynLogInfos);
  public
    /// intialize for a TSynLog class family
    // - add it in the global SynLogFileFamily[] list
    constructor Create(aSynLog: TSynLogClass);
    /// release associated memory
    // - will archive older DestinationPath\*.log files, according to
    // ArchiveAfterDays value and ArchivePath
    destructor Destroy; override;

    /// retrieve the corresponding log file of this thread and family
    // - creates the TSynLog if not already existing for this current thread
    function SynLog: TSynLog;
    /// register one object and one echo callback for remote logging
    // - aClient is typically a mORMot's TSQLHttpClient
    // - if aClientOwnedByFamily is TRUE, its life time will be manage by this
    // TSynLogFamily: it will staty alive until this TSynLogFamily is destroyed,
    // or the EchoRemoteStop() method called
    // - aClientEvent should be able to send the log row to the remote server
    // - typical use may be:
    procedure EchoRemoteStart(aClient: TObject; const aClientEvent: TOnTextWriterEcho;
      aClientOwnedByFamily: boolean);
    /// stop echo remote logging
    // - will free the aClient instance supplied to EchoRemoteStart
    procedure EchoRemoteStop;

    /// you can add some exceptions to be ignored to this list
    // - for instance, EConvertError may be added to the list
    property ExceptionIgnore: TList read fExceptionIgnore;
    /// event called to archive the .log content after a defined delay
    // - Destroy will parse DestinationPath folder for *.log files matching
    // ArchiveAfterDays property value
    // - you can set this property to EventArchiveDelete in order to delete deprecated
    // files, or EventArchiveSynLZ to compress the .log file into our propertary
    // SynLZ format: resulting file name will be ArchivePath\log\YYYYMM\*.log.synlz
    // (use FileUnSynLZ function to uncompress it)
    // - if you use SynZip.EventArchiveZip, the log files will be archived in
    // ArchivePath\log\YYYYMM.zip
    // - the aDestinationPath parameter will contain 'ArchivePath\log\YYYYMM\'
    // - this event handler will be called one time per .log file to archive,
    // then one last time with aOldLogFileName='' in order to close any pending
    // archive (used e.g. by EventArchiveZip to open the .zip only once)
    property OnArchive: TSynLogArchiveEvent read fOnArchive write fOnArchive;
    /// event called to perform a custom file rotation
    // - will be checked by TSynLog.PerformRotation to customize the rotation
    // process and do not perform the default step, if the callback returns TRUE
    property OnRotate: TSynLogRotateEvent read fOnRotate write fOnRotate;
    /// if the some kind of events shall be echoed to the console
    // - note that it will slow down the logging process a lot (console output
    // is slow by nature under Windows, but may be convenient for interactive
    // debugging of services, for instance
    // - this property shall be set before any actual logging, otherwise it
    // will have no effect
    // - can be set e.g. to LOG_VERBOSE in order to echo every kind of events
    // - EchoCustom or EchoService can be activated separately
    property EchoToConsole: TSynLogInfos read fEchoToConsole write SetEchoToConsole;
    /// can be set to a callback which will be called for each log line
    // - could be used with a third-party logging system
    // - EchoToConsole or EchoService can be activated separately
    // - you may even disable the integrated file output, via NoFile := true
    property EchoCustom: TOnTextWriterEcho read fEchoCustom write fEchoCustom;
  published
    /// the associated TSynLog class
    property SynLogClass: TSynLogClass read fSynLogClass;
    /// index in global SynLogFileFamily[] and SynLogFileIndexThreadVar[] lists
    property Ident: integer read fIdent;
    /// the current level of logging information for this family
    // - can be set e.g. to LOG_VERBOSE in order to log every kind of events
    property Level: TSynLogInfos read fLevel write SetLevel;
    /// the levels which will include a stack trace of the caller
    // - by default, contains sllError, sllException, sllExceptionOS, sllFail,
    // sllLastError and sllStackTrace
    // - exceptions will always trace the stack
    property LevelStackTrace: TSynLogInfos read fLevelStackTrace write fLevelStackTrace;
    /// the folder where the log must be stored
    // - by default, is in the executable folder
    property DestinationPath: TFileName read fDestinationPath write SetDestinationPath;
    /// the file extension to be used
    // - is '.log' by default
    property DefaultExtension: TFileName read fDefaultExtension write fDefaultExtension;
    /// if TRUE, the log file name will contain the Computer name - as '(MyComputer)'
    property IncludeComputerNameInFileName: boolean read fIncludeComputerNameInFileName write fIncludeComputerNameInFileName;
    /// can be used to customized the default file name
    // - by default, the log file name is computed from the executable name
    // (and the computer name if IncludeComputerNameInFileName is true)
    // - you can specify your own file name here, to be used instead
    // - this file name should not contain any folder, nor file extension (which
    // are set by DestinationPath and DefaultExtension properties) 
    property CustomFileName: TFileName read fCustomFileName write fCustomFileName;
    /// the folder where old log files must be compressed
    // - by default, is in the executable folder, i.e. the same as DestinationPath
    // - the 'log\' sub folder name will always be appended to this value
    // - will then be used by OnArchive event handler to produce, with the
    // current file date year and month, the final path (e.g.
    // 'ArchivePath\Log\YYYYMM\*.log.synlz' or 'ArchivePath\Log\YYYYMM.zip')
    property ArchivePath: TFileName read fArchivePath write fArchivePath;
    /// number of days before OnArchive event will be called to compress
    // or delete deprecated files
    // - will be set by default to 7 days
    // - will be used by Destroy to call OnArchive event handler on time
    property ArchiveAfterDays: Integer read fArchiveAfterDays write fArchiveAfterDays;
    /// the internal in-memory buffer size, in bytes
    // - this is the number of bytes kept in memory before flushing to the hard
    // drive; you can call TSynLog.Flush method or set AutoFlushTimeOut to true
    // in order to force the writting to disk
    // - is set to 4096 by default (4 KB is the standard hard drive cluster size)
    property BufferSize: integer read fBufferSize write fBufferSize;
    /// define how thread will be identified during logging process
    // - by default, ptMergedInOneFile will indicate that all threads are logged
    // in the same file, in occurence order (so multi-thread process on server
    // side may be difficult to interpret)
    // - if RotateFileCount and RotateFileSizeKB/RotateFileDailyAtHour are set,
    // will be ignored (internal thread list shall be defined for one process)
    property PerThreadLog: TSynLogPerThreadMode read fPerThreadLog write fPerThreadLog;
    /// if TRUE, will log high-resolution time stamp instead of ISO 8601 date and time
    // - this is less human readable, but allows performance profiling of your
    // application on the customer side (using TSynLog.Enter methods)
    // - set to FALSE by default, or if RotateFileCount and RotateFileSizeKB /
    // RotateFileDailyAtHour are set (the high resolution frequency is set
    // in the log file header, so expects a single file) 
    property HighResolutionTimeStamp: boolean read fHRTimeStamp write fHRTimeStamp;
    /// if TRUE, will log the unit name with an object instance if available
    // - unit name is available from RTTI if the class has published properties
    // - set to FALSE by default
    property WithUnitName: boolean read fWithUnitName write fWithUnitName;
    {$ifdef MSWINDOWS}
    /// the time (in seconds) after which the log content must be written on
    // disk, whatever the current content size is
    // - by default, the log file will be written for every 4 KB of log - this
    // will ensure that the main application won't be slow down by logging
    // - in order not to loose any log, a background thread can be created
    // and will be responsible of flushing all pending log content every
    // period of time (e.g. every 10 seconds)
    property AutoFlushTimeOut: cardinal read fAutoFlush write SetAutoFlush;
    {$endif}
    /// force no log to be written to any file
    // - may be usefull in conjunction e.g. with EchoToConsole or any other
    // third-party logging component 
    property NoFile: boolean read fNoFile write fNoFile;
    /// auto-rotation of logging files
    // - set to 0 by default, meaning no rotation
    // - can be set to a number of rotating files: rotation and compression will 
    // happen, and main file size will be up to RotateFileSizeKB number of bytes,
    // or when RotateFileDailyAtHour time is reached
    // - if set to 1, no .synlz backup will be created, so the main log file will
    // be restarted from scratch when it reaches RotateFileSizeKB size or when
    // RotateFileDailyAtHour time is reached
    // - if set to a number > 1, some rotated files will be compressed using the
    // SynLZ algorithm, and will be named e.g. as MainLogFileName.0.synlz ..
    // MainLogFileName.7.synlz for RotateFileCount=9 (total count = 9, including
    // 1 main log file and 8 .synlz files)
    property RotateFileCount: cardinal read fRotateFileCount write fRotateFileCount;
    /// maximum size of auto-rotated logging files, in kilo-bytes (per 1024 bytes)
    // - specify the maximum file size upon which .synlz rotation takes place
    // - is not used if RotateFileCount is left to its default 0
    property RotateFileSizeKB: cardinal read fRotateFileSize write fRotateFileSize;
    /// fixed hour of the day where logging files rotation should be performed
    // - by default, equals -1, meaning no rotation
    // - you can set a time value between 0 and 23 to force the rotation at this
    // specified hour 
    // - is not used if RotateFileCount is left to its default 0
    property RotateFileDailyAtHour: integer read fRotateFileAtHour write fRotateFileAtHour;
    /// the recursive depth of stack trace symbol to write
    // - used only if exceptions are handled, or by sllStackTrace level
    // - default value is 30, maximum is 255
    // - if stOnlyAPI is defined as StackTraceUse under Windows XP, maximum 
    // value may be around 60, due to RtlCaptureStackBackTrace() API limitations
    property StackTraceLevel: byte read fStackTraceLevel write fStackTraceLevel;
    /// how the stack trace shall use only the Windows API
    // - the class will use low-level RtlCaptureStackBackTrace() API to retrieve
    // the call stack: in some cases, it is not able to retrieve it, therefore
    // a manual walk of the stack can be processed - since this manual call can
    // trigger some unexpected access violations or return wrong positions,
    // you can disable this optional manual walk by setting it to stOnlyAPI
    // - default is stManualAndAPI, i.e. use RtlCaptureStackBackTrace() API and
    // perform a manual stack walk if the API returned no address (or <3); but
    // within the IDE, it will use stOnlyAPI, to ensure no annoyning AV occurs
    property StackTraceUse: TSynLogStackTraceUse read fStackTraceUse write fStackTraceUse;
    /// how existing log file shall be handled
    property FileExistsAction: TSynLogExistsAction read fFileExistsAction write fFileExistsAction;
    /// define how the logger will emit its line feed
    // - by default (FALSE), a single CR (#13) char will be written, to save
    // storage space
    // - you can set this property to TRUE, so that CR+LF (#13#10) chars will
    // be appended instead
    // - TSynLogFile class and our LogView tool will handle both patterns
    property EndOfLineCRLF: boolean read fEndOfLineCRLF write fEndOfLineCRLF;
  end;

  /// thread-specific internal context used during logging
  // - this structure is a hashed-per-thread variable
  TSynLogThreadContext = record
    /// the corresponding Thread ID
    ID: TThreadID;
    /// number of items stored in Recursion[]
    RecursionCount: integer;
    /// number of items available in Recursion[]
    // - faster than length(Recursion)
    RecursionCapacity: integer;
    /// used by TSynLog.Enter methods to handle recursivity calls tracing
    Recursion: array of record
      /// associated class instance to be displayed
      Instance: TObject;
      /// associated class type to be displayed
      ClassType: TClass;
      /// method name (or message) to be displayed
      MethodName: PUTF8Char;
      /// internal reference count used at this recursion level by TSynLog._AddRef
      RefCount: integer;
      /// the caller address, ready to display stack trace dump if needed
      Caller: PtrUInt;
      /// the time stamp at enter time
      EnterTimeStamp: Int64;
      /// if the method name is local, i.e. shall not be displayed at Leave()
      MethodNameLocal: (mnAlways, mnEnter, mnLeave);
    end;
  end;
  // pointer to thread-specific context information
  PSynLogThreadContext = ^TSynLogThreadContext;

  /// a per-family and/or per-thread log file content
  // - you should create a sub class per kind of log file
  // ! TSynLogDB = class(TSynLog);
  // - the TSynLog instance won't be allocated in heap, but will share a
  // per-thread (if Family.PerThreadLog=ptOneFilePerThread) or global private
  // log file instance
  // - was very optimized for speed, if no logging is written, and even during
  // log write (using an internal TTextWriter)
  // - can use available debugging information via the TSynMapFile class, for
  // stack trace logging for exceptions, sllStackTrace, and Enter/Leave labelling
  TSynLog = class(TObject, ISynLog)
  protected
    fFamily: TSynLogFamily;
    fWriter: TTextWriter;
    fWriterClass: TTextWriterClass;
    fWriterStream: TStream;
    fThreadLock: TRTLCriticalSection;
    fThreadContext: PSynLogThreadContext;
    fThreadID: TThreadID;
    fThreadIndex: integer;
    fStartTimeStamp: Int64;
    fCurrentTimeStamp: Int64;
    fFrequencyTimeStamp: Int64;
    fFileName: TFileName;
    fFileRotationSize: cardinal;
    fFileRotationNextHour: Int64;
    fThreadHash: TWordDynArray; // 64 KB buffer
    fThreadContexts: array of TSynLogThreadContext;
    fThreadContextCount: integer;
    fCurrentLevel: TSynLogInfo;
    fInternalFlags: set of (logHeaderWritten);
    {$ifdef FPC}
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    {$else}
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    {$endif}
    class function FamilyCreate: TSynLogFamily;
    procedure DoEnterLeave(aLevel: TSynLogInfo);
    procedure CreateLogWriter; virtual;
    procedure LogInternal(Level: TSynLogInfo; const TextFmt: RawUTF8;
      const TextArgs: array of const; Instance: TObject); overload;
    procedure LogInternal(Level: TSynLogInfo; const Text: RawUTF8;
      Instance: TObject; TextTruncateAtLength: integer); overload;
    procedure LogInternal(Level: TSynLogInfo; const aName: RawUTF8;
     aTypeInfo: pointer; var aValue; Instance: TObject=nil); overload; 
    // any call to this method MUST call UnLock
    function LogHeaderLock(Level: TSynLogInfo): boolean;
    procedure LogTrailerUnLock(Level: TSynLogInfo); {$ifdef HASINLINE}inline;{$endif}
    procedure LogCurrentTime;
    procedure LogFileHeader; virtual;
    {$ifndef DELPHI5OROLDER}
    procedure AddMemoryStats; virtual;
    {$endif}
    procedure AddErrorMessage(Error: cardinal);
    procedure AddStackTrace(Stack: PPtrUInt);
    procedure ComputeFileName; virtual;
    procedure PerformRotation; virtual;
    procedure AddRecursion(aIndex: integer; aLevel: TSynLogInfo);
    procedure LockAndGetThreadContext; {$ifdef HASINLINE}inline;{$endif}
    procedure LockAndGetThreadContextInternal(ID: TThreadID);
    function Instance: TSynLog;
    function ConsoleEcho(Sender: TTextWriter; Level: TSynLogInfo;
      const Text: RawUTF8): boolean; virtual;
  public
    /// intialize for a TSynLog class instance
    // - WARNING: not to be called directly! Use Enter or Add class function instead
    constructor Create(aFamily: TSynLogFamily=nil);
    /// release all memory and internal handles
    destructor Destroy; override;
    /// flush all log content to file
    // - if ForceDiskWrite is TRUE, will wait until written on disk (slow)
    procedure Flush(ForceDiskWrite: boolean);
    /// flush all log content to file and close the file
    procedure CloseLogFile;
    /// flush all log content to file, close the file, and release the instance
    // - you should never call the Free method directly, since the instance
    // is registered in a global TObjectList and an access violation may
    // occur at application closing: you can use this Release method if you
    // are sure that you won't need this TSynLog instance any more
    // - ensure there is no pending Leave element in a stack-allocated ISynLog
    // (see below) 
    // - can be used e.g. to release the instance when finishing a thread when
    // Family.PerThreadLog=ptOneFilePerThread:
    // ! var
    // !   TThreadLogger : TSynLogClass = TSynLog;
    // !
    // ! procedure TMyThread.Execute;
    // ! var log : ISynLog;
    // ! begin
    // !   log := TThreadLogger.Enter(self);
    // ! ...
    // !   log := nil; // to force logging end of method
    // !   TThreadLogger.SynLog.Release;
    // ! end;
    procedure Release;
    {/ handle generic method enter / auto-leave tracing
     - this is the main method to be called within a procedure/function to trace:
     ! procedure TMyDB.SQLExecute(const SQL: RawUTF8);
     ! var ILog: ISynLog;
     ! begin
     !   ILog := TSynLogDB.Enter(self,'SQLExecute');
     !   // do some stuff
     !   ILog.Log(sllInfo,'SQL=%',[SQL]);
     ! end;
     - returning a ISynLog interface will allow you to have an automated
     sllLeave log created when the method is left (thanks to the hidden
     try..finally block generated by the compiler to protect the ISynLog var)
     - it is convenient to define a local variable to store the returned ISynLog
     and use it for any specific logging within the method execution 
     - if you just need to access the log inside the method block, you may
     not need any ISynLog interface variable:
     ! procedure TMyDB.SQLFlush;
     ! begin
     !   TSynLogDB.Enter(self,'SQLFlush');
     !   // do some stuff
     ! end;
     - if no Method name is supplied, it will use the caller address, and
     will write it as hexa and with full unit and symbol name, if the debugging
     information is available (i.e. if TSynMapFile retrieved the .map content):
     ! procedure TMyDB.SQLFlush;
     ! begin
     !   TSynLogDB.Enter(self);
     !   // do some stuff
     ! end;
     - note that supplying a method name is faster than using the .map content:
     if you want accurate profiling, it's better to use a method name or not to
     use a .map file - note that this method name shall be a constant, and not
     a locally computed variable, since it may trigger some random GPF at
     runtime - if it is a local variable, you can set aMethodNameLocal=true
     - if TSynLogFamily.HighResolutionTimeStamp is TRUE, high-resolution
     time stamp will be written instead of ISO 8601 date and time: this will
     allow performance profiling of the application on the customer side
     - Enter() will write the class name (and the unit name for classes with
     published properties, if TSynLogFamily.WithUnitName is true) for both
     enter (+) and leave (-) events:
      $ 20110325 19325801  +    MyDBUnit.TMyDB(004E11F4).SQLExecute
      $ 20110325 19325801 info   SQL=SELECT * FROM Table;
      $ 20110325 19325801  -    MyDBUnit.TMyDB(004E11F4).SQLExecute }
    class function Enter(aInstance: TObject=nil; aMethodName: PUTF8Char=nil;
      aMethodNameLocal: boolean=false): ISynLog; overload;
    /// retrieve the current instance of this TSynLog class
    // - to be used for direct logging, without any Enter/Leave:
    // ! TSynLogDB.Add.Log(llError,'The % statement didn't work',[SQL]);
    // - to be used for direct logging, without any Enter/Leave (one parameter
    // version - just the same as previous):
    // ! TSynLogDB.Add.Log(llError,'The % statement didn't work',SQL);
    // - is just a wrapper around Family.SynLog - the same code will work:
    // ! TSynLogDB.Family.SynLog.Log(llError,'The % statement didn't work',[SQL]);
    class function Add: TSynLog;
      {$ifdef HASINLINE}inline;{$endif}
    /// retrieve the family of this TSynLog class type
    class function Family: TSynLogFamily; overload;
    /// returns a logging class which will never log anything
    // - i.e. a TSynLog sub-class with Family.Level := []
    class function Void: TSynLogClass;
    /// low-level method helper which can be called to make debugging easier
    // - log some warning message to the TSynLog family
    // - will force a manual breakpoint if tests are run from the IDE
    class procedure DebuggerNotify(Level: TSynLogInfo;
      const Format: RawUTF8; const Args: array of const);
    /// call this method to add some information to the log at the specified level
    // - % = #37 indicates a string, integer, floating-point, or class parameter
    // to be appended as text (e.g. class name)
    // - $ = #36 indicates an integer to be written with 2 digits and a comma
    // - £ = #163 indicates an integer to be written with 4 digits and a comma
    // - µ = #181 indicates an integer to be written with 3 digits without any comma
    // - ¤ = #164 indicates CR+LF chars
    // - CR = #13 indicates CR+LF chars
    // - § = #167 indicates to trim last comma
    // - since some of this characters above are > #127, they are not UTF-8
    // ready, so we expect the input format to be WinAnsi, i.e. mostly English
    // text (with chars < #128) with some values to be inserted inside
    // - note that cardinal values should be type-casted to Int64() (otherwise
    // the integer mapped value will be transmitted, therefore wrongly)
    procedure Log(Level: TSynLogInfo; const TextFmt: RawUTF8; const TextArgs: array of const;
      aInstance: TObject=nil); overload;
    /// same as Log(Level,TextFmt,[]) but with one RawUTF8 parameter
    procedure Log(Level: TSynLogInfo; const TextFmt: RawUTF8; const TextArg: RawUTF8;
      aInstance: TObject=nil); overload;
    /// same as Log(Level,TextFmt,[]) but with one Int64 parameter
    procedure Log(Level: TSynLogInfo; const TextFmt: RawUTF8; const TextArg: Int64;
      aInstance: TObject=nil); overload;
    /// call this method to add some information to the log at the specified level
    // - if Instance is set and Text is not '', it will log the corresponding
    // class name and address (to be used e.g. if you didn't call TSynLog.Enter()
    // method first) - for instance
    // ! TSQLLog.Add.Log(sllDebug,'GarbageCollector',GarbageCollector);
    // will append this line to the log:
    // $ 0000000000002DB9 debug TObjectList(00425E68) GarbageCollector
    // - if Instance is set and Text is '', will behave the same as
    // Log(Level,Instance), i.e. write the Instance as JSON content
    procedure Log(Level: TSynLogInfo; const Text: RawUTF8;
      aInstance: TObject=nil; TextTruncateAtLength: integer=maxInt); overload;
    {$ifdef UNICODE}
    /// call this method to add some VCL string to the log at a specified level
    // - this overloaded version will avoid a call to StringToUTF8()
    procedure Log(Level: TSynLogInfo; const Text: string; aInstance: TObject=nil); overload;
    {$endif}
    /// call this method to add the content of an object to the log at a
    // specified level
    // - this default implementation will just write the class name and its hexa
    // pointer value, and handle TList, TCollections and TStrings - for instance:
    // ! TSynLog.Add.Log(sllDebug,GarbageCollector);
    // will append this line to the log:
    // $ 20110330 10010005 debug {"TObjectList(00B1AD60)":["TObjectList(00B1AE20)","TObjectList(00B1AE80)"]}
    // - if aInstance is an Exception, it will handle its class name and Message:
    // $ 20110330 10010005 debug "EClassName(00C2129A)":"Exception message"
    // - use TSQLLog from mORMot.pas unit to add the record content, written
    // as human readable JSON
    procedure Log(Level: TSynLogInfo; aInstance: TObject); overload;
    /// call this method to add the content of most low-level types to the log
    // at a specified level
    // - this overridden implementation will write the value content,
    // written as human readable JSON: handle dynamic arrays and enumerations
    // - TSQLLog from mORMot.pas unit will be able to write
    // TObject/TSQLRecord and sets content as JSON
    procedure Log(Level: TSynLogInfo; const aName: RawUTF8;
      aTypeInfo: pointer; var aValue; Instance: TObject=nil); overload;
    /// call this method to add the caller address to the log at the specified level
    // - if the debugging info is available from TSynMapFile, will log the
    // unit name, associated symbol and source code line
    procedure Log(Level: TSynLogInfo); overload;
    /// call this method to add some multi-line information to the log at a
    // specified level
    // - LinesToLog content will be added, one line per one line, delimited by
    // #13#10 (CRLF)
    // - if a line starts with IgnoreWhenStartWith (already uppercase), it won't
    // be added to the log content (to be used e.g. with '--' for SQL statements)
    procedure LogLines(Level: TSynLogInfo; LinesToLog: PUTF8Char; aInstance: TObject=nil;
      const IgnoreWhenStartWith: PAnsiChar=nil);
    /// direct access to the low-level writing content
    // - should usually not be used directly, unless you ensure it is safe
    property Writer: TTextWriter read fWriter;
  published
    /// the associated logging family
    property GenericFamily: TSynLogFamily read fFamily;
    /// the associated file name containing the log
    // - this is accurate only with the default implementation of the class:
    // any child may override it with a custom logging mechanism
    property FileName: TFileName read fFileName;
  end;

  /// used by TSynLogFile to refer to a method profiling in a .log file
  // - i.e. map a sllEnter/sllLeave event in the .log file
  TSynLogFileProc = record
    /// the index of the sllEnter event in the TSynLogFile.fLevels[] array
    Index: cardinal;
    /// the associated time elapsed in this method (in micro seconds)
    // - computed from the sllLeave time difference (high resolution timer)
    Time: cardinal;
    /// the time elapsed in this method and not in nested methods
    // - computed from Time property, minus the nested calls
    ProperTime: cardinal;
  end;

  /// used by TSynLogFile to refer to global method profiling in a .log file
  // - i.e. map all sllEnter/sllLeave event in the .log file
  TSynLogFileProcDynArray = array of TSynLogFileProc;

  TSynLogFileProcArray = array[0..(MaxInt div sizeof(TSynLogFileProc))-1] of TSynLogFileProc;
  PSynLogFileProcArray = ^TSynLogFileProcArray;

  /// used by TSynLogFile.LogProcSort method
  TLogProcSortOrder = (
    soNone, soByName, soByOccurrence, soByTime, soByProperTime);
    
  /// used to parse a .log file, as created by TSynLog, into high-level data
  // - this particular TMemoryMapText class will retrieve only valid event lines
  // (i.e. will fill EventLevel[] for each line <> sllNone)
  // - Count is not the global text line numbers, but the number of valid events
  // within the file (LinePointers/Line/Strings will contain only event lines) -
  // it will not be a concern, since the .log header is parsed explicitely
  TSynLogFile = class(TMemoryMapText)
  protected
    /// map the events occurring in the .log file content
    fLevels: TSynLogInfoDynArray;
    fThreads: TWordDynArray;
    fThreadsRows: TCardinalDynArray;
    fThreadsCount: integer;
    fThreadsRowsCount: cardinal;
    fThreadMax: cardinal;
    fLineLevelOffset: cardinal;
    fLineTextOffset: cardinal;
    fLineHeaderCountToIgnore: integer;
    /// as extracted from the .log header
    fExeName, fExeVersion, fHost, fUser, fCPU, fOSDetailed, fInstanceName: RawUTF8;
    fExeDate: TDateTime;
    {$ifdef MSWINDOWS}
    fOS: TWindowsVersion;
    fOSServicePack: integer;
    fWow64: boolean;
    {$endif}
    fStartDateTime: TDateTime;
    /// retrieve all used event levels
    fLevelUsed: TSynLogInfos;
    /// =0 if date time resolution, >0 if high-resolution time stamp
    fFreq: Int64;
    /// used by EventDateTime() to compute date from time stamp
    fFreqPerDay: double;
    /// custom headers, to be searched as .ini content
    fHeaderLinesCount: integer;
    fHeaders: RawUTF8;
    /// method profiling data
    fLogProcCurrent: PSynLogFileProcArray;
    fLogProcCurrentCount: integer;
    fLogProcNatural: TSynLogFileProcDynArray;
    fLogProcNaturalCount: integer;
    fLogProcMerged: TSynLogFileProcDynArray;
    fLogProcMergedCount: integer;
    fLogProcIsMerged: boolean;
    fLogProcStack: array of cardinal;
    fLogProcStackCount: integer;
    fLogProcSortInternalOrder: TLogProcSortOrder;
    /// used by ProcessOneLine//GetLogLevelTextMap
    fLogLevelsTextMap: array[TSynLogInfo] of cardinal;
    procedure SetLogProcMerged(const Value: boolean);
    function GetEventText(index: integer): RawUTF8;
    function GetLogLevelFromText(LineBeg: PUTF8Char): TSynLogInfo;
    /// retrieve headers + fLevels[] + fLogProcNatural[], and delete invalid fLines[]
    procedure LoadFromMap(AverageLineLength: integer=32); override;
    /// compute fLevels[] + fLogProcNatural[] for each .log line during initial reading
    procedure ProcessOneLine(LineBeg, LineEnd: PUTF8Char); override;
    /// called by LogProcSort method
    function LogProcSortComp(A,B: Integer): integer;
    procedure LogProcSortInternal(L,R: integer);
  public
    /// initialize internal structure
    constructor Create; override;
    /// returns TRUE if the supplied text is contained in the corresponding line
    function LineContains(const aUpperSearch: RawUTF8; aIndex: Integer): Boolean; override;
    /// retrieve the date and time of an event
    // - returns 0 in case of an invalid supplied index
    function EventDateTime(aIndex: integer): TDateTime;
    /// sort the LogProc[] array according to the supplied order
    procedure LogProcSort(Order: TLogProcSortOrder);
    /// return the number of matching events in the log
    function EventCount(const aSet: TSynLogInfos): integer;
    /// retrieve the level of an event
    // - is calculated by Create() constructor
    // - EventLevel[] array index is from 0 to Count-1
    property EventLevel: TSynLogInfoDynArray read fLevels;
    /// retrieve all used event levels
    // - is calculated by Create() constructor
    property EventLevelUsed: TSynLogInfos read fLevelUsed;
    /// retrieve the description text of an event
    // - returns '' if supplied index is out of range
    property EventText[index: integer]: RawUTF8 read GetEventText;
    /// retrieve all event thread IDs
    // - contains something if TSynLogFamily.PerThreadLog was ptIdentifiedInOnFile
    // - for ptMergedInOneFile (default) or ptOneFilePerThread logging process,
    // the array will be void (EventThread=nil)   
    property EventThread: TWordDynArray read fThreads;
    /// the number of threads 
    property ThreadsCount: cardinal read fThreadMax;
    /// the number of occurences of each thread ID
    property ThreadsRows: TCardinalDynArray read fThreadsRows;
    /// profiled methods information
    // - is calculated by Create() constructor
    // - will contain the sllEnter index, with the associated elapsed time
    // - number of items in the array is retrieved by the LogProcCount property
    property LogProc: PSynLogFileProcArray read fLogProcCurrent;
    /// the current sort order
    property LogProcOrder: TLogProcSortOrder read fLogProcSortInternalOrder;
    /// if the method information must be merged for the same method name
    property LogProcMerged: boolean read fLogProcIsMerged write SetLogProcMerged;
    /// all used event levels, as retrieved at log file content parsing
    property LevelUsed: TSynLogInfos read fLevelUsed;
    /// high-resolution time stamp frequence, as retrieved from log file header 
    // - equals 0 if date time resolution, >0 if high-resolution time stamp
    property Freq: Int64 read fFreq;
    /// custom headers, to be searched as .ini content
    property Headers: RawUTF8 read fHeaders;
  published
    /// the associated executable name (with path)
    // - returns e.g. 'C:\Dev\lib\SQLite3\exe\TestSQL3.exe'
    property ExecutableName: RawUTF8 read fExeName;
    /// the associated executable version
    // - returns e.g. '0.0.0.0'
    property ExecutableVersion: RawUTF8 read fExeVersion;
    /// the associated executable build date and time
    property ExecutableDate: TDateTime read fExeDate;
    /// for a library, the associated instance name (with path)
    // - returns e.g. 'C:\Dev\lib\SQLite3\exe\TestLibrary.dll'
    // - for an executable, will be left void
    property InstanceName: RawUTF8 read fInstanceName;
    /// the computer host name in which the process was running on
    property ComputerHost: RawUTF8 read fHost;
    /// the computer user name who launched the process
    property RunningUser: RawUTF8 read fUser;
    /// the computer CPU in which the process was running on
    // - returns e.g. '1*0-15-1027'
    property CPU: RawUTF8 read fCPU;
    {$ifdef MSWINDOWS}
    /// the computer Operating System in which the process was running on
    property OS: TWindowsVersion read fOS;
    /// the Operating System Service Pack number
    property ServicePack: integer read fOSServicePack;
    /// if the 32 bit process was running under WOW 64 virtual emulation
    property Wow64: boolean read fWow64;
    {$endif MSWINDOWS}
    /// the computer Operating System in which the process was running on
    // - returns e.g. '2.3=5.1.2600' for Windows XP
    // - under Linux, it will return the full system version, e.g.
    // 'Linux-3.13.0-43-generic#72-Ubuntu-SMP-Mon-Dec-8-19:35:44-UTC-2014'
    property DetailedOS: RawUTF8 read fOSDetailed;
    /// the date and time at which the log file was started
    property StartDateTime: TDateTime read fStartDateTime;
    /// number of profiled methods in this .log file
    // - i.e. number of items in the LogProc[] array
    property LogProcCount: integer read fLogProcCurrentCount;
  end;

const
  /// up to 16 TSynLogFamily, i.e. TSynLog children classes can be defined
  MAX_SYNLOGFAMILY = 15;

  /// can be set to TSynLogFamily.Level in order to log all available events
  LOG_VERBOSE: TSynLogInfos = [succ(sllNone)..high(TSynLogInfo)];

  /// contains the logging levels for which stack trace should be dumped
  // - which are mainly exceptions or application errors
  LOG_STACKTRACE: TSynLogInfos = [
    sllLastError,sllError,sllException,sllExceptionOS,sllDDDError];

  /// the text equivalency of each logging level, as written in the log file
  // - PCardinal(@LOG_LEVEL_TEXT[L][3])^ will be used for fast level matching
  // so text must be unique for characters [3..6] -> e.g. 'UST4'
  LOG_LEVEL_TEXT: array[TSynLogInfo] of string[7] = (
    '       ', ' info  ', ' debug ', ' trace ', ' warn  ', ' ERROR ',
    '  +    ', '  -    ',
    ' OSERR ', ' EXC   ', ' EXCOS ', ' mem   ', ' stack ', ' fail  ',
    ' SQL   ', ' cache ', ' res   ', ' DB    ', ' http  ', ' clnt  ', ' srvr  ',
    ' call  ', ' ret   ', ' auth  ',
    ' cust1 ', ' cust2 ', ' cust3 ', ' cust4 ', ' rotat ', ' dddER ', ' dddIN ',
    ' mon   ');

  /// the "magic" number used to identify .log.synlz compressed files, as
  // created by TSynLogFamily.EventArchiveSynLZ
  LOG_MAGIC = $ABA51051;

var
  /// the kind of .log file generated by TSynTestsLogged
  TSynLogTestLog: TSynLogClass = TSynLog;


/// a TSynLogArchiveEvent handler which will delete older .log files
function EventArchiveDelete(const aOldLogFileName, aDestinationPath: TFileName): boolean;

/// a TSynLogArchiveEvent handler which will compress older .log files
// using our proprietary SynLZ format
// - resulting file will have the .synlz extension and will be located
// in the aDestinationPath directory, i.e. TSynLogFamily.ArchivePath+'\log\YYYYMM\'
// - use UnSynLZ.dpr tool to uncompress it into .log textual file
// - SynLZ is much faster than zip for compression content, but proprietary
function EventArchiveSynLZ(const aOldLogFileName, aDestinationPath: TFileName): boolean;


implementation

{$ifdef FPC}
uses
  SynFPCTypInfo // small wrapper unit around FPC's TypInfo.pp
  {$ifdef Linux}
  , SynFPCLinux,BaseUnix, Unix, dynlibs
  {$endif} ;
{$endif}


{ TSynMapFile }

const
  MAGIC_MAB = $A5A5A5A5;

function MatchPattern(P,PEnd,Up: PUTF8Char; var Dest: PUTF8Char): boolean;
begin
  result := false;
  repeat
    if P^ in [#1..' '] then repeat inc(P) until not(P^ in [#1..' ']);
    while NormToUpperAnsi7[P^]=Up^ do begin
      inc(P);
      if P>PEnd then exit;
      inc(Up);
      if (Up^=' ') and (P^ in [#1..' ']) then begin // ignore multiple spaces in P^
        while (P<PEnd) and (P^ in [#1..' ']) do inc(P);
        inc(Up);
      end;
    end;
    if Up^=#0 then // all chars of Up^ found in P^
      break else
    if Up^<>' ' then // P^ and Up^ didn't match
      exit;
    inc(Up);
  until false;
  while (P<PEnd) and (P^=' ') do inc(P); // ignore all spaces
  result := true;
  Dest := P;
end;

procedure ReadSymbol(var R: TFileBufferReader; var A: TDynArray);
var i, n, L: integer;
    S: PSynMapSymbol;
    Addr: cardinal;
    P: PByte;
begin
  n := R.ReadVarUInt32;
  A.Count := n;
  P := R.CurrentMemory;
  if (n=0) or (P=nil) then
    exit;
  S := A.Value^;
  for i := 0 to n-1 do begin
    L := FromVarUInt32(P); // inlined R.Read(S^.Name)
    SetString(S^.Name,PAnsiChar(P),L);
    inc(P,L);
    inc(PtrUInt(S),A.ElemSize);
  end;
  S := A.Value^;
  Addr := FromVarUInt32(P);
  S^.Start := Addr;
  for i := 1 to n-1 do begin
    inc(Addr,FromVarUInt32(P));
    S^.Stop := Addr-1;
    inc(PtrUInt(S),A.ElemSize);
    S^.Start := Addr;
  end;
  S^.Stop := Addr+FromVarUInt32(P);
  R.Seek(PtrUInt(P)-PtrUInt(R.MappedBuffer));
end;

const
  /// Delphi linker starts the code section at this fixed offset
  CODE_SECTION = $1000;

{$ifdef UNICODE}
{ due to a bug in Delphi 2009+, we need to fake inheritance of record,
  since TDynArrayHashed = object(TDynArray) fails to initialize
  http://blog.synopse.info/post/2011/01/29/record-and-object-issue-in-Delphi-2010 }
{$define UNDIRECTDYNARRAY}
{$endif}

constructor TSynMapFile.Create(const aExeName: TFileName=''; MabCreate: boolean=true);

  procedure LoadMap;
    var P, PEnd: PUTF8Char;
    procedure NextLine;
    begin
      while (P<PEnd) and (P^>=' ') do inc(P);
      if (P<PEnd) and (P^=#13) then inc(P);
      if (P<PEnd) and (P^=#10) then inc(P);
    end;
    function GetCode(var Ptr: integer): boolean;
    begin
      while (P<PEnd) and (P^=' ') do inc(P);
      result := false;
      if (P+10<PEnd) and
         (PInteger(P)^=ord('0')+ord('0')shl 8+ord('0')shl 16+ord('1')shl 24) and
         (P[4]=':') then begin
        if not HexDisplayToBin(PAnsiChar(P)+5,@Ptr,sizeof(Ptr)) then exit;
        while (P<PEnd) and (P^>' ') do inc(P);
        while (P<PEnd) and (P^=' ') do inc(P);
        if P<PEnd then
          result := true;
      end;
    end;
    procedure ReadSegments;
    var Beg: PAnsiChar;
        U: TSynMapUnit;
    begin
      NextLine;
      NextLine;
      while (P<PEnd) and (P^<' ') do inc(P);
      while (P+10<PEnd) and (P^>=' ') do begin
        if GetCode(U.Symbol.Start) and
           HexDisplayToBin(PAnsiChar(P),@U.Symbol.Stop,4) then begin
          while PWord(P)^<>ord('M')+ord('=')shl 8 do
            if P+10>PEnd then exit else inc(P);
          Beg := pointer(P+2);
          while (P<PEnd) and (P^>' ') do inc(P);
          SetString(U.Symbol.Name,Beg,P-Beg);
          inc(U.Symbol.Stop,U.Symbol.Start-1);
          if (U.Symbol.Name<>'') and
             ((U.Symbol.Start<>0) or (U.Symbol.Stop<>0)) then
            fUnits.FindHashedAndUpdate(U,true); // true for adding
        end;
        NextLine;
      end;
    end;
    procedure ReadSymbols;
    var Beg: PAnsiChar;
        Sym: TSynMapSymbol;
    begin
      NextLine;
      NextLine;
      while (P+10<PEnd) and (P^>=' ') do begin
        if GetCode(Sym.Start) then begin
          while (P<PEnd) and (P^=' ') do inc(P);
          Beg := pointer(P);
          {$ifdef HASINLINE}
          // trim left 'UnitName.' for each symbol (since Delphi 2005)
          case PWord(P)^ of // ignore RTL namespaces
          ord('S')+ord('y') shl 8:
             if IdemPChar(P+2,'STEM.') then
               if IdemPChar(P+7,'WIN.') then inc(P,9) else
               if IdemPChar(P+7,'RTTI.') then inc(P,10) else
               if IdemPChar(P+7,'TYPES.') then inc(P,10) else
               if IdemPChar(P+7,'ZLIB.') then inc(P,10) else
               if IdemPChar(P+7,'CLASSES.') then inc(P,10) else
               if IdemPChar(P+7,'SYSUTILS.') then inc(P,10) else
               if IdemPChar(P+7,'VARUTILS.') then inc(P,10) else
               if IdemPChar(P+7,'STRUTILS.') then inc(P,10) else
               if IdemPChar(P+7,'SYNCOBJS.') then inc(P,10) else
               if IdemPChar(P+7,'GENERICS.') then inc(P,16) else
               if IdemPChar(P+7,'CHARACTER.') then inc(P,10) else
               if IdemPChar(P+7,'TYPINFO.') then inc(P,10) else
               if IdemPChar(P+7,'VARIANTS.') then inc(P,10);
          ord('W')+ord('i') shl 8: if IdemPChar(P+2,'NAPI.') then inc(P,7);
          ord('V')+ord('c') shl 8: if IdemPChar(P+2,'L.') then inc(P,7);
          end;
          while (P<PEnd) and (P^<>'.') do if P^<=' ' then break else inc(P);
          if P^='.' then begin
            while (P<PEnd) and (P^='.') do inc(P);
            Beg := pointer(P);
          end else
            P := pointer(Beg); // no '.' found
          {$endif}
          while (P<PEnd) and (P^>' ') do inc(P);
          SetString(Sym.Name,Beg,P-Beg);
          if (Sym.Name<>'') and not (Sym.Name[1] in ['$','?']) then
            fSymbols.Add(Sym);
        end;
        NextLine;
      end;
    end;
    procedure ReadLines;
    var Beg: PAnsiChar;
        i, Count, n: integer;
        aName: RawUTF8;
        added: boolean;
        U: ^TSynMapUnit;
    begin
      Beg := pointer(P);
      while P^<>'(' do if P=PEnd then exit else inc(P);
      SetString(aName,Beg,P-Beg);
      if aName='' then
        exit;
      i := fUnits.FindHashedForAdding(aName,added);
      U := @fUnit[i];
      if added then
        U^.Symbol.Name := aName; // should not occur, but who knows...
      if U^.FileName='' then begin
        inc(P); Beg := pointer(P);
        while P^<>')' do if P=PEnd then exit else inc(P);
        SetString(U^.FileName,Beg,P-Beg);
      end;
      NextLine;
      NextLine;
      n := length(U^.Line);
      Count := n; // same unit may appear multiple times in .map content
      while (P+10<PEnd) and (P^>=' ') do begin
        while (P<PEnd) and (P^=' ') do inc(P);
        repeat
          if Count=n then begin
            n := Count+Count shr 3+256;
            SetLength(U^.Line,n);
            SetLength(U^.Addr,n);
          end;
          U^.Line[Count] := GetNextItemCardinal(P,' ');
          if not GetCode(U^.Addr[Count]) then
            break;
          if U^.Addr[Count]<>0 then
            inc(Count); // occured with Delphi 2010 :(
        until (P>=PEnd) or (P^<' ');
        NextLine;
      end;
      SetLength(U^.Line,Count);
      SetLength(U^.Addr,Count);
    end;
  var i, s,u: integer;
      RehashNeeded: boolean;
  begin // LoadMap
    fSymbols.Capacity := 8000;
    with TSynMemoryStreamMapped.Create(fMapFile) do
    try
      // parse .map sections into fSymbol[] and fUnit[]
      P := Memory;
      PEnd := P+Size;
      while P<PEnd do
        if MatchPattern(P,PEnd,'DETAILED MAP OF SEGMENTS',P) then
          ReadSegments else
        if MatchPattern(P,PEnd,'ADDRESS PUBLICS BY VALUE',P) then
          ReadSymbols else
        if MatchPattern(P,PEnd,'LINE NUMBERS FOR',P) then
          ReadLines else
          NextLine;
      // now we should have read all .map content
      s := fSymbols.Count-1;
      RehashNeeded := false;
      for i := fUnits.Count-1 downto 0 do
        with fUnit[i] do
          if (Symbol.Start=0) and (Symbol.Stop=0) then begin
            fUnits.Delete(i); // occurs with Delphi 2010 :(
            RehashNeeded := true;
          end;
      u := fUnits.Count-1;
      if RehashNeeded then
        fUnits.ReHash; // as expected by TDynArrayHashed
      {$ifopt C+}
      for i := 1 to u do
         assert(fUnit[i].Symbol.Start>fUnit[i-1].Symbol.Stop);
      {$endif}
      for i := 0 to s-1 do
        fSymbol[i].Stop := fSymbol[i+1].Start-1;
      if (u>=0) and (s>=0) then
        fSymbol[s].Stop := fUnit[u].Symbol.Stop;
    finally
      Free;
    end;
  end;

  procedure LoadMab(const aMabFile: TFileName);
  var R: TFileBufferReader;
      i: integer;
      S: TCustomMemoryStream;
      MS: TMemoryStream;
  begin
    fMapFile := aMabFile;
    if FileExists(aMabfile) then
    try
      S := TSynMemoryStreamMapped.Create(aMabFile);
      try
        MS := StreamUnSynLZ(S,MAGIC_MAB);
        if MS<>nil then
        try
          R.OpenFrom(MS.Memory,MS.Size);
          ReadSymbol(R,fSymbols);
          ReadSymbol(R,fUnits{$ifdef UNDIRECTDYNARRAY}.InternalDynArray{$endif});
          fUnits.ReHash;
          for i := 0 to fUnits.Count-1 do
          with fUnit[i] do begin
            R.Read(FileName);
            R.ReadVarUInt32Array(Line);
            R.ReadVarUInt32Array(Addr);
          end;
          MabCreate := false;
        finally
          MS.Free;
        end;
      finally
        S.Free;
      end;
    except
      on Exception do; // invalid file -> ignore any problem
    end;
  end;

var SymCount, UnitCount, i: integer;
    MabFile: TFileName;
    MapAge, MabAge: TDateTime;
    U: RawUTF8;
begin
  fSymbols.Init(TypeInfo(TSynMapSymbolDynArray),fSymbol,@SymCount);
  fUnits.Init(TypeInfo(TSynMapUnitDynArray),fUnit,nil,nil,nil,@UnitCount);
  fUnitSynLogIndex := -1;
  fUnitSystemIndex := -1;
  // 1. search for an external .map file matching the running .exe/.dll name
  if aExeName='' then begin
    fMapFile := GetModuleName(hInstance);
    {$ifdef MSWINDOWS}
    fCodeOffset := GetModuleHandle(pointer(ExtractFileName(fMapFile)))+CODE_SECTION;
    {$else}
    {$ifdef KYLIX3}
    fCodeOffset := GetTextStart; // from SysInit.pas
    {$endif}
    {$endif}
  end else
    fMapFile := aExeName;
  fMapFile := ChangeFileExt(fMapFile,'.map');
  MabFile := ChangeFileExt(fMapFile,'.mab');
  GlobalLock;
  try
    MapAge := FileAgeToDateTime(fMapFile);
    MabAge := FileAgeToDateTime(MabFile);
    if (MapAge>0) and (MabAge<MapAge) then
      LoadMap; // if no faster-to-load .mab available and accurate
    // 2. search for a .mab file matching the running .exe/.dll name
    if (SymCount=0) and (MabAge<>0) then
      LoadMab(MabFile);
    // 3. search for an embedded compressed .mab file appended to the .exe/.dll
    if SymCount=0 then
      LoadMab(GetModuleName(hInstance));
    // finalize symbols
    if SymCount>0 then begin
      for i := 1 to SymCount-1 do
        assert(fSymbol[i].Start>fSymbol[i-1].Stop);
      SetLength(fSymbol,SymCount);
      SetLength(fUnit,UnitCount);
      fSymbols.Init(TypeInfo(TSynMapSymbolDynArray),fSymbol);
      fUnits.Init(TypeInfo(TSynMapUnitDynArray),fUnit);
      if MabCreate then
        SaveToFile(MabFile); // if just created from .map -> create .mab file
      U := 'SynLog';
      fUnitSynLogIndex := fUnits.Find(U);
      U := 'System';
      fUnitSystemIndex := fUnits.Find(U);
      fHasDebugInfo := true;
    end else
      fMapFile := '';
  finally
    GlobalUnLock;
  end;
end;

procedure WriteSymbol(var W: TFileBufferWriter; const A: TDynArray);
var i, n: integer;
    Diff: integer;
    S: PSynMapSymbol;
    P: PByte;
    Beg: PtrUInt;
begin
  n := A.Count;
  W.WriteVarUInt32(n);
  if n=0 then exit;
  S := A.Value^;
  for i := 0 to n-1 do begin
    W.Write(S^.Name);
    inc(PtrUInt(S),A.ElemSize);
  end;
  S := A.Value^;
  Diff := S^.Start;
  W.WriteVarUInt32(Diff);
  P := W.WriteDirectStart(n*5,A.ArrayTypeName);
  Beg := PtrUInt(P);
  for i := 1 to n-1 do begin
    inc(PtrUInt(S),A.ElemSize);
    P := ToVarUInt32(S^.Start-Diff,P);
    Diff := S^.Start;
  end;
  P := ToVarUInt32(S^.Stop-Diff,P);
  W.WriteDirectEnd(PtrUInt(P)-Beg);
end;

procedure TSynMapFile.SaveToStream(aStream: TStream);
var W: TFileBufferWriter;
    i: integer;
    MS: TMemoryStream;
begin
  MS := THeapMemoryStream.Create;
  W := TFileBufferWriter.Create(MS,1 shl 20); // 1 MB should be enough at first
  try
    WriteSymbol(W,fSymbols);
    WriteSymbol(W,fUnits{$ifdef UNDIRECTDYNARRAY}.InternalDynArray{$endif});
    for i := 0 to high(fUnit) do
    with fUnit[i] do begin
      W.Write(FileName);
      W.WriteVarUInt32Array(Line,length(Line),wkOffsetI); // not always increasing
      W.WriteVarUInt32Array(Addr,length(Addr),wkOffsetU); // always increasing
    end;
    W.Flush;
    StreamSynLZ(MS,aStream,MAGIC_MAB);
  finally
    MS.Free;
    W.Free;
  end;
end;

function TSynMapFile.SaveToFile(const aFileName: TFileName=''): TFileName;
var F: TFileStream;
begin
  if aFileName='' then
    result := ChangeFileExt(GetModuleName(hInstance),'.mab') else
    result := aFileName;
  DeleteFile(result);
  F := TFileStream.Create(result,fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TSynMapFile.SaveToExe(const aExeName: TFileName);
var FN: TFileName;
    MS, MAB: TMemoryStream;
    Len, LenMAB: PtrUInt;
begin
  if not FileExists(aExeName) then
    exit;
  FN := SaveToFile(ChangeFileExt(aExeName,'.mab'));
  try
    MS := THeapMemoryStream.Create;
    MAB := THeapMemoryStream.Create;
    try
      // load both files
      MAB.LoadFromFile(FN);
      LenMAB := MAB.Size;
      MS.LoadFromFile(aExeName);
      Len := MS.Size;
      if Len<16 then
        exit;
      // trim existing mab content
      Len := StreamSynLZComputeLen(MS.Memory,Len,MAGIC_MAB);
      // append mab content to exe
      MS.Size := Len+LenMAB;
      move(MAB.Memory^,PAnsiChar(MS.Memory)[Len],LenMAB);
      MS.SaveToFile(aExeName);
    finally
      MAB.Free;
      MS.Free;
    end;
  finally
    DeleteFile(FN);
  end;
end;

function TSynMapFile.FindSymbol(aAddressOffset: integer): integer;
var L,R: integer;
begin
  R := high(fSymbol);
  L := 0;
  if (R>=0) and
     (aAddressOffset>=fSymbol[0].Start) and
     (aAddressOffset<=fSymbol[R].Stop) then
  repeat
    result := (L+R)shr 1;
    with fSymbol[result] do
      if aAddressOffset<Start then
        R := result-1 else
      if aAddressOffset>Stop then
        L := result+1 else
        exit;
  until L>R;
  result := -1;
end;

function TSynMapFile.FindUnit(aAddressOffset: integer; out LineNumber: integer): integer;
var L,R,n,max: integer;
begin
  LineNumber := 0;
  R := high(fUnit);
  L := 0;
  if (R>=0) and
     (aAddressOffset>=fUnit[0].Symbol.Start) and
     (aAddressOffset<=fUnit[R].Symbol.Stop) then
  repeat
    result := (L+R) shr 1;
    with fUnit[result] do
      if aAddressOffset<Symbol.Start then
        R := result-1 else
      if aAddressOffset>Symbol.Stop then
        L := result+1 else begin
        // unit found -> search line number
        L := 0;
        max := high(Addr);
        R := max;
        if R>=0 then
        repeat
          n := (L+R) shr 1;
          if aAddressOffset<Addr[n] then
            R := n-1 else
          if (n<max) and (aAddressOffset>=Addr[n+1]) then
            L := n+1 else begin
            LineNumber := Line[n];
            exit;
          end;
        until L>R;
        exit;
      end;
  until L>R;
  result := -1;
end;

var
  InstanceMapFile: TSynMapFile;

function TSynMapFile.AbsoluteToOffset(aAddressAbsolute: PtrUInt): integer;
begin
  if InstanceMapFile=nil then
    result := 0 else
    result := PtrInt(aAddressAbsolute)-PtrInt(InstanceMapFile.fCodeOffset);
end;

class procedure TSynMapFile.Log(W: TTextWriter; aAddressAbsolute: PtrUInt;
  AllowNotCodeAddr: boolean);
var u, s, Line, offset: integer;
begin
  if (W=nil) or (aAddressAbsolute=0) or (InstanceMapFile=nil) then
    exit;
  with InstanceMapFile do
  if HasDebugInfo then begin
    offset := AbsoluteToOffset(aAddressAbsolute);
    s := FindSymbol(offset);
    u := FindUnit(offset,Line);
    if s<0 then begin
      if u<0 then begin
        if AllowNotCodeAddr then begin
          W.AddPointer(aAddressAbsolute);
          W.Add(' ');
        end;
        exit;
      end;
    end else
      if (u>=0) and (s>=0) and not AllowNotCodeAddr then
        if u=fUnitSynLogIndex  then
          exit else // don't log stack trace internal to SynLog.pas :)
        if (u=fUnitSystemIndex) and (PosEx('Except',Symbols[s].Name)>0) then
          exit; // do not log stack trace of System.SysRaiseException
    W.AddPointer(aAddressAbsolute); // display addresses inside known Delphi code
    W.Add(' ');
    if u>=0 then begin
      W.AddString(Units[u].Symbol.Name);
      if s>=0 then
        if Symbols[s].Name=Units[u].Symbol.Name then
          s := -1 else
          W.Add('.');
    end;
    if s>=0 then
      W.AddString(Symbols[s].Name);
    W.Add(' ');
    if Line>0 then begin
      W.Add('(');
      W.Add(Line);
      W.Add(')',' ');
    end;
  end else begin
    W.AddPointer(aAddressAbsolute); // no .map info available -> display address
    W.Add(' ');
  end;
end;

function TSynMapFile.FindLocation(aAddressAbsolute: PtrUInt): RawUTF8;
var u,s,Line,offset: integer;
begin
  result := '';
  if (aAddressAbsolute=0) or not HasDebugInfo then
    exit;
  offset := AbsoluteToOffset(aAddressAbsolute);
  s := FindSymbol(offset);
  u := FindUnit(offset,Line);
  if (s<0) and (u<0) then
    exit;
  if u>=0 then begin
    with Units[u] do begin
      if FileName<>'' then
        result := FileName+' - ';
      result := result+Symbol.Name;
    end;
    if s>=0 then
      if Symbols[s].Name=Units[u].Symbol.Name then
        s := -1 else
        result := result+'.';
  end;
  if s>=0 then
    result := result+Symbols[s].Name;
  if Line>0 then
    result := result+' ('+Int32ToUtf8(Line)+')';
end;


{ TSynLogFamily }

type
  /// an array to all available per-thread TSynLogFile instances
  TSynLogFileIndex = array[0..MAX_SYNLOGFAMILY] of integer;

var
  /// internal list of registered TSynLogFamily
  // - up to MAX_SYNLOGFAMILY+1 families may be defined
  SynLogFamily: TObjectList = nil;

  /// internal list of created TSynLog instance, one per each log file on disk
  // - do not use directly - necessary for inlining TSynLogFamily.SynLog method
  // - also used by AutoFlushProc() to get a global list of TSynLog instances
  SynLogFileList: TObjectListLocked = nil;

threadvar
  /// each thread can access to its own TSynLogFile
  // - is used to implement TSynLogFamily.PerThreadLog=ptOneFilePerThread option
  // - the current TSynLogFile instance of the living thread is
  // ! SynLogFileList[SynLogFileIndexThreadVar[TSynLogFamily.Ident]-1]
  SynLogFileIndexThreadVar: TSynLogFileIndex;

/// if defined, will use AddVectoredExceptionHandler() API call
// - this one does not produce accurate stack trace by now, and is supported
// only since Windows XP
// - so default method using RTLUnwindProc should be prefered
{.$define WITH_VECTOREXCEPT}

{$ifndef NOEXCEPTIONINTERCEPT}

var
  /// used internaly by function GetHandleExceptionSynLog
  CurrentHandleExceptionSynLog: TSynLog;

// this is the main entry point for all intercepted exceptions
procedure SynLogException(const Ctxt: TSynLogExceptionContext);
  function GetHandleExceptionSynLog: TSynLog;
  var Index: ^TSynLogFileIndex;
      i: integer;
      ndx, n: cardinal;
  begin
    result := nil;
    if SynLogFileList=nil then begin
      // we are here if no log content was generated yet (i.e. no log file yet)
      for i := 0 to SynLogFamily.Count-1 do
        with TSynLogFamily(SynLogFamily.List[i]) do
        if fHandleExceptions then begin
          result := SynLog;
          exit;
        end;
    end else begin
      SynLogFileList.Lock;
      try
        Index := @SynLogFileIndexThreadVar;
        n := SynLogFileList.Count;
        for i := 0 to high(Index^) do begin
          ndx := Index^[i]-1;
          if ndx<=n then begin
            result := TSynLog(SynLogFileList.List[ndx]);
            if result.fFamily.fHandleExceptions then
              exit;
          end;
        end;
        for i := 0 to n-1 do begin
          result := TSynLog(SynLogFileList.List[i]);
          if result.fFamily.fHandleExceptions then
            exit;
        end;
        result := nil;
      finally
        SynLogFileList.UnLock;
      end;
    end;
  end;
var SynLog: TSynLog;
begin
  SynLog := CurrentHandleExceptionSynLog;
  if (SynLog=nil) or not SynLog.fFamily.fHandleExceptions then 
    SynLog := GetHandleExceptionSynLog;
  if (SynLog=nil) or not (Ctxt.ELevel in SynLog.fFamily.Level) then
    exit;
  if SynLog.fFamily.ExceptionIgnore.IndexOf(Ctxt.EClass)>=0 then
    exit;
  if SynLog.LogHeaderLock(Ctxt.ELevel) then
  try
    repeat
      if (Ctxt.ELevel=sllException) and (Ctxt.EInstance<>nil) and
         Ctxt.EInstance.InheritsFrom(ESynException) then begin
        if ESynException(Ctxt.EInstance).CustomLog(SynLog.fWriter,Ctxt) then
          break;
      end else
      if Assigned(TSynLogExceptionToStrCustom) then begin
        if TSynLogExceptionToStrCustom(SynLog.fWriter,Ctxt) then
          break;
      end else
        if DefaultSynLogExceptionToStr(SynLog.fWriter,Ctxt) then
          break;
      SynLog.fWriter.AddShort(' at ');
      TSynMapFile.Log(SynLog.fWriter,Ctxt.EAddr,true);
      {$ifndef WITH_VECTOREXCEPT} // stack frame OK for RTLUnwindProc by now
      SynLog.AddStackTrace(Ctxt.EStack);
      {$endif}
      break;
    until false;
    SynLog.fWriter.AddEndOfLine(SynLog.fCurrentLevel);
    SynLog.fWriter.FlushToStream; // we expect exceptions to be available on disk
  finally
    LeaveCriticalSection(SynLog.fThreadLock);
  end;
end;

{$ifdef CPU64}
  {$define WITH_VECTOREXCEPT}
{$endif}

{$ifdef DELPHI5OROLDER}
  {$define WITH_PATCHEXCEPT}
{$endif}

{$ifdef KYLIX3}
  // Kylix has a totally diverse exception scheme
  {$define WITH_MAPPED_EXCEPTIONS}
{$endif}

{$ifdef WITH_PATCHEXCEPT}

var
  // Delphi 5 doesn't define the needed RTLUnwindProc variable :(
  // so we will patch the System.pas RTL in-place
  RTLUnwindProc: Pointer;

procedure PatchCallRtlUnWind;
procedure Patch(P: PAnsiChar);
{   004038B6 52               push edx  // Save exception object
    004038B7 51               push ecx  // Save exception address
    004038B8 8B542428         mov edx,[esp+$28]
    004038BC 83480402         or dword ptr [eax+$04],$02
    004038C0 56               push esi  // Save handler entry
    004038C1 6A00             push $00
    004038C3 50               push eax
    004038C4 68CF384000       push $004038cf  // @@returnAddress
    004038C9 52               push edx
    004038CA E88DD8FFFF       call RtlUnwind
    ...
RtlUnwind:
    0040115C FF255CC14100     jmp dword ptr [$0041c15c]
    where $0041c15c is a pointer to the address of RtlUnWind in kernel32.dll
    -> we will replace [$0041c15c] by [RTLUnwindProc]    }
var i: Integer;
    addr: PAnsiChar;
begin
  for i := 0 to 31 do
    if (PCardinal(P)^=$6850006a) and  // push 0; push eax; push @@returnAddress
       (PWord(P+8)^=$E852) then begin // push edx; call RtlUnwind
      inc(P,10); // go to call RtlUnwind address
      if PInteger(P)^<0 then begin
        addr := P+4+PInteger(P)^;
        if PWord(addr)^=$25FF then begin // jmp dword ptr []
          PatchCodePtrUInt(Pointer(addr+2),cardinal(@RTLUnwindProc));
          exit;
        end;
      end;
    end else
    inc(P);
end;
asm
  mov eax,offset System.@HandleAnyException+200
  call Patch
end;

// the original unwider function, from the Windows API
procedure oldUnWindProc; external kernel32 name 'RtlUnwind';

{$endif WITH_PATCHEXCEPT}

{$ifdef WITH_MAPPED_EXCEPTIONS} // Kylix specific exception handling

{$W-} // disable stack frame generation (duplicate from Synopse.inc) 

threadvar
  CurrentTopOfStack: Cardinal;

procedure ComputeCurrentTopOfStack;
const UNWINDFI_TOPOFSTACK  = $BE00EF00; // from SysInit.pas
var top: cardinal;
begin
  asm
    mov top,esp
  end;
  top := (top and (not 3))+4;
  try
    while PCardinal(top)^<>UNWINDFI_TOPOFSTACK do
      inc(top,4);
  except
  end;
  CurrentTopOfStack := top;
end;

function IsBadReadPtr(addr: pointer; len: integer): boolean;
begin
  try
    asm
      mov eax,addr
      mov ecx,len
 @s:  mov dl,[eax]
      inc eax
      dec ecx
      jnz @s
 @e:end;
    result := false; // if we reached here, everything is ok
  except
    result := true;
  end;
end;

// types and constants from from System.pas / unwind.h

type
  PInternalUnwindException = ^TInternalUnwindException; 
  TInternalUnwindException = packed record
    exception_class: LongWord;
    exception_cleanup: Pointer;
    private_1: pointer;
    private_2: LongWord;
  end;

  PInternalRaisedException = ^TInternalRaisedException;
  TInternalRaisedException = packed record
    RefCount: Integer;
    ExceptObject: Exception;
    ExceptionAddr: PtrUInt;
    HandlerEBP: LongWord;
    Flags: LongWord;
    Cleanup: Pointer;
    Prev: PInternalRaisedException;
    ReleaseProc: Pointer;
  end;

const
  Internal_UW_EXC_CLASS_BORLANDCPP = $FBEE0001;
  Internal_UW_EXC_CLASS_BORLANDDELPHI = $FBEE0101;
  Internal_excIsBeingHandled = $00000001;
  Internal_excIsBeingReRaised = $00000002;

var oldUnwinder, newUnwinder: TUnwinder;

function HookedRaiseException(Exc: Pointer): LongBool; cdecl;
var ExcRec: PInternalRaisedException;
    Ctxt: TSynLogExceptionContext;
begin
  if CurrentHandleExceptionSynLog<>nil then
    if Exc<>nil then begin
      Ctxt.ECode := PInternalUnwindException(Exc)^.exception_class;
      case Ctxt.ECode of
      Internal_UW_EXC_CLASS_BORLANDDELPHI: begin
        ExcRec := PInternalUnwindException(Exc)^.private_1;
        if (ExcRec<>nil) and (ExcRec^.ExceptObject is Exception) then begin
          Ctxt.EInstance := ExcRec^.ExceptObject;
          Ctxt.EClass := PPointer(Ctxt.EInstance)^;
          if Ctxt.EInstance is EExternal then begin
            Ctxt.EAddr := EExternal(Ctxt.EInstance).ExceptionAddress;
            Ctxt.ELevel := sllExceptionOS;
          end else begin
            Ctxt.EAddr := ExcRec^.ExceptionAddr;
            Ctxt.ELevel := sllException;
          end;
          Ctxt.EStack := nil;
          SynLogException(Ctxt);
        end;
        // (ExcRec^.Flags and Internal_excIsBeingHandled)<>0,
        // (ExcRec^.Flags and Internal_excIsBeingReRaised)<>0);
      end;
      Internal_UW_EXC_CLASS_BORLANDCPP: ; // not handled
      end;
    end;
  if Assigned(oldUnwinder.RaiseException) then
    result := oldUnwinder.RaiseException(Exc) else
    result := false;
end;

{$else} // "regular" exception handling as defined in System.pas

type
  PExceptionRecord = ^TExceptionRecord;
  TExceptionRecord = record
    ExceptionCode: DWord;
    ExceptionFlags: DWord;
    OuterException: PExceptionRecord;
    ExceptionAddress: PtrUInt;
    NumberParameters: Longint;
    case {IsOsException:} Boolean of
    True:  (ExceptionInformation : array [0..14] of PtrUInt);
    False: (ExceptAddr: PtrUInt; ExceptObject: Exception);
  end;
  GetExceptionClass = function(const P: TExceptionRecord): ExceptClass;

const
  cDelphiExcept = $0EEDFAE0;
  cDelphiException = $0EEDFADE;

{$ifdef MSWINDOWS}
  // see http://msdn.microsoft.com/en-us/library/xcb2z8hs
  cSetThreadNameException = $406D1388;

  DOTNET_EXCEPTIONNAME: array[0..83] of RawUTF8 = (
  'Access', 'AmbiguousMatch', 'appdomainUnloaded', 'Application', 'Argument',
  'ArgumentNull', 'ArgumentOutOfRange', 'Arithmetic', 'ArrayTypeMismatch',
  'BadImageFormat', 'CannotUnloadappdomain', 'ContextMarshal', 'Cryptographic',
  'CryptographicUnexpectedOperation', 'CustomAttributeFormat', 'DirectoryNotFound',
  'DirectoryNotFound', 'DivideByZero', 'DllNotFound', 'DuplicateWaitObject',
  'EndOfStream', 'EntryPointNotFound', '', 'ExecutionEngine', 'External',
  'FieldAccess', 'FileLoad', 'FileLoad', 'FileNotFound', 'Format',
  'IndexOutOfRange', 'InvalidCast', 'InvalidComObject', 'InvalidFilterCriteria',
  'InvalidOleVariantType', 'InvalidOperation', 'InvalidProgram', 'IO',
  'IsolatedStorage', 'MarshalDirective', 'MethodAccess', 'MissingField',
  'MissingManifestResource', 'MissingMember', 'MissingMethod',
  'MulticastNotSupported', 'NotFiniteNumber', 'NotImplemented', 'NotSupported',
  'NullReference', 'OutOfMemory', 'Overflow', 'PlatformNotSupported', 'Policy',
  'Rank', 'ReflectionTypeLoad', 'Remoting', 'RemotingTimeout', 'SafeArrayTypeMismatch',
  'SafeArrayRankMismatch', 'Security', 'SEH', 'Serialization', 'Server', 'StackOverflow',
  'SUDSGenerator', 'SUDSParser', 'SynchronizationLock', 'System', 'Target',
  'TargetInvocation', 'TargetParameterCount', 'ThreadAbort', 'ThreadInterrupted',
  'ThreadState', 'ThreadStop', 'TypeInitialization', 'TypeLoad', 'TypeUnloaded',
  'UnauthorizedAccess', 'InClassConstructor', 'KeyNotFound', 'InsufficientStack',
  'InsufficientMemory');
  // http://blogs.msdn.com/b/yizhang/archive/2010/12/17/interpreting-hresults-returned-from-net-clr-0x8013xxxx.aspx
  DOTNET_EXCEPTIONHRESULT: array[0..83] of cardinal = (
   $8013151A, $8000211D, $80131015, $80131600, $80070057, $80004003, $80131502,
   $80070216, $80131503, $8007000B, $80131015, $80090020, $80004001, $80131431,
   $80131537, $80070003, $80030003, $80020012, $80131524, $80131529, $801338,
   $80131522, $80131500, $80131506, $80004005, $80131507, $80131621, $80131018,
   $80070002, $80131537, $80131508, $80004002, $80131527, $80131601, $80131531,
   $80131509, $8013153A, $80131620, $80131450, $80131535, $80131510, $80131511,
   $80131532, $80131512, $80131513, $80131514, $80131528, $80004001, $80131515,
   $80004003, $8007000E, $80131516, $80131539, $80131416, $80131517,
   $80131602, $8013150B, $8013150B, $80131533, $80131538, $8013150A, $80004005,
   $8013150C, $8013150E, $800703E9, $80131500, $80131500, $80131518, $80131501,
   $80131603, $80131604, $80138002, $80131530, $80131519, $80131520, $80131521,
   $80131534, $80131522, $80131013, $80070005, $80131543, $80131577, $80131578,
   $8013153D);

type
  // avoid linking of ComObj.pas just for EOleSysError
  EOleSysError = class(Exception)
  public
    ErrorCode: cardinal;
  end;

function ExceptionInheritsFrom(E: TClass; const Name: ShortString): boolean;
begin // avoid linking of ComObj.pas just for EOleSysError
  while (E<>nil) and (E<>Exception) do
    if IdemPropName(PShortString(PPointer(PtrInt(E)+vmtClassName)^)^,Name) then begin
      result := true;
      exit;
    end else begin
      E := PPointer(PtrInt(E)+vmtParent)^;
      if E<>nil then
        E := PPointer(E)^;
    end;
  result := false;
end;

{$endif MSWINDOWS}

{$endif WITH_MAPPED_EXCEPTIONS}

function InternalDefaultSynLogExceptionToStr(
  WR: TTextWriter; const Context: TSynLogExceptionContext): boolean;
{$ifdef MSWINDOWS}
var i: integer;
{$endif}
begin
  WR.AddClassName(Context.EClass);
  if (Context.ELevel=sllException) and (Context.EInstance<>nil) and
     (Context.EClass<>EExternalException) then begin
    {$ifdef MSWINDOWS}
    if ExceptionInheritsFrom(Context.EClass,'EOleSysError') then begin
      WR.Add(' ');
      WR.AddPointer(EOleSysError(Context.EInstance).ErrorCode);
      for i := 0 to high(DOTNET_EXCEPTIONHRESULT) do
        if DOTNET_EXCEPTIONHRESULT[i]=EOleSysError(Context.EInstance).ErrorCode then begin
          WR.AddShort(' [.NET/CLR unhandled ');
          WR.AddString(DOTNET_EXCEPTIONNAME[i]);
          WR.AddShort('Exception]');
        end; // no break on purpose, if ErrorCode matches more than one Exception
    end;
    {$endif}
    WR.Add(' ');
    if (WR.ClassType=TTextWriter) or
       not Context.EInstance.InheritsFrom(ESynException) then begin
      WR.AddShort('("');
      WR.AddJSONEscapeString(Context.EInstance.Message);
      WR.AddShort('")');
    end else
       WR.WriteObject(Context.EInstance);
  end else begin
    WR.AddShort(' (');
    WR.AddPointer(Context.ECode);
    WR.AddShort(')');
  end;
  result := false; // caller should append "at EAddr" and the stack trace
end;

{$ifndef WITH_PATCHEXCEPT}

{$ifdef WITH_MAPPED_EXCEPTIONS}

{$else NO WITH_MAPPED_EXCEPTIONS}

procedure LogExcept(stack: PPtrUInt; const Exc: TExceptionRecord);
var Ctxt: TSynLogExceptionContext;
    LastError: DWORD;
begin
  {$ifdef MSWINDOWS}
  if Exc.ExceptionCode=cSetThreadNameException then
    exit;
  {$endif}
  LastError := GetLastError;
  Ctxt.ECode := Exc.ExceptionCode;
  if (Exc.ExceptionCode=cDelphiException) and (Exc.ExceptObject<>nil) then begin
    if Exc.ExceptObject.InheritsFrom(Exception) then
      Ctxt.EClass := PPointer(Exc.ExceptObject)^ else
      Ctxt.EClass := EExternalException;
    Ctxt.EInstance := Exc.ExceptObject;
    Ctxt.ELevel := sllException;
    Ctxt.EAddr := Exc.ExceptAddr;
  end else begin
    {$ifdef MSWINDOWS}
    if Assigned(ExceptClsProc) then
      Ctxt.EClass := GetExceptionClass(ExceptClsProc)(Exc) else
    {$endif}
      Ctxt.EClass := EExternal;
    Ctxt.EInstance := nil;
    Ctxt.ELevel := sllExceptionOS;
    Ctxt.EAddr := Exc.ExceptionAddress;
  end;
  Ctxt.EStack := stack;
  SynLogException(Ctxt);
  SetLastError(LastError); // code above could have changed this
end;

{$ifdef WITH_VECTOREXCEPT}

type
  PExceptionInfo = ^TExceptionInfo;
  TExceptionInfo = packed record
    ExceptionRecord: PExceptionRecord;
    ContextRecord: pointer;
  end;

var
  AddVectoredExceptionHandler: function(FirstHandler: cardinal;
    VectoredHandler: pointer): PtrInt; stdcall;

function SynLogVectoredHandler(ExceptionInfo : PExceptionInfo): PtrInt; stdcall;
const EXCEPTION_CONTINUE_SEARCH = 0;
begin
  if CurrentHandleExceptionSynLog<>nil then
    LogExcept(nil,ExceptionInfo^.ExceptionRecord^);
  result := EXCEPTION_CONTINUE_SEARCH;
end;

{$else WITH_VECTOREXCEPT}

var oldUnWindProc: pointer;

procedure SynRtlUnwind(TargetFrame, TargetIp: pointer;
  ExceptionRecord: PExceptionRecord; ReturnValue: Pointer); stdcall;
asm
  pushad
  cmp  dword ptr CurrentHandleExceptionSynLog,0
  jz   @oldproc
  mov  eax,TargetFrame
  mov  edx,ExceptionRecord
  call LogExcept
@oldproc:
  popad
  pop ebp // hidden push ebp at asm level
  jmp oldUnWindProc
end;

{$endif WITH_VECTOREXCEPT}

{$endif WITH_MAPPED_EXCEPTIONS}

{$endif WITH_PATCHEXCEPT}

{$endif NOEXCEPTIONINTERCEPT}


procedure TSynLogFamily.SetDestinationPath(const value: TFileName);
begin
  if value='' then
    fDestinationPath := ExeVersion.ProgramFilePath else
    fDestinationPath := IncludeTrailingPathDelimiter(value);
end;

procedure TSynLogFamily.SetLevel(aLevel: TSynLogInfos);
begin
  // ensure BOTH Enter+Leave are always selected at once, if any is set
  if sllEnter in aLevel then
    include(aLevel,sllLeave) else
  if sllLeave in aLevel then
    include(aLevel,sllEnter);
  fLevel := aLevel;
{$ifndef NOEXCEPTIONINTERCEPT}
  // intercept exceptions, if necessary
  fHandleExceptions := (sllExceptionOS in aLevel) or (sllException in aLevel);
  if fHandleExceptions and (CurrentHandleExceptionSynLog=nil) then begin
    SynLog; // force CurrentHandleExceptionSynLog definition
    {$ifdef WITH_MAPPED_EXCEPTIONS}
    GetUnwinder(oldUnwinder);
    newUnwinder := oldUnwinder;
    newUnwinder.RaiseException := HookedRaiseException;
    SetUnwinder(newUnwinder);
    {$else}
    {$ifdef WITH_VECTOREXCEPT}
    AddVectoredExceptionHandler :=
      GetProcAddress(GetModuleHandle(kernel32),'AddVectoredExceptionHandler');
    // RemoveVectoredContinueHandler() is available under 64 bit editions only
    if Assigned(AddVectoredExceptionHandler) then
      // available since Windows XP
      AddVectoredExceptionHandler(0,@SynLogVectoredHandler);
    {$else WITH_VECTOREXCEPT}
    {$ifdef WITH_PATCHEXCEPT}
    PatchCallRtlUnWind;
    {$else}
    oldUnWindProc := RTLUnwindProc;
    RTLUnwindProc := @SynRtlUnwind;
    {$endif}
    {$endif WITH_VECTOREXCEPT}
    {$endif WITH_MAPPED_EXCEPTIONS}
  end;
{$endif NOEXCEPTIONINTERCEPT}
end;

procedure TSynLogFamily.SetEchoToConsole(aEnabled: TSynLogInfos);
begin
  if (self=nil) or (aEnabled=fEchoToConsole) then
    exit;
  fEchoToConsole := aEnabled;
end;

constructor TSynLogFamily.Create(aSynLog: TSynLogClass);
begin
  fSynLogClass := aSynLog;
  if SynLogFamily=nil then
    GarbageCollectorFreeAndNil(SynLogFamily,TList.Create);
  fIdent := SynLogFamily.Add(self);
  fDestinationPath := ExeVersion.ProgramFilePath; // use .exe path
  fDefaultExtension := '.log';
  fArchivePath := fDestinationPath;
  fArchiveAfterDays := 7;
  fRotateFileAtHour := -1;
  fBufferSize := 4096;
  fStackTraceLevel := 30;
  {$ifndef FPC}
  if DebugHook<>0 then // never let stManualAndAPI trigger AV within the IDE
    fStackTraceUse := stOnlyAPI;
  {$endif}
  fExceptionIgnore := TList.Create;
  fLevelStackTrace :=
    [sllError,sllException,sllExceptionOS,sllFail,sllLastError,sllStackTrace,sllDDDError];
end;

function TSynLogFamily.CreateSynLog: TSynLog;
var i: integer;
begin
  if SynLogFileList=nil then
    GarbageCollectorFreeAndNil(SynLogFileList,TObjectListLocked.Create);
  SynLogFileList.Lock;
  try
    result := fSynLogClass.Create(self);
    i := SynLogFileList.Add(result);
    if fPerThreadLog=ptOneFilePerThread then 
      if (fRotateFileCount=0) and (fRotateFileSize=0) and (fRotateFileAtHour<0) then 
        SynLogFileIndexThreadVar[fIdent] := i+1 else begin
        fPerThreadLog := ptIdentifiedInOnFile; // excluded by rotation
        fGlobalLog := result;
      end else
      fGlobalLog := result;
  finally
    SynLogFileList.UnLock;
  end;
end;

{$ifdef MSWINDOWS}

var
  AutoFlushThread: THandle = 0;
  AutoFlushSecondElapsed: cardinal;

procedure AutoFlushProc(P: pointer); stdcall;  // TThread not needed here
var i: integer;
begin
  SetCurrentThreadName('TSynLog AutoFlush',[]);
  repeat
    for i := 1 to 10 do begin // check every second for pending data
      SleepHiRes(100);
      if AutoFlushThread=0 then
        exit; // avoid GPF
    end;
    if SynLogFileList=nil then
      continue; // nothing to flush
    inc(AutoFlushSecondElapsed);
    SynLogFileList.Lock;
    try
      for i := 0 to SynLogFileList.Count-1 do
      with TSynLog(SynLogFileList.List[i]) do
        if AutoFlushThread=0 then
          break else // avoid GPF
        if (fFamily.fAutoFlush<>0) and (fWriter<>nil) and
           (AutoFlushSecondElapsed mod fFamily.fAutoFlush=0) then
          if fWriter.PendingBytes>1 then begin
            if not IsMultiThread then
              if not fWriterStream.InheritsFrom(TFileStream) then
                IsMultiThread := true; // only TFileStream is thread-safe
            Flush(false); // write pending data
          end;
     finally
       SynLogFileList.UnLock;
     end;
  until false;
  ExitThread(0);
end;

procedure TSynLogFamily.SetAutoFlush(TimeOut: cardinal);
var ID: cardinal;
begin
  fAutoFlush := TimeOut;
  if (AutoFlushThread=0) and (TimeOut<>0) {$ifndef FPC}and (DebugHook=0){$endif} then begin
    AutoFlushThread := CreateThread(nil,0,@AutoFlushProc,nil,0,ID);
    AutoFlushSecondElapsed := 0;
  end;
end;

{$endif}

destructor TSynLogFamily.Destroy;
var SR: TSearchRec;
    OldTime: integer;
    aTime: TDateTime;
    Y,M,D: word;
    aOldLogFileName, aPath: TFileName;
    tmp: array[0..11] of AnsiChar;
begin
  fDestroying := true;
  EchoRemoteStop;
  {$ifdef MSWINDOWS}
  if AutoFlushThread<>0 then
    AutoFlushThread := 0; // mark thread released to avoid GPF in AutoFlushProc
  {$endif}
  ExceptionIgnore.Free;
  try
    if Assigned(OnArchive) then
    if FindFirst(DestinationPath+'*'+DefaultExtension,faAnyFile,SR)=0 then
    try
      if ArchiveAfterDays<0 then
        ArchiveAfterDays := 0;
      OldTime := DateTimeToFileDate(Now-ArchiveAfterDays);
      repeat
        {$ifndef DELPHI5OROLDER}
        {$WARN SYMBOL_DEPRECATED OFF} // for SR.Time
        {$endif}
        if (SR.Name[1]='.') or (faDirectory and SR.Attr<>0) or
           (SR.Time>OldTime) then
          continue;
        {$ifndef DELPHI5OROLDER}
        {$WARN SYMBOL_DEPRECATED ON}
        {$endif}
        aOldLogFileName := DestinationPath+SR.Name;
        if aPath='' then begin
          aTime := FileAgeToDateTime(aOldLogFileName);
          if (aTime=0) or
             not DirectoryExists(ArchivePath+'log') and
             not CreateDir(ArchivePath+'log') then
            break;
          DecodeDate(aTime,Y,M,D);
          PCardinal(@tmp)^ := ord('l')+ord('o') shl 8+ord('g') shl 16+ord(PathDelim) shl 24;
          YearToPChar(Y,@tmp[4]);
          PWord(@tmp[8])^ := TwoDigitLookupW[M];
          PWord(@tmp[10])^ := ord(PathDelim);
          aPath := ArchivePath+Ansi7ToString(tmp,11);
        end;
        OnArchive(aOldLogFileName,aPath);
      until FindNext(SR)<>0;
    finally
      try
        OnArchive('',aPath); // mark no more .log file to archive -> close .zip
      finally
        FindClose(SR);
      end;
    end;
  finally
    {$ifdef MSWINDOWS}
    if AutoFlushThread<>0 then
      CloseHandle(AutoFlushThread); // release background thread once for all
    {$endif}
    inherited;
  end;
end;

function TSynLogFamily.SynLog: TSynLog;
var ndx: integer;
begin
  if self=nil then
    result := nil else begin
    if (fPerThreadLog=ptOneFilePerThread) and (fRotateFileCount=0) and
       (fRotateFileSize=0) and (fRotateFileAtHour<0) then begin
      ndx := SynLogFileIndexThreadVar[fIdent]-1;
      if ndx>=0 then // SynLogFileList.Lock/Unlock is not mandatory here
        result := SynLogFileList.List[ndx] else
        result := CreateSynLog;
    end else // for ptMergedInOneFile and ptIdentifiedInOnFile
      if fGlobalLog<>nil then
        result := fGlobalLog else
        result := CreateSynLog;
{$ifndef NOEXCEPTIONINTERCEPT}
    if fHandleExceptions and (CurrentHandleExceptionSynLog<>result) then
      CurrentHandleExceptionSynLog := result;
{$endif}
  end;
end;

procedure TSynLogFamily.EchoRemoteStart(aClient: TObject;
  const aClientEvent: TOnTextWriterEcho; aClientOwnedByFamily: boolean);
var i: integer;
begin
  EchoRemoteStop;
  fEchoRemoteClient := aClient;
  fEchoRemoteEvent := aClientEvent;
  fEchoRemoteClientOwned := aClientOwnedByFamily;
  SynLogFileList.Lock;
  try
    for i := 0 to SynLogFileList.Count-1 do
      if TSynLog(SynLogFileList.List[i]).fFamily=self then
        TSynLog(SynLogFileList.List[i]).fWriter.EchoAdd(fEchoRemoteEvent);
  finally
    SynLogFileList.Unlock;
  end;
end;

procedure TSynLogFamily.EchoRemoteStop;
var i: integer;
begin
  if fEchoRemoteClient=nil then
    exit;
  if fEchoRemoteClientOwned then
    try
      try
        fEchoRemoteEvent(nil,sllClient,
          FormatUTF8('%00%    Remote Client % Disconnected',
          [NowToString(false),LOG_LEVEL_TEXT[sllClient],self]));
      finally
        FreeAndNil(fEchoRemoteClient);
      end;
    except
      on Exception do ;
    end else
    fEchoRemoteClient := nil;
  if SynLogFileList<>nil then begin
    SynLogFileList.Lock;
    try
      for i := 0 to SynLogFileList.Count-1 do
        if TSynLog(SynLogFileList.List[i]).fFamily=self then
          TSynLog(SynLogFileList.List[i]).fWriter.EchoRemove(fEchoRemoteEvent);
    finally
      SynLogFileList.Unlock;
    end;
  end;
  fEchoRemoteEvent := nil;
end;


{ TSynLog }

const
  // maximum of thread IDs which can exist for a process
  // - shall be a power of 2 (used for internal TSynLog.fThreadHash)
  // - with the default 1MB stack size, max is around 2000 threads for Win32
  // - thread IDs are recycled when released, and you always should use a thread
  // pool (like we do for all our mORMot servers, including http.sys based)
  MAXLOGTHREAD = 1 shl 15;

procedure TSynLog.LockAndGetThreadContextInternal(ID: TThreadID);
var hash: integer;
    Pass2: boolean;
label storendx;
begin
  hash := PtrUInt(ID) and (MAXLOGTHREAD-1);
  Pass2 := false;
  fThreadIndex := fThreadHash[hash];
  if fThreadIndex<>0 then
    repeat
      if fThreadContexts[fThreadIndex-1].ID=ID then
        goto storendx; // found the ID
      // hash collision -> try next item in fThreadHash[] if possible
      if hash>=MAXLOGTHREAD then
        if Pass2 then begin
         fThreadIndex := 0;
         goto storendx;
        end else begin
          hash := 0;
          Pass2 := true;
        end else
        inc(hash);
      fThreadIndex := fThreadHash[hash];
    until fThreadIndex=0;
  // here we know that fThreadIndex=fThreadHash[hash]=0
  if fThreadContextCount<MAXLOGTHREAD then begin
    if fThreadContextCount>=length(fThreadContexts) then
      SetLength(fThreadContexts,fThreadContextCount+256);
    fThreadContexts[fThreadContextCount].ID := ID;
    inc(fThreadContextCount);
    fThreadHash[hash] := fThreadContextCount;
  end;
  fThreadIndex := fThreadContextCount;
storendx:
  fThreadID := ID;
  fThreadContext := @fThreadContexts[fThreadIndex];
end;

procedure TSynLog.LockAndGetThreadContext;
var ID: TThreadID;
begin
  EnterCriticalSection(fThreadLock);
  ID := GetCurrentThreadId;
  if ID<>fThreadID then
    LockAndGetThreadContextInternal(ID);
end;

function TSynLog._AddRef: Integer;
begin
  if fFamily.Level*[sllEnter,sllLeave]<>[] then begin
    LockAndGetThreadContext;
    with fThreadContext^ do
    if RecursionCount>0 then
      with Recursion[RecursionCount-1] do begin
        if (RefCount=0) and (sllEnter in fFamily.Level) then
          DoEnterLeave(sllEnter);
        inc(RefCount);
        result := RefCount;
      end else
      result := 1; // should never be 0 (mark release of TSynLog instance)
    LeaveCriticalSection(fThreadLock);
  end else
    result := 1;
end;

{$ifdef MSWINDOWS}
var
  RtlCaptureStackBackTraceRetrieved: (btUntested, btOK, btFailed) = btUntested;
  RtlCaptureStackBackTrace: function(FramesToSkip, FramesToCapture: cardinal;
    BackTrace, BackTraceHash: pointer): byte; stdcall;
{$endif}

{$STACKFRAMES ON}
function TSynLog._Release: Integer;
{$ifndef CPU64}
{$ifndef PUREPASCAL}
var aStackFrame: PtrInt;
{$endif}
{$endif}
begin
  if fFamily.Level*[sllEnter,sllLeave]<>[] then begin
    LockAndGetThreadContext;
    with fThreadContext^ do
    if RecursionCount>0 then begin
      with Recursion[RecursionCount-1] do begin
        dec(RefCount);
        if RefCount=0 then begin
          if sllLeave in fFamily.Level then begin
            if MethodName=nil then begin
              {$ifdef CPU64}
              {$ifdef MSWINDOWS}
              if RtlCaptureStackBackTrace(1,1,@Caller,nil)=0 then
                Caller := 0 else
                dec(Caller,5); // ignore caller op codes
              {$else}
              Caller := 0; // no stack trace yet under Linux64
              {$endif}
              {$else}
              {$ifdef PUREPASCAL}
              Caller := 0; // e.g. ARM Linux
              {$else}
              asm
                mov eax,[ebp+16] // +4->_IntfClear +16->initial caller
                mov aStackFrame,eax
              end;
              Caller := aStackFrame-5;
              {$endif}
              {$endif}
            end;
            DoEnterLeave(sllLeave);
          end;
          dec(RecursionCount);
        end;
        result := RefCount;
      end;
    end else
      result := 1; // should never be 0 (mark release of TSynLog)
    LeaveCriticalSection(fThreadLock);
  end else
    result := 1;
end;
{$STACKFRAMES OFF}

constructor TSynLog.Create(aFamily: TSynLogFamily);
begin
  if aFamily=nil then
    aFamily := Family;
  fFamily := aFamily;
  InitializeCriticalSection(fThreadLock);
  {$ifdef MSWINDOWS}
  if RtlCaptureStackBackTraceRetrieved=btUntested then begin
    if OSVersion<wXP then
      RtlCaptureStackBackTraceRetrieved := btFailed else begin
     @RtlCaptureStackBackTrace := GetProcAddress(
       GetModuleHandle(kernel32),'RtlCaptureStackBackTrace');
     if @RtlCaptureStackBackTrace=nil then
       RtlCaptureStackBackTraceRetrieved := btFailed else
       RtlCaptureStackBackTraceRetrieved := btOK;
    end;
  end;
  {$ifdef CPU64}
  assert(RtlCaptureStackBackTraceRetrieved=btOK);
  {$endif}
  {$endif}
  SetLength(fThreadHash,MAXLOGTHREAD); // 64 KB buffer
  SetLength(fThreadContexts,256);
end;

destructor TSynLog.Destroy;
begin
{$ifndef NOEXCEPTIONINTERCEPT}
  if fFamily.fHandleExceptions and (CurrentHandleExceptionSynLog=self) then
    CurrentHandleExceptionSynLog := nil;
{$endif}
  Flush(true);
  fWriterStream.Free;
  fWriter.Free;
  DeleteCriticalSection(fThreadLock);
  inherited;
end;

procedure TSynLog.CloseLogFile;
begin
  if fWriter=nil then
    exit;
  EnterCriticalSection(fThreadLock);
  try
    fWriter.FlushFinal;
    FreeAndNil(fWriterStream);
    FreeAndNil(fWriter);
  finally
    LeaveCriticalSection(fThreadLock);
  end;
end;

procedure TSynLog.Release;
begin
  SynLogFileList.Lock;
  try
    CloseLogFile;
    SynLogFileList.Remove(self);
    if fFamily.fPerThreadLog=ptOneFilePerThread then
      SynLogFileIndexThreadVar[fFamily.fIdent] := 0;
  finally
    SynLogFileList.Unlock;
  end;
  Free;
end;

procedure TSynLog.Flush(ForceDiskWrite: boolean);
begin
  if fWriter=nil then
    exit;
  EnterCriticalSection(fThreadLock);
  try
    fWriter.FlushToStream;
    {$ifdef MSWINDOWS}
    if ForceDiskWrite and fWriterStream.InheritsFrom(TFileStream) then
      FlushFileBuffers(TFileStream(fWriterStream).Handle);
    {$endif}
  finally
    LeaveCriticalSection(fThreadLock);
  end;
end;

{$ifdef FPC}
function TSynLog.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := E_NOINTERFACE;
end;
{$else}
function TSynLog.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := E_NOINTERFACE;
end;
{$endif}

class function TSynLog.Add: TSynLog;
begin
  result := Family.SynLog;
end;

{$STACKFRAMES ON}
class function TSynLog.Enter(aInstance: TObject; aMethodName: PUTF8Char;
  aMethodNameLocal: boolean): ISynLog;
var aSynLog: TSynLog;
    aFamily: TSynLogFamily;
    aStackFrame: PtrUInt;
begin
  // inlined aSynLog := Family.SynLog
  if PtrInt(self)=0 then begin
    result := nil;
    exit;
  end;
  aFamily := PPointer(PtrInt(Self)+vmtAutoTable)^;
  if aFamily=nil then
    aSynLog := FamilyCreate.SynLog else
    aSynLog := aFamily.SynLog;
  // recursively store parameters
  with aSynLog do
  if sllEnter in fFamily.fLevel then begin
    LockAndGetThreadContext;
    with fThreadContext^ do begin
      if RecursionCount=RecursionCapacity then begin
        inc(RecursionCapacity,32);
        SetLength(Recursion,RecursionCapacity);
      end;
      {$ifdef CPU64}
      {$ifdef MSWINDOWS}
      if RtlCaptureStackBackTrace(1,1,@aStackFrame,nil)=0 then
        aStackFrame := 0 else
        dec(aStackFrame,5); // ignore call TSynLog.Enter op codes
      {$else}
      aStackFrame := 0; // No stack trace yet under Linux64
      {$endif}
      {$else}
      {$ifdef PUREPASCAL}
      aStackFrame := 0; // e.g. ARM Linux
      {$else}
      asm
        mov eax,[ebp+4]  // retrieve caller EIP from push ebp; mov ebp,esp
        sub eax,5        // ignore call TSynLog.Enter op codes
        mov aStackFrame,eax
      end;
      {$endif}
      {$endif}
      with Recursion[RecursionCount] do begin
        Instance := aInstance;
        if aInstance=nil then
          ClassType := pointer(aInstance) else
          ClassType := PPointer(aInstance)^;
        MethodName := aMethodName;
        if aMethodNameLocal then
          MethodNameLocal := mnEnter else
          MethodNameLocal := mnAlways;
        Caller := aStackFrame;
        RefCount := 0;
      end;
      inc(RecursionCount);
      LeaveCriticalSection(fThreadLock);
    end;
  end;
  // copy to ISynLog interface -> will call TSynLog._AddRef
  result := aSynLog;
end;
{$STACKFRAMES OFF}

class function TSynLog.FamilyCreate: TSynLogFamily;
var PVMT: pointer;
begin // private sub function makes the code faster in most case
  if not InheritsFrom(TSynLog) then
    // invalid call
    result := nil else begin
    // create the properties information from RTTI
    result := TSynLogFamily.Create(self);
    // store the TSynLogFamily instance into "AutoTable" unused VMT entry
    PVMT := pointer(PtrInt(self)+vmtAutoTable);
    if PPointer(PVMT)^<>nil then
      raise ESynException.CreateUTF8('%.AutoTable VMT entry already set',[self]);
    PatchCodePtrUInt(PVMT,PtrUInt(result),true); // LeaveUnprotected=true
    // register to the internal garbage collection (avoid memory leak)
    GarbageCollectorFreeAndNil(PVMT^,result); // set to nil at finalization
  end;
end;

{$ifdef PUREPASCAL}
class function TSynLog.Family: TSynLogFamily;
begin
  if Self<>nil then begin
    result := PPointer(PtrInt(Self)+vmtAutoTable)^;
    if result=nil then
      result := FamilyCreate;
  end else
    result := nil;
end;
{$else}
class function TSynLog.Family: TSynLogFamily;
asm
  or eax,eax
  jz @null
  mov edx,[eax+vmtAutoTable]
  or edx,edx
  jz FamilyCreate
  mov eax,edx
@null:
end;
{$endif}

type
  TSynLogVoid = class(TSynLog);

class function TSynLog.Void: TSynLogClass;
begin
  TSynLogVoid.Family.Level := [];
  result := TSynLogVoid;
end;

function TSynLog.Instance: TSynLog;
begin
  result := self;
end;

{$I-}
function TSynLog.ConsoleEcho(Sender: TTextWriter; Level: TSynLogInfo;
  const Text: RawUTF8): boolean;
{$ifdef MSWINDOWS}
var tmp: AnsiString;
{$endif}
const LOGCOLORS: array[TSynLogInfo] of TConsoleColor = (
//    sllNone, sllInfo, sllDebug, sllTrace, sllWarning, sllError, sllEnter, sllLeave
  ccLightGray,ccWhite,ccLightGray,ccLightBlue,ccBrown,ccLightRed,ccGreen,ccGreen,
//    sllLastError, sllException, sllExceptionOS, sllMemory, sllStackTrace,
  ccLightRed, ccLightRed, ccLightRed, ccLightGray, ccCyan,
//    sllFail, sllSQL, sllCache, sllResult, sllDB, sllHTTP, sllClient, sllServer,
  ccLightRed, ccBrown, ccBlue, ccLightCyan, ccMagenta, ccCyan, ccLightCyan, ccLightCyan,
//    sllServiceCall, sllServiceReturn, sllUserAuth,
  ccLightMagenta, ccLightMagenta, ccMagenta,
//    sllCustom1, sllCustom2, sllCustom3, sllCustom4,
  ccLightGray, ccLightGray,ccLightGray,ccLightGray,
//    sllNewRun, sllDDDError, sllDDDInfo, sllMonitoring
  ccLightMagenta, ccLightRed, ccWhite, ccLightBlue);
begin
  result := true;
  if not (Level in fFamily.fEchoToConsole) then
    exit;
  TextColor(LOGCOLORS[Level]);
  {$ifdef MSWINDOWS}
  tmp := CurrentAnsiConvert.UTF8ToAnsi(Text);
  AnsiToOem(pointer(tmp),pointer(tmp));
  writeln(tmp);
  {$else}
  write(Text,#13#10);
  {$endif}
  ioresult;
  TextColor(ccLightGray);
end;
{$I+}

procedure TSynLog.Log(Level: TSynLogInfo; const TextFmt: RawUTF8; const TextArgs: array of const;
  aInstance: TObject);
begin
  if (self<>nil) and (Level in fFamily.fLevel) then
    LogInternal(Level,TextFmt,TextArgs,aInstance);
end;

procedure TSynLog.Log(Level: TSynLogInfo; const TextFmt: RawUTF8; const TextArg: RawUTF8;
  aInstance: TObject=nil);
begin
  if (self<>nil) and (Level in fFamily.fLevel) then
    LogInternal(Level,TextFmt,[TextArg],aInstance);
end;

procedure TSynLog.Log(Level: TSynLogInfo; const TextFmt: RawUTF8; const TextArg: Int64;
  aInstance: TObject=nil);
begin
  if (self<>nil) and (Level in fFamily.fLevel) then
    LogInternal(Level,TextFmt,[TextArg],aInstance);
end;

procedure TSynLog.Log(Level: TSynLogInfo; const Text: RawUTF8; aInstance: TObject;
  TextTruncateAtLength: integer);
begin
  if (self<>nil) and (Level in fFamily.fLevel) then
    LogInternal(Level,Text,aInstance,TextTruncateAtLength);
end;

{$ifdef UNICODE}
procedure TSynLog.Log(Level: TSynLogInfo; const Text: string; aInstance: TObject);
begin
  if (self<>nil) and (Level in fFamily.fLevel) then
    LogInternal(Level,'%',[Text],aInstance);
end;
{$endif}

procedure TSynLog.LogLines(Level: TSynLogInfo; LinesToLog: PUTF8Char; aInstance: TObject;
  const IgnoreWhenStartWith: PAnsiChar);
procedure DoLog(LinesToLog: PUTF8Char);
var s: RawUTF8;
begin
  while LinesToLog<>nil do begin
    s := trim(GetNextLine(LinesToLog,LinesToLog));
    if s<>'' then
      if (IgnoreWhenStartWith=nil) or not IdemPChar(pointer(s),IgnoreWhenStartWith) then
        LogInternal(Level,s,aInstance,maxInt);
  end;
end;
begin
  if (self<>nil) and (Level in fFamily.fLevel) then
    DoLog(LinesToLog);
end;

procedure TSynLog.Log(Level: TSynLogInfo; aInstance: TObject);
begin
  if (self<>nil) and (Level in fFamily.fLevel) then
    if aInstance<>nil then
      LogInternal(Level,'',aInstance,maxInt) else
      LogInternal(Level,'Instance=nil',nil,maxInt);
end;

procedure TSynLog.Log(Level: TSynLogInfo; const aName: RawUTF8;
  aTypeInfo: pointer; var aValue; Instance: TObject=nil);
begin
  if (self<>nil) and (Level in fFamily.fLevel) then
    LogInternal(Level,aName,aTypeInfo,aValue,Instance);
end;

{$STACKFRAMES ON}
procedure TSynLog.Log(Level: TSynLogInfo);
var aCaller: PtrUInt;
    LastError: DWORD;
begin
  if Level=sllLastError then
    LastError := GetLastError else
    LastError := 0;
  if (self<>nil) and (Level in fFamily.fLevel) then
  if LogHeaderLock(Level) then
  try
    if LastError<>0 then
      AddErrorMessage(LastError);
    {$ifdef CPU64}
    {$ifdef MSWINDOWS}
    if RtlCaptureStackBackTrace(1,1,@aCaller,nil)=0 then
      aCaller := 0 else
      dec(aCaller,5); // ignore call TSynLog.Enter op codes
    {$else}
    aCaller := 0; // no stack trace yet under Linux64
    {$endif}
    {$else}
    {$ifdef PUREPASCAL}
    aCaller := 0; // e.g. ARM Linux
    {$else}
    asm
      mov eax,[ebp+4]  // retrieve caller EIP from push ebp; mov ebp,esp
      sub eax,5        // ignore call TSynLog.Enter op codes
      mov aCaller,eax
    end;
    {$endif}
    {$endif}
    TSynMapFile.Log(fWriter,aCaller,false);
  finally
    LogTrailerUnLock(Level);
    if LastError<>0 then
      SetLastError(LastError);
  end;
end;
{$STACKFRAMES OFF}

class procedure TSynLog.DebuggerNotify(Level: TSynLogInfo;
  const Format: RawUTF8; const Args: array of const);
var Msg: RawUTF8;
begin
  if Format<>''then begin
    Msg := FormatUTF8(Format,Args);
    Add.LogInternal(Level,Msg,nil,maxInt);
    {$ifdef MSWINDOWS}
    OutputDebugStringA(pointer(CurrentAnsiConvert.UTF8ToAnsi(Msg)));
    {$endif}
    {$ifdef LINUX}
    //write(Msg);
    {$endif}
  end;
  {$ifndef FPC_OR_PUREPASCAL}
  if DebugHook<>0 then
    asm int 3 end; // force manual breakpoint if tests are run from the IDE
  {$endif}
end;

procedure TSynLog.LogFileHeader;
var WithinEvents: boolean;
    {$ifdef MSWINDOWS}
    Env: PAnsiChar;
    P: PUTF8Char;
    L: Integer;
    {$else}
    uts: UtsName;
    {$endif}
procedure NewLine;
begin
  if WithinEvents then begin
    fWriter.AddEndOfLine(sllNewRun);
    LogCurrentTime;
    fWriter.AddShort(LOG_LEVEL_TEXT[sllNewRun]);
  end else
    fWriter.Add(#13);
end;
begin
  if not QueryPerformanceFrequency(fFrequencyTimeStamp) then begin
    fFamily.HighResolutionTimeStamp := false;
    fFrequencyTimeStamp := 0;
  end else
    if (fFileRotationSize>0) or (fFileRotationNextHour<>0) then
      fFamily.HighResolutionTimeStamp := false;
  if InstanceMapFile=nil then
    GarbageCollectorFreeAndNil(InstanceMapFile,TSynMapFile.Create);
  WithinEvents := fWriter.WrittenBytes>0;
  // array of const is buggy under Delphi 5 :( -> use fWriter.Add*() below
  if WithinEvents then begin
    LogCurrentTime;
    fWriter.AddShort(LOG_LEVEL_TEXT[sllNewRun]);
    fWriter.AddChars('=',50);
    NewLine;
  end;
  with ExeVersion, fWriter do begin
    AddString(ProgramFullSpec);
    NewLine;
    AddShort('Host=');  AddString(Host);
    AddShort(' User='); AddString(User);
    {$ifdef MSWINDOWS}
    with SystemInfo, OSVersionInfo do begin
      AddShort(' CPU=');  Add(dwNumberOfProcessors); Add('*');
      Add(wProcessorArchitecture); Add('-'); Add(wProcessorLevel); Add('-');
      Add(wProcessorRevision);
      AddShort(' OS='); Add(ord(OSVersion)); Add('.'); Add(wServicePackMajor);
      Add('='); Add(dwMajorVersion); Add('.'); Add(dwMinorVersion); Add('.');
      Add(dwBuildNumber);
      AddShort(' Wow64='); Add(integer(IsWow64));
    end;
    {$else}
    {$ifdef KYLIX3}
    AddShort(' CPU=');
    Add(LibC.get_nprocs); Add('/'); Add(LibC.get_nprocs_conf);
    AddShort(' OS=');
    uname(uts);
    {$else}
    AddShort(' CPU=unknown OS=');
    FPUname(uts);
    {$endif}
    AddNoJSONEscape(@uts.sysname); Add('-'); AddNoJSONEscape(@uts.release);
    AddReplace(@uts.version,' ','-');
    AddShort(' Wow64=0');
    {$endif}
    AddShort(' Freq='); Add(fFrequencyTimeStamp);
    if IsLibrary then begin
      AddShort(' Instance=');
      AddNoJSONEscapeString(InstanceFileName);
    end;
    {$ifdef MSWINDOWS}
    NewLine;
    AddShort('Environment variables=');
    Env := GetEnvironmentStringsA;
    P := pointer(Env);
    while P^<>#0 do begin
      L := StrLen(P);
      if (L>0) and (P^<>'=') then begin
        AddNoJSONEscape(P,L);
        Add(#9);
      end;
      inc(P,L+1);
    end;
    FreeEnvironmentStringsA(Env);
    CancelLastChar; // trim last #9
    {$endif}
    NewLine;
    AddClassName(self.ClassType);
    AddShort(' '+SYNOPSE_FRAMEWORK_FULLVERSION+' ');
    AddDateTime(Now);
    if WithinEvents then
      AddEndOfLine(sllNone) else
      Add(#13,#13);
    FlushToStream;
    EchoReset; // header is not to be sent to console
  end;
  QueryPerformanceCounter(fStartTimeStamp);
  Include(fInternalFlags,logHeaderWritten);
end;

{$ifndef DELPHI5OROLDER}
{$WARN SYMBOL_DEPRECATED OFF} // for GetHeapStatus
procedure TSynLog.AddMemoryStats;
begin
  {$ifdef MSWINDOWS}
  with GetHeapStatus do
    if TotalAddrSpace<>0 then
    fWriter.Add(' AddrSpace=% Uncommitted=% Committed=% Allocated=% Free=% '+
       'FreeSmall=% FreeBig=% Unused=% Overheap=% ',
      [TotalAddrSpace,TotalUncommitted,TotalCommitted,TotalAllocated,TotalFree,
       FreeSmall,FreeBig,Unused,Overhead]);
  {$endif}
end;
{$WARN SYMBOL_DEPRECATED ON}
{$endif}

procedure TSynLog.AddErrorMessage(Error: Cardinal);
{$ifdef MSWINDOWS}
var Len: Integer;
    Buffer: array[byte] of WideChar;
begin
  Len := FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ARGUMENT_ARRAY,
    nil, Error, 0, Buffer, SizeOf(Buffer), nil);
  while (Len>0) and (ord(Buffer[Len-1]) in [0..32,ord('.')]) do dec(Len);
  Buffer[Len] := #0;
  fWriter.Add(' ','"');
  fWriter.AddOnSameLineW(@Buffer,Len);
  fWriter.AddShort('" (');
{$else}
begin
  fWriter.AddShort('Error ');
{$endif}
  fWriter.Add(Error);
  fWriter.Add(')',' ');
end;

procedure TSynLog.LogCurrentTime;
begin
  if fFamily.HighResolutionTimeStamp and (fFrequencyTimeStamp<>0) then begin
    QueryPerformanceCounter(fCurrentTimeStamp);
    dec(fCurrentTimeStamp,fStartTimeStamp);
    fWriter.AddBinToHexDisplay(@fCurrentTimeStamp,sizeof(fCurrentTimeStamp));
  end else
    fWriter.AddCurrentLogTime;
end;

function TSynLog.LogHeaderLock(Level: TSynLogInfo): boolean;
var i: integer;
begin
  LockAndGetThreadContext;
  try
    if fWriter=nil then
      CreateLogWriter; // file creation should be thread-safe
    if not (logHeaderWritten in fInternalFlags) then
      LogFileHeader;
    if (not (sllEnter in fFamily.Level)) and (Level in fFamily.fLevelStackTrace) then
       for i := 0 to fThreadContext^.RecursionCount-1 do begin
         fWriter.AddChars(' ',i+24-byte(fFamily.HighResolutionTimeStamp));
         AddRecursion(i,sllNone);
       end;
    LogCurrentTime;
    if fFamily.fPerThreadLog=ptIdentifiedInOnFile then
      fWriter.AddInt18ToChars3(fThreadIndex);
    fCurrentLevel := Level;
    fWriter.AddShort(LOG_LEVEL_TEXT[Level]);
    fWriter.AddChars(#9,fThreadContext^.RecursionCount-byte(Level in [sllEnter,sllLeave]));
  {$ifndef DELPHI5OROLDER}
    case Level of // handle additional text for some special error levels
      sllMemory: AddMemoryStats;
    end;
  {$endif}
    result := true;
  except
    on Exception do
     result := false ;
  end;
end;

procedure TSynLog.PerformRotation;
var currentMaxSynLZ: cardinal;
    i: integer;
    FN: array of TFileName;
begin
  fWriter.FlushFinal;
  FreeAndNil(fWriter);
  FreeAndNil(fWriterStream);
  currentMaxSynLZ := 0;
  if not (assigned(fFamily.fOnRotate) and
          fFamily.fOnRotate(self,fFileName)) then begin
    if fFamily.fRotateFileCount>1 then begin
      SetLength(FN,fFamily.fRotateFileCount-1);
      for i := fFamily.fRotateFileCount-1 downto 1 do begin
        FN[i-1] := ChangeFileExt(fFileName,'.'+IntToStr(i)+'.synlz');
        if (currentMaxSynLZ=0) and FileExists(FN[i-1]) then
          currentMaxSynLZ := i;
      end;
      if currentMaxSynLZ=fFamily.fRotateFileCount-1 then
        DeleteFile(FN[currentMaxSynLZ-1]); // delete e.g. '9.synlz'
      for i := fFamily.fRotateFileCount-2 downto 1 do
        RenameFile(FN[i-1],FN[i]); // e.g. '8.synlz' -> '9.synlz'
      FileSynLZ(fFileName,FN[0],LOG_MAGIC); // main -> '1.synlz'
    end;
    DeleteFile(fFileName);
  end;
  CreateLogWriter;
  LogFileHeader;
end;

procedure TSynLog.LogTrailerUnLock(Level: TSynLogInfo);
begin
  try
    if Level in fFamily.fLevelStackTrace then
      AddStackTrace(nil);
    fWriter.AddEndOfLine(fCurrentLevel);
    if (fFileRotationNextHour<>0) and (GetTickCount64>=fFileRotationNextHour) then begin
      inc(fFileRotationNextHour,MSecsPerDay);
      PerformRotation;
    end else
    if (fFileRotationSize>0) and (fWriter.WrittenBytes>fFileRotationSize) then
      PerformRotation;
  finally
    LeaveCriticalSection(fThreadLock);
  end;
end;

procedure TSynLog.LogInternal(Level: TSynLogInfo; const TextFmt: RawUTF8;
  const TextArgs: array of const; Instance: TObject);
var LastError: cardinal;
begin
  if Level=sllLastError then
    LastError := GetLastError else
    LastError := 0;
  if LogHeaderLock(Level) then
  try
    if Instance<>nil then
      fWriter.AddInstancePointer(Instance,' ');
    fWriter.Add(TextFmt,TextArgs,twOnSameLine);
    if LastError<>0 then
      AddErrorMessage(LastError);
  finally
    LogTrailerUnLock(Level);
    if LastError<>0 then
      SetLastError(LastError);
  end;
end;

procedure TSynLog.LogInternal(Level: TSynLogInfo; const Text: RawUTF8;
  Instance: TObject; TextTruncateAtLength: integer);
var LastError: cardinal;
begin
  if Level=sllLastError then
    LastError := GetLastError else
    LastError := 0;
  if LogHeaderLock(Level) then
  try
    if Text='' then begin
      if Instance<>nil then
        if Instance.InheritsFrom(Exception) then begin
          fWriter.AddInstanceName(Instance,':');
          if Instance.InheritsFrom(ESynException) then
            fWriter.WriteObject(Instance,[woFullExpand]) else begin
            fWriter.Add('"');
            fWriter.AddJSONEscapeString(Exception(Instance).Message);
            fWriter.Add('"');
          end;
        end else
          fWriter.WriteObject(Instance,[woFullExpand]);
    end else begin
      if Instance<>nil then
        fWriter.AddInstancePointer(Instance,' ');
      if length(Text)>TextTruncateAtLength then begin
        fWriter.AddOnSameLine(pointer(Text),TextTruncateAtLength);
        fWriter.AddShort('... (truncated) length=');
        fWriter.AddU(length(Text));
      end else
        fWriter.AddOnSameLine(pointer(Text));
    end;
    if LastError<>0 then
      AddErrorMessage(LastError);
  finally
    LogTrailerUnLock(Level);
    if LastError<>0 then
      SetLastError(LastError);
  end;
end;

procedure TSynLog.LogInternal(Level: TSynLogInfo; const aName: RawUTF8;
   aTypeInfo: pointer; var aValue; Instance: TObject=nil);
begin
  if LogHeaderLock(Level) then
  try
    if Instance<>nil then
      fWriter.AddInstancePointer(Instance,' ');
    fWriter.AddFieldName(aName);
    fWriter.AddTypedJSON(aTypeInfo,aValue);
  finally
    LogTrailerUnLock(Level);
  end;
end;

procedure TSynLog.ComputeFileName;
var timeNow,hourRotate,timeBeforeRotate: TDateTime;
begin
  fFileName := fFamily.fCustomFileName;
  if fFileName='' then begin
    fFileName := UTF8ToString(ExeVersion.ProgramName);
    if fFamily.IncludeComputerNameInFileName then
      fFileName := fFileName+' ('+UTF8ToString(ExeVersion.Host)+')';
  end;
  fFileRotationSize := 0;
  if fFamily.fRotateFileCount>0 then begin
    if fFamily.fRotateFileSize>0 then
      fFileRotationSize := fFamily.fRotateFileSize shl 10; // size KB -> B
    if fFamily.fRotateFileAtHour in [0..23] then begin
      hourRotate := EncodeTime(fFamily.fRotateFileAtHour,0,0,0);
      timeNow := Time;
      if hourRotate<timeNow then
        hourRotate := hourRotate+1; // trigger will be tomorrow
      timeBeforeRotate := hourRotate-timeNow;
      fFileRotationNextHour := GetTickCount64+trunc(timeBeforeRotate*MSecsPerDay);
    end;
  end;
  if (fFileRotationSize=0) and (fFileRotationNextHour=0) then
    fFileName := fFileName+' '+Ansi7ToString(NowToString(false));
  {$ifdef MSWINDOWS}
  if IsLibrary and (fFamily.fCustomFileName='') then
    fFileName := fFileName+' '+ExtractFileName(GetModuleName(HInstance));
  {$endif}
  if fFamily.fPerThreadLog=ptOneFilePerThread then
    fFileName := fFileName+' '+IntToString(Int64(GetCurrentThreadId));
  fFileName := fFamily.fDestinationPath+fFileName+fFamily.fDefaultExtension;
end;

procedure TSynLog.CreateLogWriter;
var i,retry: integer;
    exists: boolean;
    {$ifndef NOEXCEPTIONINTERCEPT}
    prevState: TSynLog;
    {$endif}
begin
  if fWriterStream=nil then begin
    ComputeFileName;
    if fFamily.NoFile then
      fWriterStream := TFakeWriterStream.Create else begin
      {$ifndef NOEXCEPTIONINTERCEPT}
      prevState := CurrentHandleExceptionSynLog; // ignore file exceptions 
      CurrentHandleExceptionSynLog := nil;
      try
      {$endif}
      if FileExists(fFileName) then
        case fFamily.FileExistsAction of
        acOverwrite:
          DeleteFile(fFileName);
        acAppend:
          Include(fInternalFlags,logHeaderWritten);
        end;
      for retry := 0 to 2 do begin
        for i := 1 to 10 do
        try
          exists := FileExists(fFileName);
          if exists and (fFamily.FileExistsAction<>acOverwrite) then begin
            if fFamily.FileExistsAction=acAppend then
              Include(fInternalFlags,logHeaderWritten);
          end else
          if (fFileRotationSize=0) or not exists then
            TFileStream.Create(fFileName,fmCreate).Free;   // create a void file
          fWriterStream := TFileStream.Create(fFileName,
            fmOpenReadWrite or fmShareDenyWrite); // open with read sharing
          break;
        except
          on Exception do
            SleepHiRes(100);
        end;
        if fWriterStream<>nil then
          break;
        fFileName := ChangeFileExt(fFileName,'-'+fFamily.fDefaultExtension);
      end;
      {$ifndef NOEXCEPTIONINTERCEPT}
      finally
        CurrentHandleExceptionSynLog := prevState;
      end;
      {$endif}
    end;
    if fWriterStream=nil then // go on if file creation fails (e.g. RO folder)
      fWriterStream := TFakeWriterStream.Create;
    if (fFileRotationSize>0) or (fFamily.FileExistsAction<>acOverwrite) then
      fWriterStream.Seek(0,soFromEnd); // in rotation mode, append at the end
  end;
  if fWriterClass=nil then
    fWriterClass := TTextWriter;
  if fWriter=nil then
    fWriter := fWriterClass.Create(fWriterStream,fFamily.BufferSize);
  fWriter.EndOfLineCRLF := fFamily.EndOfLineCRLF;
  if integer(fFamily.EchoToConsole)<>0 then
    fWriter.EchoAdd(ConsoleEcho);
  if Assigned(fFamily.EchoCustom) then
    fWriter.EchoAdd(fFamily.EchoCustom);
  if Assigned(fFamily.fEchoRemoteClient) then
    fWriter.EchoAdd(fFamily.fEchoRemoteEvent);
end;

procedure TSynLog.AddRecursion(aIndex: integer; aLevel: TSynLogInfo);
type
  {$ifdef FPC}{$PACKRECORDS 1}{$endif}
  TTypeInfo = {$ifndef FPC}packed{$endif} record
    Kind: byte;
    Name: ShortString;
  end;
  {$ifdef FPC}{$PACKRECORDS C}{$endif}
  TClassType =
    {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
    packed
    {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
    record
     ClassType: TClass;
     ParentInfo: pointer;
     PropCount: SmallInt;
     UnitName: ShortString;
  end;
  PTypeInfo = ^TTypeInfo;
  PClassType = ^TClassType;
var Info: PTypeInfo;
    MS: cardinal;
label DoEnt;
begin
  with fThreadContext^ do
  if cardinal(aIndex)<cardinal(RecursionCount) then
  with Recursion[aIndex] do begin
    if aLevel<>sllLeave then begin
      if ClassType<>nil then begin
        if fFamily.WithUnitName then begin
          Info := PPointer(PtrInt(ClassType)+vmtTypeInfo)^;
          if Info<>nil then begin
            {$ifdef FPC}
            fWriter.AddShort(PClassType(GetFPCTypeData(pointer(Info)))^.UnitName);
            {$else}
            fWriter.AddShort(PClassType(@Info^.Name[ord(Info^.Name[0])+1])^.UnitName);
            {$endif}
            fWriter.Add('.');
          end;
        end;
        fWriter.AddShort(PShortString(PPointer(PtrInt(ClassType)+vmtClassName)^)^);
        if Instance<>nil then begin
          fWriter.Add('(');
          fWriter.AddPointer(PtrUInt(Instance));
          fWriter.Add(')');
        end;
        fWriter.Add('.');
      end;
      if MethodName<>nil then begin
        if MethodNameLocal<>mnLeave then begin
          fWriter.AddNoJSONEscape(MethodName);
          if MethodNameLocal=mnEnter then
            MethodNameLocal := mnLeave;
        end;
      end else
        TSynMapFile.Log(fWriter,Caller,false);
    end;
    if fFamily.HighResolutionTimeStamp and (fFrequencyTimeStamp<>0) then
DoEnt:case aLevel of
      sllEnter:
        EnterTimeStamp := fCurrentTimeStamp;
      sllLeave: begin
        if fFrequencyTimeStamp=0 then
          MS := 0 else // avoid div per 0 exception
          MS := ((fCurrentTimeStamp-EnterTimeStamp)*(1000*1000))div fFrequencyTimeStamp;
        fWriter.AddMicroSec(MS);
      end;
      end else
    if aLevel in [sllEnter,sllLeave] then begin
      QueryPerformanceCounter(fCurrentTimeStamp);
      dec(fCurrentTimeStamp,fStartTimeStamp);
      goto DoEnt;
    end;
  end;
  fWriter.AddEndOfLine(aLevel);
end;

procedure TSynLog.DoEnterLeave(aLevel: TSynLogInfo);
begin
  if LogHeaderLock(aLevel) then
  try
    AddRecursion(fThreadContext^.RecursionCount-1,aLevel);
  finally
    LeaveCriticalSection(fThreadLock);
  end;
end;

const
  MINIMUM_EXPECTED_STACKTRACE_DEPTH = 2;

procedure TSynLog.AddStackTrace(Stack: PPtrUInt);
{$ifndef FPC}
{$ifndef CPU64}
procedure AddStackManual(Stack: PPtrUInt);
  function check2(xret: PtrUInt): Boolean;
  var i: PtrUInt;
  begin
    result := true;
    for i := 2 to 7 do
      if PWord(xret-i)^ and $38FF=$10FF then
        exit;
    result := false;
  end;
var st, max_stack, min_stack, depth: PtrUInt;
{$ifndef NOEXCEPTIONINTERCEPT}
    prevState: TSynLog;
{$endif}
begin
  depth := fFamily.StackTraceLevel;
  if depth=0 then
    exit;
  asm
    mov min_stack,ebp
  end;
  if Stack=nil then // if no Stack pointer set, retrieve current one
    Stack := pointer(min_stack);
  {$ifdef WITH_MAPPED_EXCEPTIONS}
  max_stack := CurrentTopOfStack;
  if max_stack=0 then begin
    ComputeCurrentTopOfStack;
    max_stack := CurrentTopOfStack;         
  end;
  {$else}
  asm
    mov eax,fs:[4]
    mov max_stack, eax
    // mov eax,fs:[18h]; mov ecx,dword ptr [eax+4]; mov max_stack,ecx
  end;
  {$endif}
  fWriter.AddShort(' stack trace ');
  {$ifndef NOEXCEPTIONINTERCEPT} // for IsBadCodePtr() or any internal exception
  prevState := CurrentHandleExceptionSynLog;
  CurrentHandleExceptionSynLog := nil;
  try
  {$endif}
    if PtrUInt(stack)>=min_stack then
    try
      while (PtrUInt(stack)<max_stack) do begin
        st := stack^;
        if ((st>max_stack) or (st<min_stack)) and
           not IsBadReadPtr(pointer(st-8),12) and
           ((pByte(st-5)^=$E8) or check2(st)) then begin
          TSynMapFile.Log(fWriter,st,false); // ignore any TSynLog.* methods
          dec(depth);
          if depth=0 then break;
        end;
        inc(stack);
      end;
    except
      // just ignore any access violation here
    end;
  {$ifndef NOEXCEPTIONINTERCEPT}
  finally
    CurrentHandleExceptionSynLog := prevState;
  end;
  {$endif}
end;
{$endif}
{$endif}
{$ifdef WITH_MAPPED_EXCEPTIONS}
begin
  AddStackManual(Stack);
end;
{$else}
var n, i: integer;
    BackTrace: array[byte] of PtrUInt;
{$ifndef NOEXCEPTIONINTERCEPT}
    prevState: TSynLog;
{$endif}
begin
  if fFamily.StackTraceLevel<=0 then
    exit;
  {$ifdef MSWINDOWS}
  if (fFamily.StackTraceUse=stOnlyManual) or
     (RtlCaptureStackBackTraceRetrieved<>btOK) then begin
    {$ifndef FPC}
    {$ifndef CPU64}
    AddStackManual(Stack);
    {$endif}
    {$endif}
  end else begin
    {$ifndef NOEXCEPTIONINTERCEPT}
    prevState := CurrentHandleExceptionSynLog;
    CurrentHandleExceptionSynLog := nil; // for IsBadCodePtr
    try
    {$endif}
      try
        n := RtlCaptureStackBackTrace(2,fFamily.StackTraceLevel,@BackTrace,nil);
        if (n<MINIMUM_EXPECTED_STACKTRACE_DEPTH) and
           (fFamily.StackTraceUse<>stOnlyAPI) then begin
          {$ifndef FPC}
          {$ifndef CPU64}
          AddStackManual(Stack);
          {$endif}
          {$endif}
        end else begin
          fWriter.AddShort(' stack trace API ');
          for i := 0 to n-1 do
            TSynMapFile.Log(fWriter,BackTrace[i],false); // ignore any TSynLog.* 
        end;
      except
        // just ignore any access violation here
      end;
    {$ifndef NOEXCEPTIONINTERCEPT}
    finally
      CurrentHandleExceptionSynLog := prevState;
    end;
    {$endif}
  end;
  {$endif MSWINDOWS}
end;
{$endif WITH_MAPPED_EXCEPTIONS}


{ TSynLogFile }

constructor TSynLogFile.Create;
var L: TSynLogInfo;
begin
  for L := low(TSynLogInfo) to high(TSynLogInfo) do
    fLogLevelsTextMap[L] := PCardinal(@LOG_LEVEL_TEXT[L][3])^; // [3] -> e.g. 'UST4'
end;

function TSynLogFile.GetLogLevelFromText(LineBeg: PUTF8Char): TSynLogInfo;
var P: PtrInt;
begin
  P := PtrInt(IntegerScan(@fLogLevelsTextMap[succ(sllNone)],
    ord(high(TSynLogInfo)),PCardinal(LineBeg+fLineLevelOffset)^));
  if P<>0 then
    result := TSynLogInfo((P-PtrInt(@fLogLevelsTextMap[succ(sllNone)]))shr 2+1) else
    result := sllNone;
end;

function TSynLogFile.EventCount(const aSet: TSynLogInfos): integer;
var i: integer;
begin
  result := 0;
  for i := 0 to Count-1 do
    if fLevels[i] in aSet then
      inc(result);
end;

function TSynLogFile.LineContains(const aUpperSearch: RawUTF8; aIndex: Integer): Boolean;
begin
  if (self=nil) or (cardinal(aIndex)>=cardinal(fCount)) or (aUpperSearch='') then
    result := false else
    result := GetLineContains(PUTF8Char(fLines[aIndex])+fLineTextOffset,
      fMapEnd,pointer(aUpperSearch));
end;

function TSynLogFile.EventDateTime(aIndex: integer): TDateTime;
var TimeStamp: Int64;
begin
  if cardinal(aIndex)>=cardinal(fCount) then
    result := 0 else
    if fFreq=0 then
      Iso8601ToDateTimePUTF8CharVar(fLines[aIndex],17,result) else
      if HexDisplayToBin(fLines[aIndex],@TimeStamp,sizeof(TimeStamp)) then
        result := fStartDateTime+(TimeStamp/fFreqPerDay) else
        result := 0;
end;

procedure TSynLogFile.LoadFromMap(AverageLineLength: integer=32);
  var PBeg, P, PEnd: PUTF8Char;
  function StrPosI(P,PEnd: PUTF8Char; SearchUp: PAnsiChar): PUTF8Char;
  begin
    result := P;
    while result<PEnd do
      if IdemPChar(result,SearchUp) then
        exit else
        inc(result);
    result := nil;
  end;
  function GetOne(const UP: RawUTF8; var S: RawUTF8): boolean;
  var LUP: integer;
  begin
    LUP := length(UP);
    P := StrPosI(PBeg,PEnd-LUP,pointer(UP));
    if P=nil then
      result := false else begin
      SetString(S,PAnsiChar(PBeg),P-PBeg);
      PBeg := P+LUP;
      result := pointer(S)<>nil;
    end;
  end;
  function ComputeProperTime(var procndx: Integer): cardinal; // returns leave
  var start, i: integer;
  begin
    start := procndx;
    with fLogProcNatural[procndx] do begin
      ProperTime := Time;
      result := Index;
    end;
    repeat
      inc(result);
      if result>=Cardinal(Count) then
        break;
      case fLevels[result] of
      sllEnter: begin
        inc(procndx);
        assert(fLogProcNatural[procndx].Index=result);
        result := ComputeProperTime(procndx);
      end;
      sllLeave: begin
        with fLogProcNatural[start] do
        for i := start+1 to procndx do
          dec(ProperTime,fLogProcNatural[i].ProperTime);
        break;
      end;
      end;
    until false;
  end;
  procedure CleanLevels(Log: TSynLogFile);
  var i, aCount, pCount: integer;
  begin
    aCount := 0;
    pCount := 0;
    with Log do
    for i := 0 to fCount-1 do
      if fLevels[i]<>sllNone then begin
        fLevels[aCount] := fLevels[i];
        fLines[aCount] := fLines[i];
        if fThreads<>nil then
          fThreads[aCount] := fThreads[i];
        if fLevels[i]=sllEnter then begin
          fLogProcNatural[pCount].Index := aCount;
          inc(pCount);
        end;
        inc(aCount);
      end;
    Log.fCount := aCount;
    assert(pCount=Log.fLogProcNaturalCount);
  end;
var aWow64: RawUTF8;
    i, j, Level: integer;
    TSEnter, TSLeave: Int64;
    OK: boolean;
begin
  // 1. calculate fLines[] + fCount and fLevels[] + fLogProcNatural[] from .log content
  fLineHeaderCountToIgnore := 3;
  inherited LoadFromMap(100);
  // 2. fast retrieval of header
  OK := false;
  try
{  C:\Dev\lib\SQLite3\exe\TestSQL3.exe 0.0.0.0 (2011-04-07 11:09:06)
   Host=BW013299 User=G018869 CPU=1*0-15-1027 OS=2.3=5.1.2600 Wow64=0 Freq=3579545
   TSynLog 1.13 LVCL 2011-04-07 12:04:09 }
    if (fCount<=fLineHeaderCountToIgnore) or LineSizeSmallerThan(0,24) or
       not IdemPChar(fLines[1],'HOST=') or LineSizeSmallerThan(2,24) or
       (fLevels=nil) or (fLineLevelOffset=0) then
      exit;
    PBeg := fLines[0];
    PEnd := PBeg+LineSize(0)-12;
    if PEnd<PBeg then
      exit;
    if PEnd^='(' then begin  // '(2011-04-07)' format
      if (PEnd[-1]<>' ') or (PEnd[0]<>'(') or (PEnd[11]<>')') then
        exit;
      Iso8601ToDateTimePUTF8CharVar(PEnd+1,10,fExeDate);
    end else begin  // '(2011-04-07 11:09:06)' format
      dec(PEnd,9);
      if (PEnd<PBeg) or (PEnd[-1]<>' ') or (PEnd[0]<>'(') or (PEnd[20]<>')') then
        exit;
      Iso8601ToDateTimePUTF8CharVar(PEnd+1,19,fExeDate);
    end;
    dec(PEnd);
    P := PEnd;
    repeat if P<=PBeg then exit else dec(P) until P^=' ';
    SetString(fExeVersion,PAnsiChar(P+1),PEnd-P-1);
    repeat dec(P); if P<=PBeg then exit; until P^<>' ';
    SetString(fExeName,PAnsiChar(PBeg),P-PBeg+1);
    PBeg := PUTF8Char(fLines[1])+5;
    PEnd := PUTF8Char(fLines[1])+LineSize(1);
    if not GetOne(' USER=',fHost) or not GetOne(' CPU=',fUser) or
       not GetOne(' OS=',fCPU)    or not GetOne(' WOW64=',fOsDetailed) or
       not GetOne(' FREQ=',aWow64) then
      exit;
    {$ifdef MSWINDOWS}
    fWow64 := aWow64='1';
    {$endif}
    SetInt64(PBeg,fFreq);
    if fFreq=0 then
      exit;
    while (PBeg<PEnd) and (PBeg^>' ') do inc(PBeg);
    if IdemPChar(PBeg,' INSTANCE=') then // only available for a library log
      SetString(fInstanceName,PBeg+10,PEnd-PBeg-10);
    fHeaderLinesCount := 4;
    while fHeaderLinesCount<fCount do begin
      if PAnsiChar(fLines[fHeaderLinesCount-1])^<' ' then
        break; // end of header = void line
      inc(fHeaderLinesCount);
    end;
    if (LineSize(fHeaderLinesCount-1)<>0) or
       LineSizeSmallerThan(fHeaderLinesCount,16) then
      exit;
    if fHeaderLinesCount<>4 then
      SetString(fHeaders,PAnsiChar(fLines[2]),
        PtrInt(fLines[fHeaderLinesCount-2])-PtrInt(fLines[2]));
    if PWord(fLines[fHeaderLinesCount])^<>ord('0')+ord('0')shl 8 then // YYYYMMDD -> 20101225 e.g.
      fFreq := 0 else // =0 if date time, >0 if high-resolution time stamp
      fFreqPerDay := fFreq*SecsPerDay;
    Iso8601ToDateTimePUTF8CharVar(PUTF8Char(
      fLines[fHeaderLinesCount-2])+LineSize(fHeaderLinesCount-2)-19,19,fStartDateTime);
    if fStartDateTime=0 then
      exit;
    P := pointer(fOSDetailed);
    {$ifdef MSWINDOWS} // use fOSDetailed under Linux
    fOS := TWindowsVersion(GetNextItemCardinal(P,'.'));
    fOSServicePack := GetNextItemCardinal(P);
    {$endif}
    // 3. compute fCount and fLines[] so that all fLevels[]<>sllNone
    CleanLevels(self);
    if Length(fLevels)-fCount>16384 then begin // size down only if worth it
      SetLength(fLevels,fCount);
      if fThreads<>nil then
        SetLength(fThreads,fCount);
    end;
    // 4. compute customer-side profiling
    SetLength(fLogProcNatural,fLogProcNaturalCount);
    for i := 0 to fLogProcNaturalCount-1 do
      if fLogProcNatural[i].Time>=99000000 then begin // overange 99.000.000 -> compute
        Level := 0;
        j := fLogProcNatural[i].Index;
        repeat
          inc(j);
          if j=fCount then break;
          case fLevels[j] of
          sllEnter: inc(Level);
          sllLeave: if Level=0 then begin
            if fFreq=0 then // adjust huge seconds timing from date/time column
              fLogProcNatural[i].Time :=
                Round((EventDateTime(j)-EventDateTime(fLogProcNatural[i].Index))*86400000000.0)+
                fLogProcNatural[i].Time mod 1000000 else begin
              HexDisplayToBin(fLines[fLogProcNatural[i].Index],@TSEnter,sizeof(TSEnter));
              HexDisplayToBin(fLines[j],@TSLeave,sizeof(TSLeave));
              fLogProcNatural[i].Time := ((TSLeave-TSEnter)*(1000*1000)) div fFreq;
            end;
            break;
          end else dec(Level);
          end;
        until false;
      end;
    i := 0;
    while i<fLogProcNaturalCount do begin
      ComputeProperTime(i);
      inc(i);
    end;
    LogProcMerged := false; // set LogProp[]
    OK := true;
  finally
    if not OK then begin
      Finalize(fLevels); // mark not a valid .log
      Finalize(fThreads);
      fLineLevelOffset := 0;
    end;
  end;
end;

procedure TSynLogFile.LogProcSort(Order: TLogProcSortOrder);
begin
  if (fLogProcNaturalCount<=1) or (Order=fLogProcSortInternalOrder) then
    Exit;
  fLogProcSortInternalOrder := Order;
  LogProcSortInternal(0,LogProcCount-1);
end;

function StrICompLeftTrim(Str1, Str2: PUTF8Char): PtrInt;
var C1, C2: integer;
begin
  while Str1^ in [#9,' '] do inc(Str1);
  while Str2^ in [#9,' '] do inc(Str2);
  repeat
    C1 := NormToUpperByte[ord(Str1^)];
    C2 := NormToUpperByte[ord(Str2^)];
    if (C1<>C2) or (C1<32) then
      break;
    Inc(Str1);
    Inc(Str2);
  until false;
  Result := C1-C2;
end;

function TSynLogFile.LogProcSortComp(A, B: Integer): integer;
begin
  case fLogProcSortInternalOrder of
    soByName: result :=
      StrICompLeftTrim(PUTF8Char(fLines[LogProc[A].Index])+fLineTextOffset,
                       PUTF8Char(fLines[LogProc[B].Index])+fLineTextOffset);
    soByOccurrence: result := LogProc[A].Index-LogProc[B].Index;
    soByTime:       result := LogProc[B].Time-LogProc[A].Time;
    soByProperTime: result := LogProc[B].ProperTime-LogProc[A].ProperTime;
    else  result := A-B;
  end;
end;

procedure TSynLogFile.LogProcSortInternal(L, R: integer);
  procedure Exchg(var P1,P2: TSynLogFileProc);
  var c: TSynLogFileProc;
  begin
    c := P1;
    P1 := P2;
    P2 := c;
  end;
var I,J,P: integer;
begin
  if L<R then
  repeat
    I := L; J := R;
    P := (L + R) shr 1;
    repeat
      while LogProcSortComp(I,P)<0 do inc(I);
      while LogProcSortComp(J,P)>0 do dec(J);
      if I<=J then begin
        Exchg(LogProc[i],LogProc[j]);
        if P = I then P := J else if P = J then P := I;
        Inc(I); Dec(J);
      end;
    until I>J;
    if L<J then
      LogProcSortInternal(L,J);
     L := I;
  until I>=R;
end;

procedure TSynLogFile.ProcessOneLine(LineBeg, LineEnd: PUTF8Char);
  function DecodeMicroSec(P: PByte): integer;
  var B: integer;
  begin // fast decode 00.020.006 at the end of the line
    B := ConvertHexToBin[P^];   // 00
    if B>9 then
      result := -1 else begin
      result := B;
      inc(P);
      B := ConvertHexToBin[P^];
      if B>9 then
        result := -1 else begin
        result := result*10+B;
        inc(P,2);                 // .
        B := ConvertHexToBin[P^]; // 020
        if B>9 then
          result := -1 else begin
          result := result*10+B;
          inc(P);
          B := ConvertHexToBin[P^];
          if B>9 then
            result := -1 else begin
            result := result*10+B;
            inc(P);
            B := ConvertHexToBin[P^];
            if B>9 then
              result := -1 else begin
              result := result*10+B;
              inc(P,2);                 // .
              B := ConvertHexToBin[P^]; // 006
              if B>9 then
                result := -1 else begin
                result := result*10+B;
                inc(P);
                B := ConvertHexToBin[P^];
                if B>9 then
                  result := -1 else begin
                  result := result*10+B;
                  inc(P);
                  B := ConvertHexToBin[P^];
                  if B>9 then
                    result := -1 else
                    result := result*10+B;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
var V: cardinal;
    MS: integer;
    L: TSynLogInfo;
begin
  inherited ProcessOneLine(LineBeg,LineEnd);
  if length(fLevels)<fLinesMax then
    SetLength(fLevels,fLinesMax);
  if (fCount<=fLineHeaderCountToIgnore) or (LineEnd-LineBeg<24) then
    exit;
  if fLineLevelOffset=0 then begin
    if (fCount>50) or not (LineBeg[0] in ['0'..'9']) then
      exit; // definitively does not sound like a .log content
    if LineBeg[8]=' ' then // YYYYMMDD HHMMSS is one char bigger than TimeStamp
      fLineLevelOffset := 19 else
      fLineLevelOffset := 18;
    if (LineBeg[fLineLevelOffset]='!') or // ! = thread 1 
       (GetLogLevelFromText(LineBeg)=sllNone) then begin
      inc(fLineLevelOffset,3);
      fThreadsCount := fLinesMax;
      SetLength(fThreads,fLinesMax);
    end;
    fLineTextOffset := fLineLevelOffset+4;
  end;
  L := GetLogLevelFromText(LineBeg);
  if L=sllNone then
    exit;
  fLevels[fCount-1] := L; // need exact match of level text
  include(fLevelUsed,L);
  case L of
  sllEnter: begin
    if Cardinal(fLogProcStackCount)>=Cardinal(length(fLogProcStack)) then
      SetLength(fLogProcStack,length(fLogProcStack)+256);
    fLogProcStack[fLogProcStackCount] := fLogProcNaturalCount;
    inc(fLogProcStackCount);
    if Cardinal(fLogProcNaturalCount)>=Cardinal(length(fLogProcNatural)) then
      SetLength(fLogProcNatural,length(fLogProcNatural)+32768);
    // fLogProcNatural[].Index will be set in TSynLogFile.LoadFromMap
    inc(fLogProcNaturalCount);
  end;
  sllLeave:
  if (LineEnd-LineBeg>10) and (LineEnd[-4]='.') and (LineEnd[-8]='.') and
     (fLogProcStackCount>0) then begin // 00.020.006
    MS := DecodeMicroSec(PByte(LineEnd-10));
    if MS>=0 then begin
      dec(fLogProcStackCount);
      fLogProcNatural[fLogProcStack[fLogProcStackCount]].Time := MS;
    end;
  end;
  end;
  if fThreads<>nil then begin
    if fThreadsCount<fLinesMax then begin
      fThreadsCount := fLinesMax;
      SetLength(fThreads,fLinesMax);
    end;
    V := Chars3ToInt18(LineBeg+fLineLevelOffset-5);
    fThreads[fCount-1] := V;
    if V>fThreadMax then begin
      fThreadMax := V;
      if V>=fThreadsRowsCount then begin
        fThreadsRowsCount := V+256;
        SetLength(fThreadsRows,fThreadsRowsCount);
      end;
    end;
    inc(fThreadsRows[V]);
  end;
end;

function TSynLogFile.GetEventText(index: integer): RawUTF8;
var L: cardinal;
begin
  if (self=nil) or (cardinal(index)>=cardinal(fCount)) then
    result := '' else begin
    L := GetLineSize(fLines[index],fMapEnd);
    if L<=fLineTextOffset then
      result := '' else
      SetString(result,PAnsiChar(fLines[index])+fLineTextOffset,L-fLineTextOffset);
  end;
end;

procedure TSynLogFile.SetLogProcMerged(const Value: boolean);
var i: integer;
    P: ^TSynLogFileProc;
    O: TLogProcSortOrder;
begin
  fLogProcIsMerged := Value;
  O := fLogProcSortInternalOrder;
  if Value then begin
    if fLogProcMerged=nil then begin
      fLogProcCurrent := pointer(fLogProcNatural);
      fLogProcCurrentCount := fLogProcNaturalCount;
      LogProcSort(soByName); // sort by name to identify unique
      SetLength(fLogProcMerged,fLogProcNaturalCount);
      fLogProcMergedCount := 0;
      i := 0;
      P := pointer(fLogProcNatural);
      repeat
        with fLogProcMerged[fLogProcMergedCount] do begin
          repeat
            Index := P^.Index;
            inc(Time,P^.Time);
            inc(ProperTime,P^.ProperTime);
            inc(i);
            inc(P);
          until (i>=fLogProcNaturalCount) or
                (StrICompLeftTrim(PUTF8Char(fLines[LogProc[i-1].Index])+22,
                 PUTF8Char(fLines[P^.Index])+22)<>0);
        end;
        inc(fLogProcMergedCount);
      until i>=fLogProcNaturalCount;
      SetLength(fLogProcMerged,fLogProcMergedCount);
    end;
    fLogProcCurrent := pointer(fLogProcMerged);
    fLogProcCurrentCount := fLogProcMergedCount;
  end else begin
    fLogProcCurrent := pointer(fLogProcNatural);
    fLogProcCurrentCount := fLogProcNaturalCount;
  end;
  fLogProcSortInternalOrder := soNone;
  LogProcSort(O); // restore previous sort order
end;


function EventArchiveDelete(const aOldLogFileName, aDestinationPath: TFileName): boolean;
begin
  result := DeleteFile(aOldLogFileName);
end;

function EventArchiveSynLZ(const aOldLogFileName, aDestinationPath: TFileName): boolean;
begin // aDestinationPath = 'ArchivePath\log\YYYYMM\'
  Result := false;
  if (aOldLogFileName<>'') and FileExists(aOldLogFileName) then
  try
    if DirectoryExists(aDestinationPath) or CreateDir(aDestinationPath) then
      if FileSynLZ(aOldLogFileName,
         aDestinationPath+ExtractFileName(aOldLogFileName)+'.synlz',LOG_MAGIC) then
        result := DeleteFile(aOldLogFileName);
  except
    on Exception do
      result := false;
  end;
end;


{ TSynLogCallbacks }

constructor TSynLogCallbacks.Create(aTrackedLog: TSynLogFamily);
begin
  inherited Create;
  Registrations.Init(TypeInfo(TSynLogCallbackDynArray),Registration,@fCount);
  TrackedLog := aTrackedLog;
  aTrackedLog.EchoRemoteStart(self,OnEcho,false);
end;

destructor TSynLogCallbacks.Destroy;
begin
  if TrackedLog<>nil then
    if TrackedLog.fEchoRemoteClient=self then
      TrackedLog.EchoRemoteStop; // unregister OnEcho() event
  inherited Destroy;
end;

function TSynLogCallbacks.OnEcho(Sender: TTextWriter; Level: TSynLogInfo;
  const Text: RawUTF8): boolean;
var i: integer;
begin
  result := false;
  if Count=0 then
    exit;
  Lock;
  try
    for i := Count-1 downto 0 do
      if Level in Registration[i].Levels then
      try
        Registration[i].Callback.Log(Level,Text);
        result := true;
      except
        Registrations.Delete(i); // safer to unsubscribe ASAP
      end;
  finally
    UnLock;
  end;
end;

procedure TSynLogCallbacks.Subscribe(const Levels: TSynLogInfos;
  const Callback: ISynLogCallback);
var Reg: TSynLogCallback;
begin
  Reg.Levels := Levels;
  Reg.Callback := Callback;
  Lock;
  try
    Registrations.Add(Reg);
  finally
    UnLock;
  end;
end;

procedure TSynLogCallbacks.Unsubscribe(const Callback: ISynLogCallback);
var i: integer;
begin
  Lock;
  try
    for i := Count-1 downto 0 do
      if Registration[i].Callback=Callback then
        Registrations.Delete(i);
  finally
    UnLock;
  end;
end;


initialization
  {$ifndef NOEXCEPTIONINTERCEPT}
  DefaultSynLogExceptionToStr := InternalDefaultSynLogExceptionToStr;
  {$endif}
  
end.