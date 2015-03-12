/// Win NT Service managment classes for mORMot
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit mORMotService;

{
    This file is part of Synopse mORMot framework.

    Synopse mORMot framework. Copyright (C) 2015 Arnaud Bouchez
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

  The Original Code is Synopse mORMot framework.

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


      Win NT Service managment classes
      --------------------------------

    Version 1.3
    - TService debug and enhancements
    - can compile without SQLite3Commons dependency for smaller executables

    Version 1.4 - February 8, 2010
    - whole Synopse SQLite3 database framework released under the GNU Lesser
      General Public License version 3, instead of generic "Public Domain"

    Version 1.16
    - code refactoring after Leander007 proposals for better compatibility -
      see http://synopse.info/forum/viewtopic.php?id=584
	  
    Version 1.18
    - renamed SQLite3Service.pas to mORMotService.pas
    - changed ServicesRun to return an indicator of success - see [8666906039]
    - TServiceController.CreateOpenService() use lower rights - see [c3ebb6b5d6]
    - added TServiceSingle class and its global handler (to be used instead of
      TServer which does not work as expected)
    - added logging to the Service registration and command process
    - added TServiceController.CheckParameters() generic method to control
      a service from the command line
    - check the executable file in TServiceController.CreateNewService()
    - use SQLite3Log instead of TSQLLog - see [779d773e966]
    - ensure TServiceController.CreateNewService() won't allow to install
      the service on a network drive - see [f487d3de45]

}

interface

{$I Synopse.inc}

{$ifdef LVCL}
  {$define NOMORMOTKERNEL}
  // define it if you don't need to include the mORMot.pas unit
  // - could save some code size in the final executable
{$endif}
{$ifdef DELPHI5OROLDER}
  {$define NOMORMOTKERNEL}
{$endif}

uses
  Windows, WinSVC, Messages, Classes, SysUtils,
  {$ifndef LVCL}Contnrs,{$endif}
  SynCommons,
  SynLog
  {$ifndef NOMORMOTKERNEL},
  mORMot
  {$endif} // translated caption needed only if with full UI
  ;

const
  CM_SERVICE_CONTROL_CODE = WM_USER+1000;

type
  {{ all possible states of the service }
  TServiceState =
    (ssNotInstalled, ssStopped, ssStarting, ssStopping, ssRunning,
     ssResuming, ssPausing, ssPaused, ssErrorRetrievingState);

  {{ TServiceControler class is intended to create a new service instance or
    to maintain (that is start, stop, pause, resume...) an existing service
   - to provide the service itself, use the TService class }
  TServiceController = class
  protected
    FSCHandle: THandle;
    FHandle: THandle;
    FStatus: TServiceStatus;
    FName: RawUTF8;
  private
    function GetStatus: TServiceStatus;
    function GetState: TServiceState;
  public
  {{ Creates a new service and allows to control it and/or its configuration.
   Expected Parameters (strings are unicode-ready since Delphi 2009):
   - TargetComputer - set it to empty string if local computer is the target.
   - DatabaseName - set it to empty string if the default database is supposed
                ('ServicesActive').
   - Name - name of a service.
   - DisplayName - display name of a service.
   - Path - a path to binary (executable) of the service created.
   - OrderGroup - an order group name (unnecessary)
   - Dependances - string containing a list with names of services, which must
               start before (every name should be separated with #0, entire
               list should be separated with #0#0. Or, an empty string can be
               passed if there are no dependances).
   - Username - login name. For service type SERVICE_WIN32_OWN_PROCESS, the
            account name in the form of "DomainName\Username"; If the account
            belongs to the built-in domain, ".\Username" can be specified;
            Services of type SERVICE_WIN32_SHARE_PROCESS are not allowed to
            specify an account other than LocalSystem. If '' is specified, the
            service will be logged on as the 'LocalSystem' account, in which
            case, the Password parameter must be empty too.
   - Password - a password for login name. If the service type is
            SERVICE_KERNEL_DRIVER or SERVICE_FILE_SYSTEM_DRIVER,
            this parameter is ignored.
   - DesiredAccess - a combination of following flags:
     SERVICE_ALL_ACCESS (default value), SERVICE_CHANGE_CONFIG,
     SERVICE_ENUMERATE_DEPENDENTS, SERVICE_INTERROGATE, SERVICE_PAUSE_CONTINUE,
     SERVICE_QUERY_CONFIG, SERVICE_QUERY_STATUS, SERVICE_START, SERVICE_STOP,
     SERVICE_USER_DEFINED_CONTROL
    - ServiceType - a set of following flags:
      SERVICE_WIN32_OWN_PROCESS (default value, which specifies a Win32 service
      that runs in its own process), SERVICE_WIN32_SHARE_PROCESS,
      SERVICE_KERNEL_DRIVER, SERVICE_FILE_SYSTEM_DRIVER,
      SERVICE_INTERACTIVE_PROCESS (default value, which enables a Win32 service
      process to interact with the desktop)
    - StartType - one of following values:
      SERVICE_BOOT_START, SERVICE_SYSTEM_START,
      SERVICE_AUTO_START (which specifies a device driver or service started by
      the service control manager automatically during system startup),
      SERVICE_DEMAND_START (default value, which specifies a service started by
      a service control manager when a process calls the StartService function,
      that is the TServiceController.Start method), SERVICE_DISABLED
    - ErrorControl - one of following:
      SERVICE_ERROR_IGNORE, SERVICE_ERROR_NORMAL (default value, by which
      the startup program logs the error and displays a message but continues
      the startup operation), SERVICE_ERROR_SEVERE,
      SERVICE_ERROR_CRITICAL }
    constructor CreateNewService(const TargetComputer, DatabaseName,
      Name, DisplayName, Path: string;
      const OrderGroup: string = ''; const Dependances: string = '';
      const Username: string = ''; const Password: string = '';
      DesiredAccess: DWORD = SERVICE_ALL_ACCESS;
      ServiceType: DWORD = SERVICE_WIN32_OWN_PROCESS or SERVICE_INTERACTIVE_PROCESS;
      StartType: DWORD = SERVICE_DEMAND_START; ErrorControl: DWORD = SERVICE_ERROR_NORMAL);
    {{ Opens an existing service, in order  to control it or its configuration
      from your application. Parameters (strings are unicode-ready since Delphi 2009):
   - TargetComputer - set it to empty string if local computer is the target.
   - DatabaseName - set it to empty string if the default database is supposed
                ('ServicesActive').
   - Name - name of a service.
   - DesiredAccess - a combination of following flags:
     SERVICE_ALL_ACCESS, SERVICE_CHANGE_CONFIG, SERVICE_ENUMERATE_DEPENDENTS,
     SERVICE_INTERROGATE, SERVICE_PAUSE_CONTINUE, SERVICE_QUERY_CONFIG,
     SERVICE_QUERY_STATUS, SERVICE_START, SERVICE_STOP, SERVICE_USER_DEFINED_CONTROL }
    constructor CreateOpenService(const TargetComputer, DataBaseName, Name: String;
      DesiredAccess: DWORD = SERVICE_ALL_ACCESS);
    {{ release memory and handles }
    destructor Destroy; override;
    {{ Handle of SC manager }
    property SCHandle: THandle read FSCHandle;
    {{ Handle of service opened or created
      - its value is 0 if something failed in any Create*() method }
    property Handle: THandle read FHandle;
    {{ Retrieve the Current status of the service }
    property Status: TServiceStatus read GetStatus;
    {{ Retrive the Current state of the service }
    property State: TServiceState read GetState;
    {{ Requests the service to stop }
    function Stop: boolean;
    {{ Requests the service to pause }
    function Pause: boolean;
    {{ Requests the paused service to resume }
    function Resume: boolean;
    {{ Requests the service to update immediately its current status information
      to the service control manager }
    function Refresh: boolean;
    {{ Request the service to shutdown
     - this function always return false }
    function Shutdown: boolean;
    {{ Removes service from the system, i.e. close the Service }
    function Delete: boolean;
    {{ starts the execution of a service with some specified arguments
     - this version expect PChar pointers, either AnsiString (for FPC and old
      Delphi compiler), either UnicodeString (till Delphi 2009) }
    function Start(const Args: array of PChar): boolean;
    {{ this method will check the command line parameters, and will let
       control the service according to it
      - MyServiceSetup.exe /install will install the service
      - MyServiceSetup.exe /start   will start the service
      - MyServiceSetup.exe /stop    will stop the service
      - MyServiceSetup.exe /uninstall will uninstall the service
      - so that you can write in the main block of your .dpr:
      !begin
      ! if ParamCount<>0 then
      !   with TServiceController.CreateOpenService('','',HTTPSERVICENAME) do
      !   try
      !     CheckParameters('MyService.exe',HTTPSERVICEDISPLAYNAME);
      !   finally
      !     Free;
      !   end else
      !   ...
      !end;
    }
    procedure CheckParameters(const ExeFileName,DisplayName: string); virtual;
  end;

  TService = class;

  /// callback procedure for Windows Service Controller
  TServiceControlHandler = procedure(CtrlCode: DWORD); stdcall;

  /// event triggered for Control handler
  TServiceControlEvent = procedure(Sender: TService; Code: DWORD) of object;

  /// event triggered to implement the Service functionality
  TServiceEvent = procedure(Sender: TService) of object;

  {{ TService is the class used to implement a service provided by an application }
  TService = class
  protected
    fSName: String;
    fDName: String;
    fStartType: DWORD;
    fServiceType: DWORD;
    fData: DWORD;
    fControlHandler: TServiceControlHandler;
    fOnControl: TServiceControlEvent;
    fOnInterrogate: TServiceEvent;
    fOnPause: TServiceEvent;
    fOnShutdown: TServiceEvent;
    fOnStart: TServiceEvent;
    fOnExecute: TServiceEvent;
    fOnResume: TServiceEvent;
    fOnStop: TServiceEvent;
    fStatusRec: TServiceStatus;
    fArgsList: array of string;
    fJumper: PByteArray;
    fStatusHandle: THandle;
    function GetArgCount: Integer;
    function GetArgs(Idx: Integer): String;
    function GetInstalled: boolean;
    procedure SetStatus(const Value: TServiceStatus);
    procedure CtrlHandle(Code: DWORD);
    function GetControlHandler: TServiceControlHandler;
    procedure SetControlHandler(const Value: TServiceControlHandler);
  public
    /// this method is the main service entrance, from the OS point of view
    // - it will call OnControl/OnStop/OnPause/OnResume/OnShutdown events
    // - and report the service status to the system (via ReportStatus method)
    procedure DoCtrlHandle(Code: DWORD); virtual;
    /// Creates the service
    // - the service is added to the internal registered services
    // - main application must call the global ServicesRun procedure to actually
    // start the services
    // - caller must free the TService instance when it's no longer used
    constructor Create(const aServiceName, aDisplayName: String); reintroduce; virtual;
    /// free memory and release handles
    destructor Destroy; override;
    {{ Reports new status to the system }
    function ReportStatus(dwState, dwExitCode, dwWait: DWORD): BOOL;
    {{ Installs the service in the database
      - return true on success
      - create a local TServiceController with the current executable file,
        with the supplied command line parameters}
    function Install(const Params: string=''): boolean;
    {{ Removes the service from database
      - uses a local TServiceController with the current Service Name }
    procedure Remove;
    {{ Starts the service
      - uses a local TServiceController with the current Service Name }
    procedure Start;
    {{ Stops the service
      - uses a local TServiceController with the current Service Name }
    procedure Stop;
    {{ this is the main method, in which the Service should implement its run  }
    procedure Execute; virtual;

    {{ Name of the service. Must be unique }
    property ServiceName: String read fSName;
    {{ Display name of the service }
    property DisplayName: String read fDName write fDName;
    {{ Type of service }
    property ServiceType: DWORD read fServiceType write fServiceType;
    {{ Type of start of service }
    property StartType: DWORD read fStartType write fStartType;
    {{ Number of arguments passed to the service by the service controler }
    property ArgCount: Integer read GetArgCount;
    {{ List of arguments passed to the service by the service controler }
    property Args[Idx: Integer]: String read GetArgs;
    {{ Current service status
      - To report new status to the system, assign another
       value to this record, or use ReportStatus method (better) }
    property Status: TServiceStatus read fStatusRec write SetStatus;
    {{ Any data You wish to associate with the service object }
    property Data: DWORD read FData write FData;
    {{ Whether service is installed in DataBase
      - uses a local TServiceController to check if the current Service Name exists }
    property Installed: boolean read GetInstalled;
    {{ Callback handler for Windows Service Controller
      - if handler is not set, then auto generated handler calls DoCtrlHandle
      (note that this auto-generated stubb is... not working yet - so you should
      either set your own procedure to this property, or use TServiceSingle)
      - a typical control handler may be defined as such:
      ! var MyGlobalService: TService;
      !
      ! procedure MyServiceControlHandler(Opcode: LongWord); stdcall;
      ! begin
      !   if MyGlobalService<>nil then
      !     MyGlobalService.DoCtrlHandle(Opcode);
      ! end;
      !
      ! ...
      ! MyGlobalService := TService.Create(...
      ! MyGlobalService.ControlHandler := MyServiceControlHandler;
    }
    property ControlHandler: TServiceControlHandler
      read GetControlHandler write SetControlHandler;
    {{ Start event is executed before the main service thread (i.e. in the Execute method) }
    property OnStart: TServiceEvent read fOnStart write fOnStart;
    {{ custom Execute event
      - launched in the main service thread (i.e. in the Execute method) }
    property OnExecute: TServiceEvent read fOnExecute write fOnExecute;
    {{ custom event triggered when a Control Code is received from Windows }
    property OnControl: TServiceControlEvent read fOnControl write fOnControl;
    {{ custom event triggered when the service is stopped }
    property OnStop: TServiceEvent read fOnStop write fOnStop;
    {{ custom event triggered when the service is paused }
    property OnPause: TServiceEvent read fOnPause write fOnPause;
    {{ custom event triggered when the service is resumed }
    property OnResume: TServiceEvent read fOnResume write fOnResume;
    {{ custom event triggered when the service receive an Interrogate }
    property OnInterrogate: TServiceEvent read fOnInterrogate write fOnInterrogate;
    {{ custom event triggered when the service is shut down }
    property OnShutdown: TServiceEvent read fOnShutdown write fOnShutdown;
  end;

  /// inherit from this service if your application has a single service
  // - note that TService jumper does not work well - so use this instead
  TServiceSingle = class(TService)
  public
    /// will set a global function as service controller
    constructor Create(const aServiceName, aDisplayName: String); override;
    /// will release the global service controller
    destructor Destroy; override;
  end;


var
  /// the internal list of Services handled by this unit
  // - not to be accessed directly: create TService instances, and they will
  // be added/registered to this list
  // - then run the global ServicesRun procedure
  // - every TService instance is to be freed by the main application, when
  // it's no more used
  Services: TList = nil;

  /// the main TService instance running 
  ServiceSingle: TServiceSingle = nil;

{{ launch the registered Services execution
  - the registered list of service provided by the aplication is sent
   to the operating system
  - returns TRUE on success
  - returns FALSE on error (to get extended information, call GetLastError) }
function ServicesRun: boolean;

{{ convert the Control Code retrieved from Windows into a service state
  enumeration item }
function CurrentStateToServiceState(CurrentState: DWORD): TServiceState;

/// return the ready to be displayed text of a TServiceState value
function ServiceStateText(State: TServiceState): string;


implementation


{ TServiceController }

constructor TServiceController.CreateNewService(const TargetComputer,
  DatabaseName, Name, DisplayName, Path, OrderGroup, Dependances, Username,
  Password: String; DesiredAccess, ServiceType, StartType,
  ErrorControl: DWORD);
{$ifndef NOMORMOTKERNEL}
var backupError: cardinal;
{$endif}
begin
  inherited Create;
  if Path='' then begin
    {$ifndef NOMORMOTKERNEL}
    SQLite3Log.Add.Log(sllError,'CreateNewService("%","%") with Path=""',
      [Name,DisplayName]);
    {$endif}
    Exit;
  end;
  if TargetComputer='' then
  case GetDriveType(pointer(ExtractFileDrive(Path))) of
  DRIVE_REMOTE: begin
    {$ifndef NOMORMOTKERNEL}
    SQLite3Log.Add.Log(sllError,'CreateNewService("%","%") on remote drive: Path="%"',
      [Name,DisplayName,Path]);
    {$endif}
    Exit;
  end;
  end;
  StringToUTF8(Name,FName);
  FSCHandle := OpenSCManager(pointer(TargetComputer), pointer(DatabaseName),
    SC_MANAGER_ALL_ACCESS);
  if FSCHandle=0 then begin
    {$ifndef NOMORMOTKERNEL}
    backupError := GetLastError;
    SQLite3Log.Add.Log(sllLastError,'OpenSCManager("%","%") for "%"',
      [TargetComputer,DatabaseName,FName]);
    SetLastError(backupError);
    {$endif}
    Exit;
  end;
  FHandle := CreateService(FSCHandle, pointer(Name), pointer(DisplayName),
               DesiredAccess, ServiceType, StartType, ErrorControl, pointer(Path),
               pointer(OrderGroup), nil, pointer(Dependances),
               pointer(Username), pointer(Password));
  {$ifndef NOMORMOTKERNEL}
  if FHandle=0 then begin
    backupError := GetLastError;
    SQLite3Log.Add.Log(sllLastError,'CreateService("%","%","%")',[Name,DisplayName,Path]);
    SetLastError(backupError);
  end;
  {$endif}
end;

constructor TServiceController.CreateOpenService(const TargetComputer,
  DataBaseName, Name: String; DesiredAccess: DWORD);
{$ifndef NOMORMOTKERNEL}
var backupError: cardinal;
{$endif}
begin
  inherited Create;
  StringToUTF8(Name,FName);
  FSCHandle := OpenSCManager(pointer(TargetComputer), pointer(DatabaseName),
    GENERIC_READ);
  if FSCHandle = 0 then begin
    {$ifndef NOMORMOTKERNEL}
    backupError := GetLastError;
    SQLite3Log.Add.Log(sllLastError,'OpenSCManager("%","%") for "%"',
      [TargetComputer,DatabaseName,FName]);
    SetLastError(backupError);
    {$endif}
    Exit;
  end;
  FHandle := OpenService(FSCHandle, pointer(Name), DesiredAccess);
  {$ifndef NOMORMOTKERNEL}
  if FHandle=0 then begin
    backupError := GetLastError;
    SQLite3Log.Add.Log(sllLastError,'OpenService("%")',[Name]);
    SetLastError(backupError);
  end;
  {$endif}
end;

function TServiceController.Delete: boolean;
begin
  Result := FALSE;
  if FHandle <> 0 then
    if DeleteService(FHandle) then begin
      Result := CloseServiceHandle(FHandle);
      FHandle := 0;
    end
    {$ifndef NOMORMOTKERNEL}
    else
      SQLite3Log.Add.Log(sllLastError,'DeleteService("%")',[FName]);
    {$endif}
end;

destructor TServiceController.Destroy;
begin
  if FHandle <> 0 then
    CloseServiceHandle(FHandle);
  if FSCHandle <> 0 then
    CloseServiceHandle(FSCHandle);
  inherited;
end;

function TServiceController.GetState: TServiceState;
begin
  if (self=nil) or (FSCHandle=0) then
    result := ssErrorRetrievingState else
  if FHandle=0 then
    result := ssNotInstalled else
    result := CurrentStateToServiceState(Status.dwCurrentState);
  {$ifndef NOMORMOTKERNEL}
  SQLite3Log.Add.Log(sllTrace,FName,TypeInfo(TServiceState),result);
  {$endif}
end;

function TServiceController.GetStatus: TServiceStatus;
begin
  FillChar(FStatus, Sizeof(FStatus), 0);
  QueryServiceStatus(FHandle, FStatus);
  Result := FStatus;
end;

function TServiceController.Pause: boolean;
begin
  Result := ControlService(FHandle, SERVICE_CONTROL_PAUSE, FStatus);
end;

function TServiceController.Refresh: boolean;
begin
  Result := ControlService(FHandle, SERVICE_CONTROL_INTERROGATE, FStatus);
end;

function TServiceController.Resume: boolean;
begin
  Result := ControlService(FHandle, SERVICE_CONTROL_CONTINUE, FStatus);
end;

function TServiceController.Shutdown: boolean;
begin
  Result := ControlService(FHandle, SERVICE_CONTROL_SHUTDOWN, FStatus);
end;

function StartService(hService: SC_HANDLE; dwNumServiceArgs: DWORD;
  lpServiceArgVectors: Pointer): BOOL; stdcall; external advapi32
  name {$ifdef UNICODE}'StartServiceW';{$else}'StartServiceA';{$endif}

function TServiceController.Start(const Args: array of PChar): boolean;
begin
  Result := StartService(FHandle, length(Args), @Args[0]);
end;

function TServiceController.Stop: boolean;
begin
  Result := ControlService(FHandle, SERVICE_CONTROL_STOP, FStatus);
end;

procedure TServiceController.CheckParameters(const ExeFileName,DisplayName: string);
procedure ShowError(const Msg: RawUTF8);
begin
  {$ifndef NOMORMOTKERNEL}
  SQLite3Log.Add.Log(sllLastError,Msg,self);
  {$endif}
end;
var param: string;
    i: integer;
begin
  if State=ssErrorRetrievingState then
    exit;
  for i := 1 to ParamCount do begin
    param := SysUtils.LowerCase(paramstr(i));
    {$ifndef NOMORMOTKERNEL}
    SQLite3Log.Add.Log(sllInfo,'Controling % with command "%"',[FName,param]);
    {$endif}
    if param='/install' then
      TServiceController.CreateNewService('','',UTF8ToString(FName),
          DisplayName,ExeFileName,'','','','',
          SERVICE_ALL_ACCESS,
          SERVICE_WIN32_OWN_PROCESS
            {$ifdef USEMESSAGES}or SERVICE_INTERACTIVE_PROCESS{$endif},
          SERVICE_AUTO_START).  // auto start at every boot
        Free else
    if param='/uninstall' then begin
      if not Stop then
        ShowError('Stop');
      if not Delete then
        ShowError('Delete');
    end else
    if param='/stop' then begin
      if not Stop then
        ShowError('Stop');
    end else
    if param='/start' then begin
      if not Start([]) then
        ShowError('Start');
    end;
  end;
end;

{ TService }

function FindServiceIndex(const Name: String): integer;
begin
  if Services<>nil then
  for result := 0 to Services.Count-1 do
    if TService(Services[result]).ServiceName=Name then
      exit;
  result := -1;
end;

procedure JumpToService;
asm
{$ifdef CPU64}
  {$ifdef CPUX64}
  .NOFRAME
  {$endif}
  pop  rax
  mov  rax,[rax] // !!! THIS WON'T WORK!
  mov  rdx,[esp+8]
  call TService.CtrlHandle
  ret  8 
{$else}
  pop  eax
  mov  eax, [eax]           // retrieve self value
  mov  edx, [esp+4]
  call TService.CtrlHandle
  ret  4
{$endif}
end;

constructor TService.Create(const aServiceName, aDisplayName: String);
begin
  if FindServiceIndex(aServiceName)>=0 then
    raise Exception.CreateFmt('Attempt to install a service ' +
          'with duplicated name: %s', [aServiceName]);
  fSName := aServiceName;
  fDName := aDisplayName;
  if aDisplayName = '' then
    fDName := aServiceName;
  if Services=nil then begin
    Services := TList.Create;
    GarbageCollector.Add(Services);
  end;
  Services.Add(self);
  fServiceType := SERVICE_WIN32_OWN_PROCESS or SERVICE_INTERACTIVE_PROCESS;
  fStartType   := SERVICE_AUTO_START;
  fStatusRec.dwServiceType := fServiceType;
  fStatusRec.dwCurrentState := SERVICE_STOPPED;
  fStatusRec.dwControlsAccepted := 31;
  fStatusRec.dwWin32ExitCode := NO_ERROR;
  {$ifndef NOMORMOTKERNEL}
  SQLite3Log.Add.Log(sllInfo,'% (%) running as "%"',
    [ServiceName,aDisplayName,ExeVersion.ProgramFullSpec],self);
  {$endif}
end;

procedure TService.CtrlHandle(Code: DWORD);
begin
  DoCtrlHandle(Code);
end;

destructor TService.Destroy;
var i: integer;
begin
  if fsName<>'' then begin
    i := FindServiceIndex(fsName);
    if i<0 then
      raise Exception.CreateFmt('Cannot find service %s to remove from the list',
        [fsName]);
    Services.Delete(i);
    if Assigned(fJumper) then
      VirtualFree(fJumper, 0, MEM_RELEASE);
  end;
  inherited;
end;

procedure TService.DoCtrlHandle(Code: DWORD);
begin
  {$ifndef NOMORMOTKERNEL}
  SQLite3Log.Enter(self);
  SQLite3Log.Add.Log(sllInfo,'%: command % received from OS',[ServiceName,Code],self);
  {$endif}
   case Code of
     SERVICE_CONTROL_STOP: begin
       ReportStatus(SERVICE_STOP_PENDING, NO_ERROR, 0);
       if Assigned(fOnStop) then
         fOnStop(Self);
       ReportStatus(SERVICE_STOPPED, NO_ERROR, 0);
     end;
     SERVICE_CONTROL_PAUSE: begin
       ReportStatus(SERVICE_PAUSE_PENDING, NO_ERROR, 0);
       if Assigned(fOnPause) then
         fOnPause(Self);
       ReportStatus(SERVICE_PAUSED, NO_ERROR, 0)
     end;
     SERVICE_CONTROL_CONTINUE: begin
       ReportStatus(SERVICE_CONTINUE_PENDING, NO_ERROR, 0);
       if Assigned(fOnResume) then
         fOnResume(Self);
       ReportStatus(SERVICE_RUNNING, NO_ERROR, 0);
     end;
     SERVICE_CONTROL_SHUTDOWN: begin
       if Assigned(fOnShutdown) then
         fOnShutdown(Self);
       Code := 0;
     end;
     SERVICE_CONTROL_INTERROGATE: begin
       SetServiceStatus(FStatusHandle, fStatusRec);
        if Assigned(fOnInterrogate) then
          fOnInterrogate(Self);
     end;
   end;
   if Assigned(fOnControl) then
     fOnControl(Self, Code);
end;

procedure TService.Execute;
begin
  if Assigned(fOnStart) then
    fOnStart(@Self);
  ReportStatus(SERVICE_RUNNING, NO_ERROR, 0);
  if Assigned(fOnExecute) then
    fOnExecute(@Self);
end;

function TService.GetArgCount: Integer;
begin
  result := length(FArgsList);
end;

function TService.GetArgs(Idx: Integer): String;
begin
  if cardinal(Idx)>cardinal(high(FArgsList)) then
    result := '' else // avoid GPF
    result := FArgsList[Idx];
end;

function TService.GetControlHandler: TServiceControlHandler;
var AfterCallAddr: Pointer;
    Offset: Integer;
begin
  Result := fControlHandler;
  if not Assigned(Result) then
  begin
    raise Exception.Create('Automated jumper generation is not working: '+
     'use TServiceSingle or set a custom ControlHandler');
    if fJumper=nil then begin
      fJumper := VirtualAlloc(nil, 5+sizeof(Pointer), MEM_COMMIT, PAGE_EXECUTE_READWRITE);
      if fJumper=nil then
        raise Exception.CreateFmt('Cannot allocate memory for service jump gate: %s',
          [fSName]);
      AfterCallAddr := Pointer(PtrUInt(fJumper)+5);
      Offset :=  PtrInt(@JumpToService)-PtrInt(AfterCallAddr);
      fJumper[0] := $E8; // call opcode
      PInteger(@fJumper[1])^ := Offset;       // points to JumpToService
      PPtrUInt(@fJumper[5])^ := PtrUInt(self); // will be set as EAX=self
    end;
    Result := Pointer(fJumper);
  end;
end;

function TService.GetInstalled: boolean;
begin
  with TServiceController.CreateOpenService('','',fSName,SERVICE_QUERY_STATUS) do
  try
    result := Handle<>0;
  finally
    Free;
  end;
end;

function TService.Install(const Params: string): boolean;
var schService: SC_HANDLE;
    schSCManager: SC_HANDLE;
    ServicePath: TFileName;
begin
  result := false;
  if installed then
    exit;
  ServicePath := ExeVersion.ProgramFileName;
  if Params<>'' then
    ServicePath := ServicePath+' '+Params;
  schSCManager := OpenSCManager(nil, nil, SC_MANAGER_ALL_ACCESS);
  if (schSCManager>0) then begin
     schService := CreateService(schSCManager,
       pointer(fSName), pointer(fDName), SERVICE_ALL_ACCESS,
       fServiceType, fStartType, SERVICE_ERROR_NORMAL,
       pointer(ServicePath), nil, nil, nil, nil, nil);
     if (schService>0) then begin
       result := true;
       CloseServiceHandle(schService);
     end;
  end;
end;

procedure TService.Remove;
begin
  with TServiceController.CreateOpenService('','',fSName,SERVICE_ALL_ACCESS) do
  try
    if Handle=0 then exit;
    Stop;
    Delete;
  finally
    Free;
  end;
end;

function TService.ReportStatus(dwState, dwExitCode, dwWait: DWORD): BOOL;
{$ifndef NOMORMOTKERNEL}
var status: string;
begin
  status := ServiceStateText(CurrentStateToServiceState(dwState));
  SQLite3Log.Add.Log(sllInfo,'% ReportStatus(%,%,%)',
    [ServiceName,status,dwExitCode,dwWait],self);
{$else}
begin
{$endif}
  if dwState = SERVICE_START_PENDING then
    fStatusRec.dwControlsAccepted := 0 else
    fStatusRec.dwControlsAccepted := 31;
  fStatusRec.dwCurrentState  := dwState;
  fStatusRec.dwWin32ExitCode := dwExitCode;
  fStatusRec.dwWaitHint := dwWait;
  if (dwState = SERVICE_RUNNING) or (dwState = SERVICE_STOPPED) then
    fStatusRec.dwCheckPoint := 0 else
    inc(fStatusRec.dwCheckPoint);
  result := SetServiceStatus(FStatusHandle, fStatusRec);
  if not result then
    {$ifndef NOMORMOTKERNEL}
    SQLite3Log.Add.Log(sllLastError,'% ReportStatus(%,%,%)',
      [ServiceName,status,dwExitCode,dwWait],self);
    {$endif}
end;

procedure TService.SetControlHandler(const Value: TServiceControlHandler);
begin
  fControlHandler := Value;
  if Assigned(fJumper) then
    VirtualFree(fJumper, 0, MEM_RELEASE);
end;

procedure TService.SetStatus(const Value: TServiceStatus);
begin
  fStatusRec := Value;
  if FStatusHandle <> 0 then
    SetServiceStatus(FStatusHandle, fStatusRec);
end;

procedure TService.Start;
begin
  with TServiceController.CreateOpenService('','',fSName,SERVICE_ALL_ACCESS) do
  try
    Start([]);
  finally
    Free;
  end;
end;

procedure TService.Stop;
begin
  with TServiceController.CreateOpenService('','',fSName,SERVICE_ALL_ACCESS) do
  try
    Stop;
  finally
    Free;
  end;
end;

function CurrentStateToServiceState(CurrentState: DWORD): TServiceState;
begin
  case CurrentState of
    SERVICE_STOPPED:          result := ssStopped;
    SERVICE_START_PENDING:    result := ssStarting;
    SERVICE_STOP_PENDING:     result := ssStopping;
    SERVICE_RUNNING:          result := ssRunning;
    SERVICE_CONTINUE_PENDING: result := ssResuming;
    SERVICE_PAUSE_PENDING:    result := ssPausing;
    SERVICE_PAUSED:           result := ssPaused;
    else result := ssNotInstalled; // e.g. SERVICE_CONTROL_SHUTDOWN
  end;
end;

{$ifdef NOMORMOTKERNEL} // translated caption needed only if with full UI
function ServiceStateText(State: TServiceState): string;
var P: PShortString;
begin
  P := GetEnumName(TypeInfo(TServiceState),ord(State));
  result := string(copy(P^,3,length(P^)-2));
end;
{$else}
function ServiceStateText(State: TServiceState): string;
begin
  result := PTypeInfo(TypeInfo(TServiceState))^.EnumBaseType^.GetCaption(State);
end;
{$endif}

{  function that a service process specifies as the entry point function
  of a particular service. The function can have any application-defined name
  - Args points to an array of pointers that point to null-terminated
    argument strings. The first argument in the array is the name of the service,
    and subsequent arguments are any strings passed to the service by the process
    that called the StartService function to start the service.  }
procedure ServiceProc(ArgCount: DWORD; Args: PPChar); stdcall;
var i: integer;
    Srv: TService;
begin
  i := FindServiceIndex(Args^);
  if i<0 then
    exit; // avoid any GPF
  Srv := Services.Items[i];
  for i := 1 to ArgCount-1 do begin
    Inc(Args);
    SetLength(Srv.FArgsList, length(Srv.FArgsList)+1);
    Srv.FArgsList[high(Srv.FArgsList)] := Args^;
  end;
  Srv.FStatusHandle := RegisterServiceCtrlHandler(
    pointer(Srv.fSName), @Srv.ControlHandler);
  if Srv.FStatusHandle = 0 then begin
    Srv.ReportStatus(SERVICE_STOPPED, GetLastError, 0);
    Exit;
  end;
  Srv.ReportStatus(SERVICE_START_PENDING, 0, 0);
  Srv.Execute;
end;

function ServicesRun: boolean;
var S: array of TServiceTableEntry;
    i: integer;
begin
  SetLength(S,Services.Count+1);
  for i := 0 to Services.Count-1 do begin
    S[i].lpServiceName := pointer(TService(Services.List[i]).ServiceName);
    S[i].lpServiceProc := @ServiceProc;
  end;
  result := StartServiceCtrlDispatcher(S[0]);
end;


{ TServiceSingle }

procedure SingleServiceControlHandler(Opcode: LongWord); stdcall;
begin
  if ServiceSingle<>nil then
    ServiceSingle.DoCtrlHandle(Opcode);
end;

constructor TServiceSingle.Create(const aServiceName,
  aDisplayName: String);
begin
  inherited;
  if ServiceSingle<>nil then
    raise Exception.Create('Only one TServiceSingle is allowed at a time');
  ServiceSingle := self;
  ControlHandler := SingleServiceControlHandler;
end;

destructor TServiceSingle.Destroy;
begin
  try
    inherited;
  finally
    ServiceSingle := nil;
  end;
end;

end.
