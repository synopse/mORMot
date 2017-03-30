/// `worker` module support bindings for SyNode
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SyNodeBinding_worker;

interface
{$I SyNode.inc}

uses
  SynCommons,
  Classes,
  SpiderMonkey;

type
  TJSWorkersManager = class
  private
    FPool: TObjectListLocked;
    FWorkerCounter: integer;
    FIsInDestroyState: Boolean;
    function getOldErrorReporterForCurrentThread: JSErrorReporter;
    function getCurrentWorkerThreadIndex: integer;
    function getCurrentWorkerID: integer;
    function getWorkerThreadIndexByID(ID: Integer): integer;
    function GetDoInteruptInOwnThreadhandlerForCurThread: TThreadMethod;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(cx: PJSContext; const workerName: RawUTF8; const script: SynUnicode): Integer;
    function curThreadIsWorker: Boolean;
    function getCurrentWorkerThreadName: RawUTF8;
    procedure EnqueueOutMessageToCurrentThread(const mess: RawUTF8);
    function DequeueOutMessageFromThread(aID: Integer; out mess: RawUTF8): Boolean;
    procedure EnqueueInMessageToThread(const mess: RawUTF8; aID: Integer);
    function DequeueInMessageFromCurrentThread(out mess: RawUTF8): Boolean;
    procedure TerminateThread(aID: Integer; aNeedCancelExecution: Boolean = false);
    property OldErrorReporterForCurrentThread: JSErrorReporter read getOldErrorReporterForCurrentThread;
    property DoInteruptInOwnThreadhandlerForCurThread: TThreadMethod read GetDoInteruptInOwnThreadhandlerForCurThread;
  end;

implementation

uses
  SyNode,
  Windows,
  SysUtils;

type
  TJSWorkerThread = class(TThread)
  private
    FEng: TSMEngine;
    fSMManager: TSMEngineManager;
    FNeedCallInterrupt: boolean;
    fInMessages: TRawUTF8List;
    fOutMessages: TRawUTF8List;
    oldErrorReporter: JSErrorReporter;
    fID: Integer;
    FName: RawUTF8;
    FScript: SynUnicode;
  protected
    procedure Execute; override;
    procedure doInteruptInOwnThread;
  public
    constructor Create(aSMManager: TSMEngineManager; workerName: RawUTF8; script: SynUnicode);
    destructor Destroy; override;
  end;

const
  LAST_ERROR_PROP_NAME = '__workerLastError';

constructor TJSWorkerThread.Create(aSMManager: TSMEngineManager; workerName: RawUTF8; script: SynUnicode);
begin
  inherited Create(true);
  fSMManager := aSMManager;
  fID := InterlockedIncrement(fSMManager.WorkersManager.FWorkerCounter);
  FName := workerName;
  FScript := script;
  fInMessages := TRawUTF8List.Create();
  fOutMessages := TRawUTF8List.Create();
  FEng := nil;
  Suspended := False;
end;

destructor TJSWorkerThread.Destroy;
begin
  FreeAndNil(fInMessages);
  FreeAndNil(fOutMessages);
  if not Terminated then begin
    FEng.rt.ErrorReporter := oldErrorReporter;
    if FEng.cx.IsRunning then
      FEng.CancelExecution(false);
  end;
  inherited;
end;

procedure TJSWorkerThread.doInteruptInOwnThread;
begin
  FNeedCallInterrupt := true;
  Suspended := false;
end;

/// Post message from worker thread. aviable only in worker thread
function fromWorker_postMessage(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;
  mes: RawUTF8;
const
  sInvalidCall = 'ussage: postMessage(message: *);';
begin
  try
    in_argv := vp.argv;
    if (argc <> 1) then
      raise ESMException.Create(sInvalidCall);
    mes := in_argv[0].asJson[cx];
    TSMEngine(cx.PrivateData).Manager.WorkersManager.EnqueueOutMessageToCurrentThread(mes);
    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

/// Terminate worker from thread. aviable only in worker thread
function fromWorker_terminate(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  fManager: TJSWorkersManager;
begin
  try
    fManager := TSMEngine(cx.PrivateData).Manager.WorkersManager;
    fManager.TerminateThread(fManager.getCurrentWorkerID);
    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

procedure WorkerThreadErrorReporter(cx: PJSContext; _message: PCChar; report: PJSErrorReport); cdecl;
var
  exc: jsval;
begin
  if cx.GetPendingException(exc) then
    cx.CurrentGlobalOrNull.SetProperty(cx, LAST_ERROR_PROP_NAME, exc);
  if not TSMEngine(cx.PrivateData).Manager.WorkersManager.FIsInDestroyState then
    TSMEngine(cx.PrivateData).Manager.WorkersManager.OldErrorReporterForCurrentThread(cx, _message, report);
end;

procedure TJSWorkerThread.Execute;
var
  isEmpty: Boolean;
  mess: RawUTF8;
  val, rval: jsval;
  cx: PJSContext;
  msg, exc: PJSRootedValue;
begin
  FNeedCallInterrupt := false;
  FEng := fSMManager.ThreadSafeEngine(nil);
  try
    cx := FEng.cx;

    FEng.GlobalObject.ptr.DefineFunction(cx, 'postMessage', fromWorker_postMessage, 1, JSPROP_ENUMERATE or JSPROP_READONLY or JSPROP_PERMANENT);
    FEng.GlobalObject.ptr.DefineFunction(cx, 'terminate', fromWorker_terminate, 0, JSPROP_ENUMERATE or JSPROP_READONLY or JSPROP_PERMANENT);

    oldErrorReporter := FEng.rt.ErrorReporter;
    FEng.rt.ErrorReporter := WorkerThreadErrorReporter;

    val.asInteger := ThreadID;
    FEng.GlobalObject.ptr.defineProperty(cx, 'threadID', val);
    try
      FEng.Evaluate(FScript, '<initialization>', 0, rval);
    except
      Terminate;
      exit;
    end;

    while not Terminated do begin
      if FNeedCallInterrupt then begin
        FEng.rt.InterruptCallback(cx);
        FNeedCallInterrupt := false;
      end;

      isEmpty := fSMManager.WorkersManager.FIsInDestroyState or not fSMManager.WorkersManager.DequeueInMessageFromCurrentThread(mess);

      if not isEmpty then begin

        val.asJson[cx] := mess;
        msg := cx.NewRootedValue(val);
        try
          try
            if not Terminated then begin
              FEng.GlobalObject.ptr.SetProperty(cx, LAST_ERROR_PROP_NAME, JSVAL_VOID);
              FEng.CallObjectFunction(FEng.GlobalObject, 'onmessage', [msg.ptr]);
            end;
          except
            exc := cx.NewRootedValue(FEng.GlobalObject.ptr.GetPropValue(cx, LAST_ERROR_PROP_NAME));
            try
              if not fSMManager.WorkersManager.FIsInDestroyState and not exc.ptr.isVoid then begin
                try
                  FEng.CallObjectFunction(FEng.GlobalObject, 'onerror', [msg.ptr, exc.ptr]);
                except
                end;
              end;
            finally
              cx.FreeRootedValue(exc);
            end;
          end;
        finally
          cx.FreeRootedValue(msg);
        end;
      end else if not Terminated and not FNeedCallInterrupt then
        Suspended := True;
    end;
    try
      if not fSMManager.WorkersManager.FIsInDestroyState then
        FEng.CallObjectFunction(FEng.GlobalObject, 'onterminate', []);
    except
      ;
    end;
  finally
    fSMManager.ReleaseCurrentThreadEngine;
    FEng := nil;
  end;

end;

{ TJSWorkersManager }

function TJSWorkersManager.Add(cx: PJSContext; const workerName: RawUTF8; const script: SynUnicode): Integer;
var
  thread: TJSWorkerThread;
  i: Integer;
begin
  FPool.Safe.Lock;
  try
    // delete unused threads
    for i := FPool.Count - 1 downto 0 do begin
      thread := TJSWorkerThread(FPool[i]);
      if thread.Terminated and
        (thread.fOutMessages.Count = 0) then
        FPool.Delete(i);
    end;
    thread := TJSWorkerThread.Create(TSMEngine(cx.PrivateData).Manager, workerName, script);
    FPool.Add(thread);
    Result := thread.fID;
  finally
    FPool.Safe.UnLock;
  end;
end;

constructor TJSWorkersManager.Create;
begin
  FWorkerCounter := 0;
  FIsInDestroyState := false;
  FPool := TObjectListLocked.Create();
end;

function TJSWorkersManager.curThreadIsWorker: Boolean;
begin
  FPool.Safe.Lock;
  try
    Result := getCurrentWorkerThreadIndex <> -1;
  finally
    FPool.Safe.UnLock;
  end;
end;

function TJSWorkersManager.DequeueInMessageFromCurrentThread(out mess: RawUTF8): Boolean;
var
  curThreadIndex: Integer;
begin
  Result := false;
  FPool.Safe.Lock;
  try
    curThreadIndex := getCurrentWorkerThreadIndex;
    if curThreadIndex <> -1 then begin
      Result := TJSWorkerThread(FPool[curThreadIndex]).fInMessages.PopFirst(mess);
    end;
  finally
    FPool.Safe.UnLock;
  end;
end;

function TJSWorkersManager.DequeueOutMessageFromThread(aID: Integer;
  out mess: RawUTF8): Boolean;
var
  ThreadIndex: Integer;
  thread: TJSWorkerThread;
begin
  Result := false;
  FPool.Safe.Lock;
  try
    ThreadIndex := getWorkerThreadIndexByID(aID);
    if ThreadIndex <> -1 then begin
      thread := TJSWorkerThread(FPool[ThreadIndex]);
      Result := thread.fOutMessages.PopFirst(mess);
      if thread.Terminated  and
        (thread.fOutMessages.Count = 0) then
        FPool.Delete(ThreadIndex);
    end;
  finally
    FPool.Safe.UnLock;
  end;
end;

destructor TJSWorkersManager.Destroy;
var i: Integer;
begin
  FIsInDestroyState := True;
  FPool.Safe.Lock;
  try
    for i := 0 to FPool.Count - 1 do begin
      TerminateThread(TJSWorkerThread(FPool[i]).FID, True);
    end;
  finally
    FPool.Safe.UnLock;
  end;
  FreeAndNil(FPool);
  inherited;
end;

procedure TJSWorkersManager.EnqueueInMessageToThread(const mess: RawUTF8;
  aID: Integer);
var
  ThreadIndex: Integer;
  thread: TJSWorkerThread;
begin
  FPool.Safe.Lock;
  try
    ThreadIndex := getWorkerThreadIndexByID(aID);
    if ThreadIndex <> -1 then begin
      thread := TJSWorkerThread(FPool[ThreadIndex]);
      if not thread.Terminated then begin
        thread.fInMessages.Add(mess);
        thread.Suspended := False;
      end;
    end;
  finally
    FPool.Safe.UnLock;
  end;
end;

procedure TJSWorkersManager.EnqueueOutMessageToCurrentThread(
  const mess: RawUTF8);
var
  curThreadIndex: Integer;
begin
  FPool.Safe.Lock;
  try
    curThreadIndex := getCurrentWorkerThreadIndex;
    if curThreadIndex <> -1 then begin
      TJSWorkerThread(FPool[curThreadIndex]).fOutMessages.Add(mess);
    end;
  finally
    FPool.Safe.UnLock;
  end;
end;

function TJSWorkersManager.getCurrentWorkerID: integer;
var
  i: Integer;
  curThreadID: TThreadID;
begin
  Result := -1;
  curThreadID := GetCurrentThreadId;
  for i := 0 to FPool.Count - 1 do begin
    if TJSWorkerThread(FPool[i]).ThreadID = curThreadID then begin
      Result := TJSWorkerThread(FPool[i]).fID;
      Exit;
    end;
  end;
end;

function TJSWorkersManager.getCurrentWorkerThreadIndex: integer;
var
  i: Integer;
  curThreadID: TThreadID;
begin
  Result := -1;
  curThreadID := GetCurrentThreadId;
  for i := 0 to FPool.Count - 1 do begin
    if TJSWorkerThread(FPool[i]).ThreadID = curThreadID then begin
      Result := i;
      Exit;
    end;
  end;
end;

function TJSWorkersManager.getCurrentWorkerThreadName: RawUTF8;
var
  curThreadIndex: Integer;
begin
  Result := '';
  FPool.Safe.Lock;
  try
    curThreadIndex := getCurrentWorkerThreadIndex;
    if curThreadIndex <> -1 then
      Result := TJSWorkerThread(FPool[curThreadIndex]).FName;
  finally
    FPool.Safe.UnLock;
  end;
end;

function TJSWorkersManager.getOldErrorReporterForCurrentThread: JSErrorReporter;
var
  curThreadIndex: Integer;
begin
  Result := nil;
  if FIsInDestroyState then begin
    Result := nil;
    Exit;
  end;
  FPool.Safe.Lock;
  try
    curThreadIndex := getCurrentWorkerThreadIndex;
    if curThreadIndex <> - 1 then
      Result := TJSWorkerThread(FPool[curThreadIndex]).oldErrorReporter;
  finally
    FPool.Safe.UnLock;
  end;
end;

function TJSWorkersManager.getWorkerThreadIndexByID(ID: Integer): integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FPool.Count - 1 do begin
    if TJSWorkerThread(FPool[i]).fID = ID then begin
      Result := i;
      Exit;
    end;
  end;
end;

function TJSWorkersManager.GetDoInteruptInOwnThreadhandlerForCurThread: TThreadMethod;
var
  curThreadIndex: Integer;
begin
  Result := nil;
  FPool.Safe.Lock;
  try
    curThreadIndex := getCurrentWorkerThreadIndex;
    if curThreadIndex <> -1 then
      Result := TJSWorkerThread(FPool[curThreadIndex]).doInteruptInOwnThread;
  finally
    FPool.Safe.UnLock;
  end;
end;

procedure TJSWorkersManager.TerminateThread(aID: Integer; aNeedCancelExecution: Boolean = false);
var
  ThreadIndex: Integer;
  thread: TJSWorkerThread;
begin
  FPool.Safe.Lock;
  try
    ThreadIndex := getWorkerThreadIndexByID(aID);
    if ThreadIndex <> -1 then begin
      thread := TJSWorkerThread(FPool[ThreadIndex]);
      if aNeedCancelExecution and (thread.FEng <> nil) then
        thread.FEng.CancelExecution(False);
      thread.Terminate;
      thread.Suspended := False;
    end;
  finally
    FPool.Safe.UnLock;
  end;
end;

/// create new worker thread. return worker ID
function worker_createThread(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;
  scriptOnMessage, scriptOnTerminate, scriptOnError: SynUnicode;
  fThread: TJSWorkerThread;
  workerName: RawUTF8;
  params, obj: PJSObject;
  IsInvalidCall: Boolean;
  name, onmessage, onterminate, onerror: jsval;
  val: jsval;
  FEng: TSMEngine;
const
  sInvalidCall = 'ussage: createThread({name: String, onmessage: String|Function[, onterminate: String|Function][, onerror: string|Function][, message: *]})';
begin
  try
    in_argv := vp.argv;

    IsInvalidCall := (argc < 1) and in_argv[0].isObject;
    if not IsInvalidCall then begin
      params := in_argv[0].asObject;

      params.GetProperty(cx, 'name', name);
      if name.isString then
        workerName := name.asJSString.ToUTF8(cx)
      else
        IsInvalidCall := True;

      if not IsInvalidCall then begin
        params.GetProperty(cx, 'onmessage', onmessage);
        if onmessage.isString then
          scriptOnMessage := onmessage.asJSString.ToSynUnicode(cx)
        else if onmessage.isObject then begin
          obj := onmessage.asObject;
          if obj.isFunction(cx) then
            scriptOnMessage := obj.DecompileFunction(cx, true).ToSynUnicode(cx)
          else
            IsInvalidCall := True;
        end else
          IsInvalidCall := True;
      end;

      if not IsInvalidCall then begin
        params.GetProperty(cx, 'onterminate', onterminate);
        if onterminate.isVoid then
          scriptOnTerminate := 'function(){}'
        else if onterminate.isString then
          scriptOnTerminate := onterminate.asJSString.ToSynUnicode(cx)
        else if onterminate.isObject then begin
          obj := onterminate.asObject;
          if obj.IsFunction(cx) then
            scriptOnTerminate := obj.DecompileFunction(cx).ToSynUnicode(cx)
          else
            IsInvalidCall := True;
        end else
          IsInvalidCall := True;
      end;

      if not IsInvalidCall then begin
        params.GetProperty(cx, 'onerror', onerror);
        if onerror.isVoid then
          scriptOnError := 'function(){}'
        else if onerror.isString then
          scriptOnError := onerror.asJSString.ToSynUnicode(cx)
        else if onerror.isObject then begin
          obj := onerror.asObject;
          if obj.IsFunction(cx) then
            scriptOnError := obj.DecompileFunction(cx).ToSynUnicode(cx)
          else
            IsInvalidCall := True;
        end else
          IsInvalidCall := True;
      end;
    end;

    if IsInvalidCall then
      raise ESMException.Create(sInvalidCall);

    FEng := TSMEngine(cx.PrivateData);

    val.asInteger := FEng.Manager.WorkersManager.Add(cx, workerName,
      WideFormat('this.onmessage = %s;this.onterminate = %s;this.onerror = %s',
      [scriptOnMessage, scriptOnTerminate, scriptOnError]));
    vp.rval := val;
    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

/// Post message to worker thread
function worker_postMessage(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;
  threadID: Integer;
  mes: RawUTF8;
const
  sInvalidCall = 'ussage: postMessage(threadID: Number, message: String);';
begin
  try
    in_argv := vp.argv;
    if (argc < 2) or not (in_argv[0].isNumber) or not (in_argv[1].isString) then
      raise ESMException.Create(sInvalidCall);
    if in_argv[0].isInteger then
      threadID := in_argv[0].asInteger
    else
      threadID := Round(in_argv[0].asDouble);

    mes := in_argv[1].asJSString.ToUTF8(cx);
    TSMEngine(cx.PrivateData).Manager.WorkersManager.EnqueueInMessageToThread(mes, threadID);

    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;

end;

/// Get message from worker thread
function  worker_getMessage(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;
  threadID: Integer;
  mes: RawUTF8;
const
  sInvalidCall = 'ussage: getMessage(threadID: Number);';
begin
  try
    in_argv := vp.argv;
    if (argc < 1) or not (in_argv[0].isNumber) then
      raise ESMException.Create(sInvalidCall);

    if in_argv[0].isInteger then
      threadID := in_argv[0].asInteger
    else
      threadID := Round(in_argv[0].asDouble);

    if TSMEngine(cx.PrivateData).Manager.WorkersManager.DequeueOutMessageFromThread(threadID, mes) then
      vp.rval := cx.NewJSString(mes).ToJSVal
    else
      vp.rval := JSVAL_VOID;

    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

/// Terminate worker thread.
function worker_terminate(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;
  threadID: Integer;
const
  sInvalidCall = 'ussage: terminate(threadID: Number);';
begin
  try
    in_argv := vp.argv;
    if (argc < 1) or not (in_argv[0].isNumber) then
      raise ESMException.Create(sInvalidCall);

    if in_argv[0].isInteger then
      threadID := in_argv[0].asInteger
    else
      threadID := Round(in_argv[0].asDouble);

    TSMEngine(cx.PrivateData).Manager.WorkersManager.TerminateThread(threadID, true);

    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

function SyNodeBindingProc_worker(const aEngine: TSMEngine; const bindingNamespaceName: SynUnicode): jsval;
var
  obj: PJSRootedObject;
  cx: PJSContext;
const
  props = JSPROP_ENUMERATE or JSPROP_ENUMERATE or JSPROP_PERMANENT;
begin
  cx := aEngine.cx;
  obj := cx.NewRootedObject(cx.NewObject(nil));
  obj.ptr.DefineFunction(cx, 'createThread', worker_createThread, 1, props);
  obj.ptr.DefineFunction(cx, 'postMessage', worker_postMessage, 2, props);
  obj.ptr.DefineFunction(cx, 'getMessage', worker_getMessage, 1, props);
  obj.ptr.DefineFunction(cx, 'terminate', worker_terminate, 1, props);
  Result := obj.ptr.ToJSValue;
  cx.FreeRootedObject(obj);
end;

initialization
  TSMEngineManager.RegisterBinding('worker', SyNodeBindingProc_worker);
end.
