/// `fs` module support bindings for SyNode
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SyNodeBinding_fs;

interface

{$I Synopse.inc}
{$I SyNode.inc}

uses
  SysUtils,
  SynCommons,
  SpiderMonkey,
  SyNode;

function os_realpath(const FileName: TFileName; var TargetName: TFileName): Boolean;

implementation

{$IFNDEF SM_DEBUG}
  {$UNDEF SM_DEBUG_FSTRACEFILEREAD}
  {$UNDEF SM_DEBUG_FSDUMPFILEEXCERPT}
{$ENDIF}

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ELSE}
  Unix, BaseUnix, DateUtils,
{$ENDIF}
  Classes,
  SynLog;

/// decode text file to string using BOM
//  if BOM not fount - use current system code page to convert ANSI content to unicode
//  if file not found - return empty string
//  internaly use AnyTextFileToRawUTF8
//  accept one parameter  - "path to file"
function fs_loadFile(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
const
  USAGE = 'usage loadFile(pathToFile: String, [forceUFT8: boolean])';
var
  in_argv: PjsvalVector;
  forceUTF8: boolean;
  name: String;
begin
  forceUTF8 := true;
  result := true;
  try
    in_argv := vp.argv;
    if (argc < 1) or not in_argv[0].isString or ((argc > 1) and not in_argv[1].isBoolean )then
      raise ESMException.Create(USAGE);

    if argc > 1 then
      forceUTF8 := in_argv[1].asBoolean;

    name := in_argv[0].asJSString.ToString(cx);
    {$ifdef SM_DEBUG_FSTRACEFILEREAD}
    SynSMLog.Add.Log(sllDebug, 'fs_loadFile (%) called', name);
    {$ENDIF}
    if not FileExists(name) then
      raise ESMException.Create('file does not exist');
    // implementation below dont work if called in the same time from differnt thread
    // TFileStream.Create(name, fmOpenRead).Free; // Check that file exists and can be opened;
    vp.rval := cx.NewJSString(AnyTextFileToRawUTF8(name, forceUTF8)).ToJSVal;
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

/// call RelToAbs
function fs_relToAbs(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
const
  USAGE = 'usage: relToAbs(ABase, ATail: string;)';
var
  in_argv: PjsvalVector;
  baseDir, fileName, resPath: TFileName;
begin
  try
    in_argv := vp.argv;
    if (argc <> 2) or not in_argv[0].isString or not in_argv[1].isString then
      raise ESMException.Create(USAGE);
    baseDir := in_argv[0].asJSString.ToString(cx);
    fileName := in_argv[1].asJSString.ToString(cx);
    resPath := RelToAbs(baseDir, fileName);
    vp.rval := cx.NewJSString(StringToSynUnicode(resPath)).ToJSVal;
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

/// return object{
//    atime: Date, //access time
//    mtime: Date, //modify time
//    ctime: Date  // create time
//    size: Number
//  }
// or null is file does not exist
function fs_fileStat(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
const
  USAGE = 'usage: fileStat(filePath: string;)';
var
  in_argv: PjsvalVector;
  fn: TFileName;
  obj: PJSRootedObject;
  val: jsval;
{$IFNDEF MSWINDOWS}
  info: stat;
{$ELSE}
  {$ifdef ISDELPHIXE2}
  infoRec: TDateTimeInfoRec;
  {$else}
  fad: TWin32FileAttributeData;

  function fileTimeToDateTime(ft: TFileTime): TDateTime;
  var ST,LT: TSystemTime;
  begin
    if FileTimeToSystemTime(ft,ST) and
     SystemTimeToTzSpecificLocalTime(nil,ST,LT) then
    result := SystemTimeToDateTime(LT) else
    result := 0;
  end;
  {$endif}
{$ENDIF}
begin
  try
    in_argv := vp.argv;
    if (argc < 1) or not in_argv[0].isString then
      raise ESMException.Create(USAGE);
    try
      fn := in_argv[0].asJSString.ToString(cx);
    except
      raise
    end;
    obj := cx.NewRootedObject(cx.NewObject(nil));
    try
      {$IFNDEF MSWINDOWS}
      if fpstat(fn, info) = 0 then begin
        val.asDate[cx] := UnixToDateTime(info.st_atime);
        obj.ptr.DefineProperty(cx, 'atime', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        val.asDate[cx] := UnixToDateTime(info.st_mtime);
        obj.ptr.DefineProperty(cx, 'mtime', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        val.asDate[cx] := UnixToDateTime(info.st_ctime);
        obj.ptr.DefineProperty(cx, 'ctime', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        val.asInt64 := info.st_size;
        obj.ptr.DefineProperty(cx, 'size', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        vp.rval := obj.ptr.ToJSValue;
      end else begin
        {$ifdef SM_DEBUG}
        SynSMLog.Add.Log(sllDebug, StringToUTF8(Format('fstat(%s) failed with error %s',
          [fn, SysErrorMessage(errno)])));
        {$endif}
        vp.rval := JSVAL_NULL;
      end;
      {$ELSE}
      {$ifdef ISDELPHIXE2}
      if FileGetDateTimeInfo(fn, infoRec, true) then begin
        val.asDate[cx] := infoRec.LastAccessTime;
        obj.ptr.DefineProperty(cx, 'atime', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        val.asDate[cx] := infoRec.TimeStamp;
        obj.ptr.DefineProperty(cx, 'mtime', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        val.asDate[cx] := infoRec.CreationTime;
        obj.ptr.DefineProperty(cx, 'ctime', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
          if TWin32FindData(infoRec).nFileSizeHigh > 0 then
            val := JSVAL_NAN
          else
            val.asInt64 := TWin32FindData(infoRec).nFileSizeLow;
        obj.ptr.DefineProperty(cx, 'size', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        vp.rval := obj.ptr.ToJSValue;
      end else
        vp.rval := JSVAL_NULL;
      {$else ISDELPHIXE2}
      if GetFileAttributesEx(PChar(fn), GetFileExInfoStandard, @fad) then begin
        val.asDate[cx] := fileTimeToDateTime(fad.ftLastAccessTime);
        obj.ptr.DefineProperty(cx, 'atime', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        val.asDate[cx] := fileTimeToDateTime(fad.ftLastWriteTime);
        obj.ptr.DefineProperty(cx, 'mtime', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        val.asDate[cx] := fileTimeToDateTime(fad.ftCreationTime);
        obj.ptr.DefineProperty(cx, 'ctime', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        if fad.nFileSizeHigh > 0 then
          val := JSVAL_NAN
        else
          val.asInt64 := fad.nFileSizeLow;
        obj.ptr.DefineProperty(cx, 'size', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        vp.rval := obj.ptr.ToJSValue;
      end else
        vp.rval := JSVAL_NULL;
      {$endif ISDELPHIXE2}
      {$ENDIF MSWINDOWS}
    finally
      cx.FreeRootedObject(obj);
    end;
    Result := True;
  except
    on E: Exception do begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

/// check directory exist
//  accept one parameter  - "path to dir"
function fs_directoryExists(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
const
  USAGE = 'usage: directoryExists(dirPath: string;)';
var
  in_argv: PjsvalVector;
  filePath: TFileName;
  val: jsval;
begin
  try
    in_argv := vp.argv;
    if (argc < 1) or not in_argv[0].isString then
      raise ESMException.Create(USAGE);
    filePath := in_argv[0].asJSString.ToString(cx);
    val.asBoolean := DirectoryExists(filePath);
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

/// check file exist
//  accept one parameter  - "path to file"
function fs_fileExists(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
const
  USAGE = 'usage: fileExists(filePath: string;)';
var
  in_argv: PjsvalVector;
  filePath: TFileName;
  val: jsval;
begin
  try
    in_argv := vp.argv;
    if (argc < 1) or not in_argv[0].isString then
      raise ESMException.Create(USAGE);
    filePath := in_argv[0].asJSString.ToString(cx);
    val.asBoolean := FileExists(filePath);
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

/// Used to speed up module loading.  Returns the contents of the file as
// a string or undefined when the file cannot be opened.  The speedup
// comes from not creating Error objects on failure.
function fs_internalModuleReadFile(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
const
  USAGE = 'usage: internalModuleReadFile(filePath: string;)';
var
  in_argv: PjsvalVector;
  filePath: TFileName;
  fileContent: jsval;
  {$ifdef SM_DEBUG_FSDUMPFILEEXCERPT}
  len: size_t;
  content: RawUTF8;
  {$endif}
begin
  try
    in_argv := vp.argv;
    if (argc < 1) or not in_argv[0].isString then
      raise ESMException.Create(USAGE);
    filePath := in_argv[0].asJSString.ToString(cx);
    {$ifdef SM_DEBUG_FSTRACEFILEREAD}
    SynSMLog.Add.Log(sllDebug, 'fs_internalModuleReadFile (%) called', filePath);
    {$ENDIF}
    if FileExists(filePath) then begin
      fileContent := cx.NewJSString(AnyTextFileToRawUTF8(filePath, true)).ToJSVal;
      vp.rval := fileContent;
      {$ifdef SM_DEBUG_FSDUMPFILEEXCERPT}
      len := fileContent.asJSString.Length;
      content := fileContent.asJSString.ToUTF8(cx);
      if len > 120 then
        content := Format('%s .. %s', [Copy(content, 1, 58), Copy(content, len-58, 58)]);
      SynSMLog.Add.Log(sllDebug,
        'fs_internalModuleReadFile (%): content loaded, length = % content:'#13'%',
        [filePath, len, content]);
      {$endif}
    end else begin
      vp.rval := JSVAL_VOID;
      {$ifdef SM_DEBUG}
      SynSMLog.Add.Log(sllDebug,
        'fs_internalModuleReadFile (%): file not found', StringToUTF8(filePath));
      {$endif}
    end;

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

/// Used to speed up module loading.  Returns 0 if the path refers to
// a file, 1 when it's a directory or < 0 on error (usually -ENOENT.)
// The speedup comes from not creating thousands of Stat and Error objects.
function fs_internalModuleStat(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
const
  USAGE = 'usage: internalModuleStat(filePath: string;)';
var
  in_argv: PjsvalVector;
  fn: TFileName;
  Code: Integer;
  val: jsval;
begin
  try
    in_argv := vp.argv;
    if (argc < 1) or not in_argv[0].isString then
      raise ESMException.Create(USAGE);
    try
      fn := in_argv[0].asJSString.ToString(cx);
    except
      raise
    end;
    {$IFDEF MSWINDOWS}
    Code := GetFileAttributes(PChar(fn));
    if Code <> -1 then
      if Code and FILE_ATTRIBUTE_DIRECTORY = 0 then
        Code := 0
      else
        Code := 1;
    {$ELSE} // this can be used in Windows too
    Code := FileGetAttr(fn);
    if Code <> -1 then
      if Code and faDirectory = 0 then
        Code := 0
      else
        Code := 1;
    {$ENDIF}
    val.asInteger := Code;
    vp.rval := val;

    Result := True;
  except
    on E: Exception do begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

/// read content of directory. return array of file names witout started from dot '.'
/// in case of includeDirNames - return sub direcrory with trailing slash
function fs_readDir(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;
  dir: TFileName;
  F: TSearchRec;
  res: PJSRootedObject;
  cNum, searchAttr: integer;
  includeFolders: boolean;
const
  USAGE = 'usage: readDir(dirPath: String; [includeFolders: boolean = false]): Array';
begin
  try
    in_argv := vp.argv;
    if (argc < 1) or not in_argv[0].isString then
      raise ESMException.Create(USAGE);
    if (argc = 2) and in_argv[1].isBoolean then
      includeFolders := in_argv[1].asBoolean
    else
      includeFolders := false;

    dir := in_argv[0].asJSString.ToString(cx);
    if not DirectoryExists(Dir) then
    begin
      vp.rval := JSVAL_NULL;
      Result := true;
      Exit;
    end;

    Dir := IncludeTrailingPathDelimiter(Dir);

    res := cx.NewRootedObject(cx.NewArrayObject(0));
    try
      {$WARN SYMBOL_PLATFORM OFF}
      searchAttr := faAnyFile;
      if not includeFolders then
        searchAttr := searchAttr - faDirectory;
      if FindFirst(Dir + '*.*', searchAttr, F) = 0 then begin
        cNum := 0;
        repeat
          if (F.Name <> '.') and (F.Name <> '..') then begin
            res.ptr.SetElement(cx, cNum, cx.NewJSString(F.Name).ToJSVal);
            inc(cNum);
          end;
        until FindNext(F) <> 0;
      end;
      {$WARN SYMBOL_PLATFORM ON}
      {$ifdef ISDELPHIXE2}System.{$ENDIF}SysUtils.FindClose(F);
      vp.rval := res.ptr.ToJSValue;
    finally
      cx.FreeRootedObject(res);
    end;
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

{$ifdef MSWINDOWS}
{$if defined(FPC) or not defined(ISDELPHIXE2)}
const VOLUME_NAME_DOS = $0;

// Only Wide version return a value
function GetFinalPathNameByHandleW(hFile: THandle; lpszFilePath: LPWSTR;
  cchFilePath: DWORD; dwFlags: DWORD): DWORD; stdcall; external kernel32;
{$ifend}

const LONG_PATH_PREFIX: PChar = '\\?\';
const UNC_PATH_PREFIX: PChar = '\\?\UNC\';
{$else}
function realpath(name: PChar; resolved: PChar): PChar; cdecl; external 'c' name 'realpath';
{$endif}

/// Implementation of realpath as in libuv
function os_realpath(const FileName: TFileName; var TargetName: TFileName): Boolean;
{$ifdef MSWINDOWS}
{$WARN SYMBOL_PLATFORM OFF}
var
  Handle: THandle;
  w_realpath_len: Cardinal;
  Res: WideString;
begin
  Result := False;
  if not CheckWin32Version(6, 0) then
    exit;
  Handle := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ, nil,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or FILE_FLAG_BACKUP_SEMANTICS, 0);
  if Handle = INVALID_HANDLE_VALUE then
    exit;
  try
    w_realpath_len := GetFinalPathNameByHandleW(Handle, nil, 0, VOLUME_NAME_DOS);
    if (w_realpath_len = 0) then
      exit;
    SetLength(Res, w_realpath_len);
    if GetFinalPathNameByHandleW(Handle, PWideChar(Res), w_realpath_len, VOLUME_NAME_DOS) > 0 then begin
      WideCharToStrVar(PWideChar(Res), String(TargetName));
      if StrLComp(PChar(TargetName), UNC_PATH_PREFIX, length(UNC_PATH_PREFIX)) = 0 then begin
        // convert \\?\UNC\host\folder -> \\host\folder
        TargetName := Copy(TargetName, 7, length(TargetName)-7);
        TargetName[1] := '\';
      end else if StrLComp(PChar(TargetName), LONG_PATH_PREFIX, length(LONG_PATH_PREFIX)) = 0 then
        TargetName := Copy(TargetName, length(LONG_PATH_PREFIX)+1, length(TargetName)-length(LONG_PATH_PREFIX))
      else
        Exit; // error
      Result := True;
    end;
  finally
    CloseHandle(Handle);
  end;
end;
{$WARN SYMBOL_PLATFORM ON}
{$else}
var
  Buf: TFileName;
begin
  SetLength(Buf, PATH_MAX);
  Result := realpath(PChar(FileName), PChar(Buf)) <> nil;
  if Result then
    TargetName := PChar(Buf)
  else
    TargetName := '';
end;
{$endif}

/// return the canonicalized absolute pathname
// expands all symbolic links and resolves references to /./,
// /../ and extra '/' characters in the string named by path
// to produce a canonicalized absolute pathname
function fs_realPath(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  dir, target: TFileName;
const
  USAGE = 'usage: realpath(dirPath: String): string';
begin
  try
    if (argc < 1) or not vp.argv[0].isString then
      raise ESMException.Create(USAGE);

    dir := vp.argv[0].asJSString.ToString(cx);
    //if not FileGetSymLinkTarget(dir, target) then
    if not os_realpath(dir, target) then
      target := dir;

    vp.rval := cx.NewJSString(target).ToJSVal;
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

function fs_rename(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
const
  USAGE = 'usage: rename(fromPath, toPath: String)';
var
  fromPath, toPath: TFileName;
  //f : file;
begin
  try
    if (argc <> 2) or not vp.argv[0].isString or not vp.argv[1].isString then
      raise ESMException.Create(USAGE);
    fromPath := vp.argv[0].asJSString.ToString(cx);
    toPath := vp.argv[1].asJSString.ToString(cx);
    {$IFDEF MSWINDOWS} // libc rename implementation rewrites destination if it's already exist
    if FileExists(toPath) then
      if not SysUtils.DeleteFile(toPath) then
        RaiseLastOSError;
    {$ENDIF}
    if not SysUtils.RenameFile(fromPath, toPath) then
      RaiseLastOSError;
    Result := True;
  except
    on E: Exception do begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

function SyNodeBindingProc_fs(const Engine: TSMEngine;
  const bindingNamespaceName: SynUnicode): jsval;
const
  attrs = JSPROP_READONLY or JSPROP_PERMANENT;
var
  obj: PJSRootedObject;
  cx: PJSContext;
begin
  cx := Engine.cx;
  obj := cx.NewRootedObject(cx.NewObject(nil));
  try
    obj.ptr.DefineFunction(cx, 'loadFile', fs_loadFile, 1, attrs);
    obj.ptr.DefineFunction(cx, 'relToAbs', fs_relToAbs, 2, attrs);
    obj.ptr.DefineFunction(cx, 'fileStat', fs_fileStat, 1, attrs);
    obj.ptr.DefineFunction(cx, 'directoryExists', fs_directoryExists, 1, attrs);
    obj.ptr.DefineFunction(cx, 'fileExists', fs_fileExists, 1, attrs);
    obj.ptr.DefineFunction(cx, 'internalModuleReadFile', fs_internalModuleReadFile, 1, attrs);
    obj.ptr.DefineFunction(cx, 'internalModuleStat', fs_internalModuleStat, 1, attrs);
    obj.ptr.DefineFunction(cx, 'readDir', fs_readDir, 2, attrs);
    obj.ptr.DefineFunction(cx, 'realpath', fs_realPath, 1, attrs);
    obj.ptr.DefineFunction(cx, 'rename', fs_rename, 2, attrs);

    Result := obj.ptr.ToJSValue;
  finally
    cx.FreeRootedObject(obj);
  end;
end;

initialization
  TSMEngineManager.RegisterBinding('fs', SyNodeBindingProc_fs);

end.
