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
  Unix, BaseUnix,
{$ENDIF}
  DateUtils,
  Classes,
{$IFDEF FPC}
  LazFileUtils,
{$ENDIF}
  puv_fs,
  SynLog,
  SyNodeReadWrite,
  SyNodeBinding_buffer;

{$IFDEF MSWINDOWS}
function _get_osfhandle(fd: LongInt): THandle; cdecl;
  external 'msvcrt';
{$ENDIF}

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
  USAGE = 'usage: fileStat(path: String|fd: Integer[; followSymLinks: Boolean = False])';
var
  in_argv: PjsvalVector;
  res: LongInt;
  fn: TFileName;
  obj: PJSRootedObject;
  val: jsval;
  info: Tpuv_stat_info;
begin
  try
    in_argv := vp.argv;
    if (argc < 1) or not (in_argv[0].isString or in_argv[0].isInteger) then
      raise ESMException.Create(USAGE);
    obj := cx.NewRootedObject(cx.NewObject(nil));
    try
      if in_argv[0].isString then begin
        fn := in_argv[0].asJSString.ToString(cx);
        if (argc = 1) or not in_argv[1].isBoolean or not in_argv[1].asBoolean then
          res := puv_fs_stat(fn, info)
        else
          res := puv_fs_lstat(fn, info);
      end else
        res := puv_fs_fstat(in_argv[0].asInteger, info);
      if res = 0 then begin
        val.asInt64 := info.dev;
        obj.ptr.DefineProperty(cx, 'dev', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        val.asInt64 := info.ino;
        obj.ptr.DefineProperty(cx, 'ino', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        val.asInt64 := info.nlink;
        obj.ptr.DefineProperty(cx, 'nlink', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);

        val.asInteger := info.mode;
        obj.ptr.DefineProperty(cx, 'mode', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        val.asInteger := info.uid;
        obj.ptr.DefineProperty(cx, 'uid', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        val.asInteger := info.gid;
        obj.ptr.DefineProperty(cx, 'gid', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);

        val.asInt64 := info.rdev;
        obj.ptr.DefineProperty(cx, 'rdev', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        val.asInt64 := info.size;
        obj.ptr.DefineProperty(cx, 'size', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        val.asInt64 := info.blksize;
        obj.ptr.DefineProperty(cx, 'blksize', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        val.asInt64 := info.blocks;
        obj.ptr.DefineProperty(cx, 'blocks', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);

        val.asDate[cx] := info.atime;
        obj.ptr.DefineProperty(cx, 'atime', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        //st_atime_nsec : qword
        val.asDate[cx] := info.mtime;
        obj.ptr.DefineProperty(cx, 'mtime', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        //st_mtime_nsec : qword
        val.asDate[cx] := info.ctime;
        obj.ptr.DefineProperty(cx, 'ctime', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        //st_ctime_nsec : qword
        val.asDate[cx] := info.birthtime;
        obj.ptr.DefineProperty(cx, 'birthtime', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);

        vp.rval := obj.ptr.ToJSValue;
      end else begin
        Result := False;
        vp.rval := JSVAL_VOID;
        JSOSErrorUC(cx, '', GetLastOSError, 'stat', RawUTF8(fn));
        Exit;
      end;
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
    fn := in_argv[0].asJSString.ToString(cx);
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
  encoding: TEncoding;
  info: Tpuv_stat_info;
const
  USAGE = 'usage: readDir(dirPath: String; [encoding: String = "uft8"]): Array';
begin
  try
    in_argv := vp.argv;
    if (argc < 1) or not in_argv[0].isString then
      raise ESMException.Create(USAGE);
    if (argc = 2) and in_argv[1].isString then
      encoding := ParseEncoding(in_argv[1].asJSString.ToUTF8(cx), UTF8)
    else
      encoding := UTF8;

    dir := in_argv[0].asJSString.ToString(cx);
    if puv_fs_stat(dir, info) <> 0 then begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSOSErrorUC(cx, '', GetLastOSError, 'readdir', RawUTF8(dir));
      Exit;
    end;
    if (info.mode and S_IFMT) <> S_IFDIR then begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSOSErrorUC(cx, '', 20{ENOTDIR}, 'readdir', RawUTF8(dir));
      Exit;
    end;

    Dir := IncludeTrailingPathDelimiter(Dir);

    res := cx.NewRootedObject(cx.NewArrayObject(0));
    try
      {$WARN SYMBOL_PLATFORM OFF}
      searchAttr := faAnyFile;
      if FindFirst(Dir + '*', searchAttr, F) = 0 then begin
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

function fs_renameFile(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
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

/// write Object to file using specifien encoding.
//  internaly use SyNodeReadWrite.write
function fs_writeFile(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;
  handle: THandle;
  stream: THandleStream;
  writer: SynCommons.TTextWriter;
begin
  try
    if (argc < 2) then
      raise ESMException.Create('usage writeFile(fd: Integer, fileContent: String|Object|ArrayBuffer [,encoding]');
    in_argv := vp.argv;
    {$IFDEF MSWINDOWS}
    handle := _get_osfhandle(in_argv[0].asInteger);
    {$ELSE}
    handle := in_argv[0].asInteger;
    {$ENDIF}
    stream := THandleStream.Create(handle);
    writer := SynCommons.TTextWriter.Create(stream, 65536);
    try
      vp.rval := SyNodeReadWrite.SMWrite_impl(cx, argc - 1, @in_argv[1], writer);
      result := True;
    finally
      writer.FlushFinal;
      writer.Free;
      stream.Free;
    end;
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

/// delete file AFileName: TFileName - full file path
function fs_deleteFile(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;
  path: TFileName;
  val: jsval;
  res: Integer;
begin
  try
    if (argc <> 1) then
      raise ESMException.Create('Invalid number of args for function nsm_deleteFile. Requied 1  - filePath');
    in_argv := vp.argv;
    path := in_argv[0].asJSString.ToString(cx);
    res := puv_fs_unlink(path);
    if res <> 0 then begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSOSErrorUC(cx, '', GetLastOSError, 'unlink', RawUTF8(path));
      Exit;
    end;
    val.asInteger := res;
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

function fs_makeDir(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
const
  f_usage = 'usage: makeDir(dirPath: String[; mode: Integer])';
  f_invalidPath = 'empty or relative path is not allowed';
var
  in_argv: PjsvalVector;
  dir: TFileName;
  mode: Integer;
begin
  try
    in_argv := vp.argv;
    if (argc < 1) or (argc > 2) and in_argv[0].isString then
      raise ESMException.Create(f_usage);
    dir := in_argv[0].asJSString.ToString(cx);
    if (argc = 2) then begin
      if in_argv[1].isInteger then
        mode := in_argv[1].asInteger
      else
        raise ESMException.Create('Invalid second argument type');
    end else
      mode := &777;
    if (dir = '') or {$IFNDEF FPC}IsRelativePath(dir){$ELSE}not FilenameIsAbsolute(dir){$ENDIF} then
      raise ESMException.Create(f_invalidPath);
    if puv_fs_mkdir(dir, mode) <> 0 then begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSOSErrorUC(cx, '', GetLastOSError, 'mkdir', RawUTF8(dir));
      Exit;
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

function fs_removeDir(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;
  dir: TFileName;
  val: jsval;
const
  f_usage = 'usage: removeDir(dirPath: String): Boolean';
  f_invalidPath = 'empty or relative path is not allowed';
begin
  try
    in_argv := vp.argv;
    if (argc <> 1) or not in_argv[0].isString then
      raise ESMException.Create(f_usage);
    dir := in_argv[0].asJSString.ToString(cx);
    if (dir = '') or {$IFNDEF FPC}IsRelativePath(dir){$ELSE}not FilenameIsAbsolute(dir){$ENDIF} then
      raise ESMException.Create(f_invalidPath);
    if puv_fs_rmdir(dir) <> 0 then begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSOSErrorUC(cx, '', GetLastOSError, 'rmdir', RawUTF8(dir));
      Exit;
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

function fs_openFile(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;
  fn: TFileName;
  flags, mode, handle: LongInt;
  val: jsval;
const
  f_usage = 'usage: openFile(fileName: String; flags: Integer; mode: Integer): Integer';
begin
  try
    in_argv := vp.argv;
    if (argc <> 3) or
      not (in_argv[0].isString and in_argv[1].isInteger and in_argv[2].isInteger) then
      raise ESMException.Create(f_usage);
    fn := in_argv[0].asJSString.ToString(cx);
    flags := in_argv[1].asInteger;
    mode := in_argv[2].asInteger;
    handle := puv_fs_open(fn, flags, mode);
    Result := handle >= 0;
    if Result then begin
      val.asInteger := handle;
      vp.rval := val;
    end else begin
      vp.rval := JSVAL_VOID;
      JSOSErrorUC(cx, '', GetLastOSError, 'open', RawUTF8(fn));
    end;
  except
    on E: Exception do
    begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSError(cx, E);
    end;
  end;
end;

function fs_closeFile(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;
  fd: LongInt;
  val: jsval;
const
  f_usage = 'usage: closeFile(handle: Integer): Integer';
begin
  try
    in_argv := vp.argv;
    if (argc <> 1) or not in_argv[0].isInteger then
      raise ESMException.Create(f_usage);
    fd := in_argv[0].asInteger;
    val.asInteger := puv_fs_close(fd);
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

function fs_readFile(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;
  target: PJSRootedObject;
  fd: LongInt;
  buf: PUtf8Char;
  offset, len: Integer;
  size: size_t;
  res: Int64;
  val: jsval;
begin
  try
    in_argv := vp.argv;
    if (argc < 2) then
      raise ESMTypeException.Create('fd and buffer are required');
    if not in_argv[0].isInteger or (in_argv[0].asInteger < 0) then
      raise ESMTypeException.Create('fd must be a file descriptor');
    if not in_argv[1].isObject or not in_argv[1].asObject.IsTypedArrayObject then
      raise ESMTypeException.Create('Second argument needs to be a buffer');
    fd := in_argv[0].asInteger;
    target := cx.NewRootedObject(in_argv[1].asObject);
    try
      getBufDataAndLength(cx, target, buf, size);
    finally
      cx.FreeRootedObject(target);
    end;
    if (argc > 2) then
       offset := in_argv[2].asInteger
    else
      offset := 0;
    if (offset < 0) or (offset >= size) then
      raise ESMException.Create('Offset is out of bounds');
    if (argc > 3) then
       len := in_argv[3].asInteger
    else
      len := 0;
    if (len > 0) then begin
      if (Int64(len) + offset > size) then
        raise ESMException.Create('Length extends beyond buffer');
      if (argc > 4) and not (in_argv[4].isVoid or in_argv[4].isNull) then
        raise ENotImplemented.Create('Not null position is currently not supported');
      Inc(buf, offset);
      res := puv_fs_read(fd, buf^, len);
      if res < 0 then begin
        Result := False;
        vp.rval := JSVAL_VOID;
        JSOSErrorUC(cx, '', GetLastOSError, 'read');
        Exit;
      end;
      val.asInteger := res;
    end else
      val.asInteger := 0;
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

function fs_writeFileBuffer(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;
  target: PJSRootedObject;
  fd: LongInt;
  buf: PUtf8Char;
  offset, len: Integer;
  size: size_t;
  res: Int64;
  val: jsval;
  t: JSType;
const
  f_usage = 'usage: writeFileBuffer(handle: Integer; buffer: Buffer; [offset: Integer = 0]; len: Integer; [position: Integer]): Integer';
begin
  try
    in_argv := vp.argv;
    if (argc < 4) or not in_argv[1].isObject then
      raise ESMException.Create(f_usage);
    if not in_argv[0].isInteger or (in_argv[0].asInteger < 0) then
      raise ESMTypeException.Create('fd must be a file descriptor');
    fd := in_argv[0].asInteger;
    target := cx.NewRootedObject(in_argv[1].asObject);
    try
      getBufDataAndLength(cx, target, buf, size);
    finally
      cx.FreeRootedObject(target);
    end;
    if (argc > 2) then
       offset := in_argv[2].asInteger
    else
      offset := 0;
    if (offset < 0) or (offset >= size) then
      raise ESMException.Create('Offset is out of bounds');
    if (argc > 3) then
       len := in_argv[3].asInteger
    else
      len := 0;
    if (len > 0) then begin
      if (Int64(len) + offset > size) then
        raise ESMException.Create('Length extends beyond buffer');
      if (argc > 4) and not (in_argv[4].isVoid or in_argv[4].isNull) then
        raise ENotImplemented.Create('Not null position is currently not supported');
      Inc(buf, offset);
      res := puv_fs_write(fd, buf^, len);
      if res < 0 then begin
        Result := False;
        vp.rval := JSVAL_VOID;
        JSOSErrorUC(cx, '', GetLastOSError, 'write');
        Exit;
      end;
      val.asInteger := res;
    end else
      val.asInteger := 0;
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

function fs_writeFileString(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;
  _str: PJSString;
  str: Pointer;
  position: Cardinal;
  encoding: TEncoding;
  fd: LongInt;
  buf: RawByteString;
  len, size: size_t;
  isLatin1: Boolean;
  res: Int64;
  val: jsval;
  strUtf8: RawUTF8;
const
  f_usage = 'usage: writeFileString(handle: Integer; buffer: String; position: Integer; encoding: String): Integer';
begin
  try
    in_argv := vp.argv;
    if (argc < 4) or not (in_argv[1].isString and in_argv[2].isInteger and in_argv[3].isString) then
      raise ESMException.Create(f_usage);
    if not in_argv[0].isInteger or (in_argv[0].asInteger < 0) then
      raise ESMTypeException.Create('fd must be a file descriptor');
    fd := in_argv[0].asInteger;
    _str := in_argv[1].asJSString;
    position := in_argv[2].asInteger;
    encoding := ParseEncoding(in_argv[3].asJSString.ToUTF8(cx), UTF8);
    if position <> 0 then
      raise ESMException.Create('Not 0 position is not supported currently');
    getStrDataAndLength(cx, _str, encoding, strUtf8, str, len, size, isLatin1);
    if (size > 0) then begin
      SetLength(buf, size);
      size := StringBytesWrite(PChar(buf), size, str, len, isLatin1, encoding);
      if (size = 0) then
        raise ESMException.Create('Invalid string');
      res := puv_fs_write(fd, PChar(buf)^, size);
      if res < 0 then begin
        Result := False;
        vp.rval := JSVAL_VOID;
        JSOSErrorUC(cx, '', GetLastOSError, 'write');
        Exit;
      end;
      val.asInteger := res;
    end else
      val.asInteger := 0;
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

function fs_accessFile(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;
  path: TFileName;
  mode: Integer;
  res: Integer;
begin
  try
    in_argv := vp.argv;
    if (argc <> 2) then
      raise ESMTypeException.Create('path and mode are required');
    if in_argv[0].isString then
      path := in_argv[0].asJSString.ToString(cx)
    else
      raise ESMTypeException.Create('path must be a string or Buffer');
    if in_argv[1].isInteger then
      mode := in_argv[1].asInteger
    else
      raise ESMTypeException.Create('mode must be an integer');
    res := puv_fs_access(path, mode);
    if res <> 0 then begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSOSErrorUC(cx, '', GetLastOSError, 'access', path);
      Exit;
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

function fs_chmodFile(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
var
  in_argv: PjsvalVector;
  path: RawUTF8 = '';
  mode: Integer;
  res: Integer;
begin
  try
    in_argv := vp.argv;
    if argc <> 2 then
      raise ESMTypeException.Create('path and mode are required');
    if in_argv[1].isInteger then
      mode := in_argv[1].asInteger
    else
      raise ESMTypeException.Create('mode must be an integer');
    if in_argv[0].isString then begin
      path := in_argv[0].asJSString.ToString(cx);
      res := puv_fs_chmod(path, mode)
    end else if in_argv[0].isInteger then
      res := puv_fs_fchmod(in_argv[0].asInteger, mode)
    else
      raise ESMTypeException.Create('path must be a string or Buffer');
    if res <> 0 then begin
      Result := False;
      vp.rval := JSVAL_VOID;
      JSOSErrorUC(cx, '', GetLastOSError, 'chmod', path);
      Exit;
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
    obj.ptr.DefineFunction(cx, 'access', fs_accessFile, 2, attrs);
    obj.ptr.DefineFunction(cx, 'loadFile', fs_loadFile, 1, attrs); // Used from Module
    obj.ptr.DefineFunction(cx, 'relToAbs', fs_relToAbs, 2, attrs);
    obj.ptr.DefineFunction(cx, 'fileStat', fs_fileStat, 1, attrs);
//    obj.ptr.DefineFunction(cx, 'directoryExists', fs_directoryExists, 1, attrs);
//    obj.ptr.DefineFunction(cx, 'fileExists', fs_fileExists, 1, attrs);
    obj.ptr.DefineFunction(cx, 'internalModuleReadFile', fs_internalModuleReadFile, 1, attrs);
    obj.ptr.DefineFunction(cx, 'internalModuleStat', fs_internalModuleStat, 1, attrs);
    obj.ptr.DefineFunction(cx, 'readdir', fs_readDir, 2, attrs);
    obj.ptr.DefineFunction(cx, 'realpath', fs_realPath, 1, attrs);
    obj.ptr.DefineFunction(cx, 'rename', fs_renameFile, 2, attrs);
    obj.ptr.DefineFunction(cx, 'writeFile', fs_writeFile, 3, attrs);
    obj.ptr.DefineFunction(cx, 'mkdir', fs_makeDir, 2, attrs);
    obj.ptr.DefineFunction(cx, 'rmdir', fs_removeDir, 1, attrs);
    obj.ptr.DefineFunction(cx, 'unlink', fs_deleteFile, 1, attrs);
    obj.ptr.DefineFunction(cx, 'open', fs_openFile, 3, attrs);
    obj.ptr.DefineFunction(cx, 'close', fs_closeFile, 1, attrs);
    obj.ptr.DefineFunction(cx, 'read', fs_readFile, 2, attrs);
    obj.ptr.DefineFunction(cx, 'writeBuffer', fs_writeFileBuffer, 4, attrs);
    obj.ptr.DefineFunction(cx, 'writeString', fs_writeFileString, 4, attrs);
    obj.ptr.DefineFunction(cx, 'chmodFile', fs_chmodFile, 1, attrs);
    Result := obj.ptr.ToJSValue;
  finally
    cx.FreeRootedObject(obj);
  end;
end;

initialization
  TSMEngineManager.RegisterBinding('fs', SyNodeBindingProc_fs);

end.

