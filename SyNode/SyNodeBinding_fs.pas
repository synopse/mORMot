/// `fs` module support bindings for SyNode
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SyNodeBinding_fs;

interface

{$I Synopse.inc}
{$I SyNode.inc}

uses
  SynCommons,
  SpiderMonkey,
  SyNode;

function SyNodeBindingProc_fs(const Engine: TSMEngine; const bindingNamespaceName: SynUnicode): jsval;

implementation

uses
  {$ifdef ISDELPHIXE2}System.SysUtils,{$else}SysUtils,{$endif}
  Windows;

/// decode text file to string using BOM
//  if BOM not fount - use current system code page to convert ANSI content to unicode
//  if file not found - return empty string
//  internaly use m3Commons.DecodeTextFileToString
//  accept one parameter  - "path to file"
function fs_loadFile(cx: PJSContext; argc: uintN; var vp: JSArgRec): Boolean; cdecl;
const
  USAGE = 'usage loadFile(pathToFile: String, [forceUFT8: boolean])';
var
  in_argv: PjsvalVector;
  forceUTF8: boolean;
begin
  forceUTF8 := true;
  result := true;
  try
    in_argv := vp.argv;
    if (argc < 1) or not in_argv[0].isString or ((argc > 1) and not in_argv[1].isBoolean )then
      raise ESMException.Create(USAGE);

    if argc > 1 then
      forceUTF8 := in_argv[1].asBoolean;

    vp.rval := cx.NewJSString(AnyTextFileToRawUTF8(in_argv[0].asJSString.ToString(cx), forceUTF8)).ToJSVal;
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
  USAGE = 'usage: relToAbs(ABaseDir, AFileName: string;)';
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
// or null is file not exists
function fs_fileStat(cx: PJSContext; argc: uintN; var vp: jsargRec): Boolean; cdecl;
const
  USAGE = 'usage: fileStat(filePath: string;)';
var
  in_argv: PjsvalVector;
  fn: TFileName;
  obj: PJSRootedObject;
  val: jsval;
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
begin
  try
    in_argv := vp.argv;
    if (argc < 1) or not in_argv[0].isString then
      raise ESMException.Create(USAGE);
    try
      fn := in_argv[0].asJSString.ToSynUnicode(cx);
    except
      raise
    end;
    obj := cx.NewRootedObject(cx.NewObject(nil));
    try
      {$ifdef ISDELPHIXE2}
      if FileGetDateTimeInfo(fn, infoRec, true) then begin
        val.asDate[cx] := infoRec.LastAccessTime;
        obj.ptr.DefineProperty(cx, 'atime', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        val.asDate[cx] := infoRec.TimeStamp;
        obj.ptr.DefineProperty(cx, 'mtime', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        val.asDate[cx] := infoRec.CreationTime;
        obj.ptr.DefineProperty(cx, 'ctime', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        {$IFDEF MSWINDOWS}
          if TWin32FindData(infoRec).nFileSizeHigh > 0 then
            val := JSVAL_NAN
          else
            val.asInt64 := TWin32FindData(infoRec).nFileSizeLow;
        obj.ptr.DefineProperty(cx, 'size', val, JSPROP_ENUMERATE or JSPROP_READONLY, nil, nil);
        {$ENDIF MSWINDOWS}
        {$IFDEF POSIX}
          //st: _stat platform;
          raise Exception.Create('TODO');
        {$ENDIF POSIX}
        vp.rval := obj.ptr.ToJSValue;
      end else
        vp.rval := JSVAL_NULL;
      {$else}
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
  filePath: string;
  val: jsval;
begin
  try
    in_argv := vp.argv;
    if (argc < 1) or not in_argv[0].isString then
      raise ESMException.Create(USAGE);
    filePath := in_argv[0].asJSString.ToSynUnicode(cx);
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
  filePath: string;
  val: jsval;
begin
  try
    in_argv := vp.argv;
    if (argc < 1) or not in_argv[0].isString then
      raise ESMException.Create(USAGE);
    filePath := in_argv[0].asJSString.ToSynUnicode(cx);
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

function SyNodeBindingProc_fs(const Engine: TSMEngine;
  const bindingNamespaceName: SynUnicode): jsval;
var
  obj: PJSRootedObject;
  cx: PJSContext;
begin
  cx := Engine.cx;
  obj := cx.NewRootedObject(cx.NewObject(nil));
  try
    obj.ptr.DefineFunction(cx, 'loadFile', fs_loadFile, 1, JSPROP_READONLY or JSPROP_PERMANENT);
    obj.ptr.DefineFunction(cx, 'relToAbs', fs_relToAbs, 2, JSPROP_READONLY or JSPROP_PERMANENT);
    obj.ptr.DefineFunction(cx, 'fileStat', fs_fileStat, 1, JSPROP_READONLY or JSPROP_PERMANENT);
    obj.ptr.DefineFunction(cx, 'directoryExists', fs_directoryExists, 1, JSPROP_READONLY or JSPROP_PERMANENT);
    obj.ptr.DefineFunction(cx, 'fileExists', fs_fileExists, 1, JSPROP_READONLY or JSPROP_PERMANENT);

    Result := obj.ptr.ToJSValue;
  finally
    cx.FreeRootedObject(obj);
  end;
end;

end.
