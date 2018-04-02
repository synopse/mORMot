/// `util` module support bindings for SyNode
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SyNodeBinding_const;

interface
{$I Synopse.inc}
{$I SyNode.inc}
uses
  SysUtils,
  SynCommons,
  SyNode, SpiderMonkey;

implementation

uses
  {$IFDEF MSWINDOWS}
  Windows
  {$ELSE}
  BaseUnix
  {$ENDIF}
  ;

{$IFDEF UNIX}
const
  O_DSYNC        = &0010000;
  O_NOATIME      = &1000000;

  // fs open() flags supported on other platforms
  O_RANDOM       = 0;
  O_SHORT_LIVED  = 0;
  O_SEQUENTIAL   = 0;
  O_TEMPORARY    = 0;

  O_EXLOCK       = 0;
  O_SYMLINK      = 0;
{$ENDIF}
{$IFDEF MSWINDOWS}
const
  F_OK = 0;
  R_OK = 4;
  W_OK = 2;
  X_OK = 1;

  // fs open() flags supported on Windows:
  O_APPEND      = $0008;
  O_CREAT       = $0100;
  O_EXCL        = $0400;
  O_RANDOM      = $0010;
  O_RDONLY      = 0;
  O_RDWR        = 2;
  O_SEQUENTIAL  = $0020;
  O_SHORT_LIVED = $1000;
  O_TEMPORARY   = $0040;
  O_TRUNC       = $0200;
  O_WRONLY      = 1;

  // fs open() flags supported on other platforms (or mapped on Windows):
  O_DIRECT      = $02000000; // FILE_FLAG_NO_BUFFERING
  O_DIRECTORY   = 0;
  O_DSYNC       = $04000000; // FILE_FLAG_WRITE_THROUGH
  O_EXLOCK      = $10000000; // EXCLUSIVE SHARING MODE
  O_NOATIME     = 0;
  O_NOCTTY      = 0;
  O_NOFOLLOW    = 0;
  O_NONBLOCK    = 0;
  O_SYMLINK     = 0;
  O_SYNC        = $08000000; // FILE_FLAG_WRITE_THROUGH
{$ENDIF}

function SyNodeBindingProc_consts(const aEngine: TSMEngine; const bindingNamespaceName: SynUnicode): jsval;
var
  obj, obj_fs: PJSRootedObject;
  jsv: jsval;
  cx: PJSContext;
const
  attrs = JSPROP_ENUMERATE or JSPROP_READONLY or JSPROP_PERMANENT;
begin
  cx := aEngine.cx;
  obj := cx.NewRootedObject(cx.NewObject(nil));
  obj_fs := cx.NewRootedObject(cx.NewObject(nil));
  try
    jsv.asInteger := F_OK; obj_fs.ptr.DefineProperty(cx, 'F_OK', jsv, attrs);
    jsv.asInteger := R_OK; obj_fs.ptr.DefineProperty(cx, 'R_OK', jsv, attrs);
    jsv.asInteger := W_OK; obj_fs.ptr.DefineProperty(cx, 'W_OK', jsv, attrs);
    jsv.asInteger := X_OK; obj_fs.ptr.DefineProperty(cx, 'X_OK', jsv, attrs);

    jsv.asInteger := O_APPEND; obj_fs.ptr.DefineProperty(cx, 'O_APPEND', jsv, attrs);
    jsv.asInteger := O_CREAT; obj_fs.ptr.DefineProperty(cx, 'O_CREAT', jsv, attrs);
    jsv.asInteger := O_EXCL; obj_fs.ptr.DefineProperty(cx, 'O_EXCL', jsv, attrs);
    jsv.asInteger := O_RANDOM; obj_fs.ptr.DefineProperty(cx, 'O_RANDOM', jsv, attrs);
    jsv.asInteger := O_RDONLY; obj_fs.ptr.DefineProperty(cx, 'O_RDONLY', jsv, attrs);
    jsv.asInteger := O_RDWR; obj_fs.ptr.DefineProperty(cx, 'O_RDWR', jsv, attrs);
    jsv.asInteger := O_SEQUENTIAL; obj_fs.ptr.DefineProperty(cx, 'O_SEQUENTIAL', jsv, attrs);
    jsv.asInteger := O_SHORT_LIVED; obj_fs.ptr.DefineProperty(cx, 'O_SHORT_LIVED', jsv, attrs);
    jsv.asInteger := O_TEMPORARY; obj_fs.ptr.DefineProperty(cx, '', jsv, attrs);
    jsv.asInteger := O_TRUNC; obj_fs.ptr.DefineProperty(cx, 'O_TRUNC', jsv, attrs);
    jsv.asInteger := O_WRONLY; obj_fs.ptr.DefineProperty(cx, 'O_WRONLY', jsv, attrs);

    jsv.asInteger := O_DIRECT; obj_fs.ptr.DefineProperty(cx, 'O_DIRECT', jsv, attrs);
    jsv.asInteger := O_DIRECTORY; obj_fs.ptr.DefineProperty(cx, 'O_DIRECTORY', jsv, attrs);
    jsv.asInteger := O_DSYNC; obj_fs.ptr.DefineProperty(cx, 'O_DSYNC', jsv, attrs);
    jsv.asInteger := O_EXLOCK; obj_fs.ptr.DefineProperty(cx, 'O_EXLOCK', jsv, attrs);
    jsv.asInteger := O_NOATIME; obj_fs.ptr.DefineProperty(cx, 'O_NOATIME', jsv, attrs);
    jsv.asInteger := O_NOCTTY; obj_fs.ptr.DefineProperty(cx, 'O_NOCTTY', jsv, attrs);
    jsv.asInteger := O_NOFOLLOW; obj_fs.ptr.DefineProperty(cx, 'O_NOFOLLOW', jsv, attrs);
    jsv.asInteger := O_NONBLOCK; obj_fs.ptr.DefineProperty(cx, 'O_NONBLOCK', jsv, attrs);
    jsv.asInteger := O_SYMLINK; obj_fs.ptr.DefineProperty(cx, 'O_SYMLINK', jsv, attrs);
    jsv.asInteger := O_SYNC; obj_fs.ptr.DefineProperty(cx, 'O_SYNC', jsv, attrs);

// TODO define all other consts
//
//  NODE_DEFINE_CONSTANT(target, S_IFMT);
//  NODE_DEFINE_CONSTANT(target, S_IFREG);
//  NODE_DEFINE_CONSTANT(target, S_IFDIR);
//  NODE_DEFINE_CONSTANT(target, S_IFCHR);
//#ifdef S_IFBLK
//  NODE_DEFINE_CONSTANT(target, S_IFBLK);
//#endif
//
//#ifdef S_IFIFO
//  NODE_DEFINE_CONSTANT(target, S_IFIFO);
//#endif
//
//#ifdef S_IFLNK
//  NODE_DEFINE_CONSTANT(target, S_IFLNK);
//#endif
//
//#ifdef S_IFSOCK
//  NODE_DEFINE_CONSTANT(target, S_IFSOCK);
//#endif
//
//#ifdef S_IRWXU
//  NODE_DEFINE_CONSTANT(target, S_IRWXU);
//#endif
//
//#ifdef S_IRUSR
//  NODE_DEFINE_CONSTANT(target, S_IRUSR);
//#endif
//
//#ifdef S_IWUSR
//  NODE_DEFINE_CONSTANT(target, S_IWUSR);
//#endif
//
//#ifdef S_IXUSR
//  NODE_DEFINE_CONSTANT(target, S_IXUSR);
//#endif
//
//#ifdef S_IRWXG
//  NODE_DEFINE_CONSTANT(target, S_IRWXG);
//#endif
//
//#ifdef S_IRGRP
//  NODE_DEFINE_CONSTANT(target, S_IRGRP);
//#endif
//
//#ifdef S_IWGRP
//  NODE_DEFINE_CONSTANT(target, S_IWGRP);
//#endif
//
//#ifdef S_IXGRP
//  NODE_DEFINE_CONSTANT(target, S_IXGRP);
//#endif
//
//#ifdef S_IRWXO
//  NODE_DEFINE_CONSTANT(target, S_IRWXO);
//#endif
//
//#ifdef S_IROTH
//  NODE_DEFINE_CONSTANT(target, S_IROTH);
//#endif
//
//#ifdef S_IWOTH
//  NODE_DEFINE_CONSTANT(target, S_IWOTH);
//#endif
//
//#ifdef S_IXOTH
//  NODE_DEFINE_CONSTANT(target, S_IXOTH);
//#endif
//
    obj.ptr.DefineProperty(cx, 'fs', obj_fs.ptr.ToJSValue, attrs);
    Result := obj.ptr.ToJSValue;
  finally
    cx.FreeRootedObject(obj_fs);
    cx.FreeRootedObject(obj);
  end;
end;

initialization
  TSMEngineManager.RegisterBinding('constants', SyNodeBindingProc_consts);

end.
