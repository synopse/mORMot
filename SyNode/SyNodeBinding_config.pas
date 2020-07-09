unit SyNodeBinding_config;

interface
{$I Synopse.inc}
{$I SyNode.inc}
uses
  SysUtils, SynOpenSSLWrap, SynCommons, SpiderMonkey;

implementation

uses
  SyNode;

function SyNodeBindingProc_config(const aEngine: TSMEngine; const bindingNamespaceName: SynUnicode): jsval;
var
  obj: PJSRootedObject;
  cx: PJSContext;
begin
  cx := aEngine.cx;
  obj := cx.NewRootedObject(cx.NewObject(nil));
  try
    obj.ptr.DefineProperty(cx, 'fipsMode', jsval.FalseValue, JSPROP_READONLY or JSPROP_PERMANENT);
    obj.ptr.DefineProperty(cx, 'fipsForced', jsval.FalseValue, JSPROP_READONLY or JSPROP_PERMANENT);
    obj.ptr.DefineProperty(cx, 'preserveSymlinks', jsval.BooleanValue(FindCmdLineSwitch('-preserve-symlinks')), JSPROP_READONLY or JSPROP_PERMANENT);

    Result := obj.ptr.ToJSValue;
  finally
    cx.FreeRootedObject(obj);
  end;
end;


initialization
  TSMEngineManager.RegisterBinding('config', SyNodeBindingProc_config);

end.

