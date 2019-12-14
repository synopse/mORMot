#include "SynSM.h"

#include <jsfriendapi.h>
#include <js/Date.h>
#include <js/Initialization.h>
#include <mozilla/Assertions.h>

// Declared in vm/Runtime.h
namespace js {
    extern bool gCanUseExtraThreads;
}

JS_PUBLIC_API(bool) SM_Initialize(void)
{
    return JS_Init();
}

JS_PUBLIC_API(void) SM_DisableExtraThreads(void)
{
    js::gCanUseExtraThreads = false;
}

JS_PUBLIC_API(void) SM_ShutDown(void)
{
    JS_ShutDown();
}

JS_PUBLIC_API(int64_t) SM_Now(void)
{
    return JS_Now();
}

JS_PUBLIC_API(JSString*) SM_GetEmptyString(JSContext* cx)
{
    return JS_GetEmptyString(cx);
}

JS_PUBLIC_API(JSType) SM_TypeOfValue(JSContext* cx, JS::Handle<JS::Value> v)
{
    return JS_TypeOfValue(cx, v);
}

JS_PUBLIC_API(void) SM_BeginRequest(JSContext* cx)
{
    return JS_BeginRequest(cx);
}

JS_PUBLIC_API(void) SM_EndRequest(JSContext* cx)
{
    return JS_EndRequest(cx);
}

JS_PUBLIC_API(JSContext*) SM_NewContext(uint32_t maxbytes, uint32_t maxNurseryBytes, JSContext* parentContext)
{
    return JS_NewContext(maxbytes, maxNurseryBytes, parentContext);
}

JS_PUBLIC_API(bool) SM_InitSelfHostedCode(JSContext* cx)
{
    return JS::InitSelfHostedCode(cx);
}

JS_PUBLIC_API(void) SM_DestroyContext(JSContext* cx)
{
    JS_DestroyContext(cx);
}

JS_PUBLIC_API(void*) SM_GetContextPrivate(JSContext* cx)
{
    return JS_GetContextPrivate(cx);
}

JS_PUBLIC_API(void) SM_SetContextPrivate(JSContext* cx, void* data)
{
    JS_SetContextPrivate(cx, data);
}

JS_PUBLIC_API(bool) SM_WrapObject(JSContext* cx, JS::MutableHandleObject objp)
{
    return JS_WrapObject(cx, objp);
}

JS_PUBLIC_API(JSCompartment*) SM_EnterCompartment(JSContext* cx, JSObject* target)
{
    return JS_EnterCompartment(cx, target);
}

JS_PUBLIC_API(void) SM_LeaveCompartment(JSContext* cx, JSCompartment* oldCompartment)
{
    JS_LeaveCompartment(cx, oldCompartment);
}

JS_PUBLIC_API(bool) SM_InitStandardClasses(JSContext* cx, JS::Handle<JSObject*> obj)
{
    return JS_InitStandardClasses(cx, obj);
}

JS_PUBLIC_API(JSObject*) SM_CurrentGlobalOrNull(JSContext* cx)
{
    return JS::CurrentGlobalOrNull(cx);
}

JS_PUBLIC_API(bool) SM_InitReflectParse(JSContext* cx, JS::HandleObject global)
{
    return JS_InitReflectParse(cx, global);
}

JS_PUBLIC_API(bool) SM_InitCTypesClass(JSContext* cx, JS::HandleObject global)
{
    return JS_InitCTypesClass(cx, global);
}

JS_PUBLIC_API(bool) SM_DefineDebuggerObject(JSContext* cx, JS::HandleObject obj)
{
    return JS_DefineDebuggerObject(cx, obj);
}

JS_PUBLIC_API(void) SM_GC(JSContext* cx)
{
    JS_GC(cx);
}

JS_PUBLIC_API(void) SM_MaybeGC(JSContext* cx)
{
    JS_MaybeGC(cx);
}

JS_PUBLIC_API(void) SM_SetGCParameter(JSContext* cx, JSGCParamKey key, uint32_t value)
{
    JS_SetGCParameter(cx, key, value);
}

JS_PUBLIC_API(uint32_t) SM_GetGCParameter(JSContext* cx, JSGCParamKey key)
{
    return JS_GetGCParameter(cx, key);
}

JS_PUBLIC_API(void) SM_SetGCParametersBasedOnAvailableMemory(JSContext* cx, uint32_t availMem)
{
    JS_SetGCParametersBasedOnAvailableMemory(cx, availMem);
}

JS_PUBLIC_API(JSString*) SM_NewExternalString(
    JSContext* cx, const char16_t* chars, size_t length, const JSStringFinalizer* fin)
{
    return JS_NewExternalString(cx, chars, length, fin);
}

JS_PUBLIC_API(void) SM_SetNativeStackQuota(
    JSContext* cx, size_t systemCodeStackSize, size_t trustedScriptStackSize, size_t untrustedScriptStackSize)
{
    JS_SetNativeStackQuota(cx, systemCodeStackSize, trustedScriptStackSize, untrustedScriptStackSize);
}

JS_PUBLIC_API(bool) SM_ValueToId(JSContext* cx, JS::HandleValue v, JS::MutableHandleId idp)
{
    return JS_ValueToId(cx, v, idp);
}

JS_PUBLIC_API(bool) SM_IdToValue(JSContext* cx, jsid id, JS::MutableHandle<JS::Value> vp)
{
    return JS_IdToValue(cx, id, vp);
}

JS_PUBLIC_API(JSString*) SM_ValueToSource(JSContext* cx, JS::Handle<JS::Value> v)
{
    return JS_ValueToSource(cx, v);
}

JS_PUBLIC_API(JSObject*) SM_InitClass(
    JSContext* cx, JS::HandleObject obj, JS::HandleObject parent_proto,
    const JSClass* clasp, JSNative constructor, unsigned nargs,
    const JSPropertySpec* ps, const JSFunctionSpec* fs,
    const JSPropertySpec* static_ps, const JSFunctionSpec* static_fs)
{
    return JS_InitClass(cx, obj, parent_proto, clasp, constructor, nargs, ps, fs, static_ps, static_fs);
}

JS_PUBLIC_API(const JSClass*) SM_GetClass(JSObject* obj)
{
    return JS_GetClass(obj);
}

JS_PUBLIC_API(bool) SM_HasInstance(
    JSContext* cx, JS::Handle<JSObject*> obj, JS::Handle<JS::Value> v, bool* bp)
{
    return JS_HasInstance(cx, obj, v, bp);
}

JS_PUBLIC_API(void*) SM_GetPrivate(JSObject* obj)
{
    return JS_GetPrivate(obj);
}

JS_PUBLIC_API(void) SM_SetPrivate(JSObject* obj, void* data)
{
    return JS_SetPrivate(obj, data);
}

JS_PUBLIC_API(JSObject*) SM_GetConstructor(JSContext* cx, JS::Handle<JSObject*> proto)
{
    return JS_GetConstructor(cx, proto);
}

JS_PUBLIC_API(void*) SM_GetInstancePrivate(
    JSContext* cx, JS::Handle<JSObject*> obj, const JSClass* clasp, JS::CallArgs* args)
{
    return JS_GetInstancePrivate(cx, obj, clasp, args);
}

JS_PUBLIC_API(JSObject*) SM_NewGlobalObject(
    JSContext* cx, const JSClass* clasp, JSPrincipals* principals,
    JS::OnNewGlobalHookOption hookOption, const JS::CompartmentOptions& options)
{
    return JS_NewGlobalObject(cx, clasp, principals, hookOption, options);
}

JS_PUBLIC_API(void) SM_GlobalObjectTraceHook(JSTracer* trc, JSObject* global)
{
    JS_GlobalObjectTraceHook(trc, global);
}

JS_PUBLIC_API(JSObject*) SM_NewObject(JSContext* cx, const JSClass* clasp)
{
    return JS_NewObject(cx, clasp);
}

JS_PUBLIC_API(JSObject*) SM_NewObjectWithGivenProto(JSContext* cx, const JSClass* clasp, JS::Handle<JSObject*> proto)
{
    return JS_NewObjectWithGivenProto(cx, clasp, proto);
}

JS_PUBLIC_API(bool) SM_GetPrototype(JSContext* cx, JS::HandleObject obj, JS::MutableHandleObject result)
{
    return JS_GetPrototype(cx, obj, result);
}

JS_PUBLIC_API(bool) SM_SetPrototype(JSContext* cx, JS::HandleObject obj, JS::HandleObject proto)
{
    return JS_SetPrototype(cx, obj, proto);
}

JS_PUBLIC_API(bool) SM_DefinePropertyById(
    JSContext* cx, JS::HandleObject obj, JS::HandleId id, JS::HandleValue value,
    unsigned attrs, JSNative getter, JSNative setter)
{
    return JS_DefinePropertyById(cx, obj, id, value, attrs, getter, setter);
}

JS_PUBLIC_API(bool) SM_DefineProperty(
    JSContext* cx, JS::HandleObject obj, const char* name, JS::HandleValue value,
    unsigned attrs, JSNative getter, JSNative setter)
{
    return JS_DefineProperty(cx, obj, name, value, attrs, getter, setter);
}

JS_PUBLIC_API(bool) SM_DefineUCProperty(
    JSContext* cx, JS::HandleObject obj, const char16_t* name, size_t namelen,
    JS::HandleValue value, unsigned attrs, JSNative getter, JSNative setter)
{
    return JS_DefineUCProperty(cx, obj, name, namelen, value, attrs, getter, setter);
}

JS_PUBLIC_API(bool) SM_HasProperty(JSContext* cx, JS::HandleObject obj, const char* name, bool* foundp)
{
    return JS_HasProperty(cx, obj, name, foundp);
}

JS_PUBLIC_API(bool) SM_HasUCProperty(JSContext* cx, JS::HandleObject obj, const char16_t* name, size_t namelen, bool* vp)
{
    return JS_HasUCProperty(cx, obj, name, namelen, vp);
}

JS_PUBLIC_API(bool) SM_GetPropertyById(
    JSContext* cx, JS::HandleObject obj, JS::HandleId id, JS::MutableHandleValue vp)
{
    return JS_GetPropertyById(cx, obj, id, vp);
}

JS_PUBLIC_API(bool) SM_GetProperty(
    JSContext* cx, JS::HandleObject obj, const char* name, JS::MutableHandleValue vp)
{
    return JS_GetProperty(cx, obj, name, vp);
}

JS_PUBLIC_API(bool) SM_GetUCProperty(
    JSContext* cx, JS::HandleObject obj, const char16_t* name, size_t namelen, JS::MutableHandleValue vp)
{
    return JS_GetUCProperty(cx, obj, name, namelen, vp);
}

JS_PUBLIC_API(bool) SM_GetElement(
    JSContext* cx, JS::HandleObject obj, uint32_t index, JS::MutableHandleValue vp)
{
    return JS_GetElement(cx, obj, index, vp);
}

JS_PUBLIC_API(bool) SM_SetProperty(JSContext* cx, JS::HandleObject obj, const char* name, JS::HandleValue v)
{
    return JS_SetProperty(cx, obj, name, v);
}

JS_PUBLIC_API(bool) SM_SetUCProperty(
    JSContext* cx, JS::HandleObject obj, const char16_t* name, size_t namelen, JS::HandleValue v)
{
    return JS_SetUCProperty(cx, obj, name, namelen, v);
}

JS_PUBLIC_API(bool) SM_SetElement(JSContext* cx, JS::HandleObject obj, uint32_t index, JS::HandleValue v)
{
    return JS_SetElement(cx, obj, index, v);
}

JS_PUBLIC_API(bool) SM_DeletePropertyById(
    JSContext* cx, JS::HandleObject obj, JS::HandleId id, JS::ObjectOpResult& result)
{
    return JS_DeletePropertyById(cx, obj, id, result);
}

JS_PUBLIC_API(bool) SM_DeleteElement(
    JSContext* cx, JS::HandleObject obj, uint32_t index, JS::ObjectOpResult& result)
{
    return JS_DeleteElement(cx, obj, index, result);
}

JS_PUBLIC_API(JS::AutoIdVector*) SM_EnumerateToAutoIdVector(
    JSContext* cx, JS::HandleObject obj, size_t* length, jsid** data)
{
    // TODO:
    // js::AssertHeapIsIdle(cx);
    CHECK_REQUEST(cx);
    // assertSameCompartment(cx, obj);

    JS::AutoIdVector* v = new JS::AutoIdVector(cx);
    if (!GetPropertyKeys(cx, obj, JSITER_OWNONLY, v)) {
        delete v;
        *length = 0;
        *data = nullptr;
        return nullptr;
    }

    *length = v->length();
    *data = v->begin();
    return v;
}

JS_PUBLIC_API(void) SM_DestroyAutoIdVector(JS::AutoIdVector* v)
{
    delete v;
}

JS_PUBLIC_API(bool) SM_CallFunctionValue(
    JSContext* cx, JS::HandleObject obj, JS::HandleValue fval,
    const JS::HandleValueArray& args, JS::MutableHandleValue rval)
{
    return JS_CallFunctionValue(cx, obj,fval,args, rval);
}

JS_PUBLIC_API(bool) SM_CallFunction(
    JSContext* cx, JS::HandleObject obj, JS::HandleFunction fun,
    const JS::HandleValueArray& args, JS::MutableHandleValue rval)
{
    return JS_CallFunction(cx, obj, fun, args, rval);
}

JS_PUBLIC_API(bool) SM_CallFunctionName(
    JSContext* cx, JS::HandleObject obj, const char* name,
    const JS::HandleValueArray& args, JS::MutableHandleValue rval)
{
    return JS_CallFunctionName(cx, obj, name, args, rval);
}

JS_PUBLIC_API(JSObject*) SM_New(
    JSContext* cx, JS::HandleObject ctor, const JS::HandleValueArray& args)
{
    return JS_New(cx, ctor, args);
}

JS_PUBLIC_API(bool) SM_DefineProperties(
    JSContext* cx, JS::HandleObject obj, const JSPropertySpec* ps)
{
    return JS_DefineProperties(cx, obj, ps);
}

JS_PUBLIC_API(bool) SM_AlreadyHasOwnUCProperty(
    JSContext* cx, JS::HandleObject obj, const char16_t* name,
    size_t namelen, bool* foundp)
{
    return JS_AlreadyHasOwnUCProperty(cx, obj, name, namelen, foundp);
}

JS_PUBLIC_API(JSObject*) SM_NewArrayObject(JSContext* cx, size_t length)
{
    return JS_NewArrayObject(cx, length);
}

JS_PUBLIC_API(JSObject*) SM_NewArrayObject2(JSContext* cx, const JS::HandleValueArray& contents)
{
    return JS_NewArrayObject(cx, contents);
}

JS_PUBLIC_API(bool) SM_IsArrayObject(JSContext* cx, JS::HandleObject obj, bool* isArray)
{
    return JS_IsArrayObject(cx, obj, isArray);
}

JS_PUBLIC_API(bool) SM_GetArrayLength(JSContext* cx, JS::Handle<JSObject*> obj, uint32_t* lengthp)
{
    return JS_GetArrayLength(cx, obj, lengthp);
}

JS_PUBLIC_API(uint64_t) SM_GetReservedSlot(JSObject* obj, uint32_t index)
{
    return js::GetReservedSlot(obj, index).asRawBits();
}

JS_PUBLIC_API(void) SM_SetReservedSlot(
    JSObject* obj, uint32_t index, const JS::Value& v)
{
    JS_SetReservedSlot(obj, index, v);
}

JS_PUBLIC_API(JSFunction*) SM_NewFunction(
    JSContext* cx, JSNative call, unsigned nargs, unsigned flags, const char* name)
{
    return JS_NewFunction(cx, call, nargs, flags, name);
}

JS_PUBLIC_API(JSString*) SM_GetFunctionId(JSFunction* fun)
{
    return JS_GetFunctionId(fun);
}

JS_PUBLIC_API(bool) SM_ObjectIsFunction(JSContext* cx, JSObject* obj)
{
    return JS_ObjectIsFunction(cx, obj);
}

JS_PUBLIC_API(bool) SM_DefineFunctions(
    JSContext* cx, JS::Handle<JSObject*> obj, const JSFunctionSpec* fs)
{
    return JS_DefineFunctions(cx, obj, fs);
}

JS_PUBLIC_API(JSFunction*) SM_DefineFunction(
    JSContext* cx, JS::Handle<JSObject*> obj, const char* name, JSNative call,
    unsigned nargs, unsigned attrs)
{
    return JS_DefineFunction(cx, obj, name, call, nargs, attrs);
}

JS_PUBLIC_API(JSFunction*) SM_DefineUCFunction(
    JSContext* cx, JS::Handle<JSObject*> obj,
    const char16_t* name, size_t namelen, JSNative call,
    unsigned nargs, unsigned attrs)
{
    return JS_DefineUCFunction(cx, obj, name, namelen, call, nargs, attrs);
}

JS_PUBLIC_API(bool) SM_CompileScript(
    JSContext* cx, const char* ascii, size_t length,
    const JS::CompileOptions& options, JS::MutableHandleScript script)
{
    return JS_CompileScript(cx, ascii, length, options, script);
}

JS_PUBLIC_API(bool) SM_CompileUCScript(
    JSContext* cx, const char16_t* chars, size_t length,
    const JS::CompileOptions& options, JS::MutableHandleScript script)
{
    return JS_CompileUCScript(cx, chars, length, options, script);
}

JS_PUBLIC_API(JSString*) SM_DecompileFunction(
    JSContext* cx, JS::Handle<JSFunction*> fun, unsigned indent)
{
    return JS_DecompileFunction(cx, fun, indent);
}

JS_PUBLIC_API(bool) SM_ExecuteScript(
    JSContext* cx, JS::HandleScript script, JS::MutableHandleValue rval)
{
    return JS_ExecuteScript(cx, script, rval);
}

JS_PUBLIC_API(bool) SM_CheckForInterrupt(JSContext* cx)
{
    return JS_CheckForInterrupt(cx);
}

JS_PUBLIC_API(bool) SM_AddInterruptCallback(JSContext* cx, JSInterruptCallback callback)
{
    return JS_AddInterruptCallback(cx, callback);
}

JS_PUBLIC_API(bool) SM_DisableInterruptCallback(JSContext* cx)
{
    return JS_DisableInterruptCallback(cx);
}

JS_PUBLIC_API(void) SM_ResetInterruptCallback(JSContext* cx, bool enable)
{
    JS_ResetInterruptCallback(cx, enable);
}

JS_PUBLIC_API(void) SM_RequestInterruptCallback(JSContext* cx)
{
    JS_RequestInterruptCallback(cx);
}

JS_PUBLIC_API(bool) SM_IsRunning(JSContext* cx)
{
    return JS_IsRunning(cx);
}

JS_PUBLIC_API(JSString*) SM_NewStringCopyN(JSContext* cx, const char* s, size_t n)
{
    return JS_NewStringCopyN(cx, s, n);
}

JS_PUBLIC_API(JSString*) SM_NewStringCopyUTF8Z(JSContext* cx, const JS::ConstUTF8CharsZ s)
{
    return JS_NewStringCopyUTF8Z(cx, s);
}

JS_PUBLIC_API(JS::Value) SM_GetEmptyStringValue(JSContext* cx)
{
    return JS_GetEmptyStringValue(cx);
}

JS_PUBLIC_API(JSString*) SM_NewUCStringCopyN(JSContext* cx, const char16_t* s, size_t n)
{
    return JS_NewUCStringCopyN(cx, s,n);
}

JS_PUBLIC_API(size_t) SM_GetStringLength(JSString* str)
{
    return JS_GetStringLength(str);
}

JS_PUBLIC_API(bool) SM_StringHasLatin1Chars(JSString* str)
{
    return JS_StringHasLatin1Chars(str);
}

JS_PUBLIC_API(const JS::Latin1Char*) SM_GetLatin1StringCharsAndLength(
    JSContext* cx, const JS::AutoCheckCannotGC& nogc, JSString* str, size_t* length)
{
    return JS_GetLatin1StringCharsAndLength(cx, nogc, str, length);
}

JS_PUBLIC_API(const char16_t*) SM_GetTwoByteStringCharsAndLength(
    JSContext* cx, const JS::AutoCheckCannotGC& nogc, JSString* str, size_t* length)
{
    return JS_GetTwoByteStringCharsAndLength(cx, nogc, str, length);
}

JS_PUBLIC_API(bool) SM_Stringify(
    JSContext* cx, JS::MutableHandleValue value, JS::HandleObject replacer,
    JS::HandleValue space, JSONWriteCallback callback, void* data)
{
    return JS_Stringify(cx, value, replacer, space, callback, data);
}

JS_PUBLIC_API(bool) SM_ParseJSON(
    JSContext* cx, const char16_t* chars, uint32_t len, JS::MutableHandleValue vp)
{
    return JS_ParseJSON(cx, chars, len, vp);
}

JS_PUBLIC_API(void) SM_ReportErrorASCII(JSContext* cx, const char* format, ...)
{
//    va_list ap;
//    va_start(ap, errorNumber);
//    JS_ReportErrorNumberASCIIVA(cx, errorCallback, userRef, errorNumber, ap);
//    va_end(ap);
}

JS_PUBLIC_API(void) SM_ReportErrorNumberUC(
    JSContext* cx, JSErrorCallback errorCallback, void* userRef, const unsigned errorNumber, ...)
{
//    va_list ap;
//    va_start(ap, errorNumber);
//    JS_ReportErrorNumberASCIIVA(cx, errorCallback, userRef, errorNumber, ap);
//    va_end(ap);
}

JS_PUBLIC_API(void) SM_ReportErrorNumberUTF8(
    JSContext* cx, JSErrorCallback errorCallback, void* userRef, const unsigned errorNumber, ...)
{
    va_list ap;
    va_start(ap, errorNumber);
    JS_ReportErrorNumberUTF8VA(cx, errorCallback, userRef, errorNumber, ap);
    va_end(ap);
}

JS_PUBLIC_API(void) SM_ReportOutOfMemory(JSContext* cx)
{
    JS_ReportOutOfMemory(cx);
}

JS_PUBLIC_API(JS::WarningReporter) SM_GetWarningReporter(JSContext* cx)
{
    return JS::GetWarningReporter(cx);
}

JS_PUBLIC_API(JS::WarningReporter) SM_SetWarningReporter(JSContext* cx, JS::WarningReporter reporter)
{
    return JS::SetWarningReporter(cx, reporter);
}

JS_PUBLIC_API(JSObject*) SM_NewDateObject(
    JSContext* cx, int year, int mon, int mday, int hour, int min, int sec)
{
    return JS_NewDateObject(cx, year, mon, mday, hour, min, sec);
}

JS_PUBLIC_API(JSObject*) SM_NewDateObjectMsec(JSContext* cx, double msec)
{
    JS::ClippedTime time = JS::TimeClip(msec);
    return JS::NewDateObject(cx, time);
}

JS_PUBLIC_API(bool) SM_ObjectIsDate(JSContext* cx, JS::HandleObject obj, bool* isDate)
{
    return JS_ObjectIsDate(cx, obj, isDate);
}

JS_PUBLIC_API(bool) SM_IsExceptionPending(JSContext* cx)
{
    return JS_IsExceptionPending(cx);
}

JS_PUBLIC_API(bool) SM_GetPendingException(JSContext* cx, JS::MutableHandleValue vp)
{
    return JS_GetPendingException(cx, vp);
}

JS_PUBLIC_API(void) SM_SetPendingException(JSContext* cx, JS::HandleValue v)
{
    JS_SetPendingException(cx, v);
}

JS_PUBLIC_API(void) SM_ClearPendingException(JSContext* cx)
{
    JS_ClearPendingException(cx);
}

JS_PUBLIC_API(JSErrorReport*) SM_ErrorFromException(JSContext* cx, JS::HandleObject obj)
{
    return JS_ErrorFromException(cx, obj);
}

JS_PUBLIC_API(void*) SM_GetContextOptions(JSContext* cx)
{
    return &JS::ContextOptionsRef(cx);
}

JS_PUBLIC_API(void*) SM_NewRootedValue(JSContext* cx, uint64_t initial)
{
    return js_new<JS::RootedValue>(cx, JS::Value::fromRawBits(initial));
}

JS_PUBLIC_API(void) SM_FreeRootedValue(void* val)
{
    js_delete(reinterpret_cast<JS::RootedValue*>(val));
}

JS_PUBLIC_API(void*) SM_NewRootedObject(JSContext* cx, JSObject* initial)
{
    return js_new<JS::RootedObject>(cx, initial);
}

JS_PUBLIC_API(void) SM_FreeRootedObject(void* obj)
{
    js_delete(reinterpret_cast<JS::RootedObject*>(obj));
}

JS_PUBLIC_API(void*) SM_NewRootedString(JSContext* cx, JSString* initial)
{
    return js_new<JS::RootedString>(cx, initial);
}

JS_PUBLIC_API(void) SM_FreeRootedString(void* str)
{
    js_delete(reinterpret_cast<JS::RootedString*>(str));
}

JS_PUBLIC_API(void*) SM_NewCompileOptions(JSContext* cx)
{
    return js_new<JS::CompileOptions>(cx);
}

JS_PUBLIC_API(void) SM_SetCompileOptionsFileLineAndUtf8(JS::CompileOptions& options, const char* f, unsigned l, bool isUtf8)
{
    options.setFileAndLine(f, l);
    options.setUTF8(isUtf8);
}

JS_PUBLIC_API(void) SM_FreeCompileOptions(void* co)
{
    js_delete(reinterpret_cast<JS::CompileOptions*>(co));
}

JS_PUBLIC_API(bool) SM_EvaluateScript(
    JSContext* cx, const JS::CompileOptions& options,
    const char* bytes, size_t length, JS::MutableHandleValue rval)
{
    return JS::Evaluate(cx, options, bytes, length, rval);
}

JS_PUBLIC_API(bool) SM_EvaluateUCScript(
    JSContext* cx, const JS::CompileOptions& options,
    const char16_t* chars, size_t length, JS::MutableHandleValue rval)
{
    return JS::Evaluate(cx, options, chars, length, rval);
}

JS_PUBLIC_API(JS::Value) SM_ComputeThis(JSContext* cx, JS::Value* vp)
{
    return JS::detail::ComputeThis(cx, vp);
}

JS_FRIEND_API(JSObject*) SM_NewInt8Array(JSContext* cx, uint32_t nelements)
{
    return JS_NewInt8Array(cx, nelements);
}

JS_FRIEND_API(JSObject*) SM_NewUint8Array(JSContext* cx, uint32_t nelements)
{
    return JS_NewUint8Array(cx, nelements);
}

JS_FRIEND_API(JSObject*) SM_NewUint8ClampedArray(JSContext* cx, uint32_t nelements)
{
    return JS_NewUint8ClampedArray(cx, nelements);
}

JS_FRIEND_API(JSObject*) SM_NewInt16Array(JSContext* cx, uint32_t nelements)
{
    return JS_NewInt16Array(cx, nelements);
}

JS_FRIEND_API(JSObject*) SM_NewUint16Array(JSContext* cx, uint32_t nelements)
{
    return JS_NewUint16Array(cx, nelements);
}

JS_FRIEND_API(JSObject*) SM_NewInt32Array(JSContext* cx, uint32_t nelements)
{
    return JS_NewInt32Array(cx, nelements);
}

JS_FRIEND_API(JSObject*) SM_NewUint32Array(JSContext* cx, uint32_t nelements)
{
    return JS_NewUint32Array(cx, nelements);
}

JS_FRIEND_API(JSObject*) SM_NewFloat32Array(JSContext* cx, uint32_t nelements)
{
    return JS_NewFloat32Array(cx, nelements);
}

JS_FRIEND_API(JSObject*) SM_NewFloat64Array(JSContext* cx, uint32_t nelements)
{
    return JS_NewFloat64Array(cx, nelements);
}

JS_FRIEND_API(JSObject*) SM_NewInt8ArrayFromArray(JSContext* cx, JS::HandleObject array)
{
    return JS_NewInt8ArrayFromArray(cx, array);
}

JS_FRIEND_API(JSObject*) SM_NewUint8ArrayFromArray(JSContext* cx, JS::HandleObject array)
{
    return JS_NewUint8ArrayFromArray(cx, array);
}

JS_FRIEND_API(JSObject*) SM_NewUint8ClampedArrayFromArray(JSContext* cx, JS::HandleObject array)
{
    return JS_NewUint8ClampedArrayFromArray(cx, array);
}

JS_FRIEND_API(JSObject*) SM_NewInt16ArrayFromArray(JSContext* cx, JS::HandleObject array)
{
    return JS_NewInt16ArrayFromArray(cx, array);
}

JS_FRIEND_API(JSObject*) SM_NewUint16ArrayFromArray(JSContext* cx, JS::HandleObject array)
{
    return JS_NewUint16ArrayFromArray(cx, array);
}

JS_FRIEND_API(JSObject*) SM_NewInt32ArrayFromArray(JSContext* cx, JS::HandleObject array)
{
    return JS_NewInt32ArrayFromArray(cx, array);
}

JS_FRIEND_API(JSObject*) SM_NewUint32ArrayFromArray(JSContext* cx, JS::HandleObject array)
{
    return JS_NewUint32ArrayFromArray(cx, array);
}

JS_FRIEND_API(JSObject*) SM_NewFloat32ArrayFromArray(JSContext* cx, JS::HandleObject array)
{
    return JS_NewFloat32ArrayFromArray(cx, array);
}

JS_FRIEND_API(JSObject*) SM_NewFloat64ArrayFromArray(JSContext* cx, JS::HandleObject array)
{
    return JS_NewFloat64ArrayFromArray(cx, array);
}

JS_FRIEND_API(JSObject*) SM_NewInt8ArrayWithBuffer(
    JSContext* cx, JS::HandleObject arrayBuffer, uint32_t byteOffset, int32_t length)
{
    return JS_NewInt8ArrayWithBuffer(cx, arrayBuffer, byteOffset, length);
}

JS_FRIEND_API(JSObject*) SM_NewUint8ArrayWithBuffer(
    JSContext* cx, JS::HandleObject arrayBuffer, uint32_t byteOffset, int32_t length)
{
    return JS_NewUint8ArrayWithBuffer(cx, arrayBuffer, byteOffset, length);
}

JS_FRIEND_API(JSObject*) SM_NewUint8ClampedArrayWithBuffer(
    JSContext* cx, JS::HandleObject arrayBuffer, uint32_t byteOffset, int32_t length)
{
    return JS_NewUint8ClampedArrayWithBuffer(cx, arrayBuffer, byteOffset, length);
}

JS_FRIEND_API(JSObject*) SM_NewInt16ArrayWithBuffer(
    JSContext* cx, JS::HandleObject arrayBuffer, uint32_t byteOffset, int32_t length)
{
    return JS_NewInt16ArrayWithBuffer(cx, arrayBuffer, byteOffset, length);
}

JS_FRIEND_API(JSObject*) SM_NewUint16ArrayWithBuffer(
    JSContext* cx, JS::HandleObject arrayBuffer, uint32_t byteOffset, int32_t length)
{
    return JS_NewUint16ArrayWithBuffer(cx, arrayBuffer, byteOffset, length);
}

JS_FRIEND_API(JSObject*) SM_NewInt32ArrayWithBuffer(
    JSContext* cx, JS::HandleObject arrayBuffer, uint32_t byteOffset, int32_t length)
{
    return JS_NewInt32ArrayWithBuffer(cx, arrayBuffer, byteOffset, length);
}

JS_FRIEND_API(JSObject*) SM_NewUint32ArrayWithBuffer(
    JSContext* cx, JS::HandleObject arrayBuffer, uint32_t byteOffset, int32_t length)
{
    return JS_NewUint32ArrayWithBuffer(cx, arrayBuffer, byteOffset, length);
}

JS_FRIEND_API(JSObject*) SM_NewFloat32ArrayWithBuffer(
    JSContext* cx, JS::HandleObject arrayBuffer, uint32_t byteOffset, int32_t length)
{
    return JS_NewFloat32ArrayWithBuffer(cx, arrayBuffer, byteOffset, length);
}

JS_FRIEND_API(JSObject*) SM_NewFloat64ArrayWithBuffer(
    JSContext* cx, JS::HandleObject arrayBuffer, uint32_t byteOffset, int32_t length)
{
    return JS_NewFloat64ArrayWithBuffer(cx, arrayBuffer, byteOffset, length);
}

JS_FRIEND_API(JSObject*) SM_NewSharedArrayBuffer(JSContext* cx, uint32_t nbytes)
{
    return JS_NewSharedArrayBuffer(cx, nbytes);
}

JS_FRIEND_API(JSObject*) SM_NewArrayBuffer(JSContext* cx, uint32_t nbytes)
{
    return JS_NewArrayBuffer(cx, nbytes);
}

JS_FRIEND_API(bool) SM_IsTypedArrayObject(JSObject* obj)
{
    return JS_IsTypedArrayObject(obj);
}

JS_FRIEND_API(bool) SM_IsArrayBufferViewObject(JSObject* obj)
{
    return JS_IsArrayBufferViewObject(obj);
}

JS_FRIEND_API(bool) SM_IsInt8Array(JSObject* obj)
{
    return JS_IsInt8Array(obj);
}

JS_FRIEND_API(bool) SM_IsUint8Array(JSObject* obj)
{
    return JS_IsUint8Array(obj);
}

JS_FRIEND_API(bool) SM_IsUint8ClampedArray(JSObject* obj)
{
    return JS_IsUint8ClampedArray(obj);
}

JS_FRIEND_API(bool) SM_IsInt16Array(JSObject* obj)
{
    return JS_IsInt16Array(obj);
}

JS_FRIEND_API(bool) SM_IsUint16Array(JSObject* obj)
{
    return JS_IsUint16Array(obj);
}

JS_FRIEND_API(bool) SM_IsInt32Array(JSObject* obj)
{
    return JS_IsInt32Array(obj);
}

JS_FRIEND_API(bool) SM_IsUint32Array(JSObject* obj)
{
    return JS_IsUint32Array(obj);
}

JS_FRIEND_API(bool) SM_IsFloat32Array(JSObject* obj)
{
    return JS_IsFloat32Array(obj);
}

JS_FRIEND_API(bool) SM_IsFloat64Array(JSObject* obj)
{
    return JS_IsFloat64Array(obj);
}

JS_FRIEND_API(bool) SM_GetTypedArraySharedness(JSObject* obj)
{
    return JS_GetTypedArraySharedness(obj);
}

JS_FRIEND_API(JSObject*) SM_GetObjectAsInt8Array(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, int8_t** data)
{
    return JS_GetObjectAsInt8Array(obj, length, isSharedMemory, data);
}

JS_FRIEND_API(JSObject*) SM_GetObjectAsUint8Array(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, uint8_t** data)
{
    return JS_GetObjectAsUint8Array(obj, length, isSharedMemory, data);
}

JS_FRIEND_API(JSObject*) SM_GetObjectAsUint8ClampedArray(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, uint8_t** data)
{
    return JS_GetObjectAsUint8ClampedArray(obj, length, isSharedMemory, data);
}

JS_FRIEND_API(JSObject*) SM_GetObjectAsInt16Array(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, int16_t** data)
{
    return JS_GetObjectAsInt16Array(obj, length, isSharedMemory, data);
}

JS_FRIEND_API(JSObject*) SM_GetObjectAsUint16Array(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, uint16_t** data)
{
    return JS_GetObjectAsUint16Array(obj, length, isSharedMemory, data);
}

JS_FRIEND_API(JSObject*) SM_GetObjectAsInt32Array(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, int32_t** data)
{
    return JS_GetObjectAsInt32Array(obj, length, isSharedMemory, data);
}

JS_FRIEND_API(JSObject*) SM_GetObjectAsUint32Array(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, uint32_t** data)
{
    return JS_GetObjectAsUint32Array(obj, length, isSharedMemory, data);
}

JS_FRIEND_API(JSObject*) SM_GetObjectAsFloat32Array(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, float** data)
{
    return JS_GetObjectAsFloat32Array(obj, length, isSharedMemory, data);
}

JS_FRIEND_API(JSObject*) SM_GetObjectAsFloat64Array(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, double** data)
{
    return JS_GetObjectAsFloat64Array(obj, length, isSharedMemory, data);
}

JS_FRIEND_API(JSObject*) SM_GetObjectAsArrayBufferView(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, uint8_t** data)
{
    return JS_GetObjectAsArrayBufferView(obj, length, isSharedMemory, data);
}

JS_FRIEND_API(JSObject*) SM_GetObjectAsArrayBuffer(
    JSObject* obj, uint32_t* length, uint8_t** data)
{
    return JS_GetObjectAsArrayBuffer(obj, length, data);
}

JS_FRIEND_API(js::Scalar::Type) SM_GetArrayBufferViewType(JSObject* obj)
{
    return JS_GetArrayBufferViewType(obj);
}

JS_FRIEND_API(bool) SM_IsArrayBufferObject(JSObject* obj)
{
    return JS_IsArrayBufferObject(obj);
}

JS_FRIEND_API(bool) SM_IsSharedArrayBufferObject(JSObject* obj)
{
    return JS_IsSharedArrayBufferObject(obj);
}

JS_FRIEND_API(uint32_t) SM_GetArrayBufferByteLength(JSObject* obj)
{
    return JS_GetArrayBufferByteLength(obj);
}

JS_FRIEND_API(uint32_t) SM_GetSharedArrayBufferByteLength(JSObject* obj)
{
    return JS_GetSharedArrayBufferByteLength(obj);
}

JS_FRIEND_API(bool) SM_ArrayBufferHasData(JSObject* obj)
{
    return JS_ArrayBufferHasData(obj);
}

JS_FRIEND_API(uint8_t*) SM_GetArrayBufferData(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC& nogc)
{
    return JS_GetArrayBufferData(obj, isSharedMemory, nogc);
}

JS_FRIEND_API(bool) SM_IsMappedArrayBufferObject(JSObject* obj)
{
    return JS_IsMappedArrayBufferObject(obj);
}

JS_FRIEND_API(uint32_t) SM_GetTypedArrayLength(JSObject* obj)
{
    return JS_GetTypedArrayLength(obj);
}

JS_FRIEND_API(uint32_t) SM_GetTypedArrayByteOffset(JSObject* obj)
{
    return JS_GetTypedArrayByteOffset(obj);
}

JS_FRIEND_API(uint32_t) SM_GetTypedArrayByteLength(JSObject* obj)
{
    return JS_GetTypedArrayByteLength(obj);
}

JS_FRIEND_API(uint32_t) SM_GetArrayBufferViewByteLength(JSObject* obj)
{
    return JS_GetArrayBufferViewByteLength(obj);
}

JS_FRIEND_API(int8_t*) SM_GetInt8ArrayData(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC& nogc)
{
    return JS_GetInt8ArrayData(obj, isSharedMemory, nogc);
}

JS_FRIEND_API(uint8_t*) SM_GetUint8ArrayData(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC& nogc)
{
    return JS_GetUint8ArrayData(obj, isSharedMemory, nogc);
}

JS_FRIEND_API(uint8_t*) SM_GetUint8ClampedArrayData(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC& nogc)
{
    return JS_GetUint8ClampedArrayData(obj, isSharedMemory, nogc);
}

JS_FRIEND_API(int16_t*) SM_GetInt16ArrayData(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC& nogc)
{
    return JS_GetInt16ArrayData(obj, isSharedMemory, nogc);
}

JS_FRIEND_API(uint16_t*) SM_GetUint16ArrayData(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC& nogc)
{
    return JS_GetUint16ArrayData(obj, isSharedMemory, nogc);
}

JS_FRIEND_API(int32_t*) SM_GetInt32ArrayData(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC& nogc)
{
    return JS_GetInt32ArrayData(obj, isSharedMemory, nogc);
}

JS_FRIEND_API(uint32_t*) SM_GetUint32ArrayData(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC& nogc)
{
    return JS_GetUint32ArrayData(obj, isSharedMemory, nogc);
}

JS_FRIEND_API(float*) SM_GetFloat32ArrayData(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC& nogc)
{
    return JS_GetFloat32ArrayData(obj, isSharedMemory, nogc);
}

JS_FRIEND_API(double*) SM_GetFloat64ArrayData(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC& nogc)
{
    return JS_GetFloat64ArrayData(obj, isSharedMemory, nogc);
}

JS_FRIEND_API(void*) SM_GetArrayBufferViewData(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC& nogc)
{
    return JS_GetArrayBufferViewData(obj, isSharedMemory, nogc);
}

JS_FRIEND_API(JSObject*) SM_GetArrayBufferViewBuffer(
    JSContext* cx, JS::HandleObject obj, bool* isSharedMemory)
{
    return JS_GetArrayBufferViewBuffer(cx, obj, isSharedMemory);
}

JS_PUBLIC_API(bool) SM_InitModuleClasses(JSContext* cx, JS::HandleObject obj)
{
    // Intentionally commented out
    //return js::InitModuleClasses(cx, obj);
    return true;
}

JS_PUBLIC_API(JSObject*) SM_CompileModule(JSContext* cx, JS::HandleObject obj, JS::CompileOptions& options,
    const char16_t* chars, size_t length)
{
    JS::SourceBufferHolder srcBuf(chars, length, JS::SourceBufferHolder::NoOwnership);
    JS::Rooted<JSObject*> module(cx);
    return JS::CompileModule(cx, options, srcBuf, &module) ? module.get() : nullptr;
}

JS_PUBLIC_API(void) SM_SetModuleResolveHook(JSContext* cx, JS::HandleFunction hook)
{
    JS::SetModuleResolveHook(cx, hook);
}

JS_PUBLIC_API(void*) SM_NewCompartmentOptions()
{
    return js_new<JS::CompartmentOptions>();
}

JS_PUBLIC_API(void) SM_FreeCompartmentOptions(void* opt)
{
    js_delete(reinterpret_cast<JS::CompartmentOptions*>(opt));
}




/*
JS_PUBLIC_API(JS::Value) SM_GetNaNValue(JSContext* cx)
{
    return JS_GetNaNValue(cx);
}

JS_PUBLIC_API(JS::Value) SM_GetNegativeInfinityValue(JSContext* cx)
{
    return JS_GetNegativeInfinityValue(cx);
}

JS_PUBLIC_API(JS::Value) SM_GetPositiveInfinityValue(JSContext* cx)
{
    return JS_GetPositiveInfinityValue(cx);
}

JS_PUBLIC_API(bool) SM_ValueToObject(JSContext* cx, JS::HandleValue v, JS::MutableHandleObject objp)
{
    return JS_ValueToObject(cx, v, objp);
}

JS_PUBLIC_API(JSFunction*) SM_ValueToFunction(JSContext* cx, JS::HandleValue v)
{
    return JS_ValueToFunction(cx, v);
}

JS_PUBLIC_API(JSFunction*) SM_ValueToConstructor(JSContext* cx, JS::HandleValue v)
{
    return JS_ValueToConstructor(cx, v);
}

JS_PUBLIC_API(bool) SM_StrictlyEqual(JSContext* cx, JS::Handle<JS::Value> v1, JS::Handle<JS::Value> v2, bool* equal)
{
    return JS_StrictlyEqual(cx, v1, v2, equal);
}

JS_PUBLIC_API(bool) SM_LooselyEqual(JSContext* cx, JS::Handle<JS::Value> v1, JS::Handle<JS::Value> v2, bool* equal)
{
    return JS_LooselyEqual
}

JS_PUBLIC_API(bool) SM_SameValue(JSContext* cx, JS::Handle<JS::Value> v1, JS::Handle<JS::Value> v2, bool* same);
{
    return JS_SameValue(cx, v1, v2, same);
}

JS_PUBLIC_API(bool) SM_IsBuiltinEvalFunction(JSFunction* fun);
{
    return JS_IsBuiltinEvalFunction(fun);
}

JS_PUBLIC_API(bool) SM_IsBuiltinFunctionConstructor(JSFunction* fun);
{
    return JS_IsBuiltinFunctionConstructor(fun);
}

JS_PUBLIC_API(JSContext*) SM_GetParentContext(JSContext* cx)
{
    return JS_GetParentContext(cx);
}

JS_PUBLIC_API(void) SM_SetFutexCanWait(JSContext* cx)
{
    return JS_SetFutexCanWait(cx);
}

JS_PUBLIC_API(JSVersion) JS_GetVersion(JSContext* cx)
{
    return JS_GetVersion(cx);
}

JS_PUBLIC_API(const char*) JS_VersionToString(JSVersion version)
{
    return JS_VersionToString(version);
}

JS_PUBLIC_API(JSVersion) JS_StringToVersion(const char* string)
{
    return JS_StringToVersion(string);
}

JS_PUBLIC_API(const char*) SM_GetImplementationVersion(void)
{
    return JS_GetImplementationVersion();
}

JS_PUBLIC_API(void) SM_SetDestroyCompartmentCallback(JSContext* cx, JSDestroyCompartmentCallback callback)
{
    JS_SetDestroyCompartmentCallback(cx, callback);
}

JS_PUBLIC_API(void) SM_SetSizeOfIncludingThisCompartmentCallback(JSContext* cx, JSSizeOfIncludingThisCompartmentCallback callback)
{
    JS_SetSizeOfIncludingThisCompartmentCallback(cx, callback);
}

JS_PUBLIC_API(void) SM_SetDestroyZoneCallback(JSContext* cx, JSZoneCallback callback)
{
    JS_SetDestroyZoneCallback(cx, callback);
}
JS_PUBLIC_API(void) SM_SetSweepZoneCallback(JSContext* cx, JSZoneCallback callback)
{
    JS_SetSweepZoneCallback(cx, callback);
}

JS_PUBLIC_API(void) SM_SetCompartmentNameCallback(JSContext* cx, JSCompartmentNameCallback callback)
{
    JS_SetCompartmentNameCallback(cx, callback);
}

JS_PUBLIC_API(void) SM_SetWrapObjectCallbacks(JSContext* cx, const JSWrapObjectCallbacks* callbacks)
{
    JS_SetWrapObjectCallbacks(cx, callbacks);
}

JS_PUBLIC_API(void) SM_SetCompartmentPrivate(JSCompartment* compartment, void* data)
{
    JS_SetCompartmentPrivate(compartment, data);
}

JS_PUBLIC_API(void*) SM_GetCompartmentPrivate(JSCompartment* compartment)
{
    return JS_GetCompartmentPrivate(compartment);
}

JS_PUBLIC_API(void) SM_SetZoneUserData(JS::Zone* zone, void* data)
{
    JS_SetZoneUserData(zone, data);
}

JS_PUBLIC_API(void*) SM_GetZoneUserData(JS::Zone* zone)
{
    return JS_GetZoneUserData(zone);
}











JS_PUBLIC_API(void*)
JS_NewAutoCheckCannotGC()
{
    return js_new<JS::AutoCheckCannotGC>();
}

JS_PUBLIC_API(void)
JS_FreeAutoCheckCannotGC(void* ac)
{
    js_delete(reinterpret_cast<JS::AutoCheckCannotGC*>(ac));
}
*/

