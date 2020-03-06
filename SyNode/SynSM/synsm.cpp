#ifdef _WINDOWS
#define _CRT_NONSTDC_NO_DEPRECATE // disable error C4996 - 'strdup': The POSIX name for this item is deprecated. Instead, use the ISO C and C++ conformant name: _strdup. See online help for details.
#include "stdafx.h"
#endif

#include "synsm.h"

#include <js/Date.h>

#include <mozilla/Assertions.h>

// Declared in vm/Runtime.h
namespace js {
    extern bool gCanUseExtraThreads;
}

JS_PUBLIC_API(void) SM_DisableExtraThreads(void)
{
    js::gCanUseExtraThreads = false;
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

JS_PUBLIC_API(uint64_t) SM_GetReservedSlot(JSObject* obj, uint32_t index)
{
    return js::GetReservedSlot(obj, index).asRawBits();
}

JS_PUBLIC_API(JSString*) SM_NewStringCopyUTF8N(JSContext* cx, const char* aBytes, size_t aLangth)
{
	JS::UTF8Chars utf8chars = JS::UTF8Chars(aBytes, aLangth);
	return JS_NewStringCopyUTF8N(cx, utf8chars);
}

JS_PUBLIC_API(JSObject*) SM_NewDateObjectMsec(JSContext* cx, double msec)
{
    JS::ClippedTime time = JS::TimeClip(msec);
    return JS::NewDateObject(cx, time);
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

