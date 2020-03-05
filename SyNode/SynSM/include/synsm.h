#ifndef SynSM_h
#define SynSM_h

#include <jsapi.h>
#include <jsfriendapi.h>
#include <js/Initialization.h>

extern "C" {

JS_PUBLIC_API(bool) (*SM_Initialize)(void) = &JS_Init;

JS_PUBLIC_API(void) SM_DisableExtraThreads(void);

JS_PUBLIC_API(void) (*SM_ShutDown)(void) = &JS_ShutDown;

JS_PUBLIC_API(int64_t) (*SM_Now)(void) = &JS_Now;

JS_PUBLIC_API(JSString*) (*SM_GetEmptyString)(JSContext* cx) = &JS_GetEmptyString;

JS_PUBLIC_API(JSType) (*SM_TypeOfValue)(JSContext* cx, JS::Handle<JS::Value> v) = &JS_TypeOfValue;

JS_PUBLIC_API(void) (*SM_BeginRequest)(JSContext* cx) = &JS_BeginRequest;

JS_PUBLIC_API(void) (*SM_EndRequest)(JSContext* cx) = &JS_EndRequest;

JS_PUBLIC_API(JSContext*) (*SM_NewContext)(
    uint32_t maxbytes, uint32_t maxNurseryBytes, JSContext* parentContext) = &JS_NewContext;

JS_PUBLIC_API(bool) (*SM_InitSelfHostedCode)(JSContext* cx) = &JS::InitSelfHostedCode;

JS_PUBLIC_API(void) (*SM_DestroyContext)(JSContext* cx) = &JS_DestroyContext;

JS_PUBLIC_API(void*) (*SM_GetContextPrivate)(JSContext* cx) = &JS_GetContextPrivate;

JS_PUBLIC_API(void) (*SM_SetContextPrivate)(JSContext* cx, void* data) = &JS_SetContextPrivate;

JS_PUBLIC_API(bool) (*SM_WrapObject)(JSContext* cx, JS::MutableHandleObject objp) = &JS_WrapObject;

JS_PUBLIC_API(JSCompartment*) (*SM_EnterCompartment)(JSContext* cx, JSObject* target) = &JS_EnterCompartment;

JS_PUBLIC_API(void) (*SM_LeaveCompartment)(JSContext* cx, JSCompartment* oldCompartment) = &JS_LeaveCompartment;

JS_PUBLIC_API(bool) (*SM_InitStandardClasses)(JSContext* cx, JS::Handle<JSObject*> obj) = &JS_InitStandardClasses;

JS_PUBLIC_API(JSObject*) (*SM_CurrentGlobalOrNull)(JSContext* cx) = &JS::CurrentGlobalOrNull;

JS_PUBLIC_API(bool) (*SM_InitReflectParse)(JSContext* cx, JS::HandleObject global) = &JS_InitReflectParse;

JS_PUBLIC_API(bool) (*SM_InitCTypesClass)(JSContext* cx, JS::HandleObject global) = &JS_InitCTypesClass;

JS_PUBLIC_API(bool) (*SM_DefineDebuggerObject)(JSContext* cx, JS::HandleObject obj) = &JS_DefineDebuggerObject;

JS_PUBLIC_API(void) (*SM_GC)(JSContext* cx) = &JS_GC;

JS_PUBLIC_API(void) (*SM_MaybeGC)(JSContext* cx) = &JS_MaybeGC;

JS_PUBLIC_API(void) (*SM_SetGCParameter)(JSContext* cx, JSGCParamKey key, uint32_t value) = &JS_SetGCParameter;

JS_PUBLIC_API(uint32_t) (*SM_GetGCParameter)(JSContext* cx, JSGCParamKey key) = &JS_GetGCParameter;

JS_PUBLIC_API(void) (*SM_SetGCParametersBasedOnAvailableMemory)(JSContext* cx, uint32_t availMem) = &JS_SetGCParametersBasedOnAvailableMemory;

JS_PUBLIC_API(JSString*) (*SM_NewExternalString)(
    JSContext* cx, const char16_t* chars, size_t length, const JSStringFinalizer* fin);

JS_PUBLIC_API(void) (*SM_SetNativeStackQuota)(
    JSContext* cx, size_t systemCodeStackSize, size_t trustedScriptStackSize, size_t untrustedScriptStackSize) = &JS_SetNativeStackQuota;

JS_PUBLIC_API(bool) (*SM_ValueToId)(JSContext* cx, JS::HandleValue v, JS::MutableHandleId idp) = &JS_ValueToId;

JS_PUBLIC_API(bool) (*SM_IdToValue)(JSContext* cx, jsid id, JS::MutableHandle<JS::Value> vp) = &JS_IdToValue;

JS_PUBLIC_API(JSString*) (*SM_ValueToSource)(JSContext* cx, JS::Handle<JS::Value> v) = &JS_ValueToSource;

JS_PUBLIC_API(JSObject*) (*SM_InitClass)(
    JSContext* cx, JS::HandleObject obj, JS::HandleObject parent_proto,
    const JSClass* clasp, JSNative constructor, unsigned nargs,
    const JSPropertySpec* ps, const JSFunctionSpec* fs,
    const JSPropertySpec* static_ps, const JSFunctionSpec* static_fs) = &JS_InitClass;

JS_PUBLIC_API(const JSClass*) (*SM_GetClass)(JSObject* obj) = &JS_GetClass;

JS_PUBLIC_API(bool) (*SM_HasInstance)(
    JSContext* cx, JS::Handle<JSObject*> obj, JS::Handle<JS::Value> v, bool* bp) = &JS_HasInstance;

JS_PUBLIC_API(void*) (*SM_GetPrivate)(JSObject* obj) = &JS_GetPrivate;

JS_PUBLIC_API(void) (*SM_SetPrivate)(JSObject* obj, void* data) = &JS_SetPrivate;

JS_PUBLIC_API(JSObject*) (*SM_GetConstructor)(JSContext* cx, JS::Handle<JSObject*> proto) = &JS_GetConstructor;

JS_PUBLIC_API(void*) (*SM_GetInstancePrivate)(
    JSContext* cx, JS::Handle<JSObject*> obj, const JSClass* clasp, JS::CallArgs* args) = &JS_GetInstancePrivate;

JS_PUBLIC_API(JSObject*) (*SM_NewGlobalObject)(
    JSContext* cx, const JSClass* clasp, JSPrincipals* principals,
    JS::OnNewGlobalHookOption hookOption, const JS::CompartmentOptions& options) = &JS_NewGlobalObject;

JS_PUBLIC_API(void) (*SM_GlobalObjectTraceHook)(JSTracer* trc, JSObject* global) = &JS_GlobalObjectTraceHook;

JS_PUBLIC_API(JSObject*) (*SM_NewObject)(JSContext* cx, const JSClass* clasp) = &JS_NewObject;

JS_PUBLIC_API(JSObject*) (*SM_NewObjectWithGivenProto)(
    JSContext* cx, const JSClass* clasp, JS::Handle<JSObject*> proto) = &JS_NewObjectWithGivenProto;

JS_PUBLIC_API(bool) (*SM_GetPrototype)(
    JSContext* cx, JS::HandleObject obj, JS::MutableHandleObject result) = &JS_GetPrototype;

JS_PUBLIC_API(bool) (*SM_SetPrototype)(
    JSContext* cx, JS::HandleObject obj, JS::HandleObject proto) = &JS_SetPrototype;

JS_PUBLIC_API(bool) (*SM_DefinePropertyById)(
    JSContext* cx, JS::HandleObject obj, JS::HandleId id, JS::HandleValue value,
    unsigned attrs, JSNative getter, JSNative setter) = &JS_DefinePropertyById;

JS_PUBLIC_API(bool) (*SM_DefineProperty)(
    JSContext* cx, JS::HandleObject obj, const char* name, JS::HandleValue value,
    unsigned attrs, JSNative getter, JSNative setter) = &JS_DefineProperty;

JS_PUBLIC_API(bool) (*SM_DefineUCProperty)(
    JSContext* cx, JS::HandleObject obj, const char16_t* name, size_t namelen,
    JS::HandleValue value, unsigned attrs, JSNative getter, JSNative setter) = &JS_DefineUCProperty;

JS_PUBLIC_API(bool) (*SM_HasProperty)(
    JSContext* cx, JS::HandleObject obj, const char* name, bool* foundp) = &JS_HasProperty;

JS_PUBLIC_API(bool) (*SM_HasUCProperty)(
    JSContext* cx, JS::HandleObject obj, const char16_t* name, size_t namelen, bool* vp) = &JS_HasUCProperty;

JS_PUBLIC_API(bool) (*SM_GetPropertyById)(
    JSContext* cx, JS::HandleObject obj, JS::HandleId id, JS::MutableHandleValue vp) = &JS_GetPropertyById;

JS_PUBLIC_API(bool) (*SM_GetProperty)(
    JSContext* cx, JS::HandleObject obj, const char* name, JS::MutableHandleValue vp) = &JS_GetProperty;

JS_PUBLIC_API(bool) (*SM_GetUCProperty)(
    JSContext* cx, JS::HandleObject obj, const char16_t* name, size_t namelen, JS::MutableHandleValue vp) = &JS_GetUCProperty;

JS_PUBLIC_API(bool) (*SM_GetElement)(
    JSContext* cx, JS::HandleObject obj, uint32_t index, JS::MutableHandleValue vp) = &JS_GetElement;

JS_PUBLIC_API(bool) (*SM_SetProperty)(
    JSContext* cx, JS::HandleObject obj, const char* name, JS::HandleValue v) = &JS_SetProperty;

JS_PUBLIC_API(bool) (*SM_SetUCProperty)(
    JSContext* cx, JS::HandleObject obj, const char16_t* name, size_t namelen, JS::HandleValue v) = &JS_SetUCProperty;

JS_PUBLIC_API(bool) (*SM_SetElement)(
    JSContext* cx, JS::HandleObject obj, uint32_t index, JS::HandleValue v) = &JS_SetElement;

JS_PUBLIC_API(bool) (*SM_DeletePropertyById)(
    JSContext* cx, JS::HandleObject obj, JS::HandleId id, JS::ObjectOpResult& result) = &JS_DeletePropertyById;

JS_PUBLIC_API(bool) (*SM_DeleteElement)(
    JSContext* cx, JS::HandleObject obj, uint32_t index, JS::ObjectOpResult& result) = &JS_DeleteElement;

JS_PUBLIC_API(JS::AutoIdVector*) SM_EnumerateToAutoIdVector(
    JSContext* cx, JS::HandleObject obj, size_t* length, jsid** data);

JS_PUBLIC_API(void) SM_DestroyAutoIdVector(JS::AutoIdVector* v);

JS_PUBLIC_API(bool) (*SM_CallFunctionValue)(
    JSContext* cx, JS::HandleObject obj, JS::HandleValue fval,
    const JS::HandleValueArray& args, JS::MutableHandleValue rval) = &JS_CallFunctionValue;

JS_PUBLIC_API(bool) (*SM_CallFunction)(
    JSContext* cx, JS::HandleObject obj, JS::HandleFunction fun,
    const JS::HandleValueArray& args, JS::MutableHandleValue rval) = &JS_CallFunction;

JS_PUBLIC_API(bool) (*SM_CallFunctionName)(
    JSContext* cx, JS::HandleObject obj, const char* name,
    const JS::HandleValueArray& args, JS::MutableHandleValue rval) = &JS_CallFunctionName;

JS_PUBLIC_API(JSObject*) (*SM_New)(
    JSContext* cx, JS::HandleObject ctor, const JS::HandleValueArray& args) = &JS_New;

JS_PUBLIC_API(bool) (*SM_DefineProperties)(
    JSContext* cx, JS::HandleObject obj, const JSPropertySpec* ps) = &JS_DefineProperties;

JS_PUBLIC_API(bool) (*SM_AlreadyHasOwnUCProperty)(
    JSContext* cx, JS::HandleObject obj, const char16_t* name,
    size_t namelen, bool* foundp) = &JS_AlreadyHasOwnUCProperty;

JS_PUBLIC_API(JSObject*) (*SM_NewArrayObject)(JSContext* cx, size_t length) = &JS_NewArrayObject;

JS_PUBLIC_API(JSObject*) (*SM_NewArrayObject2)(
    JSContext* cx, const JS::HandleValueArray& contents) = &JS_NewArrayObject;

JS_PUBLIC_API(bool) (*SM_IsArrayObject)(
    JSContext* cx, JS::HandleObject obj, bool* isArray) = &JS_IsArrayObject;

JS_PUBLIC_API(bool) (*SM_GetArrayLength)(
    JSContext* cx, JS::Handle<JSObject*> obj, uint32_t* lengthp) = &JS_GetArrayLength;

JS_PUBLIC_API(uint64_t) SM_GetReservedSlot(JSObject* obj, uint32_t index);

JS_PUBLIC_API(void) (*SM_SetReservedSlot)(
    JSObject* obj, uint32_t index, const JS::Value& v) = &JS_SetReservedSlot;

JS_PUBLIC_API(JSFunction*) (*SM_NewFunction)(
    JSContext* cx, JSNative call, unsigned nargs, unsigned flags, const char* name) = &JS_NewFunction;

JS_PUBLIC_API(JSString*) (*SM_GetFunctionId)(JSFunction* fun) = &JS_GetFunctionId;

JS_PUBLIC_API(bool) (*SM_ObjectIsFunction)(JSContext* cx, JSObject* obj) = &JS_ObjectIsFunction;

JS_PUBLIC_API(bool) (*SM_DefineFunctions)(
    JSContext* cx, JS::Handle<JSObject*> obj, const JSFunctionSpec* fs) = &JS_DefineFunctions;

JS_PUBLIC_API(JSFunction*) (*SM_DefineFunction)(
    JSContext* cx, JS::Handle<JSObject*> obj, const char* name, JSNative call,
    unsigned nargs, unsigned attrs) = &JS_DefineFunction;

JS_PUBLIC_API(JSFunction*) (*SM_DefineUCFunction)(
    JSContext* cx, JS::Handle<JSObject*> obj,
    const char16_t* name, size_t namelen, JSNative call,
    unsigned nargs, unsigned attrs) = &JS_DefineUCFunction;

JS_PUBLIC_API(bool) (*SM_CompileScript)(
    JSContext* cx, const char* ascii, size_t length,
    const JS::CompileOptions& options, JS::MutableHandleScript script) = &JS_CompileScript;

JS_PUBLIC_API(bool) (*SM_CompileUCScript)(
    JSContext* cx, const char16_t* chars, size_t length,
    const JS::CompileOptions& options, JS::MutableHandleScript script) = &JS_CompileUCScript;

JS_PUBLIC_API(JSString*) (*SM_DecompileFunction)(
    JSContext* cx, JS::Handle<JSFunction*> fun, unsigned indent) = &JS_DecompileFunction;

JS_PUBLIC_API(bool) (*SM_ExecuteScript)(
    JSContext* cx, JS::HandleScript script, JS::MutableHandleValue rval) = &JS_ExecuteScript;

JS_PUBLIC_API(bool) (*SM_CheckForInterrupt)(JSContext* cx) = &JS_CheckForInterrupt;

JS_PUBLIC_API(bool) (*SM_AddInterruptCallback)(
    JSContext* cx, JSInterruptCallback callback) = &JS_AddInterruptCallback;

JS_PUBLIC_API(bool) (*SM_DisableInterruptCallback)(JSContext* cx) = &JS_DisableInterruptCallback;

JS_PUBLIC_API(void) (*SM_ResetInterruptCallback)(JSContext* cx, bool enable) = &JS_ResetInterruptCallback;

JS_PUBLIC_API(void) (*SM_RequestInterruptCallback)(JSContext* cx) = &JS_RequestInterruptCallback;

JS_PUBLIC_API(bool) (*SM_IsRunning)(JSContext* cx) = &JS_IsRunning;

JS_PUBLIC_API(JSString*) (*SM_NewStringCopyN)(
    JSContext* cx, const char* s, size_t n) = &JS_NewStringCopyN;

JS_PUBLIC_API(JSString*) (*SM_NewStringCopyUTF8Z)(
    JSContext* cx, const JS::ConstUTF8CharsZ s) = &JS_NewStringCopyUTF8Z;

JS_PUBLIC_API(JSString*)
    SM_NewStringCopyUTF8N(JSContext* cx, const char* aBytes, size_t aLangth);

JS_PUBLIC_API(JS::Value) (*SM_GetEmptyStringValue)(JSContext* cx) = &JS_GetEmptyStringValue;

JS_PUBLIC_API(JSString*) (*SM_NewUCStringCopyN)(
    JSContext* cx, const char16_t* s, size_t n) = &JS_NewUCStringCopyN;

JS_PUBLIC_API(size_t) (*SM_GetStringLength)(JSString* str) = &JS_GetStringLength;

JS_PUBLIC_API(bool) (*SM_StringHasLatin1Chars)(JSString* str) = &JS_StringHasLatin1Chars;

JS_PUBLIC_API(const JS::Latin1Char*) (*SM_GetLatin1StringCharsAndLength)(
    JSContext* cx, const JS::AutoCheckCannotGC& nogc, JSString* str, size_t* length) = &JS_GetLatin1StringCharsAndLength;

JS_PUBLIC_API(const char16_t*) (*SM_GetTwoByteStringCharsAndLength)(
    JSContext* cx, const JS::AutoCheckCannotGC& nogc, JSString* str, size_t* length) = &JS_GetTwoByteStringCharsAndLength;

JS_PUBLIC_API(bool) (*SM_Stringify)(
    JSContext* cx, JS::MutableHandleValue value, JS::HandleObject replacer,
    JS::HandleValue space, JSONWriteCallback callback, void* data) = &JS_Stringify;

JS_PUBLIC_API(bool) (*SM_ParseJSON)(
    JSContext* cx, const char16_t* chars, uint32_t len, JS::MutableHandleValue vp) = &JS_ParseJSON;

JS_PUBLIC_API(void) (*SM_ReportErrorASCII)(
    JSContext* cx, const char* format, ...) = &JS_ReportErrorASCII;

JS_PUBLIC_API(void) (*SM_ReportErrorNumberUC)(
    JSContext* cx, JSErrorCallback errorCallback, void* userRef, const unsigned errorNumber, ...) = &JS_ReportErrorNumberUC;

JS_PUBLIC_API(void) (*SM_ReportErrorNumberUTF8)(
    JSContext* cx, JSErrorCallback errorCallback, void* userRef, const unsigned errorNumber, ...) = &JS_ReportErrorNumberUTF8;

JS_PUBLIC_API(void) (*SM_ReportOutOfMemory)(JSContext* cx) = &JS_ReportOutOfMemory;

JS_PUBLIC_API(JS::WarningReporter) (*SM_GetWarningReporter)(JSContext* cx) = &JS::GetWarningReporter;

JS_PUBLIC_API(JS::WarningReporter) (*SM_SetWarningReporter)(JSContext* cx, JS::WarningReporter reporter) = &JS::SetWarningReporter;

JS_PUBLIC_API(JSObject*) (*SM_NewDateObject)(
    JSContext* cx, int year, int mon, int mday, int hour, int min, int sec) = &JS_NewDateObject;

JS_PUBLIC_API(JSObject*) SM_NewDateObjectMsec(JSContext* cx, double msec);

JS_PUBLIC_API(bool) (*SM_ObjectIsDate)(JSContext* cx, JS::HandleObject obj, bool* isDate) = &JS_ObjectIsDate;

JS_PUBLIC_API(bool) (*SM_IsExceptionPending)(JSContext* cx) = &JS_IsExceptionPending;

JS_PUBLIC_API(bool) (*SM_GetPendingException)(JSContext* cx, JS::MutableHandleValue vp) = &JS_GetPendingException;

JS_PUBLIC_API(void) (*SM_SetPendingException)(JSContext* cx, JS::HandleValue v) = &JS_SetPendingException;

JS_PUBLIC_API(void) (*SM_ClearPendingException)(JSContext* cx) = &JS_ClearPendingException;

JS_PUBLIC_API(JSErrorReport*) (*SM_ErrorFromException)(JSContext* cx, JS::HandleObject obj) = &JS_ErrorFromException;

JS_PUBLIC_API(void*) SM_GetContextOptions(JSContext* cx);

JS_PUBLIC_API(void*) SM_NewRootedValue(JSContext* cx, uint64_t initial);

JS_PUBLIC_API(void) SM_FreeRootedValue(void* val);

JS_PUBLIC_API(void*) SM_NewRootedObject(JSContext* cx, JSObject* initial);

JS_PUBLIC_API(void) SM_FreeRootedObject(void* obj);

JS_PUBLIC_API(void*) SM_NewRootedString(JSContext* cx, JSString* initial);

JS_PUBLIC_API(void) SM_FreeRootedString(void* str);

JS_PUBLIC_API(void*) SM_NewCompileOptions(JSContext* cx);

JS_PUBLIC_API(void) SM_SetCompileOptionsFileLineAndUtf8(
    JS::CompileOptions& options, const char* f, unsigned l, bool isUtf8);

JS_PUBLIC_API(void) SM_FreeCompileOptions(void* co);

JS_PUBLIC_API(bool) (*SM_EvaluateScript)(
    JSContext* cx, const JS::ReadOnlyCompileOptions& options,
    const char* bytes, size_t length, JS::MutableHandleValue rval) = &JS::Evaluate;

JS_PUBLIC_API(bool) (*SM_EvaluateUCScript)(
    JSContext* cx, const JS::ReadOnlyCompileOptions& options,
    const char16_t* chars, size_t length, JS::MutableHandleValue rval) = &JS::Evaluate;

JS_PUBLIC_API(JS::Value) (*SM_ComputeThis)(JSContext* cx, JS::Value* vp) = &JS::detail::ComputeThis;

// Array functions from jsfriendsapi.h

JS_FRIEND_API(JSObject*) (*SM_NewInt8Array)(JSContext* cx, uint32_t nelements) = &JS_NewInt8Array;

JS_FRIEND_API(JSObject*) (*SM_NewUint8Array)(JSContext* cx, uint32_t nelements) = &JS_NewUint8Array;

JS_FRIEND_API(JSObject*) (*SM_NewUint8ClampedArray)(JSContext* cx, uint32_t nelements) = &JS_NewUint8ClampedArray;

JS_FRIEND_API(JSObject*) (*SM_NewInt16Array)(JSContext* cx, uint32_t nelements) = &JS_NewInt16Array;

JS_FRIEND_API(JSObject*) (*SM_NewUint16Array)(JSContext* cx, uint32_t nelements) = &JS_NewUint16Array;

JS_FRIEND_API(JSObject*) (*SM_NewInt32Array)(JSContext* cx, uint32_t nelements) = &JS_NewInt32Array;

JS_FRIEND_API(JSObject*) (*SM_NewUint32Array)(JSContext* cx, uint32_t nelements) = &JS_NewUint32Array;

JS_FRIEND_API(JSObject*) (*SM_NewFloat32Array)(JSContext* cx, uint32_t nelements) = &JS_NewFloat32Array;

JS_FRIEND_API(JSObject*) (*SM_NewFloat64Array)(JSContext* cx, uint32_t nelements) = &JS_NewFloat64Array;

JS_FRIEND_API(JSObject*) (*SM_NewInt8ArrayFromArray)(JSContext* cx, JS::HandleObject array) = &JS_NewInt8ArrayFromArray;

JS_FRIEND_API(JSObject*) (*SM_NewUint8ArrayFromArray)(JSContext* cx, JS::HandleObject array) = &JS_NewUint8ArrayFromArray;

JS_FRIEND_API(JSObject*) (*SM_NewUint8ClampedArrayFromArray)(JSContext* cx, JS::HandleObject array) = &JS_NewUint8ClampedArrayFromArray;

JS_FRIEND_API(JSObject*) (*SM_NewInt16ArrayFromArray)(JSContext* cx, JS::HandleObject array) = &JS_NewInt16ArrayFromArray;

JS_FRIEND_API(JSObject*) (*SM_NewUint16ArrayFromArray)(JSContext* cx, JS::HandleObject array) = &JS_NewUint16ArrayFromArray;

JS_FRIEND_API(JSObject*) (*SM_NewInt32ArrayFromArray)(JSContext* cx, JS::HandleObject array) = &JS_NewInt32ArrayFromArray;

JS_FRIEND_API(JSObject*) (*SM_NewUint32ArrayFromArray)(JSContext* cx, JS::HandleObject array) = &JS_NewUint32ArrayFromArray;

JS_FRIEND_API(JSObject*) (*SM_NewFloat32ArrayFromArray)(JSContext* cx, JS::HandleObject array) = &JS_NewFloat32ArrayFromArray;

JS_FRIEND_API(JSObject*) (*SM_NewFloat64ArrayFromArray)(JSContext* cx, JS::HandleObject array) = &JS_NewFloat64ArrayFromArray;

JS_FRIEND_API(JSObject*) (*SM_NewInt8ArrayWithBuffer)(
    JSContext* cx, JS::HandleObject arrayBuffer, uint32_t byteOffset, int32_t length) = &JS_NewInt8ArrayWithBuffer;

JS_FRIEND_API(JSObject*) (*SM_NewUint8ArrayWithBuffer)(
    JSContext* cx, JS::HandleObject arrayBuffer, uint32_t byteOffset, int32_t length) = &JS_NewUint8ArrayWithBuffer;

JS_FRIEND_API(JSObject*) (*SM_NewUint8ClampedArrayWithBuffer)(
    JSContext* cx, JS::HandleObject arrayBuffer, uint32_t byteOffset, int32_t length) = &JS_NewUint8ClampedArrayWithBuffer;

JS_FRIEND_API(JSObject*) (*SM_NewInt16ArrayWithBuffer)(
    JSContext* cx, JS::HandleObject arrayBuffer, uint32_t byteOffset, int32_t length) = &JS_NewInt16ArrayWithBuffer;

JS_FRIEND_API(JSObject*) (*SM_NewUint16ArrayWithBuffer)(
    JSContext* cx, JS::HandleObject arrayBuffer, uint32_t byteOffset, int32_t length) = &JS_NewUint16ArrayWithBuffer;

JS_FRIEND_API(JSObject*) (*SM_NewInt32ArrayWithBuffer)(
    JSContext* cx, JS::HandleObject arrayBuffer, uint32_t byteOffset, int32_t length) = &JS_NewInt32ArrayWithBuffer;

JS_FRIEND_API(JSObject*) (*SM_NewUint32ArrayWithBuffer)(
    JSContext* cx, JS::HandleObject arrayBuffer, uint32_t byteOffset, int32_t length) = &JS_NewUint32ArrayWithBuffer;

JS_FRIEND_API(JSObject*) (*SM_NewFloat32ArrayWithBuffer)(
    JSContext* cx, JS::HandleObject arrayBuffer, uint32_t byteOffset, int32_t length) = &JS_NewFloat32ArrayWithBuffer;

JS_FRIEND_API(JSObject*) (*SM_NewFloat64ArrayWithBuffer)(
    JSContext* cx, JS::HandleObject arrayBuffer, uint32_t byteOffset, int32_t length) = &JS_NewFloat64ArrayWithBuffer;

JS_FRIEND_API(JSObject*) (*SM_NewSharedArrayBuffer)(JSContext* cx, uint32_t nbytes) = &JS_NewSharedArrayBuffer;

JS_FRIEND_API(JSObject*) (*SM_NewArrayBuffer)(JSContext* cx, uint32_t nbytes) = &JS_NewArrayBuffer;

JS_FRIEND_API(bool) (*SM_IsTypedArrayObject)(JSObject* obj) = &JS_IsTypedArrayObject;

JS_FRIEND_API(bool) (*SM_IsArrayBufferViewObject)(JSObject* obj) = &JS_IsArrayBufferViewObject;

JS_FRIEND_API(bool) (*SM_IsInt8Array)(JSObject* obj) = &JS_IsInt8Array;

JS_FRIEND_API(bool) (*SM_IsUint8Array)(JSObject* obj) = &JS_IsUint8Array;

JS_FRIEND_API(bool) (*SM_IsUint8ClampedArray)(JSObject* obj) = &JS_IsUint8ClampedArray;

JS_FRIEND_API(bool) (*SM_IsInt16Array)(JSObject* obj) = &JS_IsInt16Array;

JS_FRIEND_API(bool) (*SM_IsUint16Array)(JSObject* obj) = &JS_IsUint16Array;

JS_FRIEND_API(bool) (*SM_IsInt32Array)(JSObject* obj) = &JS_IsInt32Array;

JS_FRIEND_API(bool) (*SM_IsUint32Array)(JSObject* obj) = &JS_IsUint32Array;

JS_FRIEND_API(bool) (*SM_IsFloat32Array)(JSObject* obj) = &JS_IsFloat32Array;

JS_FRIEND_API(bool) (*SM_IsFloat64Array)(JSObject* obj) = &JS_IsFloat64Array;

JS_FRIEND_API(bool) (*SM_GetTypedArraySharedness)(JSObject* obj) = &JS_GetTypedArraySharedness;

JS_FRIEND_API(JSObject*) (*SM_GetObjectAsInt8Array)(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, int8_t** data) = &JS_GetObjectAsInt8Array;

JS_FRIEND_API(JSObject*) (*SM_GetObjectAsUint8Array)(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, uint8_t** data) = &JS_GetObjectAsUint8Array;

JS_FRIEND_API(JSObject*) (*SM_GetObjectAsUint8ClampedArray)(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, uint8_t** data) = &JS_GetObjectAsUint8ClampedArray;

JS_FRIEND_API(JSObject*) (*SM_GetObjectAsInt16Array)(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, int16_t** data) = &JS_GetObjectAsInt16Array;

JS_FRIEND_API(JSObject*) (*SM_GetObjectAsUint16Array)(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, uint16_t** data) = &JS_GetObjectAsUint16Array;

JS_FRIEND_API(JSObject*) (*SM_GetObjectAsInt32Array)(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, int32_t** data) = &JS_GetObjectAsInt32Array;

JS_FRIEND_API(JSObject*) (*SM_GetObjectAsUint32Array)(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, uint32_t** data) = &JS_GetObjectAsUint32Array;

JS_FRIEND_API(JSObject*) (*SM_GetObjectAsFloat32Array)(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, float** data) = &JS_GetObjectAsFloat32Array;

JS_FRIEND_API(JSObject*) (*SM_GetObjectAsFloat64Array)(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, double** data) = &JS_GetObjectAsFloat64Array;

JS_FRIEND_API(JSObject*) (*SM_GetObjectAsArrayBufferView)(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, uint8_t** data) = &JS_GetObjectAsArrayBufferView;

JS_FRIEND_API(JSObject*) (*SM_GetObjectAsArrayBuffer)(
    JSObject* obj, uint32_t* length, uint8_t** data) = &JS_GetObjectAsArrayBuffer;

JS_FRIEND_API(js::Scalar::Type) (*SM_GetArrayBufferViewType)(JSObject* obj) = &JS_GetArrayBufferViewType;

JS_FRIEND_API(bool) (*SM_IsArrayBufferObject)(JSObject* obj) = &JS_IsArrayBufferObject;

JS_FRIEND_API(bool) (*SM_IsSharedArrayBufferObject)(JSObject* obj) = &JS_IsSharedArrayBufferObject;

JS_FRIEND_API(uint32_t) (*SM_GetArrayBufferByteLength)(JSObject* obj) = &JS_GetArrayBufferByteLength;

JS_FRIEND_API(uint32_t) (*SM_GetSharedArrayBufferByteLength)(JSObject* obj) = &JS_GetSharedArrayBufferByteLength;

JS_FRIEND_API(bool) (*SM_ArrayBufferHasData)(JSObject* obj) = &JS_ArrayBufferHasData;

JS_FRIEND_API(uint8_t*) (*SM_GetArrayBufferData)(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC&) = &JS_GetArrayBufferData;

JS_FRIEND_API(bool) (*SM_IsMappedArrayBufferObject)(JSObject* obj) = &JS_IsMappedArrayBufferObject;

JS_FRIEND_API(uint32_t) (*SM_GetTypedArrayLength)(JSObject* obj) = &JS_GetTypedArrayLength;

JS_FRIEND_API(uint32_t) (*SM_GetTypedArrayByteOffset)(JSObject* obj) = &JS_GetTypedArrayByteOffset;

JS_FRIEND_API(uint32_t) (*SM_GetTypedArrayByteLength)(JSObject* obj) = &JS_GetTypedArrayByteLength;

JS_FRIEND_API(uint32_t) (*SM_GetArrayBufferViewByteLength)(JSObject* obj) = &JS_GetArrayBufferViewByteLength;

JS_FRIEND_API(int8_t*) (*SM_GetInt8ArrayData)(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC&) = &JS_GetInt8ArrayData;

JS_FRIEND_API(uint8_t*) (*SM_GetUint8ArrayData)(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC&) = &JS_GetUint8ArrayData;

JS_FRIEND_API(uint8_t*) (*SM_GetUint8ClampedArrayData)(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC&) = &JS_GetUint8ClampedArrayData;

JS_FRIEND_API(int16_t*) (*SM_GetInt16ArrayData)(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC&) = &JS_GetInt16ArrayData;

JS_FRIEND_API(uint16_t*) (*SM_GetUint16ArrayData)(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC&) = &JS_GetUint16ArrayData;

JS_FRIEND_API(int32_t*) (*SM_GetInt32ArrayData)(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC&) = &JS_GetInt32ArrayData;

JS_FRIEND_API(uint32_t*) (*SM_GetUint32ArrayData)(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC&) = &JS_GetUint32ArrayData;

JS_FRIEND_API(float*) (*SM_GetFloat32ArrayData)(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC&) = &JS_GetFloat32ArrayData;

JS_FRIEND_API(double*) (*SM_GetFloat64ArrayData)(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC&) = &JS_GetFloat64ArrayData;

JS_FRIEND_API(void*) (*SM_GetArrayBufferViewData)(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC&) = &JS_GetArrayBufferViewData;

JS_FRIEND_API(JSObject*) (*SM_GetArrayBufferViewBuffer)(
    JSContext* cx, JS::HandleObject obj, bool* isSharedMemory) = &JS_GetArrayBufferViewBuffer;

JS_PUBLIC_API(bool) SM_InitModuleClasses(JSContext* cx, JS::Handle<JSObject*> obj);

JS_PUBLIC_API(JSObject*) SM_CompileModule(
    JSContext* cx, JS::Handle<JSObject*> obj, JS::CompileOptions& options,
    const char16_t* chars, size_t length);

JS_PUBLIC_API(void) (*SM_SetModuleResolveHook)(
    JSContext* cx, JS::Handle<JSFunction*> hook) = &JS::SetModuleResolveHook;

JS_PUBLIC_API(void*) SM_NewCompartmentOptions();

JS_PUBLIC_API(void) SM_FreeCompartmentOptions(void* opt);

}





/*
JS_PUBLIC_API(JS::Value) SM_GetNaNValue(JSContext* cx);

JS_PUBLIC_API(JS::Value) SM_GetNegativeInfinityValue(JSContext* cx);

JS_PUBLIC_API(JS::Value) SM_GetPositiveInfinityValue(JSContext* cx);

JS_PUBLIC_API(bool) SM_ValueToObject(JSContext* cx, JS::HandleValue v, JS::MutableHandleObject objp);

JS_PUBLIC_API(JSFunction*) SM_ValueToFunction(JSContext* cx, JS::HandleValue v);

JS_PUBLIC_API(JSFunction*) SM_ValueToConstructor(JSContext* cx, JS::HandleValue v);

JS_PUBLIC_API(bool) SM_StrictlyEqual(JSContext* cx, JS::Handle<JS::Value> v1, JS::Handle<JS::Value> v2, bool* equal);

JS_PUBLIC_API(bool) SM_LooselyEqual(JSContext* cx, JS::Handle<JS::Value> v1, JS::Handle<JS::Value> v2, bool* equal);

JS_PUBLIC_API(bool) SM_SameValue(JSContext* cx, JS::Handle<JS::Value> v1, JS::Handle<JS::Value> v2, bool* same);

JS_PUBLIC_API(bool) SM_IsBuiltinEvalFunction(JSFunction* fun);

JS_PUBLIC_API(bool) SM_IsBuiltinFunctionConstructor(JSFunction* fun);

JS_PUBLIC_API(JSContext*) SM_GetParentContext(JSContext* cx);

JS_PUBLIC_API(void) SM_SetFutexCanWait(JSContext* cx);

JS_PUBLIC_API(JSVersion) SM_GetVersion(JSContext* cx);

JS_PUBLIC_API(const char*) SM_VersionToString(JSVersion version);

JS_PUBLIC_API(JSVersion) SM_StringToVersion(const char* string);

JS_PUBLIC_API(const char*) SM_GetImplementationVersion(void);

JS_PUBLIC_API(void) SM_SetDestroyCompartmentCallback(JSContext* cx, JSDestroyCompartmentCallback callback);

JS_PUBLIC_API(void) SM_SetSizeOfIncludingThisCompartmentCallback(
    JSContext* cx, JSSizeOfIncludingThisCompartmentCallback callback);

JS_PUBLIC_API(void) SM_SetDestroyZoneCallback(JSContext* cx, JSZoneCallback callback);

JS_PUBLIC_API(void) SM_SetSweepZoneCallback(JSContext* cx, JSZoneCallback callback);

JS_PUBLIC_API(void) SM_SetCompartmentNameCallback(JSContext* cx, JSCompartmentNameCallback callback);

JS_PUBLIC_API(void) SM_SetWrapObjectCallbacks(JSContext* cx, const JSWrapObjectCallbacks* callbacks);

JS_PUBLIC_API(void) SM_SetCompartmentPrivate(JSCompartment* compartment, void* data);

JS_PUBLIC_API(void*) SM_GetCompartmentPrivate(JSCompartment* compartment);

JS_PUBLIC_API(void) SM_SetZoneUserData(JS::Zone* zone, void* data);

JS_PUBLIC_API(void*) SM_GetZoneUserData(JS::Zone* zone);








JS_PUBLIC_API(bool)
 JS_WrapValue(JSContext* cx, JS::MutableHandleValue vp);

JS_PUBLIC_API(JSObject*)
 JS_TransplantObject(JSContext* cx, JS::HandleObject origobj, JS::HandleObject target);

JS_PUBLIC_API(bool)
 JS_RefreshCrossCompartmentWrappers(JSContext* cx, JS::Handle<JSObject*> obj);

JS_PUBLIC_API(void)
 JS_IterateCompartments(JSContext* cx, void* data,
                        JSIterateCompartmentCallback compartmentCallback);

JS_PUBLIC_API(bool)
 JS_ResolveStandardClass(JSContext* cx, JS::HandleObject obj, JS::HandleId id, bool* resolved);

JS_PUBLIC_API(bool)
 JS_MayResolveStandardClass(const JSAtomState& names, jsid id, JSObject* maybeObj);

JS_PUBLIC_API(bool)
 JS_EnumerateStandardClasses(JSContext* cx, JS::HandleObject obj);

JS_PUBLIC_API(bool)
 JS_GetClassObject(JSContext* cx, JSProtoKey key, JS::MutableHandle<JSObject*> objp);

JS_PUBLIC_API(bool)
 JS_GetClassPrototype(JSContext* cx, JSProtoKey key, JS::MutableHandle<JSObject*> objp);

JS_PUBLIC_API(JSProtoKey)
 JS_IdToProtoKey(JSContext* cx, JS::HandleId id);

JS_PUBLIC_API(JSObject*)
 JS_GetFunctionPrototype(JSContext* cx, JS::HandleObject forObj);

JS_PUBLIC_API(JSObject*)
 JS_GetObjectPrototype(JSContext* cx, JS::HandleObject forObj);

JS_PUBLIC_API(JSObject*)
 JS_GetArrayPrototype(JSContext* cx, JS::HandleObject forObj);

JS_PUBLIC_API(JSObject*)
 JS_GetErrorPrototype(JSContext* cx);

JS_PUBLIC_API(JSObject*)
 JS_GetIteratorPrototype(JSContext* cx);

JS_PUBLIC_API(JSObject*)
 JS_GetGlobalForObject(JSContext* cx, JSObject* obj);

JS_PUBLIC_API(bool)
 JS_IsGlobalObject(JSObject* obj);

JS_PUBLIC_API(JSObject*)
 JS_GlobalLexicalEnvironment(JSObject* obj);

JS_PUBLIC_API(bool)
 JS_HasExtensibleLexicalEnvironment(JSObject* obj);

JS_PUBLIC_API(JSObject*)
 JS_ExtensibleLexicalEnvironment(JSObject* obj);

JS_PUBLIC_API(JSObject*)
 JS_GetGlobalForCompartmentOrNull(JSContext* cx, JSCompartment* c);

JS_PUBLIC_API(bool)
 JS_DefineProfilingFunctions(JSContext* cx, JS::HandleObject obj);

JS_PUBLIC_API(void)
 JS_SetCTypesCallbacks(JSObject* ctypesObj, const JSCTypesCallbacks* callbacks);

JS_PUBLIC_API(void*)
 JS_malloc(JSContext* cx, size_t nbytes);

JS_PUBLIC_API(void*)
 JS_realloc(JSContext* cx, void* p, size_t oldBytes, size_t newBytes);

JS_PUBLIC_API(void)
 JS_free(JSContext* cx, void* p);

JS_PUBLIC_API(void)
 JS_freeop(JSFreeOp* fop, void* p);

JS_PUBLIC_API(void)
 JS_updateMallocCounter(JSContext* cx, size_t nbytes);

JS_PUBLIC_API(char*)
 JS_strdup(JSContext* cx, const char* s);

JS_PUBLIC_API(bool)
 JS_AddExtraGCRootsTracer(JSContext* cx, JSTraceDataOp traceOp, void* data);

JS_PUBLIC_API(void)
 JS_RemoveExtraGCRootsTracer(JSContext* cx, JSTraceDataOp traceOp, void* data);

JS_PUBLIC_API(void)
 JS_SetGCCallback(JSContext* cx, JSGCCallback cb, void* data);

JS_PUBLIC_API(void)
 JS_SetObjectsTenuredCallback(JSContext* cx, JSObjectsTenuredCallback cb,
                              void* data);

JS_PUBLIC_API(bool)
 JS_AddFinalizeCallback(JSContext* cx, JSFinalizeCallback cb, void* data);

JS_PUBLIC_API(void)
 JS_RemoveFinalizeCallback(JSContext* cx, JSFinalizeCallback cb);

JS_PUBLIC_API(bool)
 JS_AddWeakPointerZoneGroupCallback(JSContext* cx, JSWeakPointerZoneGroupCallback cb, void* data);

JS_PUBLIC_API(void)
 JS_RemoveWeakPointerZoneGroupCallback(JSContext* cx, JSWeakPointerZoneGroupCallback cb);

JS_PUBLIC_API(bool)
 JS_AddWeakPointerCompartmentCallback(JSContext* cx, JSWeakPointerCompartmentCallback cb,
                                      void* data);

JS_PUBLIC_API(void)
 JS_RemoveWeakPointerCompartmentCallback(JSContext* cx, JSWeakPointerCompartmentCallback cb);

JS_PUBLIC_API(void)
 JS_UpdateWeakPointerAfterGC(JS::Heap<JSObject*>* objp);

JS_PUBLIC_API(void)
 JS_UpdateWeakPointerAfterGCUnbarriered(JSObject** objp);

JS_PUBLIC_API(bool)
 JS_IsExternalString(JSString* str);

JS_PUBLIC_API(const JSStringFinalizer*)
 JS_GetExternalStringFinalizer(JSString* str);

JS_PUBLIC_API(void)
 JS_SetNativeStackQuota(JSContext* cx, size_t systemCodeStackSize,
                        size_t trustedScriptStackSize = 0,
                        size_t untrustedScriptStackSize = 0);

JS_PUBLIC_API(bool)
 JS_StringToId(JSContext* cx, JS::HandleString s, JS::MutableHandleId idp);

JS_PUBLIC_API(bool)
 JS_PropertyStub(JSContext* cx, JS::HandleObject obj, JS::HandleId id,
                 JS::MutableHandleValue vp);

JS_PUBLIC_API(bool)
 JS_StrictPropertyStub(JSContext* cx, JS::HandleObject obj, JS::HandleId id,
                       JS::MutableHandleValue vp, JS::ObjectOpResult& result);

JS_PUBLIC_API(bool)
 JS_LinkConstructorAndPrototype(JSContext* cx, JS::Handle<JSObject*> ctor,
                                JS::Handle<JSObject*> proto);

JS_PUBLIC_API(bool)
 JS_InstanceOf(JSContext* cx, JS::Handle<JSObject*> obj, const JSClass* clasp, JS::CallArgs* args);

JS_PUBLIC_API(void)
 JS_FireOnNewGlobalObject(JSContext* cx, JS::HandleObject global);

JS_PUBLIC_API(bool)
 JS_IsNative(JSObject* obj);

JS_PUBLIC_API(JSObject*)
 JS_NewPlainObject(JSContext* cx);

JS_PUBLIC_API(bool)
 JS_DeepFreezeObject(JSContext* cx, JS::Handle<JSObject*> obj);

JS_PUBLIC_API(bool)
 JS_FreezeObject(JSContext* cx, JS::Handle<JSObject*> obj);

JS_PUBLIC_API(bool)
 JS_GetPrototypeIfOrdinary(JSContext* cx, JS::HandleObject obj, bool* isOrdinary,
                           JS::MutableHandleObject result);

JS_PUBLIC_API(bool)
 JS_IsExtensible(JSContext* cx, JS::HandleObject obj, bool* extensible);

JS_PUBLIC_API(bool)
 JS_PreventExtensions(JSContext* cx, JS::HandleObject obj, JS::ObjectOpResult& result);

JS_PUBLIC_API(bool)
 JS_SetImmutablePrototype(JSContext* cx, JS::HandleObject obj, bool* succeeded);

JS_PUBLIC_API(bool)
 JS_GetOwnPropertyDescriptorById(JSContext* cx, JS::HandleObject obj, JS::HandleId id,
                                 JS::MutableHandle<JS::PropertyDescriptor> desc);

JS_PUBLIC_API(bool)
 JS_GetOwnPropertyDescriptor(JSContext* cx, JS::HandleObject obj, const char* name,
                             JS::MutableHandle<JS::PropertyDescriptor> desc);

JS_PUBLIC_API(bool)
 JS_GetOwnUCPropertyDescriptor(JSContext* cx, JS::HandleObject obj, const char16_t* name,
                               JS::MutableHandle<JS::PropertyDescriptor> desc);

JS_PUBLIC_API(bool)
 JS_GetPropertyDescriptorById(JSContext* cx, JS::HandleObject obj, JS::HandleId id,
                              JS::MutableHandle<JS::PropertyDescriptor> desc);

JS_PUBLIC_API(bool)
 JS_GetPropertyDescriptor(JSContext* cx, JS::HandleObject obj, const char* name,
                          JS::MutableHandle<JS::PropertyDescriptor> desc);

JS_PUBLIC_API(bool)
 JS_DefineElement(JSContext* cx, JS::HandleObject obj, uint32_t index, JS::HandleValue value,
                  unsigned attrs, JSNative getter = nullptr, JSNative setter = nullptr);

JS_PUBLIC_API(bool)
 JS_HasPropertyById(JSContext* cx, JS::HandleObject obj, JS::HandleId id, bool* foundp);

JS_PUBLIC_API(bool)
 JS_HasElement(JSContext* cx, JS::HandleObject obj, uint32_t index, bool* foundp);

JS_PUBLIC_API(bool)
 JS_HasOwnPropertyById(JSContext* cx, JS::HandleObject obj, JS::HandleId id, bool* foundp);

JS_PUBLIC_API(bool)
 JS_HasOwnProperty(JSContext* cx, JS::HandleObject obj, const char* name, bool* foundp);

JS_PUBLIC_API(bool)
 JS_ForwardGetPropertyTo(JSContext* cx, JS::HandleObject obj, JS::HandleId id,
                         JS::HandleValue receiver, JS::MutableHandleValue vp);

JS_PUBLIC_API(bool)
 JS_ForwardGetElementTo(JSContext* cx, JS::HandleObject obj, uint32_t index,
                        JS::HandleObject receiver, JS::MutableHandleValue vp);

JS_PUBLIC_API(bool)
 JS_ForwardSetPropertyTo(JSContext* cx, JS::HandleObject obj, JS::HandleId id, JS::HandleValue v,
                         JS::HandleValue receiver, JS::ObjectOpResult& result);

JS_PUBLIC_API(bool)
 JS_SetPropertyById(JSContext* cx, JS::HandleObject obj, JS::HandleId id, JS::HandleValue v);

JS_PUBLIC_API(bool)
 JS_DeleteProperty(JSContext* cx, JS::HandleObject obj, const char* name,
                   JS::ObjectOpResult& result);

JS_PUBLIC_API(bool)
 JS_DeleteUCProperty(JSContext* cx, JS::HandleObject obj, const char16_t* name, size_t namelen,
                     JS::ObjectOpResult& result);

JS_PUBLIC_API(bool)
 JS_Enumerate(JSContext* cx, JS::HandleObject obj, JS::MutableHandle<JS::IdVector> props);

JS_PUBLIC_API(JSObject*)
 JS_DefineObject(JSContext* cx, JS::HandleObject obj, const char* name,
                 const JSClass* clasp = nullptr, unsigned attrs = 0);

JS_PUBLIC_API(bool)
 JS_DefineConstDoubles(JSContext* cx, JS::HandleObject obj, const JSConstDoubleSpec* cds);

JS_PUBLIC_API(bool)
 JS_DefineConstIntegers(JSContext* cx, JS::HandleObject obj, const JSConstIntegerSpec* cis);

JS_PUBLIC_API(bool)
 JS_AlreadyHasOwnPropertyById(JSContext* cx, JS::HandleObject obj, JS::HandleId id,
                              bool* foundp);

JS_PUBLIC_API(bool)
 JS_AlreadyHasOwnProperty(JSContext* cx, JS::HandleObject obj, const char* name,
                          bool* foundp);

JS_PUBLIC_API(bool)
 JS_AlreadyHasOwnElement(JSContext* cx, JS::HandleObject obj, uint32_t index, bool* foundp);

JS_PUBLIC_API(bool)
 JS_SetArrayLength(JSContext* cx, JS::Handle<JSObject*> obj, uint32_t length);

JS_PUBLIC_API(JSObject*)
 JS_NewArrayBufferWithContents(JSContext* cx, size_t nbytes, void* contents);

JS_PUBLIC_API(JSObject*)
 JS_NewArrayBufferWithExternalContents(JSContext* cx, size_t nbytes, void* contents);

JS_PUBLIC_API(void*)
 JS_StealArrayBufferContents(JSContext* cx, JS::HandleObject obj);

JS_PUBLIC_API(void*)
 JS_ExternalizeArrayBufferContents(JSContext* cx, JS::HandleObject obj);

JS_PUBLIC_API(JSObject*)
 JS_NewMappedArrayBufferWithContents(JSContext* cx, size_t nbytes, void* contents);

JS_PUBLIC_API(void*)
 JS_CreateMappedArrayBufferContents(int fd, size_t offset, size_t length);

JS_PUBLIC_API(void)
 JS_ReleaseMappedArrayBufferContents(void* contents, size_t length);

JS_PUBLIC_API(JS::Value)
 JS_GetReservedSlot(JSObject* obj, uint32_t index);

JS_PUBLIC_API(JSObject*)
 JS_GetFunctionObject(JSFunction* fun);

JS_PUBLIC_API(JSString*)
 JS_GetFunctionDisplayId(JSFunction* fun);

JS_PUBLIC_API(uint16_t)
 JS_GetFunctionArity(JSFunction* fun);

JS_PUBLIC_API(bool)
 JS_IsNativeFunction(JSObject* funobj, JSNative call);

JS_PUBLIC_API(bool)
 JS_IsConstructor(JSFunction* fun);

JS_PUBLIC_API(JSFunction*)
 JS_DefineFunctionById(JSContext* cx, JS::Handle<JSObject*> obj, JS::Handle<jsid> id, JSNative call,
                       unsigned nargs, unsigned attrs);

JS_PUBLIC_API(bool)
 JS_IsFunctionBound(JSFunction* fun);

JS_PUBLIC_API(JSObject*)
 JS_GetBoundFunctionTarget(JSFunction* fun);

JS_PUBLIC_API(bool)
 JS_BufferIsCompilableUnit(JSContext* cx, JS::Handle<JSObject*> obj, const char* utf8,
                           size_t length);

JS_PUBLIC_API(JSObject*)
 JS_GetGlobalFromScript(JSScript* script);

JS_PUBLIC_API(const char*)
 JS_GetScriptFilename(JSScript* script);

JS_PUBLIC_API(unsigned)
 JS_GetScriptBaseLineNumber(JSContext* cx, JSScript* script);

JS_PUBLIC_API(JSScript*)
 JS_GetFunctionScript(JSContext* cx, JS::HandleFunction fun);

JS_PUBLIC_API(JSString*)
 JS_DecompileScript(JSContext* cx, JS::Handle<JSScript*> script, const char* name, unsigned indent);

JS_PUBLIC_API(JSString*)
 JS_NewStringCopyZ(JSContext* cx, const char* s);

JS_PUBLIC_API(JSString*)
 JS_NewStringCopyUTF8N(JSContext* cx, const JS::UTF8Chars s);

JS_PUBLIC_API(JSString*)
 JS_AtomizeAndPinJSString(JSContext* cx, JS::HandleString str);

JS_PUBLIC_API(JSString*)
 JS_AtomizeStringN(JSContext* cx, const char* s, size_t length);

JS_PUBLIC_API(JSString*)
 JS_AtomizeString(JSContext* cx, const char* s);

JS_PUBLIC_API(JSString*)
 JS_AtomizeAndPinStringN(JSContext* cx, const char* s, size_t length);

JS_PUBLIC_API(JSString*)
 JS_AtomizeAndPinString(JSContext* cx, const char* s);

JS_PUBLIC_API(JSString*)
 JS_NewUCString(JSContext* cx, char16_t* chars, size_t length);

JS_PUBLIC_API(JSString*)
 JS_NewUCStringCopyZ(JSContext* cx, const char16_t* s);

JS_PUBLIC_API(JSString*)
 JS_AtomizeUCStringN(JSContext* cx, const char16_t* s, size_t length);

JS_PUBLIC_API(JSString*)
 JS_AtomizeUCString(JSContext* cx, const char16_t* s);

JS_PUBLIC_API(JSString*)
 JS_AtomizeAndPinUCStringN(JSContext* cx, const char16_t* s, size_t length);

JS_PUBLIC_API(JSString*)
 JS_AtomizeAndPinUCString(JSContext* cx, const char16_t* s);

JS_PUBLIC_API(bool)
 JS_CompareStrings(JSContext* cx, JSString* str1, JSString* str2, int32_t* result);

JS_PUBLIC_API(bool)
 JS_StringEqualsAscii(JSContext* cx, JSString* str, const char* asciiBytes, bool* match);

JS_PUBLIC_API(size_t)
 JS_PutEscapedString(JSContext* cx, char* buffer, size_t size, JSString* str, char quote);

JS_PUBLIC_API(bool)
 JS_FileEscapedString(FILE* fp, JSString* str, char quote);

JS_PUBLIC_API(bool)
 JS_StringIsFlat(JSString* str);

JS_PUBLIC_API(bool)
 JS_GetStringCharAt(JSContext* cx, JSString* str, size_t index, char16_t* res);

JS_PUBLIC_API(char16_t)
 JS_GetFlatStringCharAt(JSFlatString* str, size_t index);

JS_PUBLIC_API(const char16_t*)
 JS_GetTwoByteExternalStringChars(JSString* str);

JS_PUBLIC_API(bool)
 JS_CopyStringChars(JSContext* cx, mozilla::Range<char16_t> dest, JSString* str);

JS_PUBLIC_API(JSFlatString*)
 JS_FlattenString(JSContext* cx, JSString* str);

JS_PUBLIC_API(const JS::Latin1Char*)
 JS_GetLatin1FlatStringChars(const JS::AutoCheckCannotGC& nogc, JSFlatString* str);

JS_PUBLIC_API(const char16_t*)
 JS_GetTwoByteFlatStringChars(const JS::AutoCheckCannotGC& nogc, JSFlatString* str);

JS_PUBLIC_API(bool)
 JS_FlatStringEqualsAscii(JSFlatString* str, const char* asciiBytes);

JS_PUBLIC_API(size_t)
 JS_PutEscapedFlatString(char* buffer, size_t size, JSFlatString* str, char quote);

JS_PUBLIC_API(JSString*)
 JS_NewDependentString(JSContext* cx, JS::HandleString str, size_t start,
                       size_t length);

JS_PUBLIC_API(JSString*)
 JS_ConcatStrings(JSContext* cx, JS::HandleString left, JS::HandleString right);

JS_PUBLIC_API(bool)
 JS_SetDefaultLocale(JSContext* cx, const char* locale);

JS_PUBLIC_API(void)
 JS_ResetDefaultLocale(JSContext* cx);

JS_PUBLIC_API(void)
 JS_SetLocaleCallbacks(JSContext* cx, const JSLocaleCallbacks* callbacks);

JS_PUBLIC_API(const JSLocaleCallbacks*)
 JS_GetLocaleCallbacks(JSContext* cx);

JS_PUBLIC_API(void)
 JS_ReportErrorLatin1(JSContext* cx, const char* format, ...)
     MOZ_FORMAT_PRINTF(2, 3);

JS_PUBLIC_API(void)
 JS_ReportErrorUTF8(JSContext* cx, const char* format, ...)
     MOZ_FORMAT_PRINTF(2, 3);

JS_PUBLIC_API(void)
 JS_ReportErrorNumberASCII(JSContext* cx, JSErrorCallback errorCallback,
                           void* userRef, const unsigned errorNumber, ...);

JS_PUBLIC_API(void)
 JS_ReportErrorNumberASCIIVA(JSContext* cx, JSErrorCallback errorCallback,
                             void* userRef, const unsigned errorNumber, va_list ap);

JS_PUBLIC_API(void)
 JS_ReportErrorNumberLatin1(JSContext* cx, JSErrorCallback errorCallback,
                            void* userRef, const unsigned errorNumber, ...);

JS_PUBLIC_API(void)
 JS_ReportErrorNumberUTF8(JSContext* cx, JSErrorCallback errorCallback,
                            void* userRef, const unsigned errorNumber, ...);

JS_PUBLIC_API(void)
 JS_ReportErrorNumberUCArray(JSContext* cx, JSErrorCallback errorCallback,
                             void* userRef, const unsigned errorNumber,
                             const char16_t** args);

JS_PUBLIC_API(bool)
 JS_ReportWarningASCII(JSContext* cx, const char* format, ...)
     MOZ_FORMAT_PRINTF(2, 3);

JS_PUBLIC_API(bool)
 JS_ReportWarningLatin1(JSContext* cx, const char* format, ...)
     MOZ_FORMAT_PRINTF(2, 3);

JS_PUBLIC_API(bool)
 JS_ReportWarningUTF8(JSContext* cx, const char* format, ...)
     MOZ_FORMAT_PRINTF(2, 3);

JS_PUBLIC_API(bool)
 JS_ReportErrorFlagsAndNumberASCII(JSContext* cx, unsigned flags,
                                   JSErrorCallback errorCallback, void* userRef,
                                   const unsigned errorNumber, ...);

JS_PUBLIC_API(bool)
 JS_ReportErrorFlagsAndNumberLatin1(JSContext* cx, unsigned flags,
                                    JSErrorCallback errorCallback, void* userRef,
                                    const unsigned errorNumber, ...);

JS_PUBLIC_API(bool)
 JS_ReportErrorFlagsAndNumberUTF8(JSContext* cx, unsigned flags,
                                  JSErrorCallback errorCallback, void* userRef,
                                  const unsigned errorNumber, ...);

JS_PUBLIC_API(bool)
 JS_ReportErrorFlagsAndNumberUC(JSContext* cx, unsigned flags,
                                JSErrorCallback errorCallback, void* userRef,
                                const unsigned errorNumber, ...);

JS_PUBLIC_API(void)
 JS_ReportOutOfMemory(JSContext* cx);

JS_PUBLIC_API(void)
 JS_ReportAllocationOverflow(JSContext* cx);

JS_PUBLIC_API(JSObject*)
 JS_NewRegExpObject(JSContext* cx, const char* bytes, size_t length, unsigned flags);

JS_PUBLIC_API(JSObject*)
 JS_NewUCRegExpObject(JSContext* cx, const char16_t* chars, size_t length, unsigned flags);

JS_PUBLIC_API(bool)
 JS_SetRegExpInput(JSContext* cx, JS::HandleObject obj, JS::HandleString input);

JS_PUBLIC_API(bool)
 JS_ClearRegExpStatics(JSContext* cx, JS::HandleObject obj);

JS_PUBLIC_API(bool)
 JS_ExecuteRegExp(JSContext* cx, JS::HandleObject obj, JS::HandleObject reobj,
                  char16_t* chars, size_t length, size_t* indexp, bool test,
                  JS::MutableHandleValue rval);

JS_PUBLIC_API(bool)
 JS_ExecuteRegExpNoStatics(JSContext* cx, JS::HandleObject reobj, char16_t* chars, size_t length,
                           size_t* indexp, bool test, JS::MutableHandleValue rval);

JS_PUBLIC_API(bool)
 JS_ObjectIsRegExp(JSContext* cx, JS::HandleObject obj, bool* isRegExp);

JS_PUBLIC_API(unsigned)
 JS_GetRegExpFlags(JSContext* cx, JS::HandleObject obj);

JS_PUBLIC_API(JSString*)
 JS_GetRegExpSource(JSContext* cx, JS::HandleObject obj);

JS_PUBLIC_API(JSExceptionState*)
 JS_SaveExceptionState(JSContext* cx);

JS_PUBLIC_API(void)
 JS_RestoreExceptionState(JSContext* cx, JSExceptionState* state);

JS_PUBLIC_API(void)
 JS_DropExceptionState(JSContext* cx, JSExceptionState* state);

JS_PUBLIC_API(JSObject*)
 ExceptionStackOrNull(JS::HandleObject obj);

JS_PUBLIC_API(bool)
 JS_ThrowStopIteration(JSContext* cx);

JS_PUBLIC_API(bool)
 JS_IsStopIteration(const JS::Value& v);

JS_PUBLIC_API(void)
 JS_AbortIfWrongThread(JSContext* cx);

JS_PUBLIC_API(JSObject*)
 JS_NewObjectForConstructor(JSContext* cx, const JSClass* clasp, const JS::CallArgs& args);

JS_PUBLIC_API(void)
 JS_SetParallelParsingEnabled(JSContext* cx, bool enabled);

JS_PUBLIC_API(void)
 JS_SetOffthreadIonCompilationEnabled(JSContext* cx, bool enabled);

JS_PUBLIC_API(void)
 JS_SetGlobalJitCompilerOption(JSContext* cx, JSJitCompilerOption opt, uint32_t value);

JS_PUBLIC_API(bool)
 JS_GetGlobalJitCompilerOption(JSContext* cx, JSJitCompilerOption opt, uint32_t* valueOut);

JS_PUBLIC_API(bool)
 JS_IndexToId(JSContext* cx, uint32_t index, JS::MutableHandleId);

JS_PUBLIC_API(bool)
 JS_CharsToId(JSContext* cx, JS::TwoByteChars chars, JS::MutableHandleId);

JS_PUBLIC_API(bool)
 JS_IsIdentifier(JSContext* cx, JS::HandleString str, bool* isIdentifier);

JS_PUBLIC_API(void*)
JS_SetCompileOptionsFileAndLine(const char* f, unsigned l);

JS_FRIEND_API(js::Scalar::Type)
 JS_GetSharedArrayBufferViewType(JSObject* obj);

JS_FRIEND_API(bool)
 JS_IsArrayBufferViewObject(JSObject* obj);

JS_FRIEND_API(bool)
 JS_DetachArrayBuffer(JSContext* cx, JS::HandleObject obj);

JS_FRIEND_API(bool)
 JS_IsDetachedArrayBufferObject(JSObject* obj);
*/
#endif
