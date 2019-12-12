#ifndef SynSM_h
#define SynSM_h

#include <jsapi.h>
#include <jsfriendapi.h>

extern "C" JS_PUBLIC_API(bool) SM_Initialize(void);

extern "C" JS_PUBLIC_API(void) SM_DisableExtraThreads(void);

extern "C" JS_PUBLIC_API(void) SM_ShutDown(void);

extern "C" JS_PUBLIC_API(int64_t) SM_Now(void);

extern "C" JS_PUBLIC_API(JSString*) SM_GetEmptyString(JSContext* cx);

extern "C" JS_PUBLIC_API(JSType) SM_TypeOfValue(JSContext* cx, JS::Handle<JS::Value> v);

extern "C" JS_PUBLIC_API(void) SM_BeginRequest(JSContext* cx);

extern "C" JS_PUBLIC_API(void) SM_EndRequest(JSContext* cx);

extern "C" JS_PUBLIC_API(JSContext*) SM_NewContext(
    uint32_t maxbytes, uint32_t maxNurseryBytes = JS::DefaultNurseryBytes, JSContext* parentContext = nullptr);

extern "C" JS_PUBLIC_API(bool) SM_InitSelfHostedCode(JSContext* cx);

extern "C" JS_PUBLIC_API(void) SM_DestroyContext(JSContext* cx);

extern "C" JS_PUBLIC_API(void*) SM_GetContextPrivate(JSContext* cx);

extern "C" JS_PUBLIC_API(void) SM_SetContextPrivate(JSContext* cx, void* data);

extern "C" JS_PUBLIC_API(bool) SM_WrapObject(JSContext* cx, JS::MutableHandleObject objp);

extern "C" JS_PUBLIC_API(JSCompartment*) SM_EnterCompartment(JSContext* cx, JSObject* target);

extern "C" JS_PUBLIC_API(void) SM_LeaveCompartment(JSContext* cx, JSCompartment* oldCompartment);

extern "C" JS_PUBLIC_API(bool) SM_InitStandardClasses(JSContext* cx, JS::Handle<JSObject*> obj);

extern "C" JS_PUBLIC_API(JSObject*) SM_CurrentGlobalOrNull(JSContext* cx);

extern "C" JS_PUBLIC_API(bool) SM_InitReflectParse(JSContext* cx, JS::HandleObject global);

extern "C" JS_PUBLIC_API(bool) SM_InitCTypesClass(JSContext* cx, JS::HandleObject global);

extern "C" JS_PUBLIC_API(bool) SM_DefineDebuggerObject(JSContext* cx, JS::HandleObject obj);

extern "C" JS_PUBLIC_API(void) SM_GC(JSContext* cx);

extern "C" JS_PUBLIC_API(void) SM_MaybeGC(JSContext* cx);

extern "C" JS_PUBLIC_API(void) SM_SetGCParameter(JSContext* cx, JSGCParamKey key, uint32_t value);

extern "C" JS_PUBLIC_API(uint32_t) SM_GetGCParameter(JSContext* cx, JSGCParamKey key);

extern "C" JS_PUBLIC_API(void) SM_SetGCParametersBasedOnAvailableMemory(JSContext* cx, uint32_t availMem);

extern "C" JS_PUBLIC_API(JSString*) SM_NewExternalString(
    JSContext* cx, const char16_t* chars, size_t length, const JSStringFinalizer* fin);

extern "C" JS_PUBLIC_API(void) SM_SetNativeStackQuota(
    JSContext* cx, size_t systemCodeStackSize, size_t trustedScriptStackSize = 0, size_t untrustedScriptStackSize = 0);

extern "C" JS_PUBLIC_API(bool) SM_ValueToId(JSContext* cx, JS::HandleValue v, JS::MutableHandleId idp);

extern "C" JS_PUBLIC_API(bool) SM_IdToValue(JSContext* cx, jsid id, JS::MutableHandle<JS::Value> vp);

extern "C" JS_PUBLIC_API(JSString*) SM_ValueToSource(JSContext* cx, JS::Handle<JS::Value> v);

extern "C" JS_PUBLIC_API(JSObject*) SM_InitClass(
    JSContext* cx, JS::HandleObject obj, JS::HandleObject parent_proto,
    const JSClass* clasp, JSNative constructor, unsigned nargs,
    const JSPropertySpec* ps, const JSFunctionSpec* fs,
    const JSPropertySpec* static_ps, const JSFunctionSpec* static_fs);

extern "C" JS_PUBLIC_API(const JSClass*) SM_GetClass(JSObject* obj);

extern "C" JS_PUBLIC_API(bool) SM_HasInstance(
    JSContext* cx, JS::Handle<JSObject*> obj, JS::Handle<JS::Value> v, bool* bp);

extern "C" JS_PUBLIC_API(void*) SM_GetPrivate(JSObject* obj);

extern "C" JS_PUBLIC_API(void) SM_SetPrivate(JSObject* obj, void* data);

extern "C" JS_PUBLIC_API(JSObject*) SM_GetConstructor(JSContext* cx, JS::Handle<JSObject*> proto);

extern "C" JS_PUBLIC_API(void*) SM_GetInstancePrivate(
    JSContext* cx, JS::Handle<JSObject*> obj, const JSClass* clasp, JS::CallArgs* args);

extern "C" JS_PUBLIC_API(JSObject*) SM_NewGlobalObject(
    JSContext* cx, const JSClass* clasp, JSPrincipals* principals,
    JS::OnNewGlobalHookOption hookOption, const JS::CompartmentOptions& options);

extern "C" JS_PUBLIC_API(void) SM_GlobalObjectTraceHook(JSTracer* trc, JSObject* global);

extern "C" JS_PUBLIC_API(JSObject*) SM_NewObject(JSContext* cx, const JSClass* clasp);

extern "C" JS_PUBLIC_API(JSObject*) SM_NewObjectWithGivenProto(
    JSContext* cx, const JSClass* clasp, JS::Handle<JSObject*> proto);

extern "C" JS_PUBLIC_API(bool) SM_GetPrototype(
    JSContext* cx, JS::HandleObject obj, JS::MutableHandleObject result);

extern "C" JS_PUBLIC_API(bool) SM_SetPrototype(
    JSContext* cx, JS::HandleObject obj, JS::HandleObject proto);

extern "C" JS_PUBLIC_API(bool) SM_DefinePropertyById(
    JSContext* cx, JS::HandleObject obj, JS::HandleId id, JS::HandleValue value,
    unsigned attrs, JSNative getter = nullptr, JSNative setter = nullptr);

extern "C" JS_PUBLIC_API(bool) SM_DefineProperty(
    JSContext* cx, JS::HandleObject obj, const char* name, JS::HandleValue value,
    unsigned attrs, JSNative getter = nullptr, JSNative setter = nullptr);

extern "C" JS_PUBLIC_API(bool) SM_DefineUCProperty(
    JSContext* cx, JS::HandleObject obj, const char16_t* name, size_t namelen,
    JS::HandleValue value, unsigned attrs, JSNative getter = nullptr, JSNative setter = nullptr);

extern "C" JS_PUBLIC_API(bool) SM_HasProperty(
    JSContext* cx, JS::HandleObject obj, const char* name, bool* foundp);

extern "C" JS_PUBLIC_API(bool) SM_HasUCProperty(
    JSContext* cx, JS::HandleObject obj, const char16_t* name, size_t namelen, bool* vp);

extern "C" JS_PUBLIC_API(bool) SM_GetPropertyById(
    JSContext* cx, JS::HandleObject obj, JS::HandleId id, JS::MutableHandleValue vp);

extern "C" JS_PUBLIC_API(bool) SM_GetProperty(
    JSContext* cx, JS::HandleObject obj, const char* name, JS::MutableHandleValue vp);

extern "C" JS_PUBLIC_API(bool) SM_GetUCProperty(
    JSContext* cx, JS::HandleObject obj, const char16_t* name, size_t namelen, JS::MutableHandleValue vp);

extern "C" JS_PUBLIC_API(bool) SM_GetElement(
    JSContext* cx, JS::HandleObject obj, uint32_t index, JS::MutableHandleValue vp);

extern "C" JS_PUBLIC_API(bool) SM_SetProperty(
    JSContext* cx, JS::HandleObject obj, const char* name, JS::HandleValue v);

extern "C" JS_PUBLIC_API(bool) SM_SetUCProperty(
    JSContext* cx, JS::HandleObject obj, const char16_t* name, size_t namelen, JS::HandleValue v);

extern "C" JS_PUBLIC_API(bool) SM_SetElement(
    JSContext* cx, JS::HandleObject obj, uint32_t index, JS::HandleValue v);

extern "C" JS_PUBLIC_API(bool) SM_DeletePropertyById(
    JSContext* cx, JS::HandleObject obj, JS::HandleId id, JS::ObjectOpResult& result);

extern "C" JS_PUBLIC_API(bool) SM_DeleteElement(
    JSContext* cx, JS::HandleObject obj, uint32_t index, JS::ObjectOpResult& result);

extern "C" JS_PUBLIC_API(JS::AutoIdVector*) SM_EnumerateToAutoIdVector(
    JSContext* cx, JS::HandleObject obj, size_t* length, jsid** data);

extern "C" JS_PUBLIC_API(void) SM_DestroyAutoIdVector(JS::AutoIdVector* v);

extern "C" JS_PUBLIC_API(bool) SM_CallFunctionValue(
    JSContext* cx, JS::HandleObject obj, JS::HandleValue fval,
    const JS::HandleValueArray& args, JS::MutableHandleValue rval);

extern "C" JS_PUBLIC_API(bool) SM_CallFunction(
    JSContext* cx, JS::HandleObject obj, JS::HandleFunction fun,
    const JS::HandleValueArray& args, JS::MutableHandleValue rval);

extern "C" JS_PUBLIC_API(bool) SM_CallFunctionName(
    JSContext* cx, JS::HandleObject obj, const char* name,
    const JS::HandleValueArray& args, JS::MutableHandleValue rval);

extern "C" JS_PUBLIC_API(JSObject*) SM_New(
    JSContext* cx, JS::HandleObject ctor, const JS::HandleValueArray& args);

extern "C" JS_PUBLIC_API(bool) SM_DefineProperties(
    JSContext* cx, JS::HandleObject obj, const JSPropertySpec* ps);

extern "C" JS_PUBLIC_API(bool) SM_AlreadyHasOwnUCProperty(
    JSContext* cx, JS::HandleObject obj, const char16_t* name,
    size_t namelen, bool* foundp);

extern "C" JS_PUBLIC_API(JSObject*) SM_NewArrayObject(JSContext* cx, size_t length);

extern "C" JS_PUBLIC_API(JSObject*) SM_NewArrayObject2(
    JSContext* cx, const JS::HandleValueArray& contents);

extern "C" JS_PUBLIC_API(bool) SM_IsArrayObject(
    JSContext* cx, JS::HandleObject obj, bool* isArray);

extern "C" JS_PUBLIC_API(bool) SM_GetArrayLength(
    JSContext* cx, JS::Handle<JSObject*> obj, uint32_t* lengthp);

extern "C" JS_PUBLIC_API(uint64_t) SM_GetReservedSlot(JSObject* obj, uint32_t index);

extern "C" JS_PUBLIC_API(void) SM_SetReservedSlot(
    JSObject* obj, uint32_t index, const JS::Value& v);

extern "C" JS_PUBLIC_API(JSFunction*) SM_NewFunction(
    JSContext* cx, JSNative call, unsigned nargs, unsigned flags, const char* name);

extern "C" JS_PUBLIC_API(JSString*) SM_GetFunctionId(JSFunction* fun);

extern "C" JS_PUBLIC_API(bool) SM_ObjectIsFunction(JSContext* cx, JSObject* obj);

extern "C" JS_PUBLIC_API(bool) SM_DefineFunctions(
    JSContext* cx, JS::Handle<JSObject*> obj, const JSFunctionSpec* fs);

extern "C" JS_PUBLIC_API(JSFunction*) SM_DefineFunction(
    JSContext* cx, JS::Handle<JSObject*> obj, const char* name, JSNative call,
    unsigned nargs, unsigned attrs);

extern "C" JS_PUBLIC_API(JSFunction*) SM_DefineUCFunction(
    JSContext* cx, JS::Handle<JSObject*> obj,
    const char16_t* name, size_t namelen, JSNative call,
    unsigned nargs, unsigned attrs);

extern "C" JS_PUBLIC_API(bool) SM_CompileScript(
    JSContext* cx, const char* ascii, size_t length,
    const JS::CompileOptions& options, JS::MutableHandleScript script);

extern "C" JS_PUBLIC_API(bool) SM_CompileUCScript(
    JSContext* cx, const char16_t* chars, size_t length,
    const JS::CompileOptions& options, JS::MutableHandleScript script);

extern "C" JS_PUBLIC_API(JSString*) SM_DecompileFunction(
    JSContext* cx, JS::Handle<JSFunction*> fun, unsigned indent);

extern "C" JS_PUBLIC_API(bool) SM_ExecuteScript(
    JSContext* cx, JS::HandleScript script, JS::MutableHandleValue rval);

extern "C" JS_PUBLIC_API(bool) SM_CheckForInterrupt(JSContext* cx);

extern "C" JS_PUBLIC_API(bool) SM_AddInterruptCallback(
    JSContext* cx, JSInterruptCallback callback);

extern "C" JS_PUBLIC_API(bool) SM_DisableInterruptCallback(JSContext* cx);

extern "C" JS_PUBLIC_API(void) SM_ResetInterruptCallback(
    JSContext* cx, bool enable);

extern "C" JS_PUBLIC_API(void) SM_RequestInterruptCallback(JSContext* cx);

extern "C" JS_PUBLIC_API(bool) SM_IsRunning(JSContext* cx);

extern "C" JS_PUBLIC_API(JSString*) SM_NewStringCopyN(
    JSContext* cx, const char* s, size_t n);

extern "C" JS_PUBLIC_API(JSString*) SM_NewStringCopyUTF8Z(
    JSContext* cx, const JS::ConstUTF8CharsZ s);

extern "C" JS_PUBLIC_API(JS::Value) SM_GetEmptyStringValue(JSContext* cx);

extern "C" JS_PUBLIC_API(JSString*) SM_NewUCStringCopyN(
    JSContext* cx, const char16_t* s, size_t n);

extern "C" JS_PUBLIC_API(size_t) SM_GetStringLength(JSString* str);

extern "C" JS_PUBLIC_API(bool) SM_StringHasLatin1Chars(JSString* str);

extern "C" JS_PUBLIC_API(const JS::Latin1Char*) SM_GetLatin1StringCharsAndLength(
    JSContext* cx, const JS::AutoCheckCannotGC& nogc, JSString* str, size_t* length);

extern "C" JS_PUBLIC_API(const char16_t*) SM_GetTwoByteStringCharsAndLength(
    JSContext* cx, const JS::AutoCheckCannotGC& nogc, JSString* str, size_t* length);

extern "C" JS_PUBLIC_API(bool) SM_Stringify(
    JSContext* cx, JS::MutableHandleValue value, JS::HandleObject replacer,
    JS::HandleValue space, JSONWriteCallback callback, void* data);

extern "C" JS_PUBLIC_API(bool) SM_ParseJSON(
    JSContext* cx, const char16_t* chars, uint32_t len, JS::MutableHandleValue vp);

extern "C" JS_PUBLIC_API(void) SM_ReportErrorASCII(
    JSContext* cx, const char* format, ...);

extern "C" JS_PUBLIC_API(void) SM_ReportErrorNumberUC(
    JSContext* cx, JSErrorCallback errorCallback, void* userRef, const unsigned errorNumber, ...);

extern "C" JS_PUBLIC_API(void) SM_ReportOutOfMemory(JSContext* cx);

extern "C" JS_PUBLIC_API(JS::WarningReporter) SM_GetWarningReporter(JSContext* cx);

extern "C" JS_PUBLIC_API(JS::WarningReporter) SM_SetWarningReporter(JSContext* cx, JS::WarningReporter reporter);

extern "C" JS_PUBLIC_API(JSObject*) SM_NewDateObject(
    JSContext* cx, int year, int mon, int mday, int hour, int min, int sec);

extern "C" JS_PUBLIC_API(JSObject*) SM_NewDateObjectMsec(JSContext* cx, double msec);

extern "C" JS_PUBLIC_API(bool) SM_ObjectIsDate(JSContext* cx, JS::HandleObject obj, bool* isDate);

extern "C" JS_PUBLIC_API(bool) SM_IsExceptionPending(JSContext* cx);

extern "C" JS_PUBLIC_API(bool) SM_GetPendingException(JSContext* cx, JS::MutableHandleValue vp);

extern "C" JS_PUBLIC_API(void) SM_SetPendingException(JSContext* cx, JS::HandleValue v);

extern "C" JS_PUBLIC_API(void) SM_ClearPendingException(JSContext* cx);

extern "C" JS_PUBLIC_API(JSErrorReport*) SM_ErrorFromException(JSContext* cx, JS::HandleObject obj);

extern "C" JS_PUBLIC_API(void*) SM_GetContextOptions(JSContext* cx);

extern "C" JS_PUBLIC_API(void*) SM_NewRootedValue(JSContext* cx, uint64_t initial);

extern "C" JS_PUBLIC_API(void) SM_FreeRooteValue(void* val);

extern "C" JS_PUBLIC_API(void*) SM_NewRootedObject(JSContext* cx, JSObject* initial);

extern "C" JS_PUBLIC_API(void) SM_FreeRootedObject(void* obj);

extern "C" JS_PUBLIC_API(void*) SM_NewRootedString(JSContext* cx, JSString* initial);

extern "C" JS_PUBLIC_API(void) SM_FreeRootedString(void* str);

extern "C" JS_PUBLIC_API(void*) SM_NewCompileOptions(JSContext* cx);

extern "C" JS_PUBLIC_API(void) SM_SetCompileOptionsFileLineAndUtf8(
    JS::CompileOptions& options, const char* f, unsigned l, bool isUtf8);

extern "C" JS_PUBLIC_API(void) SM_FreeCompileOptions(void* co);

extern "C" JS_PUBLIC_API(bool) SM_EvaluateScript(
    JSContext* cx, const JS::CompileOptions& options,
	const char* bytes, size_t length, JS::MutableHandleValue rval);

extern "C" JS_PUBLIC_API(bool) SM_EvaluateUCScript(
    JSContext* cx, const JS::CompileOptions& options,
	const char16_t* chars, size_t length, JS::MutableHandleValue rval);

extern "C" JS_PUBLIC_API(JS::Value) SM_ComputeThis(JSContext* cx, JS::Value* vp);

// Array functions from jsfriendsapi.h

extern "C" JS_FRIEND_API(JSObject*) SM_NewInt8Array(JSContext* cx, uint32_t nelements);

extern "C" JS_FRIEND_API(JSObject*) SM_NewUint8Array(JSContext* cx, uint32_t nelements);

extern "C" JS_FRIEND_API(JSObject*) SM_NewUint8ClampedArray(JSContext* cx, uint32_t nelements);

extern "C" JS_FRIEND_API(JSObject*) SM_NewInt16Array(JSContext* cx, uint32_t nelements);

extern "C" JS_FRIEND_API(JSObject*) SM_NewUint16Array(JSContext* cx, uint32_t nelements);

extern "C" JS_FRIEND_API(JSObject*) SM_NewInt32Array(JSContext* cx, uint32_t nelements);

extern "C" JS_FRIEND_API(JSObject*) SM_NewUint32Array(JSContext* cx, uint32_t nelements);

extern "C" JS_FRIEND_API(JSObject*) SM_NewFloat32Array(JSContext* cx, uint32_t nelements);

extern "C" JS_FRIEND_API(JSObject*) SM_NewFloat64Array(JSContext* cx, uint32_t nelements);

extern "C" JS_FRIEND_API(JSObject*) SM_NewInt8ArrayFromArray(JSContext* cx, JS::HandleObject array);

extern "C" JS_FRIEND_API(JSObject*) SM_NewUint8ArrayFromArray(JSContext* cx, JS::HandleObject array);

extern "C" JS_FRIEND_API(JSObject*) SM_NewUint8ClampedArrayFromArray(JSContext* cx, JS::HandleObject array);

extern "C" JS_FRIEND_API(JSObject*) SM_NewInt16ArrayFromArray(JSContext* cx, JS::HandleObject array);

extern "C" JS_FRIEND_API(JSObject*) SM_NewUint16ArrayFromArray(JSContext* cx, JS::HandleObject array);

extern "C" JS_FRIEND_API(JSObject*) SM_NewInt32ArrayFromArray(JSContext* cx, JS::HandleObject array);

extern "C" JS_FRIEND_API(JSObject*) SM_NewUint32ArrayFromArray(JSContext* cx, JS::HandleObject array);

extern "C" JS_FRIEND_API(JSObject*) SM_NewFloat32ArrayFromArray(JSContext* cx, JS::HandleObject array);

extern "C" JS_FRIEND_API(JSObject*) SM_NewFloat64ArrayFromArray(JSContext* cx, JS::HandleObject array);

extern "C" JS_FRIEND_API(JSObject*) SM_NewInt8ArrayWithBuffer(
    JSContext* cx, JS::HandleObject arrayBuffer, uint32_t byteOffset, int32_t length);

extern "C" JS_FRIEND_API(JSObject*) SM_NewUint8ArrayWithBuffer(
    JSContext* cx, JS::HandleObject arrayBuffer, uint32_t byteOffset, int32_t length);

extern "C" JS_FRIEND_API(JSObject*) SM_NewUint8ClampedArrayWithBuffer(
    JSContext* cx, JS::HandleObject arrayBuffer, uint32_t byteOffset, int32_t length);

extern "C" JS_FRIEND_API(JSObject*) SM_NewInt16ArrayWithBuffer(
    JSContext* cx, JS::HandleObject arrayBuffer, uint32_t byteOffset, int32_t length);

extern "C" JS_FRIEND_API(JSObject*) SM_NewUint16ArrayWithBuffer(
    JSContext* cx, JS::HandleObject arrayBuffer, uint32_t byteOffset, int32_t length);

extern "C" JS_FRIEND_API(JSObject*) SM_NewInt32ArrayWithBuffer(
    JSContext* cx, JS::HandleObject arrayBuffer, uint32_t byteOffset, int32_t length);

extern "C" JS_FRIEND_API(JSObject*) SM_NewUint32ArrayWithBuffer(
    JSContext* cx, JS::HandleObject arrayBuffer, uint32_t byteOffset, int32_t length);

extern "C" JS_FRIEND_API(JSObject*) SM_NewFloat32ArrayWithBuffer(
    JSContext* cx, JS::HandleObject arrayBuffer, uint32_t byteOffset, int32_t length);

extern "C" JS_FRIEND_API(JSObject*) SM_NewFloat64ArrayWithBuffer(
    JSContext* cx, JS::HandleObject arrayBuffer, uint32_t byteOffset, int32_t length);

extern "C" JS_FRIEND_API(JSObject*) SM_NewSharedArrayBuffer(JSContext* cx, uint32_t nbytes);

extern "C" JS_FRIEND_API(JSObject*) SM_NewArrayBuffer(JSContext* cx, uint32_t nbytes);

extern "C" JS_FRIEND_API(bool) SM_IsTypedArrayObject(JSObject* obj);

extern "C" JS_FRIEND_API(bool) SM_IsArrayBufferViewObject(JSObject* obj);

extern "C" JS_FRIEND_API(bool) SM_IsInt8Array(JSObject* obj);

extern "C" JS_FRIEND_API(bool) SM_IsUint8Array(JSObject* obj);

extern "C" JS_FRIEND_API(bool) SM_IsUint8ClampedArray(JSObject* obj);

extern "C" JS_FRIEND_API(bool) SM_IsInt16Array(JSObject* obj);

extern "C" JS_FRIEND_API(bool) SM_IsUint16Array(JSObject* obj);

extern "C" JS_FRIEND_API(bool) SM_IsInt32Array(JSObject* obj);

extern "C" JS_FRIEND_API(bool) SM_IsUint32Array(JSObject* obj);

extern "C" JS_FRIEND_API(bool) SM_IsFloat32Array(JSObject* obj);

extern "C" JS_FRIEND_API(bool) SM_IsFloat64Array(JSObject* obj);

extern "C" JS_FRIEND_API(bool) SM_GetTypedArraySharedness(JSObject* obj);

extern "C" JS_FRIEND_API(JSObject*) SM_GetObjectAsInt8Array(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, int8_t** data);

extern "C" JS_FRIEND_API(JSObject*) SM_GetObjectAsUint8Array(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, uint8_t** data);

extern "C" JS_FRIEND_API(JSObject*) SM_GetObjectAsUint8ClampedArray(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, uint8_t** data);

extern "C" JS_FRIEND_API(JSObject*) SM_GetObjectAsInt16Array(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, int16_t** data);

extern "C" JS_FRIEND_API(JSObject*) SM_GetObjectAsUint16Array(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, uint16_t** data);

extern "C" JS_FRIEND_API(JSObject*) SM_GetObjectAsInt32Array(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, int32_t** data);

extern "C" JS_FRIEND_API(JSObject*) SM_GetObjectAsUint32Array(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, uint32_t** data);

extern "C" JS_FRIEND_API(JSObject*) SM_GetObjectAsFloat32Array(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, float** data);

extern "C" JS_FRIEND_API(JSObject*) SM_GetObjectAsFloat64Array(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, double** data);

extern "C" JS_FRIEND_API(JSObject*) SM_GetObjectAsArrayBufferView(
    JSObject* obj, uint32_t* length, bool* isSharedMemory, uint8_t** data);

extern "C" JS_FRIEND_API(JSObject*) SM_GetObjectAsArrayBuffer(
    JSObject* obj, uint32_t* length, uint8_t** data);

extern "C" JS_FRIEND_API(js::Scalar::Type) SM_GetArrayBufferViewType(JSObject* obj);

extern "C" JS_FRIEND_API(bool) SM_IsArrayBufferObject(JSObject* obj);

extern "C" JS_FRIEND_API(bool) SM_IsSharedArrayBufferObject(JSObject* obj);

extern "C" JS_FRIEND_API(uint32_t) SM_GetArrayBufferByteLength(JSObject* obj);

extern "C" JS_FRIEND_API(uint32_t) SM_GetSharedArrayBufferByteLength(JSObject* obj);

extern "C" JS_FRIEND_API(bool) SM_ArrayBufferHasData(JSObject* obj);

extern "C" JS_FRIEND_API(uint8_t*) SM_GetArrayBufferData(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC&);

extern "C" JS_FRIEND_API(bool) SM_IsMappedArrayBufferObject(JSObject* obj);

extern "C" JS_FRIEND_API(uint32_t) SM_GetTypedArrayLength(JSObject* obj);

extern "C" JS_FRIEND_API(uint32_t) SM_GetTypedArrayByteOffset(JSObject* obj);

extern "C" JS_FRIEND_API(uint32_t) SM_GetTypedArrayByteLength(JSObject* obj);

extern "C" JS_FRIEND_API(uint32_t) SM_GetArrayBufferViewByteLength(JSObject* obj);

extern "C" JS_FRIEND_API(int8_t*) SM_GetInt8ArrayData(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC&);

extern "C" JS_FRIEND_API(uint8_t*) SM_GetUint8ArrayData(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC&);

extern "C" JS_FRIEND_API(uint8_t*) SM_GetUint8ClampedArrayData(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC&);

extern "C" JS_FRIEND_API(int16_t*) SM_GetInt16ArrayData(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC&);

extern "C" JS_FRIEND_API(uint16_t*) SM_GetUint16ArrayData(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC&);

extern "C" JS_FRIEND_API(int32_t*) SM_GetInt32ArrayData(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC&);

extern "C" JS_FRIEND_API(uint32_t*) SM_GetUint32ArrayData(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC&);

extern "C" JS_FRIEND_API(float*) SM_GetFloat32ArrayData(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC&);

extern "C" JS_FRIEND_API(double*) SM_GetFloat64ArrayData(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC&);

extern "C" JS_FRIEND_API(void*) SM_GetArrayBufferViewData(
    JSObject* obj, bool* isSharedMemory, const JS::AutoCheckCannotGC&);

extern "C" JS_FRIEND_API(JSObject*) SM_GetArrayBufferViewBuffer(
    JSContext* cx, JS::HandleObject obj, bool* isSharedMemory);

extern "C" JS_PUBLIC_API(bool) SM_InitModuleClasses(
    JSContext* cx, JS::Handle<JSObject*> obj);

extern "C" JS_PUBLIC_API(JSObject*) SM_CompileModule(
    JSContext* cx, JS::Handle<JSObject*> obj, JS::CompileOptions& options,
	const char16_t* chars, size_t length);

extern "C" JS_PUBLIC_API(void) SM_SetModuleResolveHook(
    JSContext* cx, JS::Handle<JSFunction*> hook);







/*
extern "C" JS_PUBLIC_API(JS::Value) SM_GetNaNValue(JSContext* cx);

extern "C" JS_PUBLIC_API(JS::Value) SM_GetNegativeInfinityValue(JSContext* cx);

extern "C" JS_PUBLIC_API(JS::Value) SM_GetPositiveInfinityValue(JSContext* cx);

extern "C" JS_PUBLIC_API(bool) SM_ValueToObject(JSContext* cx, JS::HandleValue v, JS::MutableHandleObject objp);

extern "C" JS_PUBLIC_API(JSFunction*) SM_ValueToFunction(JSContext* cx, JS::HandleValue v);

extern "C" JS_PUBLIC_API(JSFunction*) SM_ValueToConstructor(JSContext* cx, JS::HandleValue v);

extern "C" JS_PUBLIC_API(bool) SM_StrictlyEqual(JSContext* cx, JS::Handle<JS::Value> v1, JS::Handle<JS::Value> v2, bool* equal);

extern "C" JS_PUBLIC_API(bool) SM_LooselyEqual(JSContext* cx, JS::Handle<JS::Value> v1, JS::Handle<JS::Value> v2, bool* equal);

extern "C" JS_PUBLIC_API(bool) SM_SameValue(JSContext* cx, JS::Handle<JS::Value> v1, JS::Handle<JS::Value> v2, bool* same);

extern "C" JS_PUBLIC_API(bool) SM_IsBuiltinEvalFunction(JSFunction* fun);

extern "C" JS_PUBLIC_API(bool) SM_IsBuiltinFunctionConstructor(JSFunction* fun);

extern "C" JS_PUBLIC_API(JSContext*) SM_GetParentContext(JSContext* cx);

extern "C" JS_PUBLIC_API(void) SM_SetFutexCanWait(JSContext* cx);

extern "C" JS_PUBLIC_API(JSVersion) SM_GetVersion(JSContext* cx);

extern "C" JS_PUBLIC_API(const char*) SM_VersionToString(JSVersion version);

extern "C" JS_PUBLIC_API(JSVersion) SM_StringToVersion(const char* string);

extern "C" JS_PUBLIC_API(const char*) SM_GetImplementationVersion(void);

extern "C" JS_PUBLIC_API(void) SM_SetDestroyCompartmentCallback(JSContext* cx, JSDestroyCompartmentCallback callback);

extern "C" JS_PUBLIC_API(void) SM_SetSizeOfIncludingThisCompartmentCallback(
    JSContext* cx, JSSizeOfIncludingThisCompartmentCallback callback);

extern "C" JS_PUBLIC_API(void) SM_SetDestroyZoneCallback(JSContext* cx, JSZoneCallback callback);

extern "C" JS_PUBLIC_API(void) SM_SetSweepZoneCallback(JSContext* cx, JSZoneCallback callback);

extern "C" JS_PUBLIC_API(void) SM_SetCompartmentNameCallback(JSContext* cx, JSCompartmentNameCallback callback);

extern "C" JS_PUBLIC_API(void) SM_SetWrapObjectCallbacks(JSContext* cx, const JSWrapObjectCallbacks* callbacks);

extern "C" JS_PUBLIC_API(void) SM_SetCompartmentPrivate(JSCompartment* compartment, void* data);

extern "C" JS_PUBLIC_API(void*) SM_GetCompartmentPrivate(JSCompartment* compartment);

extern "C" JS_PUBLIC_API(void) SM_SetZoneUserData(JS::Zone* zone, void* data);

extern "C" JS_PUBLIC_API(void*) SM_GetZoneUserData(JS::Zone* zone);








extern "C" JS_PUBLIC_API(bool)
 JS_WrapValue(JSContext* cx, JS::MutableHandleValue vp);

extern "C" JS_PUBLIC_API(JSObject*)
 JS_TransplantObject(JSContext* cx, JS::HandleObject origobj, JS::HandleObject target);

extern "C" JS_PUBLIC_API(bool)
 JS_RefreshCrossCompartmentWrappers(JSContext* cx, JS::Handle<JSObject*> obj);

extern "C" JS_PUBLIC_API(void)
 JS_IterateCompartments(JSContext* cx, void* data,
                        JSIterateCompartmentCallback compartmentCallback);

extern "C" JS_PUBLIC_API(bool)
 JS_ResolveStandardClass(JSContext* cx, JS::HandleObject obj, JS::HandleId id, bool* resolved);

extern "C" JS_PUBLIC_API(bool)
 JS_MayResolveStandardClass(const JSAtomState& names, jsid id, JSObject* maybeObj);

extern "C" JS_PUBLIC_API(bool)
 JS_EnumerateStandardClasses(JSContext* cx, JS::HandleObject obj);

extern "C" JS_PUBLIC_API(bool)
 JS_GetClassObject(JSContext* cx, JSProtoKey key, JS::MutableHandle<JSObject*> objp);

extern "C" JS_PUBLIC_API(bool)
 JS_GetClassPrototype(JSContext* cx, JSProtoKey key, JS::MutableHandle<JSObject*> objp);

extern "C" JS_PUBLIC_API(JSProtoKey)
 JS_IdToProtoKey(JSContext* cx, JS::HandleId id);

extern "C" JS_PUBLIC_API(JSObject*)
 JS_GetFunctionPrototype(JSContext* cx, JS::HandleObject forObj);

extern "C" JS_PUBLIC_API(JSObject*)
 JS_GetObjectPrototype(JSContext* cx, JS::HandleObject forObj);

extern "C" JS_PUBLIC_API(JSObject*)
 JS_GetArrayPrototype(JSContext* cx, JS::HandleObject forObj);

extern "C" JS_PUBLIC_API(JSObject*)
 JS_GetErrorPrototype(JSContext* cx);

extern "C" JS_PUBLIC_API(JSObject*)
 JS_GetIteratorPrototype(JSContext* cx);

extern "C" JS_PUBLIC_API(JSObject*)
 JS_GetGlobalForObject(JSContext* cx, JSObject* obj);

extern "C" JS_PUBLIC_API(bool)
 JS_IsGlobalObject(JSObject* obj);

extern "C" JS_PUBLIC_API(JSObject*)
 JS_GlobalLexicalEnvironment(JSObject* obj);

extern "C" JS_PUBLIC_API(bool)
 JS_HasExtensibleLexicalEnvironment(JSObject* obj);

extern "C" JS_PUBLIC_API(JSObject*)
 JS_ExtensibleLexicalEnvironment(JSObject* obj);

extern "C" JS_PUBLIC_API(JSObject*)
 JS_GetGlobalForCompartmentOrNull(JSContext* cx, JSCompartment* c);

extern "C" JS_PUBLIC_API(bool)
 JS_DefineProfilingFunctions(JSContext* cx, JS::HandleObject obj);

extern "C" JS_PUBLIC_API(void)
 JS_SetCTypesCallbacks(JSObject* ctypesObj, const JSCTypesCallbacks* callbacks);

extern "C" JS_PUBLIC_API(void*)
 JS_malloc(JSContext* cx, size_t nbytes);

extern "C" JS_PUBLIC_API(void*)
 JS_realloc(JSContext* cx, void* p, size_t oldBytes, size_t newBytes);

extern "C" JS_PUBLIC_API(void)
 JS_free(JSContext* cx, void* p);

extern "C" JS_PUBLIC_API(void)
 JS_freeop(JSFreeOp* fop, void* p);

extern "C" JS_PUBLIC_API(void)
 JS_updateMallocCounter(JSContext* cx, size_t nbytes);

extern "C" JS_PUBLIC_API(char*)
 JS_strdup(JSContext* cx, const char* s);

extern "C" JS_PUBLIC_API(bool)
 JS_AddExtraGCRootsTracer(JSContext* cx, JSTraceDataOp traceOp, void* data);

extern "C" JS_PUBLIC_API(void)
 JS_RemoveExtraGCRootsTracer(JSContext* cx, JSTraceDataOp traceOp, void* data);

extern "C" JS_PUBLIC_API(void)
 JS_SetGCCallback(JSContext* cx, JSGCCallback cb, void* data);

extern "C" JS_PUBLIC_API(void)
 JS_SetObjectsTenuredCallback(JSContext* cx, JSObjectsTenuredCallback cb,
                              void* data);

extern "C" JS_PUBLIC_API(bool)
 JS_AddFinalizeCallback(JSContext* cx, JSFinalizeCallback cb, void* data);

extern "C" JS_PUBLIC_API(void)
 JS_RemoveFinalizeCallback(JSContext* cx, JSFinalizeCallback cb);

extern "C" JS_PUBLIC_API(bool)
 JS_AddWeakPointerZoneGroupCallback(JSContext* cx, JSWeakPointerZoneGroupCallback cb, void* data);

extern "C" JS_PUBLIC_API(void)
 JS_RemoveWeakPointerZoneGroupCallback(JSContext* cx, JSWeakPointerZoneGroupCallback cb);

extern "C" JS_PUBLIC_API(bool)
 JS_AddWeakPointerCompartmentCallback(JSContext* cx, JSWeakPointerCompartmentCallback cb,
                                      void* data);

extern "C" JS_PUBLIC_API(void)
 JS_RemoveWeakPointerCompartmentCallback(JSContext* cx, JSWeakPointerCompartmentCallback cb);

extern "C" JS_PUBLIC_API(void)
 JS_UpdateWeakPointerAfterGC(JS::Heap<JSObject*>* objp);

extern "C" JS_PUBLIC_API(void)
 JS_UpdateWeakPointerAfterGCUnbarriered(JSObject** objp);

extern "C" JS_PUBLIC_API(bool)
 JS_IsExternalString(JSString* str);

extern "C" JS_PUBLIC_API(const JSStringFinalizer*)
 JS_GetExternalStringFinalizer(JSString* str);

extern "C" JS_PUBLIC_API(void)
 JS_SetNativeStackQuota(JSContext* cx, size_t systemCodeStackSize,
                        size_t trustedScriptStackSize = 0,
                        size_t untrustedScriptStackSize = 0);

extern "C" JS_PUBLIC_API(bool)
 JS_StringToId(JSContext* cx, JS::HandleString s, JS::MutableHandleId idp);

extern "C" JS_PUBLIC_API(bool)
 JS_PropertyStub(JSContext* cx, JS::HandleObject obj, JS::HandleId id,
                 JS::MutableHandleValue vp);

extern "C" JS_PUBLIC_API(bool)
 JS_StrictPropertyStub(JSContext* cx, JS::HandleObject obj, JS::HandleId id,
                       JS::MutableHandleValue vp, JS::ObjectOpResult& result);

extern "C" JS_PUBLIC_API(bool)
 JS_LinkConstructorAndPrototype(JSContext* cx, JS::Handle<JSObject*> ctor,
                                JS::Handle<JSObject*> proto);

extern "C" JS_PUBLIC_API(bool)
 JS_InstanceOf(JSContext* cx, JS::Handle<JSObject*> obj, const JSClass* clasp, JS::CallArgs* args);

extern "C" JS_PUBLIC_API(void)
 JS_FireOnNewGlobalObject(JSContext* cx, JS::HandleObject global);

extern "C" JS_PUBLIC_API(bool)
 JS_IsNative(JSObject* obj);

extern "C" JS_PUBLIC_API(JSObject*)
 JS_NewPlainObject(JSContext* cx);

extern "C" JS_PUBLIC_API(bool)
 JS_DeepFreezeObject(JSContext* cx, JS::Handle<JSObject*> obj);

extern "C" JS_PUBLIC_API(bool)
 JS_FreezeObject(JSContext* cx, JS::Handle<JSObject*> obj);

extern "C" JS_PUBLIC_API(bool)
 JS_GetPrototypeIfOrdinary(JSContext* cx, JS::HandleObject obj, bool* isOrdinary,
                           JS::MutableHandleObject result);

extern "C" JS_PUBLIC_API(bool)
 JS_IsExtensible(JSContext* cx, JS::HandleObject obj, bool* extensible);

extern "C" JS_PUBLIC_API(bool)
 JS_PreventExtensions(JSContext* cx, JS::HandleObject obj, JS::ObjectOpResult& result);

extern "C" JS_PUBLIC_API(bool)
 JS_SetImmutablePrototype(JSContext* cx, JS::HandleObject obj, bool* succeeded);

extern "C" JS_PUBLIC_API(bool)
 JS_GetOwnPropertyDescriptorById(JSContext* cx, JS::HandleObject obj, JS::HandleId id,
                                 JS::MutableHandle<JS::PropertyDescriptor> desc);

extern "C" JS_PUBLIC_API(bool)
 JS_GetOwnPropertyDescriptor(JSContext* cx, JS::HandleObject obj, const char* name,
                             JS::MutableHandle<JS::PropertyDescriptor> desc);

extern "C" JS_PUBLIC_API(bool)
 JS_GetOwnUCPropertyDescriptor(JSContext* cx, JS::HandleObject obj, const char16_t* name,
                               JS::MutableHandle<JS::PropertyDescriptor> desc);

extern "C" JS_PUBLIC_API(bool)
 JS_GetPropertyDescriptorById(JSContext* cx, JS::HandleObject obj, JS::HandleId id,
                              JS::MutableHandle<JS::PropertyDescriptor> desc);

extern "C" JS_PUBLIC_API(bool)
 JS_GetPropertyDescriptor(JSContext* cx, JS::HandleObject obj, const char* name,
                          JS::MutableHandle<JS::PropertyDescriptor> desc);

extern "C" JS_PUBLIC_API(bool)
 JS_DefineElement(JSContext* cx, JS::HandleObject obj, uint32_t index, JS::HandleValue value,
                  unsigned attrs, JSNative getter = nullptr, JSNative setter = nullptr);

extern "C" JS_PUBLIC_API(bool)
 JS_HasPropertyById(JSContext* cx, JS::HandleObject obj, JS::HandleId id, bool* foundp);

extern "C" JS_PUBLIC_API(bool)
 JS_HasElement(JSContext* cx, JS::HandleObject obj, uint32_t index, bool* foundp);

extern "C" JS_PUBLIC_API(bool)
 JS_HasOwnPropertyById(JSContext* cx, JS::HandleObject obj, JS::HandleId id, bool* foundp);

extern "C" JS_PUBLIC_API(bool)
 JS_HasOwnProperty(JSContext* cx, JS::HandleObject obj, const char* name, bool* foundp);

extern "C" JS_PUBLIC_API(bool)
 JS_ForwardGetPropertyTo(JSContext* cx, JS::HandleObject obj, JS::HandleId id,
                         JS::HandleValue receiver, JS::MutableHandleValue vp);

extern "C" JS_PUBLIC_API(bool)
 JS_ForwardGetElementTo(JSContext* cx, JS::HandleObject obj, uint32_t index,
                        JS::HandleObject receiver, JS::MutableHandleValue vp);

extern "C" JS_PUBLIC_API(bool)
 JS_ForwardSetPropertyTo(JSContext* cx, JS::HandleObject obj, JS::HandleId id, JS::HandleValue v,
                         JS::HandleValue receiver, JS::ObjectOpResult& result);

extern "C" JS_PUBLIC_API(bool)
 JS_SetPropertyById(JSContext* cx, JS::HandleObject obj, JS::HandleId id, JS::HandleValue v);

extern "C" JS_PUBLIC_API(bool)
 JS_DeleteProperty(JSContext* cx, JS::HandleObject obj, const char* name,
                   JS::ObjectOpResult& result);

extern "C" JS_PUBLIC_API(bool)
 JS_DeleteUCProperty(JSContext* cx, JS::HandleObject obj, const char16_t* name, size_t namelen,
                     JS::ObjectOpResult& result);

extern "C" JS_PUBLIC_API(bool)
 JS_Enumerate(JSContext* cx, JS::HandleObject obj, JS::MutableHandle<JS::IdVector> props);

extern "C" JS_PUBLIC_API(JSObject*)
 JS_DefineObject(JSContext* cx, JS::HandleObject obj, const char* name,
                 const JSClass* clasp = nullptr, unsigned attrs = 0);

extern "C" JS_PUBLIC_API(bool)
 JS_DefineConstDoubles(JSContext* cx, JS::HandleObject obj, const JSConstDoubleSpec* cds);

extern "C" JS_PUBLIC_API(bool)
 JS_DefineConstIntegers(JSContext* cx, JS::HandleObject obj, const JSConstIntegerSpec* cis);

extern "C" JS_PUBLIC_API(bool)
 JS_AlreadyHasOwnPropertyById(JSContext* cx, JS::HandleObject obj, JS::HandleId id,
                              bool* foundp);

extern "C" JS_PUBLIC_API(bool)
 JS_AlreadyHasOwnProperty(JSContext* cx, JS::HandleObject obj, const char* name,
                          bool* foundp);

extern "C" JS_PUBLIC_API(bool)
 JS_AlreadyHasOwnElement(JSContext* cx, JS::HandleObject obj, uint32_t index, bool* foundp);

extern "C" JS_PUBLIC_API(bool)
 JS_SetArrayLength(JSContext* cx, JS::Handle<JSObject*> obj, uint32_t length);

extern "C" JS_PUBLIC_API(JSObject*)
 JS_NewArrayBufferWithContents(JSContext* cx, size_t nbytes, void* contents);

extern "C" JS_PUBLIC_API(JSObject*)
 JS_NewArrayBufferWithExternalContents(JSContext* cx, size_t nbytes, void* contents);

extern "C" JS_PUBLIC_API(void*)
 JS_StealArrayBufferContents(JSContext* cx, JS::HandleObject obj);

extern "C" JS_PUBLIC_API(void*)
 JS_ExternalizeArrayBufferContents(JSContext* cx, JS::HandleObject obj);

extern "C" JS_PUBLIC_API(JSObject*)
 JS_NewMappedArrayBufferWithContents(JSContext* cx, size_t nbytes, void* contents);

extern "C" JS_PUBLIC_API(void*)
 JS_CreateMappedArrayBufferContents(int fd, size_t offset, size_t length);

extern "C" JS_PUBLIC_API(void)
 JS_ReleaseMappedArrayBufferContents(void* contents, size_t length);

extern "C" JS_PUBLIC_API(JS::Value)
 JS_GetReservedSlot(JSObject* obj, uint32_t index);

extern "C" JS_PUBLIC_API(JSObject*)
 JS_GetFunctionObject(JSFunction* fun);

extern "C" JS_PUBLIC_API(JSString*)
 JS_GetFunctionDisplayId(JSFunction* fun);

extern "C" JS_PUBLIC_API(uint16_t)
 JS_GetFunctionArity(JSFunction* fun);

extern "C" JS_PUBLIC_API(bool)
 JS_IsNativeFunction(JSObject* funobj, JSNative call);

extern "C" JS_PUBLIC_API(bool)
 JS_IsConstructor(JSFunction* fun);

extern "C" JS_PUBLIC_API(JSFunction*)
 JS_DefineFunctionById(JSContext* cx, JS::Handle<JSObject*> obj, JS::Handle<jsid> id, JSNative call,
                       unsigned nargs, unsigned attrs);

extern "C" JS_PUBLIC_API(bool)
 JS_IsFunctionBound(JSFunction* fun);

extern "C" JS_PUBLIC_API(JSObject*)
 JS_GetBoundFunctionTarget(JSFunction* fun);

extern "C" JS_PUBLIC_API(bool)
 JS_BufferIsCompilableUnit(JSContext* cx, JS::Handle<JSObject*> obj, const char* utf8,
                           size_t length);

extern "C" JS_PUBLIC_API(JSObject*)
 JS_GetGlobalFromScript(JSScript* script);

extern "C" JS_PUBLIC_API(const char*)
 JS_GetScriptFilename(JSScript* script);

extern "C" JS_PUBLIC_API(unsigned)
 JS_GetScriptBaseLineNumber(JSContext* cx, JSScript* script);

extern "C" JS_PUBLIC_API(JSScript*)
 JS_GetFunctionScript(JSContext* cx, JS::HandleFunction fun);

extern "C" JS_PUBLIC_API(JSString*)
 JS_DecompileScript(JSContext* cx, JS::Handle<JSScript*> script, const char* name, unsigned indent);

extern "C" JS_PUBLIC_API(JSString*)
 JS_NewStringCopyZ(JSContext* cx, const char* s);

extern "C" JS_PUBLIC_API(JSString*)
 JS_NewStringCopyUTF8N(JSContext* cx, const JS::UTF8Chars s);

extern "C" JS_PUBLIC_API(JSString*)
 JS_AtomizeAndPinJSString(JSContext* cx, JS::HandleString str);

extern "C" JS_PUBLIC_API(JSString*)
 JS_AtomizeStringN(JSContext* cx, const char* s, size_t length);

extern "C" JS_PUBLIC_API(JSString*)
 JS_AtomizeString(JSContext* cx, const char* s);

extern "C" JS_PUBLIC_API(JSString*)
 JS_AtomizeAndPinStringN(JSContext* cx, const char* s, size_t length);

extern "C" JS_PUBLIC_API(JSString*)
 JS_AtomizeAndPinString(JSContext* cx, const char* s);

extern "C" JS_PUBLIC_API(JSString*)
 JS_NewUCString(JSContext* cx, char16_t* chars, size_t length);

extern "C" JS_PUBLIC_API(JSString*)
 JS_NewUCStringCopyZ(JSContext* cx, const char16_t* s);

extern "C" JS_PUBLIC_API(JSString*)
 JS_AtomizeUCStringN(JSContext* cx, const char16_t* s, size_t length);

extern "C" JS_PUBLIC_API(JSString*)
 JS_AtomizeUCString(JSContext* cx, const char16_t* s);

extern "C" JS_PUBLIC_API(JSString*)
 JS_AtomizeAndPinUCStringN(JSContext* cx, const char16_t* s, size_t length);

extern "C" JS_PUBLIC_API(JSString*)
 JS_AtomizeAndPinUCString(JSContext* cx, const char16_t* s);

extern "C" JS_PUBLIC_API(bool)
 JS_CompareStrings(JSContext* cx, JSString* str1, JSString* str2, int32_t* result);

extern "C" JS_PUBLIC_API(bool)
 JS_StringEqualsAscii(JSContext* cx, JSString* str, const char* asciiBytes, bool* match);

extern "C" JS_PUBLIC_API(size_t)
 JS_PutEscapedString(JSContext* cx, char* buffer, size_t size, JSString* str, char quote);

extern "C" JS_PUBLIC_API(bool)
 JS_FileEscapedString(FILE* fp, JSString* str, char quote);

extern "C" JS_PUBLIC_API(bool)
 JS_StringIsFlat(JSString* str);

extern "C" JS_PUBLIC_API(bool)
 JS_GetStringCharAt(JSContext* cx, JSString* str, size_t index, char16_t* res);

extern "C" JS_PUBLIC_API(char16_t)
 JS_GetFlatStringCharAt(JSFlatString* str, size_t index);

extern "C" JS_PUBLIC_API(const char16_t*)
 JS_GetTwoByteExternalStringChars(JSString* str);

extern "C" JS_PUBLIC_API(bool)
 JS_CopyStringChars(JSContext* cx, mozilla::Range<char16_t> dest, JSString* str);

extern "C" JS_PUBLIC_API(JSFlatString*)
 JS_FlattenString(JSContext* cx, JSString* str);

extern "C" JS_PUBLIC_API(const JS::Latin1Char*)
 JS_GetLatin1FlatStringChars(const JS::AutoCheckCannotGC& nogc, JSFlatString* str);

extern "C" JS_PUBLIC_API(const char16_t*)
 JS_GetTwoByteFlatStringChars(const JS::AutoCheckCannotGC& nogc, JSFlatString* str);

extern "C" JS_PUBLIC_API(bool)
 JS_FlatStringEqualsAscii(JSFlatString* str, const char* asciiBytes);

extern "C" JS_PUBLIC_API(size_t)
 JS_PutEscapedFlatString(char* buffer, size_t size, JSFlatString* str, char quote);

extern "C" JS_PUBLIC_API(JSString*)
 JS_NewDependentString(JSContext* cx, JS::HandleString str, size_t start,
                       size_t length);

extern "C" JS_PUBLIC_API(JSString*)
 JS_ConcatStrings(JSContext* cx, JS::HandleString left, JS::HandleString right);

extern "C" JS_PUBLIC_API(bool)
 JS_SetDefaultLocale(JSContext* cx, const char* locale);

extern "C" JS_PUBLIC_API(void)
 JS_ResetDefaultLocale(JSContext* cx);

extern "C" JS_PUBLIC_API(void)
 JS_SetLocaleCallbacks(JSContext* cx, const JSLocaleCallbacks* callbacks);

extern "C" JS_PUBLIC_API(const JSLocaleCallbacks*)
 JS_GetLocaleCallbacks(JSContext* cx);

extern "C" JS_PUBLIC_API(void)
 JS_ReportErrorLatin1(JSContext* cx, const char* format, ...)
     MOZ_FORMAT_PRINTF(2, 3);

extern "C" JS_PUBLIC_API(void)
 JS_ReportErrorUTF8(JSContext* cx, const char* format, ...)
     MOZ_FORMAT_PRINTF(2, 3);

extern "C" JS_PUBLIC_API(void)
 JS_ReportErrorNumberASCII(JSContext* cx, JSErrorCallback errorCallback,
                           void* userRef, const unsigned errorNumber, ...);

extern "C" JS_PUBLIC_API(void)
 JS_ReportErrorNumberASCIIVA(JSContext* cx, JSErrorCallback errorCallback,
                             void* userRef, const unsigned errorNumber, va_list ap);

extern "C" JS_PUBLIC_API(void)
 JS_ReportErrorNumberLatin1(JSContext* cx, JSErrorCallback errorCallback,
                            void* userRef, const unsigned errorNumber, ...);

extern "C" JS_PUBLIC_API(void)
 JS_ReportErrorNumberUTF8(JSContext* cx, JSErrorCallback errorCallback,
                            void* userRef, const unsigned errorNumber, ...);

extern "C" JS_PUBLIC_API(void)
 JS_ReportErrorNumberUCArray(JSContext* cx, JSErrorCallback errorCallback,
                             void* userRef, const unsigned errorNumber,
                             const char16_t** args);

extern "C" JS_PUBLIC_API(bool)
 JS_ReportWarningASCII(JSContext* cx, const char* format, ...)
     MOZ_FORMAT_PRINTF(2, 3);

extern "C" JS_PUBLIC_API(bool)
 JS_ReportWarningLatin1(JSContext* cx, const char* format, ...)
     MOZ_FORMAT_PRINTF(2, 3);

extern "C" JS_PUBLIC_API(bool)
 JS_ReportWarningUTF8(JSContext* cx, const char* format, ...)
     MOZ_FORMAT_PRINTF(2, 3);

extern "C" JS_PUBLIC_API(bool)
 JS_ReportErrorFlagsAndNumberASCII(JSContext* cx, unsigned flags,
                                   JSErrorCallback errorCallback, void* userRef,
                                   const unsigned errorNumber, ...);

extern "C" JS_PUBLIC_API(bool)
 JS_ReportErrorFlagsAndNumberLatin1(JSContext* cx, unsigned flags,
                                    JSErrorCallback errorCallback, void* userRef,
                                    const unsigned errorNumber, ...);

extern "C" JS_PUBLIC_API(bool)
 JS_ReportErrorFlagsAndNumberUTF8(JSContext* cx, unsigned flags,
                                  JSErrorCallback errorCallback, void* userRef,
                                  const unsigned errorNumber, ...);

extern "C" JS_PUBLIC_API(bool)
 JS_ReportErrorFlagsAndNumberUC(JSContext* cx, unsigned flags,
                                JSErrorCallback errorCallback, void* userRef,
                                const unsigned errorNumber, ...);

extern "C" JS_PUBLIC_API(void)
 JS_ReportOutOfMemory(JSContext* cx);

extern "C" JS_PUBLIC_API(void)
 JS_ReportAllocationOverflow(JSContext* cx);

extern "C" JS_PUBLIC_API(JSObject*)
 JS_NewRegExpObject(JSContext* cx, const char* bytes, size_t length, unsigned flags);

extern "C" JS_PUBLIC_API(JSObject*)
 JS_NewUCRegExpObject(JSContext* cx, const char16_t* chars, size_t length, unsigned flags);

extern "C" JS_PUBLIC_API(bool)
 JS_SetRegExpInput(JSContext* cx, JS::HandleObject obj, JS::HandleString input);

extern "C" JS_PUBLIC_API(bool)
 JS_ClearRegExpStatics(JSContext* cx, JS::HandleObject obj);

extern "C" JS_PUBLIC_API(bool)
 JS_ExecuteRegExp(JSContext* cx, JS::HandleObject obj, JS::HandleObject reobj,
                  char16_t* chars, size_t length, size_t* indexp, bool test,
                  JS::MutableHandleValue rval);

extern "C" JS_PUBLIC_API(bool)
 JS_ExecuteRegExpNoStatics(JSContext* cx, JS::HandleObject reobj, char16_t* chars, size_t length,
                           size_t* indexp, bool test, JS::MutableHandleValue rval);

extern "C" JS_PUBLIC_API(bool)
 JS_ObjectIsRegExp(JSContext* cx, JS::HandleObject obj, bool* isRegExp);

extern "C" JS_PUBLIC_API(unsigned)
 JS_GetRegExpFlags(JSContext* cx, JS::HandleObject obj);

extern "C" JS_PUBLIC_API(JSString*)
 JS_GetRegExpSource(JSContext* cx, JS::HandleObject obj);

extern "C" JS_PUBLIC_API(JSExceptionState*)
 JS_SaveExceptionState(JSContext* cx);

extern "C" JS_PUBLIC_API(void)
 JS_RestoreExceptionState(JSContext* cx, JSExceptionState* state);

extern "C" JS_PUBLIC_API(void)
 JS_DropExceptionState(JSContext* cx, JSExceptionState* state);

extern "C" JS_PUBLIC_API(JSObject*)
 ExceptionStackOrNull(JS::HandleObject obj);

extern "C" JS_PUBLIC_API(bool)
 JS_ThrowStopIteration(JSContext* cx);

extern "C" JS_PUBLIC_API(bool)
 JS_IsStopIteration(const JS::Value& v);

extern "C" JS_PUBLIC_API(void)
 JS_AbortIfWrongThread(JSContext* cx);

extern "C" JS_PUBLIC_API(JSObject*)
 JS_NewObjectForConstructor(JSContext* cx, const JSClass* clasp, const JS::CallArgs& args);

extern "C" JS_PUBLIC_API(void)
 JS_SetParallelParsingEnabled(JSContext* cx, bool enabled);

extern "C" JS_PUBLIC_API(void)
 JS_SetOffthreadIonCompilationEnabled(JSContext* cx, bool enabled);

extern "C" JS_PUBLIC_API(void)
 JS_SetGlobalJitCompilerOption(JSContext* cx, JSJitCompilerOption opt, uint32_t value);

extern "C" JS_PUBLIC_API(bool)
 JS_GetGlobalJitCompilerOption(JSContext* cx, JSJitCompilerOption opt, uint32_t* valueOut);

extern "C" JS_PUBLIC_API(bool)
 JS_IndexToId(JSContext* cx, uint32_t index, JS::MutableHandleId);

extern "C" JS_PUBLIC_API(bool)
 JS_CharsToId(JSContext* cx, JS::TwoByteChars chars, JS::MutableHandleId);

extern "C" JS_PUBLIC_API(bool)
 JS_IsIdentifier(JSContext* cx, JS::HandleString str, bool* isIdentifier);

extern "C" JS_PUBLIC_API(void*)
JS_SetCompileOptionsFileAndLine(const char* f, unsigned l);

extern "C" JS_PUBLIC_API(void*)
JS_NewCompartmentOptions();

extern "C" JS_PUBLIC_API(void)
JS_FreeCompartmentOptions(void* opt);

extern "C" JS_FRIEND_API(js::Scalar::Type)
 JS_GetSharedArrayBufferViewType(JSObject* obj);

extern "C" JS_FRIEND_API(bool)
 JS_IsArrayBufferViewObject(JSObject* obj);

extern "C" JS_FRIEND_API(bool)
 JS_DetachArrayBuffer(JSContext* cx, JS::HandleObject obj);

extern "C" JS_FRIEND_API(bool)
 JS_IsDetachedArrayBufferObject(JSObject* obj);
*/
#endif
