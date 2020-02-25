unit jsbUtils;

interface
{$I Synopse.inc}
{$I SyNode.inc}
uses
  SynCommons,
  SpiderMonkey,
  SyNode;

const
  atVoid: Cardinal = 1 shl byte(JSTYPE_VOID);
  atObj: Cardinal  = 1 shl byte(JSTYPE_OBJECT);
  atFunc: Cardinal = 1 shl byte(JSTYPE_FUNCTION);
  atStr: Cardinal  = 1 shl byte(JSTYPE_STRING);
  atNum: Cardinal  = 1 shl byte(JSTYPE_NUMBER);
  atBool: Cardinal = 1 shl byte(JSTYPE_BOOLEAN);
  atNull: Cardinal = 1 shl byte(JSTYPE_NULL);
  atSymb: Cardinal = 1 shl byte(JSTYPE_SYMBOL);
  atAny: Cardinal  = 255;
  atBuf: Cardinal  = (1 shl byte(JSTYPE_OBJECT)) or (1 shl (byte(JSTYPE_LIMIT) + 1));
  atI32: Cardinal  = (1 shl byte(JSTYPE_NUMBER)) or (1 shl (byte(JSTYPE_LIMIT) + 2));
  atDbl: Cardinal  = (1 shl byte(JSTYPE_NUMBER)) or (1 shl (byte(JSTYPE_LIMIT) + 3));
  atDate: Cardinal = (1 shl byte(JSTYPE_OBJECT)) or (1 shl (byte(JSTYPE_LIMIT) + 4));

  JSTYPE2STR: array[JSType] of string = ('undefined', 'Object', 'Function', 'String',
    'Number', 'Boolean', 'null', 'Symbol', '');


  GETTER_ONLY_PROP_ATTR = JSPROP_ENUMERATE or JSPROP_PERMANENT or JSPROP_SHARED or JSPROP_GETTER;
  METHOD_ATTR = JSPROP_ENUMERATE or JSPROP_PERMANENT;
/// Check function arguments types match expected types
// set JS runtime in exception state and returns false if args not match;
// The caller must then return JS_FALSE to cause the exception to be propagated
// to the calling script
function checkFuncArgs(cx: PJSContext; argc: uintN; var vp: jsargRec; const expect: array of Cardinal): boolean;

implementation
const
  WRONG_ARG_TYPE: RawUTF8 = 'Incompatible type for ''%'' arg no. %: Got ''%'', expected ''%''';
  WRONT_ARG_CNT: RawUTF8 = 'Wrong number of parameters specified for call to ''%''';
  TYPE_BUFFER = 'ArrayBuffer|ArrayBufferView';
  TYPE_I32 = 'Int32';
  TYPE_DOUBLE='Double';
  TYPE_DATE='Date';

function checkFuncArgs(cx: PJSContext; argc: uintN; var vp: jsargRec; const expect: array of Cardinal): boolean;
var
  i: integer;
  at: cardinal;
  vt: JSType;
  es: RawUTF8;
  label err;
  function estr(e: cardinal): string;
  begin
    if byte(e) = atAny then
      exit('*');

    if (e and atVoid) = atVoid then
      Result := Result + JSTYPE2STR[JSTYPE_VOID] + '|';
    if (e and atObj) = atObj then begin
      if (e and atBuf) = atBuf then
        Result := Result + TYPE_BUFFER + '|'
      else if (e and atDate) = atDate then
        Result := Result + TYPE_DATE + '|'
      else
        Result := Result + JSTYPE2STR[JSTYPE_OBJECT] + '|'
    end;
    if (e and atFunc) = atFunc then
      Result := Result + JSTYPE2STR[JSTYPE_FUNCTION] + '|';
    if (e and atStr) = atStr then
      Result := Result + JSTYPE2STR[JSTYPE_STRING] + '|';
    if (e and atNum) = atNum then begin
      if (e and atI32) = atI32 then
        Result := Result + TYPE_I32 + '|'
      else if (e and atDbl) = atDbl then
        Result := Result + TYPE_DOUBLE + '|'
      else
        Result := Result + JSTYPE2STR[JSTYPE_NUMBER] + '|'
    end;
    if (e and atBool) = atBool then
      Result := Result + JSTYPE2STR[JSTYPE_BOOLEAN] + '|';
    if (e and atNull) = atNull then
      Result := Result + JSTYPE2STR[JSTYPE_NULL] + '|';
    if (e and atSymb) = atSymb then
      Result := Result + JSTYPE2STR[JSTYPE_SYMBOL] + '|';

    if length(Result) > 0 then
      SetLength(Result, length(result)-1); //remove last |
    //else
    //  raise ESMException.CreateUTF8('wrong estr impl for %', [e])
  end;
begin
  if (argc > length(expect)) then begin
    es := FormatUTF8(WRONT_ARG_CNT, [vp.calleObject.GetFunctionId().ToUTF8(cx)]);
    goto err;
  end;
  for i := 0 to argc -1 do begin
    vt := vp.argv^[i].ValType(cx);
    at := 1 shl byte(vt);
    if (at and expect[i]) = 0 then begin
      es := FormatUTF8(WRONG_ARG_TYPE, [vp.calleObject.GetFunctionId().ToUTF8(cx),
        i, JSTYPE2STR[vt], estr(expect[i])]);
      goto err;
    end;
    if at < atAny then
      continue;
    // check subtype
    if (expect[i] = atBuf) and not (vp.argv^[i].asObject.IsArrayBufferObject or vp.argv^[i].asObject.IsArrayBufferViewObject) then
    begin
      es := FormatUTF8(WRONG_ARG_TYPE, [vp.calleObject.GetFunctionId().ToUTF8(cx),
        i, JSTYPE2STR[vt], TYPE_BUFFER]);
      goto err;
    end else if (expect[i] = atI32) and not vp.argv^[i].isInteger then begin
      es := FormatUTF8(WRONG_ARG_TYPE, [vp.calleObject.GetFunctionId().ToUTF8(cx),
        i, JSTYPE2STR[vt], TYPE_I32]);
      goto err;
    end else if (expect[i] = atDbl) and not vp.argv^[i].isDouble then begin
      es := FormatUTF8(WRONG_ARG_TYPE, [vp.calleObject.GetFunctionId().ToUTF8(cx),
        i, JSTYPE2STR[vt], TYPE_DOUBLE]);
      goto err;
    end else if (expect[i] = atDate) and not vp.argv^[i].asObject.isDate(cx) then begin
      es := FormatUTF8(WRONG_ARG_TYPE, [vp.calleObject.GetFunctionId().ToUTF8(cx),
        i, JSTYPE2STR[vt], TYPE_DATE]);
      goto err;
    end;
  end;
  if (argc < length(expect)) then
    for i := argc to length(expect)-1 do
      if (expect[i] and atVoid) <> atVoid then begin
         es := FormatUTF8(WRONG_ARG_TYPE, [vp.calleObject.GetFunctionId().ToUTF8(cx),
           i, JSTYPE2STR[JSTYPE_VOID], estr(expect[i])]);
         goto err;
      end;
  exit(true);
err:
  JS_ReportError(cx, PChar(pointer(es)));
  Result := false;
end;

end.
