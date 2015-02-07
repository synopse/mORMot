/// service to generate mORMot cross-platform clients code from the server
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit mORMotWrappers;

{
    This file is part of Synopse mORmot framework.

    Synopse mORMot framework. Copyright (C) 2015 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse mORMot framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2015
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Sabbiolina

  
  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

  
  Version 1.18
  - first public release, corresponding to Synopse mORMot Framework 1.18

}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  {$ifdef ISDELPHIXE2}System.SysUtils,{$else}SysUtils,{$endif}
  Classes,
  Contnrs,
  Variants,
  SynCommons,
  mORMot,
  SynMustache;

/// compute the Model information, ready to be exported as JSON
// - will publish the ORM and SOA properties
// - to be used e.g. for client code generation via Mustache templates
function ContextFromModel(aServer: TSQLRestServer): variant;

/// compute the information of an interface method, ready to be exported as JSON
// - to be used e.g. for the implementation of the MVC controller via interfaces
function ContextFromMethod(const method: TServiceMethod): variant;

/// compute the information of an interface, ready to be exported as JSON
// - to be used e.g. for the implementation of the MVC controller via interfaces
function ContextFromMethods(int: TInterfaceFactory): variant;

/// generate a code wrapper for a given Model and Mustache template content
// - will use all ORM and SOA properties of the supplied server
// - aFileName will be transmitted as {{filename}}, e.g. 'mORMotClient'
// - you should also specify the HTTP port e.g. 888
// - the template content could be retrieved from a file via StringFromFile()
// - this function may be used to generate the client at build time, directly
// from a just built server, in an automated manner
function WrapperFromModel(aServer: TSQLRestServer;
  const aMustacheTemplate, aFileName: RawUTF8;
  aPort: integer): RawUTF8;

/// you can call this procedure within a method-based service allow
// code-generation of an ORM and SOA client from a web browser
// - you have to specify one or several client *.mustache file paths
// - the first path containing any *.mustache file will be used as templates
// - for instance:
// ! procedure TCustomServer.Wrapper(Ctxt: TSQLRestServerURIContext);
// ! begin // search in the current path
// !   WrapperMethod(Ctxt,['.']);
// ! end;
procedure WrapperMethod(Ctxt: TSQLRestServerURIContext; const Path: array of TFileName);

/// you can call this procedure to add a 'Wrapper' method-based service
//  to a given server, to allow code-generation of an ORM and SOA client
// - you have to specify one or several client *.mustache file paths
// - the first path containing any *.mustache file will be used as templates
// - if no path is specified (i.e. as []), it will search in the .exe folder
// - the root/wrapper URI will be accessible without authentication (i.e.
// from any plain browser)
// - for instance:
// ! aServer := TSQLRestServerFullMemory.Create(aModel,'test.json',false,true);
// ! AddToServerWrapperMethod(aServer,['..']);
procedure AddToServerWrapperMethod(Server: TSQLRestServer; const Path: array of TFileName);

/// you can call this procedure to generate the mORMotServer.pas unit needed
// to compile a given server source code using FPC
// - will locate FPCServer-mORMotServer.pas.mustache in the given Path[] array
// - will write the unit using specified file name or to mORMotServer.pas in the
// current directory if DestFileName is '', or to a sub-folder of the matching
// Path[] if DestFileName starts with '\' (to allow relative folder use)
// - the missing RTTI for records and interfaces would be defined, together
// with some patch comments for published record support (if any) for the ORM
procedure ComputeFPCServerUnit(Server: TSQLRestServer; const Path: array of TFileName;
  DestFileName: TFileName='');

/// you can call this procedure to generate the mORMotInterfaces.pas unit needed
// to register all needed interface RTTI for FPC
// - to circumvent http://bugs.freepascal.org/view.php?id=26774 unresolved issue
// - will locate FPC-mORMotInterfaces.pas.mustache in the given Path[] array
// - will write the unit using specified file name or to mORMotInterfaces.pas in
// the current directory if DestFileName is '', or to a sub-folder of the
// matching Path[] if DestFileName starts with '\' (to allow relative folder use)
// - all used interfaces will be exported, including SOA and mocking/stubing
// types: so you may have to run this function AFTER all process is done
procedure ComputeFPCInterfacesUnit(const Path: array of TFileName;
  DestFileName: TFileName='');


implementation

type
  /// a cross-platform published property kind
  // - does not match mORMot.pas TSQLFieldType: here we recognize only types
  // which may expect a special behavior in SynCrossPlatformREST.pas unit
  // - should match TSQLFieldKind order in SynCrossPlatformREST.pas
  TCrossPlatformSQLFieldKind = (
    cpkDefault, cpkDateTime, cpkTimeLog, cpkBlob, cpkModTime, cpkCreateTime,
    cpkRecord, cpkVariant);

const
  /// those text values should match TSQLFieldKind in SynCrossPlatformREST.pas
  CROSSPLATFORMKIND_TEXT: array[TCrossPlatformSQLFieldKind] of RawUTF8 = (
    'sftUnspecified', 'sftDateTime', 'sftTimeLog', 'sftBlob', 'sftModTime',
    'sftCreateTime', 'sftRecord', 'sftVariant');

type
  /// types recognized and handled by this mORMotWrappers unit
  TWrapperType = (
    wUnknown,
    wBoolean, wEnum, wSet,
    wByte, wWord, wInteger, wCardinal,
    wInt64, wID, wReference, wTimeLog, wModTime, wCreateTime,
    wCurrency, wSingle, wDouble, wDateTime,
    wRawUTF8, wString, wRawJSON, wBlob,
    wGUID, wCustomAnswer, wRecord, wArray, wVariant,
    wObject, wSQLRecord);
  /// supported languages typesets
  TWrapperLanguage = (
    lngDelphi, lngPascal, lngCS, lngJava);

const
  CROSSPLATFORM_KIND: array[TSQLFieldType] of TCrossPlatformSQLFieldKind = (
 // sftUnknown, sftAnsiText, sftUTF8Text, sftEnumerate, sftSet,    sftInteger,
    cpkDefault, cpkDefault,  cpkDefault,  cpkDefault,   cpkDefault,cpkDefault,
 // sftID,     sftRecord, sftBoolean,sftFloat,  sftDateTime,sftTimeLog,sftCurrency,
    cpkDefault,cpkDefault,cpkDefault,cpkDefault,cpkDateTime,cpkTimeLog,cpkDefault,
 // sftObject,                     sftVariant,         sftBlob, sftBlobDynArray,
    cpkDefault,{$ifndef NOVARIANTS}cpkVariant,{$endif} cpkBlob, cpkDefault,
 // sftBlobCustom,sftUTF8Custom,sftMany, sftModTime,sftCreateTime, sftTID
    cpkDefault,   cpkRecord,  cpkDefault,cpkModTime,cpkCreateTime, cpkDefault);

  SIZETODELPHI: array[0..8] of string[7] = (
    'integer','byte','word','integer','integer','int64','int64','int64','int64');

  TYPES_SIZE: array[0..8] of TWrapperType = (
    winteger,wbyte,wword,winteger,winteger,wint64,wint64,wint64,wint64);

  TYPES_LANG: array[TWrapperLanguage,TWrapperType] of RawUTF8 = (
   // lngDelphi
   ('', 'Boolean', '', '', 'Byte', 'Word', 'Integer', 'Cardinal',
    'Int64', 'TID', 'TRecordReference', 'TTimeLog', 'TModTime', 'TCreateTime',
    'Currency', 'Single', 'Double', 'TDateTime', 'RawUTF8','String', 'RawJSON',
    'TSQLRawBlob', 'TGUID', 'TServiceCustomAnswer', '', '', 'Variant', '', ''),
   // lngPascal
   ('', 'Boolean', '', '', 'Byte', 'Word', 'Integer', 'Cardinal',
    'Int64', 'TID', 'TRecordReference', 'TTimeLog', 'TModTime', 'TCreateTime',
    'Currency', 'Single', 'Double', 'TDateTime', 'String', 'String', 'Variant',
    'TSQLRawBlob', 'TGUID', 'THttpBody', '', '', 'Variant', '', ''),
   // lngCS
   ('', 'bool', '', '', 'byte', 'word', 'integer', 'uint',
    'long', 'TID', 'TRecordReference', 'TTimeLog', 'TModTime', 'TCreateTime',
    'decimal', 'single', 'double', 'double', 'string', 'string', 'dynamic',
    'byte[]', 'Guid', 'byte[]', '', '', 'dynamic', '', ''),
   // lngJava
   ('', 'boolean', '', '', 'byte', 'int', 'int', 'long', 'long', 'TID',
    'TRecordReference', 'TTimeLog', 'TModTime', 'TCreateTime', 'BigDecimal',
    'single', 'double', 'double', 'String', 'String', 'Object', 'byte[]',
    'String', 'byte[]', '', '', 'Object', '', ''));
                       
  TYPES_ORM: array[TSQLFieldType] of TWrapperType =
    (wUnknown,   // sftUnknown
     wString,    // sftAnsiText
     wRawUTF8,   // sftUTF8Text
     wEnum,      // sftEnumerate
     wSet,       // sftSet
     wUnknown,   // sftInteger - wUnknown to force exact type 
     wSQLRecord, // sftID
     wReference, // sftRecord
     wBoolean,   // sftBoolean
     wUnknown,   // sftFloat - wUnknown to force exact type
     wDateTime,  // sftDateTime
     wTimeLog,   // sftTimeLog
     wCurrency,  // sftCurrency
     wObject,    // sftObject
     wVariant,   // sftVariant
     wBlob,      // sftBlob
     wBlob,      // sftBlobDynArray
     wRecord,    // sftBlobCustom
     wRecord,    // sftUTF8Custom
     wUnknown,   // sftMany
     wModTime,   // sftModTime
     wCreateTime,// sftCreateTime
     wID);       // sftID

  TYPES_SIMPLE: array[TJSONCustomParserRTTIType] of TWrapperType = (
  //ptArray, ptBoolean, ptByte, ptCardinal, ptCurrency, ptDouble,
    wArray, wBoolean,   wByte,  wCardinal,  wCurrency,  wDouble,
  //ptInt64, ptInteger, ptRawByteString, ptRawJSON, ptRawUTF8, ptRecord,
    wInt64,  wInteger,  wBlob,           wRawJSON,  wRawUTF8,  wRecord,
  //ptSingle, ptString, ptSynUnicode, ptDateTime, ptGUID, ptID, ptTimeLog,
    wSingle,  wString,  wRawUTF8,     wDateTime,  wGUID,  wID, wTimeLog,
  //ptVariant, ptWideString, ptWord, ptCustom
    wVariant,  wRawUTF8,     wWord,  wUnknown);

  TYPES_SOA: array[TServiceMethodValueType] of TWrapperType = (
    wUnknown,wUnknown,wBoolean,wEnum,wSet,wUnknown,wUnknown,wUnknown,
    wDouble,wDateTime,wCurrency,wRawUTF8,wString,wRawUTF8,wRecord,wVariant,
    wObject,wRawJSON,wArray); // integers are wUnknown to force best type


function NULL_OR_CARDINAL(Value: Integer): RawUTF8;
begin
  if Value>0 then
    UInt32ToUtf8(Value,result) else
    result := 'null';
end;

type
  EWrapperContext = class(ESynException);

  TWrapperContext = class
  protected
    fServer: TSQLRestServer;
    fORM, fRecords,fEnumerates,fSets,fArrays,fUnits: TDocVariantData;
    fSOA: variant;
    fHasAnyRecord: boolean;
    function ContextFromInfo(typ: TWrapperType; typName: RawUTF8='';
      typInfo: PTypeInfo=nil): variant;
    function ContextNestedProperties(rtti: TJSONCustomParserRTTI): variant;
    function ContextOneProperty(prop: TJSONCustomParserRTTI): variant;
    function ContextFromMethods(int: TInterfaceFactory): variant;
    function ContextFromMethod(const meth: TServiceMethod): variant;
    function ContextArgsFromMethod(const meth: TServiceMethod): variant;
  public
    constructor Create;
    constructor CreateFromModel(aServer: TSQLRestServer);
    constructor CreateFromUsedInterfaces;
    function Context: variant;
  end;

{ TWrapperContext }

constructor TWrapperContext.Create;
begin
  TDocVariant.NewFast([@fORM,@fRecords,@fEnumerates,@fSets,@fArrays,@fUnits]);
end;

constructor TWrapperContext.CreateFromModel(aServer: TSQLRestServer);
var t,f,s: integer;
    nfoList: TSQLPropInfoList;
    nfo: TSQLPropInfo;
    nfoSQLFieldRTTITypeName: RawUTF8;
    kind: TCrossPlatformSQLFieldKind;
    hasRecord: boolean;
    fields,services: TDocVariantData;
    field,rec: variant;
    srv: TServiceFactory;
    uri: RawUTF8;
begin
  Create;
  fServer := aServer;
  TDocVariant.NewFast([@fields,@services]);
  // compute ORM information
  for t := 0 to fServer.Model.TablesMax do begin
    nfoList := fServer.Model.TableProps[t].Props.Fields;
    fields.Clear;
    fields.Init;
    hasRecord := false;
    for f := 0 to nfoList.Count-1 do begin
      nfo := nfoList.List[f];
      nfoSQLFieldRTTITypeName := nfo.SQLFieldRTTITypeName;
      if nfo.InheritsFrom(TSQLPropInfoRTTI) then
        field := ContextFromInfo(TYPES_ORM[nfo.SQLFieldType],nfoSQLFieldRTTITypeName,
          TSQLPropInfoRTTI(nfo).PropType) else
      if nfo.InheritsFrom(TSQLPropInfoRecordTyped) then begin
        hasRecord := true;
        fHasAnyRecord := true;
        field := ContextFromInfo(wRecord,nfoSQLFieldRTTITypeName,
          TSQLPropInfoRecordTyped(nfo).TypeInfo);
      end else
        raise EWrapperContext.CreateUTF8('Unexpected type % for %.%',
          [nfo,fServer.Model.Tables[t],nfo.Name]);
      kind := CROSSPLATFORM_KIND[nfo.SQLFieldType];
      _ObjAddProps(['index',f+1,'name',nfo.Name,'sql',ord(nfo.SQLFieldType),
        'sqlName',nfo.SQLFieldTypeName^,'typeKind',ord(kind),
        'typeKindName',CROSSPLATFORMKIND_TEXT[kind],'attr',byte(nfo.Attributes)],field);
      if aIsUnique in nfo.Attributes then
        _ObjAddProps(['unique',true],field);
      if nfo.FieldWidth>0 then
        _ObjAddProps(['width',nfo.FieldWidth],field);
      if f<nfoList.Count-1 then
        _ObjAddProps(['comma',','],field) else
        _ObjAddProps(['comma',null],field); // may conflict with rec.comma otherwise
      fields.AddItem(field);
    end;
    with fServer.Model.TableProps[t] do
      rec := _JsonFastFmt(
        '{tableName:?,className:?,classParent:?,fields:?,isInMormotPas:%,unitName:?,comma:%}',
        [NULL_OR_TRUE[(Props.Table=TSQLAuthGroup) or (Props.Table=TSQLAuthUser)],
         NULL_OR_COMMA[t<fServer.Model.TablesMax]],
        [Props.SQLTableName,Props.Table.ClassName,
         Props.Table.ClassParent.ClassName,Variant(fields),
         Props.ClassType^.UnitName]);
    if hasRecord then
      rec.hasRecords := true;
    fORM.AddItem(rec);
  end;
  // compute SOA information
  if fServer.Services.Count>0 then begin
    for s := 0 to fServer.Services.Count-1 do begin
      srv := fServer.Services.Index(s);
      if fServer.Services.ExpectMangledURI then
        uri := srv.InterfaceMangledURI else
        uri := srv.InterfaceURI;
      with srv do
        rec := _ObjFast(['uri',uri,'interfaceURI',InterfaceURI,
          'interfaceMangledURI',InterfaceMangledURI,
          'interfaceName',InterfaceFactory.InterfaceTypeInfo^.Name,
          'GUID',GUIDToRawUTF8(InterfaceFactory.InterfaceIID),
          'contractExpected',UnQuoteSQLString(ContractExpected),
          'instanceCreation',ord(InstanceCreation),
          'instanceCreationName',GetEnumNameTrimed(
            TypeInfo(TServiceInstanceImplementation),InstanceCreation),
          'methods',ContextFromMethods(InterfaceFactory)]);
      if srv.InstanceCreation=sicClientDriven then
        rec.isClientDriven := true;
      services.AddItem(rec);
    end;
    fSOA := _ObjFast(['enabled',True,'services',variant(services),
      'expectMangledURI',fServer.Services.ExpectMangledURI]);
  end;
end;

constructor TWrapperContext.CreateFromUsedInterfaces;
var interfaces: TObjectList;
    i: Integer;
    services: TDocVariantData;
    fact: TInterfaceFactory;
begin
  Create;
  interfaces := TInterfaceFactory.GetUsedInterfaces;
  if interfaces=nil then
    exit;
  services.Init(JSON_OPTIONS[true]);
  for i := 0 to interfaces.Count-1 do begin
    fact := interfaces.List[i];
    services.AddItem(_ObjFast([
      'interfaceName',fact.InterfaceTypeInfo^.Name,
      'methods',ContextFromMethods(fact)]));
  end;
  fSOA := _ObjFast(['enabled',True,'services',variant(services)]);
end;

function TWrapperContext.ContextArgsFromMethod(const meth: TServiceMethod): variant;
const
  DIRTODELPHI: array[TServiceMethodValueDirection] of string[7] = (
    'const','var','out','result');
  DIRTOSMS: array[TServiceMethodValueDirection] of string[7] = (
    'const','var','var','result');
var a,r: integer;
    arg: variant;
begin
  TDocVariant.NewFast(result);
  r := 0;
  for a := 1 to high(meth.Args) do begin
    with meth.Args[a] do begin
      arg := ContextFromInfo(TYPES_SOA[ValueType],'',TypeInfo);
      arg.argName := ParamName^;
      arg.dir := ord(ValueDirection);
      arg.dirName := DIRTODELPHI[ValueDirection];
      arg.dirNoOut := DIRTOSMS[ValueDirection]; // no OUT in DWS/SMS -> VAR instead
      if ValueDirection in [smdConst,smdVar] then
        arg.dirInput := true;
      if ValueDirection in [smdVar,smdOut,smdResult] then
        arg.dirOutput := true;
      if ValueDirection=smdResult then
        arg.dirResult := true;
    end;
    if a<meth.ArgsNotResultLast then
      _ObjAddProps(['commaArg','; '],arg);
    if a=high(meth.Args) then
      _ObjAddProps(['isArgLast',true],arg);
    if (meth.args[a].ValueDirection in [smdConst,smdVar]) and (a<meth.ArgsInLast) then
      _ObjAddProps(['commaInSingle',','],arg);
    if (meth.args[a].ValueDirection in [smdVar,smdOut]) and (a<meth.ArgsOutNotResultLast) then
      _ObjAddProps(['commaOut','; '],arg);
    if meth.args[a].ValueDirection in [smdVar,smdOut,smdResult] then begin
      _ObjAddProps(['indexOutResult',UInt32ToUtf8(r)+']'],arg);
      inc(r);
      if a<meth.ArgsOutLast then
        _ObjAddProps(['commaOutResult','; '],arg);
    end;
    TDocVariantData(result).AddItem(arg);
  end;
end;

function TWrapperContext.ContextFromMethod(const meth: TServiceMethod): variant;
const
  VERB_DELPHI: array[boolean] of string[9] = ('procedure','function');
begin
  with meth do begin
    result := _ObjFast(['methodName',URI,'methodIndex',ExecutionMethodIndex,
      'verb',VERB_DELPHI[ArgsResultIndex>=0],
      'args',ContextArgsFromMethod(meth),
      'argsOutputCount',ArgsOutputValuesCount]);
    if ArgsInFirst>=0 then
      result.hasInParams := true;
    if ArgsOutFirst>=0 then
      result.hasOutParams := true;
    if ArgsResultIsServiceCustomAnswer then
      result.resultIsServiceCustomAnswer := true;
  end;
end;

function TWrapperContext.ContextFromMethods(int: TInterfaceFactory): variant;
var m: integer;
begin
  TDocVariant.NewFast(result);
  for m := 0 to int.MethodsCount-1 do
    TDocVariantData(result).AddItem(ContextFromMethod(int.Methods[m]));
end;

function TWrapperContext.ContextOneProperty(prop: TJSONCustomParserRTTI): variant;
var typ: pointer;
    l,level: integer;
begin
  if prop.InheritsFrom(TJSONCustomParserCustom) then
    typ := TJSONCustomParserCustom(prop).CustomTypeInfo else
    typ := nil;
  result := ContextFromInfo(TYPES_SIMPLE[prop.PropertyType],prop.CustomTypeName,typ);
  if prop.PropertyName<>'' then
    _ObjAddProps(['propName',prop.PropertyName,'fullPropName',prop.FullPropertyName],result);
  level := 0;
  for l := 1 to length(prop.FullPropertyName) do
    if prop.FullPropertyName[l]='.' then
      inc(level);
  if level>0 then
    result.nestedIdentation := StringOfChar(' ',level*2);
  case prop.PropertyType of
  ptRecord: begin
    result.isSimple := null;
    result.nestedRecord := _ObjFast(
      ['nestedRecord',null,'fields',ContextNestedProperties(prop)]);
  end;
  ptArray: begin
    result.isSimple := null;
    if prop.NestedProperty[0].PropertyName='' then
      result.nestedSimpleArray := ContextOneProperty(prop.NestedProperty[0]) else
      result.nestedRecordArray := _ObjFast(
        ['nestedRecordArray',null,'fields',ContextNestedProperties(prop)]);
  end;
  else
    if TDocVariantData(result).GetValueIndex('toVariant')<0 then
      result.isSimple := true else
      result.isSimple := null;
  end;
end;

function TWrapperContext.ContextNestedProperties(rtti: TJSONCustomParserRTTI): variant;
var i: integer;
begin
  SetVariantNull(result);
  if rtti.PropertyType in [ptRecord,ptArray] then begin
    TDocVariant.NewFast(result);
    for i := 0 to high(rtti.NestedProperty) do
      TDocVariantData(result).AddItem(ContextOneProperty(rtti.NestedProperty[i]));
  end;
end;

function TWrapperContext.ContextFromInfo(typ: TWrapperType; typName: RawUTF8;
  typInfo: PTypeInfo): variant;
var typeWrapper: PShortString;
function VarName(lng: TWrapperLanguage): variant;
begin
  if TYPES_LANG[lng,typ]<>'' then
    RawUTF8ToVariant(TYPES_LANG[lng,typ],result) else
    if typName='' then
      SetVariantNull(result) else
      RawUTF8ToVariant(typName,result);
end;
procedure RegisterType(var list: TDocVariantData);
var info: variant;
    item: PTypeInfo;
    itemSize: integer;
    parser: TJSONRecordAbstract;
begin
  if list.SearchItemByProp('name',typName,false)>=0 then
   exit; // already registered
  if typInfo=nil then
    raise EWrapperContext.CreateUTF8('%.RegisterType(%): no RTTI',[typeWrapper^,typName]);
  case typ of
  wEnum: info := _JsonFastFmt('{name:?,values:%}',
          [typInfo^.EnumBaseType^.GetEnumNameAll(true)],[typName]);
  wSet:  info := _JsonFastFmt('{name:?,values:%}',
          [typInfo^.SetEnumType^.GetEnumNameAll(true)],[typName]);
  wRecord: begin
    parser := TTextWriter.RegisterCustomJSONSerializerFindParser(typInfo,true);
    if (parser<>nil) and (parser.Root<>nil) and (parser.Root.CustomTypeName<>'') then
      info := _ObjFast(['name',typName,'fields',ContextNestedProperties(parser.Root)]);
  end;
  wArray: begin
    item := typInfo^.DynArrayItemType(@itemSize);
    if item=nil then
      info := ContextFromInfo(TYPES_SIZE[itemSize]) else
      info := ContextFromInfo(wUnknown,'',item);
    info.name := typName;
  end;
  end;
  list.AddItem(info);
end;
var siz: integer;
    enum: PEnumType;
    unitName: RawUTF8;
begin
  if typ=wUnknown then begin
    if typInfo=nil then
      raise EWrapperContext.CreateUTF8('No RTTI nor typ for "%"',[typName]);
    typ := TYPES_ORM[typInfo.GetSQLFieldType];
    if typ=wUnknown then begin
      typ := TYPES_SIMPLE[TJSONCustomParserRTTI.TypeInfoToSimpleRTTIType(typInfo,0)];
      if typ=wUnknown then
      case typInfo^.Kind of
      tkRecord{$ifdef FPC},tkObject{$endif}:
        typ := wRecord;
      else
        raise EWrapperContext.CreateUTF8('Not enough RTTI for "%"',[typName]);
      end;
    end;
  end;
  if (typ=wRecord) and IdemPropNameU(typName,'TGUID') then
    typ := wGUID else
  if (typ=wRecord) and IdemPropNameU(typName,'TServiceCustomAnswer') then
    typ := wCustomAnswer else
    if typName='' then begin
      typName := TYPES_LANG[lngDelphi,typ];
      if (typName='') and (typInfo<>nil) then
        TypeInfoToName(typInfo,typName);
    end;
  typeWrapper := GetEnumName(TypeInfo(TWrapperType),ord(typ));
  result := _ObjFast([
    'typeWrapper',typeWrapper^,      'typeSource',typName,
    'typeDelphi',VarName(lngDelphi), 'typePascal',VarName(lngPascal),
    'typeCS',VarName(lngCS),         'typeJava',VarName(lngJava)]);
  if self=nil then
    exit; // no need to have full info if called e.g. from MVC
  if (typInfo<>nil) and (typInfo^.Kind=tkClass) then begin
    unitName := ShortStringToAnsi7String(typInfo^.ClassType^.UnitName);
    if not IdemPropNameU(unitName,'mORMot') then begin
      _ObjAddProps(['unitName',unitName],result);
      if fUnits.SearchItemByValue(unitName)<0 then
        fUnits.AddItem(unitName);
    end;
  end;
  case typ of
  wBoolean,wByte,wWord,wInteger,wCardinal,wInt64,wID,wReference,wTimeLog,
  wModTime,wCreateTime,wSingle,wDouble,wRawUTF8,wString: ; // simple types
  wDateTime:
    _ObjAddProps(['isDateTime',true,'toVariant','DateTimeToIso8601',
      'fromVariant','Iso8601ToDateTime'],result);
  wCurrency:
    _ObjAddProps(['isCurrency',true],result);
  wVariant:
    _ObjAddProps(['isVariant',true],result);
  wRawJSON:
    _ObjAddProps(['isJson',true],result);
  wEnum: begin
    _ObjAddProps(['isEnum',true,'toVariant','ord','fromVariant','Variant2'+typName],result);
    if self<>nil then
      RegisterType(fEnumerates);
  end;
  wSet: begin
    enum := typInfo^.SetEnumType;
    if enum=nil then
      siz := 0 else
      siz := enum^.SizeInStorageAsSet;
    _ObjAddProps(['isSet',true,'toVariant',SIZETODELPHI[siz],'fromVariant',typName],result);
    if self<>nil then
      RegisterType(fSets);
  end;
  wGUID:
    _ObjAddProps(['toVariant','GUIDToVariant','fromVariant','VariantToGUID'],result);
  wCustomAnswer:
    _ObjAddProps(['toVariant','HttpBodyToVariant','fromVariant','VariantToHttpBody'],result);
  wRecord: begin
     _ObjAddProps(['isRecord',true],result);
     if typInfo<>nil then begin
      _ObjAddProps(['toVariant',typName+'2Variant','fromVariant','Variant2'+typName],result);
      if self<>nil then
        RegisterType(fRecords);
    end;
  end;
  wObject,wSQLRecord: begin
    if (typ=wSQLRecord) and
       (fServer.Model.GetTableIndexInheritsFrom(TSQLRecordClass(typInfo^.ClassType^.ClassType))<0) then
      raise EWrapperContext.CreateUTF8('% should be part of the model',[typName]);
   _ObjAddProps(['isObject',true],result);
   if typInfo<>nil then
     _ObjAddProps(['toVariant','ObjectToVariant','fromVariant',typName+'.CreateFromVariant'],result);
  end;
  wArray: begin
    _ObjAddProps(['isArray',true],result);
    if typInfo<>nil then begin
      _ObjAddProps(['toVariant',typName+'2Variant','fromVariant','Variant2'+typName],result);
      if self<>nil then
        RegisterType(fArrays);
    end;
  end;
  wBlob:
    _ObjAddProps(['isBlob',true,
      'toVariant','BlobToVariant','fromVariant','VariantToBlob'],result);
  else raise EWrapperContext.CreateUTF8('Unexpected type % (%) for "%"',
    [typeWrapper^,ord(typ),typName]);
  end;
end;

function TWrapperContext.Context: variant;
var s: integer;
    authClass: TClass;
begin
  // compute the Model information as JSON
  result := _ObjFast(['time',NowToString, 'year',CurrentYear,
    'mORMotVersion',SYNOPSE_FRAMEWORK_VERSION,
    'orm',variant(fORM),
    'soa',fSOA]);
  if fServer<>nil then
    _ObjAddProps(['root',fServer.Model.Root],result);
  if fHasAnyRecord then
    result.ORMWithRecords := true; 
  if fRecords.Count>0 then begin
    result.records := variant(fRecords);
    result.withRecords := true;
    result.withHelpers := true;
  end;
  if fEnumerates.Count>0 then begin
    result.enumerates := variant(fEnumerates);
    result.withEnumerates := true;
    result.withHelpers := true;
  end;
  if fSets.Count>0 then begin
    result.sets := variant(fSets);
    result.withsets := true;
    result.withHelpers := true;
  end;
  if fArrays.Count>0 then begin
    result.arrays := variant(fArrays);
    result.withArrays := true;
    result.withHelpers := true;
  end;
  if fUnits.Count>0 then
    result.units := variant(fUnits);
  // add the first registered supported authentication class type as default
  if fServer<>nil then
    for s := 0 to fServer.AuthenticationSchemesCount-1 do begin
      authClass := fServer.AuthenticationSchemes[s].ClassType;
      if (authClass=TSQLRestServerAuthenticationDefault) or
         (authClass=TSQLRestServerAuthenticationNone) then begin
        result.authClass := authClass.ClassName;
        break;
      end;
    end;
end;

function ContextFromModel(aServer: TSQLRestServer): variant;
begin
  with TWrapperContext.CreateFromModel(aServer) do
  try
    result := Context;
  finally
    Free;
  end;
end;

function ContextFromMethod(const method: TServiceMethod): variant;
begin
  result := TWrapperContext(nil).ContextFromMethod(method);
end;

function ContextFromMethods(int: TInterfaceFactory): variant;
begin
  result := TWrapperContext(nil).ContextFromMethods(int);
end;


procedure WrapperMethod(Ctxt: TSQLRestServerURIContext; const Path: array of TFileName);
var root, templateName, templateTitle, savedName, templateExt, unitName, template,
    result, host, uri, head: RawUTF8;
    context: variant;
    SR: TSearchRec;
    i, templateFound, port: integer;
begin // URI is e.g. GET http://localhost:888/root/wrapper/Delphi/UnitName.pas
  if (Ctxt.Method<>mGET) or (high(Path)<0) then
    exit;
  templateFound := -1;
  for i := 0 to high(Path) do
    if FindFirst(Path[i]+'\*.mustache',faAnyFile,SR)=0 then begin
      templateFound := i;
      break;
    end;
  if templateFound<0 then begin
    Ctxt.Error('Please copy some .mustache files in the expected folder (e.g. %)',
      [ExpandFileName(Path[0])]);
    exit;
  end;
  context := ContextFromModel(Ctxt.Server);
  context.uri := Ctxt.URIWithoutSignature;
  if llfSSL in Ctxt.Call^.LowLevelFlags then begin
    context.protocol := 'https';
    context.https := true;
  end else
    context.protocol := 'http';
  host := Ctxt.InHeader['host'];
  if host<>'' then
    context.host := host;
  port := GetInteger(pointer(split(host,':',host)));
  if port=0 then
    port := 80;
  context.port := port;
  if IdemPropNameU(Ctxt.URIBlobFieldName,'context') then begin
    Ctxt.Returns(JSONReformat(VariantToUTF8(context),jsonUnquotedPropName),200,
      TEXT_CONTENT_TYPE_HEADER);
    exit;
  end;
  root := Ctxt.Server.Model.Root;
  if Ctxt.URIBlobFieldName='' then begin
    result := '<html><title>mORMot Wrappers</title>'+
      '<body style="font-family:verdana;"><h1>Generated Code Wrappers</h1>'+
      '<hr><h2>Available Templates:</h2><ul>';
    repeat
      Split(StringToUTF8(SR.Name),'.',templateName,templateExt);
      templateTitle := templateName;
      i := PosEx('-',templateName);
      if i>0 then begin
        SetLength(templateTitle,i-1);
        savedName := copy(templateName,i+1,maxInt);
      end else
        savedName := 'mORMotClient';
      Split(templateExt,'.',templateExt);
      uri := FormatUTF8('<a href=/%/wrapper/%/%.%',
        [root,templateName,savedName,templateExt]);
      result := FormatUTF8(
       '%<li><b>%</b><br><i>%.%</i>  -  %>download as file</a>  -  '+
       '%.txt>see as text</a> - %.mustache>see template</a></li><br>',
       [result,templateTitle,savedName,templateExt,uri,uri,uri]);
    until FindNext(SR)<>0;
    FindClose(SR);
    result := FormatUTF8('%</ul><p>You can also retrieve the corresponding '+
      '<a href=/%/wrapper/context>template context</a>.<hr><p>Generated by a '+
      '<a href=http://mormot.net>Synopse <i>mORMot</i> '+SYNOPSE_FRAMEWORK_VERSION+
      '</a> server.',[result,root]);
    Ctxt.Returns(result,HTML_SUCCESS,HTML_CONTENT_TYPE_HEADER);
    exit;
  end else
    FindClose(SR);
  Split(Ctxt.URIBlobFieldName,'/',templateName,unitName);
  Split(unitName,'.',unitName,templateExt);
  if PosEx('.',templateExt)>0 then begin // see as text
    if IdemPropNameU(Split(templateExt,'.',templateExt),'mustache') then
      unitName := ''; // force return .mustache
    head := TEXT_CONTENT_TYPE_HEADER;
  end else // download as file
    head := HEADER_CONTENT_TYPE+'application/'+LowerCase(templateExt);
  templateName := templateName+'.'+templateExt+'.mustache';
  template := StringFromFile(Path[templateFound]+UTF8ToString('\'+templateName));
  if template='' then begin
    Ctxt.Error(templateName,HTML_NOTFOUND);
    exit;
  end;
  if unitName='' then
    result := template else begin
    context.templateName := templateName;
    context.filename := unitName;
    result := TSynMustache.Parse(template).Render(context,nil,nil,nil,true);
  end;
  Ctxt.Returns(result,HTML_SUCCESS,head);
end;

function WrapperFromModel(aServer: TSQLRestServer;
  const aMustacheTemplate, aFileName: RawUTF8; aPort: integer): RawUTF8;
var context: variant;
begin
  context := ContextFromModel(aServer); // no context.uri nor context.host here
  if aPort=0 then
    aPort := 80;
  context.port := aPort;
  context.filename := aFileName;
  result := TSynMustache.Parse(aMustacheTemplate).Render(context,nil,nil,nil,true); 
end;


{ TWrapperMethodHook }

type
  TWrapperMethodHook = class(TPersistent)
  public
    SearchPath: TFileNameDynArray;
  published
    procedure Wrapper(Ctxt: TSQLRestServerURIContext);
  end;

procedure TWrapperMethodHook.Wrapper(Ctxt: TSQLRestServerURIContext);
begin
  WrapperMethod(Ctxt,SearchPath);
end;

procedure ComputeSearchPath(const Path: array of TFileName;
  out SearchPath: TFileNameDynArray);
var i: integer;
begin
  if length(Path)=0 then begin
    SetLength(SearchPath,1);
    SearchPath[0] := ExtractFilePath(paramstr(0)); // use .exe path
  end else begin
    SetLength(SearchPath,length(Path));
    for i := 0 to high(Path) do
      SearchPath[i] := Path[i];
  end;
end;

procedure AddToServerWrapperMethod(Server: TSQLRestServer; const Path: array of TFileName);
var hook: TWrapperMethodHook;
begin
  if Server=nil then
    exit;
  hook := TWrapperMethodHook.Create;
  Server.PrivateGarbageCollector.Add(hook); // Server.Free will call hook.Free
  ComputeSearchPath(Path,hook.SearchPath);
  Server.ServiceMethodRegisterPublishedMethods('',hook);
  Server.ServiceMethodByPassAuthentication('wrapper');
end;


function FindTemplate(const TemplateName: TFileName; const Path: array of TFileName): TFileName;
var SearchPath: TFileNameDynArray;
    i: integer;
begin
  ComputeSearchPath(Path,SearchPath);
  for i := 0 to High(SearchPath) do begin
    result := IncludeTrailingPathDelimiter(SearchPath[i])+TemplateName;
    if FileExists(result) then
      exit;
  end;
  result := '';
end;

procedure ComputeFPCServerUnit(Server: TSQLRestServer; const Path: array of TFileName;
  DestFileName: TFileName);
var TemplateName: TFileName;
begin
  TemplateName := FindTemplate('FPCServer-mORMotServer.pas.mustache',Path);
  if TemplateName='' then
    exit;
  if DestFileName='' then
    DestFileName := 'mORMotServer.pas' else
    if DestFileName[1]='\' then
      DestFileName := ExtractFilePath(TemplateName)+DestFileName;
  FileFromString(WrapperFromModel(Server,StringFromFile(TemplateName),
    StringToUTF8(ExtractFileName(DestFileName)),0),DestFileName);
end;

procedure ComputeFPCInterfacesUnit(const Path: array of TFileName;
  DestFileName: TFileName);
const TEMPLATE_NAME = 'FPC-mORMotInterfaces.pas.mustache';
var TemplateName: TFileName;
    ctxt: variant;
begin
  TemplateName := FindTemplate(TEMPLATE_NAME,Path);
  if TemplateName='' then
    exit;
  if DestFileName='' then
    DestFileName := 'mORMotInterfaces.pas' else
    if DestFileName[1]='\' then
      DestFileName := ExtractFilePath(TemplateName)+DestFileName;
  with TWrapperContext.CreateFromUsedInterfaces do
  try
    ctxt := Context;
  finally
    Free;
  end;
  ctxt.fileName := GetFileNameWithoutExt(ExtractFileName(DestFileName));
  FileFromString(TSynMustache.Parse(StringFromFile(TemplateName)).
    Render(ctxt,nil,nil,nil,true),DestFileName);
end;

end.

