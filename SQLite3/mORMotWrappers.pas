/// service to generate mORMot cross-platform clients code from the server
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit mORMotWrappers;

{
    This file is part of Synopse mORmot framework.

    Synopse mORMot framework. Copyright (C) 2014 Arnaud Bouchez
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

  Portions created by the Initial Developer are Copyright (C) 2014
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

  CROSSPLATFORM_KIND: array[TSQLFieldType] of TCrossPlatformSQLFieldKind = (
 // sftUnknown, sftAnsiText, sftUTF8Text, sftEnumerate, sftSet,    sftInteger,
    cpkDefault, cpkDefault,  cpkDefault,  cpkDefault,   cpkDefault,cpkDefault,
 // sftID,     sftRecord, sftBoolean,sftFloat,  sftDateTime,sftTimeLog,sftCurrency,
    cpkDefault,cpkDefault,cpkDefault,cpkDefault,cpkDateTime,cpkTimeLog,cpkDefault,
 // sftObject,                     sftVariant,         sftBlob, sftBlobDynArray,
    cpkDefault,{$ifndef NOVARIANTS}cpkVariant,{$endif} cpkBlob, cpkDefault,
 // sftBlobCustom,sftUTF8Custom,sftMany, sftModTime,sftCreateTime, sftTID
    cpkDefault,   cpkRecord,  cpkDefault,cpkModTime,cpkCreateTime, cpkDefault);
  CONST_SIZETODELPHI: array[0..4] of string[7] = (
    'integer','byte','word','integer','integer');

function NULL_OR_CARDINAL(Value: Integer): RawUTF8;
begin
  if Value>0 then
    UInt32ToUtf8(Value,result) else
    result := 'null';
end;

function ContextFromModel(aServer: TSQLRestServer): variant;
const
  TYPETOSIMPLE: array[TSQLFieldType] of TJSONCustomParserRTTIType =
    (ptCustom,   // sftUnknown
     ptString,   // sftAnsiText
     ptRawUTF8,  // sftUTF8Text
     ptCustom,   // sftEnumerate
     ptCustom,   // sftSet
     ptInt64,    // sftInteger
     ptInt64,    // sftID
     ptInt64,    // sftRecord
     ptBoolean,  // sftBoolean
     ptDouble,   // sftFloat
     ptDateTime, // sftDateTime
     ptTimeLog,  // sftTimeLog
     ptCurrency, // sftCurrency
     ptCustom,   // sftObject
{$ifndef NOVARIANTS}
     ptVariant,  // sftVariant
{$endif}
     ptRawByteString, // sftBlob
     ptRawByteString, // sftBlobDynArray
     ptRawByteString, // sftBlobCustom
     ptRecord,   // sftUTF8Custom
     ptCustom,   // sftMany
     ptTimeLog,  // sftModTime
     ptTimeLog,  // sftCreateTime
     ptInt64);   // sftID
var orm,fields,records,enumerates,sets: TDocVariantData;
    rec,field: variant;
    nfoList: TSQLPropInfoList;
    nfo: TSQLPropInfo;
    t,f,s: integer;
    typ: TJSONCustomParserRTTIType;
    kind: TCrossPlatformSQLFieldKind;
    hasRecord: boolean;
    nfoSQLFieldRTTITypeName: RawUTF8;
    parser: TJSONCustomParserRTTI;
    parsersPropInfo: TRawUTF8List;
    parsersServices: TRawUTF8List;
    typeNames: TPropNameList;
    rtti: TJSONCustomParserRTTI;
    simple: TJSONCustomParserCustomSimple;
    authClass: TClass;
begin
  SetVariantNull(result);
  if aServer=nil then
    exit;
  // compute ORM Model information
  orm.Init;
  records.Init;
  enumerates.Init;
  sets.Init;
  fields.Init;
  typeNames.Init;
  parsersPropInfo := TRawUTF8List.Create;
  parsersServices := TRawUTF8List.Create;
  try
    parsersPropInfo.CaseSensitive := false;
    parsersServices.CaseSensitive := false;
    for t := 0 to aServer.Model.TablesMax do begin
      hasRecord := false;
      nfoList := aServer.Model.TableProps[t].Props.Fields;
      fields.Clear;
      fields.Init;
      for f := 0 to nfoList.Count-1 do begin
        nfo := nfoList.List[f];
        nfoSQLFieldRTTITypeName := nfo.SQLFieldRTTITypeName;
        kind := CROSSPLATFORM_KIND[nfo.SQLFieldType];
        typ := TJSONCustomParserRTTI.TypeNameToSimpleRTTIType(nfoSQLFieldRTTITypeName);
        if typ=ptCustom then // guess from SQL type
          typ := TYPETOSIMPLE[nfo.SQLFieldType];
        field := TJSONCustomParserRTTI.ContextProperty(typ,nfoSQLFieldRTTITypeName,'','');
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
        case nfo.SQLFieldType of // handle some special complex types
        sftEnumerate:
          _ObjAddProps(['isEnum',True,'ToVariant','ord',
            'fromVariant',nfoSQLFieldRTTITypeName],field);
        sftSet: begin
          _ObjAddProps(['isSet',True,'fromVariant',nfoSQLFieldRTTITypeName],field);
          if nfo.InheritsFrom(TSQLPropInfoRTTISet) then
            _ObjAddProps(['toVariant',CONST_SIZETODELPHI[
              TSQLPropInfoRTTISet(nfo).SetEnumType^.SizeInStorageAsSet]],field) else
            _ObjAddProps(['toVariant','byte'],field);
        end;
        end;
        if nfo.InheritsFrom(TSQLPropInfoCustomJSON) then begin
          parser := TSQLPropInfoCustomJSON(nfo).CustomParser;
          if (parser<>nil) and (parser.PropertyType in [ptRecord,ptCustom]) then begin
            hasRecord := true;
            if typ=ptGuid then
              _ObjAddProps(['fromVariant','VariantToGUID','toVariant','GUIDToVariant'],field) else
              if typeNames.AddPropName(nfoSQLFieldRTTITypeName) then
                parsersPropInfo.AddObjectIfNotExisting(nfoSQLFieldRTTITypeName,nfo);
          end;
        end else
        if nfo.InheritsFrom(TSQLPropInfoRTTIEnum) then begin
          if (typ<>ptBoolean) and typeNames.AddPropName(nfoSQLFieldRTTITypeName) then
            enumerates.AddItem(_JsonFastFmt('{name:?,values:%}',
              [TSQLPropInfoRTTIEnum(nfo).EnumType^.GetEnumNameAll(true)],
              [nfoSQLFieldRTTITypeName]));
        end else
        if nfo.InheritsFrom(TSQLPropInfoRTTISet) then begin
          if typeNames.AddPropName(nfoSQLFieldRTTITypeName) then
            sets.AddItem(_JsonFastFmt('{name:?,values:%}',
              [TSQLPropInfoRTTISet(nfo).SetEnumType^.GetEnumNameAll(true)],
              [nfoSQLFieldRTTITypeName]));
        end;
        fields.AddItem(field);
      end;
      with aServer.Model.TableProps[t] do
        rec := _JsonFastFmt('{tableName:?,className:?,fields:?,isInMormotPas:%,comma:%}',
          [NULL_OR_TRUE[(Props.Table=TSQLAuthGroup) or (Props.Table=TSQLAuthUser)],
           NULL_OR_COMMA[t<aServer.Model.TablesMax]],
          [Props.SQLTableName,Props.Table.ClassName,Variant(fields)]);
      if hasRecord then
        rec.hasRecords := true;
      orm.AddItem(rec);
    end;
    for t := 0 to parsersPropInfo.Count-1 do
      records.AddItem(_ObjFast(['name',parsersPropInfo.Strings[t],
        'fields',TSQLPropInfoCustomJSON(parsersPropInfo.Objects[t]).
          CustomParser.ContextNestedProperties(parsersServices)]));
    // compute the Model information as JSON
    result := _ObjFast(['time',NowToString,'year',TimeLogNow shr (6+6+5+5+4),
      'mORMotVersion',SYNOPSE_FRAMEWORK_VERSION, 'root',aServer.Model.Root,
      'orm',variant(orm),
      'soa',aServer.Services.ContextFromRegisteredServices(parsersServices)]);
    // add the first registered supported authentication class type as default 
    for s := 0 to aServer.AuthenticationSchemesCount-1 do begin
      authClass := aServer.AuthenticationSchemes[s].ClassType;
      if (authClass=TSQLRestServerAuthenticationDefault) or
         (authClass=TSQLRestServerAuthenticationNone) then begin
        result.authClass := authClass.ClassName;
        break;
      end;
    end;
    // add the traling RTTI defined for services to the list
    for s := 0 to parsersServices.Count-1 do begin
      rtti := TJSONCustomParserRTTI(parsersServices.Objects[s]);
      if rtti.PropertyType=ptRecord then
        if typeNames.AddPropName(rtti.CustomTypeName) then
          records.AddItem(
            _ObjFast(['name',rtti.CustomTypeName,
              'fields',rtti.ContextNestedProperties(parsersServices)]));
    end;
    for s := 0 to parsersServices.Count-1 do begin
      simple := TJSONCustomParserCustomSimple(parsersServices.Objects[s]);
      if simple.InheritsFrom(TJSONCustomParserCustomSimple) then begin
        if simple.KnownType=ktEnumeration then begin
          if typeNames.AddPropName(simple.CustomTypeName) then
            enumerates.AddItem(_JsonFastFmt('{name:?,values:%}',
              [PTypeInfo(simple.CustomTypeInfo)^.EnumBaseType^.GetEnumNameAll(true)],
              [simple.CustomTypeName]));
          if simple.PropertyName='' then // <>'' if initialized from record
            // TServiceMethodArgument.ContextFromArguments() simple is temporary
            simple.Free;
        end;
      end;
    end;
    if records.Count>0 then begin
      result.records := variant(records);
      result.withRecords := true;
    end;
    if enumerates.Count>0 then begin
      result.enumerates := variant(enumerates);
      result.withEnumerates := true;
    end;
    if sets.Count>0 then begin
      result.sets := variant(sets);
      result.withsets := true;
    end;
  finally
    parsersServices.Free;
    parsersPropInfo.Free;
  end;
end;


procedure WrapperMethod(Ctxt: TSQLRestServerURIContext; const Path: array of TFileName);
var root, templateName, templateExt, unitName, template,
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
    result := '<html><body style="font-family:verdana;"><h1>Client Wrappers</h1>'+
      '<hr><h2>Available Templates:</h2><ul>';
    repeat
      Split(StringToUTF8(SR.Name),'.',templateName,templateExt);
      Split(templateExt,'.',templateExt);
      uri := FormatUTF8('<a href=/%/wrapper/%/mORMotClient.%',
        [root,templateName,templateExt,templateName]);
      result := FormatUTF8(
       '%<li><b>%</b><br><i>mORMotClient.%</i>  -  %>download as file</a>  -  '+
       '%.txt>see as text</a> - %.mustache>see template</a></li><br>',
       [result,templateName,templateExt,uri,uri,uri]);
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
    SearchPath: array of TFileName;
  published
    procedure Wrapper(Ctxt: TSQLRestServerURIContext);
  end;

procedure TWrapperMethodHook.Wrapper(Ctxt: TSQLRestServerURIContext);
begin
  WrapperMethod(Ctxt,SearchPath);
end;

procedure AddToServerWrapperMethod(Server: TSQLRestServer; const Path: array of TFileName);
var hook: TWrapperMethodHook;
    i: integer;
begin
  if Server=nil then
    exit;
  hook := TWrapperMethodHook.Create;
  Server.PrivateGarbageCollector.Add(hook); // Server.Free will call hook.Free
  if length(Path)=0 then begin
    SetLength(hook.SearchPath,1);
    hook.SearchPath[0] := ExtractFilePath(paramstr(0)); // use .exe path
  end else begin
    SetLength(hook.SearchPath,length(Path));
    for i := 0 to high(Path) do
      hook.SearchPath[i] := Path[i];
  end;
  Server.ServiceMethodRegisterPublishedMethods('',hook);
  Server.ServiceMethodByPassAuthentication('wrapper');
end;

end.
