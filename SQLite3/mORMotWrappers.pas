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

  ---------------------------------------------------------------------------
   Download the SpiderMonkey library at http://synopse.info/files/synsm.7z !
  ---------------------------------------------------------------------------


  Version 1.18
  - first public release, corresponding to Synopse mORMot Framework 1.18

}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  Windows,
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
function ContextFromModel(const aServer: TSQLRestServer): variant;

/// you can call this procedure within a method-based service to return
// all available client .template files in the supplied paths 
// - for instance:
// ! procedure TCustomServer.Wrapper(Ctxt: TSQLRestServerURIContext);
// ! begin // search in the current path
// !   WrapperMethod(Ctxt,['.']);
// ! end;
procedure WrapperMethod(Ctxt: TSQLRestServerURIContext; const Path: array of TFileName);


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
 // sftUnknown, sftAnsiText, sftUTF8Text, sftEnumerate, sftSet, sftInteger,
    cpkDefault,cpkDefault,cpkDefault,cpkDefault,cpkDefault,cpkDefault,
 // sftID, sftRecord, sftBoolean, sftFloat, sftDateTime, sftTimeLog, sftCurrency,
    cpkDefault,cpkDefault,cpkDefault,cpkDefault,cpkDateTime,cpkTimeLog,cpkDefault,
 // sftObject, {$ifndef NOVARIANTS} sftVariant, {$endif} sftBlob, sftBlobDynArray,
    cpkDefault,{$ifndef NOVARIANTS}cpkVariant,{$endif}cpkBlob,cpkDefault,
 // sftBlobCustom, sftUTF8Custom, sftMany, sftModTime, sftCreateTime
    cpkDefault,  cpkRecord,   cpkDefault,  cpkModTime, cpkCreateTime);
  CONST_SIZETODELPHI: array[0..4] of string[7] = (
    'integer','byte','word','integer','integer');

function NULL_OR_CARDINAL(Value: Integer): RawUTF8;
begin
  if Value>0 then
    UInt32ToUtf8(Value,result) else
    result := 'null';
end;

function ContextFromModel(const aServer: TSQLRestServer): variant;
const
  TYPETOSIMPLE: array[TSQLFieldType] of TJSONCustomParserRTTIType = 
    (ptCustom,   // sftUnknown
     ptString,   // sftAnsiText
     ptRawUTF8,  // sftUTF8Text
     ptCustom,   // sftEnumerate
     ptCustom,   // sftSet
     ptInteger,  // sftInteger
     ptPtrUInt,  // sftID
     ptPtrUInt,  // sftRecord
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
     ptTimeLog); // sftCreateTime
var orm,fields,records,enumerates,sets: TDocVariantData;
    rec,field: variant;
    nfoList: TSQLPropInfoList;
    nfo: TSQLPropInfo;
    t,f,s: integer;
    typ: TJSONCustomParserRTTIType;
    kind: TCrossPlatformSQLFieldKind;
    hasRecord: boolean;
    parser: TJSONCustomParserRTTI;
    parsersPropInfo: TRawUTF8List;
    parsersServices: TRawUTF8List;
    typeNames: TRawUTF8List;
function nfoTypeNotRegistered: Boolean;
begin
  result := typeNames.IndexOf(nfo.SQLFieldRTTITypeName)<0;
  if not result then
    typeNames.AddObject(nfo.SQLFieldRTTITypeName,nfo);
end;
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
  typeNames := TRawUTF8List.Create;
  parsersPropInfo := TRawUTF8List.Create;
  parsersServices := TRawUTF8List.Create;
  try
    typeNames.CaseSensitive := false;
    parsersPropInfo.CaseSensitive := false;
    parsersServices.CaseSensitive := false;
    for t := 0 to aServer.Model.TablesMax do begin
      hasRecord := false;
      nfoList := aServer.Model.TableProps[t].Props.Fields;
      fields.Clear;
      fields.Init;
      for f := 0 to nfoList.Count-1 do begin
        nfo := nfoList.List[f];
        kind := CROSSPLATFORM_KIND[nfo.SQLFieldType];
        typ := TJSONCustomParserRTTI.TypeNameToSimpleRTTIType(nfo.SQLFieldRTTITypeName);
        if typ=ptCustom then // guess from SQL type
          typ := TYPETOSIMPLE[nfo.SQLFieldType];
        field := TJSONCustomParserRTTI.ContextProperty(typ,nfo.SQLFieldRTTITypeName,'','');
        field.index := f+1;
        field.name := nfo.Name;
        field.sql := ord(nfo.SQLFieldType);
        field.sqlName := nfo.SQLFieldTypeName^;
        field.typeKind := ord(kind);
        field.typeKindName := CROSSPLATFORMKIND_TEXT[kind];
        field.attr := byte(nfo.Attributes);
        if aIsUnique in nfo.Attributes then
          field.unique := true;
        if nfo.FieldWidth>0 then
          field.width := nfo.FieldWidth;
        if f<nfoList.Count-1 then
          field.comma := ',';
        case nfo.SQLFieldType of // handle some special complex types
        sftEnumerate: begin
          field.isEnum := true;
          field.ToVariant := 'ord';
          field.fromVariant := nfo.SQLFieldRTTITypeName;
        end;
        sftSet: begin
          field.isSet := true;
          if nfo.InheritsFrom(TSQLPropInfoRTTISet) then
            field.toVariant := CONST_SIZETODELPHI[
              TSQLPropInfoRTTISet(nfo).SetEnumType^.SizeInStorageAsSet] else
            field.toVariant := 'byte';
          field.fromVariant := nfo.SQLFieldRTTITypeName;
        end;
        end;
        fields.AddItem(field);
        if nfo.InheritsFrom(TSQLPropInfoCustomJSON) then begin
          parser := TSQLPropInfoCustomJSON(nfo).CustomParser;
          if (parser<>nil) and (parser.PropertyType in [ptRecord,ptCustom]) then begin
            hasRecord := true;
            if nfoTypeNotRegistered then
              parsersPropInfo.AddObjectIfNotExisting(nfo.SQLFieldRTTITypeName,nfo);
          end;
        end else
        if nfo.InheritsFrom(TSQLPropInfoRTTIEnum) then begin
          if nfoTypeNotRegistered then
            enumerates.AddItem(_JsonFastFmt('{name:?,values:%}',
              [TSQLPropInfoRTTIEnum(nfo).EnumType^.GetEnumNameAll(true)],
              [nfo.SQLFieldRTTITypeName]));
        end else
        if nfo.InheritsFrom(TSQLPropInfoRTTISet) then begin
          if nfoTypeNotRegistered then
            sets.AddItem(_JsonFastFmt('{name:?,values:%}',
              [TSQLPropInfoRTTISet(nfo).SetEnumType^.GetEnumNameAll(true)],
              [nfo.SQLFieldRTTITypeName]));
        end;
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
      'mORMotVersion',SYNOPSE_FRAMEWORK_VERSION,
      'root',aServer.Model.Root, 'orm',variant(orm),
      'soa',aServer.Services.ContextFromRegisteredServices(parsersServices)]);
    // add the traling RTTI defined for services to the list
    for s := 0 to parsersServices.Count-1 do
      with TJSONCustomParserRTTI(parsersServices.Objects[s]) do
      if typeNames.IndexOf(CustomTypeName)<0 then
        if PropertyType=ptRecord then
          records.AddItem(
            _ObjFast(['name',CustomTypeName,
              'fields',ContextNestedProperties(parsersServices)]));
    for s := 0 to parsersServices.Count-1 do
      with TJSONCustomParserRTTI(parsersServices.Objects[s]) do
        if InheritsFrom(TJSONCustomParserCustomSimple) then
        with TJSONCustomParserCustomSimple(parsersServices.Objects[s]) do
         if KnownType=ktEnumeration then
            enumerates.AddItem(_JsonFastFmt('{name:?,values:%}',
              [PTypeInfo(CustomTypeInfo)^.EnumBaseType^.GetEnumNameAll(true)],
              [CustomTypeName]));
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
    typeNames.Free;
  end;
end;


procedure WrapperMethod(Ctxt: TSQLRestServerURIContext; const Path: array of TFileName);
var root, templateName, templateExt, unitName, template, result, host, uri, head: RawUTF8;
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
    Ctxt.Error('Please copy some .template files in the expected folder (e.g. %)',
      [ExpandFileName(Path[0])]);
    exit;
  end;
  context := ContextFromModel(Ctxt.Server);
  context.uri := Ctxt.URIWithoutSignature;
  host := Ctxt.InHeader['host'];
  context.host := host;
  port := GetInteger(pointer(split(host,':',host)));
  if port>0 then
    context.port := port;
  if IdemPropNameU(Ctxt.URIBlobFieldName,'context') then begin
    Ctxt.Returns(VariantToUTF8(context));
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
    delete(templateExt,1,1);
    result := TSynMustache.Parse(template).Render(context);
  end;
  Ctxt.Returns(result,HTML_SUCCESS,head);
end;


end.
