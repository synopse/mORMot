/// Logic-less {{mustache}} template rendering
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynMustache;

{
    This file is part of Synopse mORMot framework.

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


  Version 1.18
  - initial revision

}


{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  {$ifdef HASINLINE}
  Windows, // for Lock/UnLock inlining
  {$endif}
  Variants,
  SysUtils,
  SynCommons;


type
  /// exception raised during process of a {{mustache}} template
  ESynMustache = class(ESynException);

  /// identify the {{mustache}} tag kind
  // - mtVariable if the tag is a variable - e.g. {{myValue}}
  // - mtVariableUnescaped to unescape the variable HTML - e.g.
  // {{{myRawValue}}} or {{& name}}
  // - mtSection and mtInvertedSection for sections beginning - e.g.
  // {{#person}} or {{^person}}
  // - mtSectionEnd for sections ending - e.g. {{/person}}
  // - mtComment for comments - e.g. {{! ignore me}}
  // - mtPartial for partials - e.g. {{> next_more}}
  // - mtSetPartial for setting an internal partial - e.g.
  // {{<foo}}This is the foo partial {{myValue}} template{{/foo}}
  // - mtSetDelimiter for setting custom delimeter symbols - e.g. {{=<% %>=}} -
  // Warning: current implementation only supports two character delimiters
  // - mtTranslate for content i18n via a callback - e.g. {{"English text}}
  // - mtText for all text that appears outside a symbol
  TSynMustacheTagKind = (
    mtVariable, mtVariableUnescape,
    mtSection, mtInvertedSection, mtSectionEnd,
    mtComment, mtPartial, mtSetPartial, mtSetDelimiter, mtTranslate, mtText);

  /// store a {{mustache}} tag
  TSynMustacheTag = record
    /// the kind of the tag
    Kind: TSynMustacheTagKind;
    /// points to the mtText buffer start
    // - main template's text is not allocated as a separate string during
    // parsing, but will rather be copied directly from the template memory
    TextStart: PUTF8Char;
    /// stores the mtText buffer length
    TextLen: integer;
    /// the index in Tags[] of the other end of this section
    // - either the index of mtSectionEnd for mtSection/mtInvertedSection
    // - or the index of mtSection/mtInvertedSection for mtSectionEnd
    SectionOppositeIndex: integer;
    /// the tag content, excluding trailing {{ }} and corresponding symbol
    // - is not set for mtText nor mtSetDelimiter
    Value: RawUTF8;
  end;

  /// store all {{mustache}} tags of a given template
  TSynMustacheTagDynArray = array of TSynMustacheTag;

  /// states the section content according to a given value
  // - msNothing for false values or empty lists
  // - msSingle for non-false values but not a list
  // - msList for non-empty lists
  TSynMustacheSectionType = (msNothing,msSingle,msSinglePseudo,msList);

  TSynMustache = class;
  
  /// handle {{mustache}} template rendering context, i.e. all values
  // - this abstract class should not be used directly, but rather any
  // other the overridden classes
  TSynMustacheContext = class
  protected
    fContextCount: integer;
    fWriter: TTextWriter;
    fOwner: TSynMustache;
    fOnStringTranslate: TOnStringTranslate;
    procedure TranslateBlock(Text: PUTF8Char; TextLen: Integer); virtual;
    procedure PopContext; virtual; abstract;
    procedure AppendValue(const ValueName: RawUTF8; UnEscape: boolean);
      virtual; abstract;
    function AppendSection(const ValueName: RawUTF8): TSynMustacheSectionType;
      virtual; abstract;
    function GotoNextListItem: boolean;
      virtual; abstract;
  public
    /// initialize the rendering context for the given text writer
    constructor Create(Owner: TSynMustache; WR: TTextWriter);
    /// access to the {{"English text}} translation callback
    property OnStringTranslate: TOnStringTranslate
      read fOnStringTranslate write fOnStringTranslate;
    /// read-only access to the associated text writer instance
    property Writer: TTextWriter read fWriter;
  end;

  /// handle {{mustache}} template rendering context from a custom variant
  // - the context is given via a custom variant type implementing
  // TSynInvokeableVariantType.Lookup, e.g. TDocVariant or TSMVariant
  TSynMustacheContextVariant = class(TSynMustacheContext)
  protected
    fContext: array of record
      Document: TVarData;
      DocumentType: TSynInvokeableVariantType;
      ListCount: integer;
      ListCurrent: integer;
      ListCurrentDocument: TVarData;
      ListCurrentDocumentType: TSynInvokeableVariantType;
    end;
    procedure PushContext(aDoc: TVarData);
    procedure PopContext; override;
    procedure AppendValue(const ValueName: RawUTF8; UnEscape: boolean); override;
    function AppendSection(const ValueName: RawUTF8): TSynMustacheSectionType; override;
    function GotoNextListItem: boolean; override;
    function GetDocumentType(const aDoc: TVarData): TSynInvokeableVariantType;
    procedure GetValueFromContext(const ValueName: RawUTF8; var Value: TVarData);
    procedure AppendVariant(const Value: variant; UnEscape: boolean);
  public
    /// initialize the context from a custom variant document
    // - note that the aDocument instance shall be available during all
    // lifetime of this TSynMustacheContextVariant instance
    // - you should not use this constructor directly, but the
    // corresponding TSynMustache.Render*() methods
    constructor Create(Owner: TSynMustache; WR: TTextWriter; SectionMaxCount: integer;
       const aDocument: variant);
  end;

  /// maintain a list of {{mustache}} partials
  // - this list of partials template could be supplied to TSynMustache.Render()
  // method, to render {{>partials}} as expected
  // - using a dedicated class allows to share the partials between execution
  // context, without recurring to non SOLID global variables
  // - you may also define "internal" partials, e.g. {{<foo}}This is foo{{/foo}}
  TSynMustachePartials = class
  protected
    fList: TRawUTF8ListHashed;
    fOwned: boolean;
    function GetPartial(const PartialName: RawUTF8): TSynMustache;
  public
    /// initialize the template partials storage
    // - after creation, the partials should be registered via the Add() method
    // - you shall manage this instance life time with a try..finally Free block
    constructor Create; overload;
    /// initialize a template partials storage with the supplied templates
    // - partials list is expected to be supplied in Name / Template pairs
    // - this instance can be supplied as parameter to the TSynMustache.Render()
    // method, which will free the instances as soon as it finishes
    constructor CreateOwned(const NameTemplatePairs: array of RawUTF8); overload;
    /// initialize a template partials storage with the supplied templates
    // - partials list is expected to be supplied as a dvObject TDocVariant,
    // each member being the name/template string pairs
    // - if the supplied variant is not a matching TDocVariant, will return nil
    // - this instance can be supplied as parameter to the TSynMustache.Render()
    // method, which will free the instances as soon as it finishes
    class function CreateOwned(const Partials: variant): TSynMustachePartials; overload;
    /// register a {{>partialName}} template
    procedure Add(const aName,aTemplate: RawUTF8); overload;
    /// register a {{>partialName}} template
    procedure Add(const aName: RawUTF8; aTemplateStart,aTemplateEnd: PUTF8Char); overload;
    /// delete the partials
    destructor Destroy; override;
  end;
  
  /// stores one {{mustache}} pre-rendered template
  // - once parsed, a template will be stored in this class instance, to be
  // rendered lated via the Render() method
  // - you can use the Parse() class function to maintain a shared cache of
  // parsed templates
  // - implements all official mustache specifications, and some extensions
  // - handles {{.}} pseudo-variable for the current context object (very
  // handy when looping through a simple list, for instance)
  // - handles {{-index}} pseudo-variable for the current context array index
  // (1-based value) so that e.g.
  // "My favorite things:\n{{#things}}{{-index}}. {{.}}\n{{/things}}"
  // over {things:["Peanut butter", "Pen spinning", "Handstands"]} renders as
  // "My favorite things:\n1. Peanut butter\n2. Pen spinning\n3. Handstands\n"
  // - handles -first  -last  and  -odd  pseudo-section keys, e.g.
  // "{{#things}}{{^-first}}, {{/-first}}{{.}}{{/things}}"
  // over {things:["one", "two", "three"]} renders as 'one, two, three'
  // - allows inlined partial templates , to be defined e.g. as
  // {{<foo}}This is the foo partial {{myValue}} template{{/foo}}
  // - features {{"English text}} translation, via a custom callback
  // - this implementation is thread-safe and re-entrant (i.e. the same
  // TSynMustache instance can be used by several threads at once)
  TSynMustache = class
  protected
    fTemplate: RawUTF8;
    fTags: TSynMustacheTagDynArray;
    fInternalPartials: TSynMustachePartials;
    fSectionMaxCount: Integer;
  public
    /// parse a {{mustache}} template, and returns the corresponding
    // TSynMustache instance
    // - an internal cache is maintained by this class function
    // - this implementation is thread-safe and re-entrant: i.e. the same
    // TSynMustache returned instance can be used by several threads at once 
    class function Parse(const aTemplate: RawUTF8): TSynMustache;
    /// remove the specified {{mustache}} template from the internal cache
    // - returns TRUE on success, and FALSE if the template was not cached
    // by a previous call to Parse() class function
    class function UnParse(const aTemplate: RawUTF8): boolean;
  public
    /// initialize and parse a pre-rendered {{mustache}} template
    // - you should better use the Parse() class function instead, which
    // features an internal thread-safe cache
    constructor Create(const aTemplate: RawUTF8); overload;
    /// initialize and parse a pre-rendered {{mustache}} template
    // - you should better use the Parse() class function instead, which
    // features an internal thread-safe cache
    constructor Create(aTemplate: PUTF8Char; aTemplateLen: integer); overload; virtual;
    /// finalize internal memory
    destructor Destroy; override;

    /// renders the {{mustache}} template into a destination text buffer
    // - the context is given via our abstract TSynMustacheContext wrapper
    // - the rendering extended in fTags[] is supplied as parameters
    // - you can specify a list of partials via TSynMustachePartials.CreateOwned
    procedure Render(Context: TSynMustacheContext; TagStart,TagEnd: integer;
      Partials: TSynMustachePartials; NeverFreePartials: boolean); overload;
    /// renders the {{mustache}} template from a variant defined context
    // - the context is given via a custom variant type implementing
    // TSynInvokeableVariantType.Lookup, e.g. TDocVariant or TSMVariant
    // - you can specify a list of partials via TSynMustachePartials.CreateOwned
    // or a custom {{"English text}} callback
    // - can be used e.g. via a TDocVariant:
    // !var mustache := TSynMustache;
    // !    doc: variant;
    // !    html: RawUTF8;
    // !begin
    // !  mustache := TSynMustache.Parse(
    // !    'Hello {{name}}'#13#10'You have just won {{value}} dollars!');
    // !  TDocVariant.New(doc);
    // !  doc.name := 'Chris';
    // !  doc.value := 10000;
    // !  html := mustache.Render(doc);
    // !  // here html='Hello Chris'#13#10'You have just won 10000 dollars!'
    // - you can also retrieve the context from an ORM query of mORMot.pas:
    // ! dummy := TSynMustache.Parse(
    // !   '{{#items}}'#13#10'{{Int}}={{Test}}'#13#10'{{/items}}').Render(
    // !   aClient.RetrieveDocVariantArray(TSQLRecordTest,'items','Int,Test'));
    function Render(const Context: variant; Partials: TSynMustachePartials=nil;
      OnTranslate: TOnStringTranslate=nil): RawUTF8; overload;
    /// renders the {{mustache}} template from JSON defined context
    // - the context is given via a JSON object, defined from UTF-8 buffer
    // - you can specify a list of partials via TSynMustachePartials.CreateOwned
    // or a custom {{"English text}} callback
    // - is just a wrapper around Render(_JsonFast())
    // - you can write e.g. with the extended JSON syntax:
    // ! html := mustache.RenderJSON('{things:["one", "two", "three"]}');
    function RenderJSON(const JSON: RawUTF8; Partials: TSynMustachePartials=nil;
      OnTranslate: TOnStringTranslate=nil): RawUTF8; overload;
    /// renders the {{mustache}} template from JSON defined context
    // - the context is given via a JSON object, defined with parameters
    // - you can specify a list of partials via TSynMustachePartials.CreateOwned
    // or a custom {{"English text}} callback
    // - is just a wrapper around Render(_JsonFastFmt())
    // - you can write e.g. with the extended JSON syntax:
    // !   html := mustache.RenderJSON('{name:?,value:?}',[],['Chris',10000]);
    function RenderJSON(JSON: PUTF8Char; const Args,Params: array of const;
      Partials: TSynMustachePartials=nil; OnTranslate: TOnStringTranslate=nil): RawUTF8;
       overload;

    /// read-only access to the raw {{mustache}} template content
    property Template: RawUTF8 read fTemplate;
    /// the maximum possible number of nested contexts
    property SectionMaxCount: Integer read fSectionMaxCount;
  end;


implementation

function KindToText(Kind: TSynMustacheTagKind): PShortString;
begin
  result := GetEnumName(TypeInfo(TSynMustacheTagKind),ord(Kind));
end;

type
  TSynMustacheParser = class
  protected
    fTagStart, fTagStop: word;
    fPos, fPosMin, fPosMax, fPosTagStart: PUTF8Char;
    fTagCount: integer;
    fTemplate: TSynMustache;
    fScanStart, fScanEnd: PUTF8Char;
    function Scan(ExpectedTag: Word): boolean;
    procedure AddTag(aKind: TSynMustacheTagKind;
      aStart: PUTF8Char=nil; aEnd: PUTF8Char=nil);
  public
    constructor Create(Template: TSynMustache; const DelimiterStart, DelimiterStop: RawUTF8);
    procedure Parse(P,PEnd: PUTF8Char);
  end;

  TSynMustacheCache = class(TRawUTF8ListHashedLocked)
  public
    function Parse(const aTemplate: RawUTF8): TSynMustache;
    function UnParse(const aTemplate: RawUTF8): boolean;
  end;

var
  SynMustacheCache: TSynMustacheCache = nil;



{ TSynMustacheParser }

procedure TSynMustacheParser.AddTag(aKind: TSynMustacheTagKind;
  aStart, aEnd: PUTF8Char);
begin
  if (aStart=nil) or (aEnd=nil) then begin
    aStart := fScanStart;
    aEnd := fScanEnd;
    case aKind of
    mtComment, mtSection, mtSectionEnd, mtInvertedSection, mtSetDelimiter, mtPartial: begin
      // (indented) standalone lines should be removed from the template
      if aKind<>mtPartial then
        while (fPosTagStart>fPosMin) and (fPosTagStart[-1] in [' ',#9]) do
          dec(fPosTagStart); // ignore any indentation chars
      if (fPosTagStart=fPosMin) or (fPosTagStart[-1]=#$0A) then
        // tag starts on a new line -> check if ends on the same line
        if (fPos>fPosMax) or (fPos^=#$0A) or (PWord(fPos)^=$0A0D) then begin
          if fPos<=fPosMax then
            if fPos^=#$0A then
              inc(fPos) else
            if PWord(fPos)^=$0A0D then
              inc(fPos,2);
          if fTagCount>0 then // remove any indentation chars from previous text
            with fTemplate.fTags[fTagCount-1] do
              if Kind=mtText then
                while (TextLen>0) and (TextStart[TextLen-1] in [' ',#9]) do
                  dec(TextLen);
        end;
    end;
    end;
  end;
  if aEnd<=aStart then
    exit;
  if fTagCount>=length(fTemplate.fTags) then
    SetLength(fTemplate.fTags,fTagCount+fTagCount shr 3+32);
  with fTemplate.fTags[fTagCount] do begin
    Kind := aKind;
    SectionOppositeIndex := -1;
    case aKind of
    mtText, mtComment, mtTranslate: begin
      TextStart := aStart;
      TextLen := aEnd-aStart;
    end;
    else begin
      TextStart := fPosTagStart;
      TextLen := aEnd-fPosTagStart;
      // superfluous in-tag whitespace should be ignored
      while (aStart<aEnd) and (aStart^<=' ') do inc(aStart);
      while (aEnd>aStart) and (aEnd[-1]<=' ') do dec(aEnd);
      if aEnd=aStart then
        raise ESynMustache.CreateFmt('Void %s identifier',[KindToText(aKind)^]);
      SetString(Value,PAnsiChar(aStart),aEnd-aStart);
    end;
    end;
  end;
  inc(fTagCount);
end;

constructor TSynMustacheParser.Create(Template: TSynMustache;
  const DelimiterStart, DelimiterStop: RawUTF8);
begin
  fTemplate := Template;
  if length(DelimiterStart)<>2 then
    raise ESynMustache.CreateFmt('DelimiterStart="%s"',[DelimiterStart]);
  if length(DelimiterStop)<>2 then
    raise ESynMustache.CreateFmt('DelimiterStop="%s"',[DelimiterStop]);
  fTagStart := PWord(DelimiterStart)^;
  fTagStop := PWord(DelimiterStop)^;
end;

function GotoNextTag(P,PMax: PUTF8Char; ExpectedTag: Word): PUTF8Char;
begin
  if P<PMax then
    repeat
      if PWord(P)^<>ExpectedTag then begin
        inc(P);
        if P<PMax then continue;
        break;
      end;
      result := P;
      exit;
    until false;
  result := nil;
end;

function TSynMustacheParser.Scan(ExpectedTag: Word): boolean;
var P: PUTF8Char;
begin
  P := GotoNextTag(fPos,fPosMax,ExpectedTag);
  if P=nil then
    result := false else begin
    fScanEnd := P;
    fScanStart := fPos;
    fPos := P+2;
    result := true;
  end;
end;

procedure TSynMustacheParser.Parse(P, PEnd: PUTF8Char);
var Kind: TSynMustacheTagKind;
    Symbol: AnsiChar;
    i,j,secCount,secLevel: integer;
begin
  secCount := 0;
  if P=nil then
    exit;
  fPos := P;
  fPosMin := P;
  fPosMax := PEnd-1;
  repeat
    if not Scan(fTagStart) then
      break;
    fPosTagStart := fScanEnd;
    AddTag(mtText);
    if fPos>=fPosMax then
      break;
    Symbol := fPos^;
    case Symbol of
    '=': Kind := mtSetDelimiter;
    '{',
    '&': Kind := mtVariableUnescape;
    '#': Kind := mtSection;
    '^': Kind := mtInvertedSection;
    '/': Kind := mtSectionEnd;
    '!': Kind := mtComment;
    '>': Kind := mtPartial;
    '<': Kind := mtSetPartial;
    '"': Kind := mtTranslate; 
    else Kind := mtVariable;
    end;
    if Kind<>mtVariable then
      inc(fPos);
    if not Scan(fTagStop) then
      raise ESynMustache.CreateFmt('Unfinished {{tag "%s"',[fPos]);
    case Kind of
    mtSetDelimiter: begin
      if (fScanEnd-fScanStart<>6) or (fScanEnd[-1]<>'=') then
        raise ESynMustache.Create('mtSetDelimiter syntax is e.g. {{=<% %>=}}');
      fTagStart := PWord(fScanStart)^;
      fTagStop := PWord(fScanStart+3)^;
      continue; // do not call AddTag(mtSetDelimiter)
    end;
    mtVariableUnescape:
      if (Symbol='{') and (fTagStop=32125) and (PWord(fPos-1)^=32125) then
        inc(fPos); // {{{name}}} -> point after }}}
    end;
    AddTag(Kind);
  until false;
  AddTag(mtText,fPos,fPosMax+1);
  for i := 0 to fTagCount-1 do
  with fTemplate.fTags[i] do
  case Kind of
    mtSection, mtInvertedSection, mtSetPartial: begin
      inc(secCount);
      if secCount>fTemplate.fSectionMaxCount then
        fTemplate.fSectionMaxCount := secCount;
      secLevel := 1;
      for j := i+1 to fTagCount-1 do
        case fTemplate.fTags[j].Kind of
        mtSection, mtInvertedSection, mtSetPartial:
          inc(secLevel);
        mtSectionEnd: begin
          dec(secLevel);
          if secLevel=0 then
            if fTemplate.fTags[j].Value=Value then begin
              fTemplate.fTags[j].SectionOppositeIndex := i;
              SectionOppositeIndex := j;
              if Kind=mtSetPartial then begin
                if fTemplate.fInternalPartials=nil then
                  fTemplate.fInternalPartials := TSynMustachePartials.Create;
                fTemplate.fInternalPartials.Add(Value,
                  TextStart+TextLen+2,fTemplate.fTags[j].TextStart);
              end;               
              break;
            end else
              raise ESynMustache.CreateFmt('Got {{/%s}}, expected {{/%s}}',
                [Value,fTemplate.fTags[j].Value]);
        end;
        end;
      if SectionOppositeIndex<0 then
        raise ESynMustache.CreateFmt('Missing section end {{/%s}}',[Value]);
    end;
    mtSectionEnd: begin
      dec(secCount);
      if SectionOppositeIndex<0 then
        raise ESynMustache.CreateFmt('Unexpected section end {{/%s}}',[Value]);
    end;
  end;
  SetLength(fTemplate.fTags,fTagCount);
end;


{ TSynMustacheCache }

function TSynMustacheCache.Parse(const aTemplate: RawUTF8): TSynMustache;
var i: integer;
begin
  Lock;
  try
    i := IndexOf(aTemplate); // fast instance retrieval from shared cache
    if i>=0 then begin
      result := TSynMustache(Objects[i]);
      exit;
    end;
    result := TSynMustache.Create(aTemplate);
    AddObject(aTemplate,result);
  finally
    UnLock;
  end;
end;

function TSynMustacheCache.UnParse(const aTemplate: RawUTF8): boolean;
var i: integer;
begin
  result := false;
  if self=nil then
    exit;
  Lock;
  try
    i := IndexOf(aTemplate);
    if i>=0 then begin
      Delete(i);
      result := true;
    end;
  finally
    UnLock;
  end;
end;


{ TSynMustache }

class function TSynMustache.Parse(const aTemplate: RawUTF8): TSynMustache;
begin
  if SynMustacheCache=nil then
    GarbageCollectorFreeAndNil(SynMustacheCache,TSynMustacheCache.Create(true));
  result := SynMustacheCache.Parse(aTemplate);
end;

class function TSynMustache.UnParse(const aTemplate: RawUTF8): boolean;
begin
  result := SynMustacheCache.UnParse(aTemplate);
end;

constructor TSynMustache.Create(const aTemplate: RawUTF8);
begin
  Create(pointer(aTemplate),length(aTemplate));
end;

constructor TSynMustache.Create(aTemplate: PUTF8Char; aTemplateLen: integer);
begin
  inherited Create;
  fTemplate := aTemplate;
  with TSynMustacheParser.Create(self,'{{','}}') do
  try
    Parse(aTemplate,aTemplate+aTemplateLen);
  finally
    Free;
  end;
end;

type
  TSynMustacheProcessSection = procedure of object;

procedure TSynMustache.Render(Context: TSynMustacheContext; TagStart,TagEnd: integer;
  Partials: TSynMustachePartials; NeverFreePartials: boolean);
var partial: TSynMustache;
begin
  try
    while TagStart<=TagEnd do begin
      with fTags[TagStart] do
      case Kind of
      mtText:
        if TextLen<>0 then // may be 0 e.g. for standalone without previous Line
         Context.fWriter.AddNoJSONEscape(TextStart,TextLen);
      mtVariable:
        Context.AppendValue(Value,false);
      mtVariableUnescape:
        Context.AppendValue(Value,true);
      mtSection:
        case Context.AppendSection(Value) of
        msNothing: begin // e.g. for no key, false value, or empty list
          TagStart := SectionOppositeIndex;
          continue; // ignore whole section
        end;
        msList: begin
          while Context.GotoNextListItem do
            Render(Context,TagStart+1,SectionOppositeIndex-1,Partials,true);
          TagStart := SectionOppositeIndex;
          continue; // ignore whole section
        end;
        // msSingle,msSinglePseudo: process the section once with current context
        end;
      mtInvertedSection: // display section for no key, false value, or empty list
        if Context.AppendSection(Value)<>msNothing then begin
          TagStart := SectionOppositeIndex;
          continue; // ignore whole section
        end;
      mtSectionEnd:
        if (fTags[SectionOppositeIndex].Kind in [mtSection,mtInvertedSection]) and
           (Value[1]<>'-') then
          Context.PopContext;
      mtComment:
        ; // just ignored
      mtPartial: begin
        partial := fInternalPartials.GetPartial(Value);
        if (partial=nil) and (Context.fOwner<>self) then // recursive call
          partial := Context.fOwner.fInternalPartials.GetPartial(Value);
        if (partial=nil) and (Partials<>nil) then
          partial := Partials.GetPartial(Value);
        if partial<>nil then
          partial.Render(Context,0,high(partial.fTags),Partials,true);
      end;
      mtSetPartial:
        TagStart := SectionOppositeIndex; // ignore whole internal {{<partial}}
      mtTranslate:
        if TextLen<>0 then
          Context.TranslateBlock(TextStart,TextLen);
      else
        raise ESynMustache.CreateFmt('Kind=%s not implemented yet',
          [KindToText(fTags[TagStart].Kind)^]);
      end;
      inc(TagStart);
    end;
  finally
    if (Partials<>nil) and (Partials.fOwned) and not NeverFreePartials then
      Partials.Free;
  end;
end;

function TSynMustache.Render(const Context: variant;
  Partials: TSynMustachePartials; OnTranslate: TOnStringTranslate): RawUTF8;
var W: TTextWriter;
    Ctxt: TSynMustacheContext;
begin
  W := TTextWriter.CreateOwnedStream(4096);
  try
    Ctxt := TSynMustacheContextVariant.Create(self,W,SectionMaxCount,Context);
    try
      Ctxt.OnStringTranslate := OnTranslate;
      Render(Ctxt,0,high(fTags),Partials,false);
      W.SetText(result);
    finally
      Ctxt.Free;
    end;
  finally
    W.Free;
  end;
end;

function TSynMustache.RenderJSON(const JSON: RawUTF8;
  Partials: TSynMustachePartials; OnTranslate: TOnStringTranslate): RawUTF8;
var context: variant;
begin
  _Json(JSON,context,JSON_OPTIONS[true]);
  result := Render(context,Partials,OnTranslate);
end;

function TSynMustache.RenderJSON(JSON: PUTF8Char; const Args,
  Params: array of const; Partials: TSynMustachePartials;
  OnTranslate: TOnStringTranslate): RawUTF8;
var context: variant;
begin
  _Json(FormatUTF8(JSON,Args,Params,true),context,JSON_OPTIONS[true]);
  result := Render(context,Partials,OnTranslate);
end;

destructor TSynMustache.Destroy;
begin
  FreeAndNil(fInternalPartials);
  inherited;
end;


{ TSynMustacheContext }

constructor TSynMustacheContext.Create(Owner: TSynMustache; WR: TTextWriter);
begin
  fOwner := Owner;
  fWriter := WR;
end;

procedure TSynMustacheContext.TranslateBlock(Text: PUTF8Char; TextLen: Integer);
var s: string;
begin
  if Assigned(OnStringTranslate) then begin
    UTF8DecodeToString(Text,TextLen,s);
    OnStringTranslate(s);
    fWriter.AddNoJSONEscapeString(s);
  end else
    fWriter.AddNoJSONEscape(Text,TextLen);
end;


{ TSynMustacheContextVariant }

constructor TSynMustacheContextVariant.Create(Owner: TSynMustache; WR: TTextWriter;
  SectionMaxCount: integer; const aDocument: variant);
begin
  inherited Create(Owner,WR);
  SetLength(fContext,SectionMaxCount+1);
  PushContext(TVarData(aDocument)); // weak copy
end;

function TSynMustacheContextVariant.GetDocumentType(
  const aDoc: TVarData): TSynInvokeableVariantType;
begin
  result := nil;
  if aDoc.VType<=varAny then
    exit;
  if (fContextCount>0) and (fContext[0].DocumentType<>nil) and
     (aDoc.VType=fContext[0].DocumentType.VarType) then
    result := fContext[0].DocumentType else
    if not (FindCustomVariantType(aDoc.VType,TCustomVariantType(result)) and
       result.InheritsFrom(TSynInvokeableVariantType)) then
      result := nil;
end;

procedure TSynMustacheContextVariant.PushContext(aDoc: TVarData);
begin
  if fContextCount>=length(fContext) then
    SetLength(fContext,fContextCount+32); // roughtly set by SectionMaxCount
  with fContext[fContextCount] do begin
    Document := aDoc;
    DocumentType := GetDocumentType(aDoc);
    ListCurrent := -1;
    if DocumentType=nil then
      ListCount := -1 else
      ListCount := DocumentType.IterateCount(aDoc);
  end;
  inc(fContextCount);
end;

procedure TSynMustacheContextVariant.PopContext;
begin
  if fContextCount>1 then
    dec(fContextCount);
end;

procedure TSynMustacheContextVariant.GetValueFromContext(
  const ValueName: RawUTF8; var Value: TVarData);
var i: Integer;
begin
  if ValueName='.' then
    with fContext[fContextCount-1] do begin
      if ListCount>0 then
        Value := ListCurrentDocument else
        Value := Document;
      exit;
    end;
  for i := fContextCount-1 downto 0 do 
    with fContext[i] do
      if DocumentType<>nil then
        if ListCount<0 then begin // single item context
          DocumentType.Lookup(Value,Document,pointer(ValueName));
          if Value.VType>varNull then
            exit;
         end else
        if ValueName='-index' then begin
          Value.VType := varInteger;
          Value.VInteger := ListCurrent+1;
          exit;
        end else
        if (ListCurrent<ListCount) and (ListCurrentDocumentType<>nil) then begin
          ListCurrentDocumentType.Lookup(Value,ListCurrentDocument,pointer(ValueName));
          if Value.VType>varNull then
            exit;
        end;
end;

procedure TSynMustacheContextVariant.AppendValue(const ValueName: RawUTF8;
  UnEscape: boolean);
var Value: TVarData;
begin
  GetValueFromContext(ValueName,Value);
  AppendVariant(variant(Value),UnEscape);
end;

procedure TSynMustacheContextVariant.AppendVariant(const Value: variant;
  UnEscape: boolean);
var ValueText: RawUTF8;
    wasString: boolean;
begin
  if TVarData(Value).VType>varNull then
    if VarIsNumeric(Value) then // avoid RawUTF8 conversion for plain numbers
      fWriter.AddVariantJSON(Value,twNone) else begin
      VariantToUTF8(Value,ValueText,wasString);
      if UnEscape then
        fWriter.AddNoJSONEscape(pointer(ValueText)) else
        fWriter.AddHtmlEscape(pointer(ValueText),length(ValueText));
    end;
end;

function TSynMustacheContextVariant.AppendSection(
  const ValueName: RawUTF8): TSynMustacheSectionType;
var Value: TVarData;
begin
  result := msNothing;
  if (fContextCount>0) and (ValueName[1]='-') then
    with fContext[fContextCount-1] do
      if ListCount>=0 then begin
        if ((ValueName='-first') and (ListCurrent=0)) or
           ((ValueName='-last') and (ListCurrent=ListCount-1)) or
           ((ValueName='-odd') and (ListCurrent and 1=0)) then
          result := msSinglePseudo;
        exit;
      end;
  GetValueFromContext(ValueName,Value);
  PushContext(Value);
  if (Value.VType<=varNull) or
     ((Value.VType=varBoolean) and (not Value.VBoolean)) then
    exit; // null or false value will not display the section
  with fContext[fContextCount-1] do
      if ListCount<0 then
        result := msSingle else // single item
        if ListCount=0 then // empty list will not display the section
          exit else
          result := msList;       // non-empty list
end;

function TSynMustacheContextVariant.GotoNextListItem: boolean;
begin
  result := false;
  if fContextCount>0 then
    with fContext[fContextCount-1] do begin
      ListCurrentDocument.VType := varEmpty;
      ListCurrentDocumentType := nil;
      inc(ListCurrent);
      if ListCurrent>=ListCount then
        exit;
      DocumentType.Iterate(ListCurrentDocument,Document,ListCurrent);
      ListCurrentDocumentType := GetDocumentType(ListCurrentDocument);
      result := true;
    end;
end;


{ TSynMustachePartials }

procedure TSynMustachePartials.Add(const aName, aTemplate: RawUTF8);
begin
  fList.AddObject(aName,TSynMustache.Parse(aTemplate));
end;

procedure TSynMustachePartials.Add(const aName: RawUTF8;
  aTemplateStart, aTemplateEnd: PUTF8Char);
var aTemplate: RawUTF8;
begin
  SetString(aTemplate,PAnsiChar(aTemplateStart),aTemplateEnd-aTemplateStart);
  Add(aName,aTemplate);
end;

constructor TSynMustachePartials.Create;
begin
  fList := TRawUTF8ListHashed.Create(false);
end;

constructor TSynMustachePartials.CreateOwned(
  const NameTemplatePairs: array of RawUTF8);
var A: integer;
begin
  Create;
  fOwned := true;
  for A := 0 to high(NameTemplatePairs) div 2 do
    Add(NameTemplatePairs[A*2],NameTemplatePairs[A*2+1]);
end;

class function TSynMustachePartials.CreateOwned(
  const Partials: variant): TSynMustachePartials;
var p: integer;
begin
  result := nil;
  if DocVariantType.IsOfType(Partials) then
  with TDocVariantData(partials) do
    if (Kind=dvObject) and (Count>0) then begin
      result := TSynMustachePartials.Create;
      result.fOwned := true;
      for p := 0 to Count-1 do
        result.Add(Names[p],VariantToUTF8(Values[p]));
    end;
end;

destructor TSynMustachePartials.Destroy;
begin
  FreeAndNil(fList);
  inherited;
end;

function TSynMustachePartials.GetPartial(
  const PartialName: RawUTF8): TSynMustache;
var i: integer;
begin
  if self=nil then begin
    result := nil;
    exit;
  end;
  i := fList.IndexOf(PartialName);
  if i<0 then
    result := nil else
    result := TSynMustache(fList.Objects[i]);
end;

end.
