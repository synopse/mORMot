/// MongoDB document-oriented database direct access classes
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynMongoDB;

{
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2014 Arnaud Bouchez
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
  - first public release, corresponding to mORMot Framework 1.18


  TODO:
  - handle BULK commands support for MongoDB >=2.6 for faster writes
    see http://blog.mongodb.org/post/84922794768
  - GridFS support


}

interface

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 SQLITE3_FASTCALL

uses
  Windows,
  Classes,
  Variants, // this unit expects Variants to be available for storage
  SysUtils,
  SynCrtSock,
  SynCommons;


  
{ ************ BSON (Binary JSON) process }

type
  /// exception type used for BSON process
  EBSONException = class(ESynException);

  /// storage of a BSON binary document
  // - a specific type is defined for consistency with this unit classes
  // - binary content should follow the "int32 e_list #0" standard layout
  TBSONDocument = RawByteString;

  /// element types for BSON internal representation
  TBSONElementType = (
    betEOF, betFloat, betString, betDoc, betArray, betBinary,
    betDeprecatedUndefined, betObjectID, betBoolean, betDateTime,
    betNull, betRegEx, betDeprecatedDbptr, betJS, betDeprecatedSymbol,
    betJSScope, betInt32, betTimeStamp, betInt64);

  /// sub-types for betBinary element BSON internal representation
  TBSONElementBinaryType = (
    bbtGeneric, bbtFunction, bbtOldBinary, bbtOldUUID, bbtUUID, bbtMD5,
    bbtUser = $80);

  /// BSON ObjectID internal binary representation
  // - in MongoDB, documents stored in a collection require a unique _id field
  // that acts as a primary key: by default, it uses such a 12-byte ObjectID
  // - by design, sorting by _id: ObjectID is roughly equivalent to sorting by
  // creation time
  {$A-}
  {$ifndef UNICODE}
  TBSONObjectID = object
  {$else}
  TBSONObjectID = record
  {$endif}
    /// big-endian 4-byte value representing the seconds since the Unix epoch
    // - time is expressed in Coordinated Universal Time (UTC), not local time
    UnixCreateTime: cardinal;
    /// 3-byte machine identifier
    MachineID: array[0..2] of byte;
    /// 2-byte process id
    ProcessID: word;
    /// 3-byte counter, starting with a random value
    Counter: array[0..2] of byte;
    /// ObjectID content be filled with some unique values
    // - this implementation is thread-safe, since we use ProcessID=ThreadID
    procedure ComputeNew;
    /// convert an hexadecimal string value into one ObjectID
    // - returns TRUE if conversion was made, FALSE on any error
    function FromText(const Text: RawUTF8): boolean; overload;
    /// convert an hexadecimal string value into one ObjectID
    // - returns TRUE if conversion was made, FALSE on any error
    function FromText(Text: PUTF8Char): boolean; overload;
    /// convert a varian t into one ObjectID
    // - will first check for a TBSONVariant containing a betObjectID
    // - then will try to convert the variant from its string value, expecting
    // an hexadecimal text content
    // - returns TRUE if conversion was made, FALSE on any error
    function FromVariant(const value: variant): boolean;
    /// convert this ObjectID to its hexadecimal string value
    function ToText: RawUTF8; overload;
    /// convert this ObjectID to its hexadecimal string value
    procedure ToText(var result: RawUTF8); overload;
    /// convert this ObjectID to its TBSONVariant custom variant value
    function ToVariant: variant;
    /// returns the timestamp portion of the ObjectId() object as a Delphi date
    // - time is expressed in Coordinated Universal Time (UTC), not local time
    // so you can compare it to NowUTC returned time 
    function CreateDateTime: TDateTime;
    /// compare two Object IDs
    function Equal(const Another: TBSONObjectID): boolean;
      {$ifdef HASINLINE}inline;{$endif}
  end;

  /// points to a BSON ObjectID internal binary representation
  PBSONObjectID = ^TBSONObjectID;

  /// memory structure used for some special BSON storage as variant
  // - betObjectID kind will store a TBSONObjectID
  // - betBinary kind will store a BLOB content as RawByteString
  // - betDoc and betArray kind will store a BSON document, in its original
  // binary format as RawByteString (TBSONDocument)
  // - betDeprecatedDbptr, betJSScope and betRegEx will store the raw original
  // BSON content as RawByteString
  // - betJS and betDeprecatedSymbol will store the UTF-8 encoded string
  // as a RawUTF8
  // - betDeprecatedUndefined or betMinKey/betMaxKey do not contain any data
  // - warning: VBlob/VText use should match BSON_ELEMENTVARIANTMANAGED constant
  TBSONVariantData = packed record
    /// the variant type
    VType: TVarType;
    /// the kind of element stored
    case VKind: TBSONElementType of
    betObjectID: (
      {$HINTS OFF} // does not complain if Filler is declared but never used
      VFiller: array[1..SizeOf(TVarData)-SizeOf(TVarType)-SizeOf(TBSONElementType)
        -SizeOf(TBSONObjectID)] of byte;
      {$HINTS ON}
      VObjectID: TBSONObjectID
    );
    betBinary, betDoc, betArray, betRegEx, betDeprecatedDbptr,
    betJSScope: (
      /// store the raw binary content as a RawByteString (or TBSONDocument for
      // betDoc/betArray, i.e. the "int32 e_list #0" standard layout)
      // - you have to use RawByteString(VBlob) when accessing this field
      // - e.g. for betRegEx, it will contain raw [cstring cstring] content
      VBlob: pointer;
    );
    betJS, betDeprecatedSymbol: (
      /// store here a RawUTF8 with the associated text
      // - you have to use RawUF8(VText) when accessing this field
      VText: pointer;
    )
  end;
  {$A+}

  /// custom variant type used to store some special BSON elements
  // - internal layout will follow TBSONVariantData
  // - handled kind of item are complex BSON types, like betObjectID, betBinary
  // or betDoc/betArray
  // - it will allow conversion to/from string (and to date for ObjectID)
  TBSONVariant = class(TSynInvokeableVariantType)
  protected
    function GetNewDoc(const BSONDoc: TBSONDocument): variant;
    /// customization of JSON conversion into TBSONVariant kind of variants
    function TryJSONToVariant(var JSON: PUTF8Char; var Value: variant;
      EndOfObject: PUTF8Char): boolean; override;
    /// variant serialization will use modMongoStrict JSON-compatible mode
    procedure ToJSON(W: TTextWriter; const Value: variant; Escape: TTextWriterKind); override;
  public
    /// handle type conversion
    // - only types processed by now are string/OleStr/UnicodeString/date
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    /// handle type conversion
    // - only types processed by now are string/OleStr/UnicodeString/date
    procedure CastTo(var Dest: TVarData; const Source: TVarData;
      const AVarType: TVarType); override;
    /// clear the instance
    procedure Clear(var V: TVarData); override;
    /// copy one instance
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
    /// compare two variant values
    // - handle comparison of any variant, including TBSONVariant, via a
    // temporary JSON conversion, and case-sensitive comparison
    // - it uses case-sensitive text (hexadecimal) comparison for betObjectID
    procedure Compare(const Left, Right: TVarData;
      var Relationship: TVarCompareResult); override;
    /// convert a TBSONDocument binary content into a TBSONVariant of kind
    // betDoc or betArray
    // - see also all BSONVariant() overloaded functions, which also create
    // a TBSONVariant betDoc instance
    procedure FromBSONDocument(const BSONDoc: TBSONDocument; var result: variant;
      Kind: TBSONElementType=betDoc);
    /// convert a BLOB binary content into a TBSONVariant of kind betBinary
    // - if Bin is '', will store a NULL variant
    procedure FromBinary(const Bin: RawByteString; BinType: TBSONElementBinaryType;
      var result: variant);
    /// convert a JSON content into a TBSONVariant of kind betDoc or betArray
    // - warning: the supplied JSON buffer will be modified in-place 
    procedure FromJSON(json: PUTF8Char; var result: variant);
    /// returns TRUE if the supplied variant stores the supplied BSON kind of value
    function IsOfKind(const V: variant; Kind: TBSONElementType): boolean;
    /// retrieve a betBinary content stored in a TBSONVariant instance
    // - returns TRUE if the supplied variant is a betBinary, and set the
    // binary value into the supplied Blob variable
    // - returns FALSE otherwise
    function ToBlob(const V: Variant; var Blob: RawByteString): boolean;
    /// convert a TBSONDocument binary content into a TBSONVariant of kind betDoc
    // - is the default property, so that you can write:
    // ! BSONVariantType[BSON(['BSON',_Arr(['awesome',5.05, 1986])])]
    // - see also all BSONVariant() overloaded functions, which also create
    // a TBSONVariant betDoc instance
    property NewDoc[const BSONDoc: TBSONDocument]: variant read GetNewDoc; default;
  end;

  /// define how betDoc/betArray BSON elements will be converted as variants
  // - by default a TBSONVariant custom type will be returned, containing the
  // raw BSON binary content of the embedded document or array
  // - asDocVariantPerValue or asDocVariantPerReference could be used to
  // create a tree of TDocVariant custom kind of variant, able to access
  // to its nested properties via late-binding (asDocVariantPerReference being
  // also much faster in some cases - but less safe - than asDocVariantPerValue)
  // - asDocVariantPerValue will set JSON_OPTIONS[false] settings:
  // ! [dvoReturnNullForUnknownProperty]
  // - asDocVariantPerReference will set JSON_OPTIONS[true] settings:
  // ! [dvoValueCopiedByReference,dvoReturnNullForUnknownProperty]
  TBSONDocArrayConversion = (
    asBSONVariant, asDocVariantPerValue, asDocVariantPerReference);

  /// how TBSONElement.AddMongoJSON() method and AddMongoJSON() and
  // VariantSaveMongoJSON() functions will render their JSON content
  // - modMongoStrict and modNoMongo will follow the JSON RFC specifications
  // - modMongoShell will use a syntax incompatible with JSON RFC, but more
  // common to MongoDB daily use - as 'ObjectId()' or '{ field: /acme.*corp/i }'
  // - modMongoStrict will use the MongoDB Extended JSON syntax
  // - modNoMongo will serialize dates as ISO-8601 strings, ObjectID as hexadecimal
  // string and other MongoDB special objects in WrBase64() format
  // - see http://docs.mongodb.org/manual/reference/mongodb-extended-json
  TMongoJSONMode = (modNoMongo, modMongoStrict, modMongoShell);

  /// data structure used during BSON binary decoding of one BSON element
  // - will be retrieved by FromVariant() or FromNext()
  // - see http://bsonspec.org/#/specification
  // - this structure has been optimized to map the BSON binary content,
  // without any temporary memory allocation (the SAX way)
  {$ifndef UNICODE}
  TBSONElement = object
  protected
  {$else}
  TBSONElement = record
  private
  {$endif}
    /// used internally to set the TBSONElement content, once Kind has been set
    procedure FromBSON(bson: PByte);
  public
    /// index of this element in the original sequence list
    // - is correct only when the element has been reset before the parsing
    // loop, e.g.:
    // ! item.Index := -1; // so item.Index will count starting at 0
    // ! while item.FromNext(elem.Document) do
    // !   writeln(item.Index,' ',Item.Name,' ',Item.ValueBytes);
    Index: integer;
    /// the UTF-8 encoded name of this element
    Name: PUTF8Char;
    /// the name length (in chars) of this element
    NameLen: integer;
    /// the element type
    Kind: TBSONElementType;
    /// number of bytes in the BSON element
    // - will include the trailing #0 for string element
    ElementBytes: integer;
    /// pointer to the BSON element value
    // - is the raw value, without any parsing, e.g. points to a double value or
    // a document: "int32 e_list #0" standard layout (same as TBSONDocument)
    // - you may cast it for simple types:
    // ! PDouble(Element)^   PBoolean(Element)^   PInteger(Element)^
    // ! PInt64(Element)^    PBSONObjectID(Element)^
    // - or use the nested Data variant record to access more complex content
    // - warning: equals nil for betString/betJS after FromVariant()
    Element: pointer;
    /// depending on the Kind, will point to the sub-data
    // - since variable records can't have properties, we nest this information
    // within this main Data variable record
    // - not all Kind are handled here, only the complex data
    Data: record
    case TBSONElementType of
    betFloat, betBoolean, betInt32, betDateTime, betTimeStamp, betInt64: (
      /// this variable is not to be used directly, but for some internal
      // temporary storage, e.g. with FromVariant()
      // - use P*(Element)^ typecast instead
      InternalStorage: Int64;
    );
    betString, betJS: (
      /// points to the #0 ending string
      Text: PUTF8Char;
      /// number of bytes in Text (excluding trailing #0)
      TextLen: integer;
    );
    betDoc, betArray: (
      /// points to a "e_list #0" standard layout
      DocList: PByte;
    );
    betBinary: (
      /// points to the binary content
      Blob: pointer;
      /// number of bytes in Blob
      BlobLen: integer;
      /// corresponding sub-type of this Blob
      BlobSubType: TBSONElementBinaryType;
    );
    betRegEx: (
      RegEx: PUTF8Char;
      RegExLen: integer;
      RegExOptions: PUTF8Char;
      RegExOptionsLen: integer;
    );
    betJSScope: (
      JavaScript: PUTF8Char;
      JavaScriptLen: integer;
      ScopeDocument: PByte;
    );
    end;
    /// fill a BSON Element structure from a variant content and associated name
    // - perform the reverse conversion as made with ToVariant()
    // - since the result won't store any data but points to the original binary
    // content, the supplied Name/Value instances should remain available as long as
    // you will access to the result content
    // - aName here is just for conveniency, and could be left void
    // - supplied aTemp variable will be used for temporary storage, private to
    // this initialized TBSONElement
    procedure FromVariant(const aName: RawUTF8; const aValue: Variant; var aTemp: RawByteString);
    /// fill a BSON Element structure from a BSON encoded binary buffer list
    // - parse the next BSON element: BSON parameter should point to the
    // "e_list" of the "int32 e_list #0" BSON document
    // - will decode the supplied binary buffer into the BSON element structure,
    // then it will let BSON point to the next element, and return TRUE
    // - returns FALSE when you reached betEOF, so that you can use it in a loop,
    // and retrieve all the content as consecutive events, without any memory
    // allocation (the SAX way):
    // ! var bson: PByte;
    // !     item: TBSONElement;
    // ! ...
    // ! BSONParseLength(bson);
    // ! while item.FromNext(bson) do
    // !   writeln(item.Name);
    // - will raise an EBSONException if BSON content is not correct
    function FromNext(var BSON: PByte): boolean;
    /// search for a given name in a BSON encoded binary buffer list
    // - BSON parameter should point to the first "e_list" item of the
    // "int32 e_list #0" BSON document
    // - returns nil if the item was not found (with case-insensitive search)
    // - otherwise, has decoded the matching element structure otherwise
    function FromSearch(BSON: PByte; const aName: RawUTF8): PByte;
    /// convert a BSON element, as retrieved by TBSONElement.FromNext(),
    // into a variant
    // - it will return either standard variant values, or TBSONVariant custom type
    // for most complex kind of elements (see TBSONVariantData type definition)
    // - note that betString types will be stored as RawUTF8 varString
    // - by default, it will return TBSONVariant custom variants for documents or
    // arrays - but if storeDocArrayAsDocVariant is set, it will return a
    // TDocVariant custom kind of variant, able to access to its nested
    // properties via late-binding
    function ToVariant(DocArrayConversion: TBSONDocArrayConversion=asBSONVariant): variant; overload;
    /// convert a BSON element, as retrieved by TBSONElement.FromNext(),
    // into a variant
    // - same as the other ToVariant() overloaded function, but avoiding a copy
    // of the resulting variant
    procedure ToVariant(var result: variant;
      DocArrayConversion: TBSONDocArrayConversion=asBSONVariant); overload;
    /// convert a BSON element into an UTF-8 string
    // - any complex types (e.g. nested documents) will be converted via a
    // variant
    function ToRawUTF8: RawUTF8;
    /// convert a BSON element into an integer value
    // - will work only for betBoolean/betInt32/betInt64 types
    // - any other kind of values will return the supplied default value
    function ToInteger(const default: Int64=0): Int64;
    /// convert a BSON element, as retrieved by TBSONElement.FromNext(), into
    // its JSON representation
    // - this method will use by default the MongoDB Extended JSON syntax for
    // specific MongoDB objects but you may use modMongoShell if needed
    // - will raise an EBSONException if element is not correct
    procedure AddMongoJSON(W: TTextWriter; Mode: TMongoJSONMode=modMongoStrict); overload;
  end;

  /// used to write the BSON context
  TBSONWriter = class(TFileBufferWriter)
  protected
    fDocumentCount: integer;
    fDocument: array of record
      Offset: cardinal;
      Length: cardinal;
    end;
    procedure WriteNonVoidCString(const text: RawUTF8; const ErrorMsg: string);
      {$ifdef HASINLINE}inline;{$endif}
    procedure WriteCollectionName(Flags: integer; const CollectionName: RawUTF8);
  public
    /// write a boolean value
    procedure BSONWrite(const name: RawUTF8; const value: boolean); overload;
    /// write a floating point value
    procedure BSONWrite(const name: RawUTF8; const value: Double); overload;
    /// write a 32 bit integer value
    procedure BSONWrite(const name: RawUTF8; const value: integer); overload;
    /// write a 64 bit integer value
    procedure BSONWrite(const name: RawUTF8; const value: Int64); overload;
    /// write a string (UTF-8) value
    procedure BSONWrite(const name: RawUTF8; const value: RawUTF8; isJavaScript: boolean=false); overload;
    /// write a string (UTF-8) value from a memory buffer
    procedure BSONWrite(const name: RawUTF8; value: PUTF8Char); overload;
    /// write a binary (BLOB) value
    procedure BSONWrite(const name: RawUTF8; Data: pointer; DataLen: integer); overload;
    /// write an ObjectID value
    procedure BSONWrite(const name: RawUTF8; const value: TBSONObjectID); overload;
    /// write a RegEx value
    procedure BSONWriteRegEx(const name: RawUTF8; const RegEx: RawByteString);
    /// write a data/time value
    procedure BSONWriteDateTime(const name: RawUTF8; const value: TDateTime);
    /// write an element with no value
    // - elemType can be either betNull, betMinKey or betMaxKey
    procedure BSONWrite(const name: RawUTF8; elemtype: TBSONElementType); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// write an element with no value
    procedure BSONWrite(const name: RawUTF8; const elem: TBSONElement); overload;
    /// write a BSONVariant instance value
    procedure BSONWrite(const name: RawUTF8; const bson: TBSONVariantData); overload;
    /// write a DocVariant instance value
    procedure BSONWrite(const name: RawUTF8; const doc: TDocVariantData); overload;
    /// write a variant value
    // - handle simple types (numbers, strings...) and custom types (TDocVariant
    // and TBSONVariant, trying a translation to JSON for other custom types)
    procedure BSONWriteVariant(const name: RawUTF8; const value: variant); overload;
    /// write an open array (const Args: array of const) argument
    // - handle simple types (numbers, strings...) and custom types (TDocVariant)
    procedure BSONWrite(const name: RawUTF8; const value: TVarRec); overload;
    /// write a value from the supplied JSON content
    // - is able to handle any kind of values, including nested documents or
    // BSON extended syntax
    // - this method is used recursively by BSONWriteDocFromJSON(), and should
    // not be called directly
    // - will return JSON=nil in case of unexpected error in the supplied JSON
    procedure BSONWriteFromJSON(const name: RawUTF8; var JSON: PUTF8Char; EndOfObject: PUTF8Char);

    /// recursive writing of a BSON document or value from a TDocVariant
    // object or array, used e.g. by BSON(const doc: TDocVariantData) function
    // - caller should execute BSONAdjustDocumentsSize() on the resulting buffer
    // - this method will call BSONDocumentBegin/BSONDocumentEnd internally
    // - will raise an EBSONException if doc is not a valid TDocVariant or null
    // or if the resulting binary content is bigger than BSON_MAXDOCUMENTSIZE
    procedure BSONWriteDoc(const doc: TDocVariantData);
    /// write an object specified as name/value pairs as a BSON document
    // - data must be supplied two by two, as Name,Value pairs, e.g.
    // ! aBSONWriter.BSONWriteObject(['name','John','year',1972]);
    // - this method wil be faster than using a BSONWriteDoc(_ObjFast(...))
    procedure BSONWriteObject(const NameValuePairs: array of const);
    /// write an array specified as a list of items as a BSON document
    // - data must be supplied as a list of values e.g.
    // ! aBSONWriter.BSONWriteArray(['John',1972]);
    // - this method wil be faster than using a BSONWriteDoc(_ArrFast(...))
    procedure BSONWriteArray(const Items: array of const); 
    /// write an array of integers as a BSON Document
    procedure BSONWriteArrayOfInteger(const Integers: array of integer);
    /// write some BSON document from a supplied (extended) JSON array or object
    // - warning: the incoming JSON buffer will be modified in-place: so you
    // should make a private copy before running this method
    // - will handle only '{ ... }', '[ ... ]' or 'null' input, with the standard
    // strict JSON format, or BSON-like extensions, e.g. unquoted field names:
    // $ {id:10,doc:{name:"John",birthyear:1972}}
    // - MongoDB Shell syntax will also be recognized to create TBSONVariant, like
    // $ new Date()   ObjectId()   MinKey   MaxKey  /<jRegex>/<jOptions>
    // see @http://docs.mongodb.org/manual/reference/mongodb-extended-json
    // $ {id:new ObjectId(),doc:{name:"John",date:ISODate()}}
    // $ {name:"John",field:/acme.*corp/i}
    // - will create the BSON binary without any temporary TDocVariant storage
    function BSONWriteDocFromJSON(JSON: PUTF8Char; aEndOfObject: PUTF8Char;
      out Kind: TBSONElementType): PUTF8Char;

    /// to be called before a BSON document will be written
    // - returns the start offset of this document, which is to be specified
    // to NotifyDocumentEnd() when all properties have been called
    function BSONDocumentBegin: cardinal;
    /// to be called when a BSON document has been written
    // - it will store the current stream position into an internal array,
    // which will be written when you call AdjustDocumentsSize()
    procedure BSONDocumentEnd(Start: Cardinal; WriteEndingZero: boolean=true);
    /// after all content has been written, call this method on the resulting
    // memory buffer to store all document size as expected by the standard
    procedure BSONAdjustDocumentsSize(BSON: PByteArray); virtual;
    /// flush the content and return the whole binary encoded stream
    // - call BSONAdjustDocumentsSize() to adjust all internal document sizes
    // - expect the TBSONWriter instance to have been created as such:
    // ! TBSONWriter.Create(TRawByteStringStream);
    procedure ToBSONDocument(var result: TBSONDocument); virtual;
    /// flush the content and return the whole document as a TBSONVariant
    // - call ToBSONDocument() to adjust all internal document sizes
    // - expect the TBSONWriter instance to have been created as such:
    // ! TBSONWriter.Create(TRawByteStringStream);
    procedure ToBSONVariant(var result: variant);
  end;


const
  /// fake BSON element type which compares lower than all other possible values
  // - element type sounds to be stored as shortint, so here $ff=-1<0=betEOF
  betMinKey = TBSONElementType($ff);
  /// fake BSON element type which compares higher than all other possible values
  // - element type sounds to be stored as shortint, so here betInt64=$12<$7f
  betMaxKey = TBSONElementType($7f);

  /// kind of elements which will store a RawByteString/RawUTF8 content
  // within its TBSONVariant kind
  // - i.e. TBSONVariantData.VBlob/VText field is to be managed
  BSON_ELEMENTVARIANTMANAGED =
   [betBinary, betDoc, betArray, betRegEx, betDeprecatedDbptr,
    betJSScope, betJS, betDeprecatedSymbol];

  /// by definition, maximum MongoDB document size is 16 MB
  BSON_MAXDOCUMENTSIZE = 16*1024*1024;

  /// special JSON string content which will be used to store a betDeprecatedUndefined item
  // - *[false] is for strict JSON, *[true] for MongoDB Extended JSON
  BSON_JSON_UNDEFINED: array[boolean] of string[19] =
    ('{"$undefined":true}','undefined');
  /// special JSON string content which will be used to store a betMinKey item
  // - *[false] is for strict JSON, *[true] for MongoDB Extended JSON
  BSON_JSON_MINKEY: array[boolean] of string[15] = ('{"$minKey":1}','MinKey');
  /// special JSON string content which will be used to store a betMaxKey item
  // - *[false] is for strict JSON, *[true] for MongoDB Extended JSON
  BSON_JSON_MAXKEY: array[boolean] of string[15] = ('{"$maxKey":1}','MaxKey');
  /// special JSON patterns which will be used to format a betObjectID item
  // - *[false,*] is to be written before the hexadecimal ID, *[true,*] after
  BSON_JSON_OBJECTID: array[boolean,TMongoJSONMode] of string[15] = (
    ('"','{"$oid":"','ObjectId("'),('"','"}','")'));
  /// special JSON patterns which will be used to format a betBinary item
  // - *[false,*] is for strict JSON, *[true,*] for MongoDB Extended JSON
  BSON_JSON_BINARY: array[boolean,boolean] of string[15] = (
    ('{"$binary":"','","$type":"'),('BinData(',',"'));
  /// special JSON string content which will be used to store a betDeprecatedDbptr
  // - *[false,*] is for strict JSON, *[true,*] for MongoDB Extended JSON
  // - (not used by now for this deprecated content)
  BSON_JSON_DBREF: array[boolean,0..2] of string[11] = (
    ('{"$ref":"','","$id":"','"}'),('DBRef("','","','")'));
  /// special JSON string content which will be used to store a betRegEx
  BSON_JSON_REGEX: array[0..2] of string[15] =
    ('{"$regex":"','","$options":"','"}');
  /// special JSON patterns which will be used to format a betDateTime item
  // - *[*,false] is to be written before the date value, *[*,true] after
  BSON_JSON_DATE: array[TMongoJSONMode,boolean] of string[11] = (
    ('"','"'),('{"$date":"','"}'),('ISODate("','")'));

var
  /// global TCustomVariantType used to register BSON variant types
  // - if you use this unit, both TDocVariant and TBSONVariant custom types
  // will be registered, since they are needed for any MongoDB / BSON process
  BSONVariantType: TBSONVariant;

/// create a TBSONVariant custom variant type containing a BSON Object ID
// - will be filled with some unique values, ready to create a new document key
// - will store a BSON element of betObjectID kind
function ObjectID: variant; overload;

/// create a TBSONVariant Object ID custom variant type from a supplied text
// - will raise an EBSONException if the supplied text is not valid hexadecimal
// - will set a BSON element of betObjectID kind
function ObjectID(const Hexa: RaWUTF8): variant; overload;

/// convert a TBSONVariant Object ID custom variant into a TBSONObjectID
// - raise an exception if the supplied variant is not a TBSONVariant Object ID 
function BSONObjectID(const aObjectID: variant): TBSONObjectID; 

/// create a TBSONVariant JavaScript custom variant type from a supplied code
// - will set a BSON element of betJS kind
function JavaScript(const JS: RawUTF8): variant; overload;

/// create a TBSONVariant JavaScript and associated scope custom variant type
// from a supplied code and document
// - will set a BSON element of betJSScope kind
function JavaScript(const JS: RawUTF8; const Scope: TBSONDocument): variant; overload;

/// store some object content into BSON encoded binary
// - object will be initialized with data supplied two by two, as Name,Value
// pairs, e.g.:
// ! aBson := BSON(['name','John','year',1972]);
// - you can define nested arrays or objects as TDocVariant, e.g:
// ! aBSON := BSON(['bsonDat',_Arr(['awesome',5.05, 1986])]);
// - or you can specify nested arrays or objects with '['..']' or '{'..'}':
// ! aBSON := BSON(['BSON','[','awesome',5.05,1986,']'])
// ! u := BSONToJSON(BSON(['doc','{','name','John','year',1982,'}','id',123]));
// ! assert(u='{"doc":{"name":"John","year":1982},"id":123}');
// ! u := BSONToJSON(BSON(['doc','{','name','John','abc','[','a','b','c',']','}','id',123]));
// ! assert(u='{"doc":{"name":"John","abc":["a","b","c"]},"id":123}');
// - will create the BSON binary without any temporary TDocVariant storage
function BSON(const NameValuePairs: array of const): TBSONDocument; overload;

/// create a fields selector BSON document from a field names list
// - can be used via a TBSONVariant instance for the projection parameter of
// a TMongoRequestQuery, e.g.:
// ! BSONToJSON(BSONFieldSelector(['a','b','c']))='{"a":1,"b":1,"c":1}'
function BSONFieldSelector(const FieldNames: array of RawUTF8): TBSONDocument; overload;

/// create a fields selector BSON document from a CSV field names list
// - can be used via a TBSONVariant instance for the projection parameter of
// a TMongoRequestQuery, e.g.:
// ! BSONToJSON(BSONFieldSelector('a,b,c'))='{"a":1,"b":1,"c":1}'
function BSONFieldSelector(const FieldNamesCSV: RawUTF8): TBSONDocument; overload;

/// store some object content, supplied as (extended) JSON, into BSON encoded binary
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, e.g. unquoted field names:
// ! BSON('{id:10,doc:{name:"John",birthyear:1972}}');
// - MongoDB Shell syntax will also be recognized to create TBSONVariant, like
// ! new Date()   ObjectId()   MinKey   MaxKey  /<jRegex>/<jOptions>
// see @http://docs.mongodb.org/manual/reference/mongodb-extended-json
// ! BSON('{id:new ObjectId(),doc:{name:"John",date:ISODate()}}');
// ! BSON('{name:"John",field:/acme.*corp/i}');
// - will create the BSON binary without any temporary TDocVariant storage, by
// calling JSONBufferToBSONDocument() on a temporary copy of the supplied JSON
function BSON(const JSON: RawUTF8): TBSONDocument; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// store some object content, supplied as (extended) JSON and parameters,
// into BSON encoded binary
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, e.g. unquoted field names
// - MongoDB Shell syntax will also be recognized to create TBSONVariant, like
// ! new Date()   ObjectId()   MinKey   MaxKey  /<jRegex>/<jOptions>
// see @http://docs.mongodb.org/manual/reference/mongodb-extended-json
// - typical use could be:
// ! BSON('{%:{$in:[?,?]}}',['type'],['food','snack']);
// ! BSON('{type:{$in:?}}',[],[_Arr(['food','snack'])]);
// ! BSON('{%:[?,?,?]}',['BSON'],['awesome',5.05,1986])
// ! BSON('{%:?}',['BSON'],[_Arr(['awesome',5.05,1986])])
// ! BSON('{name:?,field:/%/i}',['acme.*corp'],['John']);
// ! BSON('{id:new ObjectId(),doc:{name:?,date:ISODate(?)}}',[],['John',NowUTC]);
// - will create the BSON binary without any temporary TDocVariant storage,
// by calling JSONBufferToBSONDocument() on the generated JSON content
// - since all content will be transformed into JSON internally, use this
// method only if the supplied parameters are simple types, and identified
// explicitely via BSON-like extensions: any complex value (e.g. a TDateTime
// or a BSONVariant binary) won't be handled as expected - use the overloaded
// BSON() with explicit BSONVariant() name/value pairs instead
function BSON(Format: PUTF8Char; const Args,Params: array of const): TBSONDocument; overload;

/// store some TDocVariant custom variant content into BSON encoded binary
// - will write either a BSON object or array, depending of the internal
// layout of this TDocVariantData instance (i.e. Kind property value)
// - if supplied variant is not a TDocVariant, raise an EBSONException
function BSON(const doc: TDocVariantData): TBSONDocument; overload;

/// store an array of integer into BSON encoded binary
// - object will be initialized with data supplied e.g. as a TIntegerDynArray
function BSONFromIntegers(const Integers: array of integer): TBSONDocument;

/// store some object content, supplied as (extended) JSON into BSON binary
// - warning: the supplied JSON buffer will be modified in-place, if necessary:
// so you should create a temporary copy before calling this function, or call
// BSON(const JSON: RawUTF8) function instead
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, e.g. unquoted field names
// - MongoDB Shell syntax will also be recognized to create TBSONVariant, like
// ! new Date()   ObjectId()   MinKey   MaxKey  /<jRegex>/<jOptions>
// see @http://docs.mongodb.org/manual/reference/mongodb-extended-json
// ! BSON('{id:new ObjectId(),doc:{name:"John",date:ISODate()}}');
// ! BSON('{name:"John",field:/acme.*corp/i}');
// - will create the BSON binary without any temporary TDocVariant storage
// - will return the kind of BSON document created, i.e. either betDoc or betArray
function JSONBufferToBSONDocument(JSON: PUTF8Char; var doc: TBSONDocument): TBSONElementType;


/// store some object content into a TBSONVariant betDoc type instance
// - object will be initialized with data supplied two by two, as Name,Value
// pairs, as expected by the corresponding overloaded BSON() function
function BSONVariant(const NameValuePairs: array of const): variant; overload;

/// create a fields selector BSON document from a field names list
// - can be used for the projection parameter of a TMongoRequestQuery, e.g.:
// ! VariantToJSON(BSONVariantFieldSelector(['a','b','c']))='{"a":1,"b":1,"c":1}'
function BSONVariantFieldSelector(const FieldNames: array of RawUTF8): variant; overload;

/// create a fields selector BSON document from a CSV field names list
// - can be used for the projection parameter of a TMongoRequestQuery, e.g.:
// ! VariantToJSON(BSONVariantFieldSelector('a,b,c'))='{"a":1,"b":1,"c":1}'
function BSONVariantFieldSelector(const FieldNamesCSV: RawUTF8): variant; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// store some object content, supplied as (extended) JSON, into a TBSONVariant
// betDoc type instance
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, as with the overloaded BSON() function
function BSONVariant(const JSON: RawUTF8): variant; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// store some object content, supplied as (extended) JSON, into a TBSONVariant
// betDoc type instance
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, as with the overloaded BSON() function
// - warning: this overloaded method will mofify the supplied JSON buffer
// in-place: you can use the overloaded BSONVariant(const JSON: RawUTF8) function
// instead if you do not want to modify the input buffer content
procedure BSONVariant(JSON: PUTF8Char; var result: variant); overload;

/// store some object content, supplied as (extended) JSON and parameters,
// into a TBSONVariant betDoc type instance
// - in addition to the JSON RFC specification strict mode, this method will
// handle some BSON-like extensions, as with the overloaded BSON() function
function BSONVariant(Format: PUTF8Char; const Args,Params: array of const): variant; overload;

/// convert a TDocVariant variant into a TBSONVariant betDoc type instance
function BSONVariant(doc: TDocVariantData): variant; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// store an array of integer into a TBSONVariant betArray type instance
// - object will be initialized with data supplied e.g. as a TIntegerDynArray
function BSONVariantFromIntegers(const Integers: array of integer): variant;

/// parse the header of a BSON encoded binary buffer, and return its length
// - BSON should point to a "int32 e_list #0" BSON document (like TBSONDocument)
// - if ExpectedBSONLen is set, this function will check that the supplied
// BSON content "int32" length matches the supplied value, and raise an
// EBSONException if this comparison fails
function BSONParseLength(var BSON: PByte; ExpectedBSONLen: integer=0): integer;

/// parse the next element in supplied BSON encoded binary buffer list
// - BSON should point to the "e_list" of the "int32 e_list #0" BSON document
// - will decode the supplied binary buffer as a variant, then it will let BSON
// point to the next element, and return TRUE
// - returns FALSE when you reached betEOF, so that you can use it in a loop:
// ! var bson: PByte;
// !     name: RaWUTF8;
// !     value: variant;
// ! ...
// ! BSONParseLength(bson);
// ! while BSONParseNextElement(bson,name,value) do
// !   writeln(name,':',value);
// - by default, it will return TBSONVariant custom variants for documents or
// arrays - but if storeDocArrayAsDocVariant is set, it will return a
// TDocVariant custom kind of variant, able to access to its nested
// properties via late-binding
// - if you want to parse a BSON list as fast as possible, you should better use
// TBSONElement.FromNext() which avoid any memory allocation (the SAX way) - in
// fact, this function is just a wrapper around TBSONElement.FromNext + ToVariant
function BSONParseNextElement(var BSON: PByte; var name: RawUTF8; var element: variant;
  DocArrayConversion: TBSONDocArrayConversion=asBSONVariant): boolean;

/// search for a given property in a a supplied BSON encoded binary buffer
// - BSON should point to a "int32 e_list #0" BSON document (like TBSONDocument)
// - returns FALSE if not item in the list matches the expected name
// - returns TRUE if the name is found in the list, then item points to the data
function BSONSearchElement(BSON: PByte; const name: RawUTF8; var item: TBSONElement): PByte;

/// search for a property by number in a a supplied BSON encoded binary buffer
// - BSON should point to a "int32 e_list #0" BSON document (like TBSONDocument)
// - returns FALSE if the list has too few elements (starting at index 0)
// - otherwise, returns TRUE then let item point to the corresponding element
function BSONPerIndexElement(BSON: PByte; index: integer; var item: TBSONElement): PByte;

/// convert a BSON document into a TDocVariant variant instance
// - BSON should point to a "int32 e_list #0" BSON document
// - if ExpectedBSONLen is set, this function will check that the supplied
// BSON content "int32" length matches the supplied value
// - by definition, asBSONVariant is not allowed as Option value
procedure BSONToDoc(BSON: PByte; var Result: Variant; ExpectedBSONLen: integer=0;
  Option: TBSONDocArrayConversion=asDocVariantPerReference); overload;

/// convert a BSON document into a TDocVariant variant instance
// - BSON should point to a "int32 e_list #0" BSON document
// - if ExpectedBSONLen is set, this function will check that the supplied
// BSON content "int32" length matches the supplied value
// - by definition, asBSONVariant is not allowed as Option value
function BSONToDoc(const BSON: TBSONDocument; ExpectedBSONLen: integer=0;
  Option: TBSONDocArrayConversion=asDocVariantPerReference): variant; overload;

/// convert a BSON document into its JSON representation
// - BSON should point to a "int32 e_list #0" BSON document
// - Kind should be either betDoc or betArray
// - if ExpectedBSONLen is set, this function will check that the supplied
// BSON content "int32" length matches the supplied value
// - this function will use by default the MongoDB Extended JSON syntax for
// specific MongoDB objects but you may use modMongoShell if needed
function BSONToJSON(BSON: PByte; Kind: TBSONElementType;
   ExpectedBSONLen: integer=0; Mode: TMongoJSONMode=modMongoStrict): RawUTF8; overload;

/// convert a BSON document into its JSON representation
// - BSON should point to a "int32 e_list #0" BSON document
// - if ExpectedBSONLen is set, this function will check that the supplied
// BSON content "int32" length matches the supplied value
// - this function will use by default the MongoDB Extended JSON syntax for
// specific MongoDB objects but you may use modMongoShell if needed
function BSONToJSON(const BSON: TBSONDocument; ExpectedBSONLen: integer=0;
  Mode: TMongoJSONMode=modMongoStrict): RawUTF8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a BSON list of elements into its JSON representation
// - BSON should point to the "e_list" of the "int32 e_list #0" BSON document,
// i.e. the item data as expected by TBSONElement.FromNext()
// - this function will use by default the MongoDB Extended JSON syntax for
// specific MongoDB objects but you may use modMongoShell if needed
procedure BSONToJSON(BSONList: PByte; Kind: TBSONElementType; W: TTextWriter;
  Mode: TMongoJSONMode=modMongoStrict); overload;

/// convert any kind of BSON/JSON element, encoded as variant, into JSON
// - this function will use by default the MongoDB Extended JSON syntax for
// specific MongoDB objects but you may use modMongoShell if needed
procedure AddMongoJSON(const Value: variant; W: TTextWriter;
  Mode: TMongoJSONMode=modMongoStrict); overload;

/// convert any kind of BSON/JSON element, encoded as variant, into JSON
// - in addition to default modMongoStrict as rendered by VariantSaveJSON(),
// this function can render the supplied variant with the Mongo Shell syntax
// or even raw JSON content
function VariantSaveMongoJSON(const Value: variant; Mode: TMongoJSONMode): RawUTF8;




{ ************ MongoDB Client }

const
  /// MongoDB server default IP port
  MONGODB_DEFAULTPORT = 27017;

type
  /// exception type used for MongoDB process
  EMongoException = class(ESynException);

  /// the available MongoDB driver Request Opcodes
  // - opReply: database reply to a client request - ResponseTo shall be set
  // - opMsg: generic msg command followed by a string (deprecated)
  // - opUpdate: update document
  // - opInsert: insert new document
  // - opQuery: query a collection
  // - opGetMore: get more data from a previous query
  // - opDelete: delete documents
  // - opKillCursors: notify database client is done with a cursor
  TMongoOperation = (
    opReply, opMsg, opUpdate, opInsert, opQuery, opGetMore, opDelete, opKillCursors);

  /// define how an opUpdate operation will behave
  // - if mufUpsert is set, the database will insert the supplied object into
  // the collection if no matching document is found
  // - if mufMultiUpdate is set, the database will update all matching objects
  // in the collection; otherwise (by default) only updates first matching doc
  TMongoUpdateFlag =
    (mufUpsert, mufMultiUpdate);

  /// define how a TMongoRequestUpdate message will behave
  TMongoUpdateFlags = set of TMongoUpdateFlag;

  /// define how an opInsert operation will behave
  // - if mifContinueOnError is set, the database will not stop processing a
  // bulk insert if one fails (e.g. due to duplicate IDs); this makes bulk
  // insert behave similarly to a series of single inserts, except lastError
  // will be set if any insert fails, not just the last one - if multiple
  // errors occur, only the most recent will be reported by getLastError
  TMongoInsertFlag =
    (mifContinueOnError);

  /// define how a TMongoRequestInsert message will behave
  TMongoInsertFlags = set of TMongoInsertFlag;

  /// define how an opDelete operation will behave
  // - if mdfSingleRemove is set, the database will remove only the first
  // matching document in the collection. Otherwise (by default) all matching
  // documents will be removed
  TMongoDeleteFlag =
    (mdfSingleRemove);

  /// define how a TMongoRequestDelete message will behave
  TMongoDeleteFlags = set of TMongoDeleteFlag;

  /// define how an opQuery operation will behave
  // - if mqfTailableCursor is set, cursor is not closed when the last data
  // is retrieved
  // - if mqfSlaveOk is set, it will allow query of replica slave; normally
  // this returns an error except for namespace "local"
  // - mqfOplogReplay is internal replication use only - driver should not set
  // - if mqfNoCursorTimeout is set, the server normally does not times out
  // idle cursors after an inactivity period (10 minutes) to prevent
  // excess memory use
  // - if mqfAwaitData is to use with TailableCursor. If we are at the end
  // of the data, block for a while rather than returning no data. After a
  // timeout period, we do return as normal
  // - if mqfExhaust is set, stream the data down full blast in multiple "more"
  // packages, on the assumption that the client will fully read all data queried
  // - if mqfPartial is set, it will get partial results from a mongos if
  // some shards are down (instead of throwing an error)
  TMongoQueryFlag =
    (mqfTailableCursor=1, mqfSlaveOk, mqfOplogReplay, mqfNoCursorTimeout,
     mqfAwaitData, mqfExhaust, mqfPartial);

  /// define how a TMongoRequestQuery message will behave
  TMongoQueryFlags = set of TMongoQueryFlag;

  /// abstract class used to create MongoDB Wire Protocol client messages
  // - see http://docs.mongodb.org/meta-driver/latest/legacy/mongodb-wire-protocol
  // - this class is not tight to the connection class itself (which is one
  // known limitation of TMongoWire for instance)
  TMongoRequest = class(TBSONWriter)
  protected
    fRequestID: integer;
    fResponseTo: integer;
    fRequestHeaderStart: cardinal;
    fRequestOpCode: TMongoOperation;
    fDatabaseName,
    fCollectionName,
    fFullCollectionName: RawUTF8;
    fBSONDocument: TBSONDocument;
  public
    /// write a standard Message Header for MongoDB client
    // - opCode is the type of the message
    // - requestID  is a client or database-generated identifier that uniquely
    // identifies this message: in case of opQuery or opGetMore messages, it will
    // be sent in the responseTo field from the database
    // - responseTo is the requestID taken from previous opQuery or opGetMore
    constructor Create(const FullCollectionName: RawUTF8;
      opCode: TMongoOperation; requestID, responseTo: Integer); reintroduce;
    /// append a query parameter as a BSON document
    // - param can be a TDocVariant, e.g. created with:
    // ! _JsonFast('{name:"John",age:{$gt,21}}');
    // ! _JsonFastFmt('{name:?,age:{$gt,?}}',[],['John',21]);
    // ! _JsonFastFmt('{name:?,field:/%/i}',['acme.*corp'],['John']);
    // - param can be a TBSONVariant containing a TBSONDocument raw binary block
    // created e.g. from:
    // ! BSONVariant(['BSON',_Arr(['awesome',5.05, 1986])])
    // ! BSONVariantType[BSON(['BSON',_Arr(['awesome',5.05, 1986])])]
    // - if param is null, it will append a void document
    // - if param is a string, it will be converted as expected by most
    // database commands, e.g.
    // ! TMongoRequestQuery.Create('admin.$cmd','buildinfo',[],1)
    // will query   { buildinfo: 1 }  to the  admin.$cmd  collection, i.e.
    // $ admin.$cmd.findOne( { buildinfo: 1 } )
    procedure BSONWriteParam(const paramDoc: variant);
    /// flush the content and return the whole binary encoded stream
    // - expect the TBSONWriter instance to have been created with reintroduced
    // Create() specific constructors inheriting from this TMongoRequest class
    // - this overridden version will adjust the size in the message header
    procedure ToBSONDocument(var result: TBSONDocument); override;
    /// write the main parameters of the request as JSON
    procedure ToJSON(W: TTextWriter; Mode: TMongoJSONMode); overload; virtual;
    /// write the main parameters of the request as JSON
    function ToJSON(Mode: TMongoJSONMode): RawUTF8; overload;
    /// identify the message, after call to any reintroduced Create() constructor
    property MongoRequestID: integer read fRequestID;
    /// the associated full collection name, e.g. 'db.test'
    property FullCollectionName: RawUTF8 read fFullCollectionName;
    /// the associated full collection name, e.g. 'db'
    property DatabaseName: RawUTF8 read fDatabaseName;
    /// the associated full collection name, e.g. 'test'
    property CollectionName: RawUTF8 read fCollectionName;
    /// the message operation code
    // - should be either opUpdate, opInsert, opQuery, opGetMore, opDelete
    // or opKillCursors, depending on the TMongoRequest* class instantiated
    property MongoRequestOpCode: TMongoOperation read fRequestOpCode;
  end;

  /// a MongoDB client message to update a document in a collection
  TMongoRequestUpdate = class(TMongoRequest)
  protected
    fSelector, fUpdate: TVarData;
  public
    /// initialize a MongoDB client message to update a document in a collection
    // - FullCollectionName is e.g. 'dbname.collectionname'
    // - how the update will be processed can be customized via Flags
    // - Selector is the BSON document query to select the document, supplied as
    // TDocVariant - i.e. created via _JsonFast() or _JsonFastFmt() - or as
    // TBSONVariant - i.e. created via BSONVariant() - or null if all documents
    // are to be updated
    // - Update is the BSON document specification of the update to perform,
    // supplied as TDocVariant or TBSONVariant
    // - there is no response to an opUpdate message
    constructor Create(const FullCollectionName: RawUTF8;
      const Selector, Update: variant; Flags: TMongoUpdateFlags=[]); reintroduce;
    /// write the main parameters of the request as JSON
    procedure ToJSON(W: TTextWriter; Mode: TMongoJSONMode); override;
  end;

  /// a MongoDB client message to insert one or more documents in a collection
  TMongoRequestInsert = class(TMongoRequest)
  public
    /// initialize a MongoDB client message to insert one or more documents in
    // a collection, supplied as variants
    // - FullCollectionName is e.g. 'dbname.collectionname'
    // - Documents is an array of TDocVariant or TBSONVariant - i.e. created via
    // _JsonFast() _JsonFastFmt() or BSONVariant()
    // - there is no response to an opInsert message
    constructor Create(const FullCollectionName: RawUTF8;
      const Documents: array of variant; Flags: TMongoInsertFlags=[]); reintroduce; overload;
    /// initialize a MongoDB client message to insert one or more documents in
    // a collection, supplied as raw BSON binary
    // - FullCollectionName is e.g. 'dbname.collectionname'
    // - Documents is the low-level concatenation of BSON documents, created
    // e.g. with a TBSONWriter stream
    // - there is no response to an opInsert message
    constructor Create(const FullCollectionName: RawUTF8;
      const Documents: TBSONDocument; Flags: TMongoInsertFlags=[]); reintroduce; overload;
    /// initialize a MongoDB client message to insert one or more documents in
    // a collection, supplied as JSON objects
    // - FullCollectionName is e.g. 'dbname.collectionname'
    // - JSONDocuments is an array of JSON objects
    // - there is no response to an opInsert message
    constructor Create(const FullCollectionName: RawUTF8;
      const JSONDocuments: array of PUTF8Char; Flags: TMongoInsertFlags=[]); reintroduce; overload;
  end;

  /// a MongoDB client message to delete one or more documents in a collection
  TMongoRequestDelete = class(TMongoRequest)
  protected
    fQuery: TVarData;
  public
    /// initialize a MongoDB client message to delete one or more documents in
    // a collection
    // - FullCollectionName is e.g. 'dbname.collectionname'
    // - Selector is the BSON document query to select the document, supplied as
    // TDocVariant - i.e. created via _JsonFast() or _JsonFastFmt() - or as
    // TBSONVariant - i.e. created via BSONVariant() - or null if all documents
    // are to be deleted
    // - warning: CreateDelete('db.coll',null) can be expensive so you should
    // better drop the whole collection
    // - there is no response to an opDelete message
    constructor Create(const FullCollectionName: RawUTF8;
      const Selector: variant; Flags: TMongoDeleteFlags=[]); reintroduce;
    /// write the main parameters of the request as JSON
    procedure ToJSON(W: TTextWriter; Mode: TMongoJSONMode); override;
  end;

  /// a MongoDB client message to query one or more documents in a collection
  TMongoRequestQuery = class(TMongoRequest)
  protected
    fNumberToReturn: integer;
    fQuery, fReturnFieldsSelector: TVarData;
  public
    /// initialize a MongoDB client message to query one or more documents in
    // a collection from a specified Cursor identifier
    // - FullCollectionName is e.g. 'dbname.collectionname'
    // - Query is the BSON document query to select the document, supplied as
    // TDocVariant - i.e. created via _JsonFast() or _JsonFastFmt() - or null
    // if all documents are to be retrieved - for instance:
    // ! _JsonFast('{name:"John",age:{$gt,21}}');
    // ! _JsonFastFmt('{name:?,age:{$gt,?}}',[],['John',21]);
    // ! _JsonFastFmt('{name:?,field:/%/i}',['acme.*corp'],['John']);
    // - if Query is a string, it will be converted as expected by most
    // database commands, e.g.
    // $ TMongoRequestQuery.Create('admin.$cmd','buildinfo',[],1)
    // will query   { buildinfo: 1 }  to the  admin.$cmd  collection, i.e.
    // $ admin.$cmd.findOne( { buildinfo: 1 } )
    // - Query can also be a TBSONVariant, e.g. created with:
    // ! BSONVariant('{name:?,age:{$gt,?}}',[],['John',21])
    // - ReturnFieldsSelector is an optional selector (set to null if not
    // applicable) as a BSON document that limits the fields in the returned
    // documents, supplied as TDocVariant or TBSONVariant - e.g. created via:
    // ! BSONVariantFieldSelector('a,b,c');
    // ! BSONVariantFieldSelector(['a','b','c']);
    // ! BSONVariant('{a:1,b:1,c:1}');
    // ! _JsonFast('{a:1,b:1,c:1}');
    // - if ReturnFieldsSelector is a string, it will be converted into
    // $ { ReturnFieldsSelector: 1 }
    constructor Create(const FullCollectionName: RawUTF8;
      const Query, ReturnFieldsSelector: variant; NumberToReturn: integer;
      NumberToSkip: Integer=0; Flags: TMongoQueryFlags=[]); reintroduce;
    /// write the main parameters of the request as JSON
    procedure ToJSON(W: TTextWriter; Mode: TMongoJSONMode); override;
    /// retrieve the NumberToReturn parameter as set to the constructor
    property NumberToReturn: integer read fNumberToReturn;
  end;

  /// a MongoDB client message to continue the query of one or more documents
  // in a collection, after a TMongoRequestQuery message
  TMongoRequestGetMore = class(TMongoRequest)
  public
    /// initialize a MongoDB client message to continue the query of one or more
    // documents in a collection, after a opQuery / TMongoRequestQuery message
    // - FullCollectionName is e.g. 'dbname.collectionname'
    // - you can specify the number of documents to return (e.g. from previous
    // opQuery response)
    // - CursorID should have been retrieved within an opReply message from the
    // database
    constructor Create(const FullCollectionName: RawUTF8;
      NumberToReturn: integer; CursorID: Int64); reintroduce;
  end;

  /// a MongoDB client message to close one or more active cursors
  TMongoRequestKillCursor = class(TMongoRequest)
  public
    /// initialize a MongoDB client message to close one or more active cursors
    // in the database
    // - it is mandatory to ensure that database resources are reclaimed by
    // the client at the end of the query
    // - if a cursor is read until exhausted (read until opQuery or opGetMore
    // returns zero for the CursorId), there is no need to kill the cursor
    // - there is no response to an opKillCursor message
    constructor Create(const CursorIDs: array of Int64); reintroduce;
  end;


  /// used to store the binary raw data a database response to a
  // TMongoRequestQuery / TMongoRequestGetMore client message
  TMongoReply = RawByteString;

  /// define an opReply message execution content 
  // - mrfCursorNotFound will be set when getMore is called but the cursor id
  // is not valid at the server; returned with zero results
  // - mrfQueryFailure is set when the query failed - results consist of one
  // document containing an "$err" field describing the failure 
  // - mrfShardConfigStale should not be used by client, just by Mongos
  // - mrfAwaitCapable is set when the server supports the AwaitData Query
  // option (always set since Mongod version 1.6)
  TMongoReplyCursorFlag = (
    mrfCursorNotFound, mrfQueryFailure, mrfShardConfigStale,
    mrfAwaitCapable);
    
  /// define a TMongoReplyCursor message execution content
  TMongoReplyCursorFlags = set of TMongoReplyCursorFlag;

  /// internal low-level binary structure mapping the TMongoReply header
  // - used e.g. by TMongoReplyCursor and TMongoConnection.GetReply()
  TMongoReplyHeader = packed record
    /// total message length, including the header
    MessageLength: integer;
    /// identifier of this message
    RequestID: integer;
    /// retrieve the RequestID from the original request
    ResponseTo: integer;
    /// low-level code of the message
    OpCode: integer;
    /// response flags
    ResponseFlags: integer;
    /// cursor identifier if the client may need to perform further opGetMore
    CursorID: Int64;
    /// where in the cursor this reply is starting
    StartingFrom: integer;
    /// number of documents in the reply
    NumberReturned: integer;
  end;

  /// points to an low-level binary structure mapping the TMongoReply header
  // - so that you can write e.g.
  // ! PMongoReplyHeader(aMongoReply)^.RequestID 
  PMongoReplyHeader = ^TMongoReplyHeader;


  /// map a MongoDB server reply message as sent by the database
  // - in response to TMongoRequestQuery / TMongoRequestGetMore messages
  // - you can use the record's methods to retrieve information about a given
  // response, and navigate within all nested documents
  // - several TMongoReplyCursor instances may map the same TMongoReply content
  // - you can safely copy one TMongoReplyCursor instance to another
  {$ifndef UNICODE}
  TMongoReplyCursor = object
  protected
  {$else}
  TMongoReplyCursor = record
  private
  {$endif}
    fReply: TMongoReply;
    fRequestID: integer;
    fResponseTo: integer;
    fResponseFlags: TMongoReplyCursorFlags;
    fCursorID: Int64;
    fStartingFrom: integer;
    fNumberReturned: integer;
    fDocuments: TPointerDynArray;
    fCurrentPosition: integer;
    fFirstDocument,
    fCurrentDocument: PAnsiChar;
    fLatestDocIndex: integer;
    fLatestDocValue: variant;
    procedure ComputeDocumentsList;
    function GetOneDocument(index: integer): variant;
  public
    /// initialize the cursor with a supplied binary reply from the server
    // - will raise an EMongoException if the content is not valid
    // - will populate all record fields with the supplied data
    procedure Init(const ReplyMessage: TMongoReply);

    /// retrieve the next document in the list, as a TDocVariant instance
    // - return TRUE if the supplied document has been retrieved
    // - return FALSE if there is no more document to get - you can use the
    // Rewind method to restart from the first document
    // - could be used e.g. as:
    // ! var Reply: TMongoReply;
    // !     doc: variant;
    // ! ...
    // !   Reply.Init(ResponseMessage);
    // !   while Reply.Next(doc) do
    // !      writeln('Name: ',doc.Name,' FirstName: ',doc.FirstName);
    function Next(out doc: variant; option: TBSONDocArrayConversion=asDocVariantPerReference): boolean; overload;
    /// retrieve the next document in the list, as BSON content
    // - return TRUE if the supplied document has been retrieved - then doc
    // points to a "int32 e_list #0" BSON document 
    // - return FALSE if there is no more document to get - you can use the
    // Rewind method to restart from the first document
    // - this method is almost immediate, since the BSON raw binary is returned
    // directly without any conversion  
    // - could be used e.g. as:
    // ! var Reply: TMongoReply;
    // !     doc: PByte;
    // ! ...
    // !   Reply.Init(ResponseMessage);
    // !   while Reply.Next(doc) do
    // !     writeln(BSONToJSON(doc,0,modMongoShell)); // fast display
    function Next(out doc: PByte): boolean; overload;
    /// retrieve the next document in the list, as a BSON binary document
    // - return TRUE if the supplied document has been retrieved - then doc
    // points to a "int32 e_list #0" BSON document
    // - return FALSE if there is no more document to get - you can use the
    // Rewind method to restart from the first document
    // - this method is slightly slower than the one returning a PByte, since
    // it will allocate a memory buffer to store the TBSONDocument binary
    // - could be used e.g. as:
    // ! var Reply: TMongoReply;
    // !     doc: TBSONDocument;
    // ! ...
    // !   Reply.Init(ResponseMessage);
    // !   while Reply.Next(doc) do
    // !     writeln(BSONToJSON(doc,0,modMongoShell)); // fast display
    function Next(out BSON: TBSONDocument): boolean; overload;
    /// retrieve the next document in the list, as JSON content
    // - return TRUE if the supplied document has been retrieved
    // - return FALSE if there is no more document to get - you can use the
    // Rewind method to restart from the first document
    // - could be used e.g. as:
    // ! var Reply: TMongoReply;
    // !     json: RawUTF8;
    // ! ...
    // !   Reply.Init(ResponseMessage);
    // !   while Reply.Next(json,modMongoShell) do
    // !     writeln(json); // fast display
    function Next(out JSON: RawUTF8; Mode: TMongoJSONMode=modMongoStrict): boolean; overload;
    /// let Next() overloaded methods point to the first document of this message
    procedure Rewind;
    
    /// retrieve a given document as a TDocVariant instance
    // - this method won't use any cache (like Document[..] property), since
    // it should be used with a local variant on stack as cache:  
    // ! var Reply: TMongoReply;
    // !     doc: variant;
    // !     i: integer;
    // ! ...
    // !   Reply.Init(ResponseMessage);
    // !   for i := 0 to Reply.DocumentCount-1 do begin
    // !      GmrfQueryFailureetDocument(i,doc);
    // !      writeln('Name: ',doc.Name,' FirstName: ',doc.FirstName);
    // !   end;
    procedure GetDocument(index: integer; var result: variant);
    /// return all documents content as a JSON array, or one JSON object
    // if there is only one document in this reply
    // - this method is very optimized and will convert the BSON binary content
    // directly into JSON
    procedure FetchAllToJSON(W: TTextWriter; Mode: TMongoJSONMode=modMongoStrict;
      WithHeader: boolean=false);
    /// return all documents content as a JSON array, or one JSON object
    // if there is only one document in this reply
    // - this method is very optimized and will convert the BSON binary content
    // directly into JSON
    function ToJSON(Mode: TMongoJSONMode=modMongoStrict; WithHeader: boolean=false): RawUTF8;
    /// append all documents content to a dynamic array of TDocVariant
    // - return the new size of the Dest[] array
    function AppendAllToDocVariantDynArray(var Dest: TVariantDynArray): integer;
    /// append all documents content to a TDocVariant array instance
    // - if the supplied instance if not already a TDocVariant of kind dvArray,
    // a new void instance will be created
    // - return the new size of the Dest array
    function AppendAllToDocVariant(var Dest: TDocVariantData): integer;
    /// append all documents content to a BSON binary stream
    // - Dest.Tag will be used to count the current item number in the resulting
    // BSON array 
    procedure AppendAllToBSON(Dest: TBSONWriter);

    /// retrieve the context execution of this message
    property ResponseFlags: TMongoReplyCursorFlags read fResponseFlags;
    /// identifier of this message
    property RequestID: integer read fRequestID;
    /// retrieve the RequestID from the original request
    property ResponseTo: integer read fResponseTo;
    /// access to the low-level binary reply message
    property Reply: TMongoReply read fReply;
    /// cursor identifier if the client may need to perform further
    // TMongoRequestGetMore messages
    // - in the event that the result set of the query fits into one OP_REPLY
    // message, CursorID will be 0
    property CursorID: Int64 read fCursorID;
    /// where in the cursor this reply is starting
    property StartingFrom: integer read fStartingFrom;
    /// number of documents in the reply
    property DocumentCount: Integer read fNumberReturned;
    /// points to the first document binary
    // - i.e. just after the Reply header
    property FirstDocument: PAnsiChar read fFirstDocument;
    /// direct access to the low-level BSON binary content of each document
    property DocumentBSON: TPointerDynArray read fDocuments;
    /// retrieve a given document as a TDocVariant instance
    // - could be used e.g. as:
    // ! var Reply: TMongoReply;
    // !     i: integer;
    // ! ...
    // !   Reply.Init(ResponseMessage);
    // !   for i := 0 to Reply.DocumentCount-1 do
    // !      writeln('Name: ',Reply.Document[i].Name,' FirstName: ',Reply.Document[i].FirstName);
    // - note that there is an internal cache for the latest retrieved document
    // by this property, so that you can call Reply.Document[i] several times
    // without any noticeable speed penalty
    property Document[index: integer]: variant read GetOneDocument;
    /// the current position of the Next() call, starting at 0
    property Position: integer read fCurrentPosition;
  end;

  /// event callback signature for iterative process of TMongoConnection
  TOnMongoConnectionReply = procedure(Request: TMongoRequest;
    const Reply: TMongoReplyCursor; var Opaque) of object;

{$M+}

  TMongoClient = class;
  TMongoDatabase = class;
  TMongoCollection = class;

  /// one TCP/IP connection to a MongoDB server
  // - all access will be protected by a mutex (critical section): it is thread
  // safe but you may use one TMongoClient per thread or a connection pool, for
  // better performance
  TMongoConnection = class
  protected
    fLock: TRTLCriticalSection;
    fLocked: cardinal;
    fClient: TMongoClient;
    fSocket: TCrtSocket;
    fServerAddress: RawUTF8;
    fServerPort: integer;
    procedure Lock;
    procedure UnLock;
    function Send(Request: TMongoRequest): boolean;
    function GetOpened: boolean;
    function GetLocked: boolean;
    // will call TMongoReplyCursor.FetchAllToJSON(TTextWriter(Opaque))
    procedure ReplyJSONStrict(Request: TMongoRequest; const Reply: TMongoReplyCursor; var Opaque);
    procedure ReplyJSONExtended(Request: TMongoRequest; const Reply: TMongoReplyCursor; var Opaque);
    // will call TMongoReplyCursor.AppendAllToDocVariantDynArray(TVariantDynArray(Opaque))
    procedure ReplyDocVariant(Request: TMongoRequest; const Reply: TMongoReplyCursor; var Opaque);
    // will call TMongoReplyCursor.AppendAllToBSON(TBSONWrite(Opaque))
    procedure ReplyBSON(Request: TMongoRequest; const Reply: TMongoReplyCursor; var Opaque);
  public
    /// initialize the connection to the corresponding MongoDB server
    // - the server address is either a host name, or an IP address
    // - if no server address is specified, will try to connect to localhost
    // - this won't create the connection, until Open method is executed
    constructor Create(const aClient: TMongoClient; const aServerAddress: RawByteString;
      aServerPort: integer=MONGODB_DEFAULTPORT); reintroduce;
    /// release the connection, including the socket
    destructor Destroy; override;
    /// connect to the MongoDB server
    // - will raise an EMongoException on error
    procedure Open;
    /// disconnect from MongoDB server
    // - will raise an EMongoException on error
    procedure Close;

    /// low-level method to send a request to the server
    // - if Request is not either TMongoRequestQuery or TMongoRequestGetMore,
    // will raise an EMongoException
    // - then will return the reply message as sent back by the database,
    // ready to be accessed using a TMongoReplyCursor wrapper
    procedure GetReply(Request: TMongoRequest; out result: TMongoReply);
    /// low-level method to send a request to the server, and return a cursor 
    // - if Request is not either TMongoRequestQuery or TMongoRequestGetMore,
    // will raise an EMongoException
    // - then will parse and return a cursor to the reply message as sent back
    // by the database, with logging if necessary
    // - raise an EMongoException if mrfQueryFailure flag is set in the reply
    procedure GetCursor(Request: TMongoRequest; var Result: TMongoReplyCursor);
    /// low-level method to send a query to the server, calling a callback event
    // on each reply
    // - is used by GetDocumentsAndFree, GetBSONAndFree and GetJSONAndFree
    // methods to receive the whole document (you should better call those)
    // - the supplied Query instance will be released when not needed any more
    procedure GetRepliesAndFree(Query: TMongoRequestQuery;
      OnEachReply: TOnMongoConnectionReply; var Opaque);

    /// send a query to the server, returning a TDocVariant instance containing
    // all the incoming data
    // - will send the Request message, and any needed TMongoRequestGetMore
    // messages to retrieve all the data from the server
    // - the supplied Query instance will be released when not needed any more
    // - if Query.NumberToReturn<>1, it will return either null or a dvArray
    // kind of TDocVariant containing all returned items
    // - if Query.NumberToReturn=1, then it will return either null or a
    // single TDocVariant instance
    function GetDocumentsAndFree(Query: TMongoRequestQuery): variant; overload;
    /// send a query to the server, returning a TDocVariant instance containing
    // all the incoming data
    // - will send the Request message, and any needed TMongoRequestGetMore
    // messages to retrieve all the data from the server
    // - the supplied Query instance will be released when not needed any more
    // - if Query.NumberToReturn<>1, it will return either null or a dvArray
    // kind of TDocVariant containing all returned items
    // - if Query.NumberToReturn=1, then it will return either null or a
    // single TDocVariant instance
    procedure GetDocumentsAndFree(Query: TMongoRequestQuery; var result: variant); overload;
    /// send a query to the server, returning a dynamic array of TDocVariant
    // instance containing all the incoming data
    // - will send the Request message, and any needed TMongoRequestGetMore
    // messages to retrieve all the data from the server
    // - the supplied Query instance will be released when not needed any more
    procedure GetDocumentsAndFree(Query: TMongoRequestQuery; var result: TVariantDynArray); overload;
    /// send a query to the server, returning a TBSONDocument instance containing
    // all the incoming data, as raw binary BSON document containing an array
    // of the returned items
    // - will send the Request message, and any needed TMongoRequestGetMore
    // messages to retrieve all the data from the server
    // - the supplied Query instance will be released when not needed any more
    function GetBSONAndFree(Query: TMongoRequestQuery): TBSONDocument;
    /// send a query to the server, returning all the incoming data as JSON
    // - will send the Request message, and any needed TMongoRequestGetMore
    // messages to retrieve all the data from the server
    // - this method is very optimized and will convert the BSON binary content
    // directly into JSON, in either modMongoStrict or modMongoShell layout
    // (modNoMongo will do the same as modMongoStrict)
    // - if Query.NumberToReturn<>1, it will return either 'null' or a '[..]'
    // JSON array with all the incoming documents retrieved from the server
    // - if Query.NumberToReturn=1, it will return either 'null' or a single
    // '{...}' JSON object
    // - the supplied Query instance will be released when not needed any more
    function GetJSONAndFree(Query: TMongoRequestQuery; Mode: TMongoJSONMode): RawUTF8;

    /// send a message to the MongoDB server
    // - will apply Client.WriteConcern policy, and run an EMongoException
    // in case of any error
    // - the supplied Request instance will be released when not needed any more
    // - by default, it will follow Client.WriteConcern pattern - but you can
    // set NoAcknowledge = TRUE to avoid calling the getLastError command
    // - will return the getLastError reply (if retrieved from server)
    function SendAndFree(Request: TMongoRequest; NoAcknowledge: boolean=false): variant;
    /// run a database command, supplied as a TDocVariant, TBSONVariant or a
    // string, and return the a TDocVariant instance
    // - see http://docs.mongodb.org/manual/reference/command for a list
    // of all available commands
    // - for instance:
    // ! RunCommand('test',_ObjFast(['dbStats',1,'scale',1024],stats);
    // ! RunCommand('test',BSONVariant(['dbStats',1,'scale',1024],stats);
    // ! RunCommand('admin','buildinfo',fServerBuildInfo);
    // - the message will be returned by the server as a single TDocVariant
    // instance (since the associated TMongoRequestQuery.NumberToSkip=1)
    // - in case of any error, the error message is returned as text
    // - in case of success, this method will return ''
    function RunCommand(const aDatabaseName: RawUTF8;
      const command: variant; var returnedValue: variant): RawUTF8; overload;
    /// run a database command, supplied as a TDocVariant, TBSONVariant or a
    // string, and return the raw BSON document array of received items
    // - this overloaded method can be used on huge content to avoid the slower
    // conversion to an array of TDocVariant instances
    // - in case of success, this method will return TRUE, or FALSE on error
    function RunCommand(const aDatabaseName: RawUTF8;
      const command: variant; var returnedValue: TBSONDocument): boolean; overload;

    /// return TRUE if the Open method has successfully been called
    property Opened: boolean read GetOpened;
    /// access to the corresponding MongoDB server
    property Client: TMongoClient read fClient;
    /// direct access to the low-level TCP/IP communication socket
    property Socket: TCrtSocket read fSocket;
    /// is TRUE when the connection is busy
    property Locked: boolean read GetLocked;
  published
    /// read-only access to the supplied server address
    // - the server address is either a host name, or an IP address
    property ServerAddress: RawUTF8 read fServerAddress;
    /// read-only access to the supplied server port
    // - the server Port is MONGODB_DEFAULTPORT (27017) by default
    property ServerPort: integer read fServerPort;
  end;

  /// array of TCP connection to a MongoDB Replica Set
  // - first item [0] is the Primary member
  // - other items [1..] are the Secondary members
  TMongoConnectionDynArray = array of TMongoConnection;

  /// define Read Preference Modes to a MongoDB replica set
  // - Important: All read preference modes except rpPrimary may return stale
  // data because secondaries replicate operations from the primary with some
  // delay - ensure that your application can tolerate stale data if you choose
  // to use a non-primary mode
  // - rpPrimary:	Default mode - all operations read from the current replica
  // set primary
  // - rpPrimaryPreferred: in most situations, operations read from the primary
  // but if it is unavailable, operations read from secondary members.
  // - rpPsecondary: all operations read from the secondary members
  // of the replica set
  // - rpPsecondaryPreferred:	in most situations, operations read from
  // secondary members but if no secondary members are available, operations
  // read from the primary
  TMongoClientReplicaSetReadPreference = (
    rpPrimary, rpPrimaryPreferred, rpSecondary, rpSecondaryPreferred);

  /// define Write Concern property of a MongoDB connection
  // - Write concern describes the guarantee that MongoDB provides when
  // reporting on the success of a write operation. The strength of the write
  // concerns determine the level of guarantee. When inserts, updates and
  // deletes have a weak write concern, write operations return quickly. In
  // some failure cases, write operations issued with weak write concerns may
  // not persist. With stronger write concerns, clients wait after sending a
  // write operation for MongoDB to confirm the write operations. MongoDB
  // provides different levels of write concern to better address the specific
  // needs of applications. Clients may adjust write concern to ensure that
  // the most important operations persist successfully to an entire
  // MongoDB deployment. For other less critical operations, clients can
  // adjust the write concern to ensure faster performance rather than
  // ensure persistence to the entire deployment.
  // - wcAcknowledged is the default safe mode: the mongod confirms the
  // receipt of the write operation. Acknowledged write concern allows clients
  // to catch network, duplicate key, and other errors.
  // - with wcJournaled, the mongod acknowledges the write operation only
  // after committing the data to the journal. This write concern ensures that
  // MongoDB can recover the data following a shutdown or power interruption.
  // - wcReplicaAcknowledged will guarantee that the write operation propagates
  // to at least one member of a replica set
  // - with wcUnacknowledged, MongoDB does not acknowledge the receipt of
  // write operation. Unacknowledged is similar to errors ignored; however,
  // drivers attempt to receive and handle network errors when possible. The
  // driver's ability to detect network errors depends on the system's
  // networking configuration.
  // - with wcErrorsIgnored, MongoDB does not acknowledge write operations.
  // With this level of write concern, the client cannot detect failed write
  // operations. These errors include connection errors and mongod exceptions
  // such as duplicate key exceptions for unique indexes. Although the errors
  // ignored write concern provides fast performance, this performance gain
  // comes at the cost of significant risks for data persistence and durability.
  // WARNING: Do not use wcErrorsIgnored write concern in normal operation.
  TMongoClientWriteConcern = (
    wcAcknowledged, wcJournaled, wcReplicaAcknowledged,
    wcUnacknowledged, wcErrorsIgnored);

  /// remote access to a MongoDB server
  // - a single server can have several active connections, if some secondary
  // hosts were defined
  TMongoClient = class
  protected
    fConnectionString: RawUTF8;
    fDatabases: TRawUTF8ListHashed;
    fConnections: TMongoConnectionDynArray;
    fReadPreference: TMongoClientReplicaSetReadPreference;
    fWriteConcern: TMongoClientWriteConcern;
    fConnectionTimeOut: Cardinal;
    fLog: TSynLog;
    fLogRequestEvent: TSynLogInfo;
    fLogReplyEvent: TSynLogInfo;
    fServerBuildInfo: variant;
    fServerBuildInfoNumber: cardinal;
    fLatestReadConnectionIndex: integer;
    function GetServerBuildInfo: variant;
    function GetServerBuildInfoNumber: cardinal;
    function GetOneReadConnection: TMongoConnection;
    function GetBytesReceived: Int64;
    function GetBytesSent: Int64;
    function GetBytesTransmitted: Int64;
  public
    /// prepare a connection to a MongoDB server or Replica Set
    // - this constructor won't create the connection until the Open method
    // is called
    // - you can specify multiple hosts, as CSV values, if necessary
    constructor Create(const Host: RawUTF8; Port: Integer=MONGODB_DEFAULTPORT;
      const SecondaryHostCSV: RawUTF8=''; const SecondaryPortCSV: RawUTF8=''); overload;
    /// connect to a database on a remote MongoDB primary server
    // - this method won't use authentication, and will return the corresponding
    // MongoDB database instance
    // - this method is an alias to the Database[] property
    function Open(const DatabaseName: RawUTF8): TMongoDatabase;
    /// secure connection to a database on a remote MongoDB server
    // - this method will use authentication and will return the corresponding
    // MongoDB database instance, with a dedicated secured connection
    // - see http://docs.mongodb.org/manual/administration/security-access-control
    function OpenAuth(const DatabaseName,UserName,PassWord: RawUTF8): TMongoDatabase; overload;
    /// close the connection and release all associated TMongoDatabase,
    // TMongoCollection and TMongoConnection instances
    destructor Destroy; override;
    /// retrieve the server version and build information
    // - return the content as a TDocVariant document, e.g.
    // ! ServerBuildInfo.version = '2.4.9'
    // ! ServerBuildInfo.versionArray = [2,4,9,0]
    // - this property is cached, so request is sent only once
    // - you may easier use ServerBuildInfoNumber to check for available
    // features at runtime
    property ServerBuildInfo: variant read GetServerBuildInfo;
    /// retrieve the server version and build information
    // - return the content as a TDocVariant document, e.g.
    // ! 2040900 for MongoDB 2.4.9, or 2060000 for MongoDB 2.6
    // - this property is cached, so can be used to check for available
    // features at runtime
    property ServerBuildInfoNumber: cardinal read GetServerBuildInfoNumber;
    /// access to a given MongoDB database
    // - try to open it via a non-authenticated connection it if not already:
    // will raise an exception on error, or will return an instance
    // - will return an existing instance if has already been opened
    property Database[const DatabaseName: RawUTF8]: TMongoDatabase read Open; default;
    /// low-level access to the TCP/IP connections of this MongoDB replica set
    // - first item [0] is the Primary member
    // - other items [1..] are the Secondary members
    property Connections: TMongoConnectionDynArray read fConnections;
    /// define the logging instance to be used for LogRequestEvent/LogReplyEvent
    property Log: TSynLog read fLog write fLog;
  published
    /// define Read Preference mode to a MongoDB replica set
    // - see http://docs.mongodb.org/manual/core/read-preference
    // - default is rpPrimary, i.e. reading from the main primary instance
    // - Important: All read preference modes except rpPrimary may return stale
    // data because secondaries replicate operations from the primary with some
    // delay - ensure that your application can tolerate stale data if you choose
    // to use a non-primary mode
    property ReadPreference: TMongoClientReplicaSetReadPreference
      read fReadPreference write fReadPreference;
    /// define Write Concern mode to a MongoDB replica set
    // - see http://docs.mongodb.org/manual/core/write-concern
    // - default is wcAcknowledged, i.e. to acknowledge all write operations
    property WriteConcern: TMongoClientWriteConcern
      read fWriteConcern write fWriteConcern;
    /// the connection definition used to connect to this MongoDB server
    property ConnectionString: RawUTF8 read fConnectionString;
    /// the connection time out, in milli seconds
    // - default value is 30000, i.e. 30 seconds
    property ConnectionTimeOut: Cardinal read fConnectionTimeOut write fConnectionTimeOut;
    /// how may bytes this client did received, among all its connections
    property BytesReceived: Int64 read GetBytesReceived;
    /// how may bytes this client did received, among all its connections
    property BytesSent: Int64 read GetBytesSent;
    /// how may bytes this client did transmit, adding both input and output
    property BytesTransmitted: Int64 read GetBytesTransmitted;
    /// if set to something else than default sllNone, will log each request
    // with the corresponding logging event kind
    // - will use the Log property for the destination log
    property LogRequestEvent: TSynLogInfo read fLogRequestEvent write fLogRequestEvent;
    /// if set to something else than default sllNone, will log each reply
    // with the corresponding logging event kind
    // - WARNING: logging all incoming data may be very verbose, e.g. when
    // retrieving a document list - use it with care, not on production, but
    // only for debugging purposes
    // - will use the Log property for the destination log
    property LogReplyEvent: TSynLogInfo read fLogReplyEvent write fLogReplyEvent;
  end;

  /// remote access to a MondoDB database
  TMongoDatabase = class
  protected
    fClient: TMongoClient;
    fName: RawUTF8;
    fCollections: TRawUTF8ListHashed;
    function GetCollection(const Name: RawUTF8): TMongoCollection;
    function GetCollectionOrCreate(const Name: RawUTF8): TMongoCollection;
    function GetCollectionOrNil(const Name: RawUTF8): TMongoCollection;
  public
    /// initialize a reference to a given MongoDB Database
    // - you should not use this constructor directly, but rather use the
    // TMongoClient.Database[] property
    // - it will connect to the Client's primary host, then retrieve all
    // collection names of this database
    constructor Create(aClient: TMongoClient; const aDatabaseName: RawUTF8);
    /// release all associated TMongoCollection instances
    destructor Destroy; override;
    
    /// run a database command, supplied as a TDocVariant, TBSONVariant or a
    // string, and return a TDocVariant instance
    // - this is the preferred method to issue database commands, as it provides
    // a consistent interface between the MongoDB shell and this driver
    // - see http://docs.mongodb.org/manual/reference/command for a list
    // of all available commands
    // - for instance:
    // ! RunCommand(_ObjFast(['dbStats',1,'scale',1024],stats);
    // ! RunCommand(BSONVariant(['dbStats',1,'scale',1024],stats);
    // ! RunCommand('dbStats',stats);
    // ! RunCommand('hostInfo',host);
    // - the message will be returned by the server as a TDocVariant instance
    // (since the associated TMongoRequestQuery.NumberToSkip=1)
    // - in case of any error, the error message is returned as text
    // - in case of success, this method will return ''
    function RunCommand(const command: variant;
      var returnedValue: variant): RawUTF8; overload;
    /// run a database command, supplied as a TDocVariant, TBSONVariant or a
    // string, and return the raw BSON document array of received items
    // - this overloaded method can be used on huge content to avoid the slower
    // conversion to an array of TDocVariant instances
    // - in case of success, this method will return TRUE, or FALSE on error
    function RunCommand(const command: variant;
      var returnedValue: TBSONDocument): boolean; overload;

    /// register an user to the given database
    procedure AddUser(const User: variant);

    /// the associated MongoDB client instance
    property Client: TMongoClient read fClient;
    /// access to a given MongoDB collection
    // - raise an EMongoDatabaseException if the collection name does not exist
    property Collection[const Name: RawUTF8]: TMongoCollection
      read GetCollection; default;
    /// access to a given MongoDB collection
    // - if the collection name does not exist, it will return nil
    property CollectionOrNil[const Name: RawUTF8]: TMongoCollection
      read GetCollectionOrNil;
    /// access to a given MongoDB collection
    // - if the collection name does not exist, it will add use the name to
    // create a TMongoCollection instance and register it to the internal list
    property CollectionOrCreate[const Name: RawUTF8]: TMongoCollection
      read GetCollectionOrCreate;
  published
    /// the database name
    property Name: RawUTF8 read fName;
  end;

  /// remote access to a MondoDB collection
  TMongoCollection = class
  protected
    fDatabase: TMongoDatabase;
    fName: RawUTF8;
    fFullCollectionName: RawUTF8;
    function AggregateCall(Operators: PUTF8Char;
      const Params: array of const; var reply,res: variant): boolean;
  public
    /// initialize a reference to a given MongoDB Collection
    // - you should not use this constructor directly, but rather use
    // TMongoClient.Database[].Collection[] property
    constructor Create(aDatabase: TMongoDatabase; const aCollectionName: RawUTF8);

    /// select documents in a collection and returns a dvArray TDocVariant
    // instance containing the selected documents
    // - Criteria can be null (to retrieve all documents) or a TDocVariant /
    // TBSONVariant query selector:
    // ! FindDoc(BSONVariant('{name:"John",age:{$gt,21}}'),null);
    // ! FindDoc(BSONVariant('{name:?,age:{$gt,?}}',[],['John',21]),null);
    // see http://docs.mongodb.org/manual/reference/operator for reference
    // - Projection can be null (to retrieve all fields) or a string to specify
    // one field name to retrieve, or a TDocVariant or TBSONVariant - e.g.:
    // ! FindDoc(BSONVariant(['name','John']),null)
    // ! FindDoc(BSONVariant(['name','John']),'_id')
    // ! FindDoc(BSONVariant(['name','John']),BSONVariantFieldSelector('name,_id'))
    // - NumberToReturn can be left to its default maxInt value to return all
    // matching documents, or specify a limit (e.g. 1 for one document - in this
    // case, the returned instance won't be a dvArray kind of TDocVariant, but
    // either null or the single returned document)
    // - if the query does not have any matching record, it will return null
    function FindDoc(const Criteria, Projection: Variant;
      NumberToReturn: integer=1; NumberToSkip: Integer=0;
      Flags: TMongoQueryFlags=[]): variant; overload;
    /// select documents in a collection and returns a dvArray TDocVariant
    // instance containing the selected documents
    // - Criteria can specify the query selector as (extended) JSON and
    // parameters:
    // ! FindDoc('{name:"John",age:{$gt,21}}',[]);
    // ! FindDoc('{name:?,age:{$gt,?}}',['John',21]));
    // see http://docs.mongodb.org/manual/reference/operator for reference
    // - this overloaded method will use a null Projection, i.e. will retrieve
    // all fields
    // - NumberToReturn can be left to its default maxInt value to return all
    // matching documents, or specify a limit (e.g. 1 for one document - in this
    // case, the returned instance won't be a dvArray kind of TDocVariant, but
    // either null or the single returned document)
    // - if the query does not have any matching record, it will return null
    function FindDoc(Criteria: PUTF8Char; const Params: array of const;
      NumberToReturn: integer=maxInt; NumberToSkip: Integer=0;
      Flags: TMongoQueryFlags=[]): variant; overload;
    /// find an existing document in a collection, by its _id field
    // - _id will identify the unique document to be retrieved
    function FindOne(const _id: TBSONObjectID): variant; overload;
    /// find an existing document in a collection, by its _id field
    // - _id will identify the unique document to be retrieved
    function FindOne(const _id: variant): variant; overload;
    /// returns a dynamic array of TDocVariant instance containing
    // all documents of a collection
    procedure FindDocs(var result: TVariantDynArray; const Projection: RawUTF8='';
      NumberToReturn: integer=maxInt; NumberToSkip: Integer=0;
      Flags: TMongoQueryFlags=[]); overload;
    /// select documents in a collection and returns a dynamic array of
    // TDocVariant instance containing the selected documents
    procedure FindDocs(Criteria: PUTF8Char; const Params: array of const;
      var result: TVariantDynArray; const Projection: RawUTF8='';
      NumberToReturn: integer=maxInt; NumberToSkip: Integer=0;
      Flags: TMongoQueryFlags=[]); overload; 

    /// select documents in a collection and returns a JSON array of documents
    // containing the selected documents
    // - Criteria can be null (to retrieve all documents) or a TDocVariant /
    // TBSONVariant query selector:
    // ! FindJSON(BSONVariant('{name:"John",age:{$gt,21}}'),null);
    // ! FindJSON(BSONVariant('{name:?,age:{$gt,?}}',[],['John',21]),null);
    // see http://docs.mongodb.org/manual/reference/operator for reference
    // - Projection can be null (to retrieve all fields) or a string to specify
    // one field name to retrieve, or a TDocVariant or TBSONVariant - e.g.:
    // ! FindJSON(BSONVariant(['name','John']),null)
    // ! FindJSON(BSONVariant(['name','John']),'_id')
    // ! FindJSON(BSONVariant(['name','John']),BSONVariantFieldSelector('name,_id'))
    // - NumberToReturn can be left to its default maxInt value to return all
    // matching documents as a '[..]' JSON array, or specify a limit (e.g. 1
    // for one document - in this case, the returned instance won't be a '[..]'
    // JSON array, but either 'null' or a single '{..}' JSON object)
    // - this method is very optimized and will convert the BSON binary content
    // directly into JSON, in either modMongoStrict or modMongoShell layout
    // (modNoMongo will do the same as modMongoStrict)
    function FindJSON(const Criteria, Projection: Variant;
      NumberToReturn: integer=maxInt; NumberToSkip: Integer=0;
      Flags: TMongoQueryFlags=[]; Mode: TMongoJSONMode=modMongoStrict): RawUTF8; overload;
    /// select documents in a collection and returns a JSON array of documents
    // containing the selected documents
    // - Criteria can specify the query selector as (extended) JSON and
    // parameters:
    // ! FindJSON('{name:"John",age:{$gt,21}}',[]);
    // ! FindJSON('{name:?,age:{$gt,?}}',['John',21]));
    // see http://docs.mongodb.org/manual/reference/operator for reference
    // - this overloaded method will use a null Projection, i.e. will retrieve
    // all fields
    // - NumberToReturn can be left to its default maxInt value to return all
    // matching documents as a '[..]' JSON array, or specify a limit (e.g. 1
    // for one document - in this case, the returned instance won't be a '[..]'
    // JSON array, but either 'null' or a single '{..}' JSON object)
    function FindJSON(Criteria: PUTF8Char; const Params: array of const;
      NumberToReturn: integer=maxInt; NumberToSkip: Integer=0;
      Flags: TMongoQueryFlags=[]; Mode: TMongoJSONMode=modMongoStrict): RawUTF8; overload;
    /// select documents in a collection and returns a JSON array of documents
    // containing the selected documents
    // - Criteria and Projection can specify the query selector as (extended)
    // JSON and parameters
    function FindJSON(Criteria: PUTF8Char; const CriteriaParams: array of const;
      const Projection: RawUTF8; 
      NumberToReturn: integer=maxInt; NumberToSkip: Integer=0;
      Flags: TMongoQueryFlags=[]; Mode: TMongoJSONMode=modMongoStrict): RawUTF8; overload;

    /// select documents in a collection and returns a TBSONDocument instance
    // containing the selected documents as a raw binary BSON array document
    // - Criteria can be null (to retrieve all documents) or a TDocVariant /
    // TBSONVariant query selector:
    // ! FindBSON(BSONVariant('{name:"John",age:{$gt,21}}'),null);
    // ! FindBSON(BSONVariant('{name:?,age:{$gt,?}}',[],['John',21]),null);
    // - Projection can be null (to retrieve all fields) or a string to specify
    // one field name to retrieve, or a TDocVariant or TBSONVariant - e.g.:
    // ! FindBSON(BSONVariant(['name','John']),null)
    // ! FindBSON(BSONVariant(['name','John']),'_id')
    // ! FindBSON(BSONVariant(['name','John']),BSONVariantFieldSelector('name,_id'))
    // - NumberToReturn can be left to its default maxInt value to return all
    // matching documents, or specify a limit (e.g. 1 for one document)
    function FindBSON(const Criteria, Projection: Variant;
      NumberToReturn: integer=maxInt; NumberToSkip: Integer=0;
      Flags: TMongoQueryFlags=[]): TBSONDocument;

    /// insert one document, supplied as (extended) JSON and parameters,
    // in the collection
    // - supplied JSON could be either strict or in MongoDB Shell syntax:
    // !   products.insert('{ _id: ?, item: ?, qty: ? }',[1,'card',15]);
    // !   // here _id is forced on the client side
    // !   products.insert('{ item: ?, qty: ? }',[1,'card',15]);
    // !   // here the _id will be created on the client side as an ObjectID
    // - you can retrieve the client-side computed ObjectID, as such:
    // ! var oid: TBSONObjectID;
    // ! ...
    // !   products.insert('{ item: ?, qty: ? }',['card',15],@oid);
    // !   writeln(oid.ToText);
    procedure Insert(Document: PUTF8Char; const Params: array of const;
      CreatedObjectID: PBSONObjectID=nil); overload;
    /// insert one or more documents in the collection
    // - Documents is an array of TDocVariant (i.e. created via _JsonFast()
    // or _JsonFastFmt()) - or of TBSONVariant (created via BSONVariant())
    // - by default, it will follow Client.WriteConcern pattern - but you can
    // set NoAcknowledge = TRUE to avoid calling the getLastError command and
    // increase the execution speed, at the expense of a unsafe process
    procedure Insert(const Documents: array of variant; Flags: TMongoInsertFlags=[];
      NoAcknowledge: boolean=false); overload;
    /// insert one or more documents in the collection
    // - Documents is the low-level concatenation of BSON documents, created
    // e.g. with a TBSONWriter stream
    // - by default, it will follow Client.WriteConcern pattern - but you can
    // set NoAcknowledge = TRUE to avoid calling the getLastError command and
    // increase the execution speed, at the expense of a unsafe process
    procedure Insert(const Documents: TBSONDocument;
      Flags: TMongoInsertFlags=[]; NoAcknowledge: boolean=false); overload;
    /// insert one or more documents in the collection
    // - JSONDocuments is an array of JSON objects
    // - by default, it will follow Client.WriteConcern pattern - but you can
    // set NoAcknowledge = TRUE to avoid calling the getLastError command and
    // increase the execution speed, at the expense of a unsafe process
    procedure InsertJSON(const JSONDocuments: array of PUTF8Char;
      Flags: TMongoInsertFlags=[]; NoAcknowledge: boolean=false); 

    /// updates an existing document or inserts a new document, depending on
    // its document parameter
    // - this document should be a TDocVariant (i.e. created via _JsonFast()
    // or _JsonFastFmt()) since we need to check for the _id field, other types
    // will be converted to a TDocVariant instance (via its JSON representation)
    // so it is pointless to use BSONVariant() here
    // - if the document does not contain an _id field, then the Save() method
    // performs an insert; during the operation, the client will add to the
    // Document variant the _id field and assign it a unique ObjectId - you can
    // optionally retrieve it with the CreatedObjectID pointer - and the method
    // returns FALSE
    // - if the document contains an _id field, then the save() method performs
    // an upsert, querying the collection on the _id field: if a document does
    // not exist with the specified _id value, the save() method performs an
    // insert; if a document exists with the specified _id value, the save()
    // method performs an update that replaces ALL fields in the existing
    // document with the fields from the document - and the method returns TRUE
    function Save(var Document: variant; CreatedObjectID: PBSONObjectID=nil): boolean; overload;
    /// updates an existing document or inserts a new document, depending on
    // its document parameter, supplied as (extended) JSON and parameters
    // - supplied JSON could be either strict or in MongoDB Shell syntax:
    // - will perform either an insert or an update, depending of the
    // presence of the _id field, as overloaded Save(const Document: variant)
    procedure Save(Document: PUTF8Char; const Params: array of const;
      CreatedObjectID: PBSONObjectID=nil); overload;

    /// modifies an existing document or several documents in a collection
    // - the method can modify specific fields of existing document or documents
    // or replace an existing document entirely, depending on the update parameter
    // - Query and Update parameters should be TDocVariant (i.e. created via
    // _JsonFast() or _JsonFastFmt()) or TBSONVariant (created via BSONVariant())
    // - Query is the selection criteria for the update; use the same query
    // selectors as used in the Find() method
    // - if Update contains a plain document, it will replace any existing data
    // - if Update contains update operators (like $set), it will update the
    // corresponding fields in the document
    procedure Update(const Query, Update: variant; Flags: TMongoUpdateFlags=[]); overload;
    /// modifies an existing document or several documents in a collection
    // - the method can modify specific fields of existing document or documents
    // or replace an existing document entirely, depending on the update parameter
    // - since all content will be transformed into JSON internally, use this
    // method only if the supplied parameters are simple types: any complex value
    // (e.g. a TDateTime or a BSONVariant binary) won't be handled as expected -
    // use the overloaded Update() with explicit BSONVariant() values instead 
    // - Query and Update parameters can be specified as JSON objects with
    // parameters
    // - Query is the selection criteria for the update; use the same query
    // selectors as used in the Find() method
    // - if Update contains a plain document, it will replace any existing data:
    // ! people.update('{name:?}',['Andy'],'{name:?,age:? }',['Andy',25],[mufUpsert]);
    // Warning: to avoid inserting the same document more than once, only use
    // mufUpsert if the query field is uniquely indexed
    // - if Update contains update operators (like $set), it will update the
    // corresponding fields in the document:
    // ! book.insert('{_id:?,item:?,stock:?}',[11,'Divine Comedy',2]);
    // ! book.update('{item:?},['Divine Comedy'],'{$set:{price:?},$inc:{$stock:?}},[18,5]);
    // ! // the updated document is now:
    // ! { "_id" : 11, "item" : "Divine Comedy", "price" : 18, "stock" : 7 }
    procedure Update(Query: PUTF8Char; const QueryParams: array of const;
      Update: PUTF8Char; const UpdateParams: array of const;
      Flags: TMongoUpdateFlags=[]); overload;
    /// modifies some fields of an existing document in a collection
    // - by default, Update() or Save() will replace the whole document
    // - this method will expect the identifier to be supplied as a variant -
    // may be via the ObjectID() function
    // - and will replace the specified fields, i.e. it will execute a $set:
    // with the supplied UpdatedFields value
    procedure UpdateOne(const _id, UpdatedFields: variant);

    /// delete an existing document or several documents in a collection
    // - Query parameter should be TDocVariant (i.e. created via _JsonFast() or
    // _JsonFastFmt()) or TBSONVariant (created via BSONVariant())
    // - Query is the selection criteria for the deletion; use the same query
    // selectors as used in the Find() method
    // - to limit the deletion to just one document, set Flags to [mdfSingleRemove] 
    // - to delete all documents matching the deletion criteria, leave it to []
    procedure Remove(const Query: variant; Flags: TMongoDeleteFlags=[]); overload;
    /// delete an existing document in a collection, by its _id field
    // - _id will identify the unique document to be deleted
    procedure RemoveOne(const _id: TBSONObjectID); overload;
    /// delete an existing document in a collection, by its _id field
    // - _id will identify the unique document to be deleted
    procedure RemoveOne(const _id: variant); overload;
    /// delete an existing document or several documents in a collection
    // - Query parameter can be specified as JSON objects with parameters
    // - Query is the selection criteria for the deletion; use the same query
    // selectors as used in the Find() method
    // - to limit the deletion to just one document, set Flags to [mdfSingleRemove] 
    // - to delete all documents matching the deletion criteria, leave it to []
    procedure RemoveFmt(Query: PUTF8Char; const QueryParams: array of const;
       Flags: TMongoDeleteFlags=[]); 

    /// creates an index on the specified field(s) if the index does
    // not already exist
    // - Keys and Options parameters should be TDocVariant (e.g. created via
    // _JsonFast() or _JsonFastFmt()) - and not TBSONVariant values
    // - for ascending/descending indexes, Keys is a document that contains pairs
    // with the name of the field or fields to index and order of the index:
    // value of 1 specifies ascending and of -1 specifies descending
    // - options is a non-mandatory document that controls the creation
    // of the index -
    // - you can write e.g.
    // ! book.EnsureIndex(_JsonFast('{ orderDate: 1 }'),null)
    // ! book.EnsureIndex(_ObjFast(['orderDate',1]),null)
    procedure EnsureIndex(const Keys, Options: variant); overload;
    /// creates an index on the specified field(s) if the index does
    // not already exist
    // - Keys are the correspondiong field names
    // - you can write e.g. to create an ascending index on a given field:
    // ! book.EnsureIndex(['orderDate']);
    procedure EnsureIndex(const Keys: array of RawUTF8; Ascending: boolean=true;
      Unique: boolean=false); overload;
    /// drops the entire collection from the database
    // - once dropped, this TMongoCollection instance will be freed: never
    // use this instance again after success (i.e. returned '')
    // - in case of error, a textual message will be returned as result
    // - once dropped, this collection will be removed from the parent
    // Database.Collection[] internal list
    // - Warning: this method obtains a write lock on the affected database
    // and will block other operations until it has completed
    function Drop: RawUTF8;

    /// calculate the number of documents in the collection
    function Count: integer;
    /// calculate the number of documents in the collection that match
    // a specific query
    // - Criteria can specify the query selector as a BSONVariant/TDocVariant
    function FindCount(const Query: variant): integer; overload;
    /// calculate the number of documents in the collection that match
    // a specific query
    // - Criteria can specify the query selector as (extended) JSON and
    // parameters:
    // ! FindCount('{name:?,age:{$gt,?}}',[],['John',21]));
    // ! FindCount('{ ord_dt: { $gt: new Date(?) } }',[],[trunc(Now)-7]);
    // - optional MaxNumberToReturn can specify a limit for the search (e.g. if
    // you do not want an exact count, but only check for a specific limit)
    // - optional NumberToSkip can specify the number of matching documents
    // to skip before counting
    function FindCount(Criteria: PUTF8Char; const Args,Params: array of const;
      MaxNumberToReturn: integer=0; NumberToSkip: Integer=0): integer; overload;
    /// calculate aggregate values using the MongoDB aggregation framework
    // and return the result as a TDocVariant instance
    // - the Aggregation Framework was designed to be more efficient than the
    // alternative map-reduce pattern
    // - you should specify the aggregation pipeline as a list of JSON object
    // operators (without the [..]) - for reference of all available phases,
    // see http://docs.mongodb.org/manual/core/aggregation-pipeline
    // - if the server sent back no {result:...} member, will return null
    // - if the server sent back one item as {result:[{..}]}, will return
    // this single item as a TDocVariant
    // - if the server sent back several items as {result:[{..},{..}]}, will
    // return a dvArray kind of TDocVariant
    // - for instance, the following will return the maximum _id value of
    // the collection:
    // ! AggregateDoc('{$group:{_id:null,max:{$max:"$_id"}}}',[]).max
    function AggregateDoc(Operators: PUTF8Char; const Params: array of const): variant;
    /// calculate JSON aggregate values using the MongoDB aggregation framework
    // - the Aggregation Framework was designed to be more efficient than the
    // alternative map-reduce pattern
    // - you should specify the aggregation pipeline as a list of JSON object
    // operators (without the [..]) - for reference of all available phases,
    // see http://docs.mongodb.org/manual/core/aggregation-pipeline
    // - for instance, the following will return as JSON a collection sorted in
    // descending order according by the age field and then in ascending order
    // according to the value in the posts field
    // ! AggregateJSON('{ $sort : { age : -1, posts: 1 } }',[])
    function AggregateJSON(Operators: PUTF8Char; const Params: array of const;
      Mode: TMongoJSONMode=modMongoStrict): RawUTF8;
  published
    /// the associated MongoDB database instance
    property Database: TMongoDatabase read fDatabase;
    /// the collection name
    property Name: RawUTF8 read fName;
    /// the full collection name, e.g. 'dbname.collectionname'
    property FullCollectionName: RawUTF8 read fFullCollectionName;
  end;

  /// exception type used for MongoDB process, once connected
  EMongoConnectionException = class(EMongoException)
  protected
    fConnection: TMongoConnection;
  public
    /// initialize the Exception for a given request
    constructor Create(const aMsg: string; aConnection: TMongoConnection); reintroduce; overload;
    /// initialize the Exception for a given request
    constructor CreateFmt(const aMsg: string; const Args: array of const;
      aConnection: TMongoConnection); reintroduce;
    /// used to customize the exception log to contain information about the Query
    // - it will log the connection parameters
    function CustomLog(WR: TTextWriter; const Context: TSynLogExceptionContext): boolean; override;
    /// the associated connection
    property Connection: TMongoConnection read fConnection;
  end;

  EMongoDatabaseException = class(EMongoConnectionException)
  protected
    fDatabase: TMongoDatabase;
  public
    /// initialize the Exception for a given request
    constructor Create(const aMsg: string; aDatabase: TMongoDatabase); reintroduce; overload;
    /// initialize the Exception for a given request
    constructor CreateFmt(const aMsg: string; const Args: array of const;
      aDatabase: TMongoDatabase); reintroduce;
    /// used to customize the exception log to contain information about the Query
    // - it will log the database parameters
    function CustomLog(WR: TTextWriter; const Context: TSynLogExceptionContext): boolean; override;
    /// the associated Database
    property Database: TMongoDatabase read fDatabase;
  end;

  /// exception type used for MongoDB query process
  EMongoRequestException = class(EMongoConnectionException)
  protected
    fRequest: TMongoRequest;
    fError: TMongoReplyCursor;
    fErrorDoc: variant;
    function GetErrorDoc: variant;
  public
    /// initialize the Exception for a given request
    constructor Create(const aMsg: string; aConnection: TMongoConnection;
      aRequest: TMongoRequest=nil); reintroduce; overload;
    /// initialize the Exception for a given request
    constructor CreateFmt(const aMsg: string; const Args: array of const;
      aConnection: TMongoConnection; aRequest: TMongoRequest); reintroduce;
    /// initialize the Exception for a given request
    constructor Create(const aMsg: string; aConnection: TMongoConnection;
      aRequest: TMongoRequest; const aError: TMongoReplyCursor); reintroduce; overload;
    /// initialize the Exception for a given request
    constructor Create(const aMsg: string; aConnection: TMongoConnection;
      aRequest: TMongoRequest; const aErrorDoc: TDocVariantData); reintroduce; overload;
    /// used to customize the exception log to contain information about the Query
    // - it will log both the failing request and the returned error message
    function CustomLog(WR: TTextWriter; const Context: TSynLogExceptionContext): boolean; override;
    /// the associated error reply document
    property ErrorReply: TMongoReplyCursor read fError;
    /// the associated error reply document, as a TDocVariant instance
    // - will return the first document available in ErrorReply, or the supplied
    // aErrorDoc: TDocVariantData instance
    property ErrorDoc: Variant read GetErrorDoc;
  end;

  /// exception type used for MongoDB query process after an Operating System
  // error (e.g. in case of socket error)
  EMongoRequestOSException = class(EMongoRequestException)
  protected
    fSystemLastError: cardinal;
  public
    /// initialize the Exception for a given request, including the last
    // error message retrieved from the operating system
    // - if such an exception is raised, you can use SystemLastError property
    // to retrieve the corresponding Operating System error code
    constructor Create(const aMsg: string; aConnection: TMongoConnection;
      aRequest: TMongoRequest=nil); reintroduce;
    /// contain the associated Operating System last error code
    // - will specify e.g. the kind of communication/socket error
    property SystemLastError: cardinal read fSystemLastError;
  end;

{$M-}



implementation

// used by TBSONElement.ToVariant() method and BSONToDoc() procedure
procedure BSONItemsToDocVariant(Kind: TBSONElementType; BSON: PByte;
  var Doc: TDocVariantData; Option: TBSONDocArrayConversion);
const OPTIONS: array[TBSONDocArrayConversion] of TDocVariantOptions =
    ([],[dvoReturnNullForUnknownProperty],
        [dvoValueCopiedByReference,dvoReturnNullForUnknownProperty]);
var item: TBSONElement;
    value: Variant;
begin
  if not (Kind in [betDoc,betArray]) then
    VarCastError;
  if (BSON=nil) or (TBSONElementType(BSON^)=betEof) then
    TVarData(Doc).VType := varNull else begin
    Doc.Init(OPTIONS[Option]);
    while item.FromNext(BSON) do begin
      item.ToVariant(value,Option);
      if Kind=betDoc then
        Doc.AddValue(item.Name,value) else
        Doc.AddItem(value);
    end;
  end;
end;


{ TBSONElement }

var
  /// size (in bytes) of a BSON element
  // - equals -1 for varying elements
  BSON_ELEMENTSIZE: array[TBSONElementType] of integer = (
    //betEOF, betFloat, betString, betDoc, betArray, betBinary,
      0,     sizeof(Double), -1,     -1,     -1,       -1,
    //betDeprecatedUndefined, betObjectID, betBoolean, betDateTime,
      0,                    sizeof(TBSONObjectID), 1, sizeof(Int64),
    //betNull, betRegEx, betDeprecatedDbptr, betJS, betDeprecatedSymbol,
      0,        -1,           -1,             -1,        -1,
    //betJSScope, betInt32, betTimeStamp, betInt64
      -1, sizeof(Integer), sizeof(Int64), SizeOf(Int64));

  /// types which do not have an exact equivalency to a standard variant
  // type will be mapped as varUnknown - and will be changed into
  // BSONVariantType.VarType
  BSON_ELEMENTTYPES: array[TBSONElementType] of word = (
    //betEOF, betFloat, betString, betDoc, betArray, betBinary,
    varEmpty, varDouble, varString, varUnknown, varUnknown, varUnknown,
    //betDeprecatedUndefined, betObjectID, betBoolean, betDateTime,
    varEmpty, varUnknown, varBoolean, varDate,
    //betNull, betRegEx, betDeprecatedDbptr, betJS, betDeprecatedSymbol,
    varNull, varUnknown, varUnknown, varUnknown, varUnknown,
    //betJSScope, betInt32, betTimeStamp, betInt64
    varUnknown, varInteger, varWord64, varInt64);

function TBSONElement.ToVariant(DocArrayConversion: TBSONDocArrayConversion): variant;
begin
  ToVariant(Result,DocArrayConversion);
end;

procedure TBSONElement.ToVariant(var result: variant;
  DocArrayConversion: TBSONDocArrayConversion);
var res: TVarData absolute result;
    resBSON: TBSONVariantData absolute result;
begin
  if not(res.VType in VTYPE_STATIC) then
    VarClear(result);
  ZeroFill(TVarData(result)); // set result.VType=varEmpty and result.VAny=nil
  case Kind of
  betFloat:
    res.VDouble := PDouble(Element)^;
  betString:
    SetString(RawUTF8(res.VAny),Data.Text,Data.TextLen);
  betJS, betDeprecatedSymbol:
    SetString(RawUTF8(resBSON.VText),Data.Text,Data.TextLen);
  betDoc, betArray:
    if DocArrayConversion=asBSONVariant then
      SetString(TBSONDocument(resBSON.VBlob),PAnsiChar(Element),ElementBytes) else begin
      BSONItemsToDocVariant(Kind,Data.DocList,TDocVariantData(result),DocArrayConversion);
      exit;
    end;
  betBinary, betRegEx, betDeprecatedDbptr, betJSScope:
    SetString(RawByteString(resBSON.VBlob),PAnsiChar(Element),ElementBytes);
  betObjectID:
    resBSON.VObjectID := PBSONObjectID(Element)^;
  betBoolean:
    res.VBoolean := PBoolean(Element)^;
  betDateTime:
    res.VDate := UnixMSTimeToDateTime(PInt64(Element)^);
  betInt32:
    res.VInteger := PInteger(Element)^;
  betInt64:
    res.VInt64 := PInt64(Element)^;
  // betNull, betDeprecatedUndefined, betMinKey or betMaxKey has no data
  end;
  res.VType := BSON_ELEMENTTYPES[Kind];
  if res.VType=varUnknown then begin
    resBSON.VType := BSONVariantType.VarType;
    resBSON.VKind := Kind;
  end;
end;

function TBSONElement.ToInteger(const default: Int64=0): Int64;
begin
  case Kind of
  betBoolean:
    result := PByte(Element)^;
  betFloat:
    result := Trunc(PDouble(Element)^);
  betInt32:
    result := PInteger(Element)^;
  betInt64:
    result := PInt64(Element)^;
  else
    result := default;
  end;
end;

function TBSONElement.ToRawUTF8: RawUTF8;
procedure ComplexType;
var V: variant;
    wasString: boolean;
begin
  ToVariant(V);
  VariantToUTF8(V,result,wasString);
end;
begin
  case Kind of
  betFloat:
    ExtendedToStr(PDouble(Element)^,DOUBLE_PRECISION,result);
  betString:
    SetString(result,Data.Text,Data.TextLen);
  betInt32:
    Int32ToUtf8(PInteger(Element)^,result);
  betInt64:
    Int64ToUtf8(PInt64(Element)^,result);
  else ComplexType;
  end;
end;

procedure TBSONElement.AddMongoJSON(W: TTextWriter; Mode: TMongoJSONMode);
label bin,regex;
begin
  case Kind of
  betFloat:
    W.Add(PDouble(Element)^);
  betString, betJS, betDeprecatedSymbol: begin
    W.Add('"');
    W.AddJSONEscape(Data.Text,Data.TextLen);
    W.Add('"');
  end;
  betDoc, betArray:
    BSONToJSON(Data.DocList,Kind,W,Mode);
  betObjectID: begin
    W.AddShort(BSON_JSON_OBJECTID[false,Mode]);
    W.AddBinToHex(Element,SizeOf(TBSONObjectID));
    W.AddShort(BSON_JSON_OBJECTID[true,Mode]);
  end;
  betDeprecatedUndefined:
    W.AddShort(BSON_JSON_UNDEFINED[Mode=modMongoShell]);
  betBinary:
  case Mode of
  modNoMongo:
    W.WrBase64(Data.Blob,Data.BlobLen,true);
  modMongoStrict: begin
    W.AddShort(BSON_JSON_BINARY[false,false]);
    W.WrBase64(Data.Blob,Data.BlobLen,false);
    W.AddShort(BSON_JSON_BINARY[false,true]);
    W.AddBinToHex(@Data.BlobSubType,1);
    W.AddShort('"}');
  end;
  modMongoShell: begin
    W.AddShort(BSON_JSON_BINARY[true,false]);
    W.AddBinToHex(@Data.BlobSubType,1);
    W.AddShort(BSON_JSON_BINARY[true,true]);
    W.WrBase64(Data.Blob,Data.BlobLen,false);
    W.AddShort('")');
  end;
  end;
  betRegEx:
  case Mode of
  modNoMongo:
bin:W.WrBase64(Element,ElementBytes,true);
  modMongoStrict:
    goto regex;
  modMongoShell:
    if (PosChar(Data.RegEx,'/')=nil) and
       (PosChar(Data.RegExOptions,'/')=nil) then begin
      W.Add('/');
      W.AddNoJSONEscape(Data.RegEx,Data.RegExLen);
      W.Add('/');
      W.AddNoJSONEscape(Data.RegExOptions,Data.RegExOptionsLen);
    end else begin
regex:W.AddShort(BSON_JSON_REGEX[0]);
      W.AddJSONEscape(Data.RegEx,Data.RegExLen);
      W.AddShort(BSON_JSON_REGEX[1]);
      W.AddJSONEscape(Data.RegExOptions,Data.RegExOptionsLen);
      W.AddShort(BSON_JSON_REGEX[2]);
    end;
  end;
  betDeprecatedDbptr:
    goto bin; // no specific JSON construct for this deprecated item
  betJSScope:
    goto bin; // no specific JSON construct for this item yet
  betBoolean:
    W.AddString(JSON_BOOLEAN[PBoolean(Element)^]);
  betDateTime: begin
    W.AddShort(BSON_JSON_DATE[Mode,false]);
    W.AddDateTime(UnixMSTimeToDateTime(PInt64(Element)^));
    W.AddShort(BSON_JSON_DATE[Mode,true]);
  end;
  betNull:
    W.AddShort('null');
  betInt32:
    W.Add(PInteger(Element)^);
  betInt64:
    W.Add(PInt64(Element)^);
  else
  if Kind=betMinKey then
    W.AddShort(BSON_JSON_MINKEY[Mode=modMongoShell]) else
  if Kind=betMaxKey then
    W.AddShort(BSON_JSON_MAXKEY[Mode=modMongoShell]) else
    raise EBSONException.CreateFmt('Unexpected BSON element of type %d',[ord(Kind)]);
  end;
end;

procedure TBSONElement.FromVariant(const aName: RawUTF8; const aValue: Variant;
  var aTemp: RawByteString);
const ELEMKIND: array[varEmpty..varWord64] of TBSONElementType = (
  betEOF, betNull, betInt32, betInt32, betFloat, betFloat, betFloat, betDateTime,
  betString, betEOF, betEOF, betBoolean, betEof, betEOF, betEOF, betEOF,
  betInt32, betInt32, betInt32, betInt64, betInt64, betInt64);
var aVarData: TVarData absolute aValue;
    aBson: TBSONVariantData absolute aValue;
    aDoc: TDocVariantData absolute aValue;
label str, st2;
begin
  if aVarData.VType=varByRef or varVariant then begin
    FromVariant(aName,PVariant(aVarData.VPointer)^,aTemp);
    exit;
  end;
  FillChar(self,sizeof(self),0);
  Name := pointer(aName);
  NameLen := length(aName);
  case aVarData.VType of
  0..varDate,varBoolean..high(ELEMKIND): begin // simple types
    Element := @Data.InternalStorage;
    Kind := ELEMKIND[aVarData.VType];
    case Kind of
    betFloat:
      PDouble(Element)^ := double(aValue);
    betDateTime:
      PInt64(Element)^ := DateTimeToUnixMSTime(aVarData.VDate);
    betBoolean:
      PBoolean(Element)^ := aVarData.VBoolean;
    betInt32:
      if not VariantToInteger(aValue,PInteger(Element)^) then
        VarCastError;
    betInt64:
      if not VariantToInt64(aValue,PInt64(Element)^) then
        VarCastError;
    end;
    ElementBytes := BSON_ELEMENTSIZE[Kind];
  end;
  varString: begin
    Kind := betString;
    Data.Text := aVarData.VAny;
    Data.TextLen := Length(RawUTF8(aVarData.VAny));
st2:ElementBytes := Data.TextLen+1;
    if aVarData.VAny=nil then
      Data.InternalStorage := 1 else
      Element := nil; // special case handled by TBSONWriter.BSONWrite()
  end;
  {$ifdef UNICODE}
  varUString: begin
    aTemp := UnicodeStringToUtf8(UnicodeString(aVarData.VAny));
    goto str;
  end;
  {$endif}
  varOleStr: begin
    RawUnicodeToUtf8(aVarData.VAny,length(WideString(aVarData.VAny)),RawUTF8(aTemp));
str:Kind := betString;
    Data.Text := pointer(aTemp);
    Data.TextLen := Length(aTemp);
    goto st2;
  end;
  else
  if aVarData.VType=BSONVariantType.VarType then begin
    Kind := aBson.VKind;
    case Kind of
    betObjectID: FromBSON(@aBson.VObjectID);
    else         FromBSON(aBson.VBlob);
    end;
    if ElementBytes<0 then
      VarCastError;
  end else
  if aVarData.VType=DocVariantType.VarType then begin
    aTemp := BSON(TDocVariantData(aValue));
    case aDoc.Kind of
    dvObject: Kind := betDoc;
    dvArray:  Kind := betArray;
    else VarCastError;
    end;
    FromBSON(pointer(aTemp));
    if ElementBytes<0 then
      VarCastError;
  end else VarCastError;
  end;
end;

const
  NULL_LOW = ord('n')+ord('u')shl 8+ord('l')shl 16+ord('l')shl 24;
  FALSE_LOW = ord('f')+ord('a')shl 8+ord('l')shl 16+ord('s')shl 24;
  TRUE_LOW  = ord('t')+ord('r')shl 8+ord('u')shl 16+ord('e')shl 24;
  NULCHAR: AnsiChar = #0;

procedure TBSONElement.FromBSON(bson: PByte);
begin // see http://bsonspec.org/#/specification
  Element := bson;
  case Kind of
  betString, betJS, betDeprecatedSymbol: begin  // "\x02" e_name string
    ElementBytes := PInteger(bson)^+sizeof(integer); // int32 (byte*) "\x00"
    Data.TextLen := PInteger(bson)^-1;
    inc(bson,sizeof(integer));
    Data.Text := pointer(bson);
  end;
  betDoc, betArray: begin  // "\x03" e_name document
    ElementBytes := PInteger(bson)^;
    inc(bson,sizeof(integer)); // points to a "e_list #0"
    Data.DocList := bson;
  end;
  betBinary: begin         // "\x05" e_name int32 subtype (byte*)
    ElementBytes := PInteger(bson)^+(sizeof(integer)+1);
    Data.BlobLen := PInteger(bson)^;
    inc(bson,sizeof(integer));
    Data.BlobSubType := TBSONElementBinaryType(bson^);
    inc(bson);
    Data.Blob := bson;
  end;
  betObjectID:             // "\x07" e_name (byte*12)
    ElementBytes := sizeof(TBSONObjectID);
  betRegEx: begin          // "\x0B" e_name cstring cstring
    Data.RegEx := Element;
    Data.RegExLen := StrLen(Data.RegEx);
    Data.RegExOptions := Data.RegEx+Data.RegExLen+1;
    Data.RegExOptionsLen := StrLen(Data.RegExOptions);
    ElementBytes := Data.RegExLen+Data.RegExOptionsLen+2;
  end;
  betJSScope: begin       // "\x0F" e_name  int32 string document
    ElementBytes := PInteger(bson)^;
    inc(bson,sizeof(integer));
    Data.JavaScriptLen := PInteger(bson)^-1;
    inc(bson,sizeof(integer));
    Data.JavaScript := pointer(bson);
    inc(bson,Data.JavaScriptLen+1);
    Data.ScopeDocument := bson;
  end;
  else
    if Kind>high(BSON_ELEMENTSIZE) then // e.g. betMinKey betMaxKey
      ElementBytes := 0 else
      ElementBytes := BSON_ELEMENTSIZE[Kind];
  end;
end;

function TBSONElement.FromNext(var BSON: PByte): boolean;
begin
  if BSON=nil then begin
    result := false;
    exit;
  end;
  Kind := TBSONElementType(BSON^);
  inc(BSON);
  case ord(Kind) of
  ord(betEOF):
    result := false;
  ord(betFloat)..ord(betInt64),ord(betMinKey),ord(betMaxKey): begin
    Name := PUTF8Char(BSON);
    NameLen := StrLen(PUTF8Char(BSON));
    inc(BSON,NameLen+1);
    FromBSON(BSON);
    if ElementBytes<0 then
      raise EBSONException.CreateFmt('Invalid %d element content',[ord(Kind)]);
    inc(BSON,ElementBytes);
    inc(Index);
    result := true;
  end;
  else raise EBSONException.CreateFmt('Unexpected %d element type',[ord(Kind)]);
  end;
end;

function TBSONElement.FromSearch(BSON: PByte; const aName: RawUTF8): PByte;
begin
  result := BSON;
  while FromNext(result) do
    if IdemPropNameU(aName,Name,NameLen) then
      exit;
  result := nil;
end;

function BSONParseLength(var BSON: PByte; ExpectedBSONLen: integer=0): integer;
begin
  if (BSON=nil) or
     ((ExpectedBSONLen<>0) and (PInteger(BSON)^<>ExpectedBSONLen)) then
     raise EBSONException.Create('Incorrect supplied BSON document content');
  result := PInteger(BSON)^;
  inc(PInteger(BSON));
end;

function BSONParseNextElement(var BSON: PByte; var name: RawUTF8; var element: variant;
  DocArrayConversion: TBSONDocArrayConversion=asBSONVariant): boolean;
var item: TBSONElement;
begin
  result := item.FromNext(BSON);
  if result then begin
    name := item.Name;
    item.ToVariant(element,DocArrayConversion);
  end;
end;

function BSONSearchElement(BSON: PByte; const name: RawUTF8; var item: TBSONElement): PByte;
begin
  if (BSON<>nil) and (BSONParseLength(BSON)<>0) then
    result := item.FromSearch(item.Data.DocList,name) else
    result := nil;
end;

function BSONPerIndexElement(BSON: PByte; index: integer; var item: TBSONElement): PByte;
begin
  result := BSON;
  if (index>=0) and (result<>nil) and (BSONParseLength(result)<>0) then
    while item.FromNext(result) do
      if index=0 then
        exit else
        dec(index);
  result := nil;
end;
  
procedure BSONToDoc(BSON: PByte; var Result: Variant; ExpectedBSONLen: Integer;
  Option: TBSONDocArrayConversion);
begin
  if Option=asBSONVariant then
    raise EBSONException.Create('BSONToDoc(option=asBSONVariant) is not allowed');
  if not(TVarData(result).VType in VTYPE_STATIC) then
    VarClear(result);
  BSONParseLength(BSON,ExpectedBSONLen);
  BSONItemsToDocVariant(betDoc,BSON,TDocVariantData(Result),Option);
end;

function BSONToDoc(const BSON: TBSONDocument; ExpectedBSONLen: integer;
  Option: TBSONDocArrayConversion): variant;
begin
  BSONToDoc(pointer(BSON),result,ExpectedBSONLen);
end;  

procedure BSONToJSON(BSONList: PByte; Kind: TBSONElementType;
  W: TTextWriter; Mode: TMongoJSONMode);
var item: TBSONElement;
begin
  case Kind of
  betDoc:
    if BSONList^=byte(betEOF) then
      W.AddShort('null') else begin
      W.Add('{');
      while item.FromNext(BSONList) do begin
        if Mode=modMongoShell then begin
          W.AddNoJSONEscape(item.Name,item.NameLen);
          W.Add(':');
        end else
          W.AddFieldName(item.Name,item.NameLen);
        item.AddMongoJSON(W,Mode);
        W.Add(',');
      end;
      W.CancelLastComma;
      W.Add('}');
    end;
  betArray: begin
    W.Add('[');
    while item.FromNext(BSONList) do begin
      item.AddMongoJSON(W,Mode);
      W.Add(',');
    end;
    W.CancelLastComma;
    W.Add(']');
  end;
  else raise EBSONException.CreateFmt('BSONToJSON(Kind=%d)',[ord(Kind)]);
  end;
end;

function BSONToJSON(const BSON: TBSONDocument; ExpectedBSONLen: integer=0;
  Mode: TMongoJSONMode=modMongoStrict): RawUTF8;
begin
  result := BSONToJSON(pointer(BSON),betDoc,ExpectedBSONLen,Mode);
end;  

function BSONToJSON(BSON: PByte; Kind: TBSONElementType; ExpectedBSONLen: integer;
  Mode: TMongoJSONMode): RawUTF8; overload;
var W: TTextWriter;
begin
  BSONParseLength(BSON,ExpectedBSONLen);
  W := TTextWriter.CreateOwnedStream;
  try
    BSONToJSON(BSON,Kind,W,Mode);
    W.SetText(result);
  finally
    W.Free;
  end;
end;

procedure AddMongoJSON(const Value: variant; W: TTextWriter; Mode: TMongoJSONMode);
procedure AddCustom;
var item: TBSONElement;
    temp: RawByteString;
begin
  item.FromVariant('',Value,temp);
  item.AddMongoJSON(W,Mode);
end;
begin
  if TVarData(Value).VType<$10F then
    W.AddVariantJSON(Value,twJSONEscape) else
    AddCustom;
end;

function VariantSaveMongoJSON(const Value: variant; Mode: TMongoJSONMode): RawUTF8;
var W: TTextWriter;
begin
  W := TTextWriter.CreateOwnedStream;
  try
    AddMongoJSON(Value,W,Mode);
    W.SetText(result);
  finally
    W.Free;
  end;
end;


{ TBSONWriter }

procedure TBSONWriter.WriteCollectionName(Flags: integer; const CollectionName: RawUTF8);
begin
  Write4(Flags);
  WriteNonVoidCString(CollectionName,'Missing collection name');
end;

procedure TBSONWriter.WriteNonVoidCString(const text: RawUTF8; const ErrorMsg: string);
begin
  if text='' then
    raise EBSONException.Create(ErrorMsg);
  Write(pointer(text),PInteger(PtrInt(text)-sizeof(integer))^+1); // +1 for #0
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; elemtype: TBSONElementType);
begin
  Write1(ord(elemtype));
  WriteNonVoidCString(name,'Missing name for a BSON element');
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const value: integer);
begin
  BSONWrite(name,betInt32);
  Write4(value);
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const value: Double);
begin
  BSONWrite(name,betFloat);
  Write8(value);
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const value: boolean);
begin
  BSONWrite(name,betBoolean);
  Write1(ord(value));
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const value: Int64);
begin
  if (value>=low(integer)) and (value<=high(integer)) then begin
    BSONWrite(name,betInt32);
    Write4(value);
  end else begin
    BSONWrite(name,betInt64);
    Write8(value);
  end;
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const value: TBSONObjectID);
begin
  BSONWrite(name,betObjectID);
  Write(@value,sizeof(value));
end;

procedure TBSONWriter.BSONWriteRegEx(const name: RawUTF8; const RegEx: RawByteString);
begin
  BSONWrite(name,betRegEx);
  Write(pointer(RegEx),length(RegEx));
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const value: RawUTF8;
  isJavaScript: boolean=false);
const TYP: array[boolean] of TBSONElementType = (betString,betJS);
var L: integer;
begin
  BSONWrite(name,TYP[isJavaScript]);
  L := length(value)+1; // +1 for ending #0
  Write4(L);
  if L=1 then
    Write1(0) else
    Write(pointer(value),L);
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; value: PUTF8Char);
var L: integer;
begin
  BSONWrite(name,betString);
  L := StrLen(Value)+1;
  Write4(L);
  if L=1 then
    Write1(0) else
    Write(value,L);
end;

procedure TBSONWriter.BSONWriteDateTime(const name: RawUTF8; const value: TDateTime);
var UnixTime: Int64;
begin
  UnixTime := DateTimeToUnixMSTime(value);
  BSONWrite(name,betDateTime);
  Write8(UnixTime);
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; Data: pointer; DataLen: integer);
begin
  BSONWrite(name,betBinary);
  Write4(DataLen);
  Write1(ord(bbtGeneric));
  Write(Data,DataLen);
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const elem: TBSONElement);
begin
  BSONWrite(name,elem.Kind);
  if (elem.Element=nil) and // handle special case of TBSONElement.FromVariant()
     (elem.Kind in [betString,betJS,betDeprecatedSymbol]) then begin
    Write4(elem.Data.TextLen+1); // int32 (byte*) "\x00"
    Write(elem.Data.Text,elem.Data.TextLen+1);
  end else
    Write(elem.Element,elem.ElementBytes);
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const bson: TBSONVariantData);
begin
  if bson.VKind=betObjectID then
    BSONWrite(name,bson.VObjectID) else begin
    BSONWrite(name,bson.VKind);
    WriteBinary(RawByteString(bson.VBlob));
  end;
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const doc: TDocVariantData);
begin
  case doc.Kind of
  dvObject:    BSONWrite(name,betDoc);
  dvArray:     BSONWrite(name,betArray);
  dvUndefined: raise EBSONException.Create('Undefined nested document');
  end;
  BSONWriteDoc(doc);
end;

function TBSONWriter.BSONDocumentBegin: cardinal;
begin
  result := TotalWritten;
  Write4(0);
end;

procedure TBSONWriter.BSONDocumentEnd(Start: Cardinal; WriteEndingZero: boolean);
begin
  if WriteEndingZero then
    Write1(0);
  if fDocumentCount>=Length(fDocument) then
    SetLength(fDocument,fDocumentCount+fDocumentCount shr 3+16);
  with fDocument[fDocumentCount] do begin
    Offset := Start;
    Length := TotalWritten-Offset;
  end;
  inc(fDocumentCount);
end;

procedure TBSONWriter.BSONAdjustDocumentsSize(BSON: PByteArray);
var i: Integer;
begin
  for i := 0 to fDocumentCount-1 do
  with fDocument[i] do
    PCardinal(@BSON[Offset])^ := Length;
end;

procedure TBSONWriter.ToBSONDocument(var result: TBSONDocument);
begin
  Flush;
  result := (Stream as TRawByteStringStream).DataString;
  BSONAdjustDocumentsSize(pointer(result));
end;

procedure TBSONWriter.ToBSONVariant(var result: variant);
var doc: TBSONDocument;
begin
  ToBSONDocument(doc);
  BSONVariantType.FromBSONDocument(doc,result);
end;

procedure TBSONWriter.BSONWrite(const name: RawUTF8; const value: TVarRec);
var tmp: RawUTF8;
begin
  case value.VType of
    vtBoolean:  BSONWrite(name,value.VBoolean);
    vtInteger:  BSONWrite(name,value.VInteger);
    vtInt64:    BSONWrite(name,value.VInt64^);
    vtCurrency: BSONWrite(name,value.VCurrency^);
    vtExtended: BSONWrite(name,value.VExtended^);
    vtVariant:  BSONWriteVariant(name,value.VVariant^);
    vtString, vtAnsiString, {$ifdef UNICODE}vtUnicodeString,{$endif}
    vtPChar, vtChar, vtWideChar, vtWideString: begin
      VarRecToUTF8(value,tmp);
      BSONWrite(name,tmp);
    end;
    else raise EBSONException.CreateFmt('Unhandled TVarRec.VType=%d',[value.VType]);
  end;
end;

procedure TBSONWriter.BSONWriteVariant(const name: RawUTF8; const value: variant);
var temp: RawUTF8;
    JSON: PUTF8Char;
begin
  with TVarData(value) do begin
    case VType of
    varEmpty,
    varNull:     BSONWrite(Name,betNull);
    varSmallint: BSONWrite(Name,VSmallInt);
    {$ifndef DELPHI5OROLDER}
    varShortInt: BSONWrite(Name,VShortInt);
    varWord:     BSONWrite(Name,VWord);
    varLongWord: BSONWrite(Name,VLongWord);
    {$endif}
    varByte:     BSONWrite(Name,VByte);
    varBoolean:  BSONWrite(Name,VBoolean);
    varInteger:  BSONWrite(Name,VInteger);
    varWord64,
    varInt64:    BSONWrite(Name,VInt64);
    varSingle:   BSONWrite(Name,VSingle);
    varDouble:   BSONWrite(Name,VDouble);
    varDate:     BSONWriteDateTime(Name,VDate);
    varCurrency: BSONWrite(Name,VCurrency);
    varString:   BSONWrite(Name,RawUTF8(VAny)); // expect UTF-8 content
    {$ifdef UNICODE}
    varUString: begin
      RawUnicodeToUtf8(VAny,length(UnicodeString(VAny)),temp);
      BSONWrite(Name,temp);
    end;
    {$endif}
    varOleStr: begin
      RawUnicodeToUtf8(VAny,length(WideString(VAny)),temp);
      BSONWrite(Name,temp);
    end;
    else
    if VType=varByRef or varVariant then
      BSONWriteVariant(name,PVariant(VPointer)^) else
    if VType=BSONVariantType.VarType then
      BSONWrite(name,TBSONVariantData(value)) else
    if VType=DocVariantType.VarType then
      BSONWrite(name,TDocVariantData(Value)) else begin
      VariantSaveJSON(value,twJSONEscape,temp);
      JSON := pointer(temp);
      BSONWriteFromJSON(name,JSON,nil);
      if JSON=nil then
        raise EBSONException.CreateFmt('Unhandled variant type %d',[VType]);
    end;
    end;
  end;
end;

procedure TBSONWriter.BSONWriteDoc(const doc: TDocVariantData);
var Name: RawUTF8;
    i: integer;
    Start: Cardinal;
begin
  Start := BSONDocumentBegin;
  if TVarData(doc).VType>varNull then // null,empty will write {}
    if TVarData(doc).VType<>DocVariantType.VarType then
      raise EBSONException.CreateFmt('doc.VType=%d',[TVarData(doc).VType]) else
    for i := 0 to doc.Count-1 do begin
      if doc.Names<>nil then
        Name := doc.Names[i] else
        UInt32ToUtf8(i,Name);
      BSONWriteVariant(Name,doc.Values[i]);
      if TotalWritten>BSON_MAXDOCUMENTSIZE then
        raise EBSONException.CreateFmt('BSON document size = %d > maximum %d',
          [TotalWritten,BSON_MAXDOCUMENTSIZE]);
    end;
  BSONDocumentEnd(Start);
end;

procedure TBSONWriter.BSONWriteObject(const NameValuePairs: array of const);
var Start: cardinal;
    Name: RawUTF8;
    i: integer;
begin
  Start := BSONDocumentBegin;
  for i := 0 to (length(NameValuePairs)shr 1)-1 do begin
    VarRecToUTF8(NameValuePairs[i*2],Name);
    BSONWrite(Name,NameValuePairs[i*2+1]);
  end;
  BSONDocumentEnd(Start);
end;

procedure TBSONWriter.BSONWriteArray(const Items: array of const);
var Start: cardinal;
    i: integer;
begin
  Start := BSONDocumentBegin;
  for i := 0 to high(Items) do
    BSONWrite(UInt32ToUtf8(i),Items[i]);
  BSONDocumentEnd(Start);
end;

procedure TBSONWriter.BSONWriteArrayOfInteger(const Integers: array of integer);
var Start: cardinal;
    i: integer;
begin
  Start := BSONDocumentBegin;
  for i := 0 to high(Integers) do
    BSONWrite(UInt32ToUtf8(i),Integers[i]);
  BSONDocumentEnd(Start);
end;

procedure TBSONWriter.BSONWriteFromJSON(const name: RawUTF8; var JSON: PUTF8Char;
  EndOfObject: PUTF8Char);
var tmp: variant; // we use a local variant for only BSONVariant values
    wasString: boolean;
    err: integer;
    Value, Dot: PUTF8Char;
    VDouble: double;
    Kind: TBSONElementType;
begin
  if JSON^ in [#1..' '] then repeat inc(JSON) until not(JSON^ in [#1..' ']);
  if BSONVariantType.TryJSONToVariant(JSON,tmp,EndOfObject) then
    // was betDateTime, betObjectID or betRegEx, from strict or extended JSON
    BSONWriteVariant(name,tmp) else
    // try from simple types
    case JSON^ of
    '[': begin // nested array
      BSONWrite(name,betArray);
      JSON := BSONWriteDocFromJSON(JSON,EndOfObject,Kind);
    end;
    '{': begin // nested document
      BSONWrite(name,betDoc);
      JSON := BSONWriteDocFromJSON(JSON,EndOfObject,Kind);
    end;
    else begin // simple types
      Value := GetJSONField(JSON,JSON,@wasString,EndOfObject);
      if JSON=nil then
        JSON := @NULCHAR;
      if Value=nil then begin
        BSONWrite(name,betNull);
        exit;
      end;
      if not wasString then begin // try if not a number
        if PInteger(Value)^=NULL_LOW then begin
          BSONWrite(name,betNull);
          exit;
        end else
        if PInteger(Value)^=FALSE_LOW then begin
          BSONWrite(name,false);
          exit;
        end else
        if PInteger(Value)^=TRUE_LOW then begin
          BSONWrite(name,true);
          exit;
        end;
        Dot := Value;
        repeat
          case Dot^ of
          '0'..'9','+','-':
            inc(Dot);
          #0: begin // integer number
            BSONWrite(name,GetInt64(Value));
            exit;
          end;
          else break;
          end;
        until false;
        VDouble := GetExtended(Value,err);
        if err=0 then begin // floating-point number
          BSONWrite(name,VDouble);
          exit;
        end;
      end;
      // found no numerical value -> will point to the in-place escaped text
      BSONWrite(name,Value);
    end;
  end;
  if TotalWritten>BSON_MAXDOCUMENTSIZE then
    raise EBSONException.CreateFmt('BSON document size = %d > maximum %d',
      [TotalWritten,BSON_MAXDOCUMENTSIZE]);
end;

function TBSONWriter.BSONWriteDocFromJSON(JSON: PUTF8Char; aEndOfObject: PUTF8Char;
  out Kind: TBSONElementType): PUTF8Char;
var Start, ndx: cardinal;
    EndOfObject: AnsiChar;
    Name: RawUTF8;
begin
  result := nil;
  if JSON=nil then
    exit;
  if JSON^ in [#1..' '] then repeat inc(JSON) until not(JSON^ in [#1..' ']);
  case JSON^ of
  '[': begin
    Kind := betArray;
    Start := BSONDocumentBegin;
    repeat inc(JSON) until not(JSON^ in [#1..' ']);
    ndx := 0;
    repeat
      UInt32ToUtf8(ndx,Name);
      BSONWriteFromJSON(Name,JSON,@EndOfObject);
      if JSON=nil then
        exit; // invalid content
      inc(ndx);
    until EndOfObject=']';
  end;
  '{': begin
    Kind := betDoc;
    Start := BSONDocumentBegin;
    repeat inc(JSON) until not(JSON^ in [#1..' ']);
    repeat
      // see http://docs.mongodb.org/manual/reference/mongodb-extended-json
      Name := GetJSONPropName(JSON);
      if Name='' then
        exit;
      BSONWriteFromJSON(Name,JSON,@EndOfObject);
      if JSON=nil then
        exit; // invalid content
    until EndOfObject='}';
  end;
  'n','N': 
    if IdemPChar(JSON+1,'ULL') then begin
      Kind := betNull;
      Start := BSONDocumentBegin;
      inc(JSON,4);
    end else
      exit;
  else exit;
  end;
  BSONDocumentEnd(Start);
  if JSON^ in [#1..' '] then repeat inc(JSON) until not(JSON^ in [#1..' ']);
  if aEndOfObject<>nil then
    aEndOfObject^ := JSON^;
  if JSON^<>#0 then
    repeat inc(JSON) until not(JSON^ in [#1..' ']);
  result := JSON; // indicates successfully parsed
end;


{ TBSONObjectID }

var
  /// first 12 bytes map TBSONObjectID
  GlobalBSONObjectID: packed record
    // bswap32-encoded
    UnixCreateTime: cardinal;
    // from COMPUTERNAME
    MachineID: array[0..2] of byte;
    // we use the Thread ID, so that ComputeNew will be thread-safe
    ProcessID: word;
    // bswap24-encoded
    Counter: integer;
    // naive but very efficient UnixCreateTime cache
    LastTick: cardinal;
    // this will handle any potential collision (24 bit Counter overflow)
    FirstCounter: integer;
    LatestCounterOverflowUnixCreateTime: cardinal;
    CollisionCount: integer;
  end;

function bswap24(a: cardinal): cardinal; {$ifdef HASINLINE}inline;{$endif}
begin
  result := ((a and $ff)shl 16) or ((a and $ff0000)shr 16) or (a and $ff00);
end;

procedure TBSONObjectID.ComputeNew;
  function ComputeMachineID: Cardinal;
  var tmp: array[byte] of AnsiChar;
  begin
    result := GetEnvironmentVariableA('COMPUTERNAME',tmp,sizeof(tmp));
    if result<1 then
      result := GetCurrentProcessId else
      result := kr32(0,@tmp,result);
  end;
var Tick, CurrentTime: cardinal;
begin // this is a bit complex, but we have to avoid any collision
  with GlobalBSONObjectID do begin
    Tick := GetTickCount shr 8;
    if LastTick<>Tick then begin
      LastTick := Tick; // huge speed improvement when caching time
      CurrentTime := bswap32(DateTimeToUnixTime(NowUTC));
      if CurrentTime<>UnixCreateTime then begin
        UnixCreateTime := CurrentTime;
        LatestCounterOverflowUnixCreateTime := UnixCreateTime;
        if CollisionCount>0 then begin
          ProcessID := GetCurrentThreadId;
          CollisionCount := 0;
        end;
      end;
    end;
    if ProcessID=0 then begin
      PCardinal(@MachineID)^ := ComputeMachineID;
      ProcessID := GetCurrentThreadId;
      FirstCounter := (cardinal(Random($ffffff))*GetTickCount) and $ffffff;
      Counter := FirstCounter;
      LatestCounterOverflowUnixCreateTime := UnixCreateTime;
    end else begin
      Counter := bswap24(Counter)+1;
      if Counter and $ffffff=FirstCounter then begin
        Counter := FirstCounter;
        if UnixCreateTime=LatestCounterOverflowUnixCreateTime then begin
          inc(ProcessID); // force no collision
          inc(CollisionCount);
          if CollisionCount>=high(word) then
            raise EBSONException.Create('ObjectID collision');
        end;
        LatestCounterOverflowUnixCreateTime := UnixCreateTime;
      end;
    end;
    Counter := bswap24(Counter);
  end;
  self := PBSONObjectID(@GlobalBSONObjectID)^;
end;

function TBSONObjectID.Equal(const Another: TBSONObjectID): boolean;
begin
  result := (PIntegerArray(@Self)[2]=PIntegerArray(@Another)[2]) and
    (PIntegerArray(@Self)[1]=PIntegerArray(@Another)[1]) and
    (PIntegerArray(@Self)[0]=PIntegerArray(@Another)[0]);
end;

function TBSONObjectID.CreateDateTime: TDateTime;
begin
  result := UnixTimeToDateTime(bswap32(UnixCreateTime));
end;

function TBSONObjectID.ToText: RawUTF8;
begin
  ToText(result);
end;

function TBSONObjectID.ToVariant: variant;
begin
  if not(TVarData(result).VType in VTYPE_STATIC) then
    VarClear(result);
  with TBSONVariantData(result) do begin
    VType := BSONVariantType.VarType;
    VKind := betObjectID;
    VObjectID := self;
  end;
end;

function TBSONObjectID.FromText(const Text: RawUTF8): boolean;
begin
  if length(Text)=SizeOf(self)*2 then
    result := HexToBin(Pointer(Text),@self,SizeOf(self)) else
    result := false;
end;

function TBSONObjectID.FromText(Text: PUTF8Char): boolean;
begin
  result := HexToBin(Pointer(Text),@self,SizeOf(self));
end;

function TBSONObjectID.FromVariant(const value: variant): boolean;
var txt: RawUTF8;
    wasString: boolean;
begin
  if TVarData(value).VType=varByRef or varVariant then
    result := FromVariant(PVariant(TVarData(value).VPointer)^) else
  if (TBSONVariantData(value).VType=BSONVariantType.VarType) and
     (TBSONVariantData(value).VKind=betObjectID) then begin
    self := TBSONVariantData(value).VObjectID;
    result:= true;
  end else begin
    VariantToUTF8(value,txt,wasString);
    result := wasString and FromText(txt);
  end;
end;

procedure TBSONObjectID.ToText(var result: RawUTF8);
begin
  SetString(result,nil,sizeof(self)*2);
  SynCommons.BinToHex(@self,pointer(result),sizeof(self));
end;


{ TBSONVariant }

procedure TBSONVariant.ToJSON(W: TTextWriter; const Value: variant; Escape: TTextWriterKind);
var item: TBSONElement;
    temp: RawByteString;
begin
  item.FromVariant('',Value,temp);
  item.AddMongoJSON(W,modMongoStrict);
end;

function TBSONVariant.GetNewDoc(const BSONDoc: TBSONDocument): variant;
begin
  FromBSONDocument(BSONDoc,result);
end;

function TBSONVariant.IsOfKind(const V: variant;
  Kind: TBSONElementType): boolean;
begin
  with TBSONVariantData(V) do
    if VType=varByRef or varVariant then
      result := IsOfKind(PVariant(TVarData(V).VPointer)^,Kind) else
      result := (self<>nil) and (VType=VarType) and (VKind=Kind);
end;

function TBSONVariant.ToBlob(const V: Variant; var Blob: RawByteString): boolean;
begin
  with TVarData(V) do
    if VType=varByRef or varVariant then begin
      result := ToBlob(PVariant(VPointer)^,Blob);
      exit;
     end;
  with TBSONVariantData(V) do begin
    result := (VType=VarType) and (VKind=betBinary);
    if result then
      if (VBlob=nil) or
         (PInteger(VBlob)^<>Length(RawByteString(VBlob))-(sizeof(integer)+1)) then
        Blob := '' else
        SetString(Blob,PAnsiChar(VBlob)+(sizeof(integer)+1),PInteger(VBlob)^);
  end;
end;

procedure TBSONVariant.FromBinary(const Bin: RawByteString;
  BinType: TBSONElementBinaryType; var result: variant);
var Len: integer;
begin // "\x05" e_name int32 subtype (byte*)
  with TBSONVariantData(result) do begin
    if not(VType in VTYPE_STATIC) then
      VarClear(result);
    if Bin='' then begin
      VType := varNull; // stores a NULL 
      exit;
    end;
    VType := VarType;
    VKind := betBinary;
    VBlob := nil; // avoid GPF here below
    Len := length(Bin);
    SetLength(RawByteString(VBlob),Len+(sizeof(integer)+1));
    PInteger(VBlob)^ := Len;
    PByteArray(VBlob)^[sizeof(integer)] := ord(BinType);
    move(pointer(Bin)^,PByteArray(VBlob)^[sizeof(integer)+1],Len);
  end;
end;

procedure TBSONVariant.FromBSONDocument(const BSONDoc: TBSONDocument;
  var result: variant; Kind: TBSONElementType);
begin
  with TBSONVariantData(result) do begin
    if not(VType in VTYPE_STATIC) then
      VarClear(result);
    VType := VarType;
    VKind := Kind;
    VBlob := nil; // avoid GPF here below
    RawByteString(VBlob) := BSONDoc;
  end;
end;

procedure TBSONVariant.FromJSON(json: PUTF8Char; var result: variant);
begin
  with TBSONVariantData(result) do begin
    if not(VType in VTYPE_STATIC) then
      VarClear(result);
    VType := VarType;
    VBlob := nil; // avoid GPF here below
    VKind := JSONBufferToBSONDocument(json,RawByteString(VBlob));
  end;
end;

function TBSONVariant.TryJSONToVariant(var JSON: PUTF8Char;
  var Value: variant; EndOfObject: PUTF8Char): boolean;
// warning: code should NOT modify JSON buffer in-place, unless it returns true
  procedure Return(kind: TBSONElementType; P: PUTF8Char; GotoEndOfObject: AnsiChar='}');
  begin
    if GotoEndOfObject<>#0 then
      while P^<>GotoEndOfObject do if P^=#0 then exit else inc(P);
    P := GotoNextNotSpace(P+1);
    if EndOfObject<>nil then
      EndOfObject^ := P^;
    if P^<>#0 then
      JSON := P+1 else
      JSON := P;
    case kind of
    betObjectID, betRegEx: begin // should handle it in TBSONWriter.BSONWrite()
      TVarData(Value).VType := VarType;
      TBSONVariantData(Value).VKind := kind;
    end; 
    betDateTime:
      TVarData(Value).VType := varDate;
    end;
    result := true;
  end;
  procedure TryDate(P: PUTF8Char; GotoEndOfObject: AnsiChar);
  var L: integer;
  begin
    P := GotoNextNotSpace(P);
    if GotoEndOfObject=')' then
      if (P^=')') then begin // new date() constructor
        TVarData(Value).VDate := NowUTC;
        Return(betDateTime,P,#0);
        exit;
      end else
      if P^ in ['0'..'9'] then begin
        TVarData(Value).VDate := GetNextItemDouble(P,')');
        if (TVarData(Value).VDate<>0) and (P<>nil) then begin
          Return(betDateTime,P-1,#0);
          exit;
        end;
      end;
    if P^<>'"' then exit;
    if PCardinal(P)^=JSON_SQLDATE_MAGIC_QUOTE then
      inc(P,3); // ignore\uFFF1 code for DateTimeToSQL/TimeLogToSQL functions
    L := 1; while P[L]<>'"' do if P[L]<=' ' then exit else inc(L);
    Iso8601ToDateTimePUTF8CharVar(P+1,L,TVarData(Value).VDate);
    if TVarData(Value).VDate<>0 then
      Return(betDateTime,P+L+1,GotoEndOfObject);
  end;
  procedure TryObjectID(P: PUTF8Char; GotoEndOfObject: AnsiChar);
  begin
    P := GotoNextNotSpace(P);
    if (GotoEndOfObject=')') and (P^=')') then begin // ObjectId() constructor
      TBSONVariantData(Value).VObjectID.ComputeNew;
      Return(betObjectID,P,#0);
      exit;
    end;
    if P^<>'"' then exit;
    if TBSONVariantData(Value).VObjectID.FromText(P+1) then
      Return(betObjectID,P+25,GotoEndOfObject);
  end;
  var Reg,Opt: PUTF8Char;
      RegLen,OptLen: Integer;
  procedure ReturnRegEx(P: PUTF8Char; GotoEndOfObject: AnsiChar);
  var buf: PAnsiChar;
  begin
    TBSONVariantData(Value).VBlob := nil; // avoid GPF
    SetString(RawByteString(TBSONVariantData(Value).VBlob),nil,RegLen+OptLen+2);
    buf := TBSONVariantData(Value).VBlob;
    move(Reg^,buf^,RegLen); inc(buf,RegLen); buf^ := #0; inc(buf);
    move(Opt^,buf^,OptLen); inc(buf,OptLen); buf^ := #0;
    Return(betRegEx,P,GotoEndOfObject);
  end;
  procedure TryRegExShell(P: PUTF8Char);
  begin
    RegLen := 0; while P[RegLen]<>'/' do if P[RegLen]<=' ' then exit else inc(RegLen);
    Reg := P;
    inc(P,RegLen);
    if P^<>'/' then exit else inc(P);
    OptLen := 0; while ord(P[OptLen]) in IsWord do inc(OptLen);
    if P[OptLen]=#0 then exit; 
    Opt := P;
    ReturnRegEx(Opt+OptLen-1,#0);
  end;
  procedure TryRegExStrict(P: PUTF8Char);
  begin // warning: this won't escape double quotes...
    P := GotoNextNotSpace(P);
    if P^<>'"' then exit else inc(P);
    RegLen := 0; while P[RegLen]<>'"' do if P[RegLen]<=' ' then exit else inc(RegLen);
    Reg := P;
    P := GotoNextNotSpace(Reg+RegLen+1);
    if P^<>',' then Exit; // $regex:"acme*.corp",$options:"i"}
    P := GotoNextNotSpace(P+1);
    if P^='"' then inc(P);
    if not CompareMem(P,@BSON_JSON_REGEX[1][4],8) then exit else inc(P,8);
    if P^='"' then inc(P);
    P := GotoNextNotSpace(P);
    if P^<>':' then exit;
    P := GotoNextNotSpace(P+1);
    if P^<>'"' then exit else inc(P);
    OptLen := 0; while P[OptLen]<>'"' do if P[OptLen]<=' ' then exit else inc(OptLen);
    Opt := P;
    ReturnRegEx(Opt+OptLen+1,'}');
  end;
var P: PUTF8Char;
begin // here JSON does not start with " or 1..9 (obvious simple types)
  // see http://docs.mongodb.org/manual/reference/mongodb-extended-json
  result := false;
  case NormToUpperAnsi7[JSON^] of
  '{': begin // strict MongoDB objects e.g. {"$undefined":true} or {"$oid":".."}
    P := GotoNextNotSpace(JSON+1);
    if P^<>'"' then exit;
    P := GotoNextNotSpace(P+1);
    if P[0]='$' then
    case P[1] of
    'u': if CompareMem(P+2,@BSON_JSON_UNDEFINED[false][5],10) then
           Return(betDeprecatedUndefined,P+12);
    'm': if CompareMem(P+2,@BSON_JSON_MINKEY[false][5],7) then
           Return(betMinKey,P+9) else
         if CompareMem(P+2,@BSON_JSON_MAXKEY[false][5],7) then
           Return(betMaxKey,P+9);
    'o': if CompareMem(P+2,@BSON_JSON_OBJECTID[false,modMongoStrict][5],4) then
           TryObjectID(P+6,'}');
    'd': if CompareMem(P+2,@BSON_JSON_DATE[modMongoStrict,false][5],5) then
           TryDate(P+7,'}');
    'r': if CompareMem(P+2,@BSON_JSON_REGEX[0][5],6) then
           TryRegExStrict(P+8);
    end;
  end;
  // MongoDB Shell Mode extended syntax
  'U': if StrCompIL(JSON+1,@BSON_JSON_UNDEFINED[true][2],8)=0 then
         Return(betDeprecatedUndefined,JSON+8,#0);
  'M': if StrCompIL(JSON+1,@BSON_JSON_MINKEY[true][2],5)=0 then
         Return(betMinKey,JSON+5,#0) else
       if StrCompIL(JSON+1,@BSON_JSON_MAXKEY[true][2],7)=0 then
         Return(betMaxKey,JSON+5,#0);
  'O': if StrCompIL(JSON+1,@BSON_JSON_OBJECTID[false,modMongoShell][2],8)=0 then
         TryObjectID(JSON+9,')');
  'N': if StrCompIL(JSON+1,'ew Date(',8)=0 then
          TryDate(JSON+9,')');
  'I': if StrCompIL(JSON+1,@BSON_JSON_DATE[modMongoShell,false][2],7)=0 then
          TryDate(JSON+8,')');
  '/': TryRegExShell(JSON+1);
  end;
end;

procedure TBSONVariant.Cast(var Dest: TVarData; const Source: TVarData);
begin
  CastTo(Dest,Source,VarType);
end;

procedure TBSONVariant.CastTo(var Dest: TVarData;
  const Source: TVarData; const AVarType: TVarType);
var Tmp: RawUTF8;
    wasString: boolean;
begin
  if AVarType=VarType then begin
    VariantToUTF8(Variant(Source),Tmp,wasString);
    if wasString then begin
      if not(Dest.VType in VTYPE_STATIC) then
        VarClear(variant(Dest));
      if TBSONVariantData(Dest).VObjectID.FromText(Tmp) then begin
        Dest.VType := VarType;
        TBSONVariantData(Dest).VKind := betObjectID;
        exit;
      end;
      variant(Dest) := BSONVariant(Tmp); // convert from JSON text
      exit;
    end;
    RaiseCastError;
  end else begin
    if Source.VType<>VarType then
      RaiseCastError;
    with TBSONVariantData(Source) do
      if (VKind=betObjectID) and (AVarType in [varDate,varDouble]) then begin
        Dest.VType := AVarType;
        Dest.VDate := VObjectID.CreateDateTime;
        exit;
      end else begin
        if VKind=betObjectID then
          VObjectID.ToText(Tmp) else
          Tmp := VariantSaveMongoJSON(variant(Source),modMongoShell);
        RawUTF8ToVariant(Tmp,Dest,AVarType); // convert to JSON text
      end;
  end;
end;

procedure TBSONVariant.Clear(var V: TVarData);
begin
  if TBSONVariantData(V).VKind in BSON_ELEMENTVARIANTMANAGED then
    RawByteString(TBSONVariantData(V).VBlob) := '';
  ZeroFill(V); // will set V.VType := varEmpty
end;

procedure TBSONVariant.Copy(var Dest: TVarData;
  const Source: TVarData; const Indirect: Boolean);
begin
  if Indirect then
    SimplisticCopy(Dest,Source,true) else begin
    if not(Dest.VType in VTYPE_STATIC) then
      VarClear(variant(Dest)); // Dest may be a complex type
    Dest := Source;
    with TBSONVariantData(Dest) do
    if VKind in BSON_ELEMENTVARIANTMANAGED then begin
      VBlob := nil; // avoid GPF
      RawByteString(VBlob) := RawByteString(TBSONVariantData(Source).VBlob);
    end;
  end;
end;

procedure TBSONVariant.Compare(const Left, Right: TVarData;
  var Relationship: TVarCompareResult);
var res: integer;
    LeftU,RightU: RawUTF8;
begin
  LeftU := VariantSaveMongoJSON(variant(Left),modMongoStrict);
  RightU := VariantSaveMongoJSON(variant(Right),modMongoStrict);
  if LeftU=RightU then
    Relationship := crEqual else begin
    res := StrComp(pointer(LeftU),pointer(RightU));
    if res<0 then
      Relationship := crLessThan else
    if res>0 then
      Relationship := crGreaterThan else
      Relationship := crEqual;
  end;
end;


{ main BSON* functions }

function ObjectID: variant;
var ID: TBSONObjectID;
begin
  ID.ComputeNew;
  result := ID.ToVariant;
end;

function ObjectID(const Hexa: RaWUTF8): variant;
var ID: TBSONObjectID;
begin
  if ID.FromText(Hexa) then
    result := ID.ToVariant else
    raise EBSONException.CreateFmt('Invalid ObjectID hexadecimal "%s"',[Hexa]);
end;

function BSONObjectID(const aObjectID: variant): TBSONObjectID;
begin
  if not result.FromVariant(aObjectID) then
    raise EBSONException.Create('BSONObjectID() over not ObjectID variant');
end;

function JavaScript(const JS: RawUTF8): variant;
begin
  with TBSONVariantData(result) do begin
    if not(VType in VTYPE_STATIC) then
      VarClear(result);
    VType := BSONVariantType.VarType;
    VKind := betJS;
    VText := nil; // avoid GPF
    RawUTF8(VText) := JS;
  end;
end;

function JavaScript(const JS: RawUTF8; const Scope: TBSONDocument): variant;
var Len, JSLen: integer;
begin
  with TBSONVariantData(result) do begin
    if not(VType in VTYPE_STATIC) then
      VarClear(result);
    VType := BSONVariantType.VarType;
    VKind := betJSScope;
    JSLen := Length(JS)+1;                        // string = int32 text#0
    Len := SizeOf(integer)*2+JSLen+length(Scope); // int32 string document
    VBlob := nil; // avoid GPF
    SetLength(RawByteString(VBlob),Len);
    PIntegerArray(VBlob)^[0] := Len;              // length:int32
    PIntegerArray(VBlob)^[1] := JSLen;            // string:int32
    Move(pointer(JS)^,PAnsiChar(VBlob)[8],JSLen); // string:text#0
    Move(pointer(Scope)^,PAnsiChar(VBlob)[8+JSLen],Length(Scope)); // document
  end;
end;

function BSON(const doc: TDocVariantData): TBSONDocument;
begin
  if TVarData(doc).VType=varVariant or varByRef then begin
    result := BSON(PDocVariantData(TVarData(doc).VPointer)^);
    exit;
  end;
  if TVarData(doc).VType<>DocVariantType.VarType then
    raise EBSONException.Create('doc is not a TDocVariant');
  with TBSONWriter.Create(TRawByteStringStream) do
  try
    BSONWriteDoc(doc);
    ToBSONDocument(result);
  finally
    Free;
  end;
end;

function BSONFromIntegers(const Integers: array of integer): TBSONDocument;
begin
  with TBSONWriter.Create(TRawByteStringStream) do
  try
    BSONWriteArrayOfInteger(Integers);
    ToBSONDocument(result);
  finally
    Free;
  end;
end;

function BSON(const NameValuePairs: array of const): TBSONDocument;
var W: TBSONWriter;
    name: RawUTF8;
    a: Integer;
    Start: cardinal;
procedure WriteValue;
var StartNested, ndx: cardinal;
begin
  case VarRecAsChar(NameValuePairs[a]) of
  ord('['): begin
    W.BSONWrite(name,betArray);
    StartNested := W.BSONDocumentBegin;
    ndx := 0;
    repeat
      inc(a);
      if VarRecAsChar(NameValuePairs[a])=ord(']') then
        break;
      UInt32ToUtf8(ndx,name);
      WriteValue;
      inc(ndx);
    until a=high(NameValuePairs);
    W.BSONDocumentEnd(StartNested);
  end;
  ord('{'): begin
    W.BSONWrite(name,betDoc);
    StartNested := W.BSONDocumentBegin;
    repeat
      inc(a);
      VarRecToUTF8(NameValuePairs[a],name);
      if (a=high(NameValuePairs)) or (name='}') then
        break;
      inc(a);
      WriteValue;
    until a=high(NameValuePairs);
    W.BSONDocumentEnd(StartNested);
  end else
    W.BSONWrite(name,NameValuePairs[a]);
  end;
end;
begin
  W := TBSONWriter.Create(TRawByteStringStream);
  try
    Start := W.BSONDocumentBegin;
    a := 0;
    while a<high(NameValuePairs) do begin
      VarRecToUTF8(NameValuePairs[a],name);
      inc(a);
      WriteValue;
      inc(a);
    end;
    W.BSONDocumentEnd(Start);
    W.ToBSONDocument(result);
  finally
    W.Free;
  end;
end;

function BSONFieldSelector(const FieldNames: array of RawUTF8): TBSONDocument;
var i: integer;
    Start: cardinal;
    W: TBSONWriter;
begin
  W := TBSONWriter.Create(TRawByteStringStream,512);
  try
    Start := W.BSONDocumentBegin;
    for i := 0 to high(FieldNames) do
      W.BSONWrite(FieldNames[i],1);
    W.BSONDocumentEnd(Start);
    W.ToBSONDocument(result);
  finally
    W.Free;
  end;
end;

function BSONFieldSelector(const FieldNamesCSV: RawUTF8): TBSONDocument; 
var FieldNames: TRawUTF8DynArray;
begin
  CSVToRawUTF8DynArray(pointer(FieldNamesCSV),FieldNames);
  result := BSONFieldSelector(FieldNames);
end;

function JSONBufferToBSONDocument(JSON: PUTF8Char; var doc: TBSONDocument): TBSONElementType;
var W: TBSONWriter;
begin
  W := TBSONWriter.Create(TRawByteStringStream);
  try
    W.BSONWriteDocFromJSON(JSON,nil,result);
    W.ToBSONDocument(doc);
  finally
    W.Free;
  end;
end;

function BSON(Format: PUTF8Char; const Args,Params: array of const): TBSONDocument;
var JSON: RawUTF8;
    v: variant;
begin
  if (Format<>nil) and (PWord(Format)^=ord('?')) and (high(Params)>=0) then begin
    VarRecToVariant(Params[0],v);
    if DocVariantType.IsOfType(v) then begin
      result := BSON(TDocVariantData(v));
      exit;
    end;
  end;
  JSON := FormatUTF8(Format,Args,Params,true);
  JSONBufferToBSONDocument(pointer(JSON),result);
end;

function BSON(const JSON: RawUTF8): TBSONDocument;
var tmp: RawUTF8; // make a private copy
begin
  SetString(tmp,PAnsiChar(pointer(JSON)),length(JSON));
  JSONBufferToBSONDocument(pointer(tmp),result);
end;

function BSONVariant(const NameValuePairs: array of const): variant;
begin
  BSONVariantType.FromBSONDocument(BSON(NameValuePairs),result);
end;

function BSONVariant(const JSON: RawUTF8): variant;
begin
  BSONVariantType.FromBSONDocument(BSON(JSON),result);
end;

procedure BSONVariant(JSON: PUTF8Char; var result: variant);
var tmp: TBSONDocument;
begin
  JSONBufferToBSONDocument(JSON,tmp);
  BSONVariantType.FromBSONDocument(tmp,result);
end;

function BSONVariant(Format: PUTF8Char; const Args,Params: array of const): variant; overload;
begin
  BSONVariantType.FromBSONDocument(BSON(Format,Args,Params),result);
end;

function BSONVariant(doc: TDocVariantData): variant; overload;
begin
  BSONVariantType.FromBSONDocument(BSON(Doc),result);
end;

function BSONVariantFieldSelector(const FieldNames: array of RawUTF8): variant;
begin
  BSONVariantType.FromBSONDocument(BSONFieldSelector(FieldNames),result);
end;

function BSONVariantFieldSelector(const FieldNamesCSV: RawUTF8): variant;
begin
  BSONVariantType.FromBSONDocument(BSONFieldSelector(FieldNamesCSV),result);
end;

function BSONVariantFromIntegers(const Integers: array of integer): variant;
begin
  BSONVariantType.FromBSONDocument(BSONFromIntegers(Integers),result,betArray);
end;


{ ************ MongoDB client }

{ TMongoRequest }

const
  WIRE_OPCODES: array[TMongoOperation] of integer = (
   1, 1000, 2001, 2002, 2004, 2005, 2006, 2007);
  CLIENT_OPCODES = [opUpdate,opInsert,opQuery,opGetMore,opDelete,opKillCursors];

var
  GlobalRequestID: Integer;

constructor TMongoRequest.Create(const FullCollectionName: RawUTF8;
  opCode: TMongoOperation; requestID, responseTo: Integer);
begin
  if not (opCode in CLIENT_OPCODES) then
    raise EMongoException.CreateFmt('Unexpected %s.Create(opCode=%s)',
      [ClassName,GetEnumName(TypeInfo(TMongoOperation),ord(opCode))^]);
  inherited Create(TRawByteStringStream);
  fFullCollectionName := FullCollectionName;
  Split(fFullCollectionName,'.',fDatabaseName,fCollectionName);
  if requestID=0 then
    fRequestID := InterlockedIncrement(GlobalRequestID) else
    fRequestID := requestID;
  fResponseTo := responseTo;
  fRequestHeaderStart := BSONDocumentBegin;
  fRequestOpCode := opCode;
  Write4(fRequestID);
  Write4(fResponseTo);
  Write4(WIRE_OPCODES[opCode]);
end;

procedure TMongoRequest.BSONWriteParam(const paramDoc: variant);
begin
  if TVarData(paramDoc).VType=varByRef or varVariant then
    BSONWriteParam(PVariant(TVarData(paramDoc).VPointer)^) else
  if VarIsStr(paramDoc) then
    BSONWriteObject([paramDoc,1]) else
  if (TVarData(paramDoc).VType=BSONVariantType.VarType) and
     (TBSONVariantData(paramDoc).VKind=betDoc) and
     (TBSONVariantData(paramDoc).VBlob<>nil) then
    WriteBinary(RawByteString(TBSONVariantData(paramDoc).VBlob)) else
    BSONWriteDoc(TDocVariantData(paramDoc)); // for TDocVariant or null
end;

procedure TMongoRequest.ToBSONDocument(var result: TBSONDocument);
begin
  if (fRequestID=0) or (fRequestOpCode=opReply) then
    raise EMongoException.CreateFmt('No previous proper %s.Create() call',[ClassName]);
  if fBSONDocument='' then begin
    BSONDocumentEnd(fRequestHeaderStart,false);
    inherited ToBSONDocument(fBSONDocument);
  end;
  result := fBSONDocument;
end;

procedure TMongoRequest.ToJSON(W: TTextWriter; Mode: TMongoJSONMode);
begin
  if self=nil then begin
    W.AddShort('null');
    exit;
  end;
  W.Add('{');
  W.AddShort('"collection":');
  W.AddJSONEscape(pointer(fFullCollectionName));
  W.AddShort(',"opCode":');
  W.AddTypedJSON(TypeInfo(TMongoOperation),fRequestOpCode);
  W.AddShort(',"requestID":');
  W.AddU(fRequestID);
  if fResponseTo<>0 then begin
    W.AddShort(',"responseTo":');
    W.AddU(fResponseTo);
  end;
  W.Add('}');
end;

function TMongoRequest.ToJSON(Mode: TMongoJSONMode): RawUTF8;
var W: TTextWriter;
begin
  W := TTextWriter.CreateOwnedStream;
  try
    ToJSON(W,Mode);
    W.SetText(result);
  finally
    W.Free;
  end;
end;


{ TMongoRequestUpdate }

constructor TMongoRequestUpdate.Create(const FullCollectionName: RawUTF8;
  const Selector, Update: variant; Flags: TMongoUpdateFlags);
begin
  inherited Create(FullCollectionName,opUpdate,0,0);
  WriteCollectionName(0,FullCollectionName);
  Write4(byte(Flags));
  BSONWriteParam(Selector);
  BSONWriteParam(Update);
  fSelector := TVarData(Selector);
  fUpdate := TVarData(Update);
end;

procedure TMongoRequestUpdate.ToJSON(W: TTextWriter; Mode: TMongoJSONMode);
begin
  inherited;
  if W.LastChar='}' then
    W.CancelLastChar;
  W.AddShort(',"selector":');
  W.AddVariantJSON(variant(fSelector));
  W.AddShort(',"update":');
  W.AddVariantJSON(variant(fUpdate));
  W.Add('}');
end;

{ TMongoRequestInsert }

constructor TMongoRequestInsert.Create(const FullCollectionName: RawUTF8;
  const Documents: array of variant; Flags: TMongoInsertFlags=[]);
var i: integer;
begin
  inherited Create(FullCollectionName,opInsert,0,0);
  WriteCollectionName(byte(Flags),FullCollectionName);
  for i := 0 to high(Documents) do
    BSONWriteParam(Documents[i]);
end;

constructor TMongoRequestInsert.Create(const FullCollectionName: RawUTF8;
  const Documents: TBSONDocument; Flags: TMongoInsertFlags=[]);
begin
  inherited Create(FullCollectionName,opInsert,0,0);
  WriteCollectionName(byte(Flags),FullCollectionName);
  Write(pointer(Documents),Length(Documents));
end;

constructor TMongoRequestInsert.Create(const FullCollectionName: RawUTF8;
  const JSONDocuments: array of PUTF8Char; Flags: TMongoInsertFlags=[]);
var i: integer;
    kind: TBSONElementType;
begin
  inherited Create(FullCollectionName,opInsert,0,0);
  WriteCollectionName(byte(Flags),FullCollectionName);
  for i := 0 to high(JSONDocuments) do
    BSONWriteDocFromJSON(JSONDocuments[i],nil,kind);
end;

{ TMongoRequestDelete }

constructor TMongoRequestDelete.Create(const FullCollectionName: RawUTF8;
  const Selector: variant; Flags: TMongoDeleteFlags=[]);
begin
  inherited Create(FullCollectionName,opDelete,0,0);
  WriteCollectionName(byte(Flags),FullCollectionName);
  Write4(byte(Flags));
  BSONWriteParam(Selector);
  fQuery := TVarData(Selector);
end;

procedure TMongoRequestDelete.ToJSON(W: TTextWriter; Mode: TMongoJSONMode);
begin
  inherited;
  if W.LastChar='}' then
    W.CancelLastChar;
  W.AddShort(',"query":');
  W.AddVariantJSON(variant(fQuery));
  W.Add('}');
end;

{ TMongoRequestQuery }

constructor TMongoRequestQuery.Create(const FullCollectionName: RawUTF8;
  const Query, ReturnFieldsSelector: variant; NumberToReturn: integer;
  NumberToSkip: Integer=0; Flags: TMongoQueryFlags=[]);
begin
  inherited Create(FullCollectionName,opQuery,0,0);
  fNumberToReturn := NumberToReturn;
  WriteCollectionName(byte(Flags),FullCollectionName);
  Write4(NumberToSkip);
  Write4(NumberToReturn);
  BSONWriteParam(Query);
  if TVarData(ReturnFieldsSelector).VType>varNull then
    BSONWriteParam(ReturnFieldsSelector);
  fQuery := TVarData(Query);
  fReturnFieldsSelector := TVarData(ReturnFieldsSelector);
end;

procedure TMongoRequestQuery.ToJSON(W: TTextWriter; Mode: TMongoJSONMode);
begin
  inherited;
  if W.LastChar='}' then
    W.CancelLastChar;
  W.AddShort(',"query":');
  W.AddVariantJSON(variant(fQuery));
  W.AddShort(',"projection":');
  W.AddVariantJSON(variant(fReturnFieldsSelector));
  W.AddShort(',"numberToReturn":');
  W.Add(fNumberToReturn);
  W.Add('}');
end;

{ TMongoRequestGetMore }

constructor TMongoRequestGetMore.Create(const FullCollectionName: RawUTF8;
  NumberToReturn: integer; CursorID: Int64);
begin
  inherited Create(FullCollectionName,opGetMore,0,0);
  WriteCollectionName(0,FullCollectionName);
  Write4(NumberToReturn);
  Write8(CursorID);
end;

{ TMongoRequestKillCursor }

constructor TMongoRequestKillCursor.Create(const CursorIDs: array of Int64);
begin
  if high(CursorIDs)<0 then
    raise EMongoException.CreateFmt('%s.Create([]) call',[ClassName]);
  inherited Create(FullCollectionName,opGetMore,0,0); 
  Write4(0); // reserved for future use
  Write4(length(CursorIDs));
  Write(@CursorIDs[0],length(CursorIDs)*sizeof(Int64));
end;


{ TMongoReplyCursor }

procedure TMongoReplyCursor.Init(const ReplyMessage: TMongoReply);
var Len: integer;
begin
  Len := length(ReplyMessage);
  with PMongoReplyHeader(ReplyMessage)^ do begin
    if (Len<sizeof(TMongoReplyHeader)) or (MessageLength<>Len) or
       (sizeof(TMongoReplyHeader)+NumberReturned*5>Len) then
      raise EMongoException.CreateFmt('Invalid opReply length=%d',[len]);
    if OpCode<>WIRE_OPCODES[opReply] then
      raise EMongoException.CreateFmt('Invalid opReply OpCode=%d',[OpCode]);
    fRequestID := RequestID;
    fResponseTo := ResponseTo;
    byte(fResponseFlags) := ResponseFlags;
    fCursorID := CursorID;
    fStartingFrom := StartingFrom;
    fNumberReturned := NumberReturned;
  end;
  fReply := ReplyMessage;
  fFirstDocument := PAnsiChar(pointer(fReply))+sizeof(TMongoReplyHeader);
  Rewind;
  fLatestDocIndex := -1;
end;

procedure TMongoReplyCursor.ComputeDocumentsList;
var i, Len: integer;
    P: PAnsiChar;
begin
  if fDocuments<>nil then
    exit;
  Len := length(fReply);
  SetLength(fDocuments,DocumentCount);
  P := fFirstDocument;
  for i := 0 to DocumentCount-1 do begin
    fDocuments[i] := P;
    inc(P,PInteger(P)^); // fast "parsing" of all supplied documents
    if P-pointer(fReply)>Len then
      raise EMongoException.CreateFmt('Invalid opReply Document[%d] content',[i]);
  end;
  if P-pointer(fReply)<>Len then
    raise EMongoException.Create('Invalid opReply Documents');
end;

function TMongoReplyCursor.GetOneDocument(index: integer): variant;
begin
  if fLatestDocIndex<>index then begin // naive but efficient cache
    GetDocument(index,fLatestDocValue);
    fLatestDocIndex := index;
  end;
  result := fLatestDocValue;
end;

procedure TMongoReplyCursor.GetDocument(index: integer; var result: variant);
begin
  if cardinal(index)>=cardinal(length(fDocuments)) then
    raise EMongoException.CreateFmt('Out of range index %d >= %d',[index,length(fDocuments)]);
  if fDocuments=nil then
    ComputeDocumentsList;
  BSONToDoc(fDocuments[index],result,0,asDocVariantPerReference);
end;

function TMongoReplyCursor.Next(out doc: variant;
  option: TBSONDocArrayConversion=asDocVariantPerReference): boolean;
var b: PByte;
begin
  if Next(b) then begin
    BSONToDoc(b,doc,0,option);
    result := true;
  end else
    result := false;
end;

function TMongoReplyCursor.Next(out doc: PByte): boolean;
begin
  if fCurrentPosition<DocumentCount then begin
    doc := PByte(fCurrentDocument);
    inc(fCurrentDocument,PInteger(fCurrentDocument)^);
    inc(fCurrentPosition);
    result := true;
  end else begin
    doc := nil;
    result := false;
  end;
end;

function TMongoReplyCursor.Next(out BSON: TBSONDocument): boolean;
var b: PByte;
begin
  if Next(b) then begin
    SetString(BSON,PAnsiChar(b)+4,PInteger(b)^);
    result := true;
  end else
    result := false;
end;

function TMongoReplyCursor.Next(out JSON: RawUTF8; Mode: TMongoJSONMode=modMongoStrict): boolean;
var b: PByte;
begin
  if Next(b) then begin
    JSON := BSONToJSON(b,betDoc,0,Mode);
    result := true;
  end else
    result := false;
end;  

procedure TMongoReplyCursor.Rewind;
begin
  fCurrentPosition := 0;
  fCurrentDocument := fFirstDocument;
end;

function TMongoReplyCursor.AppendAllToDocVariantDynArray(var Dest: TVariantDynArray): integer;
var b: PByte;
begin
  result := length(Dest);
  if (fReply='') or (DocumentCount<=0) then
    exit; // nothing to append
  SetLength(Dest,result+DocumentCount);
  Rewind;
  while Next(b) do begin
    BSONToDoc(b,Dest[result],0,asDocVariantPerReference);
    inc(result);
  end;
  if result<>length(Dest) then
    raise EMongoException.Create('Invalid opReply Documents');
end;

procedure TMongoReplyCursor.AppendAllToBSON(Dest: TBSONWriter);
var name: RawUTF8;
    i: integer;
    P: PAnsiChar;
begin
  P := FirstDocument;
  for i := 1 to DocumentCount do begin
    UInt32ToUtf8(Dest.Tag,name); // Dest.Tag = item number in array
    Dest.Tag := Dest.Tag+1;
    Dest.BSONWrite(name,betDoc);
    Dest.Write(P,PInteger(P)^);
    inc(P,PInteger(P)^);
  end;
end;

function TMongoReplyCursor.AppendAllToDocVariant(var Dest: TDocVariantData): integer;
var item: variant;
begin
  if TVarData(Dest).VType<>DocVariantType.VarType then
    TDocVariant.New(Variant(Dest),JSON_OPTIONS[true]);
  result := Dest.Count;
  if (fReply='') or (DocumentCount<=0) then
    exit; // nothing to append
  inc(result,DocumentCount);
  Dest.Capacity := result;
  Rewind;
  while Next(item) do
    Dest.AddItem(item);
  if Dest.Count<>result then
    raise EMongoException.Create('Invalid opReply Documents');
end;

procedure TMongoReplyCursor.FetchAllToJSON(W: TTextWriter; Mode: TMongoJSONMode;
  WithHeader: boolean);
var b: PByte;
begin
  if (fReply='') or (DocumentCount<=0) then begin
    W.AddShort('null');
    exit;
  end;
  if WithHeader then begin
    W.AddShort('{"ReplyHeader":');
    W.AddJSONEscape(['ResponseFlags',byte(ResponseFlags),'RequestID',RequestID,
      'ResponseTo',ResponseTo,'CursorID',CursorID,
      'StartingFrom',StartingFrom,'NumberReturned',DocumentCount]);
    W.AddShort(',"ReplyDocuments":[');
  end;
  Rewind;
  while Next(b) do begin
    inc(b,sizeof(integer)); // points to the "e_list" of "int32 e_list #0"
    BSONToJSON(b,betDoc,W,Mode);
    W.Add(',');
  end;
  W.CancelLastComma;
  if WithHeader then
    W.Add(']','}');
end;

function TMongoReplyCursor.ToJSON(Mode: TMongoJSONMode; WithHeader: boolean): RawUTF8;
var W: TTextWriter;
begin
  if (fReply='') or (DocumentCount<=0) then 
    result := 'null' else begin
    W := TTextWriter.CreateOwnedStream;
    try
      FetchAllToJSON(W,Mode,WithHeader);
      W.SetText(result);
    finally
      W.Free;
    end;
  end;
end;


{ TMongoConnection }

const
  /// message big enough to retrieve the maximum MongoDB document size
  MONGODB_MAXMESSAGESIZE = BSON_MAXDOCUMENTSIZE+sizeof(TMongoReplyHeader);

constructor TMongoConnection.Create(const aClient: TMongoClient;
  const aServerAddress: RawByteString; aServerPort: integer);
begin
  if aClient=nil then
    raise EMongoException.Create('TMongoConnection.Create(nil)');
  fClient := aClient;
  fServerAddress := trim(aServerAddress);
  if fServerAddress='' then
    fServerAddress := '127.0.0.1';
  fServerPort := aServerPort;
  InitializeCriticalSection(fLock);
end;

destructor TMongoConnection.Destroy;
begin
  try
    try
      Close;
    except
      ; // continue on socket error
    end;
  finally
    DeleteCriticalSection(fLock);
    inherited Destroy;
  end;
end;

procedure TMongoConnection.Open;
begin
  if self=nil then
    raise EMongoException.Create('TMongoConnection(nil).Open');
  if fSocket<>nil then
    raise EMongoConnectionException.Create('Duplicate Open',self);
  try
    fSocket := TCrtSocket.Open(fServerAddress,UInt32ToUtf8(fServerPort),
      cslTCP,fClient.ConnectionTimeOut);
  except
    on E: Exception do
      raise EMongoException.CreateFmt(
        'Unable to connect to MongoDB server: %s',[E.Message]);
  end;
  fSocket.TCPNoDelay := ord(true); // we buffer all output data before sending
  fSocket.KeepAlive := ord(true);  // do not close the connection without notice
end;

function TMongoConnection.GetOpened: boolean;
begin
  result := (self<>nil) and (fSocket<>nil);
end;

procedure TMongoConnection.Close;
begin
  FreeAndNil(fSocket);
end;

procedure TMongoConnection.GetDocumentsAndFree(Query: TMongoRequestQuery; var result: TVariantDynArray);
begin
  result := nil;
  GetRepliesAndFree(Query,ReplyDocVariant,result);
end;

procedure TMongoConnection.GetDocumentsAndFree(Query: TMongoRequestQuery;
  var result: variant);
var ForceOneInstance: boolean;
    docs: TVariantDynArray;
begin
  ForceOneInstance := Query.NumberToReturn=1;
  SetVariantNull(result);
  GetRepliesAndFree(Query,ReplyDocVariant,docs);
  if docs<>nil then
    if ForceOneInstance then
      result := docs[0] else
      TDocVariantData(result).InitArrayFromVariants(docs,JSON_OPTIONS[true]);
end;

function TMongoConnection.GetDocumentsAndFree(Query: TMongoRequestQuery): variant;
begin
  GetDocumentsAndFree(Query,result);
end;

function TMongoConnection.GetBSONAndFree(Query: TMongoRequestQuery): TBSONDocument;
var W: TBSONWriter;
    Start: Cardinal;
begin
  W := TBSONWriter.Create(TRawByteStringStream);
  try
    Start := W.BSONDocumentBegin;
    GetRepliesAndFree(Query,ReplyBSON,W); // W.Tag = item number in array
    W.BSONDocumentEnd(Start);
    W.ToBSONDocument(result);
  finally
    W.Free;
  end;
end;

function TMongoConnection.GetJSONAndFree(Query: TMongoRequestQuery; Mode: TMongoJSONMode): RawUTF8;
var W: TTextWriter;
    ReturnAsJSONArray: boolean;
begin
  ReturnAsJSONArray := Query.NumberToReturn>1;
  W := TTextWriter.CreateOwnedStream;
  try
    if ReturnAsJSONArray then
      W.Add('[');
    if Mode=modMongoShell then
      GetRepliesAndFree(Query,ReplyJSONExtended,W) else
      GetRepliesAndFree(Query,ReplyJSONStrict,W);
    W.CancelLastComma;
    if ReturnAsJSONArray then
      W.Add(']');
    W.SetText(result);
    if (result='') or (result='[]') or (result='{}') then
      result := 'null';
  finally
    W.Free;
  end;
end;

procedure TMongoConnection.GetRepliesAndFree(Query: TMongoRequestQuery;
  OnEachReply: TOnMongoConnectionReply; var Opaque);
var main, more: TMongoReplyCursor;
    getMore: TMongoRequestGetMore;
    count: integer;
    cursorID: Int64;
begin
  try
    if not Assigned(Query) then
      raise EMongoRequestException.Create('Query=nil',self);
    if not Assigned(OnEachReply) then
      raise EMongoRequestException.Create('OnEachReply=nil',self,Query);
    count := Query.NumberToReturn; // 0 means default return size
    GetCursor(Query,main);
    if main.DocumentCount>0 then begin
      OnEachReply(Query,main,Opaque);
      if count>0 then
        dec(count,main.DocumentCount);
    end;
    cursorID := main.CursorID;
    if cursorID<>0 then
      if (Query.NumberToReturn=0) or ((Query.NumberToReturn>0)and(count>0)) then
      repeat
        getMore := TMongoRequestGetMore.Create(
          Query.FullCollectionName,count,cursorID);
        try
          GetCursor(getMore,more);
          if mrfCursorNotFound in more.ResponseFlags then
            raise EMongoRequestException.Create('GetMore cursor not found',self,Query,more);
          if more.DocumentCount>0 then begin
            OnEachReply(Query,more,Opaque);
            dec(count,more.DocumentCount);
          end;
          cursorID := more.CursorID;
        finally
          getMore.Free;
        end;
      until ((Query.NumberToReturn>0)and(count<=0)) or (cursorID=0);
    if cursorID<>0 then // if cursor not exhausted: need to kill it
      SendAndFree(TMongoRequestKillCursor.Create([cursorID]));
  finally
    Query.Free;
  end;
end;

procedure TMongoConnection.ReplyDocVariant(Request: TMongoRequest;
  const Reply: TMongoReplyCursor; var Opaque);
begin
  Reply.AppendAllToDocVariantDynArray(TVariantDynArray(Opaque));
end;

procedure TMongoConnection.ReplyJSONStrict(Request: TMongoRequest;
  const Reply: TMongoReplyCursor; var Opaque);
var W: TTextWriter absolute Opaque;
begin
  Reply.FetchAllToJSON(W,modMongoStrict,false);
  W.Add(',');
end;

procedure TMongoConnection.ReplyJSONExtended(Request: TMongoRequest;
  const Reply: TMongoReplyCursor; var Opaque);
var W: TTextWriter absolute Opaque;
begin
  Reply.FetchAllToJSON(W,modMongoShell,false);
  W.Add(',');
end;

procedure TMongoConnection.ReplyBSON(Request: TMongoRequest;
  const Reply: TMongoReplyCursor; var Opaque);
begin
  Reply.AppendAllToBSON(TBSONWriter(Opaque));
end;

function TMongoConnection.Send(Request: TMongoRequest): boolean;
var doc: TBSONDocument;
begin
  if fSocket=nil then
    raise EMongoRequestException.Create('Missing Open',self,Request);
  if Request=nil then
    raise EMongoRequestException.Create('LockAndSend(nil)',self);
  Request.ToBSONDocument(doc);
  if (fClient.fLogRequestEvent<>sllNone) and (fClient.fLog<>nil) then
    fClient.fLog.Log(fClient.fLogRequestEvent,Request.ToJSON(modMongoShell));
  result := fSocket.TrySndLow(pointer(doc),length(doc));
end;

function TMongoConnection.SendAndFree(Request: TMongoRequest; NoAcknowledge: boolean): variant;
var cmd: variant;
begin
  SetVariantNull(result);
  try
    if self=nil then
      raise EMongoRequestException.Create('Connection=nil',self,Request);
    Lock;
    try
      if Send(Request) then begin
        if NoAcknowledge or
           (fClient.WriteConcern in [wcErrorsIgnored,wcUnacknowledged]) then
          exit;
        case fClient.WriteConcern of
          wcAcknowledged:        cmd := 'getLastError';
          wcJournaled:           cmd := BSONVariant(['getLastError',1,'j',true]);
          wcReplicaAcknowledged: cmd := BSONVariant(['getLastError',1,'w',2]);
          else raise EMongoRequestException.CreateFmt('SendAndFree WriteConcern=%d',
            [ord(fClient.WriteConcern)],self,Request);
        end;
        RunCommand(Request.DatabaseName,cmd,result);
        if not VarIsNull(result.err) then
          raise EMongoRequestException.Create('SendAndFree',self,Request,TDocVariantData(result));
      end else // socket error on sending
        if fClient.WriteConcern=wcErrorsIgnored then
          exit else
          raise EMongoRequestOSException.Create('SendAndFree',self,Request);
    finally
      UnLock;
    end;
  finally
    Request.Free;
  end;
end;

procedure TMongoConnection.GetCursor(Request: TMongoRequest; var Result: TMongoReplyCursor);
var reply: TMongoReply;
begin
  GetReply(Request,reply);
  Result.Init(reply);
  if (fClient.fLogRequestEvent<>sllNone) and (fClient.fLog<>nil) then
    fClient.fLog.Log(fClient.fLogRequestEvent,Result.ToJSON(modMongoShell));
  if mrfQueryFailure in Result.ResponseFlags then
    raise EMongoRequestException.Create('Query failure',self,Request,Result);
end;

procedure TMongoConnection.GetReply(Request: TMongoRequest; out result: TMongoReply);
var Header: TMongoReplyHeader;
    DataLen: integer;
begin
  if self=nil then
    raise EMongoRequestException.Create('Connection=nil',self,Request);
  fillchar(Header,sizeof(Header),0);
  try
    Lock;
    if Send(Request) then
      if fSocket.TrySockRecv(@Header,sizeof(Header)) then begin
        if (Header.MessageLength<SizeOf(Header)) or
           (Header.MessageLength>MONGODB_MAXMESSAGESIZE) then
          raise EMongoRequestException.CreateFmt('Reply Length=%d',
            [Header.MessageLength],self,Request);
        SetLength(result,Header.MessageLength);
        PMongoReplyHeader(result)^ := Header;
        DataLen := Header.MessageLength-sizeof(Header);
        if fSocket.TrySockRecv(@PByteArray(result)[sizeof(Header)],DataLen) then
          if Header.ResponseTo=Request.MongoRequestID then // success
            exit else
            raise EMongoRequestException.CreateFmt('Unexpected ResponseTo=%d',
              [Header.ResponseTo],self,Request);
      end else
        raise EMongoRequestException.Create('Server reset the connection: '+
          'proabably due to a bad formatted BSON request',self,Request);
    // if we reached here, this is due to a socket error
    raise EMongoRequestOSException.Create('GetReply',self,Request);
  finally
    UnLock;
  end;
end;

function TMongoConnection.RunCommand(const aDatabaseName: RawUTF8;
  const command: variant; var returnedValue: variant): RawUTF8;
begin
  GetDocumentsAndFree(
    TMongoRequestQuery.Create(aDatabaseName+'.$cmd',command,null,1),
    returnedValue);
  with TDocVariantData(returnedValue) do
    if GetValueOrDefault('ok',1)<>0 then
      result := '' else
      result := VariantToUTF8(GetValueOrDefault('errmsg','unspecified error'));
end;

function TMongoConnection.RunCommand(const aDatabaseName: RawUTF8;
  const command: variant; var returnedValue: TBSONDocument): boolean;
var res: PByte;
    item: TBSONElement;
begin
  returnedValue := GetBSONAndFree(
    TMongoRequestQuery.Create(aDatabaseName+'.$cmd',command,null,1));
  result := true;
  res := pointer(returnedValue);
  if BSONParseLength(res)>0 then
    if item.FromSearch(res,'ok')<>nil then
      if item.ToInteger(1)=0 then
        result := false;
end;

procedure TMongoConnection.Lock;
begin
  EnterCriticalSection(fLock);
  inc(fLocked);
end;

procedure TMongoConnection.UnLock;
begin
  dec(fLocked);
  LeaveCriticalSection(fLock);
end;

function TMongoConnection.GetLocked: boolean;
begin
  result := (self<>nil) and (fLocked>0);
end;


{ EMongoConnectionException }

constructor EMongoConnectionException.Create(const aMsg: string;
  aConnection: TMongoConnection);
begin
  inherited Create(aMsg);
  fConnection := aConnection;
end;

constructor EMongoConnectionException.CreateFmt(const aMsg: string; const Args: array of const;
  aConnection: TMongoConnection);
begin
  inherited CreateFmt(aMsg,Args);
  fConnection := aConnection;
end;

function EMongoConnectionException.CustomLog(WR: TTextWriter;
  const Context: TSynLogExceptionContext): boolean;
begin
  inherited CustomLog(WR,Context);
  WR.AddShort('  ');
  if fConnection<>nil then
    WR.Add('using %:%  ',[fConnection.ServerAddress,fConnection.ServerPort]);
  result := false; // log stack trace
end;


{ EMongoRequestException }

constructor EMongoRequestException.Create(const aMsg: string; aConnection: TMongoConnection;
  aRequest: TMongoRequest);
begin
  inherited Create(aMsg,aConnection);
  fRequest := aRequest;
end;

constructor EMongoRequestException.CreateFmt(const aMsg: string; const Args: array of const;
  aConnection: TMongoConnection; aRequest: TMongoRequest);
begin
  inherited CreateFmt(aMsg,Args,aConnection);
  fRequest := aRequest;
end;  

constructor EMongoRequestException.Create(const aMsg: string; aConnection: TMongoConnection;
  aRequest: TMongoRequest; const aError: TMongoReplyCursor);
begin
  Create(aMsg,aConnection,aRequest);
  fError := aError;
end;

constructor EMongoRequestException.Create(const aMsg: string; aConnection: TMongoConnection;
  aRequest: TMongoRequest; const aErrorDoc: TDocVariantData);
begin
  Create(aMsg,aConnection,aRequest);
  fErrorDoc := variant(aErrorDoc);
end;

function EMongoRequestException.CustomLog(WR: TTextWriter; const Context: TSynLogExceptionContext): boolean;
begin
  inherited CustomLog(WR,Context);
  if fRequest<>nil then begin
    WR.AddInstanceName(fRequest,':');
    fRequest.ToJSON(WR,modMongoShell);
  end;
  if fError.Reply<>'' then
    fError.FetchAllToJSON(WR,modMongoShell,True) else
  if TVarData(fErrorDoc).VType<>varEmpty then begin
    WR.AddShort(' ReplyDocument:');
    WR.AddVariantJSON(fErrorDoc);
  end;
  result := false; // log stack trace
end;

function EMongoRequestException.GetErrorDoc: variant;
begin
  if TVarData(fErrorDoc).VType=varEmpty then begin
    if not fError.Next(fErrorDoc) then
      SetVariantNull(fErrorDoc);
  end;
  result := fErrorDoc;
end;  


{ EMongoRequestOSException }

constructor EMongoRequestOSException.Create(const aMsg: string;
  aConnection: TMongoConnection; aRequest: TMongoRequest=nil);
begin
  fSystemLastError := Windows.GetLastError;
  CreateFmt('%s: %s (%d)',[aMsg,SysErrorMessage(fSystemLastError),fSystemLastError],
    aConnection,aRequest);
end;


{ EMongoDatabaseException }

constructor EMongoDatabaseException.Create(const aMsg: string;
  aDatabase: TMongoDatabase);
begin
  inherited Create(aMsg,aDatabase.Client.Connections[0]);
  fDatabase := aDatabase;
end;

constructor EMongoDatabaseException.CreateFmt(const aMsg: string; const Args: array of const;
  aDatabase: TMongoDatabase);
begin
  inherited CreateFmt(aMsg,Args,aDatabase.Client.Connections[0]);
  fDatabase := aDatabase;
end;

function EMongoDatabaseException.CustomLog(WR: TTextWriter;
  const Context: TSynLogExceptionContext): boolean;
begin
  inherited CustomLog(WR,Context);
  WR.AddJSONEscape(['Database',fDatabase.Name]);
  result := false; // log stack trace
end;


{ TMongoClient }

constructor TMongoClient.Create(const Host: RawUTF8; Port: Integer;
  const SecondaryHostCSV, SecondaryPortCSV: RawUTF8);
var secHost: TRawUTF8DynArray;
    secPort: TIntegerDynArray;
    nHost, i: integer;
begin
  fConnectionTimeOut := 30000;
  CSVToRawUTF8DynArray(pointer(SecondaryHostCSV),secHost);
  nHost := length(secHost);
  SetLength(fConnections,nHost+1);
  fConnections[0] := TMongoConnection.Create(self,Host,Port);
  if nHost>0 then begin
    CSVToIntegerDynArray(pointer(SecondaryPortCSV),secPort);
    for i := 0 to nHost-1 do begin
      if i>high(secPort) then
        Port := MONGODB_DEFAULTPORT else
        Port := secPort[i];
      fConnections[i+1] := TMongoConnection.Create(self,secHost[i],Port);
    end;
  end;
  fDatabases := TRawUTF8ListHashed.Create(true);
end;

destructor TMongoClient.Destroy;
var i: integer;
begin
  for i := 0 to high(fConnections) do
    FreeAndNil(fConnections[i]);
  FreeAndNil(fDatabases);
  inherited;
end;

function TMongoClient.GetOneReadConnection: TMongoConnection;
function GetUnlockedSecondaryIndex: integer;
var retry: integer;
begin
  if Length(fConnections)=1 then // no secondary? use primary
    result := 0 else begin
    for retry := 1 to 100 do begin // search for an inactive connection
      result := fLatestReadConnectionIndex; // simple round-robin pattern
      if result=high(fConnections) then
        if ReadPreference=rpSecondary then
          result := 1 else
          result := 0 else
        inc(result); // thread-safety is not an issue here
      if (retry<=length(fConnections)) and not fConnections[result].Opened then
      try
        fConnections[result].Open;
      except
        on E: Exception do
          continue;
      end;
      if fConnections[result].Opened then
        if fConnections[result].Locked then
          if retry mod length(fConnections)=0 then
            Sleep(2) else
            continue else
          break;
    end;
    if not fConnections[result].Opened then
      result := 0; // safe fallback to primary member in worst case
    fLatestReadConnectionIndex := result;
  end;
end;
begin
  case ReadPreference of
  rpPrimaryPreferred:
    if fConnections[0].Locked then
      result := fConnections[GetUnlockedSecondaryIndex] else
      result := fConnections[0];
  rpSecondary, rpSecondaryPreferred:
    result := fConnections[GetUnlockedSecondaryIndex];
  else // rpPrimary:
    result := fConnections[0];
  end;
end;

function TMongoClient.Open(const DatabaseName: RawUTF8): TMongoDatabase;
begin
  if self=nil then
    result := nil else begin
    result := TMongoDatabase(fDatabases.GetObjectByName(DatabaseName));
    if result=nil then begin // not already opened -> try now from primary host
      if not fConnections[0].Opened then
        fConnections[0].Open;
      result := TMongoDatabase.Create(Self,DatabaseName);
      fDatabases.AddObject(DatabaseName,result);
    end;
  end;
  if result=nil then
    raise EMongoException.CreateFmt('Unknown database "%s"',[DatabaseName]);
end;

function TMongoClient.OpenAuth(const DatabaseName, UserName,
  PassWord: RawUTF8): TMongoDatabase;
begin
  raise EMongoException.CreateFmt('OpenAuth(%s) not yet implemented',[DatabaseName]);
end;

function TMongoClient.GetServerBuildInfo: variant;
begin
  if TVarData(fServerBuildInfo).VType=varEmpty then
     fConnections[0].RunCommand('admin','buildinfo',fServerBuildInfo);
  result := fServerBuildInfo;
end;

function TMongoClient.GetServerBuildInfoNumber: cardinal;
  procedure ComputeIt;
  var vArr: variant;
  begin
    vArr := ServerBuildInfo.versionArray;
    if vArr._Count=4 then
      fServerBuildInfoNumber := // e.g. 2040900 for MongoDB 2.4.9
        vArr._(0)*1000000+vArr._(1)*10000+vArr._(2)*100+vArr._(3);
  end;
begin
  if fServerBuildInfoNumber=0 then
    ComputeIt;
  result := fServerBuildInfoNumber;
end;

function TMongoClient.GetBytesReceived: Int64;
var i: integer;
begin
  result := 0;
  for i := 0 to high(fConnections) do
    inc(result,fConnections[i].Socket.BytesIn);
end;

function TMongoClient.GetBytesSent: Int64;
var i: integer;
begin
  result := 0;
  for i := 0 to high(fConnections) do
    inc(result,fConnections[i].Socket.BytesOut);
end;

function TMongoClient.GetBytesTransmitted: Int64;
var i: integer;
begin
  result := 0;
  for i := 0 to high(fConnections) do
    inc(result,fConnections[i].Socket.BytesIn+fConnections[i].Socket.BytesOut);
end;




{ TMongoDatabase }

constructor TMongoDatabase.Create(aClient: TMongoClient;
  const aDatabaseName: RawUTF8);
var colls: PByte;
    item: TBSONElement;
    full,db,coll: RawUTF8;
begin
  fClient := aClient;
  fName := aDatabaseName;
  colls := pointer(fClient.Connections[0].GetBSONAndFree(
    TMongoRequestQuery.Create(aDatabaseName+'.system.namespaces',null,'name',maxInt)));
  // e.g. [ {name:"test.system.indexes"}, {name:"test.test"} ]
  fCollections := TRawUTF8ListHashed.Create(true);
  if BSONParseLength(colls)>0 then
    while item.FromNext(colls) do
    if item.FromSearch(item.Data.DocList,'name')<>nil then begin
      full := item.ToRawUTF8;
      split(full,'.',db,coll);
      if db<>aDatabaseName then
        raise EMongoConnectionException.CreateFmt(
          'Invalid "%s" collection name for DB "%s"',
          [full,aDatabaseName],fClient.Connections[0]);
      fCollections.AddObject(coll,TMongoCollection.Create(self,coll));
    end;
end;

destructor TMongoDatabase.Destroy;
begin
  FreeAndNil(fCollections);
  inherited;
end;

procedure TMongoDatabase.AddUser(const User: variant);
begin
  raise EMongoException.CreateFmt('No %s.AddUser() yet',[ClassName]);
end;

function TMongoDatabase.GetCollection(const Name: RawUTF8): TMongoCollection;
begin
  result := GetCollectionOrNil(Name);
  if result=nil then
    raise EMongoDatabaseException.CreateFmt('Unknown collection "%s"',[Name],self);
end;

function TMongoDatabase.GetCollectionOrCreate(const Name: RawUTF8): TMongoCollection;
begin
  result := GetCollectionOrNil(Name);
  if result=nil then begin
    result := TMongoCollection.Create(self,Name);
    fCollections.AddObject(Name,result);
  end;
end;

function TMongoDatabase.GetCollectionOrNil(const Name: RawUTF8): TMongoCollection;
begin
  result := TMongoCollection(fCollections.GetObjectByName(Name));
end;

function TMongoDatabase.RunCommand(const command: variant;
  var returnedValue: variant): RawUTF8;
begin
  result := Client.Connections[0].RunCommand(Name,Command,returnedValue);
end;

function TMongoDatabase.RunCommand(const command: variant;
  var returnedValue: TBSONDocument): boolean;
begin
  result := Client.Connections[0].RunCommand(Name,Command,returnedValue);
end;


{ TMongoCollection }

constructor TMongoCollection.Create(aDatabase: TMongoDatabase;
  const aCollectionName: RawUTF8);
begin
  fDatabase := aDatabase;
  fName := aCollectionName;
  fFullCollectionName := fDatabase.Name+'.'+fName;
end;

function TMongoCollection.AggregateCall(Operators: PUTF8Char;
  const Params: array of const; var reply,res: variant): boolean;
var pipeline: RawUTF8;
begin // see http://docs.mongodb.org/manual/reference/command/aggregate
  pipeline := FormatUTF8('{aggregate:"%",pipeline:[%]}',
    [Name,FormatUTF8(Operators,[],Params,True)]);
  // db.runCommand({aggregate:"test",pipeline:[{$group:{_id:null,max:{$max:"$int"}}}]})
  Database.RunCommand(BSONVariant(pipeline),reply);
  // { "result" : [ { "_id" : null, "max" : 1250 } ], "ok" : 1 }
  res := reply.result;
  result := not VarIsNull(res);
end;

function TMongoCollection.AggregateDoc(Operators: PUTF8Char;
  const Params: array of const): variant;
var reply: variant;
begin
  if AggregateCall(Operators,Params,reply,result) then
    TDocVariant.GetSingleOrDefault(result,result,result) else
    SetVariantNull(result);
end;

function TMongoCollection.AggregateJSON(Operators: PUTF8Char;
  const Params: array of const; Mode: TMongoJSONMode): RawUTF8;
var reply,res: variant;
begin
  if AggregateCall(Operators,Params,reply,res) then
    result := VariantSaveMongoJSON(res,Mode) else
    result := '';
end;

function TMongoCollection.Drop: RawUTF8;
var res: Variant;
begin
  if self=nil then
    result := 'No collection' else
    result := fDatabase.RunCommand(BSONVariant('{drop:?}',[],[Name]),res);
  if result='' then
    fDatabase.fCollections.Delete(fDatabase.fCollections.IndexOf(Name));
end;

procedure TMongoCollection.EnsureIndex(const Keys, Options: variant);
var doc,res: variant;
    indexName: RawUTF8;
    i,order: integer;
    useCommand: Boolean;
begin
  if DocVariantData(Keys)^.Kind<>dvObject then
    raise EMongoException.CreateFmt('%s.EnsureIndex(Keys?)',[FullCollectionName]);
  useCommand := fDatabase.Client.ServerBuildInfoNumber>=2060000;
  doc := _ObjFast(['key', Keys]);
  if not useCommand then
    doc.ns := FullCollectionName;
  if DocVariantType.IsOfType(Options) then begin
    with DocVariantData(Options)^ do
      for i := 0 to Count-1 do
        if Names[i]='name' then
          indexName := VariantToUTF8(Values[i]) else
          TDocVariantData(doc).AddValue(Names[i],Values[i]);
  end;
  if indexName='' then begin
    with DocVariantData(Keys)^ do
    for i := 0 to Count-1 do begin
      indexName := indexName+Names[i]+'_';
      order := VariantToIntegerDef(Values[i],10);
      if order=-1 then
        indexName := indexName+'_' else
      if order<>1 then
        raise EMongoException.CreateFmt('%s.EnsureIndex({%s: %s!!})',
          [FullCollectionName,Names[i],Values[i]]);
    end;
  end;
  if length(FullCollectionName)+length(indexName)>120 then
    raise EMongoException.CreateFmt(
      '%s.EnsureIndex() computed name > 128 chars: set as option',
      [FullCollectionName]);
  doc.name := indexName;
  if useCommand then
    fDatabase.RunCommand(BSONVariant(
      '{ createIndexes: ?, indexes: [?] }',[],[Name,doc]),res) else
    fDatabase.Collection['system.indexes'].Insert([doc]);
end;

procedure TMongoCollection.EnsureIndex(const Keys: array of RawUTF8;
  Ascending, Unique: boolean);
const Order: array[boolean] of Integer = (-1,1);
var k,opt: variant;
    A: integer;
begin
  TDocVariant.New(k);
  for A := 0 to high(Keys) do
    TDocVariantData(k).AddValue(Keys[A],Order[Ascending]);
  if Unique then
    opt := _ObjFast(['unique',true]);
  EnsureIndex(k,opt);
end;

function TMongoCollection.Count: integer;
var res: variant;
begin
  fDatabase.RunCommand(BSONVariant(['count',Name]),res);
  result := DocVariantData(res)^.GetValueOrDefault('n',0);
end;

function TMongoCollection.FindCount(const Query: variant): integer;
var res: variant;
begin
  fDatabase.RunCommand(BSONVariant(['count',Name,'query',Query]),res);
  result := DocVariantData(res)^.GetValueOrDefault('n',0);
end;

function TMongoCollection.FindCount(Criteria: PUTF8Char;
  const Args,Params: array of const;
  MaxNumberToReturn: integer=0; NumberToSkip: Integer=0): integer;
var cmd: RawUTF8;
    res: variant;
begin
  cmd := FormatUTF8('{count:"%",query:%',[Name,FormatUTF8(Criteria,Args,Params,true)]);
  if MaxNumberToReturn>0 then
    cmd := FormatUTF8('%,limit:%',[cmd,MaxNumberToReturn]);
  if NumberToSkip>0 then
    cmd := FormatUTF8('%,skip:%',[cmd,NumberToSkip]);
  fDatabase.RunCommand(BSONVariant(cmd+'}'),res);
  result := TDocVariantData(res).GetValueOrDefault('n',0);
end;

function TMongoCollection.FindBSON(const Criteria, Projection: Variant;
  NumberToReturn, NumberToSkip: Integer; Flags: TMongoQueryFlags): TBSONDocument;
begin
  result := Database.Client.GetOneReadConnection.GetBSONAndFree(
    TMongoRequestQuery.Create(fFullCollectionName,
      Criteria,Projection,NumberToReturn,NumberToSkip,Flags));
end;

function TMongoCollection.FindDoc(const Criteria, Projection: Variant;
  NumberToReturn, NumberToSkip: Integer; Flags: TMongoQueryFlags): variant;
begin
  Database.Client.GetOneReadConnection.GetDocumentsAndFree(
    TMongoRequestQuery.Create(fFullCollectionName,
      Criteria,Projection,NumberToReturn,NumberToSkip,Flags),result);
end;

function TMongoCollection.FindDoc(Criteria: PUTF8Char;
  const Params: array of const; NumberToReturn, NumberToSkip: Integer;
  Flags: TMongoQueryFlags): variant;
begin
  result := FindDoc(BSONVariant(Criteria,[],Params),null,
    NumberToReturn,NumberToSkip,Flags);
end;

procedure TMongoCollection.FindDocs(Criteria: PUTF8Char;
  const Params: array of const; var result: TVariantDynArray;
  const Projection: RawUTF8; NumberToReturn, NumberToSkip: Integer;
  Flags: TMongoQueryFlags);
begin
  Database.Client.GetOneReadConnection.GetDocumentsAndFree(
    TMongoRequestQuery.Create(fFullCollectionName,
      BSONVariant(Criteria,[],Params),BSONVariant(Projection),
      NumberToReturn,NumberToSkip,Flags),result);
end;

function TMongoCollection.FindOne(const _id: TBSONObjectID): variant;
begin
  result := FindOne(_id.ToVariant);
end;

function TMongoCollection.FindOne(const _id: variant): variant;
begin
  result := FindDoc(BSONVariant(['_id',_id]),null,1);
end;

procedure TMongoCollection.FindDocs(var result: TVariantDynArray;
  const Projection: RawUTF8; NumberToReturn, NumberToSkip: Integer;
  Flags: TMongoQueryFlags);
begin
  Database.Client.GetOneReadConnection.GetDocumentsAndFree(
    TMongoRequestQuery.Create(fFullCollectionName,null,null,
      NumberToReturn,NumberToSkip,Flags),result);
end;

function TMongoCollection.FindJSON(const Criteria, Projection: Variant;
  NumberToReturn, NumberToSkip: Integer; Flags: TMongoQueryFlags;
  Mode: TMongoJSONMode): RawUTF8;
begin
  result := Database.Client.GetOneReadConnection.GetJSONAndFree(
    TMongoRequestQuery.Create(fFullCollectionName,
      Criteria,Projection,NumberToReturn,NumberToSkip,Flags),Mode);
end;

function TMongoCollection.FindJSON(Criteria: PUTF8Char;
  const Params: array of const; NumberToReturn, NumberToSkip: Integer;
  Flags: TMongoQueryFlags; Mode: TMongoJSONMode): RawUTF8;
begin
  result := FindJSON(BSONVariant(Criteria,[],Params),null,
    NumberToReturn,NumberToSkip,Flags,Mode);
end;

function TMongoCollection.FindJSON(
  Criteria: PUTF8Char; const CriteriaParams: array of const;
  const Projection: RawUTF8; 
  NumberToReturn: integer=maxInt; NumberToSkip: Integer=0;
  Flags: TMongoQueryFlags=[]; Mode: TMongoJSONMode=modMongoStrict): RawUTF8;
begin
  result := FindJSON(BSONVariant(Criteria,[],CriteriaParams),
    BSONVariant(Projection),NumberToReturn,NumberToSkip,Flags,Mode);
end;

procedure TMongoCollection.Insert(const Documents: array of variant;
  Flags: TMongoInsertFlags; NoAcknowledge: boolean);
begin
  Database.Client.Connections[0].SendAndFree(TMongoRequestInsert.Create(
    fFullCollectionName,Documents,Flags),NoAcknowledge);
end;

procedure TMongoCollection.Insert(const Documents: TBSONDocument;
  Flags: TMongoInsertFlags=[]; NoAcknowledge: boolean=false);
begin
  Database.Client.Connections[0].SendAndFree(TMongoRequestInsert.Create(
    fFullCollectionName,Documents,Flags),NoAcknowledge);
end;

procedure TMongoCollection.InsertJSON(const JSONDocuments: array of PUTF8Char;
  Flags: TMongoInsertFlags; NoAcknowledge: boolean);
begin
  Database.Client.Connections[0].SendAndFree(TMongoRequestInsert.Create(
    fFullCollectionName,JSONDocuments,Flags),NoAcknowledge);
end;

function EnsureDocumentHasID(var doc: TDocVariantData; var oid: variant;
  CreatedObjectID: PBSONObjectID): boolean;
var ndx: integer;
begin // return TRUE if _id has been computed (i.e. save=insert)
  ndx := doc.GetValueIndex('_id',3,true);
  if ndx<0 then begin
    oid := ObjectID;
    doc.AddValue('_id',oid);
    result := true;
  end else
  if TVarData(doc.Values[ndx]).VType<=varNull then begin
    oid := ObjectID;
    doc.Values[ndx] := oid;
    result := true;
  end else begin 
    oid := doc.Values[ndx];
    result := false;
  end;
  if CreatedObjectID<>nil then
    CreatedObjectID^.FromVariant(oid)
end;

procedure TMongoCollection.Insert(Document: PUTF8Char;
  const Params: array of const; CreatedObjectID: PBSONObjectID);
var doc: variant;
    oid: variant;
begin
  doc := _JsonFastFmt(Document,[],Params);
  EnsureDocumentHasID(TDocVariantData(doc),oid,CreatedObjectID);
  Insert([doc]);
end;

function TMongoCollection.Save(var Document: variant;
  CreatedObjectID: PBSONObjectID): boolean;
var oid: variant;
begin
  if not DocVariantType.IsOfType(Document) then
    Document := _JsonFast(VariantSaveMongoJSON(Document,modMongoShell));
  result := EnsureDocumentHasID(TDocVariantData(Document),oid,CreatedObjectID);
  if result then
    Insert([Document]) else
    Update(BSONVariant(['_id',oid]),Document,[mufUpsert])
end;

procedure TMongoCollection.Save(Document: PUTF8Char;
  const Params: array of const; CreatedObjectID: PBSONObjectID);
var doc: variant;
begin
  doc := _JsonFastFmt(Document,[],Params);
  Save(doc,CreatedObjectID);
end;

procedure TMongoCollection.Update(Query: PUTF8Char;
  const QueryParams: array of const; Update: PUTF8Char;
  const UpdateParams: array of const; Flags: TMongoUpdateFlags);
var quer,upd: variant;
begin
  quer := BSONVariant(Query,[],QueryParams);
  upd := BSONVariant(Update,[],UpdateParams);
  self.Update(quer,upd,Flags);
end;

procedure TMongoCollection.Update(const Query, Update: variant;
  Flags: TMongoUpdateFlags);
begin
  Database.Client.Connections[0].SendAndFree(TMongoRequestUpdate.Create(
    fFullCollectionName,Query,Update,Flags),false);
end;

procedure TMongoCollection.UpdateOne(const _id, UpdatedFields: variant);
begin
  Update(BSONVariant(['_id',_id]),BSONVariant(['$set',UpdatedFields]));
end;

procedure TMongoCollection.Remove(const Query: variant;
  Flags: TMongoDeleteFlags);
begin
  Database.Client.Connections[0].SendAndFree(TMongoRequestDelete.Create(
    fFullCollectionName,Query,Flags),False);
end;

procedure TMongoCollection.RemoveOne(const _id: TBSONObjectID);
begin
  Remove(BSONVariant(['_id',_id.ToVariant]),[mdfSingleRemove]);
end;

procedure TMongoCollection.RemoveOne(const _id: variant);
begin
  Remove(BSONVariant(['_id',_id]),[mdfSingleRemove]);
end;

procedure TMongoCollection.RemoveFmt(Query: PUTF8Char;
  const QueryParams: array of const; Flags: TMongoDeleteFlags);
begin
  Remove(BSONVariant(Query,[],QueryParams),Flags);
end;


begin
  Assert(ord(betEof)=$00);
  Assert(ord(betInt64)=$12);
  Assert(ord(bbtGeneric)=$00);
  Assert(ord(bbtMD5)=$05);
  Assert(ord(bbtUser)=$80);
  Assert(sizeof(TBSONObjectID)=12);
  Assert(sizeof(TBSONVariantData)=sizeof(variant));
  Assert(sizeof(TMongoReplyHeader)=36);
  // ensure TDocVariant and TBSONVariant custom types are registered
  if DocVariantType=nil then
    DocVariantType := SynRegisterCustomVariantType(TDocVariant);
  BSONVariantType := SynRegisterCustomVariantType(TBSONVariant) as TBSONVariant;
end.

