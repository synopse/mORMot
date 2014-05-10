/// direct optimized MongoDB access for mORMot's ORM
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit mORMotMongoDB;

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
  - first public release, corresponding to mORMot Framework 1.18

  
  TODO:
  - complex WHERE clause with a MongoDB Query object instead of SQL syntax
  - handle TSQLRawBlob fields optionally with GridFS (and rely on TByteDynArray
    to store smaller BLOBs within the document)
  - allow PolyMorphic schemas: the same MongoDB collection may be able to
    store a hierarchy of TSQLRecord classes, storing only relevant fields in
    each document - this may be a huge benefit in common OOP work  
  - SQLite3 Virtual Table mode, for full integration with mORMotDB - certainly
    in a dedicated mORMotDBMongoDB unit (but perhaps we may loose interest)

}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
  Windows,
  SysUtils,
  Classes,
  Variants,
  SynCommons,
  mORMot,
  SynMongoDB;

type
  /// exeception class raised by this units
  EORMMongoDBException = class(EORMException);

  /// REST server with direct access to a MongoDB external database
  // - handle all REST commands via direct SynMongoDB call
  // - is used by TSQLRestServer.URI for faster RESTful direct access
  // - JOINed SQL statements are not handled yet
  TSQLRestStorageMongoDB = class(TSQLRestStorage)
  protected
    /// the associated MongoDB collection
    fCollection: TMongoCollection;
    fEngineLastID: integer;
    fBSONProjectionSimpleFields: variant;
    fBSONProjectionBlobFields: variant;
    fBSONProjectionBlobFieldsNames: TRawUTF8DynArray;
    // multi-thread BATCH process is secured via Lock/UnLock critical section
    fBatchMethod: TSQLURIMethod;
    fBatchWriter: TBSONWriter;
    fBatchIDs: TIntegerDynArray;
    fBatchCount: integer;
    function EngineNextID: Integer;
    function DocFromJSON(const JSON: RawUTF8; Occasion: TSQLOccasion;
      var Doc: TDocVariantData): integer;
    procedure JSONFromDoc(var doc: TDocVariantData; var result: RawUTF8);
    function BSONProjectionSet(var Projection: variant; WithID: boolean;
      const Fields: TSQLFieldBits; ExtFieldNames: PRawUTF8DynArray): integer;
    function GetJSONValues(const Res: TBSONDocument;
      const extFieldNames: TRawUTF8DynArray; W: TJSONSerializer): integer;
    // overridden methods calling the MongoDB external server
    function EngineRetrieve(TableModelIndex: integer; ID: integer): RawUTF8; override;
    function EngineList(const SQL: RawUTF8; ForceAJAX: Boolean=false; ReturnedRowCount: PPtrInt=nil): RawUTF8; override;
    function EngineAdd(TableModelIndex: integer; const SentData: RawUTF8): integer; override;
    function EngineUpdate(TableModelIndex, ID: integer; const SentData: RawUTF8): boolean; override;
    function EngineDeleteWhere(TableModelIndex: Integer;const SQLWhere: RawUTF8;
      const IDs: TIntegerDynArray): boolean; override;
    /// TSQLRestServer.URI use it for Static.EngineList to by-pass virtual table
    // - overridden method to handle most potential simple queries, e.g. like
    // $ SELECT Field1,RowID FROM table WHERE RowID=... AND/OR/NOT Field2=
    // - ORM field names into mapped MongoDB external field names
    // - handle statements to avoid slow virtual table loop over all rows, like
    // $ SELECT count(*) FROM table
    function AdaptSQLForEngineList(var SQL: RawUTF8): boolean; override;
    // BLOBs should be access directly, not through slower JSON Base64 encoding
    function EngineRetrieveBlob(TableModelIndex, aID: integer;
      BlobField: PPropInfo; out BlobData: TSQLRawBlob): boolean; override;
    function EngineUpdateBlob(TableModelIndex, aID: integer;
      BlobField: PPropInfo; const BlobData: TSQLRawBlob): boolean; override;
    // overriden method returning TRUE for next calls to EngineAdd/Update/Delete
    // will properly handle operations until InternalBatchStop is called
    function InternalBatchStart(Method: TSQLURIMethod): boolean; override;
    // internal method called by TSQLRestServer.RunBatch() to process fast
    // BULK sending to remote MongoDB database
    procedure InternalBatchStop; override;
  public
    /// initialize the direct access to the MongoDB collection
    // - all filename/binary parameters are ignored here
    // - in practice, just call the other reintroduced constructor, supplying
    // a TMongoDatabase instance
    constructor Create(aClass: TSQLRecordClass; aServer: TSQLRestServer;
      const aFileName: TFileName = ''; aBinaryFile: boolean=false); overload; override;
    /// release used memory
    destructor Destroy; override;

     /// overriden method for one single update call to the MongoDB server
    function UpdateBlobFields(Value: TSQLRecord): boolean; override;
     /// overriden method for one single read call to the MongoDB server
    function RetrieveBlobFields(Value: TSQLRecord): boolean; override;
    /// get the row count of a specified table
    // - return -1 on error
    // - return the row count of the table on success
    function TableRowCount(Table: TSQLRecordClass): integer; override;
    /// check if there is some data rows in a specified table
    function TableHasRows(Table: TSQLRecordClass): boolean; override;
    /// delete a row, calling the current MongoDB server
    // - made public since a TSQLRestStorage instance may be created
    // stand-alone, i.e. without any associated Model/TSQLRestServer
    function EngineDelete(TableModelIndex, ID: integer): boolean; override;
    /// create one index for all specific FieldNames at once
    function CreateSQLMultiIndex(Table: TSQLRecordClass; const FieldNames: array of RawUTF8;
      Unique: boolean; IndexName: RawUTF8=''): boolean; override;

    /// drop the whole table content
    // - but you can still add items to it - whereas Collection.Drop would
    // trigger GPF issues
    procedure Drop;
    /// the associated MongoDB collection instance
    property Collection: TMongoCollection read fCollection;
  end;


/// creates and register a static class on the Server-side to let a given
// ORM class be stored on a remote MongoDB server
// - will associate the supplied class with a MongoDB collection for a
// specified MongoDB database
// - to be called before Server.CreateMissingTables
// - by default, the collection name will match TSQLRecord.SQLTableName, but
// you can customize it with the corresponding parameter
// - the TSQLRecord.ID (RowID) field is always mapped to MongoDB's _id field
// - after registration, you can tune the field-name mapping by calling
// ! aModel.Props[aClass].ExternalDB.MapField(..)
// (just a regular external DB as defined in mORMotDB.pas unit) - it may be
// a good idea to use short field names on MongoDB side, to reduce the space
// used for storage (since they will be embedded within the document data)
// - it will return the corresponding TSQLRestStorageMongoDB instance -
// you can access later to it and its associated collection e.g. via:
// ! (aServer.StaticDataServer[TSQLMyTable] as TSQLRestStorageMongoDB)
function StaticMongoDBRegister(aClass: TSQLRecordClass; aServer: TSQLRestServer;
  aMongoDatabase: TMongoDatabase; aMongoCollectionName: RawUTF8=''): TSQLRestStorageMongoDB;


implementation

function StaticMongoDBRegister(aClass: TSQLRecordClass; aServer: TSQLRestServer;
  aMongoDatabase: TMongoDatabase; aMongoCollectionName: RawUTF8=''): TSQLRestStorageMongoDB;
var Props: TSQLModelRecordProperties;
begin
  result := nil;
  if (aServer=nil) or (aClass=nil) or (aMongoDatabase=nil) then
    exit; // avoid GPF
  Props := aServer.Model.Props[aClass];
  if Props=nil then
    exit; // if aClass is not part of the model
  if aMongoCollectionName='' then
    aMongoCollectionName := Props.Props.SQLTableName;
  Props.ExternalDB.Init(Props,aMongoCollectionName,
    aMongoDatabase.CollectionOrCreate[aMongoCollectionName]);
  Props.ExternalDB.MapField('ID','_id');
  result := (aServer.StaticDataCreate(aClass,'',false,TSQLRestStorageMongoDB)
    as TSQLRestStorageMongoDB);
end;


{ TSQLRestStorageMongoDB }

constructor TSQLRestStorageMongoDB.Create(aClass: TSQLRecordClass;
  aServer: TSQLRestServer; const aFileName: TFileName;
  aBinaryFile: boolean);
var F: integer;
begin
  inherited;
  if fStoredClassProps=nil then
    raise EORMMongoDBException.CreateFmt(
      'StoredClassProps needed for %s',[StoredClassRecordProps.SQLTableName]);
  // ConnectionProperties should have been set in StaticMongoDBRegister()
  fCollection := fStoredClassProps.ExternalDB.ConnectionProperties as TMongoCollection;
  BSONProjectionSet(fBSONProjectionSimpleFields,true,
    fStoredClassRecordProps.SimpleFieldsBits[soSelect],nil);
  BSONProjectionSet(fBSONProjectionBlobFields,false,
    fStoredClassRecordProps.BlobFieldsBits,@fBSONProjectionBlobFieldsNames);
  if not IsZero(fIsUnique) then
    for F := 0 to fStoredClassRecordProps.Fields.Count do
      if F in fIsUnique then
        fCollection.EnsureIndex(
          [fStoredClassProps.ExternalDB.FieldNames[f]],true,true);

end;

function TSQLRestStorageMongoDB.BSONProjectionSet(var Projection: variant;
  WithID: boolean; const Fields: TSQLFieldBits; ExtFieldNames: PRawUTF8DynArray): integer;
var i,n: integer;
    Start: cardinal;
    W: TBSONWriter;
begin
  W := TBSONWriter.Create(TRawByteStringStream);
  try
    Start := W.BSONDocumentBegin;
    if withID then
      result := 1 else
      result := 0;
    W.BSONWrite(fStoredClassProps.ExternalDB.RowIDFieldName,result);
    for i := 0 to fStoredClassProps.Props.Fields.Count-1 do
      if i in Fields then begin
        W.BSONWrite(fStoredClassProps.ExternalDB.FieldNames[i],1);
        inc(result);
      end;
    W.BSONDocumentEnd(Start);
    W.ToBSONVariant(Projection);
    if ExtFieldNames<>nil then
    with fStoredClassProps.ExternalDB do begin
      SetLength(ExtFieldNames^,result);
      if WithID then begin
        ExtFieldNames^[0] := RowIDFieldName;
        n := 1;
      end else
        n := 0;
      for i := 0 to fStoredClassProps.Props.Fields.Count-1 do
        if i in Fields then begin
          ExtFieldNames^[n] := FieldNames[i];
          inc(n);
        end;
    end;
  finally
    W.Free;
  end;
end;

function TSQLRestStorageMongoDB.CreateSQLMultiIndex(
  Table: TSQLRecordClass; const FieldNames: array of RawUTF8;
  Unique: boolean; IndexName: RawUTF8): boolean;
begin
  result := false;
  if (self=nil) or (fCollection=nil) or (Table<>fStoredClass) then
    exit;
  fCollection.EnsureIndex(FieldNames,true,Unique);
end;

procedure TSQLRestStorageMongoDB.Drop;
var DB: TMongoDatabase;
    CollName: RawUTF8;
begin
  DB := Collection.Database;
  CollName := Collection.Name;
  Collection.Drop;
  fCollection := DB.CollectionOrCreate[CollName];
  fEngineLastID := 0;
end;

destructor TSQLRestStorageMongoDB.Destroy;
begin
  inherited;
  FreeAndNil(fBatchWriter);
end;

function TSQLRestStorageMongoDB.TableHasRows(
  Table: TSQLRecordClass): boolean;
begin
  result := TableRowCount(Table)>0;
end;

function TSQLRestStorageMongoDB.TableRowCount(
  Table: TSQLRecordClass): integer;
begin
  if (fCollection=nil) or (Table<>fStoredClass) then
    result := 0 else
    result := fCollection.Count;
end;

function TSQLRestStorageMongoDB.EngineNextID: Integer;
procedure ComputeMax_ID;
var res: variant;
begin
  res := fCollection.AggregateDoc('{$group:{_id:null,max:{$max:"$_id"}}}',[]);
  if DocVariantType.IsOfType(res) then
    fEngineLastID := VariantToIntegerDef(res.max,0);
end;
begin
  if fEngineLastID=0 then
    ComputeMax_ID;
  result := InterlockedIncrement(fEngineLastID);
end;

function TSQLRestStorageMongoDB.DocFromJSON(const JSON: RawUTF8;
  Occasion: TSQLOccasion; var Doc: TDocVariantData): integer;
var i, ndx: integer;
    blob: RawByteString;
    info: TSQLPropInfo;
    typenfo: pointer;
    js: RawUTF8;
    MissingID: boolean;
    V: PVarData;
begin
  doc.InitJSON(JSON,[dvoValueCopiedByReference]);
  if (doc.Kind<>dvObject) and (Occasion<>soInsert) then
    raise EORMMongoDBException.Create('Invalid JSON context');
  if not (Occasion in [soInsert,soUpdate]) then
    raise EORMMongoDBException.CreateFmt('DocFromJSON(%s)',[
      GetEnumName(TypeInfo(TSQLOccasion),ord(Occasion))^]);
  MissingID := true;
  for i := doc.Count-1 downto 0 do // downwards for doc.Delete(i) below
    if IsRowID(pointer(doc.Names[i])) then begin
      MissingID := false;
      doc.Names[i] := fStoredClassProps.ExternalDB.RowIDFieldName;
      VariantToInteger(doc.Values[i],result);
      if Occasion=soUpdate then
        doc.Delete(i); // update does not expect any $set:{_id:..}
    end else begin
      ndx := fStoredClassProps.Props.Fields.IndexByName(doc.Names[i]);
      if ndx<0 then
        raise EORMMongoDBException.CreateFmt('Unkwnown field name "%s"',[doc.Names[i]]);
      doc.Names[i] := fStoredClassProps.ExternalDB.FieldNames[ndx];
      info := fStoredClassProps.Props.Fields.List[ndx];
      V := @Doc.Values[i];
      case V^.VType of
      varInteger: // doc.InitJSON/GetVariantFromJSON store 0,1 as varInteger
      case info.SQLFieldType of
        sftBoolean:
          Variant(V^) := boolean(V^.VInteger);
      end;
      varString: // handle some TEXT values
      case info.SQLFieldType of
        sftDateTime: // store ISO-8601 text as MongoDB date/time
          Variant(V^) := Iso8601ToDateTime(RawByteString(V^.VAny));
        {$ifdef PUBLISHRECORD}sftBlobRecord,{$endif}
        sftBlob, sftBlobCustom: // store Base64-encoded BLOB as binary
          BSONVariantType.FromBinary(BlobToTSQLRawBlob(RawByteString(V^.VAny)),
            bbtGeneric,Variant(V^));
        sftBlobDynArray: begin // store dynamic array as object (if has any JSON)
          blob := BlobToTSQLRawBlob(RawByteString(V^.VAny));
          if blob='' then
            SetVariantNull(Variant(V^)) else begin
            typenfo := (info as TSQLPropInfoRTTIDynArray).PropInfo^.PropType^;
            if typenfo=TypeInfo(TByteDynArray) then
              js := '' else // embedded BLOB type stored as BSON binary
              js := DynArraySaveJSON(typenfo,blob);
            if (js<>'') and (PInteger(js)^ and $00ffffff<>JSON_BASE64_MAGIC) then
              BSONVariantType.FromJSON(pointer(js),Variant(V^)) else
              BSONVariantType.FromBinary(blob,bbtGeneric,Variant(V^));
          end;
        end;
        end;
        // sftObject,sftVariant were already converted to object from JSON
      end;
    end;
  if (Occasion=soInsert) and MissingID then begin
    result := EngineNextID;
    doc.AddValue(fStoredClassProps.ExternalDB.RowIDFieldName,result);
  end;
  if doc.Kind<>dvObject then
    raise EORMMongoDBException.Create('Invalid JSON context');
end;

function TSQLRestStorageMongoDB.EngineAdd(TableModelIndex: integer; 
  const SentData: RawUTF8): integer;
var doc: TDocVariantData;
begin
  if (fCollection=nil) or (TableModelIndex<0) or 
    (fModel.Tables[TableModelIndex]<>fStoredClass) then
    result := 0 else
    try
      result := DocFromJSON(SentData,soInsert,Doc);
      if fBatchMethod<>mNone then
        if (fBatchMethod<>mPOST) or (fBatchWriter=nil) then
          result := 0 else begin
          inc(fBatchCount);
          fBatchWriter.BSONWriteDoc(doc);
        end else begin
        fCollection.Insert([variant(doc)]);
        if Owner<>nil then
          Owner.InternalUpdateEvent(seAdd,fStoredClass,result,nil);
      end;
    except
      result := 0;
    end;
end;

function TSQLRestStorageMongoDB.EngineUpdate(TableModelIndex, ID: integer;
  const SentData: RawUTF8): boolean;
var doc: TDocVariantData;
    query,update: variant; // use explicit TBSONVariant for type safety
begin
  if (fCollection=nil) or (ID<=0) or
     (TableModelIndex<0) or (Model.Tables[TableModelIndex]<>fStoredClass) then
    result := false else
    try
      DocFromJSON(SentData,soUpdate,Doc);
      query := BSONVariant(['_id',ID]);
      update := BSONVariant(['$set',variant(Doc)]);
      fCollection.Update(query,update);
      if Owner<>nil then
        Owner.InternalUpdateEvent(seUpdate,fStoredClass,ID,nil);
      result := true;
    except
      result := false;
    end;
end;

function TSQLRestStorageMongoDB.EngineUpdateBlob(TableModelIndex, aID: integer;
  BlobField: PPropInfo; const BlobData: TSQLRawBlob): boolean;
var query,update,blob: variant; // use explicit TBSONVariant for type safety
    FieldName: RawUTF8;
    AffectedField: TSQLFieldBits;
begin
  if (fCollection=nil) or (BlobField=nil) or (aID<=0) or
     (TableModelIndex<0) or (Model.Tables[TableModelIndex]<>fStoredClass) then
    result := false else
    try
      query := BSONVariant(['_id',aID]);
      FieldName := fStoredClassProps.ExternalDB.InternalToExternal(BlobField^.Name);
      BSONVariantType.FromBinary(BlobData,bbtGeneric,blob);
      update := BSONVariant(['$set',BSONVariant([FieldName,blob])]);
      fCollection.Update(query,update);
      if Owner<>nil then begin
        fStoredClassRecordProps.FieldIndexsFromBlobField(BlobField,AffectedField);
        Owner.InternalUpdateEvent(seUpdateBlob,fStoredClass,aID,@AffectedField);
      end;
      result := true;
    except
      result := false;
    end;
end;

function TSQLRestStorageMongoDB.UpdateBlobFields(
  Value: TSQLRecord): boolean;
var query,blob: variant;
    update: TDocVariantData;
    info: TSQLPropInfo;
    blobRaw: RawByteString;
    aID, f: integer;
begin
  result := false;
  if (fCollection=nil) or (PSQLRecordClass(Value)^<>fStoredClass) or (Value=nil) then
    exit;
  aID := Value.ID;
  if aID<=0 then
    exit;
  query := BSONVariant(['_id',aID]);
  update.Init(JSON_OPTIONS[true]);
  for f := 0 to fStoredClassRecordProps.Fields.Count-1 do begin
    info := fStoredClassRecordProps.Fields.List[f];
    if info.SQLFieldType=sftBlob then begin
      (info as TSQLPropInfoRTTIRawBlob).GetBlob(Value,blobRaw);
      BSONVariantType.FromBinary(blobRaw,bbtGeneric,blob);
      update.AddValue(fStoredClassProps.ExternalDB.FieldNames[f],blob);
    end;
  end;
  if update.Count>0 then
    try
      fCollection.Update(query,BSONVariant(['$set',variant(update)]));
      if Owner<>nil then
        Owner.InternalUpdateEvent(seUpdateBlob,fStoredClass,aID,
          @fStoredClassRecordProps.BlobFieldsBits);
      result := true;
    except
      result := false;
    end;
end;

function TSQLRestStorageMongoDB.EngineDelete(TableModelIndex, ID: integer): boolean;
begin
  result := false;
  if (fCollection<>nil) and (TableModelIndex>=0) and
     (Model.Tables[TableModelIndex]=fStoredClass) and (ID>0) then
  try
    if fBatchMethod<>mNone then
      if fBatchMethod<>mDelete then
        exit else
        AddInteger(fBatchIDs,fBatchCount,ID) else begin
      fCollection.RemoveOne(ID);
      if Owner<>nil then
        Owner.InternalUpdateEvent(seDelete,fStoredClass,ID,nil);
    end;
    result := true;
  except
    result := false;
  end;
end;

function TSQLRestStorageMongoDB.EngineDeleteWhere(TableModelIndex: Integer;
  const SQLWhere: RawUTF8; const IDs: TIntegerDynArray): boolean;
var i: integer;
begin // here we use the pre-computed IDs[]
  result := false;
  if (fCollection<>nil) and (TableModelIndex>=0) and
     (Model.Tables[TableModelIndex]=fStoredClass) and (IDs<>nil) then
    try
      if Owner<>nil then // notify BEFORE deletion
      for i := 0 to high(IDs) do
        Owner.InternalUpdateEvent(seDelete,fStoredClass,IDs[i],nil);
      fCollection.Remove(BSONVariant(
        ['_id',BSONVariant(['$in',BSONVariantFromIntegers(IDs)])]));
      result := true;
    except
      result := false;
    end;
end;

procedure TSQLRestStorageMongoDB.JSONFromDoc(var doc: TDocVariantData;
  var result: RawUTF8);
var i: integer;
    name: RawUTF8;
    W: TTextWriter;
begin
  if (doc.VarType<>DocVariantType.VarType) or (doc.Kind<>dvObject) or (doc.Count=0) then begin
    result := '';
    exit;
  end;
  W := TTextWriter.CreateOwnedStream;
  try
    W.Add('{');
    for i := 0 to doc.Count-1 do begin
      name := fStoredClassProps.ExternalDB.ExternalToInternalOrNull(doc.Names[i]);
      if name='' then
        raise EORMMongoDBException.CreateFmt('Unknown field name "%s" for table %s',
          [doc.Names[i],fStoredClassRecordProps.SQLTableName]);
      W.AddFieldName(pointer(name),Length(name));
      W.AddVariantJSON(doc.Values[i],twJSONEscape);
      W.Add(',');
    end;
    W.CancelLastComma;
    W.Add('}');
    W.SetText(result);
  finally
    W.Free;
  end;
end;

function TSQLRestStorageMongoDB.EngineRetrieve(TableModelIndex,
  ID: integer): RawUTF8;
var doc: variant;
begin
  result := '';
  if (fCollection=nil) or (ID<=0) then
    exit;
  doc := fCollection.FindDoc(BSONVariant(['_id',ID]),fBSONProjectionSimpleFields,1);
  JSONFromDoc(TDocVariantData(doc),result);
end;

function TSQLRestStorageMongoDB.EngineRetrieveBlob(TableModelIndex, aID: integer;
  BlobField: PPropInfo; out BlobData: TSQLRawBlob): boolean;
var doc: variant;
    data: TVarData;
    FieldName: RawUTF8;
begin
  if (fCollection=nil) or (BlobField=nil) or (aID<=0) or
     (TableModelIndex<0) or (Model.Tables[TableModelIndex]<>fStoredClass) then
    result := false else
    try
      FieldName := fStoredClassProps.ExternalDB.InternalToExternal(BlobField^.Name);
      doc := fCollection.FindDoc(BSONVariant(['_id',aID]),BSONVariant([FieldName,1]),1);
      if DocVariantType.IsOfType(doc) and
         DocVariantData(doc)^.GetVarData(FieldName,data) then
        BSONVariantType.ToBlob(variant(data),RawByteString(BlobData));
      result := true;
    except
      result := false;
    end;
end;

function TSQLRestStorageMongoDB.RetrieveBlobFields(
  Value: TSQLRecord): boolean;
var aID, f: Integer;
    doc: variant;
    docv: PDocVariantData;
    blob: TVarData;
    blobRaw: RawByteString;
begin
  result := false;
  if (fCollection=nil) or (PSQLRecordClass(Value)^<>fStoredClass) or (Value=nil) then
    exit;
  aID := Value.ID;
  if aID<=0 then
    exit;
  try
    doc := fCollection.FindDoc(BSONVariant(['_id',aID]),fBSONProjectionBlobFields,1);
    docv := DocVariantData(doc);
    for f := 0 to high(fStoredClassRecordProps.BlobFields) do begin
      if docv^.Names[f]=fBSONProjectionBlobFieldsNames[f] then
        BSONVariantType.ToBlob(docv^.Values[f],blobRaw) else
      if docv^.GetVarData(fBSONProjectionBlobFieldsNames[f],blob) then
        BSONVariantType.ToBlob(variant(blob),blobRaw) else
        raise EORMMongoDBException.CreateFmt('Field "%s" not found',
          [fBSONProjectionBlobFieldsNames[f]]);
      (fStoredClassRecordProps.BlobFields[f] as TSQLPropInfoRTTIRawBlob).
        SetBlob(Value,blobRaw);
    end;
    result := true;
  except
    result := false;
  end;
end;

function TSQLRestStorageMongoDB.AdaptSQLForEngineList(
  var SQL: RawUTF8): boolean;
begin
  result := true; // we do not have any Virtual Table yet -> always accept
end;

function TSQLRestStorageMongoDB.GetJSONValues(const Res: TBSONDocument;
  const extFieldNames: TRawUTF8DynArray; W: TJSONSerializer): integer;
var col, colCount, colFound: integer;
    bson: PByte;
    row: TBSONElement;
    item: array of TBSONElement;
function itemFind(const aName: RawUTF8): integer;
begin
  if aName<>'' then
    for result := 0 to colCount-1 do
      with item[result] do
        if IdemPropNameU(aName,Name,NameLen) then
          exit;
  raise EORMMongoDBException.CreateFmt('Unexpected field "%s" in row',[aName]);
end;
begin
  result := 0; // number of data rows in JSON output
  bson := pointer(Res);
  if W.Expand then
    W.Add('[');
  if Res<>'' then begin
    BSONParseLength(bson,length(Res));
    colCount := length(extFieldNames);
    if colCount<>length(W.ColNames) then
      raise EORMMongoDBException.Create('Invalid GetJSONValues() call');
    SetLength(item,colCount);
    while row.FromNext(bson) do begin
      // retrieve all values of this BSON document into item[]
      if row.Kind<>betDoc then
        raise EORMMongoDBException.CreateFmt('Invalid row %d',[ord(row.Kind)]);
      col := 0;
      while (row.Data.DocList^<>byte(betEof)) and (col<colCount) and
            item[col].FromNext(row.Data.DocList) do
        inc(col);
      if col<>colCount then
        raise EORMMongoDBException.CreateFmt('Invalid field count %d',[col]);
      // convert this BSON document as JSON, following expected column order
      if W.Expand then
        W.Add('{');
      for col := 0 to colCount-1 do begin
        if W.Expand then
          W.AddString(W.ColNames[col]);
        with item[col] do
          if IdemPropNameU(extFieldNames[col],Name,NameLen) then
            colFound := col else
            colFound := itemFind(extFieldNames[col]);
        item[colFound].AddMongoJSON(W,modNoMongo);
        W.Add(',');
      end;
      W.CancelLastComma;
      if W.Expand then
        W.Add('}',',') else
        W.Add(',');
      inc(result);
    end;
  end;
  if (result=0) and W.Expand then begin
    // we want the field names at least, even with no data
    W.Expand := false; //  {"fieldCount":2,"values":["col1","col2"]}
    W.CancelAll;
    fStoredClassRecordProps.SetJSONWriterColumnNames(W,0);
  end;
  W.EndJSONObject(0,result);
end;

function TSQLRestStorageMongoDB.EngineList(const SQL: RawUTF8;
  ForceAJAX: Boolean; ReturnedRowCount: PPtrInt): RawUTF8;
var W: TJSONSerializer;
    MS: TRawByteStringStream;
    Query,Projection: variant;
    Res: TBSONDocument;
    ResCount: PtrInt;
    extFieldNames: TRawUTF8DynArray;
    Stmt: TSynTableStatement;
procedure ComputeQuery;
const // see http://docs.mongodb.org/manual/reference/operator/query
  QUERY_OPS: array[opNotEqualTo..high(TSynTableStatementOperator)] of RawUTF8 = (
    '$ne','$lt','$lte','$gt','$gte','$in');
var QueryFieldName: RawUTF8;
begin
  if Stmt.WhereField<0 then begin
    SetVariantNull(Query);
    exit;
  end;
  QueryFieldName := fStoredClassProps.ExternalDB.FieldNameByIndex(Stmt.WhereField-1);
  if Stmt.WhereOperator=opEqualTo then
    Query := BSONVariant([QueryFieldName,Stmt.WhereValueVariant]) else
    Query := BSONVariant([QueryFieldName,
      BSONVariant([QUERY_OPS[Stmt.WhereOperator],Stmt.WhereValueVariant])]);
end;
procedure SetCount(aCount: integer);
begin
  result := FormatUTF8('[{"Count(*)":%}]'#$A,[aCount]);
  ResCount := 1;
end;
begin // same logic as in TSQLRestStorageInMemory.EngineList()
  ResCount := 0;
  if self=nil then begin
    result := '';
    exit;
  end;
  StorageLock(false);
  try
    if IdemPropNameU(fBasicSQLCount,SQL) then
      SetCount(TableRowCount(fStoredClass)) else
    if IdemPropNameU(fBasicSQLHasRows[false],SQL) or
       IdemPropNameU(fBasicSQLHasRows[true],SQL) then
      if TableRowCount(fStoredClass)=0 then begin
        result := '{"fieldCount":1,"values":["RowID"]}'#$A;
        ResCount := 0;
      end else begin // return one row with fake ID=1
        result := '[{"RowID":1}]'#$A;
        ResCount := 1;
      end else begin
      Stmt := TSynTableStatement.Create(SQL,
        fStoredClassRecordProps.Fields.IndexByName,
        fStoredClassRecordProps.SimpleFieldsBits[soSelect]);
      try
        if (Stmt.WhereValue='') or
           not IdemPropNameU(Stmt.TableName,fStoredClassRecordProps.SQLTableName) then begin
          // invalid request -> return '' to mark error
          result := '';
          exit;
        end;
        if Stmt.WhereField=SYNTABLESTATEMENTWHERECOUNT then
          // was "SELECT Count(*) FROM TableName;"
          SetCount(TableRowCount(fStoredClass)) else
        if IsZero(Stmt.Fields) and not Stmt.WithID then begin
          if Stmt.IsSelectCountWhere then
            // was "SELECT Count(*) FROM TableName WHERE ..."
            if Stmt.WhereField<0 then
              SetCount(TableRowCount(fStoredClass)) else begin
              ComputeQuery;
              SetCount(fCollection.FindCount(Query));
            end;
          exit; // also invalid "SELECT FROM Table"
        end;
        // save rows as JSON, with appropriate search according to Where* arguments
        ComputeQuery;
        BSONProjectionSet(Projection,Stmt.WithID,Stmt.Fields,@extFieldNames);
        if Stmt.FoundLimit=0 then
          Stmt.FoundLimit := maxInt;
        Res := fCollection.FindBSON(Query,Projection,Stmt.FoundLimit,Stmt.FoundOffset);
        MS := TRawByteStringStream.Create;
        try
          W := fStoredClassRecordProps.CreateJSONWriter(
            MS,ForceAJAX or (Owner=nil) or not Owner.NoAJAXJSON,Stmt.withID,Stmt.Fields,0);
          try
            ResCount := GetJSONValues(Res,extFieldNames,W);
            result := MS.DataString;
          finally
            W.Free;
          end;
        finally
          MS.Free;
        end;
      finally
        Stmt.Free;
      end;
    end;
  finally
    StorageUnLock;
  end;
  if ReturnedRowCount<>nil then
    ReturnedRowCount^ := ResCount;
end;
    
function TSQLRestStorageMongoDB.InternalBatchStart(
  Method: TSQLURIMethod): boolean;
begin
  result := false; // means BATCH mode not supported
  if (self<>nil) and (method in [mPOST,mDELETE]) then begin
    StorageLock(true); // protected by try..finally in TSQLRestServer.RunBatch
    try
      if (fBatchMethod<>mNone) or (fBatchWriter<>nil) then
        raise EORMException.Create('InternalBatchStop should have been called');
      fBatchCount := 0;
      fBatchMethod := Method;
      case Method of
      mPOST: // POST=ADD=INSERT -> EngineAdd() will add to fBatchWriter
        fBatchWriter := TBSONWriter.Create(TRawByteStringStream);
      //mDELETE: // EngineDelete() will add deleted ID to fBatchIDs[]
      end;
      result := true; // means BATCH mode is supported
    finally
      if not result then
        StorageUnLock;
    end;
  end;
end;

procedure TSQLRestStorageMongoDB.InternalBatchStop;
var docs: TBSONDocument;
begin
  try
    case fBatchMethod of
    mPOST: begin // Add/Insert
      if fBatchWriter.TotalWritten=0 then
        exit; // nothing to add
      fBatchWriter.ToBSONDocument(docs);
      fCollection.Insert(docs);
    end;
    mDELETE: begin
      SetLength(fBatchIDs,fBatchCount);
      fCollection.Remove(BSONVariant(
        ['_id',BSONVariant(['$in',BSONVariantFromIntegers(fBatchIDs)])]));
    end;
    else
      raise EORMException.CreateFmt('%s.BatchMethod=%d',
        [fStoredClassRecordProps.SQLTableName,ord(fBatchMethod)]);
    end;
  finally
    FreeAndNil(fBatchWriter);
    fBatchIDs := nil;
    fBatchCount := 0;
    fBatchMethod := mNone;
    StorageUnLock;
  end;
end;

end.
