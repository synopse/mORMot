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
  - BATCH mode, using fast MongoDB bulk insert
  - complex WHERE clause with a MongoDB Query object instead of SQL syntax
  - SQLite3 Virtual Table mode, for full integration with mORMotDB - certainly
    in a dedicated mORMotDBMongoDB unit 

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
  TSQLRestServerStaticMongoDB = class(TSQLRestServerStatic)
  protected
    /// the associated MongoDB collection
    fCollection: TMongoCollection;
    fEngineLastID: integer;
    fBSONProjectionSimpleFields: variant;
    function EngineNextID: Integer;
    function DocFromJSON(const JSON: RawUTF8; Occasion: TSQLOccasion; 
      var Doc: TDocVariantData): integer;
    procedure JSONFromDoc(var doc: Variant; var result: RawUTF8);
    // overridden methods calling the MongoDB external server
    function EngineRetrieve(TableModelIndex: integer; ID: integer): RawUTF8; override;
    function EngineList(const SQL: RawUTF8; ForceAJAX: Boolean=false; ReturnedRowCount: PPtrInt=nil): RawUTF8; override;
    function EngineAdd(Table: TSQLRecordClass; const SentData: RawUTF8): integer; override;
    function EngineUpdate(Table: TSQLRecordClass; ID: integer; const SentData: RawUTF8): boolean; override;
    function EngineDeleteWhere(Table: TSQLRecordClass; const SQLWhere: RawUTF8;
      const IDs: TIntegerDynArray): boolean; override;
    /// TSQLRestServer.URI use it for Static.EngineList to by-pass virtual table
    // - overridden method to handle most potential simple queries, e.g. like
    // $ SELECT Field1,RowID FROM table WHERE RowID=... AND/OR/NOT Field2=
    // - ORM field names into mapped MongoDB external field names
    // - handle statements to avoid slow virtual table loop over all rows, like
    // $ SELECT count(*) FROM table
    function AdaptSQLForEngineList(var SQL: RawUTF8): boolean; override;
    // BLOBs should be access directly, not through slower JSON Base64 encoding
    function EngineRetrieveBlob(Table: TSQLRecordClass; aID: integer;
      BlobField: PPropInfo; out BlobData: TSQLRawBlob): boolean; override;
    function EngineUpdateBlob(Table: TSQLRecordClass; aID: integer;
      BlobField: PPropInfo; const BlobData: TSQLRawBlob): boolean; override;
    function BSONProjectionSet(var Projection: variant; const Fields: TSQLFieldBits;
      ExtFieldNames: PRawUTF8DynArray): integer;
  public
    /// initialize the direct access to the MongoDB collection
    // - all filename/binary parameters are ignored here
    // - in practice, just call the other reintroduced constructor, supplying
    // a TMongoDatabase instance
    constructor Create(aClass: TSQLRecordClass; aServer: TSQLRestServer;
      const aFileName: TFileName = ''; aBinaryFile: boolean=false); overload; override;
    /// release used memory
    destructor Destroy; override;

    /// drop the whole table content
    // - but you can still add items to it - whereas Collection.Drop would
    // trigger GPF issues
    procedure Drop;
    /// get the row count of a specified table
    // - return -1 on error
    // - return the row count of the table on success
    function TableRowCount(Table: TSQLRecordClass): integer; override;
    /// check if there is some data rows in a specified table
    function TableHasRows(Table: TSQLRecordClass): boolean; override;
    /// delete a row, calling the current MongoDB server
    // - made public since a TSQLRestServerStatic instance may be created
    // stand-alone, i.e. without any associated Model/TSQLRestServer
    function EngineDelete(Table: TSQLRecordClass; ID: integer): boolean; override;
    /// create one index for all specific FieldNames at once
    function CreateSQLMultiIndex(Table: TSQLRecordClass; const FieldNames: array of RawUTF8;
      Unique: boolean; IndexName: RawUTF8=''): boolean; override;

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
// - after registration, you can tune the field-name mapping by calling
// ! aModel.Props[aClass].ExternalDB.MapField(..)
// (just a regular external DB as defined in mORMotDB.pas unit)
function StaticMongoDBRegister(aClass: TSQLRecordClass; aServer: TSQLRestServer;
  aMongoDatabase: TMongoDatabase; aMongoCollectionName: RawUTF8=''): boolean;


implementation

function StaticMongoDBRegister(aClass: TSQLRecordClass; aServer: TSQLRestServer;
  aMongoDatabase: TMongoDatabase; aMongoCollectionName: RawUTF8=''): boolean;
var Props: TSQLModelRecordProperties;
begin
  result := False;
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
  aServer.StaticDataCreate(aClass,'',false,TSQLRestServerStaticMongoDB);
  result := true;
end;


{ TSQLRestServerStaticMongoDB }

constructor TSQLRestServerStaticMongoDB.Create(aClass: TSQLRecordClass;
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
  BSONProjectionSet(fBSONProjectionSimpleFields,
    fStoredClassRecordProps.SimpleFieldsBits[soSelect],nil);
  if not IsZero(fIsUnique) then
    for F := 0 to fStoredClassRecordProps.Fields.Count do
      if F in fIsUnique then
        fCollection.EnsureIndex(
          [fStoredClassProps.ExternalDB.FieldNames[f]],true,true);
end;

function TSQLRestServerStaticMongoDB.BSONProjectionSet(var Projection: variant;
  const Fields: TSQLFieldBits; ExtFieldNames: PRawUTF8DynArray): integer;
var i,n: integer;
    Start: cardinal;
    W: TBSONWriter;
begin
  result := 1; // _id is always part of the MongoDB projected fields
  W := TBSONWriter.Create(TRawByteStringStream);
  try
    Start := W.BSONDocumentBegin;
    W.BSONWrite(fStoredClassProps.ExternalDB.RowIDFieldName,1);
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
      ExtFieldNames^[0] := RowIDFieldName;
      n := 1;
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

function TSQLRestServerStaticMongoDB.CreateSQLMultiIndex(
  Table: TSQLRecordClass; const FieldNames: array of RawUTF8;
  Unique: boolean; IndexName: RawUTF8): boolean;
begin
  result := false;
  if (self=nil) or (fCollection=nil) or (Table<>fStoredClass) then
    exit;
  fCollection.EnsureIndex(FieldNames,true,Unique);
end;

procedure TSQLRestServerStaticMongoDB.Drop;
var DB: TMongoDatabase;
    CollName: RawUTF8;
begin
  DB := Collection.Database;
  CollName := Collection.Name;
  Collection.Drop;
  fCollection := DB.CollectionOrCreate[CollName];
end;

destructor TSQLRestServerStaticMongoDB.Destroy;
begin
  inherited;
end;

function TSQLRestServerStaticMongoDB.TableHasRows(
  Table: TSQLRecordClass): boolean;
begin
  result := TableRowCount(Table)>0;
end;

function TSQLRestServerStaticMongoDB.TableRowCount(
  Table: TSQLRecordClass): integer;
begin
  if (fCollection=nil) or (Table<>fStoredClass) then
    result := 0 else
    result := fCollection.Count;
end;

function TSQLRestServerStaticMongoDB.EngineNextID: Integer;
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

function TSQLRestServerStaticMongoDB.DocFromJSON(const JSON: RawUTF8;
  Occasion: TSQLOccasion; var Doc: TDocVariantData): integer;
var i, ndx: integer;
    blob: RawByteString;
    info: TSQLPropInfo;
    js: RawUTF8;
    MissingID: boolean;
begin
  doc.InitJSON(JSON,[dvoValueCopiedByReference]);
  if doc.Kind<>dvObject then
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
      case info.SQLFieldType of
        sftDateTime: // store as MongoDB date/time
          doc.Values[i] := Iso8601ToDateTime(VariantToUTF8(Doc.Values[i]));
        {$ifdef PUBLISHRECORD}sftBlobRecord,{$endif}
        sftBlob, sftBlobCustom: begin // store BLOB as binary
          blob := VariantToUTF8(doc.Values[i]);
          BSONVariantType.FromBinary(BlobToTSQLRawBlob(pointer(blob)),
            bbtGeneric,doc.Values[i]);
        end;
        sftBlobDynArray: begin // try dynamic array as object from any JSON
          blob := VariantToUTF8(doc.Values[i]);
          js := DynArraySaveJSON(
            (info as TSQLPropInfoRTTIDynArray).PropInfo^.PropType^,
            BlobToTSQLRawBlob(pointer(blob)));
          if (js<>'') and (PInteger(js)^ and $00ffffff<>JSON_BASE64_MAGIC) then
            BSONVariantType.FromJSON(pointer(js),doc.Values[i]) else
            BSONVariantType.FromBinary(blob,bbtGeneric,doc.Values[i]);
        end;
        // sftObject,sftVariant were already converted to object from JSON
      end;
    end;
  if (Occasion=soInsert) and MissingID then begin
    result := EngineNextID;
    doc.AddValue(fStoredClassProps.ExternalDB.RowIDFieldName,result);
  end;
end;

function TSQLRestServerStaticMongoDB.EngineAdd(Table: TSQLRecordClass;
  const SentData: RawUTF8): integer;
var doc: TDocVariantData;
begin
  if (fCollection=nil) or (Table<>fStoredClass) then
    result := 0 else begin
    result := DocFromJSON(SentData,soInsert,Doc);
    try
      fCollection.Insert([variant(doc)]);
    except
      result := 0;
    end;
  end;
end;

function TSQLRestServerStaticMongoDB.EngineUpdate(Table: TSQLRecordClass;
  ID: integer; const SentData: RawUTF8): boolean;
var doc: TDocVariantData;
    query,update: variant; // use explicit TBSONVariant for type safety
begin
  if (fCollection=nil) or (Table<>fStoredClass) or (ID<=0) then
    result := false else begin
    DocFromJSON(SentData,soUpdate,Doc);
    try
      query := BSONVariant(['_id',ID]);
      update := BSONVariant(['$set',variant(Doc)]);
      fCollection.Update(query,update);
      result := true;
    except
      result := false;
    end;
  end;
end;

function TSQLRestServerStaticMongoDB.EngineUpdateBlob(
  Table: TSQLRecordClass; aID: integer; BlobField: PPropInfo;
  const BlobData: TSQLRawBlob): boolean;
var query,update,blob: variant; // use explicit TBSONVariant for type safety
    FieldName: RawUTF8;
begin
  if (fCollection=nil) or (Table<>fStoredClass) or (aID<=0) or (BlobField=nil) then
    result := false else begin
    query := BSONVariant(['_id',aID]);
    FieldName := fStoredClassProps.ExternalDB.InternalToExternal(BlobField^.Name);
    BSONVariantType.FromBinary(BlobData,bbtGeneric,blob);
    update := BSONVariant(['$set',BSONVariant([FieldName,blob])]);
    try
      fCollection.Update(query,update);
      result := true;
    except
      result := false;
    end;
  end;
end;

function TSQLRestServerStaticMongoDB.EngineDelete(Table: TSQLRecordClass;
  ID: integer): boolean;
begin
  if (fCollection=nil) or (Table<>fStoredClass) or (ID<=0) then
    result := false else begin
    try
      fCollection.RemoveOne(ID);
      result := true;
    except
      result := false;
    end;
  end;
end;

function TSQLRestServerStaticMongoDB.EngineDeleteWhere(
  Table: TSQLRecordClass; const SQLWhere: RawUTF8;
  const IDs: TIntegerDynArray): boolean;
begin // here we use the pre-computed IDs[]
  if (fCollection=nil) or (Table<>fStoredClass) or (IDs=nil) then
    result := false else begin
    try
      fCollection.RemoveFmt('{_id:{$in:?}}',[BSONVariantFromIntegers(IDs)]);
      result := true;
    except
      result := false;
    end;
  end;
end;

procedure TSQLRestServerStaticMongoDB.JSONFromDoc(var doc: Variant;
  var result: RawUTF8);
var i: integer;
    name: RawUTF8;
begin
  with TDocVariantData(doc) do
  if (VarType=DocVariantType.VarType) and (Kind=dvObject) then begin
    for i := 0 to Count-1 do begin
      name := fStoredClassProps.ExternalDB.ExternalToInternalOrNull(Names[i]);
      if name='' then
        raise EORMMongoDBException.CreateFmt('Unknown field name "%s" for table %s',
          [Names[i],fStoredClassRecordProps.SQLTableName]);
      Names[i] := name;
    end;
    VariantSaveJSON(doc,twJSONEscape,result);
  end else
    result := '';
end;

function TSQLRestServerStaticMongoDB.EngineRetrieve(TableModelIndex,
  ID: integer): RawUTF8;
var doc: variant;
begin
  result := '';
  if (fCollection=nil) or (ID<=0) then
    exit;
  doc := fCollection.FindDoc(BSONVariant(['_id',ID]),fBSONProjectionSimpleFields,1);
  JSONFromDoc(doc,result);
end;

function TSQLRestServerStaticMongoDB.EngineRetrieveBlob(
  Table: TSQLRecordClass; aID: integer; BlobField: PPropInfo;
  out BlobData: TSQLRawBlob): boolean;
var doc: variant;
    data: TVarData;
    FieldName: RawUTF8;
begin
  if (fCollection=nil) or (Table<>fStoredClass) or (aID<=0) or (BlobField=nil) then
    result := false else begin
    FieldName := fStoredClassProps.ExternalDB.InternalToExternal(BlobField^.Name);
    try
      doc := fCollection.FindDoc(BSONVariant(['_id',aID]),BSONVariant([FieldName,1]),1);
      if DocVariantType.IsOfType(doc) and
         DocVariantData(doc)^.GetVarData(FieldName,data) then
        BSONVariantType.ToBlob(variant(data),RawByteString(BlobData));
      result := true;
    except
      result := false;
    end;
  end;
end;

function TSQLRestServerStaticMongoDB.AdaptSQLForEngineList(
  var SQL: RawUTF8): boolean;
begin
  result := true; // we do not have any Virtual Table yet -> always accept
end;

const // see http://docs.mongodb.org/manual/reference/operator/query
  QUERY_OPS: array[TSynTableStatementOperator] of PUTF8Char = (
    '{%:?}', '{%:{$ne:?}}', '{%:{$lt:?}}', '{%:{$lte:?}}',
    '{%:{$gt:?}}', '{%:{$gte:?}}', '{%:{$in:?}}');
    
function TSQLRestServerStaticMongoDB.EngineList(const SQL: RawUTF8;
  ForceAJAX: Boolean; ReturnedRowCount: PPtrInt): RawUTF8;
var W: TJSONSerializer;
    MS: TRawByteStringStream;
    Query,Projection: variant;
    QueryFieldName: RawUTF8;
    Res: TBSONDocument;
    ResCount: PtrInt;
    col, colCount, colFound: integer;
    bson: PByte;
    row: TBSONElement;
    extFieldNames: TRawUTF8DynArray;
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
procedure SetCount(aCount: integer);
begin
  result := FormatUTF8('[{"Count(*)":%}]'#$A,[aCount]);
  ResCount := 1;
end;
begin // same logic as in TSQLRestServerStaticInMemory.EngineList()
  ResCount := 0;
  if self=nil then begin
    result := '';
    exit;
  end;
  Lock(false);
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
      with fStoredClassRecordProps,
        TSynTableStatement.Create(SQL,Fields.IndexByName,
          fStoredClassRecordProps.SimpleFieldsBits[soSelect]) do
      try
        if (WhereValue='') or
           not IdemPropNameU(TableName,SQLTableName) then
          // invalid request -> return ''
          result := '' else
        if WhereField=SYNTABLESTATEMENTWHERECOUNT then
          // was "SELECT Count(*) FROM TableName;"
          SetCount(TableRowCount(fStoredClass)) else
        if IsZero(Fields) and not WithID then
          if IsSelectCountWhere then
            // was "SELECT Count(*) FROM TableName WHERE ..."
            if WhereField<0 then
              SetCount(TableRowCount(fStoredClass)) else
              SetCount(fCollection.FindCount('{%:?}',
                [fStoredClassProps.ExternalDB.FieldNameByIndex(WhereField-1)],
                [WhereValueVariant],FoundLimit,FoundOffset)) else
            // invalid "SELECT FROM Table" ?
            exit else begin
          // save rows as JSON, with appropriate search according to Where* arguments
          if WhereField<0 then
            SetVariantNull(Query) else begin
            QueryFieldName := fStoredClassProps.ExternalDB.FieldNameByIndex(WhereField-1);
            Query := BSONVariant(QUERY_OPS[WhereOperator],[QueryFieldName],[WhereValueVariant]);
          end;
          colCount := BSONProjectionSet(Projection,Fields,@extFieldNames);
          if FoundLimit=0 then
            FoundLimit := maxInt;
          Res := fCollection.FindBSON(Query,Projection,FoundLimit,FoundOffset);
          ResCount := 0;
          MS := TRawByteStringStream.Create;
          try
            W := fStoredClassRecordProps.CreateJSONWriter(
              MS,ForceAJAX or (not Owner.NoAJAXJSON),withID,Fields,0);
            try
              bson := pointer(Res);
              if W.Expand then
                W.Add('[');
              if Res<>'' then begin
                BSONParseLength(bson,length(Res));
                SetLength(item,colCount);
                while row.FromNext(bson) do begin
                  if row.Kind<>betDoc then
                    raise EORMMongoDBException.CreateFmt('Invalid row %d',[ord(row.Kind)]);
                  if W.Expand then
                    W.Add('{');
                  col := 0;
                  while (row.Data.DocList^<>byte(betEof)) and
                        (col<colCount) and item[col].FromNext(row.Data.DocList) do
                    inc(col);
                  if col<>colCount then
                    raise EORMMongoDBException.CreateFmt('Invalid field count %d',[col]);
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
                  if W.Expand then begin
                    W.Add('}',',');
                  end else
                    W.Add(',');
                  inc(ResCount);
                end;
              end;
              if (ResCount=0) and W.Expand then begin
                // we want the field names at least, even with no data
                W.Expand := false; //  {"fieldCount":2,"values":["col1","col2"]}
                W.CancelAll;
                fStoredClassRecordProps.SetJSONWriterColumnNames(W,0);
              end;
              W.EndJSONObject(0,ResCount);
              result := MS.DataString;
            finally
              W.Free;
            end;
          finally
            MS.Free;
          end;
        end;
      finally
        Free;
      end;
    end;
  finally
    UnLock;
  end;
  if ReturnedRowCount<>nil then
    ReturnedRowCount^ := ResCount;
end;



end.
