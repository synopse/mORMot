unit MongoDBTestCases;

interface

uses
  SysUtils,
  Variants,
  SynCommons,
  SynMongoDB;

type
  TTestMongoDBDirect = class(TSynTestCase)
  protected
    fClient: TMongoClient;
    fDB: TMongoDatabase;
    fValues: TVariantDynArray;
    fExpectedCount: integer;
    procedure CleanUp; override;
    procedure Fill;
    procedure Update(Offset: integer);
    procedure Delete;
  published
    procedure ConnectToLocalServer;
    procedure DropAndPrepareCollection;
    procedure FillCollectionNoAcknowledge;
    procedure FillCollectionAcknowledge;
    procedure ReadCollection;
    procedure UpdateCollectionNoAcknowledge;
    procedure UpdateCollectionAcknowledge;
    procedure DeleteSomeItemsNoAcknowledge;
    procedure DeleteSomeItemsAcknowledge;
  end;

  TTestMongoDB = class(TSynTestsLogged)
  published
    procedure Direct_Access;
  end;

implementation


{ TTestMongoDB }

procedure TTestMongoDB.Direct_Access;
begin
  AddCase([TTestMongoDBDirect]);
end;

{ TTestMongoDBDirect }

const
  DB_NAME = 'mwx1';
  COLL_NAME = 'test24';
  {$ifdef ADD5000}
  COLL_COUNT = 100;
  HASH1 = $FFD8FF4B;
  HASH2 = $8A178B3;
  {$else}
  COLL_COUNT = 5000;
  HASH1 = $CACB7B7F;
  HASH2 = $2A005528;
  {$endif}

procedure TTestMongoDBDirect.CleanUp;
begin
  FreeAndNil(fClient);
end;

procedure TTestMongoDBDirect.ConnectToLocalServer;
var serverTime: TDateTime;
    res: variant;
    errmsg: RawUTF8;
begin
  assert(fClient=nil);
  fClient := TMongoClient.Create('localhost',27017);
  fDB := fClient.Database[DB_NAME];
  Check(fDB<>nil);
  Check(fDB.Name=DB_NAME);
  errmsg := fDB.RunCommand('hostInfo',res);
  if CheckFailed(errmsg='') or CheckFailed(not VarIsNull(res.system)) then
    exit;
  serverTime := res.system.currentTime; // direct conversion to TDateTime
  Check(serverTime<>0);
  CheckSame(Now,serverTime,0.5);
  fExpectedCount := COLL_COUNT;
end;

procedure TTestMongoDBDirect.DropAndPrepareCollection;
var Coll: TMongoCollection;
    errmsg: RawUTF8;
begin
  assert(fDB<>nil);
  Coll := fDB.CollectionOrNil[COLL_NAME];
  if Coll<>nil then
    Check(Coll.Drop='');
  Coll := fDB.CollectionOrCreate[COLL_NAME];
  Check(Coll.Name=COLL_NAME);
  Check(Coll.FullCollectionName=DB_NAME+'.'+COLL_NAME);
  Check(Coll.Database=fDB);
  Check(fDB.Collection[COLL_NAME]=Coll);
  Check(fDB.CollectionOrCreate[COLL_NAME]=Coll);
  errmsg := Coll.Drop;
  Check(errmsg<>'','dropping a non existing collection should return an error');
  Check(fClient.ServerBuildInfoNumber>20000);
end;

procedure TTestMongoDBDirect.FillCollectionNoAcknowledge;
begin
  fClient.WriteConcern := wcUnacknowledged;
  Fill;
end;

procedure TTestMongoDBDirect.FillCollectionAcknowledge;
begin
  fClient.WriteConcern := wcAcknowledged;
  Fill;
end;

procedure TTestMongoDBDirect.Fill;
var Coll: TMongoCollection;
    oid: TBSONObjectID;
    i: integer;
begin
  fDB.CollectionOrNil[COLL_NAME].Drop;
  Coll := fDB.CollectionOrCreate[COLL_NAME];
  Coll.EnsureIndex(['Name']);
  SetLength(fValues,COLL_COUNT);
  for i := 0 to COLL_COUNT-1 do begin
    TDocVariant.New(fValues[i]);
    if i<50 then
      fValues[i]._id := null else
      fValues[i]._id := ObjectID;
    fValues[i].Name := 'Name '+IntToStr(i+1);
    fValues[i].FirstName := 'FirstName '+IntToStr(i+COLL_COUNT);
    Check(Coll.Save(fValues[i],@oid)=(i<50));
    Check(BSONVariantType.IsOfKind(fValues[i]._id,betObjectID));
    Check(fValues[i]._id=oid.ToVariant,'EnsureDocumentHasID failure');
  end;
  Check(Coll.Count=COLL_COUNT);
end;

procedure TTestMongoDBDirect.ReadCollection;
var i: integer;
    Coll: TMongoCollection;
    docs: variant;
    jsonOne,jsonArray: RawUTF8;
begin
  Coll := fDB.Collection[COLL_NAME];
  Check(Coll.Count=COLL_COUNT);
  for i := 0 to COLL_COUNT-1 do begin
    jsonOne := VariantSaveJSON(fValues[i]);
    jsonArray := '['+jsonOne+']';
    //if i mod 100=0 then begin // search by name is SLOW, even with the index!
      Check(Coll.FindJSON('{Name:?}',[fValues[i].Name])=jsonArray);
      Check(Coll.FindJSON(BSONVariant(['Name','Name '+IntToStr(i+1)]),null)=jsonarray);
      Check(Coll.FindJSON(BSONVariant(['Name','Name '+IntToStr(i+1)]),null,1)=jsonone);
      docs := Coll.FindDoc('{Name:?}',[fValues[i].Name]);
      Check(VariantSaveJSON(docs)=jsonArray);
    //end;
    docs := Coll.FindDoc('{_id:?}',[fValues[i]._id]);
    Check(VariantSaveJSON(docs)=jsonArray);
    docs := Coll.FindDoc('{_id:?}',[fValues[i]._id],1);
    Check(VariantSaveJSON(docs)=jsonOne);
  end;
  jsonArray := Coll.FindJSON(null,BSONVariant('{_id:0}'));
  Check(JSONArrayCount(@jsonArray[2])=COLL_COUNT);
  Check(Hash32(jsonArray)=HASH1,'projection over a collection');
end;

procedure TTestMongoDBDirect.UpdateCollectionNoAcknowledge;
begin
  fClient.WriteConcern := wcUnacknowledged;
  Update(0);
end;

procedure TTestMongoDBDirect.UpdateCollectionAcknowledge;
begin
  fClient.WriteConcern := wcAcknowledged;
  Update(2);
end;

procedure TTestMongoDBDirect.Update(Offset: integer);
var i: integer;
    Coll: TMongoCollection;
    jsonOne,jsonArray: RawUTF8;
begin
  Coll := fDB.Collection[COLL_NAME];
  Check(Coll.Count=COLL_COUNT);
  for i := 0 to COLL_COUNT-1 do begin
    fValues[i].Name := 'Name '+IntToStr(i+Offset);
    if i<COLL_COUNT div 2 then
      Check(not Coll.Save(fValues[i])) else
      Coll.Update('{_id:?}',[fValues[i]._id],'?',[fValues[i]]);
  end;
  Check(Coll.Count=COLL_COUNT);
  for i := 0 to COLL_COUNT-1 do begin
    jsonOne := Coll.FindJSON('{_id:?}',[fValues[i]._id],1);
    Check(jsonOne=VariantSaveJSON(fValues[i]),'in-place update');
  end;
  jsonArray := Coll.FindJSON(null,BSONVariant(['_id',0]));
  Check(JSONArrayCount(@jsonArray[2])=COLL_COUNT);
  if Offset=0 then
    Check(Hash32(jsonArray)=HASH2,'projection over an updated collection');
end;


procedure TTestMongoDBDirect.Delete;
var i,j: integer;
    Coll: TMongoCollection;
    jsonOne: RawUTF8;
begin
  Coll := fDB.Collection[COLL_NAME];
  Check(Coll.Count=fExpectedCount);
  j := 0;
  for i := 0 to high(fValues) do
    if not VarIsNull(fValues[i]._id) then begin
      if j mod 5=0 then begin
        if j mod 10=0 then
          Coll.RemoveOne(BSONObjectID(fValues[i]._id)) else
        if j mod 15=0 then
          Coll.RemoveOne(fValues[i]._id) else
          Coll.RemoveFmt('{_id:?}',[fValues[i]._id]);
        fValues[i]._id := null;
        dec(fExpectedCount);
      end;
      inc(j)
    end;
  Check(Coll.Count=fExpectedCount);
  for i := 0 to high(fValues) do
    if not VarIsNull(fValues[i]._id) then begin
      jsonOne := Coll.FindJSON('{_id:?}',[fValues[i]._id],1);
      Check(jsonOne=VariantSaveJSON(fValues[i]),'delete');
    end;
end;

procedure TTestMongoDBDirect.DeleteSomeItemsAcknowledge;
begin
  fClient.WriteConcern := wcAcknowledged;
  Delete;
end;

procedure TTestMongoDBDirect.DeleteSomeItemsNoAcknowledge;
begin
  fClient.WriteConcern := wcUnacknowledged;
  Delete;
end;

end.
