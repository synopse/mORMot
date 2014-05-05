unit MongoDBTestCases;

interface

// if defined, will test with 5000 records instead of the default 100 records
{.$define ADD5000}

uses
  SysUtils,
  Variants,
  SynCommons,
  SynMongoDB,
  mORMot,
  SynSQLite3Static,
  mORMotSQLite3,
  mORMotMongoDB;

type
  TTestDirect = class(TSynTestCase)
  protected
    fClient: TMongoClient;
    fDB: TMongoDatabase;
    fValues: TVariantDynArray;
    fExpectedCount: integer;
    procedure CleanUp; override;
  published
    procedure ConnectToLocalServer;
    procedure DropAndPrepareCollection;
    procedure FillCollection;
    procedure FillCollectionBulk;
    procedure ReadCollection;
    procedure UpdateCollection;
    procedure DeleteSomeItems;
  end;
  TTestDirectWithAcknowledge = class(TTestDirect);
  TTestDirectWithoutAcknowledge = class(TTestDirect);

  TSQLORM = class(TSQLRecord)
  private
    fAge: integer;
    fName: RawUTF8;
    fDate: TDateTime;
    fValue: variant;
    fInts: TIntegerDynArray;
    fCreateTime: TCreateTime;
    fData: TSQLRawBlob;
  published
    property Name: RawUTF8 read fName write fName stored AS_UNIQUE;
    property Age: integer read fAge write fAge;
    property Date: TDateTime read fDate write fDate;
    property Value: variant read fValue write fValue;
    property Ints: TIntegerDynArray index 1 read fInts write fInts;
    property Data: TSQLRawBlob read fData write fData;
    property CreateTime: TCreateTime read fCreateTime write fCreateTime;
  end;

  TTestORM = class(TSynTestCase)
  protected
    fClient: TMongoClient;
    fDB: TMongoDatabase;
    fModel: TSQLModel;
    fServer: TSQLRestServer;
    fStartTimeStamp: TTimeLog;
    fUpdateOffset: integer;
    procedure TestOne(R: TSQLORM; aID: integer);
    procedure CleanUp; override;
  published
    procedure ConnectToLocalServer;
    procedure Insert;
    procedure Retrieve;
    procedure RetrieveAll;
    procedure RetrieveOneWithWhereClause;
    procedure Update;
    procedure Blobs;
    procedure Delete;
  end;
  TTestORMWithAcknowledge = class(TTestORM);
  TTestORMWithoutAcknowledge = class(TTestORM);

  TTestMongoDB = class(TSynTestsLogged)
  published
    procedure DirectAccess;
    procedure ORM;
  end;

implementation


{ TTestMongoDB }

procedure TTestMongoDB.DirectAccess;
begin
  AddCase([TTestDirectWithAcknowledge,TTestDirectWithoutAcknowledge]);
end;

procedure TTestMongoDB.ORM;
begin
  AddCase([TTestORMWithAcknowledge,TTestORMWithoutAcknowledge]);
end;


{ TTestDirect }

const
  DB_NAME = 'mwx1';
  COLL_NAME = 'test24';
  {$ifndef ADD5000}
  COLL_COUNT = 100;
  HASH1 = $44D5AC3E;
  HASH2 = $8A178B3;
  {$else}
  COLL_COUNT = 5000;
  HASH1 = $4EA46962;
  HASH2 = $2A005528;
  {$endif}

procedure TTestDirect.CleanUp;
begin
  FreeAndNil(fClient);
end;

procedure TTestDirect.ConnectToLocalServer;
var serverTime: TDateTime;
    res: variant;
    errmsg: RawUTF8;
begin
  assert(fClient=nil);
  fClient := TMongoClient.Create('localhost',27017);
  if ClassType=TTestDirectWithAcknowledge then
    fClient.WriteConcern := wcAcknowledged else
  if ClassType=TTestDirectWithoutAcknowledge then
    fClient.WriteConcern := wcUnacknowledged else
    assert(false);
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

procedure TTestDirect.DropAndPrepareCollection;
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

procedure TTestDirect.FillCollection;
var Coll: TMongoCollection;
    oid: TBSONObjectID;
    i: integer;
    dat: TDateTime;
    jsonArray: RawUTF8;
    bytes: Int64;
begin
  fDB.CollectionOrNil[COLL_NAME].Drop;
  Coll := fDB.CollectionOrCreate[COLL_NAME];
  Coll.EnsureIndex(['Name']);
  bytes := fClient.BytesTransmitted;
  fValues := nil;
  SetLength(fValues,COLL_COUNT);
  for i := 0 to COLL_COUNT-1 do begin
    TDocVariant.New(fValues[i]);
    if i<50 then
      fValues[i]._id := null else
      fValues[i]._id := ObjectID;
    fValues[i].Name := 'Name '+IntToStr(i+1);
    fValues[i].FirstName := 'FirstName '+IntToStr(i+COLL_COUNT);
    fValues[i].Number := i;
    dat := 1.0*(30000+i);
    fValues[i].Date := dat;
    Check(Coll.Save(fValues[i],@oid)=(i<50));
    Check(BSONVariantType.IsOfKind(fValues[i]._id,betObjectID));
    Check(fValues[i]._id=oid.ToVariant,'EnsureDocumentHasID failure');
  end;
  NotifyTestSpeed('rows inserted',COLL_COUNT,fClient.BytesTransmitted-bytes);
  Check(Coll.Count=COLL_COUNT);
  jsonArray := Coll.FindJSON(null,BSONVariant('{_id:0}'));
  Check(JSONArrayCount(@jsonArray[2])=COLL_COUNT);
  Check(Hash32(jsonArray)=HASH1,'projection over a collection');
end;

procedure TTestDirect.FillCollectionBulk;
var Coll: TMongoCollection;
    jsonArray: RawUTF8;
    bytes: Int64;
begin
  fDB.CollectionOrNil[COLL_NAME].Drop;
  Coll := fDB.CollectionOrCreate[COLL_NAME];
  Coll.EnsureIndex(['Name']);
  bytes := fClient.BytesTransmitted;
  Coll.Insert(fValues); // insert all values at once
  NotifyTestSpeed('rows inserted',Coll.Count,fClient.BytesTransmitted-bytes);
  jsonArray := Coll.FindJSON(null,BSONVariant('{_id:0}'));
  Check(JSONArrayCount(@jsonArray[2])=COLL_COUNT);
  Check(Hash32(jsonArray)=HASH1,'projection over a collection');
end;

procedure TTestDirect.ReadCollection;
var i: integer;
    Coll: TMongoCollection;
    docs: variant;
    jsonOne,jsonArray: RawUTF8;
    bytes: Int64;
begin
  Coll := fDB.Collection[COLL_NAME];
  Check(Coll.Count=COLL_COUNT);
  bytes := fClient.BytesTransmitted;
  jsonArray := Coll.FindJSON(null,BSONVariant('{_id:0}'));
  Check(JSONArrayCount(@jsonArray[2])=COLL_COUNT);
  Check(Hash32(jsonArray)=HASH1,'projection over a collection');
  NotifyTestSpeed('rows read at once',Coll.Count,fClient.BytesTransmitted-bytes);
  for i := 0 to COLL_COUNT-1 do begin
    jsonOne := VariantSaveMongoJSON(fValues[i],modMongoStrict);
    jsonArray := '['+jsonOne+']';
    //if i mod 100=0 then begin // search by name is SLOW, even with the index!
      Check(Coll.FindJSON('{Name:?}',[fValues[i].Name])=jsonArray);
      Check(Coll.FindJSON(BSONVariant(['Name','Name '+IntToStr(i+1)]),null)=jsonarray);
      Check(Coll.FindJSON(BSONVariant(['Name','Name '+IntToStr(i+1)]),null,1)=jsonone);
      docs := Coll.FindDoc('{Name:?}',[fValues[i].Name]);
      Check(VariantSaveMongoJSON(docs,modMongoStrict)=jsonArray);
    //end;
    docs := Coll.FindDoc('{_id:?}',[fValues[i]._id]);
    Check(VariantSaveMongoJSON(docs,modMongoStrict)=jsonArray);
    docs := Coll.FindDoc('{_id:?}',[fValues[i]._id],1);
    Check(VariantSaveMongoJSON(docs,modMongoStrict)=jsonOne);
  end;
end;

procedure TTestDirect.UpdateCollection;
var i: integer;
    Coll: TMongoCollection;
    jsonOne,jsonArray: RawUTF8;
    bytes: Int64;
begin
  Coll := fDB.Collection[COLL_NAME];
  Check(Coll.Count=COLL_COUNT);
  bytes := fClient.BytesTransmitted;
  for i := 0 to COLL_COUNT-1 do begin
    fValues[i].Name := 'Name '+IntToStr(i+1);
    if i<COLL_COUNT div 2 then
      Check(not Coll.Save(fValues[i])) else
      Coll.Update('{_id:?}',[fValues[i]._id],'?',[fValues[i]]);
  end;
  NotifyTestSpeed('rows updated',Coll.Count,fClient.BytesTransmitted-bytes);
  Check(Coll.Count=COLL_COUNT);
  for i := 0 to COLL_COUNT-1 do begin
    jsonOne := Coll.FindJSON('{_id:?}',[fValues[i]._id],1);
    Check(jsonOne=VariantSaveMongoJSON(fValues[i],modMongoStrict),'in-place update');
  end;
  jsonArray := Coll.FindJSON(null,BSONVariant(['_id',0,'Date',0,'Number',0]));
  Check(JSONArrayCount(@jsonArray[2])=COLL_COUNT,'projection over an updated collection');
end;

procedure TTestDirect.DeleteSomeItems;
var beforeCount,i,j: integer;
    Coll: TMongoCollection;
    jsonOne: RawUTF8;
    bytes: Int64;
begin
  Coll := fDB.Collection[COLL_NAME];
  Check(Coll.Count=fExpectedCount);
  bytes := fClient.BytesTransmitted;
  beforeCount := fExpectedCount;
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
      inc(j);
    end;
  NotifyTestSpeed('rows deleted',beforeCount-fExpectedCount,fClient.BytesTransmitted-bytes);
  Check(Coll.Count=fExpectedCount);
  for i := 0 to high(fValues) do
    if not VarIsNull(fValues[i]._id) then begin
      jsonOne := Coll.FindJSON('{_id:?}',[fValues[i]._id],1);
      Check(jsonOne=VariantSaveMongoJSON(fValues[i],modMongoStrict),'delete');
    end;
end;


{ TTestORM }

procedure TTestORM.CleanUp;
begin
  FreeAndNil(fServer);
  FreeAndNil(fModel);
  FreeAndNil(fClient);
end;

procedure TTestORM.ConnectToLocalServer;
begin
  fClient := TMongoClient.Create('localhost',27017);
  if ClassType=TTestORMWithAcknowledge then
    fClient.WriteConcern := wcAcknowledged else
  if ClassType=TTestORMWithoutAcknowledge then
    fClient.WriteConcern := wcUnacknowledged else
    assert(false);
  fDB := fClient.Database[DB_NAME];
  Check(fDB<>nil);
  Check(fDB.Name=DB_NAME);
  Check(fClient.ServerBuildInfoNumber<>0);
  fModel := TSQLModel.Create([TSQLORM]);
  fServer := TSQLRestServerDB.Create(fModel,':memory:');
  Check(StaticMongoDBRegister(TSQLORM,fServer,fDB,'mORMot'));
  fServer.CreateMissingTables;
  (fServer.StaticDataServer[TSQLORM] as TSQLRestServerStaticMongoDB).Drop;
  Check(fServer.TableRowCount(TSQLORM)=0);
  fStartTimeStamp := fServer.ServerTimeStamp;
  Check(fStartTimeStamp>10000);
  fServer.NoAJAXJSON := true;
end;

procedure TTestORM.Insert;
var R: TSQLORM;
    i: integer;
    bytes: Int64;
begin
  Check(fServer.TableRowCount(TSQLORM)=0);
  bytes := fClient.BytesTransmitted;
  R := TSQLORM.Create;
  try
    for i := 1 to COLL_COUNT do begin
      R.Name := 'Name '+Int32ToUTF8(i);
      R.Age := i;
      R.Date := 1.0*(30000+i);
      R.Value := _ObjFast(['num',i]);
      R.Ints := nil;
      R.DynArray(1).Add(i);
      Check(fServer.Add(R,True)=i);
    end;
  finally
    R.Free;
  end;
  NotifyTestSpeed('rows inserted',COLL_COUNT,fClient.BytesTransmitted-bytes);
  Check(fServer.TableRowCount(TSQLORM)=COLL_COUNT);
end;

procedure TTestORM.TestOne(R: TSQLORM; aID: integer);
begin
  Check(R.ID=aID);
  Check(R.Name='Name '+Int32ToUTF8(aID));
  Check(R.Age=aID+fUpdateOffset);
  CheckSame(R.Date,1.0*(30000+aID),1E-5);
  Check(R.Value.num=aID+fUpdateOffset);
  Check(Length(R.Ints)=1);
  Check(R.Ints[0]=aID);
  Check(R.CreateTime>=fStartTimeStamp);
end;

procedure TTestORM.Retrieve;
var R: TSQLORM;
    i: integer;
    bytes: Int64;
begin
  Check(fServer.TableRowCount(TSQLORM)=COLL_COUNT);
  bytes := fClient.BytesTransmitted;
  R := TSQLORM.Create;
  try
    for i := 1 to COLL_COUNT do begin
      Check(fServer.Retrieve(i,R));
      TestOne(R,i);
    end;
  finally
    R.Free;
  end;
  NotifyTestSpeed('rows retrieved',COLL_COUNT,fClient.BytesTransmitted-bytes);
end;

procedure TTestORM.RetrieveAll;
var n: integer;
    R: TSQLORM;
    bytes: Int64;
begin
  bytes := fClient.BytesTransmitted;
  R := TSQLORM.CreateAndFillPrepare(fServer,'');
  try
    n := 0;
    while R.FillOne do begin
      inc(n);
      TestOne(R,n);
    end;
    Check(n=COLL_COUNT);
  finally
    R.Free;
  end;
  NotifyTestSpeed('rows retrieved',COLL_COUNT,fClient.BytesTransmitted-bytes);
end;

procedure TTestORM.RetrieveOneWithWhereClause;
var R: TSQLORM;
    i,n: integer;
    bytes: Int64;
begin
  bytes := fClient.BytesTransmitted;
  for i := 1 to COLL_COUNT do begin
    R := TSQLORM.CreateAndFillPrepare(fServer,'ID=?',[i]);
    try
      n := 0;
      while R.FillOne do begin
        inc(n);
        TestOne(R,i);
      end;
      Check(n=1);
    finally
      R.Free;
    end;
  end;
  NotifyTestSpeed('rows retrieved',COLL_COUNT,fClient.BytesTransmitted-bytes);
  R := TSQLORM.CreateAndFillPrepare(fServer,'Name=?',['Name 43']);
  try
    n := 0;
    while R.FillOne do begin
      inc(n);
      TestOne(R,43);
    end;
    Check(n=1);
  finally
    R.Free;
  end;
  R := TSQLORM.CreateAndFillPrepare(fServer,'Age<?',[51]);
  try
    n := 0;
    while R.FillOne do begin
      inc(n);
      TestOne(R,n);
    end;
    Check(n=50);
  finally
    R.Free;
  end;
end;

procedure TTestORM.Update;
var R: TSQLORM;
    bytes: Int64;
    n: integer;
begin
  inc(fUpdateOffset);
  R := TSQLORM.CreateAndFillPrepare(fServer,'');
  try
    bytes := fClient.BytesTransmitted;
    n := 0;
    while R.FillOne do begin
      R.Age := R.Age+fUpdateOffset;
      R.Value.num := R.Value.num+fUpdateOffset;
      fServer.Update(R);
      inc(n);
    end;
    Check(n=COLL_COUNT);
  finally
    R.Free;
  end;
  NotifyTestSpeed('rows updated',COLL_COUNT,fClient.BytesTransmitted-bytes);
  R := TSQLORM.CreateAndFillPrepare(fServer,'');
  try
    n := 0;
    while R.FillOne do begin
      inc(n);
      TestOne(R,n);
    end;
    Check(n=COLL_COUNT);
  finally
    R.Free;
  end;
end;

procedure TTestORM.Blobs;
var R: TSQLORM;
    i, n: integer;
    blob,blobRead: TSQLRawBlob;
    bytes: Int64;
begin
  SetLength(blob,8);
  bytes := fClient.BytesTransmitted;
  for i := 1 to COLL_COUNT do begin
    PIntegerArray(blob)[0] := i;
    PIntegerArray(blob)[1] := i*$01020304;
    Check(fServer.UpdateBlob(TSQLORM,i,'Data',blob));
  end;
  NotifyTestSpeed('rows updated',COLL_COUNT,fClient.BytesTransmitted-bytes);
  R := TSQLORM.CreateAndFillPrepare(fServer,'');
  try
    n := 0;
    while R.FillOne do begin
      inc(n);
      TestOne(R,n);
      Check(R.Data='');
      fServer.RetrieveBlob(TSQLORM,n,'Data',blobRead);
      PIntegerArray(blob)[0] := n;
      PIntegerArray(blob)[1] := n*$01020304;
      Check(blobRead=blob);
    end;
    Check(n=COLL_COUNT);
  finally
    R.Free;
  end;
  R := TSQLORM.CreateAndFillPrepare(fServer,'','ID,Data');
  try
    n := 0;
    while R.FillOne do begin
      inc(n);
      Check(R.ID=n);
      Check(R.Age=0);
      PIntegerArray(blob)[0] := n;
      PIntegerArray(blob)[1] := n*$01020304;
      Check(R.Data=blob);
      PIntegerArray(blob)[0] := n*2;
      PIntegerArray(blob)[1] := n*$02030405;
      R.Data := blob;
      fServer.UpdateBlobFields(R);
    end;
    Check(n=COLL_COUNT);
  finally
    R.Free;
  end;
  R := TSQLORM.CreateAndFillPrepare(fServer,'');
  try
    n := 0;
    while R.FillOne do begin
      inc(n);
      TestOne(R,n);
      Check(R.Data='');
      Check(fServer.RetrieveBlobFields(R));
      PIntegerArray(blob)[0] := n*2;
      PIntegerArray(blob)[1] := n*$02030405;
      Check(R.Data=blob);
      R.Data := '';
    end;
    Check(n=COLL_COUNT);
  finally
    R.Free;
  end;
end;

procedure TTestORM.Delete;
var i,n: integer;
    ExpectedCount: integer;
    bytes: Int64;
    R: TSQLORM;
begin
  Check(fServer.Delete(TSQLORM,'ID in (5,10,15)'),'deletion with IN clause');
  bytes := fClient.BytesTransmitted;
  ExpectedCount := COLL_COUNT-3;
  for i := 20 to COLL_COUNT do
    if i mod 5=0 then begin
      Check(fServer.Delete(TSQLORM,i));
      dec(ExpectedCount);
    end;
  NotifyTestSpeed('rows deleted',COLL_COUNT-ExpectedCount,fClient.BytesTransmitted-bytes);
  R := TSQLORM.CreateAndFillPrepare(fServer,'');
  try
    n := 0;
    i := 0;
    while R.FillOne do begin
      inc(i);
      if i mod 5=0 then
        inc(i);
      inc(n);
      TestOne(R,i);
    end;
    Check(n=ExpectedCount);
  finally
    R.Free;
  end;
end;


end.

