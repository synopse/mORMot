unit SmartTests;

interface

uses 
  W3System,
  w3c.date,
  SynCrossPlatformSpecific,
  SynCrossPlatformREST,
  SynCrossPlatformCrypto;

procedure TestSMS;

type
  TSQLRecordPeople = class(TSQLRecord)
  protected
    fData: TSQLRawBlob;
    fFirstName: RawUTF8;
    fLastName: RawUTF8;
    fYearOfBirth: integer;
    fYearOfDeath: word;
    class function ComputeRTTI: TRTTIPropInfos; override;
  published
    property FirstName: RawUTF8 read fFirstName write fFirstName;
    property LastName: RawUTF8 read fLastName write fLastName;
    property Data: TSQLRawBlob read fData write fData;
    property YearOfBirth: integer read fYearOfBirth write fYearOfBirth;
    property YearOfDeath: word read fYearOfDeath write fYearOfDeath;
  end;


implementation

const
  MSecsPerDay = 86400000;
  OneSecDateTime = 1000/MSecsPerDay;

procedure TestsIso8601DateTime;
  procedure Test(D: TDateTime);
  var s: string;
  procedure One(D: TDateTime);
  var E: TDateTime;
      V: TTimeLog;
      J: JDate;
  begin
    J := new JDate;
    J.AsDateTime := D;
    E := J.AsDateTime;
    assert(Abs(D-E)<OneSecDateTime);
    s := DateTimeToIso8601(D);
    E := Iso8601ToDateTime(s);
    assert(Abs(D-E)<OneSecDateTime);
    V := DateTimeToTTimeLog(D);
    E := TTimeLogToDateTime(V);
    assert(Abs(D-E)<OneSecDateTime);
    assert(UrlDecode(UrlEncode(s))=s);
  end;
  begin
    One(D);
    assert(length(s)=19);
    One(Trunc(D));
    assert(length(s)=10);
    One(Frac(D));
    assert(length(s)=9);
  end;
var D: TDateTime;
    i: integer;
    s,x: string;
    T: TTimeLog;
begin
  s := '2014-06-28T11:50:22';
  D := Iso8601ToDateTime(s);
  assert(Abs(D-41818.49331)<OneSecDateTime);
  assert(DateTimeToIso8601(D)=s);
  x := TTimeLogToIso8601(135181810838);
  assert(x=s);
  T := DateTimeToTTimeLog(D);
  assert(T=135181810838);
  D := Now/20+Random*20; // some starting random date/time
  for i := 1 to 2000 do begin
    Test(D);
    D := D+Random*57; // go further a little bit: change date/time
  end;
end;

procedure TestSMS;
var doc: TJSONVariantData;
begin
  assert(crc32ascii(0,'abcdefghijklmnop')=$943AC093);
  assert(SHA256('abc')='ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad');
  assert(VariantType(123)=jvUndefined);
  assert(VariantType(null)=jvUndefined);
  assert(VariantType(TVariant.CreateObject)=jvObject);
  assert(VariantType(TVariant.CreateArray)=jvArray);
  doc := TJSONVariantData.Create('{"a":1,"b":"B"}');
  assert(doc.Kind=jvObject);
  assert(doc.Count=2);
  assert(doc.Names[0]='a');
  assert(doc.Names[1]='b');
  assert(doc.Values[0]=1);
  assert(doc.Values[1]='B');
  doc := TJSONVariantData.Create('["a",2]');
  assert(doc.Kind=jvArray);
  assert(doc.Count=2);
  assert(doc.Names.Count=0);
  assert(doc.Values[0]='a');
  assert(doc.Values[1]=2);
  TestsIso8601DateTime;
end;


{ TSQLRecordPeople }

class function TSQLRecordPeople.ComputeRTTI: TRTTIPropInfos;
begin
  result := TRTTIPropInfos.Create(
    ['Data','FirstName','LastName','YearOfBirth','YearOfDeath'],
    [sftBlob]);
end;

end.
