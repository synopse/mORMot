/// fill a VCL TClientDataset from SynVirtualDataset data access
// - this unit is a part of the freeware Synopse framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynRestVCL;

{
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2018 Arnaud Bouchez
      Synopse Informatique - https://synopse.info

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

  Portions created by the Initial Developer are Copyright (C) 2018
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  - Esteban Martin (EMartin)
  - houdw2006

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
  - first public release, corresponding to Synopse mORMot Framework 1.18,
    which is an extraction from former SynDBVCL.pas unit.
  - Added that blob field updates they are made with AddJSONEscapeString.
  - bug fix when updating accentuated string fields.
  - bug fix with datetime fields
  - bug fix with length string fields
  - fixed Delphi XE3 compilation issue with PSExecuteStatement declaration (by houdw2006)
  - added sftSessionUserID to SQLFIELDTYPETODBFIELDTYPE and SQLFieldTypeToVCLDB
  - reworked based on work of Dewen HOU (houdw2006) with some changes:
    * added again the capability of invoke mORMot interface based service
    * changed TSQLRestClientURI instead of TSQLRest.
    * removed Compute procedure for local ComputeFieldsBeforeWrite because now
      mORMot have it in server side.
  - bug fix not sent blob fields on update/insert

  * Update Logeas Informatique 2016/09/12
    - cast String<=>RawUTF8
    - Params null handled
    - Params string quoted

  - (2016/12/14) removed QuoteStr in BindParams for avoid double quotation
  - (2017/01/10)
    * removed DeletedID
    * refactoring, no functional changes
  - (2017/01/16)
    * added sftDateTimeMS, sftUnixTime to SQLFIELDTYPETODBFIELDTYPE and SQLFieldTypeToVCLDB
  - (2017/02/07)
    * add support to calculated fields (may be lookup fields too, but not tested) overriding CreateFields function
    * added TTimeLogField, derived from TSQLTimeStampField and hacking ftTimeStamp
  - (2017/03/06)
    * added support to field sftUnixMSTime
  - (2017/10/09)
    * InsertedID field changed to public property
}

{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

interface

uses
{$ifdef MSWINDOWS}
  Windows,
{$else}
  {$ifdef KYLIX3}
  Types,
  LibC,
  {$endif}
{$endif}
{$ifdef ISDELPHIXE2}System.SysUtils,{$else}SysUtils,{$endif}
  Classes,
{$ifndef DELPHI5OROLDER}
  Variants,
{$endif}
  mORMot,
  SynCommons,
  SynDB,
  SynDBVCL,
  DB,
  {$ifdef FPC}
  BufDataset
  {$else}
  Provider
  {$endif};


type
  /// generic Exception type
  ESQLRestException = class(ESynException);

  /// URI signature event
  TGetURISignatureEvent = procedure(Sender: TObject; var aURI: string) of object;

  /// a TDataSet which allows to apply updates on a Restful connection
  // - typical usage may be for instance:
  // ! cli := TSQLHttpClient.Create(host, port, CreateModel); // create the client
  // ! ds := TSynRestDataSet.Create(MainForm); // create the dataset
  // ! ds.RestClient := cli; // asign the client
  // ! ds.CommandText := 'select * from tablename where condition = :condition order by fieldname'; // classic SQL statement
  // ! ds1.Dataset := ds; // assigning the rest dataset to TDatasource that can be associated a TDBGrid for example.
  // ! ds.Open;
  // ! // ... use ds as usual, including modifications
  // ! ds.ApplyUpdates(0);
  // ! // or using from a service returning a dataset:
  // ! cli := TSQLHttpClient.Create(host, port, CreateModel); // create the client
  // ! ds := TSynRestDataSet.Create(MainForm);
  // ! ds.RestClient := cli; // asign the client
  // ! // the TSQLRecord associated should be defined with the same structure of the returned array from the service
  // ! // and the TSQLRecord must exists in the model
  // ! ds.CommandText := 'ServiceName.Operation?paramname=:paramname';
  // ! ds.Params.ParamByName('paramname').Value := 'xyz';
  // ! ds1.Dataset := ds; // assigning the rest dataset to TDatasource that can be associated a TDBGrid for example.
  // ! ds.Open;
  // ! // ... use ds as usual but cannot make modifications
  TSynRestSQLDataSet = class(TSynBinaryDataSet)
  protected
    fCommandText: string;
    fInsertedID: TID;
    fRestClient: TSQLRestClientURI;
    fOnGetURISignature: TGetURISignatureEvent;
    fParams: TParams;
    fProvider: TDataSetProvider;
    fIsSOAService: Boolean;
    fTableName: RawUTF8;
    function BindParams(const aStatement: RawUTF8): RawUTF8;
    function ExtractFields(const aSQL, aAfterStr, aBeforeStr: string): string;
    function GetSQLRecordClass: TSQLRecordClass;
    function GetSQLOccasion(const aSQL: string): TSQLOccasion;
    function GetTableName: RawUTF8;
    // get the data
    procedure InternalInitFieldDefs; override;
    function InternalFrom(const aStatement: RawUTF8): RawByteString;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure ParseCommandText;
    // TDataset implementation
    procedure CreateFields; override;
    // IProvider implementation
    procedure PSSetCommandText(const ACommandText: string); override;
    function PSGetTableName: string; override;
    function PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean; override;
    function PSIsSQLBased: Boolean; override;
    function PSIsSQLSupported: Boolean; override;
    {$ifdef ISDELPHIXE3}
    function PSExecuteStatement(const ASQL: string; AParams: TParams): Integer; overload; override;
    function PSExecuteStatement(const ASQL: string; AParams: TParams; var ResultSet: TDataSet): Integer; overload; override;
    {$else}
    function PSExecuteStatement(const ASQL: string; AParams: TParams; ResultSet: Pointer=nil): Integer; overload; override;
    {$endif}
    procedure SetCommandText(const Value: string);
    function SQLFieldsToJSON(const aSQLOccasion: TSQLOccasion; var aFieldNames: RawUTF8;
      const aSQL, aAfterStr, aBeforeStr: string; aParams: TParams): RawUTF8;
  public
    /// get column data for the current active row
    // - handle ftBoolean,ftInteger,ftLargeint,ftFloat,ftDate,ftTime,ftDateTime,ftTimeStamp
    // ftString,ftWideString kind of fields via GetRowFieldData()
    {$ifdef ISDELPHIXE3}
    {$ifdef ISDELPHIXE4}
    function GetFieldData(Field: TField; var Buffer: TValueBuffer): Boolean; override;
    {$else}
    function GetFieldData(Field: TField; Buffer: TValueBuffer): Boolean; override;
    {$endif}
    {$else}
    function GetFieldData(Field: TField; Buffer: pointer): Boolean; override;
    {$endif}
    {$ifndef UNICODE}
    function GetFieldData(Field: TField; Buffer: pointer; NativeFormat: Boolean): Boolean; override;
    {$endif}
    /// the associated RestClient must be set, or else access violation is raised.
    property RestClient: TSQLRestClientURI read fRestClient write fRestClient;
  published
    // - Statement is the nature SQL statement
    // examples:
    //   SELECT * FROM tableName
    //   SELECT * FROM tableName WHERE whereCondition ORDER BY orderByColumns
    //   SELECT * FROM tableName WHERE whereParam = :param ORDER BY orderByColumns
    // if :param is used then before open assign the value: ds.Params.ParamByName('param').value := XXX
    // - Statement invoking mORMot interface based service
    // example:
    //  Service.Operation?paramname=:paramvalue
    property CommandText: string read fCommandText write fCommandText;
    property InsertedID: TID read fInsertedID;
    /// the associated SynDB TDataSet, used to retrieve and update data
    /// event to get URI signature
    property OnGetURISignature: TGetURISignatureEvent write fOnGetURISignature;
  end;

  /// to get field data for TTimeLogField
  PTimeLog = ^TTimeLog;

  /// TTimeLogField for sftCreateTime, sftModTime and sftTimeLog
  TTimeLogField = class(TSQLTimeStampField)
  protected
    FDisplayFormat: string;
    function GetValue(var Value: TTimeLog): Boolean;
    procedure CopyData(Source, Dest: Pointer); override;
    function GetAsTimeLog: TTimeLog;
    function GetAsDateTime: TDateTime; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDataSize: Integer; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsTimeLog(const Value: TTimeLog);
    procedure SetAsDateTime(Value: TDateTime); override;
    procedure SetAsString(const Value: string); override;
    procedure SetVarValue(const Value: Variant); override;
    procedure SetDisplayFormat(const Value: string);
  public
    property Value: TTimeLog read GetAsTimeLog write SetAsTimeLog;
  published
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat;
    property EditMask;
  end;
   
// JSON columns to binary from a TSQLTableJSON, is not ideal because this code is a almost repeated code.
procedure JSONColumnsToBinary(const aTable: TSQLTableJSON; W: TFileBufferWriter;
  const Null: TSQLDBProxyStatementColumns;
  const ColTypes: TSQLDBFieldTypeDynArray);
// convert to binary from a TSQLTableJSON, is not ideal because this code is a almost repeated code.
function JSONToBinary(const aTable: TSQLTableJSON; Dest: TStream; MaxRowCount: cardinal=0; DataRowPosition: PCardinalDynArray=nil;
                      const DefaultDataType: TSQLDBFieldType = SynCommons.ftUTF8; const DefaultFieldSize: Integer = 255): cardinal;

implementation

uses
  TypInfo,
  DateUtils,
  DBConsts;

const
  FETCHALLTOBINARY_MAGIC = 1;

  SQLFIELDTYPETODBFIELDTYPE: array[TSQLFieldType] of TSQLDBFieldType =
    (SynCommons.ftUnknown,   // sftUnknown
     SynCommons.ftUTF8,      // sftAnsiText
     SynCommons.ftUTF8,      // sftUTF8Text
     SynCommons.ftInt64,     // sftEnumerate
     SynCommons.ftInt64,     // sftSet
     SynCommons.ftInt64,     // sftInteger
     SynCommons.ftInt64,     // sftID = TSQLRecord(aID)
     SynCommons.ftInt64,     // sftRecord = TRecordReference
     SynCommons.ftInt64,     // sftBoolean
     SynCommons.ftDouble,    // sftFloat
     SynCommons.ftDate,      // sftDateTime
     SynCommons.ftInt64,     // sftTimeLog
     SynCommons.ftCurrency,  // sftCurrency
     SynCommons.ftUTF8,      // sftObject
{$ifndef NOVARIANTS}
     SynCommons.ftUTF8,      // sftVariant
     SynCommons.ftUTF8,      // sftNullable
{$endif}
     SynCommons.ftBlob,      // sftBlob
     SynCommons.ftBlob,      // sftBlobDynArray
     SynCommons.ftBlob,      // sftBlobCustom
     SynCommons.ftUTF8,      // sftUTF8Custom
     SynCommons.ftUnknown,   // sftMany
     SynCommons.ftInt64,     // sftModTime
     SynCommons.ftInt64,     // sftCreateTime
     SynCommons.ftInt64,     // sftTID
     SynCommons.ftInt64,     // sftRecordVersion = TRecordVersion
     SynCommons.ftInt64,     // sftSessionUserID
     SynCommons.ftDate,      // sftDateTimeMS
     SynCommons.ftInt64,     // sftUnixTime
     SynCommons.ftInt64);    // sftUnixMSTime

  SQLFieldTypeToVCLDB: array[TSQLFieldType] of TFieldType =
    (DB.ftUnknown,           // sftUnknown
     DB.ftString,            // sftAnsiText
     DB.ftString,            // sftUTF8Text
     DB.ftLargeInt,          // sftEnumerate
     DB.ftLargeInt,          // sftSet
     DB.ftLargeInt,          // sftInteger
     DB.ftLargeInt,          // sftID = TSQLRecord(aID)
     DB.ftLargeInt,          // sftRecord = TRecordReference
     DB.ftLargeInt,          // sftBoolean
     DB.ftFloat,             // sftFloat
     DB.ftDateTime,          // sftDateTime
     DB.ftLargeInt,          // sftTimeLog
     DB.ftCurrency,          // sftCurrency
     DB.ftString,            // sftObject
{$ifndef NOVARIANTS}
     DB.ftString,            // sftVariant
     DB.ftString,            // sftNullable
{$endif}
     DB.ftBlob,              // sftBlob
     DB.ftBlob,              // sftBlobDynArray
     DB.ftBlob,              // sftBlobCustom
     DB.ftString,            // sftUTF8Custom
     DB.ftUnknown,           // sftMany
     DB.ftLargeInt,          // sftModTime
     DB.ftLargeInt,          // sftCreateTime
     DB.ftLargeInt,          // sftTID
     DB.ftLargeInt,          // sftRecordVersion = TRecordVersion
     DB.ftLargeInt,          // sftSessionUserID
     DB.ftDate,              // sftDateTimeMS
     DB.ftLargeInt,          // sftUnixTime
     DB.ftLargeInt);         // sftUnixmsTime

  VCLDBFieldTypeSQLDB: array[Low(TSQLFieldType)..High(TSQLFieldType)] of TSQLFieldType =
    (sftUnknown,        // ftUnknown
     sftAnsiText,       // ftString
     sftUTF8Text,       // ftString
     sftEnumerate,      // ftInteger
     sftSet,            // ftInteger
     sftInteger,        // ftInteger
     sftID,             // ftLargeInt = TSQLRecord(aID)
     sftRecord,         // ftLargeInt
     sftBoolean,        // ftBoolean
     sftFloat,          // ftFloat
     sftDateTime,       // ftDate
     sftTimeLog,        // ftLargeInt
     sftCurrency,       // ftCurrency
     sftObject,         // ftString
{$ifndef NOVARIANTS}
     sftVariant,        // ftString
     sftNullable,       // ftString
{$endif}
     sftBlob,           // ftBlob
     sftBlob,           // ftBlob
     sftBlob,           // ftBlob
     sftUTF8Custom,     // ftString
     sftMany,           // ftUnknown
     sftModTime,        // ftLargeInt
     sftCreateTime,     // ftLargeInt
     sftID,             // ftLargeInt
     sftRecordVersion,  // ftLargeInt = TRecordVersion
     sftSessionUserID,  // ftLargeInt
     sftDateTimeMS,     // ftDate
     sftUnixTime,       // ftLargeInt
     sftUnixMSTime);    // ftLargeInt

type
  /// define how a single row is identified
  // - for TSynVirtualDataSet, it is just the row index (starting at 0)
  TRecInfoIdentifier = integer;

  PRecInfoIdentifier = ^TRecInfoIdentifier;

  /// pointer to an internal structure used to identify a row position
  PRecInfo = ^TRecInfo;

  /// internal structure used to identify a row position
  TRecInfo = record
    /// define how a single row is identified
    RowIndentifier: TRecInfoIdentifier;
    /// any associated bookmark
    Bookmark: TRecInfoIdentifier;
    /// any associated bookmark flag
    BookmarkFlag: TBookmarkFlag;
  end;

{$ifndef FPC}

procedure JSONColumnsToBinary(const aTable: TSQLTableJSON; W: TFileBufferWriter;
  const Null: TSQLDBProxyStatementColumns; const ColTypes: TSQLDBFieldTypeDynArray);
var F: integer;
    VDouble: double;
    VCurrency: currency absolute VDouble;
    VDateTime: TDateTime absolute VDouble;
    colType: TSQLDBFieldType;
begin
  for F := 0 to length(ColTypes)-1 do
    if not (F in Null) then begin
      colType := ColTypes[F];
      if colType<ftInt64 then begin // ftUnknown,ftNull
        colType := SQLFIELDTYPETODBFIELDTYPE[aTable.FieldType(F)]; // per-row column type (SQLite3 only)
        W.Write1(ord(colType));
      end;
      case colType of
      SynCommons.ftInt64:
      begin
        W.WriteVarInt64(aTable.FieldAsInteger(F));
      end;
      SynCommons.ftDouble: begin
        VDouble := aTable.FieldAsFloat(F);
        W.Write(@VDouble,sizeof(VDouble));
      end;
      SynCommons.ftCurrency: begin
        VCurrency := aTable.Field(F);
        W.Write(@VCurrency,sizeof(VCurrency));
      end;
      SynCommons.ftDate: begin
        VDateTime := aTable.Field(F);
        W.Write(@VDateTime,sizeof(VDateTime));
      end;
      SynCommons.ftUTF8:
      begin
        W.Write(aTable.FieldBuffer(F));
      end;
      SynCommons.ftBlob:
      begin
        W.Write(aTable.FieldBuffer(F));
      end;
      else
      raise ESQLDBException.CreateUTF8('JSONColumnsToBinary: Invalid ColumnType(%)=%',
        [aTable.Get(0, F),ord(colType)]);
    end;
  end;
end;

function JSONToBinary(const aTable: TSQLTableJSON; Dest: TStream; MaxRowCount: cardinal=0; DataRowPosition: PCardinalDynArray=nil;
                      const DefaultDataType: TSQLDBFieldType = SynCommons.ftUTF8; const DefaultFieldSize: Integer = 255): cardinal;
var F, FMax, FieldSize, NullRowSize: integer;
    StartPos: cardinal;
    Null: TSQLDBProxyStatementColumns;
    W: TFileBufferWriter;
    ColTypes: TSQLDBFieldTypeDynArray;
    FieldType: TSQLDBFieldType;
begin
  FillChar(Null,sizeof(Null),0);
  result := 0;
  W := TFileBufferWriter.Create(Dest);
  try
    W.WriteVarUInt32(FETCHALLTOBINARY_MAGIC);
    FMax := aTable.FieldCount;
    W.WriteVarUInt32(FMax);
    if FMax>0 then begin
      // write column description
      SetLength(ColTypes,FMax);
      dec(FMax);
      for F := 0 to FMax do begin
        W.Write(aTable.Get(0, F));
        FieldType := SQLFIELDTYPETODBFIELDTYPE[aTable.FieldType(F)];
        if (FieldType = SynCommons.ftUnknown) and (DefaultDataType <> SynCommons.ftUnknown) then
          FieldType := DefaultDataType;
        ColTypes[F] := FieldType;
        FieldSize := aTable.FieldLengthMax(F);
        if (FieldSize = 0) and (FieldType = DefaultDataType) and (DefaultFieldSize <> 0) then
          FieldSize := DefaultFieldSize;
        W.Write1(ord(ColTypes[F]));
        W.WriteVarUInt32(FieldSize);
      end;
      // initialize null handling
      NullRowSize := (FMax shr 3)+1;
      if NullRowSize>sizeof(Null) then
        raise ESQLDBException.CreateUTF8(
          'JSONToBinary: too many columns', []);
      // save all data rows
      StartPos := W.TotalWritten;
      if aTable.Step or (aTable.RowCount=1) then // Need step first or error is raised in Table.Field function.
      repeat
        // save row position in DataRowPosition[] (if any)
        if DataRowPosition<>nil then begin
          if Length(DataRowPosition^)<=integer(result) then
            SetLength(DataRowPosition^,result+result shr 3+256);
          DataRowPosition^[result] := W.TotalWritten-StartPos;
        end;
        // first write null columns flags
        if NullRowSize>0 then begin
          FillChar(Null,NullRowSize,0);
          NullRowSize := 0;
        end;
        for F := 0 to FMax do
        begin
          if VarIsNull(aTable.Field(F)) then begin
            include(Null,F);
            NullRowSize := (F shr 3)+1;
          end;
        end;
        W.WriteVarUInt32(NullRowSize);
        if NullRowSize>0 then
          W.Write(@Null,NullRowSize);
        // then write data values
        JSONColumnsToBinary(aTable, W,Null,ColTypes);
        inc(result);
        if (MaxRowCount>0) and (result>=MaxRowCount) then
          break;
      until not aTable.Step;
    end;
    W.Write(@result,SizeOf(result)); // fixed size at the end for row count
    W.Flush;
  finally
    W.Free;
  end;
end;

{ TSynRestSQLDataSet }

function TSynRestSQLDataSet.ExtractFields(const aSQL, aAfterStr, aBeforeStr: string): string;
var
  lPosStart: Integer;
  lPosEnd: Integer;
  lSQL: string;
begin
  lSQL := StringReplace(aSQL, sLineBreak, ' ', [rfReplaceAll]);
  lPosStart := Pos(aAfterStr, lSQL)+Length(aAfterStr);
  if (aBeforeStr = '') then
    lPosEnd := Length(lSQL)
  else
    lPosEnd   := Pos(aBeforeStr, lSQL);
  Result := UTF8ToString(Trim(Copy(lSQL, lPosStart, lPosEnd-lPosStart)));
end;

function TSynRestSQLDataSet.SQLFieldsToJSON(const aSQLOccasion: TSQLOccasion;
  var aFieldNames: RawUTF8;
  const aSQL, aAfterStr, aBeforeStr: string; aParams: TParams): RawUTF8;
var
  I: Integer;
  lLastPos: Integer;
  lFieldValues: TStrings;
  lBlob: TSQLRawBlob;
  aFieldNameWriter: TTextWriter;
begin
  aFieldNames := '';
  lFieldValues := TStringList.Create;
  aFieldNameWriter := TTextWriter.CreateOwnedStream;
  try
    ExtractStrings([','], [], PChar(ExtractFields(aSQL, aAfterStr, aBeforeStr)), lFieldValues);
    lLastPos := 0;
    with TTextWriter.CreateOwnedStream do
    begin
      Add('{');
      for I := 0 to lFieldValues.Count-1 do
      begin
        if (Pos('=', lFieldValues[I]) = 0) then
          lFieldValues[I] := lFieldValues[I] + '=';
        AddFieldName(Trim(lFieldValues.Names[I]));

        // add fieldname to fieldname list
        aFieldNameWriter.AddString(Trim(StringToUTF8(lFieldValues.Names[I])));
        aFieldNameWriter.Add(',');

        if (aParams[I].DataType <> ftBlob) then
        begin
          if (TVarData(aParams[I].Value).VType = varString) then
            AddVariant(StringToUTF8(aParams[I].Value))
          else
            AddVariant(aParams[I].Value);
        end
        else
        begin
          Add('"');
          lBlob :=  BlobToTSQLRawBlob(PUTF8Char(aParams[I].AsBlob));
          AddJSONEscapeString(UTF8ToString(TSQLRawBlobToBlob(lBlob)));
          Add('"');
        end;
        Add(',');
        lLastPos := I;
      end;
      CancelLastComma;
      Add('}');
      Result := Text;
      Free;
    end;
    aFieldNameWriter.CancelLastComma;
    aFieldNames := aFieldNameWriter.Text;
    lFieldValues.Clear;
    // the first field after the where clause is the ID
    if (aSQLOccasion <> soInsert) then
      aParams[lLastPos+1].Name := 'ID';
  finally
    aFieldNameWriter.Free;
    lFieldValues.Free;
  end;
end;

{$ifndef UNICODE}
function TSynRestSQLDataSet.GetFieldData(Field: TField; Buffer: Pointer;
  NativeFormat: Boolean): Boolean;
begin
  if (Field.DataType in [ftTimeStamp]) and (Buffer<>nil) then
    Result := GetFieldData(Field, Buffer, NativeFormat)
  else
    Result := inherited GetFieldData(Field, Buffer, NativeFormat);
end;
{$endif}

{$ifdef ISDELPHIXE3}
{$ifdef ISDELPHIXE4}
function TSynRestSQLDataSet.GetFieldData(Field: TField; var Buffer: TValueBuffer): Boolean;
{$else}
function TSynRestSQLDataSet.GetFieldData(Field: TField; Buffer: TValueBuffer): Boolean;
{$endif}
{$else}
function TSynRestSQLDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
{$endif}
var Data, Dest: pointer;
    RowIndex, DataLen: integer;
begin
  if Field.DataType<>ftTimeStamp then begin
    Result := inherited GetFieldData(Field, Buffer);
    exit;
  end;

  result := false;
  RowIndex := PRecInfo(ActiveBuffer).RowIndentifier;
  Data := GetRowFieldData(Field,RowIndex,DataLen,False);
  if Data=nil then // on success, points to Int64,Double,Blob,UTF8
    exit;
  result := true;
  Dest := pointer(Buffer); // works also if Buffer is [var] TValueBuffer
  PTimeLog(Dest)^ := PTimeLog(Data)^;
end;

function TSynRestSQLDataSet.GetSQLOccasion(const aSQL: string): TSQLOccasion;
begin
  if IdemPChar(PUTF8Char(UpperCase(aSQL)), 'DELETE') then
    Result := soDelete
  else if IdemPChar(PUTF8Char(UpperCase(aSQL)), 'INSERT') then
    Result := soInsert
  else
    Result := soUpdate;
end;

function TSynRestSQLDataSet.BindParams(const aStatement: RawUTF8): RawUTF8;
var
  I: Integer;
  lParamName, value: string;
begin
  Result := aStatement;
  if (PosEx(':', aStatement) = 0) and (fParams.Count = 0) then
    Exit;
  if ((PosEx(':', aStatement) = 0) and (fParams.Count > 0)) or ((PosEx(':', aStatement) > 0) and (fParams.Count = 0)) then
    raise ESQLRestException.CreateUTF8('Statement parameters (%) not match with Params (Count=%) property',
      [aStatement, fParams.Count]);
  for I := 0 to fParams.Count-1 do
  begin
    lParamName := ':' + fParams[I].Name;
    if fParams[i].IsNull then
      value:='null'
    else
    begin
      case fParams[i].DataType of
        ftString,ftWideString: value:=fParams[I].AsString;
      else
        value:=fParams[I].AsString;
      end;
    end;
    Result := StringReplaceAll(Result,lParamName,value);
  end;
end;

procedure TSynRestSQLDataSet.CreateFields;

  // from http://delphi-inspired.blogspot.com.ar/2012/04/how-to-clone-tfield-and-tdataset-fields.html
  function CloneField(Source: TField; AOwner: TComponent): TField;

    procedure SetProp(const Name: string; const Source: TField; Result: TField);
    var
      V: variant;
      PropInfo: PPropInfo;
    begin
     PropInfo := TypInfo.GetPropInfo(Source,Name);
     if PropInfo <> nil then
       try
        V := TypInfo.GetPropValue(Source,Name);
        if not VarIsNull(V) then
           TypInfo.SetPropValue(Result,Name,V);
       except
        ; //just kill exception
       end;
    end;

  var
    i: Integer;
  const
    CloneProperty: array [0..18] of string =
    ( 'EditMask', 'FixedChar', 'Size', 'Transliterate', 'DisplayFormat'
    , 'EditFormat', 'Currency', 'MaxValue', 'MinValue', 'Precision'
    , 'DisplayValues', 'BlobType', 'ObjectType', 'IncludeObjectField'
    , 'ReferenceTableName', 'Active', 'Expression', 'GroupingLevel'
    , 'IndexName');
  begin
    Result := TFieldClass(Source.ClassType).Create(AOwner);
    Result.Alignment := Source.Alignment;
    Result.AutoGenerateValue := Source.AutoGenerateValue;
    Result.CustomConstraint := Source.CustomConstraint;
    Result.ConstraintErrorMessage := Source.ConstraintErrorMessage;
    Result.DefaultExpression := Source.DefaultExpression;
    Result.DisplayLabel := Source.DisplayLabel;
    Result.DisplayWidth := Source.DisplayWidth;
    Result.FieldKind := Source.FieldKind;
    Result.FieldName := Source.FieldName;
    Result.ImportedConstraint := Source.ImportedConstraint;
    Result.LookupDataSet := Source.LookupDataSet;
    Result.LookupKeyFields := Source.LookupKeyFields;
    Result.LookupResultField := Source.LookupResultField;
    Result.KeyFields := Source.KeyFields;
    Result.LookupCache := Source.LookupCache;
    Result.ProviderFlags := Source.ProviderFlags;
    Result.ReadOnly := Source.ReadOnly;
    Result.Required := Source.Required;
    Result.Visible := Source.Visible;

    for i := Low(CloneProperty) to High(CloneProperty) do
      SetProp(CloneProperty[i], Source, Result);
  end;

  // http://delphi-inspired.blogspot.com.ar/2012/04/how-to-clone-tfield-and-tdataset-fields.html?showComment=1360167449095#c387358386002226802
  procedure CopyFields(SourceDataset, DestDataset: TDataset; doAdd: Boolean);
  var
    i: integer;
    Fld: TField;
  begin
    if not doAdd then DestDataset.Fields.Clear;
    for i:=0 to SourceDataset.Fields.Count-1 do begin
      if Assigned(DestDataset.Fields.FindField(SourceDataset.Fields[i].FieldName)) then
         Continue;
      Fld := CloneField(SourceDataset.Fields[i], DestDataset.Fields.Dataset);
      Fld.DataSet := DestDataset.Fields.Dataset;
    end;
  end;

var
  lField: TField;
begin
  inherited;
  // copy fields from this dataset to TSynRestDataset only if fields were added manually (calculated fields, etc.)
  // in TSynRestDataset or if FieldDefs has data mean that it is in refreshing state
  if (TDataset(Owner).Fields.Count=0) or (TDataset(Owner).FieldDefs.Count>0) then
    Exit;
  CopyFields(Self, TDataset(Owner), True);
  // move calculated fields to the end
  lField := TDataset(Owner).Fields[0];
  // when refreshing the dataset it's active
  if lField.Dataset.Active then
    lField.Dataset.Close
  else
    // remove field
    lField.Dataset := nil;
  // add again the field
  lField.Dataset := TDataset(Owner);
end;

function TSynRestSQLDataSet.GetSQLRecordClass: TSQLRecordClass;
begin
  Result := fRestClient.Model.Table[GetTableName];
  if not Assigned(Result) then
    raise ESQLRestException.CreateUTF8('Table % not registered in SQL Model', [GetTableName]);
end;

function TSynRestSQLDataSet.GetTableName: RawUTF8;
var
  I: Integer;
begin
  if not fIsSOAService then
    Result := StringToUTF8(PSGetTableName)
  else
  begin
    Result := fTableName;
    I := Pos(StringToUTF8('.'), Result);
    if (I>0) then
      Result[I] := '_';  // change only the firs found
  end;
end;

procedure TSynRestSQLDataSet.InternalClose;
begin
  inherited InternalClose;
  FreeAndNil(fDataAccess);
  fData := '';
end;

function TSynRestSQLDataSet.InternalFrom(const aStatement: RawUTF8): RawByteString;

  function ExecuteSOAService(const aSOAService: RawUTF8): TSQLTableJSON;
  var
    lRespStr, lTmp, lErrMsg: RawUTF8;
    lRespObj: TDocVariantData;
  begin
    Result := Nil;
    if fRestClient.URI(fRestClient.Model.Root+'/'+ aSOAService,'GET',@lRespStr).lo=HTTP_SUCCESS then begin
      lRespObj.InitJSON(lRespStr);
      if (lRespObj.Kind = dvUndefined) then
        raise ESQLRestException.CreateUTF8('Invalid JSON response' + sLineBreak + '%' + sLineBreak + 'from' + sLineBreak + '%',
                                       [lRespStr, aSOAService]);
      if (lRespObj.Kind = dvObject) then
        if (lRespObj.GetValueIndex('errorCode') > -1) then
          if (lRespObj.GetValueIndex('errorText') > -1) then
          begin
            lErrMsg := AnyAnsiToUTF8(StringToAnsi7(VarToStr(lRespObj.Value['errorText'])));
            raise ESQLRestException.CreateUTF8('Error' + sLineBreak + '%' + sLineBreak + 'from' + sLineBreak + '%',
                                           [lRespObj.Value['errorText'], aSOAService]);
          end
          else if (lRespObj.GetValueIndex('error') > -1) then
          begin
            lErrMsg := AnyAnsiToUTF8(StringToAnsi7(VarToStr(lRespObj.Value['error'])));
            raise ESQLRestException.CreateUTF8('Error' + sLineBreak + '%' + sLineBreak + 'from' + sLineBreak + '%', [lErrMsg, aSOAService]);
          end;

      lTmp := StringToUTF8(VarToStr(lRespObj.Values[0]));
      lRespObj.Clear;
      lRespObj.InitJSON(lTmp);
      if (lRespObj.Kind <> dvArray) then
        raise ESQLRestException.CreateUTF8('The service % not return an array: <%>', [fTableName, lRespStr]);
      // if the array is empty, nothing to return
      lRespStr := StringToUTF8(VarToStr(lRespObj.Values[0]));
      if (lRespStr = '') or (lRespStr = '[]') or (lRespStr = '{}') then
        raise ESQLRestException.CreateUTF8('Service % not return a valid array: <%>', [fTableName, lRespStr]);

      Result := TSQLTableJSON.CreateFromTables([GetSQLRecordClass], '', lRespStr);
    end;
  end;

var
  lData: TRawByteStringStream;
  lSQLTableJSON: TSQLTableJSON;
  lStatement: RawUTF8;
begin
  Result := '';
  lStatement := BindParams(aStatement);
  if fIsSOAService then
    lSQLTableJSON := ExecuteSOAService(lStatement)
  else
    lSQLTableJSON := fRestClient.ExecuteList([GetSQLRecordClass], lStatement);

  if not Assigned(lSQLTableJSON) then
    raise ESQLRestException.Create(UTF8ToString(TSQLRestClientURI(fRestClient).LastErrorMessage));

  lData := TRawByteStringStream.Create('');
  try
    JSONToBinary(lSQLTableJSON, lData);
    Result := lData.DataString
  finally
    FreeAndNil(lData);
    FreeAndNil(lSQLTableJSON);
  end;
end;

procedure TSynRestSQLDataSet.InternalInitFieldDefs;
var
  F, lFieldWidth: integer;
  lFieldType: TSQLFieldType;
  lFields: TSQLPropInfoList;
  lFieldDef: TFieldDef;
begin
  inherited;
  if (GetTableName = '') then // JSON conversion to dataset ?
    Exit;
  // update field definitions from associated TSQLRecordClass of the table
  lFields := GetSQLRecordClass.RecordProps.Fields;
  for F := 0 to lFields.Count-1 do
  begin
    // in this way an exception is not raised if field name is not found
    lFieldDef := TFieldDef(TDefCollection(FieldDefs).Find(lFields.Items[F].Name));
    if Assigned(lFieldDef) then begin
      lFieldWidth := lFields.Items[F].FieldWidth;
      lFieldType := lFields.Items[F].SQLFieldType;

      if (lFields.Items[F].SQLFieldType in [sftCreateTime,sftModTime,sftTimeLog]) then begin
        lFieldDef.DataType := ftTimeStamp
      end else if lFieldDef.DataType<>SQLFieldTypeToVCLDB[lFieldType] then
        lFieldDef.DataType := SQLFieldTypeToVCLDB[lFieldType];
      if (lFieldWidth>0) and (lFieldDef.Size<>lFieldWidth) then
        lFieldDef.Size := lFieldWidth;
    end;
  end;
end;

procedure TSynRestSQLDataSet.InternalOpen;
var
  lData: RawByteString;
begin
  if (fCommandText='') then begin
    if fData<>'' then // called e.g. after From() method
      inherited InternalOpen;
    exit;
  end;
  lData := InternalFrom(fCommandText);
  if (lData <> '') then
  begin
    From(lData);
    inherited InternalOpen;
  end;
end;

procedure TSynRestSQLDataSet.ParseCommandText;
var
  temp: RawUTF8;
begin
  // if fCommandText is in the nature SQL Statement form. eg. SELECT * FROM tableName [WHERE ...]
  Split(UpperCase(StringToUTF8(fCommandText)), 'FROM', temp, fTableName);
  if (fTableName<>'') then begin
    fTableName := Trim(fTableName);
    Split(fTableName, ' ', fTableName, temp);
  end else begin // assumed SOA service by interface
    fIsSOAService := True;
    Split(StringToUTF8(fCommandText), '?', fTableName, temp);
  end;
end;

{$ifdef ISDELPHIXE3}
function TSynRestSQLDataSet.PSExecuteStatement(const ASQL: string;
  AParams: TParams): Integer;
var DS: TDataSet;
begin
  DS := nil;
  result := PSExecuteStatement(ASQL,AParams,DS);
  DS.Free;
end;

function TSynRestSQLDataSet.PSExecuteStatement(const ASQL:
    string; AParams: TParams; var ResultSet: TDataSet): Integer;
{$else}
function TSynRestSQLDataSet.PSExecuteStatement(const ASQL: string;
    AParams: TParams; ResultSet: Pointer): Integer;
{$endif}
var
  lJSONText: RawUTF8;
  lOccasion: TSQLOccasion;
  lFieldNames: RawUTF8;
  lRec: TSQLRecord;
begin // only execute writes in current implementation
  Result := -1;
  if fIsSOAService then
    DatabaseError('Cannot apply updates to SOA service');
  lOccasion := GetSQLOccasion(aSQL);
  case lOccasion of
  soDelete: begin
      lJSONText := SQLFieldsToJSON(soDelete, lFieldNames, aSQL, 'where ', '', aParams);
      lRec := GetSQLRecordClass.CreateFrom(lJSONText);
      try
        if fRestClient.Delete(GetSQLRecordClass, lRec.IDValue) then
          Result := 1;
      finally
        lRec.Free;
      end;
    end;
  soInsert: begin
      lJSONText := SQLFieldsToJSON(soInsert, lFieldNames, aSQL, '(', ') ', aParams);
      lRec := GetSQLRecordClass.CreateFrom(lJSONText);
      try
        fInsertedID := fRestClient.Add(lRec, lRec.RecordProps.FieldBitsFromCSV(lFieldNames), True);
        if fInsertedID>0 then
          Result := 1;
      finally
        lRec.Free;
      end;
    end;
  soUpdate: begin
      lJSONText := SQLFieldsToJSON(soUpdate, lFieldNames, aSQL, 'set ', 'where ', aParams);
      lRec := GetSQLRecordClass.CreateFrom(lJSONText);
      try
        if (aParams.FindParam('ID')<>nil) then
          lRec.IDValue := aParams.ParamByName('ID').Value;  // fRec.ID is readonly, fRec.IDValue is writable
        if fRestClient.Update(lRec, lRec.RecordProps.FieldBitsFromCSV(lFieldNames)) then
          Result := 1;
      finally
        lRec.Free;
      end;
    end
  end;
  if not StatusCodeIsSuccess(fRestClient.LastErrorCode) then
    DatabaseError(fRestClient.LastErrorMessage);
end;

function TSynRestSQLDataSet.PSGetTableName: string;
begin
  Result := fTableName;
end;

function TSynRestSQLDataSet.PSIsSQLBased: Boolean;
begin
  result := true;
end;

function TSynRestSQLDataSet.PSIsSQLSupported: Boolean;
begin
  result := true;
end;

procedure TSynRestSQLDataSet.PSSetCommandText(const ACommandText: string);
begin
  if (fCommandText <> ACommandText) then
    SetCommandText(ACommandText);
end;

function TSynRestSQLDataSet.PSUpdateRecord(UpdateKind: TUpdateKind;
  Delta: TDataSet): Boolean;
begin
  result := false;
end;

procedure TSynRestSQLDataSet.SetCommandText(const Value: string);
begin
  if (Value <> fCommandtext) then
  begin
    fCommandText := Value;
    ParseCommandText;
  end;
end;


{$endif FPC}

{ TTimeLogField }

procedure TTimeLogField.CopyData(Source, Dest: Pointer);
begin
  TTimeLog(Dest^) := TTimeLog(Source^);
end;

function TTimeLogField.GetAsDateTime: TDateTime;
var
  tl: TTimeLog;
begin
  if not GetValue(tl) then
    Result := 0
  else
    Result := TimeLogToDateTime(tl);
end;

function TTimeLogField.GetAsString: string;
begin
  GetText(Result, False);
end;

function TTimeLogField.GetAsTimeLog: TTimeLog;
begin
  if not GetValue(Result) then Result := 0;
end;

function TTimeLogField.GetAsVariant: Variant;
var
  D: TTimeLog;
begin
  if GetValue(D) then
    Result := D
  else Result := Null;
end;

function TTimeLogField.GetDataSize: Integer;
begin
  Result := SizeOf(TTimeLog);
end;

procedure TTimeLogField.GetText(var Text: string; DisplayText: Boolean);
var
  F: string;
  D: TTimeLog;
  lFS: TFormatSettings;
  lTZ: TTimeZoneInformation;
begin
  if GetValue(D) then begin
    if DisplayText and (FDisplayFormat <> '') then
      F := FDisplayFormat
    else
      F := '';
    GetLocaleFormatSettings(GetUserDefaultLCID, lFS);
    GetTimeZoneInformation(lTZ);
    FillCharFast(lTZ, SizeOf(lTZ), 0);
    if F<>'' then
      Text := FormatDateTime(F, IncHour(TimeLogToDateTime(D),(lTZ.Bias div -60)),lFS)
    else
      Text := DateTimeToStr(IncHour(TimeLogToDateTime(D),(lTZ.Bias div -60)),lFS);
  end else
    Text := '';
end;

function TTimeLogField.GetValue(var Value: TTimeLog): Boolean;
begin
  Result := GetData(@Value, False);
end;

procedure TTimeLogField.SetAsDateTime(Value: TDateTime);
var
  tl: TTimeLog;
begin
  if Value = 0 then Clear else
  begin
    tl := TimeLogFromDateTime(Value);
    SetAsTimeLog(tl);
  end;
end;


procedure TTimeLogField.SetAsString(const Value: string);
var
  lFS: TFormatSettings;
  lTZ: TTimeZoneInformation;
begin
  if Value = '' then Clear else begin
    GetLocaleFormatSettings(GetUserDefaultLCID, lFS);
    FillCharFast(lTZ, SizeOf(lTZ), 0);
    GetTimeZoneInformation(lTZ);
    SetAsTimeLog(TimeLogFromDateTime(IncHour(StrToDateTime(Value, lFS),(lTZ.Bias div 60))));
  end;
end;

procedure TTimeLogField.SetAsTimeLog(const Value: TTimeLog);
begin
  SetData(@Value, False);
end;

procedure TTimeLogField.SetVarValue(const Value: Variant);
var
  lValue: Int64;
begin
  if VarIsClear(Value) then
    DatabaseError(SUnassignedVar);
  VariantToInt64(Value, lValue);
  SetAsTimeLog(TTimeLog(lValue));
end;

procedure TTimeLogField.SetDisplayFormat(const Value: string);
begin
  if FDisplayFormat <> Value then
  begin
    FDisplayFormat := Value;
    PropertyChanged(False);
  end;
end;

var
  FieldClass: ^TFieldClass = nil; // temp var

initialization
  // force field type ftTimeStamp to use TTimeLogField instead of TSQLTimeStampField
  FieldClass := @DefaultFieldClasses[ftTimeStamp];
  FieldClass^:= TTimeLogField;

finalization
  // restore field type ftTimeStamp to use TSQLTimeStampField
  if Assigned(FieldClass) then
    FieldClass^ := TSQLTimeStampField;
end.
