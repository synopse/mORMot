/// minimum stand-alone cross-platform REST process for mORMot client
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.18
unit SynCrossPlatformREST;

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
  - would compile with Delphi for any platform, or with FPC or Kylix

}

{$i SynCrossPlatform.inc} // define e.g. HASINLINE

interface

uses
  SysUtils,
  Classes,
  Contnrs,
  Variants,
  TypInfo,
  SynCrossPlatformJSON;

type
  /// alias to share the same string type between client and server
  RawUTF8 = string;

  /// alias to share the same blob type between client and server
  TSQLRawBlob = TByteDynArray;

  /// Exception type raised when working with REST access
  ERestException = class(Exception);

  /// handle a JSON result table, as returned by mORMot's REST server ORM
  // - this class is expected to work with TSQLRecord instances only
  // - it will let any "RowID" JSON key match TSQLRecord.ID property
  TJSONTableREST = class(TJSONTableObject)
  protected
    /// allow to let "RowID" JSON key match TSQLRecord.ID
    function GetPropInfo(aTypeInfo: PTypeInfo; const PropName: string): PPropInfo; override;
  end;

  /// used e.g. by TSynTest for each test case
  TPublishedMethod = record
    Name: string;
    Method: TMethod;
  end;
  /// as filled by GetPublishedMethods()
  TPublishedMethodDynArray = array of TPublishedMethod;

  {$M+}
  TSQLRest = class;
  TSQLRecord = class;
  TSQLModel = class;

  TSQLRecordClass = class of TSQLRecord;
  TSQLRecordClassDynArray = array of TSQLRecordClass;

  /// store the database model
  TSQLModel = class
  protected
    fRoot: string;
    fTables: TSQLRecordClassDynArray;
    fTableNames: TStringDynArray;
  public
    /// initialize the Database Model
    // - set the Tables to be associated with this Model, as TSQLRecord classes
    // - set the optional Root URI path of this Model
    constructor Create(const Tables: array of TSQLRecordClass;
      const aRoot: string='root'); reintroduce;
    /// get index of aTable in Tables[], returns -1 if not found
    function GetTableIndex(aTable: TSQLRecordClass): integer; overload;
    /// get index of aTable in Tables[], returns -1 if not found
    function GetTableIndex(const aTableName: string): integer; overload;
    /// get index of aTable in Tables[], raise an ERestException if not found
    function GetTableIndexExisting(aTable: TSQLRecordClass): integer;
    /// the Root URI path of this Database Model
    property Root: string read fRoot;
    /// get the classes list (TSQLRecord descendant) of all registered tables
    property Tables: TSQLRecordClassDynArray read fTables;
    /// get the short names of all registered tables
    // - i.e. 'People' for TSQLRecordPeople
    property TableNames: TStringDynArray read fTableNames;
  end;
  
  /// abstract ORM class to access remote tables
  TSQLRecord = class
  protected
    fID: integer;
    fInternalState: cardinal;
    fFill: TJSONTableREST;
  public
    /// this constructor initializes the record
    constructor Create; overload; virtual;
    /// this constructor loads a record from a REST instance from its ID
    constructor Create(aClient: TSQLRest; aID: integer;
      ForUpdate: boolean=false); overload;
    /// this constructor loads a record from a REST instance
    // - you can bind parameters by using ? in the SQLWhere clause 
    constructor Create(aClient: TSQLRest; const SQLWhere: string;
      const BoundsSQLWhere: array of const); overload;
    /// internal state counter of the mORMot server at last access time
    // - can be used to check if retrieved data may be out of date
    property InternalState: cardinal read fInternalState;
  published
    /// stores the record's primary key
    property ID: integer read fID write fID;
  end;

  /// abstract REST access class
  TSQLRest = class
  protected
  public
    /// get a member from its ID
    // - return true on success
   function Retrieve(aID: integer; Value: TSQLRecord;
      ForUpdate: boolean=false): boolean; overload; virtual; abstract;
    /// get a member from a where clause
    // - you can bind parameters by using ? in the SQLWhere clause
   function Retrieve(const SQLWhere: string; Value: TSQLRecord): boolean;
     overload; virtual; abstract;
    /// get a member from a where clause
    // - you can bind parameters by using ? in the SQLWhere clause
   function Retrieve(const SQLWhere: string; const BoundsSQLWhere: array of const;
     Value: TSQLRecord): boolean; overload; virtual; 
  end;
  {$M-}

/// retrieve all the published methods of a given class, using RTTI
procedure GetPublishedMethods(Instance: TObject;
  out Methods: TPublishedMethodDynArray);

/// true if PropName is either 'ID' or 'RowID'
function IsRowID(const PropName: string): boolean;
  {$ifndef FPC}{$ifdef HASINLINE}inline;{$endif}{$endif}

/// convert text into "text" 
procedure DoubleQuoteStr(var text: string);

/// can be used to create a statement with inlined parameters 
function FormatBind(const SQLWhere: string;
  const BoundsSQLWhere: array of const): string;

  
implementation

procedure GetPublishedMethods(Instance: TObject;
  out Methods: TPublishedMethodDynArray);
var n: integer;
  procedure AddParentsFirst(C: TClass);
  type
    TMethodInfo = packed record
    {$ifdef FPC}
      Name: PShortString;
      Addr: Pointer;
    {$else}
      Len: Word;
      Addr: Pointer;
      Name: ShortString;
    {$endif}
    end;
  var M: ^TMethodInfo;
      Method: TMethod;
      i,MCount: integer;
  begin
    if C=nil then
      exit;
    AddParentsFirst(C.ClassParent); // put children methods afterward
    M := PPointer(NativeInt(C)+vmtMethodTable)^;
    if M=nil then
      exit;
    Method.Data := Instance;
    MCount := {$ifdef FPC}PCardinal{$else}PWord{$endif}(M)^;
    inc({$ifdef FPC}PCardinal{$else}PWord{$endif}(M));
    for i := 1 to MCount do begin
      Method.Code := M^.Addr;
      if n>=length(Methods) then
        SetLength(Methods,n+32);
      Methods[n].Name := string(M^.Name{$ifdef FPC}^{$endif});
      Methods[n].Method := Method;
      inc(n);
      {$ifdef FPC}
      inc(M);
      {$else}
      inc(PByte(M),M^.Len);
      {$endif}
    end;
  end;
begin
  if Instance=nil then
    exit;
  n := 0;
  AddParentsFirst(Instance.ClassType);
  SetLength(Methods,n);
end;

function IsRowID(const PropName: string): boolean;
begin
  result := IdemPropName(PropName,'ID') or
            IdemPropName(PropName,'RowID');
end;

procedure DoubleQuoteStr(var text: string);
var i,j: integer;
    tmp: string;
begin
  i := pos('"',text);
  if i=0 then begin
    text := '"'+text+'"';
    exit;
  end;
  tmp := '"'+copy(text,1,i)+'"';
  for j := i+1 to length(text) do
    if text[j]='"' then
      tmp := tmp+'""' else
      AppendChar(tmp,text[j]);
  text := tmp+'"';
end;

function FormatBind(const SQLWhere: string;
  const BoundsSQLWhere: array of const): string;
var i,deb,arg: integer;
    tmpIsString: Boolean;
    tmp: string;
begin
  result := '';
  arg := 0;
  i := 1;
  deb := 1;
  while i<=length(SQLWhere) do
    if SQLWhere[i]='?' then begin
      result := result+copy(SQLWhere,deb,i-deb)+':(';
      if arg>high(BoundsSQLWhere) then
        tmp := 'null' else
        with BoundsSQLWhere[arg] do begin
        tmpIsString := not (VType in
          [vtBoolean,vtInteger,vtInt64,vtCurrency,vtExtended,vtVariant]);
        case VType of
        vtString:     tmp := string(VString^);
        vtAnsiString: tmp := string(AnsiString(VAnsiString));
        {$ifdef UNICODE}
        vtUnicodeString: tmp := string(VUnicodeString);
        {$endif}
        vtWideString: tmp := string(WideString(VWideString));
        vtPChar:      tmp := string(VPChar);
        vtChar:       tmp := string(VChar);
        vtPWideChar:  tmp := string(VPWideChar);
        vtWideChar:   tmp := string(VWideChar);
        vtBoolean:    if VBoolean then tmp := '1' else tmp := '0';
        vtInteger:    tmp := IntToStr(VInteger);
        vtInt64:      tmp := IntToStr(VInt64^);
        vtCurrency:   DoubleToJSON(VCurrency^,tmp);
        vtExtended:   DoubleToJSON(VExtended^,tmp);
        vtVariant: if TVarData(VVariant^).VType<=varNull then
          result := 'null' else begin
          tmpIsString := VarIsStr(VVariant^);
          tmp := VVariant^;
        end;
        else tmp := '';
        end;
        if tmpIsString then
          DoubleQuoteStr(tmp);
        inc(arg);
      end;
      result := result+tmp+'):';
      inc(i);
      deb := i;
    end else
      inc(i);
  result := result+copy(SQLWhere,deb,i-deb);
end;


{ TSQLRecord }

constructor TSQLRecord.Create;
begin
  // do nothing by now: inherited classes may set some properties
end;

constructor TSQLRecord.Create(aClient: TSQLRest; aID: integer;
  ForUpdate: boolean=false);
begin
  Create;
  if aClient<>nil then
    aClient.Retrieve(aID,self,ForUpdate);
end;

constructor TSQLRecord.Create(aClient: TSQLRest;
  const SQLWhere: string; const BoundsSQLWhere: array of const);
begin
  Create;
  if aClient<>nil then
    aClient.Retrieve(SQLWhere,BoundsSQLWhere,self);
end;


{ TJSONTableREST }

function TJSONTableREST.GetPropInfo(aTypeInfo: PTypeInfo;
  const PropName: string): PPropInfo;
begin
  result := inherited GetPropInfo(aTypeInfo,PropName);
  if (result=nil) and IdemPropName(PropName,'RowID') then
    result := inherited GetPropInfo(aTypeInfo,'ID');
end;


{ TSQLModel }

function GetDisplayNameFromClass(C: TClass): string;
begin
  if C=nil then
    result := '' else begin
    result := C.ClassName;
    if IdemPropName(copy(result,1,4),'TSQL') then
      if IdemPropName(copy(result,5,6),'Record') then
        delete(result,1,10) else
        delete(result,1,4) else
      if result[1]<>'T' then
        delete(result,1,1);
  end;
end;

constructor TSQLModel.Create(const Tables: array of TSQLRecordClass;
  const aRoot: string);
var i: integer;
begin
  SetLength(fTables,length(Tables));
  SetLength(fTableNames,length(Tables));
  for i := 0 to high(Tables) do begin
    fTables[i] := Tables[i];
    fTableNames[i] := GetDisplayNameFromClass(Tables[i]);
  end;
  if aRoot<>'' then
    if aRoot[length(aRoot)]='/' then
      fRoot := copy(aRoot,1,Length(aRoot)-1) else
      fRoot := aRoot;
end;

function TSQLModel.GetTableIndex(aTable: TSQLRecordClass): integer;
begin
  for result := 0 to High(fTables) do
    if fTables[result]=aTable then
      exit;
  result := -1;
end;

function TSQLModel.GetTableIndex(const aTableName: string): integer;
begin
  for result := 0 to High(fTables) do
    if IdemPropName(fTableNames[result],aTableName) then
      exit;
  result := -1;
end;

function TSQLModel.GetTableIndexExisting(aTable: TSQLRecordClass): integer;
begin
  result := GetTableIndex(aTable);
  if result<0 then
    raise ERestException.CreateFmt('%s should be part of the Model',
      [aTable.ClassName]);
end;

{ TSQLRest }

function TSQLRest.Retrieve(const SQLWhere: string;
  const BoundsSQLWhere: array of const; Value: TSQLRecord): boolean;
begin
  result := Retrieve(FormatBind(SQLWhere,BoundsSQLWhere),Value);
end;

initialization

end.
