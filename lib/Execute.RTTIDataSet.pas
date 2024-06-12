unit Execute.RTTIDataSet;
{
   Delphi RTTIDataSet

   (c)2024 Execute SARL  <contact@execute.fr>

   http://www.execute.fr

}
interface
{.$DEFINE LOG}
uses
{$IFDEF LOG}
  Winapi.Windows,
{$ENDIF}
  System.Classes,
  System.TypInfo,
  System.SysUtils,
  System.DateUtils,
  Data.DB,
  FireDAC.Comp.Client,
  Execute.RTTI;

type
  TSQLFieldName = class(TCustomAttribute)
    constructor Create(const AFieldName: string);
    class function GetName(Field: PRecordTypeField): string;
  end;

  TSQLFormat = class(TCustomAttribute)
    class procedure Format(Instance: Pointer; TypeInfo: PTypeInfo); virtual; abstract;
    class procedure Process(Instance: Pointer; Field: PRecordTypeField);
  end;

  TSQLFormatClass = class of TSQLFormat;

  TStringsHelper = class helper for TStrings
    procedure SelectType(TypeInfo: PTypeInfo; const From: string);
  end;

  TFieldHelper = class helper for TField
    function AssignTo(Instance: Pointer; TypeInfo: PTypeInfo): Boolean;
  end;

  TDataSetHelper = class helper for TDataSet
    function Fetch: Boolean; overload;
    function Fetch<T>(var V: T): Boolean; overload; inline;
    procedure ReadType(Instance: Pointer; TypeInfo: PTypeInfo);
    procedure Read<T>(var V: T); inline;
    procedure ReadAllType(var Instance: Pointer; TypeInfo: PTypeInfo; PageSize: Integer = 100);
  end;

  TFieldFlag = (
    ffUnique,
    ffAutoInc,
    ffReadOnly,
    ffNow
  );
  TFieldFlags = set of TFieldFlag;
  FieldFlags = class(TCustomAttribute)
    constructor Create(Flags: TFieldFlags);
  end;

  MaxLength = class(TCustomAttribute)
    constructor Create(MaxLength: Integer);
  end;

  DisplayLabel = class(TCustomAttribute)
    constructor Create(const DisplayLabel: string);
  end;

  TRTTIDataSet = class(TDataSet)
  private
    FArrayType  : PTypeInfo;
    FRecordType : PTypeInfo;
    FInstance   : PPointer;
    FRecordSize : Integer;
    FRecordCount: Integer;
    FIsOpen     : Boolean;
    FRecNo      : Integer;
    FFieldFlags : TArray<TFieldFlags>;
    FAutoIncs   : Integer;
    FNowCount   : Integer;
  {$IFDEF DEBUG}
    FBufferList : TList;
  {$ENDIF}
    procedure InternalInit;
    procedure AutoInc(Buffer: Pointer);
  protected
    function GetRecordSize: Word; override;
    function IsCursorOpen: Boolean; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
  // Navigation
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    procedure InternalFirst; override;
    procedure InternalLast; override;
    procedure InternalSetToRecord(Buffer: TRecBuf); override;
  // Bookmark
    procedure InternalGotoBookmark(Bookmark: TBookmark); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: TBookmark); override;
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: TBookmark); override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
  // Insert
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer); override;
    procedure InternalPost; override;
    procedure InternalDelete; override;
    function GetIsIndexField(Field: TField): Boolean; override;
  public
  {$IFDEF DEBUG}
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  {$ENDIF}
    function GetFieldData(Field: TField; var Buffer: TValueBuffer): Boolean; override;
    procedure SetData<T>(var V: TArray<T>);
  end;

  TFDQueryEx = class(TFDQuery)
    // Select<RECORD>('FROM TABLE WHERE...');
    procedure Select<T>(const From: string); inline;
    procedure FetchAllType(var Instance: Pointer; TypeInfo: PTypeInfo; const From: string; PageSize: Integer = 100);
    // FetchAll<RECORD>('FROM TABLE WHERE ...')  PageSize = allocation unit
    function FetchAll<T>(const From: string; PageSize: Integer = 100): TArray<T>; inline;
    function ReadAll<T>(PageSize: Integer = 100): TArray<T>; inline;
    function FetchInteger(const aSQL: string; ADefault: Integer = 0): Integer;
    function FetchString(const aSQL: string; const ADefault: string = ''): string;
    function FetchDateTime(const aSQL: string; const ADefault: TDateTime): TDateTime;
    function Lookup<T>(var V: T; const From: string): Boolean; reintroduce; inline;
  end;

  ITransaction = interface
    function Query: TFDQueryEx;
  end;

  TTransactionHolder = class(TInterfacedObject, ITransaction)
  private
    FTransaction: TFDTransaction;
  public
    constructor Create(Connection: TFDConnection; ReadOnly: Boolean);
    function Query: TFDQueryEx;
  end;

  TFDConnectionEx = class(TFDConnection)
  public
    function StartTransaction(ReadOnly: Boolean = False): ITransaction;
  end;


implementation

{ TSQLFieldName }

constructor TSQLFieldName.Create(const AFieldName: string);
begin
// empty
end;

class function TSQLFieldName.GetName(Field: PRecordTypeField): string;
var
  Attr: PAttrEntry;
begin
  Result := '"' + UpperCase(string(Field.Name)) + '"';
  Attr := Field.AttrData.GetAttribute(TSQLFieldName);
  if Attr <> nil then
    Result := Attr.StringAt(0) + ' AS ' + Result;
end;

{ TSQLFormat }

class procedure TSQLFormat.Process(Instance: Pointer; Field: PRecordTypeField);
var
  A: PAttrData;
  I: PAttrEntry;
  E: PByte;
begin
  A := Field.AttrData;
  I := @PByte(A)[2];
  E := @PByte(A)[A.Len];
  while PByte(I) < E do
  begin
    if I.AttrType^.TypeData.ClassType.InheritsFrom(TSQLFormat) then
    begin
      TSQLFormatClass(I.AttrType^.TypeData.ClassType).Format(Instance, Field.Field.TypeRef^);
    end;
    Inc(PByte(I), 2 * SizeOf(Pointer) + 2 + I.ArgLen);
  end;
end;

{ TStringsHelper }

procedure TStringsHelper.SelectType(TypeInfo: PTypeInfo; const From: string);
var
  Fields: string;
  Count : Integer;
  Index : Integer;
  Field : PRecordTypeField;
begin
  Assert(TypeInfo.Kind = tkRecord, 'TFDQuery.SelectType expected Record');

  Count := TypeInfo.RecordFieldCount;
  Fields := '';
  for Index := 0 to Count - 1 do
  begin
    Field := TypeInfo.RecordFieldType(Index);
    Fields := Fields + ',' + TSQLFieldName.GetName(Field);
  end;

  Fields[1] := ' ';
  Text := 'SELECT ' + Fields + ' ' + From;
end;

{ TFieldHelper }

function GetChar(Field: TField): Char;
var
  Str: string;
begin
  Str := Field.AsString;
  if Str = '' then
    Result := ' '
  else
    Result := Str[1];
end;

function TFieldHelper.AssignTo(Instance: Pointer; TypeInfo: PTypeInfo): Boolean;
begin
  if Self = nil then
    Exit(False);
  case TypeInfo.Kind of
    tkWChar  : Char(Instance^) := GetChar(Self);
    tkUString: string(Instance^) := AsString;
    tkInteger: TypeInfo.SetOrd(Instance, AsInteger);
    tkInt64  : Int64(Instance^) := AsLargeInt;
    tkFloat  :
    begin
      if TypeInfo = System.TypeInfo(TDate) then
        TDate(Instance^) := DateOf(AsDateTime)
      else
      if TypeInfo = System.TypeInfo(TTime) then
        TTime(Instance^) := TimeOf(AsDateTime)
      else
      if TypeInfo = System.TypeInfo(TDateTime) then
        TTime(Instance^) := AsDateTime
      else
        case TypeInfo.TypeData.FloatType of
          TFloatType.ftSingle   : Single(Instance^) := AsSingle;
          TFloatType.ftDouble   : Double(Instance^) := AsFloat;
          TFloatType.ftCurr     : Currency(Instance^) := AsCurrency
//          TFloatType.ftExtended : Extended(Instance^) := Numeric();
//          TFloatType.ftComp     : Comp(Instance^) := Numeric();
        end;
    end;
    tkDynArray:
      if TypeInfo = System.TypeInfo(TBytes) then
      begin
        TBytes(Instance^) := AsBytes;
      end else begin
        raise Exception.Create('Unsupported dynamic array');
      end;
  else
    raise Exception.Create('Unsupported target type ' + string(TypeInfo.Name));
  end;
  Result := True;
end;

{ TDataSetHelper }

function TDataSetHelper.Fetch: Boolean;
begin
  if Active = False then
    Open
  else
    Next;
  Result := not Eof;
end;

function TDataSetHelper.Fetch<T>(var V: T): Boolean;
begin
  Result := Fetch;
  if Result then
    ReadType(@V, TypeInfo(T));
end;

procedure TDataSetHelper.ReadType(Instance: Pointer; TypeInfo: PTypeInfo);
var
  Count: Integer;
  Field: PRecordTypeField;
  Index: Integer;
  Data : Pointer;
begin
  Assert(TypeInfo.Kind = tkRecord);
  Field := TypeInfo.RecordFieldsPtr(Count);
  for Index := 0 to Count - 1 do
  begin
    Data := Field.GetInstance(Instance);
    if FindField(UpperCase(string(Field.Name))).AssignTo(Data, Field.Field.TypeRef^) then
      TSQLFormat.Process(Data, Field);
    Field := Field.Next;
  end;
end;

procedure TDataSetHelper.Read<T>(var V: T);
begin
  ReadType(@V, TypeInfo(T));
end;

procedure TDataSetHelper.ReadAllType(var Instance: Pointer;
  TypeInfo: PTypeInfo; PageSize: Integer = 100);
var
  Count: Integer;
  Len  : Integer;
  Item : PByte;
begin
  Count := 0;
  Len := DynArraySize(Instance);
  if PageSize <= 0 then
    PageSize := 100;
  while Fetch do
  begin
    if Count = Len then
    begin
      Inc(Len, PageSize);
      DynArraySetLength(Instance, TypeInfo, 1, @Len);
    end;
    Item := Instance;
    Inc(Item, Count * TypeInfo.DynArrayElSize);
    ReadType(Item, TypeInfo.DynArrayElType);
    Inc(Count);
  end;
  DynArraySetLength(Instance, TypeInfo, 1, @Count);
end;

type
  TBufferHeader = record
    BookmarkFlag: TBookmarkFlag;
    Position    : Integer;
  end;

  TBufferInfo = record
    Header: TBufferHeader;
    Body  : Byte;
  end;
  PBufferInfo = ^TBufferInfo;

function GetFieldFlags(D: PAttrData): TFieldFlags;
var
  A: PAttrEntry;
begin
  Result := [];
  if D <> nil then
  begin
    A := D.GetAttribute(FieldFlags);
    if A <> nil then
      Byte(Result) := A.ArgData[1];
  end;
end;

function GetMaxLength(D: PAttrData): Integer;
var
  A: PAttrEntry;
begin
  Result := 10;
  if D <> nil then
  begin
    A := D.GetAttribute(MaxLength);
    if A <> nil then
      Result := A.IntegerAt(0);
  end;
end;

function GetDisplayLabel(D: PAttrData): string;
var
  A: PAttrEntry;
begin
  Result := '';
  if D <> nil then
  begin
    A := D.GetAttribute(DisplayLabel);
    if A <> nil then
      Result := A.StringAt(0);
  end;
end;

{ FieldFlags }

constructor FieldFlags.Create(Flags: TFieldFlags);
begin
//
end;

{ MaxLength }

constructor MaxLength.Create(MaxLength: Integer);
begin
//
end;

{ DisplayLabel }

constructor DisplayLabel.Create(const DisplayLabel: string);
begin
//
end;

{ TRTTIDataSet }

{$IFDEF DEBUG}
constructor TRTTIDataSet.Create(AOwner: TComponent);
begin
  inherited;
  FBufferList := TList.Create;
end;

destructor TRTTIDataset.Destroy;
begin
  inherited;
  FBufferList.Free;
end;
{$ENDIF}

procedure TRTTIDataSet.SetBookmarkData(Buffer: TRecordBuffer; Data: TBookmark);
var
  BufferInfo: PBufferInfo absolute Buffer;
begin
{$IFDEF LOG}WriteLn('SetBookmarkData @', IntToHex(Cardinal(Buffer), 4), ':', FBufferList.IndexOf(Pointer(Buffer)), ', ', PInteger(Data)^);{$ENDIF}
  BufferInfo.Header.Position := PInteger(Data)^;
end;

procedure TRTTIDataSet.GetBookmarkData(Buffer: TRecordBuffer; Data: TBookmark);
var
  BufferInfo: PBufferInfo absolute Buffer;
begin
{$IFDEF LOG}WriteLn('GetBookmarkData @', IntToHex(Cardinal(Buffer), 4), ':', FBufferList.IndexOf(Pointer(Buffer)), ') = ', BufferInfo.Header.Position);{$ENDIF}
  PInteger(Data)^ := BufferInfo.Header.Position;
end;

procedure TRTTIDataSet.SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);
var
  BufferInfo: PBufferInfo absolute Buffer;
begin
{$IFDEF LOG}WriteLn('SetBookmarkFlag @', IntToHex(Cardinal(Buffer), 4), ':', FBufferList.IndexOf(Pointer(Buffer)), ', ', GetEnumName(TypeInfo(TBookmarkFlag), Ord(Value)), ')');{$ENDIF}
  BufferInfo.Header.BookmarkFlag := Value;
end;

function TRTTIDataSet.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
var
  BufferInfo: PBufferInfo absolute Buffer;
begin
{$IFDEF LOG}WriteLn('GetBookmarkFlag @', IntToHex(Cardinal(Buffer), 4), ':', FBufferList.IndexOf(Pointer(Buffer)), ', ', GetEnumName(TypeInfo(TBookmarkFlag), Ord(BufferInfo.Header.BookmarkFlag)), ')');{$ENDIF}
  Result := BufferInfo.Header.BookmarkFlag;
end;

procedure TRTTIDataSet.InternalInitRecord(Buffer: TRecordBuffer);
var
  BufferInfo: PBufferInfo absolute Buffer;
  Field: PRecordTypeField;
  Count: Integer;
  Index: Integer;
  nCount: Integer;
  Yet   : TDateTime;
  PDate : PDateTime;
begin
{$IFDEF LOG}WriteLn('InternalInitRecord(@', IntToHex(Cardinal(Buffer), 4), ':', FBufferList.IndexOf(Pointer(Buffer)));{$ENDIF}
  FinalizeArray(@BufferInfo.Body, FRecordType, 1);
  FillChar(BufferInfo.Body, FRecordSize, 0);
  AutoInc(@BufferInfo.Body);
  if FNowCount > 0 then
  begin
    Field := FRecordType.RecordFieldsPtr(Count);
    nCount := FNowCount;
    Yet := Now;
    for Index := 0 to Count - 1 do
    begin
      if ffNow in FFieldFlags[Index] then
      begin
        PDate := Field.GetInstance(@BufferInfo.Body);
        case Field.Field.TypeRef^.ExtendedType of
          etDate     : PDate^ := DateOf(Yet);
          etTime     : PDate^ := TimeOf(Yet);
          etDateTime : PDate^ := Yet;
        end;
        Dec(nCount);
        if nCount = 0 then
          Break;
      end;
      Field := Field.Next;
    end;
  end;

end;

procedure TRTTIDataSet.SetFieldData(Field: TField; Buffer: TValueBuffer);
var
  Instance: Pointer;
  RField  : PRecordTypeField;
  eType   : TExtendedType;
  I       : Integer;
  S       : string;
  D       : TDateTime;
  C       : Currency;
begin
{$IFDEF LOG}WriteLn('SetFieldData(', Field.Index,', @',IntToHex(Cardinal(Buffer),4),')');{$ENDIF}
  Instance := Pointer(ActiveBuffer);
  RField := FRecordType.RecordFieldType(Field.Index);
  Instance := @PBufferInfo(Instance).Body;
  Instance := RField.GetInstance(Instance);
  eType := RField.Field.TypeRef^.ExtendedType;
  case eType of
    etInteger  :
    begin
      I := PInteger(Buffer)^;
      if PInteger(Instance)^ <> I then
      begin
        PInteger(Instance)^ := I;
        SetModified(True);
      end;
    end;
    etString   :
    begin
      S := PChar(Buffer);
      if PString(Instance)^ <> S then
      begin
        PString(Instance)^ := S;
        SetModified(True);
      end;
    end;
    etDate,
    etTime,
    etDateTime:
    begin
      case eType of
        etDate    : D := PInteger(Buffer)^ - DateDelta;
        etTime    : D := Double(PInteger(Buffer)^) / Double(MSecsPerDay);
      else
       {etDateTime} D := (PDouble(Buffer)^ / MSecsPerDay) - DateDelta;
      end;
      if not SameDateTime(D, PDateTime(Instance)^) then
      begin
        PDateTime(Instance)^ := D;
        SetModified(True);
      end;
    end;
    etCurrency :
    begin
      C := PDouble(Buffer)^;
      if PCurrency(Instance)^ <> C then
      begin
        PCurrency(Instance)^ := C;
        SetModified(True);
      end;
    end
  else
    RField.Field.TypeRef^.SetValue(Instance, PChar(Buffer));
    SetModified(True);
  end;
end;

procedure TRTTIDataSet.SetData<T>(var V: TArray<T>);
var
  P: Pointer;
begin
  FArrayType := TypeInfo(TArray<T>);
  FRecordSize := FArrayType.DynArrayElSize;
  FRecordType := TypeInfo(T);
  FInstance := @V;
  TArray<T>(P) := V;
  InternalInit;
end;

procedure TRTTIDataSet.SetRecNo(Value: Integer);
begin
  if Value <> FRecNo + 1 then
  begin
    CheckBrowseMode;
    DoBeforeScroll;
    FRecNo := Value - 1;
    Resync([rmCenter]);
    DoAfterScroll;
  end;
end;

procedure TRTTIDataSet.InternalInit;
var
  Field: PRecordTypeField;
  Count: Integer;
  Index: Integer;
begin
  FRecordCount := 0;
  FRecNo := -1;
  if (FRecordType <> nil) and (FInstance <> nil) then
  begin
    FRecordCount := DynArraySize(FInstance^);
    Field := FRecordType.RecordFieldsPtr(Count);
    SetLength(FFieldFlags, Count);
    FAutoIncs := 0;
    FNowCount := 0;
    for Index := 0 to Count - 1 do
    begin
      FFieldFlags[Index] := GetFieldFlags(Field.AttrData);
      if ffAutoInc in FFieldFlags[Index] then
        Inc(FAutoIncs);
      if ffNow in FFieldFlags[Index] then
        Inc(FNowCount);
      Field := Field.Next;
    end;
  end;
end;

procedure TRTTIDataSet.AutoInc(Buffer: Pointer);
var
  Field : PRecordTypeField;
  aCount: Integer;
  Count : Integer;
  iField: Integer;
  Index : Integer;
  I1, I2: Integer;
  P: PByte;
begin
  if FAutoIncs = 0 then
    Exit;
  Field := FRecordType.RecordFieldsPtr(Count);
  aCount := FAutoIncs;
  for iField := 0 to Count - 1 do
  begin
    if ffAutoInc in FFieldFlags[iField] then
    begin
      case Field.Field.TypeRef^.ExtendedType of
        etInteger:
        begin
          I1 := 0;
          P := FInstance^;
          for Index := 0 to FRecordCount - 1 do
          begin
            I2 := PInteger(Field.GetInstance(P))^;
            if I2 > I1 then
              I1 := I2;
            Inc(P, FRecordSize);
          end;
          PInteger(Field.GetInstance(Buffer))^ := I1 + 1;
        end;
      end;
      Dec(aCount);
      if aCount = 0 then
        Exit;
    end;
    Field := Field.Next;
  end;
end;

function TRTTIDataSet.GetRecordSize: Word;
begin
  {$IFDEF LOG}WriteLn('GetRecordSize: ', FRecordSize);{$ENDIF}
  Result := FRecordSize;
end;

function TRTTIDataSet.IsCursorOpen: Boolean;
begin
  {$IFDEF LOG}WriteLn('IsCursorOpen');{$ENDIF}
  Result := FIsOpen and (FRecordType <> nil) and (FInstance <> nil);
end;

procedure TRTTIDataSet.InternalOpen;
var
  Count   : Integer;
  Index   : Integer;
  Field   : PRecordTypeField;
  Str     : string;
  Flags   : TFieldFlags;
begin
  {$IFDEF LOG}WriteLn('InternalOpen');{$ENDIF}
  InternalInit;
  InitFieldDefs;
  if not (lcPersistent in Fields.LifeCycles) then
  begin
    CreateFields;
    Field := FRecordType.RecordFieldsPtr(Count);
    for Index := 0 to Count - 1 do
    begin
      Flags := FFieldFlags[Index];
      if ffAutoInc in Flags then
        Fields[Index].AutoGenerateValue := arAutoInc;
      if ffReadOnly in Flags then
        Fields[Index].ReadOnly := True;
      Str := GetDisplayLabel(Field.AttrData);
      if Str <> '' then
        Fields[Index].DisplayLabel := Str;
      Field := Field.Next;
    end;
  end;
  BindFields(True);
  FIsOpen := True;
  FRecNo := -1;
  BookmarkSize := SizeOf(Integer);
end;

procedure TRTTIDataSet.InternalPost;
var
  Buffer: PBufferInfo;
  Target: Pointer;
begin
  Buffer := Pointer(ActiveBuffer);
  if State = dsEdit then
  begin
    Target := FInstance^;
    Inc(PByte(Target), FRecordSize * Buffer.Header.Position);
    FRecordType.Copy(@Buffer.Body, Target);
  end else begin
  // dsInsert
    AutoInc(@Buffer.Body);
  // Insert
    Inc(FRecordCount);
    DynArraySetLength(FInstance^, FArrayType, 1, @FRecordCount);
    Target := FInstance^;
    Inc(PByte(Target), FRecordSize * (FRecordCount - 1));
    FRecordType.Copy(@Buffer.Body, Target);
  end;
end;

function TRTTIDataSet.GetIsIndexField(Field: TField): Boolean;
begin
  Result := ffUnique in FFieldFlags[Field.Index];
end;

procedure TRTTIDataSet.InternalClose;
begin
  {$IFDEF LOG}WriteLn('InternalClose');{$ENDIF}
  BindFields(False);
  if not (lcPersistent in Fields.LifeCycles) then
    DestroyFields;
  FRecNo := -1;
  FIsOpen := False;
end;

procedure TRTTIDataSet.InternalDelete;
var
  Buffer: PBufferInfo;
  Source: PByte;
  Target: PByte;
begin
  Buffer := PBufferInfo(ActiveBuffer);
  Target := FInstance^;
  Inc(Target, FRecordSize * Buffer.Header.Position);
  FinalizeArray(Target, FRecordType, 1);
  Source := @Target[FRecordSize];
  Move(Source^, Target^, FRecordSize * (FRecordCount - 1 - Buffer.Header.Position));
  Dec(FRecordCount);
  Target := FInstance^;
  Inc(Target, FRecordCount * FRecordSize);
  FillChar(Target^,  FRecordSize, 0);
  DynArraySetLength(FInstance^, FArrayType, 1, @FRecordCount);
end;

procedure TRTTIDataSet.InternalSetToRecord(Buffer: TRecBuf);
var
  BufferInfo: PBufferInfo absolute Buffer;
begin
  FRecNo := BufferInfo.Header.Position;
end;

procedure TRTTIDataSet.InternalFirst;
begin
  FRecNo := -1;
end;

procedure TRTTIDataSet.InternalGotobookmark(Bookmark: TBookmark);
begin
  FRecNo := PInteger(Bookmark)^;
end;

procedure TRTTIDataSet.InternalHandleException;
begin
  {$IFDEF LOG}WriteLn('InternalHandleException');{$ENDIF}
end;

procedure TRTTIDataSet.InternalInitFieldDefs;
var
  Count   : Integer;
  Index   : Integer;
  Field   : PRecordTypeField;
  DataType: TFieldType;
  Size    : Integer;
begin
  {$IFDEF LOG}WriteLn('InternalInitFieldDefs');{$ENDIF}
  FieldDefs.Clear;
  Field := FRecordType.RecordFieldsPtr(Count);
  for Index := 0 to Count - 1 do
  begin
    Size := 0;
    case Field.Field.TypeRef^.ExtendedType of
      etInteger: DataType := TFieldType.ftInteger;
      etString:
      begin
        DataType := TFieldType.ftWideString;
        Size := GetMaxLength(Field.AttrData);
      end;
      etDateTime: DataType := TFieldType.ftDateTime;
      etDate    : DataType := TFieldType.ftDate;
      etTime    : DataType := TFieldType.ftTime;
      etCurrency: DataType := TFieldType.ftCurrency;
    else
      DataType := TFieldType.ftWideString;
      Size := 10;
    end;
    FieldDefs.Add(string(Field.Name), DataType, Size);
    Field := Field.Next;
  end;
end;

procedure TRTTIDataSet.InternalLast;
begin
  FRecNo := FRecordCount;
end;

function TRTTIDataSet.AllocRecordBuffer: TRecordBuffer;
var
  BufferInfo: PBufferInfo absolute Result;
begin
  {$IFDEF LOG}WriteLn('AllocRecordBuffer');{$ENDIF}
  GetMem(Result, SizeOf(TBufferHeader) + FRecordSize);
  InitializeArray(@BufferInfo.Body, FRecordType, 1);
  {$IFDEF LOG}
    FBufferList.Add(Result);
    WriteLn(' -> @', IntToHex(Cardinal(Result), 4),':', FBufferList.IndexOf(Result));
  {$ENDIF}
end;

procedure TRTTIDataSet.FreeRecordBuffer(var Buffer: TRecordBuffer);
var
  BufferInfo: PBufferInfo absolute Buffer;
begin
  {$IFDEF LOG}WriteLn('FreeRecordBuffer');{$ENDIF}
  InitializeArray(@BufferInfo.Body, FRecordType, 1);
  FreeMem(Buffer);
end;

function TRTTIDataSet.GetRecNo: Integer;
begin
  UpdateCursorPos;
  Result := FRecNo + 1;
end;

function TRTTIDataSet.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
  Source: Pointer;
  Target: PBufferInfo absolute Buffer;
begin
  {$IFDEF LOG}WriteLn('GetRecord(@', IntToHex(Cardinal(Buffer),4),':', FBufferList.IndexOf(Buffer), ',' , FRecNo ,',',GetEnumName(TypeInfo(TGetMode), Ord(GetMode)),')');{$ENDIF}
  case GetMode of
    gmCurrent:
      if (FRecNo < 0) or (FRecNo >= FRecordCount) then
        Result := TGetResult.grError
      else begin
        Result := TGetResult.grOK;
      end;
    gmNext:
      if FRecNo = FRecordCount then
        Result := TGetResult.grError
      else begin
        Inc(FRecNo);
        if FRecNo >= FRecordCount then
          Result := TGetResult.grEOF
        else begin
          Result := TGetResult.grOK;
        end;
      end;
    gmPrior:
      if FRecNo = -1 then
        Result := TGetResult.grError
      else begin
        Dec(FRecNo);
        if FRecNo < 0 then
          Result := TGetResult.grBOF
        else begin
          Result := TGetResult.grOK;
        end;
      end;
  else
    Result := TGetResult.grError;
  end;
  if Result = TGetResult.grOK then
  begin
    Source := FInstance^;
    Inc(PByte(Source), FRecordSize * FRecNo);
    Target.Header.BookmarkFlag := TBookmarkFlag.bfCurrent;
    Target.Header.Position := FRecNo;
    FRecordType.Copy(Source, @Target.Body);
  {$IFDEF LOG}
    WriteLn(' -> ', FRecNo ,', @', IntToHex(Cardinal(Buffer), 4),':',FBufferList.IndexOf(Buffer),', ID: ',PInteger(Source)^, ', Name: ', PString(@PByte(Source)[4])^);
  end else begin
    WriteLn(' -> ', GetEnumName(TypeInfo(TGetResult), Ord(Result)));
  {$ENDIF}
  end;
end;

function TRTTIDataSet.GetRecordCount: Integer;
begin
  Result := FRecordCount;
end;

function TRTTIDataSet.GetFieldData(Field: TField;
  var Buffer: TValueBuffer): Boolean;
var
  RField  : PRecordTypeField;
  Instance: Pointer;
  Data    : Pointer;

  procedure SetString(const Str: string);
  var
    Size    : Integer;
  begin
    FillChar(Buffer[0], Length(Buffer), 0);
    Size := Length(Str);
    if Size = 0 then
      Exit;
    Size := 2 * Size + 2;
    if Size > Length(Buffer) then
      Size := Length(Buffer);
    Move(Str[1], Buffer[0], Size);
  end;

begin
{$IFDEF LOG}WriteLn('GetFieldData(', Field.Index,', @',IntToHex(Cardinal(Buffer),4),')');{$ENDIF}
  CheckActive;
  if (FRecordCount = 0) and (State <> dsInsert) then
    Exit(False);
  Instance := Pointer(ActiveBuffer);
  if Instance = nil then
    Exit(False);
{$IFDEF LOG}WriteLn(' -> ',GetEnumName(TypeInfo(TDataSetState), Ord(State)),',@', IntToHex(Cardinal(Instance), 4), ':', FBufferList.IndexOf(Instance));{$ENDIF}
  RField := FRecordType.RecordFieldType(Field.Index);
  Instance := @PBufferInfo(Instance).Body;
  Data := RField.GetInstance(Instance);
  case RField.Field.TypeRef^.ExtendedType of
    etInteger  : PInteger(Buffer)^ := PInteger(Data)^;
    etString   : SetString(PString(Data)^);
    etDate     : PInteger(Buffer)^ := DateDelta + Trunc(PDateTime(Data)^);
    etTime     : PInteger(Buffer)^ := Round(PDateTime(Data)^ * MSecsPerDay) mod MSecsPerDay;
    etDateTime : PDouble(Buffer)^ := (DateDelta + PDateTime(Data)^) * MSecsPerDay;
    etCurrency : PDouble(Buffer)^ := PCurrency(Data)^;
  else
    SetString(RField.Field.TypeRef^.GetValue(Data));
  end;
  Result := True;
end;

{ TFDQueryEx }

function TFDQueryEx.FetchAll<T>(const From: string; PageSize: Integer = 100): TArray<T>;
begin
  FetchAllType(Pointer(Result), TypeInfo(TArray<T>), From, PageSize);
end;

procedure TFDQueryEx.FetchAllType(var Instance: Pointer;
  TypeInfo: PTypeInfo; const From: string; PageSize: Integer = 100);
begin
  SQL.SelectType(TypeInfo.DynArrayElType, From);
  ReadAllType(Instance, TypeInfo, PageSize);
end;

function TFDQueryEx.FetchDateTime(const aSQL: string;
  const ADefault: TDateTime): TDateTime;
begin
  SQL.Text := aSQL;
  if Fetch then
    Result := Fields[0].AsDateTime
  else
    Result := ADefault;
end;

function TFDQueryEx.FetchInteger(const aSQL: string; ADefault: Integer): Integer;
begin
  SQL.Text := aSQL;
  if Fetch then
    Result := Fields[0].AsInteger
  else
    Result := ADefault;
end;

function TFDQueryEx.FetchString(const aSQL, ADefault: string): string;
begin
  SQL.Text := aSQL;
  if Fetch then
    Result := Fields[0].AsString
  else
    Result := ADefault;
end;

function TFDQueryEx.Lookup<T>(var V: T; const From: string): Boolean;
begin
  Select<T>(From);
  Result := Fetch;
  if Result then
    ReadType(@V, TypeInfo(T));
end;

function TFDQueryEx.ReadAll<T>(PageSize: Integer): TArray<T>;
begin
  ReadAllType(Pointer(Result), TypeInfo(TArray<T>), PageSize);
end;

procedure TFDQueryEx.Select<T>(const From: string);
begin
  SQL.SelectType(TypeInfo(T), From);
end;

{ TTransactionHolder }

constructor TTransactionHolder.Create(Connection: TFDConnection; ReadOnly: Boolean);
begin
  inherited Create; // TInterfacedObject
  FTransaction := TFDTransaction.Create(Connection);
  FTransaction.Connection := Connection;
  FTransaction.Options.ReadOnly := ReadOnly;
end;

function TTransactionHolder.Query: TFDQueryEx;
begin
  Result := TFDQueryEx.Create(FTransaction);
  Result.Connection := FTransaction.Connection;
  Result.Transaction := FTransaction;
end;

{ TFDConnectionEx }

function TFDConnectionEx.StartTransaction(ReadOnly: Boolean = False): ITransaction;
begin
  Result := TTransactionHolder.Create(Self, ReadOnly);
end;

initialization
{$IFDEF LOG}AllocConsole{$ENDIF}
end.
