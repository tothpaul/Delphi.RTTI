unit Execute.XML.RTTI;
{
   Delphi XML RTTI unit

   (c)2024 Execute SARL  <contact@execute.fr>

   http://www.execute.fr

}
interface

uses
  System.TypInfo,
  System.SysUtils,
  System.Classes,
  System.DateUtils,
  Execute.RTTI,
  Execute.UTF8.Utils,
  Execute.XML.Tree;

const
  XML_BOOL: array[False..True] of UTF8String =('false', 'true');

type
  TBase64Bytes = {type} TBytes;
{
  <node id="1">value</node>

  TNode = record
    id: Integer;
    Value: TXMLNodeText;
  end;

}
// deprecated
  TXMLNodeText  = type string ; // => cf XMLNodeValue

{
  <node id="1">
    <bool>true</bool>
    <int>1</int>
    <text>text</text>
    <curr>1.5</curr>
    <date>2020-10-31T12:48:00Z</date>
  </node>

  node = record <=== deprecated !!!
    id  : Integer;
    bool: TXMLBoolValue;
    int : TXMLIntValue;
    text: TXMLTextValue;
    curr: TXMLCurrValue;
    date: TXMLDateValue;
  end;

  node = record
  [XMLAttributes] // optional, because it is the default
    id  : Integer;
  [XMLNodes]      // from this member until a XMLAttributes attribute
    bool: Boolean;
    int : Integer;
    text: string;
    curr: Currency;
    date: TDateTime;
  end;

}
//  TXMLTextValue = type string;
//  TXMLUtf8Value = type UTF8String;
//  TXMLBoolValue = type Boolean;
//  TXMLIntValue  = type Integer;
//  TXMLInt64Value  = type Int64;
//  TXMLCurrValue = type Currency;
//  TXMLDateValue = type TDateTime;
//  TXMLSingleValue = type Single;

  XMLAttributesAttribute = class(TCustomAttribute)
  end;

  XMLNodesAttribute = class(TCustomAttribute)
  end;

{
  <node id="123">content</node>

  TNode = record
  [XMLAttributes] // optional, because it is the default
    id: Integer;    // 123
   [XMLNodeValue]
    value: string;  // content
  end;
}
  XMLNodeValueAttribute = class(TCustomAttribute)
  end;

{
  <node>
    <id value="xxx"/>
  </node>

  node = record
     [XMLAttribute('value')]
     id: string;
  end;
}
  XMLAttributeAttribute = class(TCustomAttribute)
    constructor Create(const Attribute: UTF8String);
  end;

{
   <tns:node xmlns:tns="http://exemple.com" tns:type="typ"/>

   [TXMLNamespace('http://exemple.com')]
   TNode = record
     [TXMLName('type')]
     type_: string;
   end;
}

  XMLNameAttribute = class(TCustomAttribute)
    constructor Create(const ANamespace: UTF8String);
    class function Get(Info: PTypeInfo; const Default: UTF8String): UTF8String;
    class function GetField(Field: PRecordTypeField): UTF8String;
  end;

  XMLNamespaceAttribute = class(TCustomAttribute)
    constructor Create(const ANamespace: UTF8String);
    class function GetType(Info: PTypeInfo; const Default: UTF8String): UTF8String;
    class function GetField(Field: PRecordTypeField; const Default: UTF8String): UTF8String;
  end;

{
   <node xmlns:tns="...">
     <tns:one>1</tns:one>
     <tns:two>2</tns:two>
     <tns:three>3</tns:three>
   </node>

   [TXMLNamespaceGroup('...')]
   tns_node = record
   [XMLNodes]
     one  : Integer; // TXMLIntValue;
     two  : Integer; // TXMLIntValue;
     three: Integer; // TXMLIntValue;
   end;

   node = record
     tns: tns_node;
   end;

}
  XMLNamespaceGroupAttribute = class(TCustomAttribute)
    constructor Create(const ANamespace: UTF8String);
    class function Get(Info: PTypeInfo): UTF8String;
  end;

{
   <parent id="123">
     <field1>field1</field1>
     <field2>field2</field2>
   <parent>

   TFields = record
     field1: TXMLTextValue;
     field2: TXMLTextValue;
   end;

   [TXMLInline('items')]
   parent = record
     id: Integer;
     items:TArray<TFields>;
   end;

}
  XMLInlineAttribute = class(TCustomAttribute)
    constructor Create(const AFieldName: UTF8String);
    class function Inlined(Info: PTypeInfo): PRecordTypeField;
  end;

{ Convert a XML source into a Delphi structure variable }

  TXmlRtti = class
  private
    class procedure ParseType(Info: PTypeInfo; Instance: Pointer; const XML: UTF8String; const Root: string = '');
    class procedure ParseTree(Info: PTypeInfo; Instance: Pointer; const XML: TXMLTree; const Root: string = '');
    class procedure ParseNode(Info: PTypeInfo; Instance: Pointer; Node: PXMLNode; NameSpace: UTF8String = '');
    class function BuildType(Info: PTypeInfo; Instance: Pointer; const Name: UTF8String; Header: Boolean): UTF8String;
  public
    class procedure fromXML<T>(var V: T; const XML: UTF8String; const Root: string); overload; inline;
    class procedure fromXML<T>(var V: T; const XML: TXMLTree; const Root: string); overload; inline;
    class procedure fromXML<T>(var V: T; Node: PXMLNode; const NameSpace: UTF8String = ''); overload; inline;
    class function LoadFromFile(const AFileName: string): UTF8String;
    class function toXML<T>(const V: T; const Name: UTF8String; Header: Boolean = True): UTF8String; inline;
  end;

  TXMLParser = class
    class procedure XmlParse<T>(var V: T; const XML: UTF8String; const Root: string = ''); inline;
    class procedure XmlParseType(Info: PTypeInfo; Instance: Pointer; const XML: UTF8String; const Root: string = '');
    class procedure ParseNode<T>(Node: PXMLNode; var V: T; const Root: string = ''); inline;
    class procedure ParseNodeType(Info: PTypeInfo; Instance: Pointer; Node: PXMLNode; const Root: string = '');
  end deprecated;

function xmlDate(Date: TDateTime): UTF8String;

implementation

function xmlDate(Date: TDateTime): UTF8String;
begin
  Result := UTF8String(FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', Date))
end;

{ XMLAttributeAttribute }

constructor XMLAttributeAttribute.Create(const Attribute: UTF8String);
begin
// empty
end;

{ XMLNameAttribute }

constructor XMLNameAttribute.Create(const ANamespace: UTF8String);
begin
// empty
end;

class function XMLNameAttribute.Get(Info: PTypeInfo; const Default: UTF8String): UTF8String;
var
  Attr: PAttrEntry;
begin
  Attr := Info.GetAttribute(XMLNameAttribute);
  if Attr <> nil then
    Result := Attr.UTF8StringAt(0)
  else
    Result := Default;
end;

class function XMLNameAttribute.GetField(Field: PRecordTypeField): UTF8String;
var
  Attr: PAttrEntry;
begin
  Attr := Field.GetAttribute(XMLNameAttribute);
  if Attr <> nil then
    Result := Attr.UTF8StringAt(0)
  else
    Result := Field.Name;
end;

{ XMLNamespaceAttribute }

constructor XMLNamespaceAttribute.Create(const ANamespace: UTF8String);
begin
// empty
end;

class function XMLNamespaceAttribute.GetType(Info: PTypeInfo; const Default: UTF8String): UTF8String;
var
  Attr: PAttrEntry;
begin
  Attr := Info.GetAttribute(XMLNamespaceAttribute);
  if Attr <> nil then
    Result := Attr.UTF8StringAt(0)
  else
    Result := Default;
end;

class function XMLNamespaceAttribute.GetField(Field: PRecordTypeField; const Default: UTF8String): UTF8String;
var
  Attr: PAttrEntry;
begin
  Attr := Field.GetAttribute(XMLNamespaceAttribute);
  if Attr <> nil then
    Result := Attr.UTF8StringAt(0)
  else
    Result := Default;
end;

{ XMLNamespaceGroupAttribute }

constructor XMLNamespaceGroupAttribute.Create(const ANamespace: UTF8String);
begin
// empty
end;

class function XMLNamespaceGroupAttribute.Get(Info: PTypeInfo): UTF8String;
var
  Attr: PAttrEntry;
begin
  Attr := Info.GetAttribute(XMLNamespaceGroupAttribute);
  if Attr <> nil then
    Result := Attr.UTF8StringAt(0)
  else
    Result := '';
end;

{ XMLInlineAttribute }

constructor XMLInlineAttribute.Create(const AFieldName: UTF8String);
begin
  // empty
end;

class function XMLInlineAttribute.Inlined(Info: PTypeInfo): PRecordTypeField;
var
  Attr: PAttrEntry;
begin
  Attr := Info.GetAttribute(XMLInlineAttribute);
  if Attr <> nil then
    Result := Info.RecordFieldByName(Attr.StringAt(0))
  else
    Result := nil;
end;

{ TParser }

type
  TParser = record
    procedure Parse(Info: PTypeInfo; Instance: Pointer; const Tree: TXMLTree; Root: UTF8String);
    procedure ParseNode(Node: PXMLNode; Namespace: UTF8String; Info: PTypeInfo; Instance: Pointer; IsNode: Boolean);
    procedure ParseBytes(Node: PXMLNode; var Bytes: TBytes);
    function ParseFields(Fields: PRecordTypeField; FieldCount: Integer; const Name, Namespace, CurrentNamespace: UTF8String; Node: PXMLNode; Instance: Pointer): Boolean;
  end;

procedure TParser.Parse(Info: PTypeInfo; Instance: Pointer; const Tree: TXMLTree; Root: UTF8String);
begin
  if Root = '' then
    Root := Info.Name;
  if Tree.Root.Name.Match(UTF8String(Root)) then
  begin
    ParseNode(@Tree.Root, Tree.Root.Namespace, Info, Instance, False);
  end;
end;

procedure TParser.ParseNode(Node: PXMLNode; Namespace: UTF8String; Info: PTypeInfo; Instance: Pointer; IsNode: Boolean);
var
  Index: Integer;
  Name: UTF8String;
  NS: UTF8String;
  LName: UTF8String;
  LNS: UTF8String;
  Fields: PRecordTypeField;
  FieldCount: Integer;
  Field: PRecordTypeField;
  iField: Integer;
  FType: PTypeInfo;
begin
  if Node = nil then
    Exit;


  // <node>Value</node>
  if IsNode then
  begin
    case Info.ExtendedType of
      etString     : string(Instance^) := string(Node.Text.Value);
      etUTF8String : UTF8String(Instance^) := Node.Text.Value;
      etBoolean    : Boolean(Instance^) := Node.Text.Match('true');
      etInteger    : Integer(Instance^) := StrToIntDef(string(Node.Text.AnsiValue), 0);
      etInt64      : Int64(Instance^) := StrToInt64Def(string(Node.Text.AnsiValue), 0);
      etCurrency   : Currency(Instance^) := UTF8ToFloatDef(UTF8String(Node.Text.AnsiValue), 0);
      etDate       : TDate(Instance^) := DateOf(Node.Text.AsDateTime);
      etTime       : TTime(Instance^) := TimeOf(Node.Text.AsDateTime);
      etDateTime   : TDateTime(Instance^) := Node.Text.AsDateTime;
      etSingle     : Single(Instance^) := UTF8ToFloatDef(UTF8String(Node.Text.AnsiValue), 0);
    else
      IsNode := False;
    end;
    if IsNode then
      Exit;
  end;


  if Info.Kind = tkDynArray then
  begin
    // <node>Base64Value</node>
    if Info = TypeInfo(TBase64Bytes) then
      ParseBytes(Node, TBytes(Instance^))
    else
      ParseNode(Node, Namespace, Info.DynArrayElType, Info.DynArrayAddItem(Instance), False);
    Exit;
  end;

  // RECORD
  if Info.Kind <> tkRecord then
    Exit;

  Fields := Info.RecordFieldsPtr(FieldCount);
  if FieldCount = 0 then
    Exit;
  NS := Node.GetAttributeNS('xmlns', '');
  if NS <> '' then
    Namespace := NS;

  // ATTRIBUT => Record.Field
  for Index := 0 to Length(Node.Attrs) - 1 do
  begin

    Name := Node.Attrs[Index].Name.Value;
    NS := Node.Attrs[Index].Prefix.Value;
    if NS = '' then
    begin
      if Name = 'xmlns' then
        Continue;
      NS := Namespace;
    end else begin
      if NS = 'xmlns' then
        Continue;
      NS := Node.ResolveNS(NS);
    end;

    Field := Fields;
    for iField := 0 to FieldCount - 1 do
    begin
      FType := Field.Field.TypeRef^;
      LName := XMLNameAttribute.Get(FType, Field.Name);
      LNS := XMLNamespaceAttribute.GetField(Field, Namespace);

      if (LName = Name) and (LNS = NS) then
      begin
        FType.SetValue(Field.GetInstance(Instance), string(Node.Attrs[Index].Value.Value));
        Break;
      end;

      Field := Field.Next;
    end;
  end;

  //
  Field := XMLInlineAttribute.Inlined(Info);
  if Field <> nil then
  begin
    ParseNode(Node, Namespace, Field.Field.TypeRef^, Field.GetInstance(Instance), False);
    Exit;
  end;

  // CHILDS => Record.Field
  for Index := 0 to Length(Node.Children) - 1 do
  begin

    Name := Node.Children[Index].Name.Value;
    NS := Node.Children[Index].Namespace;
    if NS = '' then
      NS := Namespace;

    ParseFields(Fields, FieldCount, Name, NS, Namespace, @Node.Children[Index], Instance);
  end;
end;

procedure TParser.ParseBytes(Node: PXMLNode; var Bytes: TBytes);
var
  Len: Integer;
begin
  Len := Base64Len(PByte(Node.Text.Start), Node.Text.Len);
  SetLength(Bytes, Len);
  Base64Decode(PByte(Node.Text.Start), PByte(Bytes), Len);
end;

function TParser.ParseFields(Fields: PRecordTypeField; FieldCount: Integer; const Name, Namespace, CurrentNamespace: UTF8String; Node: PXMLNode; Instance: Pointer): Boolean;
var
  Field: PRecordTypeField;
  iField: Integer;
  FType: PTypeInfo;
  LNS: UTF8String;
  LName: UTF8String;
  sFields: PRecordTypeField;
  sCount:Integer;
begin
  Field := Fields;
  var IsNode := False;
  for iField := 0 to FieldCount - 1 do
  begin
    FType := Field.Field.TypeRef^;

    if Field.HasAttribute(XMLAttributesAttribute) then
      IsNode := False
    else
    if Field.HasAttribute(XMLNodesAttribute) then
      IsNode := True;

    var att := Field.GetAttribute(XMLAttributeAttribute);
    if att <> nil then
    begin
      Ftype.SetValue(Instance, string(Node.GetAttribute(att.UTF8StringAt(0)).Value));
      Exit(True);
    end;

    if (FType = TypeInfo(TXMLNodeText)) and (Name = '') then
    begin
      string(Field.GetInstance(Instance)^) := string(Node.Text.Value);
      Exit(True);
    end;

    if Field.HasAttribute(XMLNodeValueAttribute) and (Name = '') then
    begin
      ParseNode(Node, Namespace, FType, Field.GetInstance(Instance), True);
      Exit(True);
    end;

    LNS := XMLNamespaceGroupAttribute.Get(FType);
    if (FType.Kind = tkRecord) and (LNS <> '') and (LNS = Namespace) then
    begin
      sFields := FType.RecordFieldsPtr(sCount);
      if ParseFields(sFields, sCount, Name, Namespace, Namespace, node, Field.GetInstance(Instance)) then
        Exit(True);
    end else
    begin
      LName := XMLNameAttribute.GetField(Field);
      LNS := XMLNamespaceAttribute.GetField(Field, CurrentNamespace);

      if (LName = Name) and (LNS = Namespace) then
      begin
        ParseNode(Node, Namespace, FType, Field.GetInstance(Instance), IsNode);
        Exit(True);
      end;
    end;

    Field := Field.Next;
  end;
  Result := False;
end;

{ TXMLParser }

class procedure TXMLParser.ParseNode<T>(Node: PXMLNode; var V: T;
  const Root: string);
begin
  Finalize(V);
  FillChar(V, SizeOf(T), 0);
  ParseNodeType(TypeInfo(T), @V, Node, Root);
end;

class procedure TXMLParser.ParseNodeType(Info: PTypeInfo; Instance: Pointer;
  Node: PXMLNode; const Root: string);
var
  Parser: TParser;
begin
  Parser.ParseNode(Node, Node.Namespace, Info, Instance, False);
end;

class procedure TXMLParser.XmlParse<T>(var V: T; const XML: UTF8String; const Root: string = '');
begin
  Finalize(V);
  FillChar(V, SizeOf(T), 0);
  XMLParseType(TypeInfo(T), @V, XML, Root);
end;

class procedure TXMLParser.XmlParseType(Info: PTypeInfo; Instance: Pointer; const XML: UTF8String; const Root: string = '');
var
  Tree: TXMLTree;
  Parser: TParser;
begin
  Tree.Build(XML);
  Parser.Parse(Info, Instance, Tree, UTF8String(Root));
end;

{ TBuilder }

type
  TXMLQualifiedName = record
    Name: UTF8String;
    URI : UTF8String;
    Prefix: UTF8String;
    function FullName: UTF8String;
  end;

  TXMLAttr = record
    Name: TXMLQualifiedName;
    Value: UTF8String;
    function ToString: UTF8string;
  end;

  PBuilder = ^TBuilder;
  TBuilder = record
    Parent  : PBuilder;
    Indent  : UTF8String;
    Tag: TXMLQualifiedName;
    Attrs: TArray<TXMLAttr>;
    NSCount: Integer;
    function GetPrefix(const URI: UTF8String): UTF8string;
    procedure AddAttribute(const AName, AURI, AValue: UTF8String);
    function ToString(const Childs: UTF8STring): UTF8String;
    function BuildFields(Info: PTypeInfo; Instance: Pointer; const Namespace: UTF8String): UTF8String;
    function BuildNode(Info: PTypeInfo; Instance: Pointer): UTF8String;
    function Build(AParent: PBuilder; Info: PTypeInfo; Instance: Pointer; const Name, Namespace: UTF8String; const AIndent: UTF8String = ''): UTF8String;
  end;

function TXMLQualifiedName.FullName: UTF8String;
begin
  if Prefix = '' then
    Result := Name
  else
    Result := Prefix + ':' + Name;
end;

function TXMLAttr.ToString: UTF8String;
begin
  Result := ' ' + Name.FullName + '="' + Value + '"';
end;

procedure TBuilder.AddAttribute(const AName, AURI, AValue: UTF8String);
var
  Index: Integer;
begin
  Index := Length(Attrs);
  SetLength(Attrs, Index + 1);
  Attrs[Index].Name.Name := AName;
  Attrs[Index].Name.URI := AURI;
  Attrs[Index].Name.Prefix := GetPrefix(AURI);
  Attrs[Index].Value := AValue;
end;

function TBuilder.ToString(const Childs: UTF8String): UTF8String;
var
  Index: Integer;
begin
  Result := #13#10 + Indent + '<' + Tag.FullName;

  for Index := 0 to Length(Attrs) - 1 do
  begin
    Result := Result + Attrs[Index].ToString;
  end;
  Attrs := nil;

  if Childs = '' then
    Result := Result + '/>'
  else begin
    Result := Result + '>' + Childs;
    if UTF8Pos(#13, Childs) > 0 then
      Result := Result + #13#10 + Indent;
    Result := Result + '</' + Tag.FullName + '>';
  end;

  Result := Result;
end;

function TBuilder.BuildFields(Info: PTypeInfo; Instance: Pointer; const Namespace: UTF8String): UTF8String;
var
  Field : PRecordTypeField;
  Count : Integer;
  Index : Integer;
  Child : TBuilder;
begin
  if Info.ExtendedType = etUTF8String then
    Result := UTF8String(string(Instance^))
  else
  begin
    Field := Info.RecordFieldsPtr(Count);
    Result := '';
    for Index := 0 to Count - 1 do
    begin
      Result := Result + Child.Build(@Self, Field.Field.TypeRef^, Field.GetInstance(Instance), Field.Name, Namespace, Indent + ' ');
      Field := Field.Next;
    end;
  end;
end;

function TBuilder.BuildNode(Info: PTypeInfo; Instance: Pointer): UTF8String;
var
  Count : Integer;
  Child : TBuilder;
  Field : PRecordTypeField;
  IsNode: Boolean;
  Index : Integer;
  LType : PTypeInfo;
//  NS    : string;
begin
  Result := '';
  IsNode := False;
  Field := Info.RecordFieldsPtr(Count);
  for Index := 0 to Count - 1 do
  begin
    LType := Field.Field.TypeRef^;

    if Field.HasAttribute(XMLAttributesAttribute) then
      IsNode := False
    else
    if Field.HasAttribute(XMLNodesAttribute) then
      IsNode := True;

    if (LType = TypeInfo(TXMLNodeText)) then
      Result := Result + UTF8String(string(Field.GetInstance(Instance)^))
    else
    if IsNode or (LType.Kind = tkDynArray) then
      Result := Result + Child.Build(@Self, LType, Field.GetInstance(Instance), XMLNameAttribute.GetField(Field), Tag.URI, Indent + ' ')
    else
    if LType.Kind = tkRecord then
    begin
      Result := Result + Child.Build(@Self, LType, Field.GetInstance(Instance), XMLNameAttribute.GetField(Field), Tag.URI, Indent + ' ');
    end else
    if Field.HasAttribute(XMLNodeValueAttribute) then
      Result := Result + UTF8String(LType.GetValue(Field.GetInstance(Instance)))
    else
      AddAttribute(UTF8String(Field.Name), XMLNamespaceAttribute.GetField(Field, Tag.URI), UTF8String(LType.GetValue(Field.GetInstance(Instance))));
    Field := Field.Next;
  end;
end;

function TBuilder.GetPrefix(const URI: UTF8String): UTF8String;
var
  Index: Integer;
  Default: UTF8String;
begin
  if (URI = '') or (URI = 'xmlns') then
    Exit(URI);
  Default := '';
  if (Tag.URI = URI) and (Tag.Prefix <> '') then
    Exit(Tag.Prefix);
  if Parent = nil then
  begin
    if (Length(Attrs) = 0) and (NSCount = 0) then
    begin
      AddAttribute('xmlns', '', URI);
      Exit('');
    end;
    for Index := 0 to Length(Attrs) - 1 do
    begin
      if (Attrs[Index].Name.URI = '')
      and(Attrs[Index].Name.Name = 'xmlns')
      and(Default = '') then
      begin
        Default := Attrs[Index].Value;
        if Default = URI then
          Exit('');
      end;
      if (Attrs[Index].Name.URI = 'xmlns')
      and(Attrs[Index].Value = URI) then
        Exit(Attrs[Index].Name.Name);
    end;
    Inc(NSCount);
    Result := 'ns' + IntToUTF8(NSCount);
    AddAttribute(Result, 'xmlns', URI);
  end else begin
    Result := Parent.GetPrefix(URI);
  end;
end;

function TBuilder.Build(AParent: PBuilder; Info: PTypeInfo; Instance: Pointer; const Name, Namespace: UTF8String; const AIndent: UTF8String = ''): UTF8String;
var
  Index: Integer;
  Childs: UTF8String;
  LType : PTypeInfo;
begin
  Parent := AParent;
  NSCount := 0;

  Tag.URI := XMLNamespaceGroupAttribute.Get(Info);
  if Tag.URI <> '' then
  begin
    Result := Parent.BuildFields(Info, Instance, Tag.URI);
    Exit;
  end;

  Indent := AIndent;

  if Info = TypeInfo(TXMLNodeText) then
  begin
    Exit(UTF8String(string(Instance^)));
  end;

  Tag.Name := XMLNameAttribute.Get(Info, Name);
  Tag.URI := XMLNamespaceAttribute.GetType(Info, Namespace);
  Tag.Prefix := GetPrefix(Tag.URI);

  if Info.Kind = tkDynArray then
  begin
    Result := '';
    LType := Info.TypeData.DynArrElType^;
    for Index := 0 to Info.DynArrayLength(Instance) - 1 do
    begin
      Attrs := nil;
      Childs := BuildNode(LType, Info.DynArrayItem(Instance, Index));
      Result := Result + toString(Childs);
    end;
    Exit;
  end;

  case Info.ExtendedType of
    etString    : Childs := UTF8String(string(Instance^));
    etUTF8String: Childs := UTF8String(Instance^);
    etInteger   : Childs := IntToUTF8(Integer(Instance^));
    etInt64     : Childs := Int64ToUTF8(Int64(Instance^));
    etCurrency  : Childs := UTF8String(FloatToStrF(Currency(Instance^), TFloatFormat.ffFixed, 14, 2, TFormatSettings.Invariant));
    etBoolean   : Childs := XML_BOOL[Boolean(Instance^)];
    etDate      : Childs := UTF8String(FormatDateTime('yyyy-mm-dd"', TDateTime(Instance^)));
    etTime      : Childs := UTF8String(FormatDateTime('hh:nn:ss"Z"', TDateTime(Instance^)));
    etDateTime   : Childs := UTF8String(FormatDateTime('yyyy-mm-dd"T"hh:nn:ss"Z"', TDateTime(Instance^)));
  else
    Childs := BuildNode(Info, Instance);
  end;

  Result := ToString(Childs);
end;

{ TXmlRtti }

class procedure TXmlRtti.fromXML<T>(var V: T; const XML: UTF8String; const Root: string);
begin
  Finalize(V);
  FillChar(V, SizeOf(T), 0);
  ParseType(TypeInfo(T), @V, XML, Root);
end;

class procedure TXmlRtti.ParseNode(Info: PTypeInfo; Instance: Pointer;
  Node: PXMLNode; NameSpace: UTF8String = '');
var
  Parser: TParser;
begin
  Parser.ParseNode(Node, NameSpace, Info, Instance, False);
end;

class procedure TXmlRtti.ParseTree(Info: PTypeInfo; Instance: Pointer;
  const XML: TXMLTree; const Root: string);
var
  Parser: TParser;
begin
  Parser.Parse(Info, Instance, XML, UTF8String(Root));
end;

class procedure TXmlRtti.ParseType(Info: PTypeInfo; Instance: Pointer; const XML: UTF8String; const Root: string = '');
var
  Tree: TXMLTree;
begin
  Tree.Build(XML);
  ParseTree(Info, Instance, Tree, Root);
end;

class procedure TXmlRtti.fromXML<T>(var V: T; const XML: TXMLTree;
  const Root: string);
begin
  Finalize(V);
  FillChar(V, SizeOf(T), 0);
  ParseTree(TypeInfo(T), @V, XML, Root);
end;

class procedure TXmlRtti.fromXML<T>(var V: T; Node: PXMLNode; const NameSpace: UTF8String = '');
begin
  Finalize(V);
  FillChar(V, SizeOf(T), 0);
  ParseNode(TypeInfo(T), @V, Node, NameSpace);
end;

class function TXMLRtti.LoadFromFile(const AFileName: string): UTF8String;
var
  Stream: TFileStream;
begin
  Result := '';
  if FileExists(AFileName) then
  begin
    Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    try
      SetLength(Result, Stream.Size);
      Stream.Read(Result[1], Length(Result));
    finally
      Stream.Free;
    end;
  end;
end;

class function TXmlRtti.toXML<T>(const V: T; const Name: UTF8String; Header: Boolean = True): UTF8String;
begin
  Result := BuildType(TypeInfo(T), @V, Name, Header);
end;

class function TXmlRtti.BuildType(Info: PTypeInfo; Instance: Pointer; const Name: UTF8String; Header: Boolean): UTF8String;
var
  Builder: TBuilder;
begin
  if Header then
    Result := '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
  else
    Result := '';
  Result := Result + Builder.Build(nil, Info, Instance, Name, XMLNamespaceAttribute.GetType(Info, ''));
end;

end.
