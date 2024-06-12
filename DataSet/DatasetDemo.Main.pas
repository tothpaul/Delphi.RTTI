unit DatasetDemo.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Vcl.ExtCtrls, Vcl.DBCtrls,
  Vcl.Grids, Vcl.DBGrids, Vcl.StdCtrls, Execute.RTTIDataSet, Execute.JSON, Execute.XML.RTTI,
  Vcl.ComCtrls;

type
  TMyRecord = record
  [XMLAttributes]
    [FieldFlags([ffUnique, ffAutoInc])]
    id: Integer;
    Code: string;
  [XMLNodes]
    Name: string;
    [DisplayLabel('Birth Date')]
    Birth: TDate;
    [FieldFlags([ffNow, ffReadOnly])]
    CreationDate: TDateTime;
  end;

  [XMLNamespace('http://example.com')]
  TMyRecords = record
    [XMLName('Item')]
    Items: TArray<TMyRecord>;
  end;

  TForm1 = class(TForm)
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    DBNavigator1: TDBNavigator;
    Memo1: TMemo;
    Panel1: TPanel;
    btToXML: TButton;
    btToJSON: TButton;
    btFromJSON: TButton;
    btFromXML: TButton;
    btClear: TButton;
    Panel2: TPanel;
    RichEdit1: TRichEdit;
    procedure FormCreate(Sender: TObject);
    procedure btToJSONClick(Sender: TObject);
    procedure btToXMLClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
    procedure btFromJSONClick(Sender: TObject);
    procedure btFromXMLClick(Sender: TObject);
  private
    { Déclarations privées }
    Data: TMyRecords;
    DataSet: TRTTIDataSet;
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure Colorize2(RichEdit: TRichEdit; const StartWith, EndWith: string; Color: TColor; Protect: Boolean);
begin
  var Len := Length(RichEdit.Text);
  var Start := 0;
  // Starting position
  var Pos := RichEdit.FindText(StartWith, Start, Len, [stMatchCase]);
  while Pos >= 0 do
  begin
    // Ending position
    var Stop := RichEdit.FindText(EndWith, Pos + Length(StartWith), Len, [stMatchCase]);
    if Stop < 0 then
      Exit;

    // special case: [FieldFlags([ffUnique, ffAutoInc])]
    var Prev := RichEdit.FindText(StartWith, Pos + Length(StartWith), Len, [stMatchCase]);
    if (Prev > 0) and (Prev < Stop) then
    begin
      Stop := RichEdit.FindText(EndWith, Stop + Length(EndWith), Len, [stMatchCase]);
      if Stop < 0 then
        Exit;
    end;

    // Update style
    RichEdit.SelStart := Pos;
    RichEdit.SelLength := 1;
    // Special case: [XMLNamespace('http://example.com')]
    if not RichEdit.SelAttributes.Protected then
    begin
      RichEdit.SelLength := Stop + Length(EndWith) - Pos;
      RichEdit.SelAttributes.Color := Color;
      RichEdit.SelAttributes.Protected := Protect;
    end;
    Start := Stop + Length(EndWith);
    Pos := RichEdit.FindText(StartWith, Start, Len, [stMatchCase]);
  end;
end;

procedure Colorize(RichEdit: TRichEdit);
const
  Keywords: array[0..5] of string = ('type', 'record', 'string', 'var', 'begin', 'end');
begin
  var Len := Length(RichEdit.Text);
  for var i := 0 to High(Keywords) do
  begin
    var Start := 0;
    var Pos := RichEdit.FindText(Keywords[i], Start, Len, [stWholeWord]);
    while Pos >= 0 do
    begin
      RichEdit.SelStart := Pos;
      RichEdit.SelLength := Length(Keywords[i]);
      RichEdit.SelAttributes.Color := clNavy;
      RichEdit.SelAttributes.Style := [fsBold];
      Start := Pos + Length(Keywords[i]);
      Pos := RichEdit.FindText(Keywords[i], Start, Len, [stWholeWord]);
    end;
  end;
  Colorize2(Richedit, '[', ']', clPurple, False);
  Colorize2(RichEdit, '''', '''', clBlue, True);
  Colorize2(RichEdit, '//', #13, clGreen, True);
end;

procedure TForm1.btClearClick(Sender: TObject);
begin
  DataSet.Close;
  Finalize(Data);
  FillChar(Data, SizeOf(Data), 0);
  DataSet.Open;
end;

procedure TForm1.btToJSONClick(Sender: TObject);
begin
  Memo1.Text := JSON.ToJSON(Data);
end;

procedure TForm1.btToXMLClick(Sender: TObject);
begin
  Memo1.Text := TXmlRtti.toXML(Data, 'Items', True);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Colorize(RichEdit1);
  DataSet := TRTTIDataSet.Create(Self);
  DataSet.SetData<TMyRecord>(Data.Items);
  DataSource1.DataSet := DataSet;
  DataSet.Open;
end;

procedure TForm1.btFromJSONClick(Sender: TObject);
begin
  DataSet.Close;
  var S := Memo1.Text;
  JSON.fromJSON(Data, S);
  DataSet.Open;
end;

procedure TForm1.btFromXMLClick(Sender: TObject);
begin
  DataSet.Close;
  var S := Memo1.Text;
  TXMLRtti.fromXML(Data, S, 'Items');
  DataSet.Open;
end;

end.
