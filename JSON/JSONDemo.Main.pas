unit JSONDemo.Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ControlList, Vcl.StdCtrls,
  Vcl.NumberBox, Vcl.ComCtrls, Vcl.CheckLst,
  Execute.RTTI;

type
  TMyRecord = record
    Date: TDate;
    Text: UTF8String;
  end;

  TJSONData = record
    Text: string;
    Check: Boolean;
    FontStyle: TFontStyles;
    Bytes: TBytes;
    [Alias('1')]
    One: Integer;
    Records: TArray<TMyRecord>;
  end;

  TForm1 = class(TForm)
    mmJSON: TMemo;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    CheckListBox1: TCheckListBox;
    Label3: TLabel;
    Label4: TLabel;
    Edit2: TEdit;
    Label5: TLabel;
    DateTimePicker1: TDateTimePicker;
    Label6: TLabel;
    Label7: TLabel;
    Edit3: TEdit;
    Label8: TLabel;
    NumberBox1: TNumberBox;
    btToJSON: TButton;
    ControlList1: TControlList;
    lbDate: TLabel;
    lbText: TLabel;
    btAdd: TButton;
    btDel: TButton;
    btFromJSON: TButton;
    btClear: TButton;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    procedure btToJSONClick(Sender: TObject);
    procedure btFromJSONClick(Sender: TObject);
    procedure ControlList1BeforeDrawItem(AIndex: Integer; ACanvas: TCanvas;
      ARect: TRect; AState: TOwnerDrawState);
    procedure btDelClick(Sender: TObject);
    procedure btAddClick(Sender: TObject);
    procedure btClearClick(Sender: TObject);
  private
    { Déclarations privées }
    JSONData: TJSONData;
    procedure ShowJSONData;
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Execute.JSON.UTF8;

procedure TForm1.btClearClick(Sender: TObject);
begin
  Finalize(JSONData);
  FillChar(JSONData, SizeOf(JSONData), 0);
  ShowJSONData;
end;

procedure TForm1.btFromJSONClick(Sender: TObject);
begin
  var S := mmJSON.Text;
  JSON.fromJSON(JSONData, UTF8String(S));
  ShowJSONData;
end;

procedure TForm1.ShowJSONData;
begin
  Edit1.Text := JSONData.Text;
  CheckBox1.Checked := JSONData.Check;
  CheckListBox1.Checked[0] := fsBold in JSONData.FontStyle;
  CheckListBox1.Checked[1] := fsItalic in JSONData.FontStyle;
  CheckListBox1.Checked[2] := fsUnderline in JSONData.FontStyle;
  CheckListBox1.Checked[3] := fsStrikeOut in JSONData.FontStyle;
  Edit2.Text := TEncoding.ANSI.GetString(JSONData.Bytes);
  NumberBox1.ValueInt := JSONData.One;
  ControlList1.ItemCount := Length(JSONData.Records);
end;

procedure TForm1.btToJSONClick(Sender: TObject);
begin
  JSONData.Text := Edit1.Text;
  JSONData.Check := CheckBox1.Checked;
  JSONData.FontStyle := [];
  if CheckListBox1.Checked[0] then JSONData.FontStyle := JSONData.FontStyle + [fsBold];
  if CheckListBox1.Checked[1] then JSONData.FontStyle := JSONData.FontStyle + [fsItalic];
  if CheckListBox1.Checked[2] then JSONData.FontStyle := JSONData.FontStyle + [fsUnderline];
  if CheckListBox1.Checked[3] then JSONData.FontStyle := JSONData.FontStyle + [fsStrikeOut];
  JSONData.Bytes := TEncoding.ANSI.GetBytes(Edit2.Text);
  JSONData.One := NumberBox1.ValueInt;

  var S := JSON.toJSON(JSONData);
  mmJSON.Text := string(s);
end;

procedure TForm1.btAddClick(Sender: TObject);
begin
  var L := Length(JSONData.Records);
  SetLength(JSONData.Records, L + 1);
  JSONData.Records[L].Date := DateTimePicker1.DateTime;
  JSONData.Records[L].Text := Edit3.Text;
  ControlList1.ItemCount := Length(JSONData.Records);
end;

procedure TForm1.btDelClick(Sender: TObject);
begin
  if ControlList1.ItemIndex >= 0 then
  begin
    Delete(JSONData.Records, ControlList1.ItemIndex, 1);
    ControlList1.ItemCount := Length(JSONData.Records);
  end;
end;

procedure TForm1.ControlList1BeforeDrawItem(AIndex: Integer; ACanvas: TCanvas;
  ARect: TRect; AState: TOwnerDrawState);
begin
  if (AIndex < 0) or (AIndex >= Length(JSONData.Records)) then
    Exit;
  lbDate.Caption := DateToStr(JSONData.Records[AIndex].Date);
  lbText.Caption := string(JSONData.Records[AIndex].Text);
end;

end.
