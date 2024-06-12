object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 515
  ClientWidth = 766
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = btAddClick
  TextHeight = 15
  object Label1: TLabel
    Left = 248
    Top = 163
    Width = 31
    Height = 15
    Caption = 'String'
  end
  object Label2: TLabel
    Left = 248
    Top = 224
    Width = 43
    Height = 15
    Caption = 'Boolean'
  end
  object Label3: TLabel
    Left = 248
    Top = 275
    Width = 55
    Height = 15
    Caption = 'TFontStyle'
  end
  object Label4: TLabel
    Left = 248
    Top = 384
    Width = 34
    Height = 15
    Caption = 'TBytes'
  end
  object Label5: TLabel
    Left = 400
    Top = 153
    Width = 109
    Height = 15
    Caption = 'TArray<TMyRecord>'
  end
  object Label6: TLabel
    Left = 421
    Top = 174
    Width = 81
    Height = 15
    Caption = 'MyRecord.Date'
  end
  object Label7: TLabel
    Left = 421
    Top = 203
    Width = 113
    Height = 15
    Caption = 'MyRecord.UTF8string'
  end
  object Label8: TLabel
    Left = 248
    Top = 440
    Width = 45
    Height = 15
    Caption = '1 as One'
  end
  object Label9: TLabel
    Left = 8
    Top = 8
    Width = 750
    Height = 25
    AutoSize = False
    Caption = 
      'Demonstration of the capabilities of the Execute.JSON.UTF8 Unit ' +
      'to convert a complex record from and to an UTF8 JSON string'
    WordWrap = True
  end
  object Label10: TLabel
    Left = 8
    Top = 39
    Width = 750
    Height = 25
    AutoSize = False
    Caption = 
      'Use the toJSON button to convert the data on screen to a JSON st' +
      'ring in the mmJSON Memo'
    WordWrap = True
  end
  object Label11: TLabel
    Left = 8
    Top = 70
    Width = 750
    Height = 25
    AutoSize = False
    Caption = 'Use the Clear button to empty the controls'
    WordWrap = True
  end
  object Label12: TLabel
    Left = 8
    Top = 101
    Width = 750
    Height = 25
    AutoSize = False
    Caption = 
      'Use the fromJSON button to reload the JSON string and assign val' +
      'ues to the controls'
    WordWrap = True
  end
  object mmJSON: TMemo
    Left = 8
    Top = 160
    Width = 225
    Height = 324
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'mmJSON')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Edit1: TEdit
    Left = 248
    Top = 184
    Width = 121
    Height = 23
    TabOrder = 1
    Text = 'hello'
  end
  object CheckBox1: TCheckBox
    Left = 248
    Top = 245
    Width = 97
    Height = 17
    Caption = 'Check'
    Checked = True
    State = cbChecked
    TabOrder = 2
  end
  object CheckListBox1: TCheckListBox
    Left = 248
    Top = 296
    Width = 121
    Height = 73
    ItemHeight = 15
    Items.Strings = (
      'fsBold'
      'fsItalic'
      'fsUnderline'
      'fsStrikeOut')
    TabOrder = 3
  end
  object Edit2: TEdit
    Left = 248
    Top = 405
    Width = 121
    Height = 23
    TabOrder = 4
    Text = 'some data'
  end
  object DateTimePicker1: TDateTimePicker
    Left = 543
    Top = 169
    Width = 89
    Height = 23
    Date = 45164.000000000000000000
    Time = 0.510924062502454000
    TabOrder = 5
  end
  object Edit3: TEdit
    Left = 543
    Top = 199
    Width = 121
    Height = 23
    TabOrder = 6
    Text = 'hello'
  end
  object NumberBox1: TNumberBox
    Left = 248
    Top = 461
    Width = 121
    Height = 23
    TabOrder = 7
    Value = 125.000000000000000000
  end
  object btToJSON: TButton
    Left = 429
    Top = 434
    Width = 114
    Height = 52
    Caption = 'toJSON'
    TabOrder = 8
    OnClick = btToJSONClick
  end
  object ControlList1: TControlList
    Left = 400
    Top = 228
    Width = 353
    Height = 200
    ItemHeight = 40
    ItemMargins.Left = 0
    ItemMargins.Top = 0
    ItemMargins.Right = 0
    ItemMargins.Bottom = 0
    ParentColor = False
    TabOrder = 9
    OnBeforeDrawItem = ControlList1BeforeDrawItem
    object lbDate: TLabel
      Left = 16
      Top = 8
      Width = 34
      Height = 15
      Caption = 'lbDate'
    end
    object lbText: TLabel
      Left = 112
      Top = 8
      Width = 31
      Height = 15
      Caption = 'lbText'
    end
  end
  object btAdd: TButton
    Left = 678
    Top = 170
    Width = 75
    Height = 25
    Caption = 'Add'
    TabOrder = 10
    OnClick = btAddClick
  end
  object btDel: TButton
    Left = 678
    Top = 201
    Width = 75
    Height = 25
    Caption = 'Del'
    TabOrder = 11
    OnClick = btDelClick
  end
  object btFromJSON: TButton
    Left = 654
    Top = 434
    Width = 99
    Height = 52
    Caption = 'fromJSON'
    TabOrder = 12
    OnClick = btFromJSONClick
  end
  object btClear: TButton
    Left = 549
    Top = 434
    Width = 99
    Height = 52
    Caption = 'Clear'
    TabOrder = 13
    OnClick = btClearClick
  end
end
