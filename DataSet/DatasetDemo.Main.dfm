object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Execute RTTI Demo'
  ClientHeight = 617
  ClientWidth = 985
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 624
    Height = 617
    Align = alClient
    Caption = 'Panel2'
    TabOrder = 0
    ExplicitLeft = 232
    ExplicitTop = 224
    ExplicitWidth = 185
    ExplicitHeight = 41
    object DBGrid1: TDBGrid
      Left = 1
      Top = 26
      Width = 622
      Height = 261
      Align = alClient
      DataSource = DataSource1
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -12
      TitleFont.Name = 'Segoe UI'
      TitleFont.Style = []
    end
    object DBNavigator1: TDBNavigator
      Left = 1
      Top = 1
      Width = 622
      Height = 25
      DataSource = DataSource1
      Align = alTop
      TabOrder = 1
      ExplicitLeft = -15
      ExplicitWidth = 200
    end
    object Memo1: TMemo
      Left = 1
      Top = 287
      Width = 622
      Height = 288
      Align = alBottom
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Courier New'
      Font.Style = []
      Lines.Strings = (
        'Memo1')
      ParentFont = False
      TabOrder = 2
      ExplicitTop = 240
      ExplicitWidth = 564
    end
    object Panel1: TPanel
      Left = 1
      Top = 575
      Width = 622
      Height = 41
      Align = alBottom
      Caption = 'Panel1'
      ParentBackground = False
      ParentColor = True
      ShowCaption = False
      TabOrder = 3
      ExplicitLeft = 0
      ExplicitTop = 401
      ExplicitWidth = 628
      object btToXML: TButton
        AlignWithMargins = True
        Left = 85
        Top = 4
        Width = 75
        Height = 33
        Align = alLeft
        Caption = 'toXML'
        TabOrder = 0
        OnClick = btToXMLClick
      end
      object btToJSON: TButton
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 75
        Height = 33
        Align = alLeft
        Caption = 'toJSON'
        TabOrder = 1
        OnClick = btToJSONClick
      end
      object btFromJSON: TButton
        AlignWithMargins = True
        Left = 247
        Top = 4
        Width = 75
        Height = 33
        Align = alLeft
        Caption = 'fromJSON'
        TabOrder = 2
        OnClick = btFromJSONClick
      end
      object btFromXML: TButton
        AlignWithMargins = True
        Left = 328
        Top = 4
        Width = 75
        Height = 33
        Align = alLeft
        Caption = 'fromXML'
        TabOrder = 3
        OnClick = btFromXMLClick
      end
      object btClear: TButton
        AlignWithMargins = True
        Left = 166
        Top = 4
        Width = 75
        Height = 33
        Align = alLeft
        Caption = 'Clear'
        TabOrder = 4
        OnClick = btClearClick
      end
    end
  end
  object RichEdit1: TRichEdit
    Left = 624
    Top = 0
    Width = 361
    Height = 617
    Align = alRight
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'type'
      '  TMyRecord = record'
      '  [XMLAttributes]'
      '    [FieldFlags([ffUnique, ffAutoInc])]'
      '    id: Integer;'
      '    Code: string;'
      '  [XMLNodes]'
      '    Name: string;'
      '    [DisplayLabel('#39'Birth Date'#39')]'
      '    Birth: TDate;'
      '    [FieldFlags([ffNow, ffReadOnly])]'
      '    CreationDate: TDateTime;'
      '  end;'
      ''
      '  [XMLNamespace('#39'http://example.com'#39')]'
      '  TMyRecords = record'
      '    [XMLName('#39'Item'#39')]'
      '    Items: TArray<TMyRecord>;'
      '  end;'
      ''
      'var'
      '  Data: TMyRecords;'
      'begin'
      '  DataSet := TRTTIDataSet.Create(Self);'
      '  DataSet.SetData<TMyRecord>(Data.Items);'
      '  DataSource1.DataSet := DataSet;'
      '  DataSet.Open;'
      ''
      '  // Data to JSON'
      '  var js  := JSON.ToJSON(Data);'
      '  // JSON to Data'
      '  JSON.fromJSON(Data, js);'
      ''
      '  // Data to XML'
      '  var xml := TXmlRtti.toXML(Data, '#39'Items'#39', True);'
      '  // XML to Data'
      '  TXMLRtti.fromXML(Data, xml, '#39'Items'#39');'
      'end;')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object DataSource1: TDataSource
    Left = 312
    Top = 160
  end
end
