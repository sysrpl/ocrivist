object FormSelectSource: TFormSelectSource
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Select Source'
  ClientHeight = 171
  ClientWidth = 295
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LBSources: TListBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 289
    Height = 124
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
    OnDblClick = LBSourcesDblClick
    ExplicitLeft = 32
    ExplicitTop = 16
    ExplicitWidth = 121
    ExplicitHeight = 97
  end
  object PnlBottom: TPanel
    Left = 0
    Top = 130
    Width = 295
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    OnResize = PnlBottomResize
    ExplicitLeft = 80
    ExplicitTop = 136
    ExplicitWidth = 185
    object BtnOK: TButton
      Left = 136
      Top = 8
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object BtnCancel: TButton
      Left = 216
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
