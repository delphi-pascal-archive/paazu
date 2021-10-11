object Form1: TForm1
  Left = 219
  Top = 127
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Paazu'
  ClientHeight = 524
  ClientWidth = 669
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 120
  TextHeight = 16
  object PaintBox1: TPaintBox
    Left = 0
    Top = 0
    Width = 669
    Height = 524
    Align = alClient
    OnMouseDown = PaintBox1MouseDown
    OnMouseMove = PaintBox1MouseMove
    OnMouseUp = PaintBox1MouseUp
    OnPaint = PaintBox1Paint
  end
  object MainMenu1: TMainMenu
    Left = 4
    Top = 4
    object Partie1: TMenuItem
      Caption = 'Game'
      object Nouvelle1: TMenuItem
        Caption = 'New'
        OnClick = Nouvelle1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Quitter1: TMenuItem
        Caption = 'Exit'
        OnClick = Quitter1Click
      end
    end
    object Options1: TMenuItem
      Caption = 'Options'
      OnClick = Options1Click
      object Ensembles1: TMenuItem
        Caption = 'Ensembles'
      end
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 45
    OnTimer = Timer1Timer
    Left = 4
    Top = 36
  end
end
