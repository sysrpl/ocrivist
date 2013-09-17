unit threshold;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, Buttons;

type

  { TThresholdForm }

  TThresholdForm = class ( TForm )
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Image1: TImage;
    Panel1: TPanel;
    ThreshTrackBar: TTrackBar;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ThresholdForm: TThresholdForm;

implementation

{$R *.lfm}

end.

