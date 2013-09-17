{$I DelphiTwain.inc}

unit DelphiTwain_Reg;

interface

uses Classes;

{Called by Delphi to register the component}
procedure Register;

implementation

uses DelphiTwain;

{Called by Delphi to register the component}
procedure Register();
begin
  {$IFNDEF DONTUSEVCL}
    RegisterComponents('Twain', [TDelphiTwain]);
  {$ENDIF}
end;

end.
