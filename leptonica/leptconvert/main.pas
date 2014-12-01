{ LeptConvert - the Leptonica Library Converter

  Copyright (C) 2014 Malcolm Poole <malcolmgp@hotmail.com>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynMemo, SynHighlighterPas, SynHighlighterCpp,
  SynGutterBase, SynGutterMarks, SynGutterLineNumber, SynGutterChanges,
  SynGutter, SynGutterCodeFolding, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, Menus, types, strutils;

type

  { TForm1 }

  TForm1 = class(TForm)
    CopyMenuItem: TMenuItem;
    PasteMenuItem: TMenuItem;
    PopupMenu1: TPopupMenu;
    SaveButton: TButton;
    SaveDialog1: TSaveDialog;
    SynMemo1: TSynMemo;
    SynMemo2: TSynMemo;
    TranslateButton: TButton;
    InputFileNameEdit: TFileNameEdit;
    Label1: TLabel;
    TypesFoundList: TListBox;
    SynCppSyn1: TSynCppSyn;
    SynPasSyn1: TSynPasSyn;
    procedure CopyMenuItemClick(Sender: TObject);
    procedure FormCreate ( Sender: TObject ) ;
    procedure InputFileNameEditAcceptFileName ( Sender: TObject;
      var Value: String ) ;
    procedure PasteMenuItemClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure TranslateButtonClick(Sender: TObject);
    procedure SynMemo1Click(Sender: TObject);
    procedure TypesFoundListClick ( Sender: TObject ) ;
    procedure TypesFoundListDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  private
    { private declarations }
  public
    { public declarations }
  end;

  function AddConst( var txt: string ): Integer;
  function LineIsType( aline: string ): Integer;
  function RemoveCCommentMarkers( S: String ): String;
  function CatchReservedIdentifiers ( S: string ): string;

const
  TYPECOUNT = 30;

  APPNAME = 'Leptonica Library Converter';

var
  Form1: TForm1;


  ctypes: array[0..TYPECOUNT-1] of string = ( 'void',
             'void*',
             'PIX*',
             'PIX**',
             'BOX*',
             'BOX**',
             'BOXA*',
             'PIXA*',
             'PIXA**',
             'PTA*',
             'NUMA*',
             'NUMA**',
             'PIXCMAP*',
             'PIXCMAP**',
             'l_uint8',
             'l_uint8*',
             'l_uint8**',
             'l_int32',
             'l_int32*',
             'l_uint32',
             'l_uint32*',
             'l_uint32**',
             'l_float32',
             'l_float32*',
             'l_float32**',
             'FILE*',
             'size_t',
             'size_t*',
             'char*',
             'char**');




  ptypes: array[0..TYPECOUNT-1] of string = ( 'procedure',
             'Pointer',
             'PLPix',
             'PPLPix',
             'PLBox',
             'PPLBox',
             'PBoxArray',
             'PPixArray',
             'PPPixArray',
             'PPointArray',
             'PNumArray',
             'PPNumArray',
             'PPixCmap',
             'PPPixCmap',
             'Byte',
             'PByte',
             'PPByte',
             'LongInt',
             'PLongint',
             'LongWord',
             'PLongWord',
             'PPLongword',
             'Single',
             'PSingle',
             'PPSingle',
             'Pointer',   //FILE
             'LongWord',
             'PLongWord',
             'PChar',
             'PPChar');


implementation

{$R *.lfm}

{ TForm1 }

function AddConst( var txt: string ): Integer;
var
  i: SizeInt;
  tmptxt: String;
  tokenend: SizeInt;
  constval: String;
  typ: String;
  nam: String;
  comment: String;
begin
  result := -1;
  tmptxt := txt;
  comment := '';
  i := Pos('const', tmptxt);
  Delete(tmptxt, 1, i+5);
  tmptxt := Trim(tmptxt);
  i := Pos(';', tmptxt);
  if Length(tmptxt)>i+1 then comment := '//' + RemoveCCommentMarkers( Copy(tmptxt, i+1, MaxInt) );
  Delete(tmptxt, i, MaxInt);

  i := Pos('=', tmptxt);
  constval := Copy(tmptxt, i+1, MaxInt);
  constval := Trim(constval);
  if Length(constval)>1
     then if constval[1]='"' then //text constant
         begin
           constval[1] := '''';
           constval[Length(constval)] := '''';
         end;
  Delete(tmptxt, i, MaxInt);
  i := Pos(' ', tmptxt);
  typ := Copy(tmptxt, 1, i-1);
  nam := Trim(Copy(tmptxt, i+1, MaxInt));
  i := LineIsType(typ);
  if i>-1 then
     begin
       typ := ptypes[i];
       if Length(nam)>0
          then if Length(constval)>0
             then begin txt := 'const ' + nam + ' : ' + typ + ' = ' + constval + ';     ' + comment; Result := 0; end;
     end;
end;

function LineIsType( aline: string ): Integer;
var
  ct_index: Integer;
begin
  ct_index := 0;
  Result := -1;
  aline := DelSpace(aline);
  while (ct_index<TYPECOUNT) and (Result<0) do
        begin
          if ctypes[ct_index]=aline
             then Result := ct_index;
          Inc(ct_index);
        end;
end;

function RemoveCCommentMarkers(S: String): String;
var
  i: SizeInt;
begin
  i := Pos('/*', S);
  while i>0 do
        begin
          Delete(S, 1, i+1);
          i := Pos('/*', S);
        end;
  i := Pos('*/', S);
  while i>0 do
        begin
          Delete(S, i, MaxInt);
          i := Pos('*/', S);
        end;
  Result := S;
end;

function CatchReservedIdentifiers(S: string): string;
const
  RESERVED_WORDS: array [1..74] of String = (
    'absolute', 'and', 'array', 'asm', 'begin', 'case', 'const', 'constructor',
    'destructor', 'div', 'do', 'downto', 'else', 'end', 'file', 'for', 'function',
    'goto', 'if', 'implementation', 'in', 'inherited', 'inline', 'interface',
    'label', 'mod', 'nil', 'not', 'object', 'of', 'on', 'operator', 'or',
    'packed', 'procedure', 'program', 'record', 'reintroduce', 'repeat',
    'self', 'set', 'shl', 'shr', 'string', 'then', 'to', 'type',
    'unit', 'until', 'uses', 'var', 'while', 'with', 'xor', 'as', 'class',
    'except', 'exports', 'finalization', 'finally', 'initialization', 'is',
    'library', 'on', 'out', 'property', 'raise', 'threadvar', 'try',
    'dispose', 'exit', 'false', 'new', 'true');
var
  i: Integer;
begin
  Result := S;
  for i := 1 to High(RESERVED_WORDS) do
      if RESERVED_WORDS[i] = AnsiLowerCase( S ) then Result := 'L_' + S;
end;



procedure TForm1.TranslateButtonClick(Sender: TObject);
var
  aline: String;
  LineIndex: Integer;
  inComment: integer;
  ix: SizeInt;
  src_line: Integer;
  startpos: Integer;
  inCode: Integer;
  c: Integer;
  inFunction: Boolean;
  inParameters: Boolean;
  Parameters: string;
  lastParam: string;
  lastType: string;
  FunctionName: String;
  FuncResult: String;
  functiontype: String;


function TranslateType( ctype: string): string;
  var
    ct_index: Integer;
    found_ix: Integer;
  begin
    ct_index := 0;
    found_ix := -1;
    Result := ctype;
    while (ct_index<TYPECOUNT) and (found_ix<0) do
          begin
            if ctypes[ct_index]=ctype
               then found_ix := ct_index;
            Inc(ct_index);
          end;
    if found_ix>-1 then Result := ptypes[found_ix];
  end;


function ParseParameters( inputstr: String ): integer;
  var
    sp: SizeInt;
    typ, param: String;
    comma: SizeInt;
    tokenend: SizeInt;
    typeflag: Integer;
  begin
    typ := '';
    param := '';
    inputstr := Trim(inputstr);
    if Length(inputstr)>0 then
        begin
        Result := Pos(')',inputstr);
        comma := Pos(',', inputstr)-1;
        if Result>0 then comma := Result-1;
        tokenend := comma;
        if tokenend>0 then
            begin
               while (comma>1) and not (inputstr[comma] in [' ', '*']) do Dec(comma);
               param := Copy(inputstr, comma+1, tokenend-comma);
               while (comma>1) and (inputstr[comma] = '*') do begin typ := typ + inputstr[comma]; Dec(comma); end;
               while (comma>1) and (inputstr[comma] = ' ') do Dec(comma);
               tokenend := comma+1;
               while (comma>1) and (inputstr[comma] <> ' ') do Dec(comma);
               typ := Trim(copy(inputstr, comma, tokenend-comma) + typ);
            end;
        param := CatchReservedIdentifiers(param);
        if lastType=''
             then Parameters := param
        else if typ=lastType
             then Parameters := Parameters + ', ' + param
        else Parameters := Parameters + ': ' + TranslateType( lastType ) + '; ' + param;
        lastType := typ;
        if TypesFoundList.Items.IndexOf(typ)<0 then
            begin
               if TranslateType(typ)<>typ then typeflag := 0 else typeflag := SynMemo1.Lines.Count;
               TypesFoundList.Items.AddObject(typ, TObject(typeflag));
            end;
          end;
  end;

begin
  inComment := 0;
  inCode := 0;
  SynMemo1.Clear;
  inFunction := false;
  SynMemo1.Lines.Add(PadRight('{ This file is an automated translation to pascal of ' + ExtractFileName(InputFileNameEdit.FileName),78) + '}');
  SynMemo1.Lines.Add(PadRight('{ performed using the ' + APPNAME +', written by Malcolm Poole',78) + '}');
  SynMemo1.Lines.Add('');
  for src_line := 0 to SynMemo2.Lines.Count-1 do
      begin
        startpos := 1;
        aline := SynMemo2.Lines[src_line];
        if (inFunction) then
           begin
              ix := Pos('(', aline);
              if ix>0 then
                 if not inParameters then
                    begin
                      FunctionName := Copy(aline, 1, ix) ;
                      inParameters := true;
                      aline := Copy(aline, ix+1, MaxInt);
                      LineIndex := src_line;
                    end;
             if ParseParameters (aline)>0 then
                begin

                  aline := functiontype +
                           FunctionName + #32 +
                           Parameters +
                           ': ' + TranslateType( lastType )
                           + ' ) ' + FuncResult + '; cdecl; external LIBLEPT;';
                  if SynMemo1.Lines[SynMemo1.Lines.Count-1] = '' then SynMemo1.Lines.Delete(SynMemo1.Lines.Count-1);
                  SynMemo1.Lines.AddObject(aline, TObject(LineIndex));
                  SynMemo1.Lines.AddObject('', TObject(src_line)); SynMemo1.Lines.AddObject('', TObject(src_line));
                  aline := '';
                  inFunction := false;
                end;
           end;

        if not inFunction then
           begin
           if inComment + inCode<1 then
                 begin
                 ix := LineIsType( aline );
                 if ix>-1 then if not inFunction then
                    begin
                      inFunction := true;
                      inParameters := false;

                      lastType := '';
                      lastParam := '';
                      Parameters := '';
                      if ix=0 then begin functiontype := 'procedure '; FuncResult := ''; end
                      else begin functiontype := 'function '; FuncResult := ': ' + TranslateType( DelSpace( aline ) );   end;
                    end;
                 if Pos('const', aline)>0 then
                    if Pos('{', aline)<1 then // cannot handle constant arrays in pix2.h and pix4.h
                       begin
                         if AddConst(aline)>-1 then SynMemo1.Lines.InsertObject(SynMemo1.Lines.Count-1, aline, TObject(LineIndex));
                       end;
                 end;
              if inComment=0 then
              for c := 1 to Length(aline) do
                  begin
                    if aline[c]='{' then Inc(inCode)
                    else if aline[c]='}' then Dec(inCode);
                  end;
              ix := Pos('/*', aline);
              if ix>0 then
                 begin
                   Inc( inComment );
                   aline[ix] := '{';
                   startpos := ix;
                 end;
              ix := Pos('*/', aline);
              if ix>0 then
                 begin
                    Dec(inComment); aline[ix+1] := '}';
                    if (inComment=0) and (inCode=0) then
                        begin
                           SynMemo1.Lines.AddObject(Copy(aline, startpos, ix+1), TObject(src_line));
                           SynMemo1.Lines.AddObject('', TObject(src_line));
                        end;
                 end;
              if inCode=0 then if inComment>0 then SynMemo1.Lines.AddObject(aline, TObject(src_line));
           end;
      end;
end;

procedure TForm1.SaveButtonClick(Sender: TObject);
var
  s: TSearchRec;
  x: Integer;
begin
  SaveDialog1.FileName := SaveDialog1.InitialDir + ExtractFileNameOnly(InputFileNameEdit.FileName) + '.inc';
  x := 1;
  If SaveDialog1.Execute then
     begin
       if FileExistsUTF8(SaveDialog1.FileName) then
          begin
            while FileExistsUTF8(SaveDialog1.FileName + '.' + IntToStr(x)) do inc(x);
            RenameFileUTF8(SaveDialog1.FileName, SaveDialog1.FileName + '.' + IntToStr(x));
          end;
       SynMemo1.Lines.SaveToFile(SaveDialog1.FileName);
       InputFileNameEdit.InitialDir := ExtractFileDir(InputFileNameEdit.FileName);
       SaveDialog1.InitialDir := ExtractFileDir(SaveDialog1.FileName) + DirectorySeparator;
     end;
end;

procedure TForm1.InputFileNameEditAcceptFileName ( Sender: TObject;
  var Value: String ) ;
begin
  if not FileExistsUTF8(Value) then Exit;
  Caption := APPNAME + ' - ' + ExtractFileNameOnly(Value);
  SynMemo2.Lines.LoadFromFile(Value);
  SynMemo1.Clear;
  TypesFoundList.Clear;
end;

procedure TForm1.PasteMenuItemClick(Sender: TObject);
begin
  SynMemo1.PasteFromClipboard;
end;

procedure TForm1.CopyMenuItemClick(Sender: TObject);
begin
  SynMemo1.CopyToClipboard;
end;

procedure TForm1.FormCreate ( Sender: TObject ) ;
begin
  Caption := APPNAME;
end;

procedure TForm1.SynMemo1Click(Sender: TObject);
var
  SrcIndex: Integer;
begin
  SrcIndex := Integer(SynMemo1.Lines.Objects[SynMemo1.CaretY]);
  SynMemo2.CaretY := SrcIndex+5;
  SynMemo2.CaretY := SrcIndex;
end;

procedure TForm1.TypesFoundListClick ( Sender: TObject ) ;
var
  SrcIndex: Integer;
begin
  SrcIndex := Integer(TypesFoundList.Items.Objects[TypesFoundList.ItemIndex]);
  SynMemo1.CaretY := SrcIndex+5;
  SynMemo1.CaretY := SrcIndex;
end;

procedure TForm1.TypesFoundListDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  if Integer( TypesFoundList.Items.Objects[Index] ) > 0
     then TypesFoundList.Canvas.Font.Color := clRed
     else TypesFoundList.Canvas.Font.Color := clBlack;
  TypesFoundList.Canvas.FillRect(ARect);
  TypesFoundList.Canvas.TextOut(ARect.Left, ARect.Top, (Control as TListBox).Items[Index]);

end;

end.
