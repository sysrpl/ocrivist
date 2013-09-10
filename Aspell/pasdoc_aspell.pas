unit PasDoc_Aspell;

interface

uses SysUtils, Classes, PasDoc_ProcessLineTalk, contnrs
  {$IFDEF MSWINDOWS} ,registry {$ENDIF}
  {$IFDEF LINUX} , FileUtil {$ENDIF};

// Path to aspell.exe stored in Windows Registry at
// HKEY_LOCAL_MACHINE\SOFTWARE\Aspell\Path

{
  @author(Johannes Berg <johannes@sipsolutions.de>)
  @author(Michalis Kamburelis)
  a simple object vector
}
type
  TObjectVector = class(TObjectList)
  public
    { This is only to make constructor virtual, while original
      TObjectList has a static constructor. }
    constructor Create(const AOwnsObject: boolean); virtual;
{$IFNDEF FPC}
    // Fix bug in D7 TList.Sort.
    procedure Sort(Compare: TListSortCompare); reintroduce;
{$ENDIF}
  end;

function ObjectVectorIsNilOrEmpty(const AOV: TObjectVector): boolean;

function PathToAspell: string;

type
  TPasDocMessageType = (pmtPlainText, pmtInformation, pmtWarning, pmtError);

  TPasDocMessageEvent = procedure(const MessageType: TPasDocMessageType; const
                      AMessage: string; const AVerbosity: Cardinal) of object;

type
  TSpellingError = class
  public
    { the mis-spelled word }
    Word: string;
    { offset inside the checked string }
    Offset: Integer;
    { comma-separated list of suggestions }
    Suggestions: string;
  end;

  { This is a class to interface with aspell through pipe.
    It uses underlying @link(TProcessLineTalk) to execute and
    "talk" with aspell. }

  { TAspellProcess }

  TAspellProcess = class
  private
    FProcess: TProcessLineTalk;
    FAspellPath: string;
    FAspellMode: string;
    FAspellLanguage: string;
    FOnMessage: TPasDocMessageEvent;
    
    procedure DoMessage(const AVerbosity: Cardinal;
      const MessageType: TPasDocMessageType; const AMessage: string);
  public
    { Constructor.
      Values for AspellMode and AspellLanguage are the same as for
      aspell @--mode and @--lang command-line options.
      You can pass here '', then we will not pass appropriate
      command-line option to aspell. }
    constructor Create(const AAspellMode, AAspellLanguage: string;
      AOnMessage: TPasDocMessageEvent);
    destructor Destroy; override;

    property AspellMode: string read FAspellMode;
    
    property AspellLanguage: string read FAspellLanguage;

    procedure SetIgnoreWords(Value: TStringList);

    procedure AddToPersonalDict(Value: String);

    procedure AcceptForSession(Value: String);

    procedure SaveWordlist;

    { Spellchecks AString and returns result.
      Will create an array of TSpellingError objects,
      one entry for each misspelled word. 
      Offsets of TSpellingErrors will be relative to AString. }
    procedure CheckString(const AString: string; const AErrors: TObjectVector);
    
    property OnMessage: TPasDocMessageEvent read FOnMessage write FOnMessage;
  end;

implementation

uses PasDoc_Utils;

function ObjectVectorIsNilOrEmpty(const AOV: TObjectVector): boolean;
begin
  Result := (not Assigned(AOV)) or (AOV.Count = 0);
end;

function PathToAspell: string;
{$IFDEF MSWINDOWS}
var
  P: string='';
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  P := '';
  try
    // Navigate to proper "directory":
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    if Registry.OpenKeyReadOnly('\SOFTWARE\Aspell') then
      P := Registry.ReadString('Path');
  finally
    Registry.Free;
  end;
  Result := P;
end;
{$ENDIF}
{$IFDEF LINUX}
begin
  Result := ExtractFilePath( SearchFileInPath('aspell','',
                   SysUtils.GetEnvironmentVariable('PATH'),PathSeparator,[]) );
end;
{$ENDIF}

{ TObjectVector }

constructor TObjectVector.Create(const AOwnsObject: boolean);
begin
  inherited Create(AOwnsObject);
end;

{$IFNDEF FPC}
procedure TObjectVector.Sort(Compare: TListSortCompare);
begin
  if Count <= 1 then
    exit;
  inherited;
end;
{$ENDIF}

{ TAspellProcess }

constructor TAspellProcess.Create(const AAspellMode, AAspellLanguage: string;
  AOnMessage: TPasDocMessageEvent);
var FirstAspellLine: string;
begin
  inherited Create;

  FAspellPath := PathToAspell;
  if Length(FAspellPath)=0
     then Raise Exception.Create('Aspell program not found');
  FAspellMode := AAspellMode;
  FAspellLanguage := AAspellLanguage;
  FOnMessage := AOnMessage;
  
  FProcess := TProcessLineTalk.Create(nil);
  
  { calculate FProcess.CommandLine }
  FProcess.CommandLine := IncludeTrailingPathDelimiter(FAspellPath) + 'aspell -a';
  if AspellMode <> '' then
    FProcess.CommandLine := FProcess.CommandLine + ' --mode=' + AspellMode;
  if AspellLanguage <> '' then
    FProcess.CommandLine := FProcess.CommandLine + ' --lang=' + AspellLanguage;
    
  DoMessage(3, pmtInformation, 'Calling aspell process: "' + FProcess.CommandLine + '"');

  { execute }
  FProcess.Execute;

  { read and check 1st aspell output line }
  FirstAspellLine := FProcess.ReadLine;
  if Copy(FirstAspellLine, 1, 4) <> '@(#)' then
    raise Exception.CreateFmt('Wrong introduction from aspell: "%s"',
      [FirstAspellLine]);

  { switch to aspell terse mode (do not report about correct words;
    report only mispellings) }
  FProcess.WriteLine('!');
end;

destructor TAspellProcess.Destroy;
begin
  FProcess.Free;
  inherited;
end;

procedure TAspellProcess.SetIgnoreWords(Value: TStringList);
var
  i: Integer;
begin
  for i := 0 to Value.Count - 1 do
    FProcess.WriteLine('@' + Value[i]);
end;

procedure TAspellProcess.AddToPersonalDict ( Value: String ) ;
begin
  if Length(Value)>0
     then FProcess.WriteLine('*' + Value);
end;

procedure TAspellProcess.AcceptForSession ( Value: String ) ;
begin
  if Length(Value)>0
     then FProcess.WriteLine('@' + Value);
end;

procedure TAspellProcess.SaveWordlist;
begin
  FProcess.WriteLine('#');
end;

procedure TAspellProcess.CheckString(const AString: string;
  const AErrors: TObjectVector);
var
  s: string;
  p, p2: Integer;
  LError: TSpellingError;
begin
  AErrors.Clear;

  { make sure that FAspellMode is set -- should be removed, since it's
    passed to aspell command-line ? TODO. }
  if AspellMode <> '' then
  begin
    FProcess.WriteLine('-');
    FProcess.WriteLine('+' + AspellMode);
  end;

  { request spell-checking AString }
  FProcess.WriteLine('^' + SCharsReplace(AString, WhiteSpaceNL, ' '));

  repeat
    s := FProcess.ReadLine;
    { aspell returns empty line when it finished spell-checking AString }
    if s = '' then break;

    case s[1] of
      '*': Continue; // no error
      '#': begin
             LError := TSpellingError.Create;
             s := copy(s, 3, MaxInt); // get rid of '# '
             p := Pos(' ', s);
             LError.Word := copy(s, 1, p-1); // get word
             LError.Suggestions := '';
             s := copy(s, p+1, MaxInt);
             LError.Offset := StrToIntDef(s, 0)-1;
             AErrors.Add(LError);
           end;
      '&': begin
             LError := TSpellingError.Create;
             s := copy(s, 3, MaxInt); // get rid of '& '
             p := Pos(' ', s);
             LError.Word := copy(s, 1, p-1); // get word
             s := copy(s, p+1, MaxInt);
             p := Pos(' ', s);
             s := copy(s, p+1, MaxInt);
             p2 := Pos(':', s);
             LError.Suggestions := Copy(s, Pos(':', s)+2, MaxInt);
             SetLength(s, p2-1);
             LError.Offset := StrToIntDef(s, 0)-1;
             AErrors.Add(LError);
           end;
      else
        { Actually, it's nowhere formally specified that aspell error
          messages start with "Error:". So we can possibly accidentaly
          skip some error messages from aspell. }
        if IsPrefix('Error:', S) then
          DoMessage(2, pmtWarning, 'Aspell error: ' + S);
    end;
  until false;
end;

procedure TAspellProcess.DoMessage(const AVerbosity: Cardinal; 
  const MessageType: TPasDocMessageType;  const AMessage: string);
begin
  if Assigned(FOnMessage) then
    FOnMessage(MessageType, AMessage, AVerbosity);
end;

end.
