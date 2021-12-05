unit UResolution;

{$mode objfpc}
{$H+}
{$IfNDef Debug}
  {$Inline On}
{$EndIf}

interface

uses
  Classes,
  SysUtils;

function SelectResInList(X, Y: String): UInt8; inline;
procedure SetFLResolution(const ResString: String);
function GetFLResolution(const ResList: TStrings): Boolean;
procedure SetHudShift(const GamePath: String);

implementation

uses
  UMainForm,
  Windows,
  shlobj,
  strutils;

const
  HudShiftPath = '/DATA/INTERFACE/HudShift.ini';
  PerfOptionsPath = '/My Games/Freelancer/PerfOptions.ini';

var
  AspectRatio: String;

function GetMyDocuments: String; inline;
var
  PathFound: Bool;
  Path: array[0..Max_Path] of Char;
begin
  Result := '';
  PathFound := ShGetSpecialFolderPath(0, Path, CSIDL_Personal, False);
  if PathFound then
    Result := Path
  else
    ShowWarning('Could not find "My Documents" directory!');
end;

procedure GetDesktopResolution(var X, Y: String); inline;
begin
  X := IntToStr(GetSystemMetrics(SM_CXSCREEN));
  Y := IntToStr(GetSystemMetrics(SM_CYSCREEN));
end;

function SelectResInList(X, Y: String): UInt8; inline;
begin
  if (Length(X) = 0) or (Length(Y) = 0) then
    GetDesktopResolution(X, Y);

  with MainForm do
  begin
    for Result := 0 to ResolutionComboBox.Items.Count - 1 do
      if AnsiContainsText(ResolutionComboBox.Items.Strings[Result], X + '×' + Y) then
        break;
    if Result = ResolutionComboBox.Items.Count - 1 then
      Result := 0;
    ResolutionComboBox.ItemIndex := Result;
  end;
end;

procedure SetHudShift(const GamePath: String);
var
  TempString: String;
  HudShift, TempList: TStringList;
  Line, StartLine, EndLine: Int32;
  FoundHudShift: Boolean;
begin
  if FileExists(GamePath + HudShiftPath) then
  begin
    HudShift := TStringList.Create;
    TempList := TStringList.Create;

    HudShift.LoadFromFile(GamePath + HudShiftPath);

    repeat
      begin
        FoundHudShift := False;
        for Line := 0 to HudShift.Count - 1 do
          if AnsiStartsText('[HudShift]', Trim(HudShift.Strings[Line])) then
          begin                 
            FoundHudShift := True;
            StartLine := Line;
            Break;
          end;

        if FoundHudShift then
        begin
          if StartLine + 1 < HudShift.Count then
          begin
            EndLine := HudShift.Count;
            for Line := StartLine + 1 to HudShift.Count - 1 do
            begin
              TempString := Trim(HudShift.Strings[Line]);
              if AnsiStartsText('[', TempString) and AnsiEndsText(']', TempString) then
              begin
                EndLine := Line;
                Break;
              end;
            end;
          end;

          if StartLine > 0 then
            for Line := 0 to StartLine - 1 do
              TempList.Append(HudShift.Strings[Line]);
          if EndLine < HudShift.Count then
            for Line := EndLine to HudShift.Count - 1 do
              TempList.Append(HudShift.Strings[Line]);

          HudShift.Assign(TempList);
          TempList.Clear;
        end;
      end;
    until not FoundHudShift;

    TempList.Free;

    with HudShift do
    begin
      Append('[HUDShift]');
      case AspectRatio of
        '5:3': Append('Horizontal = 0.1275 ; 5:3');
        '5:4': Append('Horizontal = -0.031875 ; 5:4');
        '16:9': Append('Horizontal = 0.17 ; 16:9');
        '16:10': Append('Horizontal = 0.102 ; 16:10');
        else
          Append('Horizontal = auto ; 4:3');
      end;
    end;

    HudShift.SaveToFile(GamePath + HudShiftPath);
    HudShift.Free;
  end
  else
    ShowError('Could not find HudShift.ini!');
end;

function GetFLResolution(const ResList: TStrings): Boolean;
var
  MyDocumentsPath: String = '';
  Options: TStringList;
  Line: Int32;
  EqualPos, CommaPos: Cardinal;
  Resolution, XRes, YRes: String;
  Found: Boolean = False;
  Error: String = '';
  Warning: String = '';
begin
  Result := False;
  MyDocumentsPath := GetMyDocuments;
  if Length(MyDocumentsPath) = 0 then
    Exit;

  if FileExists(MyDocumentsPath + PerfOptionsPath) then
  begin
    Options := TStringList.Create;
    Options.LoadFromFile(MyDocumentsPath + PerfOptionsPath);

    for Line := 0 to Options.Count - 1 do
      if AnsiContainsText(Options.Strings[Line], 'size') then
      begin
        Found := True;
        EqualPos := Pos('=', Options.Strings[Line]);
        if EqualPos <> 0 then
        begin
          Resolution := Trim(Copy(Options.Strings[Line], EqualPos + 1, MaxInt));
          CommaPos := Pos(',', Resolution);
          if CommaPos <> 0 then
          begin
            XRes := Trim(Copy(Resolution, 0, CommaPos - 1));
            if Length(XRes) = 0 then
              Warning := 'Resolution width not found in Freelancer PerfOptions.ini!';

            YRes := Trim(Copy(Resolution, CommaPos + 1, MaxInt));
            if Length(YRes) = 0 then
              Warning := Warning + 'Resolution height not found in Freelancer PerfOptions.ini';

            Resolution := Utf8ToAnsi(ResList.Strings[SelectResInList(XRes, YRes)]);
            AspectRatio := Trim(LeftStr(Resolution, Pos(' ', Resolution) - 1));
            Result := True;
          end
          else
            Error := 'Resolution not found in Freelancer PerfOptions.ini';
        end
        else
          Error := 'Resolution not found in Freelancer PerfOptions.ini';

        break;
      end;

    if not Found then
      Error := 'Resolution not found in Freelancer PerfOptions.ini';

    Options.Free;
  end
  else
    Warning := MyDocumentsPath + PerfOptionsPath + ' not found!';

  if Length(Error) <> 0 then
    ShowError(Error);
  if Length(Warning) <> 0 then
    ShowWarning(Warning);
end;

procedure SetFLResolution(const ResString: String);

  procedure SetResFromList(var X, Y: String; ResString: String);
  var
    xPos: UInt8;
  begin
    ResString := Trim(Copy(ResString, Pos(' ', ResString), MaxInt));
    xPos := Pos(Utf8ToAnsi('×'), ResString);
    X := Copy(ResString, 1, xPos - 1);
    Y := Copy(ResString, xPos + 2, MaxInt);
  end;

var
  MyDocumentsPath: String;
  X, Y: String;
  Options: TStringList;
  i: Integer;
  Found: Boolean = False;
  Error: String = '';
begin
  MyDocumentsPath := GetMyDocuments;
  if Length(MyDocumentsPath) = 0 then
    Exit;

  if FileExists(MyDocumentsPath + PerfOptionsPath) then
  begin
    Options := TStringList.Create;
    Options.LoadFromFile(MyDocumentsPath + PerfOptionsPath);

    SetResFromList(X, Y, ResString);
    AspectRatio := Trim(LeftStr(ResString, Pos(' ', ResString) - 1));

    for i := 0 to Options.Count - 1 do
      if AnsiContainsText(Options.Strings[i], 'size') then
      begin
        Found := True;
        Options.Strings[i] := 'size= ' + X + ', ' + Y;
        break;
      end;

    if not Found then
    begin
      for i := 0 to Options.Count - 1 do
        if AnsiContainsText(Options.Strings[i], '[display]') then
        begin
          Found := True;
          if (i + 1) < Options.Count then
            Options.Insert(i + 1, 'size= ' + X + ', ' + Y)
          else
            Options.Append('size= ' + X + ', ' + Y);

          break;
        end;

      if not Found then
        Error := MyDocumentsPath + PerfOptionsPath + ' incomplete!';
    end;

    Options.SaveToFile(MyDocumentsPath + PerfOptionsPath);
    Options.Free;
  end
  else
    Error := MyDocumentsPath + PerfOptionsPath + ' not found!';

  if Length(Error) <> 0 then
    ShowError(Error)
  else
    ShowInformation('Resolution successfully changed.');
end;

end.
