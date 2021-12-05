unit UErrorLog;

{$mode objfpc}
{$H+}
{$IfNDef Debug}
  {$Inline On}
{$EndIf}

interface

uses
  Classes,
  SysUtils;

procedure Error(const S: String); inline;
procedure WriteErrorLog;

implementation

var
  ErrorList: TStringList;

procedure Error(const S: String); inline;
begin
  ErrorList.Append(S);
end;

procedure WriteErrorLog;
const
  ErrorLogFileName = 'FLSR-Launcher_Errors.txt';
var
  ExistingErrorLog: TStringList;
begin
  if ErrorList.Count > 0 then
  begin
    ErrorList.Insert(0, DateTimeToStr(Now));

    if FileExists(ErrorLogFileName) then
    begin
      ExistingErrorLog := TStringList.Create;
      ExistingErrorLog.LoadFromFile(ErrorLogFileName);
      ExistingErrorLog.Add('');
      ExistingErrorLog.Append(ErrorList.Text);
      ExistingErrorLog.SaveToFile(ErrorLogFileName);
      ExistingErrorLog.Free;
      ErrorList.Clear;
    end
    else
      ErrorList.SaveToFile(ErrorLogFileName);
  end;
end;

initialization
  ErrorList := TStringList.Create;

finalization
  WriteErrorLog;
  ErrorList.Free;

end.
