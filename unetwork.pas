unit UNetwork;

{$mode objfpc}
{$H+}
{$IfNDef Debug}
  {$Inline On}
{$EndIf}

interface

uses
  Classes,
  SysUtils;

function HttpGetStream(const FileName: String; var Stream: TMemoryStream): Boolean; inline;
function HttpGetStrings(const Url: String; var Strings: TStringList): Boolean; inline;
procedure KillConnection; inline;

var
  HttpCreated: Boolean = False;
  ConnectionErrorMsg: String;

implementation

uses
  httpsend,
  UErrorLog;

const
  ConnectionUrl = 'http://flsr.erikszeug.de/files/mod/';

var
  Http: THTTPSend;

function HttpResultCodes(const Code: UInt16; const FileName: String): Boolean; inline;
begin
  Result := False;
  case Code of
    100..299: Result := True;
    300..499: Error('Result Code ' + IntToStr(Code) + ': Could not find ' + FileName);
    500..599: Error('Result Code ' + IntToStr(Code) + ': Could not connect to server');
    else
      Result := False;
  end;
end;

function HttpGetStream(const FileName: String; var Stream: TMemoryStream): Boolean; inline;
begin
  Result := False;
  HTTPCreated := True;
  Http := THTTPSend.Create;
  try
    Result := Http.HTTPMethod('GET', ConnectionUrl + FileName);
    if Result then
    begin
      if HttpResultCodes(Http.ResultCode, ConnectionUrl + FileName) then
        Stream.LoadFromStream(Http.Document)
      else
        Result := False;
    end;
        {else
        if not UpdateThread.Terminated then
            begin
                Error('General HTTP Error: '+ConnectionUrl+FileName);
                ConnectionErrorMsg:='Keine Verbindung zum Server möglich!';
            end;}
  finally
    HTTPCreated := False;
    Http.Free;
  end;
end;

function HttpGetStrings(const Url: String; var Strings: TStringList): Boolean;
begin
  Result := False;
  HTTPCreated := True;
  Http := THTTPSend.Create;
  try
    Result := Http.HTTPMethod('GET', Url);
    if Result then
    begin
      if HttpResultCodes(Http.ResultCode, Url) then
        Strings.LoadFromStream(Http.Document)
      else
        Result := False;
    end;
        {else
        if not UpdateThread.Terminated then
            begin
                Error('General HTTP Error: '+URL+FileName);
                ConnectionErrorMsg:='Keine Verbindung zum Server möglich!';
            end;}
  finally
    HTTPCreated := False;
    Http.Free;
  end;
end;

procedure KillConnection; inline;
begin
  if HttpCreated then
    Http.Abort;
end;

end.
