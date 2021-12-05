unit UUpdate;

{$mode objfpc}
{$H+}
{$IfNDef Debug}
  {$Inline On}
{$EndIf}

interface

uses
  Classes,
  SysUtils;

type
  TUpdateThread = class(TThread)
  protected
    procedure Execute; override;
  end;

var
  UpdateThread: TUpdateThread;
  UpdateThreadExisting: Boolean = False;

implementation

uses
  UMainForm,
  md5,
  Forms,
  UConfig,
  UCompression,
  Unetwork,
  UErrorLog;

var
  ProgressBarFileCount: String = '';

procedure SetInfo(const Message: String); inline;
begin
  MainForm.ProgressText := ProgressBarFileCount + ' - ' + Message;
end;

function ReadHash(const Line: String): String; inline;
begin
  Result := Trim(LeftStr(Line, Pos('|', Line) - 1));
end;

function ReadName(const Line: String): String; inline;
begin
  Result := Trim(Copy(Line, Pos('|', Line) + 1, MaxInt));
end;

function DownloadFile(const FileName: String; var Stream: TMemoryStream): Boolean;
begin
  Result := HttpGetStream(FileName, Stream);
end;

function GetFile(const Hash, FileName: String): Boolean;
const
  FileExtension = '.7z';
var
  CompStream, UncompStream: TMemoryStream;
  StringStream: TStringStream; // For MD5 hash comparison
begin
  Result := False;
  CompStream := TMemoryStream.Create;

  // Downloading
  SetInfo('Downloading ' + ExtractFileName(FileName));
  if DownloadFile(FileName + FileExtension, CompStream) and (not UpdateThread.Terminated) then
  begin
    MainForm.UpdateProgressbar.StepIt;
    StringStream := TStringStream.Create('');

    // Extracting
    SetInfo('Extracting ' + ExtractFileName(FileName));
    if Decode(CompStream, StringStream) and (not UpdateThread.Terminated) then
    begin
      MainForm.UpdateProgressbar.StepIt;
      CompStream.Free;

      // MD5 hash comparisons
      SetInfo('Comparing ' + ExtractFileName(FileName));
      if (Hash = MD5Print(MD5String(StringStream.DataString))) and (not UpdateThread.Terminated) then
      begin
        MainForm.UpdateProgressbar.StepIt;

        // Creates directory and saves uncompressed file
        SetInfo('Saving ' + ExtractFileName(FileName));
        if ForceDirectories(MainForm.GamePathInput.Text + '/' + ExtractFilePath(FileName)) and (not UpdateThread.Terminated) then
        begin
          MainForm.UpdateProgressbar.StepIt;
          UncompStream := TMemoryStream.Create;
          UncompStream.LoadFromStream(StringStream);
          UncompStream.SaveToFile(MainForm.GamePathInput.Text + '/' + FileName);
          UncompStream.Free;
          Result := True;
        end
        else
          Error('Creating Directory Failed: ' + MainForm.GamePathInput.Text + '/' + ExtractFilePath(FileName));
      end
      else
        Error('Wrong MD5 Hash: ' + FileName);
    end
    else
    begin
      CompStream.Free;
      Error('Decoding Failed: ' + FileName + FileExtension);
    end;

    StringStream.Free;
  end
  else
  begin
    CompStream.Free;
    Error('Download Failed: ' + FileName + FileExtension);
  end;
end;

procedure UpdateMod;
const
  UpdateListName = 'updatelist.txt';
var
  UpdateList: TStringList;
  Stream: TMemoryStream;
  ListHash: String;
  DownloadError: Boolean = False;
  Correct: Boolean = False;
  NeedUpdate: Boolean = True;
  Line: Int32 = 0;
  ErrorCount: UInt32 = 0;
  modversionTemp: UInt32 = 0;

begin
  Stream := TMemoryStream.Create;
  UpdateList := TStringList.Create;
  ListHash := '';

  // Download
  if DownloadFile(UpdateListName, Stream) and (not UpdateThread.Terminated) then
  begin
    SetInfo('Checking update list.');
    UpdateList.LoadFromStream(Stream);
    // Find UpdateListName file within itself
    for Line := UpdateList.Count - 1 downto 0 do
    begin
      if Length(Trim(UpdateList.Strings[Line])) = 0 then
        Continue;
      if ReadName(UpdateList.Strings[Line]) = UpdateListName then
      begin
        ListHash := ReadHash(UpdateList.Strings[Line]);
        UpdateList.Delete(Line); // Remove UpdateListName file within, because the MD5 hash was created without it.
        break;
      end;
    end;
    if ListHash = MD5Print(MD5String(UpdateList.Text)) then
      Correct := True
    else
      with MainForm do
      begin
        DownloadError := True;
        UpdateProgressbar.Position := 0;
        UpdateInfoText := 'Update list invalid.' + ConnectionErrorMsg;
        UpdateButtonText := 'Check for updates';
        UpdateButtonState := 0;
      end;
  end
  else
    with MainForm do
    begin
      DownloadError := True;
      UpdateProgressbar.Position := 0;
      UpdateInfoText := 'Could not load update list.' + ConnectionErrorMsg;
      UpdateButtonText := 'Check for updates';
      UpdateButtonState := 0;
    end;
  Stream.Free;

  // Check for new version
  if (not DownloadError) and Correct then
  begin
    ListHash := '';
    for Line := UpdateList.Count - 1 downto 0 do
    begin
      if Length(Trim(UpdateList.Strings[Line])) = 0 then
        Continue;
      if ReadName(UpdateList.Strings[Line]) = 'version' then
      begin
        ListHash := ReadHash(UpdateList.Strings[Line]);
        UpdateList.Delete(Line);
        break;
      end;
    end;
    if (ListHash <> '') then
      modversionTemp := StrToInt(ListHash);
    if modversionTemp <= modversion then
      NeedUpdate := False;
  end;

  // Link for Changelog
  if (not DownloadError) and Correct then
  begin
    ListHash := '';
    for Line := UpdateList.Count - 1 downto 0 do
    begin
      if Length(Trim(UpdateList.Strings[Line])) = 0 then
        Continue;
      if ReadName(UpdateList.Strings[Line]) = 'changelog' then
      begin
        MainForm.ChangeLogUrl := ReadHash(UpdateList.Strings[Line]);
        MainForm.ChangeLogText := 'Changelog for update #' + IntToStr(modversionTemp);
        UpdateList.Delete(Line);
        break;
      end;
    end;
  end;

  if (not DownloadError) and NeedUpdate then
  begin
    // Set progressbar to maximum
    MainForm.UpdateProgressbar.Max := (UpdateList.Count * 5) + 1; // List length * 5 (steps of download and comparison process) + 1 UpdateListName file
    MainForm.UpdateProgressbar.StepIt; // One step for UpdateListName

    for Line := 0 to UpdateList.Count - 1 do
    begin
      if UpdateThread.Terminated then
        break;

      // Display of current file number relative to file count
      ProgressBarFileCount := IntToStr(Line + 1) + ' of ' + IntToStr(UpdateList.Count);
      MainForm.UpdateProgressbar.StepIt;

      // Skip empty lines
      if Length(Trim(UpdateList.Strings[Line])) = 0 then
      begin
        MainForm.UpdateProgressbar.StepBy(4);
        Continue;
      end;

      if FileExists(MainForm.GamePathInput.Text + '/' + ReadName(UpdateList.Strings[Line])) then
      begin
        // Check for hudshift and do not overwrite if present (because player made their own changes by now)
        if LowerCase(ReadName(UpdateList.Strings[Line])) = 'data/interface/hudshift.ini' then
        begin
          MainForm.UpdateProgressbar.StepBy(4);
          Continue;
        end;

        // Compare hashes
        SetInfo('Comparing ' + ExtractFileName(ReadName(UpdateList.Strings[Line])));
        if ReadHash(UpdateList.Strings[Line]) = MD5Print(MD5File(MainForm.GamePathInput.Text + '/' + ReadName(UpdateList.Strings[Line]))) then
          MainForm.UpdateProgressbar.StepBy(4)
        else
        // Just download if hashes do not match
        if not GetFile(ReadHash(UpdateList.Strings[Line]), ReadName(UpdateList.Strings[Line])) then
          Inc(ErrorCount);
      end
      else
      // Download because file does not exist
      if not GetFile(ReadHash(UpdateList.Strings[Line]), ReadName(UpdateList.Strings[Line])) then
        Inc(ErrorCount);
    end;

    // On abort
    if Line + 1 < UpdateList.Count then
      with MainForm do
      begin
        UpdateProgressbar.Position := 0;
        UpdateInfoText := 'Update incomplete.' + ConnectionErrorMsg;
        UpdateButtonText := 'Check for updates';
        UpdateButtonState := 0;
      end
    else
    // On error
    if ErrorCount <> 0 then
      with MainForm do
      begin
        UpdateProgressbar.Position := 0;
        UpdateInfoText := IntToStr(ErrorCount) + ' errors while updating.' + ConnectionErrorMsg;
        UpdateButtonText := 'Check for updates';
        UpdateButtonState := 0;
      end
    // Everything worked
    else
      with MainForm do
      begin
        UpdateProgressbar.Position := UpdateProgressbar.Max;
        UpdateInfoText := 'The up-to-date version is installed.';
        UpdateButtonText := 'Start Freelancer';
        UpdateButtonState := 2;
        modversion := modversionTemp;
        UpdateCompleted := True;
      end;
  end
  // No updated needed
  else
  if not DownloadError then
    with MainForm do
    begin
      UpdateProgressbar.Position := UpdateProgressbar.Max;
      UpdateInfoText := 'The up-to-date version is installed.';
      UpdateButtonText := 'Start Freelancer';
      UpdateButtonState := 2;
      UpdateCompleted := True;
    end;

  UpdateList.Free;
end;

procedure TUpdateThread.Execute;
begin
  with MainForm do
  begin
    UpdateInfoText := 'Searching for updates...';
    UpdateButtonText := 'Abort checking';
    UpdateButton.Enabled := True;
    GamePathButton.Enabled := False;
  end;
  MainForm.UpdateButtonState := 1;
  ConnectionErrorMsg := '';

  try
    UpdateMod;
  except
    with MainForm do
    begin
      UpdateProgressbar.Position := 0;
      UpdateInfoText := 'Error while updating.' + ConnectionErrorMsg;
      UpdateButtonText := 'Check for updates';
      UpdateButtonState := 0;
    end;
  end;

  WriteErrorLog;

  ProgressBarFileCount := '';
  with MainForm do
  begin
    ProgressText := '';
    UpdateButton.Enabled := True;
    GamePathButton.Enabled := True;
  end;
  UpdateThreadExisting := False;
end;

end.
