unit UMainForm;

{$mode objfpc}
{$H+}
{$IfNDef Debug}
  {$Inline On}
{$EndIf}

interface

uses
  LCLType,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ComCtrls,
  ExtCtrls,
  Menus,
  LCLIntf,
  Buttons,
  Windows,
  syncobjs,
  Classes;

type
  TMainForm = class(TForm)
    AutoResolutionButton: TButton;
    AccountInfoLabel: TLabel;
    AccountRenameButton: TSpeedButton;
    Moving3dTargetViewCheckBox: TCheckBox;
    ResolutionApplyButton: TButton;
    AccountDeleteButton: TSpeedButton;
    GameTimer: TTimer;
    UpdateButton: TButton;
    GamePathButton: TButton;
    AutoUpdateCheckBox: TCheckBox;
    AutoStartGameCheckBox: TCheckBox;
    WindowedCheckBox: TCheckBox;
    AccountComboBox: TComboBox;
    LogoImage: TImage;
    ResolutionComboBox: TComboBox;
    ServerInput: TEdit;
    GamePathInput: TEdit;
    ChangeLogLabel: TLabel;
    ResolutionLabel: TLabel;
    GameInfoLabel: TLabel;
    ServerLabel: TLabel;
    ProgressLabel: TLabel;
    UpdateInfoLabel: TLabel;
    PageControl: TPageControl;
    UpdateProgressbar: TProgressBar;
    GamePathDialog: TSelectDirectoryDialog;
    ResolutionTabSheet: TTabSheet;
    GameTabSheet: TTabSheet;
    ServerTabSheet: TTabSheet;
    AccountsTabSheet: TTabSheet;
    UpdateTimer: TTimer;
    InitTimer: TTimer;
    procedure AccountComboBoxChange(Sender: TObject);
    procedure AccountComboBoxKeyPress(Sender: TObject; var Key: Char);
    procedure AccountComboBoxSelect(Sender: TObject);
    procedure AccountDeleteButtonClick(Sender: TObject);
    procedure AccountRenameButtonClick(Sender: TObject);
    procedure AutoResolutionButtonClick(Sender: TObject);
    procedure GamePathButtonClick(Sender: TObject);
    procedure GameTimerTimer(Sender: TObject);
    procedure ResolutionApplyButtonClick(Sender: TObject);
    procedure UpdateButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure LogoImageClick(Sender: TObject);
    procedure ChangeLogLabelClick(Sender: TObject);
    procedure UpdateTimerTimer(Sender: TObject);
    procedure InitTimerTimer(Sender: TObject);


  public
    UpdateInfoText, ProgressText, UpdateButtonText, ChangeLogUrl, ChangeLogText: String;
    UpdateCompleted: Boolean;
    UpdateButtonState: UInt8;
  end;

procedure ShowError(const Message: String);
procedure ShowWarning(const Message: String);
procedure ShowInformation(const Message: String);

var
  MainForm: TMainForm;
  CriticalSection: TCriticalSection;

implementation

{$R *.lfm}

uses
  SysUtils,
  process,
  UUpdate,
  UConfig,
  UResolution,
  Unetwork,
  UErrorLog,
  UAccountConfig,
  UExePatch,
  URenameAccountForm;

procedure ShowError(const Message: String);
var
  BoxStyle: Integer;
begin
  BoxStyle := MB_ICONERROR + MB_OK;
  Application.MessageBox(PChar(Message), PChar('Error'), BoxStyle);
end;

procedure ShowWarning(const Message: String);
var
  BoxStyle: Integer;
begin
  BoxStyle := MB_ICONWARNING + MB_OK;
  Application.MessageBox(PChar(Message), PChar('Warning'), BoxStyle);
end;

procedure ShowInformation(const Message: String);
var
  BoxStyle: Integer;
begin
  BoxStyle := MB_ICONINFORMATION + MB_OK;
  Application.MessageBox(PChar(Message), PChar('Information'), BoxStyle);
end;

procedure TMainForm.AutoResolutionButtonClick(Sender: TObject);
begin
  SelectResInList('', '');
end;

procedure TMainForm.AccountComboBoxKeyPress(Sender: TObject; var Key: Char);
var
  AccountName: String;
begin
  AccountName := AccountComboBox.Text;
  AccountName := AccountName.Trim;
  if (Int32(Key) = VK_RETURN) and not HasAccountName(AccountName) then
  begin
    CreateAccount(AccountName);
    AccountComboBox.AddItem(AccountName, nil);
    AccountComboBox.ItemIndex := AccountComboBox.Items.IndexOf(AccountName);
    AccountComboBoxSelect(Self);
  end;
end;

procedure TMainForm.AccountComboBoxChange(Sender: TObject);
var
  ButtonsVisible: Boolean;
begin
  ButtonsVisible := AccountComboBox.ItemIndex >= 0;
  AccountRenameButton.Visible := ButtonsVisible;
  AccountDeleteButton.Visible := ButtonsVisible;
end;

procedure TMainForm.AccountComboBoxSelect(Sender: TObject);
var
  Selected: Boolean;
begin
  Selected := AccountComboBox.ItemIndex >= 0;
  if Selected then
    SetAccount(AccountComboBox.Items.Strings[AccountComboBox.ItemIndex]);
  AccountRenameButton.Visible := Selected;
  AccountDeleteButton.Visible := Selected;
end;

procedure TMainForm.AccountDeleteButtonClick(Sender: TObject);
var
  NewAccountNames: TStrings;
begin
  if AccountComboBox.ItemIndex >= 0 then
  begin
    DeleteAccount(AccountComboBox.Items.Strings[AccountComboBox.ItemIndex]);
    if GetAccountCount = 0 then
      CreateAccountFromRegistry;
    NewAccountNames := GetAccountNames;
    AccountComboBox.Items.Assign(NewAccountNames);
    NewAccountNames.Free;
    AccountComboBox.ItemIndex := 0;
    AccountComboBoxSelect(Self);
  end;
end;

procedure TMainForm.AccountRenameButtonClick(Sender: TObject);
var
  RenameAccountForm: TRenameAccountForm;
  NewName: String;
  NewAccountNames: TStrings;
begin
  if AccountComboBox.ItemIndex >= 0 then
  begin
    RenameAccountForm := TRenameAccountForm.Create(Self);
    RenameAccountForm.Input.Text := AccountComboBox.Items.Strings[AccountComboBox.ItemIndex];
    if RenameAccountForm.ShowModal = mrOk then
    begin
      NewName := RenameAccount(AccountComboBox.Items.Strings[AccountComboBox.ItemIndex], RenameAccountForm.Input.Text);
      if Length(NewName) > 0 then
      begin
        NewAccountNames := GetAccountNames;
        AccountComboBox.Items.Assign(NewAccountNames);
        NewAccountNames.Free;
        AccountComboBox.ItemIndex := AccountComboBox.Items.IndexOf(NewName);
        AccountComboBoxSelect(Self);
      end;
    end;
    RenameAccountForm.Free;
  end;
end;

procedure TMainForm.ResolutionApplyButtonClick(Sender: TObject);
begin
  //SetHudShift(MainForm.in_game.Text);  // wird am Ende gemacht, falls Hudshift nicht existiert (erstmaliges runterladen)
  SetFLResolution(Utf8ToAnsi(ResolutionComboBox.Items.Strings[ResolutionComboBox.ItemIndex]));
end;

function FindFreelancer: Boolean;
begin
  Result := False;
  while True do
    with MainForm do
      if GamePathDialog.Execute then
      begin
        if FileExists(GamePathDialog.FileName + '/EXE/Freelancer.ini') then
        begin
          GamePathInput.Text := GamePathDialog.FileName;
          GamePathDialog.InitialDir := GamePathDialog.FileName;
          Result := True;
          break;
        end
        else
          ShowError('No valid Freelancer installation found!');
      end
      else
        break;
end;

procedure TMainForm.GamePathButtonClick(Sender: TObject);
begin
  FindFreelancer;
end;

procedure StartFreelancer;
var
  Process: TProcess;
begin
  PatchFreelancerMoving3dTargetView(MainForm.Moving3dTargetViewCheckBox.Checked);

  Process := TProcess.Create(nil);
  try
    with Process do
    begin
      Executable := MainForm.GamePathInput.Text + '/EXE/Freelancer.exe';
      if MainForm.WindowedCheckBox.Checked then
        Parameters.Add('-w');
      if Length(MainForm.ServerInput.Text) <> 0 then
      begin
        Parameters.Add('-s' + MainForm.ServerInput.Text);
        Parameters.Add('-I' + MainForm.ServerInput.Text);
      end;
      Options := [poNewProcessGroup];
      Execute;
    end;
  finally
    Process.Free;
  end;
end;

procedure StartUpdate; inline;
begin
  with MainForm do
    if Length(GamePathInput.Text) = 0 then
    begin
      if not FindFreelancer then
        Exit;
    end
    else
    if not FileExists(GamePathInput.Text + '/EXE/Freelancer.ini') then
    begin
      ShowError('Could not find Freelancer at ' + GamePathInput.Text);
      if not FindFreelancer then
        Exit;
    end;
  UpdateThread := TUpdateThread.Create(True);
  UpdateThreadExisting := True;
  UpdateThread.FreeOnTerminate := True;
  UpdateThread.Start;
end;

procedure StopUpdate; inline;
begin
  UpdateThread.Terminate;
  KillConnection;
end;

procedure TMainForm.GameTimerTimer(Sender: TObject);
begin
  if UpdateCompleted and AutoStartGameCheckBox.Checked then
  begin
    StartFreelancer;
    Close;
  end;
  GameTimer.Enabled := False;
end;

procedure TMainForm.UpdateButtonClick(Sender: TObject);
begin
  case UpdateButtonState of
    0: StartUpdate;
    1:
    begin
      UpdateButton.Enabled := False;
      UpdateInfoLabel.Caption := 'Update will be aborted!';
      StopUpdate;
    end;
    2:
    begin
      StartFreelancer;
      Close;
    end;
  end;
end;

procedure TMainForm.UpdateTimerTimer(Sender: TObject);
begin
  CriticalSection.Enter;
  try
    UpdateInfoLabel.Caption := UpdateInfoText;
    ProgressLabel.Caption := ProgressText;
    UpdateButton.Caption := UpdateButtonText;
    //if (not ChangeLogLabel.Visible) and (Length(ChangeLogUrl) > 0) then
    //begin
    //  ChangeLogLabel.Caption := ChangeLogText;
    //  ChangeLogLabel.Visible := True;
    //end;
    if UpdateCompleted then
      GameTimer.Enabled := True;
  finally
    CriticalSection.Leave;
  end;
end;

procedure TMainForm.InitTimerTimer(Sender: TObject);
begin
  InitTimer.Free;
  MainForm.Enabled := False;

  LoadConfig;

  if Length(GamePathInput.Text) = 0 then
  begin
    if not FindFreelancer then
      AutoUpdateCheckBox.Checked := False;
  end
  else
  if not FileExists(GamePathInput.Text + '/EXE/Freelancer.ini') then
  begin
    ShowError('Could not find Freelancer at ' + GamePathInput.Text);
    if not FindFreelancer then
      AutoUpdateCheckBox.Checked := False;
  end
  else
    GamePathDialog.InitialDir := GamePathInput.Text;

  if AutoUpdateCheckBox.Checked then
    UpdateButtonClick(Sender);

  //if GetFLResolution(ResolutionComboBox.Items) then
  //  ResolutionTabSheet.Enabled := True;

  MainForm.Enabled := True;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  AccountNames: TStrings;
  CurrentAccountName: String;
begin
  ProgressLabel.Caption := '';
  UpdateInfoText := UpdateInfoLabel.Caption;
  ProgressText := ProgressLabel.Caption;
  UpdateButtonText := UpdateButton.Caption;
  UpdateCompleted := False;
  UpdateButtonState := 0;

  CriticalSection := TCriticalSection.Create;

  ReadAccountsFromFile;
  CurrentAccountName := FindCurrentAccountNameFromRegistry;
  // Current Account not found in database
  if Length(CurrentAccountName) = 0 then
  begin
    CurrentAccountName := CreateAccountFromRegistry;
    // The registry is empty, Freelancer was perhaps freshly installed
    if Length(CurrentAccountName) = 0 then
    begin
      AccountNames := GetAccountNames;
      // If the database does have at least one account saved
      if AccountNames.Count > 0 then
      begin
        CurrentAccountName := AccountNames.Strings[0];
        SetAccount(AccountNames.Strings[0]);
      end
      // Create entirely new account and set it
      else
      begin
        CurrentAccountName := 'Standard';
        CreateAccount(CurrentAccountName);
      end;
      AccountNames.Free;
    end;
  end;
  AccountNames := GetAccountNames;
  AccountComboBox.Items.Assign(AccountNames);
  AccountNames.Free;
  AccountComboBox.ItemIndex := AccountComboBox.Items.IndexOf(CurrentAccountName);
  AccountComboBoxSelect(Self);
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if UpdateThreadExisting then
  begin
    StopUpdate;
    UpdateThread.Terminate;
    UpdateThread.WaitFor;
  end;
  UpdateTimer.Enabled := False;
  CriticalSection.Free;
  //if ((Length(GamePathInput.Text) > 0) and ResolutionTabSheet.Enabled) and GetFLResolution(ResolutionComboBox.Items) then
  //  SetHudShift(MainForm.in_game.Text);
  SaveConfig;
  WriteAccountsToFile;
end;

procedure TMainForm.ChangeLogLabelClick(Sender: TObject);
begin
  OpenURL(ChangeLogUrl);
end;

procedure TMainForm.LogoImageClick(Sender: TObject);
begin
  OpenURL('http://flsr.erikszeug.de');
end;

end.
