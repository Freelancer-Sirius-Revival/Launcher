unit UConfig;

{$mode objfpc}
{$H+}
{$IfNDef Debug}
  {$Inline On}
{$EndIf}

interface

var
  ModVersion: UInt32 = 0;

function LoadConfig: Boolean;
procedure SaveConfig;

implementation

uses
  UMainForm,
  Classes,
  SysUtils,
  IniFiles;

const
  ConfigFileName = 'FLSR-Launcher.ini';

  LauncherSection = 'Launcher';
  ModVersionKey = 'ModVersion';
  GamePathKey = 'GamePath';
  AutoUpdateKey = 'AutoUpdate';
  AutoStartKey = 'AutoStart';

  GameSection = 'Game';
  WindowModeKey = 'WindowMode';
  ServerIpKey = 'ServerIP';
  Moving3dTargetViewKey = 'Moving3dTargetView';

function LoadConfig: Boolean;
var
  Config: TMemIniFile;
begin
  Result := False;
  if FileExists(ConfigFileName) then
  begin
    Config := TMemIniFile.Create(ConfigFileName, [ifoStripInvalid]);
    MainForm.GamePathInput.Text := Config.ReadString(LauncherSection, GamePathKey, '');
    ModVersion := Config.ReadInteger(LauncherSection, ModVersionKey, 0);
    MainForm.AutoUpdateCheckBox.Checked := Config.ReadBool(LauncherSection, AutoUpdateKey, False);
    MainForm.AutoStartGameCheckBox.Checked := Config.ReadBool(LauncherSection, AutoStartKey, False);

    MainForm.WindowedCheckBox.Checked := Config.ReadBool(GameSection, WindowModeKey, False);
    MainForm.ServerInput.Text := Config.ReadString(GameSection, ServerIpKey, '');
    MainForm.Moving3dTargetViewCheckBox.Checked := Config.ReadBool(GameSection, Moving3dTargetViewKey, False);

    Config.Free;           
    Result := True;
  end;
end;

procedure SaveConfig;
var
  Config: TMemIniFile;
begin
  Config := TMemIniFile.Create(ConfigFileName, [ifoStripInvalid]);
  Config.WriteString(LauncherSection, GamePathKey, MainForm.GamePathInput.Text);
  Config.WriteInteger(LauncherSection, ModVersionKey, ModVersion);
  Config.WriteBool(LauncherSection, AutoUpdateKey, MainForm.AutoUpdateCheckBox.Checked);
  Config.WriteBool(LauncherSection, AutoStartKey, MainForm.AutoStartGameCheckBox.Checked);

  Config.WriteBool(GameSection, WindowModeKey, MainForm.WindowedCheckBox.Checked);
  Config.WriteString(GameSection, ServerIpKey, MainForm.ServerInput.Text);
  Config.WriteBool(GameSection, Moving3dTargetViewKey, MainForm.Moving3dTargetViewCheckBox.Checked);

  Config.Free;
end;

end.
