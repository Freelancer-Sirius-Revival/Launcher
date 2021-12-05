unit UAccountConfig;

{$mode objfpc}
{$H+}
{$IfNDef Debug}
  {$Inline On}
{$EndIf}

interface

uses
  Classes,
  SysUtils;

procedure CreateAccount(const Name: String);
procedure ReadAccountsFromFile;
procedure WriteAccountsToFile;
function GetAccountNames: TStrings;
function HasAccountName(const Name: String): Boolean;
procedure SetAccount(const Name: String);
procedure DeleteAccount(const Name: String);
function FindCurrentAccountNameFromRegistry: String;
function CreateAccountFromRegistry: String;
function RenameAccount(const OldName, NewName: String): String;
function GetAccountCount: Int32;

implementation

uses
  Generics.Collections,
  Registry,
  IniFiles,
  md5;

const
  AccountsFileNamePrefix = 'Accounts';
  AccountsFileNameExtension = '.ini';
  AccountsFileName = AccountsFileNamePrefix + AccountsFileNameExtension;
  FreelancerRegistryKey = 'Software\Microsoft\Microsoft Games\Freelancer\1.0';
  MPAccountNameKey = 'MPAccountName';
  MPAccountNameSigKey = 'MPAccountNameSig';

type
  TAccount = record
    MPAccountName: String;
    MPAccountNameSig: String;
  end;
  TAccountDirectory = specialize TDictionary<String, TAccount>;

var
  Accounts: TAccountDirectory;

function GenerateMPAccountName: TAccount;
var
  GUID: TGuid;
  Hash: String;
  BlockIndex, SubIndex: Int32;
  HashBlocks: array [0..3] of String;
  TempHashBlock: String;
begin
  // Freelancer uses the MS Windows Product ID, then the User ID, and the current User Name and Time.
  // This generates some GUID
  CreateGUID(GUID);
  // Which then gets hashed to a 32 characters hash
  Hash := MD5Print(MD5String(GUID.ToString));
  // Now 4 blocks of 8 characters will be created
  for BlockIndex := 0 to 3 do
    HashBlocks[BlockIndex] := Hash.Substring(BlockIndex * 8, 8);
  // Joining the blocks with dashes makes the 35-character long MPAccountNameKey
  Result.MPAccountName := String.Join('-', HashBlocks);

  // For the MPAccountNameKey Signature, the MPAccountNameKey gets hashed
  Hash := MD5Print(MD5String(Result.MPAccountName));
  // Now 4 blocks of 8 characters will be created
  for BlockIndex := 0 to 3 do
  begin
    TempHashBlock := Hash.Substring(BlockIndex * 8, 8);
    HashBlocks[BlockIndex] := '';
    // Each block's digit pairs are reversed in the process
    for SubIndex := 3 downto 0 do
      HashBlocks[BlockIndex] += TempHashBlock.Substring(SubIndex * 2, 2);
  end;
  // Joining the blocks with dashes makes the 35-character long MPAccountNameKey Signature
  Result.MPAccountNameSig := String.Join('-', HashBlocks);
end;

function ReadAccountIdFromRegistry: TAccount;
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  Registry.RootKey := HKEY_CURRENT_USER;
  Registry.OpenKey(FreelancerRegistryKey, False);
  Result.MPAccountName := Registry.ReadString(MPAccountNameKey);
  Result.MPAccountNameSig := Registry.ReadString(MPAccountNameSigKey);
  Registry.CloseKey;
  Registry.Free;
end;

procedure WriteAccountIdToRegistry(const Account: TAccount);
var
  Registry: TRegistry;
begin
  Registry := TRegistry.Create;
  Registry.RootKey := HKEY_CURRENT_USER;
  Registry.OpenKey(FreelancerRegistryKey, True);
  Registry.WriteString(MPAccountNameKey, Account.MPAccountName);
  Registry.WriteString(MPAccountNameSigKey, Account.MPAccountNameSig);
  Registry.CloseKey;
  Registry.Free;
end;

procedure CreateAccount(const Name: String);
var
  Account: specialize TPair<String, TAccount>;
begin
  Account.Key := Name;
  Account.Value := GenerateMPAccountName;
  Accounts.Add(Account);
end;

function ClearNameOfIllegalSymbols(const Name: String): String;
const
  Symbols: array [0..3] of Char = ('[', ']', ';', '=');
var
  Index: Int32;
begin
  Result := Name;
  for Index := 0 to High(Symbols) do
    Result := Result.Replace(Symbols[Index], '', [rfReplaceAll, rfIgnoreCase]);
end;

function ReadAccounts(const FileName: String): TAccountDirectory;
var
  Ini: TMemIniFile;
  Sections: TStrings;
  Index: Int32;
  Account: specialize TPair<String, TAccount>;
begin
  Result := TAccountDirectory.Create;
  if FileExists(FileName) then
  begin
    Sections := TStringList.Create;
    Ini := TMemIniFile.Create(FileName, [ifoCaseSensitive, ifoStripComments, ifoStripInvalid, ifoStripQuotes]);
    Ini.ReadSections(Sections);
    for Index := 0 to Sections.Count - 1 do
    begin
      Account.Key := Sections.Strings[Index];
      Account.Value.MPAccountName := Ini.ReadString(Account.Key, MPAccountNameKey, '');
      Account.Value.MPAccountNameSig := Ini.ReadString(Account.Key, MPAccountNameSigKey, '');
      Result.Add(Account);
    end;
    Sections.Free;
    Ini.Free;
  end;
end;

procedure WriteAccounts(const AccountDictionary: TAccountDirectory; const FileName: String);
var
  Ini: TMemIniFile;
  Index: Int32;
  AccountsArray: array of specialize TPair<String, TAccount>;
  Name: String;
begin
  AccountsArray := AccountDictionary.ToArray;
  Ini := TMemIniFile.Create(FileName, [ifoCaseSensitive]);
  Ini.Clear;
  for Index := 0 to High(AccountsArray) do
  begin
    Name := ClearNameOfIllegalSymbols(AccountsArray[Index].Key);
    Ini.WriteString(Name, MPAccountNameKey, AccountsArray[Index].Value.MPAccountName);
    Ini.WriteString(Name, MPAccountNameSigKey, AccountsArray[Index].Value.MPAccountNameSig);
  end;
  AccountsArray := nil;
  Ini.Free;
end;

procedure ReadAccountsFromFile;
begin
  Accounts := ReadAccounts(AccountsFileName);
end;

function DidAccountsChange(const OldAccounts, NewAccounts: TAccountDirectory): Boolean;
var
  OldAccountsArray: array of specialize TPair<String, TAccount>;
  Index: Int32;
begin
  OldAccountsArray := OldAccounts.ToArray;
  for Index := 0 to High(OldAccountsArray) do
  begin
    if not NewAccounts.ContainsKey(OldAccountsArray[Index].Key) or not
      ((NewAccounts.Items[OldAccountsArray[Index].Key].MPAccountName = OldAccountsArray[Index].Value.MPAccountName) and
      (NewAccounts.Items[OldAccountsArray[Index].Key].MPAccountNameSig = OldAccountsArray[Index].Value.MPAccountNameSig))
    then
    begin
      OldAccountsArray := nil;
      Exit(True);
    end;
  end;
  OldAccountsArray := nil;
  Result := False;
end;

procedure WriteAccountsToFile;
var
  OldAccounts: TAccountDirectory;
begin
  OldAccounts := ReadAccounts(AccountsFileName);
  if DidAccountsChange(OldAccounts, Accounts) then
    WriteAccounts(OldAccounts, AccountsFileNamePrefix + '_Backup_' + FormatDateTime('yyyy-mm-dd_hh-nn-ss', Now) + AccountsFileNameExtension);
  OldAccounts.Free;
  WriteAccounts(Accounts, AccountsFileName);
  Accounts.Free;
end;

function GetAccountNames: TStrings;
var
  Index: Int32;
  AccountsArray: array of specialize TPair<String, TAccount>;
begin
  AccountsArray := Accounts.ToArray;
  Result := TStringList.Create;
  for Index := 0 to High(AccountsArray) do
    Result.Append(AccountsArray[Index].Key);
  AccountsArray := nil;
end;

function HasAccountName(const Name: String): Boolean;
begin
  Result := Accounts.ContainsKey(Name);
end;

procedure SetAccount(const Name: String);
begin
  if HasAccountName(Name) then
    WriteAccountIdToRegistry(Accounts.Items[Name]);
end;

procedure DeleteAccount(const Name: String);
begin
  Accounts.Remove(Name);
end;

function FindCurrentAccountNameFromRegistry: String;
var
  Account: TAccount;
  Index: Int32;
  AccountsArray: array of specialize TPair<String, TAccount>;
begin
  Account := ReadAccountIdFromRegistry;
  AccountsArray := Accounts.ToArray;
  for Index := 0 to High(AccountsArray) do
    if (AccountsArray[Index].Value.MPAccountName = Account.MPAccountName) and (AccountsArray[Index].Value.MPAccountNameSig = Account.MPAccountNameSig) then
      Exit(AccountsArray[Index].Key);
  Result := '';
end;

function CreateAccountFromRegistry: String;
var
  Account: specialize TPair<String, TAccount>;
begin     
  Account.Value := ReadAccountIdFromRegistry;
  if (Length(Account.Value.MPAccountName) = 0) or (Length(Account.Value.MPAccountNameSig) = 0) then
    Exit('');

  Result := 'Standard';
  if HasAccountName(Result) then
    Result := 'Standard ' + TimeToStr(Now);
  Account.Key := Result;
  Accounts.Add(Account);
end;

function RenameAccount(const OldName, NewName: String): String;
var
  Account: specialize TPair<String, TAccount>;
begin
  Result := ClearNameOfIllegalSymbols(NewName.Trim);
  if (Length(Result) > 0) and not HasAccountName(Result) then
  begin
    Account.Key := Result;
    Account.Value := Accounts.Items[OldName];
    DeleteAccount(OldName);
    Accounts.Add(Account);
  end;
end;

function GetAccountCount: Int32;
begin
  Result := Accounts.Count;
end;

end.
