unit UExePatch;

{$mode objfpc}
{$H+}
{$IfNDef Debug}
  {$Inline On}
{$EndIf}

interface

procedure PatchFreelancerMoving3dTargetView(const Moving: Boolean);

implementation

uses
  Classes,
  SysUtils,
  UMainForm;

procedure PatchFreelancerMoving3dTargetView(const Moving: Boolean);
const
  TopDown3dView: array [0..2] of Byte = ($B1, $01, $90);
  Moving3dView: array [0..2] of Byte = ($0F, $95, $C1);
var
  FileStream: TFileStream;
  FilePath: String;
begin
  FilePath := MainForm.GamePathInput.Text + '/EXE/Freelancer.exe';
  if FileExists(FilePath) then
  begin
    FileStream := TFileStream.Create(MainForm.GamePathInput.Text + '\EXE\Freelancer.exe', fmOpenWrite);
    FileStream.Position := $E3D09;
    if Moving then
      FileStream.WriteBuffer(Moving3dView[0], 3)
    else
      FileStream.WriteBuffer(TopDown3dView[0], 3);
    FileStream.Free;
  end;
end;

end.
