unit URenameAccountForm;

{$mode objfpc}
{$H+}
{$IfNDef Debug}
  {$Inline On}
{$EndIf}

interface

uses
  Classes,
  Forms,
  Controls,
  ExtCtrls,
  StdCtrls, Dialogs;

type
  TRenameAccountForm = class(TForm)
    ApplyButton: TButton;
    Input: TLabeledEdit;
    procedure InputKeyPress(Sender: TObject; var Key: Char);
  end;

var
  RenameAccountForm: TRenameAccountForm;

implementation

{$R *.lfm}

uses
  LCLType;

procedure TRenameAccountForm.InputKeyPress(Sender: TObject; var Key: Char);
begin
  if Int32(Key) = VK_RETURN then
    ModalResult := mrOk;
end;

end.
