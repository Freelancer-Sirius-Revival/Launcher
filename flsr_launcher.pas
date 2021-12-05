program flsr_launcher;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads,
 {$ENDIF} {$ENDIF}
  Interfaces,
  Forms,
  UMainForm;

{$R *.res}

begin
  Application.Scaled := True;
  Application.Title := 'Freelancer: Sirius Revival Launcher';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
