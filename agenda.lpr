program Agenda;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, uMain;

begin
  RequireDerivedFormResource := False;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
