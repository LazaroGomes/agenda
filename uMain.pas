unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, uData, uContatosForm, uTiposForm;

type
  { TfrmMain }

  TfrmMain = class(TForm)
  private
    FData: TAgendaData;
    btnContatos: TButton;
    btnTipos: TButton;
    lblTitulo: TLabel;

    procedure OnAbrirContatos(Sender: TObject);
    procedure OnAbrirTipos(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  frmMain: TfrmMain;

implementation

constructor TfrmMain.Create(AOwner: TComponent);
var
  DbPath: string;
begin
  inherited CreateNew(AOwner, 1);
  Caption := 'Agenda - GUI Principal';
  Width := 460;
  Height := 240;
  Position := poScreenCenter;

  FData := TAgendaData.Create(Self);
  DbPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'agenda.db';
  FData.Conectar(DbPath);

  lblTitulo := TLabel.Create(Self);
  lblTitulo.Parent := Self;
  lblTitulo.Caption := 'Selecione o formulário que deseja abrir';
  lblTitulo.Left := 90;
  lblTitulo.Top := 32;

  btnContatos := TButton.Create(Self);
  btnContatos.Parent := Self;
  btnContatos.Caption := 'Formulário de Contatos';
  btnContatos.Width := 170;
  btnContatos.Height := 36;
  btnContatos.Left := 48;
  btnContatos.Top := 92;
  btnContatos.OnClick := @OnAbrirContatos;

  btnTipos := TButton.Create(Self);
  btnTipos.Parent := Self;
  btnTipos.Caption := 'Formulário de Tipos';
  btnTipos.Width := 170;
  btnTipos.Height := 36;
  btnTipos.Left := 236;
  btnTipos.Top := 92;
  btnTipos.OnClick := @OnAbrirTipos;
end;

procedure TfrmMain.OnAbrirContatos(Sender: TObject);
var
  Frm: TfrmContatos;
begin
  Frm := TfrmContatos.CreateComData(Self, FData);
  try
    Frm.ShowModal;
  finally
    Frm.Free;
  end;
end;

procedure TfrmMain.OnAbrirTipos(Sender: TObject);
var
  Frm: TfrmTipos;
begin
  Frm := TfrmTipos.CreateComData(Self, FData);
  try
    Frm.ShowModal;
  finally
    Frm.Free;
  end;
end;

end.
