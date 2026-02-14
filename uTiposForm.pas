unit uTiposForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls, DBGrids, DB, uData;

type
  { TfrmTipos }

  TfrmTipos = class(TForm)
  private
    FData: TAgendaData;
    FTiposDS: TDataSource;
    FEditando: Boolean;

    lblDescricao: TLabel;
    edtDescricao: TEdit;
    btnNovo: TButton;
    btnSalvar: TButton;
    btnExcluir: TButton;
    grdTipos: TDBGrid;

    procedure LimparFormulario;
    procedure CarregarRegistroAtual;
    procedure OnNovoClick(Sender: TObject);
    procedure OnSalvarClick(Sender: TObject);
    procedure OnExcluirClick(Sender: TObject);
    procedure OnGridCellClick(Column: TColumn);
  public
    OnTiposAlterados: TNotifyEvent;
    constructor CreateComData(AOwner: TComponent; AData: TAgendaData);
  end;

implementation

constructor TfrmTipos.CreateComData(AOwner: TComponent; AData: TAgendaData);
begin
  inherited CreateNew(AOwner, 1);
  Caption := 'Tipos de contato';
  Width := 600;
  Height := 460;
  Position := poScreenCenter;

  FData := AData;
  FTiposDS := TDataSource.Create(Self);

  lblDescricao := TLabel.Create(Self);
  lblDescricao.Parent := Self;
  lblDescricao.Caption := 'Descrição';
  lblDescricao.Left := 16;
  lblDescricao.Top := 16;

  edtDescricao := TEdit.Create(Self);
  edtDescricao.Parent := Self;
  edtDescricao.Left := 16;
  edtDescricao.Top := 36;
  edtDescricao.Width := 280;

  btnNovo := TButton.Create(Self);
  btnNovo.Parent := Self;
  btnNovo.Caption := 'Novo';
  btnNovo.Left := 16;
  btnNovo.Top := 76;
  btnNovo.OnClick := @OnNovoClick;

  btnSalvar := TButton.Create(Self);
  btnSalvar.Parent := Self;
  btnSalvar.Caption := 'Salvar';
  btnSalvar.Left := 100;
  btnSalvar.Top := 76;
  btnSalvar.OnClick := @OnSalvarClick;

  btnExcluir := TButton.Create(Self);
  btnExcluir.Parent := Self;
  btnExcluir.Caption := 'Excluir';
  btnExcluir.Left := 184;
  btnExcluir.Top := 76;
  btnExcluir.OnClick := @OnExcluirClick;

  grdTipos := TDBGrid.Create(Self);
  grdTipos.Parent := Self;
  grdTipos.Left := 16;
  grdTipos.Top := 116;
  grdTipos.Width := 550;
  grdTipos.Height := 300;
  grdTipos.OnCellClick := @OnGridCellClick;

  FData.AbrirTipos;
  FTiposDS.DataSet := FData.TiposQuery;
  grdTipos.DataSource := FTiposDS;

  FEditando := False;
  LimparFormulario;
end;

procedure TfrmTipos.LimparFormulario;
begin
  edtDescricao.Clear;
end;

procedure TfrmTipos.CarregarRegistroAtual;
begin
  if FData.TiposQuery.IsEmpty then
    Exit;

  edtDescricao.Text := FData.TiposQuery.FieldByName('descricao').AsString;
end;

procedure TfrmTipos.OnNovoClick(Sender: TObject);
begin
  FEditando := False;
  LimparFormulario;
end;

procedure TfrmTipos.OnSalvarClick(Sender: TObject);
var
  TipoID: Integer;
begin
  if Trim(edtDescricao.Text) = '' then
  begin
    MessageDlg('Validação', 'Informe a descrição do tipo.', mtWarning, [mbOK], 0);
    Exit;
  end;

  if not FData.TiposQuery.IsEmpty then
    TipoID := FData.TiposQuery.FieldByName('id').AsInteger
  else
    TipoID := 0;

  try
    if FEditando and (TipoID > 0) then
      FData.AtualizarTipo(TipoID, edtDescricao.Text)
    else
      FData.InserirTipo(edtDescricao.Text);
  except
    on E: Exception do
    begin
      MessageDlg('Erro', 'Não foi possível salvar o tipo: ' + E.Message, mtError, [mbOK], 0);
      Exit;
    end;
  end;

  FData.AbrirTipos;
  FEditando := False;
  LimparFormulario;
  if Assigned(OnTiposAlterados) then
    OnTiposAlterados(Self);
end;

procedure TfrmTipos.OnExcluirClick(Sender: TObject);
var
  TipoID: Integer;
begin
  if FData.TiposQuery.IsEmpty then
    Exit;

  TipoID := FData.TiposQuery.FieldByName('id').AsInteger;
  if MessageDlg('Confirmação', 'Excluir tipo selecionado?', mtConfirmation,
    [mbYes, mbNo], 0) <> mrYes then
    Exit;

  try
    FData.ExcluirTipo(TipoID);
    FData.AbrirTipos;
  except
    on E: Exception do
    begin
      MessageDlg('Erro',
        'Não foi possível excluir o tipo. Verifique se existem contatos vinculados.' +
        LineEnding + E.Message,
        mtError, [mbOK], 0);
      Exit;
    end;
  end;

  LimparFormulario;
  if Assigned(OnTiposAlterados) then
    OnTiposAlterados(Self);
end;

procedure TfrmTipos.OnGridCellClick(Column: TColumn);
begin
  FEditando := True;
  CarregarRegistroAtual;
end;

end.
