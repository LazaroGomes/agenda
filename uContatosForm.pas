unit uContatosForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Variants, Forms, Controls, Dialogs, StdCtrls, DBGrids, DB,
  DBCtrls, uData;

type
  { TfrmContatos }

  TfrmContatos = class(TForm)
  private
    FData: TAgendaData;
    FContatosDS: TDataSource;
    FTiposDS: TDataSource;
    FEditando: Boolean;

    lblNome: TLabel;
    lblTelefone: TLabel;
    lblEmail: TLabel;
    lblTipo: TLabel;

    edtNome: TEdit;
    edtTelefone: TEdit;
    edtEmail: TEdit;
    cmbTipo: TDBLookupComboBox;

    btnNovo: TButton;
    btnSalvar: TButton;
    btnExcluir: TButton;

    grdContatos: TDBGrid;

    procedure CarregarRegistroAtual;
    procedure LimparFormulario;
    function TipoSelecionadoID: Integer;
    procedure OnNovoClick(Sender: TObject);
    procedure OnSalvarClick(Sender: TObject);
    procedure OnExcluirClick(Sender: TObject);
    procedure OnGridCellClick(Column: TColumn);
  public
    constructor CreateComData(AOwner: TComponent; AData: TAgendaData);
    procedure AtualizarTipos;
  end;

implementation

constructor TfrmContatos.CreateComData(AOwner: TComponent; AData: TAgendaData);
begin
  inherited CreateNew(AOwner, 1);
  Caption := 'Contatos';
  Width := 860;
  Height := 520;
  Position := poScreenCenter;

  FData := AData;
  FContatosDS := TDataSource.Create(Self);
  FTiposDS := TDataSource.Create(Self);

  lblNome := TLabel.Create(Self);
  lblNome.Parent := Self;
  lblNome.Caption := 'Nome';
  lblNome.Left := 16;
  lblNome.Top := 16;

  edtNome := TEdit.Create(Self);
  edtNome.Parent := Self;
  edtNome.Left := 16;
  edtNome.Top := 36;
  edtNome.Width := 250;

  lblTelefone := TLabel.Create(Self);
  lblTelefone.Parent := Self;
  lblTelefone.Caption := 'Telefone';
  lblTelefone.Left := 280;
  lblTelefone.Top := 16;

  edtTelefone := TEdit.Create(Self);
  edtTelefone.Parent := Self;
  edtTelefone.Left := 280;
  edtTelefone.Top := 36;
  edtTelefone.Width := 150;

  lblEmail := TLabel.Create(Self);
  lblEmail.Parent := Self;
  lblEmail.Caption := 'E-mail';
  lblEmail.Left := 440;
  lblEmail.Top := 16;

  edtEmail := TEdit.Create(Self);
  edtEmail.Parent := Self;
  edtEmail.Left := 440;
  edtEmail.Top := 36;
  edtEmail.Width := 250;

  lblTipo := TLabel.Create(Self);
  lblTipo.Parent := Self;
  lblTipo.Caption := 'Tipo';
  lblTipo.Left := 700;
  lblTipo.Top := 16;

  cmbTipo := TDBLookupComboBox.Create(Self);
  cmbTipo.Parent := Self;
  cmbTipo.Left := 700;
  cmbTipo.Top := 36;
  cmbTipo.Width := 130;

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

  grdContatos := TDBGrid.Create(Self);
  grdContatos.Parent := Self;
  grdContatos.Left := 16;
  grdContatos.Top := 116;
  grdContatos.Width := 816;
  grdContatos.Height := 350;
  grdContatos.OnCellClick := @OnGridCellClick;

  FContatosDS.DataSet := FData.ContatosQuery;
  grdContatos.DataSource := FContatosDS;

  FTiposDS.DataSet := FData.TiposQuery;
  cmbTipo.ListSource := FTiposDS;
  cmbTipo.ListField := 'descricao';
  cmbTipo.KeyField := 'id';

  FEditando := False;
  AtualizarTipos;
  FData.AbrirContatos;
  LimparFormulario;
end;

procedure TfrmContatos.AtualizarTipos;
begin
  FData.AbrirTipos;
  if not FData.TiposQuery.IsEmpty then
    cmbTipo.KeyValue := FData.TiposQuery.FieldByName('id').AsInteger
  else
    cmbTipo.KeyValue := Null;
end;

procedure TfrmContatos.CarregarRegistroAtual;
begin
  if FData.ContatosQuery.IsEmpty then
    Exit;

  edtNome.Text := FData.ContatosQuery.FieldByName('nome').AsString;
  edtTelefone.Text := FData.ContatosQuery.FieldByName('telefone').AsString;
  edtEmail.Text := FData.ContatosQuery.FieldByName('email').AsString;
  cmbTipo.KeyValue := FData.ContatosQuery.FieldByName('tipo_id').AsInteger;
end;

procedure TfrmContatos.LimparFormulario;
begin
  edtNome.Clear;
  edtTelefone.Clear;
  edtEmail.Clear;
  if not FData.TiposQuery.IsEmpty then
    cmbTipo.KeyValue := FData.TiposQuery.FieldByName('id').AsInteger
  else
    cmbTipo.KeyValue := Null;
end;

function TfrmContatos.TipoSelecionadoID: Integer;
begin
  if VarIsNull(cmbTipo.KeyValue) then
    Exit(0);
  Result := cmbTipo.KeyValue;
end;

procedure TfrmContatos.OnNovoClick(Sender: TObject);
begin
  FEditando := False;
  LimparFormulario;
end;

procedure TfrmContatos.OnSalvarClick(Sender: TObject);
var
  ContatoID: Integer;
begin
  if Trim(edtNome.Text) = '' then
  begin
    MessageDlg('Validação', 'Informe o nome do contato.', mtWarning, [mbOK], 0);
    Exit;
  end;

  if TipoSelecionadoID = 0 then
  begin
    MessageDlg('Validação', 'Selecione um tipo para o contato.', mtWarning, [mbOK], 0);
    Exit;
  end;

  if not FData.ContatosQuery.IsEmpty then
    ContatoID := FData.ContatosQuery.FieldByName('id').AsInteger
  else
    ContatoID := 0;

  if FEditando and (ContatoID > 0) then
    FData.AtualizarContato(ContatoID, edtNome.Text, edtTelefone.Text, edtEmail.Text,
      TipoSelecionadoID)
  else
    FData.InserirContato(edtNome.Text, edtTelefone.Text, edtEmail.Text, TipoSelecionadoID);

  FData.AbrirContatos;
  FEditando := False;
  LimparFormulario;
end;

procedure TfrmContatos.OnExcluirClick(Sender: TObject);
var
  ContatoID: Integer;
begin
  if FData.ContatosQuery.IsEmpty then
    Exit;

  ContatoID := FData.ContatosQuery.FieldByName('id').AsInteger;
  if MessageDlg('Confirmação', 'Excluir contato selecionado?', mtConfirmation,
    [mbYes, mbNo], 0) = mrYes then
  begin
    FData.ExcluirContato(ContatoID);
    FData.AbrirContatos;
    LimparFormulario;
  end;
end;

procedure TfrmContatos.OnGridCellClick(Column: TColumn);
begin
  FEditando := True;
  CarregarRegistroAtual;
end;

end.
