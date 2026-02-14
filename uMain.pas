unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Variants, Forms, Controls, Graphics, Dialogs, StdCtrls, DBGrids, DB,
  DBCtrls, uData;

type

  { TfrmMain }

  TfrmMain = class(TForm)
  private
    FData: TAgendaData;
    FContatosDS: TDataSource;

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
    FEditando: Boolean;

    procedure CarregarRegistroAtual;
    procedure LimparFormulario;
    function TipoSelecionadoID: Integer;
    procedure OnNovoClick(Sender: TObject);
    procedure OnSalvarClick(Sender: TObject);
    procedure OnExcluirClick(Sender: TObject);
    procedure OnGridCellClick(Column: TColumn);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

constructor TfrmMain.Create(AOwner: TComponent);
var
  DbPath: string;
begin
  inherited CreateNew(AOwner, 1);
  Caption := 'Agenda (SQLite)';
  Width := 820;
  Height := 500;
  Position := poScreenCenter;

  FData := TAgendaData.Create(Self);
  FContatosDS := TDataSource.Create(Self);

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
  cmbTipo.Width := 100;

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
  grdContatos.Width := 784;
  grdContatos.Height := 330;
  grdContatos.OnCellClick := @OnGridCellClick;

  DbPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'agenda.db';
  FData.Conectar(DbPath);
  FData.AbrirTipos;
  FData.AbrirContatos;

  FContatosDS.DataSet := FData.ContatosQuery;
  grdContatos.DataSource := FContatosDS;

  cmbTipo.ListSource := TDataSource.Create(Self);
  cmbTipo.ListSource.DataSet := FData.TiposQuery;
  cmbTipo.ListField := 'descricao';
  cmbTipo.KeyField := 'id';

  FEditando := False;
  LimparFormulario;
end;

destructor TfrmMain.Destroy;
begin
  inherited Destroy;
end;

procedure TfrmMain.CarregarRegistroAtual;
begin
  if FData.ContatosQuery.IsEmpty then
    Exit;

  edtNome.Text := FData.ContatosQuery.FieldByName('nome').AsString;
  edtTelefone.Text := FData.ContatosQuery.FieldByName('telefone').AsString;
  edtEmail.Text := FData.ContatosQuery.FieldByName('email').AsString;
  cmbTipo.KeyValue := FData.ContatosQuery.FieldByName('tipo_id').AsInteger;
end;

procedure TfrmMain.LimparFormulario;
begin
  edtNome.Clear;
  edtTelefone.Clear;
  edtEmail.Clear;
  if not FData.TiposQuery.IsEmpty then
    cmbTipo.KeyValue := FData.TiposQuery.FieldByName('id').AsInteger
  else
    cmbTipo.KeyValue := Null;
end;

function TfrmMain.TipoSelecionadoID: Integer;
begin
  if VarIsNull(cmbTipo.KeyValue) then
    Exit(0);
  Result := cmbTipo.KeyValue;
end;

procedure TfrmMain.OnNovoClick(Sender: TObject);
begin
  FEditando := False;
  LimparFormulario;
end;

procedure TfrmMain.OnSalvarClick(Sender: TObject);
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

procedure TfrmMain.OnExcluirClick(Sender: TObject);
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

procedure TfrmMain.OnGridCellClick(Column: TColumn);
begin
  FEditando := True;
  CarregarRegistroAtual;
end;

end.
