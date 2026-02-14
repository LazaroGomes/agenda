unit uData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, SQLite3Conn, SQLDB;

type
  { TAgendaData }

  TAgendaData = class(TComponent)
  private
    FConn: TSQLite3Connection;
    FTrans: TSQLTransaction;
    FContatosQuery: TSQLQuery;
    FTiposQuery: TSQLQuery;
    procedure CriarEstrutura;
    procedure PopularTiposPadrao;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Conectar(const ADatabasePath: string);
    procedure AbrirContatos;
    procedure AbrirTipos;
    procedure InserirContato(const ANome, ATelefone, AEmail: string; ATipoID: Integer);
    procedure AtualizarContato(AID: Integer; const ANome, ATelefone, AEmail: string; ATipoID: Integer);
    procedure ExcluirContato(AID: Integer);

    property ContatosQuery: TSQLQuery read FContatosQuery;
    property TiposQuery: TSQLQuery read FTiposQuery;
  end;

implementation

constructor TAgendaData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConn := TSQLite3Connection.Create(Self);
  FTrans := TSQLTransaction.Create(Self);
  FContatosQuery := TSQLQuery.Create(Self);
  FTiposQuery := TSQLQuery.Create(Self);

  FConn.Transaction := FTrans;
  FContatosQuery.DataBase := FConn;
  FContatosQuery.Transaction := FTrans;
  FTiposQuery.DataBase := FConn;
  FTiposQuery.Transaction := FTrans;
end;

destructor TAgendaData.Destroy;
begin
  if FConn.Connected then
    FConn.Close;
  inherited Destroy;
end;

procedure TAgendaData.Conectar(const ADatabasePath: string);
begin
  if FConn.Connected then
    Exit;

  FConn.DatabaseName := ADatabasePath;
  FConn.Connected := True;

  CriarEstrutura;
  PopularTiposPadrao;
end;

procedure TAgendaData.CriarEstrutura;
begin
  FConn.ExecuteDirect(
    'CREATE TABLE IF NOT EXISTS tipos (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  descricao TEXT NOT NULL UNIQUE' +
    ');'
  );

  FConn.ExecuteDirect(
    'CREATE TABLE IF NOT EXISTS contatos (' +
    '  id INTEGER PRIMARY KEY AUTOINCREMENT,' +
    '  nome TEXT NOT NULL,' +
    '  telefone TEXT,' +
    '  email TEXT,' +
    '  tipo_id INTEGER NOT NULL,' +
    '  FOREIGN KEY (tipo_id) REFERENCES tipos(id)' +
    ');'
  );

  FTrans.Commit;
end;

procedure TAgendaData.PopularTiposPadrao;
begin
  FConn.ExecuteDirect('INSERT OR IGNORE INTO tipos(descricao) VALUES (''Amigos'');');
  FConn.ExecuteDirect('INSERT OR IGNORE INTO tipos(descricao) VALUES (''Comercial'');');
  FConn.ExecuteDirect('INSERT OR IGNORE INTO tipos(descricao) VALUES (''Outros'');');
  FTrans.Commit;
end;

procedure TAgendaData.AbrirContatos;
begin
  if FContatosQuery.Active then
    FContatosQuery.Close;

  FContatosQuery.SQL.Text :=
    'SELECT c.id, c.nome, c.telefone, c.email, t.descricao AS tipo, c.tipo_id ' +
    'FROM contatos c ' +
    'INNER JOIN tipos t ON t.id = c.tipo_id ' +
    'ORDER BY c.nome';
  FContatosQuery.Open;
end;

procedure TAgendaData.AbrirTipos;
begin
  if FTiposQuery.Active then
    FTiposQuery.Close;

  FTiposQuery.SQL.Text := 'SELECT id, descricao FROM tipos ORDER BY descricao';
  FTiposQuery.Open;
end;

procedure TAgendaData.InserirContato(const ANome, ATelefone, AEmail: string; ATipoID: Integer);
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.DataBase := FConn;
    Q.Transaction := FTrans;
    Q.SQL.Text :=
      'INSERT INTO contatos (nome, telefone, email, tipo_id) ' +
      'VALUES (:nome, :telefone, :email, :tipo_id)';
    Q.Params.ParamByName('nome').AsString := Trim(ANome);
    Q.Params.ParamByName('telefone').AsString := Trim(ATelefone);
    Q.Params.ParamByName('email').AsString := Trim(AEmail);
    Q.Params.ParamByName('tipo_id').AsInteger := ATipoID;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
end;

procedure TAgendaData.AtualizarContato(AID: Integer; const ANome, ATelefone,
  AEmail: string; ATipoID: Integer);
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.DataBase := FConn;
    Q.Transaction := FTrans;
    Q.SQL.Text :=
      'UPDATE contatos ' +
      'SET nome = :nome, telefone = :telefone, email = :email, tipo_id = :tipo_id ' +
      'WHERE id = :id';
    Q.Params.ParamByName('id').AsInteger := AID;
    Q.Params.ParamByName('nome').AsString := Trim(ANome);
    Q.Params.ParamByName('telefone').AsString := Trim(ATelefone);
    Q.Params.ParamByName('email').AsString := Trim(AEmail);
    Q.Params.ParamByName('tipo_id').AsInteger := ATipoID;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
end;

procedure TAgendaData.ExcluirContato(AID: Integer);
var
  Q: TSQLQuery;
begin
  Q := TSQLQuery.Create(nil);
  try
    Q.DataBase := FConn;
    Q.Transaction := FTrans;
    Q.SQL.Text := 'DELETE FROM contatos WHERE id = :id';
    Q.Params.ParamByName('id').AsInteger := AID;
    Q.ExecSQL;
    FTrans.Commit;
  finally
    Q.Free;
  end;
end;

end.
