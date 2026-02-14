# Agenda em Lazarus + SQLite

Projeto simples de agenda com cadastro de contatos e tipos de contato.

## Estrutura

- `agenda.lpr`: ponto de entrada da aplicação Lazarus.
- `uMain.pas`: formulário principal (interface e ações de CRUD).
- `uData.pas`: camada de acesso a dados com SQLite (`tipos` e `contatos`).
- `schema.sql`: script SQL com a estrutura do banco.

## Tabelas

### `tipos`
- `id` (PK)
- `descricao` (`Amigos`, `Comercial`, `Outros`)

### `contatos`
- `id` (PK)
- `nome`
- `telefone`
- `email`
- `tipo_id` (FK para `tipos.id`)

## Como usar no Lazarus IDE

1. Abra o Lazarus IDE.
2. Vá em **Project > Open Project...** e selecione `agenda.lpr`.
3. Compile e execute o projeto.
4. O arquivo `agenda.db` será criado automaticamente na pasta do executável.

## Funcionalidades

- Cadastro de contato (nome, telefone, e-mail e tipo).
- Edição de contato ao selecionar um item da grade.
- Exclusão de contato selecionado.
- Criação automática das tabelas caso não existam.
