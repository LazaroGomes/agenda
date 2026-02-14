# Agenda em Lazarus + SQLite

Projeto simples de agenda com cadastro de contatos e tipos de contato.

## Estrutura

- `agenda.lpr`: ponto de entrada da aplicação Lazarus.
- `uMain.pas`: formulário principal (GUI) com navegação para os cadastros.
- `uContatosForm.pas`: formulário de contatos (CRUD de contatos).
- `uTiposForm.pas`: formulário de tipos de contato (CRUD de tipos).
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

- Formulário principal (GUI) para acesso aos módulos.
- Formulário de contatos com cadastro, edição e exclusão.
- Formulário de tipos com cadastro, edição e exclusão.
- Criação automática das tabelas e tipos padrão caso não existam.
