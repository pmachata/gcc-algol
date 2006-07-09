%{
#include "ast-tab.h"
#include "logger.h"
#include "lexer.h"
#include "parser.h"
#include "slist.h"

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

static char const* private_parser_signature = "parser";

typedef struct struct_parser_rep_t
{
  char const* signature;
  lexer_t * lexer;
  logger_t * log;
  int manage;
  statement * result;
  ast_state_t * ast;
} parser_rep_t;

// call lexer
static int
yylex (YYSTYPE * arg, parser_rep_t * parser)
{
  lexer_next_tok (parser->lexer);
  return lexer_get_tok_kind (parser->lexer);
}

// report error
static void
yyerror (parser_rep_t * parser, const char *str)
{
  log_printf (parser->log, ll_error, "yyerror: %s", str);
}

void private_dump_log_labels (parser_rep_t * parser, slist_t * slist);

/// Take all labels in `slist', make symbols from them, bind these
/// symbols with command `target', and add them to symbol table of
/// containing block `cont'.
void private_add_labels_to_symtab (parser_rep_t * parser, stmt_container * cont,
				   slist_t * slist, statement * target);

/// $0 in statement parts is used for referring to containing block of
/// given statement.  This auxiliary macro is used to do copying of
/// containing block to $0 for rules that require it.  Typically, you
/// would call it like this: COPY_BLOCK($<stmt>$, $<stmt>0);
///
/// Note: The assertion: If it's not a statement at all, odds are good
/// it'll fail due to sigsegv, or due to internal assertion in stmt_kind.
#define COPY_BLOCK(tgt,src) \
  assert (ast_isa (src, stmt_block)); tgt = src
%}

%union {
  int un_i;
  float un_f;
  estring_t * un_s;

  slist_t * lst;

  statement * stmt;
  stmt_container * cont;
  label * lbl;
}

%pure-parser
%error-verbose
 //%token-table
 //%locations
%parse-param {parser_rep_t * parser}
%lex-param {parser_rep_t * parser}

%token EOFTOK
%token KWTRUE
%token KWFALSE

%token AOPADD
%token AOPSUB
%token AOPMUL
%token AOPRDIV
%token AOPIDIV
%token AOPPOW

%token ROPLT
%token ROPLTE
%token ROPEQ
%token ROPGT
%token ROPGTE
%token ROPNEQ

%token LOPEQ
%token LOPIMP
%token LOPOR
%token LOPAND
%token LOPNOT

%token KWGOTO
%token KWIF
%token KWTHEN
%token KWELSE
%token KWFOR
%token KWDO

%token SEPCOMMA
%token SEPPERIOD
%token SEP10E
%token SEPCOLON
%token SEPSEMICOLON
%token SEPASSIGN
%token KWSTEP
%token KWUNTIL
%token KWWHILE
%token KWCOMMENT

%token SEPLPAREN
%token SEPRPAREN
%token SEPLBRACK
%token SEPRBRACK
%token SEPLQUOT
%token SEPRQUOT
%token KWBEGIN
%token KWEND

%token KWOWN
%token KWBOOLEAN
%token KWINTEGER
%token KWREAL
%token KWARRAY
%token KWSWITCH
%token KWPROCEDURE

%token KWSTRING
%token KWLABEL
%token KWVALUE

%token IDENTIFIER

%token LITFLOAT
%token LITINTEGER
%token LITSTRING

%left LOPEQ
%left LOPIMP
%left LOPOR
%left LOPAND
%left LOPNOT
%left ROPGT ROPGTE ROPLT ROPLTE ROPEQ ROPNEQ
%left AOPADD AOPSUB
%left AOPMUL AOPRDIV AOPIDIV
%left AOPPOW

%type <cont> Program
%type <stmt> DummyStatement
%type <stmt> Block
%type <stmt> BasicStatement
%type <stmt> UnconditionalStatement
%type <stmt> Statement
%type <stmt> StatementList
%type <lst> LabelList
%type <lbl> Label
%type <lbl> LabelIdentifier

%%

Program:
  {$<cont>$ = ast_as (stmt_container, stmt_container_create (parser->ast));}
  LabelList Block SEPSEMICOLON EOFTOK
    {
      log_printf (parser->log, ll_debug, "Program -> CompoundStatement");
      private_dump_log_labels (parser, $2);
      private_add_labels_to_symtab (parser, $<cont>1, $2, $3);
      container_add_stmt ($<cont>1, $<stmt>3);
      parser->result = ast_as (statement, $<cont>1);
      YYACCEPT;
    }

LabelList:
  /*epsilon*/
    {
      log_printf (parser->log, ll_debug, "LabelList -> <eps>");
      $$ = new_slist ();
    }
  |
  Label LabelList
    {
      log_printf (parser->log, ll_debug, "LabelList -> Label LabelList");
      slist_pushfront ($2, $1);
      $$ = $2;
    }

Label:
  LabelIdentifier SEPCOLON
    {
      log_printf (parser->log, ll_debug, "Label -> LabelIdentifier SEPCOLON");
      $$ = $1;
    }

LabelIdentifier:
  IDENTIFIER
    {
      log_printf (parser->log, ll_debug, "LabelIdentifier -> IDENTIFIER");
      estring_t * lit = clone_estring (lexer_get_tok_literal (parser->lexer));
      $$ = label_id_create (parser->ast, lit);
    }
  |
  LITINTEGER
    {
      log_printf (parser->log, ll_debug, "LabelIdentifier -> LITINTEGER");
      long lit = lexer_get_tok_integer (parser->lexer);
      $$ = label_int_create (parser->ast, lit);
    }

Block:
  KWBEGIN {$<stmt>$ = stmt_block_create (parser->ast);} StatementList KWEND
    {
      //allow DeclarationList after KWBEGIN
      log_printf (parser->log, ll_debug, "Block -> KWBEGIN DeclarationList StatementList KWEND");
      $$ = $<stmt>2;
    }

StatementList:
  //SEPSEMICOLON StatementList {COPY_BLOCK($<stmt>$, $<stmt>0);} Statement
  StatementList SEPSEMICOLON {COPY_BLOCK($<stmt>$, $<stmt>0);} Statement
    {
      log_printf (parser->log, ll_debug, "StatementList -> StatementList SEPSEMICOLON Statement");
      container_add_stmt (ast_as (stmt_container, $<stmt>3), $4);
      $$ = $<stmt>3;
    }
  |
  Statement
    {
      log_printf (parser->log, ll_debug, "StatementList -> Statement");
      container_add_stmt (ast_as (stmt_container, $<stmt>0), $1);
      $$ = $<stmt>0;
    }

Statement:
  UnconditionalStatement
    {
      log_printf (parser->log, ll_debug, "Statement -> UnconditionalStatement");
      $$ = $1;
    }

UnconditionalStatement:
  LabelList Block
    {
      log_printf (parser->log, ll_debug, "UnconditionalStatement -> CompoundStatement");
      private_dump_log_labels (parser, $1);
      private_add_labels_to_symtab (parser, $<cont>0, $1, $2);
      $$ = $2;
    }
  |
  BasicStatement
    {
      log_printf (parser->log, ll_debug, "UnconditionalStatement -> BasicStatement");
      $$ = $1;
    }

BasicStatement:
  LabelList DummyStatement
    {
      log_printf (parser->log, ll_debug, "BasicStatement -> LabelList DummyStatement");
      private_dump_log_labels (parser, $1);
      private_add_labels_to_symtab (parser, $<cont>0, $1, $2);
      $$ = $2;
    }

DummyStatement:
  /*epsilon*/
    {
      log_printf (parser->log, ll_debug, "DummyStatement -> <eps>");
      $$ = stmt_dummy_create (parser->ast);
    }

%%

parser_t *
new_parser (lexer_t * lexer, int manage)
{
  assert (lexer != NULL);
  parser_rep_t * ret = malloc (sizeof (parser_rep_t));

  ret->signature = private_parser_signature;
  ret->lexer = lexer;
  ret->manage = manage;
  ret->result = NULL;

  ret->ast = new_ast_state ();
  if (ret->ast == NULL)
    {
      perror ("malloc");
      free (ret);
      return NULL;
    }

  ret->log = new_logger ("parser");
  if (ret->log == NULL)
    {
      perror ("malloc");
      delete_ast_state (ret->ast);
      free (ret);
      return NULL;
    }
  log_set_filter (ret->log, ll_debug);

  return (void*)ret;
}

void
delete_parser (parser_t * _parser)
{
  if (_parser != NULL)
    {
      parser_rep_t * parser = (void*)_parser;
      delete_logger (parser->log);
      free (parser);
    }
}

parser_t *
parser (void * ptr)
{
  if (((parser_rep_t*)ptr)->signature == private_parser_signature)
    return ptr;
  else
    return NULL;
}

statement *
parser_parse (parser_t * _parser)
{
  assert (_parser != NULL);
  parser_rep_t * parser = (void*)_parser;
  log_printf (parser->log, ll_debug, "parser_parse: let's rock");

  int ret = yyparse (parser);
  if (ret == 0)
    return parser->result;
  else
    return NULL;
}

logger_t const*
parser_log (parser_t * _parser)
{
  assert (_parser != NULL);
  parser_rep_t * parser = (void*)_parser;
  return parser->log;
}

void
private_dump_log_labels (parser_rep_t * parser, slist_t * slist)
{
  slist_it_t * it = slist_iter (slist);
  estring_t * lstr = new_estring ();
  if (slist_it_has (it))
    log_printf (parser->log, ll_debug, " this block has following labels:");
  for (; slist_it_has (it); slist_it_next (it))
    {
      label_str (slist_it_get (it), lstr);
      log_printf (parser->log, ll_debug, "  %s", estr_cstr (lstr));
    }
  delete_estring (lstr);
  delete_slist_it (it);
}

void
private_add_labels_to_symtab (parser_rep_t * parser, stmt_container * cont,
			      slist_t * slist, statement * target)
{
  slist_it_t * it = slist_iter (slist);
  for (; slist_it_has (it); slist_it_next (it))
    {
      label * lbl = slist_it_get (it);
      symbol * sym = symbol_create (parser->ast, lbl);
      container_add_symbol (cont, sym);
      symbol_set_stmt (sym, target);
      symbol_set_type (sym, type_label ());
    }
  delete_slist_it (it);
}
