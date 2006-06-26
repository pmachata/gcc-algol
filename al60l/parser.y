%{
#include "ast.h"
#include "logger.h"
#include "lexer.h"
#include "parser.h"

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>

typedef struct struct_parser_rep_t
{
  lexer_t * lexer;
  logger_t * log;
  int manage;
  statement_t * result;
} parser_rep_t;

static logger_t * global_logger = NULL;

// call lexer
static int
yylex (void * arg, void * _parser)
{
  parser_rep_t * parser = _parser;
  *(int*)arg = 15;
  lexer_next_tok (parser->lexer);
  return lexer_get_tok_kind (parser->lexer);
}

// report error
static void
yyerror (const char *str)
{
  log_printf (global_logger, ll_error, "yyerror: %s", str);
}

#define YYPARSE_PARAM _parser
#define YYLEX_PARAM _parser



/// $0 in statement parts is used for referring to containing block of
/// given statement.  This auxiliary macro is used to do copying of
/// containing block to $0 for rules that require it.
///
/// Note: The assertion: If it's not a statement at all, odds are good
/// it'll fail due to sigsegv, or due to internal assertion in stmt_kind.
#define COPY_BLOCK(tgt,src) \
    assert (stmt_kind (src) == stmt_block); tgt = src

%}

%union {
  int un_i;
  float un_f;
  statement_t * stmt;
}

%pure-parser

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

%type <stmt> Program
%type <stmt> DummyStatement
%type <stmt> Block
%type <stmt> BasicStatement
%type <stmt> CompoundStatement
%type <stmt> UnconditionalStatement
%type <stmt> Statement
%type <stmt> StatementList

%%

Program:
  CompoundStatement EOFTOK
    {
      parser_rep_t * parser = _parser;
      log_printf (parser->log, ll_debug, "Program -> CompoundStatement");
      parser->result = $<stmt>1;
      YYACCEPT;
    }


CompoundStatement:
  LabelList Block
    {
      parser_rep_t * parser = _parser;
      log_printf (parser->log, ll_debug, "CompoundStatement -> LabelList Block");
      // @@@TODO: add block to $0
      $$ = $2;
    }

LabelList:
  /*epsilon*/
    {
      parser_rep_t * parser = _parser;
      log_printf (parser->log, ll_debug, "LabelList -> <eps>");
    }
  |
  Label LabelList
    {
      parser_rep_t * parser = _parser;
      log_printf (parser->log, ll_debug, "LabelList -> Label LabelList");
    }

Label:
  LabelIdentifier SEPCOLON
    {
      parser_rep_t * parser = _parser;
      log_printf (parser->log, ll_debug, "Label -> LabelIdentifier SEPCOLON");
    }

LabelIdentifier:
  IDENTIFIER
    {
      parser_rep_t * parser = _parser;
      log_printf (parser->log, ll_debug, "LabelIdentifier -> IDENTIFIER");
    }
  |
  LITINTEGER
    {
      parser_rep_t * parser = _parser;
      log_printf (parser->log, ll_debug, "LabelIdentifier -> LITINTEGER");
    }

Block:
  KWBEGIN {$<stmt>$ = new_stmt_block ();} StatementList KWEND
    {
      //allow DeclarationList after KWBEGIN
      parser_rep_t * parser = _parser;
      log_printf (parser->log, ll_debug, "Block -> KWBEGIN DeclarationList StatementList KWEND");
      $$ = $<stmt>2;
    }

StatementList:
  //SEPSEMICOLON StatementList {COPY_BLOCK($<stmt>$, $<stmt>0);} Statement
  StatementList SEPSEMICOLON {COPY_BLOCK($<stmt>$, $<stmt>0);} Statement
    {
      parser_rep_t * parser = _parser;
      log_printf (parser->log, ll_debug, "StatementList -> StatementList SEPSEMICOLON Statement");
      stmt_block_add_statement ($<stmt>3, $4);
      $$ = $<stmt>3;
    }
  |
  Statement
    {
      parser_rep_t * parser = _parser;
      log_printf (parser->log, ll_debug, "StatementList -> Statement");
      stmt_block_add_statement ($<stmt>0, $1);
      $$ = $<stmt>0;
    }

Statement:
  UnconditionalStatement
    {
      parser_rep_t * parser = _parser;
      log_printf (parser->log, ll_debug, "Statement -> UnconditionalStatement");
      $$ = $1;
    }

UnconditionalStatement:
  CompoundStatement
    {
      parser_rep_t * parser = _parser;
      log_printf (parser->log, ll_debug, "UnconditionalStatement -> CompoundStatement");
      $$ = $1;
    }
  |
  BasicStatement
    {
      parser_rep_t * parser = _parser;
      log_printf (parser->log, ll_debug, "UnconditionalStatement -> BasicStatement");
      $$ = $1;
    }

BasicStatement:
  LabelList {COPY_BLOCK($<stmt>$, $<stmt>0);} DummyStatement
    {
      parser_rep_t * parser = _parser;
      log_printf (parser->log, ll_debug, "BasicStatement -> LabelList DummyStatement");
      // TODO: use $0 to access symtab and add labels pointing to $2 there
      $$ = $3;
    }

DummyStatement:
  /*epsilon*/
    {
      parser_rep_t * parser = _parser;
      log_printf (parser->log, ll_debug, "DummyStatement -> <eps>");
      $$ = new_stmt_dummy ();
    }

%%

parser_t *
new_parser (lexer_t * lexer, int manage)
{
  parser_rep_t * ret = malloc (sizeof (parser_rep_t));

  ret->lexer = lexer;
  ret->manage = manage;
  ret->result = NULL;

  ret->log = new_logger ("parser");
  if (ret->log == NULL)
    {
      perror ("malloc");
      free (ret);
      return NULL;
    }
  log_set_filter (ret->log, ll_debug);

  // Allocate system-wide logger.
  // @@@ NOTE: this should be moved to logger or somewhere, it'll be
  // useful for system-wide messages and errors across the code.
  if (global_logger == NULL)
    {
      global_logger = new_logger ("proper");
      if (global_logger == NULL)
	{
	  delete_parser ((void*)ret);
	  return NULL;
	}
      log_set_filter (global_logger, ll_debug);
    }

  return (void*)ret;
}

void
delete_parser (parser_t * _parser)
{
  parser_rep_t * parser = (void*)_parser;
  delete_logger (parser->log);
  free (parser);
}

statement_t *
parser_parse (parser_t * _parser)
{
  parser_rep_t * parser = (void*)_parser;
  log_printf (parser->log, ll_debug, "parser_parse: let's rock");

  int ret = yyparse (parser);
  if (ret == 0)
    return parser->result;
  else
    return NULL;
}
