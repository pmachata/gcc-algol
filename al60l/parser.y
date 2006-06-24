%{
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
  int result;
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
%}

%union {
  int un_i;
  float un_f;
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

%%

Program:
  CompoundStatement
    {
      parser_rep_t * parser = _parser;
      log_printf (parser->log, ll_debug, "Program -> CompoundStatement");
      YYACCEPT;
    }

CompoundStatement:
  LabelList Block
    {
      parser_rep_t * parser = _parser;
      log_printf (parser->log, ll_debug, "CompoundStatement -> LabelList Block");
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
  KWBEGIN StatementList KWEND
    {
      //allow DeclarationList after KWBEGIN
      parser_rep_t * parser = _parser;
      log_printf (parser->log, ll_debug, "Block -> KWBEGIN DeclarationList StatementList KWEND");
    }

StatementList:
  Statement SEPSEMICOLON StatementList
    {
      parser_rep_t * parser = _parser;
      log_printf (parser->log, ll_debug, "StatementList -> Statement SEPSEMICOLON StatementList");
    }
  |
  Statement
    {
      parser_rep_t * parser = _parser;
      log_printf (parser->log, ll_debug, "StatementList -> Statement");
    }

Statement:
  UnconditionalStatement
    {
      parser_rep_t * parser = _parser;
      log_printf (parser->log, ll_debug, "Statement -> UnconditionalStatement");
    }

UnconditionalStatement:
  CompoundStatement
    {
      parser_rep_t * parser = _parser;
      log_printf (parser->log, ll_debug, "UnconditionalStatement -> CompoundStatement");
    }
  |
  BasicStatement
    {
      parser_rep_t * parser = _parser;
      log_printf (parser->log, ll_debug, "UnconditionalStatement -> BasicStatement");
    }

BasicStatement:
  LabelList DummyStatement
    {
      parser_rep_t * parser = _parser;
      log_printf (parser->log, ll_debug, "BasicStatement -> LabelList DummyStatement");
    }

DummyStatement:
  /*epsilon*/
    {
      parser_rep_t * parser = _parser;
      log_printf (parser->log, ll_debug, "DummyStatement -> <eps>");
    }

/*
Main:
    Start
	{
	  parser_rep_t * parser = _parser;
	  $$ = $1 + log_printf (parser->log, ll_debug, "Main -> Start");
	  parser->result = $$;
	  YYACCEPT;
	}

Start:
    Start KWTRUE
	{
	  parser_rep_t * parser = _parser;
	  $$ = $1 + log_printf (parser->log, ll_debug, "Start -> Start 'true'");
	}
    |
    Start KWFALSE
	{
	  parser_rep_t * parser = _parser;
	  $$ = $1 + log_printf (parser->log, ll_debug, "Start -> Start 'false'");
	}
    |
    // empty
	{
	  parser_rep_t * parser = _parser;
	  $$ = log_printf (parser->log, ll_debug, "Start -> epsilon");
	}
*/

%%

parser_t *
new_parser (lexer_t * lexer, int manage)
{
  parser_rep_t * ret = malloc (sizeof (parser_rep_t));

  ret->lexer = lexer;
  ret->manage = manage;
  ret->result = -1;

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

int
parser_parse (parser_t * _parser)
{
  parser_rep_t * parser = (void*)_parser;
  log_printf (parser->log, ll_debug, "parser_parse: let's rock");

  int ret = yyparse (parser);
  if (ret == 0)
    return parser->result;
  else
    return -1;
}
