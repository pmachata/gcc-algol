%{
#include "ast-tab.h"
#include "logger.h"
#include "lexer.h"
#include "parser.h"
#include "slist.h"
#include "util.h"

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

static char const* private_parser_signature = "parser";

typedef struct struct_parser_rep_t
{
  char const* signature;
  lexer_t * lexer;
  logger_t * log;
  int manage;
  statement * result;
  ast_state_t * ast;

  slist_t * blockstack;
  container * block;
  estring_t * tmp;
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
void private_add_labels_to_symtab (parser_rep_t * parser, container * cont,
				   slist_t * slist, statement * target);

void private_open_block (parser_rep_t * parser, container * cont);
container * private_close_block (parser_rep_t * parser);

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
  int flag;
  slist_t * lst;
  statement * stmt;
  container * cont;
  label * lbl;
  symbol * sym;
  type_t const* type;
  boundspair * bnds;
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
%type <lst> LabelList
%type <lbl> Label
%type <lbl> LabelIdentifier
%type <sym> Identifier
%type <type> IntrinsicType
%type <type> OptIntrinsicType
%type <type> Type
%type <lst> IdentifierList
%type <flag> OptOwn
%type <bnds> BoundsPair
%type <lst> BoundsPairList
%type <lst> OptBoundsPairList
%type <flag> ArithExpression /*oh well, hack...*/

%%

Program:
  {private_open_block (parser, ast_as (container, stmt_toplev_create (parser->ast)));}
  LabelList Block SEPSEMICOLON EOFTOK
    {
      log_printf (parser->log, ll_debug, "Program -> CompoundStatement");
      private_dump_log_labels (parser, $2);
      private_add_labels_to_symtab (parser, parser->block, $2, $3);
      container_add_stmt (parser->block, $3);
      parser->result = ast_as (statement, private_close_block (parser));
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

Identifier:
  IDENTIFIER
    {
      log_printf (parser->log, ll_debug, "Identifier -> IDENTIFIER");
      estring_t * lit = lexer_get_tok_literal (parser->lexer);
      label * lbl = label_id_create (parser->ast, clone_estring (lit));
      symbol * sym = symbol_create (parser->ast, lbl);
      $$ = sym;
    }


Block:
  KWBEGIN
  { private_open_block (parser, ast_as (container, stmt_block_create (parser->ast)));}
  BlockDeclarationsList StatementList
  KWEND
    {
      log_printf (parser->log, ll_debug, "Block -> KWBEGIN BlockDeclarationList StatementList KWEND");
      $$ = ast_as (statement, private_close_block (parser));
    }

BlockDeclarationsList:
  /*epsilon*/
    {
      log_printf (parser->log, ll_debug, "BlockDeclarationList -> <eps>");
    }
  |
  BlockDeclarations SEPSEMICOLON BlockDeclarationsList
    {
      log_printf (parser->log, ll_debug, "BlockDeclarationList -> BlockDeclaration BlockDeclarationList");
    }

BlockDeclarations:
  Type IdentifierList
    {
      slist_it_t * it;
      for (it = slist_iter ($2); slist_it_has (it); slist_it_next (it))
	{
	  symbol * sym = ast_as (symbol, slist_it_get (it));

	  // Classic algol only allows in this context:
	  // ['own'] {'integer'|'real'|'Boolean'} ['array']
	  type_t const* rt = type_get_root ($1);
	  if (rt != type_int ()
	      && rt != type_real ()
	      && rt != type_bool ())
	    log_printf (parser->log, ll_error,
			"Type %s is invalid in this context.",
			estr_cstr (type_str ($1, parser->tmp)));

	  // If it was array, see if identifier has dimensions and
	  // mangle `rt' to reflect number of dimensions.  Note that
	  // 'own' doesn't influence matching.
	  if (types_match ($1, type_array (type_any ())))
	    {
	      if (sym->arr_bd_list == NULL)
		log_printf (parser->log, ll_error,
			    "Identifier `%s' needs array bounds.",
			    estr_cstr (label_str (sym->lbl, parser->tmp)));
	      else
		{
		  assert (!slist_empty (sym->arr_bd_list));
		  slist_it_t * jt = slist_iter (sym->arr_bd_list);
		  while (slist_it_has (jt))
		    {
		      rt = type_array (rt);
		      slist_it_next (jt);
		    }
		  delete_slist_it (jt);
		}
	    }

	  // If original type is `own', make also this type `own'.
	  if (type_is_own ($1))
	    rt = type_own (rt);

	  // Setup symbol and add to table.
	  symbol_set_type (sym, rt);
	  int conflict = container_add_symbol (parser->block, sym);
	  if (conflict)
	    log_printf (parser->log, ll_error,
			"Duplicate identifier `%s'.",
			estr_cstr (label_str (sym->lbl, parser->tmp)));
	}
      delete_slist_it (it);
    }

Type:
  OptOwn IntrinsicType
    {
      log_printf (parser->log, ll_debug, "Type -> OptOwn IntrinsicType");
      if ($1)
	$$ = type_own ($2);
      else
	$$ = $2;
    }
  |
  OptOwn OptIntrinsicType KWARRAY
    {
      log_printf (parser->log, ll_debug, "Type -> OptOwn IntrinsicType");
      // if no type declarator is given the type 'real' is understood
      type_t const* t = type_array (($2 != NULL) ? $2 : type_real ());
      if ($1)
	t = type_own (t);
      $$ = t;
    }

OptOwn:
  /*eps*/ { $$ = 0; }
  |
  KWOWN   { $$ = 1; }

OptIntrinsicType:
  /* epsilon */
    {
      log_printf (parser->log, ll_debug, "OptIntrinsicType -> epsilon");
      $$ = NULL;
    }
  |
  IntrinsicType
    {
      log_printf (parser->log, ll_debug, "OptIntrinsicType -> IntrinsicType");
      $$ = $1;
    }

IntrinsicType:
  KWBOOLEAN
    {
      log_printf (parser->log, ll_debug, "IntrinsicType -> KWBOOLEAN");
      $$ = type_bool ();
    }
  |
  KWINTEGER
    {
      log_printf (parser->log, ll_debug, "IntrinsicType -> KWINTEGER");
      $$ = type_int ();
    }
  |
  KWREAL
    {
      log_printf (parser->log, ll_debug, "IntrinsicType -> KWREAL");
      $$ = type_real ();
    }
  |
  KWSTRING
    {
      log_printf (parser->log, ll_debug, "IntrinsicType -> KWSTRING");
      // @@TODO; note, this is invalid for local variables
      $$ = type_string ();
    }

IdentifierList:
  Identifier OptBoundsPairList
    {
      log_printf (parser->log, ll_debug, "IdentifierList -> Identifier OptBoundsPairList");
      $$ = new_slist ();
      $1->arr_bd_list = $2;
      slist_pushfront ($$, $1);
    }
  |
  IdentifierList SEPCOMMA Identifier OptBoundsPairList
    {
      log_printf (parser->log, ll_debug,
		  "IdentifierList -> IdentifierList SEPCOMMA Identifier OptBoundsPairList");
      $3->arr_bd_list = $4;
      slist_pushfront ($1, $3);
      $$ = $1;
    }

OptBoundsPairList:
  /* epsilon */
    {
      log_printf (parser->log, ll_debug, "OptBoundsPairList -> epsilon");
      $$ = NULL;
    }
  |
  SEPLBRACK BoundsPairList SEPRBRACK
    {
      log_printf (parser->log, ll_debug, "OptBoundsPairList -> SEPRBRACK BoundsPairList SEPRBRACK");
      $$ = $2;
    }

BoundsPairList:
  BoundsPair
    {
      log_printf (parser->log, ll_debug, "BoundsPairList -> BoundsPair");
      $$ = new_slist ();
      slist_pushback ($$, $1);
    }
  |
  BoundsPairList SEPCOMMA BoundsPair
    {
      log_printf (parser->log, ll_debug, "BoundsPairList -> BoundsPairList SEPCOMMA BoundsPair");
      slist_pushback ($1, $3);
      $$ = $1;
    }

BoundsPair:
  ArithExpression SEPCOLON ArithExpression
    {
      log_printf (parser->log, ll_debug, "BoundsPair -> LITINTEGER SEPCOLON LITINTEGER");
      $$ = boundspair_create (parser->ast, $1, $3);
    }

ArithExpression:
  LITINTEGER
    {
      $$ = lexer_get_tok_integer (parser->lexer);
    }

StatementList:
  StatementList SEPSEMICOLON Statement
    {
      log_printf (parser->log, ll_debug, "StatementList -> StatementList SEPSEMICOLON Statement");
      container_add_stmt (parser->block, $3);
    }
  |
  Statement
    {
      log_printf (parser->log, ll_debug, "StatementList -> Statement");
      container_add_stmt (parser->block, $1);
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
      private_add_labels_to_symtab (parser, parser->block, $1, $2);
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
      private_add_labels_to_symtab (parser, parser->block, $1, $2);
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
  memset (ret, 0, sizeof (parser_rep_t));

  jmp_buf buf;
  if (setjmp (buf) == 0)
    {
      ret->signature = private_parser_signature;
      ret->lexer = lexer;
      ret->manage = manage;
      guard_ptr (buf, 1, ret->ast = new_ast_state ());
      guard_ptr (buf, 1, ret->log = new_logger ("parser"));
      log_set_filter (ret->log, ll_debug);
      guard_ptr (buf, 1, ret->blockstack = new_slist ());
      guard_ptr (buf, 1, ret->tmp = new_estring ());
      return (void*)ret;
    }
  else
    {
      delete_parser ((parser_t*)ret);
      return NULL;
    }
}

void
delete_parser (parser_t * _parser)
{
  if (_parser != NULL)
    {
      parser_rep_t * parser = (void*)_parser;
      delete_ast_state (parser->ast);
      delete_logger (parser->log);
      delete_slist (parser->blockstack);
      delete_estring (parser->tmp);
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
private_add_labels_to_symtab (parser_rep_t * parser, container * cont,
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

void
private_open_block (parser_rep_t * parser, container * cont)
{
  assert (cont != NULL);
  slist_pushfront (parser->blockstack, parser->block);
  parser->block = cont;
}

container *
private_close_block (parser_rep_t * parser)
{
  assert (!slist_empty (parser->blockstack));
  container * cont = parser->block;
  parser->block = slist_popfront (parser->blockstack);
  return cont;
}


/*
 * Local Variables:
 * mode: c
 * c-syntactic-indentation: nil
 * End:
 */
