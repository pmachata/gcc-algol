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

  slist_t * blockstack;
  container * block;
  estring_t * tmp;
} parser_rep_t;

// call lexer
static int
yylex (YYSTYPE * arg, parser_rep_t * parser)
{
  return lexer_tok (parser->lexer, arg);
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
  // lexer portion
  estring_t * slit;
  long int ilit;
  double dlit;

  // parser portion
  int flag;
  slist_t * lst;
  statement * stmt;
  container * cont;
  label * lbl;
  symbol * sym;
  type * type;
  boundspair * bnds;
  expression * expr;
  estring_t * estr;
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

%token <slit> IDENTIFIER

%token <dlit> LITREAL
%token <ilit> LITINTEGER
%token <slit> LITSTRING

%token BOGUS

%left LOPEQ
%left LOPIMP
%left LOPOR
%left LOPAND
%left LOPNOT
%left ROPGT ROPGTE ROPLT ROPLTE ROPEQ ROPNEQ
%left AOPADD AOPSUB
%left AOPMUL AOPRDIV AOPIDIV
%left AOPPOW
%right PREC_UNARY_MINUS

%type <cont> Program
%type <lst> LabelList
%type <lbl> Label
%type <lbl> LabelIdentifier
%type <lbl> Identifier
//%type <sym> DeclaredIdentifier
%type <stmt> Block
//%type <> BlockDeclarationsList
//%type <> BlockDeclarations
%type <type> Type
%type <flag> OptOwn
%type <type> OptIntrinsicType
%type <type> IntrinsicType
%type <lst> IdentifierList
%type <lst> OptBoundsPairList
%type <lst> BoundsPairList
%type <bnds> BoundsPair
%type <expr> Expression
%type <expr> SimpleExpression
%type <expr> FunctionDesignator
%type <lst> ActualParamList
%type <estr> ParameterDelimiter
//%type <> StatementList
%type <stmt> Statement
%type <stmt> UnconditionalStatement
%type <stmt> BasicStatement
%type <lst> LeftPartList
%type <expr> LeftPart

%%

Program:
  {
    // dummy container, so that toplevel references to ->parent end up
    // somewhere sensible
    container * dummy = ast_as (container, stmt_toplev_create ());
    private_open_block (parser, dummy);

    // actual toplevel container
    container * c = ast_as (container, stmt_toplev_create ());
    c->parent = dummy;
    stmt_toplev_define_internals (ast_as (stmt_toplev, c));
    private_open_block (parser, c);
  }
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
  LabelList Label
    {
      log_printf (parser->log, ll_debug, "LabelList -> Label LabelList");
      slist_pushback ($1, $2);
      $$ = $1;
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
      estring_t * lit = $1;
      $$ = label_id_create (lit);
    }
  |
  LITINTEGER
    {
      log_printf (parser->log, ll_debug, "LabelIdentifier -> LITINTEGER");
      long lit = $1;
      $$ = label_int_create (lit);
    }

Identifier:
  IDENTIFIER
    {
      log_printf (parser->log, ll_debug, "Identifier -> IDENTIFIER");
      label * lbl = label_id_create ($1);
      $$ = lbl;
    }

/*
DeclaredIdentifier:
  Identifier
    {
      log_printf (parser->log, ll_debug, "Expression -> Identifier");
      symbol * sym = container_find_name_rec (parser->block, $1->lbl);
      if (sym == NULL)
	{
	  log_printf (parser->log, ll_error, "unknown symbol named `%s'",
		      estr_cstr (label_to_str ($1->lbl, parser->tmp)));
	  int was_there = container_add_symbol (parser->block, $1);
	  assert (!was_there);
	  sym = container_find_name (parser->block, $1->lbl);
	}
      assert (sym != NULL);
      $$ = sym;
    }
*/


Block:
  KWBEGIN
  { private_open_block (parser, ast_as (container, stmt_block_create ()));}
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
	  label * lbl = ast_as (label, slist_it_get (it));

	  // Classic algol only allows in this context:
	  // ['own'] {'integer'|'real'|'Boolean'} ['array']
	  type * rt = type_get_root ($1);
	  if (rt != type_int ()
	      && rt != type_real ()
	      && rt != type_bool ())
	    log_printf (parser->log, ll_error,
			"Type %s is invalid in this context.",
			estr_cstr (type_to_str ($1, parser->tmp)));

	  // If it was array, see if identifier has dimensions and
	  // mangle `rt' to reflect number of dimensions.  Note that
	  // 'own' doesn't influence matching.
	  if (types_match ($1, type_array_any ()))
	    {
	      if (lbl->arr_bd_list == NULL)
		log_printf (parser->log, ll_error,
			    "Identifier `%s' needs array bounds.",
			    estr_cstr (label_to_str (lbl, parser->tmp)));
	      else
		{
		  assert (!slist_empty (lbl->arr_bd_list));
		  slist_it_t * jt = slist_iter (lbl->arr_bd_list);
		  while (slist_it_has (jt))
		    {
		      rt = t_array_create (rt);
		      slist_it_next (jt);
		    }
		  delete_slist_it (jt);
		}
	    }

	  // If original type is `own', make also this type `own'.
	  if (ast_isa ($1, t_own))
	    rt = t_own_create (rt);

	  // Setup symbol and add to table.
	  symbol * sym = symbol_create (lbl);
	  symbol_set_type (sym, rt);
	  int conflict = container_add_symbol (parser->block, sym, sek_ordinary);
	  if (conflict)
	    log_printf (parser->log, ll_error,
			"Duplicate identifier `%s'.",
			estr_cstr (label_to_str (sym->lbl, parser->tmp)));
	}
      delete_slist_it (it);
    }

Type:
  OptOwn IntrinsicType
    {
      log_printf (parser->log, ll_debug, "Type -> OptOwn IntrinsicType");
      if ($1)
	$$ = t_own_create ($2);
      else
	$$ = $2;
    }
  |
  OptOwn OptIntrinsicType KWARRAY
    {
      log_printf (parser->log, ll_debug, "Type -> OptOwn IntrinsicType");
      // if no type declarator is given the type 'real' is understood
      type * t = t_array_create (($2 != NULL) ? $2 : type_real ());
      if ($1)
	t = t_own_create (t);
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
  Expression SEPCOLON Expression
    {
      log_printf (parser->log, ll_debug, "BoundsPair -> Expression SEPCOLON Expression");

      expression * lb = $1;
      expression * hb = $3;

      /* @TODO: if it's possible to evaluate expression in place, do
	 it, and check that the boundaries make sense: A:B, A<B.  Also
	 it will be necessary to generate runtime tests for that. */
      $$ = boundspair_create (lb, hb);
    }

Expression:
  SimpleExpression
    {
      log_printf (parser->log, ll_debug, "Expression -> SimpleExpression");
      $$ = $1;
    }
  |
  KWIF Expression KWTHEN SimpleExpression KWELSE Expression
    {
      log_printf (parser->log, ll_debug, "ArithmeticExpression -> KWIF BooleanExpression"
		  " KWTHEN SimpleArithmeticExpression ELSE SimpleArithmeticExpression");

/*
  @@@TODO: defer typechecking to later stage

      type * ct = expr_type ($2);
      if (!types_match (ct, type_bool ()))
	{
	  log_printf (parser->log, ll_error,
		      "invalid type `%s' of condition in conditional expression",
		      estr_cstr (type_to_str (ct, parser->tmp)));
	}
*/
      $$ = expr_if_create ($2, $4, $6);
    }

SimpleExpression:
  LITINTEGER
    {
      log_printf (parser->log, ll_debug, "SimpleExpression -> LITINTEGER");
      $$ = expr_int_create ($1);
    }
  |
  LITREAL
    {
      log_printf (parser->log, ll_debug, "SimpleExpression -> LITREAL");
      $$ = expr_real_create ($1);
    }
  |
  LITSTRING
    {
      log_printf (parser->log, ll_debug, "SimpleExpression -> LITSTRING");
      $$ = expr_string_create ($1);
    }
  |
  KWFALSE
    {
      log_printf (parser->log, ll_debug, "SimpleExpression -> KWFALSE");
      $$ = expr_bool_create (0);
    }
  |
  KWTRUE
    {
      log_printf (parser->log, ll_debug, "SimpleExpression -> KWTRUE");
      $$ = expr_bool_create (1);
    }
  |
  FunctionDesignator
    {
      log_printf (parser->log, ll_debug, "SimpleExpression -> FunctionDesignator");
      $$ = $1;
    }
  |
  SEPLPAREN Expression SEPRPAREN
    {
      log_printf (parser->log, ll_debug, "SimpleExpression -> SEPLPAREN Expression SEPRPAREN");
      $$ = $2;
    }
  |
  AOPSUB SimpleExpression %prec PREC_UNARY_MINUS
    {
      log_printf (parser->log, ll_debug, "SimpleExpression -> AOPSUB SimpleExpression");
      $$ = expr_uminus_create ($2); //@@@TODO: typecheck
    }
  |
  SimpleExpression AOPADD SimpleExpression
    {
      log_printf (parser->log, ll_debug, "SimpleExpression -> SimpleExpression AOPADD SimpleExpression");
      $$ = expr_aadd_create ($1, $3); //@@@TODO: typecheck
    }
  |
  SimpleExpression AOPSUB SimpleExpression
    {
      log_printf (parser->log, ll_debug, "SimpleExpression -> SimpleExpression AOPSUB SimpleExpression");
      $$ = expr_asub_create ($1, $3); //@@@TODO: typecheck
    }
  |
  SimpleExpression AOPMUL SimpleExpression
    {
      log_printf (parser->log, ll_debug, "SimpleExpression -> SimpleExpression AOPMUL SimpleExpression");
      $$ = expr_amul_create ($1, $3); //@@@TODO: typecheck
    }
  |
  SimpleExpression AOPIDIV SimpleExpression
    {
      log_printf (parser->log, ll_debug, "SimpleExpression -> SimpleExpression AOPIDIV SimpleExpression");
      $$ = expr_aidiv_create ($1, $3); //@@@TODO: typecheck
    }
  |
  SimpleExpression AOPRDIV SimpleExpression
    {
      log_printf (parser->log, ll_debug, "SimpleExpression -> SimpleExpression AOPRDIV SimpleExpression");
      $$ = expr_ardiv_create ($1, $3); //@@@TODO: typecheck
    }
  |
  SimpleExpression AOPPOW SimpleExpression
    {
      log_printf (parser->log, ll_debug, "SimpleExpression -> SimpleExpression AOPPOW SimpleExpression");
      $$ = expr_apow_create ($1, $3); //@@@TODO: typecheck
    }
  |
  SimpleExpression ROPNEQ SimpleExpression
    {
      log_printf (parser->log, ll_debug, "SimpleExpression -> SimpleExpression ROPNEQ SimpleExpression");
      $$ = expr_rneq_create ($1, $3); //@@@TODO: typecheck
    }
  |
  SimpleExpression ROPEQ SimpleExpression
    {
      log_printf (parser->log, ll_debug, "SimpleExpression -> SimpleExpression ROPEQ SimpleExpression");
      $$ = expr_req_create ($1, $3); //@@@TODO: typecheck
    }
  |
  SimpleExpression ROPLTE SimpleExpression
    {
      log_printf (parser->log, ll_debug, "SimpleExpression -> SimpleExpression ROPLTE SimpleExpression");
      $$ = expr_rlte_create ($1, $3); //@@@TODO: typecheck
    }
  |
  SimpleExpression ROPLT SimpleExpression
    {
      log_printf (parser->log, ll_debug, "SimpleExpression -> SimpleExpression ROPLT SimpleExpression");
      $$ = expr_rlt_create ($1, $3); //@@@TODO: typecheck
    }
  |
  SimpleExpression ROPGTE SimpleExpression
    {
      log_printf (parser->log, ll_debug, "SimpleExpression -> SimpleExpression ROPGTE SimpleExpression");
      $$ = expr_rgte_create ($1, $3); //@@@TODO: typecheck
    }
  |
  SimpleExpression ROPGT SimpleExpression
    {
      log_printf (parser->log, ll_debug, "SimpleExpression -> SimpleExpression ROPGT SimpleExpression");
      $$ = expr_rgt_create ($1, $3); //@@@TODO: typecheck
    }
  |
  SimpleExpression LOPEQ SimpleExpression
    {
      log_printf (parser->log, ll_debug, "SimpleExpression -> SimpleExpression LOPEQ SimpleExpression");
      $$ = expr_leq_create ($1, $3); //@@@TODO: typecheck
    }
  |
  SimpleExpression LOPIMP SimpleExpression
    {
      log_printf (parser->log, ll_debug, "SimpleExpression -> SimpleExpression LOPIMP SimpleExpression");
      $$ = expr_limp_create ($1, $3); //@@@TODO: typecheck
    }
  |
  SimpleExpression LOPAND SimpleExpression
    {
      log_printf (parser->log, ll_debug, "SimpleExpression -> SimpleExpression LOPAND SimpleExpression");
      $$ = expr_land_create ($1, $3); //@@@TODO: typecheck
    }
  |
  SimpleExpression LOPOR SimpleExpression
    {
      log_printf (parser->log, ll_debug, "SimpleExpression -> SimpleExpression LOPOR SimpleExpression");
      $$ = expr_lor_create ($1, $3); //@@@TODO: typecheck
    }
  |
  LOPNOT SimpleExpression
    {
      log_printf (parser->log, ll_debug, "SimpleExpression -> LOPNOT SimpleExpression");
      $$ = expr_not_create ($2); //@@@TODO: typecheck
    }

FunctionDesignator:
  Identifier SEPLPAREN ActualParamList SEPRPAREN
    {
      $$ = expr_call_create ($1, $3);
    }
  |
  Identifier
    {
      // note: identifier node may denote funcall without parameters.  The
      // translation from expr_idref to expr_call is done during semantic
      // analysis, where we can decide what type given id is.
      $$ = expr_idref_create ($1);
    }

ActualParamList:
  Expression
    {
      $$ = new_slist ();
      slist_pushback ($$, $1);
    }
  |
  ActualParamList ParameterDelimiter Expression
    {
      if ($2 != NULL)
	{
	  // @TODO: delimiters are ignored as per RRA60, but we want
	  // to check match with formals
	}

      slist_pushback ($1, $3);
    }

ParameterDelimiter:
  SEPCOMMA
    {
      $$ = NULL;
    }
  |
  SEPRPAREN IDENTIFIER SEPCOLON SEPLPAREN
    {
      $$ = $2;
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
      log_printf (parser->log, ll_debug, "UnconditionalStatement -> LabelList Block");
      private_dump_log_labels (parser, $1);
      private_add_labels_to_symtab (parser, parser->block, $1, $2);
      $$ = $2;
    }
  |
  LabelList BasicStatement
    {
      log_printf (parser->log, ll_debug, "UnconditionalStatement -> BasicStatement");
      private_add_labels_to_symtab (parser, parser->block, $1, $2);
      $$ = $2;
    }

BasicStatement:
  /* eps */
    {
      log_printf (parser->log, ll_debug, "BasicStatement -> LabelList");
      $$ = stmt_dummy_create ();
    }
  |
  LeftPartList Expression
    {
      log_printf (parser->log, ll_debug, "BasicStatement -> LabelList LeftPartList Expression");

      $$ = stmt_assign_create ();
      slist_it_t * it = slist_iter ($1);
      for (; slist_it_has (it); slist_it_next (it))
	stmt_assign_add_lhs (ast_as (stmt_assign, $$), slist_it_get (it));
      delete_slist_it (it);
      stmt_assign_set_rhs (ast_as (stmt_assign, $$), $2);
    }
  |
  FunctionDesignator
    {
      $$ = stmt_call_create ($1);
    }

LeftPartList:
  LeftPart
    {
      $$ = new_slist ();
      slist_pushback ($$, $1);
    }
  |
  LeftPartList LeftPart
    {
      slist_pushback ($1, $2);
      $$ = $1;
    }

LeftPart:
  Expression SEPASSIGN
    {
      $$ = $1;
    }

%%

parser_t *
new_parser (lexer_t * lexer, int manage)
{
  assert (lexer != NULL);
  parser_rep_t * ret = calloc (1, sizeof (parser_rep_t));
  if (ret == NULL)
    return NULL;

  jmp_buf buf;
  if (setjmp (buf) == 0)
    {
      ret->signature = private_parser_signature;
      ret->lexer = lexer;
      ret->manage = manage;
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
      label_to_str (slist_it_get (it), lstr);
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
      symbol * sym = symbol_create (lbl);
      container_add_symbol (cont, sym, sek_ordinary);
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
