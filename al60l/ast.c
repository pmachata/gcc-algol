/*
 * Copyright (c) 2006 Petr Machata
 * All rights reserved.
 */

#ifndef SELF_TEST

#include "ast.h"
#include <assert.h>
#include <string.h>

static char const* private_statement_signature = "statement";

/// Represents dummy statements.
struct stmt_dummy_rep
{
  // no data necessary
};

/// Represents goto statements.
struct stmt_goto_rep
{
  /// The expression that identifies where to jump.
  /// Constrains: type==LABEL or type==INTEGER
  struct struct_expression_rep_t * target;
};

/// Represents assignments.
struct stmt_assign_rep
{
  /// Linked list of targets.
  /// Constrains: kind==VARIABLE or kind==ARRAY_ACCESS,
  /// exists X such that type(t)==X for each t in targets.
  struct struct_expression_rep_t * targets;

  /// RHS of expression.
  struct struct_expression_rep_t * value;
};

/// Represents blocks and compounds.
struct stmt_block_rep
{
  /// Pointer to head of linked list of statements.
  struct struct_statement_rep_t * stmts;

  /// Pointer to tail of linked list of statements, for easy
  /// appending.
  struct struct_statement_rep_t * tail;

  /// Symbol table. @@@TODO
  symbol_t const** symtab;
  symbol_t const** symtab_ptr;
};

/// Represents conditionals.
struct stmt_cond_rep
{
  /// Condition guard.  Constrains: type==BOOLEAN.
  struct struct_expression_rep_t * cond;

  /// If-true statement.
  struct struct_statement_rep_t * iftrue;

  /// If-false statement, may be NULL.
  struct struct_statement_rep_t * iffalse;
};

/// Represents for loops.
struct stmt_for_rep
{
  /// Expression describing the iterating variable.
  /// Constrains: kind==VARIABLE.  In particular, kind!=ARRAY_ACCESS.
  struct struct_expression_rep_t * variable;

  /// Linked list of iteration elements, i.e. the rules for obtaining
  /// next iteration values.
  struct struct_for_elmt_rep_t * values;

  /// The body that's executed at each iteration.
  struct struct_statement_rep_t * body;
};

/// Represents procedure calls.
struct stmt_call_rep
{
  /// The expression that represents funcall.
  /// Constrains: kind==CALL
  struct struct_expression_rep_t * target;
};

/// Internal representation for statements.
typedef struct struct_statement_rep_t
{
  char const* signature;

  /// For linking statements into a list.
  struct struct_statement_rep_t * link;

  /// Link to a block that contains this stmt.
  struct struct_statement_rep_t * parent;

  /// Statement kind.
  statement_kind_t kind;

  /// Per-kind data.
  union {
    struct stmt_dummy_rep dummy;
    struct stmt_goto_rep gotor;
    struct stmt_assign_rep assign;
    struct stmt_block_rep block;
    struct stmt_cond_rep cond;
    struct stmt_for_rep forr;
    struct stmt_call_rep call;
  } u;
} statement_rep_t;



/// Boilerplate code for creating new statement.
static statement_rep_t *
private_new_stmt (statement_kind_t kind)
{
  statement_rep_t * ret = malloc (sizeof (statement_rep_t));
  if (ret == NULL)
    return NULL;

  ret->signature = private_statement_signature;
  ret->kind = kind;
  ret->link = NULL;
  ret->parent = NULL;
  return ret;
}

/// Boilerplate code for creating blocks and containers.
static statement_t *
private_new_stmt_block_or_container (statement_kind_t kind)
{
  statement_rep_t * ret = private_new_stmt (kind);
  if (ret == NULL)
    return NULL;

  ret->u.block.stmts = NULL;
  ret->u.block.tail = NULL;
  ret->u.block.symtab = malloc (1024); ///@FIXME
  ret->u.block.symtab_ptr = ret->u.block.symtab;

  return (void*)ret;
}



statement_t *
new_stmt_dummy (void)
{
  return (statement_t *)private_new_stmt (stmt_dummy);
}

statement_t *
new_stmt_container (void)
{
  return private_new_stmt_block_or_container (stmt_container);
}

statement_t *
new_stmt_block (void)
{
  return private_new_stmt_block_or_container (stmt_block);
}

statement_kind_t
stmt_kind (statement_t * _stmt)
{
  assert (_stmt != NULL);
  statement_rep_t * stmt = (void*)_stmt;

  return stmt->kind;
}

char const*
private_indent (char const* padding)
{
  char const* padding2 = padding - 1;
  if (*padding2 != ' ')
    padding2++;
  return padding2;
}


// forward...
static void private_stmt_dump (statement_rep_t * stmt, FILE * ofile, char const* padding);


// ------------------------------------
//   DUMMY STMT
// ------------------------------------

void
private_delete_stmt_dummy (statement_rep_t * stmt)
{
  // nothing to do...
}

void
private_dump_stmt_dummy (statement_rep_t * stmt, FILE * ofile, char const* padding)
{
  fprintf (ofile, "%s'comment' dummy ;\n", padding);
}


// ------------------------------------
//   CONTAINER STMT
// ------------------------------------

void
private_delete_stmt_container (statement_rep_t * stmt)
{
  // delete statements
  for (statement_rep_t * it = stmt->u.block.stmts;
       it != NULL; it = it->link)
    delete_stmt ((statement_t*)it);
}

void
private_dump_stmt_container (statement_rep_t * stmt, FILE * ofile, char const* padding)
{
  for (statement_rep_t * it = stmt->u.block.stmts;
       it != NULL; it = it->link)
    private_stmt_dump (it, ofile, padding);
}


// ------------------------------------
//   BLOCK STMT
// ------------------------------------

void
private_delete_stmt_block (statement_rep_t * stmt)
{
  private_delete_stmt_container (stmt);
}

void
private_dump_stmt_block (statement_rep_t * stmt, FILE * ofile, char const* padding)
{
  fprintf (ofile, "%s'begin'\n", padding);
  private_dump_stmt_container (stmt, ofile, private_indent (padding));
  fprintf (ofile, "%s'end';\n", padding);
}


// ------------------------------------
//   GOTO STMT
// ------------------------------------

void
private_delete_stmt_goto (statement_rep_t * stmt)
{
  assert (!"NYI!");
}

void
private_dump_stmt_goto (statement_rep_t * stmt, FILE * ofile, char const* padding)
{
  fprintf (ofile, "%s'goto' x; @@@FIXME\n", padding);
}


// ------------------------------------
//   ASSIGN STMT
// ------------------------------------

void
private_delete_stmt_assign (statement_rep_t * stmt)
{
  assert (!"NYI!");
}

void
private_dump_stmt_assign (statement_rep_t * stmt, FILE * ofile, char const* padding)
{
  fprintf (ofile, "%sx := y; @@@FIXME\n", padding);
}


// ------------------------------------
//   COND STMT
// ------------------------------------

void
private_delete_stmt_cond (statement_rep_t * stmt)
{
  assert (!"NYI!");
}

void
private_dump_stmt_cond (statement_rep_t * stmt, FILE * ofile, char const* padding)
{
  fprintf (ofile, "%s'if' x 'then'\n", padding);
  private_stmt_dump (stmt->u.cond.iftrue, ofile, private_indent (padding));
  if (stmt->u.cond.iffalse != NULL)
    {
      fprintf (ofile, "%s'else'\n", padding);
      private_stmt_dump (stmt->u.cond.iffalse, ofile, private_indent (padding));
    }
}


// ------------------------------------
//   FOR STMT
// ------------------------------------

void
private_delete_stmt_for (statement_rep_t * stmt)
{
  assert (!"NYI!");
}

void
private_dump_stmt_for (statement_rep_t * stmt, FILE * ofile, char const* padding)
{
  fprintf (ofile, "%s'for' @@@TODO 'do'\n", padding);
  private_stmt_dump (stmt->u.forr.body, ofile, private_indent (padding));
}


// ------------------------------------
//   CALL STMT
// ------------------------------------

void
private_delete_stmt_call (statement_rep_t * stmt)
{
  assert (!"NYI!");
}

void
private_dump_stmt_call (statement_rep_t * stmt, FILE * ofile, char const* padding)
{
  fprintf (ofile, "%ssomething (); @@@TODO\n", padding);
}


void(*stmt_dtor_tab[])(statement_rep_t*) = {
  private_delete_stmt_dummy,
  private_delete_stmt_container,
  private_delete_stmt_block,
  private_delete_stmt_goto,
  private_delete_stmt_assign,
  private_delete_stmt_cond,
  private_delete_stmt_for,
  private_delete_stmt_call,
};

void (*stmt_dump_tab[])(statement_rep_t * stmt, FILE * ofile, char const* ) = {
  private_dump_stmt_dummy,
  private_dump_stmt_container,
  private_dump_stmt_block,
  private_dump_stmt_goto,
  private_dump_stmt_assign,
  private_dump_stmt_cond,
  private_dump_stmt_for,
  private_dump_stmt_call,
};

char const* stmt_label_tab[] = {
  "dummy",
  "container",
  "block",
  "goto",
  "assign",
  "cond",
  "for",
  "call",
};

void
delete_stmt (statement_t * _stmt)
{
  assert (_stmt != NULL);
  statement_rep_t * stmt = (void*)_stmt;
  assert (stmt->kind >= 0 && stmt->kind < statement_kind_t_count);
  stmt_dtor_tab[stmt->kind] (stmt);
  free (stmt);
}

void
private_stmt_dump (statement_rep_t * stmt, FILE * ofile, char const* padding)
{
  assert (stmt->kind >= 0 && stmt->kind < statement_kind_t_count);
  stmt_dump_tab[stmt->kind] (stmt, ofile, padding);
}

void
stmt_dump (statement_t * _stmt, FILE * ofile)
{
  assert (_stmt != NULL);
  statement_rep_t * stmt = (void*)_stmt;
  char * padding = malloc (1024);
  memset (padding, ' ', 1024);
  padding[1023] = 0;
  padding[0] = '*';
  private_stmt_dump (stmt, ofile, padding+1023);
  free (padding);
}

char const*
stmt_label (statement_t * _stmt)
{
  assert (_stmt != NULL);
  statement_rep_t * stmt = (void*)_stmt;
  assert (stmt->kind >= 0 && stmt->kind < statement_kind_t_count);
  return stmt_label_tab[stmt->kind];
}

statement_t *
statement (void * ptr)
{
  if (((statement_rep_t*)ptr)->signature == private_statement_signature)
    return ptr;
  else
    return NULL;
}


void
stmt_block_add_statement (statement_t * _block, statement_t * _stmt)
{
  assert (_block != NULL);
  assert (_stmt != NULL);
  statement_rep_t * block = (void*)_block;
  statement_rep_t * stmt = (void*)_stmt;

  assert (block->kind == stmt_block
	  || block->kind == stmt_container);
  assert (stmt->parent == NULL);
  assert (stmt->link == NULL);

  if (block->u.block.tail == NULL)
    {
      assert (block->u.block.stmts == NULL);
      block->u.block.tail = block->u.block.stmts = stmt;
    }
  else
    {
      block->u.block.tail->link = stmt;
      block->u.block.tail = stmt;
    }
  stmt->parent = block;
}

void
stmt_block_add_decl (statement_t * _block, symbol_t const* symbol)
{
  assert (_block != NULL);
  statement_rep_t * block = (void*)_block;
  assert (block->kind == stmt_block
	  || block->kind == stmt_container);

  assert (*block->u.block.symtab_ptr = NULL);
  *block->u.block.symtab_ptr++ = symbol;
}



#else /* SELF_TEST */

#include "ast.h"
#include <stdio.h>
#include <assert.h>

int
main (void)
{
  statement_t * stmt = new_stmt_dummy ();
  assert (stmt_kind (stmt) == stmt_dummy);
  assert (statement (stmt));

  statement_t * block = new_stmt_block ();
  assert (stmt_kind (block) == stmt_block);
  assert (statement (block));
  stmt_block_add_statement (block, stmt);

  statement_t * block2 = new_stmt_block ();
  assert (statement (block2));
  stmt_block_add_statement (block2, block);

  stmt_dump (block2, stdout);

  delete_stmt (block);

  printf ("All passed.\n");
  return 0;
}

#endif
