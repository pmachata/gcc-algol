#include "ast-tab.h"
#include <stdlib.h>
#include <assert.h>

int
main (void)
{
  ast_state_t * ast = new_ast_state ();
  statement * c1 = stmt_block_create (ast);
  assert (ast_as (stmt_block, c1));
  assert (ast_as (container, c1));
  assert (ast_as (statement, c1));
  assert (!ast_as (stmt_dummy, c1));
  container_add_stmt (ast_as (container, c1), stmt_dummy_create (ast));
  container_add_stmt (ast_as (container, c1), stmt_dummy_create (ast));
  statement * toplev = stmt_toplev_create (ast);
  container_add_stmt (ast_as (container, toplev), c1);
  statement_dump (toplev, stdout, 0);
  return 0;
}
