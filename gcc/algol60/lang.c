#include "config.h" /* system.h needs to know if it's safe to include
		       <string.h>/<strings.h> stuff; this is
		       host-specific, and config.h knows it */

#include "system.h" /* stdlib, strings... system includes... for some
		       reason they can't be included directly, has
		       something to do with host/build/target
		       trichotomy. */

#include "options.h"/* generated from lang.opt; here included for
		       CL_Algol60 define */

#include "coretypes.h"
#include "tree.h"
#include "langhooks-def.h" /* for LANG_HOOKS_INITIALIZER macro */
#include "langhooks.h"

#include "toplev.h" /* for error and pedwarn; also rest_of_* stuff is
		       declared here */

#include "target.h" /* for targetm used in init */
#include "flags.h"  /* for flag_signed_char in init */

#include "cgraph.h" /* for cgraph_finalize_* stuff */

#include "tree-dump.h" /* for dump_function */


#include "algol-tree.h"


/* The front end language hooks (addresses of code for this front
   end).  These are not really very language-dependent, i.e.  C,
   Mercury and Algol60 can all use almost the same definitions.  */

#undef LANG_HOOKS_NAME
#define LANG_HOOKS_NAME "Algol60"
#undef LANG_HOOKS_INIT
#define LANG_HOOKS_INIT algol60_init
#undef LANG_HOOKS_FINISH
#define LANG_HOOKS_FINISH algol60_finish
#undef LANG_HOOKS_INIT_OPTIONS
#define LANG_HOOKS_INIT_OPTIONS algol60_init_options
#undef LANG_HOOKS_HANDLE_OPTION
#define LANG_HOOKS_HANDLE_OPTION algol60_handle_option
#undef LANG_HOOKS_POST_OPTIONS
#define LANG_HOOKS_POST_OPTIONS algol60_post_options
#undef LANG_HOOKS_PARSE_FILE
#define LANG_HOOKS_PARSE_FILE algol60_parse_file
#undef LANG_HOOKS_MARK_ADDRESSABLE
#define LANG_HOOKS_MARK_ADDRESSABLE algol60_mark_addressable
#undef LANG_HOOKS_SIGNED_TYPE
#define LANG_HOOKS_SIGNED_TYPE algol60_lang_signed_type
#undef LANG_HOOKS_UNSIGNED_TYPE
#define LANG_HOOKS_UNSIGNED_TYPE algol60_lang_unsigned_type
#undef LANG_HOOKS_SIGNED_OR_UNSIGNED_TYPE
#define LANG_HOOKS_SIGNED_OR_UNSIGNED_TYPE algol60_lang_signed_or_unsigned_type
#undef LANG_HOOKS_TYPE_FOR_MODE
#define LANG_HOOKS_TYPE_FOR_MODE algol60_lang_type_for_mode
#undef LANG_HOOKS_TYPE_FOR_SIZE
#define LANG_HOOKS_TYPE_FOR_SIZE algol60_lang_type_for_size

#undef LANG_HOOKS_CALLGRAPH_EXPAND_FUNCTION
#define LANG_HOOKS_CALLGRAPH_EXPAND_FUNCTION algol60_expand_function

/* Each front end provides its own.  */
const struct lang_hooks lang_hooks = LANG_HOOKS_INITIALIZER;

bool
algol60_init (void)
{
  /* current_function_decl: tree.h */
  current_function_decl = NULL;

  fprintf (stderr, "algol60_init\n");
  build_common_tree_nodes (flag_signed_char, false);
  build_common_tree_nodes_2 (/* short_double= */ 0);
  size_type_node = make_unsigned_type (POINTER_SIZE);
  set_sizetype (size_type_node);
  build_common_builtin_nodes ();
  (*targetm.init_builtins) ();
  return true;
}

void
algol60_finish (void)
{
  fprintf (stderr, "algol60_finish\n");
}

unsigned int
algol60_init_options (unsigned int argc, const char ** argv)
{
  unsigned int i;
  fprintf (stderr, "algol60_init_options\n");
  for (i = 0; i < argc; ++i)
    fprintf (stderr, " + `%s'\n", argv[i]);
  return CL_Algol60;
}

/* process algol60-specific compiler command-line options */
int
algol60_handle_option (size_t scode, const char *arg, int value)
{
  fprintf (stderr, "algol60_handle_option: scode=%d arg=`%s' value=%d\n", scode, arg, value);
  return 1;
}

/* called after option processing */
bool
algol60_post_options (char const* * pfilename)
{
  char const* filename = *pfilename;
  fprintf (stderr, "algol60_post_options\n");
  if (filename == 0 || strcmp (filename, "-") == 0)
    {
      filename = "stdin";
      /*
      if (dependency_tracking)
	error ("can't do dependency tracking with input from stdin");
      */
    }
  fprintf (stderr, "algol60_post_options: filename=`%s'\n", filename);

  /* Initialize the compiler back end.  */
  return false;
}

/* Parse a file.  Debug flag doesn't seem to work. */
void
algol60_parse_file (int debug)
{
  fprintf (stderr, "algol60_parse_file: debug=%d\n", debug);
  /* dummy compilation emits code for this snippet:

     'integer' 'procedure' main;
     'begin'
         main := 4;
     'end';
   */

  /* Build declaration of `main' functions */
  tree param_types = tree_cons (NULL_TREE, void_type_node, NULL_TREE);
  param_types = nreverse (param_types);
  tree fn_type = build_function_type (integer_type_node, param_types);
  tree decl = build_decl (FUNCTION_DECL, get_identifier ("main"), fn_type);
  DECL_CONTEXT (decl) = NULL_TREE;
  TREE_PUBLIC (decl) = 1;
  DECL_EXTERNAL (decl) = 0;
  TREE_STATIC (decl) = 0;
  DECL_ARGUMENTS (decl) = NULL_TREE;

  rest_of_decl_compilation (decl, 1, 0);

  /* Build RESULT DECLARATION, which is used for storing function
     return value. */
  announce_function (decl);
  pushdecl (decl);
  current_function_decl = decl;
  DECL_INITIAL (decl) = error_mark_node;
  tree resultdecl = build_decl (RESULT_DECL, NULL_TREE, TREE_TYPE (TREE_TYPE (decl)));
  DECL_CONTEXT (resultdecl) = decl;
  DECL_ARTIFICIAL (resultdecl) = 1;
  DECL_IGNORED_P (resultdecl) = 1;
  DECL_RESULT (decl) = resultdecl;

  /* Build function BODY:
     `(<block> (return (init_expr resultdecl (int_cst 4))))' */
  tree assign = build2 (INIT_EXPR, void_type_node,
			resultdecl, build_int_cst (integer_type_node, 4));
  TREE_USED (assign) = 1;
  TREE_SIDE_EFFECTS (assign) = 1;
  tree body = build1 (RETURN_EXPR, void_type_node,
		      assign);
  TREE_USED (body) = 1;
  tree block = build_block (/* tree vars =*/ NULL_TREE,
			    /* tree subblocks = */ NULL_TREE,
			    /* tree supercontext = */ NULL_TREE,
			    /* tree chain = */ NULL_TREE);
  DECL_SAVED_TREE (decl) = build3 (BIND_EXPR, void_type_node,
				   NULL_TREE, body, block);
  DECL_INITIAL (decl) = block;
  TREE_USED (block) = 1;

  /* Emit code for the function */
  allocate_struct_function (decl);
  dump_function (TDI_original, decl);
  gimplify_function_tree (decl);
  dump_function (TDI_generic, decl);
  cgraph_finalize_function (decl, /*bool nested = */ false);

  current_function_decl = NULL_TREE;

  /* For -funit-at-time; finalize whole unit */
  cgraph_finalize_compilation_unit ();
}

/* Mark EXP saying that we need to be able to take the
   address of it; it should not be allocated in a register.
   Value is 1 if successful.

   This implementation was copied from c-decl.c. */
bool
algol60_mark_addressable (tree exp)
{
  fprintf (stderr, "algol60_mark_addressable\n");

  tree x = exp;

  while (1)
    switch (TREE_CODE (x))
      {
      case COMPONENT_REF:
      case ADDR_EXPR:
      case ARRAY_REF:
      case REALPART_EXPR:
      case IMAGPART_EXPR:
	x = TREE_OPERAND (x, 0);
	break;

      case CONSTRUCTOR:
	TREE_ADDRESSABLE (x) = 1;
	return true;

      case VAR_DECL:
      case CONST_DECL:
      case PARM_DECL:
      case RESULT_DECL:
	if (DECL_REGISTER (x) && !TREE_ADDRESSABLE (x)
	    && DECL_NONLOCAL (x))
	  {
	    if (TREE_PUBLIC (x) || TREE_STATIC (x) || DECL_EXTERNAL (x))
	      {
		error ("global register variable %qD used in nested function.",
		       x);
		return 0;
	      }
	    pedwarn ("register variable %qD used in nested function.", x);
	  }
	else if (DECL_REGISTER (x) && !TREE_ADDRESSABLE (x))
	  {
	    if (TREE_PUBLIC (x) || TREE_STATIC (x) || DECL_EXTERNAL (x))
	      error ("address of global register variable %qD requested.", x);
	    else
	      error ("address of register variable %qD requested", x);
	    return false;
	  }

	/* drops in */
      case FUNCTION_DECL:
	TREE_ADDRESSABLE (x) = 1;
	/* drops out */
      default:
	return true;
    }
}

/* Return the signed version of a TYPE_NODE, a scalar type.  */
tree
algol60_lang_signed_type (tree type_node)
{
  fprintf (stderr, "algol60_lang_signed_type\n");
  return algol60_lang_type_for_size (TYPE_PRECISION (type_node), 0);
}

/* Return the unsigned version of a TYPE_NODE, a scalar type.  */
tree
algol60_lang_unsigned_type (tree type_node)
{
  fprintf (stderr, "algol60_lang_unsigned_type\n");
  return algol60_lang_type_for_size (TYPE_PRECISION (type_node), 1);
}

/* Return a type the same as TYPE except unsigned or signed according
   to UNSIGNEDP.  */
tree
algol60_lang_signed_or_unsigned_type (int unsignedp, tree type)
{
  fprintf (stderr, "algol60_lang_signed_or_unsigned_type\n");
  if (! INTEGRAL_TYPE_P (type) || TYPE_UNSIGNED (type) == unsignedp)
    return type;
  else
    return algol60_lang_type_for_size (TYPE_PRECISION (type), unsignedp);
}

/* Return a data type that has machine mode MODE.  UNSIGNEDP selects
   an unsigned type; otherwise a signed type is returned.  */
tree
algol60_lang_type_for_mode (enum machine_mode mode, int unsignedp)
{
  if (SCALAR_INT_MODE_P (mode))
    return algol60_lang_type_for_size (GET_MODE_BITSIZE (mode), unsignedp);
  else
    return NULL_TREE;
}

/* Return an integer type with the number of bits of precision given
   by PRECISION.  UNSIGNEDP is nonzero if the type is unsigned;
   otherwise it is a signed type.  */
tree
algol60_lang_type_for_size (unsigned precision, int unsignedp)
{
  fprintf (stderr, "algol60_lang_type_for_size\n");
  tree t;

  if (precision <= MAX_BITS_PER_WORD
      && signed_and_unsigned_types[precision][unsignedp] != 0)
    return signed_and_unsigned_types[precision][unsignedp];

  if (unsignedp)
    t = signed_and_unsigned_types[precision][1]
      = make_unsigned_type (precision);
  else
    t = signed_and_unsigned_types[precision][0]
      = make_signed_type (precision);

  return t;
}

void
algol60_expand_function (tree fndecl)
{
  /* We have nothing special to do while expanding functions for Algol60.  */
  tree_rest_of_compilation (fndecl);
}
