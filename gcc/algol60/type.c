#include <assert.h>
#include <stdlib.h>
#include "meta.h"
#include "util.h"

#include "type.h"
#include "estring.h"
#include "expression.h"
#include "statement.h"
#include "logger.h"
#include "slist.h"
#include "boundspair.h"
#include "symbol.h"
#include "cursor.h"


typedef struct struct_t_array_t
{
  type_t * host;
  boundspair_t * bounds;
}
t_array_t;

typedef struct struct_t_own_t
{
  type_t * host;
}
t_own_t;

typedef struct struct_t_proc_t
{
  type_t * ret_type;
  slist_t * arg_types;
}
t_proc_t;

typedef enum enum_type_kind_t
{
  tk_unknown,
  tk_any,
  tk_int,
  tk_void,
  tk_real,
  tk_string,
  tk_bool,
  tk_label,
  tk_array,
  tk_own,
  tk_proc
}
type_kind_t;

struct struct_type_t
{
  char const * signature;
  type_kind_t kind;
  union {
    t_array_t t_array;
    t_own_t t_own;
    t_proc_t t_proc;
  };
};

char const * const private_type_signature = "type";

static type_t * private_new_type (type_kind_t kind);
static void private_type_to_str (type_t const * self, estring_t * buf, int canon);
static int private_proc_types_check (t_proc_t const * self, t_proc_t const * other, int (*pred)(type_t const *, type_t const *));
static void private_array_resolve_symbols (type_t * tself, container_t * context, logger_t * log);

static type_t *
private_new_type (type_kind_t kind)
{
  type_t * ret = calloc (1, sizeof (type_t));
  ret->signature = private_type_signature;
  ret->kind = kind;
  return ret;
}


type_t *
new_t_unknown (void)
{
  static type_t * cache = NULL;
  if (cache == NULL)
    cache = private_new_type (tk_unknown);
  return cache;
}

type_t *
new_t_any (void)
{
  static type_t * cache = NULL;
  if (cache == NULL)
    cache = private_new_type (tk_any);
  return cache;
}

type_t *
new_t_int (void)
{
  static type_t * cache = NULL;
  if (cache == NULL)
    cache = private_new_type (tk_int);
  return cache;
}

type_t *
new_t_void (void)
{
  static type_t * cache = NULL;
  if (cache == NULL)
    cache = private_new_type (tk_void);
  return cache;
}

type_t *
new_t_real (void)
{
  static type_t * cache = NULL;
  if (cache == NULL)
    cache = private_new_type (tk_real);
  return cache;
}

type_t *
new_t_string (void)
{
  static type_t * cache = NULL;
  if (cache == NULL)
    cache = private_new_type (tk_string);
  return cache;
}

type_t *
new_t_bool (void)
{
  static type_t * cache = NULL;
  if (cache == NULL)
    cache = private_new_type (tk_bool);
  return cache;
}

type_t *
new_t_label (void)
{
  static type_t * cache = NULL;
  if (cache == NULL)
    cache = private_new_type (tk_label);
  return cache;
}

type_t *
new_t_array (type_t * host)
{
  type_t * ret = private_new_type (tk_array);
  ret->t_array.host = host;
  return ret;
}

type_t *
new_t_own (type_t * host)
{
  type_t * ret = private_new_type (tk_own);
  ret->t_own.host = host;
  return ret;
}

type_t *
new_t_proc (type_t * rettype, slist_t * argtypes)
{
  type_t * ret = private_new_type (tk_proc);
  ret->t_proc.ret_type = rettype;
  slist_set_type (argtypes, adapt_test, type);
  ret->t_proc.arg_types = argtypes;
  return ret;
}

type_t *
type (void * ptr)
{
  A60_CHECKED_CONVERSION (type, ptr);
}

type_t *
t_array_set_bounds (type_t * self, boundspair_t * boundspair)
{
  assert (self != NULL);
  assert (self->kind == tk_array);
  self->t_array.bounds = boundspair;
  return self;
}

boundspair_t *
t_array_bounds (type_t const * self)
{
  assert (self != NULL);
  assert (self->kind == tk_array);
  return self->t_array.bounds;
}

type_t *
t_proc_return_type (type_t const * self)
{
  assert (self != NULL);
  assert (self->kind == tk_proc);
  return self->t_proc.ret_type;
}

slist_t *
t_proc_arg_types (type_t const * self)
{
  assert (self != NULL);
  assert (self->kind == tk_proc);
  return self->t_proc.arg_types;
}

static int
private_proc_types_check (t_proc_t const * self, t_proc_t const * other,
			  int (*pred)(type_t const*, type_t const*))
{
  if (!pred (self->ret_type, other->ret_type))
    return 0;

  slist_it_t * it1 = slist_iter (self->arg_types);
  slist_it_t * it2 = slist_iter (other->arg_types);
  int ret = 0;
  while (slist_it_has (it1) && slist_it_has (it2))
    {
      type_t * t1 = slist_it_get (it1);
      type_t * t2 = slist_it_get (it2);
      if (!pred (t1, t2))
	// leave ret at 0 and jump away
	goto cleanup;

      slist_it_next (it1);
      slist_it_next (it2);
    }
  // so far, all types were compatible
  // also require same number of arguments
  ret = slist_it_has (it1) == slist_it_has (it2);

 cleanup:
  delete_slist_it (it1);
  delete_slist_it (it2);
  return ret;
}

int
types_same (type_t const * lhs, type_t const * rhs)
{
  assert (lhs != NULL);
  assert (rhs != NULL);

  while (1)
    {
      if (lhs->kind != rhs->kind)
	return 0;
      // From now on, we know that lhs->kind == rhs->kind.  Further tests
      // only check lhs->kind.

      switch (lhs->kind)
	{
	case tk_unknown:
	case tk_any:
	case tk_int:
	case tk_void:
	case tk_real:
	case tk_string:
	case tk_bool:
	case tk_label:
	  return 1;

	case tk_array:
	  lhs = lhs->t_array.host;
	  rhs = rhs->t_array.host;
	  break;

	case tk_own:
	  lhs = lhs->t_own.host;
	  rhs = rhs->t_own.host;
	  break;

	case tk_proc:
	  return private_proc_types_check (&lhs->t_proc, &rhs->t_proc, types_same);
	};
    }
}

int
types_match (type_t const * lhs, type_t const * rhs)
{
  assert (lhs != NULL);
  assert (rhs != NULL);

  while (1)
    {
      if (lhs->kind == tk_any
	  || rhs->kind == tk_any)
	{
	  return 1;
	}
      else if (lhs->kind == tk_unknown
	       || rhs->kind == tk_unknown)
	{
	  return 0;
	}
      else if (lhs->kind == tk_own)
	{
	  // intentionally swapped
	  type_t const * tmp = rhs;
	  rhs = lhs->t_own.host;
	  lhs = tmp;
	}
      else if (rhs->kind == tk_own)
	{
	  // intentionally swapped
	  type_t const * tmp = lhs;
	  lhs = rhs->t_own.host;
	  rhs = tmp;
	}
      else if (lhs->kind == tk_array
	       && rhs->kind == tk_array)
	{
	  lhs = lhs->t_array.host;
	  rhs = rhs->t_array.host;
	}
      else if (lhs->kind == tk_proc
	       && rhs->kind == tk_proc)
	{
	  return private_proc_types_check (&lhs->t_proc, &rhs->t_proc, types_match);
	}
      else
	{
	  return types_same ((void*)lhs, (void*)rhs);
	}
    }
}

expression_t *
expr_primitive_for_type (type_t const * self)
{
  assert (self != NULL);

  if (self->kind == tk_own)
    self = self->t_own.host;
  assert (self->kind != tk_own);

  switch (self->kind)
    {
    case tk_int:
      return new_expr_int (new_cursor ("", 0), 0);
    case tk_real:
      return new_expr_real (new_cursor ("", 0), new_estring_from ("0.0"));
    case tk_string:
      return new_expr_string (new_cursor ("", 0), new_estring_from (""));
    case tk_bool:
      return new_expr_bool (new_cursor ("", 0), 1);

    case tk_unknown:
    case tk_any:
    case tk_void:
    case tk_label:
    case tk_array:
    case tk_own:
    case tk_proc:
      assert (!"Should never get there!");
    };

  assert (!"Should never get there!");
}

int
is_metatype (type_t const * self)
{
  assert (self != NULL);

  while (1)
    {
      if (self->kind == tk_unknown
	  || self->kind == tk_any)
	return 1;
      else if (self->kind == tk_array)
	self = self->t_array.host;
      else if (self->kind == tk_own)
	self = self->t_own.host;
      else if (self->kind == tk_proc)
	{
	  if (is_metatype (self->t_proc.ret_type))
	    return 1;

	  slist_it_t * it = slist_iter (self->t_proc.arg_types);
	  int ret = 0;
	  for (; slist_it_has (it); slist_it_next (it))
	    if (is_metatype (type (slist_it_get (it))))
	      {
		ret = 1;
		break;
	      }
	  delete_slist_it (it);
	  return ret;
	}
      else
	return 0;
    }
}

int
type_is_own (type_t const * self)
{
  assert (self != NULL);
  return self->kind == tk_own;
}

int
type_is_array (type_t const * self)
{
  assert (self != NULL);
  return self->kind == tk_array;
}

type_t *
type_host (type_t const * self)
{
  assert (self != NULL);
  if (self->kind == tk_own)
    return self->t_own.host;
  else if (self->kind == tk_array)
    return self->t_array.host;
  else
    return NULL;
}

static void
private_array_resolve_symbols (type_t * tself, container_t * context, logger_t * log)
{
  estring_t * tmp = NULL;
  t_array_t * self = &(tself->t_array);

  // We need to resolve array bounds.
  boundspair_t * bp = self->bounds;
  expression_t * hi = boundspair_hi (bp);
  expression_t * lo = boundspair_lo (bp);

  // symbols in bounds are resolved with use of parental
  // symtab.  The following is illegal:
  //  'begin' 'integer' y; 'integer' 'array' z[1:y]; 'end';
  expr_resolve_symbols (lo, container_parent (context), log);
  expr_resolve_symbols (hi, container_parent (context), log);
  type_resolve_symbols (self->host, context, log);

  type_t * lot = expr_type (lo);
  type_t * hit = expr_type (hi);
  if (!types_match (lot, type_int ()))
    {
      log_printfc (log, ll_error, expr_cursor (lo),
		  "invalid type %s in lower boundary",
		  estr_cstr (tmp = type_to_str (lot, tmp)));
      boundspair_set_lo (bp, expr_primitive_for_type (type_int ()));
    }

  if (!types_match (hit, type_int ()))
    {
      log_printfc (log, ll_error, expr_cursor (hi),
		  "invalid type %s in upper boundary",
		  estr_cstr (tmp = type_to_str (hit, tmp)));
      boundspair_set_hi (bp, expr_primitive_for_type (type_int ()));
    }

  delete_estring (tmp);
}

void
type_resolve_symbols (type_t * self, container_t * context, logger_t * log)
{
  assert (self != NULL);

  switch (self->kind)
    {
    case tk_unknown:
    case tk_any:
      assert (!"Should never get there!");

    case tk_int:
    case tk_void:
    case tk_real:
    case tk_string:
    case tk_bool:
    case tk_label:
      // nothing to do
      return;

    case tk_array:
      private_array_resolve_symbols (self, context, log);
      return;

    case tk_own:
      type_resolve_symbols (self->t_own.host, context, log);
      return;

    case tk_proc:
      // We don't need to resolve any symbols, as it's neither
      // possible to denote array dimensions at procedure parameters,
      // nor is it allowed to return arrays.
      return;
    };
  assert (!"Should never get there!");
}

int
type_is_unknown (type_t const * self)
{
  assert (self != NULL);
  return self->kind == tk_unknown;
}

int
type_is_proc (type_t const * self)
{
  assert (self != NULL);
  return self->kind == tk_proc;
}

static void
private_type_to_str (type_t const * self, estring_t * buf, int canon)
{
  static char const* typestrs [] = {
    [tk_unknown] = "<unknown>",
    [tk_any] = "<any>",
    [tk_int] = "'integer'",
    [tk_void] = "<void>",
    [tk_real] = "'real'",
    [tk_string] = "'string'",
    [tk_bool] = "'Boolean'",
    [tk_label] = "<label>"
  };

  if ((self->kind < sizeof (typestrs) / sizeof (*typestrs))
      && typestrs[self->kind])
    {
      estr_assign_cstr (buf, typestrs[self->kind]);
      return;
    }

  switch (self->kind)
    {
    case tk_unknown:    case tk_any:    case tk_int:
    case tk_void:       case tk_real:   case tk_string:
    case tk_bool:       case tk_label:
      assert (!"Should already be covered!");

    case tk_array:
      {
	type_t * host = self->t_array.host;
	private_type_to_str (host, buf, canon);
	if (!canon || host->kind != tk_array)
	  estr_append_cstr (buf, " 'array'");
	return;
      }

    case tk_own:
      {
	private_type_to_str (self->t_own.host, buf, canon);
	estr_prepend_cstr (buf, "'own' ");
	return;
      }

    case tk_proc:
      {
	estr_assign_cstr (buf, "<proc ");
	estring_t * tmp = new_estring ();

	slist_it_t * it = slist_iter (self->t_proc.arg_types);
	while (1)
	  {
	    type_t * t = slist_it_get (it);
	    private_type_to_str (t, tmp, canon);
	    estr_append (buf, tmp);
	    slist_it_next (it);
	    if (slist_it_has (it))
	      estr_append_cstr (buf, ", ");
	    else
	      break;
	  }
	delete_slist_it (it);

	private_type_to_str (self->t_proc.ret_type, tmp, canon);
	estr_append_cstr (buf, " -> ");
	estr_append (buf, tmp);
	estr_append_cstr (buf, ">");
	delete_estring (tmp);
	return;
      }
    };
  assert (!"Should never get there!");
}

estring_t *
type_to_str (type_t const * self, estring_t * buf)
{
  assert (self != NULL);
  buf = tmpbuild (buf, (void*(*)(void))new_estring);
  private_type_to_str (self, buf, 0);
  return buf;
}

estring_t *
type_to_str_canon (type_t const * self, estring_t * buf)
{
  assert (self != NULL);
  buf = tmpbuild (buf, (void*(*)(void))new_estring);
  private_type_to_str (self, buf, 1);
  return buf;
}

type_t *
type_get_root (type_t * self)
{
  assert (self != NULL);

  while (1)
    {
      if (self->kind == tk_array)
	self = self->t_array.host;
      else if (self->kind == tk_own)
	self = self->t_own.host;
      else if (self->kind == tk_any || self->kind == tk_unknown)
	assert (!"Should never get there!");
      else
	break;
    }

  return self;
}

#define MEMOIZE(TYPE)\
type_t * type_##TYPE (void) {\
  static type_t * inst = NULL;\
  if (!inst) inst = new_t_##TYPE ();\
  return inst;\
}
#define MEMOIZE2(TYPE1,TYPE2)\
type_t * type_##TYPE1##_##TYPE2 (void) {\
  static type_t * inst = NULL;\
  if (!inst) inst = new_t_##TYPE1 (type_##TYPE2 ());\
  return inst;\
}
#define MEMOIZEPROC1(TYPERET,TYPEARG)\
type_t * type_proc_##TYPERET##_##TYPEARG (void) {\
  static type_t * inst = NULL;\
  if (!inst) inst = new_t_proc (type_##TYPERET (), new_slist_from (1, type_##TYPEARG ())); \
  return inst;\
}
#define MEMOIZEPROC2(TYPERET,TYPEARG1,TYPEARG2)\
type_t * type_proc_##TYPERET##_##TYPEARG1##_##TYPEARG2 (void) {\
  static type_t * inst = NULL;\
  if (!inst) inst = new_t_proc (type_##TYPERET (), new_slist_from (2, type_##TYPEARG1 (), type_##TYPEARG2 ())); \
  return inst;\
}

MEMOIZE(unknown)
MEMOIZE(any)
MEMOIZE(int)
MEMOIZE(void)
MEMOIZE(real)
MEMOIZE(string)
MEMOIZE(bool)
MEMOIZE(label)
MEMOIZE2(array, any)
MEMOIZEPROC1(void, int)
MEMOIZEPROC1(int, real)
MEMOIZEPROC1(real, int)
MEMOIZEPROC1(int, int)
MEMOIZEPROC1(real, real)
MEMOIZEPROC1(int, string)
MEMOIZEPROC2(int, int, int)
MEMOIZEPROC2(real, real, int)
MEMOIZEPROC2(real, int, real)
MEMOIZEPROC2(real, real, real)

#undef MEMOIZE
#undef MEMOIZE2
#undef MEMOIZEPROC1
#undef MEMOIZEPROC2


#ifndef IN_GCC
static void ATTRIBUTE_NORETURN
no_glue_built (void)
{
  fprintf (stderr, "Dummy type_*_build_generic called.\n");
  abort ();
}
void * type_own_build_generic (type_t * self ATTRIBUTE_UNUSED, void * data ATTRIBUTE_UNUSED) { no_glue_built (); }
void * type_void_build_generic (type_t * self ATTRIBUTE_UNUSED, void * data ATTRIBUTE_UNUSED) { no_glue_built (); }
void * type_int_build_generic (type_t * self ATTRIBUTE_UNUSED, void * data ATTRIBUTE_UNUSED) { no_glue_built (); }
void * type_real_build_generic (type_t * self ATTRIBUTE_UNUSED , void * data ATTRIBUTE_UNUSED) { no_glue_built (); }
void * type_string_build_generic (type_t * self ATTRIBUTE_UNUSED , void * data ATTRIBUTE_UNUSED) { no_glue_built (); }
void * type_bool_build_generic (type_t * self ATTRIBUTE_UNUSED , void * data ATTRIBUTE_UNUSED) { no_glue_built (); }
void * type_label_build_generic (type_t * self ATTRIBUTE_UNUSED , void * data ATTRIBUTE_UNUSED) { no_glue_built (); }
void * type_array_build_generic (type_t * self ATTRIBUTE_UNUSED , void * data ATTRIBUTE_UNUSED) { no_glue_built (); }
void * type_proc_build_generic (type_t * self ATTRIBUTE_UNUSED , void * data ATTRIBUTE_UNUSED) { no_glue_built (); }

no_glue_built2 (void)
{
  fprintf (stderr, "Dummy symbol_decl_for_* called.\n");
  abort ();
}
void * symbol_decl_for_own (symbol_t * sym ATTRIBUTE_UNUSED, void * data ATTRIBUTE_UNUSED) { no_glue_built2 (); }
void * symbol_decl_for_int (symbol_t * sym ATTRIBUTE_UNUSED, void * data ATTRIBUTE_UNUSED) { no_glue_built2 (); }
void * symbol_decl_for_void (symbol_t * sym ATTRIBUTE_UNUSED, void * data ATTRIBUTE_UNUSED) { no_glue_built2 (); }
void * symbol_decl_for_real (symbol_t * sym ATTRIBUTE_UNUSED, void * data ATTRIBUTE_UNUSED) { no_glue_built2 (); }
void * symbol_decl_for_string (symbol_t * sym ATTRIBUTE_UNUSED, void * data ATTRIBUTE_UNUSED) { no_glue_built2 (); }
void * symbol_decl_for_bool (symbol_t * sym ATTRIBUTE_UNUSED, void * data ATTRIBUTE_UNUSED) { no_glue_built2 (); }
void * symbol_decl_for_label (symbol_t * sym ATTRIBUTE_UNUSED, void * data ATTRIBUTE_UNUSED) { no_glue_built2 (); }
void * symbol_decl_for_array (symbol_t * sym ATTRIBUTE_UNUSED, void * data ATTRIBUTE_UNUSED) { no_glue_built2 (); }
void * symbol_decl_for_proc (symbol_t * sym ATTRIBUTE_UNUSED, void * data ATTRIBUTE_UNUSED) { no_glue_built2 (); }
#endif


void *
type_build_generic (type_t * self, void * data)
{
  assert (self != NULL);

  switch (self->kind)
    {
    case tk_unknown:
      return type_unknown_build_generic (self, data);
    case tk_any:
      return type_any_build_generic (self, data);
    case tk_own:
      return type_own_build_generic (self, data);
    case tk_int:
      return type_int_build_generic (self, data);
    case tk_void:
      return type_void_build_generic (self, data);
    case tk_real:
      return type_real_build_generic (self, data);
    case tk_string:
      return type_string_build_generic (self, data);
    case tk_bool:
      return type_bool_build_generic (self, data);
    case tk_label:
      return type_label_build_generic (self, data);
    case tk_array:
      return type_array_build_generic (self, data);
    case tk_proc:
      return type_proc_build_generic (self, data);
    };
  assert (!"Should never get there!");
}

void *
symbol_decl_for_type (type_t * t, symbol_t * sym, void * data)
{
  assert (t != NULL);
  assert (sym != NULL);

  switch (t->kind)
    {
    case tk_unknown:
      return symbol_decl_for_unknown (sym, data);
    case tk_any:
      return symbol_decl_for_any (sym, data);
    case tk_own:
      return symbol_decl_for_own (sym, data);
    case tk_int:
      return symbol_decl_for_int (sym, data);
    case tk_void:
      return symbol_decl_for_void (sym, data);
    case tk_real:
      return symbol_decl_for_real (sym, data);
    case tk_string:
      return symbol_decl_for_string (sym, data);
    case tk_bool:
      return symbol_decl_for_bool (sym, data);
    case tk_label:
      return symbol_decl_for_label (sym, data);
    case tk_array:
      return symbol_decl_for_array (sym, data);
    case tk_proc:
      return symbol_decl_for_proc (sym, data);
    };
  assert (!"Should never get there!");
}
