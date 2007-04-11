#include <stdlib.h>
#include <assert.h>

#include "a60_symtab.h"
#include "symbol.h"
#include "label.h"
#include "type.h"
#include "logger.h"
#include "estring.h"
#include "slist.h"
#include "meta.h"

static char const * private_symtab_signature = "symtab";

struct struct_a60_symtab_t
{
#ifndef NDEBUG
  char const * signature;
#endif
  a60_symtab_t * parent;
  slist_t * table;
};

static a60_symtab_t *
private_new_symtab (void)
{
  a60_symtab_t * ret = malloc (sizeof (a60_symtab_t));
#ifndef NDEBUG
  ret->signature = private_symtab_signature;
#endif
  ret->parent = NULL;
  return ret;
}

a60_symtab_t *
a60_new_symtab (void)
{
  a60_symtab_t * ret = private_new_symtab ();
  ret->table = new_slist_typed (adapt_test, a60_as_symbol);
  return ret;
}

void
a60_symtab_set_parent (a60_symtab_t * self, a60_symtab_t * parent)
{
  assert (self != NULL);
  self->parent = parent;
}

void
a60_delete_symtab (a60_symtab_t * self)
{
  if (self != NULL)
    {
      delete_slist (self->table);
      free (self);
    }
}

a60_symtab_t *
a60_as_symtab (void * obj)
{
#ifndef NDEBUG
  a60_check_access (obj, private_symtab_signature);
#endif
  return (a60_symtab_t *)obj;
}


static void
private_symtab_find_it (a60_symtab_t * self, label_t const * lbl, type_t const * atype,
			slist_it_t ** ret_prev, slist_it_t ** ret_it)
{
  slist_it_t * it = slist_iter (self->table);
  slist_it_t * prev = NULL;

  for (; slist_it_has (it); )
    {
      symbol_t * sym = slist_it_get (it);
      type_t * symtype = symbol_type (sym);
      if (label_eq (symbol_label (sym), lbl)
	  && (symtype == NULL || types_match (symtype, atype)))
	break;

      slist_it_next (it);
      if (ret_prev)
	{
	  if (prev)
	    slist_it_next (prev);
	  else
	    prev = slist_iter (self->table);
	}
    }

  if (ret_it)
    *ret_it = it;
  else
    delete_slist_it (it);

  if (ret_prev)
    *ret_prev = prev;
}

int
a60_symtab_add_symbol (a60_symtab_t * self, symbol_t * sym,
		       a60_symtab_entry_kind_t internal)
{
  assert (self != NULL);
  assert (sym != NULL);

  if (internal == sek_ordinary
      && a60_symtab_find_name (self, symbol_label (sym), type_any ()) != NULL)
    return -1;

  slist_pushback (self->table, sym);
  return 0;
}

symbol_t *
a60_symtab_erase_symbol (a60_symtab_t * self, symbol_t * sym)
{
  assert (self != NULL);
  assert (sym != NULL);

  slist_it_t * pr, * it;
  symbol_t * ret;

  private_symtab_find_it (self, symbol_label (sym), symbol_type (sym), &pr, &it);

  if (pr == NULL && it != NULL)
    // found at the beginning
    {
      ret = slist_popfront (self->table);
      delete_slist_it (it);
    }
  else
    // found somewhere in the middle
    {
      assert (pr != NULL && it != NULL);
      ret = slist_it_erase_after (pr);
      delete_slist_it (pr);
      delete_slist_it (it);
    }

  return ret;
}

int
a60_symtab_empty (a60_symtab_t const * self)
{
  return slist_empty (self->table);
}

symbol_t *
a60_symtab_find_name (a60_symtab_t * self, label_t const * lbl,
		      type_t const * atype)
{
  assert (self != NULL);
  assert (lbl != NULL);

  slist_it_t * it;
  private_symtab_find_it (self, lbl, atype, NULL, &it);
  symbol_t * ret = slist_it_has (it) ? slist_it_get (it) : NULL;
  delete_slist_it (it);
  return ret;
}

void
a60_symtab_resolve_symbols (a60_symtab_t * self, container_t * context, logger_t * log)
{
  slist_it_t * it = slist_iter (self->table);
  for (; slist_it_has (it); slist_it_next (it))
    {
      symbol_t * sym = slist_it_get (it);
      symbol_resolve_symbols (sym, context, log);
    }
  delete_slist_it (it);
}

symbol_t *
a60_symtab_find_name_rec (a60_symtab_t * self, label_t const * lbl,
			  type_t const * atype)
{
  assert (self != NULL);
  assert (lbl != NULL);

  symbol_t * sym = a60_symtab_find_name (self, lbl, atype);
  if (sym == NULL && self->parent != NULL)
    return a60_symtab_find_name_rec (self->parent, lbl, atype);
  else
    return sym;
}

symbol_t *
a60_symtab_find_name_rec_add_undefined (a60_symtab_t * self, label_t const * lbl,
					type_t * atype, logger_t * log,
					cursor_t * cursor)
{
  assert (self != NULL);
  assert (lbl != NULL);
  assert (atype != NULL);
  assert (log != NULL);

  symbol_t * found = a60_symtab_find_name_rec (self, lbl, atype);
  if (found == NULL)
    {
      if (types_same (atype, type_any ()))
	log_printfc (log, ll_error, cursor,
		     "(1) unknown symbol named `%s'",
		     estr_cstr (label_id (lbl)));
      else
	{
	  // second chance: look up any symbol of that name
	  found = a60_symtab_find_name_rec (self, lbl, type_any ());
	  if (found == NULL)
	    {
	      log_printfc (log, ll_error, cursor,
			   "(2) unknown symbol named `%s'",
			   estr_cstr (label_id (lbl)));
	    }
	  else
	    {
	      estring_t * t1s = type_to_str (atype, NULL);
	      estring_t * t2s = type_to_str (symbol_type (found), NULL);
	      estring_t * fmt =
		new_estring_fmt ("type mismatch for symbol `%s': "
				 "requested type `%s', found type `%s'",
				 estr_cstr (label_id (lbl)),
				 estr_cstr (t1s), estr_cstr (t2s));
	      log_printfc (log, ll_error, cursor, "%s", estr_cstr (fmt));
	      delete_estring (fmt);
	      delete_estring (t2s);
	      delete_estring (t1s);
	    }
	}
      int was_there = a60_symtab_add_symbol (self, new_symbol_var (lbl), sek_ordinary);
      assert (!was_there);
      found = a60_symtab_find_name (self, lbl, atype);
      // mark a type at new symbol, either fallback type_int, or
      // atype, if it has suitable type
      symbol_set_type (found, is_metatype (atype) ? type_int () : atype);
    }
  assert (found != NULL);

  return found;
}

void
a60_symtab_toplev_define_internals (a60_symtab_t * self)
{
  assert (self != NULL);

  struct interfun {
    char const* n;
    type_t * t;
  } builtins [] =
  {
    // Algol 60 intrinsics
    {"abs",  type_proc_real_real ()},
    {"abs",  type_proc_int_int ()},
    {"sign", type_proc_int_real ()},
    {"sign", type_proc_int_int ()},
    {"sqrt", type_proc_real_real ()},
    {"sin",  type_proc_real_real ()},
    {"cos",  type_proc_real_real ()},
    {"arctan", type_proc_real_real ()},
    {"ln",   type_proc_real_real ()},
    {"exp",  type_proc_real_real ()},
    {"entier", type_proc_int_real ()},
    {"entier", type_proc_real_int ()},
    // Extensions
    {"exit", type_proc_void_int ()},
    {"puts", type_proc_int_string ()},
    //
    {NULL,   NULL}
  };

  struct interfun * ptr = builtins;
  for (; ptr->n != NULL; ptr++)
    {
      // @TODO: shoud this be new_symbol_function?
      symbol_t * s = new_symbol_var (new_label (new_estring_from (ptr->n)));
      symbol_set_type (s, ptr->t);
      symbol_set_hidden (s, 1);
      int fail = a60_symtab_add_symbol (self, s, sek_internal);
      assert (fail == 0);
    }
}

estring_t *
a60_symtab_to_str (a60_symtab_t const * self ATTRIBUTE_UNUSED,
		   estring_t * buf ATTRIBUTE_UNUSED)
{
  assert (!"NYI!");
}

void
a60_symtab_each (a60_symtab_t * self, callback_t callback, void * data)
{
  assert (self != NULL);
  assert (callback != NULL);

  slist_it_t * it = slist_iter (self->table);
  for (; slist_it_has (it); slist_it_next (it))
    {
      symbol_t * sym = a60_as_symbol (slist_it_get (it));
      callback (sym, data);
    }
  delete_slist_it (it);
}
