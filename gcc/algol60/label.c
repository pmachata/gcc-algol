#include <assert.h>
#include <stdlib.h>
#include "meta.h"

#include "label.h"
#include "estring.h"
#include "boundspair.h"
#include "slist.h"

static char const * private_label_signature = "label";

typedef struct struct_label_rep_t
{
  char const * signature;
  estring_t const * id;
  slist_t * arr_bd_list;
}
label_rep_t;

label_t *
new_label (estring_t const * id)
{
  label_rep_t * ret = malloc (sizeof (label_rep_t));
  ret->signature = private_label_signature;
  ret->id = id;
  ret->arr_bd_list = NULL;
  return (void*)ret;
}

void
delete_label (label_t * _self)
{
  if (_self != NULL)
    {
      label_rep_t * self = (void *)_self;
      delete_slist (self->arr_bd_list);
      free (self);
    }
}

label_t *
label (void * ptr)
{
  A60_CHECKED_CONVERSION(label, ptr);
}

estring_t const *
label_id (label_t const * _self)
{
  A60_USER_TO_REP(label, self, const *);
  return self->id;
}

estring_t *
label_to_str (label_t const * _self, estring_t * buf)
{
  A60_USER_TO_REP(label, self, const *);
  if (buf == NULL)
    buf = new_estring ();
  estr_assign (buf, self->id);
  return buf;
}

int
label_eq (label_t const * _lhs, label_t const * _rhs)
{
  A60_USER_TO_REP(label, lhs, const *);
  A60_USER_TO_REP(label, rhs, const *);

  return !estr_compare (lhs->id, rhs->id);
}

slist_t *
label_add_boundspairs (label_t * _self)
{
  A60_USER_TO_REP(label, self, *);

  assert (self->arr_bd_list == NULL);
  self->arr_bd_list = new_slist_typed (adapt_test, boundspair);
  return self->arr_bd_list;
}

slist_t *
label_add_boundspairs_with (label_t * self, slist_t * bps)
{
  slist_t * list = label_add_boundspairs (self);
  slist_it_t * it = slist_iter (bps);
  for (; slist_it_has (it); slist_it_next (it))
    slist_pushback (list, slist_it_get (it));
  delete_slist_it (it);
  return list;
}

void
label_remove_boundspairs (label_t * _self)
{
  A60_USER_TO_REP(label, self, *);

  assert (self->arr_bd_list != NULL);
  delete_slist (self->arr_bd_list);
  self->arr_bd_list = NULL;
}

slist_t *
label_boundspairs (label_t const * _self)
{
  A60_USER_TO_REP(label, self, const *);
  return self->arr_bd_list;
}
