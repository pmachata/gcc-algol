#include <stdlib.h>
#include <assert.h>
#include "meta.h"

#include "boundspair.h"

static char const * private_boundspair_signature = "boundspair";

typedef struct struct_boundspair_rep_t
{
  char const * signature;
  expression_t * lobound;
  expression_t * hibound;
}
boundspair_rep_t;


boundspair_t *
new_boundspair (expression_t * lo, expression_t * hi)
{
  boundspair_rep_t * ret = malloc (sizeof (boundspair_rep_t));
  ret->signature = private_boundspair_signature;
  ret->hibound = hi;
  ret->lobound = lo;
  return (void*)ret;
}

void
delete_boundspair (boundspair_t * self)
{
  free (self);
}

boundspair_t *
boundspair (void * ptr)
{
  A60_CHECKED_CONVERSION (boundspair, ptr);
}

expression_t *
boundspair_hi (boundspair_t const * _self)
{
  A60_USER_TO_REP(boundspair, self, const *);
  return self->hibound;
}

expression_t *
boundspair_lo (boundspair_t const * _self)
{
  A60_USER_TO_REP(boundspair, self, const *);
  return self->lobound;
}

void
boundspair_set_hi (boundspair_t * _self, expression_t * bound)
{
  A60_USER_TO_REP(boundspair, self, *);
  self->hibound = bound;
}

void
boundspair_set_lo (boundspair_t * _self, expression_t * bound)
{
  A60_USER_TO_REP(boundspair, self, *);
  self->lobound = bound;
}
