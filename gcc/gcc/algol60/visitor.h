//-*-c-*-
#ifndef VISITOR_H_GUARD
#define VISITOR_H_GUARD

#include "visitor.i"

void * a60_visitor_dispatch (visitor_t * visitor, void * dispatch_on, void * self, void * data);

callback_t a60_visitor_pick (visitor_t * visitor, void * dispatch_on);

#endif /* VISITOR_H_GUARD */
