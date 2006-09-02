/*
 * Copyright (c) 2006 Petr Machata
 * All rights reserved.
 */

#ifndef SELF_TEST

#include "slist.h"
#include <assert.h>
#include <stdlib.h>
#include <stdarg.h>

static char const* private_slist_signature = "slist";

typedef struct struct_slist_node_rep_t
{
  struct struct_slist_node_rep_t * link;
  void * object;
} slist_node_rep_t;

typedef struct struct_slist_rep_t
{
  char const* signature;
  slist_node_rep_t * head;
  slist_node_rep_t * tail;
#ifndef NDEBUG
  void * (*test)(void * obj, void * user);
  void * test_userdata;
#endif
} slist_rep_t;

typedef struct struct_slist_it_rep_t
{
  slist_node_rep_t * pointee;
} slist_it_rep_t;

slist_rep_t *
private_alloc_slist (void)
{
  slist_rep_t * ret = malloc (sizeof (slist_rep_t));
  if (ret == NULL)
    return NULL;

  ret->signature = private_slist_signature;
  ret->head = NULL;
  ret->tail = NULL;
  ret->test = NULL;
  ret->test_userdata = NULL;

  return ret;
}

void *
adapt_test (void * obj, void * test)
{
  void * (*xtest)(void *) = test;
  return xtest (obj);
}

slist_t *
new_slist (void)
{
  return (void*)private_alloc_slist ();
}

slist_t *
new_slist_typed (void* (*test)(void * obj, void * user), void * userdata)
{
#ifndef NDEBUG
  slist_rep_t * ret = private_alloc_slist ();
  if (ret == NULL)
    return NULL;

  ret->test = test;
  ret->test_userdata = userdata;

  return (void*)ret;
#else
  return new_slist ();
#endif
}

slist_t *
new_slist_from (int num, ...)
{
  assert (num >= 0);
  slist_t * ret = new_slist ();
  va_list ap;
  va_start(ap, num);
  while (num--)
    {
      void * arg = va_arg(ap, void *);
      slist_pushback (ret, arg);
    }
  va_end(ap);
  return ret;
}

slist_t *
clone_slist (slist_t * _slist)
{
  assert (_slist != NULL);
  slist_t * ret = new_slist ();
  slist_rep_t * slist = (void*)_slist;
  slist_node_rep_t * node = slist->head;
  for (; node != NULL; node = node->link)
    slist_pushback (ret, node->object);

  return ret;
}

void
delete_slist (slist_t * _list)
{
  if (_list != NULL)
    {
      slist_rep_t * list = (void*)_list;
      slist_node_rep_t * node = list->head;

      for (; node != NULL; )
	{
	  slist_node_rep_t * next = node->link;
	  free (node);
	  node = next;
	}

      free (list);
    }
}

slist_t *
slist (void * ptr)
{
  if (((slist_rep_t*)ptr)->signature == private_slist_signature)
    return ptr;
  else
    return NULL;
}

#ifndef NDEBUG
void
private_test_element (slist_rep_t * list, void * object)
{
  if (list->test != NULL)
    assert (list->test (object, list->test_userdata));
}
#else
#  define private_test_element(LIST,OBJECT)
#endif

void
slist_pushback (slist_t * _list, void * object)
{
  assert (_list != NULL);
  slist_rep_t * list = (void*)_list;
  private_test_element (list, object);

  slist_node_rep_t * node = malloc (sizeof (slist_node_rep_t));
  node->object = object;
  node->link = NULL;

  if (list->tail == NULL)
    {
      list->tail = node;
      assert (list->head == NULL);
      list->head = node;
    }
  else
    {
      list->tail->link = node;
      list->tail = node;
    }
}

void
slist_pushfront (slist_t * _list, void * object)
{
  assert (_list != NULL);
  slist_rep_t * list = (void*)_list;
  private_test_element (list, object);

  slist_node_rep_t * node = malloc (sizeof (slist_node_rep_t));
  node->object = object;

  if (list->tail == NULL)
    {
      list->tail = node;
      assert (list->head == NULL);
      list->head = node;
      node->link = NULL;
    }
  else
    {
      node->link = list->head;
      list->head = node;
    }
}

void *
slist_popfront (slist_t * _list)
{
  assert (_list != NULL);
  slist_rep_t * list = (void*)_list;

  slist_node_rep_t * node = list->head;
  void * object = node->object;

  list->head = node->link;
  if (list->head == NULL)
    list->tail = NULL;
  free (node);

  return object;
}

void *
slist_front (slist_t * _list)
{
  assert (_list != NULL);
  slist_rep_t * list = (void*)_list;
  return list->head->object;
}

void *
slist_back (slist_t * _list)
{
  assert (_list != NULL);
  slist_rep_t * list = (void*)_list;
  return list->tail->object;
}

int
slist_empty (slist_t * _list)
{
  assert (_list != NULL);
  slist_rep_t * list = (void*)_list;

  assert ((list->head == NULL) == (list->tail == NULL));
  return (list->head == NULL);
}

void slist_each (
  slist_t * _list,
  void (*fn)(slist_t *, void *, void *),
  void * userdata)
{
  assert (_list != NULL);
  slist_rep_t * list = (void*)_list;
  assert (fn != NULL);

  slist_node_rep_t * node = list->head;
  for (; node != NULL; node = node->link)
    {
      void * obj = node->object;
      fn (_list, obj, userdata);
    }
}


slist_t *
slist_filter (
   slist_t * _list,
   int (*pred)(slist_t * /*list*/, void * /*label*/, void * /*userdata*/),
   void * userdata)
{
  assert (_list != NULL);
  slist_rep_t * list = (void*)_list;
  assert (pred != NULL);

  slist_t * ret = new_slist ();
  slist_node_rep_t * node = list->head;
  for (; node != NULL; node = node->link)
    {
      if (pred (_list, node->object, userdata))
	slist_pushback (ret, node->object);
    }

  return ret;
}


slist_it_t *
slist_iter (slist_t * _list)
{
  assert (_list != NULL);
  slist_rep_t * list = (void*)_list;

  slist_it_rep_t * ret = malloc (sizeof (slist_it_rep_t));
  if (ret == NULL)
    return NULL;

  ret->pointee = list->head;

  return (void*)ret;
}

int
slist_it_has (slist_it_t * _it)
{
  assert (_it != NULL);
  slist_it_rep_t * it = (void*)_it;
  return it->pointee != NULL;
}

void *
slist_it_get (slist_it_t * _it)
{
  assert (_it != NULL);
  slist_it_rep_t * it = (void*)_it;
  return it->pointee->object;
}

void
slist_it_next (slist_it_t * _it)
{
  assert (_it != NULL);
  slist_it_rep_t * it = (void*)_it;
  it->pointee = it->pointee->link;
}

void
delete_slist_it (slist_it_t * it)
{
  if (it != NULL)
    free (it);
}

#else /* SELF_TEST */

#include "slist.h"
#include <stdio.h>
#include <assert.h>

void
incctr (slist_t * list, void * obj, void * pctr)
{
  (*(int*)pctr)++;
}

int
odd (slist_t * list, void * obj, void * userdata)
{
  return (int)obj % 2;
}

void *
num_0_to_9 (void * obj)
{
  int number = (int)obj;
  if (number >=0 && number <= 9)
    return (void*)1;
  else
    return NULL;
}

int
main (void)
{
  slist_t * l1 = new_slist ();
  slist_t * l2 = new_slist ();
  slist_t * l3 = new_slist_typed (adapt_test, num_0_to_9);
  slist_t * l4 = new_slist_typed (adapt_test, num_0_to_9);
  slist_t * lists[] = {
    l1, l3, l2, l4, NULL
  };
  char const* names[] = {
    "ordinary list", "typed list", "other ordinary list", "other typed list", NULL
  };

  slist_t ** lit = lists;
  char const* * nit = names;

  for (; *lit != NULL; ++lit, ++nit)
    {
      slist_t * l = *lit;
      printf (" + %s\n", *nit);
      assert (slist (l));
      assert (slist_empty (l));

      for (int i = 0; i < 10; ++i)
	{
	  slist_pushback (l, (void*)i);
	  assert (!slist_empty (l));
	  assert (slist_back (l) == (void*)i);
	}

      int ctr = 0;
      slist_each (l, incctr, &ctr);
      assert (ctr == 10);
      int sum = 0;
      for (slist_it_t * it = slist_iter (l); slist_it_has (it); slist_it_next (it))
	sum += (int)slist_it_get (it);
      assert (sum == 45);

      ctr = 0;
      slist_t * l2 = slist_filter (l, odd, NULL);
      assert (slist (l2));
      slist_each (l, incctr, &ctr);
      slist_each (l2, incctr, &ctr);
      assert (ctr == 15);

      while (!slist_empty (l))
	{
	  void * fr = slist_front (l);
	  void * fr2 = slist_popfront (l);
	  assert (fr == fr2);
	  slist_pushfront (l2, l2);
	  --ctr;
	}
      assert (ctr == 5);

      ctr = 0;
      slist_each (l, incctr, &ctr);
      assert (ctr == 0);

      ctr = 0;
      slist_each (l2, incctr, &ctr);
      assert (ctr == 15);

      delete_slist (l);
      delete_slist (l2);
    }

  assert (*nit == NULL);

  printf (" + empty list\n");
  slist_it_t * it = slist_iter (new_slist ());
  for (; slist_it_has (it); slist_it_next (it))
    assert (!"empty list shouldn't execute this");
  delete_slist_it (it);

  printf ("All passed.\n");
  return 0;
}

#endif
