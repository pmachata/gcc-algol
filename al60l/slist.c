/*
 * Copyright (c) 2006 Petr Machata
 * All rights reserved.
 */

#ifndef SELF_TEST

#include "slist.h"
#include <assert.h>
#include <stdlib.h>

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
} slist_rep_t;

slist_t *
new_slist (void)
{
  slist_rep_t * ret = malloc (sizeof (slist_rep_t));
  if (ret == NULL)
    return NULL;

  ret->signature = private_slist_signature;
  ret->head = NULL;
  ret->tail = NULL;
  return (void*)ret;
}

void
delete_slist (slist_t * _list)
{
  if (_list != NULL)
    {
      slist_rep_t * list = (void*)_list;

      for (slist_node_rep_t * node = list->head;
	   node != NULL; )
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

void
slist_pushback (slist_t * _list, void * object)
{
  assert (_list != NULL);
  slist_rep_t * list = (void*)_list;

  slist_node_rep_t * node = malloc (sizeof (slist_node_rep_t));
  node->object = object;

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

  slist_node_rep_t * node = malloc (sizeof (slist_node_rep_t));
  node->object = object;

  if (list->tail == NULL)
    {
      list->tail = node;
      assert (list->head == NULL);
      list->head = node;
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

  for (slist_node_rep_t * node = list->head;
       node != NULL; node = node->link)
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

  for (slist_node_rep_t * node = list->head;
       node != NULL; node = node->link)
    {
      if (pred (_list, node->object, userdata))
	slist_pushback (ret, node->object);
    }

  return ret;
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

int
main (void)
{
  slist_t * l = new_slist ();
  assert (slist (l));
  assert (slist_empty (l));

  for (int i = 0; i < 10; ++i)
    {
      slist_pushback (l, (void*)i);
      assert (!slist_empty (l));
    }

  int ctr = 0;
  slist_each (l, incctr, &ctr);
  assert (ctr == 10);

  ctr = 0;
  slist_t * l2 = slist_filter (l, odd, NULL);
  assert (slist (l2));
  slist_each (l, incctr, &ctr);
  slist_each (l2, incctr, &ctr);
  assert (ctr == 15);

  while (!slist_empty (l))
    {
      slist_pushfront (l2, slist_popfront (l));
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

  printf ("All passed.\n");
  return 0;
}

#endif
