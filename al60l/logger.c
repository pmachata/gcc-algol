/*
 * Copyright (c) 2006 Petr Machata
 * All rights reserved.
 */

#ifndef SELF_TEST

#include "logger.h"
#include "util.h"
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char const* private_logger_signature = "logger";

char const* debug_level_str[debug_level_t_count] = {
  "debug",
  "info",
  "warn",
  "error",
  "fatal"
};

typedef struct struct_logger_rep_t
{
  char const* signature;

  char * name;
  FILE * stream;
  int ct[debug_level_t_count];
  debug_level_t threshold;
} logger_rep_t;

logger_t *
new_logger (char const* name)
{
  logger_rep_t * ret = malloc (sizeof (logger_rep_t));
  if (ret == NULL)
    return NULL;

  ret->signature = private_logger_signature;

  ret->name = a_strdup (name);
  ret->stream = stderr;
  memset (ret->ct, 0, sizeof (ret->ct));
  ret->threshold = ll_warning;

  return (void*)ret;
}

void
delete_logger (logger_t * _logger)
{
  if (_logger != NULL)
    {
      logger_rep_t * logger = (void*)_logger;
      free (logger->name);
      free (logger);
    }
}

logger_t *
logger (void * ptr)
{
  if (((logger_rep_t*)ptr)->signature == private_logger_signature)
    return ptr;
  else
    return NULL;
}

int
log_printf (logger_t * _logger, debug_level_t level,
	    char const* fmt, ...)
{
  logger_rep_t * logger = (void*)_logger;
  int ret = 0;
  va_list ap;
  va_start (ap, fmt);

  if (logger->stream != NULL
      && level >= logger->threshold)
    {
      ret += fprintf (logger->stream, "%s:\t%s:\t", logger->name, debug_level_str[level]);
      ret += vfprintf (logger->stream, fmt, ap);
      ret += fprintf (logger->stream, "\n");
      ++ logger->ct[level];
    }

  va_end (ap);
  return ret;
}

void
log_set_filter (logger_t * logger, debug_level_t level)
{
  ((logger_rep_t *)(void *)logger)->threshold = level;
}

void
log_set_stream (logger_t * logger, FILE * stream)
{
  ((logger_rep_t *)(void *)logger)->stream = stream;
}

int
log_count_messages (logger_t const* _logger, debug_level_t level)
{
  int ret = 0;
  logger_rep_t const* logger = (void*)_logger;

  for (debug_level_t l = level; l < debug_level_t_count; ++l)
    ret += logger->ct[l];

  return ret;
}

#else /* SELF_TEST */

#include "logger.h"
#include <stdio.h>
#include <assert.h>

int
main (void)
{
  logger_t * log_test = new_logger ("test");
  assert (logger (log_test));

  int ret;

  ret = log_printf (log_test, ll_debug,
		    "This is testing message #%d.",
		    log_count_messages (log_test, 0));
  assert (ret == 0);

  ret = log_printf (log_test, ll_warning,
		    "This is testing message #%d.",
		    log_count_messages (log_test, 0));
  assert (ret > 0);

  ret = log_printf (log_test, ll_error,
		    "This is testing message #%d.",
		    log_count_messages (log_test, 0));
  assert (ret > 0);

  log_set_filter (log_test, ll_error);
  ret = log_printf (log_test, ll_error,
		    "This is testing message #%d.",
		    log_count_messages (log_test, 0));
  assert (ret > 0);

  log_set_filter (log_test, ll_debug);
  ret = log_printf (log_test, ll_error,
		    "This is testing message #%d.",
		    log_count_messages (log_test, 0));
  assert (ret > 0);

  ret = log_printf (log_test, ll_debug,
		    "This is testing message #%d.",
		    log_count_messages (log_test, 0));
  assert (ret > 0);

  log_set_stream (log_test, NULL);
  ret = log_printf (log_test, ll_fatal_error,
		    "This is testing message #%d.",
		    log_count_messages (log_test, 0));
  assert (ret == 0);

  assert (log_count_messages (log_test, 0) == 5);

  delete_logger (log_test);

  printf ("All passed.\n");
  return 0;
}

#endif
