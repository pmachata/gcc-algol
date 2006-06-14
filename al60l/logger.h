/*
 * Copyright (c) 2006 Petr Machata
 * All rights reserved.
 */

#ifndef _AL60L_DEBUG_H_
#define _AL60L_DEBUG_H_

#include <stdio.h>
#include "pd.h"

/// The description of the severity of log message.  Used for
/// filtering.
typedef enum enum_debug_level_t
{
  ll_debug,             ///< Debug level information
  ll_info,              ///< General progress information
  ll_warning,           ///< Compilation warnings
  ll_error,             ///< Compilation errors
  ll_fatal_error,       ///< Fatal errors, usually leading to compiler halt
  debug_level_t_count
} debug_level_t;

typedef struct struct_logger_t { } logger_t;

/// Allocate new debug log context.  The default output stream is
/// stderr, the default threshold level is ll_warning.
logger_t * new_logger (char const* name)
     MALLOC_LIKE
     ARG_NONNULL(1);

/// Destroy the log.
void delete_logger (logger_t * logger)
     ARG_NONNULL(1);


/// This function is used for logging compiler messages.  Depending on
/// designated severity and source and effective filtering rules, the
/// function may or may not actually print anything to screen (or
/// file).  E.g. with level=ll_debug and filter set to ll_warning, the
/// message will be swallowed.
///
/// Old-style printf is used under the hoods, so the format string is
/// perfectly printf-like.
///
/// \arg logger The logger to which the message should be sent.
/// \arg level Severity of message.
/// \arg format_string printf-like formatting string.  The arguments
/// following this one are objects that should be formatted.
///
/// \return Like printf, the function returns a number of characters
/// written.  If an error is encountered, a negative number is
/// answered.  Filtered out messages return 0.
int log_printf (logger_t * logger, debug_level_t level,
		char const* format_string, ...)
     PRINTF_LIKE(3,4)
     ARG_NONNULL(1);


/// Use this function to set up message filtering based on their
/// severity in given source.  Filtering level is ll_warning by
/// default.
///
/// \arg logger The logger for which the filter should be set up.
///
/// \arg level The lowest severity that the message from given source
/// has to have to get displayed.  Application will print out all
/// messages with severity higher or the same as provided level.
void log_set_filter (logger_t * logger, debug_level_t level)
     ARG_NONNULL(1);


/// Set the stream where the output should be sent.
///
/// \arg logger The logger for which the stream should be set.  NULL
/// means don't output debug messages at all.
///
/// \arg stream The stream where unfiltered output will appear on.
void log_set_stream (logger_t * logger, FILE * stream)
     ARG_NONNULL(1);


/// Answer count of messages with a severity higher than or same as
/// given level.
///
/// \arg logger The logger for which the count should be given.
///
/// \arg level The lower bound of severity of counted messages.
///
/// \return Number of messages.
int log_count_messages (logger_t * logger, debug_level_t level)
     ARG_NONNULL(1);


/// Like debug_count_above, except that it counts messages across the
/// whole debugging pool.
///
/// \arg level The lower bound of severity of counted messages.
///
/// \return Number of messages.
int log_count_pool_messages (debug_level_t level);

#endif //_AL60L_DEBUG_H_
