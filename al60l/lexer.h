/*
 * Copyright (c) 2006 Petr Machata
 * All rights reserved.
 */

#ifndef _AL60L_LEXER_H_
#define _AL60L_LEXER_H_

//for references in parser-tab.h
#include "ast-tab.h"
#include "slist.h"

// direct includes
#include "parser-tab.h"
#include "pd.h"
#include "estring.h"
#include "logger.h"
#include <stdio.h>

typedef struct struct_lexer_t { } lexer_t;
typedef enum yytokentype token_kind_t;

/// Allocate and return new lexer, operating on given stream `file',
/// whose name is `filename'.  If `manage' is non-zero, it will be
/// closed upon destruction of this lexer.
///
/// \arg file The stream which the lexer should use to fetch tokens.
///
/// \arg filename The name of file underlying the stream.  May be
/// arbitrary descriptor, if the stream doesn originate in file.  May
/// not be NULL.
///
/// \arg manage Whether the stream should be fclosed when the lexer is
/// destroyed.
lexer_t * new_lexer (FILE * stream, char const* filename, int manage)
     MALLOC_LIKE
     ARG_NONNULL(1)
     ARG_NONNULL(2);

/// Opens the file with given name and returns new stream that
/// operates on that file.  The stream is managed (in sense of
/// new_lexer).  Returns NULL when an error occurs.
lexer_t * new_lexer_filename (char const* filename)
     MALLOC_LIKE
     ARG_NONNULL(1);

/// Destroy a lexer.
void delete_lexer (lexer_t * lexer);

/// Convert void* to lexer, if it is lexer, or return NULL.
lexer_t * lexer (void * ptr)
     ARG_NONNULL(1);


/// Get a new token.  In contrast to the rest of interfaces, this
/// modifies internal state instead of returning new object of type
/// token.  Token kind and lexeme are stored into the passed-in lexer
/// and can be requested later by lexer_get_token_kind and
/// lexer_get_token_lexeme.
void lexer_next_tok (lexer_t * lexer)
     ARG_NONNULL(1);

/// Answer the kind of last token
token_kind_t lexer_get_tok_kind (lexer_t * lexer)
     ARG_NONNULL(1);

/// Answer the lexeme that formed last token.
char const* lexer_get_tok_lexeme (lexer_t * lexer)
     ARG_NONNULL(1);

/// Answer the contents of LITSTRING token.  Returned value has to be
/// cloned if it is to be used further, as lexer will rewrite it at
/// next iteration.  The function checks whether last token held
/// string at all, and an assertion fails if not.
estring_t * lexer_get_tok_literal (lexer_t * lexer)
     ARG_NONNULL(1);

/// Answer the length of last lexeme.
int lexer_get_tok_lexeme_len (lexer_t * lexer)
     ARG_NONNULL(1);

/// Answer the number associated with the previous token, if there is
/// any.  The function checks whether last token held number at all,
/// and an assertion fails if not.
double lexer_get_tok_number (lexer_t * lexer)
     ARG_NONNULL(1);

/// Similar to lexer_get_tok_number, but for integer values.
long lexer_get_tok_integer (lexer_t * lexer)
     ARG_NONNULL(1);

/// Controls what logging messages get printed.  `Messages' controls
/// what error reporting is done.  `Tokens' controls whether debug
/// info about processed tokens gets printed.
void lexer_set_logging (lexer_t * lexer, debug_level_t messages, int tokens)
     ARG_NONNULL(1);

/// Get underlying logging device.
logger_t const* lexer_log (lexer_t * lexer)
     ARG_NONNULL(1);

#endif//_AL60L_LEXER_H_
