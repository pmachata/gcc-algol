/* :mode=c: -*-c-*-
 * Copyright (c) 2005,2006 Petr Machata
 * All rights reserved.
 */
%{
#include "cursor.h"
#include "logger.h"
#include "util.h"
#include "lexer.h"
#include "estring.h"

#include "parser-tab.h"

#include <stdio.h>
#include <assert.h>
#include <errno.h>
#include <setjmp.h>

typedef struct struct_lexer_rep_t
{
  logger_t * log;
  logger_t * toklog;
  cursor_t * cursor;
  yyscan_t flex_scanner;
  FILE * stream;
  int managed;
  token_kind_t last_tok;

  /// Usually the lexeme is stored using flex-internal buffers.
  /// However in certain cases, this does not work very well.
  /// E.g. strings have to be parsed in a special way, processing one
  /// character at a time.  That data are stored in `literal' field.
  estring_t * literal;
} lexer_rep_t;

static void private_lexer_invalid_character (lexer_rep_t * rep, char what);
static token_kind_t private_lexer_got_token (void * flex_scanner, token_kind_t token_kind);

#define YY_EXTRA_TYPE lexer_rep_t * 

static int string_level = 0;
%}

%x STRING

%option noyywrap

 /* primitives (RRA60 2.1, 2.2) */
SPACE		[ \t\n\r\v\f]
LETTER		[a-zA-Z]
DIGIT		[0-9]

%%

 /* arithmetic operators */
"+" { return private_lexer_got_token (yyscanner, AOPADD); }
"-" { return private_lexer_got_token (yyscanner, AOPSUB); }
"*" { return private_lexer_got_token (yyscanner, AOPMUL); }
"/" { return private_lexer_got_token (yyscanner, AOPRDIV); }
"'div'" { return private_lexer_got_token (yyscanner, AOPIDIV); }
"**" { return private_lexer_got_token (yyscanner, AOPPOW); }

 /* relational operators */
"<"  { return private_lexer_got_token (yyscanner, ROPLT); }
"<=" { return private_lexer_got_token (yyscanner, ROPLTE); }
"="  { return private_lexer_got_token (yyscanner, ROPEQ); }
">=" { return private_lexer_got_token (yyscanner, ROPGTE); }
">"  { return private_lexer_got_token (yyscanner, ROPGT); }
"!=" { return private_lexer_got_token (yyscanner, ROPNEQ); }

 /* logical operators */
"==" { return private_lexer_got_token (yyscanner, LOPEQ); }
"=>" { return private_lexer_got_token (yyscanner, LOPIMP); }
"||" { return private_lexer_got_token (yyscanner, LOPOR); }
"&&" { return private_lexer_got_token (yyscanner, LOPAND); }
"!"  { return private_lexer_got_token (yyscanner, LOPNOT); }

 /* brackets */
"(" { return private_lexer_got_token (yyscanner, SEPLPAREN); }
")" { return private_lexer_got_token (yyscanner, SEPRPAREN); }
"[" { return private_lexer_got_token (yyscanner, SEPLBRACK); }
"]" { return private_lexer_got_token (yyscanner, SEPRBRACK); }

 /* separators */
"," { return private_lexer_got_token (yyscanner, SEPCOMMA); }
":" { return private_lexer_got_token (yyscanner, SEPCOLON); }
":=" { return private_lexer_got_token (yyscanner, SEPASSIGN); }
";" { return private_lexer_got_token (yyscanner, SEPSEMICOLON); }

 /* keywords */
"'array'" { return private_lexer_got_token (yyscanner, KWARRAY); }
"'begin'" { return private_lexer_got_token (yyscanner, KWBEGIN); }
"'Boolean'" { return private_lexer_got_token (yyscanner, KWBOOLEAN); }
"'comment'" { return private_lexer_got_token (yyscanner, KWCOMMENT); }
"'do'"   { return private_lexer_got_token (yyscanner, KWDO); }
"'else'" { return private_lexer_got_token (yyscanner, KWELSE); }
"'end'" { return private_lexer_got_token (yyscanner, KWEND); }
"'false'" { return private_lexer_got_token (yyscanner, KWFALSE); }
"'for'"  { return private_lexer_got_token (yyscanner, KWFOR); }
"'goto'" { return private_lexer_got_token (yyscanner, KWGOTO); }
"'if'"   { return private_lexer_got_token (yyscanner, KWIF); }
"'integer'" { return private_lexer_got_token (yyscanner, KWINTEGER); }
"'label'" { return private_lexer_got_token (yyscanner, KWLABEL); }
"'own'" { return private_lexer_got_token (yyscanner, KWOWN); }
"'procedure'" { return private_lexer_got_token (yyscanner, KWPROCEDURE); }
"'true'"  { return private_lexer_got_token (yyscanner, KWTRUE); }
"'real'" { return private_lexer_got_token (yyscanner, KWREAL); }
"'step'" { return private_lexer_got_token (yyscanner, KWSTEP); }
"'string'" { return private_lexer_got_token (yyscanner, KWSTRING); }
"'switch'" { return private_lexer_got_token (yyscanner, KWSWITCH); }
"'then'" { return private_lexer_got_token (yyscanner, KWTHEN); }
"'until'" { return private_lexer_got_token (yyscanner, KWUNTIL); }
"'value'" { return private_lexer_got_token (yyscanner, KWVALUE); }
"'while'" { return private_lexer_got_token (yyscanner, KWWHILE); }

 /* strings */
 // Parsing string literal is somewhat kinky in ALGOL 60, for it
 // allows nested forms.  Thus we have to count opening `s and closing
 // 's, and collect everything that's inbetween to lexer->literal.
"`" {
  BEGIN STRING;
  string_level = 1;
  lexer_rep_t * lexer = yyget_extra (yyscanner);
  estr_clear (lexer->literal);
}

<STRING>("`"+) {
  lexer_rep_t * lexer = yyget_extra (yyscanner);
  string_level += yyget_leng (yyscanner);
  if (estr_append_cstr (lexer->literal, yyget_text (yyscanner)) < 0)
    log_printf (lexer->log, ll_fatal_error, "literal append failure");
}

<STRING>"'" {
  if (--string_level == 0)
    {
      BEGIN INITIAL;
      return private_lexer_got_token (yyscanner, LITSTRING);
    }
  else
    {
      lexer_rep_t * lexer = yyget_extra (yyscanner);
      if (estr_append_cstr (lexer->literal, yyget_text (yyscanner)) < 0)
	log_printf (lexer->log, ll_fatal_error, "literal append failure");
    }
}

<STRING>([^`\']+) {
  lexer_rep_t * lexer = yyget_extra (yyscanner);
  if (estr_append_cstr (lexer->literal, yyget_text (yyscanner)) < 0)
    log_printf (lexer->log, ll_fatal_error, "literal append failure");
}


 /* identifier */
{LETTER}({LETTER}|{DIGIT})* {
  return private_lexer_got_token (yyscanner, IDENTIFIER);
}


 /* numbers */
"."   {
  // explicitly forbid single dot, which would otherwise get
  // classified as floating point number
  lexer_rep_t * lexer = yyget_extra (yyscanner);
  private_lexer_invalid_character (lexer, *yyget_text (yyscanner));
}

([0-9]+"."[0-9]*)|([0-9]*"."[0-9]+)|([0-9]*"'e'"[+-]?[0-9]+)|([0-9]+"."[0-9]*"'e'"[+-]?[0-9]+)|([0-9]*"."[0-9]+"'e'"[+-]?[0-9]+) {
  return private_lexer_got_token (yyscanner, LITFLOAT);
  /*
  yylval.litfloat = atof (yytext);
  csr_append (Lexer::g_cursor, yytext);
  return LITFLOAT;
  */
}

[0-9]+ {
  return private_lexer_got_token (yyscanner, LITINTEGER);
  /*
  yylval.litinteger = atol (yytext);
  csr_append (Lexer::g_cursor, yytext); return LITINTEGER;
  */
}

[A-Za-z_][A-Za-z0-9_]* {
  /*
  csr_append (Lexer::g_cursor, yytext);
  yylval.name = new std::string (yytext);
  return IDENTIFIER;
  */
}

<<EOF>>	{ return private_lexer_got_token (yyscanner, EOFTOK); }

\n {
  lexer_rep_t * lexer = yyget_extra (yyscanner);
  cursor_nl (lexer->cursor);
}

" "+ {
  lexer_rep_t * lexer = yyget_extra (yyscanner);
  cursor_move (lexer->cursor, yyget_leng (yyscanner));
}

\t {
  lexer_rep_t * lexer = yyget_extra (yyscanner);
  cursor_tab (lexer->cursor, 8);
}

. {
  lexer_rep_t * lexer = yyget_extra (yyscanner);
  cursor_move (lexer->cursor, 1);
  private_lexer_invalid_character (lexer, *yytext);
}

%%

lexer_t *
new_lexer (FILE * stream, char const* filename, int manage)
{
  lexer_rep_t * ret = malloc (sizeof (lexer_rep_t));
  memset (ret, 0, sizeof (lexer_rep_t));
  jmp_buf buf;

  if (setjmp (buf) == 0)
    {
      guard_ptr (buf, 1, ret->toklog = new_logger ("token"));
      guard_ptr (buf, 1, ret->log = new_logger ("lexer"));
      guard_ptr (buf, 1, ret->cursor = new_cursor (filename));
      guard_ptr (buf, 1, ret->literal = new_estring ());

      guard_int (buf, 1, yylex_init (&ret->flex_scanner));
      yyset_extra (ret, ret->flex_scanner);

      ret->stream = stream;
      yyrestart (stream, ret->flex_scanner);

      return (void*)ret;
    }
  else
    {
      delete_logger (ret->log);
      delete_cursor (ret->cursor);
      delete_estring (ret->literal);
      // yylex_init is called the last, and nothing will fail
      // afterwards.  So we don't call yylex_destroy at all.
      free (ret);
      return NULL;
    }
}

lexer_t *
new_lexer_filename (char const* filename)
{
  FILE * f = fopen (filename, "r");
  if (f == NULL)
    return NULL;

  return new_lexer (f, filename, 1);
}

void
delete_lexer (lexer_t * _lexer)
{
  lexer_rep_t * lexer = (void*)_lexer;
  if (lexer->managed)
    fclose (lexer->stream);

  if (yylex_destroy (lexer->flex_scanner))
    perror ("yylex_destroy");

  delete_logger (lexer->log);
  delete_cursor (lexer->cursor);
  delete_estring (lexer->literal);

  free (lexer);
}

/// Procedure to write out that bad character has been hit.
/// Bad characters are considered errors.
static void
private_lexer_invalid_character (lexer_rep_t * rep, char what)
{
  log_printf (rep->log, ll_error, "Invalid character `%c' (code %d)\n", what, (int)what);
}

/// Process new token.
static token_kind_t
private_lexer_got_token (void * flex_scanner, token_kind_t token_kind)
{
  lexer_rep_t * lexer = yyget_extra (flex_scanner);
  char const* lexeme = yyget_text (flex_scanner);
  char const* literal = estr_cstr (lexer->literal);
  cursor_move (lexer->cursor, yyget_leng (flex_scanner));

  log_printf (lexer->toklog, ll_debug,
	      "%s: hit [tok:%d] [lex:%s] [lit:%s]",
	      cursor_to_str (lexer->cursor),
	      token_kind, lexeme, literal);

  return token_kind;
}



void
lexer_next_tok (lexer_t * _lexer)
{
  assert (_lexer != NULL);
  lexer_rep_t * lexer = (void*)_lexer;
  lexer->last_tok = yylex (lexer->flex_scanner);
}

token_kind_t
lexer_get_tok_kind (lexer_t * _lexer)
{
  assert (_lexer != NULL);
  lexer_rep_t * lexer = (void*)_lexer;
  return lexer->last_tok;
}

char const*
lexer_get_tok_lexeme (lexer_t * _lexer)
{
  assert (_lexer != NULL);
  lexer_rep_t * lexer = (void*)_lexer;
  return yyget_text (lexer->flex_scanner);
}

int
lexer_get_tok_lexeme_len (lexer_t * _lexer)
{
  assert (_lexer != NULL);
  lexer_rep_t * lexer = (void*)_lexer;
  return yyget_leng (lexer->flex_scanner);
}

estring_t *
lexer_get_tok_literal (lexer_t * _lexer)
{
  assert (_lexer != NULL);
  lexer_rep_t * lexer = (void*)_lexer;
  return lexer->literal;
}

void
lexer_set_logging (lexer_t * _lexer, debug_level_t messages, int tokens)
{
  assert (_lexer != NULL);
  lexer_rep_t * lexer = (void*)_lexer;
  log_set_filter (lexer->log, messages);
  if (tokens)
    log_set_filter (lexer->toklog, ll_filter_nothing);
  else
    log_set_filter (lexer->toklog, ll_filter_all);
}
