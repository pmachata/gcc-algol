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
#include <math.h>
#include <limits.h>

static char const* private_lexer_signature = "lexer";

typedef struct struct_lexer_rep_t
{
  char const* signature;

  logger_t * log;
  logger_t * toklog;
  cursor_t * cursor;
  yyscan_t flex_scanner;
  FILE * stream;
  int managed;
  token_kind_t current_tok;
  token_kind_t prev_tok;
} lexer_rep_t;

static void private_lexer_invalid_character (lexer_rep_t * rep, char what);
static token_kind_t private_lexer_got_token (void * flex_scanner, token_kind_t token_kind);
static token_kind_t private_lexer_got_eof_token (void * flex_scanner, char const* error);
static token_kind_t private_lexer_got_real (void * flex_scanner, YYSTYPE * val, char const* expstr);

#define YY_EXTRA_TYPE lexer_rep_t * 

static int string_level = 0;
%}

%x STRING
%x COMMENT
%x END_COMMENT

%option noyywrap
%option bison-bridge
%option nodefault
%option case-insensitive

 /* primitives (RRA60 2.1, 2.2) */
SPACE		[ \t\n\r\v\f]
LETTER		[a-zA-Z]
DIGIT		[0-9]

%%

 /* arithmetic operators */
"'plus'"|"+" {
  return private_lexer_got_token (yyscanner, AOPADD);
}

"'minus'"|"-" {
  return private_lexer_got_token (yyscanner, AOPSUB);
}

"'times'"|"*" {
  return private_lexer_got_token (yyscanner, AOPMUL);
}

"'over'"|"/" {
  return private_lexer_got_token (yyscanner, AOPRDIV);
}

"'div'" {
  return private_lexer_got_token (yyscanner, AOPIDIV);
}

"'pow'"|"**" {
  return private_lexer_got_token (yyscanner, AOPPOW);
}

 /* relational operators */
"'less'"|"<"  {
  return private_lexer_got_token (yyscanner, ROPLT);
}

"'notgreater'"|"<=" {
  return private_lexer_got_token (yyscanner, ROPLTE);
}

"'equal'"|"="  {
  return private_lexer_got_token (yyscanner, ROPEQ);
}

"'notless'"|">=" {
  return private_lexer_got_token (yyscanner, ROPGTE);
}

"'greater'"|">"  {
  return private_lexer_got_token (yyscanner, ROPGT);
}

"'notequal'"|"!=" {
  return private_lexer_got_token (yyscanner, ROPNEQ);
}

 /* logical operators */
"'equiv'"|"==" {
  return private_lexer_got_token (yyscanner, LOPEQ);
}

"'impl'"|"=>" {
  return private_lexer_got_token (yyscanner, LOPIMP);
}

"'or'"|"||" {
  return private_lexer_got_token (yyscanner, LOPOR);
}

"'and'"|"&&" {
  return private_lexer_got_token (yyscanner, LOPAND);
}

"'not'"|"!"  {
  return private_lexer_got_token (yyscanner, LOPNOT);
}

 /* brackets */
"(" {
  return private_lexer_got_token (yyscanner, SEPLPAREN);
}

")" {
  return private_lexer_got_token (yyscanner, SEPRPAREN);
}

"[" {
  return private_lexer_got_token (yyscanner, SEPLBRACK);
}

"]" {
  return private_lexer_got_token (yyscanner, SEPRBRACK);
}

 /* separators */
"," {
  return private_lexer_got_token (yyscanner, SEPCOMMA);
}

":" {
  return private_lexer_got_token (yyscanner, SEPCOLON);
}

":=" {
  return private_lexer_got_token (yyscanner, SEPASSIGN);
}

";" {
  return private_lexer_got_token (yyscanner, SEPSEMICOLON);
}

 /* keywords */
"'array'" {
  return private_lexer_got_token (yyscanner, KWARRAY);
}

"'begin'" {
  return private_lexer_got_token (yyscanner, KWBEGIN);
}

"'boolean'" {
  return private_lexer_got_token (yyscanner, KWBOOLEAN);
}

"'do'" {
  return private_lexer_got_token (yyscanner, KWDO);
}

"'else'" {
  return private_lexer_got_token (yyscanner, KWELSE);
}

"'end'" {
  BEGIN END_COMMENT;
  return private_lexer_got_token (yyscanner, KWEND);
}

"'false'" {
  return private_lexer_got_token (yyscanner, KWFALSE);
}

"'for'" {
  return private_lexer_got_token (yyscanner, KWFOR);
}

"'goto'" {
  return private_lexer_got_token (yyscanner, KWGOTO);
}

"'if'" {
  return private_lexer_got_token (yyscanner, KWIF);
}

"'integer'" {
  return private_lexer_got_token (yyscanner, KWINTEGER);
}

"'label'" {
  return private_lexer_got_token (yyscanner, KWLABEL);
}

"'own'" {
  return private_lexer_got_token (yyscanner, KWOWN);
}

"'procedure'" {
  return private_lexer_got_token (yyscanner, KWPROCEDURE);
}

"'true'"  {
  return private_lexer_got_token (yyscanner, KWTRUE);
}

"'real'" {
  return private_lexer_got_token (yyscanner, KWREAL);
}

"'step'" {
  return private_lexer_got_token (yyscanner, KWSTEP);
}

"'string'" {
  return private_lexer_got_token (yyscanner, KWSTRING);
}

"'switch'" {
  return private_lexer_got_token (yyscanner, KWSWITCH);
}

"'then'" {
  return private_lexer_got_token (yyscanner, KWTHEN);
}

"'until'" {
  return private_lexer_got_token (yyscanner, KWUNTIL);
}

"'value'" {
  return private_lexer_got_token (yyscanner, KWVALUE);
}

"'while'" {
  return private_lexer_got_token (yyscanner, KWWHILE);
}


 /* strings */
 /* Parsing string literal is somewhat kinky in ALGOL 60, as it allows
    nested forms.  Thus we have to count opening `s and closing 's,
    and collect everything that's inbetween to yylval->slit. */

"`" {
  BEGIN STRING;
  string_level = 1;
  yylval->slit = new_estring ();
}

<STRING>("`"+) {
  lexer_rep_t * lexer = yyget_extra (yyscanner);
  string_level += yyget_leng (yyscanner);
  if (estr_append_cstr (yylval->slit, yyget_text (yyscanner)) < 0)
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
      if (estr_append_cstr (yylval->slit, yyget_text (yyscanner)) < 0)
	log_printf (lexer->log, ll_fatal_error, "literal append failure");
    }
}

<STRING>([^`\']+) {
  lexer_rep_t * lexer = yyget_extra (yyscanner);
  if (estr_append_cstr (yylval->slit, yyget_text (yyscanner)) < 0)
    log_printf (lexer->log, ll_fatal_error, "literal append failure");
}


 /* comments */
 /* There are two kinds of comments.  Most of comments are introduced
    by the keyword 'comment', which is legal after semicolon and
    'begin' keyword.  This is <COMMENT> part.  There are also 'end'
    comments--strings between keywords 'end', and either 'else', 'end'
    or semicolon, whichever comes first.  Thes is <END_COMMENT>
    part. */

"'comment'" {
  lexer_rep_t * lexer = yyget_extra (yyscanner);

  switch (lexer->prev_tok) {
  case KWBEGIN:
  case SEPSEMICOLON:
    break;
  default:
    log_printf (lexer->log, ll_error, "Invalid comment context.");
  };

  BEGIN COMMENT;
}

<COMMENT>";" {
  BEGIN INITIAL;
}

<COMMENT>[^;]+ {
  // ignore for now
}

<END_COMMENT>"'else'" {
  BEGIN INITIAL;
  return private_lexer_got_token (yyscanner, KWELSE);
}
<END_COMMENT>"'end'" {
  return private_lexer_got_token (yyscanner, KWEND);
}
<END_COMMENT>"'"[^\' ]*"'" {
  lexer_rep_t * lexer = yyget_extra (yyscanner);
  if (strcmp (yyget_text (yyscanner), "'comment'") != 0)
    log_printf (lexer->log, ll_warning,
		"implicit 'end'-comment possibly contains keyword %s",
		yyget_text (yyscanner));
}
<END_COMMENT>";" {
  BEGIN INITIAL;
  return private_lexer_got_token (yyscanner, SEPSEMICOLON);
}
<END_COMMENT>[^\';]+ {
  // ignore for now, later we may collect comments
}


 /* identifier */
{LETTER}({LETTER}|{DIGIT})* {
  yylval->slit = new_estring_from (yyget_text (yyscanner));
  return private_lexer_got_token (yyscanner, IDENTIFIER);
}


 /* numbers */
"."   {
  // explicitly forbid single dot, which would otherwise get
  // classified as floating point number
  lexer_rep_t * lexer = yyget_extra (yyscanner);
  private_lexer_invalid_character (lexer, *yyget_text (yyscanner));
}

 /* Real numbers take up many different forms in ALGOL.  For the sake
    of clarity, the rule was broken to several instances, covering the
    various formats.  The action taken was moved to function
    private_lexer_got_real, where further parsing takes place.*/

([0-9]+"."[0-9]*) {
  return private_lexer_got_real (yyscanner, yylval, NULL);
}
([0-9]*"."[0-9]+) {
  return private_lexer_got_real (yyscanner, yylval, NULL);
}
([0-9]*"'e'"[+-]?[0-9]+) {
  return private_lexer_got_real (yyscanner, yylval, "'e'");
}
([0-9]*"'E'"[+-]?[0-9]+) {
  return private_lexer_got_real (yyscanner, yylval, "'E'");
}
(([0-9]+"."[0-9]*|[0-9]*"."[0-9]+)"'e'"[+-]?[0-9]+) {
  return private_lexer_got_real (yyscanner, yylval, "'e'");
}
(([0-9]+"."[0-9]*|[0-9]*"."[0-9]+)"'E'"[+-]?[0-9]+) {
  return private_lexer_got_real (yyscanner, yylval, "'E'");
}

 /* Integer numbers, on the other hand, are quite simple.  We don't
    allow C-ish 0x form, but we may allow it sometime in future. */
[0-9]+ {
  lexer_rep_t * lexer = yyget_extra (yyscanner);
  char * lit = yyget_text (yyscanner);
  char * pos;
  long value = strtol (lit, &pos, 10);

  if ((value == LONG_MIN
       || value == LONG_MAX)
      && errno == ERANGE)
    log_printf (lexer->log, ll_warning, "Too big an integer: `%s'", lit);

  if (*pos != 0)
    log_printf (lexer->log, ll_error, "Invalid integer: `%s'", lit);

  yylval->ilit = value;

  return private_lexer_got_token (yyscanner, LITINTEGER);
}



 /* Handle EOF token.  Check if we are not in the middle of something,
    and produce an error if we are. */
<COMMENT><<EOF>> {
  return private_lexer_got_eof_token (yyscanner, "comment");
}
<END_COMMENT><<EOF>> {
  return private_lexer_got_eof_token (yyscanner, "end-comment");
}
<STRING><<EOF>> {
  return private_lexer_got_eof_token (yyscanner, "string");
}
<<EOF>> {
  return private_lexer_got_eof_token (yyscanner, NULL);
}


 /* Whitespace */
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


 /* Unmatched characters */
"'"[^\']+"'" {
  lexer_rep_t * lexer = yyget_extra (yyscanner);
  log_printf (lexer->log, ll_error,
	      "probably misspelled keyword: %s",
	      yyget_text (yyscanner));
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
      ret->signature = private_lexer_signature;

      guard_ptr (buf, 1, ret->toklog = new_logger ("token"));
      guard_ptr (buf, 1, ret->log = new_logger ("lexer"));
      guard_ptr (buf, 1, ret->cursor = new_cursor (filename));

      guard_int (buf, 1, yylex_init (&ret->flex_scanner));
      yyset_extra (ret, ret->flex_scanner);

      ret->stream = stream;
      yyrestart (stream, ret->flex_scanner);

      ret->current_tok = -1;
      ret->prev_tok = -1;
      ret->managed = manage;
      return (void*)ret;
    }
  else
    {
      delete_logger (ret->log);
      delete_cursor (ret->cursor);
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
  if (_lexer != NULL)
    {
      lexer_rep_t * lexer = (void*)_lexer;
      if (lexer->managed)
	fclose (lexer->stream);

      if (yylex_destroy (lexer->flex_scanner))
	perror ("yylex_destroy");

      delete_logger (lexer->log);
      delete_cursor (lexer->cursor);

      free (lexer);
    }
}

lexer_t *
lexer (void * ptr)
{
  if (((lexer_rep_t*)ptr)->signature == private_lexer_signature)
    return ptr;
  else
    return NULL;
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
  cursor_move (lexer->cursor, yyget_leng (flex_scanner));

  log_printf (lexer->toklog, ll_debug,
	      "%s: hit [tok:%d] [lex:%s]",
	      cursor_to_str (lexer->cursor),
	      token_kind, lexeme);

  return token_kind;
}

/// Process EOF token, i.e. write an error message if EOF was inside
/// of something, and then push EOF token via private_lexer_got_token.
static token_kind_t
private_lexer_got_eof_token (void * flex_scanner, char const* error)
{
  if (error != NULL)
    {
      lexer_rep_t * lexer = yyget_extra (flex_scanner);
      log_printf (lexer->log, ll_error, "EOF hit inside %s.", error);
    }
  return private_lexer_got_token (flex_scanner, EOFTOK);
}

static token_kind_t
private_lexer_got_real (void * flex_scanner, YYSTYPE * val, char const* expstr)
{
  lexer_rep_t * lexer = yyget_extra (flex_scanner);
  char * lit = yyget_text (flex_scanner);
  char * pos;

  if ((pos = strstr (lit, expstr)) != NULL)
    {
      *pos = 0; // trim original literal
      pos += strlen (expstr); // move pointer to the beginning of
                              // exponential part
      // ugly, but safe
      sprintf (lit, "%se%s", ((*lit == 0)?"1":lit), pos);
    }

  double value = strtod (lit, &pos);

  if ((value == +HUGE_VAL
       || value == -HUGE_VAL)
      && errno == ERANGE)
    log_printf (lexer->log, ll_warning, "Too big a number: `%s'", lit);

  if (*pos != 0)
    log_printf (lexer->log, ll_error, "Invalid number: `%s'", lit);

  val->dlit = value;

  return private_lexer_got_token (flex_scanner, LITREAL);
}



int
lexer_tok (lexer_t * _lexer, YYSTYPE * val)
{
  assert (_lexer != NULL);
  lexer_rep_t * lexer = (void*)_lexer;
  lexer->prev_tok = lexer->current_tok;
  lexer->current_tok = yylex (val, lexer->flex_scanner);
  return lexer->current_tok;
}

token_kind_t
lexer_get_tok_kind (lexer_t * _lexer)
{
  assert (_lexer != NULL);
  lexer_rep_t * lexer = (void*)_lexer;
  return lexer->current_tok;
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

logger_t const*
lexer_log (lexer_t * _lexer)
{
  assert (_lexer != NULL);
  lexer_rep_t * lexer = (void*)_lexer;
  return (logger_t const*)lexer->log;
}
