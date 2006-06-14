/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     EOFTOK = 258,
     KWTRUE = 259,
     KWFALSE = 260,
     AOPADD = 261,
     AOPSUB = 262,
     AOPMUL = 263,
     AOPRDIV = 264,
     AOPIDIV = 265,
     AOPPOW = 266,
     ROPLT = 267,
     ROPLTE = 268,
     ROPEQ = 269,
     ROPGT = 270,
     ROPGTE = 271,
     ROPNEQ = 272,
     LOPEQ = 273,
     LOPIMP = 274,
     LOPOR = 275,
     LOPAND = 276,
     LOPNOT = 277,
     KWGOTO = 278,
     KWIF = 279,
     KWTHEN = 280,
     KWELSE = 281,
     KWFOR = 282,
     KWDO = 283,
     SEPCOMMA = 284,
     SEPPERIOD = 285,
     SEP10E = 286,
     SEPCOLON = 287,
     SEPSEMICOLON = 288,
     SEPASSIGN = 289,
     KWSTEP = 290,
     KWUNTIL = 291,
     KWWHILE = 292,
     KWCOMMENT = 293,
     SEPLPAREN = 294,
     SEPRPAREN = 295,
     SEPLBRACK = 296,
     SEPRBRACK = 297,
     SEPLQUOT = 298,
     SEPRQUOT = 299,
     KWBEGIN = 300,
     KWEND = 301,
     KWOWN = 302,
     KWBOOLEAN = 303,
     KWINTEGER = 304,
     KWREAL = 305,
     KWARRAY = 306,
     KWSWITCH = 307,
     KWPROCEDURE = 308,
     KWSTRING = 309,
     KWLABEL = 310,
     KWVALUE = 311,
     IDENTIFIER = 312,
     LITFLOAT = 313,
     LITINTEGER = 314,
     LITSTRING = 315
   };
#endif
/* Tokens.  */
#define EOFTOK 258
#define KWTRUE 259
#define KWFALSE 260
#define AOPADD 261
#define AOPSUB 262
#define AOPMUL 263
#define AOPRDIV 264
#define AOPIDIV 265
#define AOPPOW 266
#define ROPLT 267
#define ROPLTE 268
#define ROPEQ 269
#define ROPGT 270
#define ROPGTE 271
#define ROPNEQ 272
#define LOPEQ 273
#define LOPIMP 274
#define LOPOR 275
#define LOPAND 276
#define LOPNOT 277
#define KWGOTO 278
#define KWIF 279
#define KWTHEN 280
#define KWELSE 281
#define KWFOR 282
#define KWDO 283
#define SEPCOMMA 284
#define SEPPERIOD 285
#define SEP10E 286
#define SEPCOLON 287
#define SEPSEMICOLON 288
#define SEPASSIGN 289
#define KWSTEP 290
#define KWUNTIL 291
#define KWWHILE 292
#define KWCOMMENT 293
#define SEPLPAREN 294
#define SEPRPAREN 295
#define SEPLBRACK 296
#define SEPRBRACK 297
#define SEPLQUOT 298
#define SEPRQUOT 299
#define KWBEGIN 300
#define KWEND 301
#define KWOWN 302
#define KWBOOLEAN 303
#define KWINTEGER 304
#define KWREAL 305
#define KWARRAY 306
#define KWSWITCH 307
#define KWPROCEDURE 308
#define KWSTRING 309
#define KWLABEL 310
#define KWVALUE 311
#define IDENTIFIER 312
#define LITFLOAT 313
#define LITINTEGER 314
#define LITSTRING 315




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
#line 41 "parser.y"
{
  int un_i;
  float un_f;
}
/* Line 1529 of yacc.c.  */
#line 174 "parser-tab.h"
	YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif



