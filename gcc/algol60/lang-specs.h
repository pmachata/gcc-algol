/* This is the contribution to the `default_compilers' array in gcc.c
   for the Algol 60 language.  */

{".a60",   "@a60", 0, 0, 0},
{".A60",   "@a60", 0, 0, 0},
{"@a60",   "%{!E:algol601 %i %(cc1_options) %{I*}\
             %{!fsyntax-only:%(invoke_as)}}\n", 0, 0, 0},

/* @@@TODO: maybe make A60 files be preprocessed first
   (a60-cpp-input) */
