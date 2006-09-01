CC      := gcc
#CC      := gcc-4.1-rh
BISON   := bison
FLEX    := flex
TREECC  := treecc
DEBUG   := -ggdb3 -Wall -fno-inline
AR      := ar
RANLIB  := ranlib
#DEBUG   := -Wall -DNDEBUG
#PROFILE := -pg
CFLAGS  := -std=c99 -O0

# This whole shenanigan will have to be inside the gcc build system,
# to get proper include paths

ARCHIVE_NAME = xmacha31.zip
ARCHIVE_SUBDIR =
ARCHIVE_METHOD = zip -r
DOCFILE_NAME =
FILES = cursor.c cursor.h\
	logger.c logger.h\
	util.c util.h\
	lexer.l lexer.h lexer-test.c\
	parser.y parser.h\
	estring.c estring.h\
	ast.tc label.tc statement.tc symbol.tc expression.tc type.tc ast-test.c gcc.tc\
	slist.c slist.h\
	Makefile

# redefine to 1 when built as GCC frontend
BUILD_GCC_FRONTEND :=0

TOUCHSTAMP := @touch stamp

.PHONY: all doc dist clean check tests

all: al60l


util.o: util.c util.h Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -c util.c -o util.o
	$(TOUCHSTAMP)

check-util: util.o util.c Makefile
	@echo "============================================="
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -DSELF_TEST util.c util.o -o util-test
	./util-test
	rm -f ./util-test


slist.o: slist.c slist.h pd.h Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -c slist.c -o slist.o
	$(TOUCHSTAMP)

check-slist: slist.o pd.h Makefile
	@echo "============================================="
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -DSELF_TEST slist.c slist.o -o slist-test
	./slist-test
	rm -f ./slist-test


cursor.o: cursor.c cursor.h pd.h util.h Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -c cursor.c -o cursor.o
	$(TOUCHSTAMP)

check-cursor: cursor.o util.o cursor.c Makefile
	@echo "============================================="
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -DSELF_TEST cursor.c cursor.o util.o -o cursor-test
	./cursor-test
	rm -f ./cursor-test


logger.o: logger.c logger.h pd.h Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -c logger.c -o logger.o
	$(TOUCHSTAMP)

check-logger: logger.o util.o logger.c Makefile
	@echo "============================================="
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -DSELF_TEST logger.c logger.o util.o -o logger-test
	./logger-test
	rm -f ./logger-test


estring.o: estring.c estring.h pd.h Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -c estring.c -o estring.o
	$(TOUCHSTAMP)

check-estring: estring.o estring.c estring.h Makefile
	@echo "============================================="
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -DSELF_TEST estring.c estring.o -o estring-test
	./estring-test
	rm -f ./estring-test


ast-tab.c: ast.tc statement.tc symbol.tc expression.tc type.tc gcc.tc Makefile
	$(TREECC) -o ast-tab.c -h ast-tab.h ast.tc
	$(TOUCHSTAMP)

ast-tab.h: ast.tc statement.tc symbol.tc expression.tc type.tc gcc.tc ast-tab.c Makefile
	[ -f ast-tab.h ]


ast.o-x0:
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) ast-tab.c -c -o ast.o
	$(TOUCHSTAMP)

ast.o-x1:
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -DBUILD_GCC_FRONTEND ast-tab.c -c -o ast.o
	$(TOUCHSTAMP)

ast.o: ast-tab.c ast-tab.h slist.h Makefile ast.o-x$(BUILD_GCC_FRONTEND)

ast-test: ast.o slist.o logger.o util.o estring.o ast-test.c Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) ast-test.c ast.o  \
		slist.o logger.o util.o estring.o -o ast-test

check-ast: ast-test Makefile
	@echo "============================================="
	./ast-test
	rm -f ./ast-test


parser.o: parser-tab.c ast.o Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -c parser-tab.c -o parser.o
	$(TOUCHSTAMP)

parser-tab.c: parser.y parser.h lexer.h logger.h pd.h cursor.h Makefile
	$(BISON) -t -v -o parser-tab.c -d parser.y
	$(TOUCHSTAMP)

parser-tab.h: parser-tab.c
	[ -f ./parser-tab.h ]

parser-test: parser-test.c parser.o ast.o logger.o\
 lexer.o cursor.o estring.o slist.o util.o parser.h lexer.h estring.h pd.h
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -std=gnu99 -o parser-test \
		parser-test.c parser.o ast.o logger.o\
		lexer.o cursor.o estring.o slist.o util.o

al60l.a: parser-test.c parser.o ast.o logger.o\
 lexer.o cursor.o estring.o slist.o util.o parser.h lexer.h estring.h pd.h
	$(AR) cru al60l.a \
		parser.o ast.o logger.o \
		lexer.o cursor.o estring.o slist.o util.o
	$(RANLIB) al60l.a

check-parser: parser-test Makefile
	@echo "============================================="
	cd testsuite && ./torture.sh
	rm -f ./parser-test


lexer.o: lexer-tab.c Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -c lexer-tab.c -o lexer.o
	$(TOUCHSTAMP)

lexer-tab.c: lexer.l lexer.h parser-tab.h estring.h logger.h pd.h cursor.h Makefile
	$(FLEX) -R --never-interactive -o lexer-tab.c --header-file=lexer-tab.h lexer.l
	sed -i -e '/static void yyunput/s/;/__attribute__((unused));/' lexer-tab.c
	$(TOUCHSTAMP)

lexer-tab.h: lexer-tab.c
	[ -f ./lexer-tab.h ]

lexer-test: lexer-test.c logger.o lexer.o cursor.o lexer.h pd.h Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -std=gnu99 -o lexer-test \
		lexer-test.c lexer.o cursor.o logger.o util.o estring.o

check-lexer: lexer-test Makefile
	@echo "============================================="
	./lexer-test
	rm -f ./lexer-test

al60l: parser-test

dist: $(ARCHIVE_NAME)

$(ARCHIVE_NAME): $(FILES)
	-rm -Rf dist_tmpdir/
	mkdir -p dist_tmpdir/$(ARCHIVE_SUBDIR)
	cp -r $(FILES) dist_tmpdir/$(ARCHIVE_SUBDIR)
	cd dist_tmpdir && $(ARCHIVE_METHOD) $(ARCHIVE_NAME) *
	mv dist_tmpdir/$(ARCHIVE_NAME) .
	rm -Rf dist_tmpdir/

clean:
	rm -f *.o al60l *~ *-tab.c *-tab.h *-tab.output

check: check-util check-slist check-estring check-cursor check-logger \
 check-lexer check-ast check-parser
	@echo "============================================="
	@echo "Self testing successful."
	@echo "============================================="

