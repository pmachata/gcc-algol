CC      := gcc
#CC      := gcc-4.1-rh
BISON   := bison
FLEX    := flex
TREECC  := treecc
DEBUG   := -ggdb3 -Wall -fno-inline
#DEBUG   := -Wall -DNDEBUG
#PROFILE := -pg
CFLAGS  := -std=c99 -O0
#-ftree-vectorize -m3dnow -mmmx -march=i686 -mtune=athlon-tbird
#-fbranch-probabilities
#-fprofile-arcs

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
	ast.tc\
	type.c type.h\
	label.c label.h\
	slist.c slist.h\
	Makefile

.PHONY: all doc dist clean check tests

all: al60l


util.o: util.c util.h Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -c util.c -o util.o

check-util: util.o util.c Makefile
	@echo "============================================="
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -DSELF_TEST util.c util.o -o util-test
	./util-test
	rm -f ./util-test


slist.o: slist.c slist.h pd.h Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -c slist.c -o slist.o

check-slist: slist.o pd.h Makefile
	@echo "============================================="
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -DSELF_TEST slist.c slist.o -o slist-test
	./slist-test
	rm -f ./slist-test


cursor.o: cursor.c cursor.h pd.h util.h Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -c cursor.c -o cursor.o

check-cursor: cursor.o util.o cursor.c Makefile
	@echo "============================================="
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -DSELF_TEST cursor.c cursor.o util.o -o cursor-test
	./cursor-test
	rm -f ./cursor-test


logger.o: logger.c logger.h pd.h Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -c logger.c -o logger.o

check-logger: logger.o util.o logger.c Makefile
	@echo "============================================="
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -DSELF_TEST logger.c logger.o util.o -o logger-test
	./logger-test
	rm -f ./logger-test


estring.o: estring.c estring.h pd.h Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -c estring.c -o estring.o

check-estring: estring.o estring.c estring.h Makefile
	@echo "============================================="
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -DSELF_TEST estring.c estring.o -o estring-test
	./estring-test
	rm -f ./estring-test


ast-tab.c: ast.tc Makefile
	$(TREECC) -o ast-tab.c -h ast-tab.h ast.tc

ast-tab.h: ast.tc ast-tab.c Makefile
	[ -f ast-tab.h ]

ast.o: ast-tab.c ast-tab.h slist.h
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) ast-tab.c -c -o ast.o

check-ast: ast-tab.o slist.o logger.o util.o estring.o type.o ast-test.c Makefile
	@echo "============================================="
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) ast-test.c ast-tab.o slist.o logger.o \
		util.o estring.o type.o -o ast-test
	./ast-test
	rm -f ./ast-test


type.o: type.c type.h pd.h Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -c type.c -o type.o

check-type: type.o type.c type.h Makefile
	@echo "============================================="
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -DSELF_TEST type.c type.o -o type-test
	./type-test
	rm -f ./type-test


label.o: label.c label.h estring.h pd.h Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -c label.c -o label.o

check-label: label.o estring.o estring.h label.c label.h Makefile
	@echo "============================================="
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -DSELF_TEST label.c label.o estring.o -o label-test
	./label-test
	rm -f ./label-test


parser.o: parser-tab.c ast.o Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -c parser-tab.c -o parser.o

parser-tab.c: parser.y parser.h lexer.h logger.h pd.h cursor.h Makefile
	$(BISON) -o parser-tab.c -d parser.y

parser-tab.h: parser-tab.c
	[ -f ./parser-tab.h ]

parser-test: parser-test.c parser.o type.o label.o ast.o logger.o\
 lexer.o cursor.o estring.o slist.o util.o parser.h lexer.h estring.h pd.h
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -std=gnu99 -o parser-test \
		parser-test.c parser.o type.o label.o ast.o logger.o\
		lexer.o cursor.o estring.o slist.o util.o

check-parser: parser-test Makefile
	@echo "============================================="
	cd torture && ./torture.sh


lexer.o: lexer-tab.c Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -c lexer-tab.c -o lexer.o

lexer-tab.c: lexer.l lexer.h parser-tab.h estring.h logger.h pd.h cursor.h Makefile
	$(FLEX) -R -o lexer-tab.c --header-file=lexer-tab.h lexer.l

lexer-tab.h: lexer-tab.c
	[ -f ./lexer-tab.h ]

check-lexer: lexer-test.c logger.o lexer.o cursor.o lexer.h pd.h Makefile
	@echo "============================================="
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -std=gnu99 -o check-lexer \
		lexer-test.c lexer.o cursor.o logger.o util.o estring.o
	./check-lexer
	rm -f ./check-lexer


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
	rm -f *.o al60l

check: check-util check-slist check-estring check-cursor check-logger \
 check-lexer check-type check-label check-ast check-parser
	@echo "============================================="
	@echo "Self testing successful."
	@echo "============================================="
