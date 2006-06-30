CC      := gcc
#CC      := gcc-4.1-rh
BISON   := bison
FLEX    := flex
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
	ast.c ast.h\
	Makefile

.PHONY: all doc dist clean check tests

all: al60l


util.o: util.c util.h Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -c util.c

check-util: util.o util.c Makefile
	@echo "============================================="
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -DSELF_TEST util.c util.o -o util-test
	./util-test
	rm -f ./util-test


cursor.o: cursor.c cursor.h pd.h util.h Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -c cursor.c

check-cursor: cursor.o util.o cursor.c Makefile
	@echo "============================================="
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -DSELF_TEST cursor.c cursor.o util.o -o cursor-test
	./cursor-test
	rm -f ./cursor-test


logger.o: logger.c logger.h pd.h Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -c logger.c

check-logger: logger.o util.o logger.c Makefile
	@echo "============================================="
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -DSELF_TEST logger.c logger.o util.o -o logger-test
	./logger-test
	rm -f ./logger-test


estring.o: estring.c estring.h pd.h Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -c estring.c

check-estring: estring.o estring.c estring.h Makefile
	@echo "============================================="
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -DSELF_TEST estring.c estring.o -o estring-test
	./estring-test
	rm -f ./estring-test


ast.o: ast.c ast.h estring.h pd.h Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -c ast.c

check-ast: ast.o estring.o estring.h ast.c ast.h Makefile
	@echo "============================================="
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -DSELF_TEST ast.c ast.o estring.o -o ast-test
	./ast-test
	rm -f ./ast-test


parser.o: parser-tab.c Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -c parser-tab.c -o parser.o

parser-tab.c: parser.y parser.h lexer.h logger.h pd.h cursor.h Makefile
	$(BISON) -o parser-tab.c -d parser.y

parser-tab.h: parser-tab.c
	[ -f ./parser-tab.h ]

parser-test: parser-test.c parser.o ast.o logger.o lexer.o cursor.o estring.o parser.h lexer.h estring.h pd.h
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -std=gnu99 -o parser-test \
		parser-test.c parser.o ast.o lexer.o cursor.o logger.o util.o estring.o

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

check: check-util check-estring check-cursor check-logger check-lexer check-ast check-parser
	@echo "============================================="
	@echo "Self testing successful."
	@echo "============================================="
