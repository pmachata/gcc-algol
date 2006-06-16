CC      := gcc
#CC      := gcc-4.1-rh
YACC    := bison
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
	Makefile

.PHONY: all doc dist clean check tests

all: al60l


util.o: util.c util.h Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -c util.c

check-util: util.o util.c Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -DSELF_TEST util.c util.o -o util-test
	./util-test
	rm -f ./util-test


cursor.o: cursor.c cursor.h pd.h util.h Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -c cursor.c

check-cursor: cursor.o util.o cursor.c Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -DSELF_TEST cursor.c cursor.o util.o -o cursor-test
	./cursor-test
	rm -f ./cursor-test


logger.o: logger.c logger.h pd.h Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -c logger.c

check-logger: logger.o util.o logger.c Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -DSELF_TEST logger.c logger.o util.o -o logger-test
	./logger-test
	rm -f ./logger-test


parser.o: parser-tab.c Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -c parser-tab.c -o parser.o

parser-tab.c: parser.y parser.h lexer.h logger.h pd.h cursor.h Makefile
	$(YACC) -o parser-tab.c -d parser.y

parser-tab.h: parser-tab.c
	[ -f ./parser-tab.h ]

check-parser: parser-test.c parser.o logger.o lexer.o cursor.o parser.h lexer.h pd.h Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -std=gnu99 parser-test.c parser.o lexer.o cursor.o logger.o util.o -o check-parser
	export ct=`echo "'true' 'false' 'true' 'true'" | ./check-parser 2>/dev/null`; [ "x$$ct" = "x210" ]
	rm -f ./check-parser


lexer.o: lexer-tab.c Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -c lexer-tab.c -o lexer.o

lexer-tab.c: lexer.l lexer.h parser-tab.h logger.h pd.h cursor.h Makefile
	$(FLEX) -R -o lexer-tab.c --header-file=lexer-tab.h lexer.l

lexer-tab.h: lexer-tab.c
	[ -f ./lexer-tab.h ]

check-lexer: lexer-test.c logger.o lexer.o cursor.o lexer.h pd.h Makefile
	$(CC) $(DEBUG) $(PROFILE) $(CFLAGS) -std=gnu99 lexer-test.c lexer.o cursor.o logger.o util.o -o check-lexer
	./check-lexer
	rm -f ./check-lexer


al60l: util.o cursor.o logger.o lexer.o parser.o Makefile
	$(CC) $(PROFILE) $(CFLAGS) util.o cursor.o logger.o lexer.o parser.o -o al60l

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

check: check-util check-cursor check-logger check-lexer check-parser
	@echo "============================================="
	@echo "Self testing successful."
	@echo "============================================="
