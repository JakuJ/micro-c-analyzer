GHC        = stack ghc
HAPPY      = happy
HAPPY_OPTS = --array --info --ghc --coerce
ALEX       = alex
ALEX_OPTS  = --ghc
DIR		   = src/Grammar
EXECUTABLE = micro-c-analyzer
ARGS 	   = -- analyse interval pending-set sources/even.c

# List of goals not corresponding to file names.

.PHONY : all clean bench threadscope memory profiteur docs coverage

# Default goal.

all : ${DIR}/Abs.hs ${DIR}/Lex.x ${DIR}/Par.y ${DIR}/Print.hs
	-rm ${DIR}/Test.hs

# Rules for building the parser.

${DIR}/Abs.hs ${DIR}/Lex.x ${DIR}/Par.y ${DIR}/Print.hs : Grammar.cf
	bnfc --haskell -d Grammar.cf -o src

%.hs : %.y
	${HAPPY} ${HAPPY_OPTS} $<

%.hs : %.x
	${ALEX} ${ALEX_OPTS} $<

# Rules for cleaning generated files.

clean :
	-rm -r ${DIR} && \
	rm -f *.aux *.hp *.ps *.eventlog *.prof *.prof.html *.svg

# Uses the zsh 'time' utility to see how much CPU we used
bench:
	stack build
	stack exec -- $(EXECUTABLE) +RTS -s $(ARGS)

# Show parallel performance info
threadscope:
	stack build
	stack exec -- $(EXECUTABLE) +RTS -ls -s $(ARGS)
	threadscope $(EXECUTABLE).eventlog

# Show memory usage info
memory:
	stack build --profile
	stack exec --profile -- $(EXECUTABLE) +RTS -hy -L200 -i0.0001 -l-au $(ARGS)
	eventlog2html $(EXECUTABLE).eventlog
	open $(EXECUTABLE).eventlog.html

# Show profiling info
profiteur:
	stack build --profile
	stack exec --profile -- $(EXECUTABLE) +RTS -p $(ARGS)
	profiteur $(EXECUTABLE).prof
	open $(EXECUTABLE).prof.html

# Build Haddock documentation
docs:
	stack haddock --keep-going --open $(EXECUTABLE)

# Build test coverage
coverage:
	stack test --coverage
