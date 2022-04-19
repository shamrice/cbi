
# Last updated: 2022-04-19

CBL=cobc
CBLFLAGS=-Wall -O2 -fstatic-call -lncurses # --debug
CBL_MAIN=./cobol_basic_interpreter.cbl
CBL_SOURCES=$(shell find . -type f -name '*.cbl' -o -name '*.cob' | grep -v '${CBL_MAIN}')

all: build

build: cbi

clean:
	rm -rfv ../bin

cbi: ${CBL_MAIN} ${CBL_SOURCES} 	
	mkdir -p -v ../bin
	${CBL} ${CBLFLAGS} ${CBL_DEBUG_FLAG} -x -o ../bin/$@ $^

