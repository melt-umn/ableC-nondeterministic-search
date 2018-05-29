
# The directory in which to place intermediate files
BIN_DIR=../bin
# The directory in which to place compiled libraries
LIB_DIR=../lib
# The directory in which to place Silver build intermediate files
GENERATED_DIR=$(BIN_DIR)/generated

# Path from current directory to top level ableC repository
ABLEC_BASE?=../../../ableC
# Path from current directory to top level extensions directory
EXTS_BASE?=../../../extensions

# The jar file to build
ABLEC_JAR=ableC.jar
# The artifact specification grammar to compile
ARTIFACT=artifact
# All directories containing grammars that may be included
GRAMMAR_DIRS=$(ABLEC_BASE)/grammars $(EXTS_BASE)/ableC-closure/grammars $(EXTS_BASE)/ableC-refcount-closure/grammars #$(wildcard $(EXTS_BASE)/*/grammars)
# All silver files in included grammars, to be included as dependancies
GRAMMAR_SOURCES=$(shell find $(GRAMMAR_DIRS) -name *.sv -print0 | xargs -0)
# Flags passed to silver including the appropriate directories
override SVFLAGS+=$(addprefix -I ,$(GRAMMAR_DIRS)) -G $(GENERATED_DIR)

# The base name of the library to build
LIB_NAME=search
# The name of the shared library.
SHARED_LIBRARY=$(LIB_DIR)/lib$(LIB_NAME).so
# The name of the static library.
STATIC_LIBRARY=$(LIB_DIR)/lib$(LIB_NAME).a

# All C files to compile
C_FILES=$(wildcard *.c)
# All extended C files to compile
XC_FILES=$(wildcard *.xc)
# All C files that should be generated
GEN_C_FILES=$(addprefix $(BIN_DIR)/,$(XC_FILES:.xc=.c))
# All object files that should be generated
OBJECTS=$(addprefix $(BIN_DIR)/,$(C_FILES:.c=.o) $(XC_FILES:.xc=.o))

# All directories contining extension header files that may be included
INCLUDE_DIRS=$(wildcard $(EXTS_BASE)/*/include)
# All header files that may be included, to be included as dependencies
INCLUDE_SOURCES=$(foreach dir,$(INCLUDE_DIRS),$(wildcard $(dir)/*.*h))
# Flags passed to ableC including the appropriate directories
override CPPFLAGS+=$(addprefix -I,$(INCLUDE_DIRS))
# Flags passed to Java when invoking ableC
override JAVAFLAGS+=-Xss6M

# Flags passed to the C compiler, e.g. to enable various compiler extensions
override CFLAGS+=-fpic
# Flags passed to the linker when building the shared library
override LDFLAGS+=-shared

rel: CFLAGS+=-O3 -DNDEBUG
rel: LDFLAGS+=-O3
rel: all

dbg: CFLAGS+=-O0 -g -DDEBUG
dbg: LDFLAGS+=-g
dbg: all

all: $(ABLEC_JAR) $(GEN_C_FILES) $(OBJECTS) $(SHARED_LIBRARY) $(STATIC_LIBRARY)

$(ABLEC_JAR): $(GRAMMAR_SOURCES) | $(GENERATED_DIR)
	touch $(wildcard $(ARTIFACT)/*.sv)
	silver-ableC -o $@ $(SVFLAGS) $(ARTIFACT)

$(BIN_DIR)/%.c: %.xc $(INCLUDE_SOURCES) $(ABLEC_JAR) | $(BIN_DIR)
	java $(JAVAFLAGS) -jar $(ABLEC_JAR) $< $(CPPFLAGS) $(XCFLAGS)
	mv $*.i $@
	mv $*.c $@

$(BIN_DIR)/%.o: %.c $(INCLUDE_SOURCES) | $(BIN_DIR)
	$(CC) -c $(CPPFLAGS) $(CFLAGS) $< -o $@

$(SHARED_LIBRARY): $(OBJECTS) | $(LIB_DIR)
	$(CC) $(LDFLAGS) $^ -o $@

$(STATIC_LIBRARY): $(OBJECTS) | $(LIB_DIR)
	$(AR) rcs $@ $^

$(BIN_DIR):
	mkdir -p $@

$(LIB_DIR):
	mkdir -p $@

$(GENERATED_DIR): | $(BIN_DIR)
	mkdir -p $@

clean:
	rm -rf *~ *.copperdump.html build.xml $(BIN_DIR) $(LIB_DIR)

realclean: clean
	rm -rf *.jar

.PHONY: rel dbg all clean realclean
