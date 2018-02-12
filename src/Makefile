
# Path from current directory to top level ableC repository
ABLEC_BASE?=../../../ableC
# Path from current directory to top level extensions directory
EXTS_BASE?=../../../extensions

# The jar file to built
ABLEC_JAR=ableC.jar
# The artifact specification grammar to compile
ARTIFACT=artifact
# All directories containing grammars that may be included
GRAMMAR_DIRS=$(ABLEC_BASE) $(wildcard $(EXTS_BASE)/*/grammars)
# All silver files in included grammars, to be included as dependancies
GRAMMAR_SOURCES=$(shell find $(GRAMMAR_DIRS) -name *.sv -print0 | xargs -0)
# Flags passed to silver including the appropriate directories
override SVFLAGS+=$(addprefix -I ,$(GRAMMAR_DIRS))

# The directory in which to place intermediate object files
OBJECT_DIR=../bin
# The directory in which to place compiled libraries
LIB_DIR=../lib

# The base name of the library to build
LIB_NAME=search
# The name of the shared library.
SHARED_LIBRARY=$(LIB_DIR)/lib$(LIB_NAME).so
# The name of the static library.
STATIC_LIBRARY=$(LIB_DIR)/lib$(LIB_NAME).a

# All extended C files to compile
XC_FILES=$(wildcard *.xc)
# All C files that should be generated
C_FILES=$(addprefix $(OBJECT_DIR)/,$(XC_FILES:.xc=.c))
# All object files that should be generated
OBJECTS=$(addprefix $(OBJECT_DIR)/,$(XC_FILES:.xc=.o))

# All directories contining extension header files that may be included
XC_INCLUDE_DIRS=$(wildcard $(EXTS_BASE)/*/include)
# All header files that may be included, to be included as dependencies
XC_INCLUDE_SOURCES=$(foreach dir,$(XC_INCLUDE_DIRS),$(wildcard $(dir)/*.*h))
# Flags passed to ableC including the appropriate directories
override XCFLAGS+=$(addprefix -I,$(XC_INCLUDE_DIRS))
# Flags passed to Java when invoking ableC
override JAVAFLAGS+=-Xss6M

# Flags passed to the C compiler, e.g. to enable various compiler extensions
override CFLAGS+=-fpic
# Flags passed to the linker when building the shared library
override LDFLAGS+=-shared

all: $(SHARED_LIBRARY) $(STATIC_LIBRARY)

$(ABLEC_JAR): $(GRAMMAR_SOURCES)
	touch $(wildcard $(ARTIFACT)/*.sv)
	silver -o $@ $(SVFLAGS) $(ARTIFACT)

$(OBJECT_DIR)/%.c: %.xc $(XC_INCLUDE_SOURCES) $(ABLEC_JAR) | $(OBJECT_DIR)
	java $(JAVAFLAGS) -jar $(ABLEC_JAR) $< $(XCFLAGS)
	mv $*.c $@

$(SHARED_LIBRARY): $(OBJECTS) | $(LIB_DIR)
	$(CC) $(LDFLAGS) $^ -o $@

$(STATIC_LIBRARY): $(OBJECTS) | $(LIB_DIR)
	$(AR) rcs $@ $^

$(OBJECT_DIR):
	mkdir -p $@

$(LIB_DIR):
	mkdir -p $@

clean:
	rm -rf *~ $(OBJECT_DIR) $(LIB_DIR)

.PHONY: all clean
