# 
# Rules for compiling and linking the typechecker/evaluator
#
# Type
#   make         to rebuild the executable files
#   make clean   to remove all intermediate and temporary files
#   

# Files that need to be generated from other files
DEPEND += Lexer.hs Parser.hs

# When "make" is invoked with no arguments, we build an executable 
#  after building everything that it depends on
all: $(DEPEND) Main

# Build an executable for Toy interpreter
Main: $(DEPEND) Main.hs
	ghc Main.hs

# Build an executable for interactive mode
# Toyi: $(DEPEND) Toyi.hs
# 	ghc Toyi.hs

# Generate ML files from a parser definition file
Parser.hs : Parser.y
	@rm -f Parser.hs
	happy Parser.y
	@chmod -w Parser.hs

# Generate ML files from a lexer definition file
Lexer.hs : Lexer.x
	@rm -f Lexer.hs
	alex Lexer.x
	@chmod -w Lexer.hs

# Clean up the directory
clean::
	rm -rf Parser.hs Lexer.hs *.hi *.o *.info