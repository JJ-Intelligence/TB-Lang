
DEPEND += Lexer.hs Parser.hs

all: $(DEPEND) myinterpreter

myinterpreter: $(DEPEND) Main.hs
	ghc -o myinterpreter Main.hs

Parser.hs : Parser.y
	@rm -f Parser.hs
	happy Parser.y
	@chmod -w Parser.hs

Lexer.hs : Lexer.x
	@rm -f Lexer.hs
	alex Lexer.x
	@chmod -w Lexer.hs

clean::
	rm -rf Parser.hs Lexer.hs *.hi *.o *.info
