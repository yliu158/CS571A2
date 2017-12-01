run: lex lex.yy.c
	gcc lex.yy.c -ll -o a
	./a

lex: cal.l
	flex cal.l

clean:
	rm a lex.yy.c
