.PHONY: run clean

NAME = pock
CC = gcc
EXT = c
BINF = -Wall -Wfatal-errors -O3
LIBS = 
OBJF = -Wall -Wfatal-errors -O3
INC = 

SRC = $(wildcard src/*.$(EXT))
OBJ = $(SRC:src/%.$(EXT)=obj/%.o)

bin/$(NAME): src bin obj $(OBJ) $(SRC)
	$(CC) $(OBJ) $(LIBS) $(BINF) -o bin/$(NAME)

deps:
	echo -e "$(foreach F, $(SRC), obj/$$($(CC) -MM $(F))\n\t$(CC) $(INC) -c $(F) -o $(F:src/%.$(EXT)=obj/%.o) $(OBJF)\n)" > deps

bin:
	mkdir bin

obj:
	mkdir obj

run: bin/$(NAME)
	@ cd bin && ./$(NAME) $(ARGS)

clean:
	- rm -r bin/$(NAME) obj deps

update:
	rm deps

-include deps
