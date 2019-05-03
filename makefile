#-----------------------------------------------------------
# Makefile for: 
#
#			FORTH
#
#-----------------------------------------------------------

NAME=forth

CC=ml
CFLAGS=/c /coff /Fl /Sg
CDEBUG=/Zi /Zd

LD=c:/masm32/bin/link.exe
LDFLAGS=/SUBSYSTEM:CONSOLE
LDEBUG=/DEBUG /PDB:$(NAME).pdb
LIBS=/LIBPATH:\masm32\lib 

#-----------------------------------------------------------
#                       TARGETS
#-----------------------------------------------------------
$(NAME).exe: $(NAME).obj
	$(LD) $(LDFLAGS) $(LDEBUG) $(NAME).obj $(LIBS)

$(NAME).obj: $(NAME).asm
	$(CC) $(CFLAGS)  $(CDEBUG) $(NAME).asm
