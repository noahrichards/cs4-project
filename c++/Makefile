#
# Created by gmakemake (Sparc Dec 16 2008) on Tue Dec 16 22:52:03 2008
#

#
# Definitions
#

.SUFFIXES:
.SUFFIXES:	.a .o .c .C .cpp .s
.c.o:
		$(COMPILE.c) $<
.C.o:
		$(COMPILE.cc) $<
.cpp.o:
		$(COMPILE.cc) $<
.s.o:
		$(COMPILE.cc) $<
.c.a:
		$(COMPILE.c) -o $% $<
		$(AR) $(ARFLAGS) $@ $%
		$(RM) $%
.C.a:
		$(COMPILE.cc) -o $% $<
		$(AR) $(ARFLAGS) $@ $%
		$(RM) $%
.cpp.a:
		$(COMPILE.cc) -o $% $<
		$(AR) $(ARFLAGS) $@ $%
		$(RM) $%

CC =		gcc
CXX =		g++

RM = rm -f
AR = ar
LINK.c = $(CC) $(CFLAGS) $(CPPFLAGS) $(LDFLAGS)
LINK.cc = $(CXX) $(CXXFLAGS) $(CPPFLAGS) $(LDFLAGS)
COMPILE.c = $(CC) $(CFLAGS) $(CPPFLAGS) -c
COMPILE.cc = $(CXX) $(CXXFLAGS) $(CPPFLAGS) -c

########## Default flags (redefine these with a header.mak file if desired)
CXXFLAGS =	-ggdb -Wall -ansi -pedantic
CFLAGS =	-ggdb -Wall -ansi -pedantic
CLIBFLAGS =	-lm
CCLIBFLAGS =	
########## End of default flags


CPP_FILES =	Board.cpp Jam.cpp Slide.cpp change.cpp clock.cpp lloyd.cpp vclock.cpp water.cpp
C_FILES =	
S_FILES =	
H_FILES =	Change.h Clock.h Jam.h Lloyd.h Puzzle.h Slide.h Solver.h VClock.h Water.h
SOURCEFILES =	$(H_FILES) $(CPP_FILES) $(C_FILES) $(S_FILES)
.PRECIOUS:	$(SOURCEFILES)
OBJFILES =	Board.o 

#
# Main targets
#

# No "Jam" here, as it is unfinished
all:	Slide change clock lloyd vclock water 

Jam:	Jam.o $(OBJFILES)
	$(CXX) $(CXXFLAGS) -o Jam Jam.o $(OBJFILES) $(CCLIBFLAGS)

Slide:	Slide.o $(OBJFILES)
	$(CXX) $(CXXFLAGS) -o Slide Slide.o $(OBJFILES) $(CCLIBFLAGS)

change:	change.o $(OBJFILES)
	$(CXX) $(CXXFLAGS) -o change change.o $(OBJFILES) $(CCLIBFLAGS)

clock:	clock.o $(OBJFILES)
	$(CXX) $(CXXFLAGS) -o clock clock.o $(OBJFILES) $(CCLIBFLAGS)

lloyd:	lloyd.o $(OBJFILES)
	$(CXX) $(CXXFLAGS) -o lloyd lloyd.o $(OBJFILES) $(CCLIBFLAGS)

vclock:	vclock.o $(OBJFILES)
	$(CXX) $(CXXFLAGS) -o vclock vclock.o $(OBJFILES) $(CCLIBFLAGS)

water:	water.o $(OBJFILES)
	$(CXX) $(CXXFLAGS) -o water water.o $(OBJFILES) $(CCLIBFLAGS)

#
# Dependencies
#

Board.o:	Puzzle.h Slide.h
Jam.o:	Jam.h Puzzle.h Solver.h
Slide.o:	Puzzle.h Slide.h Solver.h
change.o:	Change.h Puzzle.h Solver.h
clock.o:	Clock.h Puzzle.h Solver.h
lloyd.o:	Lloyd.h Puzzle.h Solver.h
vclock.o:	Puzzle.h Solver.h VClock.h
water.o:	Puzzle.h Solver.h Water.h

#
# Housekeeping
#

Archive:	archive.tgz

archive.tgz:	$(SOURCEFILES) Makefile
	tar cf - $(SOURCEFILES) Makefile | gzip > archive.tgz

clean:
	-/bin/rm $(OBJFILES) Jam.o Slide.o change.o clock.o lloyd.o vclock.o water.o core 2> /dev/null

realclean:        clean
	-/bin/rm -rf Jam Slide change clock lloyd vclock water 
