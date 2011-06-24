#
# Created by makemake (Darwin Jun 23 2011) on Thu Jun 23 21:51:41 2011
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
BINDIR =./bin
CLIBFLAGS =	-lm
CCLIBFLAGS =	
########## End of default flags


CPP_FILES =	Board.cpp JamImpl.cpp SlideImpl.cpp change.cpp clock.cpp jam.cpp lloyd.cpp slide.cpp vclock.cpp water.cpp
C_FILES =	
S_FILES =	
H_FILES =	Change.h Clock.h Jam.h Lloyd.h Puzzle.h Slide.h Solver.h VClock.h Water.h
SOURCEFILES =	$(H_FILES) $(CPP_FILES) $(C_FILES) $(S_FILES)
.PRECIOUS:	$(SOURCEFILES)
OBJFILES =	Board.o JamImpl.o SlideImpl.o 

#
# Main targets
#

all:	${BINDIR}/change ${BINDIR}/clock ${BINDIR}/jam ${BINDIR}/lloyd ${BINDIR}/slide ${BINDIR}/vclock ${BINDIR}/water 

${BINDIR}/change:	change.o $(OBJFILES)
	@mkdir -p ${BINDIR}/
	$(CXX) $(CXXFLAGS) -o ${BINDIR}/change change.o $(OBJFILES) $(CCLIBFLAGS)

${BINDIR}/clock:	clock.o $(OBJFILES)
	@mkdir -p ${BINDIR}/
	$(CXX) $(CXXFLAGS) -o ${BINDIR}/clock clock.o $(OBJFILES) $(CCLIBFLAGS)

${BINDIR}/jam:	jam.o $(OBJFILES)
	@mkdir -p ${BINDIR}/
	$(CXX) $(CXXFLAGS) -o ${BINDIR}/jam jam.o $(OBJFILES) $(CCLIBFLAGS)

${BINDIR}/lloyd:	lloyd.o $(OBJFILES)
	@mkdir -p ${BINDIR}/
	$(CXX) $(CXXFLAGS) -o ${BINDIR}/lloyd lloyd.o $(OBJFILES) $(CCLIBFLAGS)

${BINDIR}/slide:	slide.o $(OBJFILES)
	@mkdir -p ${BINDIR}/
	$(CXX) $(CXXFLAGS) -o ${BINDIR}/slide slide.o $(OBJFILES) $(CCLIBFLAGS)

${BINDIR}/vclock:	vclock.o $(OBJFILES)
	@mkdir -p ${BINDIR}/
	$(CXX) $(CXXFLAGS) -o ${BINDIR}/vclock vclock.o $(OBJFILES) $(CCLIBFLAGS)

${BINDIR}/water:	water.o $(OBJFILES)
	@mkdir -p ${BINDIR}/
	$(CXX) $(CXXFLAGS) -o ${BINDIR}/water water.o $(OBJFILES) $(CCLIBFLAGS)

#
# Dependencies
#

Board.o:	Puzzle.h Slide.h
JamImpl.o:	Jam.h Puzzle.h
SlideImpl.o:	Puzzle.h Slide.h
change.o:	Change.h Puzzle.h Solver.h
clock.o:	Clock.h Puzzle.h Solver.h
jam.o:	Jam.h Puzzle.h Solver.h
lloyd.o:	Lloyd.h Puzzle.h Solver.h
slide.o:	Puzzle.h Slide.h Solver.h
vclock.o:	Puzzle.h Solver.h VClock.h
water.o:	Puzzle.h Solver.h Water.h

#
# Housekeeping
#

Archive:	archive.tgz

archive.tgz:	$(SOURCEFILES) Makefile
	tar cf - $(SOURCEFILES) Makefile | gzip > archive.tgz

clean:
	-/bin/rm $(OBJFILES) change.o clock.o jam.o lloyd.o slide.o vclock.o water.o core 2> /dev/null

realclean:        clean
	-/bin/rm -rf ${BINDIR}/change ${BINDIR}/clock ${BINDIR}/jam ${BINDIR}/lloyd ${BINDIR}/slide ${BINDIR}/vclock ${BINDIR}/water 
