# $Id: Makefile,v 1.14 2019-01-24 14:04:36-08 - - $

#
# General useful macros
#

MKFILE     = Makefile
MAKEFLAGS += --no-builtin-rules
DEPSFILE   = ${MKFILE}.deps
NOINCLUDE  = ci clean spotless
NEEDINCL   = ${filter ${NOINCLUDE}, ${MAKECMDGOALS}}
GMAKE      = ${MAKE} --no-print-directory

#
# File list macros
#

EXECBIN	   = sbinterp
OBJCMO	   = etc.cmo parser.cmo scanner.cmo \
             tables.cmo interp.cmo main.cmo
OBJCMI	   = ${OBJCMO:.cmo=.cmi} absyn.cmi
OBJBIN	   = ${OBJCMO:.cmo=.o}
MLSOURCE   = absyn.mli etc.mli etc.ml tables.mli tables.ml \
             interp.mli interp.ml main.ml
GENSOURCE  = tables.mli parser.mli parser.ml scanner.ml
GENFILES   = ${GENSOURCE} parser.output ${DEPSFILE}
OTHERFILES = ${MKFILE} ${DEPSFILE} using
ALLSOURCES = ${MLSOURCE} parser.mly scanner.mll ${OTHERFILES}
LISTING    = Listing.ps

#
# General targets
#

all : ${EXECBIN}

${EXECBIN} : ${OBJCMO}
	ocamlc str.cma ${OBJCMO} -o ${EXECBIN}

%.cmi : %.mli
	ocamlc -c $<

%.cmo : %.ml
	ocamlc -c $<

%.ml : %.mll
	ocamllex $<

%.mli %.ml : %.mly
	ocamlyacc -v $<


GEN_TABLES_MLI = ocamlc -i tables.ml
tables.mli : tables.ml absyn.cmi
	( echo "(* Generated file: DO NOT EDIT *)" \
	; echo "(* ${GEN_TABLES_MLI} *)" \
	; echo "(* Generated $$(date) *)" \
	; ${GEN_TABLES_MLI} | sed 's/^type/\n&/' \
	) >tables.mli

#
# Misc targets
#

clean :
	- rm ${OBJCMI} ${OBJCMO} ${OBJBIN} tables.mli

spotless : clean
	- rm ${EXECBIN} ${GENFILES} ${LISTING} ${LISTING:.ps=.pdf} 

ci : ${ALLSOURCES}
	cid + ${ALLSOURCES}

GEN_OCAMLDEP = ocamldep ${MLSOURCE} ${GENSOURCE}
GEN_FORMAT = perl -pe 's/^(.{1,72})\s+(.*)/$$1 \\\n\# $$2/'
deps : ${MLSOURCE} ${GENSOURCE}
	( echo "# Generated file: DO NOT EDIT" \
	; echo "# ${GEN_OCAMLDEP}" | ${GEN_FORMAT} \
	; echo "# Generated $$(date)" \
	; ${GEN_OCAMLDEP} \
	) >${DEPSFILE}

${DEPSFILE} : tables.mli
	@touch ${DEPSFILE}
	${GMAKE} deps

lis : ${ALLSOURCES}
	mkpspdf ${LISTING} ${ALLSOURCES}

again :
	${GMAKE} spotless
	${GMAKE} deps
	${GMAKE} ci
	${GMAKE} all
	${GMAKE} lis

ifeq "${NEEDINCL}" ""
include ${DEPSFILE}
endif

