
QUIET=--quiet
R=${R_HOME}/bin/R

# Strip .Rnw from all vignette files
FILES=${subst .Rnw, , ${wildcard *.Rnw}}

# Add .pdf to all vignette files
default: ${addsuffix .pdf, ${FILES}} # Compile all vignette files

%.tex: %.Rnw # Weave Sweave files
	${R} CMD Sweave $<

%.pdf: %.tex # Compile LaTeX files
	texi2dvi --pdf --clean ${QUIET} $<
	make clean

clean: # Remove "compilation" files
	@rm -f ${addsuffix .{tex,aux,log,out,bbl,blg,dvi}, ${FILES}}
	@rm -f ${addsuffix -*.{pdf,eps}, ${FILES}} Rplots.ps

distclean: clean # Remove also PDF files
	@rm -f ${addsuffix .pdf, ${FILES}}

force: distclean default # Force make

debug: # Compile LaTeX files in debug mode
	make QUIET=''

echo: # Echo env. variables
	@echo "Vignette files: ${FILES}"
	@echo "R binary: ${R}"

help: # Help
	@echo -e '\nTarget: Dependency # Description'; \
	echo '=================================================='; \
	egrep '^[[:alnum:].+_()%]*:' ./Makefile
