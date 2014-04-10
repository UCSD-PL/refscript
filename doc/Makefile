OTT=ott
PDFLATEX=pdflatex -halt-on-error -file-line-error

OTTFILE=src/typing.ott
MNGFILE=src/typing.tex

OTTOUTFILE=typing.ott.tex
MNGOUTFILE=typing.out.tex

OTTDEFS=ott.defs

all:
	$(OTT) -merge true -tex_show_meta false -tex_wrap false \
	-writesys $(OTTDEFS) -tex_filter $(MNGFILE) $(MNGOUTFILE) \
	-i $(OTTFILE) -o $(OTTOUTFILE)
	
	$(PDFLATEX) $(MNGOUTFILE)

clean:
	rm -f *.log *.aux

