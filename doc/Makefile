FILES = RRA60.pdf PIOCA60.pdf dp.pdf demo-1.pdf

all: $(FILES)

%.pdf: %.tex
	pdflatex $<
	-bibtex $(<:.tex=) && pdflatex $<
	pdflatex $<

dp.pdf: dp.tex dp.bib fitprj.cls vut_fit.pdf Makefile

demo-1.pdf: demo-1.tex Makefile

RRA60.pdf: RRA60.tex Makefile

PIOCA60.pdf: PIOCA60.tex Makefile

show-%: %.pdf
	xpdf $<

clean:
	-rm -f *.aux  *.bbl  *.blg  *.log  *.out *.toc *.snm $(FILES)
