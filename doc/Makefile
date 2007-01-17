FILES = RRA60.pdf PIOCA60.pdf dp.pdf demo-1.pdf

all: $(FILES)

%.pdf: %.tex
	pdflatex $<
	-bibtex $(<:.tex=) && pdflatex $<
	pdflatex $<

dp.pdf: dp.tex dp.bib fitprj.cls vut_fit.pdf

demo-1.pdf: demo-1.tex demo-gccarch.pdf

RRA60.pdf: RRA60.tex

PIOCA60.pdf: PIOCA60.tex

show-%: %.pdf
	xpdf $<

clean:
	-rm -f *.aux  *.bbl  *.blg  *.log  *.out *.toc *.snm *.nav *.brf *~ $(FILES)
