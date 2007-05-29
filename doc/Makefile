FILES = RRA60.pdf PIOCA60.pdf dp.pdf demo-1.pdf eeict.pdf showcase.pdf
FITTHESIS = cls/fitthesis.cls cls/fit-zp2.eps cls/fit-zp2.pdf cls/vut-zp2.eps cls/vut-zp2.pdf


all: $(FILES)

%.pdf: %.tex
	pdflatex $<
	-bibtex $(<:.tex=) && pdflatex $<
	pdflatex $<

dp.pdf: dp.tex dp.bib vut_fit.pdf $(FITTHESIS)

demo-1.pdf: demo-1.tex demo-gccarch.pdf

showcase.pdf: showcase.tex

RRA60.pdf: RRA60.tex

PIOCA60.pdf: PIOCA60.tex

show-%: %.pdf
	xpdf $^

clean:
	-rm -f *.aux  *.bbl  *.blg  *.log  *.out *.toc *.snm *.nav *.brf *~ $(FILES)

bordel: fitthesis
	touch bordel

.PHONY: clean all
