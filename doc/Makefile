all: RRA60.pdf PIOCA60.pdf dp.pdf

dp.pdf : dp.tex dp.bib fitprj.cls Makefile
	pdflatex dp.tex
	bibtex dp
	pdflatex dp.tex
	pdflatex dp.tex

show-dp: dp.pdf
	xpdf dp.pdf

clean:
	-rm -f dp.aux  dp.bbl  dp.blg  dp.log  dp.out  dp.pdf  dp.toc
