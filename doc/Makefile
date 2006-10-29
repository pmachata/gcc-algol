all: RRA60.pdf PIOCA60.pdf dp.pdf

dp.pdf : dp.tex dp.bib fitprj.cls Makefile
	pdfcslatex dp.tex
	bibtex dp
	pdfcslatex dp.tex
	pdfcslatex dp.tex

show-dp: dp.pdf
	xpdf dp.pdf

clean:
	-rm -f dp.aux  dp.bbl  dp.blg  dp.log  dp.out  dp.pdf  dp.toc
