all: RRA60.pdf PIOCA60.pdf dp.pdf

%.pdf : %.tex
	pdfcslatex $<
	pdfcslatex $<
	pdfcslatex $<

show-dp: dp.pdf
	xpdf dp.pdf
