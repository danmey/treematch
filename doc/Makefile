EMACS = emacs

all: treematch.pdf

%.pdf: %.org
	$(EMACS) --batch --visit=$< --eval "(org-export-as-pdf nil)"
clean:
	-rm -f *.pdf *.tex *~
