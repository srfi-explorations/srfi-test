build:
	gosh -r7 convert.scm

clean:
	rm -rf chibi
	rm -rf chicken
	rm -rf gauche
	rm -rf guile
	rm -rf kawa
	rm -rf r6rs-libraries
	rm -rf r6rs-programs
	rm -rf r7rs-libraries
	rm -rf r7rs-programs
