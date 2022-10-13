# build package documentation
doc:
	R -e 'devtools::document()'

check:
	R -e 'devtools::check()'
