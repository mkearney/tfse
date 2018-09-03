.PHONY: all clean
all:
	@echo "\n---------------------\n## Documenting...\n---------------------"
	Rscript -e "devtools::document(roclets=c('rd', 'collate', 'namespace'))"
	@echo "\n---------------------\n## Installing...\n---------------------"
	R CMD INSTALL --no-multiarch --with-keep.source .
clean:
	@echo "\n---------------------\n## Cleaning...\n---------------------"
	rm man/*.Rd
