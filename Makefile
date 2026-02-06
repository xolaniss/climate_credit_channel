# Makefile to run the analysis for the paper and compile the manuscript

## Recursively look for all files in the current directory and its subdirectories
VPATH = $(shell find . -type d)

## List of inputs
INPUT_TARGETS = artifacts_BA930_mortgages.rds \
artifacts_bank_level_shares.rds \

## Generating the manuscript 
mortgage_paper: mortgage_paper.qmd $(INPUT_TARGETS)
	quarto render $<
	
## Generating rds inputs to manuscript
artifacts_BA930_mortgages.rds: 01_BA930_mortgages.R \
$(wildcard 4.1 BA930 Multiple Bank Export (*).xlsx)
	Rscript $<

artifacts_bank_level_shares.rds: 02_calculating_bank_level_splits.R \
artifacts_BA930_mortgages.rds
	Rscript $<