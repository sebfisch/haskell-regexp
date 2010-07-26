QuickCheck=../dist/build/quickcheck-re/quickcheck-re
Criterion=../dist/build/criterion-re/criterion-re

.PHONY all: .testprogs .coverage .microbench

.testprogs: $(QuickCheck) $(Criterion)

$(QuickCheck): .cabal-build
$(Criterion): .cabal-build

.cabal-build:
	cabal configure -fQuickCheck -fCriterion
	cabal build
	touch $@

.coverage: quickcheck/hpc_index.html
	touch $@

quickcheck/hpc_index.html: quickcheck/quickcheck-re.tix
	mkdir -p quickcheck; cd quickcheck; hpc markup --srcdir=.. quickcheck-re

quickcheck/quickcheck-re.tix:
	mkdir -p quickcheck; cd quickcheck; $(QuickCheck)

.microbench: criterion/full.csv criterion/full.png \
		criterion/partial.csv criterion/partial.png
	touch $@

criterion/%.png: criterion/%.csv
	barchart criterion --title="$* matching" $<

criterion/%.csv:
	mkdir -p criterion; cd criterion; $(Criterion) --summary=$*.csv --plot-kde=png $*
