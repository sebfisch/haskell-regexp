QuickCheck=../dist/build/quickcheck-re/quickcheck-re
Criterion=../dist/build/criterion-re/criterion-re

.PHONY all: .coverage .microbench

.testprogs: $(QuickCheck) $(Criterion)

$(QuickCheck): .cabal-build
$(Criterion): .cabal-build

.cabal-build: weighted-regexp.cabal
	cabal configure -fQuickCheck -fCriterion
	cabal build
	touch $@

.coverage: .testprogs quickcheck/hpc_index.html
	touch $@

quickcheck/hpc_index.html: quickcheck/quickcheck-re.tix
	mkdir -p quickcheck; \
	cd quickcheck; \
	hpc markup --srcdir=.. quickcheck-re

quickcheck/quickcheck-re.tix: src/quickcheck.lhs
	rm -rf $@; \
	mkdir -p quickcheck; \
	cd quickcheck; $(QuickCheck)

.microbench: .testprogs \
		criterion/full.csv criterion/full.png \
		criterion/partial.csv criterion/partial.png
	touch $@

criterion/%.png: criterion/%.csv
	barchart criterion --title="$* matching" $<

criterion/%.csv: src/criterion.lhs
	mkdir -p criterion; \
	cd criterion; \
	$(Criterion) --summary=$*.csv --plot-kde=png $*
