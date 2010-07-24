.PHONY all: testcoverage

.PHONY testcoverage: quickcheck/hpc_index.html

quickcheck/hpc_index.html: .hpc quickcheck/quickcheck-re.tix
	cd quickcheck; hpc markup --srcdir=.. quickcheck-re

quickcheck/quickcheck-re.tix:
	cd quickcheck; quickcheck-re
