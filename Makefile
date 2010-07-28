PANDOC=pandoc --standalone --smart --table-of-contents \
	      --email-obfuscation=references

.PHONY all: index.html CHANGES.html

index.html: README.markdown
	$(PANDOC) --css=style.css --output=$@ $<

CHANGES.html: CHANGES.markdown
	$(PANDOC) --css=changes.css --output=$@ $<
