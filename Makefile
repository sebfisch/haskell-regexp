.PHONY all: index.html

index.html: README.markdown
	pandoc --standalone \
               --css=style.css \
               --smart \
               --sanitize-html \
               --table-of-contents \
               --email-obfuscation=references \
               --webtex \
               --output=$@ $<

