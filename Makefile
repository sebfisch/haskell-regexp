.PHONY all: index.html

index.html: README.markdown
	pandoc --standalone \
               --css=style.css \
               --smart \
               --table-of-contents \
               --email-obfuscation=references \
               --output=$@ $<

