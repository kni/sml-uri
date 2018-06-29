FILES=uri-punycode.sml uri-escape.sml uri.sml t.sml

help:
	@echo "target: all (poly mlton) test clean"

all: poly mlton

poly: scancom unicode  ${FILES} t.mlp
	poly --script t.mlp

mlton: scancom unicode ${FILES} t.mlb
	mlton t.mlb

test: scancom unicode  ${FILES} t.mlp
	prove -e 'poly --script' t.mlp

scancom:
	git clone https://github.com/kni/scancom.git scancom

unicode:
	git clone https://github.com/kni/sml-unicode.git unicode


clean:
	rm -rf scancom unicode t
