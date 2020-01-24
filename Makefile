USER_SML_LIB?=${HOME}/SML
FILES=uri-punycode.sml uri-escape.sml uri.sml t.sml

help:
	@echo "target: all (poly mlton) test clean"
	@echo "make depends && make USER_SML_LIB=lib all"

all: poly mlton

poly: ${FILES} t.mlp
	env USER_SML_LIB=${USER_SML_LIB} poly --script t.mlp

mlton: ${FILES} t.mlb
	mlton -mlb-path-var 'USER_SML_LIB ${USER_SML_LIB}' t.mlb

test: ${FILES} t.mlp
	env USER_SML_LIB=${USER_SML_LIB} prove -e 'poly --script' t.mlp


depends: lib lib/scancom lib/unicode

lib:
	mkdir lib

lib/scancom:
	git clone https://github.com/kni/scancom.git lib/scancom

lib/unicode:
	git clone https://github.com/kni/sml-unicode.git lib/unicode


clean:
	rm -rf lib scancom unicode t
