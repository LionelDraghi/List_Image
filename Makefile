.SILENT:

all: build check

build:
	echo build:
	echo ------
	gprbuild -P list_image.gpr
	echo

check: ./test_list_image
	echo tests:
	echo ------
	./test_list_image
	echo
