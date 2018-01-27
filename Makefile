.SILENT:

all: build check

build:
	echo --- build:
	gprbuild -P list_image.gpr
	echo

check: ./test_list_image
	echo --- tests:
	./test_list_image
	echo

.PHONY : clean
clean:
	echo --- clean:
	- gnat clean -P list_image.gpr
