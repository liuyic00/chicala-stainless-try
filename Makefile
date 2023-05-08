
SOURCES := $(shell find ./src ! -path "./src/main/scala/stainless/*" -type f -name "*.scala")
ifeq ($(shell uname),Darwin)
  PLATFORM=mac
else
  PLATFORM=linux
endif

compile: stainless-library
	sbt compile

stainless-library: src/main/scala/stainless

src/main/scala/stainless:
	mkdir -p .maketmp
	cd .maketmp ;\
	git clone https://github.com/epfl-lara/stainless.git
	mv .maketmp/stainless/frontends/library/stainless src/main/scala/stainless
	rm -rf .maketmp

stainless:
	./stainless-scalac-standalone/stainless-scalac-standalone-0.9.7-$(PLATFORM)/stainless.sh $(SOURCES)

clean:
	rm -rf src/main/scala/stainless
	