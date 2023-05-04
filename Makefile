
SOURCES := $(shell find ./src -type f -name *.scala)
ifeq ($(shell uname),Darwin)
  PLATFORM=mac
else
  PLATFORM=linux
endif

compile:
	sbt compile

stainless:
	./stainless-scalac-standalone/stainless-scalac-standalone-0.9.7-$(PLATFORM)/stainless.sh $(SOURCES)