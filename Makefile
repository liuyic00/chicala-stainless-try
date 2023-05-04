
SOURCES := $(shell find ./src -type f -name *.scala)

compile:
	sbt compile

stainless:
	./stainless-scalac-standalone-0.9.7-mac/stainless.sh $(SOURCES)