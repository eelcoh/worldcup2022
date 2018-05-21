#!/bin/sh
elm make src/Main.elm --output=build/worldcup.js && cp src/index.html build
