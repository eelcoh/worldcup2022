#!/bin/sh
elm make src/Main.elm --output=build/main.js && cp src/index.html build
