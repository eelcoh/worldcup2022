SRC = src
BUILD = build
HTML = src/index.html

# From https://dimiterpetrov.com/blog/elm-single-page-application-setup/

build: build-directory html js 

debug: build-directory html js-debug

build-directory:
	mkdir -p $(BUILD)

html:
	cp $(HTML) $(BUILD)/index.html

js:
	elm make $(SRC)/Main.elm --optimize --output $(BUILD)/main.js

js-debug:
	elm make $(SRC)/Main.elm  --output $(BUILD)/main.js

clean:
	echo "Removing build artifacts..."
	rm -rf ./elm-stuff
	rm -rf ./build

