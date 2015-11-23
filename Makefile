build:
	stylus -p src/styles/index.styl > dist/styles.css && elm make src/Main.elm --warn --output dist/elm.js

open:
	open public/index.html

debug:
	elm-reactor

