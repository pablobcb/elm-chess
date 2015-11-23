build:
	stylus -p src/styles/index.styl > public/styles.css && elm make src/Main.elm --warn --output public/elm.js

open:
	open public/index.html

debug:
	elm-reactor

