build:
	stylus -p frontend/styles/index.styl > public/styles.css && elm make frontend/Main.elm --warn --output public/elm.js

open:
	open public/index.html

debug:
	elm-reactor

