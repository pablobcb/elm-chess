build:
	stylus -p frontend/styles/index.styl > public/styles.css && elm make frontend/Main.elm --output public/elm.js

open:
	open public/index.html

