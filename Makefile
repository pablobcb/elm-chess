build:
	stylus -p index.styl > styles.css && elm make Main.elm
