build:
	stylus -p styles/index.styl > styles.css && elm make Main.elm
