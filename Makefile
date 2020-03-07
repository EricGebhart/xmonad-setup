.PHONY: install
install:
	cp -r .xmonad $(HOME)/
	cp xmonad.start $(HOME)/bin/
	mkdir -p ~/Documents/Art
	cp ocean_park_114.jpg ~/Documents/Art/

.PHONY: xsession
xsession:
	sudo cp xmonad.desktop /usr/share/xsessions/

all: install xsession
