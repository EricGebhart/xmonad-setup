.PHONY: install

install:
	cp -r .xmonad $(HOME)/
	mkdir -p $(HOME)/bin
	cp xmonad.start $(HOME)/bin/
	mkdir -p ~/Documents/Art
	cp ocean_park_114.jpg ~/Documents/Art/

.PHONY: xsession
xsession:
	sudo cp myXmonad.desktop /usr/share/xsessions/

startup:
	sudo cp .xinitrc $(HOME)/
	sudo cp xmonad.start /usr/local/bin/

polybar:
	mkdir -p $(HOME)/.config
	cp -r .config/polybar 	$(HOME)/.config/

conky:
	mkdir -p $(HOME)/.config
	cp -r .config/conky 	$(HOME)/.config/

rofi:
	mkdir -p $(HOME)/.config
	cp -r .config/rofi 	$(HOME)/.config/

all: install xsession polybar conky rofi startup
