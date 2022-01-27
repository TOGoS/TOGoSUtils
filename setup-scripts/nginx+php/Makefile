.PHONY: all
all: .nginx+php-installed /var/www/phpn2r

.apt-updated:
	sudo apt-get update
	touch "$@"

.git-installed: .apt-updated
	sudo apt install -y git
	touch "$@"

.nginx+php-installed: install-nginx+php .apt-updated $(shell find etc/nginx)
	./install-nginx+php
	touch "$@"

/var/www/phpn2r: install-phpn2r .git-installed .nginx+php-installed
	./install-phpn2r
