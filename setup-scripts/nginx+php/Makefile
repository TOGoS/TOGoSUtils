COMPOSER_EXE = /usr/local/bin/composer

.PHONY: all
all: .nginx+php-installed .phpn2r-installed .oid-allocator-installed

.apt-updated:
	sudo apt-get update
	touch "$@"

.git-installed: .apt-updated
	sudo apt install -y git
	touch "$@"

composer-setup.php: download-composer-setup.php
	php download-composer-setup.php

${COMPOSER_EXE}: | composer-setup.php .nginx+php-installed
	sudo php composer-setup.php --install-dir=/usr/local/bin --filename=composer
	touch "$@"

.nginx+php-installed: install-nginx+php .apt-updated $(shell find etc/nginx)
	./install-nginx+php
	touch "$@"

.phpn2r-installed: install-phpn2r .git-installed .nginx+php-installed
	./install-phpn2r
	touch "$@"

.oid-allocator-installed: install-oid-allocator .git-installed ${COMPOSER_EXE} $(shell find etc/nginx)
	./install-oid-allocator
	touch "$@"
