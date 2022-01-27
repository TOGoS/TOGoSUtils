.PHONY: all
all: ${HOME}/proj/TOGoSUtils .git-installed .node-installed ${HOME}/.deno ${HOME}/.bashrc

.apt-updated:
	sudo apt-get update
	touch "$@"

.git-installed: .apt-updated
	sudo apt install -y git
	git config --global init.defaultBranch master
	git config --global pull.ff only
	git config --global core.autoCrlf false
	touch "$@"

.node-installed: .apt-updated
	sudo apt install -y nodejs
	touch "$@"

.curl-installed: .apt-updated
	sudo apt install -y curl
	touch "$@"

${HOME}/.deno: .apt-updated .curl-installed install-deno
	./install-deno
	touch "$@"

${HOME}/proj/TOGoSUtils: .git-installed install-togosutils
	./install-togosutils
	touch "$@"

${HOME}/.bashrc: install-deno install-togosutils
	./install-deno
	./install-togosutils
