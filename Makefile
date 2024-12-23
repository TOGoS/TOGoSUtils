default:
	@echo "Maybe make install?"

install:
	echo "# TOGoSUils" >> "${HOME}/.bashrc"
	echo ". $$(pwd)/dotfiles/.bash_aliases" >> "${HOME}/.bashrc"
	echo 'export PATH=$$PATH':"$$(pwd)/bin" >> "${HOME}/.bashrc"
	if [ ! -f "${HOME}/.screenrc" ] ; then cp dotfiles/.screenrc "${HOME}/.screenrc" ; else echo '~/.screenrc already exists; skipping' ; fi

# Probably also want
# emacs24-nox
# make
# node
# screen
