default:
	@echo "Maybe make install?"

install:
	echo "# TOGoSUils" >> "${HOME}/.bashrc"
	echo ". $$(pwd)/dotfiles/.bash_aliases" >> "${HOME}/.bashrc"
	echo 'export PATH=$$PATH':"$$(pwd)/bin" >> "${HOME}/.bashrc"
