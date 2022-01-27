add_to_bashrc() {
    line="$1"
    if grep -w "$line" "$HOME/.bashrc"
    then
	# already there
	echo "'$line' already in .bashrc"
    else
	echo "Adding '$line' to .bashrc"
	echo "$line" >> "$HOME/.bashrc"
    fi
}
