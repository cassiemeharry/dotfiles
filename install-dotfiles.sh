#!/bin/sh

abspath () {
    case "$1" in
        /*)
        printf "%s\n" "$1"
        ;;

        *)
        printf "%s\n" "$PWD/$1"
        ;;
    esac
}

dotfiles="
    emacs.d

    gitconfig
    gitignore
    hgrc

    pylintrc

    zshrc
    zshenv
    zprompt
    zsh.d

    ssh/config
"

dotfiles_dir="$(dirname $(abspath $0))"

cd $HOME

for df in $dotfiles; do
    source="$dotfiles_dir/$df"
    target=".$df"
    if [ $target != "$(basename $target)" -a ! -e "$(dirname $target)" ]; then
	echo "\033[33mCreating directories \"$(dirname $target)\"\033[0m"
	mkdir -p "$(dirname $target)"
    fi
    if [ -e $target ]; then
        echo "\033[31mWarning! \"$target\" already exists!\033[0m"
    else
        echo "Linking $source to $target"
        ln -s $source $target
    fi
done
