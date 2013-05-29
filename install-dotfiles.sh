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

    ackrc
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
        echo "Creating directories \"$(dirname $target)\""
        mkdir -p "$(dirname $target)"
    fi
    if [ ! -e $target ]; then
        echo "Linking $source to $target"
        ln -s $source $target
    fi
done
