#!/usr/bin/env zsh

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

dotfiles=(
    emacs.d

    gitconfig
    hgrc

    zshrc
    zshenv
    zprompt
    zsh.d
)

dotfiles_dir="$(dirname $(abspath $0))"

cd $HOME

for df in $dotfiles; do
    source="$dotfiles_dir/$df"
    target=".$df"
    if [[ -a $target ]]; then
        print "\e[31mWarning! \"$target\" already exists!\e[0m"
    else
        print Linking $source to $target
        ln -s $source $target
    fi
done
