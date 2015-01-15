# -*- mode: sh; -*-
#
# .zshrc is sourced in interactive shells.
# It should contain commands to set up aliases,
# functions, options, key bindings, etc.
#

fpath=(~/.zsh.d/functions $fpath)
autoload -U ~/.zsh.d/functions/*(:t)

autoload -Uz compinit
compinit

#allow tab completion in the middle of a word
setopt COMPLETE_IN_WORD

setopt EXTENDED_GLOB
unsetopt NOMATCH

## keep background processes at full speed
#setopt NOBGNICE
## restart running processes on exit
#setopt HUP

## history
HISTSIZE=100000
SAVEHIST=100000
HISTFILE=~/.history

setopt APPEND_HISTORY
## for sharing history between zsh processes
setopt INC_APPEND_HISTORY
setopt EXTENDED_HISTORY
setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_NO_FUNCTIONS
setopt HIST_REDUCE_BLANKS
setopt HIST_VERIFY

setopt autocd notify
bindkey -e

# See if we can use colors
autoload -U colors
colors

typeset -ga chpwd_functions
chpwd_functions+='chpwd_auto_venv'

# Prompt
source ~/.zprompt
setopt PROMPT_SUBST
PROMPT='$(prompt)'
PS2='$(prompt2)'
RPROMPT='$(rprompt)'

# Aliases
alias ls='ls -G'
alias ll='ls -lFH'
alias la='ls -lAhS'
alias grep='grep --color=auto'
alias nano='nano -w'
alias memusage="ps -u $LOGNAME -o pid,rss,command | sort -n +1 -2"
alias pycheck="python -m py_compile"
alias django="python manage.py"
alias dj='python manage.py'
alias edit='emacsclient -nw --alternate-editor="" -c'
# Global aliases, can be specified anywhere (not just the beginning of a command)
alias -g swapouterr='3>&1 1>&2 2>&3 3>&-'

if [ -f ~/.zsh.d/syntax-highlighting/zsh-syntax-highlighting.zsh ]; then
    source ~/.zsh.d/syntax-highlighting/zsh-syntax-highlighting.zsh
fi

if [ -e ~/.zshenv ]; then
    # Machine local config
    source ~/.zshenv
fi

if [ -d ~/.rbenv ]; then
    eval "$(rbenv init -)"
fi

# OPAM configuration
if [ -d ~/.opam ]; then
    . ~/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true
    eval `opam config env`
fi
