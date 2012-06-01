#
# .zshrc is sourced in interactive shells.
# It should contain commands to set up aliases,
# functions, options, key bindings, etc.
#

autoload -Uz compinit
compinit

#allow tab completion in the middle of a word
setopt COMPLETE_IN_WORD

## keep background processes at full speed
#setopt NOBGNICE
## restart running processes on exit
#setopt HUP

## history
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.history

setopt APPEND_HISTORY
## for sharing history between zsh processes
setopt INC_APPEND_HISTORY
setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_NO_FUNCTIONS

setopt autocd notify
bindkey -e

# See if we can use colors
autoload -U colors
colors

source ~/.zshenv

fpath=(~/.zsh.d/functions $fpath)
autoload -U ~/.zsh.d/functions/*(:t)

typeset -ga chpwd_functions
chpwd_functions+='chpwd_auto_venv'

# Prompt
source ~/.zprompt
setopt PROMPT_SUBST
PROMPT='$(prompt)'
PS2='$(prompt2)'
#PROMPT='[%B%n%b@%B%m %2~%b] %(!.#.$) '
#RPROMPT='[%* - %W]'

# Aliases
alias ls='ls -G'
alias ll='ls -lFH'
alias la='ls -lAhS'
alias grep='grep --color=auto'
alias nano='nano -w'
alias memusage="ps -u $LOGNAME -o pid,rss,command | sort -n +1 -2"
alias pycheck="python -m py_compile"

drchrono=~/code/dc-web/drchrono-web
: ~drchrono
setopt CD_ABLE_VARS
