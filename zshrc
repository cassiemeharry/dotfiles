#
# .zshrc is sourced in interactive shells.
# It should contain commands to set up aliases,
# functions, options, key bindings, etc.
#

autoload -Uz compinit
compinit

#allow tab completion in the middle of a word
setopt COMPLETE_IN_WORD

setopt EXTENDED_GLOB
setopt NO_NOMATCH

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

if [ -e ~/.zshenv ]; then
    source ~/.zshenv
fi

fpath=(~/.zsh.d/functions $fpath)
autoload -U ~/.zsh.d/functions/*(:t)

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
alias edit='emacsclient -nw --alternate-editor="" -c'
function git-sync () {
    local branch="$(git symbolic-ref HEAD 2>/dev/null | cut -d'/' -f3)"
    local remote="$(git config branch.$branch.remote)"
    git stash
    git pull --rebase $remote $branch
    git push $remote $branch
    git stash apply
}

source ~/.zsh.d/syntax-highlighting/zsh-syntax-highlighting.zsh
