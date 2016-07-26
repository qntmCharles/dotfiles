setopt correct

export GOPATH=~/go
export EDITOR="nvim"
export PATH=$PATH:$GOPATH/bin:$HOME/.local/bin

. ~/.local/lib/python3.5/site-packages/powerline/bindings/zsh/powerline.zsh

# Completion
autoload -U compinit
compinit

# options
setopt completeinword
setopt auto_cd

# Case insensitive completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

# aliases
alias ll='ls -l'
alias la='ls -a'


HOSTNAME="$(hostname)"
HOSTNAME_SHORT="${HOSTNAME%%.*}"
HISTFILE="${HOME}/.history/$(date -u +%Y/%m/%d)_${HOSTNAME_SHORT}"

# Just in case. It could be a *very* busy day.
HISTSIZE=100000
SAVEHIST=100000

mkdir -p $(dirname $HISTFILE)

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
