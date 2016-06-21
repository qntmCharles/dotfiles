. /usr/lib/python3.5/site-packages/powerline/bindings/zsh/powerline.zsh

setopt correct

export GOPATH=~/go
export PATH=$PATH:$GOPATH/bin
export EDITOR="nvim"

HOSTNAME="$(hostname)"
HOSTNAME_SHORT="${HOSTNAME%%.*}"
HISTFILE="${HOME}/.history/$(date -u +%Y/%m/%d)_${HOSTNAME_SHORT}"

# Just in case. It could be a *very* busy day.
HISTSIZE=100000
SAVEHIST=100000

mkdir -p $(dirname $HISTFILE)
