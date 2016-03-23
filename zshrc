source ~/dotfiles/antigen/antigen.zsh

# Load the oh-my-zsh library
antigen use oh-my-zsh 

antigen bundle git
antigen bundle zsh-users/zsh-syntax-highlighting

antigen theme ys

# All done!
antigen apply

setopt correct

export GOPATH=~/go
export PATH=$PATH:$GOPATH/bin:$HOME/.cabal/bin;
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/usr/local/lib
export EDITOR="nvim"

HOSTNAME="$(hostname)"
HOSTNAME_SHORT="${HOSTNAME%%.*}"
HISTFILE="${HOME}/.history/$(date -u +%Y/%m/%d)_${HOSTNAME_SHORT}"

# Just in case. It could be a *very* busy day.
HISTSIZE=100000
SAVEHIST=100000

mkdir -p $(dirname $HISTFILE)

. /home/$USER/.nix-profile/etc/profile.d/nix.sh
