source ~/dotfiles/antigen/antigen.zsh

# Load the oh-my-zsh library
antigen use oh-my-zsh 

antigen bundle git
antigen bundle zsh-users/zsh-syntax-highlighting

antigen theme ys

#All done!
antigen apply

setopt correct
setopt vi

export GOPATH=~/go
export PATH=$PATH:$GOPATH/bin:/home/will/.cabal/bin
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/usr/local/lib
PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"
PATH=$HOME/.linuxbrew/bin:$PATH

alias emacs="TERM=xterm-256color emacsclient -nw"

