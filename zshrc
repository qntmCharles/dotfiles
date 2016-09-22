setopt correct

export GOPATH=~/go
export EDITOR="nvim"
export PATH=$PATH:$GOPATH/bin:$HOME/.local/bin

source ~/.local/lib/python3.5/site-packages/powerline/bindings/zsh/powerline.zsh
source ~/dotfiles/zsh-plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ~/dotfiles/zsh-plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

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

# history
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "^[[A" up-line-or-beginning-search # Up
bindkey "^[[B" down-line-or-beginning-search # Down

HOSTNAME="$(hostname)"
HOSTNAME_SHORT="${HOSTNAME%%.*}"
HISTFILE="${HOME}/.history"

HISTSIZE=100000
SAVEHIST=100000

mkdir -p $(dirname $HISTFILE)

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

transfer() { if [ $# -eq 0 ]; then echo "No arguments specified. Usage:\necho transfer /tmp/test.md\ncat /tmp/test.md | transfer test.md"; return 1; fi 
tmpfile=$( mktemp -t transferXXX ); if tty -s; then basefile=$(basename "$1" | sed -e 's/[^a-zA-Z0-9._-]/-/g'); curl --progress-bar --upload-file "$1" "https://transfer.sh/$basefile" >> $tmpfile; else curl --progress-bar --upload-file "-" "https://transfer.sh/$1" >> $tmpfile ; fi; cat $tmpfile; rm -f $tmpfile; }
