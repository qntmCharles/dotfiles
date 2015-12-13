#Vim colour scheme setup

mkdir -p ~/.vim/colors
curl -o ~/.vim/colors/solarized.vim https://raw.githubusercontent.com/altercation/vim-colors-solarized/master/colors/solarized.vim

mkdir -p ~/.local/include
mkdir -p ~/.local/bin

git clone --recursive https://github.com/Valloric/ycmd.git
./build.py --clang-completer --omnisharp-completer --gocode-completer-

#Attempt symlinking
ln -s `pwd`/emacs ~/.emacs
ln -s `pwd`/vimrc ~/.vimrc
ln -s `pwd`/xinitrc ~/.xinitrc
ln -s `pwd`/Xresources ~/.Xresources
ln -s `pwd`/lock.sh ~/.lock.sh
