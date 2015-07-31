#Vim colour scheme setup
git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
mkdir -p ~/.vim/colors
curl -o ~/.vim/colors/solarized.vim https://raw.githubusercontent.com/altercation/vim-colors-solarized/master/colors/solarized.vim

#Emacs setup
mkdir -p ~/.local/include
mkdir -p ~/.local/bin

git clone --recursive https://github.com/Valloric/ycmd.git
./build.py --clang-completer --omnisharp-completer --gocode-completer-

#Attempt symlinking
ln -s emacs ~/.emacs
ln -s vimrc ~/.vimrc
ln -s xinitrc ~/.xinitrc
ln -s Xresources ~/.Xresources
