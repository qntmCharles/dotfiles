echo "Installing vim-plug to neovim..."

curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

echo "Installing fonts..."
./fonts/install.sh

echo "Symlinking config..."

#Attempt symlinking
ln -s `pwd`/nvim ~/.config/nvim/init.vim
ln -s `pwd`/i3/config ~/.config/i3/config

echo "Installing powerline..."
pip install --user powerline-status

echo "Installing fzf..."
git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
~/.fzf/install
