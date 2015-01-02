set nocompatible
filetype off

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" Load plugins here
" #####################################################################

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'Valloric/YouCompleteMe'
Plugin 'altercation/vim-colors-solarized'
Plugin 'bling/vim-airline'
Plugin 'jiangmiao/auto-pairs'
Plugin 'fatih/vim-go'

" #####################################################################


" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required


set relativenumber		" shows line numbers
set number
syntax enable 			" enable syntax colouring
set showcmd 			" show command in bottom bar
set cursorline 			" highlight current line
set wildmenu 			" a visual autocomplete for the command menu
set lazyredraw			" only redraw when needed
set showmatch			" highlight matching parenthesis, etc
set nowrap				" disable line wrap
set background=dark
colorscheme solarized
set list				" makes whitespace visible. Belows sets the chars to use
set listchars=tab:▸\ ,eol:¬
set laststatus=2		" always display the status bar

set tabstop=4			" number of visual spaces per tab
set softtabstop=4		" number of spaces in tab while editing

set autoindent			" obv
set copyindent			" copy previous indentation on autoindenting
set shiftwidth=4		" number of spaces to use on autoindenting
set smarttab			" insert tabs on line start according to shiftwidth, not tabstop

set incsearch			" search while typing
set hlsearch			" highlight any matches

set hidden				" buffers are hidden instead of closed.
set history=1000		" remember more commands and search history
set undolevels=1000		" many levels of undo
set visualbell			" don't beep
set noerrorbells		" don't beep

set pastetoggle=<F2>	" toggle paste mode with F2. Means you can paste in text without vim messing it up.

let mapleader=","
let g:airline_powerline_fonts = 1 " use the nice patched powerline fonts. Makes the symbols actually work right.

" save keystrokes when typing commands
nnoremap ; :

" remap jj to esc, for speed. Unlikely to ever really type jj, if so can just wait for a brief moment.
imap jj <Esc>

" I need to learn to use Vim *properly*
map <up> <nop>
map <down> <nop>
map <left> <nop>
map <right> <nop>i

imap <up> <nop>
imap <down> <nop>
imap <left> <nop>
imap <right> <nop>

" Select the text that was just pasted
nnoremap <leader>v V`]

" Open .vimrc in a vertical split, for quick edits
nnoremap <leader>ev <C-w><C-v><C-l>:e $MYVIMRC<cr>

" Autosave all buffers when vim loses focus
au FocusLost * :wa
