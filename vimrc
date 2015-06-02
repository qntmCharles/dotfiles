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
Plugin 'tpope/vim-dispatch'
Plugin 'OmniSharp/omnisharp-vim'
Plugin 'scrooloose/nerdtree'
Plugin 'rust-lang/rust.vim'
Plugin 'phildawes/racer'

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
"set nowrap				" disable line wrap
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
set spell " enable spelling checks
set spelllang=en_gb " set the spell checking language
set pastetoggle=<F2>	" toggle paste mode with F2. Means you can paste in text without vim messing it up.
set colorcolumn=80

let mapleader="\<Space>"
let g:airline_powerline_fonts = 1 " use the nice patched powerline fonts. Makes the symbols actually work right.

" save keystrokes when typing commands
nnoremap ; :

" remap jj to esc, for speed. Unlikely to ever really type jj, if so can just wait for a brief moment.
imap jj <Esc>

" I need to learn to use Vim *properly*
map <up> <nop>
map <down> <nop>
map <left> <nop>
map <right> <nop>

imap <up> <nop>
imap <down> <nop>
imap <left> <nop>
imap <right> <nop>

" Select the text that was just pasted
nnoremap <leader>v V`]

" Open .vimrc in a vertical split, for quick edits
nnoremap <leader>ev <C-w><C-v><C-l>:e $MYVIMRC<cr>
nnoremap <leader>o :CtrlP<CR>
nnoremap <leader>w :w<CR>

" Autosave all buffers when vim loses focus
au FocusLost * :wa

autocmd BufNewFile,BufRead *.vert,*.frag,*.glsl set ft=c

augroup omnisharp_commands
    autocmd!

    "Set autocomplete function to OmniSharp (if not using YouCompleteMe completion plugin)
    autocmd FileType cs setlocal omnifunc=OmniSharp#Complete

    " Synchronous build (blocks Vim)
    "autocmd FileType cs nnoremap <F5> :wa!<cr>:OmniSharpBuild<cr>
    " Builds can also run asynchronously with vim-dispatch installed
    autocmd FileType cs nnoremap <leader>b :wa!<cr>:OmniSharpBuildAsync<cr>
    " automatic syntax check on events (TextChanged requires Vim 7.4)
    "autocmd BufEnter,TextChanged,InsertLeave *.cs SyntasticCheck

    " Automatically add new cs files to the nearest project on save
    autocmd BufWritePost *.cs call OmniSharp#AddToProject()

    "show type information automatically when the cursor stops moving
    autocmd CursorHold *.cs call OmniSharp#TypeLookupWithoutDocumentation()

    "The following commands are contextual, based on the current cursor position.

    autocmd FileType cs nnoremap gd :OmniSharpGotoDefinition<cr>
    autocmd FileType cs nnoremap <leader>fi :OmniSharpFindImplementations<cr>
    autocmd FileType cs nnoremap <leader>ft :OmniSharpFindType<cr>
    autocmd FileType cs nnoremap <leader>fs :OmniSharpFindSymbol<cr>
    autocmd FileType cs nnoremap <leader>fu :OmniSharpFindUsages<cr>
    autocmd FileType cs nnoremap <leader>fm :OmniSharpFindMembers<cr> "finds members in the current buffer
    " cursor can be anywhere on the line containing an issue 
    autocmd FileType cs nnoremap <leader>x  :OmniSharpFixIssue<cr>  
    autocmd FileType cs nnoremap <leader>fx :OmniSharpFixUsings<cr>
    autocmd FileType cs nnoremap <leader>tt :OmniSharpTypeLookup<cr>
    autocmd FileType cs nnoremap <leader>dc :OmniSharpDocumentation<cr>
    autocmd FileType cs nnoremap <C-K> :OmniSharpNavigateUp<cr> "navigate up by method/property/field
    autocmd FileType cs nnoremap <C-J> :OmniSharpNavigateDown<cr> "navigate down by method/property/field

augroup END

" Contextual code actions (requires CtrlP)
nnoremap <leader><space> :OmniSharpGetCodeActions<cr>
" Run code actions with text selected in visual mode to extract method
vnoremap <leader><space> :call OmniSharp#GetCodeActions('visual')<cr>

" rename with dialog
nnoremap <leader>nm :OmniSharpRename<cr>
nnoremap <F2> :OmniSharpRename<cr>      
" rename without dialog - with cursor on the symbol to rename... ':Rename newname'
command! -nargs=1 Rename :call OmniSharp#RenameTo("<args>")

" Force OmniSharp to reload the solution. Useful when switching branches etc.
nnoremap <leader>rl :OmniSharpReloadSolution<cr>
nnoremap <leader>cf :OmniSharpCodeFormat<cr>
" Load the current .cs file to the nearest project
nnoremap <leader>tp :OmniSharpAddToProject<cr>

" (Experimental - uses vim-dispatch or vimproc plugin) - Start the omnisharp server for the current solution
nnoremap <leader>ss :OmniSharpStartServer<cr>
nnoremap <leader>sp :OmniSharpStopServer<cr>

" Add syntax highlighting for types and interfaces
nnoremap <leader>th :OmniSharpHighlightTypes<cr>

" YouCompleteMe mappings
nnoremap <leader>gdf :YcmCompleter GoToDefinition<cr>
nnoremap <leader>gdc :YcmCompleter GoToDeclaration<cr>
nnoremap <leader>g :YcmCompleter GoTo<cr>

" Racer setup
let g:racer_cmd = "/home/will/Clones/racer/target/release/racer"
