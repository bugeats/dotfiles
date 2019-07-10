" ------------------------------------------------------------------------------
" ---------------- The NeoVim Runcom of Chadwick Dahlquist ---------------------
" ------------------------------------------------------------------------------

" Much Inspiration:
"   http://sheerun.net/2014/03/21/how-to-boost-your-vim-productivity/
"   https://github.com/junegunn/dotfiles/blob/master/vimrc

set nocompatible " be iMproved
filetype off " required!

" vim-plug
" ------------------------------------------------------------------------------
" https://github.com/junegunn/vim-plug

" :PlugUpdate - install or update plugins
" :PlugClean! - Remove unused directories (bang version will clean without prompt)

call plug#begin('~/.config/nvim/plugged')

" Core / UI
Plug 'Lokaltog/vim-easymotion'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'airblade/vim-gitgutter'
Plug 'airblade/vim-rooter'
Plug 'editorconfig/editorconfig-vim'
Plug 'freitass/todo.txt-vim'
Plug 'godlygeek/tabular'
Plug 'ivyl/vim-bling'
Plug 'jeetsukumaran/vim-buffergator'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/vim-easy-align'
Plug 'mkitt/tabline.vim'
Plug 'neovim/node-host'
Plug 'ntpeters/vim-better-whitespace' " causes all trailing whitespace characters to be highlighted.
Plug 'othree/csscomplete.vim'
Plug 'scrooloose/nerdtree'
Plug 'tomtom/tcomment_vim'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'yuku-t/unite-git'
Plug 'zhaocai/GoldenView.Vim'

" COC
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'honza/vim-snippets'

" Misc Language
Plug 'digitaltoad/vim-pug',       { 'for': ['pug'] }
Plug 'elzr/vim-json',             { 'for': ['json'] }
Plug 'jparise/vim-graphql',       { 'for': ['graphql'] }
Plug 'neovimhaskell/haskell-vim', { 'for': ['haskell'] }
Plug 'nono/vim-handlebars',       { 'for': ['handlebars'] }
Plug 'plasticboy/vim-markdown',   { 'for': ['markdown'] }
Plug 'rust-lang/rust.vim',        { 'for': ['rust'] }
Plug 'vim-python/python-syntax',  { 'for': ['python'] }
Plug 'vim-ruby/vim-ruby',         { 'for': ['ruby'] }
Plug 'wavded/vim-stylus',         { 'for': ['stylus'] }

" Javascript Plugins
Plug 'chemzqm/vim-jsx-improve',  { 'for': ['javascript.jsx'] }
Plug 'othree/yajs.vim',          { 'for': ['javascript', 'javascript.jsx'] }

call plug#end()


" Preferences / Defaults -----------------------------------------------------------------------------------------------

syntax on

set autoindent
set autoread
set background=dark
set backupdir=./.backup,.,/tmp
set clipboard=unnamedplus                     " support OS clipboard
set conceallevel=0                            " don't ever hide text in the name of concealing syntax
set cursorline                                " highlighted cursor row
set expandtab                                 " insert spaces instead when pressing <tab>
set formatoptions-=cro                        " no annoying comment autoformat foo
set guifont=DejaVuSansMono:h14                " This is the best programming font. I declare it.
set hidden                                    " Don't need to see abandoned buffers
set hlsearch
set ignorecase
set list
set listchars=tab:▸\ ,eol:¬                   " Use same symbols as TextMate for tabstops & EOLs
set nobackup
set noerrorbells                              " don't beep, asshole
set noswapfile
set nowrap
set number
set ruler
set rulerformat=%cx%l%V%=%P
set shell=/bin/bash
set shiftwidth=4                              " default to 4 spaces for indentation
set showtabline=2                             " always show tab line
set smartindent
set spellfile=~/.config/nvim/spell/en.utf-8.add
set synmaxcol=160                             " Don't syntax highlight past 160 cols (perf)
set t_ut=
set tabstop=4                                 " use four space chars when pressing <tab>
set termguicolors                             " enable true color support
set timeoutlen=500                            " Time in milliseconds to wait for a mapped sequence to complete.
set titlestring=%{fnamemodify(getcwd(),':t')} " set iTerm tab/window title to the current working directory name (project name)
set ttimeoutlen=50                            " Time in milliseconds to wait for a key code sequence to complete.
set visualbell                                " don't beep
set wildignore=*.swp,*.pyc

" statusline (line below the window pane)
set statusline=%f:%l:%c                       " minimal status line with file name
set statusline+=%=%y%m                        " right-aligned file type [modified]

" vertical cursor for insert mode
set guicursor=n-v-c-sm:block,i-ci-ve:ver25,r-cr-o:hor20

let g:netrw_dirhistmax = 0    " no .netrwhist turds please

" never try to automatically insert commented new lines
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" never ever use tab characters you filthy heathens
autocmd BufReadPre set expandtab



" Mappings -------------------------------------------------------------------------------------------------------------

" Pressing 'K' will split the line ('cause 'J' will join it)
nnoremap K i<CR><Esc>

" Pressing 'S' will replace current word with last yanked text
nnoremap S diw"0P

" Bubble single lines up/down (using unimpaired)
nmap <M-k> [e
nmap <M-j> ]e

" Indent single lines left/right
nmap <M-h> <<
nmap <M-l> >>

" Visual mode bubble multiple lines up/down (using unimpaired)
vmap <M-k> [e`[V`]
vmap <M-j> ]e`[V`]

" Visual mode bubble multiple lines left/right
vmap <M-h> <`[V`]
vmap <M-l> >`[V`]

imap <S-BS> <C-W>
inoremap <S-BS> <C-W>
set backspace=indent,eol,start

" Use 'M' to jump to a mark and center viewport
map <expr> M printf('`%c zz', getchar())


" Movement ---------------------------------------------------------------------

" j/k moves even for a wrapping line
nmap j gj
nmap k gk

" gp selects pasted text
nnoremap <expr> gp '`[' . strpart(getregtype(), 0, 1) . '`]'

" Filetypes --------------------------------------------------------------------

" Show current file type:
"   :set filetype?

augroup filetypes
    autocmd!
    autocmd FileType 4dgl       setlocal ts=4 sw=4 expandtab
    autocmd FileType c          setlocal equalprg=clang-format
    autocmd FileType clojure    setlocal ts=2 sw=2 expandtab
    autocmd FileType javascript setlocal ts=4 sw=4 expandtab equalprg=eslint-pretty ff=unix
    autocmd FileType json       setlocal equalprg=json_reformat " json_reformat is part of yajl: http://lloyd.github.com/yajl/
    autocmd FileType rust       setlocal ts=4 sw=4 expandtab equalprg=rustfmt
    autocmd FileType xml        setlocal equalprg=xmllint\ --format\ -
    autocmd Filetype css        setlocal ts=2 sw=2 expandtab
    autocmd Filetype cucumber   setlocal ts=2 sw=2 expandtab
    autocmd Filetype dot        setlocal ts=2 sw=2 expandtab
    autocmd Filetype feature    setlocal ts=2 sw=2 expandtab
    autocmd Filetype haml       setlocal ts=2 sw=2 expandtab
    autocmd Filetype haskell    setlocal ts=2 sw=2 expandtab
    autocmd Filetype html       setlocal ts=2 sw=2 expandtab
    autocmd Filetype jade       setlocal ts=2 sw=2 expandtab
    autocmd Filetype less       setlocal ts=2 sw=2 expandtab
    autocmd Filetype markdown   setlocal ts=2 sw=2 expandtab wrap linebreak nolist spell
    autocmd Filetype pug        setlocal ts=2 sw=2 expandtab
    autocmd Filetype ruby       setlocal ts=2 sw=2 expandtab
    autocmd Filetype sass       setlocal ts=2 sw=2 expandtab
    autocmd Filetype scss       setlocal ts=2 sw=2 expandtab
    autocmd Filetype stylus     setlocal ts=2 sw=2 expandtab
    autocmd Filetype taskpaper  setlocal tabstop=2 shiftwidth=2 noexpandtab nolist spell
    autocmd Filetype text       setlocal ts=2 sw=2 expandtab wrap linebreak nolist spell
    autocmd Filetype txt        setlocal ts=2 sw=2 expandtab wrap linebreak nolist spell
    autocmd Filetype yaml       setlocal ts=2 sw=2 expandtab
augroup END

augroup filetypedetect
    au BufRead,BufNewFile *.js set filetype=javascript
    au BufRead,BufNewFile *.4dg set filetype=4dgl
augroup END

" No git-gutter for taskpaper files
autocmd BufReadPre *.taskpaper let g:gitgutter_enabled = 0

let g:vim_json_syntax_conceal = 0


" NerdTree ---------------------------------------------------------------------

let NERDTreeShowHidden=1
let NERDTreeMinimalUI=1
let NERDTreeIgnore=['\.DS_Store$', '^__pycache__$']


" Window Movement --------------------------------------------------------------

" Resize window (arrow keys)
nmap <left>  :3wincmd ><cr>
nmap <right> :3wincmd <<cr>
nmap <down>  :3wincmd +<cr>
nmap <up>    :3wincmd -<cr>


" GoldenView -------------------------------------------------------------------

let g:goldenview__enable_at_startup = 1

" dunno why GoldenView stopped init'ing at startup, but this fixes it
autocmd VimEnter * EnableGoldenViewAutoResize


" Tests Navigation -------------------------------------------------------------

function! CurrentFileTestSplit()
  exec ":only"
  if CurrentFileIsTest()
    exec ":leftabove vsplit " . CurrentFileAlternate()
  else
    exec ":rightbelow vsplit " . CurrentFileAlternate()
  end
  exec ":GoldenViewResize"
endfunction

function! CurrentFileIsTest()
  return match(expand('%'), '\.test\.[^\.]\+$') != -1
endfunction

function! CurrentFileAlternate()
  if CurrentFileIsTest()
    return substitute(expand('%'), '\.test\.\([^\.]\+\)$', '\.\1', 'g')
  end
  return substitute(expand('%'), '\.\([^\.]\+\)$', '\.test\.\1', 'g')
endfunction

function! EditCurrentFileAlternate()
  exec ":e " . CurrentFileAlternate()
endfunction


" NukeUnusedBuffers() ----------------------------------------------------------

" :call NukeUnusedBuffers()
" remove unused (not visible) buffers

function! NukeUnusedBuffers()
  " list of *all* buffer numbers
  let l:buffers = range(1, bufnr('$'))

  " what tab page are we in?
  let l:currentTab = tabpagenr()
  try
    " go through all tab pages
    let l:tab = 0
    while l:tab < tabpagenr('$')
      let l:tab += 1

      " go through all windows
      let l:win = 0
      while l:win < winnr('$')
        let l:win += 1
        " whatever buffer is in this window in this tab, remove it from
        " l:buffers list
        let l:thisbuf = winbufnr(l:win)
        call remove(l:buffers, index(l:buffers, l:thisbuf))
      endwhile
    endwhile

    " if there are any buffers left, delete them
    if len(l:buffers)
      execute 'bwipeout' join(l:buffers)
    endif
  finally
    " go back to our original tab page
    execute 'tabnext' l:currentTab
  endtry
endfunction


" tComment ---------------------------------------------------------------------

call tcomment#type#Define('cucumber', '# %s')
call tcomment#type#Define('pug', '//- %s')
call tcomment#type#Define('python', '# %s')
call tcomment#type#Define('sass', '// %s')
call tcomment#type#Define('slim', '/ %s')


" GitGutter --------------------------------------------------------------------

" Back off there gitgutter, and stop slowing down tab rendering
let g:gitgutter_eager = 0


" Markdown ---------------------------------------------------------------------

let g:vim_markdown_folding_disabled = 1
let g:vim_markdown_conceal = 0
let g:vim_markdown_conceal_code_blocks = 0


" EasyAlign --------------------------------------------------------------------

" Start interactive EasyAlign in visual mode (e.g. vip<Enter>)
vmap <Enter> <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)"


" Supertab ---------------------------------------------------------------------

" Fix deoplete menu order
" https://github.com/ervandew/supertab#frequently-asked-questions
let g:SuperTabDefaultCompletionType = "<c-n>"


" Conquer of Completion (coc)---------------------------------------------------

" use <tab> for trigger completion and navigate to the next complete item
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction

inoremap <silent><expr> <Tab>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<Tab>" :
      \ coc#refresh()

" ---- coc-snippets ----

" exand current snippet on right arrow
imap <right> <Plug>(coc-snippets-expand)
smap <right> <Plug>(coc-snippets-expand)
xmap <right> <Plug>(coc-snippets-expand)


" Neovim Terminal Mode ---------------------------------------------------------

if has('nvim')
    " backtick sends true escape
    " tnoremap ` <Esc>
    " pre Esc twice to exit
    tnoremap <Esc><Esc> <c-\><c-n>
    au WinEnter *pid:* call feedkeys('i')
endif

" hide line numbers on terminal
au TermOpen * setlocal nonumber norelativenumber


" Color Scheme -----------------------------------------------------------------

" CTRL-S show syntax highlighting groups for word under cursor
nmap <C-S> :call <SID>SynStack()<CR>
function! <SID>SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc

" currently using a custom color scheme (in progress)
colorscheme mine


" Misc Stuff -------------------------------------------------------------------

let g:comfortable_motion_scroll_down_key = "j"
let g:comfortable_motion_scroll_up_key = "k"


" Leader -----------------------------------------------------------------------

" use spacebar for leader!
let mapleader = "\<Space>"

" LEADER LEADER LEADER LEADER LEADER LEADER LEADER LEADER LEADER LEADER LEADER L
" R LEADER LEADER LEADER LEADER LEADER LEADER LEADER LEADER LEADER LEADER LEADER
" DER LEADER LEADER LEADER LEADER LEADER LEADER LEADER LEADER LEADER LEADER LEAD
" ADER LEADER LEADER LEADER LEADER LEADER LEADER LEADER LEADER LEADER LEADER LEA

" (c)oc tasks --------------------------

" (c)oc (d)iagnostic (i)nfo            Show diagnostic message of current position, no truncate.
nnoremap <leader>cdi                   <Plug>(coc-diagnostic-info)<cr>

" (c)oc (d)iagnostic (n)ext            Jump to next diagnostic position.
nnoremap <leader>cdn                   <Plug>(coc-diagnostic-next)

" (c)oc (d)iagnostic (p)rev            Jump to previous diagnostic position.
nnoremap <leader>cdp                   <Plug>(coc-diagnostic-prev)

" (c)oc (d)efinition (j)ump            Jump to definition(s) of current symbol.
nnoremap <leader>cdj                   <Plug>(coc-definition)

" (c)oc de(c)laration (j)ump           Jump to declaration(s) of current symbol.
nnoremap <leader>ccj                   <Plug>(coc-declaration)

" (c)oc (i)mplementation (j)ump        Jump to implementation(s) of current symbol.
nnoremap <leader>cij                   <Plug>(coc-implementation)

" (c)oc (t)ype-efinition (j)ump        Jump to type definition(s) of current symbol.
nnoremap <leader>ctj                   <Plug>(coc-type-definition)

" (c)oc (r)eferences (j)ump            Jump to references of current symbol.
nnoremap <leader>crj                   <Plug>(coc-references)

" (c)oc (f)ormat (s)elected            Format selected range, would work in both visual mode and normal mode, when used in normal mode, the selection works on the motion object.
nnoremap <leader>cfs                   <Plug>(coc-format-selected)
vnoremap <leader>cfs                   <Plug>(coc-format-selected)

" (c)oc (f)ormat (b)uffer              Format the whole buffer
nnoremap <leader>cfb                   <Plug>(coc-format)

" (c)oc (r)ename (s)ymbol              Rename symbol under cursor to a new word.
nnoremap <leader>crs                   <Plug>(coc-rename)

" (c)oc (c)odeaction (l)ine            Get and run code action(s) for current line.
nnoremap <leader>cgc                   <Plug>(coc-codeaction)

" (c)oc (c)odeaction (s)elected        Get and run code action(s) with the selected region. Works with both normal and visual mode.
nnoremap <leader>ccs                   <Plug>(coc-codeaction-selected)
vnoremap <leader>ccs                   <Plug>(coc-codeaction-selected)

" (c)oc (o)pen (l)ink                  Open link under cursor.
nnoremap <leader>col                   <Plug>(coc-openlink)

" (c)oc (d)o-command (i)line           Do command from codeLens of current line.
nnoremap <leader>cdi                   <Plug>(coc-codelens-action)

" (c)oc (f)ix (c)urrent                Try to run quickfix action for diagnostics on the current line.
nnoremap <leader>cfc                   <Plug>(coc-fix-current)

" (c)oc (h)ide (f)loats                Hide all float windows.
nnoremap <leader>chf                   <Plug>(coc-float-hide)

" (c)oc (f)loat (j)ump                 Jump to first float window.
nnoremap <leader>cfj                   <Plug>(coc-float-jump)


" (f)ile tasks -------------------------

" toggle between tests and implementation with vertical splits
nnoremap <leader>ft                    :call CurrentFileTestSplit()<cr>

nnoremap <leader>ft                    :call CurrentFileTestSplit()<cr>

nnoremap <leader>fd                    :lcd %:p:h<cr>
nnoremap <leader>fec                   :e ~/.config/nvim/colors/mine.vim<cr>
nnoremap <leader>fed                   :e ~/Desktop<cr>
nnoremap <leader>feq                   :e ~/.config/coc<cr>

" (f)ile (e)dit (s)nippets
nnoremap <leader>fes                   :CocCommand snippets.editSnippets<cr>

nnoremap <leader>fet                   :call EditCurrentFileAlternate()<cr>
nnoremap <leader>fev                   :e $MYVIMRC<cr>
nnoremap <leader>fs                    :w<cr>

" copy current file path
nnoremap <leader>fcp                   :let @+ = expand("%:p")<cr>:echo @+<cr>

" copy current file relative path
nnoremap <leader>fcr                   :let @+ = expand("%")<cr>:echo @+<cr>

" copy current file relative path with 'tape' prefix
nnoremap <leader>fct                   :let @+ = "tape ".expand("%")<cr>:echo @+<cr>

" list git modified files
nnoremap <leader>flm                   :Unite git_modified<cr>
" list git cached files
nnoremap <leader>flc                   :Unite git_cached<cr>
" list git untracked files
nnoremap <leader>flu                   :Unite git_untracked<cr>
nnoremap <leader>fln                   :Unite git_untracked<cr>

" re-read vim config
nnoremap <leader>frv                   :so $MYVIMRC<cr>

" (w)indow tasks -------------------------

" (w)indow (f)ocus
nnoremap <leader>wf                    :only<cr>:NERDTreeFind<cr>:wincmd l<cr>:GoldenViewResize<cr>
" (w)indow (F)ocus
nnoremap <leader>wF                    :only<cr>:NERDTreeFind<cr>:wincmd l<cr>:Goyo<cr>
nnoremap <leader>wQ                    :qa!<cr>
nnoremap <leader>wd                    :q<cr>
nnoremap <leader>wh                    :wincmd h<cr>
nnoremap <leader>wj                    :wincmd j<cr>
nnoremap <leader>wk                    :wincmd k<cr>
nnoremap <leader>wl                    :wincmd l<cr>
nnoremap <leader>wm                    :only<cr>
nnoremap <leader>wq                    :wincmd q<cr>
nnoremap <leader>wq                    :wincmd d<cr>
nnoremap <leader>wr                    :wincmd r<cr>
nnoremap <leader>ws                    :wincmd s<cr>:wincmd j<cr>
nnoremap <leader>wv                    :wincmd v<cr>:wincmd l<cr>
" (w)indow resi(z)e
nnoremap <leader>wz                    :GoldenViewResize<cr>


" (n)ERDTree tasks -----------------------

nnoremap <leader>nf                    :NERDTreeFind<cr>


" (t)ab tasks ----------------------------

nnoremap <leader>1                     :tabn 1<CR>
nnoremap <leader>2                     :tabn 2<CR>
nnoremap <leader>3                     :tabn 3<CR>
nnoremap <leader>4                     :tabn 4<CR>
nnoremap <leader>5                     :tabn 5<CR>
nnoremap <leader>6                     :tabn 6<CR>
nnoremap <leader>7                     :tabn 7<CR>
nnoremap <leader>8                     :tabn 8<CR>
nnoremap <leader>9                     :tabn 9<CR>

nnoremap <leader>td                    :tabclose<CR>
nnoremap <leader>th                    :tabnext<CR>
nnoremap <leader>tj                    :tabnext<CR>
nnoremap <leader>tk                    :tabprev<CR>
nnoremap <leader>tl                    :tabprev<CR>
nnoremap <leader>tm                    :tabm<Space>
nnoremap <leader>tn                    :tabnew<CR>
nnoremap <leader>tt                    :tabedit<Space>

" tf 'tab floor' move the current tab all the way left
nnoremap <leader>tf                    :tabm 0<CR>

nnoremap <leader>t1                    :tabn 1<CR>
nnoremap <leader>t2                    :tabn 2<CR>
nnoremap <leader>t3                    :tabn 3<CR>
nnoremap <leader>t4                    :tabn 4<CR>
nnoremap <leader>t5                    :tabn 5<CR>
nnoremap <leader>t6                    :tabn 6<CR>
nnoremap <leader>t7                    :tabn 7<CR>
nnoremap <leader>t8                    :tabn 8<CR>
nnoremap <leader>t9                    :tabn 9<CR>

" (m)ake tasks ---------------------------

" \m to 'make' save changes / reload REPL, switch to browser
autocmd FileType clojure    nnoremap <leader>m :w<CR>:Require<CR>:!open /Applications/Google\ Chrome\ Canary.app<CR>
autocmd FileType pug        nnoremap <leader>m :w<CR>:Require<CR>:!open /Applications/Google\ Chrome\ Canary.app<CR>
autocmd FileType javascript nnoremap <leader>m :w<CR>:!open /Applications/Google\ Chrome\ Canary.app<CR>


" Lint Tasks ---------------------------

" neomake open location window
nnoremap <leader>lo                    :lopen<CR>
" neomake close location window
nnoremap <leader>lc                    :lclose<CR>
" neomake go to current error/warning
nnoremap <leader>ll                    :ll<CR>
" neomake next error/warning
nnoremap <leader>ln                    :lnext<CR>
" neomake previous error/warning
nnoremap <leader>lp                    :lprev<CR>
" (l)int (f)ix
nnoremap <leader>lf                    :CocCommand eslint.executeAutofix<CR>

" Buffer Tasks -------------------------

" (b)(n) nuke buffers that are not visible
nnoremap <leader>bn                    :call NukeUnusedBuffers()<CR>
" (b)(l) list buffers in Buffergator
nnoremap <leader>bl                    :BuffergatorOpen<CR>


" eXternal Tasks -----------------------

" \xb to open web browser
nnoremap <leader>xb                    :!open /Applications/Google\ Chrome\ Canary.app<CR>
" \xB to open chrome
nnoremap <leader>xB                    :!open /Applications/Google\ Chrome.app<CR>
" \xs to open Spotify
nnoremap <leader>xs                    :!open /Applications/Spotify.app<CR>

" (r)e-format tasks ------------------
" (r)e-format (l)ines - sort paragraph lines
nnoremap <leader>rl                    vip:sort<CR>


" Misc Tasks ---------------------------

" global text search (grep)
nnoremap <leader>/                      :CocList grep<cr>
" \; auto append semicolon
nnoremap <silent><leader>;              meA;<Esc>`e
" \, auto append comma
nnoremap <silent><leader>,              meA,<Esc>`e
" \q ever so slightly faster quit command
nnoremap <leader>q                     :q<CR>
" \Q quit hard!
nnoremap <leader>Q                     :qa!<CR>
" \d show/hide NerdTree
nnoremap <leader>d                     :NERDTreeToggle<cr>
" \p to show fuzzy search
nnoremap <leader>p                     :CocList files<cr>
" dup a line/selection, with commented version above <-- this is awesome
vnoremap <leader>c                     y gv :TComment<cr> gv<Esc> p
nnoremap <leader>c                     V y gv :TComment<cr> gv<Esc> p
" j - insert blank line below
nnoremap <silent><leader>j             :set paste<CR>m`o<Esc>``:set nopaste<CR>
" k - insert blank line above
nnoremap <silent><leader>k             :set paste<CR>m`O<Esc>``:set nopaste<CR>
" \ts skip tests
nnoremap <leader>ts                    :%s/test(/test.skip(/g<CR>
" \tu unskip tests
nnoremap <leader>tu                    :%s/test.skip(/test(/g<CR>

" ------------------------------------------------------------------------------
"
" < That's all folks >
"  ------------------
"         \   ^__^
"          \  (oo)\_______
"             (__)\       )\/\
"                 ||----w |
"                 ||     ||
" ------------------------------------------------------------------------------
