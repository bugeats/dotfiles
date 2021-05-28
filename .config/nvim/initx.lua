-- TODO:
--   color listchars (EOL, TAB)

local cmd = vim.cmd  -- to execute Vim commands e.g. cmd('pwd')
local fn = vim.fn    -- to call Vim functions e.g. fn.bufnr()
local g = vim.g      -- a table to access global variables
local scopes = {o = vim.o, b = vim.bo, w = vim.wo}

local function opt(scope, key, value)
  scopes[scope][key] = value
  if scope ~= 'o' then scopes['o'][key] = value end
end

local function map(mode, lhs, rhs, opts)
  local options = {noremap = true}
  if opts then options = vim.tbl_extend('force', options, opts) end
  vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

-- Core ------------------------------------------------------------------------

vim.g.mapleader = " " -- use spacebar for leader!


-- Plugins ---------------------------------------------------------------------

require('packer').startup(function ()
  use 'wbthomason/packer.nvim' -- Packer can manage itself
  use 'eddyekofo94/gruvbox-flat.nvim'

  use 'mkitt/tabline.vim' -- complete and succinct tabline config

  use 'sunjon/shade.nvim' -- shade inactive windows

  use {
    -- helpers to use native language server client
    'neovim/nvim-lspconfig',
    run = 'npm install -g typescript typescript-language-server'
  }

  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }

  use 'nvim-treesitter/playground'

  use {
    -- killer fuzzy finder
    'nvim-telescope/telescope.nvim',
    requires = {
      'nvim-lua/popup.nvim',
      'nvim-lua/plenary.nvim'
    }
  }

  use 'hrsh7th/nvim-compe' -- autocomplete in lua for LSP, buffers, and snippets

  use {
    'hoob3rt/lualine.nvim',
    requires = {'kyazdani42/nvim-web-devicons', opt = true}
  }

  use {
    'kyazdani42/nvim-tree.lua',
    requires = {'kyazdani42/nvim-web-devicons', opt = true}

  }

  use {
    -- git line status in gutter, among other things
    'lewis6991/gitsigns.nvim',
    requires = {
      'nvim-lua/plenary.nvim'
    }
  }
end)


-- Options ---------------------------------------------------------------------

local indent = 2

opt('b', 'expandtab', true)                           -- Use spaces instead of tabs
opt('b', 'shiftwidth', indent)                        -- Size of an indent
opt('b', 'smartindent', true)                         -- Insert indents automatically
opt('b', 'tabstop', indent)                           -- Number of spaces tabs count for
opt('w', 'number', true)                              -- Print line number

cmd('set showtabline=2')                              -- always show tab line

cmd('set list')                                       -- Show whitespace characters
cmd('set listchars=tab:▸\\ ,eol:¬')                   -- Use same symbols as TextMate for tabstops & EOLs

cmd('set clipboard=unnamedplus')                      -- Support OS clipboard
cmd('set signcolumn=yes')                             -- Always show the signcolumn to avoid jumpy rendering


-- Treesitter ------------------------------------------------------------------

require'nvim-treesitter.configs'.setup {
  ensure_installed = 'maintained', -- one of "all", "maintained" (parsers with maintainers), or a list of languages
  highlight = {
    enable = true
  },
  indent = {
    enable = true
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = 'gnn',
      node_incremental = 'grn',
      scope_incremental = 'grc',
      node_decremental = 'grm',
    },
  },
}


-- Lualine ---------------------------------------------------------------------

local function filepath ()
  return fn.expand('%')
end

require('lualine').setup {
  options = {
    theme = 'gruvbox-flat'
  },
  sections = {
    lualine_a = {'branch'},
    lualine_b = {filepath},
    lualine_c = {'location'},
    lualine_x = {'encoding'},
    lualine_y = {'fileformat'},
    lualine_z = {'filetype'}
  },
  tabline = {},
}


-- Autocomplete ----------------------------------------------------------------

vim.o.completeopt = "menuone,noselect"

require'compe'.setup {
  enabled = true;
  autocomplete = true;
  debug = false;
  min_length = 1;
  preselect = 'enable';
  throttle_time = 80;
  source_timeout = 200;
  incomplete_delay = 400;
  max_abbr_width = 100;
  max_kind_width = 100;
  max_menu_width = 100;
  documentation = true;

  source = {
    path = true;
    buffer = true;
    calc = true;
    nvim_lsp = true;
    nvim_lua = true;
  };
}

cmd("inoremap <silent><expr> <C-Space> compe#complete()")
cmd("inoremap <silent><expr> <CR>      compe#confirm('<CR>')")
cmd("inoremap <silent><expr> <C-e>     compe#close('<C-e>')")
cmd("inoremap <silent><expr> <C-f>     compe#scroll({ 'delta': +4 })")
cmd("inoremap <silent><expr> <C-d>     compe#scroll({ 'delta': -4 })")


-- Lua -------------------------------------------------------------------------

local system_name
if vim.fn.has("mac") == 1 then
  system_name = "macOS"
elseif vim.fn.has("unix") == 1 then
  system_name = "Linux"
elseif vim.fn.has('win32') == 1 then
  system_name = "Windows"
else
  print("Unsupported system for sumneko")
end

-- set the path to the sumneko installation; if you previously installed via the now deprecated :LspInstall, use
local sumneko_root_path = vim.fn.stdpath('cache')..'/lspconfig/sumneko_lua/lua-language-server'
local sumneko_binary = sumneko_root_path.."/bin/"..system_name.."/lua-language-server"

require'lspconfig'.sumneko_lua.setup {
  cmd = {sumneko_binary, "-E", sumneko_root_path .. "/main.lua"};
  settings = {
    Lua = {
      runtime = {
        -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
        version = 'LuaJIT',
        -- Setup your lua path
        path = vim.split(package.path, ';'),
      },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = {'vim'},
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = {
          [vim.fn.expand('$VIMRUNTIME/lua')] = true,
          [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true,
        },
      },
      -- Do not send telemetry data containing a randomized but unique identifier
      telemetry = {
        enable = false,
      },
    },
  },
}


-- Typescript ------------------------------------------------------------------

require'lspconfig'.tsserver.setup{}


-- Telescope -------------------------------------------------------------------
-- https://github.com/nvim-telescope/telescope.nvim

cmd("nnoremap <leader>ff <cmd>lua require('telescope.builtin').find_files(require('telescope.themes').get_dropdown({}))<cr>")
cmd("nnoremap <leader>fg <cmd>lua require('telescope.builtin').live_grep()<cr>")
cmd("nnoremap <leader>fb <cmd>lua require('telescope.builtin').buffers()<cr>")
cmd("nnoremap <leader>fh <cmd>lua require('telescope.builtin').help_tags()<cr>")
cmd("nnoremap <leader>ft <cmd>lua require('telescope.builtin').treesitter()<cr>")


-- Nvim Tree -------------------------------------------------------------------

vim.g.nvim_tree_git_hl = 1 -- enable file highlight for git attributes

cmd("nnoremap <leader>d :NvimTreeToggle<cr>")


-- Misc Leader -----------------------------------------------------------------

-- nnoremap <leader>wf                    :only<cr>:NERDTreeFind<cr>:wincmd l<cr>:GoldenViewResize<cr>
-- nnoremap <leader>wF                    :only<cr>:NERDTreeFind<cr>:wincmd l<cr>:Goyo<cr>

local function leader(key, cmd)
  local keyX = '<leader>' .. key
  local cmdX = cmd .. '<cr>'
  vim.api.nvim_buf_set_keymap(0, 'n', keyX, cmdX, {noremap = true, silent = true})
end

leader('ld', '<cmd>lua vim.lsp.buf.definition()')
leader('la', '<cmd>lua vim.lsp.buf.code_action()')
leader('lr', '<cmd>lua vim.lsp.buf.rename()')

leader('bb', ':bprevious') -- buffer back

cmd('nnoremap K <cmd>lua vim.lsp.buf.hover()<CR>')

---- window ----

-- resize window with arrow keys
cmd("nmap <silent><left>  :3wincmd ><cr>")
cmd("nmap <silent><right> :3wincmd <<cr>")
cmd("nmap <silent><down>  :3wincmd +<cr>")
cmd("nmap <silent><up>    :3wincmd -<cr>")

cmd("nnoremap <leader>wh :wincmd h<cr>")
cmd("nnoremap <leader>wj :wincmd j<cr>")
cmd("nnoremap <leader>wk :wincmd k<cr>")
cmd("nnoremap <leader>wl :wincmd l<cr>")
cmd("nnoremap <leader>wm :only<cr>") -- window maximize
cmd("nnoremap <leader>wq :wincmd q<cr>") -- window quit
cmd("nnoremap <leader>wr :wincmd r<cr>") -- window rotate
cmd("nnoremap <leader>ws :wincmd s<cr>:wincmd j<cr>")
cmd("nnoremap <leader>wv :wincmd v<cr>:wincmd l<cr>")

---- tab ----

cmd("nnoremap <leader>1 :tabn 1<CR>")
cmd("nnoremap <leader>2 :tabn 2<CR>")
cmd("nnoremap <leader>3 :tabn 3<CR>")
cmd("nnoremap <leader>4 :tabn 4<CR>")
cmd("nnoremap <leader>5 :tabn 5<CR>")
cmd("nnoremap <leader>6 :tabn 6<CR>")
cmd("nnoremap <leader>7 :tabn 7<CR>")
cmd("nnoremap <leader>8 :tabn 8<CR>")
cmd("nnoremap <leader>9 :tabn 9<CR>")

cmd("nnoremap <leader>tq :tabclose<CR>") -- tab quit
cmd("nnoremap <leader>th :tabnext<CR>")
cmd("nnoremap <leader>tj :tabnext<CR>")
cmd("nnoremap <leader>tk :tabprev<CR>")
cmd("nnoremap <leader>tl :tabprev<CR>")
cmd("nnoremap <leader>tn :tabnew<CR>")

cmd("nnoremap <leader>tf :tabm 0<CR>") -- tab floor - move the current tab all the way left


-- Gitsigns --------------------------------------------------------------------

require('gitsigns').setup {
  signs = {
    add          = {hl = 'GitSignsAdd'   , text = '+', numhl='GitSignsAddNr'   , linehl='GitSignsAddLn'},
    change       = {hl = 'GitSignsChange', text = '~', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn'},
    delete       = {hl = 'GitSignsDelete', text = '-', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
    topdelete    = {hl = 'GitSignsDelete', text = '-', numhl='GitSignsDeleteNr', linehl='GitSignsDeleteLn'},
    changedelete = {hl = 'GitSignsChange', text = '~', numhl='GitSignsChangeNr', linehl='GitSignsChangeLn'},
  },
  keymaps = {} -- no keymaps please
}


-- Color Scheme ----------------------------------------------------------------

require'shade'.setup({
  overlay_opacity = 50,
  opacity_step = 1
})

cmd 'colorscheme gruvbox-flat'
