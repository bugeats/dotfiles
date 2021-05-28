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


-- Plugins ---------------------------------------------------------------------

require('packer').startup(function ()
  use 'wbthomason/packer.nvim' -- Packer can manage itself
  use 'eddyekofo94/gruvbox-flat.nvim'

  use 'mkitt/tabline.vim' -- complete and succinct tabline config

  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }

  use 'nvim-treesitter/playground'

  use {
    'hoob3rt/lualine.nvim',
    requires = {'kyazdani42/nvim-web-devicons', opt = true}
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

-- Color Scheme ----------------------------------------------------------------

cmd 'colorscheme gruvbox-flat'
