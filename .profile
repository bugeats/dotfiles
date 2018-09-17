# Path -------------------------------------------------------------------------

# homebrew
export PATH="/usr/local/bin:$PATH"

# npm binaries
export PATH="/usr/local/share/npm/bin:$PATH"

# heroku
export PATH="/usr/local/heroku/bin:$PATH"

# dotfiles/bin
export PATH="/Users/chadwick/bin:$PATH"

# added by Anaconda3 5.1.0 installer
export PATH="/Users/chadwick/anaconda3/bin:$PATH"

# rust cargo package manager
export PATH="$HOME/.cargo/bin:$PATH"


# Neovim -----------------------------------------------------------------------

export EDITOR="/usr/local/bin/nvim -f"
export NVIM_TUI_ENABLE_TRUE_COLOR=1
# This will force Neovim use 24 bit colors
# export TERM="screen-256color"


# Node -------------------------------------------------------------------------

export NODE_PATH=$NODE_PATH:`npm root -g`

# nvm / node version manager
source /usr/share/nvm/init-nvm.sh


# Aliases ----------------------------------------------------------------------

alias vd='git diff | nvim -'
alias less='less -r'

alias ga='git add'
alias gaa='git add .'
alias gaaa='git add --all'
alias gau='git add --update'
alias gb='git branch'
alias gbd='git branch --delete '
alias gc='git commit'
alias gcf='git commit --fixup'
alias gcm='git commit --message'
alias gco='git checkout'
alias gcob='git checkout -b'
alias gcod='git checkout develop'
alias gcom='git checkout master'
alias gcos='git checkout staging'
alias gd='git diff -- . ":(exclude)*/yarn.lock"'
alias gda='git diff HEAD'
alias gi='git init'
alias gld='git log --pretty=format:"%h %ad %s" --date=short --all'
alias glg='git log --graph --oneline --decorate --all'
alias gm='git merge --no-ff'
alias gma='git merge --abort'
alias gmc='git merge --continue'
alias gp='git push'
alias gpu='git pull'
alias gpur='git pull --rebase'
alias gr='git rebase'
alias gs='git status'
alias gss='git status --short'
alias gst='git stash'
alias gsta='git stash apply'
alias gstd='git stash drop'
alias gstl='git stash list'
alias gstp='git stash pop'
alias gsts='git stash save'

# Misc -------------------------------------------------------------------------

ssh-add -K ~/.ssh/id_rsa &> /dev/null

