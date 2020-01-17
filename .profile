# Path -------------------------------------------------------------------------

# homebrew
export PATH="/usr/local/bin:$PATH"

# npm binaries
export PATH="/usr/local/share/npm/bin:$PATH"

# heroku
export PATH="/usr/local/heroku/bin:$PATH"

# dotfiles/bin
export PATH="~/bin:$PATH"

# added by Anaconda3 5.1.0 installer
export PATH="/Users/chadwick/anaconda3/bin:$PATH"

# genymotion adb sharing with expo
export PATH="/opt/genymotion/tools:$PATH"

# rust cargo package manager
export PATH="$HOME/.cargo/bin:$PATH"

# ripgrep grep replacement
export RIPGREP_CONFIG_PATH="~/.config/ripgrep/ripgreprc"

# golang junk
export PATH=$PATH:$(go env GOPATH)/bin
export GOPATH=$(go env GOPATH)


# Neovim -----------------------------------------------------------------------

export EDITOR="nvim -f"
export NVIM_TUI_ENABLE_TRUE_COLOR=1
# This will force Neovim use 24 bit colors
# export TERM="screen-256color"


# Node -------------------------------------------------------------------------

export NODE_PATH=$NODE_PATH:`npm root -g`

# nvm / node version manager
# source /usr/share/nvm/init-nvm.sh


# Aliases ----------------------------------------------------------------------

alias vd='git diff | nvim -'
alias less='less -r'

alias ga='git add'
alias gaa='git add .'
alias gaaa='git add --all'
alias gau='git add --update'
alias gb='git branch'
alias gbd='git branch --delete '
alias gbda='git branch | egrep -v "(^\*|master|develop)" | xargs git branch -D'
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


# Add RVM to PATH for scripting. Make sure this is the last PATH variable change.
export PATH="$PATH:$HOME/.rvm/bin"

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
