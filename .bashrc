# this file is for bash specific stuff
# where ~/.profile is for more general environment setup

# VI mode mutherfucker
set -o vi

# Display git status in prompt
parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ [\1]/'
}

# echo -n X | hexdump

YELLOW='\[\033[33m\]'
BLUE='\[\033[34m\]'
RED='\[\033[31m\]'
DIM='\[\033[2m\]'
PS_CLEAR='\[\033[0m\]'
star=$'\xe1\x97\x92'

export PS1="${DIM}\w${PS_CLEAR}${YELLOW}\$(parse_git_branch) ${RED}$star ${PS_CLEAR}"

# Show current working directory in iTerm tab title
# https://gist.github.com/phette23/5270658
if [ $ITERM_SESSION_ID ]; then
  export PROMPT_COMMAND='echo -ne "\033];${PWD##*/}\007"; ':"$PROMPT_COMMAND";
fi

# Now load general profile
. ~/.profile

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
