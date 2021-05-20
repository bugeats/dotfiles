# this file is for zsh specific stuff
# where ~/.profile is for more general environment setup

# VI mode mutherfucker
set -o vi

star=$'\xe1\x97\x92'

autoload -U colors && colors

# Load version control information
autoload -Uz vcs_info
precmd() { vcs_info }

# Format the vcs_info_msg_0_ variable
zstyle ':vcs_info:git:*' formats ' [%b]'

# Enable substitution in the prompt.
setopt prompt_subst

PROMPT="%{$fg[grey]%}%~%{$reset_color%}%{$fg[yellow]%}\$vcs_info_msg_0_ %{$fg[magenta]%}$star %{$reset_color%}%"

# Now load general profile
. ~/.profile
