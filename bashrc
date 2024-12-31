############################################################
## Terminal behavior
############################################################

# # Change the window title of X terminals
# case $TERM in
#   xterm*|rxvt|Eterm|eterm)
#     PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/$HOME/~}\007"'
#     ;;
#   screen)
#     PROMPT_COMMAND='echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/$HOME/~}\033\\"'
#     ;;
# esac

function parse_git_dirty {
  [[ $(git status 2> /dev/null | tail -n1) != "nothing to commit, working directory clean" ]] && echo "*"
}
function parse_git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/\1$(parse_git_dirty)/"
}

if [ `which git 2> /dev/null` ]; then
  function git_prompt {
    parse_git_branch
  }
else
  function git_prompt {
    echo ""
  }
fi

if [ -n "$BASH" ]; then
  # export PS1='\[\033[32m\]\n[\s: \w] $(git_prompt)\n\[\033[31m\][\u@\h]\$ \[\033[00m\]'
  export PS1='\[\033[32m\]\n[\s: \w] \[\033[36m\]$(git_prompt)\n\$  \[\033[00m\]'
fi

############################################################
## Optional shell behavior
############################################################

shopt -s cdspell
shopt -s extglob
shopt -s checkwinsize
shopt -s histappend

export PAGER="less"
export EDITOR="vim"

############################################################
## History
############################################################

export HISTIGNORE="&:pwd:ls:ll:lal:[bf]g:exit:rm*:sudo rm*"
# remove duplicates from the history (when a new item is added)
export HISTCONTROL=erasedups
# increase the default size from only 1,000 items
export HISTSIZE=10000

############################################################
## Aliases
############################################################

if [ -e ~/.bash_aliases ]; then
  . ~/.bash_aliases
fi

############################################################
## Bash Completion, if available
############################################################

if [ -f /usr/local/etc/bash_completion ]; then
  . /usr/local/etc/bash_completion
elif  [ -f /etc/bash_completion ]; then
  . /etc/bash_completion
elif  [ -f /etc/profile.d/bash_completion ]; then
  . /etc/profile.d/bash_completion
elif [ -e ~/.bash_completion ]; then
  # Fallback. This should be sourced by the above scripts.
  . ~/.bash_completion
fi

## source fzf if available
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

## MYSQL
export MYSQL_PS1="\u [\d]> "

## direnv
eval "$(direnv hook bash)"


## add Homebrew to the PATH
export PATH=/opt/homebrew/bin:$PATH

## disable turbo telemetry
export TURBO_TELEMETRY_DISABLED=1