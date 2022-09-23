############################################################
## Bash
############################################################

alias ..="cd .."
alias ...="cd ../../"
alias ....="cd ../../../"
alias .2="cd ../../"
alias .3="cd ../../../"
alias .4="cd ../../../../"
alias .5="cd ../../../../../"
alias ~="cd ~"

alias c="clear"

alias path='echo -e ${PATH//:/\\n}'
alias ax="chmod a+x"

############################################################
## List
############################################################

if [[ `uname` == 'Darwin' ]]; then
  alias ls="ls -G"
  # good for dark backgrounds
  export LSCOLORS=gxfxcxdxbxegedabagacad
else
  alias ls="ls --color=auto"
  # good for dark backgrounds
  export LS_COLORS='no=00:fi=00:di=00;36:ln=00;35:pi=40;33:so=01;35:bd=40;33;01:cd=40;33;01:or=01;05;37;41:mi=01;05;37;41:ex=00;31:'
fi

alias l="ls"
alias ll="ls -lh"
alias la="ls -a"
alias lal="ls -alh"

############################################################
## OS X
############################################################

# remove .DS_Store files recursively
alias dstore-clean='find . -type f -name .DS_Store -print0 | xargs -0 rm'

# Show/hide hidden files in Finder
alias showdotfiles="defaults write com.apple.finder AppleShowAllFiles -bool true && killall Finder"
alias hidedotfiles="defaults write com.apple.finder AppleShowAllFiles -bool false && killall Finder"

############################################################
## Git
############################################################

alias gs='git status'
alias gl='git log --oneline'
alias gb='git checkout -b'
alias gd='git diff'
alias gfp='git fetch -p'

############################################################
## Miscellaneous
############################################################

alias e='emacs'

export GREP_COLOR="1;37;41"
alias grep="grep --color=auto"

alias flushdns='dscacheutil -flushcache'

alias whichlinux='uname -a; cat /etc/*release; cat /etc/issue'
