# ~/.bash_profile: executed by bash for login shells.

if [ -e ~/.bashrc ] ; then
  . ~/.bashrc
fi

# Put all local machine customizations in ~/.bash_local
if [ -e ~/.bash_local ] ; then
  . ~/.bash_local
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi