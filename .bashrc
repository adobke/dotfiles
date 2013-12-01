#
# ~/.bashrc
#

BROWSER=google-chrome
export BROWSER
EDITOR=vim
export EDITOR
# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
PS1='\[\e[1;36m\][\u@\h \W]\$\[\e[0m\] '

export HISTCONTROL=ignoreboth
complete -cf sudo
complete -cf man
