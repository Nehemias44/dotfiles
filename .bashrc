# .bashrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

run-help() { help "$READLINE_LINE" 2>/dev/null || man "$READLINE_LINE"; }
bind -m vi-insert -x '"\eh": run-help'
bind -m emacs -x     '"\eh": run-help'

PS1="\[\033[1;32m\]\h\[\033[0;37m\]:\[\033[31m\][\[\033[1;34m\]\u\[\033[0;31m\]]\[\033[0;37m\]:\[\033[35m\]\w\[\033[1;33m\]$\[\033[0m\] "

export PATH="$HOME/.local/bin/:$PATH"
export TERM="xterm-256color"                      
export HISTCONTROL=ignoredups:erasedups           
export ALTERNATE_EDITOR=""                        
export EDITOR="nano"                              
export VISUAL="emacs"
export BAT_THEME="ansi"

alias gitdot='/usr/bin/git --git-dir=$HOME/.dotfiles.git/ --work-tree=$HOME'

alias reboot='sudo reboot'
alias shutdown='sudo shutdown'

alias vim='nvim'
alias cat='bat'
alias top='htop'

# Navegacion
alias ..='cd ..' 
alias ...='cd ../..'
alias .3='cd ../../..'
alias .4='cd ../../../..'
alias .5='cd ../../../../..'

# Confirmar antes
alias cp="cp -i"
alias mv='mv -i'
alias rm='rm -i'

# Colorea la salida del comando grep (Bueno para los archivos logs)
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

# Cambiando "ls" por "exa"
# alias ls='exa --color=always --group-directories-first --icons'
alias ls='exa -al --color=always --group-directories-first --icons' 
alias la='exa -a --color=always --group-directories-first --icons'  
alias ll='exa -l --color=always --group-directories-first --icons'  
alias lt='exa -aT --color=always --group-directories-first --icons' 
alias l.='exa -a | egrep "^\."'

alias xlog='bat $HOME/.local/share/xorg/Xorg.0.log'
alias xerr='cat $HOME/.local/share/xorg/Xorg.0.log | grep "(EE)"'
alias xwar='cat $HOME/.local/share/xorg/Xorg.0.log | grep "(WW)"'

# youtube-dl
alias yta-aac="youtube-dl --extract-audio --audio-format aac "
alias yta-best="youtube-dl --extract-audio --audio-format best "
alias yta-flac="youtube-dl --extract-audio --audio-format flac "
alias yta-m4a="youtube-dl --extract-audio --audio-format m4a "
alias yta-mp3="youtube-dl --extract-audio --audio-format mp3"
alias yta-opus="youtube-dl --extract-audio --audio-format opus "
alias yta-vorbis="youtube-dl --extract-audio --audio-format vorbis "
alias yta-wav="youtube-dl --extract-audio --audio-format wav "
alias ytv-best="youtube-dl -f bestvideo+bestaudio "
