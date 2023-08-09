fpath+=$HOME/.zfunc

show_colour() {
    printf '\e]4;1;%s\a\e[0;41m  \e[m' "$1"
}

autoload -U compinit promptinit
compinit
promptinit

zstyle ':completion:*' menu select

setopt completealiases
setopt HIST_IGNORE_DUPS

bindkey -e
bindkey "^[[5~" history-beginning-search-backward #PageUp
bindkey "^[[6~" history-beginning-search-forward #PageDown

SAVEHIST=1000  # Save most-recent 1000 lines
HISTFILE=~/.zsh_history

. /usr/share/zsh/plugins/zsh-history-substring-search/zsh-history-substring-search.zsh

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh 
#source $HOME/.zsh/catppuccin_mocha-zsh-syntax-highlighting.zsh

eval "$(starship init zsh)"

if [ -f $HOME/.theme ]; then
	. $HOME/.theme
fi

export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.ghcup/bin:$PATH"
export PATH="$HOME/.cabal/bin:$PATH"
export PATH="$HOME/.local/share/gem/ruby/3.2.0/bin:$PATH"
export TERM="xterm-256color"                   
export HISTCONTROL=ignoredups:erasedups           
export ALTERNATE_EDITOR="nvim"                        
export EDITOR="nvim"                              
# export VISUAL=""
export BAT_THEME="ansi"
export RANGER_LOAD_DEFAULT_RC=FALSE


alias Tamzen-20=Tamzen10x20r

alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles.git/ --work-tree=$HOME'
alias agenda='/usr/bin/git --git-dir=$HOME/.agenda.git/ --work-tree=$HOME'

#alias reboot='sudo reboot'
alias shutdown='shutdown -h now'

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
alias ls='exa -al --color=always --group-directories-first' 
alias la='exa -a --color=always --group-directories-first'  
alias ll='exa -l --color=always --group-directories-first'  
alias lt='exa -aT --color=always --group-directories-first' 
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

export PATH=$PATH:/home/nehemias/.spicetify
