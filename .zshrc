autoload -U compinit promptinit
compinit
promptinit

# Esto establecerá el prompt por defecto al tema walters
# prompt walters

zstyle ':completion:*' menu select

setopt completealiases
setopt HIST_IGNORE_DUPS

# [[ -n "${key[PageUp]}"   ]]  && bindkey  "${key[PageUp]}"    history-beginning-search-backward
# [[ -n "${key[PageDown]}" ]]  && bindkey  "${key[PageDown]}"  history-beginning-search-forward

bindkey "^[[5~" history-beginning-search-backward #PageUp
bindkey "^[[6~" history-beginning-search-forward #PageDown

SAVEHIST=1000  # Save most-recent 1000 lines
HISTFILE=~/.zsh_history

source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

eval "$(starship init zsh)"

if [ -f $HOME/.theme ]; then
	. $HOME/.theme
fi

export PATH="$HOME/.local/bin/:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export TERM="xterm-256color"                   
export HISTCONTROL=ignoredups:erasedups           
export ALTERNATE_EDITOR=""                        
export EDITOR="nano"                              
export VISUAL="emacs"
export BAT_THEME="ansi"


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
alias ls='exa --icons --color=always --group-directories-first'
alias ls='exa --icons -al --color=always --group-directories-first' 
alias la='exa --icons -a --color=always --group-directories-first'  
alias ll='exa --icons -l --color=always --group-directories-first'  
alias lt='exa --icons -aT --color=always --group-directories-first' 
alias l.='exa -a | egrep "^\."'

# pacman and yay
alias pacsyu='sudo pacman -Syu'                  # update only standard pkgs
alias pacsyyu='sudo pacman -Syyu'                # Refresh pkglist & update standard pkgs
alias yaysua='yay -Sua --noconfirm'              # update only AUR pkgs (yay)
alias yaysyu='yay -Syu --noconfirm'              # update standard pkgs and AUR pkgs (yay)
alias unlock='sudo rm /var/lib/pacman/db.lck'    # remove pacman lock
alias cleanup='sudo pacman -Rns $(pacman -Qtdq)' # remove orphaned packages

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
