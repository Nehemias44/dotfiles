# Enable autocomplete commands
autoload -U compinit
compinit

# Autocomplete via keys
zstyle ':completion:*' menu select

# Find news executable on $PATH
zstyle ':completion:*' rehash true

# Auntocomplete alias
setopt completealiases

# Enable history
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory

# Ignore deplicates in history
setopt HIST_IGNORE_DUPS

# Remenber lasts visited directories 
DIRSTACKFILE="$HOME/.cache/zsh/dirs"
if [[ -f $DIRSTACKFILE ]] && [[ $#dirstack -eq 0 ]]; then
  dirstack=( ${(f)"$(< $DIRSTACKFILE)"} )
  [[ -d $dirstack[1] ]] && cd $dirstack[1]
fi
chpwd() {
  print -l $PWD ${(u)dirstack} >$DIRSTACKFILE
}

DIRSTACKSIZE=20

setopt autopushd pushdsilent pushdtohome

## Delete duplicate entries
setopt pushdignoredups

## Revert operators +/-.
setopt pushdminus

# Syntax highlighting
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Automatic autocomplete suggestions 
source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh

# Enable colors
autoload -U colors && colors

# Prompt
PROMPT="%{$fg_bold[yellow]%}%n%{$fg[magenta]%} in%{$fg_bold[green]%} %1~%{$fg_bold[white]%} do: %{$reset_color%}"



# Alias

## personal
alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles.git/ --work-tree=$HOME'
alias agenda='/usr/bin/git --git-dir=$HOME/.agenda.git/ --work-tree=$HOME'

## ls to eza
alias ls='eza'
alias la='eza -la --group-directories-first --icons'
alias lt='eza --tree'

## cat to bat
export BAT_THEME="ansi"
alias cat='bat'

cd $HOME
