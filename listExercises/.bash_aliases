# HUNTER HARRIS
# BASH ALIASES

# improving ls
alias ls="ls --color"
alias la="ls -a"
alias ll="ls -lh"

# convenience (misc)
alias vscode="code"
alias chmod="sudo chmod"
alias o="xdg-open"
alias vi="vim"
alias reload="source ~/.bashrc"
alias reinit="cp ~/Dev/BashConfig/.bash* ."

# system monitoring
alias iptraf="sudo iptraf-ng"
alias nethog="sudo nethogs"
alias os?="lsb_release -a"
alias ramhog="ps -o pid,user,%mem,command ax | sort -b -k3 -r | head -n 20"

# cal poly
alias cpvpn="sudo openconnect --protocol=gp cpvpn.calpoly.edu --user=hvharris"
alias cpconnect="ssh hvharris@unix2.csc.calpoly.edu"

# homebrewing
alias home="cd ~"
alias turt="python3 ~/Projects/Turtle/shell.py"
alias claw="python3 ~/Projects/Claw/src/claw.py"

# notes
alias thelp="less ~/Documents/.terminator_help"
alias vhelp="less ~/Documents/.vim_help"

# navigation
alias go-claw="cd ~/Projects/Claw/"
alias go-turt="cd ~/Projects/Turtle/"
alias go-abyss="cd ~/Graphics/Abyss/"


