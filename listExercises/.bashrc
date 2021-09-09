# HUNTER HARRIS
# BASH PROFILE


# welcome message
# TODO: Find cool text file

### CONDITIONAL IMPORTS ###
# if a file exist in current directory import it
# in not, check in home directory
# allows for local development of support files w/o
# 'global' files in home directory interfering

# aliases
if [ -f ./.bash_aliases ]
  then
    . ./.bash_aliases
  else
    [ -f ~/.bash_aliases ] && . ~/.bash_aliases
fi

# prompts
if [ -f ./.bash_prompts ]
  then
    . ./.bash_prompts
  else
    [ -f ~/.bash_prompts ] && . ~/.bash_prompts
fi
