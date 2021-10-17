# HUNTER HARRIS
# GENERATE MAKEFILE FOR HASKELL

# Usage
# Provide 1 argument NAME that is the file name the haskell
# source minus the extension. For example: test.hs -> test

# Script will generate a makefile with:
# the ghc compile
# the -Wall flag enabled

genFile() {
  echo -e "# HUNTER HARRIS
# GENERATED MAKEFILE

make_cmd:
\tghc -Wall $1.hs

clean:
\t@if [ -f $1 ]; then rm $1; fi && rm $1.hi $1.o" > makefile
}

# Gaurd against overwritting existing makefile
if [ -f makefile ]
then
  echo "makefile already exists, will not create new one"
  exit 1
fi

# Ensure name is provided
if [ "$#" != 1 ] 
then
  echo "expected 1 argument <NAME>, given $#"
  exit 1
else
  echo "generating makefile for $1"
  $(genFile $1)
  exit 0
fi

