if [ $# != 1 ]; then
    echo "usage: $0 <version>"
    echo "e.g. $0 0.1"
    exit
else
    find algol60/ ! -type d | egrep -v '~$|\.svn' | tar cvjf gcc-algol60-$1.tar.bz2 -T -
fi
