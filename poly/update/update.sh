#!
# update.sh

declare SRCDIR="/home/sa/src"

update()
{
    echo "root: $1"
    for project in `ls -d $1/*`; do
        if test -d $project; then
            echo
            echo "project: $project"
            for item in `ls -A $project`; do
                if [[ $item == ".bzr" ]] ; then
                    cd $project
                    bzr pull
                    bzr update
                elif [[ $item == ".svn" ]]; then 
                    cd $project
                    svn update
                elif [[ $item == ".git" ]]; then
                    cd $project
                    git pull
                 elif [[ $item == ".hg" ]]; then
                     cd $project
                     hg pull
                     hg update
                else 
                    continue
                fi
            done
        fi
    done
}


# main
if test -z $@; then
    if test -d $SRCDIR; then
        update $SRCDIR
    fi
else
    for path in $@; do
        update $path
    done
fi

