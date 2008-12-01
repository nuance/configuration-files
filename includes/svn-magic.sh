#!/bin/bash

SVN_PATH="svn+ssh://svn.yelpcorp.com/loc"

function newbranch {
    datestamp=`date +%Y%m%d`
    name=$1

    branchname="${USER}_${datestamp}_${name}"

    echo "Creating branch $branchname"

    echo "*** svn copy $SVN_PATH/main $SVN_PATH/branch/$branchname ***"
    svn copy $SVN_PATH/main $SVN_PATH/branch/$branchname

    cd ~/pg
    echo "*** copg branch/$branchname $name ***"
    ~/pg/copg branch/$branchname $name

    cd $name
    echo "*** switchloc ***"
    switchloc

    echo "*** screen title ***"
    echo -ne '\ek' $name '\e\\'

    echo "*** setting PYTHONPATH ***"
    export PYTHONPATH=`pwd`

    echo "*** restarting apache ***"
    myapachectl -k graceful
}

function switchtonewbranch {
    datestamp=`date +%Y%m%d`
    name=$1

    branchname="${USER}_${datestamp}_${name}"

    echo "Creating branch $branchname"

    echo "*** svn copy $SVN_PATH/main $SVN_PATH/branch/$branchname ***"
    svn copy $SVN_PATH/main $SVN_PATH/branch/$branchname
	svn switch $SVN_PATH/branch/$branchname

	folder=`pwd`

	cd ~/pg
	mv $folder $name
	
    cd $name
    echo "*** switchloc ***"
    switchloc

    echo "*** screen title ***"
    echo -ne '\ek' $name '\e\\'

    echo "*** setting PYTHONPATH ***"
    export PYTHONPATH=`pwd`

    echo "*** restarting apache ***"
    myapachectl -k graceful
}

# Create a branch from $1 with name $2 - don't run any yelp magic on it
function simplebranch() {
    datestamp=`date +%Y%m%d`
    name=$2

    branchname="${USER}_${datestamp}_${name}"

    echo "Creating branch $branchname"

    echo "*** svn copy $SVN_PATH/$1 $SVN_PATH/branch/$branchname ***"
    svn copy $SVN_PATH/$1 $SVN_PATH/branch/$branchname
    svn switch $SVN_PATH/branch/$branchname

    folder=`pwd`

    cd ~/pg
    mv $folder $name
	
    cd $name
}

function burl {
    svn info | grep URL | cut -f2 -d' '
}

function bname {
    burl | rev | cut -f1 -d'/' | rev
}

function bstart {
    svn log --stop-on-copy -r 1:HEAD | egrep "^r[0-9]+" | cut -f1 -d' ' | head -n 1 | sed 's/^r//'
}

function bstop {
    svn log --stop-on-copy -r 1:HEAD | egrep "^r[0-9]+" | cut -f1 -d' ' | tail -n 1 | sed 's/^r//'
}

# Both these use git-style pageing rather than the standard dump everything svn style

function blog {
    svn log --stop-on-copy | less
}

function bdiff {
    svn diff -r $(bstart):$(bstop) $SVN_PATH/branch/$(bname) | less
}

# Convenience functions

function bmerge {
    echo "Branch name: $(bname)"
    echo "Revision range: $(bstart):$(bstop)"
    echo "Merge string: merge -r $(bstart):$(bstop) $SVN_PATH/branch/$(bname)"
}

function lsbranch {
    # Assuming you're using my naming convention, there is a Y3K bug here
    svn ls $SVN_PATH/branch/ | egrep "^${USER}\_2"
}

function rmbranch {
    branch_name=$1

    echo "Removing $SVN_PATH/branch/$branch_name ..."
    # $* let's me pass in messages
    svn rm $SVN_PATH/branch/$branch_name $*
}

function switchloc {
    rm $HOME/pg/loc
    ln -s `pwd` $HOME/pg/loc
}
