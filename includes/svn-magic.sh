#!/bin/bash

function newbranch {
    datestamp=`date +%Y%m%d`
    name=$1

    branchname="${USER}_${datestamp}_${name}"

    echo "Creating branch $branchname"

    echo "*** svn copy https://svn/loc/main https://svn/loc/branch/$branchname ***"
    svn copy https://svn/loc/main https://svn/loc/branch/$branchname

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

    echo "*** svn copy https://svn/loc/main https://svn/loc/branch/$branchname ***"
    svn copy https://svn/loc/main https://svn/loc/branch/$branchname
	svn switch https://svn/loc/branch/$branchname

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

function bname() {
    svn info | grep URL | cut -f2 -d' ' | rev | cut -f1 -d'/' | rev
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
    svn diff -r $(bstart):$(bstop) https://svn/loc/branch/$(bname) | less
}

# Convenience functions

function bmerge {
    echo "Branch name: $(bname)"
    echo "Revision range: $(bstart):$(bstop)"
    echo "Merge string: merge -r $(bstart):$(bstop) https://svn/loc/branch/$(bname)"
}

function lsbranch {
    # Assuming you're using my naming convention, there is a Y3K bug here
    svn ls https://svn/loc/branch/ | egrep "^${USER}\_2"
}

function switchloc {
    rm $HOME/pg/loc
    ln -s `pwd` $HOME/pg/loc
}
