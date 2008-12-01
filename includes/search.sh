export CURRENT_BRANCH=loc
export BT=/nail/home/$USER/pg/${CURRENT_BRANCH}/
export PYTHONPATH=$BT

# these should be your own unique ports:
# class business search
export LUCY_PORT=13500
export LUCY_JCONSOLE_PORT=13501
# talk search
export CATHY_PORT=13502
# spell checker
export VLAD_PORT=13503
# review
export REVIEWSEARCH_PORT=13504
# list
export LISTSEARCH_PORT=13505
# For tmp files.
export YELP_DEVELOPER_PID_PREFIX=$USER-
# Index location for lucene, yelp_search.sh will add yelp_index to
# this name.
export YELP_INDEX_PATH=/nail/home/$USER/pg/${CURRENT_BRANCH}/search/lucy/
# Set up your class path
export CLASSPATH=$(find /nail/home/$USER/pg/${CURRENT_BRANCH}/search/lucy/lib/ -name "*.jar" -print  | awk '{ printf("%s:", $1) }')
