# Automated platform feature testing 

DIR=tools-for-build

# FIXME: Use this to test for dlopen presence and hence
# load-shared-object buildability

# $1 feature
# $2 additional flags
#
# Assumes the presence of $1-test.c, which when built and
# run should return with 104 if the feature is present.
#
featurep() {
    bin="$DIR/$1-test"
    rm -f $bin
    cc $DIR/$1-test.c $2 -o $bin > /dev/null 2>&1 && $bin > /dev/null 2>&1
    if [ "$?" = 104 ]
    then
	printf " :$1"
    fi
    rm -f $bin
}

featurep os-provides-dladdr -ldl
