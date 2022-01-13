#!/bin/sh

# This script is useful primarily when you have a target machine
# that can make its own 'local-target-features', but you'd rather run
# the bulk of the compilation on a different (presumably faster) machine.

# The script is to executed on the machine that compiles the Lisp tree.
# SSH is invoked to compile the C runtime.
# Passwordless login to the target machine is required.

#config_options="--without-gencgc --with-cheneygc"
if [ $1 = -p ]
then
  ssh_port_opt="-p $2"
  scp_port_opt="-P $2"
  shift ; shift
else
  ssh_port_opt=""
  scp_port_opt=""
fi
case $1 in
  sync)
   # Perform configuration on the target machine at the same git revision
   local_rev=`git rev-parse HEAD`
   checkout="git checkout $local_rev"
   ;;
  head)
   checkout="git checkout master"
   ;;
  nosync)
   checkout="echo not syncing remote"
   ;;
  *)
   echo "Usage error: cross-make.sh {sync|head|nosync} host dir [env]"
   exit 1
esac
shift
host=$1 # can have the form 'user@host' if necessary
root=$2 # path to source directory on $host
ENV=$3  # if you need to set SBCL_ARCH,CFLAGS,etc remotely

set -ex

ssh $ssh_port_opt $host cd $root \; $checkout '&&' \
  $ENV sh make-config.sh $config_options '&&' \
  mv version.lisp-expr remote-version.lisp-expr
scp $scp_port_opt $host:$root/{remote-version.lisp-expr,local-target-features.lisp-expr,output/build-id.inc} .
mv build-id.inc output
#diff version.lisp-expr remote-version.lisp-expr || exit 1

# make-host-1 and copy the generated C headers to the target machine
sh make-host-1.sh
tar cf - src/runtime/genesis | ssh $ssh_port_opt $host cd $root \; tar xf -

# make-target-1 and copy back the artifacts
ssh $ssh_port_opt $host cd $root \; $ENV sh make-target-1.sh
scp $scp_port_opt $host:$root/output/stuff-groveled-from-headers.lisp output

# make-host-2 and copy over the artifact
sh make-host-2.sh
scp $scp_port_opt -C output/cold-sbcl.core $host:$root/output

# make-target-2 and the two contribs required to execute 'core.test.sh'
ssh $ssh_port_opt $host cd $root \; sh make-target-2.sh '&&' \
   sh make-target-contrib.sh sb-posix sb-bsd-sockets
