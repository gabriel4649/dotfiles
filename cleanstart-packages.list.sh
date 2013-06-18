#! /bin/bash
 
################################################################################
# cleanstart-packages.list.sh
# by silverwav - OpenPGP key:03187548 15 Apr 2009
#
# Note this whole script just does:
#
# apt-get install -y package1 package2 package3 ...
#
# But it reads the packages from a file(packages.list) and does some checking.
#
# This turns out to be very handy as you can add notes or comment things out.
# (After 6 months its handy to have a note of what packages to reinstall and why!)
# Useful for clean installs and VMs to load optional packages quickly.
#
################################################################################
#
# Usage:
# cleanstart-packages.list.sh
# (reads a file called packages.list by default).
# or
# cleanstart-packages.list.sh <filename>
#
# Any line starting with a # is ignored as are Blank lines.
# Any other lines, the first word is taken as the package name.
#
# Stops on any error. Correct and rerun.
#
################################################################################
#
# Based on a script found on this site:
# Ubuntu TIP: Automating Package Installation – apt-get to the rescue
# http://tech.shantanugoel.com/2008/03/06/
# ubuntu-tip-automating-package-installation-%E2%80%93-apt-get-to-the-rescue.html
#
################################################################################
clear
echo "--------------------------------------------------------------------------------"
echo "                 (cleanstart) Script for installing packages (client)                 "
echo "--------------------------------------------------------------------------------"
 
# ensure script is run as root/sudo
if [ "$(id -u)" != "0" ]
then
    echo ""
    echo "Must execute the script as root user."
    echo "--------------------------------------------------------------------------------"
    exit 1
fi
 
# check the argument count
if [ $# -gt "1" ]
then
    echo ""
    echo "Only one file with package names allowed."
    echo "--------------------------------------------------------------------------------"
    exit 1
fi
 
################################################################################
#### Main
#### args: (1)
#### 1. [out] List of input package names
################################################################################
 
# package names to be installed
PACKAGE_NAME_LIST=""
 
# check if filename was supplied as comand line parameter
if [ $# -eq "1" ]
then
    PACKAGE_NAME_LIST=$(cat $1 | grep -v -e "^#" | cut -f1 -d' ')
else
    PACKAGE_NAME_LIST=$(cat packages.list | grep -v -e "^#" | cut -f1 -d' ')
fi
 
echo ""
echo "Installing packages:" ${PACKAGE_NAME_LIST}
echo "--------------------------------------------------------------------------------"
 
apt-get install -y ${PACKAGE_NAME_LIST}
 
echo ""
echo "Done"
echo "--------------------------------------------------------------------------------"
