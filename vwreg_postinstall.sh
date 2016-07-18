#!/bin/bash
#Check for file vwchk.txt and directory regparam
#Found vwchk not needed anymore - left in but commented out
#if [ -f vwchk.txt ]; then
#        rm -rf vwchk.txt
#fi
#
if [ ! -d regparam ]; then
        mkdir regparam
fi
#

