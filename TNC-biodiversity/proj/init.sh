#!/bin/bash

# set everything up

cd 
mkdir TNC
cd TNC

mkdir results
mkdir -p data/proj

# load scripts
svn export https://dapa-climate-change.googlecode.com/svn/trunk/TNC-biodiversity/proj/project.sh
svn export https://dapa-climate-change.googlecode.com/svn/trunk/TNC-biodiversity/proj/variables.sh
svn export https://dapa-climate-change.googlecode.com/svn/trunk/TNC-biodiversity/proj/copy_daemon.sh
