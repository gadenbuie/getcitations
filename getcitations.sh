#!/bin/bash
name=`echo $1 | cut -f1 -d'.'`
Rscript exportcitations.R $1 $name.bib
