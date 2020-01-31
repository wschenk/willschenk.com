#!/bin/bash

for i in *.base
do
    last_file="$i"
    final_file="${i%.base}"

    for stage in ${final_file}.*
    do
	# Strip out emacs junk
	if [[ $stage == *~ ]]; then
	    continue    
	fi
	
	# We are at the end!
	if [[ $stage == *.base ]]; then
	    stage=$final_file
	fi

	# Skip parsing of diff files
	if [[ $stage == *.diff ]]; then
	    continue
	fi

	echo Creating $stage.diff

	# -Z Ignores space
	# -U 1 sets to the unified patch format with one surrounding lines
	# Setting --label directly lets use avoid timestamps
	diff -Z -U 1 --label $last_file --label $stage $last_file $stage > $stage.diff

	last_file=$stage
    done
    echo
done
