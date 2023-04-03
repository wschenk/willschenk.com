#!/bin/bash

WORKDIR=/tmp/repos

function runstats {
    cd $1
    pwd

    if [ ! -f ../first_commit.json ]; then
        askgit "select message, author_name, author_email, author_when 
               from commits order by committer_when asc limit 1" --format json > ../first_commit.json
    fi

    askgit "select summary, author_name, author_when, additions 
           from commits order by additions desc limit 10" --format json > ../big_adds.json
    askgit "select summary, author_name, author_when, deletions 
           from commits order by deletions desc limit 10" --format json > ../big_dels.json

    askgit "select count(*) as count, author_name 
           from commits group by author_name 
           order by count desc" --format json > ../commiters.json

    askgit "select strftime(\"%Y-%m\", author_when) as 'month', 
       count(*) as total_commits, sum(additions), sum(deletions)
       from commits group by strftime(\"%Y-%me\", author_when) 
       order by strftime(\"%Y-%m\", author_when);" --format json > ../commits_timeline.json


    askgit "select name, commits.id, commits.message, author_name, author_email, author_when 
           from tags, commits where tags.target_type = commits.id order by author_when" --format json > ../tags.json

    askgit "select name, commits.id, author_when from tags, commits 
           where tags.target_type = commits.id order by author_when" --format csv > ../tags.csv 

    PREV_DATE=$(askgit "select min(author_when) as first from commits" --format csv |grep -v first)
    while IFS=, read -r tag id date; do
        if [[ "$tag" != "name" ]]; then
            echo ${tag},${id},${PREV_DATE},${date}
            PREV_DATE=${date}
        fi
    done < ../tags.csv > ../tag_ranges.csv

    #mkdir -p ../tags

    #while IFS=, read -r tag id start end; do
    #echo $tag
    #askgit "select count(*) as count from commits where author_when > '${start}' and author_when <= '${end}'" --format json | jq .count 
    #askgit "select count(*) as count, author_name from commits where author_when > '${start}' and author_when <= '${end}' group by author_name order by count desc"
    #done < ../tag_ranges.csv
}


find ${WORKDIR} -name repo -type d -print |  
while IFS= read -r repo
do
runstats $repo
exit
    echo repo is $repo
done
