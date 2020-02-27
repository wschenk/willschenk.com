---
title: Exploring a Git repository
subtitle: digital forensics
tags:
  - howto
  - git
  - ruby
date: "2020-02-10"
draft: true
---

Lets see what we can see about a code repository by looking at the `git` log to tell things about how mature and supported a project is. 

Install `git`.  When I run `git --version` on my laptop is says `2.11.0` which is old but fine for these purposes.

## First lets get a repo

I'm going to pull an old repo from GitHub, one that probably was written to write GitHub itself (since the original author was a co-founder of the company) so it'll have some interesting artifact in there since, clearly, it wasn't built originally on GitHub.

```bash
cd /tmp
git clone https://github.com/ruby-git/ruby-git
```

## Who has worked on this project

1. Extract features from the repo `git log` command
2. Look at it `--reverse` which starts from the beginning
3. Use `--format` to specify what we are looking for
4. Sort everything (date is first) so that everything is in order.

The format options we are using 

- `%aI` author date in rfc822
- `%ae` author email
- `%an` author name
- `%D` commit decoration (i.e. tag or branch)

which we then sort and output to our work file `commits.log`.  We're write out our analysis scripts to run off of that.

```bash
git log --reverse --pretty='format:%aI|%ae|%an|%D' | sort > commits.log
```

We can process this data to get a sense of what is happening in the project.  Lets first group all the commits by month, see how many are made, and who was making them.

1. `count` the number of commits, `last_month` is used to track the last month that we've seen in the commit log, and `people` is a map of names to commit counts
2. Loop over the `authors.log` file
3. Get the `month` but truncating the date
4. If we are in a different month, print out the current stats and reset
5. Print out the current month stats at the end

{{% embed "monthly_commits.rb" "ruby" "yes" %}}

We can visualize this with a simple HTML file

{{% embed "monthly.html" "html" %}}

Which looks like this:

{{% img img="monthly.png" style="width:100%" %}}

This repo is more that 12 years old at the time of this writing, and its clear that there's been different times when it's active and inactive. Lets fill in data for the months where "nothing" happened to the repository. As we cycle through the dates (which have been sorted previously) when we see a "new" month, we check to see if we expect to something between. If so, we add a line with zeros, and then repeat until we find the month in the file

1. Calculate the next expected month by converting to a tag object, shifting forward a month, and then `strftime` format the date to get to our format
2. If that's not what we see for the next commit month, print out a zero row
3. Go back to step 1, shifting our expect date a month forward
4. Unless we've skipped 24 months which seems like a long time for a project to be idle

{{% embed "monthly_commits_show_blanks.rb" "ruby" "yes" %}}

Here we can see that there have been long periods of time where nothing was happening with the code. Looking at the data, there was a bunch of stuff that happened starting 2007-11 though about 2009-10, then things got quiet again until 2011-08 through 2012-04, then a false start in 2013-01, and 2013-04 to 2015-01, a couple tweaks 2015-10, 2016-02, then 2018-01 through 2020-02 (time of writing). So some stopping and starting on this project.  Does that mean everything is stable? Maybe a new version of git came out, or Ruby idioms have changed or something.  We could probably look at the log messages or the actual code changes to tell.  But first lets figure out a timeline to see.

Variables:

1. `CONTRIBUTOR_ACTIVE_MONTHS` is the minimum time between commits that an author is considered active in the project, here 3.
2. `PROJECT_IDLE_MONTHS` is the minimum time between commits that a project is considered active, here 6.
3. `author_last_seen` is the date that we've last seen a committer.
4. `author_commits` is the count of their commits.
5. `last_month` is the same as above.
6. `period_commits` is the count of commits in the active period.
7. `period_authors` is the running count of author activity in the active period.
8. `tag_authors` is the running count of author activity between tags.

Logic:

1. Use `Date.parse` to use for the commit time, which rounds to the day.
2. First commit gets `project_started' event, project is `active`
3. First time we see a person in an active period, they get a `started_contributing` event
4. If we notice a `tag` string as a decorator, create a `project` `tag` event to help track releases. 
5. If the previous commit was more than `PROJECT_IDLE_MONTHS` ago, output a `project_idle` event with the `period_authors` (names with commit counts) data, and a total of the number of commits.
6. If the project was previously in the `idle` state and we got a commit, output a `project_active` event
7. Loop through all the active authors, and if any of their last commits is more than `CONTRIBUTOR_ACTITVE_MONTHS` is the past then they get a `left_project` event with their count of commits
8. At the end of the file, output a `last_data` event with the current list of period contributions.
9. Sort everything by date and output the file.

{{% embed "project_timeline.rb" "ruby" "yes" %}}

This will spit out a CSV file that summarizes what happens to the project to get an overall sense of the timeline. Lets create an HTML page to visualize this.  In this we will calculate the duration between active and idle states so we can group a bit of what's happening over all.  Also we can look at who actually made the tag, which will give us a sense of who is the active maintainer of the project.

{{% embed "project_timeline.html" "html" %}}

## Looking at entities




## All these work off range

- start/stop by time
- start/stop by tag
- start/stop by release?

## Summary

          statistic,                 value
          number-of-commits,           919
          number-of-entities,          730
          number-of-entities-changed, 3397
          number-of-authors,            79
		  
## Activity by author

maybe added, removed, entities touched

         author,        added, deleted
         Adam Tornhill, 13826,    1670
         Some One Else,   123,      80
         Mr Petersen,       3,       3

## Org Metrics

          entity,         n-authors, n-revs
          InfoUtils.java, 12,        60
          BarChart.java,   7,        30
          Page.java,       4,        27


## References

1. https://git-scm.com/docs/git-log
1. https://stackoverflow.com/questions/13208734/get-the-time-and-date-of-git-tags
1. https://pragprog.com/book/atcrime/your-code-as-a-crime-scene
2. http://www.adamtornhill.com/code/codemaat.htm
3. https://github.com/adamtornhill/code-maat
