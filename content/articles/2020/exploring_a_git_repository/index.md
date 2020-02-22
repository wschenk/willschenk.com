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

I'm going to pull an old repo from github, one that probably was written to write GitHub itself (since the original author was a cofounder of the company) so it'll have some interesting artifact in there since, clearly, it wasn't built originally on GitHub.

```bash
cd /tmp
git clone https://github.com/ruby-git/ruby-git
```

## Who has worked on this project

1. Extract features from the repo `git log` command
2. Look at it `--reverse` which starts from the beginning
3. Use `--format` to specify what we are looking for
4. Sort everything (date is first) so that everything is in order.

The format options we are using are
- '%aI' author date in rfc822
- `%ae` author email
- `%an` author name

```bash
git log --reverse --pretty='format:%aI|%ae|%an|%cD' | sort > authors.log
```

We can process this data to get a sense of what is happening in the project.  Lets first group all the commits by month, see how many are made, and who was making them.

{{% embed "monthly_commits.rb" "ruby" "yes" %}}

We can visualize this with a simple HTML file

{{% embed "static/monthly.html" "html" %}}

Which looks like this:

{{% img img="monthly.png" style="width:100%" %}}

This repo is more that 12 years old at the time of this writing, and its clear that there's been different times when it's active and inactive. Lets fill in data for the months where "nothing" happened to the repository. As we cycle through the dates (which have been sorted previously) when we see a "new" month, we check to see if we expect to something between. If so, we add a line with zeros, and then repeat until we find the month in the file.

{{% embed "monthly_commits_show_blanks.rb" "ruby" "yes" %}}




We'll do this by using scripting things using ruby.

## Install the library

{{% embed "Gemfile" "ruby" "yes" %}}

Then run `bundle` to install the necessary project information.

Since we are using the `ruby-git` library and I don't know how it works, lets run our investigations against that.  Check it out wherever you want, I'm going to put it in `/tmp`

```bash
cd /tmp
git clone https://github.com/ruby-git/ruby-git
```

## Look at the log

The first thing we'll do is to look at the log.  We're going to look at the last 5 log messages and

- Print out the authors information and the timestamp of the commit
- The commit message
- The total count of lines added, deleted and files changed
- Counts of added and deleted for each file

{{% embed "log.rb" "ruby" "yes" %}}

## Specifying ranges

There are a lot of options other than _last 5_ to look at it.  We can also look at a time slice _since( "1 month ago" )_



Show commits, diff stats

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

1. https://pragprog.com/book/atcrime/your-code-as-a-crime-scene
2. http://www.adamtornhill.com/code/codemaat.htm
3. https://github.com/adamtornhill/code-maat
