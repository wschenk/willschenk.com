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

Lets see what we can see about a code repository by looking at the `git` log to tell things about how mature and supported a project is.  We'll do this by using scripting things using ruby.

## Install the library

{{% embed "Gemfile" "ruby" "yes" %}}

Then run `bundle` to install the necessary project information.

## Look at the log from start time to end time

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
