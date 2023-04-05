---
title: Indexing a hugo site using pagefind
subtitle: static all the way down
tags:
  - hugo
date: 2023-04-05
draft: true
---

## Build the site

Once you've built site, using `hugo` in our case, we need to run `pagefind` to index the output:

```bash
npx pagefind --source "public"
```

# References

1. https://pagefind.app/docs/installation/
