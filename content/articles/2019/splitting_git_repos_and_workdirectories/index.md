---
title: Splitting Git Repos and Work Directories
subtitle: all the fun things git can do
tags:
  - howto
  - git
  - hugo
date: "2019-04-20"
repository: https://github.com/wschenk/split-git
remote: git@github.com:wschenk/split-git.git
---

I found a tutorial on [how to manage your dotfiles](https://www.atlassian.com/git/tutorials/dotfiles), that works by splitting up the git repository (normally the `.git` directory) from the work directory.  Since I have a lot of code that I put in my tutorials, I adapted the technique to have individual article directories mirrored in their own github repository.

## Repositories and Work Directories

The normal usage of `git` is to type `git clone <remote>` to get a copy of the local directory, mess with stuff, and then `add` and `commit` your changes.  If you have multiple branches, you switch the files in your work directory to a different branch, but you are still using the same repository.

The structure looks like this (you'll see more files in the lsit if you do this command, I edited them out for clarity)

```bash
$ git clone https://github.com/wschenk/willschenk.com
$ ls -la willschenk.com
drwxr-xr-x 1 wschenk wschenk  280 Apr 20 14:32 .
drwxr-xr-x 1 wschenk wschenk  870 Apr 20 14:58 ..
drwxr-xr-x 1 wschenk wschenk  102 Apr 18 13:11 content
drwxr-xr-x 1 wschenk wschenk  204 Apr 20 14:54 .git
-rw-r--r-- 1 wschenk wschenk  501 Apr 20 14:32 .gitignore
drwxr-xr-x 1 wschenk wschenk   64 Apr 20 14:22 layouts
drwxr-xr-x 1 wschenk wschenk   18 Mar 31 22:21 themes
```

The `.git` directory inside of this folder is where the git database lives, all of the commits and history and branches and all that.  At the same level you have the working copy, which in this case is the `master` branch.

What I'd like to do is to keep this whole structure intact, but also have the files in `content/articles/2019/splitting_git_repos_and_workdirectories/` tracked in an additional github repository for better sharing and collaboration.

## Splitting

This script is meant to be used as a replacement to the `git` command line on the shell to interact with the mirrored repository.

For this to work great create a remote repository somewhere [like GitHub](https://github.com/new) and then put the resulting `repostory` in the front matter.  (I use `yml` so adjust the awk parsing if you use something else.)

1. First it checks to see if there's a local .gitignore, and if not creates one.
2. Then it loads the `.local-git` file where we are checkout out the repository.  If this goes away we don't care, because...
3. It pulls down the repo from the root.
4. Then it passes all your commands to `git` pointing to the seperate repository and with the working dir set to the current directory.


[`seperate-git.bash`](seperate-git.bash):

{{% code file="articles/2019/splitting_git_repos_and_workdirectories/seperate-git.bash" language="js" %}}

## Running

Here's a comparison of what running looks like using the new command

```bash
$ bash seperate-git.bash status
Pulling down remote git repository into /tmp/tmp.p4KSGNKQNh
Cloning into bare repository '/tmp/tmp.p4KSGNKQNh'...
warning: You appear to have cloned an empty repository.
On branch master

Initial commit

Untracked files:
  (use "git add <file>..." to include in what will be committed)

        .gitignore
        index.md
        seperate-git.bash

nothing added to commit but untracked files present (use "git add" to track)

$ bash seperate-git.bash remote -v
origin  git@github.com:wschenk/split-git.git (fetch)
origin  git@github.com:wschenk/split-git.git (push)
```

And compare that with the regular `git` command:

```bash
$ git status
Changes not staged for commit:
  (use "git add <file>..." to update what will be committed)
  (use "git checkout -- <file>..." to discard changes in working directory)

        modified:   index.md

no changes added to commit (use "git add" and/or "git commit -a")

$ git remote -v
origin  git@github.com:wschenk/willschenk.com.git (fetch)
origin  git@github.com:wschenk/willschenk.com.git (push)
```

And if this script actually works, you should be able to see the results here: [https://github.com/wschenk/split-git](https://github.com/wschenk/split-git)


## References

1. https://www.atlassian.com/git/tutorials/dotfiles
