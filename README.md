`kerscher.github.io`
====================

**WARNING! This branch is _highly outdated_ and probably only builds on x86 systems.**
**It hasn't been updated in years. Use the `main` branch instead.**
**This is for historical context only.** 

[![BSD3 license](https://img.shields.io/badge/licence-BSD%203--clause-blue.svg)](https://github.com/kerscher/kerscher.github.io/blob/source/LICENCE.md)

There is not much content at the moment, but foundations for writing are in place.

Instructions
------------

This is a [Hakyll](https://jaspervdj.be/hakyll) blog, and can be deployed on Github Pages as per instructions [here](https://jaspervdj.be/hakyll/tutorials/github-pages-tutorial.html), or by executing locally:

```shell
git stash
git checkout sources
stack build
stack exec kerscher-github-io -- clean
stack exec kerscher-github-io -- build
git fetch --all
git checkout -b master --track github/master
rsync --archive --filter='P docs/'          \
                --filter='P cache/'         \
                --filter='P .git/'          \
                --filter='P .gitignore'     \
                --filter='P .stack-work'    \
                --filter='P dist-newstyle/' \
                --delete-excluded           \
                docs/ .
git add --all
git commit -m "Publish."
git push github master:master
git checkout sources
git branch --delete --force master
git stash pop
```
