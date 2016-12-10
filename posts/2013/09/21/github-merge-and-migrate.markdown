---
author: Ian
tags: programming
title: "GitHub: Merging Repos, Migrating Issues"
published: 2013-09-21 12:41:04
---

For the [BayesHive][bh] project, we have code split over a number of
different Git repositories, all hosted on GitHub.  We use GitHub for
issue tracking and keeping notes on wikis.

Some of the splitting into separate repositories makes sense (our web
app is in its own repository, for example), but some of the splitting
is mostly historical and makes things a little inconvenient -- the
infrastructure for the Baysig statistical modelling language lives in
three different repositories, which made sense at one time, but
doesn't now.  Having these closely related bits of code in different
repositories makes it more difficult to track down regressions (`git
bisect` doesn't work across multiple repositories) and can make
branching messy (if you're implementing a feature that requires work
in multiple different repositories, you need to remember to create a
branch in each of them, otherwise it's easy to commit inconsistent
changes to the master branch in one repo and your topic branch in
another repo).

As a result of all this, we decided to merge some of our repositories
together.  Based on a bit of Googling, this seems to be a relatively
common requirement, and the advice that's on offer out there is a
little conflicting and confusing.  In this article, I'll try to reduce
some of the confusion related to repository merging, as well as
showing how to migrate GitHub issues from the pre-existing multiple
repositories into the new merged repository.  I've written a couple of
small Haskell programs to do these tasks -- they're available
[here][repo].

<!--MORE-->

## Merging repositories

What we want to do here is to take a number of pre-existing
repositories and merge them all into a single repository, with the
contents of each pre-existing repository in a separate sub-directory,
and a single common history of commits reflecting the historical order
of changes made to each of the pre-existing repositories.

You will find lots of pages talking about Git sub-projects, sub-tree
merging, and other slightly exotic approaches.  That's not what's
needed here.  The best explanation I've seen of a simple approach that
(almost) works is [here][merge].  Apart from one small error, this
does the trick.

Assuming that we have repositories called `repo1`, `repo2`, ... at
URLs `url1`, `url2`, ..., the merge goes in two stages (automated in
the `merge-repos.hs` program in my [git-utils repository][repo]):

### Step 1: pre-processing

For each repository, we want to create a copy of the repository that
has its Git index rewritten to make it so that all of the items in the
repository live in the sub-directory that we want to use for this
individual repository in the merged repository.  (Rewriting the Git
index this way makes it look as though these things have *always*
lived in this sub-directory, which is exactly what we want.)

For each repository, we do this (taken verbatim from [here][merge]):

~~~~ {.shell}
git clone url1 repo1_copy
cd repo1_copy
git filter-branch --prune-empty --tree-filter '
if [[ ! -e repo1 ]]; then
  mkdir -p repo1
  git ls-tree --name-only $GIT_COMMIT | xargs -I files mv files repo1
fi'
~~~~

The result of this is that we will have one `_copy` repository for
each of our "input" repositories, with the contents reorganised to
live under a sub-directory.

### Step 2: merging

We now want to merge the individual copy repositories.  Start by
cloning the first one:

~~~~ {.shell}
git clone repo1_copy merged
cd merged
~~~~

Now, for each of the remaining repositories, we do the following:

~~~~ {.shell}
git remote add -f repo2 ../repo2_copy
git merge -s ours --no-commit repo2/master
git read-tree --prefix=/ -u repo2/master
git commit -m "Merged repo2"
git pull -s subtree repo2 master
~~~~

(The minor mistake in [the instructions I followed][merge] is in the
`git read-tree` command had the wrong value for the `prefix` argument,
which was leading to extra levels of sub-directories in the merged
repository.)

Once you've done this for each of your pre-existing repositories,
you'll end up with a single repository with a common history, which
you can then push to a new remote on GitHub or wherever for people to
use.


## Migrating issues

If you use GitHub, merging your repositories is only part of the
story.  You probably also want to take all of the issues on your
pre-existing repositories and migrate them to the merged repository.
And you don't want to do that by hand!  Fortunately, GitHub has a web
API that makes this pretty easy to do.  I did it using Haskell (the
`merge-issues.hs` program in my [git-utils repository][repo]), but you
could do it using whatever tools your comfortable with.  The Haskell
[github][hs-github] library is very convenient though.

I did this in two stages: first, getting all the issues and associated
comments from the pre-existing repositories, then applying these
issues to the new repository.

Cutting out the details of error handling, getting all the issues for
a given repository is simple (here, `ghAuth` is our authentication for
connecting to GitHub, `ghRepoUser` is the GitHub user name of the
owner of the repository we're accessing and `r` is the repository
name):

~~~ {.haskell}
eopen <- issuesForRepo' (Just ghAuth) ghRepoUser r []
eclosed <- issuesForRepo' (Just ghAuth) ghRepoUser r [OnlyClosed]
~~~~

(We get the open and closed issues separately, just because the GitHub
API defaults to giving open issues only.)  Then we can get all the
comments for a given issue, represented by the Haskell value `i`:

~~~~ {.haskell}
ecs <- comments' (Just ghAuth) ghRepoUser r (issueNumber i)
~~~~

Once we have all the information about all the issues in the
pre-existing repositories, we merge that information to produce a ful
list of issues for the new repository sorted globally by issue
creation date.  We can then run through this global list of issues,
creating them on the new merged repository one by one.  Here, we
create a single issue, for a repository `r` (the names like
`issueTitle`, `issueBody`, etc. are fields in the structure describing
the new issue, brought into scope with a record wildcard):

~~~~ {.haskell}
let labels = Just $ r : map labelName issueLabels
    newi = NewIssue issueTitle
                    issueBody
                    (githubOwnerLogin <$> issueAssignee)
                    (milestoneNumber <$> issueMilestone)
                    labels
res <- createIssue ghAuth ghUser ghRepo newi
~~~~

We add an extra label based on the original repository name to allow
us to distinguish the origin of issues in the new merged repository
and we copy the original assignee and milestone (if any) to the new
issue (it's important that the assignee should already be added as a
collaborator on the new merged repository, otherwise we won't be able
to assign issues to them).  We can then add comments to the new issue,
running over the list of comments we retrieved from the original
repository in chronological order:

~~~~ {.haskell}
r <- createComment ghAuth ghUser ghRepo iss (issueCommentBody comm)
~~~~

Finally, for closed issues, after we've created the new issue and
added any relevant comments, we close it by editing the issue state:

~~~~ {.haskell}
let edit = editOfIssue { editIssueState = Just "closed" }
r <- editIssue ghAuth ghUser ghRepo iss edit
~~~~

The end result is that all of the issues from the pre-existing
repositories are migrated to the new merged repository.

There are a couple of warts to the process.  First, all of the issues
and comments on the new repository appear under the GitHub user name
you use for connecting and running the scripts.  That's pretty
unavoidable though, since there's no way (and neither should there be
any way) of spoofing another user's GitHub ID to generate issues or
comments that appear to come from someone else.  Second, the issue
numbers change compared to those in the original repositories.  Again,
that's pretty unavoidable since you're merging multiple numbered lists
into one.


## Conclusions

The couple of tools I've written to help with this process don't
automate everything (you still need to create yourself a new GitHub
repo, add it as a remote for your new merged repo and push to GitHub
by hand; you need to set up any collaborators manually so that you can
assign issues to them; you need to merge wiki contents by hand; etc.),
but this is something that you need to do infrequently enough that
that doesn't really matter.


[bh]: http://www.bayeshive.com/
[repo]: https://github.com/ian-ross/git-utils
[merge]: http://refactr.com/blog/2012/03/git-merging-two-repositories/
[hs-github]: http://hackage.haskell.org/package/github
