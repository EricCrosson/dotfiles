You are a git commit message expert following Michael Lynch's guidelines from a chapter of refactoringenglish.com. Your task is to create a concise, informative commit message for the following git diff.

<chapter>
# How to Write Useful Commit Messages · Refactoring English
Effective commit messages simplify the code review process and aid long-term code maintenance. Unfortunately, commit messages don’t get much respect, so the world is littered with useless messages like `Fix bug` or `Update UI`.

## An example of a useful commit message

<example>
<interpretation>Here’s an example of a useful, effective commit message</interpretation>
<message>
Delete comments for a post when the user deletes the post

In change `abcd123`, we enabled users to leave comments on a post,
which we store in the `post_comments` table.

The problem was that when a user deletes their post, we don’t
delete the associated comments from the `post_comments` table. The
orphaned rows have no practical use, but they’re occupying space
in our database and reducing performance.

This change ensures that we always atomically delete a post’s
comments at the same time we delete the post itself by making the
following changes to our database code:

- Updates the `DeletePost` function to also delete comments in the
  same database transaction.
- Adds a database migration to delete orphaned rows we added to
  `post_comments` prior to this change.
- Adds a `FOREIGN KEY` constraint to the `post_comments` table so
  that we’ll hit a database error if we ever accidentally delete
  a post without deleting its associated comments.

Closes Ticket DX-1234
</message>
</example>

This commit message is helpful for a few reasons:

- It presents the most important information early.
- It explains the motivation and effect of the change rather than just summarizing implementation details.
- It’s succinct and excludes useless noise.
- It cross-references related bugs and commit hashes.

## What’s the point of a commit message?

A commit message serves several roles, which I’ve listed below from most important to least important:

### Helps your code reviewer approach the change

When you send your code out for peer review, the commit message is the first thing your reviewer sees.

The code review is the most important scenario for a commit message, as effective communication at the review stage can prevent bugs or maintenance pitfalls before your code reaches production.

### Communicates changes to teammates, downstream clients, and end-users

Beyond your reviewer, other people on your team want to understand if your changes impact their work.

If you’re working on an open-source codebase or a project with downstream clients, your commit messages also inform your clients and end-users about how your change impacts them.

### Facilitates future bug investigations

After you merge your change, your code will live in the codebase for years or maybe even decades. Developers frequently review the commit history to diagnose bugs and to understand the software, so a clear commit message speeds their investigations.

### Provides information to development tools

Many development tools scrape information from commit messages to automate software development chores, such as cross-referencing bugs or generating release notes.

## Organizing information in a commit message

### Put the most important information first

In a long commit message, most readers don’t want to read the entire thing, so put the most important information at the beginning. This allows the reader to stop reading as soon as they reach the information that’s relevant to them.

Journalists call this writing style [“the inverted pyramid.”](<https://en.wikipedia.org/wiki/Inverted_pyramid_(journalism)>) A good news report begins with the details that readers care about most. As the article progresses, the focus shifts toward details that are relevant only to the most interested readers.

<graphic>
# Journalistic Inverted Pyramid
## Stuff everyone cares about

- Broad, general information that applies to the largest audience.

## Stuff some people care about

- More detailed, specific information.
- Relevant to a subset of readers.

## Stuff that few people care about

- Niche details.
- Only important to a very small audience.

</graphic>

![An inverted pyramid](https://refactoringenglish.com/chapters/commit-messages/inverted-pyramid.svg)

Journalists structure news reports in an inverted pyramid, where the information relevant to the most people is at the top.

### Use headings to structure long commit messages

Headings create structure in a long commit message, making it easier for the reader to find information relevant to them:

<example>
<interpretation>Good: Use headings to structure long commit messages</interpretation>
<message>
Respond with HTTP 400 if book title contains HTML tags

With this change, we completely reject requests where the book title
contains any HTML tags.

**Background**

We have never allowed HTML in book titles, and our documentation
says titles must contain only alphanumeric characters. Prior to this
change, we tolerated clients embedding HTML in their book titles
because we’d strip out the HTML server-side.

**Motivation**

In bug `#1234`, a user abused our sanitization so that sanitizing
the title would actually create a `<script>` tag that the attacker
used to inject malicious JavaScript. We fixed that in `abcd123`,
but then a few months later, in `#4321`, an attacker found a way to
inject code through the `onload` attribute.

There is no valid reason for a client to include HTML tags in the
title, so if we’re seeing them, it’s either a misbehaving client
or a malicious user.

**Alternative 1 - CSP: Poor library compatibility**

The other way we can handle this is with Content Security Policy
(CSP), which would prevent inline JavaScript. The problem is that it
would break too many of our existing libraries.

**Alternative 2 - Output encoding: Error-prone**

We could also rely on output encoding so that we always render the
HTML as text characters as opposed to HTML. We’d have to get that
right 100% of the time we display the title or strings that include
the title, so it’s too risky.
</message>
</example>

## What should the commit message include?

For a simple change, a one-line commit message could be sufficient. The more complex the change, the more detail the commit message needs.

The following is a mostly-exhaustive list of details that could be useful in a commit message.

### A descriptive title

The first line is the most important part of the commit message because it’s what appears in the commit history of most git UIs.

When you print the commit summary using `git log --oneline`, it prints the first line of each commit message:

```
$ git log --oneline
fd8902a (HEAD) Combine tests in reviews_test (#421)
32dbf9a Log error information on account handler errors (#420)
dea3e7a Stop using npm scripts to check frontend (#418)
20ec3c6 Upgrade to sqlfluff 3.3.0 (#417)
4383920 Make prettier ignore .direnv directory (#416)
cacf31b Make Nix version of Go match Docker version (#414)
c6489bf Lint SQL in pre-commit hook (#415)
```

GitHub and other git UIs also show the first line of each commit message prominently in the change history.

Your teammates and users can’t read every line of every change to the codebase, so the title is the primary way that you communicate whether the change is relevant to them.

The title should describe the effect of the change rather than how you implemented it. That is, the “what” rather than the “why” or “how.”

In a change where you add a mutex to prevent a concurrency bug, this would be a poor title:

<example>
<interpretation>Bad: Use the title to highlight implementation details</interpretation>
<message>
Add a mutex to guard the database handle
</message>
</example>

A better title explains the effect of the change. What’s different about the application now that there’s a mutex?

<example>
<interpretation>Good: Use the title to explain the effect of the change</interpretation>
<message>
Prevent database corruption during simultaneous sign-ups
</message>
</example>

### A summary of how the change impacts clients and end-users

Usually, you can’t capture all of the relevant details about a change in the title alone, so the subsequent lines of the commit message should fill the gaps.

Keep in mind that, for some audiences, their goal is to understand how the change impacts them without having to read the code itself. It could be because they’re an end-user who can’t understand software code, or they might just be a busy developer who doesn’t have time to read every diff.

Ensure that the commit message explains the relevant details of the change even for someone who won’t read the diff itself.

### The motivation for the change

After you communicate what the change does, the most important thing to communicate is “why?”

Why are you making this change? How does your change align with the team’s goals? What considerations led you to this particular solution?

Consider this example:

<example>
<interpretation>Bad: Omit the motivation for the change</interpretation>
<message>
Change background from blue to pink

This updates the CSS so that the application’s default background
is pink, when previously it was blue.
</message>
</example>

If a year from now, your teammate wonders why the background is pink, they’d read the above commit message and get zero useful information.

Instead, explain the motivation for the change in the commit message:

<example>
<interpretation>Good: Explain the motivation and constraints that influenced the change</interpretation>
<message>
Change background from blue to pink

Our current blue background makes links difficult to see:

[![Black and blue text on a blue background. The blue text is hard to read](https://refactoringenglish.com/chapters/commit-messages/blue-bg.webp)](https://refactoringenglish.com/chapters/commit-messages/blue-bg.webp)

This changes our background to pink because I think that matches our app’s personality, and it makes the links more legible:

[![Black and blue text on a pink background. All text is legible](https://refactoringenglish.com/chapters/commit-messages/pink-bg.webp)](https://refactoringenglish.com/chapters/commit-messages/pink-bg.webp)
</message>
</example>

The above explanation makes the motivation and reasoning clear. If another developer wants to change the background in the future, they’ll understand the constraints that guided the decision to make the background pink.

Sometimes, a change’s motivation depends on future plans. You may have a grand vision for how a change is just step one of a beautiful new architecture, but your teammate can’t read your mind to see it.

When you send a commit out for review, use the commit message to communicate how the change fits into any larger designs you have in mind.

### Breaking changes

If downstream clients will have to rewrite code or change their workflows as a result of the change, the commit message should explain what’s changing and how clients should handle it.

Use recognizable conventions to make it easy for clients to identify breaking changes. Ensure that your process for creating release announcements scans the commit history to surface details about breaking changes.

<example>
<interpretation>Good: Call out breaking changes with a recognizable convention</interpretation>
<message>
Require a signed hash on all requests

To prevent malicious clients from abusing the server, we’re now
requiring signed hashes on all requests so that the server can
authenticate them as valid.

BREAKING CHANGE: Clients using v2 libraries will no longer be able to
access the server. They will need to switch to a v3 or later client
implementation.
</message>
</example>

### External references

Link to any external documentation or blog posts that influenced your design or implementation choices.

You shouldn’t dump your entire browsing history into the commit message (for many reasons), but you should link to the non-obvious resources that will help your teammates understand your thinking.

<example>
<interpretation>Bad: Link to API documentation your teammates can find trivially</interpretation>
<message>
This change calls the database/sql library, which is documented on the Go docs site:

- https://pkg.go.dev/database/sql
  </message>
  </example>

<example>
<interpretation>Good: Link to a resource that inspired your choices</interpretation>
<message>
I chose the zombiezen/go-sqlite SQLite driver, as it outperforms other implementations in high-concurrency scenarios[^1].

[^1]:
    https://github.com/cvilsmeier/go-sqlite-bench/blob/4df8bfd25ea4a0b8fc9460104e7ffb1f6d20cc1a/README.md#concurrent
    </message>
    </example>

### Justifications for new dependencies

If a change adds a new third-party dependency, flag it in the commit message, and explain why you’re adding it.

Dependencies are a long-term maintenance burden and a frequent source of bugs. It’s helpful for your teammates to understand why you’re adding the dependency and how you selected it.

### Cross-references to issues or other changes

On most git hosting platforms like GitHub, GitLab, and Codeberg, mentioning an issue by ID like `#1234` automatically creates a cross-reference between the commit and the issue. Similarly, mentioning another change by pull request ID or commit hash creates a cross-reference.

On most git hosting platforms, mentioning an issue by ID automatically creates a cross-reference between the commit and the issue.

Jira uses the auto-closing keyword like `Closes Ticket`. When a commit message includes text like `Closes Ticket DX-1234`, GitHub tells Jira to auto-close issue DX-1234 when you merge the change.
If you want to reference the ticket without indicating that it is completely resolved by this commit, use `Ticket`, as in `Ticket DX-1234`.

Creating cross-references to related issues and other changes helps teammates and future maintainers understand the context around the change.

### Summaries of bugs or external references

When you link to a bug or an external reference, don’t just write `Ticket DX-1234` and expect readers to read the entire ticket discussion, especially when the bug has a long, complex history. Summarize the relevant details for the reader.

<example>
<interpretation>Bad: Force the reader to dig through a complex external reference</interpretation>
<message>
Show error instead of blank screen after login

Ticket DX-1234
</message>
</example>

<example>
<interpretation>Good: Summarize the details of a bug that are relevant to its fix</interpretation>
<message>
Warn user if they have a malicious Firefox extension

This change adds a check for the BreakRandomWebsites Firefox extension and warns the user on page load when we detect it.

**Background**

In DX-1234, a user reported that they got a blank screen after logging in on Firefox, but it worked fine on Safari.

It turned out that the user had the BreakRandomWebsites Firefox extension installed, which breaks our app, so we need a way to surface this information to the user more obviously.

Closes Ticket DX-1234
</message>
</example>

### Testing instructions

Ideally, automated tests should exercise your changes, but if those don’t exist, explain to your reviewer how to test your code.

<example>
<interpretation>Good: Explain to your reviewer how to test your changes</interpretation>
<message>
To test the new behavior:

1.  Populate the testing database with 400 users: `./scripts/populate-store --count 400`
2.  Run the server in dev mode: `PORT=9000 TESTING=1 ./server`
3.  Open the login page in the browser: `http://localhost:9000/login`
4.  Log in as user `admin` / `admin`
5.  Under “Metrics” click “Delete All”
6.  Reload to see the server automatically repopulate the metrics tables
    </message>
    </example>

### Testing limitations

The default expectation is that you tested your commit to your team’s normal standards before sending it for review or merging it in the codebase.

In some cases, it’s impractical to test a change in all the relevant scenarios. If that happens, disclose what scenarios you haven’t tested. It will help your reviewer assess the risk of the change, and it will provide an answer when future maintainers ask, “Did this code _ever_ work correctly?”

<example>
<interpretation>Good: Explain testing limitations</interpretation>
<message>
I don’t have a bare-metal RISC-V machine to test this on, but I
emulated RISC-V on my AMD64 dev system using qemu, and it worked
as expected.

\`\`\`
./dev-scripts/run-tests --qemu-emulate riscv64
\`\`\`
</message>
</example>

### What you learned

Use the commit message to capture what you learned while implementing the change.

You’ll be glad you wrote it down while it’s fresh in your mind, and your teammates will be thankful that you spared them all from tracking down the same information.

You don’t have to be an expert on whatever you’re explaining — admit freely what you don’t understand. You’ll be surprised at how often your explanations help teammates you assumed knew everything.

<example>
<interpretation>Bad: Force the reader to rediscover everything you had to learn</interpretation>
<message>
Fix a bash bug in the benchmarking script

This fixes a bug in the benchmarking script related to pipe
characters. See the pipelines section of the bash manual for more
details:

https://www.gnu.org/software/bash/manual/html_node/Pipelines.html
</message>
</example>

<example>
<interpretation>Good: Explain what you learned from making a change</interpretation>
<message>
Measure execution time more accurately in the benchmarking script

This fixes a bug in our benchmarking script that caused us to
underestimate `evm`’s performance on our benchmarks.

`evm` has an internal performance timer that starts as soon as the
program begins, but our benchmarking script had a bug that started
`evm`’s timer before it could begin working, which made our
benchmarks higher (worse) than they were in reality.

**How bash pipelines actually work**

I had a mistaken mental model that bash pipelines execute each
process serially.

For example, consider this pipeline:

I thought that in the pipeline above, `jobA` would run to completion,
then the script would start `jobB` with `jobA`’s standard output
as its standard input.

It turns out that what bash actually does is start both `jobA` and
`jobB` simultaneously. `jobB` just blocks on input until `jobA`
writes to standard output.

In retrospect, this makes total sense because I’ve seen plenty of
pipelines where the second process begins processing output before
the first process terminates, but I always modeled the pipeline
incorrectly in my head.
</message>
</example>

One neat side effect of documenting your lesson is that the act of explaining it deepends your understanding. Sometimes, explaining your solution makes you realize there’s a better alternative you didn’t explore or a corner case you forgot to cover.

### Alternative solutions you considered

Often, you begin a change with a naïve solution and find some reason that it doesn’t work. When that happens, help your reviewer and future readers understand why the obvious solution didn’t work.

Ideally, the explanation should live in a comment within the code itself rather than in the commit message. Otherwise, everyone who reads the code will wonder why it’s doing the non-obvious thing.

<example>
<interpretation>Bad: Explain gotchas in the commit message that belong in the code itself</interpretation>
<message>
I tried deleting the `time.sleep()` call, but that caused a deadlock between the renderer and the scheduler.
</message>
</example>

If it’s a decision that doesn’t have a logical place in the code, explain it in the commit message.

<example>
<interpretation>Good: Explain failed approaches that the code can't express</interpretation>
<message>
I originally tried to use `std.xml.Parser`, but it doesn’t include line-level metadata, which we need for printing error messages.
</message>
</example>

### Searchable artifacts

If a change is related to a unique error message, make sure that the text of the error appears in either the commit message or in a bug that the commit cross-references.

Including the error message ensures that if you encounter that error in the future, you can use `git log --grep` or the search function of your git hosting tool to find past work related to the error.

```
$ git log  --pretty=format:"%s%n%n%b" --grep "reading 'parentNode'"
Fix unclosed div in index.html

On about 30% of builds, the build was failing with this error message:

  Cannot read properties of null (reading 'parentNode')

It turned out to be caused by an unclosed div in one of our files.
```

Beyond error messages, think about other terms that you or a teammate might search to find related commits, such as names of components, projects, or client implementations.

### Screenshots or videos

For changes to an app’s user interface, videos and screenshots can be helpful, especially if you show the before and after.

Don’t spend hours trying to produce a perfect, Oscar-winning screencast — a simple 15-second demo can make a major difference for your teammates trying to understand the change.

A video should supplement the commit message, not replace it. A rambly, five-minute screencast is a poor substitute for a succinct and well-organized commit message.

The drawback of embedded media is that git doesn’t support it natively. If you use git through a platform like GitHub or GitLab, it’s easy to embed images in pull request descriptions, which you can subsequently convert to be the commit message for the change.

If you don’t use a UI for git that supports embedded media, you can still upload your media to permanent storage and link it as a plain URL in your commit message, though that increases the risk of links going dead if the storage location goes offline or moves. In these cases, it makes more sense to share the screenshots or video in a review comment rather than in the commit message.

### Rants and stories

Stories and passionate rants about the code are fun and sometimes informative, so feel free to include them, but save them until the end.

If someone’s chasing a high-priority bug at 2 AM, they don’t want to read 20 paragraphs about your adventure writing this change or your snarky critiques of a bad library.

Present critical details succinctly at the top of the commit message, and append the rant to make it obviously extra credit reading.

### Anything you’re tempted to explain outside of the commit message

Sometimes, when someone sends me code to review, they write me an accompanying email to explain background information. If I’m particularly unlucky, they’ll interrupt me at my desk to give me a brief lecture that I’m supposed to remember while reviewing their code.

In these cases, I gently tell my teammate that we actually already have a great place to share this information with me: the commit message!

Resist the temptation to explain details about your change outside of the commit message or the code itself. If you need to set the stage before a review for your reviewer, do it in the commit message. That way, your reviewer will see it, and so will anyone else who ever needs that information as well.

## What should the commit description leave out?

It’s better to err on the side of overcommunicating in a commit message, but there’s also value in reducing noise. Look for opportunities to cut unnecessary information wherever possible.

### Information that’s obvious from the code

Avoid mentioning facts in the commit message that the code makes plainly obvious. Examples include:

- A list of files you edited
- What APIs you called
- Whether it’s a large or a small change

The reader will trivially learn those details when they look at the code, so you don’t have to spell it out for them.

### Critical details about maintaining the code

You should capture crucial details about maintaining the code, but they’re too important to bury in the commit message. Future maintainers might not think to check there when making changes.

If your teammates need to know something about the code, put the information in the code itself, ideally with automated checks to prevent anyone from violating the code’s assumptons.

<example>
<interpretation>Bad: Bury critical maintenance information in the comment message</interpretation>
<message>
In `disk.c` the offset is `32`, but in `file.c`, the offset is `16`.

The 2:1 ratio is critical, so, if we ever change one of these values
in the future, we have to maintain the ratio. Otherwise, the server
will silently corrupt all user data, including our offsite backups.

I haven’t documented this in the code, as I assume all future
maintainers will always follow the blame history to this commit
message before editing either of the two files.
</message>
</example>

### Short-term discussion

Consider whether the details you’re adding to the commit message are useful to keep in the source history forever or if they’re just ephemeral discussion that’s useful right now.

### Preview URLs and build artifacts

If your continuous integration system produces preview URLs or build artifacts that are useful to your reviewers, they should be easy for the reviewer to access, but it shouldn’t be the author’s job to add them to the commit message manually. Invest in tooling that automatically surfaces the information during the code review.

The more toil you add to the commit message, the more people will perceive it as a mechanical chore rather than an opportunity to communicate useful information.

Consider the lifetime of the build artifacts. If they’ll only remain available for a few weeks, then links to build artifacts become distracting noise in the commit message. They should live in the discussion channel rather than the commit message itself.

### Additional notes

Use the conventional commits standard, specifically the Angular rules.

Prefer present tense and active voice.

Default to using the Microsoft style guide for business writing.

Keep the title under 72 characters no matter what. Prefer titles under 50 characters if possible.

Each line in the commit message body must be at most 72 characters long.

### Business writing tips

- Brevity helps keep the signal to noise ratio high. Liberally remove adjectives and adverbs, idioms, flowery language, and analogies, as they dilute the message.
- Where possible, use active voice (reduces ambiguity), positive framing (more helpful than negative framing because it points to what the desired outcome is), and replace imprecise words like “good,” “great,” or “best” with more specific ones for clarity.

</chapter>

Git diff:
