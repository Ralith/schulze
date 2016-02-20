## Vote Format

Votes inside a post look like this:

    ##vote
    1 A; B, C; D
    2 Plan Tran
    Bonus C

Votes must be in bold, must contain no blank lines, may not lie inside quotes, and must not be followed by bold text
without a blank line or non-bold text in between them. Options omitted from a question are treated as if ranked below
the options that are included. Questions that are omitted from a vote indicate that the voter has no preferences for
that question.

For question 1, the example voter:

- Prefers A over both B and C
- Is indifferent between B and C
- Prefers A, B, and C over D
- Prefers all of the above over anything else

For question 2, the voter prefers "Plan Tran" over anything else

For question "Bonus", the voter prefers C over anything else

If a post contains multiple votes, only the last is considered. If a voter makes multiple votes in different posts, only
the most recently given preferences for each question are considered.

Although the format supports voting for multiple options with the same level of preference, this should not be confused
with voting for a combination as a single option that might win monolithically. In other words, a vote for "A, B" is not
a vote to do both A and B, it is a vote to do either A or B. To vote for a combination, voters should create write-ins,
or use singular option names like "A+B", which have no special meaning.

Voters favoring an option that is similar to other options should consider taking advantage of the format to vote for
the similar options in addition to but at a lower preference rank than their favored option. This increases the chance
that something like their desired option will win, without weakening their vote for their desired option.

###### Question names

- "1", "2", and "Bonus" are question names
- Question names are arbitrary text that does not contain white space
- A "." or a ":" at the end of a question name is ignored
- Question names are case-insensitive

###### Option names

- "A", "B", "C", "D" and "Plan Tran" are option names.
- Option names are arbitrary text that does not contain line breaks, "," or ";"
- White space surrounding option names is ignored
- Option names are case-insensitive

Although option names may be arbitrarily long, the following conventions are recommended:

Avoid long option names. The longer they are, the more likely someone will make typos while entering them, requiring
manual cleanup if their vote is to be properly counted.

Write-ins of more than a few words should not be written as options directly. Instead, the author of a write-in should
describe their plan in prose of whatever length, and give it a short, unique option name such as "Plan Tran". The
write-in author and any other voters so inclined may then vote for the write-in with the short name.

## GUI usage

Launch the GUI by running the executable named "votescrape-gui". On Windows, this is located inside the "bin" folder.

#### Vote Count window

The main window is what opens when you start the program. It consists of a "Scrape" tab, used for scraping posts
directly from a thread, and a "File" tab, used for counting votes stored in a text file on your computer. Whichever tab
is selected, pressing the "Count" button will open a results window. Counts may be performed simultaneously.

##### Scrape tab

- Both inputs expect URIs that link to a specific post in a thread. For example:
  http://forums.somethingawful.com/showthread.php?threadid=3764408&pagenumber=1&perpage=40#post456324218 is an
  appropriate URI. You can find the URI for a given post by looking for the `#` link at the post's bottom-left and
  copying the URI it links to.
- The "To post" field is optional. If left blank, all posts following the "From post" will be scraped.
- The specific post identified by the "From post" link is not scraped. You should use the link to the post that opens voting.

##### File tab

You can use this tab to count votes in a plain textfile instead of from a thread.

Votes in a text file need different formatting:

- Instead of `##vote`, votes must start with the poster's name
- Votes must be separated by blank lines
- No non-vote text is permitted in the file

#### Vote Results window

Whenever you begin a count, a results window opens. A spinner will be displayed over the window until the count is
complete. You can close the window at any time to cancel the count.

If you're scraping from a thread and the count is taking a very long time, it's possible that SA has dropped one or more
HTTP requests. This can happen if you're scraping or viewing a lot of pages in a short period of time, or if you're just
unlucky. Cancel the count and try again. If the problem recurs, try letting it cool down for a few minutes. If this
happens to you frequently when you're not scraping or viewing large numbers of pages, please report a bug.

##### Winners tab

Each line contains a question name followed by a ranking of the options in a format similar to votes. For
example, "A; B, C; D" indicates that A won the vote, B and C are tied for second, and D comes third.

##### Counts tab

For each question, these charts convey how many people prefer an option to any other option. This is the distilled data
that is used to actually compute vote winners. Each cell indicates the number of voters that prefer the option whose row
the cell lies in over the option whose column the cell lies in.

Unlike first-past-the-post, preference-based voting does not allow for a simple scalar metric of how close two options
were. Use these charts if necessary to judge the exact relative degree of support for different options. Note that the
option with the largest number of preferences over other options is not necessarily the winner, because, for example, it
may be disfavored by a similarly large number of voters who are not otherwise in agreement.

##### Votes tab

The unprocessed votes scraped from a thread or file are displayed here. Use this tab to double-check that votes were
counted correctly. If you need to make corrections such as fixing voter typos or adding missing votes, you can copy the
scraped votes into a text file, make any changes, and then count the votes directly from the text file using the "File"
tab of the main window.

## Algorithm

Votes are weighed using [the Schulze method](http://m-schulze.9mail.de/schulze1.pdf). This method works by considering
how many voters prefer any given option over any other, and selecting the option that, directly or indirectly, is most
preferred and least disliked compared to the others. Rankings of non-winning options are produced by removing the
winning option from consideration and re-evaluating all votes.

By allowing voters to express more than one preference and counting their votes in this manner, we encourage voters to
select options more true to their preferences. For example, if a voter perceives an undesirable option A winning and a
merely tolerable option B in second place, first-past-the-post voting would force them to vote for the merely tolerable
option, instead of a third option C that they greatly prefer but perceive as unlikely to win. With a preference-based
voting method such as the Schulze method, the voter can express his true preference for C without sacrificing their
ability to support B in favor of A. This remains true no matter how many or how few other voters are making use of the
opportunity to express complex preferences.
