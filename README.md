# myepisodes

These are for functions accessing the MyEpisodes RSS feeds to extract myepisodes info in R.

Currently useful for extracting show info for a given feed, for example:

```R
my_username <- "my_username"
my_password <- "1234567890abcdef123456789abcdef1"

shows <- shows_from_myepisodes(my_username, my_password, "mylist")
summary_of_shows(shows)
```

Can also update shows to mark as watched or acquired, calls to appropriate page
in browser, so must be logged in:

```R
example_show <- shows[[1]]
mark_episode_as_acquired(example_show)
mark_episode_as_watched(example_show)
```
