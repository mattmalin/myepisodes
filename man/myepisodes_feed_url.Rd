\name{myepisodes_feed_url}
\alias{myepisodes_feed_url}
\title{get the MyEpisodes RSS feed url for a desired feed}
\usage{
  myepisodes_feed_url(uid, pwdmd5, feed = "mylist",
    onlyunacquired = TRUE, showignored = FALSE)
}
\arguments{
  \item{uid}{myepisodes username}

  \item{pwdmd5}{myepisodes MD5 hashed password}

  \item{feed}{the desired feed currently there are: mylist,
  today, yesterday, tomorrow, unacquired, unwatched, all.}

  \item{onlyunacquired}{TRUE/FALSE - You can use this to
  decide if you only want the feed to list the episodes you
  haven't acquired.}

  \item{showignored}{TRUE/FALSE - You can use this to
  decide if you want the feed to show your ignored shows
  too.}
}
\description{
  Returns the custom MyEpisodes RSS feed url for a desired
  feed.
}
\examples{
myepisodes_feed_url("foouser", "321654321654321", feed = "mylist", onlyunacquired = TRUE, showignored = FALSE)
}
\author{
  Matt Malin <\email{email@mattmalin.co.uk}>
}

