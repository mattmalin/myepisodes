\name{shows_from_myepisodes}
\alias{shows_from_myepisodes}
\title{Get info from shows given desired MyEpisodes feed information}
\usage{
  shows_from_myepisodes(uid, pwdmd5, feed = "mylist",
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
\value{
  list of lists of episode information
}
\description{
  Given MyEpisodes username and md5 hashed password, along
  with desired list and other settings, gets show and
  episode information as a list of show information lists.
}
\examples{
\dontrun{shows_from_myepisodes("foouser", "321654321654321", feed = "mylist", onlyunacquired = TRUE, showignored = FALSE)}
}
\author{
  Matt Malin <\email{email@mattmalin.co.uk}>
}
\seealso{
  \link{shows_from_myepisodes_feed},
  \link{myepisodes_feed_url}
}

