\name{shows_from_myepisodes_feed}
\alias{shows_from_myepisodes_feed}
\title{Create list of information for individual shows from MyEpisodes feed}
\usage{
  shows_from_myepisodes_feed(myepisodes_feed_url)
}
\arguments{
  \item{myepisodes_feed_url}{path to a MyEpisodes RSS feed}
}
\value{
  list, containing show info
}
\description{
  Given the path to a MyEpisodes feed, extracts the show
  information for each show, returned as a list.
}
\examples{
\dontrun{shows_from_myepisodes_feed(myepisodes_feed_url("user", "password"))}
  mock_feed_url <- file.path(system.file(package = "myepisodes"), "test_data/mock_mylist.xml")
  shows_from_myepisodes_feed(mock_feed_url)
}
\author{
  Matt Malin <\email{email@mattmalin.co.uk}>
}
\seealso{
  \link{show_info_from_xml}, \link{myepisodes_feed_url}
}

