\name{xml_shows_from_myepisodes_feed}
\alias{xml_shows_from_myepisodes_feed}
\title{Get shows from given MyEpisode feeds (retaining individual XML structure)}
\usage{
  xml_shows_from_myepisodes_feed(myepisodes_feed_url)
}
\arguments{
  \item{myepisodes_feed_url}{path to a MyEpisodes RSS feed}
}
\value{
  XMLNodeList with all "item"s from feed, corresponding to
  episodes
}
\description{
  Given a MyEpisodes feed, outputs shows separately as an
  XMLNodeList for use in \link{show_info_from_xml} or other
  XML referencing functions.
}
\examples{
\dontrun{xml_shows_from_myepisodes_feed(myepisodes_feed_url("user", "password"))}
  mock_feed_url <- file.path(system.file(package = "myepisodes"), "test_data/mock_mylist.xml")
  xml_shows_from_myepisodes_feed(mock_feed_url)
}
\author{
  Matt Malin <\email{email@mattmalin.co.uk}>
}
\seealso{
  \link{show_info_from_xml}, \link{myepisodes_feed_url},
  \link{shows_from_myepisodes_feed}
}

