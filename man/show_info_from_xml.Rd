\name{show_info_from_xml}
\alias{show_info_from_xml}
\title{Get tv episode information from XML}
\usage{
  show_info_from_xml(xml_show)
}
\arguments{
  \item{xml_show}{XMLNode of "item" child from MyEpisodes
  feed}
}
\value{
  list for individual show
}
\description{
  Given an XML show from
  \link{xml_shows_from_myepisodes_feed} in the format of
  individual items in MyEpisodes feeds, returns a list for
  a given show containing show_name, season, ep, ep_title,
  and date_aired.
}
\examples{
mock_feed_url <- file.path(system.file(package = "myepisodes"), "test_data/mock_mylist.xml")
  xml_shows <- xml_shows_from_myepisodes_feed(mock_feed_url)
  show_info_from_xml(xml_shows[[1]])
}
\author{
  Matt Malin <\email{email@mattmalin.co.uk}>
}
\seealso{
  \link{xml_shows_from_myepisodes_feed},
  \link{myepisodes_feed_url},
  \link{shows_from_myepisodes_feed}
}

