\name{summary_of_shows}
\alias{summary_of_shows}
\title{Display summary of all the tv episodes in a list}
\usage{
  summary_of_shows(shows)
}
\arguments{
  \item{shows}{list of shows as in form from
  \link{show_info_from_xml}}
}
\value{
  character vector summarising each show
}
\description{
  Given a list of shows (in form from
  \link{show_info_from_xml}) will display a summary,
  displaying each episode of the form \"show_name - SxNN\"
}
\examples{
mock_feed_url <- file.path(system.file(package = "myepisodes"), "test_data/mock_mylist.xml")
  mock_shows <- shows_from_myepisodes_feed(mock_feed_url)
  summary_of_shows(mock_shows)
}
\author{
  Matt Malin <\email{email@mattmalin.co.uk}>
}

