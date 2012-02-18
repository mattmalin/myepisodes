\name{ep_number}
\alias{ep_number}
\title{Shows episode season/number in format SxNN}
\usage{
  ep_number(show_item, max_length = 2)
}
\arguments{
  \item{show_item}{a show item list, typically from
  \link{show_info_from_xml}}

  \item{max_length=2}{the length to pad episode numbers to,
  usually 2 is standard.}
}
\value{
  character of form NxNN
}
\description{
  Returns episode from a show info list, of the form e.g.
  1x12, 8x04, left padding the episode number to max_length
  (default 2) if necessary.
}
\author{
  Matt Malin <\email{email@mattmalin.co.uk}>
}
\seealso{
  \link{show_info_from_xml}
}

