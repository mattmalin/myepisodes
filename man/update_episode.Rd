\name{update_episode}
\alias{update_episode}
\title{Marks on MyEpisodes if an episode has been watched/acquired (through browser)}
\usage{
  update_episode(tv_episode, seen = TRUE)
}
\arguments{
  \item{tv_episode}{a show_info list from
  \link{show_info_from_xml} or similar}

  \item{seen}{TRUE/FALSE whether to also mark episode as
  seen, just marks acquired if FALSE}
}
\value{
  character of the URL for the myepisodes update link,
  should also load link in a browser to get the desired
  behaviour, assuming user is logged in to myepisodes.
}
\description{
  Given a tv episode (list as from
  \link{show_info_from_xml}) this marks on MyEpisodes if a
  show has been watched or acquired. Given the lack of a
  proper MyEpisodes API and until a better solution is
  implemented here, it currently loads a browser at the
  appropriate update link.
}
\author{
  Matt Malin <\email{email@mattmalin.co.uk}>
}

