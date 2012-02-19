\name{mark_episode_as_acquired}
\alias{mark_episode_as_acquired}
\title{Marks on MyEpisodes if an episode has been acquired (through browser)}
\usage{
  mark_episode_as_acquired(tv_episode)
}
\arguments{
  \item{tv_episode}{a show_info list from
  \link{show_info_from_xml} or similar}
}
\value{
  character of the URL for the myepisodes update link,
  should also load link in a browser to get the desired
  behaviour, assuming user is logged in to myepisodes.
}
\description{
  Given a tv episode (list as from
  \link{show_info_from_xml}) this marks on MyEpisodes if a
  show has been acquired. Given the lack of a proper
  MyEpisodes API and until a better solution is implemented
  here, it currently loads a browser at the appropriate
  update link.
}
\author{
  Matt Malin <\email{email@mattmalin.co.uk}>
}

