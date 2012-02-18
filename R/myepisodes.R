#' myepisodes functions
#'
#' Various functions to play with MyEpisodes data
#' @docType package
#' @name myepisodes
#' @import XML
#' @author Matt Malin <\email{email@@mattmalin.co.uk}>
#' @aliases myepisodes package-myepisodes
NULL

#' get the MyEpisodes RSS feed url for a desired feed
#'
#' Returns the custom MyEpisodes RSS feed url for a desired feed.
#' @param uid myepisodes username
#' @param pwdmd5 myepisodes MD5 hashed password
#' @param feed the desired feed 
#' currently there are: mylist, today, yesterday, tomorrow, unacquired, 
#' unwatched, all.
#' @param onlyunacquired TRUE/FALSE - You can use this to decide if you only 
#' want the feed to list the episodes you haven't acquired.
#' @param showignored TRUE/FALSE - You can use this to decide if you want the 
#' feed to show your ignored shows too.
#' @author Matt Malin <\email{email@@mattmalin.co.uk}>
#' @examples myepisodes_feed_url("foouser", "321654321654321", feed = "mylist", onlyunacquired = TRUE, showignored = FALSE)
#' @export
myepisodes_feed_url <- function(uid, pwdmd5, feed = "mylist", onlyunacquired = TRUE, showignored = FALSE) {
  # there are currently six feeds available
  # mylist, today, yesterday, tomorrow, unacquired, unwatched, all
  myepisodes_feed_url <- paste(
    "http://www.myepisodes.com/rss.php?feed=",
	feed,
	"&onlyunacquired=",
	ifelse(isTRUE(onlyunacquired), 1, 0),
	"&showignored=",
	ifelse(isTRUE(showignored), 1, 0),
	"&sort=desc&uid=",
	uid,
	"&pwdmd5=",
	pwdmd5,
	sep = "")
  return(myepisodes_feed_url)
}

#' Create list of information for individual shows from MyEpisodes feed
#'
#' Given the path to a MyEpisodes feed, extracts the show information for
#' each show, returned as a list.
#'
#' @param myepisodes_feed_url path to a MyEpisodes RSS feed
#' @return list, containing show info
#' @seealso \link{show_info_from_xml},  \link{myepisodes_feed_url}
#' @examples shows_from_myepisodes_feed(myepisodes_feed_url("user", "password"))
#'   mock_feed_url <- file.path(system.file(package = "myepisodes"), "test_data/mock_mylist.xml")
#'   shows_from_myepisodes_feed(mock_feed_url)
#' @author Matt Malin <\email{email@@mattmalin.co.uk}>
#' @export
shows_from_myepisodes_feed <- function(myepisodes_feed_url) {
  episodes <- xml_shows_from_myepisodes_feed(myepisodes_feed_url)
  lapply(episodes, show_info_from_xml)
}

#' Get shows from given MyEpisode feeds (retaining individual XML structure)
#' 
#' Given a MyEpisodes feed, outputs shows separately as an XMLNodeList
#' for use in \link{show_info_from_xml} or other XML referencing functions.
#' @param myepisodes_feed_url path to a MyEpisodes RSS feed
#' @seealso \link{show_info_from_xml},  \link{myepisodes_feed_url}, 
#' \link{shows_from_myepisodes_feed}
#' @return XMLNodeList with all \"item\"s from feed, corresponding to episodes
#' @author Matt Malin <\email{email@@mattmalin.co.uk}>
#' @export
#' @examples xml_shows_from_myepisodes_feed(myepisodes_feed_url("user", "password"))
#'   mock_feed_url <- file.path(system.file(package = "myepisodes"), "test_data/mock_mylist.xml")
#'   xml_shows_from_myepisodes_feed(mock_feed_url)
xml_shows_from_myepisodes_feed <- function(myepisodes_feed_url) {
  myepisodes_feed <- xmlTreeParse(myepisodes_feed_url, getDTD = FALSE)
  r <- xmlRoot(myepisodes_feed)

  episodes <- r[["channel"]][sapply(xmlChildren(r[[1]]), xmlName) == "item"]
  
  return(episodes)
}

#' Get tv episode information from XML
#'
#' Given an XML show from \link{xml_shows_from_myepisodes_feed} in the format
#' of individual items in MyEpisodes feeds, returns a list for a given show
#' containing show_name, season, ep, ep_title, and date_aired.
#'
#' @param xml_show XMLNode of \"item\" child from MyEpisodes feed
#' @return list for individual show
#' @author Matt Malin <\email{email@@mattmalin.co.uk}>
#' @export
#' @seealso \link{xml_shows_from_myepisodes_feed},  \link{myepisodes_feed_url}, 
#' \link{shows_from_myepisodes_feed}
#' @examples 
#'   mock_feed_url <- file.path(system.file(package = "myepisodes"), "test_data/mock_mylist.xml")
#'   xml_shows <- xml_shows_from_myepisodes_feed(mock_feed_url)
#'   show_info_from_xml(xml_shows[[1]])
show_info_from_xml <- function(xml_show) {
  if(!any(class(xml_show) == "XMLNode")) {
    stop("xml_show must be of class XMLNode, are you perhaps passing in more than one episode?")
  }
  xml_title <- xmlValue(xml_show[["title"]])

  myepisodes_grep_pattern <- "\\[ (.+) \\]\\[ 0*(\\d+)x([0-9]+{2}[0-9]?) \\]\\[ (.+) \\]\\[ (.+) \\]"
  
  show_name <- gsub(myepisodes_grep_pattern, "\\1", xml_title)
  season <- as.integer(gsub(myepisodes_grep_pattern, "\\2", xml_title))
  ep <- as.integer(gsub(myepisodes_grep_pattern, "\\3", xml_title))
  ep_title   <- gsub(myepisodes_grep_pattern, "\\4", xml_title)
  date_aired <- gsub(myepisodes_grep_pattern, "\\5", xml_title)
  
  return(list(
    show_name = show_name,
	season = season,
	ep = ep,
	ep_title = ep_title,
	date_aired = date_aired
	)
  )
}

#' Shows episode season/number in format SxNN
#'
#' Returns episode from a show info list, of the form e.g. 1x12, 8x04,
#' left padding the episode number to max_length (default 2) if necessary.
#'
#' @param show_item a show item list, typically from \link{show_info_from_xml}
#' @param max_length=2 the length to pad episode numbers to, usually 2 is standard.
#' @return character of form NxNN
#' @author Matt Malin <\email{email@@mattmalin.co.uk}>
#' @export
#' @seealso \link{show_info_from_xml}
ep_number <- function(show_item, max_length = 2) {
  if(nchar(format(show_item$ep)) > max_length) {
    ep <- show_item$ep
  } else {
    ep <- paste(rep("0", length = 2 - nchar(format(show_item$ep))), show_item$ep, sep = "")
  }
  ep_number <- paste(show_item$season, ep, sep = "x")
  return(ep_number)
}

#' Display summary of all the tv episodes in a list
#'
#' Given a list of shows (in form from \link{show_info_from_xml}) will display
#' a summary, displaying each episode of the form \"show_name - SxNN\"
#'
#' @param shows list of shows as in form from \link{show_info_from_xml}
#' @return character vector summarising each show
#' @author Matt Malin <\email{email@@mattmalin.co.uk}>
#' @export
#' @examples 
#'   mock_feed_url <- file.path(system.file(package = "myepisodes"), "test_data/mock_mylist.xml")
#'   mock_shows <- shows_from_myepisodes_feed(mock_feed_url)
#'   summary_of_shows(mock_shows)
summary_of_shows <- function(shows) {
  as.character(sapply(shows, FUN = function(item) paste(item$show_name, " - ", ep_number(item), sep = "")))
}
