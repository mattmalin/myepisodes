#' myepisodes functions
#'
#' Various functions to play with MyEpisodes data
#' @docType package
#' @name myepisodes
#' @import XML
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
#' @author Matt Malin <\email{R@@mattmalin.co.uk}>
#' @examples feed_url("foouser", "321654321654321", feed = "mylist", onlyunacquired = TRUE, showignored = FALSE)
#' @export
feed_url <- function(uid, pwdmd5, feed = "mylist", onlyunacquired = TRUE, showignored = FALSE) {
  # there are currently six feeds available
  # mylist, today, yesterday, tomorrow, unacquired, unwatched, all
  feed_url <- paste(
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
  return(feed_url)
}

shows_from_xml <- function(myepisodes_feed_url) {
  episodes <- xml_shows(myepisodes_feed_url)
  lapply(episodes, show_info_from_xml)
}

xml_shows <- function(myepisodes_feed_url) {
  myepisodes_feed <- xmlTreeParse(myepisodes_feed_url, getDTD = FALSE)
  r <- xmlRoot(myepisodes_feed)

  episodes <- r[["channel"]][sapply(xmlChildren(r[[1]]), xmlName) == "item"]
  
  return(episodes)
}

show_info_from_xml <- function(xml_show) {
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
