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
