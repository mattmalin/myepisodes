#' myepisodes functions
#'
#' Various functions to play with MyEpisodes data
#' @docType package
#' @name myepisodes
#' @aliases myepisodes package-myepisodes
NULL

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
