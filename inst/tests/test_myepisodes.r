################################################################################
# myepisodes package tests
.test_dir <- system.file(package = "myepisodes")

library(testthat)
library(XML)
################################################################################

test_that("feed urls are returned correctly", {
  expect_that(
    myepisodes_feed_url("foouser", "barpw", feed = "mylist", onlyunacquired = TRUE, showignored = FALSE),
	is_identical_to("http://www.myepisodes.com/rss.php?feed=mylist&onlyunacquired=1&showignored=0&sort=desc&uid=foouser&pwdmd5=barpw")
  )
  expect_that(
    myepisodes_feed_url("foouser", "barpw", feed = "given_feed", onlyunacquired = FALSE, showignored = FALSE),
	is_identical_to("http://www.myepisodes.com/rss.php?feed=given_feed&onlyunacquired=0&showignored=0&sort=desc&uid=foouser&pwdmd5=barpw")
  )
  expect_that(
    myepisodes_feed_url("foouser", "barpw", feed = "given_feed", onlyunacquired = TRUE, showignored = TRUE),
	is_identical_to("http://www.myepisodes.com/rss.php?feed=given_feed&onlyunacquired=1&showignored=1&sort=desc&uid=foouser&pwdmd5=barpw")
  )
})

test_that("given appropriate feed XML, shows are separated to individual elements", {
  mylist_xml <- file.path(.test_dir, "test_data", "mock_mylist.xml")
  expect_that(file.exists(mylist_xml), is_identical_to(TRUE))
  
  expected_tvshows <- list(
    item = list(
	  show_name = "Mock Show (1901)",
	  season = as.integer(1),
	  ep = as.integer(1),
	  ep_title = "Awesome Title",
	  date_aired = "17-Feb-1901"	  
	),
	item = list(
	  show_name = "Another Mock Show (2012)",
	  season = as.integer(4),
	  ep = as.integer(5),
	  ep_title = "Another Awesome Title",
	  date_aired = "17-Feb-2012"	  
	)
  )
    
  expect_that(shows_from_myepisodes_feed(mylist_xml), is_identical_to(expected_tvshows))
})
