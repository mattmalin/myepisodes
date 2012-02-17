################################################################################
# myepisodes package tests
.test_dir <- system.file(package = "myepisodes")

library(testthat)
library(XML)
################################################################################

test_that("feed urls are returned correctly", {
  expect_that(
    feed_url("foouser", "barpw", feed = "mylist", onlyunacquired = TRUE, showignored = FALSE),
	is_identical_to("http://www.myepisodes.com/rss.php?feed=mylist&onlyunacquired=1&showignored=0&sort=desc&uid=foouser&pwdmd5=barpw")
  )
  expect_that(
    feed_url("foouser", "barpw", feed = "given_feed", onlyunacquired = FALSE, showignored = FALSE),
	is_identical_to("http://www.myepisodes.com/rss.php?feed=given_feed&onlyunacquired=0&showignored=0&sort=desc&uid=foouser&pwdmd5=barpw")
  )
  expect_that(
    feed_url("foouser", "barpw", feed = "given_feed", onlyunacquired = TRUE, showignored = TRUE),
	is_identical_to("http://www.myepisodes.com/rss.php?feed=given_feed&onlyunacquired=1&showignored=1&sort=desc&uid=foouser&pwdmd5=barpw")
  )
})

test_that("given appropriate feed XML, shows are separated to individual elements", {
  mylist_xml <- file.path(.test_dir, "test_data", "mock_mylist.xml")
  expect_that(file.exists(mylist_xml), is_identical_to(TRUE))
  
  # tests currently are only for XML functions themselves, mocking up 
  # implementation in the test so far
  mylist <- xmlTreeParse(mylist_xml, getDTD = FALSE)
  r <- xmlRoot(mylist)

  episodes <- r[["channel"]][sapply(xmlChildren(r[[1]]), xmlName) == "item"]
  
  expect_that(xmlValue(episodes[[1]][["title"]]), is_identical_to("[ Mock Show (1901) ][ 01x01 ][ Awesome Title ][ 17-Feb-1901 ]"))
  expect_that(xmlValue(episodes[[2]][["title"]]), is_identical_to("[ Another Mock Show (2012) ][ 01x01 ][ Another Awesome Title ][ 17-Feb-2012 ]"))
})
