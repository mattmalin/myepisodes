# myepisodes package tests
library(testthat)

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
