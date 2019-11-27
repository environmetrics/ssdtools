context("lnormlnorm")

test_that("lnormlnorm", {
  data <- ssdtools::boron_data
  dist <- ssdtools:::ssd_fit_dist(data, dist = "lnormlnorm")
  
  expect_true(is.fitdist(dist))
  expect_false(is.fitdistcens(dist))
  expect_equal(coef(dist), c(meanlog1 = 0.949706713910721, sdlog1 = 0.55446333681171, meanlog2 = 3.20119275002971, 
sdlog2 = 0.768836125716127, r = 0.283997185736815))

  dist <- ssd_fit_dist(fluazinam, left = "left", right = "right")
  expect_false(is.fitdist(dist))
  expect_true(is.fitdistcens(dist))
  expect_equal(coef(dist), c(meanlog = 4.976920, sdlog = 2.687785), tolerance = 0.0000001)

  fluazinam2 <- fluazinam[rev(order(fluazinam$left)), ]
  fluazinam2$Weight <- 1:nrow(fluazinam2)

  dist <- ssd_fit_dist(fluazinam2, weight = "Weight", left = "left", right = "right")
  expect_false(is.fitdist(dist))
  expect_true(is.fitdistcens(dist))
  expect_equal(coef(dist), c(meanlog = 3.566750, sdlog = 2.182757), tolerance = 0.000001)
})

