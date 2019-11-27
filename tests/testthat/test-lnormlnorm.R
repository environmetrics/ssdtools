context("lnormlnorm")

test_that("qnormlnorm", {
  expect_equal(qlnormlnorm(0.00001, meanlog1 = -9, sdlog1 = 3, meanlog2 = 5, sdlog2 = 3,r = 0.5),
               5.49268037508074e-10)
  expect_error(qlnormlnorm(0.00001, meanlog1 = -10, sdlog1 = 3, meanlog2 = 5, sdlog2 = 3,r = 0.5),
               "values at end points not of opposite sign")
})

test_that("rlnormlnorm replicable etc", {
  set.seed(102)
  expect_equal(rlnormlnorm(6, meanlog2 = 100),
                   c(2.18494359287356e+43, 0.818499800822172, 
                     4.13535470261134e+43, 
                     1.10659981431677, 0.982316505792159, 1.73290203349917e+43))
})

test_that("rlnormlnorm uses length", {
  set.seed(102)
  expect_equal(rlnormlnorm(2:7, meanlog2 = 100),
                   c(2.18494359287356e+43, 0.818499800822172, 
                     4.13535470261134e+43, 
                     1.10659981431677, 0.982316505792159, 1.73290203349917e+43))
})

test_that("fit lnormlnorm", {
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

