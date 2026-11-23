# Tango T (2010), Statistical Methods for Disease Clustering, Table 7.7:
# Knox test for the 22 patients with Kaposi's sarcoma in Uganda.

knox_tango <- function(ds, dt) {
    knox_test(x = kaposi_uganda, crs = NULL, ds = ds, dt = dt, sym = 99,
              sp_link = FALSE, planar_coord = TRUE)$knox
}

test_that("the pairs close in space and in time are those of Tango", {
    expect_equal(sapply(1:5, function(d) knox_tango(d, 0)$pairs_space),
                 c(4, 5, 5, 11, 13))
    expect_equal(sapply(0:4, function(d) knox_tango(1, d)$pairs_time),
                 c(15, 31, 59, 73, 96))
    expect_equal(knox_tango(1, 0)$pairs, 231)
})

test_that("the Knox statistic, expected value and mid-p are those of Table 7.7", {
    # rows: critical space limit 1-5 km; columns: critical time limit 0-4 years
    obs <- rbind(c(1, 1, 1, 1, 1), c(2, 2, 2, 2, 2), c(2, 2, 2, 2, 2),
                 c(2, 2, 2, 4, 7), c(2, 2, 2, 4, 8))
    expected <- rbind(c(0.260, 0.537, 1.022, 1.264, 1.662),
                      c(0.325, 0.671, 1.277, 1.580, 2.078),
                      c(0.325, 0.671, 1.277, 1.580, 2.078),
                      c(0.714, 1.476, 2.809, 3.476, 4.571),
                      c(0.844, 1.745, 3.320, 4.108, 5.403))
    # Poisson mid-p of Table 7.7; NA where the book does not agree with its own
    # expected value (row 1 km, 2-4 years; 5 km, 0 years)
    midp <- rbind(c(0.129, 0.258, NA, NA, NA),
                  c(0.024, 0.088, 0.251, 0.340, 0.480),
                  c(0.024, 0.088, 0.251, 0.340, 0.480),
                  c(0.098, 0.310, 0.651, 0.364, 0.135),
                  c(NA, 0.388, 0.744, 0.490, 0.138))
    for (s in 1:5) for (t in 0:4) {
        k <- knox_tango(s, t)
        expect_equal(k$knox, obs[s, t + 1])
        expect_lt(abs(k$expected - expected[s, t + 1]), 0.001)
        if (!is.na(midp[s, t + 1]))
            expect_lt(abs(k$p_poisson - midp[s, t + 1]), 0.002)
    }
})

test_that("the Monte Carlo p-value is never 0", {
    k <- knox_tango(2, 0)
    expect_gte(k$p_value, 1 / 100)
    expect_lte(k$p_value, 1)
})

test_that("knox_test() returns the space-time links", {
    k <- knox_test(x = kaposi_uganda, crs = NULL, ds = 2, dt = 0, sym = 9,
                   sp_link = FALSE, planar_coord = TRUE)
    expect_equal(nrow(k$st_link), k$knox$knox)
    expect_null(k$space_time_link)
})
