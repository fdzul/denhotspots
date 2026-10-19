test_that("the magrittr pipe is exported", {
    expect_equal(c(1, 4, 9) %>% sqrt(), c(1, 2, 3))
})
