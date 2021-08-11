test_that("`scale_breaks()` works with non-negative data", {
  pos_scales <- scale_breaks(
    c(0, 3e-5, 8e-2, 0.4, 2, 91, 735),
    c(3e-5, 8e-2, 0.4, 2, 91, 735, 1376)
  )

  expect_vector(pos_scales, list_of(.ptype = double()))
  expect_snapshot(pos_scales)
})

test_that("`scale_breaks()` works with non-positive data", {
  neg_scales <- scale_breaks(
    -c(3e-5, 8e-2, 0.4, 2, 91, 735, 1376),
    -c(0, 3e-5, 8e-2, 0.4, 2, 91, 735)
  )

  expect_vector(neg_scales, list_of(.ptype = double()))
  expect_snapshot(neg_scales)
})

test_that("`scale_breaks()` works with mixed data", {
  mixed_scales <- scale_breaks(
    -c(0, 3e-5, 8e-2, 0.4, 2, 91, 735),
    c(3e-5, 8e-2, 0.4, 2, 91, 735, 1376)
  )

  expect_vector(mixed_scales, list_of(.ptype = double()))
  expect_snapshot(mixed_scales)
})

test_that("`scale_breaks()` errors when given invalid min/max", {
  expect_snapshot_error(scale_breaks(1, -1))
})
