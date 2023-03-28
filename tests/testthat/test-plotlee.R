test_that("`simplify_trace_ids()` works as expected", {
  
  p <-
    plotly::plot_ly() |>
    plotly::add_markers(data = mtcars[1:10, ],
                        x = ~wt,
                        y = ~mpg,
                        color = I("black")) |>
    plotly::add_paths(data = mtcars[11:20, ],
                      x = ~wt,
                      y = ~mpg,
                      color = I("black")) |>
    simplify_trace_ids()
  
  expect_identical(names(p$x$visdat),
                   paste0("trace_", 1:3))
  
  expect_identical(p$x$cur_data,
                   "trace_3")
})
