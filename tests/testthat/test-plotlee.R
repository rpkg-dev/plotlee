test_that("`write_img()` converts to all supported formats", {
  
  skip_if_not_installed("webp")
  
  plotly::plot_ly(data = mtcars,
                  type = "scatter",
                  mode = "markers",
                  x = ~mpg,
                  y = ~hp) |>
    list("mtcars_mpg_by_hp" = _) |>
    plotlee::write_img(formats = c(formats_raster, formats_postscript))
  
  all_output_paths <- paste0("mtcars_mpg_by_hp.", c("svg", formats_raster, formats_postscript))
  
  purrr::walk(all_output_paths,
              \(path) expect_true(fs::file_exists(path)))
  
  # teardown
  fs::file_delete(path = all_output_paths)
})

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
                   paste0("trace_", 0:2))
  
  expect_identical(p$x$cur_data,
                   "trace_2")
})
