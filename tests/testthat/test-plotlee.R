test_that("`write_img()` converts a single plot to all supported formats", {
  
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

test_that("`write_img()` can convert multiple plots", {
  
  list("mtcars_mpg_by_hp" = plotly::plot_ly(data = mtcars,
                                            type = "scatter",
                                            mode = "markers",
                                            x = ~mpg,
                                            y = ~hp),
       "mtcars_mpg_by_qsec" = plotly::plot_ly(data = mtcars,
                                              type = "scatter",
                                              mode = "markers",
                                              x = ~mpg,
                                              y = ~qsec)) |>
    plotlee::write_img()
  
  all_output_paths <-
    c("mtcars_mpg_by_hp",
      "mtcars_mpg_by_qsec") |>
    purrr::map(\(x) paste0(x, c(".svg", ".pdf"))) |>
    purrr::list_c(ptype = character())
                                 
  purrr::walk(all_output_paths,
              \(path) expect_true(fs::file_exists(path)))
  
  # teardown
  fs::file_delete(path = all_output_paths)
})

test_that("`write_img()` doesn't choke on zero-length input", {
  
  expect_no_error(plotly::plot_ly(data = mtcars,
                                  type = "scatter",
                                  mode = "markers",
                                  x = ~mpg,
                                  y = ~hp) |>
                    list("mtcars_mpg_by_hp" = _) |>
                    _[0L] |>
                    plotlee::write_img())
})

test_that("`write_img()` returns the paths to generated SVGs", {
  
  paths <-
    list("mtcars_mpg_by_hp" = plotly::plot_ly(data = mtcars,
                                              type = "scatter",
                                              mode = "markers",
                                              x = ~mpg,
                                              y = ~hp),
         "mtcars_mpg_by_qsec" = plotly::plot_ly(data = mtcars,
                                                type = "scatter",
                                                mode = "markers",
                                                x = ~mpg,
                                                y = ~qsec)) |>
    plotlee::write_img()
  
  all_output_paths <- fs::path(getwd(), c("mtcars_mpg_by_hp",
                                          "mtcars_mpg_by_qsec"),
                               ext = "svg")
  expect_identical(paths,
                   all_output_paths)
  
  # teardown
  c("mtcars_mpg_by_hp",
    "mtcars_mpg_by_qsec") |>
    purrr::map(\(x) paste0(x, c(".svg", ".pdf"))) |>
    purrr::list_c(ptype = character()) |>
    fs::file_delete()
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
