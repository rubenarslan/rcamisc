#' render an rmarkdown file in background using RStudio Jobs
#'
#' if you want to
#'
#' @param input .rmd document to be knitted
#' @param params params to pass to the .rmd
#' @param output_file name of the output_file (and the job)
#'
#'
#' @export
#' @examples
#' \dontrun{
#'    render_job("document.Rmd", list(dataset = "df1"), "summary_df1.html")
#' }
#'
render_job <- function(input, params = list(), output_file) {
  tmp <- tempfile()
  .rcamisc_global_input_file_for_rstudio_job <<- input
  .rcamisc_global_params_for_rstudio_job <<- params
  .rcamisc_global_output_file_for_rstudio_job <<- output_file
  rmarkdown_render <- rmarkdown::render
  cat('rmarkdown::render(input = .rcamisc_global_input_file_for_rstudio_job,
                      params = .rcamisc_global_params_for_rstudio_job,
                      output_file = .rcamisc_global_output_file_for_rstudio_job
      )',
      file = tmp,
      sep = "\n")
  rstudioapi::jobRunScript(tmp,
                           workingDir = getwd(),
                           output_file, importEnv = TRUE)
}

