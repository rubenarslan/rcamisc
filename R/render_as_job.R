#' render an rmarkdown file in background using RStudio Jobs
#'
#' if you want to
#'
#' @param input .Rmd document to be knitted
#' @param params params to pass to the .Rmd
#' @param output_file name of the output_file (and the job)
#'
#' @importFrom utils capture.output
#'
#' @export
#' @examples
#' \dontrun{
#'    render_job("document.Rmd", list(dataset = "df1"), "summary_df1.html")
#' }
#'
render_job <- function(input= rstudioapi::getSourceEditorContext()$path,
                       params = NULL,
                       output_file = NULL) {
  tmp <- tempfile(fileext = ".R")
  name <- output_file
  if(is.null(name)) {
    name <- input
  }

  params_string <- capture.output(dput(params))
  output_file_string <- capture.output(dput(output_file))
  so_that_its_used <- rmarkdown::render
  cat(paste0('rmarkdown::render(input = "',input,'",
                      params = ',params_string,',
                      output_file = ',output_file_string,'
      )'),
      file = tmp,
      sep = "\n")
  rstudioapi::jobRunScript(tmp,
                           workingDir = getwd(),
                           name, importEnv = FALSE)
}

