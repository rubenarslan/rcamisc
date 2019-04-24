#' Am I going mad?
#'
#' It's easy to attach packages that overwrite functions from other packages. Especially dplyr has a lot of conflicts
#' with base packages, MASS and plyr. Because some of these conflicts do not always lead to error messages, sometimes
#' just incorrect behaviour, this function exists. Don't trust your faulty memory, just check whether dplyr's (or any other
#' package's) functions are 'on top' if you so desire.
#'
#' @param package the package you want to be on top (loaded last), defaults to dplyr
#' @param fix defaults to true. Detaches the desired package (without unloading) and loads it again. Won't work for base packages and can't overwrite functions that you defined yourself.
#' @param iteration for internal use only, if set to 0 the function will call itself to check that it worked, if set to 1, it won't.
#' @export
#' @examples
#' amigoingmad(fix = FALSE, package = 'rcamisc')
amigoingmad = function(package = "dplyr", fix = TRUE, iteration = 0 ) {
  if (iteration > 1) {
    stop("Can't fix.")
  }
  conf = unique(conflicts())
  want_package = paste0("package:", package)
  conflicts_desired_package = conf[conf %in% ls(want_package)]
  conflict_envs = sapply(conflicts_desired_package, FUN = function(x) {
    environmentName(pryr::where(x, globalenv())) # relative to globalenv, not pkg env
  })
  is_good = conflict_envs == want_package
  potentially_bad_confs = conflicts_desired_package[!is_good]
  potentially_bad_envs = conflict_envs[!is_good]
  have_to_fix = rep(FALSE, length(potentially_bad_confs))
  for (i in seq_along(potentially_bad_confs)) {
    if (!identical(body(get(potentially_bad_confs[i], pos = want_package)),
                   body(get(potentially_bad_confs[i])))) {
      have_to_fix[i] = TRUE
    }
  }

  if (any(have_to_fix)) {
    message("The following functions don't have the environment you want.")
    print(data.frame(`function.` = potentially_bad_confs[have_to_fix],
                     environment = potentially_bad_envs[have_to_fix]),
          row.names = F)
    if (fix) {
      base::detach(name = want_package, character.only = TRUE)
      base::library(package, character.only = TRUE)
      message("Tried to fix this, calling myself again to make sure...")
      amigoingmad(package, fix, iteration + 1)
      message("Sanity restored!")
    }
  } else if (iteration == 0) {
    message("Everything looks normal. Maybe it's you.")
  }
}

#' Open in Excel
#'
#' Simple helper, so I don't complain about the slugginess of RStudio's View so much
#'
#' @param x a dataframe to open in Excel
#' @export
#' @examples
#' \dontrun{
#' view_in_excel(Titanic)
#' }
view_in_excel <- function(x) {
  if (interactive()) {
    excel <- paste0(tempdir(), "/",
                    deparse(substitute(x)),
                    ".xlsx")
    rio::export(x, excel)
    system(paste('open -a "Microsoft Excel"', excel))
  }
}


#' build a bibliography bibtex file from your packrat lockfile
#'
#' Packrat helps you maintain consistent package versions for a project. To be able to give due credit in a way that academics understand, it's helpful to be able to generate citations.
#'
#'
#' @param overwrite_bib whether to overwrite an existing bibtex file of the same name
#' @param silent defaults to false. whether to cat out a nocite string to use in your header
#' @param cite_only_directly_called whether to call only the packages you called yourself (default) or also their dependencies
#' @param lockfile_path path to the packrat lock file to use
#' @param bibliography_path path to the bibtex file to generate
#' @param cite_packrat whether to cite packrat even if it's not loaded explicitly, defaults to the reverse of cite_only_directly_called
#' @export
#'
packrat_bibliography = function(overwrite_bib = FALSE,
                                silent = FALSE,
                                cite_only_directly_called = TRUE,
                                lockfile_path = "packrat/packrat.lock",
                                bibliography_path = "packrat_bibliography.bibtex",
                                cite_packrat = !cite_only_directly_called) {

  if (file.exists(bibliography_path) && !overwrite_bib) {
    stop("Bibliography file existed and won't be overwritten, specify overwrite_bib = TRUE.")
  }
  # use internal function to read lockfile (uses readDcf)
  if (cite_only_directly_called) {
    package_names = packrat:::dirDependencies("./")
  } else {
    stopifnot(file.exists(lockfile_path))
    packages = packrat:::readLockFilePackages(lockfile_path)
    package_names = names(packages) # get pkg names
  }

  citation_objects = list()
  citation_objects$R = utils::citation()
  citation_objects$R$note = paste("version", as.character(getRversion()))
  citation_objects$R$key = "R"

  if (cite_packrat) {
    package_names = union("packrat", package_names)
  }

  for (i in seq_along(package_names)) {
    pkg_name = package_names[i]
    citation_obj = utils::citation(pkg_name)
    citation_obj$key = pkg_name	# by default the bibtex entries lack keys, we use the pkg name ,
    pkg_version = as.character(utils::packageVersion(pkg_name))
    if (is.null(citation_obj$note)) {
      citation_obj$note = ''
    }
    citation_obj$note = paste(citation_obj$note, "version", pkg_version)
    citation_objects[[ pkg_name ]] = citation_obj
  }

  bibliography = list()
  for (i in seq_along(citation_objects)) {
    name = names(citation_objects)[i]
    citation_obj = citation_objects[[i]]
    # don't lowercase R
    citation_obj[1]$title = stringr::str_replace_all(citation_obj[1]$title, "\\bR\\b", "{R}")
    # don't uppercase the package title
    citation_obj[1]$title = stringr::str_replace_all(citation_obj[1]$title, "^([a-zA-Z0-9.]+)+:", "{\\1}:")
    class(citation_obj) = c("citation", "bibentry")
    bibliography[[name]] = paste0(
      as.character(utils::toBibtex(citation_obj)),
      collapse = "\n")
  }

  bibliography = paste0(bibliography, collapse = "\n\n") # concatenate citations

  # write bibliography to file
  writeLines(iconv(bibliography, to = "UTF-8"), bibliography_path, useBytes = TRUE)

  # generate YAML reference with nocite
  if (!silent) {
    cat(paste0("
							 bibliography: ", bibliography_path, "
							 nocite: |
							 ", paste0("@", c("R", package_names), collapse = " ")))
  }
  invisible(bibliography)
}