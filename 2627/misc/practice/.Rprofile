# The course is a renv project two levels up, and that is where knitr and
# rmarkdown live.  R only reads the .Rprofile of the folder it starts in, so
# point renv at the parent project when this folder is rendered on its own.
#
# Delete this file (and _quarto.yml) if you would rather keep practice.qmd
# inside the main project folder and render it from there.
renv_project <- normalizePath(file.path("..", ".."), mustWork = FALSE)

if (file.exists(file.path(renv_project, "renv", "activate.R"))) {
  Sys.setenv(RENV_PROJECT = renv_project)
  source(file.path(renv_project, "renv", "activate.R"))
}
