extension <- 'doc'
demoname <- paste0('informe-laguna-merin-', extension, '.Rmd')
demofile <- system.file("examples", demoname, package = "siabox")
rs <- grepl('rstudio', .Platform$GUI, ignore.case = TRUE) &
  RStudio.Version()$version >= '1.2.640'
if (rs) {
  contenidos <- readLines(demofile)
  rstudioapi::documentNew(paste(contenidos, collapse = '\n'), 'rmarkdown')
} else {
  i <- 0
  wdarch <- dir()
  while (any(wdarch == demoname)) {
    nstr <- stringr::str_pad(i, 2, pad = '0')
    demoname <- gsub('_*[0-9]{0,2}\\.Rmd', paste0('_', nstr, '.Rmd'), demoname)
    i <- i + 1
  }
  file.copy(demofile, demoname)
  message('ATENCI\u00d3N: Se cre\u00f3 el archivo "',
          demoname,
          '" en la carpeta de trabajo')
  file.edit(demoname)
}
