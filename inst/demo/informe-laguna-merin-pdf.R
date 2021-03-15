extension <- 'pdf'
demoname <- paste0('informe-laguna-merin-', extension, '.Rmd')
demofile <- system.file("examples", demoname, package = "manoSIAR")

if (grepl('rstudio', .Platform$GUI, ignore.case = TRUE)) {
  contenidos <- readLines(system.file('examples', paste0(rootname, '.Rmd'),
                                      package = 'manoSIAR'))
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
  message('Se cre\u00f3 el archivo ', demoname, ' en la carpeta de trabajo')
  file.edit(demoname)
}
