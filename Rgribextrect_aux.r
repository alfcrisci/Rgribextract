########################################################################################################

robust.system <- function (cmd) {
  stderrFile = tempfile(pattern="R_robust.system_stderr", fileext=as.character(Sys.getpid()))
  stdoutFile = tempfile(pattern="R_robust.system_stdout", fileext=as.character(Sys.getpid()))
  
  retval = list()
  retval$exitStatus = system(paste0(cmd, " 2> ", shQuote(stderrFile), " > ", shQuote(stdoutFile)))
  retval$stdout = readLines(stdoutFile)
  retval$stderr = readLines(stderrFile)
  
  unlink(c(stdoutFile, stderrFile))
  return(retval)
}

#########################################################################################################
# https://gist.github.com/benbalter/5858851 references

shp2geojson=function(x) {
  out=gsub("shp","geojson",x)
  system(paste0("ogr2ogr -f GeoJSON -t_srs crs:84  -overwrite ",out," ",x))
}
