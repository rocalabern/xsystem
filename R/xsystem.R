#' @title x.system 
#' @export
x.system <- function (command) {
  message(paste0("[Command]>", command, "\n"))
  textOutput <- system(command, intern = TRUE)
  message(paste(textOutput, collapse = "\n"))
  message("\n")
  invisible(textOutput)
}

#' @title x.readFolder 
#' @export
x.readFolder <- function (folder, ...) {
  list.files(folder, ...)
}

#' @title x.readFile 
#' @export
x.readFile <- function (file, nlines = 10, ...) {
  readLines(file, nlines, ...)
}

#' @title x.getTempFile 
#' @export
x.getTempFile <- function (prefix= "file_", ext=".txt", dir=tempdir()) {
  tempFile = tempfile(pattern=prefix, fileext=ext, tmpdir=dir)
  return(tempFile)
}

#' @title x.writeTempFile 
#' @export
x.writeTempFile <- function (text, prefix= "file_", ext=".txt", read = TRUE, nlines = 10) {
  tempFile = x.getTempFile(prefix, ext=ext)
  x.writeFile(tempFile, text, read=FALSE)
  if (read) readLines(tempFile, nlines)
  invisible(tempFile)
}

#' @title x.writeFile 
#' @export
x.writeFile <- function (file, text, read = TRUE, nlines = 10) {
  fileConn<-file(file, "w")
  writeLines(text, fileConn)
  close(fileConn)
  if (read) return(readLines(file, nlines))
  else invisible(NULL)
}

#' @title x.appendFile 
#' @export
x.appendFile <- function (file, text, read = TRUE, nlines = 10) {
  fileConn<-file(file, "a")
  writeLines(text, fileConn)
  close(fileConn)  
  if (read) return(readLines(file, nlines))
  else invisible(NULL)
}

#' @title x.systemBAT 
#' @export
x.systemBAT <- function (command) {
  tempFile = x.getTempFile("commands_", ext=".bat")
  x.writeFile(tempFile, command, read=FALSE)
  invisible(x.system(tempFile))
}