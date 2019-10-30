#' Reads the testflx.dat MAESTRA/MAESPA output file
#'
#' @description Reads the test flux output file (testflx.dat).
#'
#' @param filename Name of the test output file (default to "testflx.dat".
#' @return Returns a dataframe.
#' @author Rémi Vezy
#' @keywords utilities
#' @examples
#' \dontrun{
#' # Simple as this:
#' test <- readtestflx()
#' }
#' @export
readtestflx= function (filename = "testflx.dat") {
  daylines <- readLines(filename)
  colloc <- grep("DAY", daylines)[2]
  testflux <- data.table::fread(filename, skip = colloc, data.table = F)
  names(testflux)= delempty(strsplit(delempty(strsplit(daylines[colloc],"DAY:")[[1]]), " ")[[1]])[-1]
  testflux
}