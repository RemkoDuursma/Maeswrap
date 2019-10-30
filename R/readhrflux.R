#' Reads the hrflux.dat MAESTRA/MAESPA output file
#'
#' @description Reads the hourly output file (hrflux.dat).
#'
#'
#' @param filename Default name of the (half-)hourly output file.
#' @return Returns a dataframe.
#' @author Remko Duursma
#' @keywords utilities
#' @examples
#'
#'
#' \dontrun{
#'
#' # Simple as this:
#' mysim2 <- readhrflux()
#' }
#' @export
readhrflux= function (filename = "hrflux.dat"){
  hrlines <- readLines(filename, n= 50)
  colloc <- grep("Columns", hrlines)
  hrflux <- data.table::fread(filename, skip = colloc, na.strings = "NaN", data.table = F)
  names(hrflux) <- delempty(strsplit(delempty(strsplit(hrlines[colloc], "Columns:")[[1]]), " ")[[1]])
  hrflux$conttime <- hrflux$DOY + hrflux$HOUR/max(hrflux$HOUR)
  
  hrflux
}

