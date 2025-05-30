#' Retrieves most recent file matching name
#'
#' Retrieves the name of the most recent file matching the name pattern
#' File name is expected to be in the format: YYYY-MM-DDnamepattern
#' @param mydir
#' @param namepattern
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' file_find("/path/to/my/data/", "mydatafile.csv")
#' }
file_find = function(mydir, namepattern){
  # get the most recent file fitting a pattern
  myfiles = list.files(path = mydir, pattern = namepattern)
  datefiles = stringr::str_match(myfiles, "[0-9]{4}-[0-9]{2}-[0-9]{2}")
  datefiles = datefiles[!is.na(datefiles)]
  mydates = do.call(c, lapply(datefiles, function(x) lubridate::ymd(x)))
  mydate = max(mydates)
  return(paste0(mydir, mydate, namepattern))
}
