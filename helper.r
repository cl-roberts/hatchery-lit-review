#--- helper functions ---#

# tabContent
# this function generates HTML code from a R list to be displayed below table
# x: R list to be converted to HTML code

tabContent <- function(x) {

  out <- paste0("<b>", stringr::str_to_title(names(x[1])), ":</b><br>", x[1], "<br><br>")
  for(i in 2:length(x)) {
    out <- append(out, paste0("<b>", stringr::str_to_title(names(x[i])), ":</b><br>", x[i], "<br><br>"))
  }

  return(paste0(out, sep = " "))

}
