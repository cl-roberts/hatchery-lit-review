#--- helper functions ---#

# tabContent
# this function generates HTML code from a R list to be displayed below table
# x: R list to be converted to HTML code

tabContent <- function(x) {

  tabHeaders <- names(x) |>
    str_replace_all("_", " ") |>
    str_to_sentence()

  null_terms <- c("NA", "N/A", "not stated", "not specified", "not clearly stated", 
                  "not applicable", "see above", "irrelevant to study",
                  "not mentioned", "none stated", "no specific")

  out <- paste0("<b>", tabHeaders[1], ":</b><br>", x[1], "<br><br>")
  for(i in 2:length(x)) {

    if(is.null(x[i]) | x[i] == "" | x[i] == " " | x[i] == "No" | x[i] == "No." | 
        x[i] == "no" | x[i] == "None" | x[i] == "none") {

      condition <- TRUE
      
    } else {

      condition <- x[[i]] |>
        str_detect(pattern = regex(paste0("\\b", null_terms, "\\b"), ignore_case = TRUE)) |>
        any()

    }

    if (condition) next    
    
    out <- append(out, paste0("<b>", tabHeaders[i], ":</b><br>", x[i], "<br><br>"))
  
  }

  return(paste0(out, sep = " "))

}
