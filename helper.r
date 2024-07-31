#--- helper functions ---#

library(stringr)

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

  out <- paste0("<b>", tabHeaders[1], ":</b><br>", x[1], "<br>")
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
    
    out <- append(out, paste0("<b>", tabHeaders[i], ":</b><br>", iconv(x[i], sub = ""), "<br><br>"))
  
  }

  return(paste0(out, sep = " "))

}


# str_to_bib
# this function creates a string to be written to a bib file for citations
# x: named list or data frame with names corresponding to bib entries

str_to_bib <- function(x) {

  article_key <- gsub(" ", "", paste0(x$citation, x$id, collapse = "_")) 

  x$title <- x$title |>
    iconv("UTF-8", "UTF-8", sub = "")

  x$authors_full <- x$authors_full |>
    str_replace_all("; ", " and ")

  article_meta <- list(author = x$authors_full,
                      title = x$title,
                      journal = x$journal,
                      year = x$year)

  out <- paste0("@Article{", article_key, ",\n", 
                paste0("\t", names(article_meta), " = {", 
                       article_meta, "},", collapse = "\n"),
                "\n}")

  return(out)

}
