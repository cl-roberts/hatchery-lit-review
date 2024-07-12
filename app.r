################################################################################

# Hatchery literature review app

# Author: CL Roberts

################################################################################


#------------------------------ front matter ----------------------------------#

# attach packages ----

library(shiny)
library(dplyr)
library(DT)

# read and wrangle data ----

lit_raw <- read.csv("lit-database.csv") 

database_names <- data.frame(
    column_names = c("x", "id", "citation", "title", "reviewer_name", 
                     "id", "title", "authors", "year", "journal", "lit_type",
                     "basin", "objective", "hypothesis", "management_science",
                     "discipline", "methods", "data_source", "study_dates", "indigenous", 
                     "knowledge_type", "interdisciplinary", "interdisciplinary_desc",
                     "species", "augmentation_type", "augmentation_objective_bool",
                     "augmentation_objective", "aquatic_system_type", "generations",
                     "generations_if_known", "life_stage_released", "age_released",
                     "feeding_stage_released", "feed_type_bool", "feed_type", 
                     "conclusion_bool", "conclusion", "wild_definition_bool", 
                     "wild_definition", "conservation_standard_bool", 
                     "conservation_standard", "knowledge_gaps_bool", 
                     "knowledge gaps", "management_rec_bool", "key_finding", 
                     "key_themes", "climate_change", "address_research_topics"),
    descriptions = gsub("\\.+", replacement = " ", names(lit_raw)) |>
        gsub("[0-9]", replacement = "", x = _) |>
        gsub("[[:space:]][a-zA-Z][[:space:]]", replacement = "", x = _) |>
        trimws() |>
        paste0("?"),
    raw_names = names(lit_raw)
)

name_vector <- database_names$raw_names
names(name_vector) <- database_names$column_names

lit <- lit_raw |>
    select(which(!duplicated(database_names$column_names))) |>
    rename(any_of(name_vector)) 
    
lit$year <- gsub("[^0-9.]", "", lit$year) |>
    as.numeric()

lit$indigenous_bool <- ifelse(lit$indigenous == "Yes", TRUE, FALSE)
lit$peer_reviewed_bool <- grepl("peer-reviewed", lit$lit_type, ignore.case = TRUE)

#------------------------------ user interface --------------------------------#

ui <- fluidPage(

  # app title ----
  titlePanel("Hatchery Literature Review App"),

  # sidebar layout with input and output definitions ----
  sidebarLayout(

    # sidebar panel for inputs ----
    sidebarPanel(

      # year slider ----
      sliderInput(inputId = "year",
                  label = "Year Published:",
                  min = min(lit$year),
                  max = max(lit$year),
                  value = range(lit$year),
                  sep = ""),

      # row of dropdown menus ----
      fluidRow( 
          column(width = 4,
            selectInput(inputId = "basin",
                        label = "Basin:",
                        choices = c("All", "Arctic", "Atlantic", "Pacific", "Indian", "Other"),
                        selected = "All"),
          ),
          column(width = 4,
            selectInput(inputId = "genus",
                      label = "Genus:", 
                      choices = c("All", "Oncorhynchus", "Salmo"),
                      selected = "All")
          ),
          column(width = 4,
            conditionalPanel("input.genus == 'Oncorhynchus'",
              selectInput(inputId = "species_oncorhynchus",
                        label = "Species:", 
                        choices = c("All", 
                                    "gorbuscha (pink salmon)",
                                    "keta (chum salmon)",
                                    "kisutch (coho salmon)",
                                    "nerka (sockeye salmon)",
                                    "mykiss (steelhead)",
                                    "tshawytscha (chinook salmon)"),
                        selected = "All")
            ),
            conditionalPanel("input.genus == 'Salmo'",
              selectInput(inputId = "species_salmo",
                        label = "Species:", 
                        choices = c("All",
                                    "salar (Atlantic salmon)", 
                                    "trutta (brown trout)"),
                        selected = "All")
            )
          )
        ),

      # row of search bars ----
      fluidRow(
        column(width = 6,
          textInput(inputId = "journal",
                    label = "Journal", 
                    placeholder = "e.g. Conservation Genetics")
        ),
        column(width = 6,
          textInput(inputId = "discipline",
                    label = "Discipline", 
                    placeholder = "e.g. Stock Assessment")
        )
      ),

      # row of check boxes ----
      fluidRow(
        column(width = 6,
          checkboxInput(inputId = "indigenous",
                        label = "Indigenous knowledge considered?",
                        value = FALSE),
          checkboxInput(inputId = "peer_reviewed",
                        label = "Peer reviewed?", 
                        value = TRUE)
        ),
        column(width = 6,
          checkboxGroupInput(inputId = "management_science",
                       label = "Management or science focused?", 
                       choices = c("Management", "Science"),
                       selected = NULL)
        )
      ),

      # download button ----
      downloadButton("downloadData", "Download Metadata for Selected Articles", 
                     style = "display: flow;")                     

    ),

    # main table panel ----
    mainPanel(

      dataTableOutput('table')  

    )
  
  ),
 
)

#--------------------------------- server -------------------------------------#

server <- function(input, output) {

  datasetInput <- reactive({

    year_filter <- (input$year[1] <= lit$year) & (lit$year <= input$year[2])  

    if (input$basin == "All") {

      basin_filter <- rep(TRUE, nrow(lit))

    } else {

      basin_filter <- grepl(input$basin, lit$basin, ignore.case = TRUE)

    }
    
    journal_filter <- grepl(input$journal, lit$journal, ignore.case = TRUE)

    if (input$discipline != "") {

      search_terms <- input$discipline |>
        strsplit(split = " ") |>
        unlist()

      matches <- matrix(nrow = nrow(lit), ncol = length(search_terms))
      for(i in seq(search_terms)) {
        matches[,i] <- grepl(pattern = search_terms[i], x = lit$discipline, ignore.case = TRUE)
      }

      discipline_filter <- apply(X = matches, MARGIN = 1, FUN = all)
  
    } else {

      discipline_filter <- rep(TRUE, nrow(lit))
    
    }

    if (input$genus == "All") {

      genus_filter <- rep(TRUE, nrow(lit))
      oncorhynchus_filter <- rep(TRUE, nrow(lit))
      salmo_filter <- rep(TRUE, nrow(lit))

    } else {

      genus_filter <- as.list(lit$species) |>
        sapply(FUN = function(x) {
                pattern_end <- ifelse(input$genus == "Oncorhynchus", " |Pacific Salmon", " ")
                grepl(pattern = paste0(input$genus, pattern_end), 
                                      x = x, ignore.case = TRUE)
                })
  
      if (input$species_oncorhynchus == "All") {

        oncorhynchus_filter <- rep(TRUE, nrow(lit))

      } else {

        selected_species <- gsub("[[:punct:]]", " ", x = input$species_oncorhynchus) |> 
          strsplit(split = " ") |>
          unlist()  

        oncorhynchus_filter <- as.list(lit$species) |>
            sapply(FUN = function(x) grepl(pattern = selected_species, x = x, ignore.case = TRUE))

      } 
    
      if (input$species_salmo == "All") {

        salmo_filter <- rep(TRUE, nrow(lit))

      } else {

        selected_species <- gsub("[[:punct:]]", " ", x = input$species_salmo) |> 
          strsplit(split = " ") |>
          unlist() 

        salmo_filter <- as.list(lit$species) |>
            sapply(FUN = function(x) grepl(pattern = selected_species, x = x, ignore.case = TRUE))
        
      }

    }

    if (input$genus == "All") {

      species_filter <- rep(TRUE, nrow(lit))
    
    } else if (input$genus == "Oncorhynchus") {
    
      species_filter <- oncorhynchus_filter
    
    } else if (input$genus == "Salmo") {
    
      species_filter <- salmo_filter
    
    }
    
    indigenous_filter <- ifelse(input$indigenous, lit$indigenous_bool, !lit$indigenous_bool)

    peer_reviewed_filter <- ifelse(input$peer_reviewed, lit$peer_reviewed_bool, !lit$peer_reviewed_bool)

    management_science_nopunc <- gsub("[[:punct:]]", " ", x = lit$management_science)

    if (is.null(input$management_science)) {

      management_science_filter <- rep(TRUE, nrow(lit))
  
    } else if (length(input$management_science) == 1) {

      management_science_filter <- grepl(input$management_science, 
                                        management_science_nopunc, 
                                        ignore.case = TRUE) 

    } else if (length(input$management_science) > 1) {

      management_and_science <- sapply(as.list(input$management_science), 
                                      FUN = function(x) {
                                          grepl(x, management_science_nopunc, ignore.case = TRUE) 
                                        }) |>
        apply(MARGIN = 1, FUN = all)

      both <- grepl("both", management_science_nopunc, ignore.case = TRUE)

      management_science_filter <- apply(cbind(management_and_science, both), MARGIN = 1, FUN = any)

    }

    tbl <- lit |> 
      filter(year_filter, basin_filter, journal_filter, discipline_filter,
            genus_filter, species_filter, indigenous_filter, peer_reviewed_filter,
            management_science_filter)

    tbl

  })

  output$table <- renderDataTable({

    show_columns <- colnames(lit) %in% c("id", "citation", "title")

    datatable(datasetInput(), 
              options = list(columnDefs = list(list(visible=FALSE, 
                                                    targets=which(!show_columns)
                                                    )),
                             rowCallback = JS(
                              "function(row, data) {",
                              paste0("var full_text = ",
                                     "'\\n\\nHypothesis: ' +",
                                     paste0("data[", which(colnames(lit) == "hypothesis"), "] + "),
                                     "'\\n\\nObjective: ' +",
                                     paste0("data[", which(colnames(lit) == "objective"), "] + "),
                                     "'\\n\\nConclusion: ' +",
                                     paste0("data[", which(colnames(lit) == "conclusion"), "]")                                     
                                     ),
                              "$('td', row).attr('title', full_text);",
                              "}"
                              )
                            )
              )

  })

  output$downloadData <- downloadHandler(

    filename = function() {
      paste0("selected_article_metadata_", gsub(" ","_", gsub(":",".", Sys.time())),".csv")
    },

    content = function(file) {

      write.csv(datasetInput()[input$table_rows_selected,], file, row.names = TRUE)

    }

  )

}

# tbl <- lit[4:10,]

# tbl[c(2, 5),"id"] %in% lit_raw$Covidence..


# Create Shiny app ----
shinyApp(ui = ui, server = server)
