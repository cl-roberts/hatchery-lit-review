library(shiny)
library(dplyr)
library(DT)

# read in data
lit_raw <- read.csv("lit-database.csv") 

database_names <- data.frame(
    column_names = c("x", "covidence_id", "citation", "title", "reviewer_name", 
                     "covidence_id", "title", "authors", "year", "journal", "lit_type",
                     "basin", "objective", "hypothesis", "management_science",
                     "discipline", "methods", "data_source", "study_dates", "indigenous_bool", 
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

ui <- fluidPage(

  # tags$head(tags$style(HTML('
  #   #genus+ div>.selectize-input {font-style: italic;}
  #   #genus+ div>.selectize-dropdown {font-style: italic;}
  #   #species+ div>.selectize-input {font-style: italic;}
  #   #species+ div>.selectize-dropdown {font-style: italic;}
  #                           '))),

  # App title ----
  titlePanel("Hatchery Literature Review App"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      sliderInput(inputId = "year",
                  label = "Year Published:",
                  min = min(lit$year),
                  max = max(lit$year),
                  value = range(lit$year),
                  sep = ""),

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
      )

    ),

    # main table panel
    mainPanel(

      dataTableOutput("table")

    )
  
  ),
 
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  output$table <- renderDataTable({

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

        selected_species <- strsplit(input$species_oncorhynchus, split = " ") |>
          unlist() |>
          gsub("[[:punct:]]", "", x = _)  

        oncorhynchus_filter <- as.list(lit$species) |>
            sapply(FUN = function(x) grepl(pattern = selected_species, x = x, ignore.case = TRUE))

      } 
    
      if (input$species_salmo == "All") {

        salmo_filter <- rep(TRUE, nrow(lit))

      } else {

        selected_species <- strsplit(input$species_salmo, split = " ") |>
          unlist() |>
          gsub("[[:punct:]]", "", x = _)  

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
    
    tbl <- lit |> 
      filter(year_filter, basin_filter, journal_filter, discipline_filter,
             genus_filter, species_filter) |>
      select(covidence_id, citation, journal, basin, species)

    datatable(tbl)

  })

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
