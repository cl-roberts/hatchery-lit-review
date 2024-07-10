library(shiny)
library(dplyr)
library(DT)

# read in data
lit_raw <- read.csv("lit-database.csv") 

database_names <- data.frame(
    column_names = c("x", "covidence_id", "citation", "title", "reviewer_name", 
                     "covidence_id", "title", "authors", "year", "journal", "lit_type",
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
    rename(any_of(name_vector)) |>
    select(covidence_id, citation, year, journal, basin)
    
lit$year <- gsub("[^0-9.]", "", lit$year) |>
    as.numeric()

ui <- fluidPage(

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
                  sep = "")

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

      lit |> 
        filter(between(year, input$year[1], input$year[2])) |>
        datatable()

    })

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
