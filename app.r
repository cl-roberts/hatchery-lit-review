################################################################################

# Hatchery literature review app

# Author: CL Roberts

################################################################################


#------------------------------ front matter ----------------------------------#

# attach packages and source utility scripts ----

library(shiny)
library(DT)
library(bslib)
library(tidyverse)
library(rlang)
library(stringr)

source("setup.r")
source("helper.r")

#------------------------------ user interface --------------------------------#

ui  <- page_sidebar(

  # app theme ----
  theme = bs_theme(bootswatch = "sandstone", font_scale = .8),

  # app title ----
  title = h2("Hatchery Literature Review App"),

  tags$head(tags$script(
              HTML('$(document).ready(function() {
                      $(".navbar .container-fluid")
                        .append("<img id=fish src=fish.svg align=right height=57.5px>");
                    });'))
            ),

  # sidebar panel for inputs ----
  sidebar = sidebar(title = h4("Search Literature:"), width = 350,

    # year slider ----
    sliderInput(inputId = "year",
                label = "Year Published:",
                min = min(filter_vars$year),
                max = max(filter_vars$year),
                value = range(filter_vars$year),
                sep = ""),

    # row of dropdown menus ----
    fluidRow( 
        column(width = 6,
          selectInput(inputId = "genus",
                    label = "Genus:", 
                    choices = c("All species", "Oncorhynchus", "Salmo"),
                    selected = "All species")
        ),
        column(width = 6,
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

    # search bars ----
    column(width = 12,
      textInput(inputId = "key_words",
                label = "Key Words", 
                placeholder = "e.g. Stock Assessment")
    ),
    column(width = 12,
      selectInput(inputId = "basin",
                  label = "Basin:",
                  choices = c("All", "Arctic", "Atlantic", "Pacific", "Indian", "Other"),
                  selected = "All"),
    ),
    column(width = 12,
      textInput(inputId = "journal",
                label = "Journal", 
                placeholder = "e.g. Conservation Genetics")
    ),


    # check boxes ----
    column(width = 12,
      checkboxGroupInput(inputId = "management_science",
                    label = "Management or science focused?", 
                    choices = c("Management", "Science"),
                    selected = NULL, inline = TRUE)
    ),

    radioButtons(inputId = "indigenous",
                  label = "Indigenous knowledge considered?",
                  choices = c("Yes", "No", "Either"), selected = "Either"),
    radioButtons(inputId = "peer_reviewed",
                  label = "Peer reviewed?", 
                  choices = c("Yes", "No", "Either"), selected = "Either"),
    radioButtons(inputId = "climate_change",
                  label = "Addresses climate change?", 
                  choices = c("Yes", "No", "Either"), selected = "Either"),

    # download button ----
    downloadButton("downloadData", "Download Metadata for Selected Articles", 
                    style = "display: flow;")

  ),

  layout_columns(

    # card(card_header(h4("Debugging pane")), 
    #      textOutput('debug')),

    card(card_header(h4("Literature Search Results")),
         dataTableOutput('table')),

    card(card_header(h4("More Information on Selected Articles")), 
         uiOutput('selected_with_tabs')),

    col_widths = c(7, 5)

  )

)

#--------------------------------- server -------------------------------------#

server <- function(input, output) {

  # bs_themer()  # uncomment to enable a widget for previewing different themes

  # set of table filters reactive to user input ----
  datasetInput <- reactive({

    # filter by year range ---
    year_filter <- (input$year[1] <= filter_vars$year) & (filter_vars$year <= input$year[2])  

    # filter by hydrological basin ---
    if (input$basin == "All") {

      basin_filter <- all_trues

    } else {

      basin_filter <- grepl(input$basin, filter_vars$basin, ignore.case = TRUE)

    }
    
    # search for specific journals ---
    journal_filter <- grepl(input$journal, filter_vars$journal, ignore.case = TRUE)

    # search using key words (uses both discipline and key themes) ---
    if (input$key_words != "") {
      
      search_key_words <- input$key_words |>
        strsplit(split = " ") |>
        unlist()

      matches <- matrix(nrow = nrow(filter_vars), ncol = length(search_key_words))
      for(i in seq(search_key_words)) {
        tmp_discipline <- grepl(pattern = search_key_words[i], x = filter_vars$discipline, ignore.case = TRUE)
        tmp_key_themes <- grepl(pattern = search_key_words[i], x = filter_vars$key_themes, ignore.case = TRUE)
        matches[,i] <- apply(cbind(tmp_discipline, tmp_key_themes), MARGIN = 1, FUN = any)
      }

      key_words_filter <- apply(X = matches, MARGIN = 1, FUN = all)
  
    } else {

      key_words_filter <- all_trues
    
    }

    # filter by genus and species ---
    if (input$genus == "All species") {

      genus_filter <- all_trues
      oncorhynchus_filter <- all_trues
      salmo_filter <- all_trues

    } else {

      genus_filter <- as.list(filter_vars$species) |>
        sapply(FUN = function(x) {
                pattern_end <- ifelse(input$genus == "Oncorhynchus", " |Pacific Salmon", " ")
                grepl(pattern = paste0(input$genus, pattern_end), 
                                      x = x, ignore.case = TRUE)
                })
  
      if (input$species_oncorhynchus == "All") {

        oncorhynchus_filter <- all_trues

      } else {

        selected_species <- gsub("[[:punct:]]", " ", x = input$species_oncorhynchus) |> 
          strsplit(split = " ") |>
          unlist()  

        oncorhynchus_filter <- as.list(filter_vars$species) |>
            sapply(FUN = function(x) grepl(pattern = selected_species[1], x = x, ignore.case = TRUE))

      } 
    
      if (input$species_salmo == "All") {

        salmo_filter <- all_trues

      } else {

        selected_species <- gsub("[[:punct:]]", " ", x = input$species_salmo) |> 
          strsplit(split = " ") |>
          unlist() 

        salmo_filter <- as.list(filter_vars$species) |>
            sapply(FUN = function(x) grepl(pattern = selected_species[1], x = x, ignore.case = TRUE))
        
      }

    }

    # condense into single filter ---
    if (input$genus == "All species") {

      species_filter <- all_trues
    
    } else if (input$genus == "Oncorhynchus") {
    
      species_filter <- oncorhynchus_filter
    
    } else if (input$genus == "Salmo") {
    
      species_filter <- salmo_filter
    
    }
    
    # filter by indigenous knowledge check box ---
    if (input$indigenous == "Yes") {

      indigenous_filter <- filter_vars$indigenous_bool

    } else if (input$indigenous == "No") {

      indigenous_filter <- !filter_vars$indigenous_bool

    } else {
      
      indigenous_filter <- all_trues

    }
    
    # filter by peer reviewed check box ---
    if (input$peer_reviewed == "Yes") {

      peer_reviewed_filter <- filter_vars$peer_reviewed_bool

    } else if (input$peer_reviewed == "No") {

      peer_reviewed_filter <- !filter_vars$peer_reviewed_bool

    } else {

      peer_reviewed_filter <- all_trues

    }

    # filter by climate change check box ---
    if (input$climate_change == "Yes") {

      climate_change_filter <- filter_vars$climate_change_bool

    } else if (input$climate_change == "No") {

      climate_change_filter <- !filter_vars$climate_change_bool

    } else {

      climate_change_filter <- all_trues

    }

    # filter by management and/or science focused ---
    management_science_nopunc <- gsub("[[:punct:]]", " ", x = filter_vars$management_science)

    if (is.null(input$management_science)) {

      management_science_filter <- all_trues
  
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

    # apply all filters and return table ---
    tbl <- display_vars |> 
      filter(year_filter, basin_filter, journal_filter, key_words_filter,
            genus_filter, species_filter, indigenous_filter, peer_reviewed_filter,
            management_science_filter, climate_change_filter)

    rownames(tbl) <- tbl$id
    tbl 

  })

  # render filtered table ---
  output$table <- renderDataTable({

    show_columns <- colnames(display_vars) %in% c("citation", "title")

    datatable(datasetInput(), escape = FALSE, 
              selection = list(mode = "multiple"),
              style = "auto", colnames = str_to_title(colnames(datasetInput())),
              options = list(columnDefs = list(list(visible=FALSE, 
                                                    targets=which(!show_columns)
                                                    ))
                            )
              )

  })

  # identify selected tabs ----
  filteredTable_selected <- eventReactive(input$table_rows_selected, {

    ids <- input$table_rows_selected
    rownames(datasetInput()[ids,])

  })

  # generate selected tabs ----
  output$selected_with_tabs <- renderUI({

    tabs <- filteredTable_selected() %>% 
              map2(1:length(.), ~ tabPanel(title = .x, {

                datasetInput()[rownames(datasetInput()) == .,] |>
                  select(!id) |>
                  tabContent() |>
                  HTML()

              }))

    tabsetPanel_wselection <- partial(tabsetPanel, selected = filteredTable_selected()[length(filteredTable_selected())])
    
    tagList(exec(tabsetPanel_wselection, !!!tabs)) 
    
  }) 

  # handle csv download for selected rows ---
  output$downloadData <- downloadHandler(

    filename = function() {

      paste0("selected_articles", gsub(" ","_", gsub(":",".", Sys.time())),".csv")

    },

    content = function(file) {

      rows <- match(datasetInput()[input$table_rows_selected,"id"], lit$id)

      write.table(lit[rows,], file, sep = ",", 
                  row.names = FALSE, col.names = database_names$descriptions)

    }

  )

  # prints output to UI for debugging purposes ----
  # output$debug <- renderText({
  #   toString()
  # })

}


# open shiny app ----
shinyApp(ui = ui, server = server)
