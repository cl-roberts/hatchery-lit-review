#---- read and wrangle data ---#

lit_raw <- read.csv("data/lit-database.csv") 

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
                     "knowledge_gaps", "management_recommendation", "key_finding", 
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
lit$climate_change_bool <- ifelse(lit$climate_change == "Yes", TRUE, FALSE)

# table with covariates for filtering articles ----
filter_vars <- lit |>
  select(id, year, basin, journal, discipline, species, indigenous_bool, 
         peer_reviewed_bool, management_science, key_themes, climate_change_bool)

# table for displaying information on filtered articles ----
display_vars <- lit |>
  select(id, citation, title, key_finding, hypothesis, objective, methods, conclusion, key_themes,
         discipline, augmentation_objective, augmentation_type, aquatic_system_type,
         generations, life_stage_released, age_released, feeding_stage_released,
         wild_definition, conservation_standard, knowledge_gaps, management_recommendation) 

rownames(display_vars) <- display_vars$id

# assigns all trues for filters not yet set ----
all_trues <- rep(TRUE, nrow(filter_vars))
