install.packages("shinydashboard")
install.packages("DT")

#testing, but works the best!!

library(shiny)
library(shinydashboard)
library(googlesheets4)
library(fitzRoy)
library(dplyr)
library(purrr)
library(stringr)
library(DT)

# UI ----------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "SuperCoach Data Updater"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Update Data", tabName = "update", icon = icon("sync")),
      menuItem("Settings", tabName = "settings", icon = icon("cog"))
    )
  ),
  dashboardBody(
    tabItems(
      # Update Data tab
      tabItem(tabName = "update",
              fluidRow(
                box(
                  title = "Google Sheet Information",
                  width = 12,
                  textInput("sheet_url", "Google Sheet URL", ""),
                  selectInput("afl_round", "AFL Round to Update", 
                              choices = 0:24, selected = 0),  # Default to Round 1
                  actionButton("update_btn", "Update SuperCoach Data", 
                               class = "btn-primary")
                )
              ),
              fluidRow(
                box(
                  title = "Progress",
                  width = 12,
                  verbatimTextOutput("progress_log")
                )
              ),
              fluidRow(
                box(
                  title = "Results Preview",
                  width = 12,
                  DT::DTOutput("results_preview")
                )
              )
      ),
      
      # Settings tab
      tabItem(tabName = "settings",
              fluidRow(
                box(
                  title = "Authentication",
                  width = 12,
                  p("This app requires authentication to access your Google Sheets."),
                  actionButton("auth_btn", "Authenticate Google Sheets", 
                               class = "btn-success"),
                  checkboxInput("save_auth", "Save authentication for future sessions", TRUE)
                )
              )
      )
    )
  )
)


# server function ---------------------------------------------------------
server <- function(input, output, session) {
  # Log output for progress tracking
  log_output <- reactiveVal("")
  
  # Add to log function
  add_to_log <- function(message) {
    current_log <- log_output()
    log_output(paste0(current_log, message, "\n"))
    output$progress_log <- renderText({ log_output() })
  }
  
  # Reactive value to store preview data
  preview_data <- reactiveVal(NULL)
  
  # Authentication
  observeEvent(input$auth_btn, {
    add_to_log("Authenticating with Google...")
    tryCatch({
      # Set up authentication
      if(input$save_auth) {
        googlesheets4::gs4_auth()
      } else {
        googlesheets4::gs4_auth(cache = FALSE)
      }
      add_to_log("Authentication successful!")
    }, 
    error = function(e) {
      add_to_log(paste("Authentication error:", e$message))
    })
  })
  
  # Update button logic
  observeEvent(input$update_btn, {
    req(input$sheet_url)
    
    add_to_log("Starting update process...")
    add_to_log(paste("Processing round:", input$afl_round))
    
    # Validate sheet URL
    if (!str_detect(input$sheet_url, "^https://docs.google.com/spreadsheets/d/")) {
      add_to_log("Error: Invalid Google Sheet URL")
      return()
    }
    
    # Try to access the Google Sheet
    tryCatch({
      sheet_id <- input$sheet_url
      add_to_log("Connecting to Google Sheet...")
      sheet_data <- googlesheets4::sheet_names(sheet_id)
      add_to_log(paste("Found", length(sheet_data), "tabs in the sheet"))
      
      # Get SuperCoach data for the selected round
      add_to_log("Fetching SuperCoach data from fitzRoy...")
      
      tryCatch({
        # Get the round number as numeric
        round_val <- as.numeric(input$afl_round)
        round_string <- paste("Round", round_val)
        
        add_to_log(paste("Fetching all player stats and filtering for", round_string))
        
        # Get all player stats and filter for the specific round
        player_stats <- fitzRoy::fetch_player_stats_footywire(
          season = as.numeric(format(Sys.Date(), "%Y")),
          round_number = NULL,
          check_existing = TRUE
        )
        
        # Filter for specific round
        player_stats_filtered <- player_stats %>% 
          filter(Round == round_string)
        
        # Final check for data
        if (is.null(player_stats_filtered) || nrow(player_stats_filtered) == 0) {
          add_to_log(paste("Error: No player data found for", round_string))
          return()
        }
        
        # Identify which column contains SuperCoach points
        sc_column <- NULL
        if ("SC" %in% names(player_stats_filtered)) {
          sc_column <- "SC"
        } else {
          add_to_log("Warning: Column 'SC' not found in player_stats")
        }
        
        if (is.null(sc_column)) {
          add_to_log("Warning: SuperCoach column not found in data. Available columns:")
          add_to_log(paste(names(player_stats_filtered), collapse = ", "))
          add_to_log("Attempting to continue with available data...")
          return()
        }
        
        add_to_log(paste("Retrieved SuperCoach data for", nrow(player_stats_filtered), "players in", round_string))
        add_to_log(paste("Using column", sc_column, "for SuperCoach points"))
        
        # Process each tab in the sheet
        updated_tabs <- 0
        
        for (tab_name in sheet_data) {
          add_to_log(paste("Processing tab:", tab_name))
          
          # Read the current tab
          tab_data <- googlesheets4::read_sheet(sheet_id, sheet = tab_name)
          
          if (is.null(tab_data) || nrow(tab_data) == 0) {
            add_to_log(paste("Skipping empty tab:", tab_name))
            next
          }
          
          # Assume first column contains player names
          player_col_name <- names(tab_data)[1]
          add_to_log(paste("Using player column:", player_col_name))
          
          # Create new column name for the round
          round_col_name <- paste0("SC_Round_", input$afl_round)
          
          # Check if the round column already exists in the sheet
          column_exists <- round_col_name %in% names(tab_data)
          
          # Find column position - either existing or where it will be added
          col_index <- if(column_exists) {
            which(names(tab_data) == round_col_name)
          } else {
            length(names(tab_data)) + 1  # Add at the end
          }
          
          # Get column letter for Google Sheets API
          col_letter <- LETTERS[col_index]
          if(col_index > 26) {
            col_letter <- paste0(LETTERS[(col_index-1) %/% 26], LETTERS[((col_index-1) %% 26) + 1])
          }
          
          # Create a vector to track player matches
          matches_list <- list()
          
          # For each player in the tab, find their stats
          for (i in 1:nrow(tab_data)) {
            player_name <- tab_data[[player_col_name]][i]
            if (is.na(player_name) || player_name == "") next
            
            # Look for the player in the player_stats
            player_match <- NULL
            
            # Try exact match first (case insensitive)
            player_match <- player_stats_filtered %>%
              filter(toupper(Player) == toupper(player_name))
            
            # Try partial match if exact match fails
            if (nrow(player_match) == 0) {
              player_match <- player_stats_filtered %>%
                filter(stringr::str_detect(toupper(Player), toupper(player_name)))
            }
            
            if (nrow(player_match) > 0) {
              # Store the row number, player name, and SC value
              matches_list[[length(matches_list) + 1]] <- list(
                row = i + 1,  # +1 for header row
                player = player_name,
                sc_value = as.numeric(player_match[[sc_column]][1])
              )
            }
          }
          
          # Create a data frame for preview
          preview_data_df <- tab_data
          
          # If the column doesn't exist in preview data, add it
          if (!column_exists) {
            preview_data_df[[round_col_name]] <- NA_real_
          }
          
          # Update the preview data with the SC values
          for (match in matches_list) {
            preview_data_df[[round_col_name]][match$row - 1] <- match$sc_value
          }
          
          # Store for preview
          if (updated_tabs == 0) {
            preview_data(preview_data_df)
          }
          
          # If column doesn't exist in actual sheet, create it
          if (!column_exists) {
            add_to_log(paste("Adding new column:", round_col_name))
            
            # Add the header only 
            googlesheets4::range_write(
              sheet_id,
              data = data.frame(header = round_col_name),
              sheet = tab_name,
              range = paste0(col_letter, "1"),
              col_names = FALSE
            )
          }
          
          # Update only the specific SC value cells to preserve formatting
          matches_count <- length(matches_list)
          if (matches_count > 0) {
            add_to_log(paste("Found SuperCoach scores for", matches_count, "out of", nrow(tab_data), "players"))
            
            # Update individual cells for each match
            for (match in matches_list) {
              # Create the cell reference
              cell_ref <- paste0(col_letter, match$row)
              
              # Write just the SC value to that specific cell
              googlesheets4::range_write(
                sheet_id,
                data = data.frame(score = match$sc_value),
                sheet = tab_name,
                range = cell_ref,
                col_names = FALSE
              )
            }
            
            add_to_log(paste("Updated individual SuperCoach scores in tab:", tab_name))
          } else {
            add_to_log(paste("No matching players found in tab:", tab_name))
          }
          
          updated_tabs <- updated_tabs + 1
        }
        
        add_to_log(paste("Update complete! Updated", updated_tabs, "tabs with SuperCoach data for", round_string))
        
      }, 
      error = function(e) {
        add_to_log(paste("Error fetching player stats:", e$message))
      })
    },
    error = function(e) {
      add_to_log(paste("Error accessing Google Sheet:", e$message))
    })
  })
  
  # Render the preview data table
  output$results_preview <- renderDataTable({
    req(preview_data())
    preview_data()
  })
}

shinyApp(ui = ui, server = server)

