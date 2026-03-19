library(sortable)
library(shiny)
library(bslib)
library(dplyr)
library(readxl)
library(writexl)
library(stringi)
library(reactable)
library(smartabaseR)
library(blastula)
library(htmltools)
library(purrr)
library(DBI)
library(odbc)
library(tidyr)
library(shinyjs)


get_lookup_sql <- function() {
  con <- DBI::dbConnect(odbc::odbc(), "Aquarium")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbReadTable(
    con,
    DBI::Id(
      catalog = "Scratch",
      schema  = "morr",
      table   = "international_lookup_table_ab_uuid"
    ),
    check.names = FALSE
  ) %>%
    tibble::as_tibble()
}

get_lookup_sql_for_app <- function() {
  get_lookup_sql() %>%
    dplyr::mutate(
      `Date of Birth` = ifelse(
        is.na(`Date of Birth`),
        NA_character_,
        format(as.Date(`Date of Birth`), "%d/%m/%Y")
      )
    )
}

generate_unique_ids <- function(n, existing_ids) {
  ids <- character(n)
  all_ids <- as.character(existing_ids)

  for (i in seq_len(n)) {
    repeat {
      new_id <- paste0("9", stringi::stri_rand_strings(1, 6, pattern = "[0-9]"))
      if (!(new_id %in% all_ids)) {
        ids[i] <- new_id
        all_ids <- c(all_ids, new_id)
        break
      }
    }
  }

  ids
}

insert_lookup_sql <- function(new_row) {
  con <- DBI::dbConnect(odbc::odbc(), "Aquarium")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  table_id <- DBI::Id(
    catalog = "Scratch",
    schema  = "morr",
    table   = "international_lookup_table_ab_uuid"
  )

  existing_ids <- DBI::dbReadTable(
    con,
    table_id,
    check.names = FALSE
  ) %>%
    dplyr::pull(UUID)

  new_uuid <- generate_unique_ids(1, existing_ids)

  dob_raw <- new_row$`Date of Birth`

  dob_clean <- if (length(dob_raw) == 0 || is.null(dob_raw) || is.na(dob_raw)) {
    as.Date(NA)
  } else if (inherits(dob_raw, "Date")) {
    dob_raw
  } else {
    as.Date(as.character(dob_raw), format = "%d/%m/%Y")
  }

  row_to_insert <- new_row %>%
    dplyr::mutate(
      `Date of Birth` = dob_clean,
      UUID = new_uuid
    )

  DBI::dbAppendTable(
    con,
    table_id,
    row_to_insert
  )

  new_uuid
}



enrich_age <- function(df) {
  if (!"Date of Birth" %in% names(df)) return(df)
  df %>%
    mutate(
      dob = as.Date(`Date of Birth`, format = "%d-%m-%Y"),
      Age = ifelse(
        !is.na(dob),
        round(as.numeric(difftime(Sys.Date(), dob, units = "days")) / 365.25, 1),
        Age
      )
    ) %>%
    select(-dob, -`Date of Birth`)
}

ensure_cols <- function(df, cols) {
  for (col in cols) {
    if (!col %in% names(df)) df[[col]] <- NA
  }
  df
}

RECIPIENT <- "pei-smartabase-profil-aaaatofucqgvfmyj2hort6lfe4@marlins.org.slack.com"


send_profile_email <- function(player_row) {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  write.csv(player_row, tmp, row.names = FALSE)

  email <- blastula::compose_email(
    body = blastula::md(
      paste0(
        "**New Smartabase profile created**\n\n",
        "- Name: ", player_row$`First Name`, " ", player_row$`Last Name`, "\n",
        "- DOB: ", ifelse(
          is.na(player_row$`Date of Birth`),
          "NA",
          format(as.Date(player_row$`Date of Birth`), "%d/%m/%Y")
        ), "\n",
        "- UUID: ", player_row$UUID, "\n",
        "- Username: ", player_row$Username, "\n",
        "- Email Address: ", player_row$`Email Address`, "\n",
        "- Primary Group: ", player_row$`Groups as Athlete`, "\n",
        "- Secondary Group: ", player_row$`Secondary Group`, "\n",
        "- Signing Class: ", player_row$`Tertiary Group`, "\n\n",
        "Profile CSV attached for import."
      )
    )
  ) %>%
    blastula::add_attachment(file = tmp,
                             filename = paste0(
                               tolower(player_row$`First Name`), "_",
                               tolower(player_row$`Last Name`), "_profile.csv"
                             ))

  blastula::smtp_send(
    email,
    from = Sys.getenv("SB_USER_EMAIL"),
    to = RECIPIENT,
    subject = paste0(
      "New Smartabase Profile: ",
      player_row$`First Name`, " ", player_row$`Last Name`
    ),
    credentials = blastula::creds_file("gmail_creds")
  )
}

# --- Marlins Brand ---
m <- list(
  black   = "#000000",
  blue    = "#00A3E0",
  red     = "#EF3340",
  slate   = "#41748D",
  bg      = "#F5F6F8",
  card    = "#FFFFFF",
  border  = "#E0E3E8",
  muted   = "#8C939D",
  text    = "#1A1D23",
  success = "#2E7D56",
  warn    = "#D4790E"
)

countries <- c("Aruba", "Bahamas", "Brazil", "Colombia", "Curacao",
               "Dominican Republic", "Mexico", "Nicaragua", "Panama",
               "Venezuela", "Other")

positions <- c("SP", "RP", "CF", "RF", "LF", "3B", "SS", "2B", "1B", "C")

scouts <- paste("Scout", 1:10)



load_sb_users <- function(single_user = FALSE) {
  url <- Sys.getenv("SB_URL")
  username <- Sys.getenv("SB_USER")
  password <- Sys.getenv("SB_PASS")

  if (!nzchar(url) || !nzchar(username) || !nzchar(password)) {
    stop("Missing one or more required env vars: SB_URL, SB_USER, SB_PASS")
  }

  invisible(
    sb_login(
      url = url,
      username = username,
      password = password,
      option = sb_login_option(interactive_mode = FALSE)
    )
  )

  user_filter <- if (single_user) {
    sb_get_user_filter(
      user_key = "username",
      user_value = username
    )
  } else {
    sb_get_user_filter()
  }

  sb_get_user(
    url = url,
    username = username,
    password = password,
    filter = user_filter,
    option = sb_get_user_option(
      interactive_mode = FALSE,
      include_all_cols = FALSE,
      cache = FALSE,
      include_missing_user = FALSE,
      guess_col_type = FALSE
    )
  ) %>%
    mutate(about = trimws(about)) %>%
    select(about, user_id)
}


# --- CSS ---
app_css <- paste0('
  @import url("https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;600;700&display=swap");

  :root {
    --black: ', m$black, ';
    --blue: ', m$blue, ';
    --red: ', m$red, ';
    --slate: ', m$slate, ';
    --bg: ', m$bg, ';
    --card: ', m$card, ';
    --border: ', m$border, ';
    --muted: ', m$muted, ';
    --text: ', m$text, ';
  }

  * { box-sizing: border-box; }

  body {
    background: var(--bg);
    font-family: "DM Sans", sans-serif;
    color: var(--text);
    font-size: 14px;
    line-height: 1.5;
  }

  .app-header {
    background: var(--black); padding: 18px 32px;
    margin: -16px -16px 28px -16px;
    display: flex; align-items: center; gap: 14px;
  }
  .app-header .logo-mark {
    width: 36px; height: 36px;
    background: var(--blue); border-radius: 8px;
    display: flex; align-items: center; justify-content: center;
    font-weight: 700; font-size: 18px; color: var(--black);
  }
  .app-header h1 {
    color: #FFFFFF; font-size: 1.15rem; font-weight: 600;
    margin: 0; letter-spacing: -0.01em;
  }
  .app-header .env-badge {
    margin-left: auto;
    background: rgba(255,255,255,0.08);
    color: var(--muted);
    font-size: 0.7rem; font-weight: 500;
    padding: 3px 10px; border-radius: 4px;
    text-transform: uppercase; letter-spacing: 0.05em;
  }

  .nav-tabs { border-bottom: 2px solid var(--border); margin-bottom: 24px; }
  .nav-tabs .nav-link {
    font-weight: 600; font-size: 0.82rem; color: var(--muted);
    border: none; padding: 10px 20px;
    text-transform: uppercase; letter-spacing: 0.04em;
    transition: color 0.15s;
  }
  .nav-tabs .nav-link:hover { color: var(--text); background: none; border: none; }
  .nav-tabs .nav-link.active {
    color: var(--blue); background: none;
    border: none; border-bottom: 3px solid var(--blue);
  }

  .rank-list-container .rank-list {
    background: transparent !important;
    padding: 0 !important;
  }
  .rank-list-container .rank-list-item {
    background: white;
    border: 1px solid var(--border);
    border-radius: 8px;
    padding: 12px 16px;
    margin-bottom: 6px;
    cursor: grab;
    display: flex;
    align-items: center;
    gap: 12px;
    font-size: 0.88rem;
    transition: box-shadow 0.15s, border-color 0.15s;
  }
  .rank-list-container .rank-list-item:hover {
    border-color: var(--blue);
    box-shadow: 0 2px 8px rgba(0,163,224,0.1);
  }
  .rank-list-container .rank-list-item.sortable-chosen {
    border-color: var(--blue);
    box-shadow: 0 4px 16px rgba(0,163,224,0.15);
  }
  .rank-num {
    background: var(--slate); color: white;
    font-weight: 700; font-size: 0.75rem;
    width: 28px; height: 28px; border-radius: 50%;
    display: flex; align-items: center; justify-content: center;
    flex-shrink: 0;
  }
  .rank-player-name { font-weight: 600; }
  .rank-player-meta { font-size: 0.75rem; color: var(--muted); margin-left: auto; }
  .rank-grip { color: #C4C9D1; font-size: 1.1rem; margin-right: 4px; cursor: grab; }

  .card-section {
    background: var(--card);
    border: 1px solid var(--border);
    border-radius: 10px;
    padding: 24px 28px;
    margin-bottom: 20px;
  }
  .card-section h4 {
    font-size: 0.82rem; font-weight: 700;
    text-transform: uppercase; letter-spacing: 0.06em;
    color: var(--slate); margin: 0 0 16px 0;
    padding-bottom: 10px;
    border-bottom: 1px solid var(--border);
  }

  .form-label, label {
    font-size: 0.75rem; font-weight: 600;
    color: var(--muted); text-transform: uppercase;
    letter-spacing: 0.04em; margin-bottom: 4px;
  }
  .form-control, .selectize-input, .form-select {
    border: 1px solid var(--border) !important;
    border-radius: 6px !important;
    font-size: 0.88rem !important;
    padding: 8px 12px !important;
    transition: border-color 0.15s;
  }
  .form-control:focus, .selectize-input.focus, .form-select:focus {
    border-color: var(--blue) !important;
    box-shadow: 0 0 0 3px rgba(0,163,224,0.1) !important;
  }
  .selectize-input { min-height: 38px !important; }

  .btn-action {
    font-weight: 600; font-size: 0.82rem;
    padding: 10px 20px; border-radius: 8px;
    border: none; cursor: pointer;
    transition: all 0.15s; width: 100%;
    display: flex; align-items: center;
    justify-content: center; gap: 6px;
  }
  .btn-primary-app { background: var(--blue); color: white; }
  .btn-primary-app:hover { background: #0090c7; }
  .btn-secondary-app { background: var(--slate); color: white; }
  .btn-secondary-app:hover { opacity: 0.9; }
  .btn-danger-app {
    background: transparent; color: var(--red);
    border: 1px solid var(--red);
  }
  .btn-danger-app:hover { background: rgba(239,51,64,0.05); }
  .btn-ghost {
    background: transparent; color: var(--text);
    border: 1px solid var(--border);
  }
  .btn-ghost:hover { background: var(--bg); }

  .status-bar {
    display: flex; align-items: center; gap: 8px;
    padding: 10px 16px; border-radius: 8px;
    font-size: 0.82rem; font-weight: 500;
  }
  .status-bar.info { background: rgba(0,163,224,0.06); color: var(--slate); }
  .status-bar .dot {
    width: 8px; height: 8px; border-radius: 50%;
    background: var(--blue); flex-shrink: 0;
  }

  .batch-counter {
    display: inline-flex; align-items: center; gap: 6px;
    background: rgba(0,163,224,0.08); color: var(--blue);
    font-weight: 700; font-size: 0.88rem;
    padding: 6px 14px; border-radius: 20px;
    margin-bottom: 12px;
  }

  .ReactTable { font-size: 0.82rem; }
  .rt-thead.-header {
    border-bottom: 2px solid var(--slate) !important;
    font-weight: 700; font-size: 0.72rem;
    color: var(--slate); text-transform: uppercase;
    letter-spacing: 0.05em;
  }
  .rt-td {
    border-bottom: 1px solid var(--border) !important;
    padding: 8px 12px !important;
  }
  .rt-tr-group:hover { background: var(--bg) !important; }
  .rt-search {
    border: 1px solid var(--border); border-radius: 6px;
    padding: 8px 12px; font-size: 0.82rem;
  }
  .rt-search:focus {
    outline: none; border-color: var(--blue);
    box-shadow: 0 0 0 3px rgba(0,163,224,0.1);
  }

  .help-text {
    font-size: 0.75rem; color: var(--muted);
    margin-top: 4px; font-style: italic;
  }

  .step-indicator {
    display: flex; align-items: center; gap: 8px;
    margin-bottom: 20px;
  }
  .step {
    display: flex; align-items: center; gap: 6px;
    font-size: 0.75rem; font-weight: 600;
    color: var(--muted);
  }
  .step.active { color: var(--blue); }
  .step .num {
    width: 22px; height: 22px; border-radius: 50%;
    display: flex; align-items: center; justify-content: center;
    font-size: 0.7rem; font-weight: 700;
    background: var(--border); color: var(--muted);
  }
  .step.active .num { background: var(--blue); color: white; }
  .step-line { width: 30px; height: 1px; background: var(--border); }

  .spread-cell {
    font-size: 0.7rem; color: var(--muted);
    font-style: italic;
  }
  .consensus-rank {
    font-weight: 700; color: var(--blue);
  }
')



# --- UI ---
ui <- page_fluid(
  theme = bs_theme(
    bg = m$bg, fg = m$text, primary = m$blue,
    base_font = font_google("DM Sans"), font_scale = 0.95
  ),
  tags$head(tags$style(HTML(app_css))),
  useShinyjs(),
  
  # Header
  div(class = "app-header",
      div(class = "logo-mark", "M"),
      h1("International Scouting Dashboard"),
      div(class = "env-badge", "Marlins Baseball")
  ),
  
  navset_tab(
  
    # ============ TAB 1: REVIEW BOARD (view only) ============
    nav_panel("Review Board",

      div(class = "card-section",
          h4("Review Board"),
          div(class = "help-text", style = "margin-top:-8px; margin-bottom:16px;",
              "Load rankings by scout or country. Select scouts for head-to-head comparison, or a country to see all players scouted there."),
          fluidRow(
            column(2,
              selectInput("review_group", "Signing Class",
                          choices = c("Show All" = "All",
                                      "International Scouting",
                                      "Eligible International Scouting",
                                      "26/27 International Scouting",
                                      "27/28 International Scouting",
                                      "28/29 International Scouting",
                                      "29/30 International Scouting",
                                      "30/31 International Scouting"))
            ),
            column(3,
              selectInput("review_scouts", "Scouts to Compare",
                          choices = c("Select scouts..." = "", scouts),
                          multiple = TRUE, selectize = TRUE)
            ),
            column(2,
              selectInput("review_country", "Country",
                          choices = c("Show All" = "All", countries))
            ),
            column(2,
              selectInput("review_status_filter", "Status Filter",
                          choices = c("Show All" = "All",
                                      "Uncommitted / Free Agent" = "unsigned",
                                      "Committed / Signed" = "signed"))
            ),
            column(3,
              div(style = "padding-top: 24px;",
                  actionButton("load_review", "Load Board",
                               class = "btn-action btn-primary-app",
                               style = "width:100%;"))
            )
          )
      ),

      # Consensus board
      div(class = "card-section",
          h4("Consensus Board"),
          div(class = "help-text", style = "margin-top:-8px; margin-bottom:12px;",
              "Average rank across scouts who evaluated each player. Spread = max rank \u2013 min rank (lower = more agreement)."),
          uiOutput("review_status"),
          br(),
          reactableOutput("consensus_table")
      ),

      # Head-to-head
      div(class = "card-section",
          h4("Head-to-Head Comparison"),
          div(class = "help-text", style = "margin-top:-8px; margin-bottom:12px;",
              "Each scout\u2019s rank side by side. Dash = scout has not evaluated that player."),
          reactableOutput("h2h_table")
      )
    ),

    # ============ TAB 2: RANK PLAYERS (adjust) ============
    nav_panel("Rank Players",

      div(class = "step-indicator",
          div(class = "step active",
              div(class = "num", "1"), "Select Group"),
          div(class = "step-line"),
          div(class = "step",
              div(class = "num", "2"), "Drag to Rank"),
          div(class = "step-line"),
          div(class = "step",
              div(class = "num", "3"), "Lock In")
      ),
      
      div(class = "card-section",
          h4("Signing Class"),
          fluidRow(
            column(3,
              selectInput("rank_group", "Filter by Group",
                          choices = c("Show All" = "All",
                                      "International Scouting",
                                      "Eligible International Scouting",
                                      "26/27 International Scouting",
                                      "27/28 International Scouting",
                                      "28/29 International Scouting",
                                      "29/30 International Scouting",
                                      "30/31 International Scouting"))
            ),
            column(3,
              selectInput("rank_scout", "Scout Name",
                          choices = scouts)
            ),
            column(3,
              selectInput("rank_status_filter", "Status Filter",
                          choices = c("Show All" = "All",
                                      "Uncommitted / Free Agent" = "unsigned",
                                      "Committed / Signed" = "signed"))
            ),
            column(3,
              div(style = "padding-top: 24px;",
                  actionButton("load_rankings", "Load Players",
                               class = "btn-action btn-primary-app",
                               style = "width:100%;"))
            )
          )
      ),
      
      div(class = "card-section",
          h4("Drag Players to Set Rankings"),
          div(class = "help-text", style = "margin-top:-8px; margin-bottom:16px;",
              "Drag players to reorder. #1 = top prospect. Only changed positions are pushed when you lock in."),
          uiOutput("rank_status"),
          br(),
          uiOutput("rank_list_ui")
      ),
      
      div(class = "card-section",
          h4("Submit"),
          fluidRow(
            column(4, actionButton("lock_in", "Lock In Rankings",
                                   class = "btn-action btn-secondary-app",
                                   style = "width:100%;")),
            column(4, actionButton("reset_ranks", "Reset Order",
                                   class = "btn-action btn-danger-app",
                                   style = "width:100%;"))
          ),
          div(class = "help-text", style = "margin-top:12px;",
              "\"Lock In\" pushes only players whose rank changed to Smartabase as new events.")
      ),
      
      div(class = "card-section",
          h4("Current Rankings Preview"),
          reactableOutput("rank_preview_table")
      )
    ),
    
    
    # ============ TAB 3: ADD TO FOLLOW LIST ============
    nav_panel("Add/Update Follow List",
              
      # Step indicator
      div(class = "step-indicator",
          div(class = "step active",
              div(class = "num", "1"), "Select Player"),
          div(class = "step-line"),
          div(class = "step",
              div(class = "num", "2"), "Review / Update"),
          div(class = "step-line"),
          div(class = "step",
              div(class = "num", "3"), "Push to Smartabase")
      ),
      
      # Player selection
    div(class = "card-section",
          h4("Select Player"),
          fluidRow(
            column(4,
              selectInput("group_filter", "Filter by Signing Class",
                          choices = c("Show All" = "All",
                                      "International Scouting",
                                      "Eligible International Scouting",
                                      "26/27 International Scouting",
                                      "27/28 International Scouting",
                                      "28/29 International Scouting",
                                      "29/30 International Scouting",
                                      "30/31 International Scouting")),
              div(class = "help-text",
                  "Narrow the player list by signing class, or leave as \"Show All\" to see all players in the org.")
            ),
            column(8,
              selectizeInput("player", "Player Name", choices = NULL, width = "100%",
                             options = list(placeholder = "Start typing to search...")),
              div(class = "help-text",
                  "Narrow by signing class or search all. Existing evaluations auto-populate when you select a player.")
            )
          ),
          uiOutput("last_eval_info")
      ),
      
      # Evaluation entry
      div(class = "card-section",
          h4("Evaluation Details"),
          fluidRow(
            column(2,
              numericInput("rank", "Rank", value = NA, min = 1, max = 500),
              div(class = "help-text", "1 = highest")
            ),
            column(2, dateInput("date_seen", "Date Last Seen", value = Sys.Date(),
                                format = "mm/dd/yyyy")),
            column(2, uiOutput("country_ui")),
            column(2, selectInput("position", "Future Position", choices = c("Select..." = "", positions))),
            column(2, selectInput("scout", "Scout Name", choices = c("Select..." = "", scouts))),
            column(2, selectInput("status", "Status",
                                  choices = c("Free Agent", "Uncommitted",
                                              "Committed - Marlins",
                                              "Committed - Other", "Signed")))
          ),
          fluidRow(
            column(3, textInput("agent_name", "Agent Name",
                                placeholder = "e.g. John Smith")),
            column(3, uiOutput("signing_bonus_ui"))
          )
      ),
      
      # Batch actions
      div(class = "card-section",
          h4("Review & Submit"),
          uiOutput("batch_status"),
          br(),
          fluidRow(
            column(3, actionButton("add_eval", "Add to Batch",
                        class = "btn-action btn-primary-app")),
            column(3, actionButton("submit", "Push to Smartabase",
                        class = "btn-action btn-secondary-app")),
            column(3, actionButton("clear", "Clear Batch",
                        class = "btn-action btn-danger-app"))
          ),
          div(class = "help-text", style = "margin-top:12px;",
              "Add evaluations to the batch, review below, then push once. New entries and updates both work here."),
          br(),
          reactableOutput("batch_table")
      )
    ),

    # ============ TAB 4: INITIATE SMARTABASE PROFILE ============
    nav_panel("Initiate Smartabase Profile",
              
      # Step indicator
      div(class = "step-indicator",
          div(class = "step active",
              div(class = "num", "1"), "Add Player Info"),
          div(class = "step-line"),
          div(class = "step",
              div(class = "num", "2"), "Assign Groups"),
          div(class = "step-line"),
          div(class = "step",
              div(class = "num", "3"), "Save & Send")
      ),
      
      
    
      
      # Groups
      div(class = "card-section",
          h4("New Player"),
          fluidRow(
            column(3, textInput("first_name", "First Name", placeholder = "e.g. Carlos")),
            column(3, textInput("last_name", "Last Name", placeholder = "e.g. Rodriguez")),
            column(3, dateInput("dob", "Date of Birth", value = NULL,
                                startview = "decade", format = "mm/dd/yyyy")),
            column(3, selectInput("group3", "Signing Class",
                                  choices = c("Select..." = "",
                                              "Eligible International Scouting",
                                              "26/27 International Scouting",
                                              "27/28 International Scouting",
                                              "28/29 International Scouting",
                                              "29/30 International Scouting",
                                              "30/31 International Scouting")))
          )
      ),

      # Actions
      div(class = "card-section",
          h4("Actions"),
          fluidRow(
            column(6, actionButton("add_profile", "Add Player + Send Email",
                class = "btn-action btn-primary-app")),
            column(6, downloadButton("download_lookup", "Download Lookup",
                class = "btn-action btn-ghost"))
          ),
          div(class = "help-text", style = "margin-top:12px;",
              "Adding a player saves them to SQL and sends a notification email with CSV attachment.")
      ),
      
      # Lookup table
      div(class = "card-section",
          h4("Player Directory"),
          reactableOutput("lookup_table")
      )
    )
  )
)

# --- Server ---
server <- function(input, output, session) {

  sb_users <- reactiveVal(tibble(about = character(), user_id = character()))
  event_cache <- reactiveVal(NULL)
  pending_bonus <- reactiveVal(NULL)

  observe({
    cat("SB_URL set:", nzchar(Sys.getenv("SB_URL")), "\n")
    cat("SB_USER set:", nzchar(Sys.getenv("SB_USER")), "\n")
    cat("SB_PASS set:", nzchar(Sys.getenv("SB_PASS")), "\n")
    cat("R_CONFIG_ACTIVE:", Sys.getenv("R_CONFIG_ACTIVE"), "\n")

    one_user <- tryCatch(
      load_sb_users(single_user = TRUE),
      error = function(e) e
    )

    if (inherits(one_user, "error")) {
      showNotification(
        paste("Smartabase login / single-user test failed:", conditionMessage(one_user)),
        type = "error",
        duration = NULL
      )
      return()
    }

    all_users <- tryCatch(
      load_sb_users(single_user = FALSE),
      error = function(e) e
    )

    if (inherits(all_users, "error")) {
      showNotification(
        paste("Single-user test passed, but all-user pull failed:", conditionMessage(all_users)),
        type = "error",
        duration = NULL
      )
      return()
    }

    sb_users(
      all_users %>%
        arrange(about, desc(user_id)) %>%
        distinct(about, .keep_all = TRUE)
    )

   dupes <- all_users %>% group_by(about) %>% filter(n() > 1) %>% ungroup()
    if (nrow(dupes) > 0) {
      showNotification(
        paste0(n_distinct(dupes$about), " duplicate Smartabase profiles detected."),
        type = "warning", duration = 10
      )
      cat("Duplicate SB profiles:\n",
          paste(unique(dupes$about), collapse = "\n"), "\n")
    }
    showNotification("App loaded and ready.", type = "message", duration = 5)
  })


  observeEvent(input$review_scouts, {
    req(length(input$review_scouts) > 0)
    updateSelectInput(session, "review_country", selected = "All")
  }, ignoreInit = TRUE)

  observeEvent(input$review_country, {
    req(input$review_country != "All")
    updateSelectizeInput(session, "review_scouts", selected = character(0))
  }, ignoreInit = TRUE)

observe({
    req(nrow(sb_users()) > 0)
    if (!is.null(event_cache())) return()

    events <- tryCatch({
      sb_get_event(
        form       = "International Follow Rank_draft",
        date_range = c("01/01/2025", format(Sys.Date() + 1, "%d/%m/%Y")),
        url        = Sys.getenv("SB_URL"),
        username   = Sys.getenv("SB_USER"),
        password   = Sys.getenv("SB_PASS"),
        option     = sb_get_event_option(interactive_mode = FALSE)
      )
    }, error = function(e) NULL)

    event_cache(events)
  })

  refresh_cache <- function() { event_cache(NULL) }

  
  # ---- Lookup ----
  lookup <- reactiveVal(get_lookup_sql_for_app())

  # ---- Conditional Signing Bonus (Add to Follow List) ----
  output$signing_bonus_ui <- renderUI({
    req(input$status)
    if (input$status %in% c("Committed - Marlins", "Committed - Other", "Signed")) {
      numericInput("signing_bonus", "Signing Bonus ($)", value = NA, min = 0)
    }
  })

  output$country_ui <- renderUI({
    events <- event_cache()
    has_prior <- FALSE

    if (!is.null(input$player) && nzchar(input$player) &&
        !is.null(events) && nrow(events) > 0) {
      has_prior <- any(events$about == input$player)
    }

   if (has_prior) {
      tagList(
        selectInput("country", "Country", choices = c("Select..." = "", countries)),
        tags$script(HTML("$('#country').prop('disabled', true);")),
        div(class = "help-text", "Locked from prior entry")
      )
    } else {
      selectInput("country", "Country", choices = c("Select..." = "", countries))
    }
  })

  # ---- Review Board (Tab 1) ----
  review_data <- reactiveVal(NULL)

  observeEvent(input$load_review, {
    has_scouts  <- length(input$review_scouts) >= 1
    has_country <- input$review_country != "All"

    raw <- event_cache()
    req(!is.null(raw), nrow(raw) > 0)

    df <- raw %>%
      select(any_of(c("about", "Rank", "Date Last Seen", "Country", "Future Position",
                       "Scout Name", "Age", "UUID", "Date of Birth", "Status",
                       "Agent Name", "Signing Bonus", "event_id")))

    if (has_scouts) {
      df <- df %>% filter(`Scout Name` %in% input$review_scouts)
    }

    if (has_country) {
      df <- df %>% filter(Country == input$review_country)
    }

    df <- df %>%
      arrange(desc(event_id)) %>%
      distinct(about, `Scout Name`, .keep_all = TRUE) %>%
      select(-event_id)

    df <- ensure_cols(df, c("Status", "Agent Name", "Signing Bonus"))

    # df <- enrich_age(df, lookup())
    df <- enrich_age(df)

    if (is.numeric(df$`Date Last Seen`)) {
      df <- df %>%
        mutate(`Date Last Seen` = format(
          as.Date(as.POSIXct(`Date Last Seen` / 1000, origin = "1970-01-01")),
          "%m/%d/%Y"
        ))
    }

    if (input$review_group != "All") {
      group_players <- lookup() %>%
        filter(if_any(c(`Groups as Athlete`, `Secondary Group`,
                        `Tertiary Group`, `Fourth Group`),
                      ~ . == input$review_group)) %>%
        mutate(about = paste(`First Name`, `Last Name`))

      df <- df %>% filter(about %in% group_players$about)
    }

    if (nrow(df) == 0) {
      showNotification("No rankings found for the selected filters.", type = "warning")
    }

    review_data(df)
  })

  # Status filter helper

  apply_status_filter <- function(df, filter_val) {
    if (filter_val == "All") return(df)
    unsigned <- c("Free Agent", "Uncommitted")
    signed   <- c("Committed - Marlins", "Committed - Other", "Signed")
    if (filter_val == "unsigned") {
      df %>% filter(is.na(Status) | Status %in% unsigned)
    } else {
      df %>% filter(Status %in% signed)
    }
  }

  filtered_review <- reactive({
    df <- review_data()
    req(!is.null(df), nrow(df) > 0)
    apply_status_filter(df, input$review_status_filter)
  })

  filtered_rank <- reactive({
    df <- rank_players()
    req(nrow(df) > 0)
    apply_status_filter(df, input$rank_status_filter)
  })

  output$review_status <- renderUI({
    df <- review_data()
    if (is.null(df) || nrow(df) == 0) {
      div(class = "status-bar info",
          div(class = "dot", style = paste0("background:", m$muted)),
          "Select scouts and click \"Load Board\" to view rankings.")
    } else {
      filt <- apply_status_filter(df, input$review_status_filter)
      n_players <- n_distinct(filt$about)
      n_scouts  <- n_distinct(filt$`Scout Name`)
      div(class = "batch-counter",
          paste0(n_players, " players across ", n_scouts, " scout(s)"))
    }
  })

  # Consensus table: mean rank, spread, # scouts who ranked
  output$consensus_table <- renderReactable({
    df <- filtered_review()
    req(nrow(df) > 0)

    consensus <- df %>%
      group_by(about, Country, `Future Position`) %>%
      summarise(
        Age            = first(na.omit(Age)),
        `Last Seen`    = max(`Date Last Seen`, na.rm = TRUE),
        `Mean Rank`    = round(mean(Rank, na.rm = TRUE), 1),
        `Min Rank`     = min(Rank, na.rm = TRUE),
        `Max Rank`     = max(Rank, na.rm = TRUE),
        Spread         = max(Rank, na.rm = TRUE) - min(Rank, na.rm = TRUE),
        `# Scouts`     = n_distinct(`Scout Name`),
        Status         = first(na.omit(Status)),
        `Agent Name`   = first(na.omit(`Agent Name`)),
        `Signing Bonus` = first(na.omit(`Signing Bonus`)),
        .groups = "drop"
      ) %>%
      arrange(`Mean Rank`)

    reactable(
      consensus,
      compact = TRUE, highlight = TRUE, searchable = TRUE,
      defaultPageSize = 50,
      columns = list(
        about            = colDef(name = "Player", minWidth = 160,
                                  style = list(fontWeight = 600)),
        `Future Position` = colDef(name = "Pos", minWidth = 55),
        `Last Seen`       = colDef(minWidth = 95),
        Country          = colDef(minWidth = 110),
        Age = colDef(minWidth = 55,
  cell = function(value) {
    if (is.na(value)) htmltools::span(style = "color:#D4790E; font-weight:600;", "No DOB")
    else value
  }),
        #UUID             = colDef(minWidth = 85),
        `Mean Rank`      = colDef(minWidth = 80,
          style = function(value) list(fontWeight = 700, color = m$blue)),
        `Min Rank`       = colDef(name = "Best", minWidth = 55),
        `Max Rank`       = colDef(name = "Worst", minWidth = 55),
        Spread           = colDef(minWidth = 65,
          style = function(value) {
            col <- if (value <= 3) m$success
                   else if (value <= 10) m$warn
                   else m$red
            list(fontWeight = 600, color = col)
          }),
        `# Scouts`       = colDef(minWidth = 70),
        Status           = colDef(minWidth = 120,
          cell = function(value) if (!is.na(value) && value == "Uncommitted") "" else value),
        `Agent Name`     = colDef(name = "Agent", minWidth = 110),
        `Signing Bonus`  = colDef(name = "Bonus", minWidth = 90,
          cell = function(value) {
            if (is.na(value)) "\u2014" else paste0("$", formatC(value, format = "f", digits = 0, big.mark = ","))
          })
      ),
      defaultColDef = colDef(
        headerStyle = list(fontWeight = 700, fontSize = "0.72rem",
                           textTransform = "uppercase", letterSpacing = "0.05em",
                           color = m$slate),
        style = list(fontSize = "0.82rem")
      )
    )
  })

  # Head-to-head: wide table with one column per scout
  output$h2h_table <- renderReactable({
    df <- filtered_review()
    req(nrow(df) > 0)

    # Player metadata (take first non-NA per player)
    meta <- df %>%
      distinct(about, .keep_all = TRUE) %>%
      select(about, Country, `Future Position`, Age, `Date Last Seen`,
             Status, `Agent Name`, `Signing Bonus`)

    # Pivot scout ranks wide
    wide <- df %>%
      select(about, `Scout Name`, Rank) %>%
      pivot_wider(names_from = `Scout Name`, values_from = Rank)

    h2h <- meta %>%
      left_join(wide, by = "about") %>%
      arrange(about)

    # Build column defs: fixed cols + one per scout
    scout_names <- setdiff(names(h2h), names(meta))

    scout_cols <- setNames(
      lapply(scout_names, function(s) {
        colDef(
          name = s, minWidth = 75, align = "center",
          style = function(value) {
            if (is.na(value)) return(list(color = m$border))
            list(fontWeight = 700, color = m$blue)
          },
          na = "\u2014"
        )
      }),
      scout_names
    )

    fixed_cols <- list(
      about             = colDef(name = "Player", minWidth = 160,
                                 style = list(fontWeight = 600)),
      `Future Position`  = colDef(name = "Pos", minWidth = 55),
      `Date Last Seen`   = colDef(name = "Last Seen", minWidth = 95),
      Country           = colDef(minWidth = 110),
      Age = colDef(minWidth = 55,
        cell = function(value) {
          if (is.na(value)) htmltools::span(style = "color:#D4790E; font-weight:600;", "No DOB")
          else value
        }),
      #UUID              = colDef(minWidth = 85),
      Status            = colDef(minWidth = 120,
        cell = function(value) if (!is.na(value) && value == "Uncommitted") "" else value),
      `Agent Name`      = colDef(name = "Agent", minWidth = 110),
      `Signing Bonus`   = colDef(name = "Bonus", minWidth = 90,
        cell = function(value) {
          if (is.na(value)) "\u2014" else paste0("$", formatC(value, format = "f", digits = 0, big.mark = ","))
        })
    )

    reactable(
      h2h,
      compact = TRUE, highlight = TRUE, searchable = TRUE,
      defaultPageSize = 50,
      columns = c(fixed_cols, scout_cols),
      defaultColDef = colDef(
        headerStyle = list(fontWeight = 700, fontSize = "0.72rem",
                           textTransform = "uppercase", letterSpacing = "0.05em",
                           color = m$slate),
        style = list(fontSize = "0.82rem")
      )
    )
  })

  # ---- Tab 2: Rank Players ----
  rank_players <- reactiveVal(tibble(
    about = character(), UUID = character(),
    Country = character(), `Future Position` = character()
  ))
  
  observeEvent(input$load_rankings, {
    
   
    fresh_rankings <- event_cache()
    req(!is.null(fresh_rankings), nrow(fresh_rankings) > 0)
    
    ranked <- fresh_rankings %>%
      filter(`Scout Name` == input$rank_scout) %>%
      select(any_of(c("about", "user_id", "Rank", "Date Last Seen", "Country",
                       "Future Position", "Scout Name", "Age", "UUID", "Date of Birth",
                       "Status", "Agent Name", "Signing Bonus", "event_id"))) %>%
      arrange(desc(event_id)) %>%
      distinct(about, .keep_all = TRUE) %>%
      select(-event_id)

    ranked <- ensure_cols(ranked, c("Status", "Agent Name", "Signing Bonus"))

    # ranked <- enrich_age(ranked, lookup())
    ranked <- enrich_age(ranked)
    
    if (is.numeric(ranked$`Date Last Seen`)) {
      ranked <- ranked %>%
        mutate(`Date Last Seen` = format(
          as.Date(as.POSIXct(`Date Last Seen` / 1000, origin = "1970-01-01")),
          "%m/%d/%Y"
        ))
    }
    
    if (input$rank_group != "All") {
      group_players <- lookup() %>%
        filter(if_any(c(`Groups as Athlete`, `Secondary Group`,
                        `Tertiary Group`, `Fourth Group`),
                      ~ . == input$rank_group)) %>%
        mutate(about = paste(`First Name`, `Last Name`))
      
      ranked <- ranked %>%
        filter(about %in% group_players$about)
    }
    
    if (nrow(ranked) == 0) {
      showNotification(
        paste0("No rankings found for ", input$rank_scout, " in this group."),
        type = "warning"
      )
    }
    
    rank_players(ranked %>% arrange(Rank))
  })
  
  output$rank_status <- renderUI({
    rp <- rank_players()
    n <- if (nrow(rp) > 0) nrow(apply_status_filter(rp, input$rank_status_filter)) else 0
    if (n == 0) {
      div(class = "status-bar info",
          div(class = "dot", style = paste0("background:", m$muted)),
          "Select a group and click \"Load Players\" to begin ranking.")
    } else {
      div(class = "batch-counter",
          paste0(n, " players loaded — drag to rank"))
    }
  })
  
  output$rank_list_ui <- renderUI({
    df <- filtered_rank()
    req(nrow(df) > 0)
    
    labels <- setNames(
      lapply(seq_len(nrow(df)), function(i) {
        bonus_txt <- if (!is.na(df$`Signing Bonus`[i])) {
          paste0(" | $", formatC(df$`Signing Bonus`[i], format = "f", digits = 0, big.mark = ","))
        } else ""
        agent_txt <- if (!is.na(df$`Agent Name`[i]) && nzchar(df$`Agent Name`[i])) {
          df$`Agent Name`[i]
        } else ""
        status_txt <- if (!is.na(df$Status[i]) && df$Status[i] != "Uncommitted") df$Status[i] else ""

        HTML(paste0(
          '<div style="display:flex; align-items:center; gap:12px; width:100%;">',
            '<span class="rank-grip">&#9776;</span>',
            '<span class="rank-num">', i, '</span>',
            '<span class="rank-player-name">', df$about[i], '</span>',
            '<span style="font-size:0.75rem; color:#41748D; font-weight:600;">',
              df$`Future Position`[i], '</span>',
            '<span style="font-size:0.75rem; color:#8C939D;">',
              df$Country[i], '</span>',
            '<span style="font-size:0.75rem; color:#8C939D;">',
              'Age: ', if (is.na(df$Age[i])) '<span style="color:#D4790E;font-weight:600;">No DOB</span>' else df$Age[i], '</span>',
            '<span style="font-size:0.72rem; color:#D4790E; font-weight:600;">',
              status_txt, bonus_txt, '</span>',
            if (nzchar(agent_txt)) paste0(
              '<span style="font-size:0.72rem; color:#8C939D; font-style:italic;">',
                agent_txt, '</span>'
            ) else "",
            '<span class="rank-player-meta">', df$UUID[i], '</span>',
          '</div>'
        ))
      }),
      df$about
    )
    
    div(class = "rank-list-container",
        rank_list(
          text = NULL,
          labels = labels,
          input_id = "player_ranking",
          options = sortable_options(
            animation = 150,
            ghostClass = "sortable-ghost"
          )
        )
    )
  })
  
  # Preview table showing current order
  output$rank_preview_table <- renderReactable({
    df <- filtered_rank()
    req(nrow(df) > 0)
    
    if (!is.null(input$player_ranking)) {
      ordered_names <- input$player_ranking
      
      df <- tibble(about = ordered_names) %>%
        left_join(filtered_rank() %>% select(-Rank), by = "about") %>%
        mutate(Rank = row_number())
    } else {
      df <- df %>% mutate(Rank = row_number())
    }
    
    reactable(
      df %>% select(Rank, about, `Future Position`, Country, Age,
                    `Scout Name`, `Date Last Seen`, UUID,
                    Status, `Agent Name`, `Signing Bonus`),
      compact = TRUE, highlight = TRUE, searchable = TRUE,
      defaultPageSize = 50,
      columns = list(
        Rank              = colDef(minWidth = 60,
          style = function(value) list(fontWeight = 700, color = m$blue)),
        about             = colDef(name = "Player", minWidth = 160),
        `Future Position` = colDef(name = "Pos", minWidth = 60),
        Country           = colDef(minWidth = 120),
        Age = colDef(minWidth = 55,
          cell = function(value) {
            if (is.na(value)) htmltools::span(style = "color:#D4790E; font-weight:600;", "No DOB")
            else value
          }),
        `Scout Name`      = colDef(name = "Scout", minWidth = 90),
        `Date Last Seen`  = colDef(name = "Last Seen", minWidth = 100),
        UUID              = colDef(minWidth = 90),
        Status            = colDef(minWidth = 120,
          cell = function(value) if (!is.na(value) && value == "Uncommitted") "" else value),
        `Agent Name`      = colDef(name = "Agent", minWidth = 110),
        `Signing Bonus`   = colDef(name = "Bonus", minWidth = 90,
          cell = function(value) {
            if (is.na(value)) "\u2014" else paste0("$", formatC(value, format = "f", digits = 0, big.mark = ","))
          })
      ),
      defaultColDef = colDef(
        headerStyle = list(fontWeight = 700, fontSize = "0.72rem",
                           textTransform = "uppercase", letterSpacing = "0.05em",
                           color = m$slate),
        style = list(fontSize = "0.82rem")
      )
    )
  })
  
  # Reset order
  observeEvent(input$reset_ranks, {
    df <- rank_players()
    rank_players(df %>% arrange(about))
  })
  
  # Lock in rankings — push to Smartabase
  observeEvent(input$lock_in, {
    req(!is.null(input$player_ranking))
    
    showModal(modalDialog(
      title = "Lock In Rankings?",
      paste0("Push ", length(input$player_ranking), 
             " player rankings to Smartabase as ", input$rank_scout, "?"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_lock_in", "Yes, Lock In",
                     class = "btn-action btn-secondary-app")
      )
    ))
  })
  
  observeEvent(input$confirm_lock_in, {
    removeModal()

    ordered_names <- input$player_ranking
    new_ranks <- tibble(about = ordered_names, Rank = seq_along(ordered_names))

    old_ranks <- filtered_rank() %>%
      mutate(old_rank = row_number()) %>%
      select(about, old_rank)

    changed <- new_ranks %>%
      left_join(old_ranks, by = "about") %>%
      filter(Rank != old_rank | is.na(old_rank))

    if (nrow(changed) == 0) {
      showNotification("No rank changes detected.", type = "message")
      return()
    }

    push_df <- changed %>%
      select(about, Rank) %>%
      left_join(rank_players() %>% select(-Rank), by = "about") %>%
      mutate(
        `Date Last Seen` = format(Sys.Date(), "%d/%m/%Y"),
        `Scout Name`     = input$rank_scout
      )

    if (any(is.na(push_df$user_id))) {
      missing <- push_df %>% filter(is.na(user_id)) %>% pull(about)
      showNotification(
        paste("Not in Smartabase:", paste(missing, collapse = ", ")),
        type = "error", duration = 15
      )
      return()
    }

    tryCatch({
      sb_insert_event(
        df   = push_df %>% select(about, user_id, Rank, `Date Last Seen`,
                                   Country, `Future Position`, `Scout Name`, UUID,
                                   Status, `Agent Name`, `Signing Bonus`),
        form = "International Follow Rank_draft",
        url      = Sys.getenv("SB_URL"),
        username = Sys.getenv("SB_USER"),
        password = Sys.getenv("SB_PASS"),
        option   = sb_insert_event_option(interactive_mode = FALSE)
      )
      showNotification(
        paste0("Locked in! ", nrow(push_df), " of ", length(ordered_names),
               " rankings updated (only changed ranks pushed)."),
        type = "message", duration = 8
      )
      refresh_cache()
    }, error = function(e) {
      showNotification(paste("Push failed:", e$message), type = "error", duration = 10)
    })
  })
  
  # ---- Tab 4: Create Profile ----
  observeEvent(input$add_profile, {
  req(input$first_name, input$last_name)

  existing <- lookup()

  dup <- existing %>%
    filter(
      tolower(`First Name`) == tolower(trimws(input$first_name)),
      tolower(`Last Name`) == tolower(trimws(input$last_name))
    )

  if (nrow(dup) > 0) {
    showNotification(
      paste0(
        input$first_name, " ", input$last_name,
        " already exists (UUID: ", dup$UUID[1], ")"
      ),
      type = "warning"
    )
    return()
  }

  uname <- paste0(
    tolower(trimws(input$first_name)), ".",
    tolower(trimws(input$last_name))
  )

  new_row <- tibble(
    `First Name`        = trimws(input$first_name),
    `Last Name`         = trimws(input$last_name),
    `Date of Birth`     = if (is.null(input$dob) || is.na(input$dob)) as.Date(NA) else as.Date(input$dob),
    UUID                = NA_character_,
    Username            = uname,
    `Email Address`     = paste0(uname, "@smartabase.com"),
    Password            = paste0(tools::toTitleCase(trimws(input$first_name)), "12345"),
    Address             = NA_character_,
    `Suburb/City`       = NA_character_,
    State               = NA_character_,
    Country             = NA_character_,
    `Postcode/ZIP`      = NA_character_,
    `Phone Number`      = NA_character_,
    `Groups as Athlete` = "All Amateur Scouting Prospects",
    `Secondary Group`   = "International Scouting",
    `Tertiary Group`    = input$group3,
    `Fourth Group`      = NA_character_
  )

  new_uuid <- tryCatch({
    insert_lookup_sql(new_row)
  }, error = function(e) {
    showNotification(
      paste("Failed to add player:", e$message),
      type = "error",
      duration = 10
    )
    return(NULL)
  })

  req(!is.null(new_uuid))

  lookup(get_lookup_sql_for_app())

  inserted_row <- get_lookup_sql() %>%
    filter(UUID == new_uuid)

  email_ok <- tryCatch({
    send_profile_email(inserted_row)
    TRUE
  }, error = function(e) {
    showNotification(
      paste("Player added, but email failed:", e$message),
      type = "warning",
      duration = 10
    )
    FALSE
  })

  updateTextInput(session, "first_name", value = "")
  updateTextInput(session, "last_name", value = "")
  updateDateInput(session, "dob", value = NULL)
  updateSelectInput(session, "group3", selected = "")
 

  if (email_ok) {
    showNotification(
      paste0(
        "Added ", inserted_row$`First Name`, " ", inserted_row$`Last Name`,
        " and sent email — UUID: ", new_uuid
      ),
      type = "message"
    )
  } else {
    showNotification(
      paste0(
        "Added ", inserted_row$`First Name`, " ", inserted_row$`Last Name`,
        " — UUID: ", new_uuid
      ),
      type = "message"
    )
  }
})
  
  
  
  output$lookup_table <- renderReactable({
    df <- lookup()
    if (nrow(df) == 0) return(reactable(tibble(Message = "No players yet. Add one above.")))
    
    reactable(
      df %>% select(`First Name`, `Last Name`, UUID,
                    `Groups as Athlete`, `Secondary Group`, `Tertiary Group`),
      searchable = TRUE, compact = TRUE, highlight = TRUE,
      defaultPageSize = 15,
      columns = list(
        `First Name`       = colDef(name = "First", minWidth = 100),
        `Last Name`        = colDef(name = "Last", minWidth = 100),
        UUID               = colDef(minWidth = 90),
        `Groups as Athlete` = colDef(name = "Primary Group", minWidth = 140),
        `Secondary Group`  = colDef(name = "Secondary", minWidth = 140),
        `Tertiary Group`   = colDef(name = "Tertiary", minWidth = 140)
      ),
      defaultColDef = colDef(
        headerStyle = list(fontWeight = 700, fontSize = "0.72rem",
                           textTransform = "uppercase", letterSpacing = "0.05em",
                           color = m$slate),
        style = list(fontSize = "0.82rem")
      )
    )
  })
  
  # Download
output$download_lookup <- downloadHandler(
  filename = function() paste0("uuid_lookup_table_", Sys.Date(), ".xlsx"),
  content  = function(file) write_xlsx(lookup(), file)
)
  
  
  
  # ---- Tab 3: Scout Rankings ----
  
 filtered_players <- reactive({
    req(nrow(sb_users()) > 0)
    lookup_df <- lookup() %>%
      filter(
        `Groups as Athlete` == "All Amateur Scouting Prospects",
        `Secondary Group` == "International Scouting"
      )

    sb_df <- sb_users() %>%
      distinct(about, user_id) %>%
      arrange(about)

  # If no group filter, return all Smartabase users
  if (input$group_filter == "All") {
    lookup_uuids <- lookup_df %>%
      mutate(about = paste(`First Name`, `Last Name`)) %>%
      distinct(about, .keep_all = TRUE) %>%
      select(about, UUID)

    return(
      sb_df %>%
        left_join(lookup_uuids, by = "about") %>%
        arrange(about)
    )
  }

  # Use lookup only to identify who belongs to the selected group
 lookup_grp <- lookup_df %>%
    filter(
      if_any(
        c(`Groups as Athlete`, `Secondary Group`, `Tertiary Group`, `Fourth Group`),
        ~ . == input$group_filter
      )
    ) %>%
    mutate(about = paste(`First Name`, `Last Name`)) %>%
    select(about, UUID)

   sb_df %>%
    inner_join(lookup_grp, by = "about") %>%
    arrange(about)
  })
  
  observeEvent(filtered_players(), {
  fp <- filtered_players()
  updateSelectizeInput(
    session,
    "player",
    choices = setNames(fp$about, fp$about),
    server = TRUE
  )
})
  
  # Batch
  batch <- reactiveVal(tibble(
    about = character(), UUID = character(), Rank = numeric(),
    `Date Last Seen` = character(), Country = character(),
    `Future Position` = character(), `Scout Name` = character(),
    Status = character(), `Agent Name` = character(),
    `Signing Bonus` = numeric()
  ))

 
  
  output$batch_status <- renderUI({
    n <- nrow(batch())
    if (n == 0) {
      div(class = "status-bar info",
          div(class = "dot", style = paste0("background:", m$muted)),
          "No evaluations queued. Add players below.")
    } else {
      div(class = "batch-counter",
          paste0(n, " evaluation", ifelse(n > 1, "s", ""), " ready to push"))
    }
  })

  observeEvent(input$player, {
    req(input$player, nzchar(input$player))
    events <- event_cache()

    # Default reset
    reset_form <- function() {
      updateNumericInput(session, "rank", value = NA)
      updateDateInput(session, "date_seen", value = Sys.Date())
      updateSelectInput(session, "country", selected = "")
      updateSelectInput(session, "position", selected = "")
      updateSelectInput(session, "scout", selected = "")
      updateSelectInput(session, "status", selected = "Free Agent")
      updateTextInput(session, "agent_name", value = "")
      pending_bonus(NULL)
    }

    if (is.null(events) || nrow(events) == 0) { reset_form(); return() }

    latest <- events %>%
      filter(about == input$player) %>%
      arrange(desc(event_id)) %>%
      slice(1)

    if (nrow(latest) == 0) { reset_form(); return() }

    # Parse date
    date_val <- tryCatch({
      if (is.numeric(latest$`Date Last Seen`)) {
        as.Date(as.POSIXct(latest$`Date Last Seen` / 1000, origin = "1970-01-01"))
      } else {
        as.Date(as.character(latest$`Date Last Seen`), format = "%d/%m/%Y")
      }
    }, error = function(e) Sys.Date())

    status_val <- if (!is.na(latest$Status)) latest$Status else "Free Agent"
    agent_val  <- if (!is.na(latest$`Agent Name`)) latest$`Agent Name` else ""

    updateNumericInput(session, "rank", value = latest$Rank)
    updateDateInput(session, "date_seen", value = date_val)
    updateSelectInput(session, "country", selected = latest$Country)
    updateSelectInput(session, "position", selected = latest$`Future Position`)
    updateSelectInput(session, "scout", selected = latest$`Scout Name`)
    updateSelectInput(session, "status", selected = status_val)
    updateTextInput(session, "agent_name", value = agent_val)

    # Queue bonus update (conditional UI may not exist yet)
    if (status_val %in% c("Committed - Marlins", "Committed - Other", "Signed")) {
      pending_bonus(if (!is.na(latest$`Signing Bonus`)) latest$`Signing Bonus` else NA)
    } else {
      pending_bonus(NULL)
    }
  }, ignoreInit = TRUE)

  observe({
    bonus <- pending_bonus()
    req(!is.null(bonus), !is.null(input$signing_bonus))
    updateNumericInput(session, "signing_bonus", value = bonus)
    pending_bonus(NULL)
  })
  
  observeEvent(input$add_eval, {
    req(input$player, input$rank)
    
    # Prevent double-click
    shinyjs::disable("add_eval")
    on.exit(shinyjs::enable("add_eval"), add = TRUE)
    
    # Duplicate check
    if (input$player %in% batch()$about) {
      showNotification(
        paste0(input$player, " is already in the batch."),
        type = "warning"
      )
      return()
    }
    
    uuid <- filtered_players() %>%
      filter(about == input$player) %>%
      pull(UUID)
    
    new_row <- tibble(
      about             = input$player,
      UUID              = uuid,
      Rank              = input$rank,
      `Date Last Seen`  = format(input$date_seen, "%d/%m/%Y"),
      Country           = input$country,
      `Future Position` = input$position,
      `Scout Name`      = input$scout,
      Status            = input$status,
      `Agent Name`      = ifelse(nzchar(input$agent_name), input$agent_name, NA_character_),
      `Signing Bonus`   = if (input$status %in% c("Committed - Marlins", "Committed - Other", "Signed") &&
                               !is.null(input$signing_bonus) && !is.na(input$signing_bonus)) {
                            input$signing_bonus
                          } else {
                            NA_real_
                          }
    )
    
    batch(bind_rows(batch(), new_row))
    updateNumericInput(session, "rank", value = NA)
    updateSelectizeInput(session, "player", selected = "")
    updateTextInput(session, "agent_name", value = "")
    updateSelectInput(session, "status", selected = "Free Agent")
    showNotification(paste0("Added ", input$player, " (Rank ", input$rank, ")"),
                     type = "message", duration = 3)
  })
  
  observeEvent(input$clear, { batch(batch()[0, ]) })
  
  output$batch_table <- renderReactable({
    df <- batch()
    if (nrow(df) == 0) return(reactable(tibble(Message = "Batch is empty.")))
    
    reactable(df, compact = TRUE, highlight = TRUE,
              defaultPageSize = 25,
              columns = list(
                about            = colDef(name = "Player", minWidth = 160),
                UUID             = colDef(minWidth = 90),
                Rank             = colDef(minWidth = 60,
                  style = function(value) list(fontWeight = 700, color = m$blue)),
                `Date Last Seen` = colDef(name = "Last Seen", minWidth = 100),
                Country          = colDef(minWidth = 120),
                `Future Position` = colDef(name = "Position", minWidth = 80),
                `Scout Name`     = colDef(name = "Scout", minWidth = 90),
                Status           = colDef(minWidth = 120,
                  cell = function(value) if (!is.na(value) && value == "Uncommitted") "" else value),
                `Agent Name`     = colDef(name = "Agent", minWidth = 110),
                `Signing Bonus`  = colDef(name = "Bonus", minWidth = 90,
                  cell = function(value) {
                    if (is.na(value)) "\u2014" else paste0("$", formatC(value, format = "f", digits = 0, big.mark = ","))
                  })
              ),
              defaultColDef = colDef(
                headerStyle = list(fontWeight = 700, fontSize = "0.72rem",
                                   textTransform = "uppercase", letterSpacing = "0.05em",
                                   color = m$slate),
                style = list(fontSize = "0.82rem")
              ))
  })
  
  # Push to Smartabase
  observeEvent(input$submit, {
    req(nrow(batch()) > 0)
    
    # Confirmation modal
    showModal(modalDialog(
      title = "Push to Smartabase?",
      paste0("You are about to insert ", nrow(batch()),
             " evaluation(s) into Smartabase. This cannot be undone."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_push", "Yes, Push",
                     class = "btn-action btn-secondary-app")
      )
    ))
  })
  
  observeEvent(input$confirm_push, {
    removeModal()
    
    push_data <- batch() %>%
  left_join(sb_users(), by = "about")
    
    if (any(is.na(push_data$user_id))) {
      missing <- push_data %>% filter(is.na(user_id)) %>% pull(about)
      showNotification(
        paste("These players aren't in Smartabase yet:", paste(missing, collapse = ", ")),
        type = "error", duration = 15
      )
      return()
    }
    
    tryCatch({
      sb_insert_event(
        df   = push_data,
        form = "International Follow Rank_draft",
        url      = Sys.getenv("SB_URL"),
        username = Sys.getenv("SB_USER"),
        password = Sys.getenv("SB_PASS"),
        option   = sb_insert_event_option(interactive_mode = FALSE)
      )
      showNotification(
        paste0("Success! ", nrow(push_data), " evaluation(s) pushed to Smartabase."),
        type = "message", duration = 8
      )
      batch(batch()[0, ])
      refresh_cache()
    }, error = function(e) {
      showNotification(paste("Push failed:", e$message), type = "error", duration = 10)
    })
  })

  output$last_eval_info <- renderUI({
    req(input$player, nzchar(input$player))
    events <- event_cache()
    if (is.null(events) || nrow(events) == 0) return(NULL)

    latest <- events %>%
      filter(about == input$player) %>%
      arrange(desc(event_id)) %>%
      slice(1)

    if (nrow(latest) == 0) {
      return(div(class = "status-bar", style = paste0(
        "margin-top:10px; background:rgba(212,121,14,0.08); color:", m$warn, ";"),
        div(class = "dot", style = paste0("background:", m$warn)),
        "No previous reports found for this player."))
    }

    date_display <- tryCatch({
      if (is.numeric(latest$`Date Last Seen`)) {
        format(as.Date(as.POSIXct(latest$`Date Last Seen` / 1000, origin = "1970-01-01")), "%m/%d/%Y")
      } else {
        d <- as.Date(as.character(latest$`Date Last Seen`), format = "%d/%m/%Y")
        format(d, "%m/%d/%Y")
      }
    }, error = function(e) "Unknown")

    div(class = "status-bar info", style = "margin-top:10px;",
        div(class = "dot"),
        paste0("Last evaluated by ", latest$`Scout Name`,
               " on ", date_display,
               " — Rank: ", latest$Rank,
               if (!is.na(latest$Status) && latest$Status != "Free Agent")
                 paste0(" — ", latest$Status) else ""))
  })

}

shinyApp(ui, server)

