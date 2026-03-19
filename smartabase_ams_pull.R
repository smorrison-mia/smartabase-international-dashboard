library(smartabaseR)
library(tidyverse)
library(data.table)

Sys.getenv("SB_USER")


fd_data <- sb_get_event(
  form = "Forcedecks Trials",
  date_range = c("01/01/2026", "02/03/2026"),
  url = Sys.getenv("SB_URL"),
  username = Sys.getenv("SB_USER"),
  password = Sys.getenv("SB_PASS")
)

vald_data <- sb_get_event(
  form = "VALD Performance Test",
  date_range = c("01/01/2026", "02/03/2026"),
  url = Sys.getenv("SB_URL"),
  username = Sys.getenv("SB_USER"),
  password = Sys.getenv("SB_PASS")
)

watt_bike_data <- sb_get_event(
  form = "Watt Bike",
  date_range = c("01/01/2025", "02/03/2026"),
  url = Sys.getenv("SB_URL"),
  username = Sys.getenv("SB_USER"),
  password = Sys.getenv("SB_PASS")
)


statsports_data <- sb_get_event(
  form = "STATSports SONRA",
  date_range = c("01/01/2026", "02/03/2026"),
  url = Sys.getenv("SB_URL"),
  username = Sys.getenv("SB_USER"),
  password = Sys.getenv("SB_PASS")
)





# lets take the max from each test in VALD



fd_clean <- janitor::clean_names(fd_data) %>%
  arrange(user_id, start_date, trial) %>%
  group_by(user_id, start_date) %>%
  tidyr::fill(test_type, .direction = "down") %>%
  ungroup()

dt <- as.data.table(fd_clean)
num_cols <- setdiff(names(dt)[sapply(dt, is.numeric)], c("trial", "user_id"))

fd_data_max <- dt[, lapply(.SD, max, na.rm = TRUE),
                  by = .(user_id, about, test_type, start_date),
                  .SDcols = num_cols]


# get max velo with smart speed

ss_max_speed <- statsports_data %>%
  janitor::clean_names() %>%
  group_by(user_id, about) %>%
  summarise(max_speed = max(max_speed, na.rm = TRUE), .groups = "drop")


wb_recent <- watt_bike_data %>%
  janitor::clean_names() %>%
  mutate(start_date_parsed = dmy(start_date)) %>%
  group_by(user_id, about) %>%
  slice_max(start_date_parsed, n = 1) %>%
  ungroup() %>%
  select(user_id, about, full_name, uuid, start_date, distance)



# # vald_max <- vald_data %>% 
# #   janitor::clean_names() %>% 
# #   arrange(user_id, start_date) %>%
# #   group_by(user_id, start_date) %>%
# #   tidyr::fill(test_type, .direction = "down") %>%
# #   ungroup()



# dt <- as.data.table(vald_max)
# num_cols <- setdiff(names(dt)[sapply(dt, is.numeric)], "user_id")

# vald_data_max <- dt[, lapply(.SD, max, na.rm = TRUE),
#                     by = .(user_id, about, test_type, start_date),
#                     .SDcols = num_cols]

nordic_max <- vald_data %>%
  janitor::clean_names() %>%
  arrange(user_id, start_date) %>%
  group_by(user_id, start_date) %>%
  tidyr::fill(test_type, .direction = "down") %>%
  filter(test_type == "Nordic") %>%
  group_by(user_id, about, side) %>%
  summarise(max_force = max(maximum_force, na.rm = TRUE), .groups = "drop")

# merge into a final table


# --- CMJ columns mapped to PCA groups ---
cmj_cols <- c(
  # Group 1: Propulsive velocity/power/jump height
  "jump_height_flight_time_cm",
  "flight_time_s",
  "concentric_peak_velocity_m_s",
  "vertical_velocity_at_takeoff_m_s",
  "peak_power_bm_w_kg",
  "concentric_mean_force_bm_n_kg",
  
  # Group 2: Braking power/velocity
  "eccentric_mean_power_bm_w_kg",
  "eccentric_peak_velocity_m_s",
  "eccentric_peak_power_bm_w_kg",
  
  # Group 3: Braking/propulsive force
  "concentric_mean_force_n",
  "eccentric_mean_braking_force_n",
  "concentric_peak_force_n",
  "eccentric_peak_force_n",
  "force_at_zero_velocity_n",
  "weight_kg",
  
  # Group 4: Net impulse
  "positive_impulse_ns",
  "concentric_impulse_ns",
  
  # Group 5: Landing force, propulsive impulse
  "peak_landing_force_n",
  "concentric_mean_power_bm_w_kg",
  "eccentric_braking_impulse",
  
  # Group 6: L|R asymmetries
  "concentric_mean_force_asym_n",
  "eccentric_mean_force_asym_n",
  "eccentric_braking_rfd_asym_n_s",
  "peak_landing_force_asym_n",
  
  # Group 7: Braking RFD
  "eccentric_braking_rfd_n_s",
  "eccentric_braking_rfd_bm_n_s_kg",
  
  # Group 8: Landing, unweighting
  "peak_landing_force_left_n",
  "peak_landing_force_right_n",
  "eccentric_unloading_impulse_ns",
  
  # Group 9: Depth, RSI, phase timing
  "countermovement_depth_cm",
  "rsi_modified_m_s",
  "braking_phase_duration",
  "contraction_time_s"
)

# IBSQT columns
ibsqt_cols <- c(
  "peak_vertical_force_n",
  "peak_vertical_force_bm_n_kg"
)

# --- Filter to columns that actually exist ---
cmj_cols <- intersect(cmj_cols, names(fd_data_max))
ibsqt_cols <- intersect(ibsqt_cols, names(fd_data_max))

# --- Build long format for CMJ ---
cmj_long <- fd_data_max %>%
  filter(test_type == "CMJ") %>%
  select(about, user_id, start_date, all_of(cmj_cols)) %>%
  pivot_longer(cols = all_of(cmj_cols), names_to = "metric", values_to = "value") %>%
  mutate(test_type = "CMJ")

# --- IBSQT ---
ibsqt_long <- fd_data_max %>%
  filter(test_type == "IBSQT") %>%
  select(about, user_id, start_date, all_of(ibsqt_cols)) %>%
  pivot_longer(cols = all_of(ibsqt_cols), names_to = "metric", values_to = "value") %>%
  mutate(test_type = "IBSQT")

# --- Statsports ---
speed_long <- ss_max_speed %>%
  transmute(about, user_id, start_date = NA_character_,
            test_type = "Max Speed", metric = "max_speed_ms", value = max_speed)

# --- Watt Bike ---
wb_long <- wb_recent %>%
  transmute(about, user_id, start_date,
            test_type = "Watt Bike", metric = "distance", value = distance)

# --- VALD Nordic ---
nordic_long <- nordic_max %>%
  transmute(about, user_id, start_date = NA_character_,
            test_type = "Nordic",
            metric = paste0("nordic_max_force_", tolower(side)),
            value = max_force)

# --- Combine all ---
all_tests <- bind_rows(cmj_long, ibsqt_long, speed_long, wb_long, nordic_long) %>%
  select(about, user_id, test_type, start_date, metric, value)




str(all_tests)





library(DBI)
library(odbc)
library(readxl)
library(dplyr)
library(stringr)
library(tibble)

path_lookup <- '/Users/smorrison/Downloads/uuid_lookup_table.xlsx'

clean_lookup_seed <- function(path) {
  df <- readxl::read_excel(path, col_types = "text") %>%
    as_tibble()

  required_cols <- c(
    "First Name", "Last Name", "Date of Birth", "UUID", "Username",
    "Email Address", "Password", "Address", "Suburb/City", "State",
    "Country", "Postcode/ZIP", "Phone Number", "Groups as Athlete",
    "Secondary Group", "Tertiary Group", "Fourth Group"
  )

  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing expected columns: ", paste(missing_cols, collapse = ", "))
  }

  # helper: trim whitespace; convert blanks/"NA"/"NaN" to NA
  clean_text <- function(x) {
    x <- trimws(as.character(x))
    x[x %in% c("", "NA", "NaN", "NULL", "null")] <- NA_character_
    x
  }

  # helper: convert mixed Excel serial / text DOB to Date
  clean_dob <- function(x) {
    x <- trimws(as.character(x))
    x[x %in% c("", "NA", "NaN", "NULL", "null")] <- NA_character_

    out <- rep(as.Date(NA), length(x))

    is_num <- grepl("^[0-9]+(\\.[0-9]+)?$", x)
    if (any(is_num, na.rm = TRUE)) {
      out[is_num] <- as.Date(as.numeric(x[is_num]), origin = "1899-12-30")
    }

    # If any non-numeric dates appear later, try common formats
    is_non_num <- !is_num & !is.na(x)
    if (any(is_non_num)) {
      parsed1 <- as.Date(x[is_non_num], format = "%d/%m/%Y")
      parsed2 <- as.Date(x[is_non_num], format = "%m/%d/%Y")
      parsed3 <- as.Date(x[is_non_num], format = "%Y-%m-%d")
      out[is_non_num] <- dplyr::coalesce(parsed1, parsed2, parsed3)
    }

    out
  }

  df %>%
    mutate(across(all_of(required_cols), clean_text)) %>%
    mutate(
      `Date of Birth` = clean_dob(`Date of Birth`),
      UUID = str_pad(gsub("[^0-9]", "", UUID), width = 7, side = "left", pad = "0")
    ) %>%
    distinct(UUID, .keep_all = TRUE)
}

lookup_seed <- clean_lookup_seed(path_lookup)
glimpse(lookup_seed)


connection_string <- "Aquarium"
db_name_lookup <- "Scratch"
schema_name_lookup <- "morr"
table_name_lookup <- "international_lookup_table_ab_uuid"

con <- DBI::dbConnect(odbc::odbc(), connection_string)

lookup_table_id <- DBI::Id(
  catalog = db_name_lookup,
  schema  = schema_name_lookup,
  table   = table_name_lookup
)

if (DBI::dbExistsTable(con, lookup_table_id)) {
  stop("Table already exists: [Scratch].[morr].[international_lookup_table_ab_uuid]")
}

DBI::dbWriteTable(
  conn = con,
  name = lookup_table_id,
  value = lookup_seed,
  row.names = FALSE
)

DBI::dbDisconnect(con)


con <- DBI::dbConnect(odbc::odbc(), connection_string)

lookup_sql <- DBI::dbReadTable(con, lookup_table_id) %>%
  as_tibble()

glimpse(lookup_sql)
head(lookup_sql)

DBI::dbDisconnect(con)
