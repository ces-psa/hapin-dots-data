#------------------------------------------------------------------------------*
# Read data from all dot mission files
#------------------------------------------------------------------------------*

# Load used packages
library(package = "tidyverse")




#------------------------------------------------------------------------------*
# section_name ----
#------------------------------------------------------------------------------*

# Metadata from each configured and deployed dot
missions <- read_csv(file = "data/geocene-exports/missions.csv")


# Events tagged by Geocene
geocene_events <- read_csv(file = "data/geocene-exports/events.csv")


# Read in data from every file
dots_data <- missions %>%
  filter(
    grepl("Guatemala|India|Rwanda|Peru", campaign)
  ) %>%
  select(
    irc = campaign, mission_id = id, dot_id = dot_name,
    mission_name, creator_username
  ) %>%
  mutate(
    irc = gsub(
      pattern = "hapin-",
      replacement = "",
      x = tolower(irc)
    ),
    # Guatemala specific fixes
    irc = case_when(
      irc == "guatemala" & grepl("rwanda|peru|india", creator_username) ~
        gsub(".+(rwanda|peru|india).+", "\\1", creator_username),
      TRUE ~ irc
    ),
    dot_id = gsub("dot[^0-9]", "", tolower(dot_id)),
    file = paste0("data/geocene-exports/", mission_id, ".csv"),
    file = map(
      file,
      ~ .x %>%
        read_csv(col_types = cols(.default = col_character())) %>%
        # first timestamp as date of the mission
        mutate(
          dot_date = as.character(as.Date(first(timestamp)))
        )
    )
  ) %>%
  unnest() %>%
  mutate(
    timestamp = lubridate::ymd_hms(timestamp),
    value = as.numeric(value)
  )




#------------------------------------------------------------------------------*
# Prepare dots data for analysis ----
#------------------------------------------------------------------------------*

# pack data by mission
mission_data <- dots_data %>%
  group_by(irc, mission_id, dot_id, start_date = dot_date) %>%
  # This will store all the data in tables for each row (column "data")
  nest() %>%
  # Only keep missions that started after the first dots installation
  left_join(dots_start) %>%
  filter(
    start_date >= first_install_date
  ) %>%
  select(-first_install_date)


# Function to harmonize dot_ids
fix_dot_id <- . %>%
  mutate(
    old_id = dot_id,
    # special cases
    dot_id = recode(
      dot_id,
      LBC = NA_character_
    ),
    dot_id = dot_id %>%
      tolower() %>%
      # stardardize double(multiple) dot placement ids
      gsub(
        pattern = "([rl])[^a-zA-Z0-9]+([a-zA-Z0-9]+)",
        replacement = "\\1\\2",
        x = .
      ) %>%
      gsub(
        pattern = "[^rla-zA-Z0-9]+",
        replacement = ";",
        x = .
      )
  ) %>%
  # Separate double(multiple) dot placements
  separate_rows(
    dot_id, sep = ";"
  ) %>%
  extract(
    col = dot_id, into = c("side", "dot_id"),
    regex = "([rl])?([a-zA-Z0-9]+)"
  ) %>%
  mutate(
    # add leading zeroes
    dot_id = stringr::str_pad(
      string = dot_id, side = "left",
      width = 4, pad = "0"
    )
  )


# End of script
