#------------------------------------------------------------------------------*
# Get monitoring data
#------------------------------------------------------------------------------*


#------------------------------------------------------------------------------*
# Prepare analysis environment ----
#------------------------------------------------------------------------------*

# Load used libraries
library(package = "tidyverse")




#------------------------------------------------------------------------------*
# Read dot setup data from RedCap export ----
#------------------------------------------------------------------------------*


# Get data from full RedCap raw export
all_rc_data <- # will take all the files in the folder
  list.files(
    path = "data/redcap-exports",
    full.names = TRUE
  ) %>%
  tibble(
    file = .,
    lf = tolower(.)
  ) %>%
  # Get metadata from the name
  extract(
    col = lf, into = c("irc", "file_timestamp"),
    regex = ".*(guatemala|india|peru|rwanda).*?_([-_0-9]+)[.]csv",
    remove = TRUE, ignore.case = TRUE
  ) %>%
  mutate(
    irc = tolower(irc),
    # file date stored with ISO date format
    file_date = lubridate::ymd(file_timestamp),
    file_data = map(
      .x = file, # for every file
      read_csv,  # read its contents
      # and read each variable as text
      # to preserve dates and times later
      col_types = cols(.default = col_character())
    )
  ) %>%
  unnest()





# prepare data from all IRC dot setups ----
dots_setup_rc <- all_rc_data %>%
  select(
    irc, HHID, redcap_event_name,
    matches("h40_.*(visit|time|date|dot|area|stove)"),
    # remove because we are not using it but was included because it matched dot
    -matches("complete|problem|action|other|add")
  ) %>%
  mutate_all(as.character) %>%
  #----------------------------------------------------------------------------*
  # Reshape and subset the data to the variables that are needed
  #----------------------------------------------------------------------------*
  gather(
    key = column, value = value,
    matches("visit|time|date|dot|area|stove"),
    -matches("h40_date"),
    na.rm = TRUE
  ) %>%
  mutate(
    # explicit crf_copy on variable names
    column = if_else(
      condition = !grepl("_v[0-9]$", column),
      true = paste0(column, "_v1"),
      false = column
    ),
    # Fix non-standard variablenames
    column = gsub(
      pattern = "countinue",
      replacement = "continue",
      column
    )
  ) %>%
  # separate variable context
  extract(
    col = column,
    into = c("crf", "variable", "dot_correlative", "crf_copy"),
    regex = "(^[^_]+)_([^0-9]+)([0-9]+)?_v([0-9]+$)"
  ) %>%
  spread(key = variable, value = value) %>%
  # keep the correct date given the crf copy
  mutate(
    crf_date = if_else(
      condition = crf_copy == 1,
      true = h40_date,
      false = h40_date_v2
    )
  ) %>%
  arrange(HHID, crf_date) %>%
  # collect the data depending on the visit type
  select(
    irc, HHID, redcap_event_name, crf_date,
    crf_copy, dot_id = dot, visit,
    everything(), -crf, -h40_date, -h40_date_v2
  ) %>%
  gather(key = variable, value = value, matches("(ins|dl|stop)_(date|time)")) %>%
  separate(
    col = variable,
    into = c("step", "variable")
  ) %>%
  filter(!is.na(value)) %>%
  spread(key = variable, value = value) %>%
  rename(step_date = date) %>%
  arrange(HHID, dot_id, crf_date) %>%
  mutate_at(
    vars(matches("date")),
    funs(as.Date)
  ) %>%
  print()






# next dot dl based on most recent one
dots_setup_rc %>%
  group_by(HHID) %>%
  summarize(
    dot_start = min(crf_date, na.rm = TRUE),
    most_recent = max(crf_date),
    next_check = most_recent + lubridate::days(15),
    active_dots = dot_id[step != "stop" & crf_date == most_recent] %>% unique() %>% length(),
    which_dots = dot_id[step != "stop" & crf_date == most_recent] %>% unique() %>% paste(collapse = "; ")
  )






#------------------------------------------------------------------------------*
# Prepara dots setup data for analysis ----
#------------------------------------------------------------------------------*


# Implicitly label dot data recording events as "missions" (sets)
dots_setup_labeled <- dots_setup_rc %>%
  # Guatemala specific fixes
  mutate(
    dot_id = case_when(
      irc == "guatemala" & dot_id == "LBC" & crf_date == "2018-09-28" ~ "0119",
      TRUE ~ dot_id
    )
  ) %>%
  # Identify missions
  mutate(
    step = factor(step, levels = c("ins", "dl", "stop"))
  ) %>%
  arrange(irc, HHID, dot_id, crf_date, step) %>%
  group_by(irc, HHID) %>%
  # Tag runs of records starting at "ins" and ending at "stop" %>%
  mutate(
    border = step == "ins",
    set = stats::filter(
      x = border, filter = 1, method = "recursive"
    )
  ) %>%
  # label sets without dowload events because we do not expect data
  group_by(irc, HHID, dot_id, set) %>%
  mutate(
    expect_data = any(step == "dl")
  ) %>%
  ungroup() %>%
  select(
    irc:visit, expect_data, everything()
  ) %>%
  print(n = Inf)


# Get mission windows from RC data
dots_rc_missions <- dots_setup_labeled %>%
  group_by(irc, HHID, dot_id, set, stove) %>%
  summarize(
    downloads = sum(expect_data),
    start_date = as.character(min(crf_date, na.rm = TRUE)),
    last_date = as.character(max(crf_date, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  arrange(irc, HHID, start_date) %>%
  print()


# Get the first date of dot setup for each IRC
dots_start <- dots_rc_missions %>%
  group_by(irc) %>%
  summarize(
    first_install_date = min(start_date, na.rm = TRUE)
  ) %>%
  ungroup()



# Number of missions with data downloads recorded in redcap
dots_rc_missions %>% count(irc, wt = downloads > 0)


# Number of mission with data from geocene export
mission_data %>% count(irc)


# Find dots listed in the missions but not in the RedCap
mission_data %>%
  fix_dot_id() %>%
  filter(
    ! dot_id %in% pull(fix_dot_id(dots_rc_missions), dot_id)
  ) %>%
  arrange(
    irc, start_date, dot_id
  ) %>%
  count(irc, dot_id)


# Check missmatches
full_join_check <- dots_rc_missions %>%
  fix_dot_id() %>%
  full_join(
    fix_dot_id(mission_data) %>%
      select(-old_id)
  ) %>%
  mutate(
    status = case_when(
      is.na(HHID) ~ "mission not listed in RedCap",
      is.na(mission_id) & downloads == 0 ~ 
        "ok - no data dowloaded yet",
      is.na(mission_id) ~ "RedCap missing Geocene data",
      !is.na(HHID) & !is.na(mission_id) ~
        "ok - has RedCap and Geocene data"
    ) %>%
      factor(
        levels = c(
          "ok - has RedCap and Geocene data",
          "ok - no data dowloaded yet",
          "RedCap missing Geocene data",
          "mission not listed in RedCap"
        )
      )
  )



full_join_check %>%
  count(irc, status) %>%
  # uncomment lines to anonymize ircs
  # mutate(
  #   irc = factor(
  #     irc,
  #     labels = sample(
  #       x = seq(from = 1, to = length(unique(irc))),
  #       size = length(unique(irc))
  #     )
  #   )
  # ) %>%
  spread(key = irc, value = n, fill = 0) %>%
  print()


# Join redcap records with data
dots_setup_data <- dots_rc_missions %>%
  left_join(mission_data)


# End of script
