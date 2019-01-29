#------------------------------------------------------------------------------*
# Dot data automated QC ----
#------------------------------------------------------------------------------*


library(package = "tidyverse")

source(file = "scripts/0_get_dots_raw_data.R", encoding = "UTF-8")
source(file = "scripts/get-data.R", encoding = "UTF-8")




#------------------------------------------------------------------------------*
# section_name ----
#------------------------------------------------------------------------------*

dots_setup_data <- dots_rc_missions %>%
  fix_dot_id() %>%
  full_join(
    fix_dot_id(mission_data)
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



dots_setup_data %>% count(irc, downloads > 0, is.na(mission_id))



tagged_dots_data <- dots_rc_missions %>%
  full_join(
    dots_data %>%
      mutate(
        start_date = dot_date
      )
  ) %>%
  filter(!is.na(mission_id))


cooking_events_households <- dots_setup_data %>%
  mutate(
    stove_type = if_else(
      condition = grepl("gas", stove),
      true = "lpg",
      false = "traditional"
    )
  ) %>%
  select(-data) %>%
  left_join(
    geocene_events %>%
      filter(event_kind == "cooking") %>%
      mutate(
        cooking_time = as.numeric(stop_time - start_time, units = "hours")
      ) %>%
      group_by(mission_id = mission) %>%
      summarize(
        cooking_events = n(),
        cooking_time = sum(cooking_time)
      )
  ) %>%
  group_by(irc, HHID, stove_type) %>%
  summarize(
    monitoring_start = min(as.Date(start_date)),
    monitoring_end = max(as.Date(last_date)),
    monitoring_days = as.numeric(monitoring_end - monitoring_start, units = "days"),
    cooking_events = sum(cooking_events, na.rm = TRUE),
    cooking_time = sum(cooking_time, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  print()


cooking_events_summary <- cooking_events_households %>%
  group_by(irc, stove_type) %>%
  summarize(
    monitored_households = HHID %>% unique() %>% length(),
    with_events = sum(!is.na(cooking_time)),
    monitoring_days = sum(monitoring_days, na.rm = TRUE),
    cooking_events = sum(cooking_events, na.rm = TRUE),
    cooking_time = sum(cooking_time, na.rm = TRUE),
    cooking_hours_per_day = cooking_time / monitoring_days
  ) %>%
  select(
    irc, stove_type,
    "Monitored households (n)" = monitored_households,
    "Households with events (n)" = with_events,
    "Monitoring days (n)" = monitoring_days,
    # "Cooking events (n)" = cooking_events,
    # "Cooking time (hours)" = cooking_time,
    "Cooking per day (hours/day)" = cooking_hours_per_day
  ) %>%
  gather(key = variable, value = value, -irc, -stove_type, factor_key = TRUE) %>%
  mutate(
    value = round(value, 1)
  ) %>%
  spread(key = stove_type, value = value, "") %>%
  ungroup() %>%
  spread(irc, traditional) %>%
  knitr::kable() %>%
  print()




minute_events <- geocene_events %>%
  filter(event_kind == "cooking") %>%
  rownames_to_column() %>%
  gather(variable, time, start_time, stop_time) %>%
  mutate(time = lubridate::floor_date(time, unit = "minutes")) %>%
  select(rowname, mission_id = mission, time) %>%
  group_by(rowname, mission_id) %>%
  padr::pad() %>%
  mutate(event = "cooking") %>%
  ungroup() %>%
  select(-rowname)





dots_events_data <- tagged_dots_data %>%
  mutate(
    stove_type = if_else(
      condition = grepl("gas", stove),
      true = "lpg",
      false = "traditional"
    )
  ) %>%
  unnest() %>%
  select(
    irc, id = HHID,
    mission_id, dot_id, stove_type, timestamp, value
  ) %>%
  mutate(
    time = lubridate::floor_date(timestamp, unit = "minutes")
  ) %>%
  left_join(minute_events) %>%
  group_by(irc, mission_id, dot_id) %>%
  mutate(
    value = if_else(
      condition = value < 1,
      true = 1,
      false = value
    ),
    event = if_else(
      condition = is.na(event),
      true = "normal",
      false = event
    ),
    group = stats::filter(
      event != lag(event, default = "normal"), filter = 1, method = "recursive"
    )
  ) %>%
  arrange(id, mission_id, timestamp) %>%
  ungroup() %>%
  print()





#------------------------------------------------------------------------------*
# Prepare dashboard for individual households ----
#------------------------------------------------------------------------------*

library(package = "trelliscopejs")


dots_data_trellis <- dots_events_data %>%
  ungroup() %>%
  mutate(
    timestamp = timestamp - lubridate::hours(6),
    dot_date = as.Date(timestamp),
    dot_time = timestamp %>%
      lubridate::floor_date(unit = "5 minutes") %>%
      lubridate::`date<-`(as.Date("2018-01-01"))
  ) %>%
  select(
    irc, id, stove_type, event,
    mission_id, dot_id, timestamp, value, dot_date, dot_time
  ) %>%
  group_by(irc, id, stove_type) %>%
  nest()



dots_gg_trellis <- dots_data_trellis %>%
  mutate(
    panel = map_plot(
      data,
      ~ {
        
        .x %>%
          ggplot() +
          geom_hline(
            yintercept = 50,
            linetype = "dashed",
            color = "blue"
          ) +
          geom_line(
            aes(
              x = timestamp, y = value,
              color = event, group = paste(dot_id, dot_date)
            ),
            alpha = 0.3
          ) +
          labs(
            y = "Temperature (°C)",
            fill = "Reference",
            color = "Geocene label"
          ) +
          facet_wrap(~dot_id, strip.position = "right", scales = "free_x") +
          scale_color_manual(values = c(normal = "grey60", cooking = "#E41A1C")) +
          scale_fill_manual(values = "blue") +
          scale_y_log10(breaks = c(10, 50, 100, 500, 1000, 2000)) +
          scale_x_datetime(date_labels = "%H:%M") +
          expand_limits(y = c(10, 2000)) +
          theme_bw() +
          theme(
            legend.position = "right"
          )
      }
    )
  )


dots_gg_trellis %>%
  # slice(1) %>%
  filter(complete.cases(select(., -data, -panel))) %>%
  trelliscope(name = "dots-dy", width = 1000, self_contained = TRUE)



# End of script
