esi <- function(deaths,
                recovered,
                severity = 6.5,
                epsilon = 20) {
  D <- deaths
  S <- severity
  R <- recovered

  A <- log10(1 + D)

  beta <- tanh((S * D - R) / epsilon + (D + R) / (2 * epsilon))

  I = A * beta
  return(I)
}


get_country_data <-
  function(countries_api = "https://corona-api.com/countries") {
    require('jsonlite')
    require("tibble")
    json_data <- fromJSON(countries_api)
    country_data <- as_tibble(json_data$data)

    return(country_data)
  }

load_data <-
  function(countries_api = "https://corona-api.com/countries",
           cache_expiration = lubridate::hours(24)) {
    require('jsonlite')
    require("readr")
    require("lubridate")
    require("magrittr")
    cache_file <- "data_cache.rds"

    if (file.exists(cache_file)) {
      # cache still valid?
      if ((file.info(cache_file)$mtime + cache_expiration) > lubridate::now()) {
        expir_date <-
          (file.info(cache_file)$mtime + cache_expiration) - lubridate::now()
        message(paste("Cache will expire in", format(expir_date)))
        all_data <- read_rds(cache_file)
        return(all_data)
      }
    }

    message("Cache expired or not available.")
    country_data <- get_country_data()
    countries <- country_data$code

    all_data <- tibble()
    pb <- progress::progress_bar$new(format = "  downloading [:bar] :percent eta: :eta",
                                     total = length(countries))

    for (country in countries) {
      pb$tick()
      local_data <- fromJSON(paste0(countries_api, "/", country))
      local_complete <-
        local_data$data$timeline %>% as_tibble() %>% mutate(code = country)
      all_data <- all_data %>% bind_rows(local_complete)
    }

    res_data <- all_data %>%
      mutate(date = as.Date(date)) %>%
      filter(is.na(is_in_progress)) %>%
      select(-is_in_progress) %>%
      left_join(country_data %>% select(name, code, population), by = c("code")) %>%
      group_by(code, date) %>%
      rename(country = name) %>%
      rename(death = new_deaths, death_cum = deaths) %>%
      rename(recovered_cum = recovered, confirmed_cum = confirmed) %>%
      rename(recovered = new_recovered, confirmed = new_confirmed) %>%
      mutate(death_pm = death / population * 1000000) %>%
      mutate(confirmed_pm = confirmed / population * 1000000) %>%
      mutate(recovered_pm = recovered / population * 1000000) %>%
      mutate(active_pm = active / population * 1000000) %>%
      mutate(death_cum_pm = death_cum / population * 1000000) %>%
      mutate(confirmed_cum_pm = confirmed_cum / population * 1000000) %>%
      mutate(recovered_cum_pm = recovered_cum / population * 1000000)
    # match old naming convention

    write_rds(res_data, cache_file)

    return(res_data)
  }


# TESTS -----

# Vaccination data ----
if (FALSE) {
  vaccine_data <-
    readr::read_csv(
      "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv"
    )

  vaccine_data %>% filter(location == "Germany") %>%
    ggplot() +
    aes(x = date, y = total_vaccinations_per_hundred) +
    geom_point()


  vaccine_data %>% filter(location %in% c(europe, "United Kingdom")) %>%
    group_by(location) %>%
    mutate(highest_vaccine_rate = max(total_vaccinations_per_hundred, na.rm = T)) %>%
    ggplot() +
    aes(x = date,
        y = total_vaccinations_per_hundred,
        color = fct_inorder(location)) +
    geom_line() +
    gghighlight::gghighlight(highest_vaccine_rate < 50,
                             label_key = location,
                             use_direct_label = T)
}


# other tests ----
if (FALSE) {
  europe <-
    c(
      "Germany",
      "Belgium",
      "Spain",
      "Italy",
      "UK",
      "France",
      "Denmark",
      "Norway",
      "Sweden",
      "Finland",
      "Portugal",
      "Slovakia",
      "Albania",
      "Andorra",
      "Austria",
      "Bosnia and Herzegovina",
      "Croatia",
      "Cyprus",
      "Czechia",
      "Greece",
      "Estonia",
      "Ireland",
      "Luxembourg",
      "Malta",
      "Montenegro",
      "Monaco",
      "Netherlands",
      "Poland",
      "Romania",
      "Serbia",
      "Switzerland"
    )

  window_size <- 7

  load_data() %>%
    group_by(country) %>%
    mutate(
      death_roll = rollapplyr(
        death,
        window_size,
        mean,
        partial = TRUE,
        na.rm = TRUE
      ),
      # use of a 7-day rolling average
      recovered_roll = rollapplyr(
        recovered,
        window_size,
        mean,
        partial = TRUE,
        na.rm = TRUE
      ),
      confirmed_roll = rollapplyr(
        confirmed,
        window_size,
        mean,
        partial = TRUE,
        na.rm = TRUE
      ),
      confirmed_roll_pm = rollapplyr(
        confirmed_pm,
        window_size,
        mean,
        partial = TRUE,
        na.rm = TRUE
      )
    ) %>%
    pivot_longer(
      cols = c(
        "death",
        "confirmed",
        "recovered",
        "confirmed_cum",
        "recovered_cum",
        "death_cum",
        "active",
        "death_pm",
        "confirmed_pm",
        "recovered_pm",
        "death_cum_pm",
        "confirmed_cum_pm",
        "recovered_cum_pm",
        "active_pm",
        "death_roll",
        "recovered_roll",
        "confirmed_roll"
      ),
      names_to = "variable",
      values_to = "value"
    ) %>%


    #filter(str_detect(variable, "million") | str_detect(variable, "active")) %>%
    filter(country %in% europe) %>%
    ggplot() +
    aes(
      x = as.Date(date),
      y = value,
      group = country,
      color = country
    ) + geom_line() +
    # gghighlight(code %in% c("DE", "FR", "IN", "BR", "BE")) +
    facet_wrap(vars(variable), scale = "free_y")

  all_data$name %>% unique()





  load_data() %>%
    filter(country %in% europe) %>%
    ggplot() +
    aes(x = date, y = country, fill = death_pm) +
    geom_tile() +
    scale_fill_viridis_c()
}
