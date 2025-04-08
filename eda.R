library(ggplot2)
library(ggtext)
library(dplyr)
library(tidyr)
library(ggalluvial)
library(stringr)
library(plotly)
library(networkD3)
library(leaflet)


year_bar_chart <- function(data, title, subtitle) {
    ggplot(data, aes(x = Year, y = value)) +
        geom_bar(stat = "identity", fill = "#0072B2") +
        geom_text(aes(label = value), vjust = -0.5, size = 3.5) +
        labs(title = title, subtitle = subtitle) +
        theme_minimal() +
        theme(
            plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 12),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1)
        )
}
library(ggrepel)

year_line_chart <- function(data, title, subtitle, metrics = NULL) {
  plot_data <- data

  if (!is.null(metrics)) {
    plot_data <- plot_data %>% filter(Metric %in% metrics)
  }

  plot <- ggplot(plot_data, aes(x = Year, y = value, group = Metric)) +
    geom_line(size = 1.2, alpha = 0.85) +
    ggrepel::geom_text_repel(
      aes(label = value),
      size = 4.5,
      show.legend = FALSE,
      max.overlaps = Inf,
      box.padding = 0.3,
      point.padding = 0.2,
      color = "black",
    ) +
    labs(title = title, subtitle = subtitle, color = NULL) +
    theme_minimal(base_family = "sans") +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 14),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.text = element_text(size = 12)
    )

  return(plot)
}

plot_difference_bar <- function(data,
                                title = "Difference between Total Affected and Incidents",
                                subtitle = "Per Year",
                                highlight_2024 = FALSE) {
  # Summarise and calculate difference
  summary_data <- data %>%
    group_by(Year) %>%
    summarise(
      Total_Incidents = n(),
      Total_Affected = sum(`Total.affected`, na.rm = TRUE),
      Difference = Total_Affected - Total_Incidents
    )

  # Add highlight column if needed
  if (highlight_2024) {
    summary_data <- summary_data %>%
      mutate(Highlight = ifelse(Year == 2024, "2024", "Other"))

    p <- ggplot(summary_data, aes(x = as.factor(Year), y = Difference, fill = Highlight)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("2024" = "#0072B2", "Other" = "#D55E00"))
  } else {
    p <- ggplot(summary_data, aes(x = as.factor(Year), y = Difference)) +
      geom_bar(stat = "identity", fill = "#D55E00")
  }

  # Final plot assembly
  p +
    geom_text(aes(label = Difference), vjust = -0.5, size = 3, fontface = "bold") +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Year",
      y = "Difference"
    ) +
    theme_minimal(base_family = "sans") +
    theme(
      # Transparent background and clean frame
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),

      # Clean border
      panel.border = element_rect(color = "black", fill = NA, size = 1),

      # Remove grid lines
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),

      # Title styling
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 14),

      # Axis styling
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),

      # Hide legend unless needed
      legend.position = "none"
    )
}

library(forcats)
library(RColorBrewer)  # for palettes

plot_top_countries_by_attack_2024 <- function(data, top_n = 5, title = "Countries with Most Incidents in 2024") {
  # Filter for 2024 only
  data_2024 <- data %>%
    filter(Year == 2024)

  # Group by Country and Attack Type, count incidents
  country_attack_counts <- data_2024 %>%
    group_by(Country, `Means.of.attack`) %>%
    summarise(Incidents = n(), .groups = "drop")

  # Get top N countries by total incidents
  top_countries <- country_attack_counts %>%
    group_by(Country) %>%
    summarise(Total = sum(Incidents)) %>%
    arrange(desc(Total)) %>%
    slice_head(n = top_n) %>%
    pull(Country)

  # Filter and factor countries to preserve order
  plot_data <- country_attack_counts %>%
    filter(Country %in% top_countries) %>%
    mutate(Country = factor(Country, levels = top_countries))

  # Plot
  ggplot(plot_data, aes(x = Country, y = Incidents, fill = `Means.of.attack`)) +
    geom_bar(stat = "identity", width = 0.75) +
    scale_fill_brewer(palette = "Spectral", name = "Attack Type") +  # 🎨 Palette you can swap!
    labs(
      title = title,
      subtitle = "Stacked by Type of Attack",
      x = "Country",
      y = "Number of Incidents"
    ) +
    theme_minimal(base_family = "sans") +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(angle = 40, hjust = 1, size = 12),
      axis.text.y = element_text(size = 12),
      plot.margin = margin(t = 10, r = 20, b = 20, l = 20),
      legend.text = element_text(size = 11),
      legend.title = element_text(size = 12)
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
}



plot_attack_impact_sankey_top_countries_2024 <- function(data,
                                                         attack_col = "Means.of.attack",
                                                         killed_col = "Total.killed",
                                                         wounded_col = "Total.wounded",
                                                         kidnapped_col = "Total.kidnapped",
                                                         country_col = "Country",
                                                         year_col = "Year",
                                                         top_n_countries = 5,
                                                         top_n_attacks = 6,
                                                         year = 2024,
                                                         title = "Attack Type to Impact Type (Top 5 Countries in 2024)") {
  # Step 1: Filter for the year
  data_filtered <- data %>%
    filter(!!sym(year_col) == year)

  # Step 2: Top countries by total incidents
  top_countries <- data_filtered %>%
    count(!!sym(country_col), sort = TRUE) %>%
    slice_head(n = top_n_countries) %>%
    pull(!!sym(country_col))

  # Step 3: Filter to top countries
  data_top <- data_filtered %>%
    filter(!!sym(country_col) %in% top_countries)

  # Step 4: Reshape and summarize
  sankey_data <- data_top %>%
    select(all_of(c(attack_col, killed_col, wounded_col, kidnapped_col))) %>%
    rename(
      Attack = all_of(attack_col),
      Killed = all_of(killed_col),
      Wounded = all_of(wounded_col),
      Kidnapped = all_of(kidnapped_col)
    ) %>%
    pivot_longer(
      cols = c(Killed, Wounded, Kidnapped),
      names_to = "Impact",
      values_to = "Count"
    ) %>%
    group_by(Attack, Impact) %>%
    summarise(Total = sum(Count, na.rm = TRUE), .groups = "drop")

  # Step 5: Filter top attack types
  top_attacks <- sankey_data %>%
    group_by(Attack) %>%
    summarise(Attack_Total = sum(Total)) %>%
    arrange(desc(Attack_Total)) %>%
    slice_head(n = top_n_attacks) %>%
    pull(Attack)

  sankey_data <- sankey_data %>%
    filter(Attack %in% top_attacks)

  # Step 6: Wrap long text
  sankey_data$Attack <- str_wrap(sankey_data$Attack, width = 15)

  # Step 7: Plot
  ggplot(sankey_data,
         aes(axis1 = Attack, axis2 = Impact, y = Total)) +
    geom_alluvium(aes(fill = Attack), width = 1/12) +
    geom_stratum(width = 1/12, fill = "gray90", color = "black") +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3.5) +
    scale_x_discrete(limits = c("Attack", "Impact"), expand = c(.05, .05)) +
    labs(title = title, y = "Number of People", x = NULL) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.text.y = element_blank(),
      panel.grid = element_blank()
    )
}

plot_interactive_dot_motive <- function(data,
                                        year_col = "Year",
                                        motive_col = "Motive",
                                        affected_col = "Total.affected",
                                        hover_cols = c("Country", "Means.of.attack")) {

  # Clean + prep data
  plot_data <- data %>%
    filter(!is.na(!!sym(motive_col)), !is.na(!!sym(year_col))) %>%
    mutate(`Total Affected` = as.numeric(gsub(",", "", .data[[affected_col]]))) %>%
    group_by(!!sym(year_col), !!sym(motive_col)) %>%
    summarise(
      Total_Affected = sum(.data[[affected_col]], na.rm = TRUE),
      .groups = "drop"
    )

  # Build interactive plot
  plot_ly(
    data = plot_data,
    x = ~get(year_col),
    y = ~get(motive_col),
    size = ~Total_Affected,
    color = ~get(motive_col),
    type = 'scatter',
    mode = 'markers',
    marker = list(sizemode = 'area', opacity = 0.7, line = list(width = 1, color = '#333')),
    text = ~paste("Year:", get(year_col),
                  "<br>Motive:", get(motive_col),
                  "<br>Total Affected:", Total_Affected),
    hoverinfo = "text"
  ) %>%
    layout(
      title = "Motive of Attacks Over Time",
      xaxis = list(title = "Year"),
      yaxis = list(title = "Motive"),
      showlegend = FALSE
    )
}

plot_deadly_motive_sankey_2024 <- function(data,
                                           attack_col = "Means.of.attack",
                                           motive_col = "Motive",
                                           killed_col = "Total.killed",
                                           country_col = "Country",
                                           year_col = "Year",
                                           deadly_attacks = c("Aerial bombardment", "Shelling", "Shooting"),
                                           top_n_countries = 5,
                                           year = 2024,
                                           title = "Motives Behind Deadliest Attack Types in 2024 (Top 5 Countries)") {

  # Step 1: Filter to 2024 and non-missing motives
  data_filtered <- data %>%
    filter(.data[[year_col]] == year, !is.na(.data[[motive_col]]))

  # Step 2: Get top N countries by incident count
  top_countries <- data_filtered %>%
    count(.data[[country_col]], sort = TRUE) %>%
    slice_head(n = top_n_countries) %>%
    pull(1)

  # Step 3: Filter to those countries and selected deadly attacks
   sankey_data <- data_filtered %>%
    filter(.data[[country_col]] %in% top_countries,
            .data[[attack_col]] %in% deadly_attacks) %>%
    group_by(Motive = .data[[motive_col]], Attack = .data[[attack_col]]) %>%
    summarise(Killed = sum(as.numeric(gsub(",", "", .data[[killed_col]])), na.rm = TRUE), .groups = "drop") %>%
    filter(Killed > 0) %>%
    mutate(
        Attack = ifelse(Attack == "Aerial bombardment", "Aerial\nbombardment", Attack)
    )

  # Step 4: Plot
  ggplot(sankey_data,
         aes(axis1 = Motive, axis2 = Attack, y = Killed)) +
    geom_alluvium(aes(fill = Motive), width = 1/12) +
    geom_stratum(width = 1/12, fill = "gray90", color = "black") +
    geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3.5) +
    scale_x_discrete(limits = c("Motive", "Attack"), expand = c(.05, .05)) +
    labs(
      title = title,
      y = "Number of People Killed",
      x = NULL
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.text.y = element_blank(),
      panel.grid = element_blank()
    )
}

plot_cluster_map <- function(data,
                             lat_col = "Latitude",
                             lon_col = "Longitude",
                             label_col = "Motive",
                             popup_cols = c("Country", "Attack Type", "Total Affected"),
                             map_title = "Cluster Map of Incidents") {

  # Create label and popup text
  data$label_text <- data[[label_col]]
  
  data$popup_text <- apply(data[popup_cols], 1, function(row) {
    paste0("<b>", popup_cols, ":</b> ", row, collapse = "<br>")
  })

  # Create leaflet map
  leaflet(data) %>%
    addTiles() %>%
    addMarkers(
      lng = ~get(lon_col),
      lat = ~get(lat_col),
      label = ~label_text,
      popup = ~popup_text,
      clusterOptions = markerClusterOptions()
    )
}


render_cluster_leaflet_map <- function(data,
                                       lat_col = "Latitude",
                                       lon_col = "Longitude",
                                       popup_cols = c("Country", "Motive", "Means.of.attack", "Total.affected", "Year"),
                                       cluster = TRUE,
                                       tiles = "OpenStreetMap") {
  # Check if required columns exist
  required_cols <- c(lat_col, lon_col, popup_cols)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in data: ", paste(missing_cols, collapse = ", "))
  }

  # Create popup text
  data$popup_text <- apply(data[popup_cols], 1, function(row) {
    paste0("<b>", popup_cols, ":</b> ", row, collapse = "<br>")
  })

  # Create static lat/lon columns for leaflet to reference
  data$lat <- data[[lat_col]]
  data$lon <- data[[lon_col]]
  data$year <- data[["Year"]]

  return(data)
}

#### INTERACTIVE 

# Install if not already
# install.packages("gdtools")

# # Register Exo 2 from Google Fonts
# gdtools::register_gfont("Exo 2")
library(ggiraph)
year_line_chart <- function(data, title, subtitle = NULL, metrics = NULL) {
  plot_data <- data

  if (!is.null(metrics)) {
    plot_data <- plot_data %>% filter(Metric %in% metrics)
  }

  # Filter important labels only
  label_data <- plot_data %>%
    group_by(Metric) %>%
    filter(
      Year %% 5 == 0 |
      Year == min(Year) |
      Year == max(Year) |
      value == max(value)
    )

  plot <- ggplot(plot_data, aes(x = Year, y = value, color = Metric, group = Metric)) +
    geom_line(size = 1.2, alpha = 0.85) +
    geom_point(size = 2, alpha = 0.85) +
    ggrepel::geom_text_repel(
    data = label_data,
    aes(label = value),
    size = 4.5,
    fontface = "bold",
    box.padding = 0.4,
    point.padding = 0.3,
    segment.color = NA,
    nudge_y = ifelse(label_data$Metric == "Total_Affected", 15, -15),
    show.legend = FALSE,
    max.overlaps = Inf
    ) +
    labs(title = title, subtitle = subtitle, x = "Year", y = "Number of Incidents", color = NULL) +
    theme_minimal(base_family = "sans") +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.text = element_text(size = 12)
    )

  return(plot)
}



# Load data
incidents <- read.csv("security_incidents.csv")


top_attack_by_country <- incidents %>%
  filter(Year == 2024) %>%
  group_by(Country) %>%
  summarise(Total.incidents = n(), .groups = "drop") %>%
  arrange(desc(Total.incidents)) %>%
  slice_head(n = 5) %>%
  pull(Country) %>%
  {\(top_countries) incidents %>%
      filter(Year == 2024, Country %in% top_countries) %>%
      group_by(Country, `Means.of.attack`) %>%
      summarise(Count = n(), .groups = "drop") %>%
      group_by(Country) %>%
      mutate(Total = sum(Count)) %>%
      slice_max(Count, n = 1, with_ties = FALSE) %>%
      mutate(Percent = round(100 * Count / Total, 1)) %>%
      select(Country, `Means.of.attack`, Count, Total, Percent)
  }()

print(top_attack_by_country)

missing_summary <- incidents %>%
    group_by(Year) %>%
    summarise(
        Missing_Motive = sum(is.na(Motive)),
        Missing_Means_of_Attack = sum(is.na(`Means.of.attack`)),
        .groups = "drop"
    )

print(missing_summary)

unknown_attack <- incidents %>%
  summarise(
    total = n(),
    unknown = sum(Means.of.attack == "Unknown", na.rm = TRUE),
    percent_unknown = round(unknown / total * 100, 1)
  )

print(unknown_attack)
