# Overview tab

#---------------------------------------------------------------------
#data required

nat_summary <- read_csv('data/nat_summary.csv', col_types = cols(.default = "c"))

nat_summary_total <- filter(nat_summary, school_type == 'Total')

#---------------------------------------------------------------------
#National bar chart functions

# rate

national_bars_rate <- function(category) {
  
  if (category == 'P') {
    data <- nat_summary_total %>%
      mutate(year = as.factor(year),
             value = as.numeric(perm_excl_rate))
    
    plot <- data %>%
      ggplot(aes(x = formatyr(year), y = value)) +
      geom_bar(fill = "#2B8CC4", stat = "identity") +
      ylab("Permanent exclusion rate")
  }
  
  if (category == 'F') {
    data <- nat_summary_total %>%
      mutate(year = as.factor(year),
             value = as.numeric(fixed_excl_rate))
    
    plot <- data %>%
      ggplot(aes(x = formatyr(year), y = value)) +
      geom_bar(fill = "#2E358B", stat = "identity") +
      ylab("Fixed period exclusion rate")
  }
  
  plot <- plot +
    scale_y_continuous(breaks = seq(0, max(data$value + 0.5), 0.50)) +
    theme_classic() +
    theme(axis.title.x = element_blank(),
          text = element_text(size = 14)) +
    geom_text(
      data = data,
      aes(label = sprintf("%.2f", value)),
      colour = "white",
      vjust = 2
    )
  
  return(plot)
}


# number 

national_bars_num <- function(category) {
  
  if (category == 'P') {
    data <- nat_summary_total %>%
      mutate(year = as.factor(year),
             value = as.numeric(perm_excl))
    
    plot <- data %>% 
      ggplot(aes(x = formatyr(year), y = value)) +
      geom_bar(fill = "#2B8CC4", stat = "identity") +
      ylab("Permanent exclusions")
  }
  
  if (category == 'F') {
    data <- nat_summary_total %>%
      mutate(year = as.factor(year),
             value = as.numeric(fixed_excl))
    
    plot <- data %>%
      ggplot(aes(x = formatyr(year), y = value)) +
      geom_bar(fill = "#2E358B", stat = "identity") +
      ylab("Fixed period exclusions") 
  }
  
  plot <- plot + 
    theme_classic() +
    theme(axis.title.x = element_blank(),
          text=element_text(size=14)) +
    geom_text(
      data = data,
      aes(label = prettyNum(value, big.mark = ",")),
      colour="white",
      vjust = 2) +
    scale_y_continuous(labels = scales::comma)
  
  return(plot)
}
