# Characteristics tab

char_series <- function(char, sch_type, category) {
  
  if (char =='gender') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('Gender', 'Total'), school_type == sch_type)
  } else if (char =='sen') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('SEN_provision', 'Total'), school_type == sch_type) 
  } else if (char =='fsm') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('FSM_Eligible', 'Total'), school_type == sch_type) 
  } else if (char =='age') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('Age', 'Total'), school_type == sch_type) 
  }
  
  if (category == 'P') {
    ylabtitle <- "Permanent exclusion percentage"
    d <- d %>% mutate(y_var = perm_excl_rate) %>% filter(y_var != 'x')
  } else if (category == 'F') {
    ylabtitle <- "Fixed period exclusion percentage"
    d <- d %>% mutate(y_var = fixed_excl_rate) %>% filter(y_var != 'x')
  } else if (category == 'O') {
    ylabtitle <- "One or more fixed period exclusion percentage"
    d <- d %>% mutate(y_var = one_plus_fixed_rate) %>% filter(y_var != 'x')
  }
  
  return(
    d %>%
      ggplot +
      aes(x = as.factor(formatyr(year)), 
          y = as.numeric(y_var), 
          group = characteristic_1, colour = as.factor(characteristic_1)) +
      geom_path(size = 1) +
      xlab("Academic year") +
      ylab(ylabtitle) +
      scale_y_continuous(limits = c(0, max(as.numeric(d$y_var))*1.1)) +
      theme_classic() +
      geom_text(
        d = d %>% filter(year == min(as.numeric(year))+101),
        aes(label = characteristic_1),
        size = 5,
        hjust = 0,
        vjust = -1) +
      theme(legend.position = "none") +
      #scale_color_manual(values = c("goldenrod2", "burlywood1", "chocolate2", "darkred"))+
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"))) 
}


char_series_table <- function(char, sch_type, category) {
  
  if (char =='gender') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('Gender', 'Total'), school_type == sch_type)
  } else if (char =='sen') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('SEN_provision', 'Total'), school_type == sch_type) 
  } else if (char =='fsm') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('FSM_Eligible', 'Total'), school_type == sch_type) 
  } else if (char =='age') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('Age', 'Total'), school_type == sch_type) 
  }
  
  if (category == 'P') {
    data <- d %>% mutate(t_var = perm_excl_rate)
  } else if (category == 'F') {
    data <- d %>% mutate(t_var = fixed_excl_rate)
  } else if (category == 'O') {
    data <- d %>% mutate(t_var = one_plus_fixed_rate)
  }
  
  data$year <- formatyr(data$year)
  
  char_sch <- data %>% select(year,characteristic_1,t_var)  
  
  data_wide <- char_sch %>% spread(key = year, value =  t_var)
  
  colnames(data_wide)[1] <- ""
  
  return(data_wide)
  
}


### proportion chart 

char_prop <- function(char, sch_type, category){
  
  if (char =='gender') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('Gender'), school_type == sch_type, year == 201516)
  } else if (char =='sen') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('SEN_provision'), school_type == sch_type, year == 201516) 
  } else if (char =='fsm') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('FSM_Eligible'), school_type == sch_type, year == 201516) 
  } else if (char =='age') {
    return(NULL)
  }
  
  if (category == 'P') {
    data <- d %>% select(characteristic_1,perm_excl) %>% mutate(var = perm_excl)
  } else if (category == 'F') {
    data <- d %>% select(characteristic_1,fixed_excl) %>% mutate(var = fixed_excl)
  } else if (category == 'O') {
    data <- d %>% select(characteristic_1,one_plus_fixed) %>% mutate(var = one_plus_fixed)
  }
  
  return(data %>%
           plot_ly(labels = ~characteristic_1, values = ~var, textinfo="percent+value") %>%
           add_pie(hole = 0.5) %>%
           layout(showlegend = T,legend = list(x = 0.5, y = -0.4),
                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), 
                  margin = list(l = 20, r = 20, b = 20 ,t = 20,pad = 4)))
  
  
}


## dumbell plot

char_gaps <- function(char, sch_type, category){
  
  char_ud <- char_ud %>%
    mutate(school_type = ifelse(
      school_type == "State-funded primary","Primary",
      ifelse(school_type == "State-funded secondary","Secondary",
             ifelse(school_type == "Special", "Special", 
                    ifelse(school_type == "Total", "Total", "NA"))))) 
  
  if(category=='P') {
    measure <- 'perm_excl_rate'
  } else if (category=='F') {
    measure <- 'fixed_excl_rate'
  } else if (category=='O') {
    measure <- 'one_plus_fixed_rate'
  }
  
  
  if (char == 'gender') {
    d <- char_ud %>% filter(characteristic_desc %in% c('Gender'), school_type == sch_type,characteristic_1 %in% c('Gender_male', 'Gender_female'))
    data <- d %>% select(year, characteristic_1, measure) %>% spread(key = characteristic_1, value = measure) %>%
      mutate(diff = as.numeric(Gender_male) - as.numeric(Gender_female))
    data$char_no <- as.numeric(data$Gender_female)
    data$char_yes <-as.numeric(data$Gender_male)
    yes_label <- "Boys"
    no_label <- "Girls"
  } else if (char == 'sen') {
    d <- char_ud %>% filter(characteristic_desc %in% c('SEN_provision'), school_type == sch_type,characteristic_1 %in% c('SEN_provision_SEN_with_statement_EHC', 'SEN_Provision_No_SEN'))
    data <- d %>% select(year, characteristic_1, measure) %>% spread(key = characteristic_1, value = measure) %>%
      mutate(diff = as.numeric(SEN_provision_SEN_with_statement_EHC) - as.numeric(SEN_Provision_No_SEN))
    data$char_no <- as.numeric(data$SEN_Provision_No_SEN)
    data$char_yes <-as.numeric(data$SEN_provision_SEN_with_statement_EHC)
    yes_label <- "Statement/EHC"
    no_label <- "No SEN"
  } else if (char == 'fsm') {
    d <- char_ud %>% filter(characteristic_desc %in% c('FSM_Eligible'), school_type == sch_type,characteristic_1 %in% c('FSM_Eligible', 'FSM_NotEligible'))
    data <- d %>% select(year, characteristic_1, measure) %>% spread(key = characteristic_1, value = measure) %>%
      mutate(diff = as.numeric(FSM_Eligible) - as.numeric(FSM_NotEligible))
    data$char_no <- as.numeric(data$FSM_NotEligible)
    data$char_yes <-as.numeric(data$FSM_Eligible)
    yes_label <- "FSM"
    no_label <- "Not FSM"
  }else if (char =='age') {
    return(NULL)
  }
  
  
  max_year <- formatyr(max(data$year))
  data$year <- factor(formatyr(data$year))
  
  return(ggplot() + 
           geom_segment(data=data, aes(y=year, yend=year, x=0, xend=max(data$char_yes)*1.1), color="#b2b2b2", size=0.15) +
           geom_dumbbell(data=data, aes(y=year, x=char_yes, xend=char_no),
                         size=2, color="gray", size_x=3, size_xend=3,
                         colour_x="steelblue4", colour_xend="steelblue2") + 
           geom_text(data=filter(data, year==max_year),
                     aes(x=char_yes, y=year, label=yes_label),
                     color="steelblue4", size=5, vjust=-2, fontface="bold") + 
           geom_text(data=filter(data, year==max_year),
                     aes(x=char_no, y=year, label=no_label),
                     color="steelblue2", size=5, vjust=-2, fontface="bold") + 
           geom_text(data=data, aes(x=char_yes, y=year, label=char_yes),
                     color="steelblue4", size=5, vjust=2.5) + 
           geom_text(data=data, color="steelblue2", size=5, vjust=2.5,
                     aes(x=char_no, y=year, label=char_no)) + 
           geom_text(data=data, aes(label=diff, y=year, x=max(c(data$char_no,data$char_yes))*1.1), color="#7a7d7e", size=5, vjust =2.5) +
           geom_text(data=filter(data, year==max_year), aes(x=max(c(data$char_no,data$char_yes))*1.1, y=year, label="diff"),
                     color="#7a7d7e", size=5, vjust=-2) + 
           scale_x_continuous(expand=c(0,0), limits=c(min(c(data$char_no,data$char_yes))*0.6, max(c(data$char_no,data$char_yes))*1.2)) + 
           labs(x=NULL, y=NULL) + 
           theme_bw() + 
           theme(panel.border=element_blank(),
                 axis.ticks=element_blank(),
                 axis.text.x=element_blank(),
                 plot.title=element_text(size=9, margin=margin(b=12),hjust=-0.2,face="bold"),
                 axis.text=element_text(size=12)) 
  )
  
}

