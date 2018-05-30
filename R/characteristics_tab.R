# Characteristics tab

#---------------------------------------------------------------------
#Read in data required
nat_char_prep <- read_csv('data/nat_char_prep.csv', col_types = cols(.default = "c"))

#---------------------------------------------------------------------
#Ordering for outputs

age_order_bar <- c(
  '4 and under',
  '5',
  '6',
  '7',
  '8',
  '9',
  '10',
  '11',
  '12',
  '13',
  '14',
  '15',
  '16',
  '17',
  '18',
  '19 and over'
)

age_order_table <- c(
  '4 and under',
  '5',
  '6',
  '7',
  '8',
  '9',
  '10',
  '11',
  '12',
  '13',
  '14',
  '15',
  '16',
  '17',
  '18',
  '19 and over',
  'Total'
)

button_ethnicity_group <-
  data.frame(
    measure = c(
      'White Total',
      'White British' ,
      'White Irish',
      'Traveller of Irish Heritage',
      'Gypsy Roma',
      'Any other White background',
      'Mixed Total',
      'White and Black Carribbean',
      'White and Black African',
      'White and Asian',
      'Any other Mixed background',
      'Asian Total',
      'Indian',
      'Pakistani',
      'Bangladeshi',
      'Any other Asian background',
      'Black Total',
      'Black Caribbean',
      'Black African',
      'Any other Black background',
      'Chinese',
      'Any other Ethnic group',
      'Minority Ethnic pupil',
      'Unclassified',
      'Total'
    ),
    group = c(
      'Major Ethnic Grouping',
      'Minor Ethnic Grouping',
      'Minor Ethnic Grouping',
      'Minor Ethnic Grouping',
      'Minor Ethnic Grouping',
      'Minor Ethnic Grouping',
      'Major Ethnic Grouping',
      'Minor Ethnic Grouping',
      'Minor Ethnic Grouping',
      'Minor Ethnic Grouping',
      'Minor Ethnic Grouping',
      'Major Ethnic Grouping',
      'Minor Ethnic Grouping',
      'Minor Ethnic Grouping',
      'Minor Ethnic Grouping',
      'Minor Ethnic Grouping',
      'Major Ethnic Grouping',
      'Minor Ethnic Grouping',
      'Minor Ethnic Grouping',
      'Minor Ethnic Grouping',
      'Major Ethnic Grouping',
      'Minor Ethnic Grouping',
      'Minor Ethnic Grouping',
      'Minor Ethnic Grouping',
      'Major Ethnic Grouping'
    )
  ) 
                            
reason_order_ethn_plot_2 <- c(
  'White Total',
  'White British' ,
  'White Irish',
  'Traveller of Irish Heritage',
  'Gypsy Roma',
  'Any other White background',
  'Mixed Total',
  'White and Black Carribbean',
  'White and Black African',
  'White and Asian',
  'Any other Mixed background',
  'Asian Total',
  'Indian',
  'Pakistani',
  'Bangladeshi',
  'Any other Asian background',
  'Black Total',
  'Black Caribbean',
  'Black African',
  'Any other Black background',
  'Chinese',
  'Any other Ethnic group',
  'Minority Ethnic pupil',
  'Unclassified',
  'Total'
)

#---------------------------------------------------------------------
#Time series table

char_series_table <- function(char, sch_type, category, table_ethn_measure) {
  
  if (char =='gender') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('Gender', 'Total'), school_type == sch_type)
  } else if (char =='sen') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('SEN_provision', 'Total'), school_type == sch_type) 
  } else if (char =='fsm') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('FSM_Eligible', 'Total'), school_type == sch_type) 
  } else if (char =='ethn') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('Ethnicity', 'Total'), school_type == sch_type) %>%
      mutate(characteristic_1 = dplyr::recode(characteristic_1,
                                              Ethnicity_Major_White_Total = 'White Total',               
                                              Ethnicity_Minor_White_British  = 'White British' ,            
                                              Ethnicity_Minor_Irish = 'White Irish'     ,                
                                              Ethnicity_Minor_Traveller_of_Irish_heritage = 'Traveller of Irish Heritage' ,
                                              Ethnicity_Minor_Gypsy_Roma = 'Gypsy Roma' ,                
                                              Ethnicity_Minor_Any_other_white_background = 'Any other White background',  
                                              Ethnicity_Major_Mixed_Total = 'Mixed Total'         ,      
                                              Ethnicity_Minor_White_and_Black_Caribbean = 'White and Black Carribbean'  ,
                                              Ethnicity_Minor_White_and_Black_African = 'White and Black African' ,  
                                              Ethnicity_Minor_White_and_Asian = 'White and Asian'     ,       
                                              Ethnicity_Minor_Any_other_Mixed_background = 'Any other Mixed background',
                                              Ethnicity_Major_Asian_Total = 'Asian Total'                ,
                                              Ethnicity_Minor_Indian  = 'Indian'               ,    
                                              Ethnicity_Minor_Pakistani = 'Pakistani'         ,         
                                              Ethnicity_Minor_Bangladeshi = 'Bangladeshi'      ,         
                                              Ethnicity_Minor_Any_other_Asian_background = 'Any other Asian background',  
                                              Ethnicity_Major_Black_Total = 'Black Total'            ,   
                                              Ethnicity_Minor_Black_Caribbean = 'Black Caribbean'     ,       
                                              Ethnicity_Minor_Black_African = 'Black African'          ,   
                                              Ethnicity_Minor_Any_other_black_background = 'Any other Black background' ,  
                                              Ethnicity_Minor_Chinese = 'Chinese'                   ,
                                              Ethnicity_Minor_Any_Other_Ethnic_Group = 'Any other Ethnic group'    ,  
                                              Ethnicity_Minority_ethnic_pupil = 'Minority Ethnic pupil'           ,
                                              Ethnicity_Unclassified = 'Unclassified' ,
                                              Total = 'Total')) %>% 
      mutate(characteristic_1 = factor(characteristic_1, levels = reason_order_ethn_plot_2 )) %>%
      mutate(ethnic_level = ifelse(grepl("Total", characteristic_1), "Major Ethnic Grouping", "Minor Ethnic Grouping")) %>%
      filter(ethnic_level %in% table_ethn_measure)
    
  } else if (char =='age') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('Age', 'Total'), school_type == sch_type) %>%
      mutate(characteristic_1 = dplyr::recode(characteristic_1,
                                              `Age 4 and under`="4 and under",
                                              `Age 5`= "5",
                                              `Age 6`="6",
                                              `Age 7`= "7",
                                              `Age 8`="8",
                                              `Age 9`="9",
                                              `Age 10`="10",
                                              `Age 11` = "11",                                              
                                              `Age 12` = "12",                                                         
                                              `Age 13` = "13",                                                         
                                              `Age 14` = "14",                                                           
                                              `Age 15` = "15",                                                           
                                              `Age 16` = "16",
                                              `Age 17` = "17",
                                              `Age 18` = "18",
                                              `Age 19 and over`= "19 and over",
                                              `Total` = "Total")) 
    
    d$characteristic_1 <- factor(d$characteristic_1, levels = age_order_table[1:17]) 
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
  
  colnames(data_wide)[1] <- "Characteristic"

  
  return(data_wide)
  
}

#---------------------------------------------------------------------
#Plot exclusion rates (shown for gender, sen and fsm only)

char_series <- function(char, sch_type, category) {
  
  if (char =='gender') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('Gender', 'Total'), school_type == sch_type)
  } else if (char =='sen') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('SEN_provision', 'Total'), school_type == sch_type) 
  } else if (char =='fsm') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('FSM_Eligible', 'Total'), school_type == sch_type) 
  } else if (char =='ethn') {
    return("")
  } else if (char =='age') {
    return("")
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
      scale_colour_manual(values = gov_cols_2) +
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
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold"),
            text=element_text(family="Arial")))
  
} 

#---------------------------------------------------------------------
#Plot exclusion rates (shown for age only)

char_series_age <- function(char, sch_type, category, input) {
  
  d <- nat_char_prep %>% filter(characteristic_desc %in% c('Age', 'Total'), school_type == sch_type) 
  
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
        filter(characteristic_1 %in% input) %>%
        select(year, characteristic_1, y_var) %>%
        ggplot +
        aes(x = as.factor(formatyr(year)), 
            y = as.numeric(y_var), 
            group = characteristic_1, colour = characteristic_1) +
        geom_path(size = 1) +
        scale_colour_manual(values = gov_cols_2) +
        xlab("Academic year") +
        ylab(ylabtitle) +
        scale_y_continuous(limits = c(0, max(as.numeric(d$y_var))*1.1)) +
        theme_classic() +
        geom_text(
          d = d %>% filter(year == min(as.numeric(year))+101 & characteristic_1 %in% input),
          aes(label = characteristic_1,
          size = 5,
          hjust = 0,
          vjust = -1)) +
        theme(legend.position = "none") +
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold"),
              text=element_text(family="Arial")))
}

#---------------------------------------------------------------------
#Plot exclusion rates (shown for ethnicity only)

Radio_Button_Ethnicity <- c("Major Ethnic Grouping", "Minor Ethnic Grouping")

char_series_ethn <- function(char, sch_type, category, Radio_Button_Ethnicity, List_Of_Ethnicities) {
  
  d <- nat_char_prep %>% filter(characteristic_desc %in% c('Ethnicity', 'Total'), school_type == sch_type) %>%
      mutate(characteristic_1 = dplyr::recode(characteristic_1,
                                              Ethnicity_Major_White_Total = 'White Total',               
                                              Ethnicity_Minor_White_British  = 'White British' ,            
                                              Ethnicity_Minor_Irish = 'White Irish'     ,                
                                              Ethnicity_Minor_Traveller_of_Irish_heritage = 'Traveller of Irish Heritage' ,
                                              Ethnicity_Minor_Gypsy_Roma = 'Gypsy Roma' ,                
                                              Ethnicity_Minor_Any_other_white_background = 'Any other White background',  
                                              Ethnicity_Major_Mixed_Total = 'Mixed Total'         ,      
                                              Ethnicity_Minor_White_and_Black_Caribbean = 'White and Black Carribbean'  ,
                                              Ethnicity_Minor_White_and_Black_African = 'White and Black African' ,  
                                              Ethnicity_Minor_White_and_Asian = 'White and Asian'     ,       
                                              Ethnicity_Minor_Any_other_Mixed_background = 'Any other Mixed background',
                                              Ethnicity_Major_Asian_Total = 'Asian Total'                ,
                                              Ethnicity_Minor_Indian  = 'Indian'               ,    
                                              Ethnicity_Minor_Pakistani = 'Pakistani'         ,         
                                              Ethnicity_Minor_Bangladeshi = 'Bangladeshi'      ,         
                                              Ethnicity_Minor_Any_other_Asian_background = 'Any other Asian background',  
                                              Ethnicity_Major_Black_Total = 'Black Total'            ,   
                                              Ethnicity_Minor_Black_Caribbean = 'Black Caribbean'     ,       
                                              Ethnicity_Minor_Black_African = 'Black African'          ,   
                                              Ethnicity_Minor_Any_other_black_background = 'Any other Black background' ,  
                                              Ethnicity_Minor_Chinese = 'Chinese'                   ,
                                              Ethnicity_Minor_Any_Other_Ethnic_Group = 'Any other Ethnic group'    ,  
                                              Ethnicity_Minority_ethnic_pupil = 'Minority Ethnic pupil'           ,
                                              Ethnicity_Unclassified = 'Unclassified' ,
                                              Total = 'Total')) %>%
      mutate(characteristic_1 = factor(characteristic_1, levels = reason_order_ethn_plot_2 )) %>%
      mutate(ethnic_level = ifelse(grepl("Total", characteristic_1), "Major Ethnic Grouping", "Minor Ethnic Grouping")) %>% 
      filter(ethnic_level %in% Radio_Button_Ethnicity)

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
        mutate(ethnic_level = ifelse(grepl("Total", characteristic_1), "Major Ethnic Grouping", "Minor Ethnic Grouping")) %>%
        filter(ethnic_level %in% Radio_Button_Ethnicity & characteristic_1 %in% List_Of_Ethnicities) %>%
        select(year, characteristic_1, y_var) %>%
        ggplot +
        aes(x = as.factor(formatyr(year)), 
            y = as.numeric(y_var), 
            group = characteristic_1, colour = characteristic_1) +
        scale_colour_manual(values = gov_cols_2) +
        geom_path(size = 1) +
        xlab("Academic year") +
        ylab(ylabtitle) +
        scale_y_continuous(limits = c(0, max(as.numeric(d$y_var))*1.1)) +
        theme_classic() +
        geom_text(
          d = d %>% mutate(characteristic_1 = dplyr::recode(characteristic_1,
                                                            Ethnicity_Major_White_Total = 'White Total',               
                                                            Ethnicity_Minor_White_British  = 'White British' ,            
                                                            Ethnicity_Minor_Irish = 'White Irish'     ,                
                                                            Ethnicity_Minor_Traveller_of_Irish_heritage = 'Traveller of Irish Heritage' ,
                                                            Ethnicity_Minor_Gypsy_Roma = 'Gypsy Roma' ,                
                                                            Ethnicity_Minor_Any_other_white_background = 'Any other White background',  
                                                            Ethnicity_Major_Mixed_Total = 'Mixed Total'         ,      
                                                            Ethnicity_Minor_White_and_Black_Caribbean = 'White and Black Carribbean'  ,
                                                            Ethnicity_Minor_White_and_Black_African = 'White and Black African' ,  
                                                            Ethnicity_Minor_White_and_Asian = 'White and Asian'     ,       
                                                            Ethnicity_Minor_Any_other_Mixed_background = 'Any other Mixed background',
                                                            Ethnicity_Major_Asian_Total = 'Asian Total'                ,
                                                            Ethnicity_Minor_Indian  = 'Indian'               ,    
                                                            Ethnicity_Minor_Pakistani = 'Pakistani'         ,         
                                                            Ethnicity_Minor_Bangladeshi = 'Bangladeshi'      ,         
                                                            Ethnicity_Minor_Any_other_Asian_background = 'Any other Asian background',  
                                                            Ethnicity_Major_Black_Total = 'Black Total'            ,   
                                                            Ethnicity_Minor_Black_Caribbean = 'Black Caribbean'     ,       
                                                            Ethnicity_Minor_Black_African = 'Black African'          ,   
                                                            Ethnicity_Minor_Any_other_black_background = 'Any other Black background' ,  
                                                            Ethnicity_Minor_Chinese = 'Chinese'                   ,
                                                            Ethnicity_Minor_Any_Other_Ethnic_Group = 'Any other Ethnic group'    ,  
                                                            Ethnicity_Minority_ethnic_pupil = 'Minority Ethnic pupil'           ,
                                                            Ethnicity_Unclassified = 'Unclassified' ,
                                                            Total = 'Total')) %>% filter(year == min(as.numeric(year))+101) %>% 
          mutate(ethnic_level = ifelse(grepl("Total", characteristic_1), "Major Ethnic Grouping", "Minor Ethnic Grouping")) %>%
          filter(ethnic_level %in% Radio_Button_Ethnicity & characteristic_1 %in% List_Of_Ethnicities),
          aes(label = characteristic_1 ,
              size = 5,
              hjust = 0,
              vjust = -1)) +
        theme(legend.position = "none") +
        theme(axis.text=element_text(size=12),
              axis.title=element_text(size=14,face="bold"),
              text=element_text(family="Arial")))
}

ethnicity_data <- function(x){
  
  x %>% filter(characteristic_desc %in% c('Ethnicity', 'Total')) %>%
    mutate(characteristic_1 = dplyr::recode(characteristic_1,
                                            Ethnicity_Major_White_Total = 'White Total',               
                                            Ethnicity_Minor_White_British  = 'White British' ,            
                                            Ethnicity_Minor_Irish = 'White Irish'     ,                
                                            Ethnicity_Minor_Traveller_of_Irish_heritage = 'Traveller of Irish Heritage' ,
                                            Ethnicity_Minor_Gypsy_Roma = 'Gypsy Roma' ,                
                                            Ethnicity_Minor_Any_other_white_background = 'Any other White background',  
                                            Ethnicity_Major_Mixed_Total = 'Mixed Total'         ,      
                                            Ethnicity_Minor_White_and_Black_Caribbean = 'White and Black Carribbean'  ,
                                            Ethnicity_Minor_White_and_Black_African = 'White and Black African' ,  
                                            Ethnicity_Minor_White_and_Asian = 'White and Asian'     ,       
                                            Ethnicity_Minor_Any_other_Mixed_background = 'Any other Mixed background',
                                            Ethnicity_Major_Asian_Total = 'Asian Total'                ,
                                            Ethnicity_Minor_Indian  = 'Indian'               ,    
                                            Ethnicity_Minor_Pakistani = 'Pakistani'         ,         
                                            Ethnicity_Minor_Bangladeshi = 'Bangladeshi'      ,         
                                            Ethnicity_Minor_Any_other_Asian_background = 'Any other Asian background',  
                                            Ethnicity_Major_Black_Total = 'Black Total'            ,   
                                            Ethnicity_Minor_Black_Caribbean = 'Black Caribbean'     ,       
                                            Ethnicity_Minor_Black_African = 'Black African'          ,   
                                            Ethnicity_Minor_Any_other_black_background = 'Any other Black background' ,  
                                            Ethnicity_Minor_Chinese = 'Chinese'                   ,
                                            Ethnicity_Minor_Any_Other_Ethnic_Group = 'Any other Ethnic group'    ,  
                                            Ethnicity_Minority_ethnic_pupil = 'Minority Ethnic pupil'           ,
                                            Ethnicity_Unclassified = 'Unclassified' ,
                                            Total = 'Total')) %>%
    mutate(characteristic_1 = factor(characteristic_1, levels = reason_order_ethn_plot_2)) %>%
    mutate(ethnic_level = ifelse(grepl("Total", characteristic_1), "Major Ethnic Grouping", "Minor Ethnic Grouping")) %>%
    distinct(ethnic_level, characteristic_1)
}

#---------------------------------------------------------------------
#Bar chart distribution plot


bar_chart_percentages <- function(char, sch_type, category) {
  
  if (char =='gender') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('Gender'), school_type == sch_type, year == "201516")
  } else if (char =='sen') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('SEN_provision'), school_type == sch_type, year == "201516") 
  } else if (char =='fsm') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('FSM_Eligible'), school_type == sch_type, year == "201516") 
  } else if (char =='ethn') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('Ethnicity', 'Total'), school_type == sch_type, year == "201516") %>% filter(characteristic_1 == "Ethnicity_Major_White_Total" | characteristic_1 == "Ethnicity_Major_Mixed_Total" | characteristic_1 == "Ethnicity_Major_Mixed_Total" | characteristic_1 == "Ethnicity_Major_Asian_Total" | characteristic_1 == "Ethnicity_Major_Black_Total" | characteristic_1 == "Ethnicity_Minor_Chinese" | characteristic_1 == "Ethnicity_Minor_Any_Other_Ethnic_Group") 
  } else if (char =='age') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('Age'), school_type == sch_type, year == "201516")

  }
  
  if (category == 'P') {
    ylabtitle <- "Distribution of permanent exclusions, 2015/16 \n"
    d <- d %>% group_by (year) %>% filter(perm_excl != 'x') %>% mutate(y_var = as.numeric(perm_excl) / sum(as.numeric(perm_excl))) %>% filter(y_var != 'x')
  } else if (category == 'F') {
    ylabtitle <- "Distribution of fixed period exclusion, 2015/16 \n"
    d <- d %>% group_by (year) %>% filter(fixed_excl != 'x') %>% mutate(y_var = as.numeric(fixed_excl) / sum(as.numeric(fixed_excl))) %>% filter(y_var != 'x')
  } else if (category == 'O') {
    ylabtitle <- "Distribution of pupils with one or more fixed period exclusion, 2015/16 \n"
    d <- d %>% group_by (year) %>% filter(one_plus_fixed != 'x') %>% mutate(y_var = as.numeric(one_plus_fixed) / sum(as.numeric(one_plus_fixed))) %>% filter(y_var != 'x')
  }
  
  if (char =='gender' | char =='sen' | char =='fsm') {
    return(
      d %>%
        ggplot +
        aes(x = as.factor(characteristic_desc), 
            y = as.numeric(y_var), 
            fill = as.factor(characteristic_1)) +
        geom_bar(stat='identity', size = 1) +
        scale_fill_manual("", guide = guide_legend(reverse=TRUE, nrow = 1), values = gov_cols_2[c(5,6,8,10)]) +
        scale_y_continuous(limits = c(0,1)) + 
        coord_flip() +
        geom_text(aes(label=ifelse(as.numeric(y_var) >= 0.02, 
                                   paste0(sprintf("%.0f", 
                                                  as.numeric(y_var)*100),"%"),"")),
                  position=position_stack(vjust=0.5), 
                  colour="black",
                  size = 8) +
        theme(line = element_blank(), 
              rect = element_blank(), 
              text = element_text(family = "Arial", 
                                  face = "plain", colour = "black", size = 11, lineheight = 0.9, 
                                  hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), 
                                  debug = FALSE), axis.text = element_blank(), axis.title = element_blank(), 
              legend.text = element_text(size = rel(1.2)), 
              legend.title = element_text(hjust = 0), 
              legend.position = "bottom",
              plot.title = element_text(size = 14, face = "bold", hjust = 0),
              strip.text = element_text(size = rel(0.8)), 
              plot.margin = unit(c(0, 0, 0, 0), "lines"), complete = TRUE) + 
        ggtitle(ylabtitle)) } 
  
  
  if (char =='ethn') {
    return (
      
      d %>%
        mutate(characteristic_1 = dplyr::recode(characteristic_1,
                                                Ethnicity_Major_White_Total="White",
                                                Ethnicity_Major_Mixed_Total= "Mixed",
                                                Ethnicity_Major_Asian_Total="Asian",
                                                Ethnicity_Major_Black_Total= "Black",
                                                Ethnicity_Minor_Chinese="Chinese",
                                                Ethnicity_Minor_Any_Other_Ethnic_Group="Other"))%>%
        mutate(characteristic_1 = factor(characteristic_1, levels = c("White", "Mixed", "Asian", "Black", "Chinese", "Other"))) %>%
        ggplot +
        aes(x = as.factor(characteristic_1), 
            y = as.numeric(y_var)) +
        geom_bar(stat = "identity", fill = "#2B8CC4") +
        scale_y_continuous(limits = c(0,max(d$y_var)+0.1)) +
        geom_text(aes(label=paste(format(round(y_var*100, 1)), "%", sep = "")), position=position_dodge(width=0.9), vjust=-0.25) +
        theme(line = element_blank(), 
              rect = element_blank(), 
              text = element_text(family = "Arial", 
                                  face = "plain", colour = "black", size = 11, lineheight = 0.9, 
                                  hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), 
                                  debug = FALSE),
              legend.text = element_text(size = rel(1.2)), 
              legend.title = element_text(hjust = 0), 
              legend.position = "bottom",
              plot.title = element_text(size = 14, face = "bold", hjust = 0),
              strip.text = element_text(size = rel(0.8)), 
              plot.margin = unit(c(0, 0, 0, 0), "lines"), complete = TRUE) + 
        labs(x = "Major ethnic group", y = "") +
        ggtitle(ylabtitle)) }
  
  else if (char =='age') {
    return (
      
      d %>%
        mutate(characteristic_1 = dplyr::recode(characteristic_1,
                                                `Age 4 and under`="4 and under",
                                                `Age 5`= "5",
                                                `Age 6`="6",
                                                `Age 7`= "7",
                                                `Age 8`="8",
                                                `Age 9`="9",
                                                `Age 10`="10",
                                                `Age 11` = "11",                                              
                                                `Age 12` = "12",                                                         
                                                `Age 13` = "13",                                                         
                                                `Age 14` = "14",                                                           
                                                `Age 15` = "15",                                                           
                                                `Age 16` = "16",
                                                `Age 17` = "17",
                                                `Age 18` = "18",
                                                `Age 19 and over`= "19 and over")) %>% 
        mutate(characteristic_1 = factor(characteristic_1, levels = age_order_bar)) %>%
        ggplot +
        aes(x = as.factor(characteristic_1), 
            y = as.numeric(y_var)) +
        geom_bar(stat = "identity", fill = "#2B8CC4") +
        scale_y_continuous(limits = c(0,max(d$y_var)+0.1)) +
        geom_text(aes(label=paste(format(round(y_var*100, 1)), "%", sep = "")), position=position_dodge(width=0.9), vjust=-0.25) +
        theme(line = element_blank(), 
              rect = element_blank(), 
              text = element_text(family = "Arial", 
                                  face = "plain", colour = "black", size = 11, lineheight = 0.9, 
                                  hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), 
                                  debug = FALSE),
              legend.text = element_text(size = rel(1.2)), 
              legend.title = element_text(hjust = 0), 
              legend.position = "bottom",
              plot.title = element_text(size = 14, face = "bold", hjust = 0),
              strip.text = element_text(size = rel(0.8)), 
              plot.margin = unit(c(0, 0, 0, 0), "lines"), complete = TRUE) + 
        labs(x = "Age", y = "") +
        ggtitle(ylabtitle)) 
  }
}

#---------------------------------------------------------------------
#Download data

characteristics_data_download <- function(char) {
  if (char =='gender') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('Gender', 'Total'))
  } else if (char =='sen') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('SEN_provision', 'Total')) 
  } else if (char =='fsm') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('FSM_Eligible', 'Total')) 
  } else if (char =='age') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('Age', 'Total')) 
  } else if (char =='ethn') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('Ethnicity', 'Total'))
  }
}

#---------------------------------------------------------------------
#Text to explain how the tab is working

characteristics_text_explainer <- function(char, sch_type, category) {
  
  if (char =='gender') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('Gender'), school_type == sch_type, year == "201516")
    
  } else if (char =='sen') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('SEN_provision'), school_type == sch_type, year == "201516") 
    
  } else if (char =='fsm') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('FSM_Eligible'), school_type == sch_type, year == "201516") 
    
  } else if (char =='ethn') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('Ethnicity', 'Total'), school_type == sch_type, year == "201516") %>% filter(characteristic_1 == "Ethnicity_Major_White_Total" | characteristic_1 == "Ethnicity_Major_Mixed_Total" | characteristic_1 == "Ethnicity_Major_Mixed_Total" | characteristic_1 == "Ethnicity_Major_Asian_Total" | characteristic_1 == "Ethnicity_Major_Black_Total" | characteristic_1 == "Ethnicity_Minor_Chinese" | characteristic_1 == "Ethnicity_Minor_Any_Other_Ethnic_Group") 
    
  } else if (char =='age') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('Age'), school_type == sch_type, year == "201516")
    
  } 
  if (category == 'P') {
    
    return(paste("The below data refers to permanent school exclusions data for pupils in England by their", 
 
                 
                if (unique(d$characteristic_desc) == 'Gender') {
                  paste(" gender of pupils. We show a time series table of data and a graph of permanent exclusions by a pupils gender, and below that a plot to represent the proportion of all permanent exclusions by gender in 2015/16.")
    
                                 
                 }  else if (unique(d$characteristic_desc) == 'SEN_provision') {
                   paste(" special educational needs (SEN) provision status. We show a time series table of data and a graph of permanent exclusion rates by SEN status, and below that a plot to represent the proportion of all permanent exclusions by SEN status in 2015/16.")
                 
                  } else if (unique(d$characteristic_desc) == 'FSM_Eligible') {
                   paste(" free school meal (FSM) eligibility of pupils. We show a time series table of data and a graph of permanent exclusions by FSM eligibility, and below that a plot to represent the proportion of all permanent exclusions by pupil FSM eligbility in 2015/16.")

                  } else if (unique(d$characteristic_desc) == 'Ethnicity') {
                    paste(" ethnicity of pupils. We show a time series table of data and a graph of permanent exclusions by pupils ethnicity, and below that a plot to represent the proportion of all permanent exclusions by pupil ethnicity in 2015/16. In the graphical plot, you can switch between major and minor ethncic groupings using the radio buttons provided.")
 
                  } else if (unique(d$characteristic_desc) == 'Age') {
                    paste(" age of pupils. We show a time series table of data and a graph of permanent exclusions by pupils age, and below that a plot to represent the proportion of all permanent exclusions by pupil age in 2015/16. In the time series graph, you can add or remove age groups using the radio buttons provided.")
                    
                  } else 
                   paste("")
                 
                 
    ))
    
  } else if (category == 'F') {
    
    return(paste("The below data refers to fixed period school exclusions data for pupils in England by their", 
                 
                 if (unique(d$characteristic_desc) == 'Gender') {
                   paste(" gender of pupils. We show a time series table of data and a graph of fixed period exclusions by a pupils gender, and below that a plot to represent the proportion of all fixed period exclusions by gender in 2015/16.")
                   
                   
                 }  else if (unique(d$characteristic_desc) == 'SEN_provision') {
                   paste(" special educational needs (SEN) provision status. We show a time series table of data and a graph of fixed period exclusion rates by SEN status, and below that a plot to represent the proportion of all fixed period exclusions by SEN status in 2015/16.")
                   
                 } else if (unique(d$characteristic_desc) == 'FSM_Eligible') {
                   paste(" free school meal (FSM) eligibility of pupils. We show a time series table of data and a graph of fixed period exclusions by FSM eligibility, and below that a plot to represent the proportion of all fixed period exclusions by pupil FSM eligbility in 2015/16.")
                   
                 } else if (unique(d$characteristic_desc) == 'Ethnicity') {
                   paste(" ethnicity of pupils. We show a time series table of data and a graph of fixed period exclusions by pupils ethnicity, and below that a plot to represent the proportion of all fixed period exclusions by pupil ethnicity in 2015/16. In the graphical plot, you can switch between major and minor ethncic groupings using the radio buttons provided.")
                   
                 } else if (unique(d$characteristic_desc) == 'Age') {
                   paste(" age of pupils. We show a time series table of data and a graph of fixed period exclusions by pupils age, and below that a plot to represent the proportion of all fixed period exclusions by pupil age in 2015/16. In the time series graph, you can add or remove age groups using the radio buttons provided.")
                   
                 } else 
                   paste("")
                 
                 
    ))
    
    
  } else if (category == 'O') {
    
    
    return(paste("The below data refers to one or more fixed period school exclusions data for pupils in England by their", 
                 
                 if (unique(d$characteristic_desc) == 'Gender') {
                   paste(" gender of pupils. We show a time series table of data and a graph of one or more fixed period exclusions by a pupils gender, and below that a plot to represent the proportion of all one or more fixed period exclusions by gender in 2015/16.")
                   
                   
                 }  else if (unique(d$characteristic_desc) == 'SEN_provision') {
                   paste(" special educational needs (SEN) provision status. We show a time series table of data and a graph of one or more fixed period exclusion rates by SEN status, and below that a plot to represent the proportion of all one or more fixed period exclusions by SEN status in 2015/16.")
                   
                 } else if (unique(d$characteristic_desc) == 'FSM_Eligible') {
                   paste(" free school meal (FSM) eligibility of pupils. We show a time series table of data and a graph of one or more fixed period exclusions by FSM eligibility, and below that a plot to represent the proportion of all one or more fixed period exclusions by pupil FSM eligbility in 2015/16.")
                   
                 } else if (unique(d$characteristic_desc) == 'Ethnicity') {
                   paste(" ethnicity of pupils. We show a time series table of data and a graph of one or more fixed period exclusions by pupils ethnicity, and below that a plot to represent the proportion of all one or more fixed period exclusions by pupil ethnicity in 2015/16. In the graphical plot, you can switch between major and minor ethncic groupings using the radio buttons provided.")
                   
                 } else if (unique(d$characteristic_desc) == 'Age') {
                   paste(" age of pupils. We show a time series table of data and a graph of one or more fixed period exclusions by pupils age, and below that a plot to represent the proportion of all one or more fixed period exclusions by pupil age in 2015/16. In the time series graph, you can add or remove age groups using the radio buttons provided.")
                   
                 } else 
                   paste("")
                 
    ))
    
  }
  
}