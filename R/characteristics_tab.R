# Characteristics tab

reason_order_bar <- c('4 and under',
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
                      '19 and over')

reason_order_ethn_plot <- c(
  
  "Ethnicity_Major_White_Total"   ,             
  "Ethnicity_Minor_White_British"         ,      
  "Ethnicity_Minor_Irish"                ,      
  "Ethnicity_Minor_Traveller_of_Irish_heritage" ,
  "Ethnicity_Minor_Gypsy_Roma"                , 
  "Ethnicity_Minor_Any_other_white_background" , 
  "Ethnicity_Major_Mixed_Total"              ,  
  "Ethnicity_Minor_White_and_Black_Caribbean"   ,
  "Ethnicity_Minor_White_and_Black_African"   , 
  "Ethnicity_Minor_White_and_Asian"          ,   
  "Ethnicity_Minor_Any_other_Mixed_background" ,
  "Ethnicity_Major_Asian_Total"           ,      
  "Ethnicity_Minor_Indian"            ,         
  "Ethnicity_Minor_Pakistani"         ,          
  "Ethnicity_Minor_Bangladeshi"       ,         
  "Ethnicity_Minor_Any_other_Asian_background" , 
  "Ethnicity_Major_Black_Total"        ,        
  "Ethnicity_Minor_Black_Caribbean"        ,     
  "Ethnicity_Minor_Black_African"        ,      
  "Ethnicity_Minor_Any_other_black_background"  ,
  "Ethnicity_Minor_Chinese"             ,       
  "Ethnicity_Minor_Any_Other_Ethnic_Group"  ,    
  "Ethnicity_Minority_ethnic_pupil"      ,      
  "Ethnicity_Unclassified" ,
  "Total")

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

button_ethnicity_group <- data.frame( measure = c('White Total',               
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
                                'Total'),
                    group = c('Major Ethnic Grouping',
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
                              'Major Ethnic Grouping')) 
                            
                          

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
      scale_fill_manual("", guide = guide_legend(reverse=TRUE, nrow = 1), values = gov_cols_2[c(1,3,10,11)]) +
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
            axis.text = element_blank(), 
            axis.title = element_blank(), 
            legend.text = element_text(size = rel(1.2)), 
            strip.text = element_text(size = rel(0.8)), 
            plot.margin = unit(c(0, 0, 0, 0), "lines"), complete = TRUE,
            title = element_text(ylabtitle),
            legend.position = "bottom",
            legend.title=element_blank(),
            plot.title = element_text(size = 14, face = "bold", hjust = 0)) + 
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
        geom_text(aes(label=paste(format(round(y_var*100, 1)), "%", sep = ""), nsmall = 1), position=position_dodge(width=0.9), vjust=-0.25) +
        theme(line = element_blank(), 
              rect = element_blank(), 
              legend.text = element_blank(), 
              strip.text = element_text(size = rel(0.8)), 
              plot.margin = unit(c(0, 0, 0, 0), "lines"), complete = TRUE,
              title = element_text(ylabtitle),
              legend.position = "bottom",
              legend.title=element_blank(),
              plot.title = element_text(size = 14, face = "bold", hjust = 0)) + 
        labs(x = "Age", y = "") +
        ggtitle("Distribution of fixed period exclusions by major ethnic group, 2015/16")) }
  
  
  
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
        mutate(characteristic_1 = factor(characteristic_1, levels = reason_order_bar)) %>%
        ggplot +
        aes(x = as.factor(characteristic_1), 
            y = as.numeric(y_var)) +
        geom_bar(stat = "identity", fill = "#2B8CC4") +
        scale_y_continuous(limits = c(0,max(d$y_var)+0.1)) +
        geom_text(aes(label=paste(format(round(y_var*100, 1)), "%", sep = ""), nsmall = 1), position=position_dodge(width=0.9), vjust=-0.25) +
        theme(line = element_blank(), 
              rect = element_blank(), 
              legend.text = element_blank(), 
              strip.text = element_text(size = rel(0.8)), 
              plot.margin = unit(c(0, 0, 0, 0), "lines"), complete = TRUE,
              title = element_text(ylabtitle),
              legend.position = "bottom",
              legend.title=element_blank(),
              plot.title = element_text(size = 14, face = "bold", hjust = 0)) + 
        labs(x = "Age", y = "") +
        ggtitle(ylabtitle)) 
    
   
  }
  
  
  }



char_series <- function(char, sch_type, category) {
  
  if (char =='gender') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('Gender', 'Total'), school_type == sch_type)
  } else if (char =='sen') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('SEN_provision', 'Total'), school_type == sch_type) 
  } else if (char =='fsm') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('FSM_Eligible', 'Total'), school_type == sch_type) 
  } else if (char =='ethn') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('Ethnicity', 'Total'), school_type == sch_type) 
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
  
  if (char =='gender' | char =='sen' | char =='fsm') {
  return(
    d %>%
      ggplot +
      aes(x = as.factor(formatyr(year)), 
          y = as.numeric(y_var), 
          group = characteristic_1, colour = as.factor(characteristic_1)) +
      geom_path(size = 1) +
      scale_colour_manual(values = gov_cols_2) +
      xlab("Academic year") +
      ylab(ylabtitle) +
      scale_y_continuous(limits = c(0, max(as.numeric(d$y_var))*1.1)) +
      theme_gov() +
      geom_text(
        d = d %>% filter(year == min(as.numeric(year))+101),
        aes(label = characteristic_1),
        size = 5,
        hjust = 0,
        vjust = -1) +
      theme(legend.position = "none") +
      theme(axis.text=element_text(size=12),
            axis.title=element_text(size=14,face="bold")))}
  
  
  else if (char =='age') {
    return(
      
      d %>%
        ggplot +
        aes(x = as.factor(formatyr(year)), 
            y = as.numeric(y_var), 
            group = characteristic_1, colour = as.factor(characteristic_1)) +
        geom_path(size = 1) +
        xlab("Academic year") +
        ylab(ylabtitle) +
        scale_colour_manual(values = gov_cols_2) +
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
              axis.title=element_text(size=14,face="bold")))}
  
}




char_series_age <- function(char, sch_type, category, input) {
  
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
  
  if (char =='gender' | char =='sen' | char =='fsm') {
    return(
      d %>%
        ggplot +
        aes(x = as.factor(formatyr(year)), 
            y = as.numeric(y_var), 
            group = characteristic_1, colour = as.factor(characteristic_1)) +
        geom_path(size = 1) +
        scale_colour_manual(values = gov_cols_2) +
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
              axis.title=element_text(size=14,face="bold")))}
  
  
  else if (char =='age') {
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
              axis.title=element_text(size=14,face="bold")))}
  
}


Radio_Button_Ethnicity <- c("Major Ethnic Grouping", "Minor Ethnic Grouping")
List_Of_Ethnicities <- c()

char_series_ethn <- function(char, sch_type, category, Radio_Button_Ethnicity, List_Of_Ethnicities) {
  
  if (char =='gender') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('Gender', 'Total'), school_type == sch_type)
  } else if (char =='sen') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('SEN_provision', 'Total'), school_type == sch_type) 
  } else if (char =='fsm') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('FSM_Eligible', 'Total'), school_type == sch_type) 
  } else if (char =='age') {
    d <- nat_char_prep %>% filter(characteristic_desc %in% c('Age', 'Total'), school_type == sch_type) 
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
      filter(ethnic_level %in% Radio_Button_Ethnicity)
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
  
  if (char =='gender' | char =='sen' | char =='fsm') {
    return(
      d %>%
        ggplot +
        aes(x = as.factor(formatyr(year)), 
            y = as.numeric(y_var), 
            group = characteristic_1, colour = as.factor(characteristic_1)) +
        geom_path(size = 1) +
        scale_colour_manual(values = gov_cols_2) +
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
              axis.title=element_text(size=14,face="bold")))}
  
  
  
  
  else if (char =='ethn') {
    return(
      
      
      
      d %>%
        mutate(ethnic_level = ifelse(grepl("Total", characteristic_1), "Major Ethnic Grouping", "Minor Ethnic Grouping")) %>%
        filter(ethnic_level %in% Radio_Button_Ethnicity & characteristic_1 %in% List_Of_Ethnicities) %>%
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
              axis.title=element_text(size=14,face="bold")))}
  
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



reason_order_plot <- c(
  'Age 4 and under',
  'Age 5',
  'Age 6',
  'Age 7',
  'Age 8',
  'Age 9',
  'Age 10',
  'Age 11',                                              
  'Age 12',                                                         
  'Age 13',                                                         
  'Age 14',                                                           
  'Age 15',                                                           
  'Age 16',
  'Age 17',
  'Age 18',
  'Age 19 and over',
  'Total')



reason_order_table <- c('4 and under',
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
                      'Total')


# reason_order_ethn_plot <- c()


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
    
    d$characteristic_1 <- factor(d$characteristic_1, levels = reason_order_table[1:17]) 
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

