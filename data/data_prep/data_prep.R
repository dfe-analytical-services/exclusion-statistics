## Data prep - this is done outside of the application with csv outputs then used to input the app.

####--------------------------------------------------------
## Front page 
# CSV file for bar chart input

main_ud <- read_csv('data/SFR35_2017_national_region_la_school_data.csv', col_types = cols(.default = "c"))

reason_ud <- read_csv("data/SFR35_2017_reason_for_exclusion.csv", col_types = cols(.default = "c"))

char_ud <- read_csv('data/SFR35_2017_National_characteristics.csv', col_types = cols(.default = "c"))

school_names_raw <- read_csv('data/get_schools_information.csv', col_types = cols(.default = "c"))


####--------------------------------------------------------
## Front page 
# CSV file for bar chart input

nat_summary_csv <-
  dplyr::select(
    filter(main_ud, level == 'National'),
    year,
    school_type,
    num_schools,
    headcount,
    perm_excl,
    perm_excl_rate,
    fixed_excl,
    fixed_excl_rate,
    one_plus_fixed,
    one_or_more_fixed_excl_rate
  ) %>%
  arrange(year)

write.csv(nat_summary_csv, file = "data/nat_summary.csv", row.names = FALSE)

####--------------------------------------------------------
## Characteristics page 
# CSV file for ...

nat_char_prep_csv <- filter(char_ud, ! characteristic_1 %in% c("SEN_provision_Unclassified","FSM_Unclassified","Age_unclassified")) %>%
  mutate(school_type = ifelse(
    school_type == "State-funded primary","Primary",
    ifelse(school_type == "State-funded secondary","Secondary",
           ifelse(school_type == "Special", "Special", 
                  ifelse(school_type == "Total", "Total", "NA"))))) 

nat_char_prep_csv$characteristic_1 <- recode(nat_char_prep_csv$characteristic_1,
                                         Gender_male="Boys",
                                         Gender_female= "Girls",
                                         SEN_Provision_No_SEN="No SEN",
                                         SEN_provision_SEN_with_statement_EHC= "SEN with statement or EHC",
                                         SEN_provision_SEN_without_statement="SEN without a statement or EHC",
                                         FSM_Eligible="FSM eligible",
                                         FSM_NotEligible="FSM not eligible",
                                         Age_4_and_under = "Age 4 and under",                                              
                                         Age_5 = "Age 5",                                                         
                                         Age_6 = "Age 6",                                                         
                                         Age_7 = "Age 7",                                                           
                                         Age_8 = "Age 8",                                                           
                                         Age_9 = "Age 9",   
                                         Age_10 = "Age 10",                                                          
                                         Age_11 = "Age 11",                                                           
                                         Age_12 = "Age 12",                                                         
                                         Age_13 = "Age 13",                                                         
                                         Age_14 = "Age 14",                                                         
                                         Age_15 = "Age 15",                                                         
                                         Age_16 = "Age 16",                                                         
                                         Age_17 = "Age 17",                                                         
                                         Age_18 = "Age 18",                                                         
                                         Age_19_and_over = "Age 19 and over"                                                 
                                         
)

write.csv(nat_char_prep_csv, file = "data/nat_char_prep.csv", row.names = FALSE)


####--------------------------------------------------------
## LA trends page 
# CSV file for ...

clean_la_data_csv <- dplyr::select(
    filter(main_ud, level == 'National' | level == 'Local authority' & school_type != 'dummy' & la_name != "."),
    year,
    level,
    la_name,
    school_type,
    num_schools,
    headcount,
    perm_excl,
    perm_excl_rate,
    fixed_excl,
    fixed_excl_rate,
    one_plus_fixed,
    one_or_more_fixed_excl_rate) %>%
    mutate(school_type = ifelse(
      school_type == "state-funded primary","Primary",
      ifelse(school_type == "state-funded secondary","Secondary",
             ifelse(school_type == "special", "Special", 
                    ifelse(school_type == "total", "Total", NA))))) %>%
    mutate(la_name = ifelse(is.na(la_name), "England",
                            ifelse(!is.na(la_name), la_name, NA))) %>%
    filter(!is.na(school_type))

write.csv(clean_la_data_csv, file = "data/clean_la_data.csv", row.names = FALSE)

comparison_la_data_csv <- dplyr::select(
    filter(main_ud, (level == 'National' | level == 'Region' | level == 'Local authority') & school_type == 'total'),
    year,
    school_type,
    level,
    region_name,
    la_name,
    perm_excl_rate,
    fixed_excl_rate,
    one_or_more_fixed_excl_rate) %>%
    mutate(area = ifelse(is.na(la_name) & is.na(region_name), "England",
                         ifelse(is.na(la_name), region_name,la_name ))) 

write.csv(comparison_la_data_csv, file = "data/comparison_la_data.csv", row.names = FALSE)  


####--------------------------------------------------------
## Reason page 

clean_la_exclusions_data_csv <- dplyr::select(
    filter(reason_ud, level == 'National' | level == 'Local authority' & school_type != 'dummy'),
    year,
    la_name,
    school_type,
    perm_physical_pupils,
    perm_physical_adult,
    perm_verbal_pupil,          
    perm_verbal_adult,
    perm_bullying,
    perm_racist_abuse,          
    perm_sexual_misconduct,
    perm_drug_alcohol,
    perm_damage,                
    perm_theft,
    perm_persistent_disruptive,
    perm_other,
    fixed_physical_pupils,
    fixed_physical_adult,
    fixed_verbal_pupil,         
    fixed_verbal_adult,
    fixed_bullying,
    fixed_racist_abuse,         
    fixed_sexual_misconduct,
    fixed_drug_alcohol,
    fixed_damage,
    fixed_theft,
    fixed_persistent_disruptive,
    fixed_other) %>%
    mutate(la_name = ifelse(is.na(la_name), "England",
                            ifelse(!is.na(la_name), la_name, NA))) %>%
    filter(!is.na(school_type))

write.csv(clean_la_exclusions_data_csv, file = "data/clean_la_exclusions_data.csv", row.names = FALSE)  


####--------------------------------------------------------
## Map page 
# CSV file for ...


ukLocalAuthoritises <- shapefile("data/England_LA_2016.shp")

exc_data <- filter(main_ud, level == 'Local authority', school_type == 'total', year ==201516) %>%
  select(old_la_code,perm_excl_rate, fixed_excl_rate, headcount, perm_excl, fixed_excl)

exc_data$perm_excl_rate <- as.numeric(exc_data$perm_excl_rate)
exc_data$fixed_excl_rate <- as.numeric(exc_data$fixed_excl_rate)
exc_data$headcount <- as.numeric(exc_data$headcount)
exc_data$perm_excl <- as.numeric(exc_data$perm_excl)
exc_data$fixed_excl <- as.numeric(exc_data$fixed_excl)

ukLocalAuthoritises <- spTransform(ukLocalAuthoritises, CRS("+proj=longlat +ellps=GRS80"))
englishLocalAuthorities = subset(ukLocalAuthoritises, LA15CD %like% "E") # Code begins with E

englishLocalAuthorityData <- merge(englishLocalAuthorities,
                                   exc_data,
                                   by.x = 'LA_Code',
                                   by.y = 'old_la_code',
                                   all.y = TRUE)

writeOGR(obj=englishLocalAuthorityData, dsn="data/englishLocalAuthorityData", layer="la_map", driver="ESRI Shapefile")


