#library(data.table)
#flights = read.csv('flights14.csv')
df_homicide = read.csv('./csv_data/homicide_data.csv')

library(shiny)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(googleVis)
library(tidyverse)


#### tab1: WHERE? data #### 
# state , crime.sovled, count 

# create dataframe crime type = both
create_crimetype_both <- function(df){
  tmp_df <- df %>% 
    mutate(state = as.character(state)) %>% 
    group_by(state) %>% 
    summarise(count = n()) %>% 
    mutate(crime.solved = 'both')
  tmp_df <- tmp_df[c(1,3,2)]
  return(tmp_df)
}

df_state_homicide <- df_homicide %>% 
  mutate(state = as.character(state), crime.solved = as.character(crime.solved)) %>% 
  group_by(state,crime.solved) %>%
    summarise(count = n())

df_state_homicide <- bind_rows(df_state_homicide,create_crimetype_both(df_homicide)) 

# state, ratio 
df_state_ratio_homicide <- 
  df_state_homicide %>%
  filter(crime.solved == 'Yes' |  crime.solved == 'both') %>% 
  group_by(state) %>% 
  summarise(ratio = sum(count[crime.solved=='Yes'] / count[crime.solved=='both']))
  #mutate(ratio = count[df_state_homicide$crime.solved=='Yes'])
 # summarise(ratio = sum(count[crime.solved=='Yes']) )


# create variable with 'both,yes,no' as choice
#choice_crime_solved <- arrange(unique(df_state_homicide[,"crime.solved"]),crime.solved)
sort_crime_solved_value = sort(unique(df_state_homicide$crime.solved))
choice_crime_solved <-  c(sort_crime_solved_value[1],sort_crime_solved_value[3],sort_crime_solved_value[2],'ratio(yes)')




#### tab2: WHEN data #### 
# sorted count group by state
df_state_sorted_by_count <- df_state_homicide %>% 
  filter(crime.solved == 'both') %>% 
  arrange(desc(count)) 


##  create graph data ##
# each year  
df_year_notSolved <- df_homicide %>% 
  group_by(year,crime.solved) %>% 
  summarise(count = n()) %>% 
  filter(crime.solved=='No') %>% 
  mutate(notSolved = count) %>% 
  select(year,notSolved)


df_year_solved <- df_homicide %>% 
  group_by(year,crime.solved) %>% 
  summarise(count = n()) %>% 
  filter(crime.solved=='Yes') %>% 
  mutate(solved = count) %>% 
  select(year,solved)

df_year_crime_solved<- 
  cbind(df_year_notSolved,df_year_solved) %>% 
  select(year,solved, notSolved)

df_year_all <- df_homicide %>% 
  group_by(year) %>% 
  summarise(count = n())

#### tab3: Who? data #### 
df_filterd_sex_age<- df_homicide %>% 
  filter(victim.sex !="Unknown" & perpetrator.sex!="Unknown" & !victim.age > 200 & perpetrator.age > 0) 

# 1st_graph
df_victim_perpetrator_sex <- df_filterd_sex_age %>% 
  mutate(who = paste0(victim.sex," killed by ",perpetrator.sex)) %>% 
  group_by(who) %>% 
  summarise(count = n())

# 2nd graph
df_homicide_age <- df_filterd_sex_age %>% 
  select(victim.sex,victim.age,perpetrator.sex,perpetrator.age) %>% 
  mutate(victim.sex = paste0( victim.sex," Victim"), perpetrator.sex = paste0( perpetrator.sex," Perpetrator")) 

df_victim_age <- df_homicide_age %>% 
  select(victim.sex,victim.age) %>% 
  transmute(gender_victim_perpetrator =victim.sex , age=victim.age) 

df_perpetrator_age <- df_homicide_age %>% 
  select(perpetrator.sex,perpetrator.age) %>% 
  transmute(gender_victim_perpetrator =perpetrator.sex , age=perpetrator.age)

df_victim_perpetrator_age <- rbind(df_victim_age,df_perpetrator_age)

df_victim_perpetrator_age_linegraph <- df_victim_perpetrator_age %>% 
  group_by(age,gender_victim_perpetrator) %>% 
  summarise(count = n()) %>% 
  spread( gender_victim_perpetrator, count, fill = 0, convert = FALSE, drop = TRUE,sep = NULL)


# df_victim_age %>% filter(age > 100)
# 
 df_homicide %>% filter(victim.age> 100)
 df_homicide %>% filter(perpetrator.age < 1) %>% nrow()
 

#spread(data, key, value, fill = NA, convert = FALSE, drop = TRUE,sep = NULL)

#### tab4: Using what? data #### 

# perpetrator gener :  Male, Female, both 
choice_gender = c('both','Male','Female')


##  create graph data ##
# 1st graph 
# weapon, perpetrator sex, delete victim,perpetrator's sex == unknown 
df_weapon_perpetrator_all<- df_homicide %>% 
  filter(victim.sex != 'Unknown' & perpetrator.sex != 'Unknown') %>% 
  group_by(weapon) %>%
  summarise(count = n()) %>% 
  mutate(perpetrator.sex=as.factor('both'))

df_weapon_perpetrator_all <- df_weapon_perpetrator_all[c(1,3,2)]

df_weapon_perpetrator_gender <- df_homicide %>% 
  filter(victim.sex != 'Unknown' & perpetrator.sex != 'Unknown') %>% 
  group_by(weapon,perpetrator.sex) %>%
  summarise(count = n()) 

df_weapon_perpetrator_all <- bind_rows(df_weapon_perpetrator_all,df_weapon_perpetrator_gender)

df_weapon_perpetrator_all <- arrange(df_weapon_perpetrator_all,desc(count))

#### Group by Gun  ### 
df_weapon_perpetrator_all_gunGroup <- 
df_weapon_perpetrator_all %>% 
  filter(weapon %in% c("Handgun","Shotgun","Firearm","Gun","Rifle")) %>% 
  group_by(perpetrator.sex) %>% 
  summarise(count = sum(count)) %>% 
  mutate(weapon = "Gun")
df_weapon_perpetrator_all_gunGroup <- df_weapon_perpetrator_all_gunGroup[c(3,1,2)]

df_weapon_perpetrator_all_gunGroup = bind_rows(df_weapon_perpetrator_all_gunGroup,df_weapon_perpetrator_all %>% filter(!weapon %in% c("Handgun","Shotgun","Firearm","Gun","Rifle") ))

# 2st grpah : weapson per year  # 
df_weapon_gun_group_year_both <- df_homicide %>% 
  filter(victim.sex != 'Unknown' & perpetrator.sex != 'Unknown' & (weapon %in% c("Handgun","Shotgun","Firearm","Gun","Rifle"))) %>% 
  group_by(year) %>%
  summarise(count = n()) %>% 
  mutate(perpetrator.sex=as.factor('both')) %>% 
  mutate(weapon = "Gun")

df_weapon_perpetrator_year_both<- df_homicide %>% 
  filter(victim.sex != 'Unknown' & perpetrator.sex != 'Unknown' & !(weapon %in% c("Handgun","Shotgun","Firearm","Gun","Rifle"))) %>% 
  group_by(weapon, year) %>%
  summarise(count = n()) %>% 
  mutate(perpetrator.sex=as.factor('both'))


df_weapon_perpetrator_year_both <- bind_rows(df_weapon_perpetrator_year_both,df_weapon_gun_group_year_both)

df_weapon_perpetrator_year_both <- arrange(df_weapon_perpetrator_year_both,desc(count))

df_weapon_perpetrator_gender_year <- df_homicide %>% 
  filter(victim.sex != 'Unknown' & perpetrator.sex != 'Unknown') %>% 
  group_by(weapon,perpetrator.sex, year) %>%
  summarise(count = n()) 

df_weapon_perpetrator_gender_year_gunGroup <- 
  df_weapon_perpetrator_gender_year %>% 
  filter(weapon %in% c("Handgun","Shotgun","Firearm","Gun","Rifle")) %>% 
  group_by(perpetrator.sex,year) %>% 
  summarise(count = sum(count)) %>% 
  mutate(weapon = "Gun")

df_weapon_perpetrator_gender_year_gunGroup <- df_weapon_perpetrator_gender_year_gunGroup[c(4,1,2,3)]

df_weapon_perpetrator_gender_year_gunGroup_all <-  bind_rows(df_weapon_perpetrator_gender_year_gunGroup,df_weapon_perpetrator_gender_year %>% filter(!weapon %in% c("Handgun","Shotgun","Firearm","Gun","Rifle") ))

df_weapon_perpetrator_gender_year_gunGroup_all <- bind_rows(df_weapon_perpetrator_gender_year_gunGroup_all,df_weapon_perpetrator_year_both)

#spread(data, key, value, fill = NA, convert = FALSE, drop = TRUE,sep = NULL)

df_weapon_year_all_graph <- df_weapon_perpetrator_gender_year_gunGroup_all %>% 
  spread(weapon,count,fill = 0, convert = FALSE, drop = TRUE,sep = NULL)


#### tab5: who killed who? data #### 

df_rel_category <-
  df_homicide %>% 
  mutate(relationship = as.character(relationship)) %>% 
  mutate(relationship.category = 
           if_else(relationship %in% c('Acquaintance','Neighbor'),'Acquaintance',
                   if_else(grepl("(friend)",tolower(relationship)),'Friend',
                           if_else(grepl("(wife|husband)",tolower(relationship)),'Husband/Wife',
                                   if_else(grepl("(Stranger|Unknown)",relationship),relationship,
                                           if_else(grepl("^(Employe)",relationship),"Work","Family")))))) 



df_rel_category_groupby <- df_rel_category %>% 
  group_by(relationship.category) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))




df_rel_groupby <- df_rel_category %>% 
  group_by(relationship) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) 
#choice value 
choice_rel_category = c("All",c(distinct(df_rel_category_groupby,relationship.category)))




# add detail : #1 Wife/Husband  
# df_rel_detail <- 
# df_rel_category %>% 
#   mutate(relationship.detil = if_else(relationship == "Common-Law Husband", "Common-Law Husband killed by Common-Law Wife", 
#                                       if_else(relationship == "Common-Law Wife", "Common-Law Wife killed by Common-Law Husband", 
#                                               if_else(relationship == "Ex-Wife", "Ex-Wife killed by Ex-Husband", 
#                                                       if_else(relationship == "Ex-Husband", "Ex-Husband killed by Ex-Wife",
#                                                               if_else(relationship == "Wife", "Wife killed by Husband",
#                                                                       if_else(relationship == "Husband", "Husband killed by Wife",".."
#                                                                       )))))))
# add detail : #2 Work 
df_rel_detail <- df_rel_category %>% 
  mutate(relationship.detail = case_when( relationship == "Employee" ~ "Employee killed by Employer",
                                         relationship == "Employer" ~ "Employer killed by Employee", 
                                         relationship == "Common-Law Husband" ~ "Common-Law Husband killed by Common-Law Wife",
                                         relationship == "Common-Law Wife" ~ "Common-Law Wife killed by Common-Law Husband",
                                         relationship == "Ex-Wife"~ "Ex-Wife killed by Ex-Husband", 
                                         relationship == "Ex-Husband" ~ "Ex-Husband killed by Ex-Wife",
                                         relationship == "Wife"~ "Wife killed by Husband",
                                         relationship == "Husband"~ "Husband killed by Wife",
                                         relationship == "Boyfriend"   ~ "Boyfriend Killed by his Lover",
                                         relationship == "Girlfriend"   ~ "Girlfriend Killed by her Lover",
                                         relationship == "Boyfriend/Girlfriend" & victim.sex == "Male" ~ "Boyfriend Killed by his Lover",
                                         relationship == "Boyfriend/Girlfriend" & victim.sex == "Female" ~ "Girlfriend Killed by her Lover",
                                         relationship == "Friend" & victim.sex == "Male" & perpetrator.sex == "Female" ~ "Friend Male killed by Female",
                                         relationship == "Friend" & victim.sex == "Male" & perpetrator.sex == "Male" ~ "Friend Male killed by Male",
                                         relationship == "Friend" & victim.sex == "Female" & perpetrator.sex == "Female" ~ "Friend Female killed by Female",
                                         relationship == "Friend" & victim.sex == "Female" & perpetrator.sex == "Male" ~ "Friend Female killed by Male",
                                         relationship == "Brother"& perpetrator.sex == "Female" ~ "Brother killed by Sister",
                                         relationship == "Brother"& perpetrator.sex == "Male" ~ "Brother killed by Brother",
                                         relationship == "Sister"& perpetrator.sex == "Female" ~ "Sister killed by Sister",
                                         relationship == "Sister"& perpetrator.sex == "Male" ~ "Sister killed by Brother",
                                         relationship == "Daughter"& perpetrator.sex == "Female" ~ "Daughter killed by Mother",
                                         relationship == "Daughter"& perpetrator.sex == "Male" ~ "Daughter killed by Father",
                                         relationship == "Son"& perpetrator.sex == "Female" ~ "Son killed by Mother",
                                         relationship == "Son"& perpetrator.sex == "Male" ~ "Son killed by Father",
                                         relationship == "Father"& perpetrator.sex == "Female" ~ "Father killed by Daughter",
                                         relationship == "Father"& perpetrator.sex == "Male" ~ "Father killed by Son",
                                         relationship == "Mother"& perpetrator.sex == "Female" ~ "Mother killed by Daughter",
                                         relationship == "Mother"& perpetrator.sex == "Male" ~ "Mother killed by Son",
                                         relationship %in% c("Family","In-Law") ~ "Family",
                                         relationship %in% c("Stepdaughter","Stepson","Stepmother","Stepfather") ~ "Stepfamily",
                                         TRUE                           ~ relationship))

df_rel_detail_groupby <-  df_rel_detail %>% 
  group_by(relationship.detail,relationship.category) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

df_rel_detail_groupby_year<-  df_rel_detail %>% 
  group_by(relationship.detail,relationship.category,year) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) 

df_rel_detail_groupby_year_all <- df_rel_detail_groupby_year %>%  
  group_by(relationship.category,year) %>% 
  summarise(count = sum(count)) %>% 
  spread(relationship.category,count,fill = 0, convert = FALSE, drop = TRUE,sep = NULL) 

# add detail : #3 friend            
          


           
#if_else((relationship =="Acquaintance") & (victim.sex = "Male"), paste0(relationship,":")))

#%>% spread(relationship, count ,fill = 0, convert = FALSE, drop = TRUE,sep = NULL)

#c(distinct(df_rel_category_groupby,relationship.category))
# tmp <-
# df_weapon_perpetrator_gender_year_gunGroup_all %>% group_by(weapon,perpetrator.sex) %>% summarise(count = sum(count))
#df_weapon_perpetrator_gender = arrange(df_weapon_perpetrator_gender, desc(count))
#df_weapon?_victim_gender = arrange(df_weapon_victim_gender, desc(count))
#   
# df_homicide %>% 
#   filter(victim.sex != 'Unknown' & perpetrator.sex != 'Unknown') %>% 
#   group_by(victim.sex) %>%
#   summarise(count = n()) 


# df_tmp_all <- df_homicide %>% 
#   group_by(year,state) %>% 
#   summarise(count = n()) %>% 
#   filter(year > 1996)
# select box value 
#choice_rank_item <- c('Top 5 State', 'Bottom 5 State', 'All States')
# unique(df_year_homicide$state)
# distinct(df_year_homicide,state)
