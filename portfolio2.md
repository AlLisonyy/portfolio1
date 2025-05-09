---
title: "Thesis data cleaning"
output: 
  html_document:
    keep_md: true
---
## Goal
For this portfolio, I plan to use the dataset from my thesis study. The study is a correlational design, so there are many scales and variables that need to be cleaned and organized, such as reverse coding. This portfolio will focus on this process.


``` r
##install packages
##install.packages("ltm")
##install.packages("psych")
##install.packages("ppcor")
##For this portfolio, I would like to use the dataset from my thesis study.
library(psych)
library(haven)
library(ltm)
library(ppcor)
library(tidyverse)
Thesis <- read_sav("Thesis.sav")    
```

## Step 1: clean data

``` r
##The first step is to clean data, and I started with getting rid of participants that did not finished the study, as well as deleting variable that does not have any data, such as recipient name.  
Thesis_clean <- Thesis %>% 
 dplyr::select(-c(StartDate, EndDate, IPAddress, RecordedDate, ResponseId, RecipientLastName, RecipientFirstName, RecipientEmail,  ExternalReference, LocationLatitude, LocationLongitude, DistributionChannel, UserLanguage, robot_timer_First_Click, robot_timer_Last_Click, robot_timer_Page_Submit, robot_timer_Click_Count, browser_info_Browser, browser_info_Version, browser_info_Operating_System, browser_info_Resolution)) %>%
  filter(Progress >= 50 &  ##Keep only participants with progress >= 50
  Consent1 != 2 & ##Remove people's answers who did not give consent to participate in the study
  (!is.na(difficulties)) & ##Remove people's answers who had difficulties when answering questions, their data might be invalid
  Status == 0 &  ##Remove participants whose status is not 0
  Finished == 1
  )
```

## Step 2: reverse coding the scales

``` r
##Define columns that should remain as text, which are my short-answer responses
text_cols <- c("BS_TL_CG_1", "BS_TL_CG_2", "BS_TL_CG_3", "BS_TL_CG_4", "BS_TL_CG_5", 
"BS_TL_Recycle_1", "BS_TL_Recycling_2", "BS_TL_Recycling_3", "BS_TL_Recycling_4", "BS_TL_Recycling_5", 
"BS_TL_Crime_1", "BS_TL_Crime_2", "BS_TL_Crime_3", "BS_TL_Crime_4", "BS_TL_Crime_5", 
"major", "BS_shortanswer1", "BS_shortanswer2", "comment") 

Thesis_clean <- Thesis_clean %>%
  mutate(across(-all_of(text_cols), ~ as.numeric(.)))
```

```
## Warning: There were 3 warnings in
## `mutate()`.
## The first warning was:
## ℹ In argument:
##   `across(-all_of(text_cols),
##   ~as.numeric(.))`.
## Caused by warning:
## ! NAs introduced by coercion
## ℹ Run
##   `dplyr::last_dplyr_warnings()`
##   to see the 2 remaining warnings.
```

``` r
##Convert the numeric columns: I noticed that SPSS assigned the wrong value for the thought rating items, so I have to recoded the wrong value first before reverse-coding it
Thesis_scale <- Thesis_clean %>%
  mutate(
    across(c(BS_TR_CG_1, BS_TR_CG_2, BS_TR_CG_3, BS_TR_CG_4, BS_TR_CG_5, BS_TR_Recylcing_1, BS_TR_Recylcing_2, BS_TR_Recylcing_3,
             BS_TR_Recylcing_4, BS_TR_Recylcing_5, BS_TR_Crime_1, BS_TR_Crime_2, BS_TR_Crime_3, BS_TR_Crime_4, BS_TR_Crime_5), ~ recode(., 
             `4` = 0, `5` = 1, `6` = 2, `7` = 3, `8` = 4, `9` = 5, `10` = 6, `11` = 7, `12` = 8, `13` = 9, `14` = 10))
    )##Thought-Rating for the originally wrong value   
                                                                                                                      
##step II: 
Thesis_scale <- Thesis_scale %>%
  mutate(
    across(c(BPS_1, BPS_2, BPS_3, BPS_6, BPS_9, BPS_11), ~ recode(., 
      `1` = 5, `2` = 4, `4` = 2, `5` = 1)), ##Bullshit Propensity Scale
    
    across(c(BS_TR_CG_1, BS_TR_CG_2, BS_TR_CG_3, BS_TR_CG_4, BS_TR_CG_5, BS_TR_Recylcing_1, BS_TR_Recylcing_2, BS_TR_Recylcing_3, BS_TR_Recylcing_4, BS_TR_Recylcing_5, BS_TR_Crime_1, BS_TR_Crime_2, BS_TR_Crime_3, BS_TR_Crime_4, BS_TR_Crime_5), ~ recode(., 
      `10` = 0, `9` = 1, `8` = 2, `7` = 3, `6` = 4, `4` = 6, `3` = 7, `2` = 8, `1` = 9, `0` = 10)), ##Thought-Rating
    
    across(c(self_esteem_2, self_esteem_5, self_esteem_6, self_esteem_8, self_esteem_9), ~ recode(., 
      `1` = 4, `11` = 3, `12` = 2, `13` = 1)), ##Rosenberg Self-esteem Scale

    across(c(self_esteem_1, self_esteem_3, self_esteem_4, self_esteem_7, self_esteem_10), ~ recode(., 
      `1` = 1, `11` = 2, `12` = 3, `13` = 4)), ##Rosenberg Self-esteem Scale
    
    across(c(ncog3, ncog4, ncog5, ncog7, ncog8, ncog9, ncog12, ncog16, ncog17), ~ recode(., 
      `1` = 5, `2` = 4, `4` = 2, `5` = 1)), ##Need for Cognition
    
    across(c(DTS_Nar_2, DTS_Nar_6, DTS_Nar_8, DTS_Path_2, DTS_Path_7), ~ recode(., 
      `1` = 5, `2` = 4, `4` = 2, `5` = 1)), ##The Short Dark Triad
    
    across(c(NPI_1, NPI_2, NPI_3, NPI_4, NPI_5, NPI_6, NPI_7, NPI_8, NPI_9, NPI_10, NPI_11, NPI_12, NPI_13, NPI_14, NPI_15, NPI_16), ~ recode(., 
      `1` = 2, `2` = 1)), ##Narcissistic Personality Inventory–16
    
    across(c(A4CTS_1, A4CTS_2, A4CTS_3, A4CTS_4, A4CTS_5, A4CTS_6, A4CTS_19, A4CTS_20, A4CTS_21, A4CTS_22, A4CTS_23, A4CTS_24), ~ recode(.,
      `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1)), ##4-Component Thinking Styles Questionnaire
    
    across(c(LSRP_SPS_3, LSRP_SPS_7, LSRP_PPS_10, LSRP_PPS_12, LSRP_PPS_14, LSRP_PPS_16), ~ recode(.,
      `1` = 4, `2` = 3, `3` = 2, `4` = 1)), ##Levenson Self-Report Psychopathy Scale
   
    across(c(aopen3, aopen4, aopen5, aopen7, aopen8), ~ recode(.,
      `1` = 6, `3` = 5, `4` = 4, `5` = 3, `6` = 2, `7` = 1)), ##Actively Open-Minded Thinking about Evidence Scale
    across(c(aopen1, aopen2, aopen6), ~ recode(.,
      `1` = 1, `3` = 2, `4` = 3, `5` = 4, `6` = 5, `7` = 6)),
    
    across(c(nclos2, nclos5, nclos15, nclos12, nclos17, nclos18, nclos19, nclos20, nclos22, nclos24, nclos27, nclos28, nclos34, nclos37, nclos38, nclos42), ~ recode(.,
      `1` = 6, `2` = 5, `3` = 4, `4` = 3, `5` = 2, `6` = 1)), ##Need for closure

    across(c(hex1, hex9, hex10, hex12, hex14, hex15, hex19, hex20, hex21, hex24, hex26, hex28, hex30, hex31, hex32, hex35, hex41, hex42, hex44, hex46, hex48, hex49, hex52, hex53, hex55, hex56, hex57, hex59, hex60), ~ recode(.,
      `1` = 5, `2` = 4, `4` = 2, `5` = 1)) ##HEXACO
    )
```
I realized that I should have create new variables for the recoded ones, so that would be easier for me to keep track on if the numbers are already recoded. I had too many variables this time so I did not do that. 
I also had HEXACO's Honesty-Humility subscale with a low reliability, I ran a more close analysis looking at item correlations and found that I was missing one item for reverse coding... 

## Step 3.1: calculating the Thought listing and rating task

``` r
## For the thought listing and rating task, I need to first identify participants' valid responses. I coded 1 for all valid responses and 0 for all the invalid ones. The criteria is that if the participant answered NA, not sure, or answers suggesting they do not have answers, I code it as 0. I created variables such as CG_score, RC_score, and CR_score for each question. I verify all of the thought rating scores by sliding the thought rating variables next to their respective thought scores (e.g., “thought_rating1” should be next to “thought1_score”)

## How to compute: I Sum the total number of thoughts for each participant per the topic. In other words, if a participant wrote three valid thoughts for topic 1, they should have “3” for their “sum_recoded_TR_topic”.

Thesis_scale <- Thesis_scale %>%
  dplyr::mutate(
    recoded_college = rowSums(dplyr::select(., CG1_score, CG2_score, CG3_score, CG4_score, CG5_score), na.rm = TRUE),
    recoded_recycling = rowSums(dplyr::select(., RC1_score, RC2_score, RC3_score, RC4_score, RC5_score), na.rm = TRUE),
    recoded_crime = rowSums(dplyr::select(., CR1_score, CR2_score, CR3_score, CR4_score, CR5_score), na.rm = TRUE)
  )

#the two version I also tried but did not let me build the website:
#Thesis_scale <- Thesis_scale %>%
  #dplyr::mutate(
   # recoded_college = rowSums(select(., CG1_score, CG2_score, CG3_score, CG4_score, CG5_score), na.rm = TRUE),
    #recoded_recycling = rowSums(select(., RC1_score, RC2_score, RC3_score, RC4_score, RC5_score), na.rm = TRUE),
    #recoded_crime = rowSums(select(., CR1_score, CR2_score, CR3_score, CR4_score, CR5_score), na.rm = TRUE)  )
#Thesis_scale <- Thesis_scale %>%
  #rowwise() %>%
 # dplyr::mutate(
  #  recoded_college = sum(c_across(c(CG1_score, CG2_score, CG3_score, CG4_score, CG5_score)), na.rm = TRUE),
  #  recoded_recycling = sum(c_across(c(RC1_score, RC2_score, RC3_score, RC4_score, RC5_score)), na.rm = TRUE),
  #  recoded_crime = sum(c_across(c(CR1_score, CR2_score, CR3_score, CR4_score, CR5_score)), na.rm = TRUE),  ) %>%
 # ungroup()


## Next, I need to delete the scores in the BS_TR_topic items if I marked "0" in the accordingly topic_score variables. In other words, I need to put "0" in the BS_TR_topic when the participant did not provide valid answers.
##topic: college graduate
Thesis_scale$BS_TR_CG_1 <- ifelse(Thesis_scale$CG1_score == 0, NA_integer_, Thesis_scale$BS_TR_CG_1)
Thesis_scale$BS_TR_CG_2 <- ifelse(Thesis_scale$CG2_score == 0, NA_integer_, Thesis_scale$BS_TR_CG_2)
Thesis_scale$BS_TR_CG_3 <- ifelse(Thesis_scale$CG3_score == 0, NA_integer_, Thesis_scale$BS_TR_CG_3)
Thesis_scale$BS_TR_CG_4 <- ifelse(Thesis_scale$CG4_score == 0, NA_integer_, Thesis_scale$BS_TR_CG_4)
Thesis_scale$BS_TR_CG_5 <- ifelse(Thesis_scale$CG5_score == 0, NA_integer_, Thesis_scale$BS_TR_CG_5)
##topic: recycling
Thesis_scale$BS_TR_Recylcing_1 <- ifelse(Thesis_scale$RC1_score == 0, NA_integer_, Thesis_scale$BS_TR_Recylcing_1)
Thesis_scale$BS_TR_Recylcing_2 <- ifelse(Thesis_scale$RC2_score == 0, NA_integer_, Thesis_scale$BS_TR_Recylcing_2)
Thesis_scale$BS_TR_Recylcing_3 <- ifelse(Thesis_scale$RC3_score == 0, NA_integer_, Thesis_scale$BS_TR_Recylcing_3)
Thesis_scale$BS_TR_Recylcing_4 <- ifelse(Thesis_scale$RC4_score == 0, NA_integer_, Thesis_scale$BS_TR_Recylcing_4)
Thesis_scale$BS_TR_Recylcing_5 <- ifelse(Thesis_scale$RC5_score == 0, NA_integer_, Thesis_scale$BS_TR_Recylcing_5)
##topic: retruning to crime after prison
Thesis_scale$BS_TR_Crime_1 <- ifelse(Thesis_scale$CR1_score == 0, NA_integer_, Thesis_scale$BS_TR_Crime_1)
Thesis_scale$BS_TR_Crime_2 <- ifelse(Thesis_scale$CR2_score == 0, NA_integer_, Thesis_scale$BS_TR_Crime_2)
Thesis_scale$BS_TR_Crime_3 <- ifelse(Thesis_scale$CR3_score == 0, NA_integer_, Thesis_scale$BS_TR_Crime_3)
Thesis_scale$BS_TR_Crime_4 <- ifelse(Thesis_scale$CR4_score == 0, NA_integer_, Thesis_scale$BS_TR_Crime_4)
Thesis_scale$BS_TR_Crime_5 <- ifelse(Thesis_scale$CR5_score == 0, NA_integer_, Thesis_scale$BS_TR_Crime_5)
## Next, I sum the recoded_TR_topic variables separately for each topic. If the participants wrote about five answers, they should all have five “sum_recoded_TR_topic” scores (one for each topic).
Thesis_scale <- Thesis_scale %>%
  dplyr::mutate(
    sum_recoded_TR_college = rowSums(dplyr::select(., BS_TR_CG_1, BS_TR_CG_2, BS_TR_CG_3, BS_TR_CG_4, BS_TR_CG_5), na.rm = TRUE),
    sum_recoded_TR_recycling = rowSums(dplyr::select(., BS_TR_Recylcing_1, BS_TR_Recylcing_2, BS_TR_Recylcing_3, BS_TR_Recylcing_4, BS_TR_Recylcing_5), na.rm = TRUE),
    sum_recoded_TR_crime = rowSums(dplyr::select(., BS_TR_Crime_1, BS_TR_Crime_2, BS_TR_Crime_3, BS_TR_Crime_4, BS_TR_Crime_5), na.rm = TRUE)
    )
###I als0 just want to double check what does na.rm did for my data in terms of na.rm = TRUE/FALSE
summary(Thesis_scale$recoded_college)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   5.000   5.000   4.487   5.000   5.000
```

``` r
## Lastly, I use this equation to calculate the BS Proportion per topic, whereby each participant will have five individual BS Proportion scores (one for each topic): sum_recoded_thought_rating_topic1 / (sum thought_frequency_topic1 x 10)
Thesis_scale <- Thesis_scale %>%
  dplyr::mutate(
    BSthought_college = sum_recoded_TR_college / (recoded_college * 10),
    BSthought_recycling = sum_recoded_TR_recycling / (recoded_recycling * 10),
    BSthought_crime = sum_recoded_TR_crime / (recoded_crime * 10)
    )
## Then I combine them into one score
Thesis_scale <- Thesis_scale %>%
  dplyr::mutate(BSthought = (BSthought_college + BSthought_recycling + BSthought_crime)/ 3)
## Check if it worked for one
head(Thesis_scale$BSthought_college)
```

```
## [1] 0.225 0.320 0.380 0.340 0.480 0.180
```

``` r
## There is another variable called self-reported knowledge of each topic. I will need to average the knowledge of each topic and created a new variable called knowledge_score
Thesis_scale <- Thesis_scale %>%
  mutate(
    knowledge_score = (Knowledge_CG + Knowledge_recycle + Knowledge_Crime)/3
  )
```

## Step 3.2: calculating the cogntive ability measurements

``` r
##For measuring cognitive ability, the scales are mainly short questions asking word/ math problems and is not likert scales. So I will need to calculate and code the participants' answers into numeric values. 
##Numeracy scale
##compute their answers, if they answer one question right, they will get one point.
Thesis_scale$ns_1 <- ifelse(Thesis_scale$NS1 == 500, 1, 0)
Thesis_scale$ns_2 <- ifelse(Thesis_scale$NS2 == 10, 1, 0)
Thesis_scale$ns_3 <- ifelse(Thesis_scale$NS3 == 0.1, 1, 0)
Thesis_scale$ns_4 <- ifelse(Thesis_scale$NS4 == 9, 1, 0)
Thesis_scale$ns_5 <- ifelse(Thesis_scale$NS5 == 8, 1, 0)
Thesis_scale$ns_6 <- ifelse(Thesis_scale$NS6 == 2, 1, 0)
Thesis_scale$ns_7 <- ifelse(Thesis_scale$NS7 == 2, 1, 0)
Thesis_scale$ns_8 <- ifelse(Thesis_scale$NS8 == 10, 1, 0)
Thesis_scale$ns_9 <- ifelse(Thesis_scale$NS9 == 100, 1, 0)
Thesis_scale$ns_10 <- ifelse(Thesis_scale$NS10 == 20, 1, 0)
Thesis_scale$ns_11 <- ifelse(Thesis_scale$NS11 == 5, 1, 0)
##calculate the mean of each question they got right
Thesis_scale <- Thesis_scale %>%
  mutate(ns_score = (ns_1 + ns_2 + ns_3 + ns_4 + ns_5 + ns_6 + ns_7 + ns_8 + ns_9 + ns_10)/ 10)

##General Social Survey Wordsum Vocabulary Test
##compute their answers, if they answer one question right, they will get one point.
Thesis_scale$word1 <- ifelse(Thesis_scale$wordsum_1 == 4, 1, 0)
Thesis_scale$word2 <- ifelse(Thesis_scale$wordsum_2 == 5, 1, 0)
Thesis_scale$word3 <- ifelse(Thesis_scale$wordsum_3 == 5, 1, 0)
Thesis_scale$word4 <- ifelse(Thesis_scale$wordsum_4 == 3, 1, 0)
Thesis_scale$word5 <- ifelse(Thesis_scale$wordsum_5 == 1, 1, 0)
Thesis_scale$word6 <- ifelse(Thesis_scale$wordsum_6 == 3, 1, 0)
Thesis_scale$word7 <- ifelse(Thesis_scale$wordsum_7 == 5, 1, 0)
Thesis_scale$word8 <- ifelse(Thesis_scale$wordsum_8 == 4, 1, 0)
Thesis_scale$word9 <- ifelse(Thesis_scale$wordsum_9 == 4, 1, 0)
Thesis_scale$word10 <- ifelse(Thesis_scale$wordsum_10 == 1, 1, 0)
##calculate the mean of each question they got right
Thesis_scale <- Thesis_scale %>%
  mutate(word_score = (word1 + word2 + word3 + word4 + word5 + word6 + word7 + word8 + word9 + word10)/ 10)

##The Cognitive Reflection Test
##compute their answers, if they answer one question right, they will get one point.
Thesis_scale$crt1 <- ifelse(Thesis_scale$crt1 == 5, 1, 0)
Thesis_scale$crt2 <- ifelse(Thesis_scale$crt2 == 5, 1, 0)
Thesis_scale$crt3 <- ifelse(Thesis_scale$crt3 == 47, 1, 0)
##calculate the mean of each question they got right
Thesis_scale <- Thesis_scale %>%
  mutate(crt_score = (crt1 + crt2 + crt3)/ 3)
```


## Step 4: save the cleaned datset for analysis

``` r
saveRDS(Thesis_scale, file = "Thesis_scale.rds")
saveRDS(Thesis_clean, file = "Thesis_clean.rds")
```

