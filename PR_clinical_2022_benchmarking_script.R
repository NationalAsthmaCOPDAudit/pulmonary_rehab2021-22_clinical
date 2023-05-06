
#--------------------------------------------------------------------------------------------#
# P U L M O N A R Y   R E H A B   C L I N I C A L   b e n c h m a r k i n g    s c r i p t   #
#                                                                                            #
# Author: Alex                                                                               #
# Date created: 2022-08-09                                                                   #
#--------------------------------------------------------------------------------------------#



library(dplyr)
# library(readstata13)
# library(xlsx)
# library(janitor)
# library(officer)
# library(flextable)
library(tidyverse)
library(survival)
library(survminer)
library(ggplot2)
library(survsup)
# library(epitools)
library(psych)
library(lme4)
'%!in%' <- function(x,y)!('%in%'(x,y))
library(car)
library(extrafont)
loadfonts()
fonts()
nlc <- function(x) {cat(paste("\n", x, "\n", sep = ""))}



dat <- readRDS("Z:/Group_work/Alex/Encrypted/Alex/PR/PR clinical 2021-22/Data/tidyData/PR_clinical_2021-22_cleaned.RDS")





# glimpse(flat.all)
# 
# write.xlsx(dat, file, sheetName = "Sheet1", 
#            col.names = TRUE, row.names = FALSE)


# sprintf("%.1f", round((BM_prac_test_nume/BM_prac_test_denom)*100,1)),

# Use summarise function to get necessary columns
bmk <- dat %>% dplyr::group_by(org_code) %>%
  summarise(trust_code = first(trust_code), 
            cases.audited = n(),
            start_90_denom = sum(!is.na(BM_start_90)),
            start_90_nume = sum(BM_start_90, na.rm = TRUE),
            start_90_perc = (start_90_nume/start_90_denom)*100,
            
            BM_prac_test_denom = sum(!is.na(BM_prac_test)),
            BM_prac_test_nume = sum(BM_prac_test, na.rm = TRUE),
            BM_prac_test_perc = (BM_prac_test_nume/BM_prac_test_denom)*100,
            
            BM_discharge_assess_denom = sum(!is.na(BM_discharge_assess)),
            BM_discharge_assess_nume = sum(BM_discharge_assess, na.rm = TRUE),
            BM_discharge_assess_perc = (BM_discharge_assess_nume/BM_discharge_assess_denom)*100,
            
            BM_exercise_plan_denom = sum(!is.na(BM_exercise_plan)),
            BM_exercise_plan_nume = sum(BM_exercise_plan, na.rm = TRUE),
            BM_exercise_plan_perc = (BM_exercise_plan_nume/BM_exercise_plan_denom)*100,
            
            BM_MCID_exercise_denom = sum(!is.na(BM_MCID_exercise)),
            BM_MCID_exercise_nume = sum(BM_MCID_exercise, na.rm = TRUE),
            BM_MCID_exercise_perc = (BM_MCID_exercise_nume/BM_MCID_exercise_denom)*100,
            
            BM_MCID_CAT_CRQ_denom = sum(!is.na(BM_MCID_CAT_CRQ)),
            BM_MCID_CAT_CRQ_nume = sum(BM_MCID_CAT_CRQ, na.rm = TRUE),
            BM_MCID_CAT_CRQ_perc = (BM_MCID_CAT_CRQ_nume/BM_MCID_CAT_CRQ_denom)*100)

bmk
# quartz1 is for calculating stuff, quartz_fmt is the well-formatted one



quartz1 <- matrix(data = NA, nrow = 3, ncol = 7)
quartz1[1:3, 1] <- c("lower.quartile", "median", "upper.quartile")

quartz1[1:3, 2] <- quantile(bmk$start_90_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz1[1:3, 3] <- quantile(bmk$BM_prac_test_perc,probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz1[1:3, 4] <- quantile(bmk$BM_discharge_assess_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz1[1:3, 5] <- quantile(bmk$BM_exercise_plan_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz1[1:3, 6] <- quantile(bmk$BM_MCID_exercise_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)
quartz1[1:3, 7] <- quantile(bmk$BM_MCID_CAT_CRQ_perc, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4)


colnames(quartz1) <- c("statistic", "start_90_perc", "BM_prac_test_perc", "BM_discharge_assess_perc", 
                       "BM_exercise_plan_perc", "BM_MCID_exercise_perc", "BM_MCID_CAT_CRQ_perc") 

quartz1 <- as.data.frame(quartz1)
# quartz1 %>% mutate_if(is.factor, as.character(.)) %>% mutate_at(~vars(-statistic), ~as.numeric)

quartz1 <- quartz1 %>% mutate_at(.vars = vars(-statistic), .funs = ~as.numeric(as.character(.)))

quartz1 <- quartz1 %>% mutate_at(.vars = vars(-statistic), .funs = ~round(., 0))

# Now that we're rounding the medians anyway, this is a very long-winded way to do it and I could have 
# just used quartz1 to make quartz_fmt

quartz_fmt <- matrix(data = NA, nrow = 3, ncol = 7)
quartz_fmt[1:3, 1] <- c("lower.quartile", "median", "upper.quartile")

quartz_fmt[1:3, 2] <- sprintf("%.0f", round(quantile(bmk$start_90_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))
quartz_fmt[1:3, 3] <- sprintf("%.0f", round(quantile(bmk$BM_prac_test_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))
quartz_fmt[1:3, 4] <- sprintf("%.0f", round(quantile(bmk$BM_discharge_assess_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))
quartz_fmt[1:3, 5] <- sprintf("%.0f", round(quantile(bmk$BM_exercise_plan_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))
quartz_fmt[1:3, 6] <- sprintf("%.0f", round(quantile(bmk$BM_MCID_exercise_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))
quartz_fmt[1:3, 7] <- sprintf("%.0f", round(quantile(bmk$BM_MCID_CAT_CRQ_perc,
                                                     probs = c(0.25, 0.5, 0.75), na.rm = TRUE, type = 4),0))


colnames(quartz_fmt) <- c("statistic", "start_90_perc", "BM_prac_test_perc", "BM_discharge_assess_perc", 
                          "BM_exercise_plan_perc", "BM_MCID_exercise_perc", "BM_MCID_CAT_CRQ_perc") 

quartz_fmt <- as.data.frame(quartz_fmt)

# write.csv(quartz_fmt, file =
#           "Z:/Group_work/PS_AA/PR 2022/Data/dataStore/PR_clinical_2021-22_benchmarking_quartiles_2022-08-09.csv",
#           row.names = FALSE)


# It's at this point that we round BMK so it can be compared to the medians. 

colnames(bmk)

bmk <- bmk %>% mutate_at(.vars = vars(contains("perc")), .funs = ~round(., 0))



# Now, using quartz1, we add in the BMK colour code.

bmk <- bmk %>% mutate(start_90_colour_end = ifelse(start_90_denom < 5 | is.na(start_90_denom) == TRUE, "Grey",
                                                   ifelse(start_90_perc < quartz1$start_90_perc[1], "Red",
                                                          ifelse(start_90_perc >= quartz1$start_90_perc[3], "Green", 
                                                                 "Yellow"))),
                      BM_prac_test_colour_end = ifelse(BM_prac_test_denom < 5 | is.na(BM_prac_test_denom) == TRUE, "Grey",
                                                       ifelse(BM_prac_test_perc < quartz1$BM_prac_test_perc[1], "Red",
                                                              ifelse(BM_prac_test_perc >= quartz1$BM_prac_test_perc[3], "Green", 
                                                                     "Yellow"))),
                      BM_discharge_assess_colour_end = ifelse(BM_discharge_assess_denom < 5 | is.na(BM_discharge_assess_denom) == TRUE, "Grey",
                                                              ifelse(BM_discharge_assess_perc < quartz1$BM_discharge_assess_perc[1], "Red",
                                                                     ifelse(BM_discharge_assess_perc >= quartz1$BM_discharge_assess_perc[3], "Green", 
                                                                            "Yellow"))),
                      BM_exercise_plan_colour_end = ifelse(BM_exercise_plan_denom < 5 | is.na(BM_exercise_plan_denom) == TRUE, "Grey",
                                                           ifelse(BM_exercise_plan_perc < quartz1$BM_exercise_plan_perc[1], "Red",
                                                                  ifelse(BM_exercise_plan_perc >= quartz1$BM_exercise_plan_perc[3], "Green", 
                                                                         "Yellow"))),
                      BM_MCID_exercise_colour_end = ifelse(BM_MCID_exercise_denom < 5 | is.na(BM_MCID_exercise_denom) == TRUE, "Grey",
                                                           ifelse(BM_MCID_exercise_perc < quartz1$BM_MCID_exercise_perc[1], "Red",
                                                                  ifelse(BM_MCID_exercise_perc >= quartz1$BM_MCID_exercise_perc[3], "Green", 
                                                                         "Yellow"))),
                      BM_MCID_CAT_CRQ_colour_end = ifelse(BM_MCID_CAT_CRQ_denom < 5 | is.na(BM_MCID_CAT_CRQ_denom) == TRUE, "Grey",
                                                          ifelse(BM_MCID_CAT_CRQ_perc < quartz1$BM_MCID_CAT_CRQ_perc[1], "Red",
                                                                 ifelse(BM_MCID_CAT_CRQ_perc >= quartz1$BM_MCID_CAT_CRQ_perc[3], "Green", 
                                                                        "Yellow"))))






bmk <- bmk %>% add_column(start_90_colour = bmk$start_90_colour_end, .after = "start_90_perc") %>% 
  add_column(BM_prac_test_colour = bmk$BM_prac_test_colour_end, .after = "BM_prac_test_perc") %>%
  add_column(BM_discharge_assess_colour = bmk$BM_discharge_assess_colour_end, .after = "BM_discharge_assess_perc") %>% 
  add_column(BM_exercise_plan_colour = bmk$BM_exercise_plan_colour_end, .after = "BM_exercise_plan_perc") %>% 
  add_column(BM_MCID_exercise_colour = bmk$BM_MCID_exercise_colour_end, .after = "BM_MCID_exercise_perc") %>%
  add_column(BM_MCID_CAT_CRQ_colour = bmk$BM_MCID_CAT_CRQ_colour_end, .after = "BM_MCID_CAT_CRQ_perc") %>%
  select(-start_90_colour_end, -BM_prac_test_colour_end, -BM_discharge_assess_colour_end, -BM_exercise_plan_colour_end,
         -BM_MCID_exercise_colour_end, -BM_MCID_CAT_CRQ_colour_end)





bmk_all <- dat %>%
  summarise(org_code = "National",
            trust_code = "National", 
            cases.audited = n(),
            start_90_denom = sum(!is.na(BM_start_90)),
            start_90_nume = sum(BM_start_90, na.rm = TRUE),
            start_90_perc = round((start_90_nume/start_90_denom)*100, 0),
            
            BM_prac_test_denom = sum(!is.na(BM_prac_test)),
            BM_prac_test_nume = sum(BM_prac_test, na.rm = TRUE),
            BM_prac_test_perc = round((BM_prac_test_nume/BM_prac_test_denom)*100, 0),
            
            BM_discharge_assess_denom = sum(!is.na(BM_discharge_assess)),
            BM_discharge_assess_nume = sum(BM_discharge_assess, na.rm = TRUE),
            BM_discharge_assess_perc = round((BM_discharge_assess_nume/BM_discharge_assess_denom)*100, 0),
            
            BM_exercise_plan_denom = sum(!is.na(BM_exercise_plan)),
            BM_exercise_plan_nume = sum(BM_exercise_plan, na.rm = TRUE),
            BM_exercise_plan_perc = round((BM_exercise_plan_nume/BM_exercise_plan_denom)*100, 0),
            
            BM_MCID_exercise_denom = sum(!is.na(BM_MCID_exercise)),
            BM_MCID_exercise_nume = sum(BM_MCID_exercise, na.rm = TRUE),
            BM_MCID_exercise_perc = round((BM_MCID_exercise_nume/BM_MCID_exercise_denom)*100, 0),
            
            BM_MCID_CAT_CRQ_denom = sum(!is.na(BM_MCID_CAT_CRQ)),
            BM_MCID_CAT_CRQ_nume = sum(BM_MCID_CAT_CRQ, na.rm = TRUE),
            BM_MCID_CAT_CRQ_perc = round((BM_MCID_CAT_CRQ_nume/BM_MCID_CAT_CRQ_denom)*100, 0))

# We want to keep the column order of the site-level table
# We then need to change the row order so that the national analysis is at the top.
# We therefore put the last row at the top using the indexing below

bmk <- bind_rows(bmk, bmk_all)
bmk <- bmk[c(nrow(bmk), 1:(nrow(bmk)-1)), ]

bmk <- bmk %>% mutate_at(.vars = vars(matches("perc")), .funs = ~sprintf("%.0f", round(., 0)))



bmk

str(bmk)

# write.csv(bmk, file =
#  "Z:/Group_work/PS_AA/PR 2022/Data/dataStore/PR_clinical_2021-22_benchmarking_2022-08-09.csv",
#   row.names = FALSE)





