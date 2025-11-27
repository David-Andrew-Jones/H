################################################################################
#______________________________________________________________________________#
#' _____ 04 Deriving net costs from all cause costs 
#______________________________________________________________________________#
################################################################################

#' _____ Attach predicted baselines cost for each age to each patient
#' Make a new dataframe of predicted costs (transformed to daily amounts) and make wide. 
df_cost_predictions_deterministic_wide <- df_cost_predictions_deterministic %>%
        select(patientid, age_diag, age, additional_age, predicted_annual_cost) %>%
        mutate(predicted_daily_cost = predicted_annual_cost / 365.25) %>%
        select(-predicted_annual_cost) %>%
        pivot_wider(names_from = additional_age, values_from = c(age, predicted_daily_cost))

#' Join to original cost dataframe
df_sim_prepped_prediagcosts <- df_sim_prepped %>%
        left_join(df_cost_predictions_deterministic_wide, by = c("patientid")) %>%
        # Simulate date of birth - will already be in HDI analysis
        group_by(patientid) %>%
        mutate(date_birth = diag_date %m-% years(age_diag) %m-% days(sample(1:364, 1, replace = TRUE ))) %>%
        ungroup() 

#' _____ Estimate net costs for each cost outcome
df_sim_prepped_prediagcosts_fu <-  df_sim_prepped_prediagcosts %>%
        # 1. Calculate total follow-up time for each cost outcome
        mutate(total_days_fu = case_when(cost_outcome == "ac_diag" ~ as.double(difftime(diag_date, (diag_date %m-% months(6))) , units = c("days")),
                                           # 0-1 scenarios: end of follow-up censorship doesnt apply so only need to account for death within a year
                                           cost_outcome == "ac_treat_0_1year" & !is.na(death_date) & 
                                                   date_fu1y > death_date ~ as.double(difftime(death_date, diag_date) , units = c("days")), # 0-1: Death within the year
                                           # 1-2 scenarios: end of follow-up censorship doesnt apply so only need to account for death within interval
                                           cost_outcome == "ac_treat_1_2year" & !is.na(death_date) & 
                                                   date_fu2y > death_date ~ as.double(difftime(death_date, date_fu1y) , units = c("days")), # 1-2: Death within the year
                                           # 2-3 scenarios: death within interval & censorship
                                           cost_outcome == "ac_treat_2_3year" & !is.na(death_date) & 
                                                   date_fu3y > death_date ~ as.double(difftime(death_date, date_fu2y) , units = c("days")), # 2-3: Death within the year
                                           cost_outcome == "ac_treat_2_3year" & 
                                                   date_fu3y > as.Date("31/12/2019", "%d/%m/%Y") ~ as.double(difftime(as.Date("31/12/2019", "%d/%m/%Y"), date_fu2y) , units = c("days")), # 2-3: Censored
                                           # 3-4 scenarios: death within interval & censorship
                                           cost_outcome == "ac_treat_3_4year" & !is.na(death_date) & 
                                                   date_fu4y > death_date ~ as.double(difftime(death_date, date_fu3y) , units = c("days")), # 3-4: Death within the year
                                           cost_outcome == "ac_treat_3_4year" & 
                                                   date_fu4y > as.Date("31/12/2019", "%d/%m/%Y") ~ as.double(difftime(as.Date("31/12/2019", "%d/%m/%Y"), date_fu3y) , units = c("days")), # 3-4: Censored
                                           # 4-5 scenarios: death within interval & censorship
                                           cost_outcome == "ac_treat_4_5year" & !is.na(death_date) & 
                                                   date_fu5y > death_date ~ as.double(difftime(death_date, date_fu4y) , units = c("days")), # 4-5: Death within the year
                                           cost_outcome == "ac_treat_4_5year" & 
                                                   date_fu5y > as.Date("31/12/2019", "%d/%m/%Y") ~ as.double(difftime(as.Date("31/12/2019", "%d/%m/%Y"), date_fu4y) , units = c("days")), # 4-5: Censored
                                           # 5-6 scenarios: death within interval & censorship
                                           cost_outcome == "ac_treat_5year" & !is.na(death_date) & 
                                                   date_fu6y > death_date ~ as.double(difftime(death_date, date_fu5y) , units = c("days")), # 5-6: Death within the year
                                           cost_outcome == "ac_treat_5year" & 
                                                   date_fu6y > as.Date("31/12/2019", "%d/%m/%Y") ~ as.double(difftime(as.Date("31/12/2019", "%d/%m/%Y"), date_fu5y) , units = c("days")), # 5-6: Censored
                                           # Add 5-6 scenarios!
                                           #' _____ PoC
                                           cost_outcome == "diag" ~ as.double(difftime(diag_date, (diag_date %m-% months(6))) , units = c("days")),
                                           # do all patient in the dataframe have EoL by default? if so:
                                           cost_outcome == "EoL" & is.na(death_date) ~ NA,
                                           # EoL scenarios - 1. Death within a year. 2. Death after - in which case full 1 year follow-up given by .default at end
                                           cost_outcome == "EoL" & !is.na(death_date) & 
                                                   date_fu1y > death_date ~ as.double(difftime(death_date, diag_date) , units = c("days")), # 0-1: Death within the year
                                           # Conditions now need to take account death
                                           # 0-1: patient dies in the 1-2 interval. No censorship scenario
                                           cost_outcome == "treat_0_1year" & !is.na(death_date) & 
                                                   date_fu2y > death_date ~ as.double(difftime((death_date - years(1)), diag_date) , units = c("days")),
                                           # 1-2: patient dies in the 2-3 interval. No censorship scenario
                                           cost_outcome == "treat_1_2year" & !is.na(death_date) & 
                                                   date_fu3y > death_date ~ as.double(difftime((death_date - years(1)), date_fu1y) , units = c("days")),
                                           # 2-3: patient dies in the 3-4 interval or censorship
                                           cost_outcome == "treat_2_3year" & !is.na(death_date) & 
                                                   date_fu4y > death_date ~ as.double(difftime((death_date - years(1)), date_fu2y) , units = c("days")),
                                           cost_outcome == "treat_2_3year" & 
                                                   date_fu3y > as.Date("31/12/2019", "%d/%m/%Y") ~ as.double(difftime(as.Date("31/12/2019", "%d/%m/%Y"), date_fu2y) , units = c("days")), 
                                           # 3-4: patient dies in the 4-5 interval or censorship
                                           cost_outcome == "treat_3_4year" & !is.na(death_date) & 
                                                   date_fu5y > death_date ~ as.double(difftime((death_date - years(1)), date_fu3y) , units = c("days")),
                                           cost_outcome == "treat_3_4year" & 
                                                   date_fu4y > as.Date("31/12/2019", "%d/%m/%Y") ~ as.double(difftime(as.Date("31/12/2019", "%d/%m/%Y"), date_fu3y) , units = c("days")), 
                                           # 4-5: patient dies in the 5-6 interval or censorship
                                           cost_outcome == "treat_4_5year" & !is.na(death_date) & 
                                                   date_fu6y > death_date ~ as.double(difftime((death_date - years(1)), date_fu4y) , units = c("days")),
                                           cost_outcome == "treat_4_5year" & 
                                                   date_fu5y > as.Date("31/12/2019", "%d/%m/%Y") ~ as.double(difftime(as.Date("31/12/2019", "%d/%m/%Y"), date_fu4y) , units = c("days")), 
                                           # 5-6: only censorship
                                           cost_outcome == "treat_5year" & 
                                                   date_fu6y > as.Date("31/12/2019", "%d/%m/%Y") ~ as.double(difftime(as.Date("31/12/2019", "%d/%m/%Y"), date_fu5y) , units = c("days")), 
                                           .default = 365.25 )) %>% 
        # 2. Calculate number of days in the 6 month pre diagnosis to diagnosis windo and annual follow-up windows there are at different ages
        mutate(# Date they turned the age at which they were diagnosed and other dates of birthdays relevant to the period
               date_birthday2agediag = ymd(date_birth  %m+% years(age_diag)),
               date_birthday1 = ymd(date_birth  %m+% years(age_diag) %m-% years(1)), # birthday before the one they were diagnosed after
               date_birthday3 = ymd(date_birth  %m+% years(age_diag) %m+% years(1)),
               date_birthday4 = ymd(date_birth  %m+% years(age_diag) %m+% years(2)),
               date_birthday5 = ymd(date_birth  %m+% years(age_diag) %m+% years(3)),
               date_birthday6 = ymd(date_birth  %m+% years(age_diag) %m+% years(4)),
               date_birthday7 = ymd(date_birth  %m+% years(age_diag) %m+% years(5)),
               # 1. For the diagnosis period calculate the day intervals between birthdays
               # i.e the Number of days between the date they turned diagnosis age, and the date 6 month before this. 
               # This will be negative in the instance their diagnosis was > 6 months after they had their birthday 
               date_diff_DateAgeDiag_and_DateDiagMinus6m = as.double(difftime(date_birthday2agediag, (diag_date %m-% months(6))) , units = c("days")),
               # Number of days between the date they turned diagnosis age and diagnosis date
               date_diff_DateAgeDiag_and_DateDiag = as.double(difftime(diag_date,  date_birthday2agediag) , units = c("days")),
               # 2. For EoL period
               # Find the birthday BEFORE death
               date_diff_Death_Birthday2 = as.double(difftime(death_date,  date_birthday2agediag) , units = c("days")),
               date_diff_Death_Birthday3 = as.double(difftime(death_date,  date_birthday3) , units = c("days")),
               date_diff_Death_Birthday4 = as.double(difftime(death_date,  date_birthday4) , units = c("days")),
               date_diff_Death_Birthday5 = as.double(difftime(death_date,  date_birthday5) , units = c("days")),
               date_diff_Death_Birthday6 = as.double(difftime(death_date,  date_birthday6) , units = c("days")),
               date_diff_Death_Birthday7 = as.double(difftime(death_date,  date_birthday7) , units = c("days"))) %>%
        mutate(across(date_diff_Death_Birthday2:date_diff_Death_Birthday7, ~ case_when(. <= 0 | is.na(.) ~ 100000, .default = . ))) %>%
        mutate(nearest_birthday_death = pmap_chr(across(date_diff_Death_Birthday2:date_diff_Death_Birthday7), ~ names(c(...)[which.min(c(...))]))) %>%
        mutate(# 3. For the annual periods:
               # Number of days diagnosis date and next birthday
               date_diff_DateDiag_to_birthday = as.double(difftime(date_birthday3,  diag_date) , units = c("days")),
               # Number of days between birthday and next year of follow_up
               date_diff_agebirthday_to_1year = as.double(difftime(date_fu1y,  date_birthday3), units = c("days"))) %>%
        # 3. Split total follow up into days before birthday and days after
        mutate(before_birthday_fu = case_when(!grepl(c("diag|eol" ), cost_outcome) ~ date_diff_DateDiag_to_birthday, 
                                               grepl(c("diag" ), cost_outcome) ~ date_diff_DateAgeDiag_and_DateDiagMinus6m, # as the diagnosis phase is only 6 months
                                              .default = NA),
               after_birthday_fu = case_when(!grepl(c("eol" ), cost_outcome) ~ total_days_fu - before_birthday_fu,
                                             # Scenario 1. Person dies within one year. Last birthday happened before diagnosis
                                             grepl(c("eol" ), cost_outcome) & nearest_birthday_death == "date_diff_Death_Birthday2" ~ total_days_fu,
                                             # Scenario 2. Person dies within one year. Birthday within follow-up. or 1 year of follow-up
                                             grepl(c("eol" ), cost_outcome) & nearest_birthday_death == "date_diff_Death_Birthday3" ~ date_diff_Death_Birthday3,
                                             grepl(c("eol" ), cost_outcome) & nearest_birthday_death == "date_diff_Death_Birthday4" ~ date_diff_Death_Birthday4,
                                             grepl(c("eol" ), cost_outcome) & nearest_birthday_death == "date_diff_Death_Birthday5" ~ date_diff_Death_Birthday5,
                                             grepl(c("eol" ), cost_outcome) & nearest_birthday_death == "date_diff_Death_Birthday6" ~ date_diff_Death_Birthday6,
                                             grepl(c("eol" ), cost_outcome) & nearest_birthday_death == "date_diff_Death_Birthday7" ~ date_diff_Death_Birthday7,
                                             .default = NA),
               before_birthday_fu = case_when(!grepl(c("eol" ), cost_outcome) & before_birthday_fu > total_days_fu  ~ total_days_fu, # This is the case in the annual FU when they are censored before their birthday
                                              !grepl(c("eol" ), cost_outcome) & before_birthday_fu < 0  ~ 0, # This is the case in the diag outcome where their birthday is not in the 6months pre diag to diag interval
                                              # Scenario 1
                                              grepl(c("eol" ), cost_outcome) & total_days_fu == after_birthday_fu ~ 0,
                                              grepl(c("eol" ), cost_outcome) & total_days_fu > after_birthday_fu ~ total_days_fu - after_birthday_fu,
                                              .default = before_birthday_fu),
               after_birthday_fu = case_when(after_birthday_fu < 0  ~ 0, # Happens when the whole follow-up is before the birthday in the interval year(where FU < a year) 
                                             before_birthday_fu == 0  ~ total_days_fu, # To account for case in the diag outcome where their birthday is not in the 6months pre diag to diag interval
                                             .default = after_birthday_fu)) %>%
        mutate(check = total_days_fu - (before_birthday_fu + after_birthday_fu)) %>%
        # 4. calculate baseline cost for each cost outcome as the sum of the follow-up multiplied by the age-specific predicted daily cost
        mutate(baseline_cost= case_when(grepl(c("diag" ), cost_outcome) ~ (predicted_daily_cost_1 * before_birthday_fu) + (predicted_daily_cost_2 * after_birthday_fu),
                                     grepl(c("0_1" ), cost_outcome) ~ (predicted_daily_cost_2 * before_birthday_fu) + (predicted_daily_cost_3 * after_birthday_fu),
                                     grepl(c("1_2" ), cost_outcome) ~ (predicted_daily_cost_3 * before_birthday_fu) + (predicted_daily_cost_4 * after_birthday_fu),
                                     grepl(c("2_3" ), cost_outcome) ~ (predicted_daily_cost_4 * before_birthday_fu) + (predicted_daily_cost_5 * after_birthday_fu),
                                     grepl(c("3_4" ), cost_outcome) ~ (predicted_daily_cost_5 * before_birthday_fu) + (predicted_daily_cost_6 * after_birthday_fu),
                                     grepl(c("4_5" ), cost_outcome) ~ (predicted_daily_cost_6 * before_birthday_fu) + (predicted_daily_cost_7 * after_birthday_fu),
                                     grepl(c("t_5" ), cost_outcome) ~ (predicted_daily_cost_7 * before_birthday_fu) + (predicted_daily_cost_8 * after_birthday_fu),
                                     grepl(c("eol" ), cost_outcome) & !is.na(death_date) & nearest_birthday_death == "date_diff_Death_Birthday2" ~ 
                                             (predicted_daily_cost_2 * before_birthday_fu) + (predicted_daily_cost_3 * after_birthday_fu),
                                     grepl(c("eol" ), cost_outcome) & !is.na(death_date) & nearest_birthday_death == "date_diff_Death_Birthday3" ~ 
                                             (predicted_daily_cost_3 * before_birthday_fu) + (predicted_daily_cost_4 * after_birthday_fu),
                                     grepl(c("eol" ), cost_outcome) & !is.na(death_date) & nearest_birthday_death == "date_diff_Death_Birthday4" ~ 
                                             (predicted_daily_cost_4 * before_birthday_fu) + (predicted_daily_cost_5 * after_birthday_fu),
                                     grepl(c("eol" ), cost_outcome) & !is.na(death_date) & nearest_birthday_death == "date_diff_Death_Birthday5" ~ 
                                             (predicted_daily_cost_5 * before_birthday_fu) + (predicted_daily_cost_6 * after_birthday_fu),
                                     grepl(c("eol" ), cost_outcome) & !is.na(death_date) & nearest_birthday_death == "date_diff_Death_Birthday6" ~ 
                                             (predicted_daily_cost_6 * before_birthday_fu) + (predicted_daily_cost_7 * after_birthday_fu),
                                     grepl(c("eol" ), cost_outcome) & !is.na(death_date) & nearest_birthday_death == "date_diff_Death_Birthday7" ~ 
                                             (predicted_daily_cost_7 * before_birthday_fu) + (predicted_daily_cost_8 * after_birthday_fu),
                                     .default = NA)) %>%
        # 4. Calculate net cost
        mutate(net_cost = sum_costs_outcome - baseline_cost)
                