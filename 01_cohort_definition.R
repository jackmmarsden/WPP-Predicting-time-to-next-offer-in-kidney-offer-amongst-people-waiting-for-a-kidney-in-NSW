
# Load libraries
library(data.table)
library(dplyr)
library(tidyr)
library(haven)
library(readxl)
library(lubridate)
library(labelled) # generate_dictionary
library(stringr)
library(skimr) # To summarise datasets

# Set directories
 usyd <- "//shared.sydney.edu.au/research-data/"
 anzdataloc <- paste0(usyd,"PRJ-MODUS/3 Data Management/ANZDATA useable data")

 safebod_anzdataloc <- paste0(usyd, "PRJ-SAFEBOD\\3. Data Management\\3.5 Datasets_Relinkage 2022\\Formatted data\\ANZDATA_ANZOD")
 safebod_organmatchloc <- paste0(usyd, "PRJ-SAFEBOD\\3. Data Management\\3.5 Datasets_Relinkage 2022\\Formatted data\\OrganMatchDetails")
 safebod_codurfloc <- paste0(usyd, "PRJ-SAFEBOD\\3. Data Management\\3.5 Datasets_Relinkage 2022\\Formatted data\\Mortality")
 safebod_rbdmloc <- paste0(usyd, "PRJ-SAFEBOD\\3. Data Management\\3.5 Datasets_Relinkage 2022\\Formatted data\\Mortality")

 safebod_summaryloc <- paste0(usyd, "PRJ-SAFEBOD\\3. Data Management\\3.5 Datasets_Relinkage 2022\\Summary datasets")

# # Load data ----
# ## SAFEBOD datasets
 safebod_omstatushistory <- read_dta(file.path(safebod_anzdataloc, "42952 OMStatusHistory.dta"))
 safebod_anzdatapatients <- read_dta(file.path(safebod_anzdataloc, "42952 ANZDATAPatients.dta"))
 safebod_anzdatatransplants <- read_dta(file.path(safebod_anzdataloc, "42952 ANZDATATransplants.dta"))
 safebod_anzdatacomorbidities <- read_dta(file.path(safebod_anzdataloc, "42952 ANZDATAComorbidities.dta"))
 safebod_anzdatacourseoftreatments <- read_dta(file.path(safebod_anzdataloc, "42952 ANZDATACourseOfTreatments.dta"))
 safebod_omdetails <- read_dta(file.path(safebod_organmatchloc, "prj2017272_organmatch_details_v3.dta"))
 safebod_omrecip <- read_dta(file.path(safebod_organmatchloc, "prj_2017_27_organmatch_recip_v2.dta"))
 safebod_codurf <- read_dta(file.path(safebod_codurfloc, "codurf_sensitive.dta"))
 safebod_rbdm <- read_dta(file.path(safebod_rbdmloc, "rbdm_deaths_sensitive.dta"))
 safebod_anzoddestination <- read_dta(file.path(safebod_anzdataloc, "42952 ANZODDestination.dta"))

 safebod_donor_recipient_mapping <- read_dta(file.path(safebod_summaryloc, "safebod_organ_transplants.dta"))


# Cohort for modelling time to next offer and time to suspension ----

## Create a dataset with transplant dates, failure dates, and transplant type
txdates <- safebod_anzdatatransplants %>%
  rename(txdate = transplantdate,
         txfaildate = graftfailuredate,
         txenddate = endtransplantdate,
         txstate = transplantcentrestate) %>%
  mutate(txtype = case_when(
    donorsourcecode == 100 ~ "Deceased",
    is.na(donorsourcecode) ~ "Unknown",
    TRUE ~ "Living")) %>%
  mutate(txexchange = if_else(donorsourcecode %in% c(402, 403), 1, 0)) %>%
  select(ppn, graftno, txdate, txfaildate, txenddate, txtype, txstate, txexchange) %>%
  pivot_wider(id_cols = "ppn",
              names_from = "graftno",
              values_from = c("txdate", "txfaildate", "txenddate", "txtype", "txstate", "txexchange")) %>%
  select(ppn, ends_with("_1"), ends_with("_2"), ends_with("_3"), ends_with("_4")) 



# Create a dataset with dates of death
codurf_uniqueppn <- safebod_codurf %>% 
  # Filter out ppn with two death dates
  filter(ppn != "00000001586747") %>%
  # Keep only PPN and date of death
  select(ppn, death_date) %>%
  # Rename
  rename(codurf_deathdate = death_date) %>%
  # Remove duplicates 
  distinct() %>%
  arrange(ppn)

# Find excluded ppns to exclude later in the analysis for linkage error
# Note all ppns had both death dates examined to see if it could have been
# clerical error/typo, but all dates substantially different (ymd all diff)
# Keep to exclude later
excluded_ppns <- safebod_rbdm %>%
  select(ppn, DEATH_DATE) %>%
  distinct() %>%
  group_by(ppn) %>%
  filter(n()>1) %>%
  select(ppn) %>%
  distinct()

rbdm_uniqueppn <- safebod_rbdm %>% 
  # Keep only PPN and date of death
  rename(rbdm_deathdate = DEATH_DATE) %>%
  select(ppn, rbdm_deathdate) %>%
  # Remove duplicates 
  distinct() %>%
  # Get rid of ppns with >1 death date
  arrange(ppn) %>%
  group_by(ppn) %>%
  filter(n()<2) %>%
  ungroup()

## Combine CODURF and RBDM death dates
deathdates <- full_join(codurf_uniqueppn, rbdm_uniqueppn) %>%
  # Keep the earleist deathdate for each person
  mutate(deathdate = pmin(codurf_deathdate, rbdm_deathdate, na.rm = TRUE)) %>%
  # Keep the relevant variables
  select(ppn, deathdate) %>%
  # Remove duplicates
  distinct() %>%
  # Sort by PPN
  arrange(ppn)


## Check if anybody was activated on the waiting list during period with functioning transplant
## n=9 people with activations during transplanted period, easier to exclude them
safebod_omstatushistory %>%
  left_join(txdates) %>%
  mutate(flag = case_when(
    waitdate > txdate_1 & waitdate < txenddate_1 & waitstatus == 2 ~ 1,
    waitdate > txdate_2 & waitdate < txenddate_2 & waitstatus == 2 ~ 1,
    waitdate > txdate_3 & waitdate < txenddate_3 & waitstatus == 2 ~ 1,
    waitdate > txdate_4 & waitdate < txenddate_4 & waitstatus == 2 ~ 1,
    TRUE ~ 0)) %>%
  group_by(ppn) %>%
  mutate(everflag = max(flag)) %>%
  ungroup() %>%
  filter(everflag == 1)


# Find state of transplant for transplants in ANZDATA
safebod_anzdata_state <- safebod_anzdatapatients %>%
  mutate(state_rrtstart = initialparentcentrestate,
         graftno = 1) %>%
  select(ppn, graftno, state_rrtstart) %>%
  full_join(safebod_anzdatatransplants %>%
              mutate(state_tx = referringcentrestate) %>%
              select(ppn, graftno, state_tx)) %>%
  arrange(ppn, graftno) %>%
  mutate(state_rrtstart = if_else(state_rrtstart == 0, NA_real_, state_rrtstart),
         state_tx = if_else(state_tx == 0, NA_real_, state_tx),
         state = coalesce(state_rrtstart, state_tx),
         state_nt = replace_na((state == 1) * 1, 0),
         state_nsw = replace_na((state == 2) * 1, 0),
         state_vic = replace_na((state == 3) * 1, 0),
         state_qld = replace_na((state == 4) * 1, 0),
         state_sa = replace_na((state == 5) * 1, 0),
         state_wa = replace_na((state == 6) * 1, 0),
         state_tas = replace_na((state == 7) * 1, 0),
         state_nz = replace_na((state == 8) * 1, 0),
         state_act = replace_na((state == 9) * 1, 0)) %>%
  group_by(ppn) %>%
  mutate(state_count = max(state_nt) + max(state_nsw) + max(state_vic) +
           max(state_qld) + max(state_sa) + max(state_wa) + max(state_tas) +
           max(state_nz) + max(state_act)) %>%
  mutate(state = if_else(is.na(state) & max(state_nsw) == 1 & state_count == 1, 2, state)) %>%
  ungroup() %>%
  mutate(state = to_character(state)) %>%
  select(ppn, graftno, state)


## Create a dataset with waitlist status changes
waitlist_activity <- safebod_omstatushistory %>%
  # Keep only waitlist activity related to kidney only (e.g. if listed for kidney/pancreas, only keep the kidney only wailtist activity)
  filter(organprogramname == "Kidney") %>%
  # Add transplant dates
  left_join(txdates) %>%
  # Add dates of death
  left_join(deathdates) %>% 
  # Remove all records occurring more than 2 days after date of death (just in case dates are off by 1 or 2 days)
  filter((waitdate < (deathdate + 2)) | is.na(deathdate)) %>%
  # Remove all records for people ever activated during period with functioning transplant
  mutate(flag = case_when(
    waitdate > txdate_1 & waitdate < txenddate_1 & waitstatus == 2 ~ 1,
    waitdate > txdate_2 & waitdate < txenddate_2 & waitstatus == 2  ~ 1,
    waitdate > txdate_3 & waitdate < txenddate_3 & waitstatus == 2  ~ 1,
    waitdate > txdate_4 & waitdate < txenddate_4 & waitstatus == 2  ~ 1,
    TRUE ~ 0)) %>%
  group_by(ppn) %>%
  mutate(everflag = max(flag)) %>%
  ungroup() %>%
  filter(everflag != 1) %>%
  # Remove all waitlist status changes during periods of functioning transplant
  mutate(tx = case_when(
    waitdate > txdate_1 & waitdate < txenddate_1 ~ 1,
    waitdate > txdate_2 & waitdate < txenddate_2 ~ 2,
    waitdate > txdate_3 & waitdate < txenddate_3 ~ 3,
    waitdate > txdate_4 & waitdate < txenddate_4 ~ 4,
    TRUE ~ 0)) %>%
  filter(tx == 0) %>%
  # Create a variable indicating which graftno waiting list activity relates to
  mutate(graftno = case_when(
    is.na(txdate_1) | waitdate <= txdate_1 ~ 1,
    is.na(txdate_2) | waitdate <= txdate_2 ~ 2,
    is.na(txdate_3) | waitdate <= txdate_3 ~ 3,
    is.na(txdate_4) | waitdate <= txdate_4 ~ 4,
    !is.na(txdate_4) ~ 5,
    TRUE ~ NA_integer_)) %>%
  select(ppn, graftno, everything()) %>% 
  # Add a row removing people from the waiting list at time of each transplant
  mutate(original = 1) %>%
  bind_rows(safebod_anzdatatransplants %>%
              mutate(waitdate = transplantdate,
                     waitstatus = 3,
                     original = 0) %>%
              select(ppn, graftno, waitdate, waitstatus, original)) %>%
  group_by(ppn) %>%
  mutate(ever_in_original = max(original, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(ever_in_original == 1) %>%
  select(-original, -ever_in_original) %>%
  # Add a row removing people from the waiting list at time of death
  mutate(original = 1) %>%
  bind_rows(deathdates %>%
              mutate(waitdate = deathdate,
                     waitstatus = 4,
                     original = 0) %>%
              select(ppn, waitdate, waitstatus, original)) %>%
  group_by(ppn) %>%
  mutate(ever_in_original = max(original, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(ever_in_original == 1) %>%
  select(-original, -ever_in_original) %>% 
  # Create a variable with waitlist status as a string
  mutate(waitstatus = case_when(
    waitstatus == 1 ~ "Removed",
    waitstatus == 2 ~ "Activated",
    waitstatus == 3 ~ "Transplanted",
    waitstatus == 4 ~ "Death",
    TRUE ~ NA_character_)) %>% 
  # Create a variable for activation sequence
  group_by(ppn, graftno) %>%
  mutate(activation_seq = cumsum(waitstatus == "Activated")) %>%
  ungroup() %>%
  select(ppn, graftno, waitseq, activation_seq, waitdate, waitstatus, everything()) %>%
  # Remove records for 'removals' which occur before first activation
  filter(activation_seq != 0) %>%
  # Keep only the actual status changes (i.e. keep only the first record in a 
  # block of 'on holds' since that is the first date on hold)
  group_by(ppn, graftno, activation_seq, waitstatus) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  # Create a variable for whether patient is currently active
  mutate(active = if_else(waitstatus == "Activated", 1, 0)) %>%
  # Remove records where status is removed if patient has not yet ever been activated
  group_by(ppn, graftno) %>%
  mutate(activation_cumcount = cumsum(waitstatus == "Activated")) %>%
  filter(!(waitstatus == "Removed" & activation_cumcount == 0)) %>%
  ungroup() %>% 
  # Update sequence variable
  group_by(ppn, graftno) %>%
  arrange(waitseq) %>%
  mutate(waitseq = row_number()) %>%
  ungroup() %>%
  # Reshape variables for transplant details into long format
  mutate(
    txdate = case_when(
      graftno == 1 ~ txdate_1,
      graftno == 2 ~ txdate_2,
      graftno == 3 ~ txdate_3,
      graftno == 4 ~ txdate_4,
      TRUE ~ NA_Date_),
    txfaildate = case_when(
      graftno == 1 ~ txfaildate_1,
      graftno == 2 ~ txfaildate_2,
      graftno == 3 ~ txfaildate_3,
      graftno == 4 ~ txfaildate_4,
      TRUE ~ NA_Date_),
    txtype = case_when(
      graftno == 1 ~ txtype_1,
      graftno == 2 ~ txtype_2,
      graftno == 3 ~ txtype_3,
      graftno == 4 ~ txtype_4,
      TRUE ~ NA_character_),
    txstate = case_when(
      graftno == 1 ~ txstate_1,
      graftno == 2 ~ txstate_2,
      graftno == 3 ~ txstate_3,
      graftno == 4 ~ txstate_4,
      TRUE ~ NA_real_),
    txexchange = case_when(
      graftno == 1 ~ txexchange_1,
      graftno == 2 ~ txexchange_2,
      graftno == 3 ~ txexchange_3,
      graftno == 4 ~ txexchange_4,
      TRUE ~ NA_real_)) %>%
  select(-ends_with("_1"), -ends_with("_2"), -ends_with("_3"), -ends_with("_4")) %>%
  group_by(ppn, graftno) %>%
  mutate(txdate = max(txdate, na.rm = TRUE),
         txfaildate = max(txfaildate, na.rm = TRUE),
         txtype = max(txtype, na.rm = TRUE),
         txstate = max(txstate, na.rm = TRUE),
         txexchange = max(txexchange, na.rm = TRUE)) %>%
  ungroup() %>%
  # Update status from 'removed' to 'deceased donor transplant' if removal is 
  # immediately followed by a transplant, or to 'living donor transplant' if
  # living donor transplant occurs within next 3 months or within 6 months for kidney exchange 
  # (ignoring cases where death is before transplant)
  mutate(waitseq_excl_death = if_else(waitstatus == "Death",
                                      waitseq + max(waitseq),
                                      waitseq)) %>%
  arrange(ppn, graftno, waitseq_excl_death) %>%
  mutate(waitstatus = case_when(
    waitstatus == "Removed" & 
      lead(waitstatus) == "Transplanted" & 
      txtype == "Deceased" ~ "Deceased donor transplant",
    waitstatus == "Removed" & 
      lead(waitstatus) == "Transplanted" & 
      txtype == "Living" & 
      txexchange == 0 &
      (lead(waitdate) - waitdate) <= (365.25 * (3/12)) ~ "Living donor transplant",
    waitstatus == "Removed" & 
      lead(waitstatus) == "Transplanted" & 
      txtype == "Living" & 
      txexchange == 1 &
      (lead(waitdate) - waitdate) <= (365.25 * (6/12)) ~ "Living donor transplant", # Allow 6 months for kidney exchange
    waitstatus == "Removed" & 
      lead(waitstatus) == "Transplanted" & 
      is.na(txtype) & 
      (lead(waitdate) - waitdate) <= (365.25 * (3/12)) ~ "Unknown transplant", # If transplanted within 3 months of removal, assume it was planned
    TRUE ~ waitstatus)) %>%
  select(-waitseq_excl_death) %>%
  arrange(ppn, graftno, waitseq) %>%
  # Update status from 'removed' to 'died' if removal is immediately followed by death
  arrange(ppn, graftno, waitseq) %>%
  mutate(waitstatus = case_when(
    waitstatus == "Removed" & lead(waitstatus) == "Death" ~ "Died",
    TRUE ~ waitstatus)) %>%
  # Remove records where waitstatus is 'transplanted' if it is ever preceded 
  # by a living donor or deceased donor transplant
  group_by(ppn, graftno) %>%
  mutate(cumcount_ever_tx = cumsum(waitstatus %in% c("Living donor transplant", "Deceased donor transplant", "Unknown transplant"))) %>%
  filter(!(waitstatus == "Transplanted" & cumcount_ever_tx != 0)) %>%
  ungroup() %>%
  select(-cumcount_ever_tx) %>%
  # Remove records where waitstatus is 'death' if it is ever preceded 
  # by 'died'
  group_by(ppn, graftno) %>%
  mutate(cumcount_ever_died = cumsum(waitstatus == "Death")) %>%
  filter(!(waitstatus == "Death" & cumcount_ever_died != 0)) %>%
  ungroup() %>%
  select(-cumcount_ever_died) %>%
  # Update status from 'transplanted' to 'living donor transplant' or 'deceased donor transplant'
  arrange(ppn, graftno, waitseq) %>%
  mutate(waitstatus = case_when(
    waitstatus == "Transplanted" & lag(txtype) == "Living" ~ "Living donor transplant",
    waitstatus == "Transplanted" & lag(txtype) == "Deceased" ~ "Deceased donor transplant",
    waitstatus == "Transplanted" ~ "Unknown transplant",
    TRUE ~ waitstatus)) %>%
  # Update status from 'death' to 'died'
  arrange(ppn, graftno, waitseq) %>%
  mutate(waitstatus = case_when(
    waitstatus == "Death" ~ "Died",
    TRUE ~ waitstatus)) %>%
  # Remove records where there was no activation for that graftno
  group_by(ppn, graftno) %>%
  mutate(ever_activated = max(active)) %>%
  filter(ever_activated == 1) %>%
  ungroup() %>% 
  # Remove records for subsequent removals/activations on the same day 
  # e.g. activated then removed = keep, since maybe a kidney was offered that day
  # activated then removed then activated then removed = keep one pair of activated then removed only)
  # activated then removed then activated = keep one activated only
  # PPNs 00000000000439, 00000000000481, 00000004319023, 00000009792179
  group_by(ppn, graftno, waitdate) %>%
  mutate(waitstatus_activated_seq = cumsum(waitstatus == "Activated"),
         max_waitstatus_activated_seq = max(waitstatus_activated_seq),
         waitstatus_removed_seq = cumsum(waitstatus == "Removed" | str_detect(waitstatus, "transplant")),
         max_waitstatus_removed_seq = max(waitstatus_removed_seq),
         complete_activation_removal_pairs = pmin(max_waitstatus_activated_seq, max_waitstatus_removed_seq)) %>%
  ungroup() %>%
  filter(!((complete_activation_removal_pairs >= 2) &
             (waitstatus_activated_seq <= complete_activation_removal_pairs) &
             (waitstatus_removed_seq <= complete_activation_removal_pairs))) %>%
  # Keep only NSW residents
  left_join(safebod_anzdata_state) %>%
  filter(state == "NSW") %>%
  # Remove people with > 1 death date identified earlier
  anti_join(excluded_ppns, by = "ppn") %>% 
  # Keep only relevant variables
  select(ppn, graftno, waitseq, activation_seq, waitdate, waitstatus, active) %>%
  # Sort data
  arrange(ppn, graftno, waitseq)


## Create dataset with kidney offers
kidneys_available <- safebod_anzoddestination %>%
  # Keep kidneys only
  filter(organcode %in% c(10, 11, 12, 13)) %>%
  # Create a variable for number of kidneys available (assume 2 kidneys available unless they were transplanted dual/en-bloc)
  group_by(ppn) %>%
  mutate(n_kidneys = max(if_else(organcode == 13, 1, 2))) %>%
  ungroup() %>%
  # Keep one row per PPN
  select(ppn, n_kidneys) %>%
  distinct() %>%
  arrange(ppn)


kidney_offers <- safebod_omdetails %>%
  # Remove records with missing recipient PPN
  filter(Recipient_PPN != "") %>%
  # Keep only NSW residents
  filter(RECIPIENT_RES_STATE == "NSW") %>%
  # Sort by recipient PPN
  arrange(Recipient_PPN, Matched_Date, MATCH_EVENT_ID) %>%
  # Add number of kidneys available (assume 2 kidneys available from interstate donors where data unavailable)
  left_join(kidneys_available, by = c("DONOR_PPN" = "ppn")) %>%
  mutate(n_kidneys = replace_na(n_kidneys, 2)) %>% 
  # Remove duplicate offers (same donor to same recipient)
  # Keep either the offer that was accepted or the highest ranked offer
  mutate(transplanted_sortseq = case_when(
    Transplanted == "1" ~ 1,
    Transplanted == "0" ~ 2,
    Transplanted == "NULL" ~ 3)) %>%
  arrange(RECIPIENT_OM_ID, DONOR_OM_ID, transplanted_sortseq, Rank) %>%
  group_by(RECIPIENT_OM_ID, DONOR_OM_ID) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  # Create variables for cumulative number of kidneys transplanted from each donor
  arrange(DONOR_OM_ID, Rank) %>% 
  mutate(tx = (Transplanted == "1") * 1) %>%
  group_by(DONOR_OM_ID) %>%
  mutate(cum_tx_count = cumsum(tx)) %>%
  ungroup() %>%
  mutate(n_kidneys_remaining = n_kidneys - cum_tx_count) %>%
  # Keep only the records relating to 'real' offers 
  # (i.e. recipient ranked above where last available kidney was transplanted, or recipient was transplanted)
  filter(n_kidneys_remaining != 0 | tx == 1) %>%
  # Create a variable for whether the offer was accepted
  mutate(offer_accepted = tx) %>%
  # Add transplant dates
  left_join(txdates, by = c("Recipient_PPN" = "ppn")) %>%
  # Create a variable indicating which graftno matching record relates to
  mutate(graftno = case_when(
    is.na(txdate_1) | Matched_Date <= txdate_1 ~ 1,
    is.na(txdate_2) | Matched_Date <= txdate_2 ~ 2,
    is.na(txdate_3) | Matched_Date <= txdate_3 ~ 3,
    is.na(txdate_4) | Matched_Date <= txdate_4 ~ 4,
    !is.na(txdate_4) ~ 5,
    TRUE ~ NA_integer_)) %>%
  # Rename variables
  rename(ppn = Recipient_PPN,
         donor_ppn = DONOR_PPN,
         offer_date = Matched_Date) %>%
  # Create a sequence variable (to sort multiple offers on the same day)
  arrange(ppn, graftno, offer_date, -offer_accepted) %>%
  group_by(ppn, graftno) %>%
  mutate(offer_seq = row_number()) %>%
  ungroup() %>%
  # Keep only the relevant variables
  select(ppn, graftno, offer_seq, offer_date, offer_accepted, donor_ppn)


## Create a cohort for analysis of time to next offer
cohort_offers_suspensions <- waitlist_activity %>% # Start with everybody ever waitlisted
  # Combine waitlist activity and kidney offers into a single dataset
  mutate(dataset = "waitlist") %>%
  bind_rows(kidney_offers %>% mutate(dataset = "offers")) %>%
  # Restrict to NSW residents only
  #   Keep only those who had any kidney offers (since this includes NSW residents only), OR
  #     if they had no kidney offers but were waitlisted while resident in NSW and never received a deceased donor transplant
  group_by(ppn, graftno) %>%
  mutate(ever_kidney_offer = max(dataset == "offers"),
         ever_waitlisted = max(dataset == "waitlist"), # prev line was ever_waitlisted = max(dataset == "offers"), results in cohort of n = 2962
         ever_dtx = max(waitstatus == "Deceased donor transplant")) %>%
  ungroup() %>%
  filter(ever_kidney_offer == 1 | 
           (ever_kidney_offer == 0 & ever_waitlisted == 1 & ever_dtx == 0)) %>%
  # Create a single variable for event date
  mutate(date = pmin(waitdate, offer_date, na.rm = TRUE)) %>% 
  # Create a date sequence variable with value of 0 if date only appears once for that PPN and graftno
  arrange(ppn, graftno, date) %>%
  mutate(dateseq = as.integer(date)) %>%
  group_by(ppn, graftno, date) %>%
  mutate(datecount = n()) %>%
  ungroup() %>%
  mutate(dateseq = if_else(datecount == 1, 0, dateseq)) %>%
  # Create binary variables for activated and removed
  mutate(activated = replace_na(waitstatus == "Activated", 0) * 1,
         removed = replace_na(waitstatus == "Removed" | str_detect(waitstatus, "transplant"), 0) * 1) %>%
  # Create variables for waitseq where missing values (i.e. records from offers dataset) are minimal or maximal
  mutate(waitseq_min = replace_na(waitseq, 0),
         waitseq_max = replace_na(waitseq, 999999)) %>%
  # Sort by event date, making sure that offers are in the correct order so they only occur while patient is active
  # e.g. check PPN 00000007450802
  group_by(ppn, graftno, dateseq) %>%
  mutate(sortseq = case_when(
    datecount == 1 ~ 1, # If only one record on that date, don't need sortseq
    sum(dataset == "waitlist") == 0 ~ offer_seq, # if all records are kidney offers, just sort by offer_seq
    sum(dataset == "offers") == 0 ~ waitseq, # if all records are waitlist activity, just sort by waitseq
    dataset == "waitlist" ~ waitseq, # If there are both offers and waitlist activity, then use waitseq for waitlist activity
    dataset == "offers" & sum(removed) >= 1 ~ 
      max(waitseq_min * removed) - 0.1, # If there are both offers and waitlist activity including a removal, then make sure offers occur immediately before last removal 
    dataset == "offers" & sum(activated) >= 1 ~ 
      min(waitseq_max * activated) + 0.1, # If there are both offers and waitlist activity but no removals, then make sure offers occur immediately after first activation
    TRUE ~ NA)) %>%
  ungroup() %>%
  arrange(ppn, graftno, date, sortseq) %>%
  group_by(ppn, graftno, date) %>%
  mutate(sortseq = row_number()) %>%
  ungroup() %>%
  # Create a variable for whether patient was active on the waitlist during each period
  group_by(ppn, graftno) %>%
  mutate(group = cumsum(dataset == "waitlist")) %>%
  ungroup() %>%
  group_by(ppn, graftno, group) %>%
  mutate(active = max(0, active, na.rm = TRUE)) %>%
  ungroup() %>% 
  # Keep only those who were ever active on the waitlist 
  # (excludes some with kidney offers after end of waitlist data in 2019)
  group_by(ppn, graftno) %>%
  mutate(ever_active = max(active)) %>%
  ungroup() %>%
  filter(ever_active == 1) %>%
  # Keep only the records after first activation
  group_by(ppn, graftno) %>%
  filter(cumsum(active) >= 1) %>%
  ungroup() %>%
  # Add censoring dates (death, living donor transplant, end of follow up)
  left_join(deathdates) %>% # Death
  mutate(lastfollowup_date = as.Date("2019-12-31")) %>% # End of follow-up
  left_join(safebod_anzdatatransplants %>% # Living donor transplants
              filter(donorsourcecode != 100) %>%
              rename(livingdonortx_date = transplantdate) %>%
              select(ppn, graftno, livingdonortx_date), 
            by = c("ppn", "graftno")) %>%
  # Filter out observations with dates after follow up date
  filter(date <= lastfollowup_date) %>%
  # Add extra row for each person with last event
  mutate(original = 1) %>%
  group_by(ppn) %>%
  group_modify(~ add_row(., original = 0)) %>%
  mutate(graftno = if_else(original == 0, max(graftno, na.rm = TRUE), graftno)) %>%
  ungroup() %>% 
  group_by(ppn, graftno) %>%
  mutate(deathdate = if_else(original == 0, max(deathdate, na.rm = TRUE), deathdate) %>% na_if(as.Date(-Inf)),
         lastfollowup_date = if_else(original == 0, max(lastfollowup_date, na.rm = TRUE), lastfollowup_date) %>% na_if(as.Date(-Inf)),
         livingdonortx_date = if_else(original == 0, max(livingdonortx_date, na.rm = TRUE), livingdonortx_date) %>% na_if(as.Date(-Inf)),
         date = if_else(original == 0, pmin(deathdate, lastfollowup_date, livingdonortx_date, na.rm = TRUE), date)) %>%
  # Create variables for start and end of each period
  group_by(ppn, graftno) %>%
  mutate(start = lag(date),
         end = date,
         origin = min(start, na.rm = TRUE)) %>%
  ungroup() %>%
  # Create variables for outcome after end of period
  mutate(outcome = case_when(
    date > as.Date("2019-12-31") ~ "End of follow-up",
    dataset == "offers" & offer_accepted == 0 ~ "Declined offer",
    dataset == "offers" & offer_accepted == 1 ~ "Accepted offer",
    dataset == "waitlist" & waitstatus == "Removed" ~ "Suspension",
    dataset == "waitlist" & waitstatus == "Activated" ~ "Re-activation",
    dataset == "waitlist" & waitstatus == "Deceased donor transplant" ~ "Deceased donor transplant",
    dataset == "waitlist" & waitstatus == "Living donor transplant" ~ "Living donor transplant",
    dataset == "waitlist" & waitstatus == "Unknown transplant" ~ "Unknown transplant",
    end == livingdonortx_date ~ "Living donor transplant",
    end == deathdate ~ "Death",
    end == lastfollowup_date ~ "End of follow-up",
    TRUE ~ NA_character_)) %>%
  # Remove records after accepted offer unless it is a deceased donor transplant 
  # or if patient never received a deceased donor transplant
  # e.g. PPN 00000002000792, 00000002209276
  group_by(ppn, graftno) %>%
  mutate(accepted_cumcount = cumsum(outcome == "Accepted offer"),
         dtx_cumcount = cumsum(outcome == "Deceased donor transplant"),
         ever_dtx = max(outcome == "Deceased donor transplant")) %>%
  ungroup() %>%
  filter(!(ever_dtx != 0 & 
             !(outcome %in% c("Accepted offer", "Deceased donor transplant")) & 
             accepted_cumcount != dtx_cumcount)) %>%
  # Replace outcome as accepted offer and transplanted if accepted offer is immediately followed by deceased donor transplant
  group_by(ppn, graftno) %>%
  mutate(outcome = case_when(
    outcome == "Accepted offer" & 
      lead(outcome) == "Deceased donor transplant" & 
      abs(lead(end) - lead(start)) <= 2 ~ "Accepted offer (transplanted)", # transplant within 2 days of offer
    outcome == "Accepted offer" & 
      (lead(outcome) != "Deceased donor transplant" | 
         abs(lead(end) - lead(start)) > 2) ~ "Accepted offer (not transplanted)", # transplant more than 2 days after offer
    outcome == "Accepted offer" & end == as.Date('2019-12-31') ~ "Accepted offer (transplanted)", # Accepted offer at end of follow-up, assume transplanted
    outcome == "Deceased donor transplant" & 
      lead(outcome) == "Accepted offer" &
      abs(lead(end) - lead(start)) <= 2 ~ "Accepted offer (transplanted)", # offer recorded within 2 days after transplant
    outcome == "Deceased donor transplant" & 
      (is.na(lead(outcome)) | lead(outcome) != "Accepted offer") ~ "Accepted offer (transplanted)", # transplant without a corresponding offer, assume an offer was place. This can happen when transplant is interstate
    TRUE ~ outcome)) %>%
  ungroup() %>%
  # Remove first record for people with multiple records since start date is reflected from 2nd record onwards
  filter(!is.na(start)) %>% 
  # Remove records where start date is after end date
  filter(start <= end) %>%
  # Remove records after transplant
  group_by(ppn, graftno) %>%
  mutate(transplanted_cumcount = cumsum(outcome %in% c("Accepted offer (transplanted)", 
                                                       "Living donor transplant", 
                                                       "Unknown transplant"))) %>%
  filter(transplanted_cumcount == 0 | 
           (transplanted_cumcount == 1 & outcome %in% c("Accepted offer (transplanted)", 
                                                        "Living donor transplant", 
                                                        "Unknown transplant"))) %>%
  ungroup() %>%
  # Create a variable for whether patient was active during period
  group_by(ppn, graftno) %>%
  mutate(suspended_cumcount = lag(cumsum(outcome == "Suspension")),
         activation_cumcount = lag(cumsum(outcome == "Re-activation") + 1),
         active = case_when(
           is.na(lag(outcome)) & outcome == "Re-activation" ~ 0,
           is.na(lag(outcome)) ~ 1,
           suspended_cumcount == activation_cumcount ~ 0,
           outcome == "Re-activation" ~ 0,
           TRUE ~ 1)) %>%
  ungroup() %>%
  # Remove patients who were never active
  group_by(ppn, graftno) %>%
  mutate(ever_active = max(active)) %>%
  ungroup() %>%
  filter(ever_active == 1) %>%
  # Create a variable for ordering records
  arrange(ppn, graftno, date, sortseq) %>%
  group_by(ppn, graftno) %>%
  mutate(order = row_number()) %>%
  ungroup() %>%
  # Add donor details for those with a transplant but no data on the offer
  # This can happen when people move interstate, so their offers are no longer captured
  mutate(lookup_ppn = if_else(outcome == "Accepted offer (transplanted)" & is.na(donor_ppn), ppn, NA_character_),
         lookup_graftno = if_else(outcome == "Accepted offer (transplanted)" & is.na(donor_ppn), graftno, NA_real_)) %>%
  left_join(safebod_donor_recipient_mapping %>%
              filter(organ == 5,
                     donor_type == 1) %>%
              mutate(lookup_donorppn = donor_ppn) %>%
              select(recipient_ppn, graftno, lookup_donorppn),
            by = join_by(lookup_ppn == recipient_ppn, lookup_graftno == graftno)) %>% 
  mutate(donor_ppn = coalesce(donor_ppn, lookup_donorppn)) %>%
  mutate(donor_ppn = if_else(outcome == "Accepted offer (transplanted)" & is.na(donor_ppn), "", donor_ppn)) %>%
  # Only keep the relevant variables
  arrange(ppn, graftno, order) %>%
  select(ppn, graftno, order, active, origin, start, end, outcome, 
         donor_ppn)


## Add patient characteristics
### Person-level characteristics
cohort_offers_suspensions_birthdate <- cohort_offers_suspensions %>%
  select(ppn) %>%
  distinct() %>%
  left_join(safebod_anzdatapatients %>%
              rename(birthdate = dateofbirth) %>%
              select(ppn, birthdate))

cohort_offers_suspensions_sex <- cohort_offers_suspensions %>%
  select(ppn) %>%
  distinct() %>%
  left_join(safebod_anzdatapatients %>%
              mutate(sex = factor(gendercode,
                                  levels = c("Male", "Female"),
                                  labels = c("Male", "Female"))) %>%
              mutate(female = (sex == "Female") * 1) %>%
              select(ppn, sex, female))

cohort_offers_suspensions_bloodgroup <- cohort_offers_suspensions %>%
  select(ppn) %>%
  distinct() %>%
  left_join(safebod_omrecip %>% 
              mutate(ppn = Recipient_PPN,
                     bloodgroup_omrecip = RECIPIENT_BLOOD_TYPE) %>%
              select(ppn, bloodgroup_omrecip) %>%
              filter(ppn != "") %>%
              distinct() %>%
              group_by(ppn) %>% 
              mutate(seq = row_number()) %>%
              ungroup() %>%
              pivot_wider(id_cols = "ppn", 
                          values_from = "bloodgroup_omrecip",
                          names_from = "seq",
                          names_prefix = "bloodgroup_omrecip")) %>% 
  left_join(safebod_omdetails %>% 
              mutate(ppn = Recipient_PPN,
                     bloodgroup_omdetails = RECIPIENT_FINAL_ABO) %>%
              select(ppn, bloodgroup_omdetails) %>%
              filter(ppn != "") %>%
              distinct() %>%
              group_by(ppn) %>% 
              mutate(seq = row_number()) %>%
              ungroup() %>%
              pivot_wider(id_cols = "ppn", 
                          values_from = "bloodgroup_omdetails",
                          names_from = "seq",
                          names_prefix = "bloodgroup_omdetails")) %>%
  left_join(safebod_anzdatatransplants %>%
              mutate(bloodgroup_anzdatatransplants = str_replace_all(recip_bloodgroupcode, "\\d", "")) %>%
              select(ppn, bloodgroup_anzdatatransplants) %>%
              distinct() %>%
              group_by(ppn) %>%
              mutate(seq = row_number()) %>%
              ungroup() %>%
              pivot_wider(id_cols = "ppn", 
                          values_from = "bloodgroup_anzdatatransplants",
                          names_from = "seq",
                          names_prefix = "bloodgroup_anzdatatransplants")) %>%
  mutate(
    bloodgroup_o = replace_na(pmax(
      bloodgroup_omrecip1 == "O",
      bloodgroup_omrecip2 == "O",
      bloodgroup_omdetails1 == "O",
      bloodgroup_omdetails2 == "O",
      bloodgroup_anzdatatransplants1 == "O",
      na.rm = TRUE), 0),
    bloodgroup_a = replace_na(pmax(
      bloodgroup_omrecip1 == "A",
      bloodgroup_omrecip2 == "A",
      bloodgroup_omdetails1 == "A",
      bloodgroup_omdetails2 == "A",
      bloodgroup_anzdatatransplants1 == "A",
      na.rm = TRUE), 0),
    bloodgroup_b = replace_na(pmax(
      bloodgroup_omrecip1 == "B",
      bloodgroup_omrecip2 == "B",
      bloodgroup_omdetails1 == "B",
      bloodgroup_omdetails2 == "B",
      bloodgroup_anzdatatransplants1 == "B",
      na.rm = TRUE), 0),
    bloodgroup_ab = replace_na(pmax(
      bloodgroup_omrecip1 == "AB",
      bloodgroup_omrecip2 == "AB",
      bloodgroup_omdetails1 == "AB",
      bloodgroup_omdetails2 == "AB",
      bloodgroup_anzdatatransplants1 == "AB",
      na.rm = TRUE), 0),
    bloodgroup = case_when(
      (bloodgroup_o + bloodgroup_a + bloodgroup_b + bloodgroup_ab) == 1 & bloodgroup_o == 1 ~ "O",
      (bloodgroup_o + bloodgroup_a + bloodgroup_b + bloodgroup_ab) == 1 & bloodgroup_a == 1 ~ "A",
      (bloodgroup_o + bloodgroup_a + bloodgroup_b + bloodgroup_ab) == 1 & bloodgroup_b == 1 ~ "B",
      (bloodgroup_o + bloodgroup_a + bloodgroup_b + bloodgroup_ab) == 1 & bloodgroup_ab == 1 ~ "AB",
      (bloodgroup_o + bloodgroup_a + bloodgroup_b + bloodgroup_ab) == 0 ~ NA_character_,
      TRUE ~ bloodgroup_omrecip1)) %>% # In cases of conflict, assume first reported bloodgroup by Organ Match is correct
  mutate(bloodgroup = factor(bloodgroup,
                             levels = c("O", "A", "B", "AB"))) %>%
  select(ppn, bloodgroup)


cohort_offers_suspensions_kidneydisease <- cohort_offers_suspensions %>%
  select(ppn) %>%
  distinct() %>%
  left_join(safebod_anzdatapatients %>%
              mutate(kidneydisease = factor(
                case_when(
                  primaryrenaldiseasecode %in% c(0) ~ "Other",
                  primaryrenaldiseasecode %in% c(1) ~ "Uncertain",
                  primaryrenaldiseasecode %between% c(100, 182)  ~ "Glomerular disease",
                  primaryrenaldiseasecode %between% c(190, 199)  ~ "Glomerular disease",
                  primaryrenaldiseasecode %in% c(302) ~ "Hypertension",
                  primaryrenaldiseasecode %between% c(400, 499)  ~ "Polycystic disease",
                  primaryrenaldiseasecode %in% c(500) ~ "Reflux nephropathy",
                  primaryrenaldiseasecode %between% c(800, 899)  ~ "Diabetes",
                  primaryrenaldiseaseother == "HYPERTENSION" ~ "Hypertension",
                  primaryrenaldiseaseother == "IGA NEPHROPATHY" ~ "Glomerular disease",
                  !is.na(primaryrenaldiseasecode) | primaryrenaldiseaseother != "" ~ "Other",
                  TRUE ~ NA_character_),
                levels = c("Glomerular disease", "Diabetes", "Polycystic disease", 
                           "Hypertension", "Reflux nephropathy", "Other"),
                labels = c("Glomerular disease", "Diabetes", "Polycystic disease", 
                           "Hypertension", "Reflux nephropathy", "Other"))) %>%
              select(ppn, kidneydisease))


cohort_offers_suspensions_rrtstartdate <- cohort_offers_suspensions %>%
  select(ppn) %>%
  distinct() %>%
  left_join(safebod_anzdatatransplants %>% 
              group_by(ppn) %>%
              mutate(first_txdate = min(transplantdate)) %>%
              ungroup() %>%
              select(ppn, first_txdate) %>%
              distinct()) %>%
  left_join(safebod_anzdatacourseoftreatments %>% 
              filter(str_detect(treatmentcode_text, "PD|HD")) %>%
              group_by(ppn) %>%
              mutate(first_dialysisdate = min(treatmentdate)) %>%
              ungroup() %>%
              select(ppn, first_dialysisdate) %>%
              distinct()) %>%
  mutate(rrtstartdate = pmin(first_txdate, first_dialysisdate, na.rm = TRUE)) %>%
  select(ppn, rrtstartdate)


### Graft-level characteristics
mpra <- safebod_omdetails %>% 
  # Rename PPN
  rename(ppn = Recipient_PPN) %>%
  # Remove records with missing PPN
  filter(ppn != "") %>%
  # Extract mPRA for each record
  mutate(mpra1 = as.numeric(RECIPIENT_MPRA),
         mpra2 = str_extract(PRE_01042019_MATCH_EVENT_HISTO, "(?<=Class 1 Antibody Peak was )\\d+") %>% as.numeric()) %>%
  # Add transplant dates
  left_join(txdates) %>%
  # Create a variable indicating which graftno matching record relates to
  mutate(graftno = case_when(
    is.na(txdate_1) | Matched_Date <= txdate_1 ~ 1,
    is.na(txdate_2) | Matched_Date <= txdate_2 ~ 2,
    is.na(txdate_3) | Matched_Date <= txdate_3 ~ 3,
    is.na(txdate_4) | Matched_Date <= txdate_4 ~ 4,
    !is.na(txdate_4) ~ 5,
    TRUE ~ NA_integer_)) %>%
  # Add mPRA from ANZDATA
  full_join(safebod_anzdatatransplants %>%
              select(ppn, graftno, maxcytotoxicantibodies) %>%
              pivot_wider(id_cols = "ppn",
                          names_prefix = "anzdata_mpra_",
                          names_from = "graftno",
                          values_from = "maxcytotoxicantibodies")) %>%
  # Create variable with a single mPRA for each record
  select(ppn, graftno, contains("mpra")) %>%
  arrange(ppn, graftno) %>% 
  group_by(ppn, graftno) %>%
  mutate(organmatch_mpra = max(coalesce(mpra1, mpra2), na.rm = TRUE),
         anzdata_mpra = case_when(
           graftno == 1 ~ anzdata_mpra_1,
           graftno == 2 ~ anzdata_mpra_2,
           graftno == 3 ~ anzdata_mpra_3,
           graftno == 4 ~ anzdata_mpra_4,
           TRUE ~ NA)) %>%
  mutate(mpra = max(pmax(organmatch_mpra, anzdata_mpra, na.rm = TRUE), na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(ppn) %>%         
  mutate(mpra = replace_na(mpra, max(mpra, na.rm = TRUE))) %>%
  ungroup() %>%
  # Keep one row per person per graftno
  select(ppn, graftno, mpra) %>%
  distinct() %>%
  # Remove records with no mPRA data
  filter(mpra != -Inf) %>%
  # Sort by PPN
  arrange(ppn, graftno)


cohort_offers_suspensions_mpra <- cohort_offers_suspensions %>%
  select(ppn, graftno) %>%
  distinct() %>%
  left_join(mpra)

cohort_offers_suspensions_prevtxcount <- cohort_offers_suspensions %>%
  select(ppn, graftno) %>%
  distinct() %>%
  mutate(prevtxcount = graftno - 1)


### Time-varying characteristics
cohort_offers_suspensions_comorbdates <- cohort_offers_suspensions %>%
  select(ppn) %>%
  distinct() %>%
  left_join(safebod_anzdatacomorbidities) %>%
  arrange(ppn, codate) %>%
  group_by(ppn) %>%
  mutate(everchroniclung = cummax((chroniclungcode %in% c('S','Y'))*1),
         evercoronaryartery = cummax((coronaryarterycode %in% c('S','Y'))*1),
         everperipheralvascular = cummax((peripheralvascularcode %in% c('S','Y'))*1),
         evercerebrovascular = cummax((cerebrovascularcode %in% c('S','Y'))*1),
         everdiabetes = cummax((diabetescode %in% c('O','P','Q'))*1),
         everhepatitisc = cummax((hepatitisccode %in% c(1))*1)) %>%
  ungroup() %>%
  mutate(comorbcount =
           everchroniclung + 
           evercoronaryartery + 
           everperipheralvascular + 
           evercerebrovascular + 
           everdiabetes + 
           everhepatitisc) %>% 
  # Keep only the date on which number of comorbidities increased
  select(ppn, codate, comorbcount) %>%
  group_by(ppn, comorbcount) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  # Reshape to one row per person
  group_by(ppn) %>%
  mutate(seq = row_number()) %>%
  ungroup() %>%
  pivot_wider(id_cols = "ppn",
              values_from = c("codate", "comorbcount"),
              names_from = "seq")



# Createcohort for model of time to next offer
cohort_timetonextoffer <- cohort_offers_suspensions %>%
  group_by(ppn, graftno) %>%
  mutate(offer_seq = cumsum(replace_na(str_detect(lag(outcome), "offer"), 0)) + 1) %>%
  ungroup() %>% 
  group_by(ppn, graftno, offer_seq) %>%
  mutate(start = min(start)) %>%
  filter(row_number() == n()) %>%
  ungroup() %>%
  # Create a survival outcome variable (offer yes vs no)
  mutate(offer = str_detect(outcome, "offer") * 1) %>%
  # Create a survival time variable (number of days to next offer)
  mutate(time = pmax(as.numeric(end - start), 0.01)) %>%
  # Keep only the relevant variables
  select(ppn, graftno, offer_seq, start, end, time, outcome, offer) %>%
  # Age
  left_join(cohort_offers_suspensions_birthdate) %>%
  mutate(age = as.numeric(start - birthdate)/365.25) %>%
  # Sex
  left_join(cohort_offers_suspensions_sex) %>%
  # Bloodgroup
  left_join(cohort_offers_suspensions_bloodgroup) %>%
  # Primary kidney disease
  left_join(cohort_offers_suspensions_kidneydisease) %>%
  # Time since kidney failure
  left_join(cohort_offers_suspensions_rrtstartdate) %>%
  # Filter out those < 18 at rrtstartdate
  mutate(agerrtstart = as.numeric(rrtstartdate - birthdate)/365.25) %>% 
  filter(agerrtstart >= 18) %>% 
  select(-birthdate, -agerrtstart) %>% 
  # Filter out people with start dates before rrt start date
  filter(rrtstartdate < start + 3) %>% 
  group_by(ppn) %>% 
  mutate(firststart = min(start),
         rrtstartdate = min(firststart, rrtstartdate)) %>% 
  ungroup() %>% 
  mutate(eskd_years = as.numeric(start - rrtstartdate)/365.25) %>%
  # Cumulative time on dialysis
  left_join(cohort_offers_suspensions_rrtstartdate) %>%
  left_join(txdates %>% select(ppn, starts_with('txdate_'), starts_with('txfaildate_'))) %>%
  mutate(dialysis_years = (1/365.25) * as.numeric(case_when(
    graftno == 1 ~ (start - rrtstartdate),
    graftno == 2 ~ (txdate_1 - rrtstartdate) + (start - txfaildate_1),
    graftno == 3 ~ (txdate_1 - rrtstartdate) + (txdate_2 - txfaildate_1) + (start - txfaildate_2),
    graftno == 4 ~ (txdate_1 - rrtstartdate) + (txdate_2 - txfaildate_1) + (txdate_3 - txfaildate_2) + (start - txfaildate_3),
    TRUE ~ NA))) %>%
  # Add waiting time variable
  mutate(waiting_months = (12/365.25) * as.numeric(case_when(
    graftno == 1 ~ (start - rrtstartdate),
    graftno == 2 ~ (start - txfaildate_1),
    graftno == 3 ~ (start - txfaildate_2),
    graftno == 4 ~ (start - txfaildate_3),
    TRUE ~ NA))) %>%
  # Add previous dialysis years variable
  mutate(prior_dialysis_years = dialysis_years - waiting_months/12) %>%
  select(-rrtstartdate, -starts_with('txdate_'), -starts_with('txfaildate_')) %>%
  # Add ethnicity, encode
  left_join(safebod_anzdatapatients %>% select(ppn, ethnicity1code, ethnicity1other, ethnicity2code, ethnicity2other)) %>% 
  mutate(ethnicity = factor(
    case_when(
      is.na(ethnicity1code) & ethnicity1other == "" & 
        is.na(ethnicity2code) & ethnicity2other == "" ~ NA,
      ethnicity1code %in% c(1102, 1103, 1104) | ethnicity2code %in% c(1102, 1103, 1104) ~ "Indigenous",
      between(ethnicity1code, 5000, 7999) | between(ethnicity2code, 5000, 7999) ~ "Asian",
      !(ethnicity1code %in% c(8100, 8200, 1101, 1202, 10, 1) | 
          between(ethnicity1code, 2000, 3999)) | 
        !(ethnicity2code %in% c(8100, 8200, 1101, 1202, 10, 1) | 
            between(ethnicity2code, 2000, 3999))  ~ "Other",
      TRUE ~ "White"), 
    levels = c("White", "Indigenous", "Asian", "Other"))) %>%
# Sensitisation (mPRA)
  left_join(cohort_offers_suspensions_mpra) %>%
  # PRevious transplants
  left_join(cohort_offers_suspensions_prevtxcount) %>%
  # Number of comorbidities
  left_join(cohort_offers_suspensions_comorbdates) %>%
  mutate(comorbcount = case_when(
    codate_6 >= start & codate_6 <= end ~ comorbcount_6,
    codate_5 >= start & codate_5 <= end ~ comorbcount_5,
    codate_4 >= start & codate_4 <= end ~ comorbcount_4,
    codate_3 >= start & codate_3 <= end ~ comorbcount_3,
    codate_2 >= start & codate_2 <= end ~ comorbcount_2,
    codate_1 >= start & codate_1 <= end ~ comorbcount_1,
    TRUE ~ 0)) %>%
  select(-starts_with("codate_"), -starts_with("comorbcount_")) %>% 
  # PRA is 0 if missing, if blood group missing, is truly missing
  mutate(mpra = ifelse((is.na(mpra) & !is.na(bloodgroup)), 0, mpra),
         # Kidney disease other if missing
         kidneydisease = ifelse(is.na(kidneydisease), 
                                "Other", 
                                as.character(kidneydisease)),
         # Dialysis and eskd years can't be below 0
         prior_dialysis_years = ifelse(prior_dialysis_years < 0, 0, prior_dialysis_years),
         dialysis_years = ifelse(dialysis_years  < 0, 0, dialysis_years),
         eskd_years = ifelse(eskd_years  < 0, 0, eskd_years)) %>% 
  # Kidney disease as factor
  mutate(kidneydisease = factor(kidneydisease, 
                                levels = levels(cohort_offers_suspensions_kidneydisease$kidneydisease))) %>%
  select(-ethnicity1code, -ethnicity1other, -ethnicity2code, -ethnicity2other) %>% 
  # Add extra blood group data
  left_join(safebod_anzdatabloodgroup) %>% 
  mutate(bloodgroup = case_when(!is.na(bloodgroup) ~ bloodgroup,
                                TRUE ~ factor(bloodgroupcode, 
                                                 levels = levels(bloodgroup))))

