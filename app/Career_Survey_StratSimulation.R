#######################################
# Test Stratification Career Survey
#######################################
# Autor: Arian Studer
# Date: 06.11.2025


# stratification variables:
#   - field of study (FoS): STEM-IT (computer science (CS), math, physics, electrical engineering (EE), statistics), non-STEM-IT (all other fields) 
#   - parental tertiary education: yes (high SES), no (low SES) 
#   - gender: man, woman, other, prefer not to say

# rules: 
# rule 1: Module 3 gets 75% of computer science
# rule 2: module 3 gets 40% of the total pool of math, statistics, physics, EE 
# rule 3: module 3 gets 0 % of non-STEM-IT (hard exclusion)
# rule 4: module 1 gets 70 & of interviewees with Low SES & Non-STEM-IT
# rule 5: STEM-IT which are not in module 3 are equally split across module 1, 2 and 4 
# rule 6: Non-STEM-IT & low SES which are not in module 1 are equally split between modules 2 and 4
# rule 7: non-STEM-IT & high SES are equally split between modules 1, 2 and 4 
# rule 8: gender should be rougly balanced within the modules but do not affect probabilities
# rule 9: when module 3 reaches a total of 1500 it becomes ineligible. the probabilieties are re-normalized over the remaining eligible modules. 
# rule 10: when module 1 reaches 1000 participants with low SES, the 70% boost is stoped and the probabilities are re-normalized over eligible modules.
# rule 11: when rule 9 and 10 are in effect, we use equal randomization across eligible modules for each stratum.
# general: all rules concerning module 3 (rule 1, rule 2 and rule 3) have higher priority than the other rules.


# Changes:
# 10.11. new rule
#     rule 12: no PH participants for Module 1? (Cecilia)
#     additional fos_level "PH"
#     updated base_probs so "PH" follows NonSTEM branches
#     updated apply_caps to treat "PH" as ineligible for M1




# ---- libraries ----
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
})


# ---- Helper: enumerate allowed levels ----
fos_levels   <- c("CS", "MSPEE", "NonSTEM", "PH")
ses_levels   <- c("Low", "High")
gender_levels<- c("man","woman","other","pns")
module_levels<- c("M1","M2","M3","M4")
leadership_levels <- c("Low", "High")

# ---- Base probabilities per stratum (Rules 1–7, priority for M3 rules) ----
base_probs <- function(fos, ses) {
  # Returns named numeric vector c(M1,M2,M3,M4)
  if (fos == "CS") {
    # Rule 1 + 5
    return(c(M1=1/12, M2=1/12, M3=0.75, M4=1/12))
  }
  if (fos == "MSPEE") {
    # Rule 2 + 5
    return(c(M1=0.20, M2=0.20, M3=0.40, M4=0.20))
  }
  # Treat PH as a subcategory of NonSTEM (same base probs as NonSTEM)
  if (fos %in% c("NonSTEM", "PH") && ses == "Low") {
    # Rule 3 + 4 + 6
    return(c(M1=0.70, M2=0.15, M3=0.00, M4=0.15))
  }
  if (fos %in% c("NonSTEM", "PH") && ses == "High") {
    # Rule 3 + 7
    return(c(M1=1/3, M2=1/3, M3=0.00, M4=1/3))
  }
  stop("Unexpected stratum in base_probs().")
}

# ---- Apply caps & renormalization (Rules 9–11) ----------------------------
# p is named vector c(M1,M2,M3,M4)
# caps$m3_closed (TRUE if M3 >= 1500), caps$m1_low_closed (TRUE if M1 Low-SES >= 1000)
# Implement our explicit refinements:
# - If M3 closed: set M3=0, renormalize
# - If M1 Low-SES closed: for NonSTEM & Low only, revert to equal split over M1,M2,M4
# - If both closed: equal randomization across eligible modules for each stratum (Rule 11)

apply_caps <- function(p, fos, ses, caps) {
  # p is named vector c(M1,M2,M3,M4)
  
  # build eligibility from caps
  ineligible <- character(0)
  if (isTRUE(caps$m3_closed)) ineligible <- c(ineligible, "M3")
  # Hard exclusion: PH must never be assigned M1
  if (fos == "PH") ineligible <- c(ineligible, "M1")
  # Cap 2 never makes M1 ineligible in general; but PH hard exclusion still applies above.
  
  # Ensure any explicitly ineligible modules are zero
  p[ineligible] <- 0
  
  # BOTH caps active → equal split across eligible modules (Rule 11)
  if (isTRUE(caps$m3_closed) && isTRUE(caps$m1_low_closed)) {
    eligible <- setdiff(names(p), ineligible)        # => M1, M2, M4
    p[] <- 0
    p[eligible] <- 1 / length(eligible)
    return(p)
  }
  
  # Cap 1 (M3 closed) → M3 strictly 0
  if (isTRUE(caps$m3_closed)) {
    p["M3"] <- 0
  }
  
  # Cap 2 (M1 Low-SES cap reached) affects ONLY NonSTEM & Low:
  # drop the 70% boost and revert to equal split over M1, M2, M4
  # BUT: for PH (sub-category) we must not give any probability to M1.
  if (isTRUE(caps$m1_low_closed) && fos %in% c("NonSTEM","PH") && ses == "Low") {
    p[] <- 0
    if (fos == "PH") {
      # PH: distribute equally over M2 and M4 only (M1 forbidden)
      p[c("M2","M4")] <- 1/2
    } else {
      # NonSTEM: equal split over M1, M2, M4 (original behavior)
      p[c("M1","M2","M4")] <- 1/3
    }
  }
  
  # final renormalization (keeps any zeroed modules at 0)
  s <- sum(p)
  if (s > 0) p <- p / s
  p
}


# ---- Draw a module given probabilities ------------------------------------
draw_module <- function(p) {
  sample(names(p), size = 1, prob = p)
}

# ---- Scenario builder: create N participants by independent marginals -----
# Accepts either:
#  (a) N and marginals list(list(fos=..., ses=..., gender=...)); or
#  (b) a data frame with columns FoS, SES, Gender (then N = nrow(df))
make_participants <- function(N = NULL,
                              marginals = list(
                                fos    = c(CS=.15, MSPEE=.35, NonSTEM=.50, PH = 0),
                                ses    = c(Low=.40, High=.60),
                                gender = c(man=.45, woman=.45, other=.07, pns=.03),
                                leadership = c(Low = .50, High = .50)
                              ),
                              df = NULL,
                              seed = 1) {
  set.seed(seed)
  if (!is.null(df)) {
    df <- df %>%
      transmute(FoS = factor(FoS, levels=fos_levels),
                SES = factor(SES, levels=ses_levels),
                Gender = factor(Gender, levels=gender_levels),
                Leadership = factor(Leadership, levels = leadership_levels))
    return(df)
  }
  stopifnot(!is.null(N))
  # align marginals robustly if you implemented align_marginal earlier; otherwise simple sampling:
  fos <- sample(fos_levels,   N, replace=TRUE, prob = marginals$fos[fos_levels])
  ses <- sample(ses_levels,   N, replace=TRUE, prob = marginals$ses[ses_levels])
  gen <- sample(gender_levels,N, replace=TRUE, prob = marginals$gender[gender_levels])
  lead <- sample(leadership_levels, N, replace=TRUE, prob = marginals$leadership[leadership_levels])
  tibble(FoS=factor(fos, levels=fos_levels),
         SES=factor(ses, levels=ses_levels),
         Gender=factor(gen, levels=gender_levels),
         Leadership=factor(lead, levels=leadership_levels))
}

# ---- Main simulator --------------------------------------------------------
simulate_stratification <- function(participants,
                                    m3_cap = 1500,
                                    m1_low_cap = 1000,
                                    seed = 42,
                                    log_every = 0) {
  set.seed(seed)
  
  # Running counters
  tot_mod <- c(M1=0L, M2=0L, M3=0L, M4=0L)
  m1_low_counter <- 0L
  
  # Storage
  out <- participants %>%
    mutate(id = row_number(),
           M1=NA_real_, M2=NA_real_, M3=NA_real_, M4=NA_real_,
           chosen = NA_character_,
           m3_closed = FALSE,
           m1_low_closed = FALSE,
           forced_M2 = FALSE)   # new column to mark forced allocations
  
  # --- PRECOMPUTE TARGET for hard allocation: 60% of (woman & High leadership)
  is_woman_high <- as.character(participants$Gender) == "woman" &
    as.character(participants$Leadership) == "High"
  subgroup_idx <- which(is_woman_high)
  n_subgroup <- length(subgroup_idx)
  n_target_force_M2 <- if (n_subgroup > 0) round(0.6 * n_subgroup) else 0L
  forced_M2_assigned <- 0L
  
  for (i in seq_len(nrow(participants))) {
    fos <- as.character(participants$FoS[i])
    ses <- as.character(participants$SES[i])
    
    # Determine which caps are currently active (pre-draw state)
    caps <- list(
      m3_closed = (tot_mod["M3"] >= m3_cap),
      m1_low_closed = (m1_low_counter >= m1_low_cap)
    )
    
    # Base probs (Rules 1–7)
    p <- base_probs(fos, ses)
    
    # Apply caps & renormalize (Rules 9–11)
    p_adj <- apply_caps(p, fos, ses, caps)
    
    # HARD ALLOCATION rule (not probabilistic):
    # If this participant belongs to the subgroup and we haven't yet
    # assigned the target number of forced M2s, assign M2 directly.
    if (is_woman_high[i] && forced_M2_assigned < n_target_force_M2) {
      m <- "M2"
      forced_M2_assigned <- forced_M2_assigned + 1L
      forced_flag <- TRUE
    } else {
      # normal draw
      # safety: if p_adj sums to zero (should not happen), fall back to equal prob
      if (sum(p_adj, na.rm = TRUE) <= 0) {
        nonzero_names <- names(p_adj)
        p_adj[] <- 1 / length(p_adj)
      }
      m <- draw_module(p_adj)
      forced_flag <- FALSE
    }
    
    # Update counters
    tot_mod[m] <- tot_mod[m] + 1L
    if (m == "M1" && ses == "Low") m1_low_counter <- m1_low_counter + 1L
    
    # Write row
    out$M1[i] <- p_adj["M1"]; out$M2[i] <- p_adj["M2"]; out$M3[i] <- p_adj["M3"]; out$M4[i] <- p_adj["M4"]
    out$chosen[i] <- m
    out$m3_closed[i] <- caps$m3_closed
    out$m1_low_closed[i] <- caps$m1_low_closed
    out$forced_M2[i] <- forced_flag
    
    if (log_every > 0 && i %% log_every == 0) {
      message(sprintf("i=%d | M1=%d (LowSES=%d) M2=%d M3=%d M4=%d | forced_M2_assigned=%d",
                      i, tot_mod["M1"], m1_low_counter, tot_mod["M2"], tot_mod["M3"], tot_mod["M4"],
                      forced_M2_assigned))
    }
  }
  
  # Summaries (unchanged)
  sum_mod <- out %>%
    count(chosen, name = "n") %>%
    complete(chosen = module_levels, fill = list(n=0L)) %>%
    arrange(match(chosen, module_levels)) %>%
    mutate(share = n / sum(n))
  
  sum_stratum <- out %>%
    count(FoS, SES, chosen, name = "n") %>%
    group_by(FoS, SES) %>%
    mutate(share = n / sum(n)) %>%
    ungroup()
  
  sum_gender <- out %>%
    count(chosen, Gender, name = "n") %>%
    group_by(chosen) %>%
    mutate(share = n / sum(n)) %>%
    ungroup()
  
  list(
    assignments = out,
    modules_summary = sum_mod,
    stratum_by_module = sum_stratum,
    gender_by_module = sum_gender,
    final_counters = list(
      modules = tot_mod,
      m1_lowSES = m1_low_counter,
      forced_M2_target = n_target_force_M2,
      forced_M2_assigned = forced_M2_assigned
    )
  )
}


# ---- how to use it: ----

# 1. change the marginals in make_participants() to reflect plausible incoming mixes (FoS, SES, Gender). set N to any test size (e.g., 2k, 5k, 10k).

# 2. run simulate_stratification(...). you’ll get:
# - assignments: row-level log (FoS/SES/Gender, probs used, caps active, chosen module).
# - modules_summary: totals & shares per module.
# - stratum_by_module: composition per FoS×SES.
# - gender_by_module: gender distribution within modules.
# - final_counters: final module counts and the M1-Low-SES counter.

# 3. tweak m3_cap or m1_low_cap to test policy changes.

# ---- Example usage ---------------------------------------------------------
# Scenario A: unknown final N, test with N=5,000 and plausible marginals
# (adjust these to stress-test)
if (sys.nframe() == 0) {
  N <- 10000
  participants <- make_participants(
    N = N,
    marginals = list(
      fos        = c(CS = .18, MSPEE = .32, NonSTEM = .49, PH = .01),
      ses        = c(Low = .45, High = .55),
      gender     = c(man = .46, woman = .46, other = .05, pns = .03),
      leadership = c(High = .50, Low = .50)   # <-- NEW: include Leadership marginals
    ),
    seed = 123
  )
  
  sim <- simulate_stratification(participants,
                                 m3_cap = 1500,
                                 m1_low_cap = 1000,
                                 seed = 777,
                                 log_every = 1000)
  
  print(sim$modules_summary)
  print(sim$final_counters)
  
  # Inspect when caps first became active
  first_m3_closed <- sim$assignments %>% filter(m3_closed) %>% summarise(first_id=min(id)) %>% pull(first_id)
  first_m1low_closed <- sim$assignments %>% filter(m1_low_closed) %>% summarise(first_id=min(id)) %>% pull(first_id)
  message("First M3-closed at id: ", ifelse(is.finite(first_m3_closed), first_m3_closed, "never"))
  message("First M1-LowSES-closed at id: ", ifelse(is.finite(first_m1low_closed), first_m1low_closed, "never"))
  
  # Quick gender & leadership balance check per module
  print(sim$gender_by_module %>% arrange(chosen, desc(share)))
  # check forced allocation among the subgroup
  print(glue::glue("Forced M2 target: {sim$final_counters$forced_M2_target}, assigned: {sim$final_counters$forced_M2_assigned}"))
  print(sim$assignments %>% filter(Gender=="woman", Leadership=="High") %>% count(chosen))
}



