#######################################
# Test Stratification Career Survey - App withour rule editor
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
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

# ---- source your stratification functions here ------------------------------

source("Career_Survey_StratSimulation.R")  # the script we wrote earlier

ui <- fluidPage(
  titlePanel("Survey Stratification Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("N", "Number of participants", 5000, min = 100, step = 100),
      sliderInput("pCS", "Share Computer Science (%)", 0, 100, 18),
      sliderInput("pMSPEE", "Share Math/Stats/Phys/EE (%)", 0, 100, 32),
      sliderInput("pNonSTEM", "Share Non-STEM (%)", 0, 100, 50),
      sliderInput("pPH", "Share PH (subcategory of Non-STEM) (%)", 0, 100, 0), # NEW
      sliderInput("pLow", "Low SES (%)", 0, 100, 45),
      sliderInput("pLeadHigh", "Share Leadership ambition — High (%)", 0, 100, 50),
      numericInput("m3_cap", "Module 3 cap", 1500, min = 100, step = 100),
      numericInput("m1_cap", "M1 low SES cap", 1000, min = 100, step = 100),
      actionButton("run", "Run simulation")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Module characteristics", tableOutput("table_modules")),
        tabPanel("FoS × SES table", tableOutput("table_stratum")),
        tabPanel("Gender balance", plotOutput("plot_gender")),
        tabPanel("Summary", verbatimTextOutput("summary_text")),
        tabPanel("Notes", verbatimTextOutput("notes"))
      )
    )
  )
)

version_notes <- "
Base rules - Version 1.1: 
rule 1: Module 3 gets 75% of computer science
rule 2: module 3 gets 40% of the total pool of math, statistics, physics, EE 
rule 3: module 3 gets 0 % of non-STEM-IT (hard exclusion)
rule 4: module 1 gets 70 & of interviewees with Low SES & Non-STEM-IT
rule 5: STEM-IT which are not in module 3 are equally split across module 1, 2 and 4 
rule 6: Non-STEM-IT & low SES which are not in module 1 are equally split between modules 2 and 4
rule 7: non-STEM-IT & high SES are equally split between modules 1, 2 and 4 
rule 8: gender should be rougly balanced within the modules but do not affect probabilities
rule 9: when module 3 reaches a total of 1500 it becomes ineligible. the probabilieties are re-normalized over the remaining eligible modules. 
rule 10: when module 1 reaches 1000 participants with low SES, the 70% boost is stoped and the probabilities are re-normalized over eligible modules.
rule 11: when rule 9 and 10 are in effect, we use equal randomization across eligible modules for each stratum.
general: all rules concerning module 3 (rule 1, rule 2 and rule 3) have higher priority than the other rules.

Version 1.2 - last changed 10.11.2025
Changes:
- Added FoS category 'PH'
- PH is treated as NonSTEM but has a hard exclusion from M1
- Updated participant generator and cap logic accordingly

Version 1.3 - last changed 11.11.2025
Changes:
- new level: leadership_ambitions: High/Low
- added leadership_ambitions as slider to participant creator
- new rule: exactly 60% of participants with Gender == woman & Leadership == High to M2
"

server <- function(input, output, session) {
  sim_data <- eventReactive(input$run, {
    # normalize FoS shares to sum to 1 (in case user fiddles sliders)
    pCS <- input$pCS/100
    pMSPEE <- input$pMSPEE/100
    pNonSTEM <- input$pNonSTEM/100
    pPH <- input$pPH/100
    total_fos <- pCS + pMSPEE + pNonSTEM + pPH
    if (total_fos <= 0) {
      # fallback to defaults if user sets everything to zero
      pCS <- 0.18; pMSPEE <- 0.32; pNonSTEM <- 0.50; pPH <- 0
    } else {
      pCS <- pCS / total_fos
      pMSPEE <- pMSPEE / total_fos
      pNonSTEM <- pNonSTEM / total_fos
      pPH <- pPH / total_fos
    }
    
    pLow <- input$pLow/100
    if (pLow <= 0) pLow <- 0.45
    
    # compute leadership marginal
    pLeadHigh <- input$pLeadHigh / 100
    if (is.null(pLeadHigh) || pLeadHigh < 0) pLeadHigh <- 0.5
    
    participants <- make_participants(
      N = input$N,
      marginals = list(
        fos = c(CS = pCS, MSPEE = pMSPEE, NonSTEM = pNonSTEM, PH = pPH),
        ses = c(Low = pLow, High = 1 - pLow),
        gender = c(man = .46, woman = .46, other = .05, pns = .03),
        leadership = c(High = pLeadHigh, Low = 1 - pLeadHigh)
      )
    )
    
    simulate_stratification(participants,
                            m3_cap = input$m3_cap,
                            m1_low_cap = input$m1_cap)
  })
  
  output$table_modules <- renderTable({
    sim <- sim_data()
    
    # Build a table: for each module, counts and shares of FoS
    tab_fos <- sim$assignments %>%
      count(chosen, FoS, name = "n") %>%
      group_by(chosen) %>%
      mutate(module_total = sum(n),
             share = n / module_total) %>%
      ungroup()
    
    # pivot wider to have columns per FoS with counts and shares
    counts_fos_wide <- tab_fos %>%
      select(chosen, FoS, n) %>%
      pivot_wider(names_from = FoS, values_from = n, values_fill = 0) %>%
      rename(Module = chosen)
    
    shares_fos_wide <- tab_fos %>%
      select(chosen, FoS, share) %>%
      pivot_wider(names_from = FoS, values_from = share, values_fill = 0) %>%
      rename(Module = chosen) %>%
      mutate(across(-Module, ~ round(.x, 3)))
    
    fos_out <- counts_fos_wide %>%
      left_join(shares_fos_wide, by = "Module", suffix = c("_count", "_share"))
    
    # --- SES composition (counts + shares) ----------------------------------
    tab_ses <- sim$assignments %>%
      count(chosen, SES, name = "n") %>%
      group_by(chosen) %>%
      mutate(module_total = sum(n),
             share = n / module_total) %>%
      ungroup()
    
    counts_ses_wide <- tab_ses %>%
      select(chosen, SES, n) %>%
      pivot_wider(names_from = SES, values_from = n, values_fill = 0) %>%
      rename(Module = chosen)
    
    shares_ses_wide <- tab_ses %>%
      select(chosen, SES, share) %>%
      pivot_wider(names_from = SES, values_from = share, values_fill = 0) %>%
      rename(Module = chosen) %>%
      mutate(across(-Module, ~ round(.x, 3)))
    
    # rename Low/High columns to LowSES/HighSES (both count and share)
    counts_ses_wide <- counts_ses_wide %>%
      rename_with(.cols = everything(), .fn = function(x) x) # noop to satisfy pipe
    # perform renames if columns exist
    if ("Low" %in% names(counts_ses_wide)) names(counts_ses_wide)[names(counts_ses_wide) == "Low"] <- "LowSES_count"
    if ("High" %in% names(counts_ses_wide)) names(counts_ses_wide)[names(counts_ses_wide) == "High"] <- "HighSES_count"
    if ("Low" %in% names(shares_ses_wide)) names(shares_ses_wide)[names(shares_ses_wide) == "Low"] <- "LowSES_share"
    if ("High" %in% names(shares_ses_wide)) names(shares_ses_wide)[names(shares_ses_wide) == "High"] <- "HighSES_share"
    
    ses_out <- counts_ses_wide %>%
      left_join(shares_ses_wide, by = "Module", suffix = c("_count", "_share"))
    
    # --- Leadership composition: focus on High leadership only -----------------
    tab_lead <- sim$assignments %>%
      count(chosen, Leadership, name = "n") %>%
      group_by(chosen) %>%
      mutate(module_total = sum(n),
             share = n / module_total) %>%
      ungroup()
    
    counts_lead_wide <- tab_lead %>%
      select(chosen, Leadership, n) %>%
      pivot_wider(names_from = Leadership, values_from = n, values_fill = 0) %>%
      rename(Module = chosen)
    
    shares_lead_wide <- tab_lead %>%
      select(chosen, Leadership, share) %>%
      pivot_wider(names_from = Leadership, values_from = share, values_fill = 0) %>%
      rename(Module = chosen) %>%
      mutate(across(-Module, ~ round(.x, 0)))
    
    # create HighLead_count / HighLead_share columns (safe if Leadership levels missing)
    if ("High" %in% names(counts_lead_wide)) {
      counts_lead_wide <- counts_lead_wide %>% rename(HighLead_count = High)
    } else {
      counts_lead_wide$HighLead_count <- 0
    }
    if ("High" %in% names(shares_lead_wide)) {
      shares_lead_wide <- shares_lead_wide %>% rename(HighLead_share = High)
    } else {
      shares_lead_wide$HighLead_share <- 0
    }
    
    lead_out <- counts_lead_wide %>%
      left_join(shares_lead_wide, by = "Module", suffix = c("_count", "_share")) %>%
      select(Module, HighLead_count, HighLead_share)
    
    # combine FoS, SES and Leadership outputs side-by-side
    out <- fos_out %>%
      full_join(ses_out, by = "Module", suffix = c("", "")) %>%
      left_join(lead_out, by = "Module")
    
    # add module total column (ensure present)
    module_totals <- sim$assignments %>%
      count(chosen, name = "Total") %>%
      rename(Module = chosen)
    out <- left_join(out, module_totals, by = "Module")
    
    # tidy column order: Module, Total, FoS_count/ FoS_share pairs, SES_count/SES_share pairs, Leadership
    fos_order <- c("CS", "MSPEE", "NonSTEM", "PH")
    ses_order <- c("LowSES", "HighSES")
    ordered_cols <- c("Module", "Total",
                      as.vector(rbind(paste0(fos_order, "_count"), paste0(fos_order, "_share"))),
                      as.vector(rbind(paste0(ses_order, "_count"), paste0(ses_order, "_share"))),
                      "HighLead_count", "HighLead_share")
    # keep only columns that exist (safety)
    ordered_cols <- ordered_cols[ordered_cols %in% names(out)]
    out <- out %>% select(any_of(ordered_cols))
    
    # ensure modules order M1..M4
    out <- out %>% arrange(factor(Module, levels = c("M1", "M2", "M3", "M4")))
    
    out
  }, striped = TRUE, hover = TRUE, spacing = "m")
  
  
  output$table_stratum <- renderTable({
    sim_data()$stratum_by_module %>%
      select(FoS, SES, chosen, n, share) %>%
      arrange(FoS, SES, chosen)
  })
  
  output$plot_gender <- renderPlot({
    sim <- sim_data()
    ggplot(sim$gender_by_module, aes(Gender, share, fill = Gender)) +
      geom_col() +
      facet_wrap(~chosen) +
      theme_minimal() +
      labs(y="Share", title="Gender balance by module")
  })
  
  output$summary_text <- renderPrint({
    sim <- sim_data()
    sim$final_counters
  })
  
  output$notes <- renderText({
    version_notes
  })
}
shinyApp(ui = ui, server = server)






