# R code to recreate the empirical results, tables, and figures in paper
# "Voting Experiences, Perceptions of Fraud, and Voter Confidence" (joint with R. Michael Alvarez and Jian Cao).
# For questions regarding this code, contact Yimeng Li.
# Date: Jan 23, 2021.

# Set working directory:
setwd("C:/OneDrive - California Institute of Technology/Empirical - Voter Confidence")

# Load libraries:
library(Amelia) # version 1.7.6
library(dplyr) # version 1.0.3
library(ggplot2) # version 3.3.3
library(hIRT) # version 0.3.0
library(margins) # version 0.3.26
library(matrixStats) # version 0.57.0
library(prediction) # version 0.3.14
library(stringr) # version 1.4.0
library(survey) # version 4.0

# Load dataset:
load("./Data/OCvoters_design_recoded.RData")
OCsurvey_variables <- OCsurvey_design[["variables"]]

# List of variables:
var_list = list(
  confidence     = c("confidence_own_o", "confidence_own_b", "confidence_OC_o", "confidence_OC_b", "confidence_CA_o", "confidence_CA_b", "confidence_US_o", "confidence_US_b"),
  exp_in_person  = c("poll_finding_difficult", "reg_problem", "poll_closed", "wait_time_long", "equip_problem", "pollworker_rating_poor"),
  misc_in_person = c("pollworker_color", "pollworker_old"),
  exp_by_mail    = c("absentee_get_problem", "absentee_mark_problem", "instruction_hard"),
  misc_by_mail   = c("return_early", "return_usps"),
  fraud_hacking  = c("fraud_1_common", "fraud_2_common", "fraud_3_common", "fraud_4_common", "fraud_5_common", "fraud_6_common", "hacking_nationwide_issue", "hacking_locally_issue"),
  use_twitter    = c("use_twitter"),
  social_media   = c("twitter_discuss_neg", "discuss_concerns", "receive_concerns"),
  misc_noms      = c("age", "educ", "follow_news", "race_factor", "income", "party_registration", "congressional_district"),
  misc_ords      = c("female", "first_time", "marital", "homeowner", "permanent_absentee_status"),
  misc_IRT       = c("age", "female", "educ", "race_factor", "marital", "homeowner", "income"),
  misc           = c("age", "female", "educ", "race_factor", "marital", "homeowner", "income", "follow_news", "first_time", "permanent_absentee_status", "party_registration", "congressional_district")
)

#****************************************************************************************
# Voter Confidence
#****************************************************************************************

confidence_all_svymean <-
  svymean(~confidence_own+confidence_OC+confidence_CA+confidence_US, design = OCsurvey_design, na.rm = TRUE)

confidence_all <-
  as.data.frame(cbind(Estimate = coef(confidence_all_svymean), confint(confidence_all_svymean))) %>%
  mutate(scale = case_when(str_detect(rownames(.), "confidence_own") ~ "Own vote",
                           str_detect(rownames(.), "confidence_OC") ~ "Votes in OC",
                           str_detect(rownames(.), "confidence_CA") ~ "Votes in CA",
                           str_detect(rownames(.), "confidence_US") ~ "Votes in US",
                           TRUE ~ NA_character_),
         scale_factor = factor(scale, levels = c("Own vote", "Votes in OC", "Votes in CA", "Votes in US")),
         confidence = case_when(str_detect(rownames(.), "Not at all confident") ~ "Not at all confident",
                                str_detect(rownames(.), "Not too confident") ~ "Not too confident",
                                str_detect(rownames(.), "Somewhat confident") ~ "Somewhat confident",
                                str_detect(rownames(.), "Very confident") ~ "Very confident",
                                TRUE ~ NA_character_),
         confidence_factor = factor(confidence, levels = c("Very confident", "Somewhat confident",
                                                           "Not too confident", "Not at all confident")))

confidence_outcome <- ggplot(confidence_all, aes(x = confidence_factor, y = Estimate, fill = scale_factor)) +
  geom_bar(stat = "identity", position = position_dodge(width=0.9)) +
  geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`), width = 0.2, position = position_dodge(width=0.9)) +
  labs(x = "Confidence that ... was/were counted as intended", y = "Proportion of Respondents", fill = "") +
  scale_fill_grey() +
  scale_y_continuous(limits = c(0, 0.6), breaks = c(0, 0.2, 0.4, 0.6)) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(text = element_text(size=10))

ggsave("./Result/OCconfidence_outcome.pdf", confidence_outcome, width = 8, height = 4, units = "in", dpi = 600)
# ggsave("./Result/OCconfidence_outcome.eps", confidence_outcome, width = 8, height = 4, units = "in", dpi = 900)

#****************************************************************
# Multiple Imputation via Amelia
#****************************************************************

subset_rules = list(
  in_person_twitter = list(
    rows = which(OCsurvey_variables$mode == "In person" & OCsurvey_variables$use_twitter == 1),
    columns = unlist(var_list[c("confidence", "exp_in_person", "misc_in_person", "fraud_hacking", "social_media", "misc_noms", "misc_ords")])
  ),
  in_person_no_twitter = list(
    rows = which(OCsurvey_variables$mode == "In person" & OCsurvey_variables$use_twitter == 0),
    columns = unlist(var_list[c("confidence", "exp_in_person", "misc_in_person", "fraud_hacking", "misc_noms", "misc_ords")])
  ),
  by_mail_twitter = list(
    rows = which(OCsurvey_variables$mode == "By mail" & OCsurvey_variables$use_twitter == 1),
    columns = unlist(var_list[c("confidence", "exp_by_mail", "misc_by_mail", "fraud_hacking", "social_media", "misc_noms", "misc_ords")])
  ),
  by_mail_no_twitter = list(
    rows = which(OCsurvey_variables$mode == "By mail" & OCsurvey_variables$use_twitter == 0),
    columns = unlist(var_list[c("confidence", "exp_by_mail", "misc_by_mail", "fraud_hacking", "misc_noms", "misc_ords")])
  )
)

OCsurvey_design_MI = list(imp1 = OCsurvey_design, imp2 = OCsurvey_design, imp3 = OCsurvey_design, imp4 = OCsurvey_design, imp5 = OCsurvey_design)

MI_amelia = function(data, M = 5, imputed_design_objects, subset, var_nominals, var_ordinals){
  a.out = amelia(x = data[subset[["rows"]], subset[["columns"]]], m = M, noms = var_nominals, ords = var_ordinals)
  for (m in 1:M) {
    imputed_design_objects[[m]][["variables"]][subset[["rows"]], subset[["columns"]]] = a.out[["imputations"]][[m]]
  }
  return(imputed_design_objects)
}

set.seed(2020)

OCsurvey_design_MI = MI_amelia(data = OCsurvey_variables, imputed_design_objects = OCsurvey_design_MI, subset = subset_rules[["in_person_twitter"]],    var_nominals = unlist(var_list[c("fraud_hacking", "misc_noms")]), var_ordinals = unlist(var_list[c("confidence", "exp_in_person", "misc_in_person", "social_media", "misc_ords")]))
OCsurvey_design_MI = MI_amelia(data = OCsurvey_variables, imputed_design_objects = OCsurvey_design_MI, subset = subset_rules[["in_person_no_twitter"]], var_nominals = unlist(var_list[c("fraud_hacking", "misc_noms")]), var_ordinals = unlist(var_list[c("confidence", "exp_in_person", "misc_in_person", "misc_ords")]))
OCsurvey_design_MI = MI_amelia(data = OCsurvey_variables, imputed_design_objects = OCsurvey_design_MI, subset = subset_rules[["by_mail_twitter"]],      var_nominals = unlist(var_list[c("fraud_hacking", "misc_noms")]), var_ordinals = unlist(var_list[c("confidence", "exp_by_mail", "misc_by_mail", "social_media", "misc_ords")]))
OCsurvey_design_MI = MI_amelia(data = OCsurvey_variables, imputed_design_objects = OCsurvey_design_MI, subset = subset_rules[["by_mail_no_twitter"]],   var_nominals = unlist(var_list[c("fraud_hacking", "misc_noms")]), var_ordinals = unlist(var_list[c("confidence", "exp_by_mail", "misc_by_mail", "misc_ords")]))

# save(OCsurvey_design_MI, file = "./Data/OCsurvey_design_MI.RData")

#**********************************************************************************************
# Hierarchical IRT for In-Person Experience, By-Mail Experience, and Social Media
#**********************************************************************************************

IRT_do = function(data, y_var, x_var, z_var){
  y = data[y_var]
  x = model.matrix(as.formula(paste0("~", paste0(x_var, collapse = " + "))), data)
  z = model.matrix(as.formula(paste0("~", paste0(z_var, collapse = " + "))), data)
  hIRT.out = hltm(y, x, z, control = list(max_iter = 50000))
  return(hIRT.out)
}

param_IRT = list(
  y_var_social_media = var_list[["social_media"]],
  y_var_exp_in_person = var_list[["exp_in_person"]],
  y_var_exp_by_mail = var_list[["exp_by_mail"]],
  x_var = unlist(var_list[["misc_IRT"]]),
  z_var = unlist(var_list[["misc_IRT"]])
)

apply_IRT_do = function(design_object, param){
  
  # Construct social media IRT:
  data_social_media = design_object[["variables"]] %>% filter(use_twitter == 1)
  hIRT.out_social_media = IRT_do(data_social_media, param[["y_var_social_media"]], param[["x_var"]], param[["z_var"]])
  data_social_media = bind_cols(data_social_media, latent_scores(hIRT.out_social_media) %>% select(social_media_IRT = post_mean))
  design_object[["variables"]] = design_object[["variables"]] %>%
    left_join(data_social_media %>% select(id, social_media_IRT), by = "id") %>%
    mutate(social_media_IRT = if_else(is.na(social_media_IRT), 0, social_media_IRT))
  
  # Construct in-person voting experience IRT:
  data_exp_in_person = design_object[["variables"]] %>% filter(mode == "In person")
  hIRT.out_exp_in_person = IRT_do(data_exp_in_person, param[["y_var_exp_in_person"]], param[["x_var"]], param[["z_var"]])
  data_exp_in_person = bind_cols(data_exp_in_person, latent_scores(hIRT.out_exp_in_person) %>% select(exp_in_person_IRT = post_mean))
  design_object[["variables"]] = design_object[["variables"]] %>%
    left_join(data_exp_in_person %>% select(id, exp_in_person_IRT), by = "id")
  
  # Construct by-mail voting experience IRT:
  data_exp_by_mail = design_object[["variables"]] %>% filter(mode == "By mail")
  hIRT.out_exp_by_mail = IRT_do(data_exp_by_mail, param[["y_var_exp_by_mail"]], param[["x_var"]], param[["z_var"]])
  data_exp_by_mail = bind_cols(data_exp_by_mail, latent_scores(hIRT.out_exp_by_mail) %>% select(exp_by_mail_IRT = post_mean))
  design_object[["variables"]] = design_object[["variables"]] %>%
    left_join(data_exp_by_mail %>% select(id, exp_by_mail_IRT), by = "id")
  
  return(list(hIRT.out_social_media = hIRT.out_social_media,
              hIRT.out_exp_in_person = hIRT.out_exp_in_person,
              hIRT.out_exp_by_mail = hIRT.out_exp_by_mail,
              design_object = design_object))
}

IRT_outputs = lapply(OCsurvey_design_MI, apply_IRT_do, param = param_IRT)

hIRT.out_social_media = lapply(IRT_outputs, `[[`, "hIRT.out_social_media")
hIRT.out_exp_in_person = lapply(IRT_outputs, `[[`, "hIRT.out_exp_in_person")
hIRT.out_exp_by_mail = lapply(IRT_outputs, `[[`, "hIRT.out_exp_by_mail")
OCsurvey_MI = lapply(IRT_outputs, `[[`, "design_object")

# save(IRT_outputs, file = "./Data/IRT_outputs.RData")
# save(OCsurvey_MI, file = "./Data/OCsurvey_MI.RData")

var_list[["social_media_IRT"]] = c("social_media_IRT")
var_list[["exp_in_person_IRT"]] = c("exp_in_person_IRT")
var_list[["exp_by_mail_IRT"]] = c("exp_by_mail_IRT")

#****************************************************************
# (Weighted) Logistic Regressions
#****************************************************************

replace_NA = function(design_object){
  design_object[["variables"]] = design_object[["variables"]] %>%
    mutate(exp_in_person_IRT = if_else(is.na(exp_in_person_IRT), 0, exp_in_person_IRT),
           exp_by_mail_IRT = if_else(is.na(exp_by_mail_IRT), 0, exp_by_mail_IRT))
  return(design_object)
}

OCsurvey_MI_NAreplaced = lapply(OCsurvey_MI, replace_NA)

models = list(
  both_modes_own = as.formula(paste0("confidence_own_b", "~", paste0(unlist(var_list[c("misc", "use_twitter", "social_media_IRT", "fraud_hacking")]), collapse = " + "), " + ", "in_person", " + " , paste0(unlist(var_list[c("misc", "use_twitter", "social_media_IRT", "fraud_hacking")]), "*in_person", collapse = " + "))),
  both_modes_OC  = as.formula(paste0("confidence_OC_b", "~", paste0(unlist(var_list[c("misc", "use_twitter", "social_media_IRT", "fraud_hacking")]), collapse = " + "), " + ", "in_person", " + " , paste0(unlist(var_list[c("misc", "use_twitter", "social_media_IRT", "fraud_hacking")]), "*in_person", collapse = " + "))),
  both_modes_CA  = as.formula(paste0("confidence_CA_b", "~", paste0(unlist(var_list[c("misc", "use_twitter", "social_media_IRT", "fraud_hacking")]), collapse = " + "), " + ", "in_person", " + " , paste0(unlist(var_list[c("misc", "use_twitter", "social_media_IRT", "fraud_hacking")]), "*in_person", collapse = " + "))),
  both_modes_US  = as.formula(paste0("confidence_US_b", "~", paste0(unlist(var_list[c("misc", "use_twitter", "social_media_IRT", "fraud_hacking")]), collapse = " + "), " + ", "in_person", " + " , paste0(unlist(var_list[c("misc", "use_twitter", "social_media_IRT", "fraud_hacking")]), "*in_person", collapse = " + "))),
  in_person_own = as.formula(paste0("confidence_own_b", "~", paste0(unlist(var_list[c("misc", "use_twitter", "social_media_IRT", "fraud_hacking", "exp_in_person_IRT")]), collapse = " + "))),
  in_person_OC  = as.formula(paste0("confidence_OC_b", "~", paste0(unlist(var_list[c("misc", "use_twitter", "social_media_IRT", "fraud_hacking", "exp_in_person_IRT")]), collapse = " + "))),
  in_person_CA  = as.formula(paste0("confidence_CA_b", "~", paste0(unlist(var_list[c("misc", "use_twitter", "social_media_IRT", "fraud_hacking", "exp_in_person_IRT")]), collapse = " + "))),
  in_person_US  = as.formula(paste0("confidence_US_b", "~", paste0(unlist(var_list[c("misc", "use_twitter", "social_media_IRT", "fraud_hacking", "exp_in_person_IRT")]), collapse = " + "))),
  by_mail_own = as.formula(paste0("confidence_own_b", "~", paste0(unlist(var_list[c("misc", "use_twitter", "social_media_IRT", "fraud_hacking", "exp_by_mail_IRT")]), collapse = " + "))),
  by_mail_OC  = as.formula(paste0("confidence_OC_b", "~", paste0(unlist(var_list[c("misc", "use_twitter", "social_media_IRT", "fraud_hacking", "exp_by_mail_IRT")]), collapse = " + "))),
  by_mail_CA  = as.formula(paste0("confidence_CA_b", "~", paste0(unlist(var_list[c("misc", "use_twitter", "social_media_IRT", "fraud_hacking", "exp_by_mail_IRT")]), collapse = " + "))),
  by_mail_US  = as.formula(paste0("confidence_US_b", "~", paste0(unlist(var_list[c("misc", "use_twitter", "social_media_IRT", "fraud_hacking", "exp_by_mail_IRT")]), collapse = " + ")))
)

subsets_mode = list(
  both_modes_own = c("In person", "By mail"),
  both_modes_OC = c("In person", "By mail"),
  both_modes_CA = c("In person", "By mail"),
  both_modes_US = c("In person", "By mail"),
  in_person_own = c("In person"),
  in_person_OC = c("In person"),
  in_person_CA = c("In person"),
  in_person_US = c("In person"),
  by_mail_own = c("By mail"),
  by_mail_OC = c("By mail"),
  by_mail_CA = c("By mail"),
  by_mail_US = c("By mail")
)

#********** Estimate Logistic Regressions **********

apply_svyglm_nested = function(design_object, model, subset_mode){
  result = svyglm(formula = model,
                  design = subset(design_object, mode %in% subset_mode),
                  family = binomial(link = "logit"))
  return(result)
}

apply_svyglm = function(design_objects, model, subset_mode){
  results = lapply(design_objects, apply_svyglm_nested, model = model, subset_mode = subset_mode)
  return(results)
}

estimation_results = mapply(apply_svyglm, models, subsets_mode, MoreArgs = list(design_objects = OCsurvey_MI_NAreplaced), SIMPLIFY = FALSE)
estimation_results_alt = mapply(apply_svyglm, models, subsets_mode, MoreArgs = list(design_objects = OCsurvey_MI), SIMPLIFY = FALSE)

# save(estimation_results, file = "./Data/estimation_results.RData")
# save(estimation_results_alt, file = "./Data/estimation_results_alt.RData")

#********** Compute Average Marginal (or Partial) Effects **********
# ! Note: This section takes a few hours to run.

apply_margins_nested = function(svyglm_object, design_object, subset_mode){
  margins_summary = summary(margins(model = svyglm_object, design = subset(design_object, mode %in% subset_mode)))
}

apply_margins = function(svyglm_objects, subset_mode, design_objects){
  summaries = mapply(apply_margins_nested, svyglm_objects, design_objects, MoreArgs = list(subset_mode = subset_mode), SIMPLIFY = FALSE)
  return(summaries)
}

margins_summaries = mapply(apply_margins, estimation_results, subsets_mode, MoreArgs = list(design_objects = OCsurvey_MI_NAreplaced), SIMPLIFY = FALSE)

# save(margins_summaries, file = "./Data/margins_summaries.RData")

#********** Pool Estimates and Standard Errors across Imputations **********

pool_est_se = function(summaries){
  summaries_comb = bind_cols(summaries)
  pooled_summary = data.frame(
    item = summaries_comb$`factor...1`,
    Coefficient = rowMeans(summaries_comb %>% select(starts_with("AME"))),
    within = rowMeans(summaries_comb %>% transmute_at(vars(starts_with("SE")), function(x) x^2)),
    between = rowVars(as.matrix(summaries_comb %>% select(starts_with("AME"))))
  ) %>%
    mutate(`Std. Error` = sqrt(within + between + between/length(summaries)),
           `t value` = Coefficient/`Std. Error`,
           `Pr(>|t|)` = pnorm(abs(`t value`), lower.tail = FALSE)*2,
           lb = Coefficient - qnorm(0.975)*`Std. Error`,
           ub = Coefficient + qnorm(0.975)*`Std. Error`) %>%
    select(item, Coefficient, `Std. Error`, `t value`, `Pr(>|t|)`, lb, ub)
}

pooled_margins_summaries = lapply(margins_summaries, pool_est_se)

#****************************************************************
# Plot Average Marginal (or Partial) Effects
#****************************************************************

#********** Prepare Outputs for Plotting **********

format_summaries = function(summary){
  formatted_summary = summary %>%
    mutate(
      item_names = case_when(
        item == "age30-44" ~ "demo",
        item == "age45-64" ~ "demo",
        item == "age65 or above" ~ "demo",
        item == "female" ~ "demo",
        item == "educSome college, but no degree (yet)" ~ "demo",
        item == "educ2-year college degree" ~ "demo",
        item == "educ4-year college degree" ~ "demo",
        item == "educPostgraduate degree (MA, MBA, MD, JD, PhD, etc.)" ~ "demo",
        item == "race_factorHispanic" ~ "Hispanic or Latino",
        item == "race_factorAsian" ~ "Asian or Asian American",
        item == "race_factorOther races" ~ "Other races",
        item == "marital" ~ "demo",
        item == "homeowner" ~ "demo",
        item == "income$75,000 to $124,999" ~ "demo",
        item == "income$125,000 to $174,999" ~ "demo",
        item == "income$175,000 or more" ~ "demo",
        item == "party_registration1" ~ "Registered Democratic voter",
        item == "party_registration2" ~ "Registered Republican voter",
        item == "first_time" ~ "demo",
        item == "permanent_absentee_status" ~ "demo",
        item == "in_person" ~ "Voted in person",
        item == "follow_newsMost of the time" ~ "Follow news most of the time",
        item == "follow_newsSome of the time" ~ "Follow news some of the time",
        item == "use_twitter" ~ "Twitter user",
        item == "social_media_IRT" ~ "Receive/discuss concerns on Twitter",
        item == "exp_in_person_IRT" ~ "Bad in-person voting experience",
        item == "exp_by_mail_IRT" ~ "Bad by-mail voting experience",
        item == "fraud_1_commonYes" ~ "Voting more than once",
        item == "fraud_2_commonYes" ~ "Ballot stealing or tampering",
        item == "fraud_3_commonYes" ~ "Voter impersonation: at the polls",
        item == "fraud_4_commonYes" ~ "Non-citizen voting",
        item == "fraud_5_commonYes" ~ "Voter impersonation: absentee ballots",
        item == "fraud_6_commonYes" ~ "Officials manipulating vote count",
        item == "fraud_1_commonNot sure" ~ "other",
        item == "fraud_2_commonNot sure" ~ "other",
        item == "fraud_3_commonNot sure" ~ "other",
        item == "fraud_4_commonNot sure" ~ "other",
        item == "fraud_5_commonNot sure" ~ "other",
        item == "fraud_6_commonNot sure" ~ "other",
        item == "hacking_locally_issueYes" ~ "Local computer hacking",
        item == "hacking_nationwide_issueYes" ~ "Nationwide computer hacking",
        item == "hacking_locally_issueNot sure" ~ "other",
        item == "hacking_nationwide_issueNot sure" ~ "other",
        item == "congressional_district39th Congressional District" ~ "district fixed effect",
        item == "congressional_district45th Congressional District" ~ "district fixed effect",
        item == "congressional_district46th Congressional District" ~ "district fixed effect",
        item == "congressional_district47th Congressional District" ~ "district fixed effect",
        item == "congressional_district48th Congressional District" ~ "district fixed effect",
        item == "congressional_district49th Congressional District" ~ "district fixed effect",
        TRUE ~ NA_character_),
      item_names = factor(item_names, levels = rev(
        c("demo", "district fixed effect", "other",
          "Hispanic or Latino", "Asian or Asian American", "Other races",
          "Registered Democratic voter", "Registered Republican voter",
          "Voted in person",
          "Follow news most of the time", "Follow news some of the time",
          "Twitter user", "Receive/discuss concerns on Twitter",
          "Bad in-person voting experience", "Bad by-mail voting experience",
          "Voting more than once", "Ballot stealing or tampering", "Voter impersonation: at the polls", "Non-citizen voting", "Voter impersonation: absentee ballots", "Officials manipulating vote count",
          "Local computer hacking", "Nationwide computer hacking")
      )),
      facetgrid = case_when(
        item_names %in% c("demo", "district fixed effect", "other") ~ "demo, fe, other",
        item_names %in% c("Hispanic or Latino", "Asian or Asian American", "Other races") ~ "Race/Ethnicity",
        item_names %in% c("Registered Democratic voter", "Registered Republican voter") ~ "Party ID",
        item_names %in% c("Voted in person") ~ "Mode",
        item_names %in% c("Follow news most of the time", "Follow news some of the time") ~ "Follow\n News",
        item_names %in% c("Twitter user", "Receive/discuss concerns on Twitter") ~ "Social\n Media",
        item_names %in% c("Bad in-person voting experience", "Bad by-mail voting experience") ~ "Exp",
        item_names %in% c("Voting more than once", "Ballot stealing or tampering", "Voter impersonation: at the polls",
                          "Non-citizen voting", "Voter impersonation: absentee ballots", "Officials manipulating vote count") ~ "Fraud \n Perception",
        item_names %in% c("Local computer hacking", "Nationwide computer hacking") ~ "Hacking\n Perception"
      ),
      facetgrid = factor(facetgrid, levels = c("Race/Ethnicity", "Party ID", "Mode", "Follow\n News", "Social\n Media", "Exp", "Fraud \n Perception", "Hacking\n Perception")),
      p_value = case_when(
        `Pr(>|t|)` < 0.05 ~ "p < 0.05",
        `Pr(>|t|)` > 0.05 ~ "p > 0.05",
        TRUE ~ NA_character_)
    )
  return(formatted_summary)
}

formatted_margins_summaries = lapply(pooled_margins_summaries, format_summaries)

#********** Plot Average Marginal Effects on Own Vote Confidence **********

plot_AME_own = function(formatted_margins_summary){
  AME_plot =
    ggplot(formatted_margins_summary %>% filter(!is.na(facetgrid)),
           aes(x = item_names, y = Coefficient, colour = p_value)) +
    geom_point() +
    geom_errorbar(aes(ymin = lb, ymax = ub), size = 1, width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_grid(facetgrid ~ ., scales = "free", space = "free") +
    labs(x = "", y = "Average Marginal Effect", colour = "") +
    scale_y_continuous(limits = c(-0.4, 0.2), breaks = c(-0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2)) +
    scale_color_manual(values = c("p < 0.05" = "#000000", "p > 0.05" = "#999999")) +
    theme_bw() +
    theme(legend.position = "none") +
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    theme(text = element_text(size = 14), axis.text.x = element_text(size = 12), strip.text = element_text(size = 7)) +
    coord_flip()
  return(AME_plot)
}

AME_both_modes_own = plot_AME_own(formatted_margins_summaries[["both_modes_own"]])
ggsave("./Result/AME_both_modes_own.pdf", AME_both_modes_own, width = 8, height = 6, units = "in", dpi = 600)

AME_in_person_own = plot_AME_own(formatted_margins_summaries[["in_person_own"]])
ggsave("./Result/AME_in_person_own.pdf", AME_in_person_own, width = 8, height = 6, units = "in", dpi = 600)

AME_by_mail_own = plot_AME_own(formatted_margins_summaries[["by_mail_own"]])
ggsave("./Result/AME_by_mail_own.pdf", AME_by_mail_own, width = 8, height = 6, units = "in", dpi = 600)

# ggsave("./Result/AME_both_modes_own.eps", AME_both_modes_own, width = 8, height = 6, units = "in", dpi = 800)
# ggsave("./Result/AME_in_person_own.eps", AME_in_person_own, width = 8, height = 6, units = "in", dpi = 800)
# ggsave("./Result/AME_by_mail_own.eps", AME_by_mail_own, width = 8, height = 6, units = "in", dpi = 800)

#********** Plot Average Marginal Effects on OC, CA, US Votes Confidence **********

margins_summaries_by_mode = list(
  both_modes = bind_rows(
    formatted_margins_summaries[["both_modes_OC"]] %>% mutate(region = "OC"),
    formatted_margins_summaries[["both_modes_CA"]] %>% mutate(region = "CA"),
    formatted_margins_summaries[["both_modes_US"]] %>% mutate(region = "US")
  ) %>% mutate(region = factor(region, levels = c("OC", "CA", "US"))),
  in_person = bind_rows(
    formatted_margins_summaries[["in_person_OC"]] %>% mutate(region = "OC"),
    formatted_margins_summaries[["in_person_CA"]] %>% mutate(region = "CA"),
    formatted_margins_summaries[["in_person_US"]] %>% mutate(region = "US")
  ) %>% mutate(region = factor(region, levels = c("OC", "CA", "US"))),
  by_mail = bind_rows(
    formatted_margins_summaries[["by_mail_OC"]] %>% mutate(region = "OC"),
    formatted_margins_summaries[["by_mail_CA"]] %>% mutate(region = "CA"),
    formatted_margins_summaries[["by_mail_US"]] %>% mutate(region = "US")
  ) %>% mutate(region = factor(region, levels = c("OC", "CA", "US")))
)

plot_AME_OC_CA_US = function(margins_summary_by_mode){
  AME_plot =
    ggplot(margins_summary_by_mode %>% filter(!is.na(facetgrid)),
           aes(x = item_names, y = Coefficient, colour = p_value, ymin = lb, ymax = ub, shape = region, group = region)) +
    geom_point(position = position_dodge(width = 0.9), show.legend = T) +
    geom_errorbar(size = 1, width = 0.2, position = position_dodge(width = 0.9), show.legend = T) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_grid(facetgrid ~ ., scales = "free", space = "free") +
    labs(x = "", y = "Average Marginal Effect", colour = "", shape = "", group = "") +
    scale_y_continuous(limits = c(-0.4, 0.2), breaks = c(-0.4, -0.3, -0.2, -0.1, 0, 0.1, 0.2)) +
    scale_color_manual(values = c("p < 0.05" = "#000000", "p > 0.05" = "#999999"), guide = "none") +
    theme_bw() +
    theme(legend.position = "bottom") +
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
    theme(text = element_text(size = 14), axis.text.x = element_text(size = 12), strip.text = element_text(size = 7)) +
    coord_flip()
  return(AME_plot)
}

AME_both_modes_OC_CA_US = plot_AME_OC_CA_US(margins_summaries_by_mode[["both_modes"]])
ggsave("./Result/AME_both_modes_OC_CA_US.pdf", AME_both_modes_OC_CA_US, width = 8, height = 6, units = "in", dpi = 600)

AME_in_person_OC_CA_US = plot_AME_OC_CA_US(margins_summaries_by_mode[["in_person"]])
ggsave("./Result/AME_in_person_OC_CA_US.pdf", AME_in_person_OC_CA_US, width = 8, height = 6, units = "in", dpi = 600)

AME_by_mail_OC_CA_US = plot_AME_OC_CA_US(margins_summaries_by_mode[["by_mail"]])
ggsave("./Result/AME_by_mail_OC_CA_US.pdf", AME_by_mail_OC_CA_US, width = 8, height = 6, units = "in", dpi = 600)

# ggsave("./Result/AME_both_modes_OC_CA_US.eps", AME_both_modes_OC_CA_US, width = 8, height = 6, units = "in", dpi = 800)
# ggsave("./Result/AME_in_person_OC_CA_US.eps", AME_in_person_OC_CA_US, width = 8, height = 6, units = "in", dpi = 800)
# ggsave("./Result/AME_by_mail_OC_CA_US.eps", AME_by_mail_OC_CA_US, width = 8, height = 6, units = "in", dpi = 800)

#****************************************************************
# Different Aspects of Voter Experience and Social Media Usage
#****************************************************************

IRT_prepare = function(data, y_var, x_var, z_var){
  y = data[y_var]
  x = model.matrix(as.formula(paste0("~", paste0(x_var, collapse = " + "))), data)
  z = model.matrix(as.formula(paste0("~", paste0(z_var, collapse = " + "))), data)
  return(list(y = y, x = x, z = z))
}

apply_cf_imputations = function(est_result, hIRT.out, param, component, IRT_variable){
  
  if (IRT_variable == "exp_in_person_IRT") {
    data_exp_in_person = est_result[["data"]] %>% filter(mode == "In person")
    
    data_for_hIRT_0 = IRT_prepare(data_exp_in_person %>% mutate_at(vars(component), function(x) 0), param[["y_var_exp_in_person"]], param[["x_var"]], param[["z_var"]])
    data_for_hIRT_1 = IRT_prepare(data_exp_in_person %>% mutate_at(vars(component), function(x) 1), param[["y_var_exp_in_person"]], param[["x_var"]], param[["z_var"]])
  } else if (IRT_variable == "exp_by_mail_IRT") {
    data_exp_by_mail = est_result[["data"]] %>% filter(mode == "By mail")
    
    data_for_hIRT_0 = IRT_prepare(data_exp_by_mail %>% mutate_at(vars(component), function(x) 0), param[["y_var_exp_by_mail"]], param[["x_var"]], param[["z_var"]])
    data_for_hIRT_1 = IRT_prepare(data_exp_by_mail %>% mutate_at(vars(component), function(x) 1), param[["y_var_exp_by_mail"]], param[["x_var"]], param[["z_var"]])
  } else if (IRT_variable == "social_media_IRT") {
    data_social_media = est_result[["data"]]
    
    data_for_hIRT_0 = IRT_prepare(data_social_media %>% mutate_at(vars(component), function(x) 0), param[["y_var_social_media"]], param[["x_var"]], param[["z_var"]])
    data_for_hIRT_1 = IRT_prepare(data_social_media %>% mutate_at(vars(component), function(x) 1), param[["y_var_social_media"]], param[["x_var"]], param[["z_var"]])
  } else { stop("Error: Argument input is invalid.") }
  
  latent_score_0 = predict_hIRT(hIRT.out, data_for_hIRT_0[["y"]], data_for_hIRT_0[["x"]], data_for_hIRT_0[["z"]])$post_mean
  latent_score_1 = predict_hIRT(hIRT.out, data_for_hIRT_1[["y"]], data_for_hIRT_1[["x"]], data_for_hIRT_1[["z"]])$post_mean
  
  if (IRT_variable == "exp_in_person_IRT") {
    pred_confidence_0 = prediction(est_result, data = data_exp_in_person %>% mutate(exp_in_person_IRT = latent_score_0))
    pred_confidence_1 = prediction(est_result, data = data_exp_in_person %>% mutate(exp_in_person_IRT = latent_score_1))
  } else if (IRT_variable == "exp_by_mail_IRT") {
    pred_confidence_0 = prediction(est_result, data = data_exp_by_mail %>% mutate(exp_by_mail_IRT = latent_score_0))
    pred_confidence_1 = prediction(est_result, data = data_exp_by_mail %>% mutate(exp_by_mail_IRT = latent_score_1))
  } else if (IRT_variable == "social_media_IRT") {
    pred_confidence_0 = prediction(est_result, data = data_social_media %>% mutate(social_media_IRT = latent_score_0) %>% filter(use_twitter == 1))
    pred_confidence_1 = prediction(est_result, data = data_social_media %>% mutate(social_media_IRT = latent_score_1) %>% filter(use_twitter == 1))
  } else { stop("Error: Argument input is invalid.") }
  
  confidence_0 = summary(pred_confidence_0)$Prediction
  confidence_1 = summary(pred_confidence_1)$Prediction
  
  return(list(confidence_0 = confidence_0, confidence_1 = confidence_1))
}

pool_cf_imputations = function(est_results, hIRT.outs, param, component, IRT_variable){
  confidence = mapply(apply_cf_imputations, est_results, hIRT.outs, MoreArgs = list(param = param, component = component, IRT_variable = IRT_variable), SIMPLIFY = FALSE)
  confidence_0 = mean(unlist(lapply(confidence, `[[`, "confidence_0")))
  confidence_1 = mean(unlist(lapply(confidence, `[[`, "confidence_1")))
  return(list(confidence_0 = confidence_0, confidence_1 = confidence_1))
}

apply_cf_components = function(est_results, hIRT.outs, param, components, IRT_variable){
  confidence = sapply(components, pool_cf_imputations, est_results = est_results, hIRT.outs = hIRT.outs, param = param, IRT_variable = IRT_variable, simplify = FALSE, USE.NAMES = TRUE)
  confidence = bind_rows(confidence, .id = "component")
  return(confidence)
}

apply_cf_levels = function(est_results_all_levels, hIRT.outs, param, components, IRT_variable){
  confidence = sapply(est_results_all_levels, apply_cf_components, hIRT.outs = hIRT.outs, param = param, components = components, IRT_variable = IRT_variable, simplify = FALSE, USE.NAMES = TRUE)
  confidence = bind_rows(confidence, .id = "level")
  return(confidence)
}

cf_exp_in_person = apply_cf_levels(est_results = estimation_results_alt[c("in_person_own", "in_person_OC", "in_person_CA", "in_person_US")],
                                   hIRT.outs = hIRT.out_exp_in_person,
                                   param = param_IRT,
                                   components = param_IRT[["y_var_exp_in_person"]],
                                   IRT_variable = "exp_in_person_IRT")

cf_exp_by_mail = apply_cf_levels(est_results = estimation_results_alt[c("by_mail_own", "by_mail_OC", "by_mail_CA", "by_mail_US")],
                                 hIRT.outs = hIRT.out_exp_by_mail,
                                 param = param_IRT,
                                 components = param_IRT[["y_var_exp_by_mail"]],
                                 IRT_variable = "exp_by_mail_IRT")

cf_social_media = apply_cf_levels(est_results = estimation_results_alt[c("both_modes_own", "both_modes_OC", "both_modes_CA", "both_modes_US")],
                                  hIRT.outs = hIRT.out_social_media,
                                  param = param_IRT,
                                  components = param_IRT[["y_var_social_media"]],
                                  IRT_variable = "social_media_IRT")
