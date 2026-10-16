# load packages
pacman::p_load(dplyr, tidyr, boot, openxlsx, dagitty, ggdag, ggplot2)

# dag
dag <- dagitty('dag {
"Alcohol use" [pos="-1.027,-0.635"]
"Condom use" [pos="0.374,0.696"]
"Duration of sex work" [pos="-1.093,0.638"]
"Education/ socioeconomic status" [pos="0.369,-0.647"]
"Injecting drug use" [exposure,pos="-0.473,-1.026"]
"NGO coverage" [pos="0.081,1.136"]
"Street-based SW" [exposure,pos="-1.371,-0.152"]
"Survey year" [pos="-1.101,1.194"]
"Volume of clients" [pos="-0.399,1.305"]
Age [pos="-0.186,0.044"]
City [pos="-0.800,-0.341"]
HIV [outcome,pos="0.682,-0.221"]
Rape [exposure,pos="-0.414,0.540"]
"Alcohol use" -> "Condom use"
"Alcohol use" -> "Injecting drug use"
"Alcohol use" -> HIV
"Alcohol use" <-> "Education/ socioeconomic status"
"Condom use" -> HIV
"Duration of sex work" -> "Injecting drug use"
"Duration of sex work" -> "Street-based SW"
"Education/ socioeconomic status" -> "Injecting drug use"
"Education/ socioeconomic status" -> Rape
"Injecting drug use" -> HIV
"NGO coverage" -> "Condom use"
"NGO coverage" -> HIV
"Street-based SW" -> "Alcohol use"
"Street-based SW" -> "Injecting drug use"
"Street-based SW" -> "NGO coverage"
"Street-based SW" -> "Volume of clients"
"Street-based SW" -> HIV
"Street-based SW" -> Rape
"Survey year" -> "Condom use"
"Survey year" -> "Street-based SW"
"Survey year" -> HIV
"Volume of clients" -> "NGO coverage"
"Volume of clients" -> HIV
"Volume of clients" -> Rape
Age -> "Duration of sex work"
Age -> HIV
City -> "Education/ socioeconomic status"
City -> "NGO coverage"
City -> "Street-based SW"
City -> HIV
Rape -> "Condom use"
Rape -> "NGO coverage"
Rape -> HIV
}')

# role of each variable in the analysis
roles <- c(
  "Street-based SW"                 = "Exposure",
  "Injecting drug use"              = "Mediator",
  "Rape"                            = "Mediator",
  "HIV"                             = "Outcome",
  "Age"                             = "Adjusted (all models)",
  "Education/ socioeconomic status" = "Adjusted (all models)",
  "Duration of sex work"            = "Adjusted (all models)",
  "Survey year"                     = "Adjusted (all models)",
  "City"                            = "Adjusted (all models)",
  "Alcohol use"                     = "Adjusted (outcome model only)",
  "Volume of clients"               = "Adjusted (outcome model only)",
  "Condom use"                      = "Not adjusted",
  "NGO coverage"                    = "Not adjusted"
)

dag_tidy <- tidy_dagitty(dag) %>%
  mutate(role = factor(roles[name], levels = unique(roles)))

# plot dag coloured by role
dag_plot <- ggplot(dag_tidy, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(edge_colour = "grey50") +
  geom_dag_point(aes(colour = role), size = 12) +
  geom_dag_label_repel(aes(label = name), size = 3.5, show.legend = FALSE) +
  scale_colour_manual(values = c(
    "Exposure"                      = "#E69F00",
    "Mediator"                      = "#56B4E9",
    "Outcome"                       = "#D55E00",
    "Adjusted (all models)"         = "#009E73",
    "Adjusted (outcome model only)" = "#80CDC1",
    "Not adjusted"                  = "grey75"
  )) +
  labs(colour = NULL) +
  theme_dag() +
  theme(legend.position = "bottom")
dag_plot

# save dag to data folder
ggsave("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Chapter Writing/Chapter 6 - Ukraine FSWs IDU violence and HIV/data/SW data/mediation_dag.png",
       dag_plot, width = 12, height = 8, dpi = 300, bg = "white")

# minimal adjustment sets for each path
adjustmentSets(dag, "Street-based SW", "HIV", effect = "total")
adjustmentSets(dag, "Street-based SW", "Injecting drug use", effect = "total")
adjustmentSets(dag, "Street-based SW", "Rape", effect = "total")
adjustmentSets(dag, "Injecting drug use", "HIV", effect = "total")
adjustmentSets(dag, "Rape", "HIV", effect = "total")
adjustmentSets(dag, "Street-based SW", "HIV", effect = "direct")

# covariate sets
base_set <- c("Age", "Education/ socioeconomic status", "Duration of sex work",
              "Survey year", "City")
y_set    <- c(base_set, "Alcohol use", "Volume of clients")

# check there are no biasing paths
isAdjustmentSet(dag, base_set, "Street-based SW", "HIV")
isAdjustmentSet(dag, base_set, "Street-based SW", "Injecting drug use")
isAdjustmentSet(dag, base_set, "Street-based SW", "Rape")
isAdjustmentSet(dag, c(y_set, "Street-based SW", "Rape"), "Injecting drug use", "HIV")
isAdjustmentSet(dag, c(y_set, "Street-based SW", "Injecting drug use"), "Rape", "HIV")

# set wd
setwd("C:/Users/vl22683/OneDrive - University of Bristol/Documents/PhD Chapter Writing/Chapter 6 - Ukraine FSWs IDU violence and HIV/data/SW data")

# load data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

# restrict to 2013 to 2021
sw_combined_clean <- sw_combined_clean %>%
  filter(as.numeric(as.character(year)) >= 2013)

# table 1 characteristics by survey year

# whole sample from the 2013 to 2021 survey rounds
t1_dat <- sw_combined_clean

# sample size by survey year and overall
t1_n <- t1_dat %>% count(year)
t1_n
nrow(t1_dat)

# variables in table 1 with labels
t1_rows <- c("Age (years)"          = "age_cat",
             "HIV infection"        = "hiv_test_rslt_bin",
             "Street-based SW"      = "street_sw_bin",
             "Injecting drug use"   = "idu_12m_bin",
             "Rape"                 = "violence_rape_ever_bin",
             "Condom use"           = "client_condom_lastsex_bin",
             "NGO client"           = "ngo_client_lifetime_bin",
             "Education"            = "education_3cat",
             "Volume of clients"    = "sw_partners_clients_30d_3cat",
             "Duration of sex work" = "years_in_sw_3cat")

# labels for categories
t1_labels <- c("Underage" = "<18", "18-24" = "18–24", "25-34" = "25–34", "35+" = "≥35",
               "Negative" = "No", "Positive" = "Yes",
               "Primary" = "Primary education", "Secondary" = "Secondary education", "Higher" = "Higher education",
               "0-19" = "0–19", "20-49" = "20–49", "50+" = "≥50",
               "0-3" = "≤3 years", "4-9" = "4–9 years", "10+" = "≥10 years")

# function for n (%) by survey year and overall
t1_var <- function(var) {
  lv <- c(levels(factor(t1_dat[[var]])), "Missing")
  d  <- t1_dat %>%
    mutate(year  = as.character(year),
           level = as.character(.data[[var]]),
           level = factor(if_else(is.na(level), "Missing", level), levels = lv))

  bind_rows(d, d %>% mutate(year = "Overall")) %>%
    mutate(year = factor(year, levels = c(sort(unique(as.character(t1_dat$year))), "Overall"))) %>%
    count(year, level, .drop = FALSE) %>%
    group_by(year) %>%
    mutate(cell = sprintf("%s (%.1f)", formatC(n, big.mark = ",", format = "d"), 100 * n / sum(n))) %>%
    ungroup() %>%
    select(-n) %>%
    pivot_wider(names_from = year, values_from = cell) %>%
    mutate(level = as.character(level))
}

# build table 1
table1 <- bind_rows(lapply(names(t1_rows), function(nm) {
  bind_rows(tibble(level = nm), t1_var(t1_rows[[nm]]))
})) %>%
  mutate(level = recode(level, !!!t1_labels)) %>%
  rename(Characteristic = level)

# add sample size to column names
names(table1)[names(table1) %in% t1_n$year] <- sprintf("%s (N=%s)", t1_n$year, formatC(t1_n$n, big.mark = ",", format = "d"))
names(table1)[names(table1) == "Overall"] <- sprintf("Overall (N=%s)", formatC(nrow(t1_dat), big.mark = ",", format = "d"))
names(table1) <- sub("^2017 ", "2017/18 ", names(table1))
table1

# save table 1
openxlsx::write.xlsx(table1, "table1_characteristics.xlsx")

# variables
outcome   <- "hiv_test_rslt_bin"
exposure  <- "street_sw_bin"
mediators <- c("idu_12m_bin", "violence_rape_ever_bin")

# confounders from dag
covariates <- c("age_cat", "education_3cat", "alcohol_30d_bin",
                "sw_partners_clients_30d_3cat", "years_in_sw_3cat", "year", "city")

# convert yes/no variables to 0/1
to01 <- function(x) {
  if (is.numeric(x)) return(x)
  ifelse(is.na(x), NA_integer_, as.integer(x %in% c("Yes", "Positive")))
}

# complete case data
dat <- sw_combined_clean %>%
  dplyr::select(all_of(c(outcome, exposure, mediators, covariates))) %>%
  mutate(across(all_of(c(outcome, exposure, mediators)), to01)) %>%
  drop_na() %>%
  mutate(across(all_of(covariates), ~ droplevels(as.factor(.x))))

# dummy code covariates
X <- model.matrix(~ ., data = dat[covariates])[, -1, drop = FALSE]
colnames(X) <- gsub("[^A-Za-z0-9_]", "_", colnames(X))

dat <- bind_cols(select(dat, -all_of(covariates)), as.data.frame(X))

# covariates from dag
y_only    <- grep("^alcohol_30d_bin|^sw_partners_clients_30d_3cat", colnames(X), value = TRUE)
covs_base <- paste(setdiff(colnames(X), y_only), collapse = " + ")
covs_y    <- paste(colnames(X), collapse = " + ")

# check sample
nrow(dat)
table(dat[[outcome]], dat[[exposure]])

# product of coefficients

# model formulas
f_t  <- as.formula(paste("hiv_test_rslt_bin ~ street_sw_bin +", covs_base))
f_m1 <- as.formula(paste("idu_12m_bin ~ street_sw_bin +", covs_base))
f_m2 <- as.formula(paste("violence_rape_ever_bin ~ street_sw_bin +", covs_base))
f_y  <- as.formula(paste("hiv_test_rslt_bin ~ street_sw_bin + idu_12m_bin + violence_rape_ever_bin +", covs_y))

# design matrices
mmt <- model.matrix(f_t, dat)
mm1 <- model.matrix(f_m1, dat)
mm2 <- model.matrix(f_m2, dat)
mmy <- model.matrix(f_y, dat)

# average risk difference from logistic model
ame <- function(X, beta, var) {
  X1 <- X; X1[, var] <- 1
  X0 <- X; X0[, var] <- 0
  mean(plogis(X1 %*% beta) - plogis(X0 %*% beta))
}

# main analysis

# function to fit models and calculate indirect effects and proportion mediated
rd_fun <- function(d, i) {
  tot <- glm.fit(mmt[i, ], d$hiv_test_rslt_bin[i], family = binomial())$coefficients
  m1  <- lm.fit(mm1[i, ], d$idu_12m_bin[i])$coefficients
  m2  <- lm.fit(mm2[i, ], d$violence_rape_ever_bin[i])$coefficients
  y   <- glm.fit(mmy[i, ], d$hiv_test_rslt_bin[i], family = binomial())$coefficients

  # average risk differences from logistic models
  c_total <- ame(mmt[i, ], tot, "street_sw_bin")
  cprime  <- ame(mmy[i, ], y, "street_sw_bin")
  b1      <- ame(mmy[i, ], y, "idu_12m_bin")
  b2      <- ame(mmy[i, ], y, "violence_rape_ever_bin")

  # paths to mediators
  a1 <- m1[["street_sw_bin"]]
  a2 <- m2[["street_sw_bin"]]

  # total, direct and indirect effects and proportion mediated
  c(c_total = c_total,
    a1 = a1, a2 = a2, b1 = b1, b2 = b2,
    cprime     = cprime,
    ind_idu    = a1*b1,
    ind_rape   = a2*b2,
    total_ind  = a1*b1 + a2*b2,
    prop_idu   = a1*b1 / c_total,
    prop_rape  = a2*b2 / c_total,
    prop_total = (a1*b1 + a2*b2) / c_total,
    # log odds
    c_total_lo = tot[["street_sw_bin"]],
    cprime_lo  = y[["street_sw_bin"]],
    b1_lo      = y[["idu_12m_bin"]],
    b2_lo      = y[["violence_rape_ever_bin"]])
}

# parallel bootstrap
n_cores <- max(1, parallel::detectCores() - 1)
cl <- parallel::makeCluster(n_cores)
parallel::clusterExport(cl, c("mmt", "mm1", "mm2", "mmy", "ame"))

# bootstrap confidence intervals
set.seed(123)
bt_rd <- boot(dat, rd_fun, R = 1000, parallel = "snow", ncpus = n_cores, cl = cl)

# main results
rd_results <- data.frame(
  label = names(bt_rd$t0),
  est   = bt_rd$t0,
  ci.lower = apply(bt_rd$t, 2, quantile, 0.025, na.rm = TRUE),
  ci.upper = apply(bt_rd$t, 2, quantile, 0.975, na.rm = TRUE),
  row.names = NULL
)

# odds ratios for the log odds rows
rd_results <- rd_results %>%
  mutate(across(c(est, ci.lower, ci.upper), ~ if_else(grepl("_lo$", label), exp(.x), .x)),
         label = sub("_lo$", "_or", label))
rd_results

# save main results
write.csv(rd_results, "mediation_main_results.csv", row.names = FALSE)

# check fitted probabilities 0 to 1
fit_m1 <- lm(f_m1, data = dat)
fit_m2 <- lm(f_m2, data = dat)

summary(fitted(fit_m1))
summary(fitted(fit_m2))

# proportion of fitted values outside 0 to 1
mean(fitted(fit_m1) < 0 | fitted(fit_m1) > 1)
mean(fitted(fit_m2) < 0 | fitted(fit_m2) > 1)

# check exposure by mediator interaction in the hiv model
int_fit <- glm(update(f_y, . ~ . + street_sw_bin:idu_12m_bin + street_sw_bin:violence_rape_ever_bin),
               data = dat, family = binomial)
summary(int_fit)$coefficients[c("street_sw_bin:idu_12m_bin", "street_sw_bin:violence_rape_ever_bin"), ]
