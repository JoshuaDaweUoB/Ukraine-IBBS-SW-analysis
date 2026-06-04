# For each variable in exposures, print its levels or unique values
for (var in renamed_vars) {
  cat("\nVariable:", var, "\n")
  if (is.factor(sw_data_2008_clean[[var]])) {
    print(levels(sw_data_2008_clean[[var]]))
  } else {
    print(unique(sw_data_2008_clean[[var]]))
  }
}

# variables to relevel

# education
sw_data_2008_clean <- sw_data_2008_clean %>%
  mutate(
    education_3cat = case_when(
      education %in% c(
        "Complete general secondary (or vocational) (11 years etc.), incomplete higher",
        "Basic (incomplete) secondary (complete 9 years)"
      ) ~ 0,
      education %in% c(
        "Complete higher education (higher educational institutions of accreditation levels III and IV)",
        "Basic higher (technical school, higher educational institutions of accreditation levels I and II)"
      ) ~ 1,
      education %in% c("Other", "Primary (incomplete 7 years)") ~ 2,
      TRUE ~ NA_real_
    ),
    education_3cat = factor(
      education_3cat,
      levels = c(0, 1, 2),
      labels = c("Secondary/Basic secondary", "Higher/Basic higher", "Other/Primary")
    )
  )

# marital_status
sw_data_2008_clean <- sw_data_2008_clean %>%
  mutate(
    marital_status_3cat = case_when(
      marital_status %in% c(
        "Married but living neither with my husband nor with some other sexual partner",
        "Unmarried and not living with a sexual partner"
      ) ~ 0,
      marital_status %in% c(
        "Officially unmarried but living with a permanent sexual partner",
        "Married and living with my husband"
      ) ~ 1,
      marital_status == "Married but living with some other sexual partner" ~ 2,
      TRUE ~ NA_real_
    ),
    marital_status_3cat = factor(
      marital_status_3cat,
      levels = c(0, 1, 2),
      labels = c("Unmarried/no partner", "Married/partnered", "Other")
    )
  )

# travelled for work past 12 months
sw_data_2008_clean <- sw_data_2008_clean %>%
  mutate(
    city_travel_12m_bin = case_when(
      city_travel_12m == "Yes" ~ 1,
      city_travel_12m == "No"  ~ 0,
      TRUE ~ NA_real_
    ),
    city_travel_12m_bin = factor(
      city_travel_12m_bin,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

# drunk alcohol past 30 days
sw_data_2008_clean <- sw_data_2008_clean %>%
  mutate(
    alcohol_30d_bin = case_when(
      alcohol_30d_bin == "Yes" ~ 1,
      alcohol_30d_bin == "No"  ~ 0,
      TRUE ~ NA_real_
    ),
    alcohol_30d_bin = factor(
      alcohol_30d_bin,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

# drugs past 30 days
sw_data_2008_clean <- sw_data_2008_clean %>%
  mutate(
    drugs_30d_bin = case_when(
      drugs_30d_bin == "Yes" ~ 1,
      drugs_30d_bin %in% c("No", "I used before, now I don’t") ~ 0,
      TRUE ~ NA_real_
    ),
    drugs_30d_bin = factor(
      drugs_30d_bin,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

# used condom last sex with clients
sw_data_2008_clean <- sw_data_2008_clean %>%
  mutate(
    client_condom_lastsex_3cat = case_when(
      client_condom_lastsex == "Yes" ~ 1,
      client_condom_lastsex == "No"  ~ 0,
      client_condom_lastsex == "Difficult to answer" ~ 2,
      TRUE ~ NA_real_
    ),
    client_condom_lastsex_3cat = factor(
      client_condom_lastsex_3cat,
      levels = c(0, 1, 2),
      labels = c("No", "Yes", "Uncertain")
    )
  )

# used condom last sex with permanent partner
sw_data_2008_clean <- sw_data_2008_clean %>%
  mutate(
    perm_partner_condom_lastsex_3cat = case_when(
      perm_partner_condom_lastsex == "Yes" ~ 1,
      perm_partner_condom_lastsex == "No"  ~ 0,
      perm_partner_condom_lastsex %in% c("No such partner", "Difficult to answer") ~ 2,
      TRUE ~ NA_real_
    ),
    perm_partner_condom_lastsex_3cat = factor(
      perm_partner_condom_lastsex_3cat,
      levels = c(0, 1, 2),
      labels = c("No", "Yes", "No partner / Uncertain")
    )
  )

# condom consistency with clients
sw_data_2008_clean <- sw_data_2008_clean %>%
  mutate(
    client_condom_freq_3cat = case_when(
      client_condom_freq_30d == "Always" ~ 2,
      client_condom_freq_30d %in% c(
        "Not always, but more than in a half of cases (>50% cases)",
        "In a half of cases (50% cases)"
      ) ~ 1,
      client_condom_freq_30d %in% c(
        "Less than in a half of cases (<50% cases)",
        "Never"
      ) ~ 0,
      TRUE ~ NA_real_
    ),
    client_condom_freq_3cat = factor(
      client_condom_freq_3cat,
      levels = c(0, 1, 2),
      labels = c("Rarely/Never", "Sometimes", "Always")
    )
  )

# NGO access in lifetime
sw_data_2008_clean <- sw_data_2008_clean %>%
  mutate(
    ngo_access_lifetime_bin = case_when(
      ngo_access_lifetime == "Yes" ~ 1,
      ngo_access_lifetime == "No"  ~ 0,
      TRUE ~ NA_real_
    ),
    ngo_access_lifetime_bin = factor(
      ngo_access_lifetime_bin,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

# HIV tested in lifetime
sw_data_2008_clean <- sw_data_2008_clean %>%
  mutate(
    hiv_tested_lifetime_bin = case_when(
      hiv_tested_lifetime == "Yes" ~ 1,
      hiv_tested_lifetime == "No"  ~ 0,
      TRUE ~ NA_real_
    ),
    hiv_tested_lifetime_bin = factor(
      hiv_tested_lifetime_bin,
      levels = c(0, 1),
      labels = c("No", "Yes")
    )
  )

# self reported hiv status
sw_data_2008_clean <- sw_data_2008_clean %>%
  mutate(
    hiv_status_selfreport_bin = case_when(
      hiv_status_selfreport == "HIV positive" ~ 1,
      hiv_status_selfreport == "HIV negative" ~ 0,
      TRUE ~ NA_real_
    ),
    hiv_status_selfreport_bin = factor(
      hiv_status_selfreport_bin,
      levels = c(0, 1),
      labels = c("HIV negative", "HIV positive")
    )
  )

# sti and bbv testing results
sw_data_2008_clean <- sw_data_2008_clean %>%
  mutate(
    hiv_test_rslt_bin = case_when(
      hiv_test_rslt == "Positive" ~ 1,
      hiv_test_rslt == "Negative" ~ 0,
      TRUE ~ NA_real_
    ),
    hiv_test_rslt_bin = factor(
      hiv_test_rslt_bin,
      levels = c(0, 1),
      labels = c("Negative", "Positive")
    ),

    syphilis_test_rslt_bin = case_when(
      syphilis_test_rslt == "Positive" ~ 1,
      syphilis_test_rslt == "Negative" ~ 0,
      TRUE ~ NA_real_
    ),
    syphilis_test_rslt_bin = factor(
      syphilis_test_rslt_bin,
      levels = c(0, 1),
      labels = c("Negative", "Positive")
    )
  )

## check relevelling worked

# education
with(
  sw_data_2008_clean,
  table(education, education_3cat, useNA = "ifany")
)

# marital status
with(
  sw_data_2008_clean,
  table(marital_status, marital_status_3cat, useNA = "ifany")
)

# city travel (12m)
with(
  sw_data_2008_clean,
  table(city_travel_12m, city_travel_12m_bin, useNA = "ifany")
)

# alcohol (30d)
table(sw_data_2008_clean$alcohol_30d_bin, useNA = "ifany")

# drugs (30d)
table(sw_data_2008_clean$drugs_30d_bin, useNA = "ifany")

# client condom last sex
with(
  sw_data_2008_clean,
  table(client_condom_lastsex, client_condom_lastsex_3cat, useNA = "ifany")
)

# permanent partner condom last sex
with(
  sw_data_2008_clean,
  table(
    perm_partner_condom_lastsex,
    perm_partner_condom_lastsex_3cat,
    useNA = "ifany"
  )
)

# client condom frequency (30d)
with(
  sw_data_2008_clean,
  table(client_condom_freq_30d, client_condom_freq_3cat, useNA = "ifany")
)

# ngo access (lifetime)
with(
  sw_data_2008_clean,
  table(ngo_access_lifetime, ngo_access_lifetime_bin, useNA = "ifany")
)

# hiv tested (lifetime)
with(
  sw_data_2008_clean,
  table(hiv_tested_lifetime, hiv_tested_lifetime_bin, useNA = "ifany")
)

# self-reported hiv status
with(
  sw_data_2008_clean,
  table(
    hiv_status_selfreport,
    hiv_status_selfreport_bin,
    useNA = "ifany"
  )
)

# hiv test result
with(
  sw_data_2008_clean,
  table(hiv_test_rslt, hiv_test_rslt_bin, useNA = "ifany")
)

# syphilis test result
with(
  sw_data_2008_clean,
  table(syphilis_test_rslt, syphilis_test_rslt_bin, useNA = "ifany")
)


## 2009 archive


# For each variable in exposures, print its levels or unique values
for (var in renamed_vars) {
  cat("\nVariable:", var, "\n")
  if (is.factor(sw_data_2009_clean[[var]])) {
    print(levels(sw_data_2009_clean[[var]]))
  } else {
    print(unique(sw_data_2009_clean[[var]]))
  }
}

# create missing columns to match 2008 structure
missing_vars <- c(
  "country_travel_12m", "partners_nonsw_7d", "condom_access_12m",
  "alcohol_30d_bin", "idu_12m_bin", "idu_30d_num", "used_syringe_30d_bin",
  "ngo_access_lifetime", "ngo_access_12m", "ngo_access_30d"
)
sw_data_2009_clean[missing_vars] <- NA

# relevelling variables
sw_data_2009_clean <- sw_data_2009_clean %>%
  mutate(
    # education
    education_3cat = case_when(
      education %in% c(
        "Complete general secondary (or vocational) (11 years etc.), incomplete higher",
        "Basic (incomplete) secondary (complete 9 years)"
      ) ~ 0,
      education %in% c(
        "Complete higher education (higher educational institutions of accreditation levels III and IV)",
        "Basic higher (technical school, higher educational institutions of accreditation levels I and II)"
      ) ~ 1,
      education %in% c("Other", "Primary (incomplete 9 years)") ~ 2,
      TRUE ~ NA_real_
    ),
    education_3cat = factor(education_3cat, levels=c(0,1,2),
                             labels=c("Secondary/Basic secondary","Higher/Basic higher","Other/Primary")),

    # marital status
    marital_status_3cat = case_when(
      marital_status %in% c(
        "Married but living neither with my husband nor with some other sexual partner",
        "Unmarried and not living with a sexual partner"
      ) ~ 0,
      marital_status %in% c(
        "Officially unmarried but living with a permanent sexual partner",
        "Married and living with my husband"
      ) ~ 1,
      marital_status == "Married but living with some other sexual partner" ~ 2,
      TRUE ~ NA_real_
    ),
    marital_status_3cat = factor(marital_status_3cat, levels=c(0,1,2),
                                 labels=c("Unmarried/no partner","Married/partnered","Other")),

    # city travel
    city_travel_12m_bin = case_when(
      city_travel_12m == "Yes" ~ 1,
      city_travel_12m == "No"  ~ 0,
      TRUE ~ NA_real_
    ),
    city_travel_12m_bin = factor(city_travel_12m_bin, levels=c(0,1), labels=c("No","Yes")),

    # alcohol 30d binary from frequency
    alcohol_30d_bin = case_when(
      alcohol_30d_num %in% c("Never","No answer") ~ 0,
      alcohol_30d_num %in% c("Less than once a week","No less than once a week","Every day") ~ 1,
      TRUE ~ NA_real_
    ),
    alcohol_30d_bin = factor(alcohol_30d_bin, levels=c(0,1), labels=c("No","Yes")),

    # drugs
    drugs_30d_bin = case_when(
      drugs_30d_bin == "Yes" ~ 1,
      drugs_30d_bin %in% c("No","I used before, now I don’t") ~ 0,
      TRUE ~ NA_real_
    ),
    drugs_30d_bin = factor(drugs_30d_bin, levels=c(0,1), labels=c("No","Yes")),

    # condom use last sex with client
    client_condom_lastsex_3cat = case_when(
      client_condom_lastsex == "Yes" ~ 1,
      client_condom_lastsex == "No"  ~ 0,
      client_condom_lastsex == "Difficult to answer" ~ 2,
      TRUE ~ NA_real_
    ),
    client_condom_lastsex_3cat = factor(client_condom_lastsex_3cat, levels=c(0,1,2),
                                        labels=c("No","Yes","Uncertain")),

    # condom use last sex with permanent partner
    perm_partner_condom_lastsex_3cat = case_when(
      perm_partner_condom_lastsex == "Yes" ~ 1,
      perm_partner_condom_lastsex == "No" ~ 0,
      perm_partner_condom_lastsex %in% c("No such partners","Difficult to answer") ~ 2,
      TRUE ~ NA_real_
    ),
    perm_partner_condom_lastsex_3cat = factor(perm_partner_condom_lastsex_3cat, levels=c(0,1,2),
                                             labels=c("No","Yes","No partner / Uncertain")),

    # client condom frequency 30d
    client_condom_freq_3cat = case_when(
      client_condom_freq_30d == "Always" ~ 2,
      client_condom_freq_30d %in% c("More often than half of the cases (>50%)","Half of the cases (50% of the cases)") ~ 1,
      client_condom_freq_30d %in% c("Less often than half of the cases (<50%)","Never") ~ 0,
      TRUE ~ NA_real_
    ),
    client_condom_freq_3cat = factor(client_condom_freq_3cat, levels=c(0,1,2),
                                    labels=c("Rarely/Never","Sometimes","Always")),

    # permanent partner 30d condom use
    perm_partner_condom_30d_bin = case_when(
      perm_partner_condom_30d == "I used a condom every time" ~ 1,
      perm_partner_condom_30d == "There was a case of not using" ~ 0,
      TRUE ~ NA_real_
    ),
    perm_partner_condom_30d_bin = factor(perm_partner_condom_30d_bin, levels=c(0,1), labels=c("No","Yes")),

    # casual partner 30d condom use
    cas_partner_condom_30d_bin = case_when(
      cas_partner_condom_30d == "I used a condom every time" ~ 1,
      cas_partner_condom_30d == "There was a case of not using" ~ 0,
      TRUE ~ NA_real_
    ),
    cas_partner_condom_30d_bin = factor(cas_partner_condom_30d_bin, levels=c(0,1), labels=c("No","Yes")),

    # HIV tested lifetime
    hiv_tested_lifetime_bin = case_when(
      hiv_tested_lifetime == "Yes" ~ 1,
      hiv_tested_lifetime %in% c("No","No answer") ~ 0,
      TRUE ~ NA_real_
    ),
    hiv_tested_lifetime_bin = factor(hiv_tested_lifetime_bin, levels=c(0,1), labels=c("No","Yes")),

    # HIV status self-report
    hiv_status_selfreport_bin = case_when(
      hiv_status_selfreport == "HIV positive" ~ 1,
      hiv_status_selfreport == "HIV negative" ~ 0,
      TRUE ~ NA_real_
    ),
    hiv_status_selfreport_bin = factor(hiv_status_selfreport_bin, levels=c(0,1),
                                      labels=c("HIV negative","HIV positive")),

    # HIV test result
    hiv_test_rslt_bin = case_when(
      hiv_test_rslt == "Positive" ~ 1,
      hiv_test_rslt == "Negative" ~ 0,
      TRUE ~ NA_real_
    ),
    hiv_test_rslt_bin = factor(hiv_test_rslt_bin, levels=c(0,1), labels=c("Negative","Positive")),

    # syphilis test result
    syphilis_test_rslt_bin = case_when(
      syphilis_test_rslt == "Positive" ~ 1,
      syphilis_test_rslt == "Negative" ~ 0,
      TRUE ~ NA_real_
    ),
    syphilis_test_rslt_bin = factor(syphilis_test_rslt_bin, levels=c(0,1), labels=c("Negative","Positive"))
  )

## check relevelling worked for 2009

# education
with(
  sw_data_2009_clean,
  table(education, education_3cat, useNA = "ifany")
)

# marital status
with(
  sw_data_2009_clean,
  table(marital_status, marital_status_3cat, useNA = "ifany")
)

# city travel (12m)
with(
  sw_data_2009_clean,
  table(city_travel_12m, city_travel_12m_bin, useNA = "ifany")
)

# alcohol (30d)
table(sw_data_2009_clean$alcohol_30d_bin, useNA = "ifany")

# drugs (30d)
table(sw_data_2009_clean$drugs_30d_bin, useNA = "ifany")

# client condom last sex
with(
  sw_data_2009_clean,
  table(client_condom_lastsex, client_condom_lastsex_3cat, useNA = "ifany")
)

# permanent partner condom last sex
with(
  sw_data_2009_clean,
  table(
    perm_partner_condom_lastsex,
    perm_partner_condom_lastsex_3cat,
    useNA = "ifany"
  )
)

# client condom frequency (30d)
with(
  sw_data_2009_clean,
  table(client_condom_freq_30d, client_condom_freq_3cat, useNA = "ifany")
)

# permanent partner 30d condom use
with(
  sw_data_2009_clean,
  table(perm_partner_condom_30d, perm_partner_condom_30d_bin, useNA = "ifany")
)

# casual partner 30d condom use
with(
  sw_data_2009_clean,
  table(cas_partner_condom_30d, cas_partner_condom_30d_bin, useNA = "ifany")
)

# hiv tested (lifetime)
with(
  sw_data_2009_clean,
  table(hiv_tested_lifetime, hiv_tested_lifetime_bin, useNA = "ifany")
)

# self-reported hiv status
with(
  sw_data_2009_clean,
  table(
    hiv_status_selfreport,
    hiv_status_selfreport_bin,
    useNA = "ifany"
  )
)

# hiv test result
with(
  sw_data_2009_clean,
  table(hiv_test_rslt, hiv_test_rslt_bin, useNA = "ifany")
)

# syphilis test result
with(
  sw_data_2009_clean,
  table(syphilis_test_rslt, syphilis_test_rslt_bin, useNA = "ifany")
)









# variables to derive
sw_data_2009_clean <- sw_data_2009_clean %>%
  mutate(
    alcohol_30d_bin = case_when(
      alcohol_30d_num %in% c("Every day", "Less than once a week", "No less than once a week") ~ "Yes",
      alcohol_30d_num == "Never" ~ "No",
      TRUE ~ NA_character_
    ),
    alcohol_30d_daily_bin = case_when(
      alcohol_30d_num == "Every day" ~ "Yes",
      alcohol_30d_num %in% c("Never", "Less than once a week", "No less than once a week") ~ "No",
      TRUE ~ NA_character_
    ),
    ngo_access_12m = ifelse(
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Syringe exchange` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Obtaining disinfecting solutions` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Obtaining hygienic means` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Obtaining condoms` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Obtaining information booklets or brochures` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Helpline service` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Attending mutual support groups` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Attending consultations on safe injecting drug use` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Attending consultations on HIV/AIDS, STI, and ways of their prevention` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Lawyer’s consultations` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Psychologist’s consultations` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Peer-to-peer consultations` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Free HIV/AIDS testing` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Free testing for venereal diseases (testing and counseling)` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Free medical treatment of venereal diseases` == "Yes" |
      `D5. What help or services did you receive from NGOs in the LAST 12 MONTHS? Other` == "Yes",
      "Yes",
      "No"
    ),
    idu_30d_bin = case_when(
      if_any(c(
        `С3.2.1 Drugs taken via injection in the past 30 days? Tramadol/tramal`,
        `С3.2.2 Drugs taken via injection in the past 30 days? Heroin`,
        `С3.2.3 Drugs taken via injection in the past 30 days? Liquid opium extract`,
        `С3.2.4 Drugs taken via injection in the past 30 days? Other Opiates`,
        `С3.2.5 Drugs taken via injection in the past 30 days? Cocaine`,
        `С3.2.6 Drugs taken via injection in the past 30 days? Amphetamine`,
        `С3.2.7 Drugs taken via injection in the past 30 days? Methamphetamine powder (crystallized)`,
        `С3.2.8 Drugs taken via injection in the past 30 days? Methamphetamine liquid («tina», «pervitin»)`,
        `С3.2.9 Drugs taken via injection in the past 30 days? Methcathinone («tweaker»)`,
        `С3.2.10 Drugs taken via injection in the past 30 days? Cathinone («magma»)`,
        `С3.2.11 Drugs taken via injection in the past 30 days? “Extasy”, MDMA`,
        `С3.2.12 Drugs taken via injection in the past 30 days? Other Stimulants`,
        `С3.2.13 Drugs taken via injection in the past 30 days? LCD, mushrooms`,
        `С3.2.14 Drugs taken via injection in the past 30 days? Other`
      ), ~ . == "Yes") ~ "Yes",
      
      if_any(c(
        `С3.2.1 Drugs taken via injection in the past 30 days? Tramadol/tramal`,
        `С3.2.2 Drugs taken via injection in the past 30 days? Heroin`,
        `С3.2.3 Drugs taken via injection in the past 30 days? Liquid opium extract`,
        `С3.2.4 Drugs taken via injection in the past 30 days? Other Opiates`,
        `С3.2.5 Drugs taken via injection in the past 30 days? Cocaine`,
        `С3.2.6 Drugs taken via injection in the past 30 days? Amphetamine`,
        `С3.2.7 Drugs taken via injection in the past 30 days? Methamphetamine powder (crystallized)`,
        `С3.2.8 Drugs taken via injection in the past 30 days? Methamphetamine liquid («tina», «pervitin»)`,
        `С3.2.9 Drugs taken via injection in the past 30 days? Methcathinone («tweaker»)`,
        `С3.2.10 Drugs taken via injection in the past 30 days? Cathinone («magma»)`,
        `С3.2.11 Drugs taken via injection in the past 30 days? “Extasy”, MDMA`,
        `С3.2.12 Drugs taken via injection in the past 30 days? Other Stimulants`,
        `С3.2.13 Drugs taken via injection in the past 30 days? LCD, mushrooms`,
        `С3.2.14 Drugs taken via injection in the past 30 days? Other`
      ), ~ . == "No") ~ "No",
      
      TRUE ~ NA_character_
    )
  )


## 2011 archive


# derived variables
sw_data_2011_clean <- sw_data_2011_clean %>%
  mutate(
    client_condom_bin_30d = case_when(
      `B12.2.2 Think again about events of the LAST 30 DAYS. Did you have a case of NOT using a condom with a CLIENT while providing such service as vaginal sex?` == "There was a case of not using" |
      `В12.3.2 Think again about events of the LAST 30 DAYS. Did you have a case of NOT using a condom with a CLIENT while providing such service as anal sex?` == "There was a case of not using" ~ "No",
      
      `B12.2.2 Think again about events of the LAST 30 DAYS. Did you have a case of NOT using a condom with a CLIENT while providing such service as vaginal sex?` == "There was no such case" &
      `В12.3.2 Think again about events of the LAST 30 DAYS. Did you have a case of NOT using a condom with a CLIENT while providing such service as anal sex?` == "There was no such case" ~ "Yes",
      
      TRUE ~ NA_character_
    ),
    alcohol_30d_bin = case_when(
      alcohol_30d_num %in% c("Every day", "Less than once a week", "No less than once a week") ~ "Yes",
      alcohol_30d_num == "Never" ~ "No",
      TRUE ~ NA_character_
    ),
    alcohol_30d_daily_bin = case_when(
      alcohol_30d_num == "Every day" ~ "Yes",
      alcohol_30d_num %in% c("Never", "Less than once a week", "No less than once a week") ~ "No",
      TRUE ~ NA_character_
    ),
    ngo_access_12m = ifelse(
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Syringe exchange` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Obtaining disinfecting solutions` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Obtaining hygienic means` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Obtaining condoms` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Obtaining information booklets or brochures` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Helpline service` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Attending mutual support groups` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Attending consultations on safe injecting drug use` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Attending consultations on HIV/AIDS, STI, and ways of their prevention` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Lawyer’s consultations` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Psychologist’s consultations` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Peer-to-peer consultations` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Free HIV/AIDS testing` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Free testing for venereal diseases (testing and counseling)` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Free medical treatment of venereal diseases` == "Yes" |
      `D5. What help or services did you receive from non-governmental organizations in the LAST 12 MONTHS? Other` == "Yes",
      "Yes",
      "No"
    ),
    sex_with_alcohol_30d = case_when(
      `С5.1 How often did you use the following during the LAST MONTH [30 days] before a sexual contact(s) with your client(s)? Alcohol` == "Never" ~ "No",
      `С5.1 How often did you use the following during the LAST MONTH [30 days] before a sexual contact(s) with your client(s)? Alcohol` %in% c("50%", ">50%", "<50%", "Always") ~ "Yes",
      TRUE ~ NA_character_
    ),
    sex_with_drugs_30d = case_when(
      `С5.2 How often did you use the following during the LAST MONTH [30 days] before a sexual contact(s) with your client(s)? Narcotic substances` == "Never" ~ "No",
      `С5.2 How often did you use the following during the LAST MONTH [30 days] before a sexual contact(s) with your client(s)? Narcotic substances` %in% c("50%", ">50%", "<50%", "Always") ~ "Yes",
      TRUE ~ NA_character_
    )
  )


# 2013 archive


# derived variables
sw_data_2013_clean <- sw_data_2013_clean %>%
  mutate(
    alcohol_30d_daily_bin = case_when(
      alcohol_30d_num == c("20-39 times","40 times and more") ~ "Yes",
      alcohol_30d_num %in% c("Never", "1-2 times", "3-5 times", "6-9 times", "10-19 times") ~ "No",
      TRUE ~ NA_character_
    ),
    alcohol_30d_bin = case_when(
      alcohol_30d_num %in% c("1-2 times", "3-5 times", "6-9 times", "10-19 times", "20-39 times", "40 times and more") ~ "Yes",
      alcohol_30d_num == "Never" ~ "No",
      TRUE ~ NA_character_
    ),
    violence_support_any = case_when(
      violence_support_ngo == "Yes" |
      `F3_2. Have you addressed anywhere or to anyone for help? To relatives (parents, husband/cohabitant, friend)` == "Yes" |
      `F3_3. Have you addressed anywhere or to anyone for help? To other client, whom I provide sexual services` == "Yes" |
      `F3_4. Have you addressed anywhere or to anyone for help? To police` == "Yes" |
      `F3_5. Have you addressed anywhere or to anyone for help? To other girl/woman who provide sexual services` == "Yes" |
      `F3_6. Have you addressed anywhere or to anyone for help? To pimp/”mom”` == "Yes" |
      `F3_7. Have you addressed anywhere or to anyone for help? Other` == "Yes" ~ "Yes",
      
      `F3_8. Have you addressed anywhere or to anyone for help? Did not address for help` == "Yes" ~ "No",
      
      `F3_9. Have you addressed anywhere or to anyone for help? Difficult to answer/refusal to answer` == "Yes" ~ "Refuse to answer",
      
      TRUE ~ NA_character_
    ),
    ngo_access_12m = case_when(
      `D6_1 What help or services did you receive from NGO in the LAST 12 MONTHS? Free treatment of venereal diseases` == "Yes" |
      `D6_2 What help or services did you receive from NGO in the LAST 12 MONTHS? Free testing for venereal diseases` == "Yes" |
      `D6_3 What help or services did you receive from NGO in the LAST 12 MONTHS? Free HIV/AIDS testing` == "Yes" |
      `D6_4 What help or services did you receive from NGO in the LAST 12 MONTHS? Free testing for Hepatitis C` == "Yes" |
      `D6_5 What help or services did you receive from NGO in the LAST 12 MONTHS? Attending groups of mutual support` == "Yes" |
      `D6_6 What help or services did you receive from NGO in the LAST 12 MONTHS? Attending consultations on HIV/AIDS, sexually transmitted diseases, and ways of their prevention` == "Yes" |
      `D6_7 What help or services did you receive from NGO in the LAST 12 MONTHS? Attending consultations on safe injecting drug use` == "Yes" |
      `D6_8 What help or services did you receive from NGO in the LAST 12 MONTHS?  Peer-to-peer consultations` == "Yes" |
      `D6_9 What help or services did you receive from NGO in the LAST 12 MONTHS? Psychologist’s consultations` == "Yes" |
      `D6_10 What help or services did you receive from NGO in the LAST 12 MONTHS?  Lawyer’s consultations` == "Yes" |
      `D6_11 What help or services did you receive from NGO in the LAST 12 MONTHS?  Helpline service` == "Yes" |
      `D6_12 What help or services did you receive from NGO in the LAST 12 MONTHS?  Syringe exchange` == "Yes" |
      `D6_13 What help or services did you receive from NGO in the LAST 12 MONTHS?  Obtaining disinfecting solutions` == "Yes" |
      `D6_14 What help or services did you receive from NGO in the LAST 12 MONTHS?  Obtaining information booklets or brochures` == "Yes" |
      `D6_15 What help or services did you receive from NGO in the LAST 12 MONTHS? Obtaining hygienic means` == "Yes" |
      `D6_16 What help or services did you receive from NGO in the LAST 12 MONTHS?  Obtaining condoms` == "Yes" |
      `D6_17 What help or services did you receive from NGO in the LAST 12 MONTHS? Other` == "Yes" ~ "Yes",
      
      `D6_18 What help or services did you receive from NGO in the LAST 12 MONTHS?  Did not receive help or services` == "Yes" ~ "No",
      
      TRUE ~ NA_character_
    )
  )

# 2015 archive 

# derived variables
sw_data_2015_clean <- sw_data_2015_clean %>%
  mutate(
    residence_30d = case_when(
      `А9_1 What has been your permanent place of residence in the last month (30 days) - My own apartment` == "Yes" ~ "Own apartment",
      `А9_2 What has been your permanent place of residence in the last month (30 days) - The apartment of my relatives / friends (do not pay rent)` == "Yes" ~ "Relatives/friends apartment",
      `А9_3 What has been your permanent place of residence in the last month (30 days) - Rented apartment (rent alone or together with someone)` == "Yes" ~ "Rented apartment",
      `А9_4 What has been your permanent place of residence in the last month (30 days) - Hostel` == "Yes" ~ "Hostel",
      `А9_5 What has been your permanent place of residence in the last month (30 days) - Shelter, children’s home, boarding house` == "Yes" ~ "Shelter/boarding house",
      `А9_6 What has been your permanent place of residence in the last month (30 days) - Nowhere to live (frequent change of the place of residence)` == "Yes" ~ "Unstable housing",
      `А9_7 What has been your permanent place of residence in the last month (30 days) - Street, abandoned apartments,basement or attic, railway stations (homeless)` == "Yes" ~ "Homeless",
      `А9_8 What has been your permanent place of residence in the last month (30 days) - Other` == "Yes" ~ "Other",
      TRUE ~ NA_character_
    ),
    partners_sw_30d = as.numeric(`Number of irregular clients from whom you RECEIVED COMPENSATION [money or other] for providing sexual services in the LAST 30 DAYS.`) + 
                      as.numeric(`Number of regular clients from whom you RECEIVED COMPENSATION [money or other] for providing sexual services in the LAST 30 DAYS.`),
    partners_nonsw_30d = as.numeric(`Number of regular sexual partners from whom you did NOT RECEIVE COMPENSATION [money or other] in the LAST 30 DAYS.`) + 
                        as.numeric(`Number of casual sexual partners from whom you did NOT RECEIVE COMPENSATION [money or other] in the LAST 30 DAYS.`),                      
    client_condom_bin_30d = case_when(
    `В9.1. Please think of the last 30 DAYS once again. Was there a case when you DID NOT use a condom with your client during vaginal sex?` == "Yes, there was such a case" |
    `В8.1. Please think of the last 30 DAYS once again. Was there a case when you DID NOT use a condom with your client during anal sex?` == "Yes, there was such a case" ~ "No",
    
    `В9.1. Please think of the last 30 DAYS once again. Was there a case when you DID NOT use a condom with your client during vaginal sex?` == "Always used" &
    `В8.1. Please think of the last 30 DAYS once again. Was there a case when you DID NOT use a condom with your client during anal sex?` == "Always used" ~ "Yes",
    
    TRUE ~ NA_character_
    ),
    client_condom_freq_30d = case_when(
    `B9. How often have you used a condom during vaginal sex?` == "Always (100%)" &
    `B10. How often have you used a condom during anal sex?` == "Always (100%)" ~ "Always (100%)",
    
    `B9. How often have you used a condom during vaginal sex?` == "Always (100%)" &
    `B10. How often have you used a condom during anal sex?` == "I didn't have such sex" ~ "Always (100%)",
    
    `B9. How often have you used a condom during vaginal sex?` == "I didn't have such sex" &
    `B10. How often have you used a condom during anal sex?` == "Always (100%)" ~ "Always (100%)",
    
    `B9. How often have you used a condom during vaginal sex?` %in% c("In the majority of cases (75%)", "Always (100%)") &
    `B10. How often have you used a condom during anal sex?` %in% c("In the majority of cases (75%)", "Always (100%)", "I didn't have such sex") ~ "In the majority of cases (75%)",
    
    `B9. How often have you used a condom during vaginal sex?` %in% c("In half of cases (50%)", "In the majority of cases (75%)", "Always (100%)") &
    `B10. How often have you used a condom during anal sex?` %in% c("In half of cases (50%)", "In the majority of cases (75%)", "Always (100%)", "I didn't have such sex") ~ "In half of cases (50%)",
    
    `B9. How often have you used a condom during vaginal sex?` %in% c("Sometimes (25%)", "In half of cases (50%)", "In the majority of cases (75%)", "Always (100%)") &
    `B10. How often have you used a condom during anal sex?` %in% c("Sometimes (25%)", "In half of cases (50%)", "In the majority of cases (75%)", "Always (100%)", "I didn't have such sex") ~ "Sometimes (25%)",
    
    `B9. How often have you used a condom during vaginal sex?` != "Never" &
    `B10. How often have you used a condom during anal sex?` != "Never" &
    (`B9. How often have you used a condom during vaginal sex?` == "Rarely (less than 10%)" |
    `B10. How often have you used a condom during anal sex?` == "Rarely (less than 10%)") ~ "Rarely (less than 10%)",
    
    `B9. How often have you used a condom during vaginal sex?` == "Never" |
    `B10. How often have you used a condom during anal sex?` == "Never" ~ "Never",
    
    TRUE ~ NA_character_
  ),
    alcohol_30d_daily_bin = case_when(
      alcohol_30d_num >= 30 ~ "Yes",
      alcohol_30d_num < 30 ~ "No",
      TRUE ~ NA_character_
    ),
    alcohol_30d_bin = case_when(
      alcohol_30d_num >= 1 ~ "Yes",
      alcohol_30d_num == 0 ~ "No",
      TRUE ~ NA_character_
    ),
    violence_rape_ever = case_when(
      `F2.1_7 If “yes”, how? - Raped` == "Yes" |
      `F2.1_8 If “yes”, how? - Forced to provide sexual services in the form of perversion` == "Yes" ~ "Yes",
      
      `F2.1_7 If “yes”, how? - Raped` == "No" &
      `F2.1_8 If “yes”, how? - Forced to provide sexual services in the form of perversion` == "No" ~ "No",
      
      `F2.1_7 If “yes”, how? - Raped` == "No question asked" &
      `F2.1_8 If “yes”, how? - Forced to provide sexual services in the form of perversion` == "No question asked" ~ "No question asked",
      
      TRUE ~ NA_character_
    ),
    drugs_30d_bin = case_when(
      `C2_1 Have you used drugs non-injectably (smoked, sniffed, swallowed, etc.) in the past 30 days (last month)?` == "Yes" |
      `idu_30d_bin` == "Yes" ~ "Yes",
      
      `C2_1 Have you used drugs non-injectably (smoked, sniffed, swallowed, etc.) in the past 30 days (last month)?` == "No" &
      `idu_30d_bin` == "No" ~ "No",
      
      `C2_1 Have you used drugs non-injectably (smoked, sniffed, swallowed, etc.) in the past 30 days (last month)?` == "No question asked" &
      `idu_30d_bin` == "No question asked" ~ "No question asked",
      
      TRUE ~ NA_character_
    ),
    drugs_30d_num = as.numeric(`C2_1_1 Have you used drugs non-injectably in the past 30 days (last month)? If yes, how many times?`) + 
                as.numeric(idu_30d_num),
   sex_with_drugs_30d = case_when(
      `С5_2 How often did you use the following during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration? Narcotic substances` == "Never" ~ "No",
      `С5_2 How often did you use the following during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration? Narcotic substances` %in% c("Always (100%)", "In the majority of cases (75%)", "In half of cases (50%)", "Sometimes (25%)", "Rarely (less than 10%)") ~ "Yes",
      `С5_2 How often did you use the following during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration? Narcotic substances` == "Did not use in the last 30 days" ~ "Did not use drugs",
      TRUE ~ NA_character_
    ),
    sex_with_alcohol_30d = case_when(
      `С5_1 How often did you use the following during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration? Alcohol` == "Never" ~ "No",
      `С5_1 How often did you use the following during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration? Alcohol` %in% c("Always (100%)", "In the majority of cases (75%)", "In half of cases (50%)", "Sometimes (25%)", "Rarely (less than 10%)") ~ "Yes",
      `С5_1 How often did you use the following during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration? Alcohol` == "Did not use in the last 30 days" ~ "Did not use alcohol",
      TRUE ~ NA_character_
    ),
    sex_with_drugs_and_alcohol_30d = case_when(
      `С5_3 How often did you use the following during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration? Alcohol + drugs` == "Never" ~ "No",
      `С5_3 How often did you use the following during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration? Alcohol + drugs` %in% c("Always (100%)", "In the majority of cases (75%)", "In half of cases (50%)", "Sometimes (25%)", "Rarely (less than 10%)") ~ "Yes",
      `С5_3 How often did you use the following during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration? Alcohol + drugs` == "Did not use in the last 30 days" ~ "Did not use substances",
      TRUE ~ NA_character_
    ),
     violence_support_any = case_when(
      violence_support_ngo == "Yes" |
      `F3_2 Have you addressed any where or to anyone for help? - To relatives (parents, husband/cohabitant, friend)` == "Yes" |
      `F3_3 Have you addressed any where or to anyone for help? - To other client, whom I provide sexual services` == "Yes" |
      `F3_4 Have you addressed any where or to anyone for help? - To police` == "Yes" |
      `F3_5 Have you addressed any where or to anyone for help? - To other girl/woman who provide sexual services` == "Yes" |
      `F3_6 Have you addressed any where or to anyone for help? - To pimp/”mom”` == "Yes" |
      `F3_7 Have you addressed any where or to anyone for help? - Other` == "Yes" ~ "Yes",
      
      `F3_8 Have you addressed any where or to anyone for help? - Did not address for help` == "Yes" ~ "No",
      
      `F3_9 Have you addressed any where or to anyone for help? - Difficult to answer/refusal to answer` == "Yes" ~ "Refuse to answer",
      
      TRUE ~ NA_character_
    ),             
    primary_drug_30m = case_when(
      `С4.1_1 Which of the injecting drugs do you consider a primary one for you? - Hanka` == "Yes" ~ "Hanka",
      `С4.1_2 Which of the injecting drugs do you consider a primary one for you? - Pervitin` == "Yes" ~ "Pervitin",
      `С4.1_3 Which of the injecting drugs do you consider a primary one for you? - Stimulants` == "Yes" ~ "Stimulants",
      `С4.1_4 Which of the injecting drugs do you consider a primary one for you? - Methadone` == "Yes" ~ "Methadone",
      `С4.1_5 Which of the injecting drugs do you consider a primary one for you? - Nalbuphine` == "Yes" ~ "Nalbuphine",
      `С4.1_6 Which of the injecting drugs do you consider a primary one for you? - Salt` == "Yes" ~ "Salt",
      `С4.1_7 Which of the injecting drugs do you consider a primary one for you? - Opium` == "Yes" ~ "Opium",
      `С4.1_8 Which of the injecting drugs do you consider a primary one for you? - Diphenhydramine` == "Yes" ~ "Diphenhydramine",
      `С4.1_9 Which of the injecting drugs do you consider a primary one for you? - Amphetamine` == "Yes" ~ "Amphetamine",
      `С4.1_10 Which of the injecting drugs do you consider a primary one for you? - Subutex` == "Yes" ~ "Subutex",
      `С4.1_11 Which of the injecting drugs do you consider a primary one for you? - Methamphetamine (crystal)` == "Yes" ~ "Methamphetamine (crystal)",
      
      # If all are "No", then no primary drug
      if_all(c(
        `С4.1_1 Which of the injecting drugs do you consider a primary one for you? - Hanka`,
        `С4.1_2 Which of the injecting drugs do you consider a primary one for you? - Pervitin`,
        `С4.1_3 Which of the injecting drugs do you consider a primary one for you? - Stimulants`,
        `С4.1_4 Which of the injecting drugs do you consider a primary one for you? - Methadone`,
        `С4.1_5 Which of the injecting drugs do you consider a primary one for you? - Nalbuphine`,
        `С4.1_6 Which of the injecting drugs do you consider a primary one for you? - Salt`,
        `С4.1_7 Which of the injecting drugs do you consider a primary one for you? - Opium`,
        `С4.1_8 Which of the injecting drugs do you consider a primary one for you? - Diphenhydramine`,
        `С4.1_9 Which of the injecting drugs do you consider a primary one for you? - Amphetamine`,
        `С4.1_10 Which of the injecting drugs do you consider a primary one for you? - Subutex`,
        `С4.1_11 Which of the injecting drugs do you consider a primary one for you? - Methamphetamine (crystal)`
      ), ~ . == "No") ~ "No drug use",
      
      # If all are "No question asked"
      if_all(c(
        `С4.1_1 Which of the injecting drugs do you consider a primary one for you? - Hanka`,
        `С4.1_2 Which of the injecting drugs do you consider a primary one for you? - Pervitin`,
        `С4.1_3 Which of the injecting drugs do you consider a primary one for you? - Stimulants`,
        `С4.1_4 Which of the injecting drugs do you consider a primary one for you? - Methadone`,
        `С4.1_5 Which of the injecting drugs do you consider a primary one for you? - Nalbuphine`,
        `С4.1_6 Which of the injecting drugs do you consider a primary one for you? - Salt`,
        `С4.1_7 Which of the injecting drugs do you consider a primary one for you? - Opium`,
        `С4.1_8 Which of the injecting drugs do you consider a primary one for you? - Diphenhydramine`,
        `С4.1_9 Which of the injecting drugs do you consider a primary one for you? - Amphetamine`,
        `С4.1_10 Which of the injecting drugs do you consider a primary one for you? - Subutex`,
        `С4.1_11 Which of the injecting drugs do you consider a primary one for you? - Methamphetamine (crystal)`
      ), ~ . == "No question asked") ~ "No question asked",
      
      TRUE ~ NA_character_
    )
)

# 2017 archive


# derived variables
sw_data_2017_clean <- sw_data_2017_clean %>%
  mutate(
  age = dplyr::if_else(
    is.na(`Interview date`) | is.na(`А6. Specify your year of birth`),
    NA_integer_,
    as.integer(format(as.Date(`Interview date`, format = "%d/%m/%Y"), "%Y")) -
      as.integer(`А6. Specify your year of birth`)
  ),
  partners_sw_30d = case_when(
  is.na(`B4_1_1 Number in the PAST MONTH (30 days) Permanent clients from which you RECEIVED A FEE [money or other] for sexual services`) & 
  is.na(`B4_2_1 Number in the PAST MONTH (30 days) Casual clients from which you RECEIVED FEE [money or other]`) ~ NA_real_,
  
  TRUE ~ (`B4_1_1 Number in the PAST MONTH (30 days) Permanent clients from which you RECEIVED A FEE [money or other] for sexual services` %||% 0) + 
         (`B4_2_1 Number in the PAST MONTH (30 days) Casual clients from which you RECEIVED FEE [money or other]` %||% 0)
  ),
  partners_nonsw_30d = case_when(
    is.na(`B4_3_1 Number in the PAST MONTH (30 days) Permanent partners from which you RECEIVED NO FEE [money or other])`) & 
    is.na(`B4_4_1 Number in the PAST MONTH (30 days) Casual partners from which you RECEIVED NO FEE [money or other]`) ~ NA_real_,
    
    TRUE ~ (`B4_3_1 Number in the PAST MONTH (30 days) Permanent partners from which you RECEIVED NO FEE [money or other])` %||% 0) + 
          (`B4_4_1 Number in the PAST MONTH (30 days) Casual partners from which you RECEIVED NO FEE [money or other]` %||% 0)
  ),
  residence_90d = case_when(
    `А11_1 What has been your permanent place of residence in the last 3 months (90 days) - My own apartment` == "Yes" ~ "Own apartment",
    `А11_2 What has been your permanent place of residence in the last 3 months (90 days)  - The apartment of my relatives / friends (do not pay rent)` == "Yes" ~ "Relatives/friends apartment",
    `А11_3 What has been your permanent place of residence in the last 3 months (90 days)  - Rented apartment (rent alone or together with someone)` == "Yes" ~ "Rented apartment",
    `А11_4 What has been your permanent place of residence in the last 3 months (90 days)  - Hostel` == "Yes" ~ "Hostel",
    `А11_5 What has been your permanent place of residence in the last 3 months (90 days)  - Shelter, children’s home, boarding house` == "Yes" ~ "Shelter/boarding house",
    `А11_6 What has been your permanent place of residence in the last 3 months (90 days)  - Nowhere to live (frequent change of the place of residence)` == "Yes" ~ "Unstable housing",
    `А11_7 What has been your permanent place of residence in the last 3 months (90 days)  - Street, abandoned apartments,basement or attic, railway stations (homeless)` == "Yes" ~ "Homeless",
    `А11_8 What has been your permanent place of residence in the last 3 months (90 days)  - Other without information` == "Yes" ~ "Other",
    `А11_9 What has been your permanent place of residence in the last 3 months (90 days)  - Just released from incarceration (Other)` == "Yes" ~ "Released from incarceration",
    TRUE ~ NA_character_
  ),
  client_condom_bin_30d = dplyr::case_when(
    `B16. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during vaginal sex?` == "Always (100%)" &
    `B17. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during anal  sex?` == "Always (100%)" ~ "Yes",

    `B16. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during vaginal sex?` == "Always (100%)" &
    `B17. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during anal  sex?` == "No such contact" ~ "Yes",

    `B16. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during vaginal sex?` == "No such contact" &
    `B17. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during anal  sex?` == "Always (100%)" ~ "Yes",

    `B16. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during vaginal sex?` == "No such contact" &
    `B17. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during anal  sex?` == "No such contact" ~ "No such contact",

    TRUE ~ "No"
  ),
  client_condom_freq_30d = case_when(
    `B16. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during vaginal sex?` == "Always (100%)" &
    `B17. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during anal  sex?` == "Always (100%)" ~ "Always (100%)",
    
    `B16. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during vaginal sex?` == "Always (100%)" &
    `B17. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during anal  sex?` == "No such contact" ~ "Always (100%)",
    
    `B16. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during vaginal sex?` == "No such contact" &
    `B17. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during anal  sex?` == "Always (100%)" ~ "Always (100%)",
    
    `B16. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during vaginal sex?` %in% c("In the majority of cases (75%)", "Always (100%)") &
    `B17. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during anal  sex?` %in% c("In the majority of cases (75%)", "Always (100%)", "No such contact") ~ "In the majority of cases (75%)",
    
    `B16. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during vaginal sex?` %in% c("In half of cases (50%)", "In the majority of cases (75%)", "Always (100%)") &
    `B17. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during anal  sex?` %in% c("In half of cases (50%)", "In the majority of cases (75%)", "Always (100%)", "No such contact") ~ "In half of cases (50%)",
    
    `B16. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during vaginal sex?` %in% c("Sometimes (25%)", "In half of cases (50%)", "In the majority of cases (75%)", "Always (100%)") &
    `B17. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during anal  sex?` %in% c("Sometimes (25%)", "In half of cases (50%)", "In the majority of cases (75%)", "Always (100%)", "No such contact") ~ "Sometimes (25%)",
    
    `B16. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during vaginal sex?` != "Never" &
    `B17. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during anal  sex?` != "Never" &
    (`B16. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during vaginal sex?` == "Rarely (less than 10%)" |
    `B17. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during anal  sex?` == "Rarely (less than 10%)") ~ "Rarely (less than 10%)",
    
    `B16. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during vaginal sex?` == "Never" |
    `B17. Remember all your sexual contacts with CLIENTS from whom you have RECEIVED REMUNERATION for the LAST [30 days]. How often have you used a condom during anal  sex?` == "Never" ~ "Never",
    
    TRUE ~ NA_character_
  ),
  alcohol_30d_bin = case_when(
    alcohol_30d_num >= 1 ~ "Yes",
    alcohol_30d_num == 0 ~ "No",
    TRUE ~ NA_character_
  ),
  alcohol_30d_daily_bin = case_when(
    alcohol_30d_num >= 28 ~ "Yes",
    alcohol_30d_num < 28 ~ "No",
    TRUE ~ NA_character_
  ),
  primary_drug_30d = case_when(
    `С5_1 Which of the injecting drugs have you used most often during last 12 months? - Amphetamine in powder form (“fen”)` == "Yes" ~ "Amphetamine in powder form (“fen”)",
    `С5_2 Which of the injecting drugs have you used most often during last 12 months? - Liquid opium extract (“shyrka”, “chorna”)` == "Yes" ~ "Liquid opium extract (“shyrka”, “chorna”)",
    `С5_3 Which of the injecting drugs have you used most often during last 12 months? - Heroin` == "Yes" ~ "Heroin",
    `С5_4 Which of the injecting drugs have you used most often during last 12 months? - Methadone` == "Yes" ~ "Methadone",
    `С5_5 Which of the injecting drugs have you used most often during last 12 months? - Subitex` == "Yes" ~ "Subitex",
    `С5_6 Which of the injecting drugs have you used most often during last 12 months? - Buprenorphine` == "Yes" ~ "Buprenorphine",
    `С5_7 Which of the injecting drugs have you used most often during last 12 months? - Methamphetamine solution (“vint”, “pervintin”)` == "Yes" ~ "Methamphetamine solution (“vint”, “pervintin”)",
    `С5_8 Which of the injecting drugs have you used most often during last 12 months? - Desomorphine` == "Yes" ~ "Desomorphine",
    `С5_9 Which of the injecting drugs have you used most often during last 12 months? - Poppy (poppy straw, seeds)` == "Yes" ~ "Poppy (poppy straw, seeds)",
    `С5_10 Which of the injecting drugs have you used most often during last 12 months? - Opiates` == "Yes" ~ "Opiates",
    `С5_11 Which of the injecting drugs have you used most often during last 12 months? - Bath salt` == "Yes" ~ "Bath salt",
    `С5_12 Which of the injecting drugs have you used most often during last 12 months? - Methylenedioxymethamphetamine (ecstasy, MDMA)` == "Yes" ~ "Methylenedioxymethamphetamine (ecstasy, MDMA)",
    `С5_13 Which of the injecting drugs have you used most often during last 12 months? - Marijuana (hashish, weed)` == "Yes" ~ "Marijuana (hashish, weed)",
    `С5_14 Which of the injecting drugs have you used most often during last 12 months? - Stimulants` == "Yes" ~ "Stimulants",
    `С5_15 Which of the injecting drugs have you used most often during last 12 months? - Nalbuphine` == "Yes" ~ "Nalbuphine",
    `С5_16 Which of the injecting drugs have you used most often during last 12 months? - Diphenhydramine` == "Yes" ~ "Diphenhydramine",
    `С5_17 Which of the injecting drugs have you used most often during last 12 months? -  Difficult to answer / don’t remember` == "Yes" ~ "Difficult to answer / don’t remember",
    `С5_18 Which of the injecting drugs have you used most often during last 12 months? - Pharmaceutical drugs` == "Yes" ~ "Pharmaceutical drugs",

    if_all(c(
      `С5_1 Which of the injecting drugs have you used most often during last 12 months? - Amphetamine in powder form (“fen”)`,
      `С5_2 Which of the injecting drugs have you used most often during last 12 months? - Liquid opium extract (“shyrka”, “chorna”)`,
      `С5_3 Which of the injecting drugs have you used most often during last 12 months? - Heroin`,
      `С5_4 Which of the injecting drugs have you used most often during last 12 months? - Methadone`,
      `С5_5 Which of the injecting drugs have you used most often during last 12 months? - Subitex`,
      `С5_6 Which of the injecting drugs have you used most often during last 12 months? - Buprenorphine`,
      `С5_7 Which of the injecting drugs have you used most often during last 12 months? - Methamphetamine solution (“vint”, “pervintin”)`,
      `С5_8 Which of the injecting drugs have you used most often during last 12 months? - Desomorphine`,
      `С5_9 Which of the injecting drugs have you used most often during last 12 months? - Poppy (poppy straw, seeds)`,
      `С5_10 Which of the injecting drugs have you used most often during last 12 months? - Opiates`,
      `С5_11 Which of the injecting drugs have you used most often during last 12 months? - Bath salt`,
      `С5_12 Which of the injecting drugs have you used most often during last 12 months? - Methylenedioxymethamphetamine (ecstasy, MDMA)`,
      `С5_13 Which of the injecting drugs have you used most often during last 12 months? - Marijuana (hashish, weed)`,
      `С5_14 Which of the injecting drugs have you used most often during last 12 months? - Stimulants`,
      `С5_15 Which of the injecting drugs have you used most often during last 12 months? - Nalbuphine`,
      `С5_16 Which of the injecting drugs have you used most often during last 12 months? - Diphenhydramine`,
      `С5_17 Which of the injecting drugs have you used most often during last 12 months? -  Difficult to answer / don’t remember`,
      `С5_18 Which of the injecting drugs have you used most often during last 12 months? - Pharmaceutical drugs`
    ), ~ . == "No") ~ "No drug use",

    if_all(c(
      `С5_1 Which of the injecting drugs have you used most often during last 12 months? - Amphetamine in powder form (“fen”)`,
      `С5_2 Which of the injecting drugs have you used most often during last 12 months? - Liquid opium extract (“shyrka”, “chorna”)`,
      `С5_3 Which of the injecting drugs have you used most often during last 12 months? - Heroin`,
      `С5_4 Which of the injecting drugs have you used most often during last 12 months? - Methadone`,
      `С5_5 Which of the injecting drugs have you used most often during last 12 months? - Subitex`,
      `С5_6 Which of the injecting drugs have you used most often during last 12 months? - Buprenorphine`,
      `С5_7 Which of the injecting drugs have you used most often during last 12 months? - Methamphetamine solution (“vint”, “pervintin”)`,
      `С5_8 Which of the injecting drugs have you used most often during last 12 months? - Desomorphine`,
      `С5_9 Which of the injecting drugs have you used most often during last 12 months? - Poppy (poppy straw, seeds)`,
      `С5_10 Which of the injecting drugs have you used most often during last 12 months? - Opiates`,
      `С5_11 Which of the injecting drugs have you used most often during last 12 months? - Bath salt`,
      `С5_12 Which of the injecting drugs have you used most often during last 12 months? - Methylenedioxymethamphetamine (ecstasy, MDMA)`,
      `С5_13 Which of the injecting drugs have you used most often during last 12 months? - Marijuana (hashish, weed)`,
      `С5_14 Which of the injecting drugs have you used most often during last 12 months? - Stimulants`,
      `С5_15 Which of the injecting drugs have you used most often during last 12 months? - Nalbuphine`,
      `С5_16 Which of the injecting drugs have you used most often during last 12 months? - Diphenhydramine`,
      `С5_17 Which of the injecting drugs have you used most often during last 12 months? -  Difficult to answer / don’t remember`,
      `С5_18 Which of the injecting drugs have you used most often during last 12 months? - Pharmaceutical drugs`
    ), ~ . == "No question asked") ~ "No question asked",
    TRUE ~ NA_character_
  ),
  sex_with_drugs_30d = case_when(
    `С7_2 How often did you use the Narcotic substances during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration?` == "Never" ~ "No",
    `С7_2 How often did you use the Narcotic substances during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration?` %in% c("Always (100%)", "In the majority of cases (75%)", "In half of cases (50%)", "Sometimes (25%)", "Rarely (less than 10%)") ~ "Yes",
    `С7_2 How often did you use the Narcotic substances during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration?` == "Didn’t use during last 30 days" ~ "Did not use drugs",
    TRUE ~ NA_character_
  ),

  sex_with_alcohol_30d = case_when(
    `С7_1 How often did you use the Alcohol during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration?` == "Never" ~ "No",
    `С7_1 How often did you use the Alcohol during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration?` %in% c("Always (100%)", "In the majority of cases (75%)", "In half of cases (50%)", "Sometimes (25%)", "Rarely (less than 10%)") ~ "Yes",
    `С7_1 How often did you use the Alcohol during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration?` == "Didn’t use during last 30 days" ~ "Did not use alcohol",
    TRUE ~ NA_character_
  ),

  sex_with_drugs_and_alcohol_30d = case_when(
    `С7_2 How often did you use the Alcohol + drugs during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration?` == "Never" ~ "No",
    `С7_2 How often did you use the Alcohol + drugs during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration?` %in% c("Always (100%)", "In the majority of cases (75%)", "In half of cases (50%)", "Sometimes (25%)", "Rarely (less than 10%)") ~ "Yes",
    `С7_2 How often did you use the Alcohol + drugs during the LAST MONTH [30 days] before a sexual contact(s) with your client(s) from whom you received the remuneration?` == "Didn’t use during last 30 days" ~ "Did not use substances",
    TRUE ~ NA_character_
  ),
  ngo_access_6m = case_when(
    `F5_1 Have you received condoms from representatives of this NGO during the last 6 months? - Male condoms` == "Yes" |
    `F5_2 Have you received condoms from representatives of this NGO during the last 6 months? - Female condoms` == "Yes" ~ "Yes",

    `F5_1 Have you received condoms from representatives of this NGO during the last 6 months? - Male condoms` == "No" &
    `F5_2 Have you received condoms from representatives of this NGO during the last 6 months? - Female condoms` == "No" ~ "No",

    `F5_1 Have you received condoms from representatives of this NGO during the last 6 months? - Male condoms` == "No question asked" &
    `F5_2 Have you received condoms from representatives of this NGO during the last 6 months? - Female condoms` == "No question asked" ~ "No question asked",

    TRUE ~ NA_character_
  ),
  violence_rape_ever = case_when(
    `H1_6 If “yes”, how?  - Raped` == "Yes" |
    `H1_7 If “yes”, how?  - Forced to provide sexual services in the form of perversion` == "Yes" ~ "Yes",

    `H1_6 If “yes”, how?  - Raped` == "No" &
    `H1_7 If “yes”, how?  - Forced to provide sexual services in the form of perversion` == "No" ~ "No",

    `H1_6 If “yes”, how?  - Raped` == "No question asked" &
    `H1_7 If “yes”, how?  - Forced to provide sexual services in the form of perversion` == "No question asked" ~ "No question asked",

    TRUE ~ NA_character_
  ),
  violence_support = case_when(

    if_any(c(
      violence_support_ngo,
      `H3_2 Have you addressed anywhere or to anyone for help? - To relatives (parents, husband/cohabitant, friend)`,
      `H3_3 Have you addressed anywhere or to anyone for help? - To other client, whom I provide sexual services`,
      `H3_4 Have you addressed anywhere or to anyone for help? - To police`,
      `H3_5 Have you addressed anywhere or to anyone for help? - To other girl/woman who provide sexual services`,
      `H3_6 Have you addressed anywhere or to anyone for help? - To pimp/”mom”`,
      `H3_7 Have you addressed anywhere or to anyone for help? - Other`
    ), ~ . == "Yes") ~ "Yes",

    `H3_8 Have you addressed anywhere or to anyone for help? - Did not address for help` == "Yes" ~ "No",
    if_any(c(
      `H3_9 Have you addressed anywhere or to anyone for help? - Difficult to answer`,
      `H3_10 Have you addressed anywhere or to anyone for help? - Refusal to answer`
    ), ~ . == "Yes") ~ NA_character_,
   
    if_all(c(
      violence_support_ngo,
      `H3_2 Have you addressed anywhere or to anyone for help? - To relatives (parents, husband/cohabitant, friend)`,
      `H3_3 Have you addressed anywhere or to anyone for help? - To other client, whom I provide sexual services`,
      `H3_4 Have you addressed anywhere or to anyone for help? - To police`,
      `H3_5 Have you addressed anywhere or to anyone for help? - To other girl/woman who provide sexual services`,
      `H3_6 Have you addressed anywhere or to anyone for help? - To pimp/”mom”`,
      `H3_7 Have you addressed anywhere or to anyone for help? - Other`,
      `H3_8 Have you addressed anywhere or to anyone for help? - Did not address for help`
    ), ~ . == "No") ~ "No",

    TRUE ~ NA_character_
  )
)


## 2021 archive


# derived variables
sw_data_2021_clean <- sw_data_2021_clean %>%
  mutate(
  partners_sw_30d = case_when(
    is.na(`b2_1_1 How many partners with whom you had sexual contact within the last 30 days belonged to Regular client`) &
    is.na(`b2_2_1 How many partners with whom you had sexual contact within the last 30 days belonged to Non-regular client`) ~ NA_real_,
    TRUE ~
      coalesce(as.numeric(`b2_1_1 How many partners with whom you had sexual contact within the last 30 days belonged to Regular client`), 0) +
      coalesce(as.numeric(`b2_2_1 How many partners with whom you had sexual contact within the last 30 days belonged to Non-regular client`), 0)
  ),

  partners_nonsw_30d = case_when(
    is.na(`b2_3_1 How many partners with whom you had sexual contact within the last 30 days belonged to Permanent sexual partner`) &
    is.na(`b2_4_1 How many partners with whom you had sexual contact within the last 30 days belonged to Casual sexual partner`) ~ NA_real_,
    TRUE ~
      coalesce(as.numeric(`b2_3_1 How many partners with whom you had sexual contact within the last 30 days belonged to Permanent sexual partner`), 0) +
      coalesce(as.numeric(`b2_4_1 How many partners with whom you had sexual contact within the last 30 days belonged to Casual sexual partner`), 0)
  ),
  client_condom_bin_30d = case_when(
    `b12 During VAGINAL sex during the last 30 days?` == "Always (100%)" &
    `b13 And during ANAL sex during the last 30 days?` == "Always (100%)" ~ "Yes",

    `b12 During VAGINAL sex during the last 30 days?` == "Always (100%)" &
    `b13 And during ANAL sex during the last 30 days?` == "No such contacts" ~ "Yes",

    `b12 During VAGINAL sex during the last 30 days?` == "No such contacts" &
    `b13 And during ANAL sex during the last 30 days?` == "Always (100%)" ~ "Yes",

    `b12 During VAGINAL sex during the last 30 days?` == "No such contacts" &
    `b13 And during ANAL sex during the last 30 days?` == "No such contacts" ~ "No such contacts",

    TRUE ~ "No"
  ),
  client_condom_freq_30d = case_when(
    `b12 During VAGINAL sex during the last 30 days?` == "Always (100%)" &
    `b13 And during ANAL sex during the last 30 days?` == "Always (100%)" ~ "Always (100%)",
    
    `b12 During VAGINAL sex during the last 30 days?` == "Always (100%)" &
    `b13 And during ANAL sex during the last 30 days?` == "No such contacts" ~ "Always (100%)",
    
    `b12 During VAGINAL sex during the last 30 days?` == "No such contacts" &
    `b13 And during ANAL sex during the last 30 days?` == "Always (100%)" ~ "Always (100%)",
    
    `b12 During VAGINAL sex during the last 30 days?` %in% c("In most cases (75%)", "Always (100%)") &
    `b13 And during ANAL sex during the last 30 days?` %in% c("In most cases (75%)", "Always (100%)", "No such contacts") ~ "In most cases (75%)",
    
    `b12 During VAGINAL sex during the last 30 days?` %in% c("In half of the cases (50%)", "In most cases (75%)", "Always (100%)") &
    `b13 And during ANAL sex during the last 30 days?` %in% c("In half of the cases (50%)", "In most cases (75%)", "Always (100%)", "No such contacts") ~ "In half of the cases (50%)",
    
    `b12 During VAGINAL sex during the last 30 days?` %in% c("Sometimes (25%)", "In half of the cases (50%)", "In most cases (75%)", "Always (100%)") &
    `b13 And during ANAL sex during the last 30 days?` %in% c("Sometimes (25%)", "In half of the cases (50%)", "In most cases (75%)", "Always (100%)", "No such contacts") ~ "Sometimes (25%)",
    
    `b12 During VAGINAL sex during the last 30 days?` != "Never" &
    `b13 And during ANAL sex during the last 30 days?` != "Never" &
    (`b12 During VAGINAL sex during the last 30 days?` == "Rare (10%)" |
    `b13 And during ANAL sex during the last 30 days?` == "Rare (10%)") ~ "Rare (10%)",
    
    `b12 During VAGINAL sex during the last 30 days?` == "Never" |
    `b13 And during ANAL sex during the last 30 days?` == "Never" ~ "Never",
    
    TRUE ~ NA_character_
  ),
   perm_partner_condom_lastsex = case_when(
    `b33 During VAGINAL sex during the last 30 days?` %in% c("No question asked", "Refusal to answer") |
      `b34 And during ANAL sex during the last 30 days?` %in% c("No question asked", "Refusal to answer") ~ "No question asked",
    `b33 During VAGINAL sex during the last 30 days?` == "No such contacts" &
      `b34 And during ANAL sex during the last 30 days?` == "No such contacts" ~ "No such contacts",
    `b33 During VAGINAL sex during the last 30 days?` %in% c("Never", "Rare (10%)", "Sometimes (25%)", "In half of the cases (50%)", "In most cases (75%)") |
      `b34 And during ANAL sex during the last 30 days?` %in% c("Never", "Rare (10%)", "Sometimes (25%)", "In half of the cases (50%)", "In most cases (75%)") ~ "No condom",
    `b33 During VAGINAL sex during the last 30 days?` %in% c("Always (100%)", "No such contacts") &
      `b34 And during ANAL sex during the last 30 days?` %in% c("Always (100%)", "No such contacts") ~ "Condom",
    TRUE ~ NA_character_
  ),
  cas_partner_condom_lastsex = case_when(
    `b39 During VAGINAL sex during the last 30 days?` %in% c("No question asked", "Refusal to answer") |
      `b40 And during ANAL sex during the last 30 days?` %in% c("No question asked", "Refusal to answer") ~ "No question asked",
    `b39 During VAGINAL sex during the last 30 days?` == "No such contacts" &
      `b40 And during ANAL sex during the last 30 days?` == "No such contacts" ~ "No such contacts",
    `b39 During VAGINAL sex during the last 30 days?` %in% c("Never", "Rare (10%)", "Sometimes (25%)", "In half of the cases (50%)", "In most cases (75%)") |
      `b40 And during ANAL sex during the last 30 days?` %in% c("Never", "Rare (10%)", "Sometimes (25%)", "In half of the cases (50%)", "In most cases (75%)") ~ "No condom",
    `b39 During VAGINAL sex during the last 30 days?` %in% c("Always (100%)", "No such contacts") &
      `b40 And during ANAL sex during the last 30 days?` %in% c("Always (100%)", "No such contacts") ~ "Condom",
    TRUE ~ NA_character_
  ),
  alcohol_30d_bin = case_when(
    `c1 How often do you have a drink containing alcohol?` %in% c("Never") ~ "No",
    `c1 How often do you have a drink containing alcohol?` %in% c("2-4 times a month", "2-3 times a week", "4 or more times a week", "Monthly or less") ~ "Yes",
    `c1 How often do you have a drink containing alcohol?` %in% c("Don't know/don't remember", "Refusal to answer") ~ "No answer",
    TRUE ~ NA_character_
  ),
  alcohol_30d_daily_bin = case_when(
    alcohol_30d_num %in% c("Used, don't remember the amount") ~ "don't remember",
    alcohol_30d_num %in% c("Refusal to answer") ~ "Refuse to answer",
    alcohol_30d_num %in% c("Not used in the last 30 days") ~ "No",
    suppressWarnings(as.numeric(alcohol_30d_num)) >= 28 ~ "Yes",
    suppressWarnings(as.numeric(alcohol_30d_num)) < 28 ~ "No",
    TRUE ~ NA_character_
  ),
  drugs_12m_bin = case_when(
    `c5 Some people try different drugs. Have you used any non-injectable drugs (smoked, sniffed, swallowed, etc.) or not?` %in% c(
      "Yes, I have been using them for the last 30 days",
      "Yes, I've been using them for the last 12 months (but not for 30 days)",
      "Так, я вживав/ла протягом останніх 12 місяців (але не протягом 30 днів)"
    ) |
    `c6 Have you used any injectable drugs (with a syringe) or not?` %in% c(
      "Yes, I have been using them for the last 30 days",
      "Yes, I've been using them for the last 12 months (but not for 30 days)",
      "Так, я вживав/ла протягом останніх 12 місяців (але не протягом 30 днів)"
    ) ~ "Yes",
    `c5 Some people try different drugs. Have you used any non-injectable drugs (smoked, sniffed, swallowed, etc.) or not?` %in% c("Don't know/don't remember", "Refusal to answer") &
    `c6 Have you used any injectable drugs (with a syringe) or not?` %in% c("Don't know/don't remember", "Refusal to answer") ~ "No answer",
    `c5 Some people try different drugs. Have you used any non-injectable drugs (smoked, sniffed, swallowed, etc.) or not?` %in% c("Never used or never even tried", "Yes, I used it, but more than 12 months ago") &
    `c6 Have you used any injectable drugs (with a syringe) or not?` %in% c("Never used or never even tried", "Yes, I used it, but more than 12 months ago", "Don't know/don't remember", "Refusal to answer") ~ "No",
    `c6 Have you used any injectable drugs (with a syringe) or not?` %in% c("Never used or never even tried", "Yes, I used it, but more than 12 months ago") &
    `c5 Some people try different drugs. Have you used any non-injectable drugs (smoked, sniffed, swallowed, etc.) or not?` %in% c("Never used or never even tried", "Yes, I used it, but more than 12 months ago", "Don't know/don't remember", "Refusal to answer") ~ "No",
    TRUE ~ NA_character_
  ),
  idu_12m_bin = case_when(
    `c6 Have you used any injectable drugs (with a syringe) or not?` %in% c(
      "Yes, I have been using them for the last 30 days",
      "Yes, I've been using them for the last 12 months (but not for 30 days)",
      "Так, я вживав/ла протягом останніх 12 місяців (але не протягом 30 днів)"
    ) ~ "Yes",
    `c6 Have you used any injectable drugs (with a syringe) or not?` %in% c(
      "Never used or never even tried",
      "Yes, I used it, but more than 12 months ago"
    ) ~ "No",
    `c6 Have you used any injectable drugs (with a syringe) or not?` %in% c(
      "Don't know/don't remember",
      "Refusal to answer"
    ) ~ "No answer",
    TRUE ~ NA_character_
  ),
  sex_with_alcohol_30d = case_when(
    `c12_1 How often in the last month (30 days) before sexual intercourse with the client, you used ALCOHOL` == "Never" ~ "No",
    `c12_1 How often in the last month (30 days) before sexual intercourse with the client, you used ALCOHOL` %in% c(
      "Always (100%)", "In most cases (75%)", "In half of the cases (50%)", "Sometimes (25%)", "Rarely (10%)"
    ) ~ "Yes",
    `c12_1 How often in the last month (30 days) before sexual intercourse with the client, you used ALCOHOL` == "Has not used for the last 30 days" ~ "Did not use alcohol",
    `c12_1 How often in the last month (30 days) before sexual intercourse with the client, you used ALCOHOL` == "Don't know/don't remember" ~ NA_character_,
    TRUE ~ NA_character_
  ),
  sex_with_drugs_30d = case_when(
    `c12_2 How often in the last month (30 days) before sexual intercourse with the client, you used DRUGS` == "Never" ~ "No",
    `c12_2 How often in the last month (30 days) before sexual intercourse with the client, you used DRUGS` %in% c(
      "Always (100%)", "In most cases (75%)", "In half of the cases (50%)", "Sometimes (25%)", "Rarely (10%)"
    ) ~ "Yes",
    `c12_2 How often in the last month (30 days) before sexual intercourse with the client, you used DRUGS` == "Has not used for the last 30 days" ~ "Did not use drugs",
    `c12_2 How often in the last month (30 days) before sexual intercourse with the client, you used DRUGS` == "Don't know/don't remember" ~ NA_character_,
    TRUE ~ NA_character_
  ),
  sex_with_drugs_and_alcohol_30d = case_when(
    `c12_3 How often in the last month (30 days) before sexual intercourse with the client, you used ALCOHOL+DRUGS` == "Never" ~ "No",
    `c12_3 How often in the last month (30 days) before sexual intercourse with the client, you used ALCOHOL+DRUGS` %in% c(
      "Always (100%)", "In most cases (75%)", "In half of the cases (50%)", "Sometimes (25%)", "Rarely (10%)"
    ) ~ "Yes",
    `c12_3 How often in the last month (30 days) before sexual intercourse with the client, you used ALCOHOL+DRUGS` == "Has not used for the last 30 days" ~ "Did not use substances",
    `c12_3 How often in the last month (30 days) before sexual intercourse with the client, you used ALCOHOL+DRUGS` == "Don't know/don't remember" ~ NA_character_,
    TRUE ~ NA_character_
  ),
  ngo_access_12m = case_when(
    if_any(
      c(
        `f6_1 Have you received following free items, from NGO or social worker within the last 12 months? - Male condoms`,
        `f6_2 Have you received following free items, from NGO or social worker within the last 12 months? - Female condoms`,
        `f6_3 Have you received following free items, from NGO or social worker within the last 12 months? - Lubricants`,
        `f6_4 Have you received following free items, from NGO or social worker within the last 12 months? - Social worker consultation`,
        `f6_5 Have you received following free items, from NGO or social worker within the last 12 months? - HIV testing`,
        `f6_6 Have you received following free items, from NGO or social worker within the last 12 months? - Hepatitis B testing`,
        `f6_7 Have you received following free items, from NGO or social worker within the last 12 months? - Hepatitis C testing`,
        `f6_8 Have you received following free items, from NGO or social worker within the last 12 months? - Syphilis testing`,
        `f6_9 Have you received following free items, from NGO or social worker within the last 12 months? - TB screening`,
        accessed_syringe_12m,
        `f6_11 Have you received following free items, from NGO or social worker within the last 12 months? - Information materials`,
        `f6_12 Have you received following free items, from NGO or social worker within the last 12 months? - Case management by a social worker`,
      ),
      ~ . == "Yes"
    ) ~ "Yes",
    if_all(
      c(
        `f6_1 Have you received following free items, from NGO or social worker within the last 12 months? - Male condoms`,
        `f6_2 Have you received following free items, from NGO or social worker within the last 12 months? - Female condoms`,
        `f6_3 Have you received following free items, from NGO or social worker within the last 12 months? - Lubricants`,
        `f6_4 Have you received following free items, from NGO or social worker within the last 12 months? - Social worker consultation`,
        `f6_5 Have you received following free items, from NGO or social worker within the last 12 months? - HIV testing`,
        `f6_6 Have you received following free items, from NGO or social worker within the last 12 months? - Hepatitis B testing`,
        `f6_7 Have you received following free items, from NGO or social worker within the last 12 months? - Hepatitis C testing`,
        `f6_8 Have you received following free items, from NGO or social worker within the last 12 months? - Syphilis testing`,
        `f6_9 Have you received following free items, from NGO or social worker within the last 12 months? - TB screening`,
        accessed_syringe_12m,
        `f6_11 Have you received following free items, from NGO or social worker within the last 12 months? - Information materials`,
        `f6_12 Have you received following free items, from NGO or social worker within the last 12 months? - Case management by a social worker`,
      ),
      ~ . %in% c("No", "Never")
    ) ~ "No",
    if_any(
      c(
        `f6_1 Have you received following free items, from NGO or social worker within the last 12 months? - Male condoms`,
        `f6_2 Have you received following free items, from NGO or social worker within the last 12 months? - Female condoms`,
        `f6_3 Have you received following free items, from NGO or social worker within the last 12 months? - Lubricants`,
        `f6_4 Have you received following free items, from NGO or social worker within the last 12 months? - Social worker consultation`,
        `f6_5 Have you received following free items, from NGO or social worker within the last 12 months? - HIV testing`,
        `f6_6 Have you received following free items, from NGO or social worker within the last 12 months? - Hepatitis B testing`,
        `f6_7 Have you received following free items, from NGO or social worker within the last 12 months? - Hepatitis C testing`,
        `f6_8 Have you received following free items, from NGO or social worker within the last 12 months? - Syphilis testing`,
        `f6_9 Have you received following free items, from NGO or social worker within the last 12 months? - TB screening`,
        accessed_syringe_12m,
        `f6_11 Have you received following free items, from NGO or social worker within the last 12 months? - Information materials`,
        `f6_12 Have you received following free items, from NGO or social worker within the last 12 months? - Case management by a social worker`,
      ),
      ~ . == "Don't know/don’t remember"
    ) ~ "Don’t know/don’t remember",
    TRUE ~ NA_character_
  ),
  ngo_sti_12m = case_when(
    `f6_8 Have you received following free items, from NGO or social worker within the last 12 months? - Syphilis testing` == "Yes" ~ "Yes",
    `f6_8 Have you received following free items, from NGO or social worker within the last 12 months? - Syphilis testing` %in% c("No", "Never") ~ "No",
    `f6_8 Have you received following free items, from NGO or social worker within the last 12 months? - Syphilis testing` == "Don't know/don’t remember" ~ "Don’t know/don’t remember",
    TRUE ~ NA_character_
  ),
  harm_reduction_12m = case_when(
    accessed_syringe_12m == "Yes" |
      `f6_1 Have you received following free items, from NGO or social worker within the last 12 months? - Male condoms` == "Yes" |
      `f6_2 Have you received following free items, from NGO or social worker within the last 12 months? - Female condoms` == "Yes" ~ "Yes",
    accessed_syringe_12m == "No" &
      `f6_1 Have you received following free items, from NGO or social worker within the last 12 months? - Male condoms` == "No" &
      `f6_2 Have you received following free items, from NGO or social worker within the last 12 months? - Female condoms` == "No" ~ "No",
    accessed_syringe_12m == "Don't know/don’t remember" |
      `f6_1 Have you received following free items, from NGO or social worker within the last 12 months? - Male condoms` == "Don't know/don’t remember" |
      `f6_2 Have you received following free items, from NGO or social worker within the last 12 months? - Female condoms` == "Don't know/don’t remember" ~ "Don’t know/don’t remember",
    TRUE ~ NA_character_
  ),
  violence_rape_ever = case_when(
      `h2_5 If yes, in what way? Forced to provide free sexual services` == "Yes" | `h2_6 If yes, in what way? Raped` == "Yes" ~ "Yes",
      `h2_5 If yes, in what way? Forced to provide free sexual services` == "No" & `h2_6 If yes, in what way? Raped` == "No" ~ "No",
      `h2_5 If yes, in what way? Forced to provide free sexual services` == "No question asked" | `h2_6 If yes, in what way? Raped` == "No question asked" ~ "No question asked",
      TRUE ~ NA_character_
  ),
  violence_client = case_when(
      `h3_1 Who was the perpetrator? Regular client` == "Yes" | `h3_2 Who was the perpetrator? Non-regular client` == "Yes" ~ "Yes",
      `h3_1 Who was the perpetrator? Regular client` == "No" & `h3_2 Who was the perpetrator? Non-regular client` == "No" ~ "No",
      `h3_1 Who was the perpetrator? Regular client` == "No question asked" | `h3_2 Who was the perpetrator? Non-regular client` == "No question asked" ~ "No question asked",
      TRUE ~ NA_character_
  ),
  violence_support_any = case_when(
      violence_support_ngo == "Yes" |
      `h4_2 Have you contacted any person or facility after the violence incident? - To a close person (parents, husband/roommate, girlfriend)` == "Yes" |
      `h4_3 Have you contacted any person or facility after the violence incident? - To another client to whom I provide sexual services` == "Yes" |
      `h4_4 Have you contacted any person or facility after the violence incident? - To the police` == "Yes" |
      `h4_5 Have you contacted any person or facility after the violence incident? - To another sex worker` == "Yes" |
      `h4_6 Have you contacted any person or facility after the violence incident? - To the pimp/administrator of the apartment` == "Yes" |
      `h4_7 Have you contacted any person or facility after the violence incident? - Other` == "Yes" ~ "Yes",
      
      `h4_8 Have you contacted any person or facility after the violence incident? - Did not ask for help` == "Yes" ~ "No",
      
      `h4_9 Have you contacted any person or facility after the violence incident? - Don't know/don't remember` == "Yes" |
      `h4_10 Have you contacted any person or facility after the violence incident? - Refusal to answer` == "Yes" ~ "Refuse to answer",
      
      TRUE ~ NA_character_
  ),
  hiv_tested_12m = case_when(
      hiv_tested_lifetime == "No" ~ "No",
      `i8 Please, try to recall when was the last time you were tested?` == "More than 12 months ago" ~ "No",
      `i8 Please, try to recall when was the last time you were tested?` %in% c("Within the last 6 months", "Within the last 6-12 months") ~ "Yes",
      `i8 Please, try to recall when was the last time you were tested?` == "Don't know/don't remember" ~ "Don't know/don't remember",
      TRUE ~ NA_character_
  ),
  avoided_healthcare_12m_bin = case_when(
      avoided_healthcare_12m_stigma == "Yes" |
      avoided_healthcare_12m_disclosure == "Yes" |
      avoided_healthcare_12m_violence == "Yes" |
      avoided_healthcare_12m_police == "Yes" ~ "Yes",
      avoided_healthcare_12m_stigma == "No question asked" |
      avoided_healthcare_12m_disclosure == "No question asked" |
      avoided_healthcare_12m_violence == "No question asked" |
      avoided_healthcare_12m_police == "No question asked" ~ "No question asked",
      TRUE ~ "No"
  ),
  avoided_hiv_test_12m_bin = case_when(
      avoided_hiv_test_12m_stigma == "Yes" |
      avoided_hiv_test_12m_disclosure == "Yes" |
      avoided_hiv_test_12m_violence == "Yes" |
      avoided_hiv_test_12m_police == "Yes" ~ "Yes",
      avoided_hiv_test_12m_stigma == "No question asked" |
      avoided_hiv_test_12m_disclosure == "No question asked" |
      avoided_hiv_test_12m_violence == "No question asked" |
      avoided_hiv_test_12m_police == "No question asked" ~ "No question asked",
      TRUE ~ "No"
  ),
  avoided_hiv_care_12m_bin = case_when(
      avoided_hiv_care_12m_stigma == "Yes" |
      avoided_hiv_care_12m_disclosure == "Yes" |
      avoided_hiv_care_12_violence == "Yes" |
      avoided_hiv_care_12m_police == "Yes" ~ "Yes",
      avoided_hiv_care_12m_stigma == "No question asked" |
      avoided_hiv_care_12m_disclosure == "No question asked" |
      avoided_hiv_care_12_violence == "No question asked" |
      avoided_hiv_care_12m_police == "No question asked" ~ "No question asked",
      TRUE ~ "No"
  ),
  avoided_hiv_treat_12m_bin = case_when(
      avoided_hiv_treat_12m_stigma == "Yes" |
      avoided_hiv_treat_12m_disclosure == "Yes" |
      avoided_hiv_treat_12m_violence == "Yes" |
      avoided_hiv_treat_12m_police == "Yes" ~ "Yes",
      avoided_hiv_treat_12m_stigma == "No question asked" |
      avoided_hiv_treat_12m_disclosure == "No question asked" |
      avoided_hiv_treat_12m_violence == "No question asked" |
      avoided_hiv_treat_12m_police == "No question asked" ~ "No question asked",
      TRUE ~ "No"
  )
)

# recategorise



vars <- c("partners_sw_30d", "partners_nonsw_30d", "client_condom_bin_30d", "client_condom_freq_30d", "perm_partner_condom_lastsex", "alcohol_30d_daily_bin", "drugs_12m_bin", "idu_12m_bin", "sex_with_drugs_and_alcohol_30d", "sex_with_alcohol_30d", "sex_with_drugs_30d", "ngo_access_12m", "violence_rape_ever", "violence_client", "violence_support_any", "hiv_tested_12m", "avoided_healthcare_12m_bin", "avoided_hiv_test_12m_bin", "avoided_hiv_care_12m_bin", "avoided_hiv_treat_12m_bin")

present_vars <- intersect(vars, names(sw_data_2021_clean))

cat_tabs_2021 <- setNames(lapply(present_vars, function(v) {
  tbl <- table(sw_data_2021_clean[[v]], sw_data_2021_clean$hiv_test_rslt, useNA = "ifany")
  list(
    counts  = addmargins(tbl),
    row_pct = round(100 * prop.table(tbl, 1), 1)   # within category level
  )
}), present_vars)

cat_tabs_2021

table(sw_combined_clean$avoided_healthcare_12m_stigma, useNA = "ifany")

# factorise hiv
sw_combined_clean$hiv_test_rslt_bin <- as.factor(sw_combined_clean$hiv_test_rslt_bin)

# ensure adjustment variables are factors
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    ukraine_region = as.factor(ukraine_region),
    year = as.factor(year)
  )

# HIV prevalence by year and city
sw_combined_clean$hiv_test_rslt_bin_num <- ifelse(sw_combined_clean$hiv_test_rslt_bin == "Positive", 1,
                                                  ifelse(sw_combined_clean$hiv_test_rslt_bin == "Negative", 0, NA))
hiv_prevalence <- sw_combined_clean %>%
  group_by(city, year) %>%
  summarise(prevalence = mean(hiv_test_rslt_bin_num, na.rm = TRUE)) %>%
  ungroup()

# Get list of cities
cities <- unique(hiv_prevalence$city)

for (c in cities) {
  city_data <- hiv_prevalence %>% filter(city == c)
  
  # Only plot if there are at least 2 years of data
  if (nrow(city_data) < 2) next
  
  p <- ggplot(city_data, aes(x = year, y = prevalence)) +
    geom_line(color = "black", size = 0.8) +
    geom_point(color = "black", size = 1.5) +
    scale_x_continuous(breaks = city_data$year) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(title = paste("HIV Prevalence in", c),
         x = "Year",
         y = "Prevalence") +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA)
    )
  
   ggsave(filename = paste0("hiv_prevalence_", c, ".png"), plot = p, width = 6, height = 4)
}

# Calculate prevalence by region and year
hiv_prevalence <- sw_combined_clean %>%
  group_by(ukraine_region, year) %>%
  summarise(prevalence = mean(hiv_test_rslt_bin_num, na.rm = TRUE)) %>%
  ungroup()

# Get list of regions
regions <- unique(hiv_prevalence$ukraine_region)

for (r in regions) {
  region_data <- hiv_prevalence %>% filter(ukraine_region == r)
  
  # Only plot if there are at least 2 years of data
  if (nrow(region_data) < 2) next
  
  p <- ggplot(region_data, aes(x = year, y = prevalence)) +
    geom_line(color = "black", size = 0.8) +
    geom_point(color = "black", size = 1.5) +
    scale_x_continuous(breaks = region_data$year) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(title = paste("HIV Prevalence in", r),
         x = "Year",
         y = "Prevalence") +
    theme_minimal(base_size = 14) +
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA)
    )
  
  ggsave(filename = paste0("hiv_prevalence_", r, ".png"), plot = p, width = 6, height = 4)
}

# Calculate prevalence by occupied_partial and year
hiv_prevalence <- sw_combined_clean %>%
  group_by(occupied_partial, year) %>%
  summarise(prevalence = mean(hiv_test_rslt_bin_num, na.rm = TRUE)) %>%
  ungroup()

# Plot all levels in one figure
p <- ggplot(hiv_prevalence, aes(x = year, y = prevalence, color = occupied_partial, group = occupied_partial)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = 2008:2021, limits = c(2008, 2021)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "HIV Prevalence by occupied or attacked by Russia",
       x = "Year",
       y = "Prevalence",
       color = "Occupied or attacked") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("hiv_prevalence_by_occupied_partial.png", plot = p, width = 7, height = 5)

# Filter for cities with data both before 2015 and in/after 2015
years_before <- c(2011, 2013)
years_after <- c(2015, 2017, 2021)

# Find cities with at least one year before 2015 and one year in/after 2015
cities_before <- sw_combined_clean %>%
  filter(year %in% years_before) %>%
  distinct(city)
cities_after <- sw_combined_clean %>%
  filter(year %in% years_after) %>%
  distinct(city)

target_cities <- intersect(cities_before$city, cities_after$city)

# Only keep rows for those cities and years 2011-2021
years_required <- c(2011, 2013, 2015, 2017, 2021)
sw_filtered <- sw_combined_clean %>%
  filter(city %in% target_cities, year %in% years_required)

# Calculate prevalence by occupied_partial and year (for filtered cities)
hiv_prevalence <- sw_filtered %>%
  group_by(occupied_partial, year) %>%
  summarise(prevalence = mean(hiv_test_rslt_bin_num, na.rm = TRUE)) %>%
  ungroup()

# Plot all levels in one figure
p <- ggplot(hiv_prevalence, aes(x = year, y = prevalence, color = occupied_partial, group = occupied_partial)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = years_required, limits = c(2011, 2021)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "HIV Prevalence by occupied or attacked by Russia",
       x = "Year",
       y = "Prevalence",
       color = "Occupied or attacked") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("hiv_prevalence_by_occupied_partial.png", plot = p, width = 7, height = 5)

# Calculate prevalence by region and year
hiv_prevalence <- sw_combined_clean %>%
  group_by(ukraine_region_4cat, year) %>%
  summarise(prevalence = mean(hiv_test_rslt_bin_num, na.rm = TRUE)) %>%
  ungroup()

# Plot all levels in one figure
p <- ggplot(hiv_prevalence, aes(x = year, y = prevalence, color = ukraine_region_4cat, group = ukraine_region_4cat)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = 2008:2021, limits = c(2008, 2021)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "HIV Prevalence by region",
       x = "Year",
       y = "Prevalence",
       color = "Region") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("hiv_prevalence_by_region_4cat.png", plot = p, width = 7, height = 5)

# Calculate prevalence by city and year
hiv_prevalence <- sw_combined_clean %>%
  group_by(city, year) %>%
  summarise(prevalence = mean(hiv_test_rslt_bin_num, na.rm = TRUE)) %>%
  ungroup()

# Plot all levels in one figure
p <- ggplot(hiv_prevalence, aes(x = year, y = prevalence, color = city, group = city)) +
  geom_line(size = 0.8) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = 2008:2021, limits = c(2008, 2021)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "HIV Prevalence by city",
       x = "Year",
       y = "Prevalence",
       color = "City") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

ggsave("hiv_prevalence_by_city.png", plot = p, width = 7, height = 5)


# Add "Overall" pseudo-city
sw_combined_clean <- bind_rows(sw_combined_clean, sw_combined_clean %>% mutate(city = "Overall"))

sw_combined_clean <- bind_rows(
  sw_combined_clean,
  sw_combined_clean %>% mutate(street_sw_bin = "Overall")
)

sw_combined_clean <- sw_combined_clean %>%
  mutate(street_sw_bin = factor(street_sw_bin, levels = c("No", "Yes", "Overall")))

# Create a workbook
wb <- createWorkbook()

for (var in vars) {
  
  cat("Processing:", var, "\n")
  
  # Summarise by city/year and IDU status
  summary_df <- sw_combined_clean %>%
    filter(!is.na(.data[[var]]), !is.na(street_sw_bin)) %>%
    group_by(city, year, street_sw_bin) %>%
    summarise(
      IDU_n = sum(.data[[var]] == "Yes" & idu_ever_3cat == "Yes", na.rm = TRUE),
      IDU_denom = sum(idu_ever_3cat == "Yes", na.rm = TRUE),
      IDU_pct = ifelse(IDU_denom > 0, round(IDU_n/IDU_denom*100,1), NA),
      
      No_IDU_n = sum(.data[[var]] == "Yes" & idu_ever_3cat == "No", na.rm = TRUE),
      No_IDU_denom = sum(idu_ever_3cat == "No", na.rm = TRUE),
      No_IDU_pct = ifelse(No_IDU_denom > 0, round(No_IDU_n/No_IDU_denom*100,1), NA),
      
      Overall_n = sum(.data[[var]] == "Yes", na.rm = TRUE),
      Overall_denom = n(),
      Overall_pct = round(Overall_n/Overall_denom*100,1),
      
      .groups = "drop"
    ) %>%
    arrange(city, year, street_sw_bin)
  
  # Name the sheet as first 10 characters of variable
  sheet_name <- substr(var, 1, 25)
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet = sheet_name, summary_df)
}

# Save workbook
saveWorkbook(wb, "IDU_stratified_summary.xlsx", overwrite = TRUE)
cat("Saved all summaries to IDU_stratified_summary.xlsx\n")






## idu over time

sw_combined_clean <- readRDS("sw_combined_clean.rds")

sw_combined_clean <- sw_combined_clean %>%
  mutate(
    idu_ever_3cat = case_when(
      idu_ever_3cat %in% c("Yes", "1") ~ "Yes",
      idu_ever_3cat %in% c("No", "0") ~ "No",
      TRUE ~ NA_character_
    )
  )

city_prev <- sw_combined_clean %>%
  group_by(city, year) %>%
  summarise(
    n_total = n(),
    no_prop = sum(idu_ever_3cat == "No", na.rm = TRUE) / n_total,
    yes_prop = sum(idu_ever_3cat == "Yes", na.rm = TRUE) / n_total,
    na_prop  = sum(is.na(idu_ever_3cat)) / n_total,
    .groups = "drop"
  ) %>%
  mutate(
    level = "City",
    yes = paste0(round(yes_prop * 100, 1), "%")
  ) %>%
  select(level, city, year, no_prop, yes, na_prop)

overall_prev <- sw_combined_clean %>%
  group_by(year) %>%
  summarise(
    n_total = n(),
    no_prop = sum(idu_ever_3cat == "No", na.rm = TRUE) / n_total,
    yes_prop = sum(idu_ever_3cat == "Yes", na.rm = TRUE) / n_total,
    na_prop  = sum(is.na(idu_ever_3cat)) / n_total,
    .groups = "drop"
  ) %>%
  mutate(
    level = "Overall",
    city = "All cities",
    yes = paste0(round(yes_prop * 100, 1), "%")
  ) %>%
  select(level, city, year, no_prop, yes, na_prop)

idu_prev <- bind_rows(city_prev, overall_prev) %>%
  arrange(level, city, year)

wb <- createWorkbook()
addWorksheet(wb, "idu_prevalence")
writeData(wb, "idu_prevalence", idu_prev)
saveWorkbook(wb, "idu_prevalence_by_city_year.xlsx", overwrite = TRUE)














## hiv prevalence tables

# Load data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

# Keep only selected cities
cities_keep <- c("Cherkasy","Dnipro", "Kropyvnytskyi","Kyiv", "Mariupol","Odesa")

sw_combined_clean <- sw_combined_clean %>%
  filter(city %in% cities_keep)

# Clean variables
sw_combined_clean <- sw_combined_clean %>%
  mutate(
    idu_ever_3cat = factor(idu_ever_3cat, levels = c("No", "Yes")),
    street_sw_bin = factor(street_sw_bin, levels = c("No", "Yes")),
    hiv = case_when(
      hiv_test_rslt_bin %in% c("Positive","1","Yes") ~ 1,
      hiv_test_rslt_bin %in% c("Negative","0","No") ~ 0,
      TRUE ~ NA_real_
    )
  )

# function to return "n/N (pct)"
calc_str <- function(x) {
  N <- sum(!is.na(x))
  n <- sum(x == 1, na.rm = TRUE)
  if (N == 0) return(NA_character_)
  paste0(n, "/", N, " (", round(n/N*100, 1), "%)")
}

# Create HIV table (by city)
hiv_table <- sw_combined_clean %>%
  group_by(city, year) %>%
  summarise(
    Overall = calc_str(hiv),

    Street = calc_str(hiv[street_sw_bin == "Yes"]),
    Indoor = calc_str(hiv[street_sw_bin == "No"]),

    IDU = calc_str(hiv[idu_ever_3cat == "Yes"]),
    Non_IDU = calc_str(hiv[idu_ever_3cat == "No"]),

    Street_IDU = calc_str(hiv[street_sw_bin == "Yes" & idu_ever_3cat == "Yes"]),
    Street_non_IDU = calc_str(hiv[street_sw_bin == "Yes" & idu_ever_3cat == "No"]),

    Indoor_IDU = calc_str(hiv[street_sw_bin == "No" & idu_ever_3cat == "Yes"]),
    Indoor_non_IDU = calc_str(hiv[street_sw_bin == "No" & idu_ever_3cat == "No"]),

    .groups = "drop"
  )

# Create overall table (all cities combined)
overall_table <- sw_combined_clean %>%
  group_by(year) %>%
  summarise(
    Overall = calc_str(hiv),

    Street = calc_str(hiv[street_sw_bin == "Yes"]),
    Indoor = calc_str(hiv[street_sw_bin == "No"]),

    IDU = calc_str(hiv[idu_ever_3cat == "Yes"]),
    Non_IDU = calc_str(hiv[idu_ever_3cat == "No"]),

    Street_IDU = calc_str(hiv[street_sw_bin == "Yes" & idu_ever_3cat == "Yes"]),
    Street_non_IDU = calc_str(hiv[street_sw_bin == "Yes" & idu_ever_3cat == "No"]),

    Indoor_IDU = calc_str(hiv[street_sw_bin == "No" & idu_ever_3cat == "Yes"]),
    Indoor_non_IDU = calc_str(hiv[street_sw_bin == "No" & idu_ever_3cat == "No"]),

    .groups = "drop"
  )

# Create Excel workbook
wb <- createWorkbook()

# Write one sheet per city
for (ct in cities_keep) {
  
  df_city <- hiv_table %>%
    filter(city == ct) %>%
    select(-city)
  
  sheet_name <- substr(ct, 1, 31)
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, df_city)
}

# Add overall sheet
addWorksheet(wb, "Overall")
writeData(wb, "Overall", overall_table)

# Save workbook
saveWorkbook(wb, "HIV_prevalence_by_city.xlsx", overwrite = TRUE)

## idu 12m

# Load data
sw_combined_clean <- readRDS("sw_combined_clean.rds")

sw_combined_clean <- sw_combined_clean %>%
  mutate(
    idu_12m_3cat = case_when(
      idu_12m_3cat %in% c("Yes", "1") ~ "Yes",
      idu_12m_3cat %in% c("No", "0") ~ "No",
      TRUE ~ NA_character_
    )
  )

city_prev <- sw_combined_clean %>%
  group_by(city, year) %>%
  summarise(
    n_total = n(),
    no_prop = sum(idu_12m_3cat == "No", na.rm = TRUE) / n_total,
    yes_prop = sum(idu_12m_3cat == "Yes", na.rm = TRUE) / n_total,
    na_prop  = sum(is.na(idu_12m_3cat)) / n_total,
    .groups = "drop"
  ) %>%
  mutate(
    level = "City",
    yes = paste0(round(yes_prop * 100, 1), "%")
  ) %>%
  select(level, city, year, no_prop, yes, na_prop)

overall_prev <- sw_combined_clean %>%
  group_by(year) %>%
  summarise(
    n_total = n(),
    no_prop = sum(idu_12m_3cat == "No", na.rm = TRUE) / n_total,
    yes_prop = sum(idu_12m_3cat == "Yes", na.rm = TRUE) / n_total,
    na_prop  = sum(is.na(idu_12m_3cat)) / n_total,
    .groups = "drop"
  ) %>%
  mutate(
    level = "Overall",
    city = "All cities",
    yes = paste0(round(yes_prop * 100, 1), "%")
  ) %>%
  select(level, city, year, no_prop, yes, na_prop)

idu_12m_prev <- bind_rows(city_prev, overall_prev) %>%
  arrange(level, city, year)

wb <- createWorkbook()
addWorksheet(wb, "idu_12m_prevalence")
writeData(wb, "idu_12m_prevalence", idu_12m_prev)
saveWorkbook(wb, "idu_12m_prevalence_by_city_year.xlsx", overwrite = TRUE)

## ngo access over time

sw_combined_clean <- readRDS("sw_combined_clean.rds")

sw_combined_clean <- sw_combined_clean %>%
  mutate(
    ngo_client_lifetime = case_when(
      ngo_client_lifetime %in% c("Yes", "1") ~ "Yes",
      ngo_client_lifetime %in% c("No", "0") ~ "No",
      TRUE ~ NA_character_
    )
  )

city_prev <- sw_combined_clean %>%
  group_by(city, year) %>%
  summarise(
    n_total = n(),
    no_prop = sum(ngo_client_lifetime == "No", na.rm = TRUE) / n_total,
    yes_prop = sum(ngo_client_lifetime == "Yes", na.rm = TRUE) / n_total,
    na_prop  = sum(is.na(ngo_client_lifetime)) / n_total,
    .groups = "drop"
  ) %>%
  mutate(
    level = "City",
    yes = paste0(round(yes_prop * 100, 1), "%")
  ) %>%
  select(level, city, year, no_prop, yes, na_prop)

overall_prev <- sw_combined_clean %>%
  group_by(year) %>%
  summarise(
    n_total = n(),
    no_prop = sum(ngo_client_lifetime == "No", na.rm = TRUE) / n_total,
    yes_prop = sum(ngo_client_lifetime == "Yes", na.rm = TRUE) / n_total,
    na_prop  = sum(is.na(ngo_client_lifetime)) / n_total,
    .groups = "drop"
  ) %>%
  mutate(
    level = "Overall",
    city = "All cities",
    yes = paste0(round(yes_prop * 100, 1), "%")
  ) %>%
  select(level, city, year, no_prop, yes, na_prop)

ngo_prev <- bind_rows(city_prev, overall_prev) %>%
  arrange(level, city, year)

wb <- createWorkbook()
addWorksheet(wb, "ngo_prevalence")
writeData(wb, "ngo_prevalence", ngo_prev)
saveWorkbook(wb, "ngo_prevalence_by_city_year.xlsx", overwrite = TRUE)

# prevalence by region

# Add "Overall" pseudo-region
sw_combined_clean <- bind_rows(
  sw_combined_clean,
  sw_combined_clean %>% mutate(ukraine_region_4cat = "Overall")
)

sw_combined_clean <- bind_rows(
  sw_combined_clean,
  sw_combined_clean %>% mutate(street_sw_bin = "Overall")
)

sw_combined_clean <- sw_combined_clean %>%
  mutate(street_sw_bin = factor(street_sw_bin, levels = c("No", "Yes", "Overall")))

wb <- createWorkbook()

for (var in vars) {
  
  cat("Processing:", var, "\n")
  
  summary_df <- sw_combined_clean %>%
    filter(!is.na(.data[[var]]), !is.na(street_sw_bin)) %>%
    group_by(ukraine_region_4cat, year, street_sw_bin) %>%
    summarise(
      IDU_n = sum(.data[[var]] == "Yes" & idu_ever_3cat == "Yes", na.rm = TRUE),
      IDU_denom = sum(idu_ever_3cat == "Yes", na.rm = TRUE),
      IDU_pct = ifelse(IDU_denom > 0, round(IDU_n/IDU_denom*100,1), NA),
      
      No_IDU_n = sum(.data[[var]] == "Yes" & idu_ever_3cat == "No", na.rm = TRUE),
      No_IDU_denom = sum(idu_ever_3cat == "No", na.rm = TRUE),
      No_IDU_pct = ifelse(No_IDU_denom > 0, round(No_IDU_n/No_IDU_denom*100,1), NA),
      
      Overall_n = sum(.data[[var]] == "Yes", na.rm = TRUE),
      Overall_denom = n(),
      Overall_pct = round(Overall_n/Overall_denom*100,1),
      
      .groups = "drop"
    ) %>%
    arrange(ukraine_region_4cat, year, street_sw_bin)
  
  sheet_name <- substr(var, 1, 25)
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet = sheet_name, summary_df)
}

saveWorkbook(wb, "IDU_stratified_summary_by_region.xlsx", overwrite = TRUE)