# 1 = Ich habe mein Verhalten nicht geändert
# 2 = Ich bin gegenüber meiner Umgebung misstrauischer / vorsichtiger.
# 3 = Ich verzichte (weitgehend) auf die Nutzung sozialer Medien
# 4 = Ich äußere mich zu be- stimmten Themen seltener als früher
# 5 = Ich meide bestimmte Orte oder Veranstaltungen
# 6 = Ich habe meinen Wohnort geändert
# 7 = Anders, und zwar
#
# Enge Definition von substantieller Repräsentation: Nur 4
# Wenn weit: auch 3 und 6 - Einschränkungen von Interaktionen mit Bürger*innen
behavioral_change_index <- lapply(
  paste0("v_sorge_umgang", 3:5),
  function(var){
    x <- as.vector(kommrep_loc[[var]])
    x <- ifelse(x == 98, NA, x)
    x <- ifelse(x == 1, 1L, 0L)
  }
) %>%
  as.data.frame() %>%
  as.matrix() %>%
  apply(1, sum, na.rm = TRUE)


threat_comm_index <- lapply(
  # v_perzb_art1 = Art der der Bedrohung: Anrufe, E-Mails, Briefe oder Faxe
  # v_perzb_art2 = Art der der Bedrohung: sozialen Netzwerke
  paste0("v_perzb_art", 1:2), 
  function(var){
    x <- as.vector(kommrep_loc[[var]])
    x <- ifelse(x == 98, NA, x)
    x <- x - 1
    x
  }
) %>%
  as.data.frame() %>%
  as.matrix() %>%
  apply(1, function(row) if (all(is.na(row))) NA else sum(row, na.rm = TRUE))

threat_physical_index <- lapply(
  paste0("v_perzb_art", 3:5),
  function(var){
    x <- as.vector(kommrep_loc[[var]])
    x <- ifelse(x == 98, NA, x)
    x <- x - 1
    x
  }
) %>%
  as.data.frame() %>%
  as.matrix() %>%
  apply(1, function(row) if (all(is.na(row))) NA else sum(row, na.rm = TRUE))


kommrep_loc_lm <- kommrep_loc %>%
  
  mutate(exit = as_factor(v_exit)) %>%
  
  mutate(stay = recode(
    exit,
    `Weiß nicht` = "FALSE",
    `Ja` = "FALSE",
    `Nein` = "TRUE"
  )) %>%
  mutate(stay = as.logical(stay)) %>%
  
  mutate(threat_verbal = threat_comm_index / 2) %>% 
  mutate(threat_physical = threat_physical_index / 3) %>% 
  
  mutate(threat_verbal_bin = ifelse(threat_verbal > 0, TRUE, FALSE)) %>%
  mutate(threat_physical_bin = ifelse(threat_physical > 0, TRUE, FALSE)) %>%

  mutate(threat_experience = ifelse(as.vector(v_bedrohung) == 1L, TRUE, FALSE)) %>% 

  mutate(age = 2022 - v_alter) %>%
  mutate(age = age - min(age, na.rm = TRUE)) %>% 
  mutate(age = age / max(age, na.rm = TRUE)) %>% 
  
  mutate(AfD = ifelse(party == "AfD", TRUE, FALSE)) %>%
  mutate(CDU = ifelse(party == "CDU", TRUE, FALSE)) %>%
  mutate(CSU = ifelse(party == "CSU", TRUE, FALSE)) %>%
  mutate(FW = ifelse(party == "Freie Wähler", TRUE, FALSE)) %>%
  mutate(LINKE = ifelse(party == "Die Linke", TRUE, FALSE)) %>%
  mutate(GRUENE = ifelse(party == "Bündnis 90/Die Grünen", TRUE, FALSE)) %>%
  mutate(SPD = ifelse(party == "SPD", TRUE, FALSE)) %>%
  
  mutate(asian = as.logical(as.integer(as.factor(v_out3_mmh)) - 1) | as.logical(as.integer(as.factor(v_out3_mkm)) - 1)) %>%	# Asiatisch
  mutate(asian = ifelse(is.na(asian), FALSE, asian)) %>% 
  
  mutate(jewish = as.logical(as.integer(as.factor(v_out4_mmh)) - 1) | as.logical(as.integer(as.factor(v_out4_mkm)) - 1)) %>%	# Jüdisch
  mutate(jewish = ifelse(is.na(jewish), FALSE, jewish)) %>% 
  
  mutate(muslim = as.logical(as.integer(as.factor(v_out5_mmh)) - 1)| as.logical(as.integer(as.factor(v_out5_mkm)) - 1)) %>%	# Muslimisch
  mutate(muslim = ifelse(is.na(muslim), FALSE, muslim)) %>% 
  
  mutate(sintiroma = as.logical(as.integer(as.factor(v_out6_mmh)) - 1) | as.logical(as.integer(as.factor(v_out6_mkm)) - 1)) %>%	# Sinti oder Roma
  mutate(sintiroma = ifelse(is.na(sintiroma), FALSE, sintiroma)) %>% 
  
  mutate(turkish = as.logical(as.integer(as.factor(v_out7_mmh)) - 1)| as.logical(as.integer(as.factor(v_out7_mkm)) - 1)) %>%	# Türkisch
  mutate(turkish = ifelse(is.na(turkish), FALSE, turkish)) %>% 
  
  mutate(eastern = as.logical(as.integer(as.factor(v_out8_mmh)) - 1) | as.logical(as.integer(as.factor(v_out8_mkm)) - 1)) %>%	# Osteuropäisch
  mutate(eastern = ifelse(is.na(eastern), FALSE, eastern)) %>% 
  
  mutate(black = as.logical(as.integer(as.factor(v_out9_mmh)) - 1)| as.logical(as.integer(as.factor(v_out9_mkm)) - 1)) %>%	# Schwarze Person
  mutate(black = ifelse(is.na(black), FALSE, black)) %>% 
  
  mutate(muslim_turkish = muslim | turkish) %>% 
  
  mutate(racialized = asian | jewish | muslim | sintiroma | turkish | eastern | black) %>% 
  
  mutate(exit = as_factor(v_exit)) %>%
  mutate(exit = ifelse(exit == "Weiß nicht", NA, exit)) %>%
  mutate(exit = !as.logical(as.integer(exit) - 1)) %>% 
  
  # Measurements for substantial representation
  mutate(muted1 = ifelse(v_sorge_umgang4 == 1L, TRUE, FALSE)) %>%
  mutate(muted2 = ifelse(is.na(muted1), FALSE, muted1)) %>%
  
  mutate(behavioral_change = ifelse(behavioral_change_index > 0L, TRUE, FALSE)) %>%
  
  mutate(female_diverse = as_factor(v_geschlecht)) %>% 
  mutate(female_diverse = recode(
    female_diverse,
    `weiblich` = "TRUE",
    `männlich` = "FALSE",
    `divers` = "TRUE",
    `weitere / andere` = "TRUE"
  )) %>%
  mutate(female_diverse = as.logical(female_diverse)) %>%
  
  mutate(class_normalized = ifelse(as.vector(v_schicht) != 98, ((v_schicht - 1L) / 5L) * -1 + 1, NA)) %>%
  
  mutate(`class` = as_factor(v_schicht)) %>%
  mutate(
    `class` = recode(
      `class`,
      `Der Oberschicht` = "upper",
      `Der oberen Mittelschicht` = "upper",
      `Der Mittelschicht` = "middle",
      `Der unteren Mittelschicht` = "lower",
      `Der Arbeiterschicht` = "lower",
      `Der Unterschicht` = "lower",
      `Weiß nicht` = "dont't know / not available"
    )
  ) %>%
  mutate(`class` = fct_na_value_to_level(`class`, level = "dont't know / not available")) %>%
  
  
  mutate(
    # issues triggering classism
    soc = ifelse(v_issue_1_recoded %in% c("Armut/Obdachlosigkeit", "Klassismus", "Wohnen/Bau"), TRUE, FALSE), 
    
    # issues triggering sexism
    gen = ifelse(v_issue_1_recoded %in% c("Gleichstellung"), TRUE, FALSE), 
    
    # issues triggering racism
    div = ifelse(v_issue_1_recoded %in% c("Migration/Integration", "Extremismus", "Sozialer Zusammenhalt"), TRUE, FALSE) 
  )
