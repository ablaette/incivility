behavioral_change_index <- lapply(
  paste0("v_sorge_umgang", 2:6),
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
  
  mutate(behavioral_change = behavioral_change_index / 5) %>%
  mutate(behavioral_change = ifelse(
    as.vector(v_sorge_umgang1) == 0L, TRUE, FALSE)
  ) %>%
  
  mutate(female_diverse = as_factor(v_geschlecht)) %>% 
  mutate(female_diverse = recode(
    female_diverse,
    `weiblich` = "TRUE",
    `männlich` = "FALSE",
    `divers` = "TRUE",
    `weitere / andere` = "TRUE"
  )) %>%
  mutate(female_diverse = as.logical(female_diverse)) %>%
  
  mutate(class = ifelse(as.vector(v_schicht) != 98, v_schicht - 1L / 6L, NA)) %>%
  
  mutate(
    # issues triggering classism
    soc = ifelse(v_issue_1_recoded %in% c("Armut/Obdachlosigkeit", "Klassismus", "Wohnen/Bau"), TRUE, FALSE), 
    
    # issues triggering sexism
    gen = ifelse(v_issue_1_recoded %in% c("Gleichstellung"), TRUE, FALSE), 
    
    # issues triggering racism
    div = ifelse(v_issue_1_recoded %in% c("Migration/Integration", "Extremismus", "Sozialer Zusammenhalt"), TRUE, FALSE) 
  )
