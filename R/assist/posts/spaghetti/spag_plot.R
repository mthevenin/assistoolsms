spag_plot <- function(donnees, 
                      var_x,
                      var_y,
                      var_group,
                      ordre = "alpha" ,
                      decroiss = "non",
                      titre   = "", 
                      titre_x  = "",
                      titre_y   ="", 
                      source    = "",
                      interval = 10,
                      n_col = 4L){
  
  
  
  
  ### GESTION LIBRARY ----
  
  # Liste des packages à charger
  packages <- c("dplyr", "ggplot2", "ggthemes")
  
  # Vérifier si les packages sont déjà installés
  missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  
  # Installer les packages manquants
  if (length(missing_packages) > 0) {
    message("Installation des packages manquants : ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages, dependencies = TRUE)
  }
  
  # Charger les packages
  lapply(packages, require, character.only = TRUE)
  
  
  
  
  ### GESTION DES ERREURS ----
  
  # Vérification du parametre ordre
  if (ordre[1] != "alpha" && is.numeric(ordre) != TRUE && is.vector(ordre) != TRUE  ) {
    stop("Erreur : ordre doit être soit être 'alpha', soit une valeur de var_x, soit un vecteur avec les noms des groupes dans l'ordre voulu")
  }
  
  # Vérification de parametre decroiss
  if (decroiss != "oui" && decroiss != "non") {
    stop("Erreur : decroiss doit être 'oui' ou 'non'")
  }
  
  
  ### GESTION DES PARAMETRES ----
  
  dt <- donnees
  
  dt <- dt %>%
    mutate(grp= get(var_group)) %>% 
    mutate(grp2 = grp)  %>% 
    mutate(var_y = as.numeric(get(var_y)),
           var_x  = as.numeric(get(var_x)))
  
  min_x <- min(dt$var_x)
  max_x <- max(dt$var_x)
  
  ### GESTION DE L'ORDRE DES GRAPHIQUES ----
  
  if(ordre[1] == "alpha" & decroiss == "oui") {
    
    grp_ordre <- as_tibble(dt %>%
                             group_by(grp) %>% 
                             slice(1) %>% 
                             arrange(desc(grp))%>% 
                             select(grp))
    vect_ordre <- as.vector(unlist(grp_ordre[,1]))
    
  } else if(ordre[1] == "alpha" & decroiss == "non") {
    
    grp_ordre <- as.data.frame(dt %>%
                                 group_by(grp) %>% 
                                 slice(1)      %>% 
                                 arrange(grp)  %>% 
                                 select(grp))
    vect_ordre <<- as.vector(unlist(grp_ordre[,1]))
    
  } else if(is.numeric(ordre) == TRUE & decroiss == "oui") {
    
    grp_ordre <- as.data.frame(dt %>%
                                 filter(var_x == ordre) %>% 
                                 arrange(desc(var_y))%>% 
                                 select(grp))
    vect_ordre <<- as.vector(unlist(grp_ordre[,1]))
    
    
  } else if(is.numeric(ordre) == TRUE & decroiss == "non") {
    
    grp_ordre <- as.data.frame(dt %>%
                                 filter(var_x == ordre) %>% 
                                 arrange(var_y) %>% 
                                 select(grp))
    vect_ordre <<- as.vector(unlist(grp_ordre[,1]))
    
    
    
  } else if(is.vector(ordre) == TRUE) {
    
    vect_ordre <<- ordre
    
    
  }
  
  dt$grp <- factor(dt$grp, levels = vect_ordre)
  dt$grp2 <- factor(dt$grp2, levels = vect_ordre)
  
  ### REALISATION DU GRAPHIQUE ----
  
  ggplot(dt) +
    aes(x = var_x, y = var_y) +
    geom_line(data = dt %>% dplyr::select(-grp), aes(group = grp2), color = "grey", lwd = 0.5) +    
    geom_line(colour = "#C24168", lwd = 1.4) +
    labs(title = titre_y) +
    theme_minimal() +
    labs(x = titre_x, 
         y = titre_y, 
         title = titre, 
         caption = source) +
    scale_x_continuous(guide = guide_axis(n.dodge = 2), breaks = seq(min_x, 
                                                                     max_x, 
                                                                     interval)) + 
    facet_wrap(vars(grp), ncol = n_col)
  
  
}