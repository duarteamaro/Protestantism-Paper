## first draft of mainstream right robustness check models to have more clarity



needs(tidyverse, modelsummary)


# import data here later

m_LR_dep <- lm(data = d_FN_controls |> 
                 mutate(year = as.character(year)),
               pvoixLR ~ share_protestant + year + dep)

m_LR_hist <- lm(data = d_FN_controls |> 
                  mutate(year = as.character(year)),
                pvoixLR ~ share_protestant + year + dep + letter + brigade + 
                  dist_cassini + HubDist + revratio1790 +
                  total_protests + pconjsign1686 + pserment1791 +
                  altitude_moyenne + latitude_centre + longitude_centre)

m_LR_cont <- lm(data = d_FN_controls |> 
                  mutate(year = as.character(year)),
                pvoixLR ~ share_protestant + year + dep + revmoy + pop + petranger + 
                  pchom + prixbien)

m_LR_all <- lm(data = d_FN_controls |> 
                 mutate(year = as.character(year)),
               pvoixLR ~ share_protestant + year + dep + letter + brigade + 
                 dist_cassini + HubDist + revratio1790 +
                 total_protests + pconjsign1686 + pserment1791 + 
                 altitude_moyenne + latitude_centre + longitude_centre +
                 revmoy + pop + petranger + pchom + prixbien)

m_LR_all_sup <- lm(data = d_FN_controls |> 
                     mutate(year = as.character(year)),
                   pvoixLR ~ share_protestant + year + dep + letter + brigade + 
                     dist_cassini + HubDist + revratio1790 +
                     total_protests + pconjsign1686 + pserment1791 + 
                     altitude_moyenne + latitude_centre + longitude_centre +
                     revmoy + pop + petranger + pchom + prixbien + psup)

m_LR_all_educ <- lm(data = d_FN_controls |> 
                      mutate(year = as.character(year)),
                    pvoixLR ~ share_protestant + year + dep + letter + brigade + 
                      dist_cassini + HubDist + revratio1790 +
                      total_protests + pconjsign1686 + pserment1791 + 
                      altitude_moyenne + latitude_centre + longitude_centre +
                      revmoy + pop + petranger + pchom + prixbien + psup + pbac)


modelsummary(
  list(
    "Dep FEs"              = m_LR_dep, 
    "Historical Controls"  = m_LR_hist, 
    "Contemp. Controls"    = m_LR_cont, 
    "Both Controls"        = m_LR_all, 
    "Both Sets + Higher Ed" = m_LR_all_sup,
    "Both Sets + Higher Ed + High School" = m_LR_all_educ
  ), 
  vcov = "robust", 
  cluster = "codecommune",
  stars = TRUE,
  coef_omit = "year|dep",
  gof_map = tibble::tribble(
    ~raw,            ~clean,    ~fmt,
    "nobs",          "N",       0,
    "r.squared",     "R^2",     2,
    "adj.r.squared", "Adj. R^2", 2
  ),
  output = "tables/table_minstream_right.tex",
  
)
