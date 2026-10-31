## build-historic-panel.R
##
## Commune x election panel for the legislative elections 1848-2022
## (Cage-Piketty commune files), on the 2025 commune frame, with the 1839
## Protestant share and the historical controls of code.Rmd.
##
## Outcomes (all in % of exprimes, turnout in % of inscrits):
##   - our eight party groups (tables/party_family_dictionary.csv)
##   - the five Cage-Piketty blocs (pvoteG ... pvoteD)
##   - turnout
##
## Election-year commune codes are moved to their 2025 successor with the INSEE
## COG movement table (all events since 1943) and votes are summed within the
## 2025 commune. Communes touched by a split or creation that cannot be undone
## this way are dropped. Codes that cannot be resolved are written to
## tables/cassini_flag_list.csv.
##
## Also writes historic_families.rds (the vote of every Cage-Piketty party
## family, long), referendum_panel.rds (the referendums of 1793, 1795, 1946,
## 1992 and 2005) and presidential_panel.rds / presidential_votes.rds (the
## presidential elections 1848 and 1965-2022, both rounds), all on the same
## frame with treatment and controls.
##
## Needs clean_protestant_1839.csv (build-protestant-data.R),
## tables/party_family_dictionary.csv (build-party-dictionary.R) and
## language_1886.csv (build-language-1886.R).

library(tidyverse)

dict <- read_csv("tables/party_family_dictionary.csv", show_col_types = FALSE)

elections <- dict |> distinct(election, year, republic, file)

group_vars <- tribble(
  ~group,                    ~var,
  "Communist/Extreme left",  "p_communist",
  "Socialist left",          "p_socialist",
  "Green/ecologist",         "p_green",
  "Republican/Radical left", "p_radical",
  "Centre republican",       "p_centre",
  "Republican right",        "p_right",
  "Anti-republican right",   "p_antirep",
  "Other/Divers",            "p_other"
)


# commune codes -----------------------------------------------------------------

cog_mvt <- read_csv("cog/v_mvt_commune_2025.csv", show_col_types = FALSE)

cog_modern <- read_csv("cog/v_commune_2025.csv", show_col_types = FALSE) |>
  filter(TYPECOM == "COM") |>
  pull(COM)

## mergers (31-35), code changes (41, 50) and status changes (70, 71):
## old code -> code after its most recent event
cog_succ <- cog_mvt |>
  filter(MOD %in% c("31", "32", "33", "34", "35", "41", "50", "70", "71"),
         COM_AV != COM_AP) |>
  slice_max(DATE_EFF, by = COM_AV, with_ties = FALSE)

to_modern <- function(codes) {
  for (i in 1:8) {
    codes <- if_else(!codes %in% cog_modern & codes %in% cog_succ$COM_AV,
                     cog_succ$COM_AP[match(codes, cog_succ$COM_AV)], codes)
  }
  if_else(codes %in% cog_modern, codes, NA_character_)
}

## splits (21) of communes that were not merged after 1943, and creations (20):
## their old returns cannot be matched to today's territory
first_merger <- cog_mvt |>
  filter(MOD %in% c("31", "32", "33", "34"), COM_AV != COM_AP) |>
  summarise(first_merger = min(DATE_EFF), .by = COM_AV)

splits <- cog_mvt |>
  filter(MOD == "21") |>
  left_join(first_merger, by = c("COM_AP" = "COM_AV")) |>
  filter(is.na(first_merger) | first_merger > DATE_EFF)

created <- cog_mvt |> filter(MOD == "20")

split_flagged <- c(splits$COM_AV, splits$COM_AP, created$COM_AP) |>
  unique() |>
  to_modern() |>
  na.omit()


# votes -------------------------------------------------------------------------

read_votes <- function(file) {
  read_csv(file, show_col_types = FALSE, guess_max = 1e5,
           col_types = cols(codecommune = col_character())) |>
    dplyr::select(codecommune, nomcommune, inscrits, votants, exprimes,
                  voteG, voteCG, voteC, voteCD, voteD,
                  starts_with("voix"), -starts_with("voixtot")) |>
    mutate(codecommune = str_pad(codecommune, width = 5, pad = "0"),
           across(c(inscrits:voteD, starts_with("voix")), as.numeric),
           code_insee = to_modern(codecommune))
}

## per 2025 commune: totals and blocs, and the votes of each party family
commune_votes <- function(file, election) {
  votes <- read_votes(file) |> filter(!is.na(code_insee))
  list(
    totals = votes |>
      summarise(across(c(inscrits:voteD), \(x) sum(x, na.rm = TRUE)), .by = code_insee) |>
      filter(exprimes > 0) |>
      mutate(election = election),
    families = votes |>
      pivot_longer(starts_with("voix"), names_to = "family", names_prefix = "voix",
                   values_to = "voix") |>
      summarise(voix = sum(voix, na.rm = TRUE), .by = c(code_insee, family)) |>
      mutate(election = election)
  )
}

votes <- map2(elections$file, elections$election, commune_votes)

d_totals <- map(votes, "totals") |> list_rbind()

## party family shares in % of exprimes
d_families <- map(votes, "families") |>
  list_rbind() |>
  inner_join(d_totals |> dplyr::select(code_insee, election, exprimes),
             by = c("code_insee", "election")) |>
  mutate(share = 100 * voix / exprimes) |>
  dplyr::select(code_insee, election, family, share)

## our groups are sums of families
d_groups <- d_families |>
  inner_join(dict |> dplyr::select(election, family, group), by = c("election", "family")) |>
  left_join(group_vars, by = "group") |>
  summarise(share = sum(share), .by = c(code_insee, election, var)) |>
  pivot_wider(names_from = var, values_from = share)

d_votes <- d_totals |>
  left_join(d_groups, by = c("code_insee", "election")) |>
  mutate(across(starts_with("vote"), \(x) 100 * x / exprimes),
         turnout = if_else(inscrits > 0 & votants <= inscrits, 100 * votants / inscrits, NA_real_)) |>
  rename_with(\(x) paste0("p", x), starts_with("vote")) |>
  left_join(elections |> dplyr::select(-file), by = "election")

## election-year codes we could not place
map2(elections$file, elections$election, \(file, election)
     read_csv(file, show_col_types = FALSE, col_select = c(codecommune, nomcommune),
              col_types = cols(codecommune = col_character())) |>
       mutate(codecommune = str_pad(codecommune, width = 5, pad = "0")) |>
       filter(is.na(to_modern(codecommune))) |>
       transmute(election, code = codecommune, name = nomcommune)) |>
  list_rbind() |>
  write_csv("tables/cassini_flag_list.csv")


# referendums -------------------------------------------------------------------------

## the referendums published by Cage-Piketty. 1793 and 1795 passed with 99% yes,
## there only turnout varies.
referendums <- tibble(year = c(1793, 1795, 1946, 1992, 2005)) |>
  mutate(file = paste0("cage_piketty/ref", year, "_csv/ref", year, "comm.csv"))

read_referendum <- function(file, year) {
  read_csv(file, show_col_types = FALSE, guess_max = 1e5,
           col_select = c(codecommune, inscrits, votants, exprimes, voixOUI),
           col_types = cols(codecommune = col_character())) |>
    mutate(code_insee = to_modern(str_pad(codecommune, width = 5, pad = "0")),
           across(c(inscrits, votants, exprimes, voixOUI), as.numeric)) |>
    filter(!is.na(code_insee)) |>
    summarise(across(c(inscrits, votants, exprimes, voixOUI), \(x) sum(x, na.rm = TRUE)),
              .by = code_insee) |>
    filter(exprimes > 0) |>
    transmute(code_insee, year,
              yes = 100 * voixOUI / exprimes,
              turnout = if_else(inscrits > 0 & votants <= inscrits, 100 * votants / inscrits,
                                NA_real_))
}

d_referendums <- map2(referendums$file, referendums$year, read_referendum) |>
  list_rbind()


# presidential elections -------------------------------------------------------------

presidentials <- read_csv("tables/presidential_candidates.csv", show_col_types = FALSE) |>
  distinct(year, republic, file)

## both rounds per 2025 commune: voixXXX first round, voixT2XXX run-off
read_presidential <- function(file, year) {
  read_csv(file, show_col_types = FALSE, guess_max = 1e5,
           col_types = cols(codecommune = col_character())) |>
    dplyr::select(codecommune, inscrits, votants, exprimes,
                  any_of(c("inscritsT2", "votantsT2", "exprimesT2")), starts_with("voix")) |>
    mutate(code_insee = to_modern(str_pad(codecommune, width = 5, pad = "0")),
           across(-c(codecommune, code_insee), as.numeric)) |>
    filter(!is.na(code_insee)) |>
    summarise(across(-codecommune, \(x) sum(x, na.rm = TRUE)), .by = code_insee) |>
    mutate(year = year)
}

d_presidential <- map2(presidentials$file, presidentials$year, read_presidential) |>
  list_rbind()

## candidate shares in % of exprimes of the round
d_candidates <- d_presidential |>
  pivot_longer(starts_with("voix"), names_to = "candidate", names_prefix = "voix",
               values_to = "voix") |>
  filter(!is.na(voix)) |>
  mutate(round = if_else(str_starts(candidate, "T2"), 2, 1),
         candidate = str_remove(candidate, "^T2"),
         share = 100 * voix / if_else(round == 2, exprimesT2, exprimes)) |>
  filter(is.finite(share)) |>
  dplyr::select(code_insee, year, round, candidate, share)

d_presidential <- d_presidential |>
  filter(exprimes > 0) |>
  transmute(code_insee, year,
            turnout = if_else(votants <= inscrits, 100 * votants / inscrits, NA_real_),
            turnout_t2 = if_else(votantsT2 <= inscritsT2, 100 * votantsT2 / inscritsT2, NA_real_)) |>
  left_join(presidentials |> dplyr::select(year, republic), by = "year")


# treatment and controls ----------------------------------------------------------

d_prot <- read_csv("clean_protestant_1839.csv", show_col_types = FALSE) |>
  filter(!is.na(share_protestant)) |>
  dplyr::select(code_insee, dep, nomdep, share_protestant, category, pop1839)

## only the requested columns are read, the C&P files are large
read_cp <- function(file, ...) {
  cols <- rlang::enquos(...)
  read_csv(file, show_col_types = FALSE, col_select = c(codecommune, !!!cols)) |>
    rename(code_insee = codecommune) |>
    mutate(code_insee = str_pad(code_insee, width = 5, pad = "0"))
}

read_ag <- function(file, ...) {
  read_csv(file, show_col_types = FALSE) |>
    dplyr::select(code_insee = insee_com, ...) |>
    mutate(code_insee = str_pad(code_insee, width = 5, pad = "0")) |>
    distinct(code_insee, .keep_all = TRUE)
}

protests <- read_csv("gay/nicolas_events_all_FR_labels.csv", show_col_types = FALSE) |>
  count(code_insee = str_pad(insee_2021, width = 5, pad = "0"), name = "total_protests")

geo <- read_csv("communes-france-2025.csv", show_col_types = FALSE) |>
  dplyr::select(code_insee, altitude_moyenne, latitude_centre, longitude_centre)

d_controls <- geo |>
  left_join(read_ag("albertus_gay/dist_cassini.csv", dist_cassini), by = "code_insee") |>
  left_join(read_ag("albertus_gay/distance_relays_1708.csv", HubDist), by = "code_insee") |>
  left_join(read_ag("albertus_gay/letter_posts_1710.csv", letter), by = "code_insee") |>
  left_join(read_ag("albertus_gay/marechaussee_1720.csv", brigade), by = "code_insee") |>
  left_join(protests, by = "code_insee") |>
  left_join(read_cp("cage_piketty/religiositecommunes1791.csv", pserment1791, prefract1791),
            by = "code_insee") |>
  left_join(read_cp("cage_piketty/alphabetisationcommunes.csv", pconjsign1686), by = "code_insee") |>
  left_join(read_cp("cage_piketty/revcommunes.csv", revratio1790), by = "code_insee") |>
  left_join(read_cp("cage_piketty/publicprivecommunes1894.csv", privecanton1894_prim),
            by = "code_insee") |>
  left_join(read_csv("language_1886.csv", show_col_types = FALSE,
                     col_types = cols(code_insee = "c")),
            by = "code_insee") |>
  mutate(across(c(letter, brigade, total_protests), \(x) replace_na(x, 0)),
         across(c(pconjsign1686, pserment1791, prefract1791, privecanton1894_prim), \(x) 100 * x))

years <- unique(c(elections$year, referendums$year, presidentials$year))

## variables measured in the election year: stems x years from a C&P file
read_years <- function(file, stems) {
  read_cp(file, any_of(as.vector(outer(stems, years, paste0)))) |>
    pivot_longer(-code_insee, names_to = c(".value", "year"), names_pattern = "^(.*)(\\d{4})$") |>
    mutate(year = as.numeric(year))
}

## population, and the cleavage proxies of historic-cleavages.R: income,
## agglomeration size, literacy (1866-1946), occupations and higher education
## (both from 1960; before that C&P carry the 1960s values backwards)
d_years <- list(
  read_years("cage_piketty/popcommunes.csv", c("pop", "popagglo")),
  read_years("cage_piketty/revcommunes.csv", "revratio"),
  read_years("cage_piketty/alphabetisationcommunes.csv", "palpha"),
  read_years("cage_piketty/cspcommunes.csv", c("pouvr", "pagri", "pcadr")),
  read_years("cage_piketty/diplomescommunes.csv", "psup")
) |>
  reduce(full_join, by = c("code_insee", "year")) |>
  mutate(across(c(palpha, pouvr, pagri, pcadr, psup), \(x) 100 * x))


# panels --------------------------------------------------------------------------

panel <- d_votes |>
  filter(!code_insee %in% split_flagged) |>
  inner_join(d_prot, by = "code_insee") |>
  left_join(d_controls, by = "code_insee") |>
  left_join(d_years, by = c("code_insee", "year")) |>
  mutate(log_pop = log(pop + 1))

write_rds(panel, "historic_panel.rds", compress = "gz")

d_families |>
  semi_join(panel, by = c("code_insee", "election")) |>
  write_rds("historic_families.rds", compress = "gz")

referendum_panel <- d_referendums |>
  filter(!code_insee %in% split_flagged) |>
  inner_join(d_prot, by = "code_insee") |>
  left_join(d_controls, by = "code_insee") |>
  left_join(d_years, by = c("code_insee", "year")) |>
  mutate(log_pop = log(pop + 1))

write_rds(referendum_panel, "referendum_panel.rds", compress = "gz")

presidential_panel <- d_presidential |>
  filter(!code_insee %in% split_flagged) |>
  inner_join(d_prot, by = "code_insee") |>
  left_join(d_controls, by = "code_insee") |>
  left_join(d_years, by = c("code_insee", "year")) |>
  mutate(log_pop = log(pop + 1))

write_rds(presidential_panel, "presidential_panel.rds", compress = "gz")

d_candidates |>
  semi_join(presidential_panel, by = c("code_insee", "year")) |>
  write_rds("presidential_votes.rds", compress = "gz")
