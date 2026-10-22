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
## Needs clean_protestant_1839.csv (build-protestant-data.R) and
## tables/party_family_dictionary.csv (build-party-dictionary.R).

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

## family votes summed into our groups, per 2025 commune
group_shares <- function(votes, election) {
  votes |>
    pivot_longer(starts_with("voix"), names_to = "family", names_prefix = "voix",
                 values_to = "voix") |>
    inner_join(dict |> filter(election == .env$election) |> dplyr::select(family, group),
               by = "family") |>
    left_join(group_vars, by = "group") |>
    summarise(voix = sum(voix, na.rm = TRUE), .by = c(code_insee, var)) |>
    pivot_wider(names_from = var, values_from = voix)
}

commune_votes <- function(file, election) {
  votes <- read_votes(file) |> filter(!is.na(code_insee))
  votes |>
    summarise(across(c(inscrits:voteD), \(x) sum(x, na.rm = TRUE)), .by = code_insee) |>
    left_join(group_shares(votes, election), by = "code_insee") |>
    filter(exprimes > 0) |>
    mutate(across(c(starts_with("vote"), any_of(group_vars$var)), \(x) 100 * x / exprimes),
           turnout = if_else(inscrits > 0 & votants <= inscrits, 100 * votants / inscrits, NA_real_),
           election = election) |>
    rename_with(\(x) paste0("p", x), starts_with("vote"))
}

d_votes <- map2(elections$file, elections$election, commune_votes) |>
  list_rbind() |>
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


# treatment and controls ----------------------------------------------------------

d_prot <- read_csv("clean_protestant_1839.csv", show_col_types = FALSE) |>
  filter(!is.na(share_protestant)) |>
  dplyr::select(code_insee, dep, nomdep, share_protestant, category, pop1839)

read_cp <- function(file, ...) {
  read_csv(file, show_col_types = FALSE) |>
    dplyr::select(code_insee = codecommune, ...) |>
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
  left_join(read_cp("cage_piketty/religiositecommunes1791.csv", pserment1791), by = "code_insee") |>
  left_join(read_cp("cage_piketty/alphabetisationcommunes.csv", pconjsign1686), by = "code_insee") |>
  left_join(read_cp("cage_piketty/revcommunes.csv", revratio1790), by = "code_insee") |>
  mutate(across(c(letter, brigade, total_protests), \(x) replace_na(x, 0)),
         pconjsign1686 = 100 * pconjsign1686,
         pserment1791 = 100 * pserment1791)

## population in the election year
pop <- read_cp("cage_piketty/popcommunes.csv", matches("^pop[0-9]{4}$")) |>
  pivot_longer(-code_insee, names_to = "year", names_prefix = "pop", values_to = "pop") |>
  mutate(year = as.numeric(year)) |>
  filter(year %in% elections$year)


# panel ---------------------------------------------------------------------------

panel <- d_votes |>
  filter(!code_insee %in% split_flagged) |>
  inner_join(d_prot, by = "code_insee") |>
  left_join(d_controls, by = "code_insee") |>
  left_join(pop, by = c("code_insee", "year")) |>
  mutate(log_pop = log(pop + 1))

write_rds(panel, "historic_panel.rds", compress = "gz")
