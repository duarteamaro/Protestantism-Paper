## build-party-dictionary.R
##
## One row per election x party family (the voixXXX columns of the Cage-Piketty
## commune files), with a readable label, the national vote share, the
## Cage-Piketty bloc and our own grouping. Written to
## tables/party_family_dictionary.csv, which the historic scripts read.
##
## To regroup parties, change `groups` or `group_exceptions` below and re-run
## this script and build-historic-panel.R.

library(tidyverse)

## 1871 is left out: only about half of the communes have returns and the
## missing ones are less Protestant. The July 1871 by-elections and 1877 are
## not in the data.
elections <- tribble(
  ~file,                                          ~election, ~year, ~republic,
  "cage_piketty/leg1848comm.csv",                 "1848",  1848, "Second",
  "cage_piketty/leg1849_csv/leg1849comm.csv",     "1849",  1849, "Second",
  "cage_piketty/leg1876_csv/leg1876comm.csv",     "1876",  1876, "Third",
  "cage_piketty/leg1881_csv/leg1881comm.csv",     "1881",  1881, "Third",
  "cage_piketty/leg1885comm.csv",                 "1885",  1885, "Third",
  "cage_piketty/leg1889_csv/leg1889comm.csv",     "1889",  1889, "Third",
  "cage_piketty/leg1893_csv/leg1893comm.csv",     "1893",  1893, "Third",
  "cage_piketty/leg1898_csv/leg1898comm.csv",     "1898",  1898, "Third",
  "cage_piketty/leg1902_csv/leg1902comm.csv",     "1902",  1902, "Third",
  "cage_piketty/leg1906_csv/leg1906comm.csv",     "1906",  1906, "Third",
  "cage_piketty/leg1910_csv/leg1910comm.csv",     "1910",  1910, "Third",
  "cage_piketty/leg1914_csv/leg1914comm.csv",     "1914",  1914, "Third",
  "cage_piketty/leg1919_csv/leg1919comm.csv",     "1919",  1919, "Third",
  "cage_piketty/leg1924_csv/leg1924comm.csv",     "1924",  1924, "Third",
  "cage_piketty/leg1928_csv/leg1928comm.csv",     "1928",  1928, "Third",
  "cage_piketty/leg1932_csv/leg1932comm.csv",     "1932",  1932, "Third",
  "cage_piketty/leg1936_csv/leg1936comm.csv",     "1936",  1936, "Third",
  "cage_piketty/leg1945_csv/leg1945comm.csv",     "1945",  1945, "Fourth",
  "cage_piketty/leg1946Juin_csv/leg1946comm.csv", "1946J", 1946, "Fourth",
  "cage_piketty/leg1946Nov_csv/leg1947comm.csv",  "1946N", 1946, "Fourth",
  "cage_piketty/leg1951_csv/leg1951comm.csv",     "1951",  1951, "Fourth",
  "cage_piketty/leg1956_csv/leg1956comm.csv",     "1956",  1956, "Fourth",
  "cage_piketty/leg1958comm.csv",                 "1958",  1958, "Fifth",
  "cage_piketty/leg1962comm.csv",                 "1962",  1962, "Fifth",
  "cage_piketty/leg1967comm.csv",                 "1967",  1967, "Fifth",
  "cage_piketty/leg1968comm.csv",                 "1968",  1968, "Fifth",
  "cage_piketty/leg1973comm.csv",                 "1973",  1973, "Fifth",
  "cage_piketty/leg1978comm.csv",                 "1978",  1978, "Fifth",
  "cage_piketty/leg1981comm.csv",                 "1981",  1981, "Fifth",
  "cage_piketty/leg1986comm.csv",                 "1986",  1986, "Fifth",
  "cage_piketty/leg1988comm.csv",                 "1988",  1988, "Fifth",
  "cage_piketty/leg1993comm.csv",                 "1993",  1993, "Fifth",
  "cage_piketty/leg1997comm.csv",                 "1997",  1997, "Fifth",
  "cage_piketty/leg2002comm.csv",                 "2002",  2002, "Fifth",
  "cage_piketty/leg2007comm.csv",                 "2007",  2007, "Fifth",
  "cage_piketty/leg2012comm.csv",                 "2012",  2012, "Fifth",
  "cage_piketty/leg2017comm.csv",                 "2017",  2017, "Fifth",
  "cage_piketty/leg2022comm.csv",                 "2022",  2022, "Fifth"
)

family_labels <- c(
  CONS = "Conservateurs", DEMSOC = "Democrates-socialistes (Montagne)",
  REPCONS = "Republicains conservateurs", REPMOD = "Republicains moderes", DIV = "Divers",
  RAD = "Radicaux", REP = "Republicains", LEG = "Legitimistes", MON = "Monarchistes/Orleanistes",
  CLE = "Clericaux", LIB = "Liberaux", BON = "Bonapartistes", PLEB = "Plebiscitaires (bonapartistes)",
  PLEBI = "Plebiscitaires (bonapartistes)", CONST = "Constitutionnels", CENT = "Centre",
  CED = "Centre droit", SOCOUV = "Socialistes ouvriers", UR = "Union republicaine (gambettistes)",
  DVG = "Divers gauche", DVD = "Divers droite", REAC = "Reactionnaires", OPP = "Opportunistes",
  REPIND = "Republicains independants", RADREP = "Radicaux-republicains",
  RADSOC = "Radicaux-socialistes", RADDIV = "Radicaux divers", SOC = "Socialistes",
  BOUL = "Boulangistes", REVIS = "Revisionnistes", REPDIV = "Republicains divers",
  RALLIE = "Rallies (catholiques rallies)", REPLIB = "Republicains liberaux",
  SOCREV = "Socialistes revolutionnaires", ANT = "Nationalistes antisemites",
  NATI = "Nationalistes", REPMIN = "Republicains ministeriels", SOCGUES = "Socialistes guesdistes",
  RADDIS = "Radicaux dissidents", SOCUNI = "Socialistes unifies (SFIO)",
  REPPROG = "Republicains progressistes", PROG = "Progressistes", SFIO = "SFIO (socialistes)",
  SOCIND = "Socialistes independants", RADIND = "Radicaux independants",
  RADARD = "Radicaux (Alliance dem.)", ARD = "Alliance republicaine democratique",
  FR = "Federation republicaine", FRLIB = "Federation rep. / liberaux",
  FRARD = "Federation rep. / Alliance dem.", ALP = "Action liberale populaire (catholiques)",
  ADPRD = "Alliance dem. / Parti rep. dem.", FRPR = "Federation rep. / progressistes",
  REPSOC = "Republicains-socialistes", UNR = "Union nat. republicaine (Bloc national)",
  ERD = "Entente republicaine democratique", BN = "Bloc national", AF = "Action francaise",
  URERD = "Union/Entente rep. democratique", BOP = "Bloc ouvrier et paysan (communistes)",
  REPRAD = "Republicains radicaux", CONSAF = "Conservateurs / Action francaise",
  ADRG = "Alliance dem. / Radicaux de gauche", ADIND = "Alliance dem. independants",
  ADPDP = "Alliance dem. / Parti dem. populaire", AGR = "Agrariens", AD = "Alliance democratique",
  USR = "Union socialiste republicaine", MRP = "MRP (democrates-chretiens)",
  ADPAYS = "Parti agraire et paysan", URDDVD = "Union rep. dem. / divers droite",
  RG = "Rassemblement des gauches", PRL = "Parti republicain de la liberte",
  UDSR = "Union democratique et socialiste de la Resistance",
  FRURD = "Federation republicaine / Union republicaine democratique",
  POUJ = "Poujadistes (UDCA)",
  RGR = "Rassemblement des gauches republicaines", GAUL = "Gaullistes (RPF)",
  GAULG = "Gaullistes de gauche", INDPAYS = "Independants et paysans",
  PCI = "Parti communiste internationaliste", RPF = "Rassemblement du peuple francais (gaullistes)",
  UIPRN = "Independants, paysans et rep. nationaux", RGRIF = "RGR / independants",
  UNIR = "Union des nat. indep. et rep. (gaullistes diss.)", CNI = "Centre national des independants",
  FRREP = "Front republicain", RFGAUL = "Republicains sociaux (gaullistes)",
  UFFUDCA = "Union et fraternite francaise (poujadistes)", AUD = "Autres / divers",
  PCFSFIO = "Listes communes PCF-SFIO", AUG = "Autres gauche", PCF = "Parti communiste francais",
  PRRS = "Parti radical et radical-socialiste", COM = "Communistes",
  UNRUDT = "UNR-UDT (gaullistes)", UD5UNR = "Union dem. pour la Ve Rep. (gaullistes)",
  UDR = "Union des democrates pour la Republique (gaullistes)",
  UDRURP = "UDR / Union des rep. de progres (gaullistes)", MOD = "Moderes",
  RADCENT = "Radicaux (centre)", CRR = "Centre republicain",
  UFDRAD = "Union des forces dem. / radicaux", UDSRMIN = "UDSR (minorite)",
  INDV = "Independants divers (droite)", PSU = "Parti socialiste unifie",
  FGDS = "Federation de la gauche dem. et socialiste", CDEM = "Centre democrate",
  CPDM = "Centre Progres et Democratie moderne", AUDAR = "Autres droite / audience",
  RADDROIT = "Radicaux de droite", REG = "Regionalistes / autonomistes", TED = "Divers (TED)",
  MR = "Mouvement reformateur", PS = "Parti socialiste", RIURP = "Rep. independants / URP",
  CDPURP = "Centre dem. et progres / URP", MRG = "Mouvement des radicaux de gauche",
  LO = "Lutte ouvriere (extreme gauche)", RPR = "Rassemblement pour la Republique (gaullistes)",
  UDF = "Union pour la democratie francaise", ECO = "Ecologistes", PSRDG = "PS / Radicaux de gauche",
  UDFRPR = "Listes communes UDF-RPR", FN = "Front national",
  MAJ = "Majorite presidentielle (droite)", RDG = "Radicaux de gauche",
  VEC = "Les Verts (ecologistes)", GEC = "Generation ecologie",
  UMP = "Union pour un mouvement populaire", UDFD = "UDF / divers droite",
  PRGDVG = "PRG / divers gauche", CPNT = "Chasse, Peche, Nature, Traditions",
  MNRAUD = "Mouvement national republicain (extr. droite)",
  LCR = "Ligue communiste revolutionnaire (extr. gauche)",
  PREP = "Pole republicain (souverainistes)", MPF = "Mouvement pour la France (souverainistes)",
  FG = "Front de gauche (PCF-Melenchon)", EELV = "Europe Ecologie Les Verts",
  CENALLI = "Centre / Alliance centriste", NCE = "Nouveau Centre", PRV = "Parti radical valoisien",
  LREM = "La Republique en marche", LR = "Les Republicains", LFI = "La France insoumise",
  MDM = "Mouvement democrate (MoDem)", UDI = "Union des democrates et independants",
  RDGDVG = "Radicaux de gauche / divers gauche", DLF = "Debout la France (souverainistes)",
  NUP = "NUPES (union de la gauche)", ENS = "Ensemble (majorite presidentielle, macronistes)",
  RN = "Rassemblement national", REC = "Reconquete (extr. droite)",
  DIVGAUL = "Divers gaullistes"
)

## our grouping; families not listed end up in "Other/Divers"
groups <- list(
  `Communist/Extreme left` = c("COM", "PCF", "BOP", "PCI", "PCFSFIO", "AUG", "FG", "LFI",
                               "LO", "LCR"),
  `Socialist left` = c("SOC", "SOCOUV", "SOCREV", "SOCGUES", "SOCUNI", "SOCIND", "SFIO",
                       "DEMSOC", "DVG", "PS", "PSRDG", "FGDS", "NUP", "PSU"),
  `Green/ecologist` = c("ECO", "VEC", "GEC", "EELV"),
  `Republican/Radical left` = c("RAD", "RADSOC", "RADDIV", "RADREP", "RADIND", "RADARD",
                                "RADDIS", "UR", "PRRS", "REPSOC", "USR", "REPRAD", "MRG",
                                "RDG", "PRGDVG", "RDGDVG"),
  `Centre republican` = c("REP", "REPMOD", "REPCONS", "CONST", "CENT", "CED", "OPP",
                          "REPDIV", "REPMIN", "RG", "RGR", "RGRIF", "UDSR", "FRREP", "MRP",
                          "CDEM", "CPDM", "CDPURP", "MOD", "NCE", "CENALLI", "MDM", "UDI",
                          "CRR", "MR", "RADDROIT", "LREM", "ENS", "RADCENT", "UFDRAD"),
  `Republican right` = c("RALLIE", "REPLIB", "LIB", "ALP", "ADPRD", "FRPR", "FR", "FRLIB",
                         "FRARD", "ARD", "AD", "ADRG", "ADIND", "ADPDP", "AGR", "PROG",
                         "REPPROG", "ERD", "URERD", "FRURD", "BN", "UNR", "UNRUDT", "UD5UNR",
                         "UDR", "UDRURP", "RPR", "UDF", "UDFRPR", "UDFD", "REPIND", "RIURP",
                         "CNI", "LR", "UMP", "DVD", "URDDVD", "INDPAYS", "INDV", "PRL", "RPF",
                         "MPF", "MAJ", "PRV", "RFGAUL", "UNIR", "GAULG", "ADPAYS", "UIPRN",
                         "DIVGAUL"),
  `Anti-republican right` = c("CONS", "LEG", "MON", "BON", "PLEB", "PLEBI", "CLE", "REAC",
                              "BOUL", "REVIS", "ANT", "NATI", "AF", "CONSAF", "UFFUDCA",
                              "POUJ", "FN", "RN", "REC", "MNRAUD", "DLF")
)

group_lookup <- enframe(groups, name = "group", value = "family") |>
  unnest(family)

## a family code that means something else in a given election. In 1881 the
## "divers gauche" are moderate republicans (Cage-Piketty put them in the
## centre), not socialists.
group_exceptions <- tribble(
  ~election, ~family, ~group_exception,
  "1881",    "DVG",   "Centre republican"
)


# national shares and Cage-Piketty blocs ---------------------------------------

## voixXXX are the family votes and add up to exprimes. In list elections the
## files also carry voixtotXXX (summed over all candidates of a list), which
## are left out.
read_families <- function(file) {
  read_csv(file, show_col_types = FALSE, guess_max = 1e5) |>
    dplyr::select(exprimes, starts_with("voix"), -starts_with("voixtot"),
                  voteG, voteCG, voteC, voteCD, voteD) |>
    mutate(across(everything(), \(x) replace_na(as.numeric(x), 0))) |>
    filter(exprimes > 0)
}

## Cage-Piketty build each bloc as a sum of party families, so regressing the
## bloc totals on the family votes recovers which bloc a family belongs to
## (DIV is split equally over the five blocs)
cp_bloc <- function(raw) {
  x <- raw |> dplyr::select(starts_with("voix")) |> as.matrix()
  w <- raw |>
    dplyr::select(starts_with("vote")) |>
    map(\(y) replace_na(coef(lm.fit(x, y)), 0)) |>
    as_tibble() |>
    as.matrix()
  tibble(family = str_remove(colnames(x), "^voix"),
         cp_bloc = str_remove(colnames(w)[max.col(w, ties.method = "first")], "^vote"),
         cp_bloc_weight = round(apply(w, 1, max), 2))
}

family_table <- function(file) {
  raw <- read_families(file)
  raw |>
    summarise(across(starts_with("voix"), \(x) round(100 * sum(x) / sum(raw$exprimes), 2))) |>
    pivot_longer(everything(), names_to = "family", values_to = "natl_vote_share_pct",
                 names_prefix = "voix") |>
    left_join(cp_bloc(raw), by = "family")
}

dict <- elections |>
  mutate(families = map(file, family_table)) |>
  unnest(families) |>
  left_join(group_lookup, by = "family") |>
  left_join(group_exceptions, by = c("election", "family")) |>
  mutate(label = coalesce(unname(family_labels[family]), family),
         group = coalesce(group_exception, group, "Other/Divers")) |>
  dplyr::select(election, year, republic, file, family, label,
                natl_vote_share_pct, cp_bloc, cp_bloc_weight, group)

write_csv(dict, "tables/party_family_dictionary.csv")
