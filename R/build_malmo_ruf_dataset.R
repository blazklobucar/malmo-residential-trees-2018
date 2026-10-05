# =============================================================================
# Malmö residential urban forest inventory 2018 — dataset build script
#
# Reads the raw field data (i-Tree Eco database + Excel files) and writes
#   output/internal/  full tables incl. personal data   -> NEVER publish
#   output/public/    pseudonymised tables for deposit  -> SND / Zenodo
#   output/qc/        quality-control flags and checks
#
# Usage:  Rscript build_malmo_ruf_dataset.R [raw_dir] [out_dir]
# Needs:  mdbtools (mdb-export) on PATH; R packages readxl dplyr tidyr readr stringr
# =============================================================================

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(readr); library(stringr)
})

args    <- commandArgs(trailingOnly = TRUE)
raw_dir <- if (length(args) >= 1) args[1] else "raw"
out_dir <- if (length(args) >= 2) args[2] else "output"

f_ieco     <- file.path(raw_dir, "Malmo_v622019-09-18.ieco")
f_registry <- file.path(raw_dir, "Points_registry.xlsx")
f_fullinv  <- file.path(raw_dir, "Full_inventory_RUF_2018.xlsx")
f_survey   <- list.files(raw_dir, pattern = "Enk.*Svar.*xlsx$", full.names = TRUE)[1]

SURVEY_YEAR <- 2018
# The public plot IDs are a random permutation of the field point numbers. The seed that fixes the
# permutation is a SECRET: anyone who knows it can recover the field numbers from the public IDs.
# Set it before running, e.g. Sys.setenv(RUF_PUBLIC_SEED = "<private number>"), and never commit it.
PUBLIC_SEED <- suppressWarnings(as.integer(Sys.getenv("RUF_PUBLIC_SEED")))
if (is.na(PUBLIC_SEED)) stop("Set the private seed first: Sys.setenv(RUF_PUBLIC_SEED = \"<number>\")")
MF_PLOT_ID_OFFSET <- 300         # i-Tree multi-household PlotId = registry PID + 300

for (d in c("internal", "public", "qc")) dir.create(file.path(out_dir, d), recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------
read_mdb <- function(file, table) {
  if (Sys.which("mdb-export") == "") stop("mdb-export not found: install mdbtools")
  txt <- system2("mdb-export", c(shQuote(file), shQuote(table)), stdout = TRUE)
  read_csv(I(paste(txt, collapse = "\n")), show_col_types = FALSE, guess_max = 1e5)
}

# "55° 32' 23.912\" N" -> 55.539976
dms_to_dd <- function(x) {
  m <- str_match(x, "(\\d+)°\\s*(\\d+)'\\s*([\\d.]+)\"?\\s*([NSEW])")
  dd <- as.numeric(m[, 2]) + as.numeric(m[, 3]) / 60 + as.numeric(m[, 4]) / 3600
  ifelse(m[, 5] %in% c("S", "W"), -dd, dd)
}

clean_taxon <- function(x) {
  x <- str_squish(x)
  x <- str_replace(x, "atlamtica", "atlantica")
  # close unbalanced cultivar quotes, e.g. "Sorbus commixta 'Carmencita"
  x <- ifelse(str_count(x, "'") %% 2 == 1, paste0(x, "'"), x)
  x
}
binomial <- function(x) {
  w <- str_split(x, " ")
  vapply(w, function(v) {
    if (length(v) < 2 || str_detect(v[2], "^['(]") || str_detect(v[2], "^[A-Z]")) return(paste(v[1], "sp."))
    if (v[2] %in% c("x", "×") && length(v) >= 3) return(paste(v[1], "×", v[3]))
    paste(v[1], v[2])
  }, character(1))
}

flags <- list()
flag <- function(table, id, issue, action) {
  flags[[length(flags) + 1]] <<- tibble(table = table, id = as.character(id), issue = issue, action = action)
}

# -----------------------------------------------------------------------------
# 1. i-Tree Eco database
# -----------------------------------------------------------------------------
it <- lapply(setNames(nm = c("EcoPlots", "EcoStrata", "EcoTrees", "EcoStems", "EcoConditions",
                             "EcoYearResults")),
             function(t) read_mdb(f_ieco, t))

strata <- it$EcoStrata |> select(StrataKey, stratum = Description, stratum_area_ha = Size)

it_plots <- it$EcoPlots |>
  left_join(strata, by = "StrataKey") |>
  transmute(PlotKey, itree_plot_id = PlotId, stratum,
            housing_type = if_else(stratum == "SRes", "single_household", "multi_household"),
            pid = if_else(stratum == "SRes", PlotId, PlotId - MF_PLOT_ID_OFFSET),
            plot_area_m2 = PlotSize * 1e4,                     # i-Tree stores hectares
            plot_tree_cover_pct = if_else(stratum == "SRes", NA_real_, PercentTreeCover),
            plot_pct_measured   = if_else(stratum == "SRes", NA_real_, PercentMeasured),
            itree_date = as.Date(Date, "%m/%d/%y"))

# i-Tree plot 519 (PID 219) is an empty 100 m2 plot recorded on the same day (2018-09-18) as the
# complete inventory of single-household property PID 219, but filed under the multi-household stratum.
# The property inventory supersedes it, so the 519 record and anything attached to it are dropped.
DROP_ITREE_PLOTS <- c(519)
drop_keys <- it_plots$PlotKey[it_plots$itree_plot_id %in% DROP_ITREE_PLOTS]
it_plots  <- it_plots |> filter(!itree_plot_id %in% DROP_ITREE_PLOTS)
it$EcoTrees <- it$EcoTrees |> filter(!PlotKey %in% drop_keys)

cond <- it$EcoConditions |> select(ConditionKey, condition_class = Description, dieback_pct = PctDieback)

it_trees <- it$EcoTrees |>
  left_join(it_plots |> select(PlotKey, itree_plot_id, pid, housing_type), by = "PlotKey") |>
  left_join(cond, by = c("CrownCondition" = "ConditionKey")) |>
  mutate(across(where(is.numeric), ~ if_else(.x == -1, NA_real_, as.numeric(.x)))) |>
  arrange(itree_plot_id, TreeId)

it_stems <- it$EcoStems |>
  select(TreeKey, stem_no = StemId, dbh_cm = Diameter) |>
  mutate(dbh_cm = round(dbh_cm, 1))

# -----------------------------------------------------------------------------
# 2. Plot registry (sample frame) — contains personal data
# -----------------------------------------------------------------------------
reg_raw <- read_excel(f_registry, sheet = "Sheet1") |> filter(!is.na(PID))

registry <- reg_raw |>
  transmute(
    pid = as.integer(PID),
    housing_type_registry = case_when(`Single houshold` == 1 ~ "single_household",
                                      `Multiple household` == 1 ~ "multi_household",
                                      TRUE ~ NA_character_),
    lat = dms_to_dd(Y_plot), lon = dms_to_dd(X_plot),
    property_area_m2 = LotSize_m2,
    owner_name = Name, street = Street, postcode_city = `Post number and city`,
    housing_company = `Housing company`, other_note = Other,
    risk_of_overlap = `Risk of overlap`, completed_raw = `Completed inventory`,
    declined = !is.na(DNC), lacking_survey_registry = !is.na(`lacking survey`),
    registry_comment = comments,
    postal_area = str_extract(postcode_city, "^\\d{3}")
  )

# -----------------------------------------------------------------------------
# 3. Full property inventory (single-household) — authoritative species + DBH
# -----------------------------------------------------------------------------
fi <- read_excel(f_fullinv) |>
  rename_with(str_squish) |>
  mutate(row_in_file = row_number(),
         pid = as.integer(PID),
         inv_date = as.Date(Date, "%d/%m/%Y"),
         species_reported = clean_taxon(Species)) |>
  arrange(pid, row_in_file)

# The i-Tree SRes trees were entered from this sheet in the same order: verify, then join row-wise
sres_trees <- it_trees |> filter(housing_type == "single_household")
stopifnot(nrow(sres_trees) == nrow(fi), all(sres_trees$pid == fi$pid))
dbh1_check <- it_stems |> filter(stem_no == 1) |> right_join(sres_trees |> select(TreeKey), by = "TreeKey")
stopifnot(all(abs(dbh1_check$dbh_cm - fi$`DBH 1`) < 0.05))
sres_trees$species_reported <- fi$species_reported
sres_trees$inv_date <- fi$inv_date

# -----------------------------------------------------------------------------
# 4. Species lookup (i-Tree code -> taxon)
# -----------------------------------------------------------------------------
code_from_inventory <- sres_trees |>
  mutate(taxon = binomial(species_reported)) |>
  count(Species, taxon) |>
  group_by(Species) |> slice_max(n, n = 1, with_ties = FALSE) |> ungroup() |>
  transmute(itree_code = Species, taxon, lookup_source = "majority name in full inventory", verify = FALSE)

# Codes that only occur in multi-household plots: best interpretation, to be verified
code_manual <- tribble(
  ~itree_code, ~taxon,
  "ACTA",  "Acer tataricum",
  "AL",    "Alnus sp.",
  "COMA",  "Cornus mas",
  "FREX1", "Fraxinus excelsior",
  "PRMA",  "Prunus maackii",
  "PTFR",  "Pterocarya fraxinifolia",
  "SOAR",  "Sorbus aria",
  "TIEU1", "Tilia × europaea"
) |> mutate(lookup_source = "manual interpretation of i-Tree code", verify = TRUE)

species_lookup <- bind_rows(code_from_inventory, code_manual) |>
  mutate(genus = word(taxon, 1)) |> arrange(itree_code)

missing_codes <- setdiff(unique(it_trees$Species), species_lookup$itree_code)
if (length(missing_codes)) stop("Unmapped i-Tree codes: ", paste(missing_codes, collapse = ", "))

# QC: inventory names whose genus disagrees with the i-Tree code they were entered as
sres_trees |>
  left_join(species_lookup |> select(itree_code, code_genus = genus), by = c("Species" = "itree_code")) |>
  filter(word(species_reported, 1) != code_genus) |>
  rowwise() |>
  group_walk(~ flag("trees", paste0("PID ", .x$pid, " tree ", .x$TreeId),
                    paste0("Inventory name '", .x$species_reported, "' entered in i-Tree as ", .x$Species,
                           " (", .x$code_genus, ")"),
                    "Inventory name used; i-Tree code was wrong"))

# -----------------------------------------------------------------------------
# 5. Assemble trees and stems
# -----------------------------------------------------------------------------
mf_trees <- it_trees |> filter(housing_type == "multi_household") |>
  mutate(species_reported = NA_character_, inv_date = NA)

trees_internal <- bind_rows(sres_trees, mf_trees) |>
  left_join(species_lookup |> select(itree_code, code_taxon = taxon), by = c("Species" = "itree_code")) |>
  left_join(it_stems |> group_by(TreeKey) |>
              summarise(n_stems = n(), dbh_eq_cm = round(sqrt(sum(dbh_cm^2)), 1), .groups = "drop"),
            by = "TreeKey") |>
  mutate(
    taxon = if_else(is.na(species_reported), code_taxon, binomial(species_reported)),
    genus = word(taxon, 1),
    measured = housing_type == "multi_household",
    # single-household trees: only species + DBH were measured; the rest are i-Tree defaults
    height_m              = if_else(measured, TreeHeightTotal, NA_real_),
    crown_base_height_m   = if_else(measured, HeighttoCrownBase, NA_real_),
    crown_width_1_m       = if_else(measured, CrownWidth1, NA_real_),
    crown_width_2_m       = if_else(measured, CrownWidth2, NA_real_),
    crown_light_exposure  = if_else(measured, CrownLightExposure, NA_real_),
    crown_missing_pct     = if_else(measured, PercentCrownMissing, NA_real_),
    crown_dieback_pct     = if_else(measured, dieback_pct, NA_real_),
    street_tree           = if_else(measured, StreetTree == 1, NA),
    distance_from_centre_m = if_else(measured, DistancefromCenter, NA_real_),
    direction_from_centre_deg = if_else(measured, DirectionfromCenter, NA_real_)
  ) |>
  group_by(pid, housing_type) |> mutate(tree_no = row_number()) |> ungroup()

stems_internal <- it_stems |>
  inner_join(trees_internal |> select(TreeKey, pid, housing_type, tree_no), by = "TreeKey")

# -----------------------------------------------------------------------------
# 6. Plots: one row per sample point
# -----------------------------------------------------------------------------
tree_counts <- trees_internal |> count(pid, housing_type, name = "n_trees")
inv_dates   <- trees_internal |> filter(!is.na(inv_date)) |> group_by(pid) |>
  summarise(inv_date = min(inv_date), .groups = "drop")

plots_internal <- registry |>
  full_join(it_plots |> select(-PlotKey), by = "pid", relationship = "one-to-many") |>
  left_join(tree_counts, by = c("pid", "housing_type")) |>
  left_join(inv_dates, by = "pid") |>
  mutate(
    housing_type = coalesce(housing_type, housing_type_registry),
    n_trees = if_else(!is.na(itree_plot_id), coalesce(n_trees, 0L), NA_integer_),
    status = case_when(!is.na(itree_plot_id) ~ "inventoried",
                       completed_raw == "NOT RESIDENTIAL" ~ "excluded_not_residential",
                       declined | completed_raw == "does not want to cooperate" ~ "declined_or_no_response",
                       TRUE ~ "not_inventoried"),
    # single-household: the inventory sheet date is the visit date (i-Tree holds a 2018-01-01 placeholder)
    field_date = if_else(housing_type == "single_household", inv_date, itree_date),
    field_date = if_else(field_date >= as.Date("2018-08-01") & field_date <= as.Date("2018-11-30"), field_date, as.Date(NA)),
    plot_type = case_when(housing_type == "single_household" ~ "whole_property",
                          housing_type == "multi_household" ~ "circular_100m2")
  ) |>
  arrange(pid, itree_plot_id)

# Known issues
bad_dates <- fi |> filter(!(inv_date >= as.Date("2018-08-01") & inv_date <= as.Date("2018-11-30"))) |> distinct(pid, Date)
for (i in seq_len(nrow(bad_dates))) flag("plots", paste("PID", bad_dates$pid[i]),
  paste0("Inventory date '", bad_dates$Date[i], "' outside the Sep-Nov 2018 field season (likely typo)"), "field date set to NA")
flag("plots", "PID 219",
  "i-Tree plot 519: empty 100 m2 plot filed as multi-household on the same day as the full inventory of single-household PID 219",
  "Plot 519 removed; property inventory (9 trees) kept")
stopifnot(!any(duplicated(plots_internal$pid)))
for (p in plots_internal$pid[which(plots_internal$housing_type != plots_internal$housing_type_registry)])
  flag("plots", paste("PID", p), "Housing type in i-Tree differs from registry", "i-Tree stratum used")
for (p in plots_internal$pid[plots_internal$housing_type == "single_household" & plots_internal$n_trees %in% 0])
  NULL  # zero-tree properties are valid observations, not errors
flag("itree_model", "SRes stratum",
     "Single-household height, crown base, crown width, crown light exposure, condition and plot tree cover (28%) in the i-Tree file are placeholder/default values, not measurements",
     "Set to NA in public tree table; state in report")
mf_canopy_no_trees <- plots_internal |> filter(housing_type == "multi_household", n_trees == 0, plot_tree_cover_pct > 0)
for (i in seq_len(nrow(mf_canopy_no_trees))) flag("plots", paste("PID", mf_canopy_no_trees$pid[i]),
  "Multi-household plot with tree cover > 0 but no tree stems inside plot", "Valid (overhanging canopy); no action")

# -----------------------------------------------------------------------------
# 7. Survey (single-household owners)
# -----------------------------------------------------------------------------
sv_raw <- read_excel(f_survey)
stopifnot(ncol(sv_raw) == 62)

q4 <- c("trees_in_garden", "street_trees", "shrubs", "potted_plants", "food_garden", "pollinator_garden",
        "raised_rain_garden", "rain_garden", "rain_barrel", "permeable_surfaces", "swale", "green_roof", "green_wall")
q8 <- c("work_in_garden", "relax", "be_physically_active", "watch_wildlife", "socialise_family_friends",
        "socialise_neighbours", "children_play", "barbecue", "keep_pets", "eat", "read", "grow_food")
q9 <- c("colourful", "free_of_toxins", "free_of_weeds", "conserve_ecosystems", "add_property_value",
        "easy_to_maintain", "show_property_is_cared_for", "need_little_watering", "mostly_lawn",
        "shaded_and_cool", "low_maintenance_cost", "no_plants", "many_native_plants", "nice_neighbourhood")
stopifnot(sum(str_starts(names(sv_raw), "4\\.")) == length(q4),
          sum(str_starts(names(sv_raw), "8\\.")) == length(q8),
          sum(str_starts(names(sv_raw), "9\\.")) == length(q9))

sv_names <- c("timestamp", "pid_raw",
  "q01_gardening_inspiration", "q02_contact_with_municipality", "q02_free_text", "q03_attitude_to_municipal_advice",
  paste0("q04_likely_to_add_", q4),
  "q05_trees_improve_own_garden", "q06_trees_improve_outside_garden", "q07_tree_benefits",
  paste0("q08_want_to_", q8),
  paste0("q09_want_garden_", q9),
  "q10_planted_trees_last_5yr", "q10_n_trees_planted", "q11_removed_trees_reasons", "q12_tree_knowledge",
  "q13a_positive_comments", "q13b_negative_comments", "q14_birth_year", "q15_gender", "q16_education",
  "q17_move_in_year", "q18_property_age_raw", "q19_household_size", "q20_greet_neighbours", "q21_contact_details")
survey_questions <- tibble(variable = sv_names, question_sv = names(sv_raw))
names(sv_raw) <- sv_names

survey_internal <- sv_raw |>
  mutate(pid = suppressWarnings(as.integer(pid_raw)),
         response_date = as.Date(timestamp))
for (r in which(is.na(survey_internal$pid))) flag("survey", paste("row", r),
  paste0("Survey response with PID '", survey_internal$pid_raw[r], "' cannot be linked to a plot"),
  "Kept, unlinked")

# Year answers: 4-digit years kept if <= survey year; bare 2-digit numbers 20-99 read as 19xx
# (birth and move-in years only); anything else (free text, ages, single digits) -> NA
parse_year <- function(x, allow_two_digit = TRUE) {
  x  <- str_squish(as.character(x))
  y4 <- as.integer(str_extract(x, "\\b(1[89]|20)\\d{2}\\b"))
  y2 <- if_else(allow_two_digit & str_detect(coalesce(x, ""), "^[2-9]\\d$"), 1900L + suppressWarnings(as.integer(x)), NA_integer_)
  y  <- coalesce(y4, y2)
  if_else(!is.na(y) & y <= SURVEY_YEAR, y, NA_integer_)
}
bad_move <- which(!is.na(survey_internal$q17_move_in_year) & is.na(parse_year(survey_internal$q17_move_in_year)))
for (r in bad_move) flag("survey", paste("row", r),
  paste0("Move-in year answer not usable as a year: '", survey_internal$q17_move_in_year[r], "'"), "Set to NA in public file")

# -----------------------------------------------------------------------------
# 8. Public (pseudonymised) tables
# -----------------------------------------------------------------------------
set.seed(PUBLIC_SEED)
key <- plots_internal |> distinct(pid, housing_type) |>
  mutate(plot_id = sprintf("RUF%03d", sample(n()))) 

# 3-digit postal areas with < 5 sample points are merged
pa_counts <- plots_internal |> count(postal_area)
small_pa  <- pa_counts$postal_area[pa_counts$n < 5 | is.na(pa_counts$postal_area)]

round_to <- function(x, b) round(x / b) * b

plots_public <- plots_internal |>
  left_join(key, by = c("pid", "housing_type")) |>
  transmute(plot_id, housing_type, status, plot_type,
            postal_area = if_else(postal_area %in% small_pa, "other", paste0(postal_area, "xx")),
            plot_area_m2 = case_when(plot_type == "whole_property" ~ round_to(plot_area_m2, 50),
                                     plot_type == "circular_100m2" ~ 100),
            property_area_class = cut(property_area_m2, c(0, 500, 750, 1000, 1500, 5000, 20000, Inf),
                                      labels = c("<500", "500-750", "750-1000", "1000-1500", "1500-5000",
                                                 "5000-20000", ">20000"), right = FALSE),
            field_month = format(field_date, "%Y-%m"),
            n_trees, plot_tree_cover_pct, plot_pct_measured,
            survey_returned = if_else(housing_type == "single_household" & status == "inventoried",
                                      pid %in% survey_internal$pid, NA)) |>
  arrange(plot_id)

trees_public <- trees_internal |>
  left_join(key, by = c("pid", "housing_type")) |>
  transmute(tree_id = sprintf("%s_T%03d", plot_id, tree_no), plot_id, housing_type,
            species_reported, taxon, genus, itree_code = Species,
            n_stems, dbh_eq_cm, height_m, crown_base_height_m, crown_width_1_m, crown_width_2_m,
            crown_light_exposure, crown_missing_pct, crown_dieback_pct, street_tree,
            distance_from_centre_m, direction_from_centre_deg) |>
  arrange(tree_id)

stems_public <- stems_internal |>
  left_join(key, by = c("pid", "housing_type")) |>
  transmute(tree_id = sprintf("%s_T%03d", plot_id, tree_no), stem_no, dbh_cm) |>
  arrange(tree_id, stem_no)

survey_public <- survey_internal |>
  left_join(key |> filter(housing_type == "single_household"), by = "pid") |>
  mutate(q14_birth_decade   = floor(parse_year(q14_birth_year) / 10) * 10,
         q17_move_in_decade = floor(parse_year(q17_move_in_year) / 10) * 10,
         q18_build_decade   = floor(parse_year(q18_property_age_raw, allow_two_digit = FALSE) / 10) * 10) |>
  select(plot_id, starts_with("q"), -q02_free_text, -q14_birth_year, -q17_move_in_year,
         -q18_property_age_raw, -q21_contact_details) |>
  relocate(q14_birth_decade, .after = q13b_negative_comments) |>
  relocate(q17_move_in_decade, q18_build_decade, .after = q16_education) |>
  arrange(plot_id)

# -----------------------------------------------------------------------------
# 9. Write
# -----------------------------------------------------------------------------
w <- function(x, sub, name) write_csv(x, file.path(out_dir, sub, paste0(name, ".csv")), na = "")

w(plots_internal, "internal", "plots_internal")
w(trees_internal |> select(-TreeKey, -PlotKey, -PlotLandUseKey, -CrownCondition), "internal", "trees_internal")
w(stems_internal |> select(-TreeKey), "internal", "stems_internal")
w(survey_internal, "internal", "survey_internal")
w(key, "internal", "pid_to_public_id_key")

w(plots_public, "public", "plots")
w(trees_public, "public", "trees")
w(stems_public, "public", "stems")
w(survey_public, "public", "survey")
w(species_lookup, "public", "species_lookup")
w(survey_questions |>
    mutate(variable = recode(variable, q14_birth_year = "q14_birth_decade", q17_move_in_year = "q17_move_in_decade",
                             q18_property_age_raw = "q18_build_decade")) |>
    filter(variable %in% names(survey_public)), "public", "survey_questions")

qc <- bind_rows(flags)
w(qc, "qc", "qc_flags")

summ <- list(
  sample_points = nrow(registry),
  status = table(plots_internal$status, plots_internal$housing_type),
  trees = table(trees_internal$housing_type),
  survey_responses = nrow(survey_internal),
  survey_linked = sum(!is.na(survey_internal$pid)),
  qc_flags = nrow(qc)
)
capture.output(print(summ), file = file.path(out_dir, "qc", "build_summary.txt"))
print(summ)
message("Done. Public tables in ", file.path(out_dir, "public"))
