library(tidyverse)

# Import ------------------------------------------------------------------

## Girls
donnees_filles <- read_csv("Dados\\filles1980-2023.csv", show_col_types = F)

## Boys
donnees_gars <- read_csv("Dados\\gars1980-2023.csv", show_col_types = F)

# Compute -----------------------------------------------------------------

threshold <- 12
  
## Filles
filles <- donnees_filles |> 
  select(!Total) |> 
  rename(first_name = Prenom_feminin) |> 
  filter(first_name != "Somme:") |> 
  pivot_longer(cols = !first_name, names_to = "year", values_to = "N") |> 
  group_by(year) |>
  arrange(desc(N)) |> 
  filter(N >= threshold)

## Gars
gars <- donnees_gars |> 
  select(!Total) |> 
  rename(first_name = Prenom_masculin) |>
  filter(first_name != "Somme:") |> 
  pivot_longer(cols = !first_name, names_to = "year", values_to = "N") |> 
  group_by(year) |>
  arrange(desc(N)) |> 
  filter(N >= threshold)

# Bind --------------------------------------------------------------------

## First name that can be either for a boy or a girl.
bg <- intersect(filles$first_name, gars$first_name)

donnees <- bind_rows(
  list(
    "Filles"  = filles,
    "Garçons" = gars
  ),
  .id = "gender"
) |> 
  mutate(
    first_name = if_else(first_name %in% bg, paste0(first_name, " (", toupper(str_sub(gender, 1, 1)), ")"), first_name),
    first_name = str_to_title(first_name)
  ) |> 
  arrange(first_name, year) |> 
  group_by(first_name) |> 
  mutate(
    pct  = lag((lead(N)-N)/N), # Year growth
    year = as_date(paste0(year, "12-31"))
  ) |> 
  ungroup()

# Export ------------------------------------------------------------------

write_csv(donnees, "Quarto\\Resources\\filles-gars_1980-2023.csv")