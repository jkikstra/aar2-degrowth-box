# List your packages
packages <- c("vroom",
              "readxl",
              "here",
              "ggthemes",
              "patchwork",
              "geomtextpath",
              "tidyverse",
              "scales")

# Install any missing packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
invisible(lapply(packages, install_if_missing))

# Load all packages at once
lapply(packages, library, character.only = TRUE)

# load utility functions of Kikstra et al. (2024)
source(here("data-raw", "australia_kikstra2024", "utils.R"))

# PATH TO DATA
PATH.KIKSTRA.DATA <- here("data-raw", "australia_kikstra2024", "li.csv")





# INIT ====
starting.year <<- 2020
normalization.year <<- 2020


budget.choice.highlight <- "4Gt"

upsc.yrs <- c(2020,2030,2040,2050,2060,2070,2080,2090,2100)


vars.tech <- c(
  "Primary Energy|Wind",
  "Primary Energy|Solar",
  "Primary Energy|Biomass",
  "Primary Energy|Fossil"
)

# LOAD ====
li.raw.notnorm <- vroom(PATH.KIKSTRA.DATA) %>%
  add_degrowth_level() %>%
  add_scenario_set_type() %>%
  filter(
    variable %in% vars.tech
  ) %>%
  filter(
    `Climate policy` == "GHG budget"
  )
li.raw.notnorm <- li.raw.notnorm %>% filter(!(variable%in%c("Primary Energy|Wind","Primary Energy|Solar"))) %>%
  bind_rows(
    li.raw.notnorm %>% filter(variable%in%c("Primary Energy|Wind","Primary Energy|Solar")) %>%
      pivot_wider(names_from = variable, values_from = value) %>%
      mutate(`Primary Energy|Wind and Solar` = `Primary Energy|Wind` + `Primary Energy|Solar`) %>%
      select(-c("Primary Energy|Wind","Primary Energy|Solar")) %>%
      mutate(variable="Primary Energy|Wind and Solar") %>%
      rename(value=`Primary Energy|Wind and Solar`)
  )

li.gdp <- vroom(PATH.KIKSTRA.DATA) %>%
  add_degrowth_level() %>%
  add_scenario_set_type() %>%
  filter(
    variable %in% c("GDP (PPP)","Population")
  ) %>%
  filter(
    `Climate policy` == "GHG budget"
  ) %>% to_per_capita()

li.consumption <- vroom(PATH.KIKSTRA.DATA) %>%
  add_degrowth_level() %>%
  add_scenario_set_type() %>%
  filter(
    variable %in% c("Consumption","Population")
  ) %>%
  filter(
    `Climate policy` == "GHG budget"
  ) %>% to_per_capita()

# PROCESS ====
baselines <-
  li.raw.notnorm %>%
  filter(
    year %in% upsc.yrs,
    `Annual consumption per capita (at utility peak)`=="baseline"
  ) %>% ungroup() %>%
  select(variable,year,value, `GHG budget`) %>% rename(baseline=value)

baselines.gdp <-
  li.gdp %>%
  filter(
    year %in% upsc.yrs,
    `Annual consumption per capita (at utility peak)`=="baseline"
  ) %>% ungroup() %>%
  select(variable,year,value, `GHG budget`) %>% rename(baseline=value)

baselines.consumption <-
  li.consumption %>%
  filter(
    year %in% upsc.yrs,
    `Annual consumption per capita (at utility peak)`=="baseline"
  ) %>% ungroup() %>%
  select(variable,year,value, `GHG budget`) %>% rename(baseline=value)

# PLOT ====
df.F.toprow <- li.raw.notnorm %>%
  filter(
    year %in% upsc.yrs,
    `Annual consumption per capita (at utility peak)`=="40k"
  ) %>% mutate(`Economic trend` = "Slowing GDP growth") %>%
  bind_rows(baselines %>% rename(value=baseline) %>% mutate(`Economic trend` = "Continuing GDP growth")) %>%
  filter(
    `GHG budget`==budget.choice.highlight
  ) %>%

  mutate_cond(variable == "Primary Energy|Biomass", variable = "Biomass") %>%
  mutate_cond(variable == "Primary Energy|Fossil", variable = "Fossil") %>%
  mutate_cond(variable == "Primary Energy|Wind and Solar", variable = "Wind and Solar")
p.scaleup.levels.40k <- ggplot(df.F.toprow,
                           aes(colour = `Economic trend`)) +
  facet_grid(.~variable) +
  geom_hline(yintercept = 0, colour="darkgrey") +


  geom_line(
    data = . %>% filter(variable!="Wind and Solar"),
    aes(x=year,
        y=value),
    linewidth=2
  ) +
  geom_textpath(
    data = . %>% filter(variable=="Wind and Solar"),
    aes(x=year,
        y=value,
        label=`Economic trend`),
    hjust=.75,
    linewidth=2
  ) +



  scale_colour_manual(breaks = c("Continuing GDP growth", "Slowing GDP growth"), values = c("grey", "dodgerblue")) +
  scale_x_continuous(breaks = seq(2020,2100,20)) +
  scale_y_continuous(limits = c(0,9), expand = c(0,0)) +
  theme_classic() +
  theme_hc() +
  ylab("EJ/yr") + xlab(NULL) +
  labs(title = "Primary energy supply technology change under different growth pathways ",
       subtitle = "For a the same emissions reduction target (4GtCO2 GHG until net zero GHG),\nbased on the Australian energy system."
       ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "none")

p.scaleup.levels.40k


write_delim(x = df.F.toprow,
            file = here("figure_panel_F", "data_panel_F_toprow.csv"),
            delim = ",")

# add a decoupling plot ===
decl.years <- seq(2020,2100,10)

li.emissions.gdp <- vroom(PATH.KIKSTRA.DATA) %>%
  add_degrowth_level() %>%
  add_scenario_set_type() %>%
  filter(
    variable %in% c("GDP (PPP)","GHG emissions", "Final Energy")
  ) %>%
  filter(
    `Climate policy` == "GHG budget",
    `GHG budget` == "4Gt",

  )

data.ghg.decoupling.40k.A <- li.emissions.gdp %>%
  filter(
    year %in% decl.years,
    `Annual consumption per capita (at utility peak)`=="40k"
  ) %>% mutate(`Economic trend` = "Slowing GDP growth") %>%
  bind_rows(li.emissions.gdp %>%
              filter(
                year %in% decl.years,
                `Annual consumption per capita (at utility peak)`=="baseline"
              ) %>%
              mutate(`Economic trend` = "Continuing GDP growth"))
df.F.bottomrow_left <- data.ghg.decoupling.40k.A %>%
  normalise_iamc_long(starting.year = 2020)
p.ghg.decoupling.40k.A <- ggplot(df.F.bottomrow_left,
                               aes(colour = `Economic trend`)) +
  geom_hline(yintercept = 0, colour="darkgrey") +


  geom_line(
    data = . %>% filter(variable%in%c("GHG emissions", "Final Energy")),
    aes(x=year,
        y=value,
        linetype=variable),
    linewidth=2
  ) +
  geom_textpath(
    data = . %>% filter(variable=="GDP (PPP)"),
    aes(x=year,
        y=value,
        linetype=variable,
        label=`Economic trend`),
    hjust=.75,
    linewidth=2
  ) +

  scale_colour_manual(breaks = c("Continuing GDP growth", "Slowing GDP growth"), values = c("grey", "dodgerblue")) +
  scale_x_continuous(breaks = decl.years) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  theme_hc() +
  ylab(NULL) + xlab(NULL) +
  labs(title = "Change in economic activity and emissions",
       subtitle = "Relative to 2020",
       ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  guides(linetype=guide_legend("Indicator"),
         color="none")

p.ghg.decoupling.40k.A

write_delim(x = df.F.bottomrow_left,
            file = here("figure_panel_F", "data_panel_F_bottomrow_left.csv"),
            delim = ",")


data.ghg.decoupling.40k.B <- data.ghg.decoupling.40k.A %>% select(-unit) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(value = `GHG emissions`/`GDP (PPP)` / 1e3,
         variable = "GHG intensity of economic activity",
         unit = "tCO2-eq/$")
data.fe.decoupling.40k.B <- data.ghg.decoupling.40k.A %>% select(-unit) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  mutate(value = `Final Energy`/`GDP (PPP)` * 1e3,
         variable = "Final energy intensity of economic activity",
         unit = "MJ/$")


p.ghg.decoupling.40k.B <- ggplot(data.ghg.decoupling.40k.B,
                                 aes(colour = `Economic trend`)) +
  geom_hline(yintercept = 0, colour="darkgrey") +


  geom_line(
    aes(x=year,
        y=value,
        linetype=variable),
    linewidth=2
  ) +

  scale_colour_manual(breaks = c("Continuing GDP growth", "Slowing GDP growth"), values = c("grey", "dodgerblue")) +
  scale_x_continuous(breaks = decl.years) +
  theme_classic() +
  theme_hc() +
  ylab("tCO2/$") + xlab(NULL) +
  labs(title = "Emissions intensity",
       subtitle = "GHG emissions per $",
  ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  guides(linetype="none",#guide_legend("Indicator"),
         color="none")

p.ghg.decoupling.40k.B


write_delim(x = data.ghg.decoupling.40k.B,
            file = here("figure_panel_F", "data_panel_F_bottomrow_middle.csv"),
            delim = ",")


p.fe.decoupling.40k.B <- ggplot(data.fe.decoupling.40k.B,
                                 aes(colour = `Economic trend`)) +
  geom_hline(yintercept = 0, colour="darkgrey") +


  geom_line(
    aes(x=year,
        y=value,
        linetype=variable),
    linewidth=2
  ) +

  scale_colour_manual(breaks = c("Continuing GDP growth", "Slowing GDP growth"), values = c("grey", "dodgerblue")) +
  scale_x_continuous(breaks = decl.years) +
  theme_classic() +
  theme_hc() +
  ylab("MJ/$") + xlab(NULL) +
  labs(title = "Final energy intensity",
       subtitle = "Energy use per $",
  ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  guides(linetype="none",#guide_legend("Indicator"),
         color="none")

p.fe.decoupling.40k.B


write_delim(x = data.fe.decoupling.40k.B,
            file = here("figure_panel_F", "data_panel_F_bottomrow_right.csv"),
            delim = ",")


p.decoupling <- ((p.scaleup.levels.40k) / (p.ghg.decoupling.40k.A | p.ghg.decoupling.40k.B | p.fe.decoupling.40k.B)) +
  plot_layout()

p.decoupling

save_ggplot(p = p.decoupling,
            f = here("figure_panel_F", "kikstra-renewables-upscaling-with-decoupling"),
            h=200, w=280)
