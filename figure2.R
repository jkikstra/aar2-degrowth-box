#' Script from Kikstra et al. 2023 preprint

renv::restore()
library(here)
source(here("data-raw", "kikstra2023", "for-reviewers_towardsdegrowth-v1", "utils.R"))
install.packages("geomtextpath")
load_pkgs()




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
li.raw.notnorm <- vroom(here("data-raw", "kikstra2023", "for-reviewers_towardsdegrowth-v1", "data", "li.csv")) %>%
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

li.gdp <- vroom(here("data-raw", "kikstra2023", "for-reviewers_towardsdegrowth-v1", "data", "li.csv")) %>%
  add_degrowth_level() %>%
  add_scenario_set_type() %>%
  filter(
    variable %in% c("GDP (PPP)","Population")
  ) %>%
  filter(
    `Climate policy` == "GHG budget"
  ) %>% to_per_capita()

li.consumption <- vroom(here("data-raw", "kikstra2023", "for-reviewers_towardsdegrowth-v1", "data", "li.csv")) %>%
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
p.scaleup.levels.40k <- ggplot(li.raw.notnorm %>%
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
                               mutate_cond(variable == "Primary Energy|Wind and Solar", variable = "Wind and Solar"),
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
  # scale_x_continuous(breaks = upsc.yrs) +
  scale_x_continuous(breaks = seq(2020,2100,20)) +
  scale_y_continuous(limits = c(0,9), expand = c(0,0)) +
  theme_classic() +
  theme_hc() +
  ylab("EJ/yr") + xlab(NULL) +
  labs(title = "Primary energy supply technology change under different growth pathways",
       subtitle = "For a the same emissions reduction target (4GtCO2 GHG until net zero GHG).",
       caption = "Based on the Australian energy system as in Kikstra et al. 2023.") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "none")

p.scaleup.levels.40k

p.scaleup.levels.40k.gdp <- ggplot(li.gdp %>%
                                 filter(
                                   year %in% upsc.yrs,
                                   `Annual consumption per capita (at utility peak)`=="40k"
                                 ) %>% mutate(`Economic trend` = "Stopping GDP growth") %>%
                                 bind_rows(baselines.gdp %>% rename(value=baseline) %>% mutate(`Economic trend` = "Continuing GDP growth")) %>%
                                 filter(
                                   `GHG budget`==budget.choice.highlight
                                 ),
                               aes(colour = `Economic trend`)) +
  facet_grid(.~variable) +
  geom_hline(yintercept = 0, colour="darkgrey") +

  geom_textpath(
    aes(x=year,
        y=value,
        label=`Economic trend`),
    hjust=.75,
    linewidth=2
  ) +



  scale_colour_manual(breaks = c("Continuing GDP growth", "Stopping GDP growth"), values = c("grey", "dodgerblue")) +
  # scale_x_continuous(breaks = upsc.yrs) +
  scale_x_continuous(breaks = seq(2020,2100,20)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  theme_hc() +
  ylab("Thousand $ per year") + xlab(NULL) +
  labs(title = "GDP (PPP) per capita") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "none")

p.scaleup.levels.40k.gdp


# SAVE ====
save_ggplot(p = p.scaleup.levels.40k,
            f = here("figures", "kikstra-renewables-upscaling"),
            h=120, w=200)


# add a decoupling plot ===
decl.years <- seq(2020,2100,10)

li.emissions.gdp <- vroom(here("data-raw", "kikstra2023", "for-reviewers_towardsdegrowth-v1", "data", "li.csv")) %>%
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

p.ghg.decoupling.40k.A <- ggplot(data.ghg.decoupling.40k.A %>%
                                   normalise_iamc_long(starting.year = 2020),
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
  # scale_x_continuous(breaks = upsc.yrs) +
  scale_x_continuous(breaks = decl.years) +
  scale_y_continuous(#limits = c(0,2.5), expand = c(0,0),
                     labels = scales::percent) +
  theme_classic() +
  theme_hc() +
  ylab(NULL) + xlab(NULL) +
  labs(title = "Change in economic activity and emissions",
       subtitle = "Relative to 2020",
       # caption = "Based on the Australian energy system as in Kikstra et al. 2023."
       ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  guides(linetype=guide_legend("Indicator"),
         color="none")

p.ghg.decoupling.40k.A



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
  # scale_x_continuous(breaks = upsc.yrs) +
  scale_x_continuous(breaks = decl.years) +
  # scale_y_continuous(limits = c(0,2.5), expand = c(0,0), labels = ) +
  theme_classic() +
  theme_hc() +
  ylab("tCO2/$") + xlab(NULL) +
  labs(title = "Emissions intensity",
       subtitle = "GHG emissions per $",
       # caption = "Based on the Australian energy system as in Kikstra et al. 2023."
  ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  guides(linetype="none",#guide_legend("Indicator"),
         color="none")

p.ghg.decoupling.40k.B

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
  # scale_x_continuous(breaks = upsc.yrs) +
  scale_x_continuous(breaks = decl.years) +
  # scale_y_continuous(limits = c(0,2.5), expand = c(0,0), labels = ) +
  theme_classic() +
  theme_hc() +
  ylab("MJ/$") + xlab(NULL) +
  labs(title = "Final energy intensity",
       subtitle = "Energy use per $",
       # caption = "Based on the Australian energy system as in Kikstra et al. 2023."
  ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  guides(linetype="none",#guide_legend("Indicator"),
         color="none")

p.fe.decoupling.40k.B

p.decoupling <- ((p.scaleup.levels.40k) / (p.ghg.decoupling.40k.A | p.ghg.decoupling.40k.B | p.fe.decoupling.40k.B)) +
  plot_layout() #+
  # plot_annotation(tag_level = "A")

p.decoupling

save_ggplot(p = p.decoupling,
            f = here("figures", "kikstra-renewables-upscaling-with-decoupling"),
            h=200, w=280)
