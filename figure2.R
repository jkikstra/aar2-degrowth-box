#' Script from Kikstra et al. 2023 preprint

renv::restore()
library(here)
source(here("data-raw", "kikstra2023", "for-reviewers_towardsdegrowth-v1", "utils.R"))
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

li.norm <- li.raw.notnorm %>%
  normalise_iamc_long(starting.year = normalization.year)


# PROCESS ====
baselines <-
  li.raw.notnorm %>%
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
                             ) %>% mutate(`Economic trend` = "Stopping GDP growth") %>%
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



  scale_colour_manual(breaks = c("Continuing GDP growth", "Stopping GDP growth"), values = c("grey", "dodgerblue")) +
  # scale_x_continuous(breaks = upsc.yrs) +
  scale_x_continuous(breaks = seq(2020,2100,20)) +
  scale_y_continuous(limits = c(0,9), expand = c(0,0)) +
  theme_classic() +
  theme_hc() +
  ylab("EJ/yr") + xlab(NULL) +
  labs(title = "Primary energy supply technology change under different growth pathways\nfor an ambitious emissions reduction scenario.",
       caption = "Based on the Australian energy system as in Kikstra et al. 2023.") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "none")

p.scaleup.levels.40k



# SAVE ====
save_ggplot(p = p.scaleup.levels.40k,
            f = here("figures", "kikstra-renewables-upscaling"),
            h=120, w=200)
