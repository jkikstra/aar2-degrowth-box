# pkgs <- c(
#   "readxl",
#   "here",
#   "ggthemes",
#   "ggsci",
#   "patchwork",
#   "geomtextpath",
#
#   "tidyverse",
#   "vroom"
# )
# custom_filter <- function(here){
#   loaded_packages <- (.packages()) # make sure that only loaded packages (and their dependencies) are saved to the lockfile via custom option in snapshot
# }
# auto.snapshot <- getOption("renv.config.auto.snapshot") # save library state to lockfile; `selection 3` (3: Cancel, and resolve the situation on your own.)
# options(renv.config.auto.snapshot = FALSE)
# options(renv.snapshot.filter = custom_filter)
# invisible(lapply(pkgs, install.packages, prompt=FALSE, character.only = TRUE)) # install packages with renv
# invisible(lapply(pkgs, library, character.only = TRUE)) # install packages with renv
# renv::snapshot(lockfile = here("renv.lock"), type = "custom")
renv::restore()
library(here)
source(here("data-raw", "kikstra2023", "for-reviewers_towardsdegrowth-v1", "utils.R"))
load_pkgs()


#' Ideas for design of figure: “the need for degrowth in Austria”.
#'
#' Idea 1:
#' - panel A ‘past trends’ (Fanning et al. / Wiedmann)
#' - panel B: ‘current’ (Fanning et al.?)
#' - panel C ‘future’ [or more panels]
#'      (I wouldn’t know of something for Austria,
#'      but we could piece together things from
#'      e.g. D’Alessandro 2020 [France],
#'      Kikstra Australia preprint,
#'      and maybe some other work?).
#'



#' Panel A: 'past trends'
#' - World Bank
GDP <- read_excel(here("data-raw", "API_NY.GDP.MKTP.PP.KD_DS2_en_excel_v2_5795779.xls"),
                  sheet = "Data",
                  skip = 3) %>% select(where(~(is.numeric(.)|is.character(.)))) %>%
  pivot_longer(`1990`:`2022`, names_to = "year", values_to = "value") %>%
  rename(iso3c=`Country Code`)
GDP.AUT <- GDP %>% filter(iso3c=="AUT")

#' - Fanning 2022
#'

# data
fanning.sheet.names.historical <- c(
  "Biophysical_Historical",
  "Social_Historical"
)

panelA.data.fanning.AUT <- NULL
for (s in fanning.sheet.names.historical){

  df <- read_excel(
    path = here("data-raw", "Fanning2022.xlsx"),
    sheet = s
  )
  panelA.data.fanning.AUT <- panelA.data.fanning.AUT %>%
    bind_rows(
       df %>%
        filter(iso3c=="AUT") %>%
        pivot_longer(cols = 4:ncol(df), names_to = "variable", values_to = "value") %>%
        mutate(type = s)
    )
}

panelA.data.fanning.AUT.reported.vars <- panelA.data.fanning.AUT %>%
  drop_na() %>%
  group_by(country,iso3c,date,type) %>%
  count() %>% rename(reported.thresholds=n)

panelA.data.fanning.AUT.thresholdscrossed <- panelA.data.fanning.AUT %>%
  group_by(country,iso3c,date,type) %>%
  left_join(panelA.data.fanning.AUT.reported.vars) %>%
  mutate(crossed=ifelse(value>=1,1,0)) %>%
  summarise(crossed.percentage=sum(crossed/reported.thresholds,na.rm=T),
            average.higher.than.threshold = mean(value,na.rm=T))

p.A.PB.trends <- ggplot(
  data = panelA.data.fanning.AUT %>%
    left_join(panelA.data.fanning.AUT.thresholdscrossed),
  aes(x=date, y=value)
) +
  geom_labelhline(yintercept = 1, label = "Limit",
                  hjust = 0.01) +
  annotate(geom = "rect", xmin = 1990, xmax = 2015, ymin = 0, ymax = 1,
           fill = "#00BA38", alpha = 0.2) +
  geom_line(
    data = . %>% filter(type=="Biophysical_Historical"),
    aes(colour=variable, linetype=variable),
    linewidth = 1.5
  ) +

  theme_classic() +
  theme_hc() +
  theme(legend.title=element_blank()) +
  scale_y_continuous(limits = c(0,5), expand = c(0,0), name = "Biophysical pressure") +
  scale_x_continuous(limits = c(1990,2015), expand = c(0,0), name=NULL) +
  labs(
    subtitle = "Biophysical limits crossed",
    caption = "Data: Fanning et al. 2022"
  )
p.A.PB.trends

p.A.SP.trends <- ggplot(
  data = panelA.data.fanning.AUT %>%
    left_join(panelA.data.fanning.AUT.thresholdscrossed),
  aes(x=date, y=value)
) +
  geom_labelhline(yintercept = 1, label = "Minimum",
                  hjust = 0.01) +
  annotate(geom = "rect", xmin = 1990, xmax = 2015, ymin = 1, ymax = 2,
           fill = "#00BA38", alpha = 0.2) +
  geom_line(
    data = . %>% filter(type=="Social_Historical"),
    aes(colour=variable, linetype=variable),
    linewidth = 1.5
  ) +

  theme_classic() +
  theme_hc() +
  theme(legend.title=element_blank()) +
  scale_y_continuous(limits = c(0,2), expand = c(0,0), name = "Social achievement") +
  scale_x_continuous(limits = c(1990,2015), expand = c(0,0), name=NULL) +
  labs(
    subtitle = "Social provisioning needs met",
    caption = "Data: Fanning et al. 2022"
  )
p.A.SP.trends


p.A.thresholdscrossed <- ggplot(
  data = panelA.data.fanning.AUT.thresholdscrossed
) +
  geom_col(aes(x=date,y=-crossed.percentage,fill=type),data=. %>% filter(type=="Biophysical_Historical")) +
  geom_col(aes(x=date,y=crossed.percentage,fill=type),data=. %>% filter(type=="Social_Historical")) +

  theme_classic() +
  theme_hc() +
  theme(legend.title=element_blank())
p.A.thresholdscrossed


#' Panel B: 'doughnut' / Austria compared to other countries (histograms)
#' - Fanning 2022
#'
panelA.data.fanning.ALL <- NULL
for (s in fanning.sheet.names.historical){

  df <- read_excel(
    path = here("data-raw", "Fanning2022.xlsx"),
    sheet = s
  )
  panelA.data.fanning.ALL <- panelA.data.fanning.ALL %>%
    bind_rows(
      df %>%
        # filter(iso3c!="AUT") %>%
        pivot_longer(cols = 4:ncol(df), names_to = "variable", values_to = "value") %>%
        mutate(type = s)
    )
}
panelA.data.fanning.ALL.reported.vars <- panelA.data.fanning.ALL %>%
  drop_na() %>%
  group_by(country,iso3c,date,type) %>%
  count() %>% rename(reported.thresholds=n)

panelA.data.fanning.ALL.thresholdscrossed <- panelA.data.fanning.ALL %>%
  group_by(country,iso3c,date,type) %>%
  left_join(panelA.data.fanning.ALL.reported.vars) %>%
  mutate(crossed=ifelse(value>=1,1,0)) %>%
  summarise(crossed.percentage=sum(crossed/reported.thresholds,na.rm=T),
            average.higher.than.threshold = mean(value,na.rm=T))

p.country.hists <- ggplot(
  data = panelA.data.fanning.ALL.thresholdscrossed %>%
    filter(date==2015)
) +
  facet_grid(~type) +
  geom_histogram(aes(x=crossed.percentage, fill=type),bins=10) +

  theme_classic() +
  theme_hc() +
  theme(legend.title=element_blank()) +

  ylab("Number of countries") +
  xlab("Percentage of thresholds crossed")

p.country.hists


p.oneill.style <- ggplot(
  data = panelA.data.fanning.ALL.thresholdscrossed %>%
    filter(date==2015) %>% select(-average.higher.than.threshold) %>%
    pivot_wider(names_from = type, values_from = crossed.percentage)
) +
  geom_point(aes(x=Biophysical_Historical, y=Social_Historical),
             colour="grey",
             alpha=0.5,
             size=1.5,
             position = position_jitter(width=0.01,height=0.01)) +
  geom_point(aes(x=Biophysical_Historical, y=Social_Historical),
             data=. %>% filter(iso3c=="AUT"),
             colour="dodgerblue",
             size=3,
             position = position_dodge(width=0.01)) +
  geom_text(aes(x=Biophysical_Historical, y=Social_Historical),
            data=. %>% filter(iso3c=="AUT"),
            colour="dodgerblue",
            # size=3,
            fontface="bold",
            label = "Austria",
            vjust = -0.3, hjust = 1.2) +

  theme_classic() +
  # theme_hc() +
  theme(legend.title=element_blank()) +

  ylab("Social thresholds achieved [%]") +
  xlab("Biophysical thresholds crossed [%]")

p.oneill.style




#' Panel C [option 1]: 'decoupling'
#'
lt.options <- c("solid",
                "dashed",
                "dotted",
                "longdash",
                "dotdash",
                # "twodash",
                "1F"#,
                # "F1",
                # "4C88C488",
                # "12345678"
)
bp.vars <- panelA.data.fanning.AUT %>% filter(type=="Biophysical_Historical") %>% pull(variable) %>% unique() %>% sort()
s.vars <- panelA.data.fanning.AUT %>% filter(type=="Social_Historical") %>% pull(variable) %>% unique() %>% sort()


p.decoupling.panels <- ggplot(
  data = panelA.data.fanning.AUT %>% rename(model=iso3c,scenario=type,region=country,year=date) %>% mutate(unit="Relative to threshold") %>%
    normalise_iamc_long(starting.year = 1995) %>% drop_na() %>%
    mutate(scenario=ifelse(scenario=="Biophysical_Historical","Biophysical",
                           ifelse(scenario=="Social_Historical","Social",
                           scenario))),
    # left_join(GDP.AUT %>% select(iso3c,year,value) %>% rename(gdp=value), relationship = "many-to-many"),
  aes(x=year,y=value,group=variable)
) +
  facet_wrap(.~scenario) +
  geom_line(
    aes(linetype=variable,
        linewidth=variable,
        colour=scenario)
  ) +
  geom_textpath(
    data=GDP.AUT %>% select(iso3c,year,value) %>% mutate(year=as.numeric(year)) %>% mutate(model="World Bank",
                                                         region=iso3c,
                                                         variable="GDP",
                                                         unit = "PPP (constant 2017 international $)") %>%
      expand_grid(., scenario=c("Biophysical", "Social")) %>%
      normalise_iamc_long(starting.year = 1995) %>%
      filter(year<=2015),
    aes(label=variable),
    linewidth=1.5,
    hjust=0.65
  ) +

  scale_linetype_manual(
    breaks = c(s.vars,
               bp.vars),
    values = c(lt.options,lt.options[1:5],
               lt.options)
  ) +
  scale_linewidth_manual(
    breaks = c(s.vars,
               bp.vars),
    values = c(rep(1,6),rep(1.5,5),
               rep(1.5,6))
  ) +

  theme_classic() +
  theme_hc() +
  labs(
    subtitle = "Decoupling trends in Austria",
    caption =
  ) + ylab("Value relative to 1995") + xlab(NULL) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "right") +
  scale_y_continuous(breaks = c(0.5,1,1.5))

p.decoupling.panels


p.decoupling.panels <- ggplot(
  data = panelA.data.fanning.AUT %>% rename(model=iso3c,scenario=type,region=country,year=date) %>% mutate(unit="Relative to threshold") %>%
    normalise_iamc_long(starting.year = 1995) %>% drop_na() %>%
    mutate(scenario=ifelse(scenario=="Biophysical_Historical","Biophysical",
                           ifelse(scenario=="Social_Historical","Social",
                                  scenario))),
  # left_join(GDP.AUT %>% select(iso3c,year,value) %>% rename(gdp=value), relationship = "many-to-many"),
  aes(x=year,y=value,group=variable)
) +
  facet_wrap(variable~.) +
  geom_line(
    aes(linetype=variable,
        linewidth=variable,
        colour=scenario)
  ) +
  geom_textpath(
    data=GDP.AUT %>% select(iso3c,year,value) %>% mutate(year=as.numeric(year)) %>% mutate(model="World Bank",
                                                                                           region=iso3c,
                                                                                           variable="GDP",
                                                                                           unit = "PPP (constant 2017 international $)") %>%
      expand_grid(., scenario=c("Biophysical", "Social")) %>%
      normalise_iamc_long(starting.year = 1995) %>%
      filter(year<=2015),
    aes(label=variable),
    linewidth=1.5,
    hjust=0.65
  ) +

  scale_linetype_manual(
    breaks = c(s.vars,
               bp.vars),
    values = c(lt.options,lt.options[1:5],
               lt.options)
  ) +
  scale_linewidth_manual(
    breaks = c(s.vars,
               bp.vars),
    values = c(rep(1,6),rep(1.5,5),
               rep(1.5,6))
  ) +

  theme_classic() +
  theme_hc() +
  labs(
    subtitle = "Decoupling trends in Austria",
    caption =
  ) + ylab("Value relative to 1995") + xlab(NULL) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "right") +
  scale_y_continuous(breaks = c(0.5,1,1.5))

p.decoupling.panels



#' Panel C [option 2]: 'future'
#' - Fanning 2022
#' - Kikstra 2023
#'
#'
#' tbd.



# combine and save
p.historical.degrowthbox.wlabels <- (p.A.PB.trends | p.A.SP.trends) /
  p.oneill.style /
  p.decoupling.panels

p.historical.degrowthbox.wlabels

p.historical.degrowthbox.nolabels <- ((p.A.PB.trends + theme(legend.position = "none")) | (p.A.SP.trends + theme(legend.position = "none"))) /
  (p.oneill.style + theme(legend.position = "none")) /
  (p.decoupling.panels + theme(legend.position = "none"))

p.historical.degrowthbox.nolabels

# SAVE ====
save_ggplot(p = p.historical.degrowthbox.wlabels,
            f = here("figures", "aar2-historical-trends-wlabels"),
            h=250, w=200)
save_ggplot(p = p.historical.degrowthbox.nolabels,
            f = here("figures", "aar2-historical-trends"),
            h=250, w=200)
