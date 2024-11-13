# List your packages
packages <- c("vroom",
              "readxl",
              "here",
              "ggthemes",
              "patchwork",
              "geomtextpath",
              "tidyverse")

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
PATH.FANNING.DATA <- here("data-raw", "boundaries_fanning2022", "Fanning2022.xlsx")




# Panel A: 'past trends': biophysical ------------------------------------------
# data
fanning.sheet.names.historical <- c(
  "Biophysical_Historical",
  "Social_Historical"
)

panel.ABD.data.fanning.AUT <- NULL
for (s in fanning.sheet.names.historical){

  df <- read_excel(
    path = PATH.FANNING.DATA,
    sheet = s
  )
  panel.ABD.data.fanning.AUT <- panel.ABD.data.fanning.AUT %>%
    bind_rows(
       df %>%
        filter(iso3c=="AUT") %>%
        pivot_longer(cols = 4:ncol(df), names_to = "variable", values_to = "value") %>%
        mutate(type = s)
    )
}

panelAB.data.fanning.AUT.reported.vars <- panel.ABD.data.fanning.AUT %>%
  drop_na() %>%
  group_by(country,iso3c,date,type) %>%
  count() %>% rename(reported.thresholds=n)

panelAB.data.fanning.AUT.thresholdscrossed <- panel.ABD.data.fanning.AUT %>%
  group_by(country,iso3c,date,type) %>%
  left_join(panelAB.data.fanning.AUT.reported.vars) %>%
  mutate(crossed=ifelse(value>=1,1,0)) %>%
  summarise(crossed.percentage=sum(crossed/reported.thresholds,na.rm=T),
            average.higher.than.threshold = mean(value,na.rm=T))

df.A.PB.trends <- panel.ABD.data.fanning.AUT %>%
  left_join(panelAB.data.fanning.AUT.thresholdscrossed) %>%
  filter(type=="Biophysical_Historical")
p.A.PB.trends <- ggplot(
  data = df.A.PB.trends,
  aes(x=date, y=value)
) +
  geom_labelhline(yintercept = 1, label = "Limit",
                  hjust = 0.01) +
  annotate(geom = "rect", xmin = 1990, xmax = 2015, ymin = 0, ymax = 1,
           fill = "#00BA38", alpha = 0.2) +
  geom_line(
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
    # caption = "Data: Fanning et al. 2022"
  )
p.A.PB.trends

write_delim(x = df.A.PB.trends,
            file = here("figure_panel_A_B_C_D", "data_panel_A.csv"),
            delim = ",")


# Panel B: 'past trends': social needs -----------------------------------------
df.B.SP.trends <- panel.ABD.data.fanning.AUT %>%
  left_join(panelAB.data.fanning.AUT.thresholdscrossed) %>%
  filter(type=="Social_Historical")
p.B.SP.trends <- ggplot(
  data = df.B.SP.trends,
  aes(x=date, y=value)
) +
  geom_labelhline(yintercept = 1, label = "Minimum",
                  hjust = 0.01) +
  annotate(geom = "rect", xmin = 1990, xmax = 2015, ymin = 1, ymax = 2,
           fill = "#00BA38", alpha = 0.2) +
  geom_line(
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
    # caption = "Data: Fanning et al. 2022"
  )
p.B.SP.trends
write_delim(x = df.B.SP.trends,
            file = here("figure_panel_A_B_C_D", "data_panel_B.csv"),
            delim = ",")

# Panel C: '2D doughnut' / Austria compared to other countries -----------------
panelC.data.fanning.ALL <- NULL
for (s in fanning.sheet.names.historical){

  df <- read_excel(
    path = PATH.FANNING.DATA,
    sheet = s
  )
  panelC.data.fanning.ALL <- panelC.data.fanning.ALL %>%
    bind_rows(
      df %>%
        # filter(iso3c!="AUT") %>%
        pivot_longer(cols = 4:ncol(df), names_to = "variable", values_to = "value") %>%
        mutate(type = s)
    )
}
panelC.data.fanning.ALL.reported.vars <- panelC.data.fanning.ALL %>%
  drop_na() %>%
  group_by(country,iso3c,date,type) %>%
  count() %>% rename(reported.thresholds=n)

panelC.data.fanning.ALL.thresholdscrossed <- panelC.data.fanning.ALL %>%
  group_by(country,iso3c,date,type) %>%
  left_join(panelC.data.fanning.ALL.reported.vars) %>%
  mutate(crossed=ifelse(value>=1,1,0)) %>%
  summarise(crossed.percentage=sum(crossed/reported.thresholds,na.rm=T),
            average.higher.than.threshold = mean(value,na.rm=T))

df.C <- panelC.data.fanning.ALL.thresholdscrossed %>%
  filter(date==2015) %>% select(-average.higher.than.threshold) %>%
  pivot_wider(names_from = type, values_from = crossed.percentage)
p.C.2D.Austria <- ggplot(
  data = df.C
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
            fontface="bold",
            label = "Austria",
            vjust = 1, hjust = 1.2) +

  # Draw a green circle with a white center
  geom_point(aes(x = 0, y = 1), color = "darkgreen", fill = "white", shape = 21, size = 8) +
  # Add the text label at the specified position
  annotate("text", x = 0, y = 0.95, color = "darkgreen", label = "Sustainability", hjust = -0.1, vjust = 1) +


  theme_classic() +
  theme(legend.title=element_blank()) +

  ylab("Social thresholds achieved [%]") +
  xlab("Biophysical thresholds crossed [%]")

p.C.2D.Austria

write_delim(x = df.C,
            file = here("figure_panel_A_B_C_D", "data_panel_C.csv"),
            delim = ",")


# Panel D : 'past trends': 'decoupling' ----------------------------------------

# GDP data
#' - World Bank
GDP <- read_excel(here("data-raw", "gdp_worldbank", "API_NY.GDP.MKTP.PP.KD_DS2_en_excel_v2_5795779.xls"),
                  sheet = "Data",
                  skip = 3) %>% select(where(~(is.numeric(.)|is.character(.)))) %>%
  pivot_longer(`1990`:`2022`, names_to = "year", values_to = "value") %>%
  rename(iso3c=`Country Code`)
GDP.AUT <- GDP %>% filter(iso3c=="AUT")

# linetype options
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
bp.vars <- panel.ABD.data.fanning.AUT %>% filter(type=="Biophysical_Historical") %>% pull(variable) %>% unique() %>% sort()
s.vars <- panel.ABD.data.fanning.AUT %>% filter(type=="Social_Historical") %>% pull(variable) %>% unique() %>% sort()

df.D.GDP <- GDP.AUT %>% select(iso3c,year,value) %>% mutate(year=as.numeric(year)) %>% mutate(model="World Bank",
                                                                                              region=iso3c,
                                                                                              variable="GDP",
                                                                                              unit = "PPP (constant 2017 international $)") %>%
  expand_grid(., scenario=c("Biophysical", "Social")) %>%
  normalise_iamc_long(starting.year = 1995) %>%
  filter(year<=2015)
df.D.Fanning <- panel.ABD.data.fanning.AUT %>% rename(model=iso3c,scenario=type,region=country,year=date) %>% mutate(unit="Relative to threshold") %>%
  normalise_iamc_long(starting.year = 1995) %>% drop_na() %>%
  mutate(scenario=ifelse(scenario=="Biophysical_Historical","Biophysical",
                         ifelse(scenario=="Social_Historical","Social",
                                scenario)))

p.D.decoupling <- ggplot(
  data = df.D.Fanning,
  aes(x=year,y=value,group=variable)
) +
  facet_wrap(.~scenario) +
  geom_line(
    aes(linetype=variable,
        linewidth=variable,
        colour=scenario)
  ) +
  geom_textpath(
    data=df.D.GDP,
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

p.D.decoupling

write_delim(x = df.D.Fanning,
            file = here("figure_panel_A_B_C_D", "data_panel_D_sustainability.csv"),
            delim = ",")
write_delim(x = df.D.GDP,
            file = here("figure_panel_A_B_C_D", "data_panel_D_gdp.csv"),
            delim = ",")


# combine and save
p.historical.degrowthbox.wlabels <- (p.A.PB.trends | p.B.SP.trends) /
  p.C.2D.Austria /
  p.D.decoupling

p.historical.degrowthbox.wlabels

p.historical.degrowthbox.nolabels <- ((p.A.PB.trends + theme(legend.position = "none")) | (p.B.SP.trends + theme(legend.position = "none"))) /
  (p.C.2D.Austria + theme(legend.position = "none")) /
  (p.D.decoupling + theme(legend.position = "none"))

p.historical.degrowthbox.nolabels

# SAVE ====
save_ggplot(p = p.historical.degrowthbox.wlabels,
            f = here("figure_panel_A_B_C_D", "aar2-historical-trends-wlabels"),
            h=250, w=200)
save_ggplot(p = p.historical.degrowthbox.nolabels,
            f = here("figure_panel_A_B_C_D", "aar2-historical-trends"),
            h=250, w=200)
