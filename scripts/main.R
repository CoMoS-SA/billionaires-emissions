library(tidyverse)
library(magrittr)
library(readxl)

#### Preliminaries ####
data <- read_csv("data/example_data.csv", 
                         col_types = cols(ICB_Subsector_code = col_character()))

#remove billionaires with insufficient information
data %<>% 
  filter(!(name%in%c(
    "Sukanto Tanoto", 
    "Theo Albrecht Jr", 
    "Ray Dalio",
    "Tilman Fertitta",
    "Charles Butt & family",
    "George Kaiser",
    "Renata Kellnerova",
    "Robert Smith",
    "John Grayken",
    "Eric Smidt",
    "Tatyana Kim",
    "Daniel Kretinsky",
    "Frederik Paulsen",
    "Odd Reitan",
    "David Green")
  ))

data %>% 
  group_by(name) %>% 
  summarise(
    wealth = sum(value[asset_type!="misc"]) - sum(value[asset_type=="misc"]),
    tot_emission_baseline = sum(`tCO2e_billionaire_baseline`),
    nationality_ISO3 = nationality_single_res[1],
    multiple_nationality_ISO3 = multiple_nationality_ISO3[1],
    gender = gender[1]
  ) %>% 
  ungroup() -> data_billionaire

data_billionaire %<>% 
  mutate(
    wealth = wealth/1000000000, #bln$
    tot_emission_baseline=tot_emission_baseline/1000 #ktCO2e
  ) %>% 
  mutate(
    emission_intensity = tot_emission_baseline/wealth #ktCO2e/Bln$
  )

#### Descriptive billionaires ####

data_billionaire %>% 
  ggplot(aes(x=wealth, y=tot_emission_baseline)) + 
  geom_point(size=0.75, alpha=0.7, color="grey42") + 
  geom_smooth(method = "lm", color="black", se=F, size=0.5, linetype=2) +
  xlab("Wealth (Bln$, log scale)") +
  ylab("Emissions (ktCO2e, log scale)") +
  scale_y_log10(labels = scales::label_log()) +
  scale_x_log10() +
  theme_minimal() + 
  theme(
    panel.grid.minor = element_blank()
  ) -> scatter1

ggExtra::ggMarginal(
  scatter1,
  type = "histogram",
  margins = "both",
  bins = 30,
  size = 7,
  xparams = list(fill = "indianred3", colour="black", alpha=0.8, size=0.2),
  yparams = list(fill = "steelblue", colour="black", alpha=0.8, size=0.2)
) -> scatter1h


data_billionaire %>% 
  ggplot(aes(x=emission_intensity, y=tot_emission_baseline)) + 
  geom_point(size=0.75, alpha=0.7, color="grey42") + 
  geom_smooth(method = "lm", color="black", se=F, size=0.5, linetype=2) +
  xlab("Emission intensity (tCO2e/Mln$, log scale)") +
  ylab("Emissions (ktCO2e, log scale)") +
  scale_y_log10(labels = scales::label_log()) +
  scale_x_log10(labels = scales::label_log()) +
  theme_minimal() + 
  theme(
    panel.grid.minor = element_blank()
  ) -> scatter2

ggExtra::ggMarginal(
  scatter2,
  type = "histogram",
  margins = "both",
  bins = 30,
  size = 7,
  xparams = list(fill = "aquamarine4", colour="black", alpha=0.8, size=0.2),
  yparams = list(fill = "steelblue", colour="black", alpha=0.8, size=0.2)
) -> scatter2h

#variance decomposition

data_billionaire %>% 
  filter(tot_emission_baseline>0) %>% 
  mutate(
    log_wealth = log(wealth),
    log_tot_emission_baseline = log(tot_emission_baseline),
    log_emission_intensity = log(emission_intensity)
  ) %>% 
  summarize(
    var_log_wealth = var(log_wealth),
    var_log_tot_emission_baseline = var(log_tot_emission_baseline),
    var_log_emission_intensity = var(log_emission_intensity),
    cov_w_i = cov(log_wealth,log_emission_intensity)
  ) %>% 
  mutate(
    c_w = var_log_wealth/var_log_tot_emission_baseline,
    c_i = var_log_emission_intensity/var_log_tot_emission_baseline,
    c_cov = (2*cov_w_i)/var_log_tot_emission_baseline
  ) 

#compute shares

#wealth
q50 = quantile(data_billionaire$wealth, 0.5)
q90 = quantile(data_billionaire$wealth, 0.9)

data_billionaire %>% 
  filter(wealth>q50) %>% 
  pull(wealth) %>% sum()/sum(data_billionaire$wealth) -> sh_50

data_billionaire %>% 
  filter(wealth>q90) %>% 
  pull(wealth) %>% sum()/sum(data_billionaire$wealth) -> sh_90

#emissions
q50e = quantile(data_billionaire$tot_emission_baseline, 0.5)
q90e = quantile(data_billionaire$tot_emission_baseline, 0.9)

data_billionaire %>% 
  filter(tot_emission_baseline>q50e) %>% 
  pull(tot_emission_baseline) %>% sum()/sum(data_billionaire$tot_emission_baseline) -> sh_50e

data_billionaire %>% 
  filter(tot_emission_baseline>q90e) %>% 
  pull(tot_emission_baseline) %>% sum()/sum(data_billionaire$tot_emission_baseline) -> sh_90e

#emission intensity

q50ei = quantile(data_billionaire$emission_intensity, 0.5)
q90ei = quantile(data_billionaire$emission_intensity, 0.9)

data_billionaire %>% 
  filter(emission_intensity>q50ei) %>% 
  pull(emission_intensity) %>% sum()/sum(data_billionaire$emission_intensity) -> sh_50ei

data_billionaire %>% 
  filter(emission_intensity>q90ei) %>% 
  pull(emission_intensity) %>% sum()/sum(data_billionaire$emission_intensity) -> sh_90ei

tibble(
  'Wealth'=c(1-sh_50,sh_90),
  'Emissions'=c(1-sh_50e,sh_90e),
  'Emission Intensity'=c(1-sh_50ei,sh_90ei),
  name = c("Bottom50%", "Top10%")
) %>% 
  pivot_longer(cols = -name,      # tutte le colonne tranne 'name'
               names_to = "variable",
               values_to = "value") %>% 
  mutate(
    name = factor(name, levels = c("Bottom50%", "Top10%"), labels = c("Bottom50%\nof top 500 billionaires", "Top10%\nof top 500 billionaires")),
    variable = factor(variable, levels =c('Wealth', 'Emissions', 'Emission Intensity'))
  ) %>% 
  filter(variable!='Emission Intensity') %>% 
  ggplot() + 
  ggpattern::geom_bar_pattern(
    aes(x=name, y=value, fill = variable, pattern = variable),
    stat = "identity", position = "dodge", alpha=0.8,
    pattern_spacing = c(0.05,0.03,0.05,0.03),
    pattern_fill="black",
    pattern_angle = 20,
    pattern_density=c(0.05,0.1,0.05,0.1),
    pattern_size = c(0.1,0.3,0.1,0.3)
  ) + 
  scale_fill_manual(values=c('Wealth'="indianred3", 'Emissions'="steelblue", 'Emission Intensity'="darkslategrey")) +
  ggpattern::scale_pattern_manual(values = c(
    "Wealth" = "stripe",
    "Emissions" = "circle"
  )) + 
  scale_y_continuous(labels = scales::label_percent()) +
  ylab("Share") +
  guides(
    pattern = guide_legend(
      override.aes = list(
        pattern_spacing = 0.02,
        pattern_density = 0.03,
        pattern_size = c(0.1)
      )
    )
  ) +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    legend.position = c(0.2,0.85), 
    legend.title = element_blank(),
    axis.title.x = element_blank()
  ) -> g_shares

fig1 <- cowplot::plot_grid(scatter1h, scatter2h, g_shares, nrow=1, labels = c('A','B', 'C'))

ggsave("charts/fig1.pdf", fig1, width=12, height = 3.5)

#### Descriptive asset level ####

data %>% 
  left_join(
    .,
    data_billionaire %>% 
      mutate(decile = ntile(wealth, 10)) %>% 
      select(name, decile),
    by="name"
  ) %>% 
  group_by(name, asset_type2) %>%
  summarise(
    mean_value=sum(value, na.rm=T)/1000000000,
    decile = decile[1]
  ) %>% 
  ungroup() %>%
  group_by(decile, asset_type2) %>% 
  summarise(
    mean_value = mean(mean_value, na.rm=T)
  ) %>% 
  ungroup() %>% 
  mutate(mean_value=case_when(
    asset_type2=="debt" ~ -mean_value,
    T ~ mean_value
  )) %>% 
  add_row(decile=10, asset_type2="infrastructure/business asset", mean_value=0) %>% 
  mutate(
    asset_type2 = factor(
      asset_type2,
      levels = c("debt","liquidity", "other/mixed/unknown", "infrastructure/business asset","luxury","real estate", "private equity","public equity"),
      labels = c("Liabilities","Cash","Mixed", "Infrastructures & Business","Luxury", "Real estate", "Unlisted equity", "Publicly traded equity")
    )
  ) %>% 
  ggplot() + 
  ggpattern::geom_area_pattern(
    aes(x=decile, y=mean_value, fill = asset_type2, pattern = asset_type2),
    alpha=0.8, color="black", linewidth = 0.2,
    pattern_spacing = c(0.02),
    pattern_fill="black",
    pattern_angle = 20,
    pattern_density=c(0.05),
    pattern_size = c(0.2)
  ) + 
  ggrepel::geom_text_repel(
    data = tibble(x=1.5, y=12.5, label="Cash"),
    aes(x = x, y = y, label = label),
    segment.size = 0.3,
    direction = "y",
    nudge_x = 0.2,
    nudge_y = 10,
    hjust = 1,
    min.segment.length = 0,
    segment.color = "black",
    size = 3
  ) +
  ggrepel::geom_text_repel(
    data = tibble(x=2.5, y=12, label="Mixed"),
    aes(x = x, y = y, label = label),
    segment.size = 0.3,
    direction = "y",
    nudge_x = -0.5,
    nudge_y = 19,
    hjust = 1,
    min.segment.length = 0,
    segment.color = "black",
    size = 3
  ) +
  ggrepel::geom_text_repel(
    data = tibble(x=3.5, y=13, label="Infrastructures\n& Business"),
    aes(x = x, y = y, label = label),
    segment.size = 0.3,
    direction = "y",
    nudge_x = 0.2,
    nudge_y = 19,
    hjust = 0.7,
    min.segment.length = 0,
    segment.color = "black",
    size = 3
  ) +
  ggrepel::geom_text_repel(
    data = tibble(x=4, y=12.2, label="Luxury"),
    aes(x = x, y = y, label = label),
    segment.size = 0.3,
    direction = "y",
    nudge_x = 0.9,
    nudge_y = 19,
    hjust = 0,
    min.segment.length = 0,
    segment.color = "black",
    size = 3
  ) +
  ggrepel::geom_text_repel(
    data = tibble(x=6, y=15, label="Real estate"),
    aes(x = x, y = y, label = label),
    segment.size = 0.3,
    direction = "y",
    nudge_x = 0.5,
    nudge_y = 22,
    hjust = 0.7,
    min.segment.length = 0,
    segment.color = "black",
    size = 3
  ) +
  ggrepel::geom_text_repel(
    data = tibble(x=8, y=17, label="Unlisted equity"),
    aes(x = x, y = y, label = label),
    segment.size = 0.3,
    direction = "y",
    nudge_x = -0.5,
    nudge_y = 30,
    hjust = 0.7,
    min.segment.length = 0,
    segment.color = "black",
    size = 3
  ) +
  ggrepel::geom_text_repel(
    data = tibble(x=9.8, y=45, label="Publicly traded\nequity"),
    aes(x = x, y = y, label = label),
    segment.size = 0.3,
    direction = "y",
    nudge_x = -1.2,
    nudge_y = 30,
    hjust = 0.7,
    min.segment.length = 0,
    segment.color = "black",
    size = 3
  ) +
  ggrepel::geom_text_repel(
    data = tibble(x=9.5, y=-2, label="Liabilities"),
    aes(x = x, y = y, label = label),
    segment.size = 0.3,
    direction = "y",
    nudge_x = -1.2,
    nudge_y = -5,
    hjust = 0.7,
    min.segment.length = 0,
    segment.color = "black",
    size = 3
  ) +
  geom_segment(aes(x=1, xend=10, y=0, yend=0),color="black", linewidth = 0.1) +
  scale_x_continuous(breaks=1:10) +
  scale_fill_manual(values = c(
    "Liabilities"="#c7522a",
    "Cash"="#d68a58",
    "Mixed"="#e5c185",
    "Infrastructures & Business"="#b8cdab",
    "Luxury"="#74a892",
    "Real estate"="#008585",
    
    "Publicly traded equity"="#01426a",
    "Unlisted equity"="#975a58"
  )) +
  ggpattern::scale_pattern_manual(values = c(
    "Liabilities"="none",
    "Cash"="none",
    "Mixed"="none",
    "Infrastructures & Business"="none",
    "Luxury"="none",
    "Real estate"="none",
    
    "Publicly traded equity"="stripe",
    "Unlisted equity"="circle"
  )) + 
  xlab("Wealth decile") + 
  ylab("Average portfolio composition (Bln$)") +
  guides(fill=guide_legend(nrow=1)) +
  guides(fill = guide_legend(nrow=3, byrow = F)) +
  theme_minimal() + 
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.4,0.8),
    legend.title = element_blank()
  ) -> g_stack_w

data %>% 
  filter(!(asset_type2%in%c("debt", "liquidity", "other/mixed/unknown"))) %>% 
  mutate(
    asset_type3 = case_when(
      !asset_type2 %in% c("private equity", "public equity") ~ "other",
      TRUE ~ asset_type2
    )
  ) %>% 
  group_by(asset_type3, ICB_Industry_name) %>% 
  summarise(
    sum_ICB=sum(`tCO2e_billionaire_baseline`, na.rm=T)/1000000
  ) %>% 
  ungroup() %>% 
  mutate(
    ICB_Industry_name = case_when(
      ICB_Industry_name=="Basic Materials" ~ "Basic\nmaterials",
      ICB_Industry_name=="Consumer Discretionary" ~ "Consumer\ndiscretionary",
      ICB_Industry_name=="Consumer Staples" ~ "Consumer\nstaples",
      ICB_Industry_name=="Real Estate" ~ "Real estate",
      ICB_Industry_name=="Telecommunications" ~ "Telecom",
      TRUE ~ ICB_Industry_name
    )
  ) -> dt_asset_type3

dt_asset_type3 %>% 
  filter(
    !(
      (is.na(ICB_Industry_name) & asset_type3=="public equity")
      |
        (is.na(ICB_Industry_name) & asset_type3=="private equity")
    )
  ) %>% 
  group_by(asset_type3) %>% 
  summarise(
    sum_asset = sum(sum_ICB)
  ) %>% 
  ungroup() -> dt_asset_type3_inset


order_industries <- dt_asset_type3 %>%
  filter(asset_type3 == "public equity") %>%
  arrange(desc(sum_ICB)) %>%
  pull(ICB_Industry_name)


dt_asset_type3 %>% 
  mutate(
    asset_type3 = factor(
      asset_type3,
      levels = c( "public equity", "private equity"),
      labels = c("Publicly traded equity", "Unlisted equity" )
    ),
    ICB_Industry_name = factor(ICB_Industry_name, levels = order_industries)
  ) %>% 
  filter(!is.na(ICB_Industry_name)) %>% 
  ggplot() + 
  ggpattern::geom_col_pattern(
    aes(x=ICB_Industry_name, y=sum_ICB, fill=asset_type3, pattern=asset_type3),
    alpha=0.8, position="dodge",colour="black",
    pattern_spacing = c(0.02),
    pattern_fill="black",
    pattern_angle = 20,
    pattern_density=c(0.05),
    pattern_size = c(0.2)
  ) + 
  scale_fill_manual(values = c(
    "Publicly traded equity"="#01426a",
    "Unlisted equity"="#975a58"
  )) +
  ggpattern::scale_pattern_manual(values = c(
    "Publicly traded equity"="stripe",
    "Unlisted equity"="circle"
  )) + 
  ylab("Total emissions (MtCO2e)") +
  guides(fill=guide_legend(nrow=1)) +
  theme_minimal() + 
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, vjust=1.17, hjust=0.75),
  ) -> g_emission_sectors


dt_asset_type3_inset %>% 
  mutate(
    asset_type3 = factor(
      asset_type3,
      levels = c("public equity", "private equity", "other"),
      labels = c("Publicly\ntraded equity", "Unlisted\nequity", "Other\nprivate assets")
    )
  ) %>% 
  ggplot() + 
  ggpattern::geom_col_pattern(
    aes(x=asset_type3, y=sum_asset, fill=asset_type3, pattern=asset_type3),
    alpha=0.8, colour="black",
    pattern_spacing = c(0.04),
    pattern_fill="black",
    pattern_angle = 20,
    pattern_density=c(0.05),
    pattern_size = c(0.2)
  ) + 
  scale_fill_manual(values = c(
    "Publicly\ntraded equity"="#01426a",
    "Unlisted\nequity"="#975a58",
    "Other\nprivate assets"="gray13"
  )) +
  ggpattern::scale_pattern_manual(values = c(
    "Publicly\ntraded equity"="stripe",
    "Unlisted\nequity"="circle",
    "Other\nprivate assets" = "none"
  )) + 
  ylab("Total emissions (MtCO2e)") +
  guides(fill=guide_legend(nrow=1)) +
  theme_minimal() + 
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "none",
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.background = element_rect(color="black")
  ) -> g_emission_asset_type

g_emission_sectors +
  annotation_custom(
    grob = ggplotGrob(g_emission_asset_type), 
    xmin = 5, xmax = 11,   # coord in scala asse x di g_asset_type
    ymin = 80, ymax = 180    # coord in scala asse y di g_asset_type
  ) + theme(
    plot.margin = unit(c(5.5,5.5,-10,5.5), "pt")
  ) -> g_emissions


fig2_aux <- cowplot::ggdraw( cowplot::plot_grid(g_stack_w,
                                                g_emissions,
                                                labels=c('A', 'B'),
                                                nrow=1))


ggsave("charts/fig2.pdf", fig2_aux, width=12, height = 4)


#### Citizenship ####

data_billionaire %>%
  pull(nationality_ISO3) %>% table() %>% 
  as.data.frame() %>%
  as_tibble() %>% 
  rename("name"=".") %>% 
  mutate(
    west_brics=case_when(
      name%in%c("USA", "AUS", "AUT", "CAN", "CHE", "CYP", "CZE", "DEU", "DNK", "ESP", "FRA", 
                "GBR", "GRC", "HUN", "IRL", "ISR", "ITA", "JPN", "KOR", "LIE", "LVA", "MCO", "NLD", "NOR", "NZL", "POL", "PRT", "SWE", "UKR") ~ 1,
      name%in%c("BRA", "RUS", "IND", "CHN", "ZAF") ~ 2,
      T ~3
    )
  ) %>% 
  mutate(group = ifelse(
    name%in%c("USA", "CHN", "RUS", "IND", "DEU", "GBR", "FRA", "CAN", "ITA", "AUS", "TWN", "ISR", "CHE", "BRA", "HKG", "ZAF", "IDN", "SWE", "SGP"),
    as.character(name),
    "ROW"
  )) %>% 
  group_by(group, west_brics) %>% 
  summarise(
    Freq = sum(Freq)
  )  %>% 
  ungroup() %>% 
  mutate(
    group= as.factor(group)
  ) %>% 
  mutate(group = fct_reorder(group, Freq)) %>%
  mutate(group = fct_relevel(group, "ROW", after = 0)) %>% 
  mutate(west_brics = factor(west_brics, 
                             levels = c(1,2,3),
                             labels = c("Western countries", "BRICS", "Rest of the world")
  )) %>% 
  ggplot() +
  geom_bar(aes(x=Freq, y=group, fill=west_brics), stat = "identity") +
  scale_fill_manual(values=c(
    "Western countries"="#2f3e46", 
    "BRICS"="#52796f",
    "Rest of the world"="#cad2c5"
  )) +
  xlab("Number of top 500 billionaires") + 
  ylab("Citizenship") + 
  theme_bw() + 
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) -> gdesc1

(187 + 59 + 25 + 24 + 20 + 16 + 13 + 10)/486
(187)/486

#inset
data_billionaire %>%
  mutate(
    west_brics=case_when(
      nationality_ISO3%in%c("USA", "AUS", "AUT", "CAN", "CHE", "CYP", "CZE", "DEU", "DNK", "ESP", "FRA", 
                            "GBR", "GRC", "HUN", "IRL", "ISR", "ITA", "JPN", "KOR", "LIE", "LVA", "MCO", "NLD", "NOR", "NZL", "POL", "PRT", "SWE", "UKR") ~ 1,
      nationality_ISO3%in%c("BRA", "RUS", "IND", "CHN", "ZAF") ~ 2,
      T ~3
    )
  ) %>% 
  group_by(west_brics) %>% 
  summarise(
    wealth = mean(wealth),
    emission = mean(tot_emission_baseline),
    emission_intensity = mean(emission_intensity)
  ) %>% 
  pivot_longer(cols=c(wealth, emission, emission_intensity)) %>% 
  mutate(west_brics = factor(west_brics, 
                             levels = c(1,2,3),
                             labels = c("Western countries", "BRICS", "Rest of the world")
  ),
  name = factor(name, 
                levels = c("wealth","emission","emission_intensity"),
                labels = c("Average wealth\n(Bln$)", "Average emissions\n(ktCO2e)", "Average emission intensity\n(tCO2e/Mln$)")
  )) %>% 
  ggplot() +
  geom_bar(aes(x=factor(west_brics), y=value, fill=west_brics), stat = "identity") +
  facet_wrap(name~., scales="free") +
  scale_fill_manual(values=c(
    "Western countries"="#2f3e46", 
    "BRICS"="#52796f",
    "Rest of the world"="#cad2c5"
  )) +
  xlab("Number of top 500 billionaires") + 
  ylab("Citizenship") + 
  theme_bw() + 
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_blank(),
    strip.background = element_rect(fill="white"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )  -> gdesc2

gdesc1 +
  annotation_custom(
    grob = ggplotGrob(gdesc2),
    xmin = 30, xmax = 190,
    ymin = 5, ymax = 14
  ) -> ggdesc

ggsave("charts/citizenship.pdf", ggdesc, height = 6, width=8.5)

#### Comparison with literature ####

data_billionaire_public <- data %>% 
  filter(asset_type=="public") %>% 
  group_by(name) %>% 
  summarise(
    wealth = sum(value[asset_type!="misc"]) - sum(value[asset_type=="misc"]),
    tot_emission_baseline = sum(`tCO2e_billionaire_baseline`),
    nationality_ISO3 = nationality_single_res[1],
    multiple_nationality_ISO3 = multiple_nationality_ISO3[1],
    gender = gender[1]
  ) %>% 
  ungroup() %>% 
  mutate(
    wealth = wealth/1000000000, #bln$
    tot_emission_baseline=tot_emission_baseline/1000 #ktCO2e
  ) %>% 
  mutate(
    emission_intensity = tot_emission_baseline/wealth #compute emission intensity ktCO2e/Bln$
  ) 

tibble(
  group = c("Total population", "Bottom 50%", "Middle 40%", "Top 10%", "Top 1%"),
  share_private = c(NA, 3,20,77,41),
  share_all = c(NA, 10,30,60,30),
  tco2_private = c(3.8, 0.2, 2, 32.6, 165.2),
  tco2_all = c(6.4, 1.2, 5, 43.6, 209.5)
) -> data_cr

data_billionaire %>% 
  mutate(tot_emission_baseline=tot_emission_baseline*1000) %>% 
  summarise(sum_emi=sum(tot_emission_baseline)) %>% 
  mutate(pc_emission = sum_emi/nrow(data_billionaire)) %>% pull(pc_emission) -> pc_em_value

data_cr %>% 
  select(group, tco2_all) %>% 
  add_row(group="Top 500 billionaires", tco2_all = pc_em_value) %>% 
  mutate(
    ratio_to_billionaires = round(tco2_all[group=="Top 500 billionaires"]/tco2_all,0)
  ) %>% 
  rename(
    "Group"=group,
    "Per capita emissions (tCO2e)"=tco2_all,
    "Billionaires-to-group ratio"=ratio_to_billionaires
  ) %>% 
  mutate(
    across(-Group, scales::comma)
  )-> tab_df


tibble(
  country = c("Top 1% France", "Top 1% United States", "Top 1% Germany", "Top 500 billionaires"),
  emission_intensity = c(23.98, 37.42, 50.54, mean(data_billionaire_public$emission_intensity)),
) %>% 
  mutate(
    "Billionaires-to-group ratio" = round(emission_intensity[country=="Top 500 billionaires"]/emission_intensity,1)
  ) %>% 
  rename("Group"=country, "Emission intensity (tCO2e/Mln$)"=emission_intensity) %>% 
  mutate(
    across(-Group, scales::comma)
  )-> tab_df2


tab_df %>% 
  kableExtra::kbl(
    format = "latex",
    booktabs = TRUE,
    align = c("l", "c", "c"),
    caption = "Insert caption"
  ) %>%
  kableExtra::kable_styling(
    latex_options = c("hold_position"),
    position = "center"
  )

tab_df2 %>% 
  kableExtra::kbl(
    format = "latex",
    booktabs = TRUE,
    align = c("l", "c", "c"),
    caption = "Insert caption"
  ) %>%
  kableExtra::kable_styling(
    latex_options = c("hold_position"),
    position = "center"
  )

#for footnote when not excluding private the value is
mean(data_billionaire$emission_intensity)

#### Taxation ####

#bring back to $ e ton
data_billionaire_for_taxation <- data_billionaire %>% 
  mutate(
    wealth = wealth*1000000000,
    tot_emission_baseline = tot_emission_baseline*1000
  )

vec_scagl_b <- quantile(data_billionaire_for_taxation$tot_emission_baseline,seq(0.1,0.9, by=0.1))

#tax rates vector
vec_aliquota_high_progressive_b <- c(0.01,0.02,0.03) 
vec_aliquota_flat_b <- 0.0136 #manually adjusted after seeing results to make it comparable 

#compute revenues in three taxation schemes
data_billionaire_for_taxation %>%
  mutate(
    scaglione_baseline = case_when(
      tot_emission_baseline >= 0 & tot_emission_baseline <= vec_scagl_b[5] ~ 1,
      tot_emission_baseline > vec_scagl_b[5] & tot_emission_baseline <= vec_scagl_b[9] ~ 2,
      TRUE ~ 3
    )
  ) %>% #compute base revenues
  mutate(
    tax_revenue_baseline = case_when(
      scaglione_baseline==1 ~ wealth*vec_aliquota_high_progressive_b[1],
      scaglione_baseline==2 ~ ((tot_emission_baseline - vec_scagl_b[5])/tot_emission_baseline)*wealth*vec_aliquota_high_progressive_b[2] +(1 - ((tot_emission_baseline - vec_scagl_b[5])/tot_emission_baseline))*wealth*vec_aliquota_high_progressive_b[1],
      TRUE ~ ((tot_emission_baseline - vec_scagl_b[9])/tot_emission_baseline)*wealth*vec_aliquota_high_progressive_b[3] + ((vec_scagl_b[9] - vec_scagl_b[5])/tot_emission_baseline)*wealth*vec_aliquota_high_progressive_b[2]  + (vec_scagl_b[5]/tot_emission_baseline)*wealth*vec_aliquota_high_progressive_b[1]
    ),
    tax_revenue_flat = wealth*vec_aliquota_flat_b,
    tax_revenue_cr = tot_emission_baseline*150
  ) %>%
  #assume elasticity for reshuffling
  mutate( #behavioral responses: wealth and emissions fall by 17% because of portfolio reshuffling
    reduced_emissions = tot_emission_baseline*0.83,
    reduced_wealth = ifelse(tot_emission_baseline>0, wealth*0.83, wealth)
  ) %>% 
  mutate(
    scaglione_baseline_el = case_when(
      reduced_emissions >= 0 & reduced_emissions <= vec_scagl_b[5] ~ 1,
      reduced_emissions > vec_scagl_b[5] & reduced_emissions <= vec_scagl_b[9] ~ 2,
      TRUE ~ 3
    )
  ) %>% 
  #compute base revenues with elasticity
  mutate(
    tax_revenue_baseline_el = case_when(
      scaglione_baseline_el==1 ~ reduced_wealth*vec_aliquota_high_progressive_b[1],
      scaglione_baseline_el==2 ~ ((reduced_emissions - vec_scagl_b[5])/reduced_emissions)*reduced_wealth*vec_aliquota_high_progressive_b[2] +(1 - ((reduced_emissions - vec_scagl_b[5])/reduced_emissions))*reduced_wealth*vec_aliquota_high_progressive_b[1],
      TRUE ~ ((reduced_emissions - vec_scagl_b[9])/reduced_emissions)*reduced_wealth*vec_aliquota_high_progressive_b[3] + ((vec_scagl_b[9] - vec_scagl_b[5])/reduced_emissions)*reduced_wealth*vec_aliquota_high_progressive_b[2]  + (vec_scagl_b[5]/reduced_emissions)*reduced_wealth*vec_aliquota_high_progressive_b[1]
    ),
    tax_revenue_flat_el = reduced_wealth*vec_aliquota_flat_b,
    tax_revenue_cr_el = reduced_emissions*150
  ) -> data_tax_revenues_billionaires


data_tax_revenues_billionaires %>% 
  select(name, wealth, starts_with("tax")) %>% 
  pivot_longer(cols = starts_with("tax"),
               names_to = "variable",
               values_to = "revenue") %>% 
  mutate(
    scheme = case_when(
      str_detect(variable, "baseline") ~ "baseline",
      str_detect(variable, "flat") ~ "flat",
      str_detect(variable, "cr") ~ "cr",
      TRUE ~ NA_character_
    ),
    suffix = case_when(
      str_detect(variable, "_el$") ~ "reshuffling",
      TRUE ~ "base"
    )
  ) -> revenues_billionaires_long

revenues_billionaires_long %<>% 
  mutate(
    effective_tax_rate = revenue/wealth
  )

revenues_billionaires_long %>% 
  group_by(scheme, suffix) %>% 
  summarise(mean(effective_tax_rate)) #to check comparability

revenues_billionaires_long %>% 
  group_by(scheme, suffix) %>% 
  summarise(
    revenue = sum(revenue)/1000000000
  ) %>% 
  ungroup() %>% 
  mutate(
    scheme = factor(scheme,
                    levels = c("baseline", "flat", "cr"),
                    labels = c("Carbon wealth tax\n(1% B50, 2% P50P90, 3% T10)", "Wealth tax\n(Equivalent average effective tax rate ≈ 1.4%)", "Carbon tax\n(150$ per Ton)")),
    suffix = factor(suffix,
                    levels = c("base", "reshuffling"),
                    labels = c("Full taxable base", "Reduced taxable base (Advani & Tarrant, 2021)"))
  ) %>% 
  ggplot() + 
  geom_bar(aes(x=scheme, y=revenue, fill=scheme, alpha=suffix), stat = "identity", position = "dodge") + 
  geom_text(aes(x=scheme, y=revenue+6, group=suffix, label = round(revenue,0)),position = position_dodge(width = 0.9)) + 
  geom_hline(aes(yintercept = 102.48), linetype=2) +
  annotate(geom="text", x=3, y=102.48+12, label="Global carbon pricing\nannual revenues", size=3) + 
  ylab("Tax revenues (billion $)") + 
  scale_fill_manual(values=c("Carbon wealth tax\n(1% B50, 2% P50P90, 3% T10)"="#2e4057", "Wealth tax\n(Equivalent average effective tax rate ≈ 1.4%)"="#04a6a8", "Carbon tax\n(150$ per Ton)"="#dc810d")) +
  scale_alpha_manual(values=c("Full taxable base"=1, "Reduced taxable base (Advani & Tarrant, 2021)"= 0.5)) +
  guides(  fill  = "none",
           alpha = guide_legend(order = 2, nrow = 1)) +
  theme_bw() + 
  theme(
    axis.title.x = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
    legend.box     = "vertical", 
    legend.title = element_blank()
  )  -> g_tax_three

ggsave("charts/fig4.pdf", g_tax_three, width=8, height = 4.2, device = cairo_pdf)

#effective tax rates

left_join(
  revenues_billionaires_long %>% 
    filter(suffix=="base") %>% 
    group_by(variable) %>%  
    mutate(decile = ntile(wealth, 10)) %>% 
    ungroup() %>%
    group_by(scheme, decile) %>% 
    summarise(
      effective_tax_rate_wealth = mean(effective_tax_rate)
    ) %>% 
    ungroup,
  revenues_billionaires_long %>% 
    filter(suffix=="base") %>% 
    left_join(
      .,
      data_tax_revenues_billionaires %>% select(name, tot_emission_baseline),
      by="name"
    ) %>% 
    group_by(variable) %>%  
    mutate(decile = ntile(tot_emission_baseline, 10)) %>% 
    ungroup() %>%
    group_by(scheme, decile) %>% 
    summarise(
      effective_tax_rate_emissions = mean(effective_tax_rate)
    ) %>% 
    ungroup,
  by=c("scheme", "decile")
) %>% 
  pivot_longer(-c(scheme, decile)) %>% 
  mutate(
    scheme = factor(scheme,
                    levels = c("baseline", "flat", "cr"),
                    labels = c("Carbon wealth tax\n(1% B50, 2% P50P90, 3% T10)", "Wealth tax\n(Equivalent average effective tax rate ≈ 1.4%)", "Carbon tax\n(150$ per Ton)")),
    name = factor(name, 
                  levels = c("effective_tax_rate_wealth", "effective_tax_rate_emissions"),
                  labels=c("Wealth deciles", "Emission deciles"))
  ) %>% 
  ggplot() + 
  geom_point(aes(x=decile, y=value, color=scheme, shape=scheme)) +
  geom_line(aes(x=decile, y=value, color=scheme)) +
  facet_wrap(name~., scales="free") +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(breaks = 1:10) +
  xlab("Billionaire decile")+
  ylab("Average effective tax rate") +
  scale_color_manual(values=c("Carbon wealth tax\n(1% B50, 2% P50P90, 3% T10)"="#2e4057", "Wealth tax\n(Equivalent average effective tax rate ≈ 1.4%)"="#04a6a8", "Carbon tax\n(150$ per Ton)"="#dc810d")) +
  scale_shape_manual(values=c("Carbon wealth tax\n(1% B50, 2% P50P90, 3% T10)"=16, "Wealth tax\n(Equivalent average effective tax rate ≈ 1.4%)"=1, "Carbon tax\n(150$ per Ton)"=15)) +
  theme_minimal() + 
  theme(
    panel.grid.minor = element_blank(),
    legend.title = element_blank(),
    legend.position = "bottom",
    axis.title.x = element_blank()
  ) -> g_prog

ggsave("charts/fig5.pdf", g_prog, width=10, height = 3.5, device = cairo_pdf)

#### Comparing us and chancel to baku ####

mb <- 1.984 #multiplier needed for us to reach baku (300), adjusted manually
mcr <- 3.54 #multiplier needed for cr to reach baku (300), adjusted manually

#compute revenues
data_billionaire_for_taxation %>%
  mutate(
    scaglione_baseline = case_when(
      tot_emission_baseline >= 0 & tot_emission_baseline <= vec_scagl_b[5] ~ 1,
      tot_emission_baseline > vec_scagl_b[5] & tot_emission_baseline <= vec_scagl_b[9] ~ 2,
      TRUE ~ 3
    )
  ) %>%
  mutate(
    tax_revenue_baseline = case_when(
      scaglione_baseline==1 ~ wealth*vec_aliquota_high_progressive_b[1],
      scaglione_baseline==2 ~ ((tot_emission_baseline - vec_scagl_b[5])/tot_emission_baseline)*wealth*vec_aliquota_high_progressive_b[2] +(1 - ((tot_emission_baseline - vec_scagl_b[5])/tot_emission_baseline))*wealth*vec_aliquota_high_progressive_b[1],
      TRUE ~ ((tot_emission_baseline - vec_scagl_b[9])/tot_emission_baseline)*wealth*vec_aliquota_high_progressive_b[3] + ((vec_scagl_b[9] - vec_scagl_b[5])/tot_emission_baseline)*wealth*vec_aliquota_high_progressive_b[2]  + (vec_scagl_b[5]/tot_emission_baseline)*wealth*vec_aliquota_high_progressive_b[1]
    ),
    tax_revenue_baseline_baku = case_when(
      scaglione_baseline==1 ~ wealth*vec_aliquota_high_progressive_b[1]*mb,
      scaglione_baseline==2 ~ ((tot_emission_baseline - vec_scagl_b[5])/tot_emission_baseline)*wealth*vec_aliquota_high_progressive_b[2]*mb +(1 - ((tot_emission_baseline - vec_scagl_b[5])/tot_emission_baseline))*wealth*vec_aliquota_high_progressive_b[1]*mb,
      TRUE ~ ((tot_emission_baseline - vec_scagl_b[9])/tot_emission_baseline)*wealth*vec_aliquota_high_progressive_b[3]*mb + ((vec_scagl_b[9] - vec_scagl_b[5])/tot_emission_baseline)*wealth*vec_aliquota_high_progressive_b[2]*mb  + (vec_scagl_b[5]/tot_emission_baseline)*wealth*vec_aliquota_high_progressive_b[1]*mb
    ),
    tax_revenue_cr = tot_emission_baseline*150,
    tax_revenue_cr_baku = tot_emission_baseline*150*mcr
    
  ) -> data_tax_revenues_compare_baku


data_tax_revenues_compare_baku %>% 
  select(name, wealth, starts_with("tax")) %>% 
  pivot_longer(cols = starts_with("tax"),
               names_to = "variable",
               values_to = "revenue") %>% 
  mutate(
    scheme = case_when(
      str_detect(variable, "baseline") ~ "baseline",
      str_detect(variable, "cr") ~ "cr",
      TRUE ~ NA_character_
    ),
    suffix = case_when(
      str_detect(variable, "_baku$") ~ "baku",
      TRUE ~ "base"
    )
  ) -> revenues_baku_long

revenues_baku_long %>% 
  group_by(scheme, suffix) %>% 
  summarise(
    revenue = sum(revenue)/1000000000
  ) #to check that we just reach 300 ajusting mb and mcr

revenues_baku_long %<>% 
  mutate(
    effective_tax_rate = revenue/wealth
  )

revenues_baku_long %>% 
  left_join(
    .,
    data_tax_revenues_compare_baku %>% select(name, tot_emission_baseline),
    by="name"
  ) %>% 
  group_by(variable) %>%  
  mutate(decile = ntile(tot_emission_baseline, 10)) %>% 
  ungroup() %>%
  group_by(scheme, suffix, decile) %>% 
  summarise(
    effective_tax_rate = mean(effective_tax_rate)
  ) %>% 
  ungroup %>% 
  mutate(
    scheme = factor(scheme,
                    levels = c("baseline", "cr"),
                    labels = c("Carbon wealth tax", "Carbon tax")),
    suffix = factor(suffix,
                    levels = c("base", "baku"),
                    labels = c("Baseline", "Taxation rate collecting\nCOP29 financing goal")
    )
  ) %>% 
  ggplot() + 
  geom_point(aes(x=decile, y=effective_tax_rate, color=scheme, alpha=suffix,shape=suffix)) +
  geom_line(aes(x=decile, y=effective_tax_rate, color=scheme, alpha=suffix, linetype=suffix), size=0.8) +
  geom_hline(yintercept = 0.075, linetype=2) +
  annotate("text", x=7, y=0.082, label="Average rate of return to wealth", size=3) +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_x_continuous(breaks = 1:10) +
  xlab("Emission decile")+
  ylab("Average effective tax rate") +
  scale_color_manual(values=c("Carbon wealth tax"="#2e4057", "Carbon tax"="#dc810d"), name=NULL) +
  scale_alpha_manual(values=c("Baseline"=0.5, "Taxation rate collecting\nCOP29 financing goal"=1), name=NULL) +
  scale_linetype_manual(values=c("Baseline"=2, "Taxation rate collecting\nCOP29 financing goal"=1), name=NULL) +
  scale_shape_manual(values=c("Baseline"=1, "Taxation rate collecting\nCOP29 financing goal"=16), name=NULL) +
  guides(  color  = guide_legend(order = 2, nrow = 1, byrow = TRUE, override.aes = list(alpha = 1, shape=NA)),
           alpha    = guide_legend(
             order = 1, nrow = 1, byrow = TRUE,
             override.aes = list(
               linetype = c(1,2),
               shape    = c(16,1),
               alpha    = c(1,1),
               color    = "gray71"
             ),
             reverse = TRUE 
           ),
           linetype = "none",
           shape    = "none") +
  theme_minimal() + 
  theme(
    panel.grid.minor = element_blank(),
    legend.box     = "vertical", 
    legend.position = "inside",
    legend.position.inside = c(0.3,0.75),
    legend.key.width = unit(1, 'cm'),
    legend.box.background = element_rect(fill="white"),
  ) -> g_prog_emissions_baku


#### Hard to abate ####

data %>% 
  filter(asset_type2%in%c("public equity", "private equity")) %>% 
  filter(!is.na(ICB_Subsector_name)) %>%
  mutate(
    hard_to_abate = case_when(
      # strictly hta
      ICB_Subsector_name=="Aerospace" ~ 1,
      ICB_Subsector_name=="Airlines" ~ 1,
      ICB_Subsector_name=="Aluminum" ~ 1,
      ICB_Subsector_name=="Cement" ~ 1,
      ICB_Subsector_name=="Chemicals and Synthetic Fibers" ~ 1,
      ICB_Subsector_name=="Chemicals: Diversified" ~ 1,
      ICB_Subsector_name=="Iron and Steel" ~ 1,
      ICB_Subsector_name=="Marine Transportation" ~ 1,
      ICB_Subsector_name=="Pharmaceuticals" ~ 1,
      ICB_Subsector_name=="Specialty Chemicals" ~ 1,
      ICB_Subsector_name=="Nonferrous Metals" ~ 1,
      ICB_Subsector_name=="Metal Fabricating" ~ 1,
      ICB_Subsector_name=="General Mining" ~ 1,
      ICB_Subsector_name=="Gold Mining" ~ 1,
      ICB_Subsector_name=="Paints and Coatings" ~ 1,
      ICB_Subsector_name=="Copper" ~ 1,
      ICB_Subsector_name=="Semiconductors" ~ 1,
      ICB_Subsector_name=="Paper" ~ 1,
      ICB_Subsector_name=="Aerospace & Defense" ~ 1,
      ICB_Subsector_name=="Glass" ~ 1,
      ICB_Subsector_name=="Plastics" ~ 1,
      # fossil fuels
      ICB_Subsector_name=="Integrated Oil and Gas" ~ 2,
      ICB_Subsector_name=="Pipelines" ~ 2,
      ICB_Subsector_name=="Oil Equipment and Services" ~ 2,
      ICB_Subsector_name=="Oil Refining and Marketing" ~ 2,
      ICB_Subsector_name=="Oil: Crude Producers" ~ 2,
      ICB_Subsector_name=="Coal" ~ 2,
      ICB_Subsector_name=="Gas Distribution" ~ 2,
      ICB_Subsector_name=="Conventional Electricity" ~ 2,
      ICB_Subsector_name=="Oil, Gas and Coal" ~ 2,
      # mildly hta
      ICB_Subsector_name=="Fertilizers"~3,
      ICB_Subsector_name=="Transportation Services" ~ 3,
      ICB_Subsector_name=="Auto Parts" ~ 3,
      ICB_Subsector_name=="Automobiles" ~ 3,
      ICB_Subsector_name=="Machinery: Agricultural" ~ 3,
      ICB_Subsector_name=="Machinery: Construction and Handling" ~ 3,
      ICB_Subsector_name=="Machinery: Industrial" ~ 3,
      ICB_Subsector_name=="Machinery: Tools " ~ 3,
      ICB_Subsector_name=="Building Materials: Other" ~ 3,
      ICB_Subsector_name=="Construction" ~ 3,
      ICB_Subsector_name=="Containers and Packaging" ~ 3,
      ICB_Subsector_name=="Delivery Services" ~ 3,
      ICB_Subsector_name=="Diversified Industrials" ~ 3,
      ICB_Subsector_name=="Waste & Disposal Services" ~ 3,
      ICB_Subsector_name=="Waste and Disposal Services" ~ 3,
      ICB_Subsector_name=="Machinery: Engines" ~ 3,
      ICB_Subsector_name=="Machinery: Specialty" ~ 3,
      ICB_Subsector_name=="Diversified Materials" ~ 3,
      # non hta
      TRUE ~4
    )
  ) %>% 
  mutate(
    hb_1 = ifelse(hard_to_abate==1, 1, 0),
    hb_2 = ifelse(hard_to_abate%in%c(1,2), 1, 0),
    hb_3 = ifelse(hard_to_abate%in%c(1,2,3), 1, 0),
  ) %>% 
  group_by(name) %>%
  summarise(
    share_emission_hb_1=sum(tCO2e_billionaire_baseline[hb_1==1])/sum(tCO2e_billionaire_baseline),
    share_emission_hb_2=sum(tCO2e_billionaire_baseline[hb_2==1])/sum(tCO2e_billionaire_baseline),
    share_emission_hb_3=sum(tCO2e_billionaire_baseline[hb_3==1])/sum(tCO2e_billionaire_baseline),
    share_wealth_hb_1=sum(value[hb_1==1])/sum(value),
    share_wealth_hb_2=sum(value[hb_2==1])/sum(value),
    share_wealth_hb_3=sum(value[hb_3==1])/sum(value)
  ) %>% ungroup() -> hb_data

#### Table hard to abate ####

revenues_baku_long %>% 
  filter(scheme=="baseline", suffix=="base") %>%
  left_join(
    .,
    data_tax_revenues_compare_baku %>% select(name, tot_emission_baseline, emission_intensity),
    by="name"
  ) %>% 
  left_join(
    .,
    hb_data,
    by="name"
  ) %>% 
  mutate(
    share_emission_hb_1 = ifelse(is.na(share_emission_hb_1), 0, share_emission_hb_1),
    share_emission_hb_2 = ifelse(is.na(share_emission_hb_2), 0, share_emission_hb_2),
    share_emission_hb_3 = ifelse(is.na(share_emission_hb_3), 0, share_emission_hb_3),
    share_wealth_hb_1 = ifelse(is.na(share_wealth_hb_1), 0, share_wealth_hb_1),
    share_wealth_hb_2 = ifelse(is.na(share_wealth_hb_2), 0, share_wealth_hb_2),
    share_wealth_hb_3 = ifelse(is.na(share_wealth_hb_3), 0, share_wealth_hb_3)
  ) %>% 
  mutate(
    decile_emission = ntile(tot_emission_baseline, 10),
    decile_wealth = ntile(wealth, 10),
    decile_em_int = ntile(emission_intensity, 10),
    decile_hb = ntile(share_emission_hb_1,10),
    hb3 = case_when(
      share_emission_hb_1<0.001 ~1,
      share_emission_hb_1>0.9 ~3,
      T ~2
    )
  ) %>% ungroup() %>% 
  select(-variable, -revenue, -scheme, -suffix, -effective_tax_rate) -> data_comparison_hba


data_comparison_hba %>% 
  filter(hb3==3) %>%
  filter(decile_emission%in%c(9,10) | decile_wealth%in%c(9,10)) %>% 
  select(name, wealth, decile_wealth, tot_emission_baseline, decile_emission) %>%
  arrange(-decile_wealth) %>% 
  left_join(
    .,
    data %>% 
      filter(asset_type!="misc") %>% 
      group_by(name, ICB_Subsector_name) %>% 
      summarise(
        sum_emissions = sum(`tCO2e_billionaire_baseline`),
        sum_wealth = sum(value)
      ) %>% 
      ungroup() %>% 
      group_by(name) %>% 
      mutate(
        max_emission = max(sum_emissions, na.rm=T),
        max_wealth = max(sum_wealth, na.rm=T)
      ) %>% 
      ungroup() %>% 
      filter(sum_emissions==max_emission) %>% 
      select(name, ICB_Subsector_name),
    by="name"
  ) %>% 
  mutate(
    wealth = round(wealth/1000000000,1), #bln$
    tot_emission_baseline=round(tot_emission_baseline/1000,1) #ktCO2e
  ) -> table_hb_to_appendix

table_hb_to_appendix %>% 
  kableExtra::kbl(
    format = "latex",
    booktabs = TRUE,
    align = c("l", "c", "c", "c", "c", "c"),
    caption = "Insert caption"
  ) %>%
  kableExtra::kable_styling(
    latex_options = c("hold_position"),
    position = "center"
  )

data_comparison_hba %>% 
  filter(hb3==3) %>% nrow()

data_comparison_hba %>% 
  filter(hb3==3) %>% 
  filter(decile_wealth%in%c(9,10) | decile_emission%in%c(9,10)) %>% nrow()

22/44 #50% 

###### Figures hard to abate ####

revenues_baku_long %>% 
  filter(scheme=="baseline", suffix=="base") %>% 
  left_join(
    .,
    data_tax_revenues_compare_baku %>% select(name, tot_emission_baseline),
    by="name"
  ) %>% 
  left_join(
    .,
    hb_data,
    by="name"
  ) %>% 
  mutate(
    share_emission_hb_1 = ifelse(is.na(share_emission_hb_1), 0, share_emission_hb_1),
    share_emission_hb_2 = ifelse(is.na(share_emission_hb_2), 0, share_emission_hb_2),
    share_emission_hb_3 = ifelse(is.na(share_emission_hb_3), 0, share_emission_hb_3),
    share_wealth_hb_1 = ifelse(is.na(share_wealth_hb_1), 0, share_wealth_hb_1),
    share_wealth_hb_2 = ifelse(is.na(share_wealth_hb_2), 0, share_wealth_hb_2),
    share_wealth_hb_3 = ifelse(is.na(share_wealth_hb_3), 0, share_wealth_hb_3)
  ) %>% 
  group_by(variable) %>%  
  mutate(
    decile = ntile(tot_emission_baseline, 10)
  ) %>% 
  ungroup() %>% 
  group_by(decile) %>% 
  summarise(
    share_emission_hb_1 = mean(share_emission_hb_1),
    share_emission_hb_2 = mean(share_emission_hb_2),
    share_emission_hb_3 = mean(share_emission_hb_3),
    share_wealth_hb_1 = mean(share_wealth_hb_1),
    share_wealth_hb_2 = mean(share_wealth_hb_2),
    share_wealth_hb_3 = mean(share_wealth_hb_3)
  ) %>% 
  ungroup() %>% 
  pivot_longer(-decile) -> hb_long_data

revenues_baku_long %>% 
  filter(scheme=="baseline", suffix=="base") %>% 
  left_join(
    .,
    data_tax_revenues_compare_baku %>% select(name, tot_emission_baseline),
    by="name"
  ) %>% 
  left_join(
    .,
    hb_data,
    by="name"
  ) %>% 
  mutate(
    share_emission_hb_1 = ifelse(is.na(share_emission_hb_1), 0, share_emission_hb_1),
    share_emission_hb_2 = ifelse(is.na(share_emission_hb_2), 0, share_emission_hb_2),
    share_emission_hb_3 = ifelse(is.na(share_emission_hb_3), 0, share_emission_hb_3),
    share_wealth_hb_1 = ifelse(is.na(share_wealth_hb_1), 0, share_wealth_hb_1),
    share_wealth_hb_2 = ifelse(is.na(share_wealth_hb_2), 0, share_wealth_hb_2),
    share_wealth_hb_3 = ifelse(is.na(share_wealth_hb_3), 0, share_wealth_hb_3)
  ) %>% 
  group_by(variable) %>%  
  mutate(
    decile = ntile(wealth, 10)
  ) %>% 
  ungroup() %>% 
  group_by(decile) %>% 
  summarise(
    share_emission_hb_1 = mean(share_emission_hb_1),
    share_emission_hb_2 = mean(share_emission_hb_2),
    share_emission_hb_3 = mean(share_emission_hb_3),
    share_wealth_hb_1 = mean(share_wealth_hb_1),
    share_wealth_hb_2 = mean(share_wealth_hb_2),
    share_wealth_hb_3 = mean(share_wealth_hb_3)
  ) %>% 
  ungroup() %>% 
  pivot_longer(-decile) -> hb_long_data_w

rbind(
  hb_long_data %>% 
    mutate(
      type=case_when(name%in%c("share_emission_hb_1", "share_emission_hb_2", "share_emission_hb_3") ~ "emission", TRUE ~ "wealth"),
      cat=case_when(name%in%c("share_emission_hb_1", "share_wealth_hb_1") ~ "1", name%in%c("share_emission_hb_2", "share_wealth_hb_2") ~ "2", TRUE ~ "3")
    ) %>% 
    select(-name) %>% 
    add_column(decile_type="emission"),
  hb_long_data_w %>% 
    mutate(
      type=case_when(name%in%c("share_emission_hb_1", "share_emission_hb_2", "share_emission_hb_3") ~ "emission", TRUE ~ "wealth"),
      cat=case_when(name%in%c("share_emission_hb_1", "share_wealth_hb_1") ~ "1", name%in%c("share_emission_hb_2", "share_wealth_hb_2") ~ "2", TRUE ~ "3")
    ) %>% 
    select(-name) %>% 
    add_column(decile_type="wealth")
) -> data_plot_hba

data_plot_hba %>% 
  filter(type=="emission") %>% 
  mutate(
    col = paste0(decile_type,cat)
  ) %>% 
  mutate(
    decile_type = factor(
      decile_type,
      levels = c("emission", "wealth"),
      labels = c("Emission decile", "Wealth decile")
    ),
    cat = factor(
      cat,
      levels = c("1", "2", "3"),
      labels = c("Strictly HTA", "Strictly HTA + Fossil Fuels", "Strictly HTA + Fossil Fuels + Mildly HTA")
    )
  ) %>% 
  ggplot() + 
  geom_point(aes(x=decile, y=value, color=col, shape=cat)) +
  geom_line(aes(x=decile, y=value, color=col, linetype=cat, linewidth=cat)) +
  facet_wrap(.~decile_type, strip.position="bottom") +
  scale_x_continuous(breaks=1:10) + 
  scale_y_continuous(labels=function(x) scales::percent(x, accuracy = 1)) +
  scale_color_manual(values=c("emission1"="steelblue", "wealth1"="indianred3", "emission2"="#000000", "wealth2"="#000000", "emission3"="#969696", "wealth3"="#969696")) +
  scale_linetype_manual(values=c("Strictly HTA"=1, "Strictly HTA + Fossil Fuels"=2, "Strictly HTA + Fossil Fuels + Mildly HTA"=3))+ 
  scale_linewidth_manual(values=c("Strictly HTA"=0.75, "Strictly HTA + Fossil Fuels"=0.45, "Strictly HTA + Fossil Fuels + Mildly HTA"=0.45))+ 
  xlab("Decile") + 
  ylab("Average share of emissions from HTA sectors") +
  guides( 
           color="none"
  )+
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    strip.text = element_text(size=11),
    legend.position = "bottom",
    legend.key.width = unit(1, 'cm')
  ) -> g_hta_emissions

ggsave("charts/hta_emissions.pdf", g_hta_emissions, width=10, height = 5, device = cairo_pdf)

data_plot_hba %>% 
  filter(type=="wealth") %>% 
  mutate(
    col = paste0(decile_type,cat)
  ) %>% 
  mutate(
    decile_type = factor(
      decile_type,
      levels = c("emission", "wealth"),
      labels = c("Emission decile", "Wealth decile")
    ),
    cat = factor(
      cat,
      levels = c("1", "2", "3"),
      labels = c("Strictly HTA", "Strictly HTA + Fossil Fuels", "Strictly HTA + Fossil Fuels + Mildly HTA")
    )
  ) %>% 
  ggplot() + 
  geom_point(aes(x=decile, y=value, color=col, shape=cat)) +
  geom_line(aes(x=decile, y=value, color=col, linetype=cat, linewidth=cat)) +
  facet_wrap(.~decile_type, strip.position="bottom") +
  scale_x_continuous(breaks=1:10) + 
  scale_y_continuous(labels=function(x) scales::percent(x, accuracy = 1)) +
  scale_color_manual(values=c("emission1"="steelblue", "wealth1"="indianred3", "emission2"="#000000", "wealth2"="#000000", "emission3"="#969696", "wealth3"="#969696")) +
  scale_linetype_manual(values=c("Strictly HTA"=1, "Strictly HTA + Fossil Fuels"=2, "Strictly HTA + Fossil Fuels + Mildly HTA"=3))+ 
  scale_linewidth_manual(values=c("Strictly HTA"=0.75, "Strictly HTA + Fossil Fuels"=0.45, "Strictly HTA + Fossil Fuels + Mildly HTA"=0.45))+ 
  xlab("Decile") + 
  ylab("Average share of billionaires' wealth from HTA sectors") +
  guides( 
    color="none"
  )+
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_blank(),
    strip.text = element_text(size=11),
    legend.position = "bottom",
    legend.key.width = unit(1, 'cm')
  ) -> g_hta_wealth

ggsave("charts/hta_wealth.pdf", g_hta_wealth, width=10, height = 5, device = cairo_pdf)

#add to "baku" figure

hb_long_data %>% 
  filter(name=="share_emission_hb_1") %>% 
  ggplot() +
  geom_rect(
    aes(xmin = decile-0.5, xmax = decile+0.5, ymin = 0.22, ymax = 0.26, fill=value), alpha=0.4,
  ) +
  scale_fill_gradient(low = "#ece7f2", high = "#045a8d", name="Share of HTA emissions", labels=function(x) scales::percent(x, accuracy = 1), breaks=c(0.01, 0.2, 0.6) )+
  geom_label(
    data=hb_long_data %>% 
      filter(name=="share_emission_hb_1"),
    aes(x=decile, y=0.24, label=paste0(round(value*100,0), "%")), size=3
  ) + 
  scale_x_continuous(limits = c(0.5,10.5), expand=c(0.0000001,0)) +
  xlab("Average share of emissions from HTA sectors") +
  ylab("a") +
  guides(fill=guide_legend(direction="horizontal")) + 
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(color="white"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_line(color="white"),
    axis.title.y = element_text(color="white"),
    legend.position = "none",
  ) -> g_hta

g_prog_emissions_baku_hta <- cowplot::plot_grid(g_prog_emissions_baku, g_hta, nrow=2, rel_heights = c(0.83,0.17))

ggsave("charts/fig6.pdf", g_prog_emissions_baku_hta, width=8, height = 5, scale=1)

#### Toy chart #####

toy_data <- data.frame(
  id = c("A", "B"),
  emissions = c(100, 200),
  wealth = c(200, 200)
)

toy_data_long <- toy_data %>%
  pivot_longer(cols = c(emissions, wealth),
               names_to = "variable",
               values_to = "value")

toy_data <- tibble(
  y_init = c(1,2.75,1,2.75),
  y_end = c(2,3.75,2,3.75),
  x_init = c(0,0,300,300),
  x_end = c(0+200,0+100,300+200,300+200)
)

sc1 <- tibble(
  x_init = c(0,0),
  x_end = c(50,50),
  y_init = c(1,2.75),
  y_end = c(2,3.75)
)

sc2 <- tibble(
  x_init = c(50,50),
  x_end = c(150,100),
  y_init = c(1,2.75),
  y_end = c(2,3.75)
)

sc3 <- tibble(
  x_init = c(150),
  x_end = c(200),
  y_init = c(1),
  y_end = c(2)
)

w1 <- tibble(
  x_init = c(300,300),
  x_end = c(350,400),
  y_init = c(1,2.75),
  y_end = c(2,3.75)
)

w2 <- tibble(
  x_init = c(350,400),
  x_end = c(450,500),
  y_init = c(1,2.75),
  y_end = c(2,3.75)
)

w3 <- tibble(
  x_init = c(450),
  x_end = c(500),
  y_init = c(1),
  y_end = c(2)
)

perc <- tibble(
  x = c(25,25,75, 100, 175, 350,325, 450, 400, 475),
  y = c(3.25,1.5,3.25, 1.5, 1.5,3.25,1.5, 3.25, 1.5, 1.5),
  label = c("50%", "25%", "50%", "50%", "25%", 
            "50%", "25%", "50%", "50%", "25%")
)

nn <- tibble(
  x = c(50,50,100,150,200, 500, 500),
  y = c(2.75,1,2.75,1,1, 2.75, 1) - 0.13,
  label = c("50~'(' * p[50] * ')'", "50~'(' * p[50] * ')'", "100", "150~'(' * p[90] * ')'", "200", "200", "200")
)

rev <- tibble(
  x = c(600,600),
  y = c(3.25,1.5),
  label = c("3", "4")
)

titles.x <- tibble(
  x = c(100, 400, 600),
  y = c(4, 4, 4),
  label = c("Emissions", "Wealth", "Revenues")
)

titles.y <- tibble(
  x = c(-25, -25),
  y = c(1.5, 3.25),
  label = c("Billonaire B", "Billionaire A")
)

arrows <- tibble(
  x_init = c(225, 225),
  x_end = c(275, 275),
  y_init = c(3.25, 1.5),
  y_end = c(3.25, 1.5)
)

arrows2 <- tibble(
  x_init = c(225, 225) +300,
  x_end = c(275, 275) +300,
  y_init = c(3.25, 1.5),
  y_end = c(3.25, 1.5)
)

arrows3 <- tibble(
  x_init = c(350, 450, 325, 400, 475),
  x_end = c(350, 450, 325, 400, 475),
  y_init = c(2.75, 2.75, 1, 1, 1) -0.25,
  y_end = c(2.75, 2.75, 1, 1, 1) - 0.1
)

taus <- tibble(
  x = c(350, 450, 325, 400, 475),
  y = c(2.75, 2.75, 1, 1, 1) -0.35,
  label = c("1%", "2%", "1%", "2%", "3%")
)

toy_data %>% 
  ggplot() + 
  geom_rect(data=sc1, aes(xmin=x_init, xmax=x_end, ymin=y_init, ymax=y_end), fill="steelblue", alpha=0.2) +
  geom_rect(data=sc2, aes(xmin=x_init, xmax=x_end, ymin=y_init, ymax=y_end), fill="steelblue", alpha=0.6) +
  geom_rect(data=sc3, aes(xmin=x_init, xmax=x_end, ymin=y_init, ymax=y_end), fill="steelblue", alpha=1) +
  geom_rect(data=w1, aes(xmin=x_init, xmax=x_end, ymin=y_init, ymax=y_end), fill="indianred3", alpha=0.2) +
  geom_rect(data=w2, aes(xmin=x_init, xmax=x_end, ymin=y_init, ymax=y_end), fill="indianred3", alpha=0.6) +
  geom_rect(data=w3, aes(xmin=x_init, xmax=x_end, ymin=y_init, ymax=y_end), fill="indianred3", alpha=1) +
  geom_rect(aes(xmin=x_init, xmax=x_end, ymin=y_init, ymax=y_end), color="black", fill="transparent")+
  geom_text(data=perc, aes(x=x, y=y, label=label)) +
  geom_text(data=nn, aes(x=x, y=y, label=label), parse=TRUE) +
  geom_text(data=taus, aes(x=x, y=y, label=paste0("tau==\"", label, "\"") ), size=3, color="#737373", parse = TRUE) +
  geom_text(data=rev, aes(x=x, y=y, label=label)) +
  geom_text(data=titles.x, aes(x=x, y=y, label=label), fontface = "bold",) +
  geom_text(data=titles.y, aes(x=x, y=y, label=label), fontface = "bold",angle = 90) +
  geom_segment(data=arrows,
               aes(x = x_init, y = y_init, xend = x_end, yend = y_end),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
               color = "black"
  ) +
  geom_segment(data=arrows2,
               aes(x = x_init, y = y_init, xend = x_end, yend = y_end),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
               color = "black"
  ) +
  geom_segment(data=arrows3,
               aes(x = x_init, y = y_init, xend = x_end, yend = y_end),
               arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
               color = "#737373"
  ) +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) -> g_toy

tibble(
  x = seq(0, +1, length.out=2000),
  y= 0.7 * dbeta(x, shape1 = 5, shape2 = 3) + 
    0.10 * dbeta(x, shape1 = 2, shape2 = 3)
) -> dt_dist

components <- sample(1:2, size = 1e6, replace = TRUE, prob = c(0.7, 0.3))

th_q <- ifelse(components == 1,
               rbeta(sum(components == 1), shape1 = 5, shape2 = 3),
               rbeta(sum(components == 2), shape1 = 2, shape2 = 3))

tibble(
  x = seq(0, quantile(th_q, probs = c(0.5)), length.out=2000),
  y= 0.7 * dbeta(x, shape1 = 5, shape2 = 3) + 
    0.10 * dbeta(x, shape1 = 2, shape2 = 3)
) %>% pivot_longer(c(y)) -> dt_dist_q50

tibble(
  x = seq(quantile(th_q, probs = c(0.5)), quantile(th_q, probs = c(0.9)), length.out=2000),
  y= 0.7 * dbeta(x, shape1 = 5, shape2 = 3) + 
    0.10 * dbeta(x, shape1 = 2, shape2 = 3)
) %>% pivot_longer(c(y)) -> dt_dist_q90

tibble(
  x = seq(quantile(th_q, probs = c(0.9)), 1, length.out=2000),
  y= 0.7 * dbeta(x, shape1 = 5, shape2 = 3) + 
    0.10 * dbeta(x, shape1 = 2, shape2 = 3)
) %>% pivot_longer(c(y)) -> dt_dist_qup

dt_dist %>% 
  ggplot() + 
  geom_ribbon(data = dt_dist_q50, aes(x=x, ymin=0, ymax=value), fill="steelblue", alpha=0.2) +
  geom_ribbon(data = dt_dist_q90, aes(x=x, ymin=0, ymax=value), fill="steelblue", alpha=0.6) +
  geom_ribbon(data = dt_dist_qup, aes(x=x, ymin=0, ymax=value), fill="steelblue", alpha=1) +
  annotate("segment", x=dt_dist_q50[nrow(dt_dist_q50),"x"]%>% pull(), y=0, xend=dt_dist_q50[nrow(dt_dist_q50),"x"]%>% pull(), yend=dt_dist_q50[nrow(dt_dist_q50),"value"] %>% pull(), linetype=2, linewidth=0.3) +
  annotate("segment", x=dt_dist_q90[nrow(dt_dist_q90),"x"]%>% pull(), y=0, xend=dt_dist_q90[nrow(dt_dist_q90),"x"]%>% pull(), yend=dt_dist_q90[nrow(dt_dist_q90),"value"] %>% pull(), linetype=2, linewidth=0.3) +
  annotate("text", x=dt_dist_q50[nrow(dt_dist_q50),"x"]%>% pull(), y=-0.15, label=expression(50~(p[50])), color="black", angle=90, size=3) +
  annotate("text", x=dt_dist_q90[nrow(dt_dist_q90),"x"]%>% pull(), y=-0.15, label=expression(150~(p[90])), color="black", angle=90, size=3) +
  geom_line(aes(x=x, y=y)) +
  coord_flip() +
  scale_y_reverse() +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  ) -> g_disth

g_toy + 
  geom_vline(xintercept = -65, linetype = 2, size=0.5) + 
  annotate("text", x=-170, y=4, label="Emission distribution", fontface="bold") +
  annotate("line", x=-205, y=2) +
  annotation_custom(
    grob = ggplotGrob(g_disth), 
    xmin = -225, xmax = -85,
    ymin = 0.75, ymax = 3.90
  ) -> g_toy_dist

ggsave("charts/fig3.pdf", g_toy_dist, width=12, height = 4)

#version without explicit percentiles and distribution

nn2 <- tibble(
  x = c(50,50,100,150,200, 500, 500),
  y = c(2.75,1,2.75,1,1, 2.75, 1) - 0.13,
  label = c("50~'(' * t[1] * ')'", "50~'(' * t[1] * ')'", "100", "150~'(' * t[2] * ')'", "200", "200", "200")
)

toy_data %>% 
  ggplot() + 
  geom_rect(data=sc1, aes(xmin=x_init, xmax=x_end, ymin=y_init, ymax=y_end), fill="steelblue", alpha=0.2) +
  geom_rect(data=sc2, aes(xmin=x_init, xmax=x_end, ymin=y_init, ymax=y_end), fill="steelblue", alpha=0.6) +
  geom_rect(data=sc3, aes(xmin=x_init, xmax=x_end, ymin=y_init, ymax=y_end), fill="steelblue", alpha=1) +
  geom_rect(data=w1, aes(xmin=x_init, xmax=x_end, ymin=y_init, ymax=y_end), fill="indianred3", alpha=0.2) +
  geom_rect(data=w2, aes(xmin=x_init, xmax=x_end, ymin=y_init, ymax=y_end), fill="indianred3", alpha=0.6) +
  geom_rect(data=w3, aes(xmin=x_init, xmax=x_end, ymin=y_init, ymax=y_end), fill="indianred3", alpha=1) +
  geom_rect(aes(xmin=x_init, xmax=x_end, ymin=y_init, ymax=y_end), color="black", fill="transparent")+
  geom_text(data=perc, aes(x=x, y=y, label=label)) +
  geom_text(data=nn2, aes(x=x, y=y, label=label), parse=TRUE) +
  geom_text(data=taus, aes(x=x, y=y, label=paste0("tau==\"", label, "\"") ), size=3, color="#737373", parse = TRUE) +
  geom_text(data=rev, aes(x=x, y=y, label=label)) +
  geom_text(data=titles.x, aes(x=x, y=y, label=label), fontface = "bold",) +
  geom_text(data=titles.y, aes(x=x, y=y, label=label), fontface = "bold",angle = 90) +
  geom_segment(data=arrows,
               aes(x = x_init, y = y_init, xend = x_end, yend = y_end),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
               color = "black"
  ) +
  geom_segment(data=arrows2,
               aes(x = x_init, y = y_init, xend = x_end, yend = y_end),
               arrow = arrow(length = unit(0.3, "cm"), type = "closed"),
               color = "black"
  ) +
  geom_segment(data=arrows3,
               aes(x = x_init, y = y_init, xend = x_end, yend = y_end),
               arrow = arrow(length = unit(0.1, "cm"), type = "closed"),
               color = "#737373"
  ) +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  ) -> g_toy2

ggsave("charts/fig3_nodist.pdf", g_toy2, width=9, height = 4)

#### Various ####

#2024 54,43 billion Tons of CO2e global (https://ourworldindata.org/greenhouse-gas-emissions) accessed on 6/5/2026
(sum(data_billionaire$tot_emission_baseline)*1000/1000000000)/54.43

#WID net personal wealth $ PPP 2024 accessed on 6/5/2026
(sum(data_billionaire$wealth)/1000)/(809.11*1.03)

#top 15 emitters and emission intensities
cbind(
  data_billionaire %>% 
    select(name, tot_emission_baseline) %>% 
    arrange(-tot_emission_baseline) %>% 
    slice(1:15) %>% 
    add_column(rank=1:15, .before=1),
  data_billionaire %>% 
    select(name, emission_intensity) %>% 
    arrange(-emission_intensity) %>% 
    rename(name2=name) %>% 
    slice(1:15) %>% 
    add_column(rank2=1:15, .before=1)
) %>% 
  as.tibble() %>% 
  kableExtra::kbl(
    format = "latex",
    booktabs = TRUE,
    align = c("l", "l", "c","l", "l", "c"),
    caption = "Insert caption"
  ) %>%
  kableExtra::add_header_above(
    c(
      "By emission" = 3,
      "By intensity" = 3)
  ) %>% 
  kableExtra::kable_styling(
    latex_options = c("hold_position"),
    position = "center"
  )



