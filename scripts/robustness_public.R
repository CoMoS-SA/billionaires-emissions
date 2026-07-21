library(tidyverse)
library(magrittr)
library(readxl)

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
    tot_emission_baseline = sum(`tCO2e_billionaire_S1_publicCR`),
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

#### Taxation ####

data_billionaire_for_taxation <- data_billionaire %>% 
  mutate(
    wealth = wealth*1000000000,
    tot_emission_baseline = tot_emission_baseline*1000
  )

vec_scagl_b <- quantile(data_billionaire_for_taxation$tot_emission_baseline,seq(0.1,0.9, by=0.1))

vec_aliquota_high_progressive_b <- c(0.01,0.02,0.03) 
vec_aliquota_flat_b <- 0.0132

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
  summarise(mean(effective_tax_rate))

revenues_billionaires_long %>% 
  group_by(scheme, suffix) %>% 
  summarise(
    revenue = sum(revenue)/1000000000
  ) %>% 
  ungroup() %>% 
  mutate(
    scheme = factor(scheme,
                    levels = c("baseline", "flat", "cr"),
                    labels = c("Carbon wealth tax\n(1% B50, 2% P50P90, 3% T10)", "Wealth tax\n(Equivalent average effective tax rate ≈ 1.3%)", "Carbon tax\n(150$ per Ton)")),
    suffix = factor(suffix,
                    levels = c("base", "reshuffling"),
                    labels = c("Full taxable base", "Reduced taxable base (Advani & Tarrant, 2021)"))
  ) %>% 
  mutate(art_pos = ifelse(scheme=="Carbon tax\n(150$ per Ton)" & suffix=="Full taxable base", revenue+6, revenue+6)) %>% 
  ggplot() + 
  geom_bar(aes(x=scheme, y=revenue, fill=scheme, alpha=suffix), stat = "identity", position = "dodge") + 
  geom_text(aes(x=scheme, y=art_pos, group=suffix, label = round(revenue,0)),position = position_dodge(width = 0.9)) + 
  geom_hline(aes(yintercept = 102.48), linetype=2) +
  annotate(geom="text", x=3, y=102.48+12, label="Global carbon pricing\nannual revenues", size=3) + 
  ylab("Tax revenues (billion $)") + 
  scale_fill_manual(values=c("Carbon wealth tax\n(1% B50, 2% P50P90, 3% T10)"="#2e4057", "Wealth tax\n(Equivalent average effective tax rate ≈ 1.3%)"="#04a6a8", "Carbon tax\n(150$ per Ton)"="#dc810d")) +
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

ggsave("charts/fig4_public_cr.pdf", g_tax_three, width=8, height = 4.2, device = cairo_pdf)

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

ggsave("charts/fig5_public_cr.pdf", g_prog, width=10, height = 3.5, device = cairo_pdf)
