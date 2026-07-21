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

##### Build data on residence ####
data %>% 
  group_by(name) %>% 
  summarise(
    wealth = sum(value[asset_type!="misc"]) - sum(value[asset_type=="misc"]),
    tot_emission_baseline = sum(`tCO2e_billionaire_baseline`),
    residence = residence[1]
  ) %>% 
  ungroup() -> data_billionaire_res

data_billionaire_res %<>% 
  mutate(
    wealth = wealth/1000000000, #bln$
    tot_emission_baseline=tot_emission_baseline/1000 #ktCO2e
  ) %>% 
  mutate(
    emission_intensity = tot_emission_baseline/wealth #ktCO2e/Bln$
  )

data_billionaire_res %<>%
  mutate(res_aux = str_trim(str_replace(residence, ".*,", ""))) %>% 
  mutate(
    res_code = case_when(
      res_aux%in%c("Arizona", "Arkansas", "California", "Colorado", "Connecticut", "District of Columbia",
                 "Florida","Georgia","Hawaii", "Illinois", "Indiana", "Kansas", "Kentucky",
                 "Louisiana", "Maryland", "Massachusetts", "Michigan", "Missouri", "Montana",
                 "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New York",
                 "North Carolina", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Tennessee", "Texas",
                 "Virginia", "Washington", "Wisconsin", "Wyoming"
                 ) ~ "USA",
      res_aux=="Argentina" ~ "ARG",
      res_aux=="Australia" ~ "AUS",
      res_aux=="Austria" ~ "AUT",
      res_aux=="Bahamas" ~ "BHS",
      res_aux=="Belgium" ~ "BEL",
      res_aux=="Brazil" ~ "BRA",
      res_aux=="Canada" ~ "CAN",
      res_aux=="Cayman Islands" ~ "CYM",
      res_aux=="Chile" ~ "CHL",
      res_aux=="China" ~ "CHN",
      res_aux=="Colombia" ~ "COL",
      res_aux=="Denmark" ~ "DNK",
      res_aux=="Egypt" ~ "EGY",
      res_aux=="Eswatini (Swaziland)" ~ "SWZ",
      res_aux=="France" ~ "FRA",
      res_aux=="Germany" ~ "DEU",
      res_aux=="Greece" ~ "GRC",
      res_aux=="Hong Kong" ~ "HKG",
      res_aux=="India" ~ "IND",
      res_aux=="Indonesia" ~ "IDN",
      res_aux=="Israel" ~ "ISR",
      res_aux=="Italy" ~ "ITA",
      res_aux=="Japan" ~ "JPN",
      res_aux=="Kazakhstan" ~ "KAZ",
      res_aux=="Latvia" ~ "LVA",
      res_aux=="Mexico" ~ "MEX",
      res_aux=="Monaco" ~ "MCO",
      res_aux=="New Zealand" ~ "NZL",
      res_aux=="Nigeria" ~ "NGA",
      res_aux=="Panama" ~ "PAN",
      res_aux=="Philippines" ~ "PHL",
      res_aux=="Russia" ~ "RUS",
      res_aux=="Saudi Arabia" ~ "SAU",
      res_aux=="Singapore" ~ "SGP",
      res_aux=="South Africa" ~ "ZAF",
      res_aux=="South Korea" ~ "KOR",
      res_aux=="Spain" ~ "ESP",
      res_aux=="Sweden" ~ "SWE",
      res_aux=="Switzerland" ~ "CHE",
      res_aux=="Taiwan" ~ "TWN",
      res_aux=="Thailand" ~ "THA",
      res_aux=="Ukraine" ~ "UKR",
      res_aux=="United Arab Emirates" ~ "ARE",
      res_aux=="United Kingdom" ~ "GBR",
      res_aux=="Uzbekistan" ~ "UZB"
    )
  ) 

#### Graph like nationality ####

data_billionaire_res %>%
  pull(res_code) %>% table() %>% 
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
  xlab("Number of Top500 Billionaires") + 
  ylab("Citizenship") + 
  theme_bw() + 
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  ) -> gdesc1_res

#inset
data_billionaire_res %>%
  mutate(
    west_brics=case_when(
      res_code%in%c("USA", "AUS", "AUT", "CAN", "CHE", "CYP", "CZE", "DEU", "DNK", "ESP", "FRA", 
                            "GBR", "GRC", "HUN", "IRL", "ISR", "ITA", "JPN", "KOR", "LIE", "LVA", "MCO", "NLD", "NOR", "NZL", "POL", "PRT", "SWE", "UKR") ~ 1,
      res_code%in%c("BRA", "RUS", "IND", "CHN", "ZAF") ~ 2,
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
  )  -> gdesc2_res

gdesc1_res +
  annotation_custom(
    grob = ggplotGrob(gdesc2_res),
    xmin = 30, xmax = 190,
    ymin = 5, ymax = 14
  ) -> ggdesc_res

ggsave("charts/residence.pdf", ggdesc_res, height = 6, width=8.5)

#### Construct data on "movers" ####

data_billionaire %>% 
  left_join(
    .,
    data_billionaire_res %>%  select(name, res_code),
    by="name"
  ) %>% 
  filter(res_code!=nationality_ISO3) %>% 
  select(name, wealth, tot_emission_baseline, nationality_ISO3, res_code) -> data_movers
  
data_movers %>% 
  summarise(
    n=n(),
    wealth=sum(wealth),
    tot_emission_baseline=sum(tot_emission_baseline)
  ) 

data_billionaire %>% 
  summarise(
    n=n(),
    wealth=sum(wealth),
    tot_emission_baseline=sum(tot_emission_baseline)
  )

63/486

1010/9914

92105/564255

#### By numerosity ####

library(ggalluvial)

top_nat <- data_movers %>%
  count(nationality_ISO3, sort = TRUE) %>%
  filter(n > 3) %>%
  pull(nationality_ISO3)

top_res <- data_movers %>%
  count(res_code, sort = TRUE) %>%
  filter(n > 3) %>%
  pull(res_code)

dm <- data_movers %>%
  mutate(
    short_nat = ifelse(nationality_ISO3 %in% top_nat, nationality_ISO3, "Other"),
    short_res = ifelse(res_code %in% top_res, res_code, "Other")
  )

nat_order <- dm %>% count(short_nat, sort = TRUE) %>% pull(short_nat)
res_order <- dm %>% count(short_res, sort = TRUE) %>% pull(short_res)

df <- dm %>% count(short_nat, short_res, name = "n")

ld <- to_lodes_form(
  df,
  key  = "x",
  axes = c("short_nat", "short_res"),
  id   = "alluvium"
)

ld <- ld %>%
  group_by(alluvium) %>%
  mutate(
    origin = stratum[x == "short_nat"][1]
  ) %>%
  ungroup()

pos_nat <- setNames(seq_along(nat_order), nat_order)
pos_res <- setNames(seq_along(res_order), res_order)

ld <- ld %>%
  mutate(
    label = as.character(stratum),
    stratum_num = ifelse(
      x == "short_nat",
      pos_nat[label],
      pos_res[label]
    ),
    stratum_num = factor(stratum_num,
                         levels = seq_len(max(length(nat_order), length(res_order))))
  )

tot_nat <- dm %>%
  count(short_nat) 

tot_res <- dm %>%
  count(short_res) 

tot_nat_vec <- setNames(tot_nat$n, tot_nat$short_nat)
tot_res_vec <- setNames(tot_res$n, tot_res$short_res)

ggplot(ld%>% mutate(title="Numerosity"),
       aes(x = x,
           stratum = stratum_num,
           alluvium = alluvium,
           y = n)) +
  geom_alluvium(aes(fill = origin), alpha = 0.8) +
  geom_stratum(fill = "grey90", color = "grey40") +
  geom_text(
    stat = "stratum",
    aes(label = after_stat({
      lab <- ifelse(x == 1,
                    nat_order[as.numeric(stratum)],
                    res_order[as.numeric(stratum)])
      val <- ifelse(x == 1,
                    tot_nat_vec[lab],
                    tot_res_vec[lab])
      paste0(lab, " (", round(val,1), ")")
    })),
    size = 2.8
  ) +
  facet_wrap(title~.) +
  scale_x_discrete(
    labels = c("Nationality", "Residence"),
    expand = c(0.02, 0.02)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  ylab("Number of Top500 Billionaires") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    legend.position = "none",
    panel.grid = element_blank(),
    strip.text=element_text(size=13),
    axis.title.y = element_text(size=14),
    axis.text.x = element_text(size=13),
    axis.text.y = element_text(size=10)
  ) -> g1


ggplot(ld%>% mutate(title="Numerosity"),
       aes(x = x,
           stratum = stratum_num,
           alluvium = alluvium,
           y = n)) +
  geom_alluvium(aes(fill = origin), alpha = 0.8) +
  geom_stratum(fill = "grey90", color = "grey40") +
  geom_text(
    stat = "stratum",
    aes(label = after_stat({
      lab <- ifelse(x == 1,
                    nat_order[as.numeric(stratum)],
                    res_order[as.numeric(stratum)])
      val <- ifelse(x == 1,
                    tot_nat_vec[lab],
                    tot_res_vec[lab])
      paste0(lab, " (", round(val,1), ")")
    })),
    size = 3.8
  ) +
  facet_wrap(title~.) +
  scale_x_discrete(
    labels = c("Nationality", "Residence"),
    expand = c(0.02, 0.02)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  ylab("Number of Top500 Billionaires") +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    legend.position = "none",
    panel.grid = element_blank(),
    strip.text=element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size=13),
    axis.text.y = element_text(size=11)
  ) -> g1_aut

ggsave("charts/residence_nationality.pdf", g1_aut, width=11, height = 7)

#### By wealth #####

nat_order_w <- dm %>%
  group_by(short_nat) %>%
  summarise(total = sum(wealth, na.rm = TRUE)) %>%
  arrange(desc(total)) %>%
  pull(short_nat)

res_order_w <- dm %>%
  group_by(short_res) %>%
  summarise(total = sum(wealth, na.rm = TRUE)) %>%
  arrange(desc(total)) %>%
  pull(short_res)

df_w <- dm %>%
  group_by(short_nat, short_res) %>%
  summarise(
    n = sum(wealth, na.rm = TRUE),
    .groups = "drop"
  )

ld_w <- to_lodes_form(
  df_w,
  key  = "x",
  axes = c("short_nat", "short_res"),
  id   = "alluvium"
)

ld_w <- ld_w %>%
  group_by(alluvium) %>%
  mutate(
    origin = stratum[x == "short_nat"][1]
  ) %>%
  ungroup()

pos_nat_w <- setNames(seq_along(nat_order_w), nat_order_w)
pos_res_w <- setNames(seq_along(res_order_w), res_order_w)

ld_w <- ld_w %>%
  mutate(
    label = as.character(stratum),
    stratum_num = ifelse(
      x == "short_nat",
      pos_nat_w[label],
      pos_res_w[label]
    ),
    stratum_num = factor(stratum_num,
                         levels = seq_len(max(length(nat_order_w), length(res_order_w))))
  )

tot_nat_w <- dm %>%
  group_by(short_nat) %>%
  summarise(total = sum(wealth, na.rm = TRUE), .groups = "drop")

tot_res_w <- dm %>%
  group_by(short_res) %>%
  summarise(total = sum(wealth, na.rm = TRUE), .groups = "drop")

tot_nat_vec_w <- setNames(tot_nat_w$total, tot_nat_w$short_nat)
tot_res_vec_w <- setNames(tot_res_w$total, tot_res_w$short_res)

ggplot(ld_w %>% 
         mutate(title="Wealth"),
       aes(x = x,
           stratum = stratum_num,
           alluvium = alluvium,
           y = n)) +
  geom_alluvium(aes(fill = origin), alpha = 0.8) +
  geom_stratum(fill = "grey90", color = "grey40") +
  geom_text(
    stat = "stratum",
    aes(label = after_stat({
      lab <- ifelse(x == 1,
                    nat_order_w[as.numeric(stratum)],
                    res_order_w[as.numeric(stratum)])
      val <- ifelse(x == 1,
                    tot_nat_vec_w[lab],
                    tot_res_vec_w[lab])
      paste0(lab, " (", round(val,1), " Bln$)")
    })),
    size = 2.8
  ) +
  facet_wrap(title~.) +
  scale_x_discrete(
    labels = c("Nationality", "Residence"),
    expand = c(0.02, 0.02)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  ylab("Wealth (Bln$)") +
  theme_minimal() +
  theme(
    axis.title.x=element_blank(),
    legend.position = "none",
    panel.grid = element_blank(),
    strip.text=element_text(size=13),
    axis.title.y = element_text(size=14),
    axis.text.x = element_text(size=13),
    axis.text.y = element_text(size=10)
  )  -> g2

#### By emissions #####

dm_e <- dm %>% filter(name!="Lakshmi Mittal")

nat_order_e <- dm_e %>%
  group_by(short_nat) %>%
  summarise(total = sum(tot_emission_baseline, na.rm = TRUE)) %>%
  arrange(desc(total)) %>%
  pull(short_nat)

res_order_e <- dm_e %>%
  group_by(short_res) %>%
  summarise(total = sum(tot_emission_baseline, na.rm = TRUE)) %>%
  arrange(desc(total)) %>%
  pull(short_res)

df_e <- dm_e %>%
  group_by(short_nat, short_res) %>%
  summarise(
    n = sum(tot_emission_baseline, na.rm = TRUE),
    .groups = "drop"
  )

ld_e <- to_lodes_form(
  df_e,
  key  = "x",
  axes = c("short_nat", "short_res"),
  id   = "alluvium"
)

ld_e <- ld_e %>%
  group_by(alluvium) %>%
  mutate(
    origin = stratum[x == "short_nat"][1]
  ) %>%
  ungroup()

pos_nat_e <- setNames(seq_along(nat_order_e), nat_order_e)
pos_res_e <- setNames(seq_along(res_order_e), res_order_e)

ld_e <- ld_e %>%
  mutate(
    label = as.character(stratum),
    stratum_num = ifelse(
      x == "short_nat",
      pos_nat_e[label],
      pos_res_e[label]
    ),
    stratum_num = factor(stratum_num,
                         levels = seq_len(max(length(nat_order_e), length(res_order_e))))
  )

tot_nat_e <- dm_e %>%
  group_by(short_nat) %>%
  summarise(total = sum(tot_emission_baseline, na.rm = TRUE)/1000, .groups = "drop")

tot_res_e <- dm_e %>%
  group_by(short_res) %>%
  summarise(total = sum(tot_emission_baseline, na.rm = TRUE)/1000, .groups = "drop")

tot_nat_vec_e <- setNames(tot_nat_e$total, tot_nat_e$short_nat)
tot_res_vec_e <- setNames(tot_res_e$total, tot_res_e$short_res)

ggplot(ld_e %>% mutate(title="Emissions"),
       aes(x = x,
           stratum = stratum_num,
           alluvium = alluvium,
           y = n/1000)) +
  
  geom_alluvium(aes(fill = origin), alpha = 0.8) +
  geom_stratum(fill = "grey90", color = "grey40") +
  geom_text(
    stat = "stratum",
    aes(label = after_stat({
      lab <- ifelse(x == 1,
                    nat_order_e[as.numeric(stratum)],
                    res_order_e[as.numeric(stratum)])
      val <- ifelse(x == 1,
                    tot_nat_vec_e[lab],
                    tot_res_vec_e[lab])
      label_full <- paste0(lab, " (", round(val,1), " MtCO2e)")
      ifelse(
        x == 1 & as.numeric(stratum) == length(nat_order_e),
        "",
        label_full
      )
    })),
    size = 2.8
  ) +
  facet_wrap(title~.) +
  scale_x_discrete(
    labels = c("Nationality", "Residence"),
    expand = c(0.02, 0.02)
  ) +
  scale_y_continuous(expand = c(0, 0)) +
  ylab("Emissions (MtCO2e)") +
  theme_minimal() +
  theme(
    axis.title.x=element_blank(),
    legend.position = "none",
    panel.grid = element_blank(),
    strip.text=element_text(size=13),
    axis.title.y = element_text(size=14),
    axis.text.x = element_text(size=13),
    axis.text.y = element_text(size=10)
  )  -> g3

dm %>% filter(name=="Lakshmi Mittal") %>% pull(tot_emission_baseline)
#in the group other to GRB we have omitted a huge outlier, Mittal, which has emissions equal to 48218.69

g_final <- cowplot::plot_grid(g1,g2,g3, nrow=1)

ggsave("charts/residence_nationality_full.pdf", g_final, width=15, height = 7)

#chi è last one in emissions? 

#GRB 0.6 MtCO2e
#mancano istraele a sx e canada a dx
