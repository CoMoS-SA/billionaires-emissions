library(tidyverse)
library(readxl)

set.seed(2)

billionaires <- tibble(
  name = paste0("B", sprintf("%03d", 1:8)),
  age = sample(45:80, 8),
  residence = c("Bal Harbour, Florida", "Beijing, China", "Marseille, France", "Pune, India", "Modena, Italy", "Sao Paulo, Brazil", "Truckee, California", "Moscow, Russia"),
  marital_status = c("Divorced","Engaged","Married","In Relationship","Separated,Widowed", "Remarried","Widowed","Single"),
  children = sample(0:5,8,replace=TRUE),
  education=c("Drop Out, Harvard University", "Bachelor of Arts/Science, Qingdao University of Science and Technology", "Bachelor of Arts/Science, University of Arkansas",
              "Master of Science, University of Mumbai", "Master of Arts, University of Cambridge", "Diploma, High School", "Drop Out, University of Miami", "Bachelor in Arts/Science, University of Stockholm"),
  
  gender = sample(c("M","F"), 8, replace=TRUE),
  nationality_single_res = c("USA","CHN","FRA","IND","ITA","BRA", "USA","RUS"),
  nationality_ISO3_forbes = nationality_single_res,
  multiple_nationality_ISO3 = c("USA;DEU;NZL", "CHN","FRA","IND","ITA","BRA", "USA","RUS"),
  rank = 1:8,
  networth = c(180,150,120,90,70,55,45,35)*1e9
)


assets <- tribble(
  ~asset_type,  ~asset_type2,                        ~label_bloomberg,             ~company_name,           ~note,     ~total,     ~own_share,     ~tCO2e_asset_baseline,  ~tCO2e_asset_S1_publicCR,  ~tCO2e_asset_S1S2, ~liquidity_weight, ~liquidity_description, ~ICB_Industry_name,    ~ICB_Subsector_name,               ~ICB_Subsector_code,  ~Emissions_source,~Custodial_chains,                                      
  "public",     "public equity",                     "Company_A",                  "Company_A",             "note_1",  9.127e+11,    0.8,          15367810,               15367810,                  15367810*5.1,      1,                 "highly liquid",         "Technology",         "Computer Services",               "10101010",           "Refinitiv",       "No",         
  "public",     "public equity",                     "Company_B",                  "Company_B",             "note_2",  6.384e+10,    0.6,          138879,                 125879,                    138879*4.1,        1,                 "highly liquid",         "Industrials",        "Aerospace & Defense",             "50201010",           "Refinitiv",       "Yes",  
  "public",     "public equity",                     "Company_B",                  "Company_B",             "note_2",  6.384e+10,    0.3,          138879,                 125879,                    138879*4.1,        1,                 "highly liquid",         "Industrials",        "Aerospace & Defense",             "50201010",           "Refinitiv",       "Yes",  
  "public",     "public equity",                     "Company_C",                  "Company_C",             "note_3",  2.457e+10,    1,            235814,                 235814,                    235814*3.7,        1,                 "highly liquid",         "Basic Materials",    "Iron and Steel",                  "55102010",           "Refinitiv",       "No",           
  "public",     "public equity",                     "Company_D",                  "Company_D",             "note_4",  4.529e+10,    0.4,          380000,                 380000,                    380000*5.1,        1,                 "highly liquid",         "Utilities",          "Conventional Electricity",        "65101015",           "Ratio",           "No", 
  "public",     "public equity",                     "Company_D",                  "Company_D",             "note_4",  4.529e+10,    0.5,          380000,                 380000,                    380000*5.1,        1,                 "highly liquid",         "Utilities",          "Conventional Electricity",        "65101015",           "Ratio",           "No",           
  "public",     "public equity",                     "Company_E",                  "Company_E",             "note_5",  2.175e+9,     0.7,          947450,                 947450,                    947450*4.5,        1,                 "highly liquid",         "Telecommunications", "Telecommunications Equipment",    "15101010",           "CR US 2019",      "No",                             
  "private",    "private equity",                    "Company_F",                  "Company_F",             "note_6",  8.104e+8,     1,            840174,                 840174,                    840174*4.1,        0.5,               "semi-liquid",           "Energy",             "Integrated Oil and Gas",          "60101000",           "CR US 2019",      "No",           
  "private",    "private equity",                    "Company_G",                  "Company_G",             "note_7",  6.547e+4,     0.7,          0,                      0,                         0+1000,            0.5,               "semi-liquid",           "Financials",         "Asset Managers and Custodians",   "30202010",           "CR US 2019",      "Yes", 
  "private",    "private equity",                    "Company_G",                  "Company_G",             "note_7",  6.547e+4,     0.2,          0,                      0,                         0+1000,            0.5,               "semi-liquid",           "Financials",         "Asset Managers and Custodians",   "30202010",           "CR US 2019",      "Yes",                       
  "private",    "private equity",                    "Company_H",                  "Company_H",             "note_8",  3.251e+9,     1,            20739,                  30678,                     20739*3.9,         0.5,               "semi-liquid",           "Health Care",        "Pharmaceuticals",                 "20103015",           "CR US 2019",      "No",         
  "private",    "infrastructure/business asset",     "Infrastructure_asset",       "Infrastructure_asset",  "note_9",  8.013e+8,     0.6,          15400,                  18582,                     15400*4.1,         0,                 "mostly illiquid",       NA,                   NA,                                NA,                   "CR France 2017",  "No",     
  "private",    "luxury",                            "Jet_1",                      "Jet_1",                 "note_10", 1.689e+10,     1,           2562,                   2562,                      2562*4.1,          0,                 "mostly illiquid",       NA,                   NA,                                NA,                   "CR France 2017",  "No",     
  "private",    "other/mixed/unknown",               "Other_assets",               "Other_assets",          "note_11", 4.138e+7,     1,            0,                      0,                         0,                 NA,                "not classifiable",      NA,                   NA,                                NA,                   NA,                "No",      
  "private",    "real estate",                       "Villa_1",                    "Villa_1",               "note_12", 3.596e+8,     1,            384,                    384,                       384*3.2,           0,                 "mostly illiquid",       NA,                   NA,                                NA,                   "CR France 2017",  "No",     
  "private",    "luxury",                            "Yacht_1",                     "Yacht_1",                "note_13", 6.251e+9,     1,            1303,                   1303,                      1303*4.6,          0,                 "mostly illiquid",       NA,                   NA,                                NA,                   "Alestig 2024",    "No",   
  "misc",       "debt",                              NA,                           NA,                      "note_14", 9.674e+6,     1,            0,                      0,                         0,                 NA,                "not classifiable",      NA,                   NA,                                NA,                   NA,                "No",     
  "cash",       "liquidity",                         NA,                           NA,                      "note_15", 2.929e+8,     1,            0,                      0,                         0,                 1,                 "highly liquid",         NA,                   NA,                                NA,                   NA,                "No"     
)


assets <- assets %>%
  mutate(
    value = total*own_share,
    tCO2e_billionaire_baseline = tCO2e_asset_baseline*own_share,
    tCO2e_billionaire_S1_publicCR = tCO2e_asset_S1_publicCR*own_share,
    tCO2e_billionaire_S1S2 = tCO2e_asset_S1S2*own_share,
  )



assignment <- c(
  "B001","B001","B002","B003","B004",
  "B005","B006","B007","B008",
  "B002","B003","B004","B005",
  "B006","B007","B008","B001","B002"
)

assets$name <- assignment

portfolio <- left_join(assets,billionaires,by="name")

data <- read_excel("data/data.xlsx", 
                   col_types = c("text", "numeric", "text", 
                                 "text", "numeric", "text", "text", 
                                 "text", "text", "text", "numeric", 
                                 "numeric", "text", "text", "text", 
                                 "text", "text", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "text", "text", 
                                 "text", "text", "text", "text"))

portfolio <- portfolio[, colnames(data)]

write_csv(
  portfolio,
  "data/example_data.csv"
)

