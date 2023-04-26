# read data -----
library(tidyverse)
library(readr)
library(skimr)
library(stringr)
library(dbplyr)

data_jobs <- read_csv("Raw/Levels_Fyi_Salary_Data.csv")

# Lowercasing everything ----

?toupper

data_jobs <- data_jobs %>%
  mutate_if(is.character, str_to_lower)

# Cleaning company ----

# taking initial/end spaces out ----
data_jobs <- data_jobs %>%
  mutate_all(str_trim)

company_count <- data_jobs %>%
  group_by(company) %>%
  arrange(company) %>%
  count(sort = TRUE)


# cleaning titles in company ----

# thinking about doing a comparision of big companies as opposed to smaller companies

## amazon ----

data_jobs$company[data_jobs$company == "amazon web services"]<- "amazon"

data_jobs$company[data_jobs$company == "amazon.com"]<- "amazon"

data_jobs$company[data_jobs$company == "amzon"]<- "amazon"

company_count <- data_jobs %>%
  group_by(company) %>%
  arrange(company) %>%
  count(sort = TRUE)

## apple ----

data_jobs$company[data_jobs$company == "apple inc."] <- "apple"

# cant tell if some companies are the same or different. example- aruba and aruba networks. 
# example 2- bain and bain & company. bain could mean bain & company or bain capital (two seperate companies)
# example 3- bank of america merrll lynch is a management subdivision of bank of america. I wonder if theres a difference in jobs between the two or if i should just set them as the same 
# automatic vs automattic 

## deloitte ----

data_jobs$company[data_jobs$company == "deloitte advisory"] <- "deloitte"

data_jobs$company[data_jobs$company == "deloitte consulting"] <- "deloitte"

data_jobs$company[data_jobs$company == "deloitte consulting llp"] <- "deloitte"

company_count <- data_jobs %>%
  group_by(company) %>%
  arrange(company) %>%
  count(sort = TRUE)

## akamai ----

data_jobs$company[data_jobs$company == "akamai technologies"] <- "akamai"

## arista ----

data_jobs$company[data_jobs$company == "arista networks"] <- "arista"

## bloomberg ----

data_jobs$company[data_jobs$company == "bloomberg lp"] <- "bloomberg"

## bolt ----

data_jobs$company[data_jobs$company == "bolt (eu)"] <- "bolt"

## booking ----

data_jobs$company[data_jobs$company == "booking.com"] <- "booking"

## bosch ----

data_jobs$company[data_jobs$company == "bosch global"] <- "bosch"

## cadence ----

data_jobs$company[data_jobs$company == "cadence design systems"] <- "candence"

## cgi ----

data_jobs$company[data_jobs$company == "cgi group"] <- "cgi"

## cisco ----

#cisco now owns meraki and i dont know if i should put them as seperate companies or the same

data_jobs$company[data_jobs$company == "cisco systems"] <- "cisco"

## citibank ----

data_jobs$company[data_jobs$company == "citi"] <- "citibank"

## cognizant ----

data_jobs$company[data_jobs$company == "cognizant technology solutions"] <- "cognizant"

company_count <- data_jobs %>%
  group_by(company) %>%
  arrange(company) %>%
  count(sort = TRUE)

## costco ----

data_jobs$company[data_jobs$company == "costco wholesale"] <- "costco"

## coupa ----

data_jobs$company[data_jobs$company == "coupa software"] <- "coupa"

## cox enterprises ----

# consists of cox automotive and etc 

data_jobs$company[data_jobs$company == "cox automotive"] <- "cox enterprises"

## dell ----

data_jobs$company[data_jobs$company == "dell"] <- "dell technologies"

## discover ----

data_jobs$company[data_jobs$company == "discover financial services"] <- "discover"

## dish ----

data_jobs$company[data_jobs$company == "dish network"] <- "dish"

## disney ----

data_jobs$company[data_jobs$company == "disney streaming services"] <- "disney"

## epam systems ----

data_jobs$company[data_jobs$company == "epam"] <- "epam systems"

## epic ----

# epic systems and epic games were two different companies. one did video games and one did healthcare software. but they combined into one as of april 2022

## ernst and young ----

data_jobs$company[data_jobs$company == "ernst & young"] <- "ernst and young"

## expedia ----

data_jobs$company[data_jobs$company == "expedia group"] <- "expedia"

## ey ----

data_jobs$company[data_jobs$company == "ey-parthenon"] <- "ey"

## ford motor ----

data_jobs$company[data_jobs$company == "ford"] <- "ford motor"

data_jobs$company[data_jobs$company == "ford motor company"] <- "ford motor"

## general electric ----

data_jobs$company[data_jobs$company == "ge"] <- "general electric"

data_jobs$company[data_jobs$company == "ge aviation"] <- "general electric"

data_jobs$company[data_jobs$company == "ge digital"] <- "general electric"

data_jobs$company[data_jobs$company == "ge healthcare"] <- "general electric"

## general dynamic -----

data_jobs$company[data_jobs$company == "general dynamics information technology"] <- "general dynamic"

data_jobs$company[data_jobs$company == "general dynamics mission systems"] <- "general dynamic"

## gojek ----

data_jobs$company[data_jobs$company == "gojek tech"] <- "gojek"

## google ----

data_jobs$company[data_jobs$company == "google llc"] <- "google"

## guidewire ----

data_jobs$company[data_jobs$company == "guidewire software"] <- "guidewire"

## here technologies ----

data_jobs$company[data_jobs$company == "here"] <- "here technologies"

## hewlett packard enterprise ----

data_jobs$company[data_jobs$company == "hpe"] <- "hewlett packard enterprise"

## hp ----

data_jobs$company[data_jobs$company == "hp inc"] <- "hp"

## infosys ----

data_jobs$company[data_jobs$company == "infosys ltd"] <- "infosys"

## intel ----

data_jobs$company[data_jobs$company == "intel corporation"] <- "intel"

## intuitive ----

data_jobs$company[data_jobs$company == "intuitive surgical"] <- "intuitive"

## johnson & johnson ----

data_jobs$company[data_jobs$company == "johnson and johnson"] <- "johnson & johnson"

## cant tell if johnson is part of johnson and johnson or johnson controls or its own seprate thing

## jp morgan ----

data_jobs$company[data_jobs$company == "jp morgan chase"] <- "jp morgan"

data_jobs$company[data_jobs$company == "jpmorgan"] <- "jp morgan"

data_jobs$company[data_jobs$company == "jpmorgan chase"] <- "jp morgan"

data_jobs$company[data_jobs$company == "chase"] <- "jp morgan"

# juniper ----

# cant tell what juniper is part of. can be part of juniper square or juniper networks but online doesnt provide a definitive answer

## l3harris technologies ----

data_jobs$company[data_jobs$company == "l3harris"] <- "l3harris technologies"

## liberty mutual ----

data_jobs$company[data_jobs$company == "liberty mutual insurance"] <- "liberty mutual"

## lowe's ----

data_jobs$company[data_jobs$company == "lowes"] <- "lowe's"

## macy's ----

data_jobs$company[data_jobs$company == "macy's,"] <- "macy's"

## mckinsey ----

data_jobs$company[data_jobs$company == "mckinsey & company"] <- "mckinsey"

## mi ----

# watch for xiaomi company because thats what it is known as 

## microchip ----

data_jobs$company[data_jobs$company == "microchip technology"] <- "microchip"

## micron technology ----

data_jobs$company[data_jobs$company == "micron"] <- "micron technology"

## microsoft ----

data_jobs$company[data_jobs$company == "microsoft corporation"] <- "microsoft"

## moody's ----

data_jobs$company[data_jobs$company == "moody's analytics"] <- "moody's"

## motorola ----

data_jobs$company[data_jobs$company == "motorola solutions"] <- "motorola"

#n/a ----

# theres a n/a in whcih they didnt respond which company they work at 

## nuance ----

data_jobs$company[data_jobs$company == "nuance communications"] <- "nuance"

## nxp semiconductors ----

data_jobs$company[data_jobs$company == "nxp"] <- "nxp semiconductors"

## panasonic ----

data_jobs$company[data_jobs$company == "panasonic avionics"] <- "panasonic"

## procore technologies ----

data_jobs$company[data_jobs$company == "procore"] <- "procore technologies"

## qualcomm ---

data_jobs$company[data_jobs$company == "qualcomm inc"] <- "qualcomm"

## raytheon ----

data_jobs$company[data_jobs$company == "raytheon technologies"] <- "raytheon"

## samsung ----

data_jobs$company[data_jobs$company == "samsung electronics america"] <- "samsung"

data_jobs$company[data_jobs$company == "samsung research america"] <- "samsung"

## sap ----

data_jobs$company[data_jobs$company == "sap concur"] <- "sap"

## sas ----

data_jobs$company[data_jobs$company == "sas institute"] <- "sas"

data_jobs$company[data_jobs$company == "sas software"] <- "sas"

## snap ----

data_jobs$company[data_jobs$company == "snapchat"] <- "snap"

## sony ----

data_jobs$company[data_jobs$company == "sony interactive entertainment"] <- "snony"

## pwc ----

data_jobs$company[data_jobs$company == "strategy by pwc"] <- "pwc"

## tata consultancy services ----
  
data_jobs$company[data_jobs$company == "tcs"] <- "tata consultancy services"

## toyota ----

data_jobs$company[data_jobs$company == "toyota research institute"] <- "toyota"

data_jobs$company[data_jobs$company == "toyota usa"] <- "toyota"

## verizon ----

data_jobs$company[data_jobs$company == "verizon media"] <- "verizon"

## visa ----

data_jobs$company[data_jobs$company == "visa inc"] <- "visa"

## walmart ----

data_jobs$company[data_jobs$company == "walmart labs"] <- "walmart"

## wipro ----

data_jobs$company[data_jobs$company == "wipro limited"] <- "wipro"

## zillow ----

data_jobs$company[data_jobs$company == "zillow group"] <- "zillow"

# zoom ----

#zoom and zoominfo arent affiliated 

## zs associates ----

data_jobs$company[data_jobs$company == "zs"] <- "zs associates"

company_count <- data_jobs %>%
  group_by(company) %>%
  arrange(company) %>%
  count(sort = TRUE)


# write/save data

write.csv(data_jobs, "Processed/data_jobs_cleaned.csv")
