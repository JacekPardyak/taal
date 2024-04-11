library(tidyverse)
df <- readxl::read_excel("./geluk/DataForFigure2.1+with+sub+bars+2024.xls")

countries <- "geo, Country name, Land
NL, Netherlands, Nederland
KG, Kyrgyzstan, Kirgizië
BR, Brazil, Brazilië
PL, Poland, Polen
RO, Romania, Roemenië
VN, Vietnam, Vietnam
CN, China, China
AR, Argentina, Argentinië
" %>% I() %>%
  read_csv()



countries %>% left_join(df) %>% 
  select(c(Land,  `Ladder score`)) %>% 
  rename(Resultaat = "Ladder score") %>%
  arrange(desc(Resultaat))
  
countries %>% left_join(df) %>% 
  select(! c(Land, `Ladder score`, `Country name`, upperwhisker, lowerwhisker, `Dystopia + residual`)) %>%
    rename(BPC = "Explained by: Log GDP per capita") %>%
    rename(SOS = "Explained by: Social support") %>%
    rename(GLV = "Explained by: Healthy life expectancy") %>%
    rename(VLM = "Explained by: Freedom to make life choices") %>%
    rename(VGH = "Explained by: Generosity") %>%
    rename(PVC = "Explained by: Perceptions of corruption") %>%
  column_to_rownames(var = "geo") %>%
  t() -> tt
