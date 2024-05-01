# poster: Women and World Peace

# packages
library(tidyverse)
library(knitr)
library(readxl)
library(haven)
library(janitor)
library(modelsummary)
library(lfe)
library(gt)
library(corrr)
library(ggrepel)
library(kableExtra)
library(patchwork)

# data
  # https://www.gu.se/en/quality-government/qog-data/data-downloads/basic-dataset
  # https://www.qogdata.pol.gu.se/data/codebook_bas_jan24.pdf
df = read_xlsx('qog_bas_cs_jan24.xlsx')

# histograms
p1 = df %>%
  ggplot(aes(vdem_gender)) +
  geom_histogram(bins = 15, color = 'white', fill = '#af4b91') +
  labs(x = 'Women\'s Political Empowerment \n(0 = low, 1 = high)') +
  theme_minimal()

p2 = df %>%
  ggplot(aes(gpi_gpi)) +
  geom_histogram(bins = 15, color = 'white', fill = '#d7642c') +
  labs(x = 'Level of Peacefulness \n(1 = high, 5 = low)', y = '') +
  theme_minimal()

layout = 'AB'

p1 + p2 + guide_area() +
  plot_layout(design = layout)

# rename variables for tables
renamed_df = df %>%
  mutate(`Global Peace` = gpi_gpi,
         `Women's Political Empowerment` = vdem_gender,
         `Gender Inequality` = gii_gii,
         `Economy Status` = bti_mes,
         Population = pwt_pop,
         `Adult Literacy` = wdi_litrad)

# descriptive statistics
summary_gpi = summarize(renamed_df,
                        variable = "Global Peace",
                        min = round(min(gpi_gpi, na.rm = TRUE), 1),
                        max = round(max(gpi_gpi, na.rm = TRUE), 1),
                        mean = round(mean(gpi_gpi, na.rm = TRUE), 1),
                        median = round(median(gpi_gpi, na.rm = TRUE), 1))

summary_vdem = summarize(renamed_df,
                         variable = "Women's Political Empowerment",
                         min = round(min(vdem_gender, na.rm = TRUE), 1),
                         max = round(max(vdem_gender, na.rm = TRUE), 1),
                         mean = round(mean(vdem_gender, na.rm = TRUE), 1),
                         median = round(median(vdem_gender, na.rm = TRUE), 1))

summary_gii_gii = summarize(renamed_df,
                            variable = "Gender Inequality",
                            min = round(min(gii_gii, na.rm = TRUE), 1),
                            max = round(max(gii_gii, na.rm = TRUE), 1),
                            mean = round(mean(gii_gii, na.rm = TRUE), 1),
                            median = round(median(gii_gii, na.rm = TRUE), 1))

summary_bti_mes = summarize(renamed_df,
                            variable = "Economy Status",
                            min = round(min(bti_mes, na.rm = TRUE), 1),
                            max = round(max(bti_mes, na.rm = TRUE), 1),
                            mean = round(mean(bti_mes, na.rm = TRUE), 1),
                            median = round(median(bti_mes, na.rm = TRUE), 1))

summary_pwt_pop = summarize(renamed_df,
                            variable = "Population",
                            min = round(min(pwt_pop, na.rm = TRUE), 1),
                            max = round(max(pwt_pop, na.rm = TRUE), 1),
                            mean = round(mean(pwt_pop, na.rm = TRUE), 1),
                            median = round(median(pwt_pop, na.rm = TRUE), 1))


summary_wdi_litrad = summarize(renamed_df,
                               variable = "Adult Literacy",
                               min = round(min(wdi_litrad, na.rm = TRUE), 1),
                               max = round(max(wdi_litrad, na.rm = TRUE), 1),
                               mean = round(mean(wdi_litrad, na.rm = TRUE), 1),
                               median = round(median(wdi_litrad, na.rm = TRUE), 1))

summary_combined = bind_rows(`Global Peace` = summary_gpi, 
                             `Women's Political Empowerment` = summary_vdem,
                             `Gender Inequality` = summary_gii_gii,
                             `Economy Status` = summary_bti_mes,
                             Population = summary_pwt_pop,
                             `Adult Literacy` = summary_wdi_litrad)

kable(summary_combined) %>%
  kable_styling(font_size = 100)

# linear regression model
mods = list(
  '(1)' = lm(`Global Peace` ~ `Women's Political Empowerment`, renamed_df),
  '(2)' = lm(`Global Peace` ~ `Women's Political Empowerment` + `Gender Inequality`, renamed_df),
  '(3)' = lm(`Global Peace` ~ `Women's Political Empowerment` + `Economy Status` + Population + `Gender Inequality` + `Adult Literacy`, renamed_df)
)

model = modelsummary(mods, fmt = 2, gof_map = 'nobs', output = 'kableExtra', statistic = NULL,
                     stars = c('*' = 0.05))

model %>%
  kable_styling(font_size = 100)

# scatter plot and lm
df = df %>%
  mutate(gii_bi = if_else(gii_gii > 0.5, 1, 0))

df$gii_bi <- as.factor(df$gii_bi)

df %>%
  ggplot(aes(vdem_gender, gpi_gpi)) +
  geom_point(color = '#d7642c', size = 4, alpha = 0.5) +
  geom_smooth(method = lm, se=F, lwd=2, color='#af4b91') +
  labs(y = 'Level of Peacefulness', x = 'Women\'s Political Empowerment') +
  theme(axis.text.x = element_text(size = 100),
        axis.title.x = element_text(size = 100),
        axis.text.y = element_text(size = 100),
        axis.title.y = element_text(size = 100)) +
  theme_minimal()