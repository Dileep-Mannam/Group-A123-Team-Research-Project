# Libraries
library(tidyverse)
library(ggplot2)

#  Load Dataset
df <- read_csv("C:/Users/admin/Downloads/intel.csv", show_col_types = FALSE)

#  Extract Year from different Release Date formats
parse_year <- function(x) {
  s <- as.character(x)
  
  # Try 4-digit year
  four <- stringr::str_extract(s, "\\d{4}")
  if (!is.na(four)) return(as.integer(four))
  
  # Try 2-digit year with apostrophe (e.g., 18')
  two <- stringr::str_extract(s, "(\\d{2})'")
  if (!is.na(two)) {
    y <- as.integer(gsub("'", "", two))
    return(ifelse(y <= 25, 2000 + y, 1900 + y))
  }
  
  # Try last 2 digits
  twob <- stringr::str_extract(s, "\\d{2}$")
  if (!is.na(twob)) {
    y <- as.integer(twob)
    return(ifelse(y <= 25, 2000 + y, 1900 + y))
  }
  
  return(NA_integer_)
}

df <- df %>%
  mutate(Year = map_int(`Release Date`, parse_year)) %>%
  filter(Year >= 2014, Year <= 2019)

#  Convert Cores to numeric and create CoreGroup
df <- df %>%
  mutate(Cores_num = as.numeric(Cores),
         CoreGroup = case_when(
           Cores_num <= 2 ~ "2",
           Cores_num <= 4 ~ "3-4",
           Cores_num <= 8 ~ "5-8",
           Cores_num <= 16 ~ "9-16",
           TRUE ~ "17+"
         ))

df$CoreGroup <- factor(df$CoreGroup, levels = c("2", "3-4", "5-8", "9-16", "17+"))

# Remove missing CoreGroup if any
df <- df[!is.na(df$CoreGroup), ]

#  Build Contingency Table (Required)
tab <- table(df$CoreGroup, df$Status)

print("Contingency Table:")
print(tab)

# Save table for Appendix
write.csv(as.data.frame(tab), "contingency_table.csv", row.names = FALSE)

#  Visualisation 
#  Proportion (stacked) bar plot - MAIN PLOT
p <- ggplot(df, aes(x = CoreGroup, fill = Status)) +
  geom_bar(position = "fill") +
  labs(
    title = "Proportions of CPU Status by Core Group (Intel 2014–2019)",
    x = "Core Group (Cores)",
    y = "Proportion"
  ) +
  theme_minimal()

ggsave("Proportion_Status_by_CoreGroup.png", p, width = 8, height = 5, dpi = 300)
print(p)

#  Count bar plot - SUPPLEMENTARY PLOT
p2 <- ggplot(df, aes(x = CoreGroup, fill = Status)) +
  geom_bar(position = "stack") +
  labs(
    title = "Counts of CPU Status by Core Group (Intel 2014–2019)",
    x = "Core Group (Cores)",
    y = "Count"
  ) +
  theme_minimal()

ggsave("Count_Status_by_CoreGroup.png", p2, width = 8, height = 5, dpi = 300)
print(p2)

#  Hypothesis Testing: Chi-Square Test of Independence
chi <- chisq.test(tab)

print("Chi-Square Test Result:")
print(chi)

# If expected cell counts < 5, run Monte Carlo simulation
if (any(chi$expected < 5)) {
  chi_sim <- chisq.test(tab, simulate.p.value = TRUE, B = 20000)
  print("Monte Carlo Chi-Square Test Result:")
  print(chi_sim)
}
