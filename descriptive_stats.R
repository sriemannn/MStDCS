library(dplyr)
library(tidyr)
library(rempsyc)
source("util.R")

d <- read.csv("neuropsychological_data.csv")

print(paste0("Number of rows: ", nrow(d)))
print(unique(d$subject))

names(d) <- gsub("_", ".", names(d))

d$wohnsituation <- recode(
  factor(d$wohnsituation),
  "0" = "Partnerschaft", "1" = "allein", "2" = "Elternhaus", "3" = "WG",
  "4" = "Sonstiges"
)
d$rauchen <- recode(
  factor(d$rauchen),
  "0" = "Nichtraucher", "1" = "Ehemaliger", "2" = "Raucher"
)
d$familienstand <- recode(
  factor(d$familienstand),
  "0" = "ledig", "1" = "verheiratet", "2" = "verwitwet", "3" = "geschieden"
)
d$schulabschluss <- recode(
  factor(d$schulabschluss),
  "0" = "ohne Abschluss", "1" = "Hauptschulabschluss",
  "2" = "mittlere Reife", "3" = "Hochschulreife", "4" = "Universität"
)
d$erwerbstätig <- recode(
  factor(d$erwerbstätig),
  "0" = "nicht erwerbstätig", "1" = "erwerbstätig"
)
d$studium <- recode(factor(d$studium), "0" = "kein Studium", "1" = "Student*in")
d$alkohol <- recode(
  factor(d$alkohol),
  "0" = "kein Alkohol", "<1" = " weniger als 1 Glas",
  "1" = "1 Glas", "1-3" = "1-3 Gläser"
)
d$medikamente <- recode(
  factor(d$medikamente),
  "0" = "keine Medikamente", "1" = "Medikamente"
)

print("Check for missing values")
d %>%
  select_if(is.numeric) %>%
  drop_na(c("alter.bei.testung"))

d <- d %>% mutate(
  vlmt..p = ifelse(vlmt..p < 0, NA, vlmt..p),
)

num_vars <- d %>%
  select_if(is.numeric) %>%
  mutate(
    group = factor(d$group, levels = c("control", "patient")),
    stim.group = factor(d$stim.group, levels = c("anodal", "cathodal")),
  ) %>%
  drop_na(c("alter.bei.testung")) %>% # später entfernen
  group_by(group, stim.group) %>%
  summarise_all(list(M = mean, SD = sd), na.rm = TRUE) %>%
  pivot_longer( # nach unten läger
    cols = !contains("group"), # wählt alle Spalten aus
    names_to = c("Test", "stats"), # teilt die Spaltennamen in Test und Statistik
    names_sep = "_" # trennt die Spaltennamen an den '_'
  ) %>%
  pivot_wider(
    names_from = c("group", "stim.group", "stats"), # macht aus den statistischen Werten Spalten
    values_from = value # macht aus den Werten Werte?
  )

num_vars <- num_vars %>%
  filter(
    !Test %in% c(
      "vlmt..w.f.",
      "ehrenamt",
      "sonstige.gruppen",
      "vlmt.dg1",
      "vlmt.dg5",
      "vlmt..fp",
      "vlmt..p",
      "vlmt.dg6",
      "vlmt..dg5.dg6.",
      "x.tmt.b.zeit.....tmt.a.zeit.",
      "bvmt..recognition.d.prime"
    )
  )
num_vars$Test <- c(
  "EDSS",
  "Age",
  "Education (years)",
  "BVMT total recall",
  "BVMT delayed recall",
  "BVMT recognition hits",
  "BVMT recognition false alarms",
  "BVMT recognition d prime",
  "VLMT learning",
  "VLMT delayed recall",
  "VLMT forgetting",
  "VLMT interference",
  "EHI laterality quotient",
  "TMT-A time",
  "TMT-A errors",
  "TMT-B time",
  "TMT-B errors",
  "FSMC cognition",
  "FSMC motor",
  "FSMC total",
  "SDMT total",
  "HADS-D depression",
  "HADS-D anxiety"
)
names(num_vars) <- gsub("_", ".", names(num_vars))
desc_num_vars <- nice_table(
  num_vars,
  title = c(
    "Table 1",
    "Descriptive statistics for neuropsychological tests."
  ),
  note = paste0(
    "M=mean. SD=standard deviation. ",
    "EDSS=Expanded Disability Status Scale. ",
    "BVMT=Brief Visuospatial Memory Test. ",
    "VLMT=Verbal Learning and Memory Test. ",
    "EHI=Edinburgh Handedness Inventory. ",
    "TMT=Trail Making Test. ",
    "FSMC=Fatigue Scale for Motor and Cognitive Functions. ",
    "SDMT=Symbol Digit Modalities Test. ",
    "HADS-D=Hospital Anxiety and Depression Scale - German version. "
  ),
  separate.header = TRUE,
)
format_and_save(desc_num_vars, "desc_num_vars")

cat_vars <- cbind(
  select_if(d, is.factor),
  select_if(d, is.character)
)

cat_vars_filtered <- cat_vars %>%
  select(
    geschlecht,
    group,
    stim.group
  )

sex <- cat_vars_filtered %>%
  group_by(group, stim.group, geschlecht) %>%
  count() %>%
  drop_na() %>%
  pivot_wider(
    names_from = c("geschlecht"),
    values_from = n
  ) %>%
  mutate(
    sex.ratio = paste0(männlich, "/", weiblich)
  ) %>%
  select(
    -männlich,
    -weiblich
  ) %>%
  pivot_wider(
    names_from = c("group", "stim.group"),
    values_from = "sex.ratio"
  ) %>%
  as.data.frame()
names(sex) <- gsub("_", ".", names(sex))
sex <- cbind(
  data.frame(x = c(
    "Sex (male/female)"
  )) %>% rename(" " = x),
  sex
)
format_and_save(nice_table(
  sex,
  separate.header = TRUE
), "cat_vars")
