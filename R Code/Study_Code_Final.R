library(dplyr)
library(readr)
library(gtsummary)
library(tidyr)
library(ggplot2)
library(purrr)
library(gridExtra)
library(ggstats)
library(sf)
library(urbnmapr)

##### Part 1:Data Preparation #####
# Download from: https://chpl.healthit.gov/
path_prefix <- "/path/to/directory"
df1 <- read.csv(paste0(path_prefix, "clinician-certified-technology-pi-2021-1.txt"), stringsAsFactors = FALSE)
df2 <- read.csv(paste0(path_prefix, "clinician-certified-technology-pi-2021-2.txt"), stringsAsFactors = FALSE)
df3 <- read.csv(paste0(path_prefix, "clinician-certified-technology-pi-2021-3.txt"), stringsAsFactors = FALSE)
df4 <- read.csv(paste0(path_prefix, "clinician-certified-technology-pi-2021-4.txt"), stringsAsFactors = FALSE)

# Combine the individual dataframes into a single dataframe
combined_df <- bind_rows(df1, df2, df3, df4)

# Prepare combined_df

# Group by cehrt_id_clean and developer_name to filter distinct combinations
distinct_combined_df <- combined_df %>%
  group_by(cehrt_id_clean, developer_name) %>%
  summarise()

# Group by cehrt_id_clean again and create a sequence for each distinct developer_name
combined_df_prepared <- distinct_combined_df %>%
  group_by(cehrt_id_clean) %>%
  mutate(developer_seq = paste0("developer_", row_number())) %>%
  select(cehrt_id_clean, developer_seq, developer_name)

# Reshape to wide format
combined_df_wide <- combined_df_prepared %>%
  pivot_wider(names_from = developer_seq, values_from = developer_name)

# Read in QPAR file
# Download from: https://qpp.cms.gov/resources/link/29eaf961-e9ab-44a8-8700-3aaccb9c4e27
qpar2021 <- read.csv(paste0(path_prefix, "2021_puf_QPPAR-18260.csv"), stringsAsFactors = FALSE)

# Replace spaces and dashes in column names with periods
names(qpar2021) <- gsub(" ", ".", names(qpar2021))
names(qpar2021) <- gsub("-", ".", names(qpar2021))

# Perform the LEFT JOIN ON cehrt ID. Secondarily, create a flag for unsuccessful joins
final_data <- qpar2021 %>%
  left_join(combined_df_wide, by = c("pi.cehrt.id" = "cehrt_id_clean")) %>%
  mutate(mismatch_flag = ifelse(!is.na(pi.cehrt.id) & is.na(developer_1), 1, 0))

##### Part 2: Subset to data of interest #####

# Define the list of specialties for clinicians only
selected_specialties <- c(
  "Internal Medicine", "Family Practice", "Diagnostic Radiology",
  "Anesthesiology", "Cardiology", "Orthopedic Surgery", "Obstetrics/Gynecology",
  "Ophthalmology", "General Surgery", "Neurology", "Hospitalist", "Gastroenterology",
  "Psychiatry", "Pathology", "Dermatology", "Pulmonary Disease", "Urology",
  "Otolaryngology", "Nephrology", "Hematology/Oncology", "Physical Medicine and Rehabilitation",
  "Pediatric Medicine", "Infectious Disease", "Endocrinology", "Rheumatology",
  "Radiation Oncology", "Neurosurgery", "Interventional Cardiology", "Critical Care (Intensivists)",
  "Vascular Surgery", "Medical Oncology", "Interventional Radiology", "Plastic and Reconstructive Surgery",
  "Cardiac Electrophysiology", "Pain Management", "Thoracic Surgery", "Allergy/Immunology",
  "General Practice", "Hand Surgery", "Geriatric Medicine", "Interventional Pain Management",
  "Colorectal Surgery (Formerly Proctology)", "Cardiac Surgery", "Gynecological/Oncology",
  "Surgical Oncology", "Hematology", "Advanced heart failure and transplant cardiology",
  "Sleep Medicine", "Nuclear Medicine", "Micrographic Dermatologic Surgery", "Maxillofacial Surgery",
  "Preventive Medicine"
)

# Subset the data based on the selected specialties. N=698,730 -> N=406,980
subset_data <- final_data[final_data$clinician.specialty %in% selected_specialties, ]

# Filter the data based on non.patient.facing being FALSE. N=406,980 -> N=333,130
subset_data <- subset_data[subset_data$non.patient.facing == "False", ]

# Filter the data based on ambulatory.surgical.center being FALSE. N=333,130 -> N=333,056
subset_data <- subset_data[subset_data$ambulatory.surgical.center == "False", ]

# Filter the data based on practice.state.or.us.territory being present. N=333,056 -> N=333,054
subset_data <- subset_data[!is.na(subset_data$practice.state.or.us.territory), ]

# Filter out participation.type = MIS APM. N=333,054 -> N=241,141
subset_data <- subset_data[subset_data$participation.type != 'MIPS APM', ]

# Filter out non-engaged clinicians. N=241,141 -> N=210,728
subset_data <- subset_data[subset_data$engaged == "True", ]

# Filter out data where a certified vendor could not be located. N=210,7281 -> N=209,351
subset_data <- subset_data[subset_data$mismatch_flag == 0, ]


# Define the state_to_subregion list for remaining analyses
state_to_subregion <- list(
  ME='New England', NH='New England', VT='New England', MA='New England',
  RI='New England', CT='New England',
  NY='Middle Atlantic', NJ='Middle Atlantic', PA='Middle Atlantic',
  WI='East North Central', MI='East North Central', IL='East North Central',
  IN='East North Central', OH='East North Central',
  MO='West North Central', ND='West North Central', SD='West North Central',
  NE='West North Central', KS='West North Central',
  MN='West North Central', IA='West North Central',
  DE='South Atlantic', MD='South Atlantic', DC='South Atlantic',
  VA='South Atlantic', WV='South Atlantic', NC='South Atlantic',
  SC='South Atlantic', GA='South Atlantic', FL='South Atlantic',
  KY='East South Central', TN='East South Central', MS='East South Central',
  AL='East South Central',
  OK='West South Central', AR='West South Central', TX='West South Central',
  LA='West South Central',
  ID='Mountain', MT='Mountain', WY='Mountain', NV='Mountain', UT='Mountain',
  CO='Mountain', AZ='Mountain', NM='Mountain',
  AK='Pacific', WA='Pacific', OR='Pacific', CA='Pacific', HI='Pacific',
  GU='U.S. Territories', VI='U.S. Territories', PR='U.S. Territories',
  MP='U.S. Territories'
)

# Define the map_to_subregion function
map_to_subregion <- function(state) {
  if (!is.null(state_to_subregion[[state]])) {
    return(state_to_subregion[[state]])
  } else {
    print(paste("State not found:", state))
    return("Unknown")
  }
}

# Create Census subregion from states/territories
subset_data$us_census_subregion <- sapply(subset_data$practice.state.or.us.territory, map_to_subregion)

##### Part 3: Prepare variables for analysis #####

# Convert columns with "TRUE" and "FALSE" values to 1 and 0
cols_with_TRUE_FALSE <- sapply(subset_data, function(x) all(x %in% c("True", "False", NA)))
subset_data[cols_with_TRUE_FALSE] <- lapply(subset_data[cols_with_TRUE_FALSE], function(x) ifelse(x == "True", 1, ifelse(x == "False", 0, NA)))

# Convert the values in the rural.clinician column
subset_data$rural.clinician <- ifelse(subset_data$rural.clinician == 0, "Urban",
                                      ifelse(subset_data$rural.clinician == 1, "Rural", NA))

# Create a binary for whether a provider has a certified EHR
subset_data$pi_cehrt_binary <-  ifelse(is.na(subset_data$developer_1), 0, 1)

# Columns to check
developer_columns <- c("developer_1", "developer_2", "developer_3",
                       "developer_4", "developer_5", "developer_6",
                       "developer_7", "developer_8", "developer_9")

# Extract unique developers
unique_developers <- subset_data %>%
  select(all_of(developer_columns)) %>%
  unlist() %>%
  unique() %>%
  na.omit()

# Create flags for each unique developer
for (developer in unique_developers) {
  flag_name <- paste0(tolower(gsub("[^[:alnum:]]", "", developer)), "_flag")

  # Initialize the column with NAs
  subset_data[[flag_name]] <- NA_integer_

  # Set to 1 where the developer appears in any of the columns
  subset_data[[flag_name]][apply(subset_data[developer_columns] == developer, 1, any)] <- 1

  # Set to 0 where there are any developers listed, but not the current developer
  subset_data[[flag_name]][apply(!is.na(subset_data[developer_columns]) & subset_data[developer_columns] != developer, 1, any)] <- 0
}

# List of columns to retain in analytic dataset
columns_to_keep <- c(
  "practice.size", "clinician.specialty", "years.in.medicare", "us_census_subregion", "participation.type", "medicare.patients",
  "allowed.charges", "services", "opted.into.mips", "small.practitioner", "rural.clinician", "hpsa.clinician",
  "hospital.based.clinician",  "facility.based", "extreme.hardship",
  "final.score", "payment.adjustment.percentage", "complex.patient.bonus", "extreme.hardship.quality",
  "promoting.interoperability..pi..category.score", "extreme.hardship.pi",  "epicsystemscorporation_flag",
  "athenahealthinc_flag", "cernercorporation_flag", "eclinicalworksllc_flag", "nextgenhealthcare_flag",
  "alteradigitalhealthinc_flag",  "modernizingmedicine_flag", "allscripts_flag",  "virencehealthtechnologies_flag",
  "nextech_flag", "medentcommunitycomputerserviceinc_flag", "modernizingmedicinegastroenterologyinc_flag",
  "greenwayhealthllc_flag", "compulinkhealthcaresolutions_flag",  "aprimaanemdscompany_flag",
  "phoenixorthollc_flag", "advancedmd_flag",  "curemdcominc_flag",  "sticomputerservicesinc_flag",
  "bizmaticsinc_flag",  "ezdermllc_flag", "medpharmservicesllc_flag", "netsmarttechnologies_flag",
  "tenzingmedicalllc_flag", "varianmedicalsystems_flag",  "exscribeinc_flag", "ifauniteditechinc_flag",
  "objectivemedicalsystemsllc_flag",  "triarqpracticeservices_flag",  "wrshealth_flag",
  "mdchartsllc_flag", "harriscaretrackerinc_flag",  "advanceddatasystemscorporation_flag",
  "questdiagnosticsincorporated_flag", "systemedxinc_flag",   "pcesystems_flag",  "fivosinc_flag",
  "firstinsightcorporation_flag", "assurecarellc_flag", "carecloudinc_flag",  "drchronoinc_flag",
  "primeclinicalsystemsinc_flag", "comtroninc_flag",  "henryscheinmedicalsystems_flag",
  "medirecinc_flag",  "nethealth_flag", "integraconnectnewcollc_flag",  "mendelsonkornblumorthopedicspinespecialists_flag",
  "microfourinc_flag",  "geniusdocinc_flag",  "medconnectinc_flag", "azaleahealth_flag",  "emedpracticellc_flag",
  "omnimdinc_flag", "trimedtechnologies_flag", "enablehealthcareinc_flag",  "digidmsinc_flag",
  "mdland_flag",  "modulemd_flag",  "elationhealthinc_flag",  "persivia_flag",  "streamlinemdllc_flag",
  "allegiancemdsoftwareinc_flag", "adaptamedllc_flag",  "acurussolutionsinc_flag",  "experity_flag",
  "practicealternativesinc_flag", "crediblebehavioralhealthinc_flag", "webedoctorinc_flag",
  "eyefinityinc_flag",  "mednetmedicalsolutions_flag",  "medicalinformaticsengineering_flag",
  "benchmarksystems_flag",  "relimedsolutionsllc_flag", "versasuite_flag",  "braintreehealth_flag",
  "zhhealthcareinc_flag", "networkingtechnologyincdbarxnt_flag",  "customcomputingcorporation_flag",
  "radysansinc_flag", "medicalmineinc_flag",  "techsoftinc_flag", "medtrioinc_flag",
  "practicesuiteinc_flag",  "glenwoodsystemsllc_flag",  "healthcare2000inc_flag",
  "healogicsinc_flag",  "provationsoftwareinc_flag",  "pi_cehrt_binary", "practice.state.or.us.territory"
)

# List of columns to retain for QI dataset
qi_columns_to_keep <- c(
  "rural.clinician", "final.score", "quality.measure.score.1", "quality.measure.score.2",
  "quality.measure.score.3", "quality.measure.score.4", "quality.measure.score.5",
  "quality.measure.score.6", "quality.measure.score.7", "quality.measure.score.8",
  "quality.measure.score.9", "quality.measure.score.10",
  "promoting.interoperability..pi..category.score", "pi.bonus",
  "pi.measure.score.1",
  "pi.measure.score.2", "pi.measure.score.3", "pi.measure.score.4",
  "pi.measure.score.5", "pi.measure.score.6", "pi.measure.score.7",
  "pi.measure.score.8", "pi.measure.score.9", "pi.measure.score.10", "pi.measure.score.11",
  "ia.measure.score.1", "ia.measure.score.2", "ia.measure.score.3",
  "ia.measure.score.4"
)

# Subset final_data for qi
subset_data_qi <- subset_data %>%
  select(all_of(qi_columns_to_keep))

# Create Table 1: Overview of all data by rurality
table1_qi <- subset_data_qi %>%
  tbl_summary(by = rural.clinician, missing="no") %>%
  add_overall() %>%
  add_p()

print(table1_qi)

table_1_qi_clean <- as_tibble(table1_qi)
write.csv(table_1_qi_clean, "/path/to/output/directory")

# Subset final_data
subset_data <- subset_data %>%
  select(all_of(columns_to_keep))

##### Part 4: Descriptive statistics #####

# Create Table 1: Overview of all data by rurality
table1 <- subset_data %>%
  tbl_summary(by = rural.clinician, missing="no") %>%
  add_overall() %>%
  add_p()

print(table1)

# Create Bar Plot for Percentage Certified EHR by Specialty
subset_data_percent <- subset_data %>%
  group_by(Specialty = as.factor(clinician.specialty), Rurality = as.factor(rural.clinician), EHR_Status = as.factor(pi_cehrt_binary)) %>%
  tally() %>%
  group_by(Specialty, Rurality) %>%
  mutate(Percentage = n/sum(n) * 100)

specialty_renames <- c(
  "Diagnostic Radiology" = "Diag. Radiology",
  "Orthopedic Surgery" = "Ortho. Surgery",
  "Obstetrics/Gynecology" = "Ob/Gyn",
  "Gastroenterology" = "Gastroent.",
  "Pulmonary Disease" = "Pulm. Disease",
  "Otolaryngology" = "ENT",
  "Hematology/Oncology" = "Hematol./Oncol.",
  "Physical Medicine and Rehabilitation" = "Phys. Med. & Rehab",
  "Pediatric Medicine" = "Pediatrics",
  "Infectious Disease" = "Inf. Disease",
  "Radiation Oncology" = "Radiat. Oncol.",
  "Interventional Cardiology" = "Interv. Cardiol.",
  "Critical Care (Intensivists)" = "Crit. Care",
  "Vascular Surgery" = "Vasc. Surgery",
  "Medical Oncology" = "Med. Oncol.",
  "Interventional Radiology" = "Interv. Radiol.",
  "Plastic and Reconstructive Surgery" = "Plast. & Recon. Surg.",
  "Cardiac Electrophysiology" = "Cardiac Electrophys.",
  "Thoracic Surgery" = "Thorac. Surgery",
  "Allergy/Immunology" = "Allergy/Immunol.",
  "Interventional Pain Management" = "Interv. Pain Mgmt.",
  "Colorectal Surgery (Formerly Proctology)" = "Colorectal Surg.",
  "Gynecological/Oncology" = "Gynecol./Oncol.",
  "Surgical Oncology" = "Surg. Oncol.",
  "Advanced heart failure and transplant cardiology" = "Adv. Heart Failure",
  "Micrographic Dermatologic Surgery" = "Micrograph. Surg.",
  "Maxillofacial Surgery" = "Maxillofac. Surg."
)

subset_data_percent <- subset_data_percent %>%
  mutate(Specialty = recode(Specialty, !!!specialty_renames))

print(subset_data_percent, n = 208)

ggpbr_2 <- ggplot(subset_data_percent, aes(x = Rurality, y = Percentage, fill = EHR_Status)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +  # Note the "stack" position
  geom_text(aes(label = sprintf("%.1f%%", Percentage)),
            position = position_stack(vjust = 0.5),  # Adjust the position to match stacking
            size = 2,
            color = "white") +
  facet_wrap(~ Specialty, scales = "free_y", ncol = 6) +
  labs(title = "Percentage with Certified EHR by Specialty and Rurality",
       y = "Percentage with Certified EHR",
       x = "") +
  scale_fill_brewer(palette = "Set1", name = "Certified EHR") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 10))

plot(ggpbr_2)

# Write to csv
tbl1_tib <- as_tibble(table1)
write.csv(tbl1_tib, file = "/path/to/output/directory", row.names = FALSE)

# Create Table 2, which subsets by clinician specialty

# Define a function to safely remove the clinician.specialty column (if present)
safe_remove_column <- function(data, column_name) {
  if (column_name %in% colnames(data)) {
    data <- data %>% select(-!!sym(column_name))
  }
  return(data)
}

# Define a function to convert each tbl_summary to a tibble and add specialty column
convert_to_tibble_with_specialty <- function(tbl, specialty_name) {
  tbl_tibble <- as_tibble(tbl)
  tbl_tibble <- safe_remove_column(tbl_tibble, "clinician.specialty") # Safely exclude the clinician.specialty column
  tbl_tibble$specialty <- specialty_name
  return(tbl_tibble)
}

# Create a list of tables for each specialty
tables_list <- map(selected_specialties, ~ {
  subset_data %>%
    filter(clinician.specialty == .x) %>%
    tbl_summary(by = rural.clinician, missing="no") %>%
    add_overall()
})

# Convert each table in tables_list to a tibble with specialty column
tables_tibble_list <- map2(tables_list, selected_specialties, convert_to_tibble_with_specialty)

# Bind all tibbles together
combined_table <- bind_rows(tables_tibble_list)

# Convert the tbl_summary object to a tibble
convert_to_tibble_with_specialty <- function(tbl, specialty) {
  tibble_output <- as_tibble(tbl) %>%
    filter(!grepl("clinician.specialty", `**Characteristic**`))

  return(tibble_output)
}

# Convert each tbl_summary to a tibble and store them in a list
tables_tibble_list <- map2(tables_list, selected_specialties, convert_to_tibble_with_specialty)

# Combine the tibbles by joining on the "**Characteristic**" column
tbl2 <- reduce(tables_tibble_list, full_join, by = "**Characteristic**")

write.csv(tbl2, file = "/path/to/output/directory", row.names = FALSE)

# Subset the data to include only rows where there is a developer
subset_data1 <- subset_data %>%
  filter(pi_cehrt_binary == 1)

# Create Table 3 that only has clinicians with an EHR
table3 <- subset_data1 %>%
  tbl_summary(by = rural.clinician) %>%
  add_overall() %>%
  add_p()

tbl3 <- as_tibble(table3)
write.csv(tbl3, file = "/path/to/output/directory", row.names = FALSE)

# Create Table 4 looking at clinicians by certified EHR status
table4 <- subset_data %>%
  tbl_summary(by=pi_cehrt_binary) %>%
  add_overall() %>%
  add_p

tbl4 <- as_tibble(table4)
write.csv(tbl4, file = "/path/to/output/directory", row.names = FALSE)

###### Part 5: Create visualizations ######

# List of developer flags
developer_flags <- c(
  "epicsystemscorporation_flag",
  "athenahealthinc_flag", "cernercorporation_flag", "eclinicalworksllc_flag", "nextgenhealthcare_flag",
  "alteradigitalhealthinc_flag",  "modernizingmedicine_flag", "allscripts_flag",  "virencehealthtechnologies_flag",
  "nextech_flag", "medentcommunitycomputerserviceinc_flag", "modernizingmedicinegastroenterologyinc_flag",
  "greenwayhealthllc_flag", "compulinkhealthcaresolutions_flag",  "aprimaanemdscompany_flag",
  "phoenixorthollc_flag", "advancedmd_flag",  "curemdcominc_flag",  "sticomputerservicesinc_flag",
  "bizmaticsinc_flag",  "ezdermllc_flag", "medpharmservicesllc_flag", "netsmarttechnologies_flag",
  "tenzingmedicalllc_flag", "varianmedicalsystems_flag",  "exscribeinc_flag", "ifauniteditechinc_flag",
  "objectivemedicalsystemsllc_flag",  "triarqpracticeservices_flag",  "wrshealth_flag",
  "mdchartsllc_flag", "harriscaretrackerinc_flag",  "advanceddatasystemscorporation_flag",
  "questdiagnosticsincorporated_flag", "systemedxinc_flag",   "pcesystems_flag",  "fivosinc_flag",
  "firstinsightcorporation_flag", "assurecarellc_flag", "carecloudinc_flag",  "drchronoinc_flag",
  "primeclinicalsystemsinc_flag", "comtroninc_flag",  "henryscheinmedicalsystems_flag",
  "medirecinc_flag",  "nethealth_flag", "integraconnectnewcollc_flag",  "mendelsonkornblumorthopedicspinespecialists_flag",
  "microfourinc_flag",  "geniusdocinc_flag",  "medconnectinc_flag", "azaleahealth_flag",  "emedpracticellc_flag",
  "omnimdinc_flag", "trimedtechnologies_flag", "enablehealthcareinc_flag",  "digidmsinc_flag",
  "mdland_flag",  "modulemd_flag",  "elationhealthinc_flag",  "persivia_flag",  "streamlinemdllc_flag",
  "allegiancemdsoftwareinc_flag", "adaptamedllc_flag",  "acurussolutionsinc_flag",  "experity_flag",
  "practicealternativesinc_flag", "crediblebehavioralhealthinc_flag", "webedoctorinc_flag",
  "eyefinityinc_flag",  "mednetmedicalsolutions_flag",  "medicalinformaticsengineering_flag",
  "benchmarksystems_flag",  "relimedsolutionsllc_flag", "versasuite_flag",  "braintreehealth_flag",
  "zhhealthcareinc_flag", "networkingtechnologyincdbarxnt_flag",  "customcomputingcorporation_flag",
  "radysansinc_flag", "medicalmineinc_flag",  "techsoftinc_flag", "medtrioinc_flag",
  "practicesuiteinc_flag",  "glenwoodsystemsllc_flag",  "healthcare2000inc_flag",
  "healogicsinc_flag",  "provationsoftwareinc_flag"
)

# Melt the dataframe to calculate the frequencies of each developer flag being set to 1
developer_frequencies <- subset_data1 %>%
  select(all_of(developer_flags)) %>%
  gather(key = "developer_flag", value = "flag_value") %>%
  filter(flag_value == 1) %>%
  count(developer_flag, sort = TRUE)

# Plotting the top 20 developers
developer_counts_top_20 <- ggplot(head(developer_frequencies, 20), aes(x = reorder(developer_flag, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 20 Developers by Frequency", x = "Developer", y = "Frequency") +
  theme_minimal()

plot(developer_counts_top_20)

# Compute the developer frequencies
developer_frequencies <- subset_data1 %>%
  select(developer_flags) %>%
  summarise_all(sum) %>%
  gather(key = "developer_flag", value = "n") %>%
  arrange(-n)

# Plotting the top 20 developers on log scale
plot <- ggplot(head(developer_frequencies, 20), aes(x = reorder(developer_flag, n), y = n)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(trans='log2', labels = scales::comma) +
  coord_flip() +
  labs(title = "Top 20 Developers by Frequency", x = "Developer", y = "Frequency (Log Scale)") +
  theme_minimal()

plot(plot)

top_20_developers <- head(developer_frequencies, 20)

# Compute the developer frequencies
developer_frequencies_rural <- subset_data1 %>%
  filter(rural.clinician == 'Rural') %>%
  select(developer_flags) %>%
  summarise_all(sum) %>%
  gather(key = "developer_flag", value = "n") %>%
  arrange(-n)

# Compute the developer frequencies
developer_frequencies_not_rural <- subset_data1 %>%
  filter(rural.clinician == 'Urban') %>%
  select(developer_flags) %>%
  summarise_all(sum) %>%
  gather(key = "developer_flag", value = "n") %>%
  arrange(-n)

# Calculate total number of sites for rural and non-rural
total_sites_rural <- sum(developer_frequencies_rural$n)
total_sites_not_rural <- sum(developer_frequencies_not_rural$n)

# Compute percentage for each developer
developer_frequencies_rural$percentage <- (developer_frequencies_rural$n / total_sites_rural) * 100
developer_frequencies_not_rural$percentage <- (developer_frequencies_not_rural$n / total_sites_not_rural) * 100

# Relabel the developer flags
developer_relabels <- c(
  'epicsystemscorporation_flag' = 'Epic Systems',
  'athenahealthinc_flag' = 'Athena Health',
  'cernercorporation_flag' = 'Cerner Corporation',
  'eclinicalworksllc_flag' = 'eClinicalWorks',
  'nextgenhealthcare_flag' = 'NextGen Healthcare',
  'alteradigitalhealthinc_flag' = 'Altera Digital Health',
  'modernizingmedicine_flag' = 'Modernizing Medicine',
  'allscripts_flag' = 'Veradigm',
  'virencehealthtechnologies_flag' = 'Virence Health',
  'nextech_flag' = 'Nextech',
  'medentcommunitycomputerserviceinc_flag' = 'MEDENT',
  'modernizingmedicinegastroenterologyinc_flag' = 'Modernizing Medicine',
  'greenwayhealthllc_flag' = 'Greenway Health',
  'compulinkhealthcaresolutions_flag' = 'Compulink',
  'tenzingmedicalllc_flag' = 'Tenzing Medical'
)

# Update the developer_flag column in both dataframes
developer_frequencies_rural$developer_flag <- recode(developer_frequencies_rural$developer_flag, !!!developer_relabels)
developer_frequencies_not_rural$developer_flag <- recode(developer_frequencies_not_rural$developer_flag, !!!developer_relabels)


# Replotting the data with bold text and comma-separated count numbers
plot_rural_updated <- ggplot(head(developer_frequencies_rural, 10),
                             aes(x = reorder(developer_flag, n), y = n, fill = developer_flag)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_y_continuous(trans='log2', labels = scales::comma) +
  coord_flip() +
  labs(title = "Top 10 EHR Vendors for Rural Clinicians",
       x = "EHR Vendor",
       y = "Frequency (Log Scale)") +
  theme_minimal() +
  geom_text(aes(label=sprintf("%s (%.1f%%)", scales::comma(n), percentage)), hjust=1.5, size=3, fontface = "bold")

plot_not_rural_updated <- ggplot(head(developer_frequencies_not_rural, 10),
                                 aes(x = reorder(developer_flag, n), y = n, fill = developer_flag)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_y_continuous(trans='log2', labels = scales::comma) +
  coord_flip() +
  labs(title = "Top 10 EHR Vendors for Urban Clinicians",
       x = "EHR Vendor",
       y = "Frequency (Log Scale)") +
  theme_minimal() +
  geom_text(aes(label=sprintf("%s (%.1f%%)", scales::comma(n), percentage)), hjust=1.5, size=3, fontface = "bold")

# Using the Nature color palette from the ggsci package
library(ggsci)
plot_rural_updated <- plot_rural_updated + scale_fill_npg()
plot_not_rural_updated <- plot_not_rural_updated + scale_fill_npg()

# Combine the two plots
grid.arrange(plot_rural_updated, plot_not_rural_updated, ncol = 1)


# Create a plot of vendors by subregion by rurality

# 1.1 Convert wide format to long format
long_data <- subset_data1 %>%
  select(us_census_subregion, rural.clinician, developer_flags) %>%
  gather(key = "developer", value = "flag", developer_flags) %>%
  filter(flag == 1) %>%
  select(-flag)

long_data$developer <- recode(long_data$developer, !!!developer_relabels)


# Count occurrences and filter out 'U.S. Territories'
developer_data <- long_data %>%
  filter(us_census_subregion != "U.S. Territories") %>%
  group_by(us_census_subregion, rural.clinician, developer) %>%
  summarise(count = n())

# Calculate percentages
developer_data <- developer_data %>%
  group_by(us_census_subregion, rural.clinician) %>%
  mutate(total = sum(count), percentage = (count/total) * 100) %>%
  arrange(us_census_subregion, rural.clinician, -percentage) %>%
  slice_head(n = 5)

region_plot <- ggplot(developer_data, aes(x = reorder(developer, percentage), y = percentage, fill = rural.clinician)) +
  geom_col(position = "dodge", aes(fill = rural.clinician)) +
  scale_fill_npg(name = "Rurality") +
  coord_flip() +
  labs(#title = "Top 5 EHR Developers by Subregion and Rurality",
    x = "EHR Vendor",
    y = "Percentage") +
  facet_wrap(~ us_census_subregion, scales = "free_y", ncol = 2) +
  theme_minimal() +
  theme(legend.position="bottom")

print(region_plot)

print(head(developer_data))

write.csv(developer_data, file = "/path/to/output/directory", row.names = FALSE)

# Standardize developer data so that no NULL values are present in the plots

# Create all combinations of subregion, rurality, and developer
complete_set <- expand.grid(us_census_subregion = unique(developer_data$us_census_subregion),
                            rural.clinician = unique(developer_data$rural.clinician),
                            developer = unique(developer_data$developer))

# Merge with the original dataset
full_developer_data <- merge(complete_set, developer_data,
                             by = c("us_census_subregion", "rural.clinician", "developer"),
                             all.x = TRUE)

# Replace NA in count with 0
full_developer_data$count[is.na(full_developer_data$count)] <- 0

# Recalculate percentages
full_developer_data <- full_developer_data %>%
  group_by(us_census_subregion, rural.clinician) %>%
  mutate(total = sum(count),
         percentage = count / total * 100)

# Replace NA in percentage with 0
full_developer_data$percentage[is.na(full_developer_data$percentage)] <- 0

# Region plot
region_plot <- ggplot(full_developer_data, aes(x = reorder(developer, percentage), y = percentage, fill = rural.clinician)) +
  geom_col(position = "dodge", aes(fill = rural.clinician)) +
  scale_fill_npg(name = "Rurality") +
  coord_flip() +
  labs(x = "EHR Vendor", y = "Percentage") +
  facet_wrap(~ us_census_subregion, scales = "free_y", ncol = 2) +
  theme_minimal() +
  theme(legend.position="bottom")

print(region_plot)

# Modified plotting code with percentage labels
region_plot_percentage_label <- ggplot(full_developer_data, aes(x = reorder(developer, percentage), y = percentage, fill = rural.clinician)) +
  geom_col(position = "dodge", aes(fill = rural.clinician)) +
  geom_text(aes(label = sprintf("%.1f%%", percentage),
                y = percentage + 1, # Adjust this value to position the labels appropriately
                group = rural.clinician),
            position = position_dodge(width = 1),
            vjust = -0.25, # Vertical adjustment for label positioning
            hjust = 0.5,   # Horizontal adjustment for label positioning
            size = 3,      # Text size, adjust as needed
            color = "black") +
  scale_fill_npg(name = "Rurality") +
  coord_flip() +
  labs(x = "EHR Vendor", y = "Percentage") +
  facet_wrap(~ us_census_subregion, scales = "free_y", ncol = 2) +
  theme_minimal() +
  theme(legend.position="bottom")

print(region_plot_percentage_label)


# Logistic Regression for Certified EHR

# Set Factor Levels
subset_data$rural.clinician <- factor(subset_data$rural.clinician, levels=c("Urban", "Rural"))
subset_data$rural.clinician=relevel(as.factor(subset_data$rural.clinician),ref="Urban")

subset_data$clinician.specialty=relevel(as.factor(subset_data$clinician.specialty),ref="Family Practice")

subset_data$us_census_subregion=relevel(as.factor(subset_data$us_census_subregion),ref="East North Central")


m12 <- glm(pi_cehrt_binary ~ rural.clinician, family = binomial, data = subset_data)
m1_univariable <- m12 %>%
  tbl_regression(exponentiate = TRUE)   %>%
  add_nevent(location = "level") %>%
  add_n(location = "level") %>%
  # adding event rate
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(
        stat_nevent_rate =
          ifelse(
            !is.na(stat_nevent),
            paste0(style_sigfig(stat_nevent / stat_n, scale = 100), "%"),
            NA
          ),
        .after = stat_nevent
      )
  ) %>%
  # merge the colums into a single column
  modify_cols_merge(
    pattern = "{stat_nevent} / {stat_n} ({stat_nevent_rate})",
    rows = !is.na(stat_nevent)
  ) %>%
  # update header to event rate
  modify_header(stat_nevent = "**Event Rate**")

print(m1_univariable)


m1 <- glm(pi_cehrt_binary ~ rural.clinician + years.in.medicare + clinician.specialty +
             small.practitioner + hpsa.clinician  + hospital.based.clinician +
             facility.based + clinician.specialty + extreme.hardship +
             us_census_subregion, family = binomial, data = subset_data)

m1_multivariable <- m1 %>%
  tbl_regression(exponentiate = TRUE)   %>%
  add_nevent(location = "level") %>%
  add_n(location = "level") %>%
  # adding event rate
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(
        stat_nevent_rate =
          ifelse(
            !is.na(stat_nevent),
            paste0(style_sigfig(stat_nevent / stat_n, scale = 100), "%"),
            NA
          ),
        .after = stat_nevent
      )
  ) %>%
  # merge the colums into a single column
  modify_cols_merge(
    pattern = "{stat_nevent} / {stat_n} ({stat_nevent_rate})",
    rows = !is.na(stat_nevent)
  ) %>%
  # update header to event rate
  modify_header(stat_nevent = "**Event Rate**")

print(m1_multivariable)


subset_r <- subset_data %>% filter(rural.clinician == "Rural")

subset_u <- subset_data %>% filter(rural.clinician == "Urban")

m_all_r <- glm(pi_cehrt_binary ~ facility.based + hospital.based.clinician + years.in.medicare + clinician.specialty + hpsa.clinician + us_census_subregion + small.practitioner + extreme.hardship, family = binomial, data = subset_r) %>%
  tbl_regression(exponentiate = TRUE)

m_all_rural <- glm(pi_cehrt_binary ~ extreme.hardship + small.practitioner + facility.based + hospital.based.clinician + hpsa.clinician + years.in.medicare + clinician.specialty + us_census_subregion, family = binomial, data = subset_r)

subset_r$extreme.hardship <- factor(subset_r$extreme.hardship, levels = c(0, 1), labels = c('No', 'Yes'))
subset_r$small.practitioner <- factor(subset_r$small.practitioner, levels = c(0, 1), labels = c('No', 'Yes'))
subset_r$facility.based <- factor(subset_r$facility.based, levels = c(0, 1), labels = c('No', 'Yes'))
subset_r$hospital.based.clinician <- factor(subset_r$hospital.based.clinician, levels = c(0, 1), labels = c('No', 'Yes'))
subset_r$hpsa.clinician <- factor(subset_r$hpsa.clinician, levels = c(0, 1), labels = c('No', 'Yes'))

m_all_rural_ts <- glm(pi_cehrt_binary ~ as.factor(extreme.hardship) + as.factor(small.practitioner) + as.factor(facility.based) + as.factor(hospital.based.clinician) + as.factor(hpsa.clinician) + years.in.medicare + clinician.specialty + us_census_subregion, family = binomial, data = subset_r)

m_all_rural_tbl <- m_all7_rural_ts %>%
  tbl_regression(exponentiate = TRUE)   %>%
  add_nevent(location = "level") %>%
  add_n(location = "level") %>%
  # adding event rate
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(
        stat_nevent_rate =
          ifelse(
            !is.na(stat_nevent),
            paste0(style_sigfig(stat_nevent / stat_n, scale = 100), "%"),
            NA
          ),
        .after = stat_nevent
      )
  ) %>%
  # merge the colums into a single column
  modify_cols_merge(
    pattern = "{stat_nevent} / {stat_n} ({stat_nevent_rate})",
    rows = !is.na(stat_nevent)
  ) %>%
  # update header to event rate
  modify_header(stat_nevent = "**Event Rate**")

m_all_rural_tbl <- as_tibble(m_all7_rural_tbl)
write.csv(m_all7_rural_tbl, "/path/to/output/directory")

plot_glm_rural <- ggcoef_table(
  m_all_rural,
  exponentiate = TRUE,
  errorbar = TRUE,
  errorbar_height = 0,
  errorbar_coloured = TRUE,
  include = c(
    "extreme.hardship",
    "practice.size",
    "small.practitioner",
    "hpsa.clinician",
    "hospital.based.clinician",
    "facility.based",
    "years.in.medicare",
    "medicare.patients",
    "us_census_subregion"),
  variable_labels = c(
    extreme.hardship = "Extreme Hardship",
    small.practitioner = "Small Practitioner",
    practice.size = "Practice Size",
    hpsa.clinician = "HPSA Clinician",
    facility.based = "Facility Based",
    hospital.based.clinician = "Hospital Based",
    years.in.medicare = "Years in Medicare",
    medicare.patients = "Number of Medicare Patients",
    us_census_subregion = "Census Subregion")
)

plot(plot_glm_rural) + scale_color_npg()

m_all_urban <- glm(pi_cehrt_binary ~ extreme.hardship + small.practitioner + facility.based + hospital.based.clinician + hpsa.clinician + years.in.medicare + clinician.specialty + us_census_subregion , family = binomial, data = subset_u)

subset_u$extreme.hardship <- factor(subset_u$extreme.hardship, levels = c(0, 1), labels = c('No', 'Yes'))
subset_u$small.practitioner <- factor(subset_u$small.practitioner, levels = c(0, 1), labels = c('No', 'Yes'))
subset_u$facility.based <- factor(subset_u$facility.based, levels = c(0, 1), labels = c('No', 'Yes'))
subset_u$hospital.based.clinician <- factor(subset_u$hospital.based.clinician, levels = c(0, 1), labels = c('No', 'Yes'))
subset_u$hpsa.clinician <- factor(subset_u$hpsa.clinician, levels = c(0, 1), labels = c('No', 'Yes'))

m_all_urban_ts <- glm(pi_cehrt_binary ~ extreme.hardship + small.practitioner + facility.based + hospital.based.clinician + hpsa.clinician + years.in.medicare + clinician.specialty + us_census_subregion , family = binomial, data = subset_u)


m_all_urban_tbl <- m_all_urban_ts %>%
  tbl_regression(exponentiate = TRUE)   %>%
  add_nevent(location = "level") %>%
  add_n(location = "level") %>%
  # adding event rate
  modify_table_body(
    ~ .x %>%
      dplyr::mutate(
        stat_nevent_rate =
          ifelse(
            !is.na(stat_nevent),
            paste0(style_sigfig(stat_nevent / stat_n, scale = 100), "%"),
            NA
          ),
        .after = stat_nevent
      )
  ) %>%
  # merge the colums into a single column
  modify_cols_merge(
    pattern = "{stat_nevent} / {stat_n} ({stat_nevent_rate})",
    rows = !is.na(stat_nevent)
  ) %>%
  # update header to event rate
  modify_header(stat_nevent = "**Event Rate**")

m_all_urban_tbl <- as_tibble(m_all_urban_tbl)
write.csv(m_all_urban_tbl, "/path/to/output/directory")


plot_glm_urban <- ggcoef_table(
  m_all_urban,
  exponentiate = TRUE,
  errorbar = TRUE,
  errorbar_height = 0,
  errorbar_coloured = TRUE,
  include = c(
    "extreme.hardship",
    "practice.size",
    "small.practitioner",
    "hpsa.clinician",
    "hospital.based.clinician",
    "facility.based",
    "years.in.medicare",
    "medicare.patients",
    "us_census_subregion"),
  variable_labels = c(
    extreme.hardship = "Extreme Hardship",
    small.practitioner = "Small Practitioner",
    practice.size = "Practice Size",
    hpsa.clinician = "HPSA Clinician",
    facility.based = "Facility Based",
    hospital.based.clinician = "Hospital Based",
    years.in.medicare = "Years in Medicare",
    medicare.patients = "Number of Medicare Patients",
    us_census_subregion = "Census Subregion")
)

models2 <- list(
  "Urban Participants" = m_all_urban,
  "Rural Participants" = m_all_rural
)

glm_compared <- ggcoef_compare(models2,
                               type = "faceted",
                               #errorbar_height = 0,
                               errorbar_coloured = TRUE,
                               add_reference_rows = FALSE,
                               exponentiate = TRUE,
                               categorical_terms_pattern = "{level} (ref: {reference_level})",
                               include = c(
                                 "extreme.hardship",
                                 "small.practitioner",
                                 "facility.based",
                                 "hospital.based.clinician",
                                 "hpsa.clinician",
                                 "years.in.medicare",
                                 "us_census_subregion"),
                               variable_labels = c(
                                 extreme.hardship = "Extreme Hardship",
                                 small.practitioner = "Small Practitioner",
                                 facility.based = "Facility-Based Clinician",
                                 hospital.based.clinician = "Hospital-Based Clinician",
                                 hpsa.clinician = 'HPSA Clinician',
                                 years.in.medicare = "Years in Medicare",
                                 us_census_subregion = "Census Subregion"))

# 1000 x 750 pixels
plot(glm_compared)  + scale_color_npg()

plot(plot_glm_urban)

m_all_r_tibble <- as_tibble(m_all_r)
write.csv(m_all_r_tibble, file = "/path/to/output/directory", row.names = FALSE)

m_all_u <- glm(pi_cehrt_binary ~ extreme.hardship + practice.size + years.in.medicare +
                  medicare.patients + small.practitioner  + hospital.based.clinician +
                  clinician.specialty + hpsa.clinician +  facility.based +
                  us_census_subregion, family = binomial, data = subset_u)%>%
  tbl_regression(exponentiate = TRUE)

m_all_u_tibble <- as_tibble(m_all_u)
write.csv(m_all7_u_tibble, file = "/path/to/output/directory", row.names = FALSE)

# Linear regrssion for PIS
subset_data <- subset_data %>%
  rename(promoting_interoperability = `promoting.interoperability..pi..category.score`)


lm_all <- lm(promoting_interoperability ~ rural.clinician + extreme.hardship + small.practitioner +
               facility.based + hospital.based.clinician + hpsa.clinician + years.in.medicare +
               clinician.specialty + us_census_subregion, data = subset_data)

tbl_lm_all <- lm_all %>%
  tbl_regression()

# Fit the linear regression model
lm1 <- lm(promoting_interoperability ~ rural.clinician + practice.size + years.in.medicare +
            medicare.patients +
            clinician.specialty +  us_census_subregion, data = subset_data)

plot_lm_all <- ggcoef_table(
  lm_all,
  errorbar = TRUE,
  errorbar_coloured = TRUE,
  add_reference_rows = FALSE,
  table_witdhs = c(3, 1),
  categorical_terms_pattern = "{level} (ref: {reference_level})",
  ci_pattern = "{estimate} ({conf.low}-{conf.high})",
  table_stat = c("ci"),
  table_header = "Beta (95% CI)",
  table_stat_label = list(
    estimate = scales::label_number(accuracy = .01),
    conf.low = scales::label_number(accuracy = .01),
    conf.high = scales::label_number(accuracy = .01)
  ),
  include = c(
    "rural.clinician",
    "extreme.hardship",
    "small.practitioner",
    "facility.based",
    "hospital.based.clinician",
    "hpsa.clinician",
    "years.in.medicare",
    "us_census_subregion"),
  variable_labels = c(
    rural.clinician = "Rurality",
    extreme.hardship = "Extreme Hardship",
    small.practitioner = "Small Practitioner",
    facility.based = "Facility-Based Clinician",
    hospital.based.clinician = "Hospital-Based Clinician",
    hpsa.clinician = 'HPSA Clinician',
    years.in.medicare = "Years in Medicare",
    us_census_subregion = "Census Subregion")
)

plot(plot_lm_all)

lm2 <- lm(promoting_interoperability ~ rural.clinician*extreme.hardship + practice.size + years.in.medicare +
            medicare.patients + small.practitioner  + hospital.based.clinician +
            clinician.specialty + hpsa.clinician +  facility.based +
            us_census_subregion, data = subset_data)

lm2_tib <- lm2 %>%  tbl_regression()

lm2_tib <- as_tibble(lm2_tib)

write.csv(lm2_tib, file = "/path/to/output/directory", row.names = FALSE)

print(lm2_tib)

plot_lm2 <- ggcoef_table(
  lm2,
  errorbar = TRUE,
  errorbar_height = 0,
  errorbar_coloured = TRUE,
  include = c(
    "rural.clinician",
    "extreme.hardship",
    "practice.size",
    "years.in.medicare",
    "medicare.patients",
    "small.practitioner",
    "hpsa.clinician",
    "hospital.based.clinician",
    "facility.based",
    "us_census_subregion"),
  variable_labels = c(
    rural.clinician = "Rurality",
    extreme.hardship = "Extreme Hardship",
    small.practitioner = "Small Practitioner",
    practice.size = "Practice Size",
    facility.based = "Facility-Based",
    hpsa.clinician = "HPSA",
    hospital.based.clinician = "Hospital-Based",
    years.in.medicare = "Years in Medicare",
    medicare.patients = "Number of Medicare Patients",
    us_census_subregion = "Census Subregion"))

plot(plot_lm2)

subset_r <- subset_data %>% filter(rural.clinician == "Rural")

subset_u <- subset_data %>% filter(rural.clinician == "Urban")

lm2_urban <- lm(promoting_interoperability ~ extreme.hardship + practice.size + years.in.medicare +
                  medicare.patients + small.practitioner  + hospital.based.clinician +
                  clinician.specialty + hpsa.clinician +  facility.based +
                  us_census_subregion, data = subset_u)

lm2_rural <- lm(promoting_interoperability ~ extreme.hardship + practice.size + years.in.medicare +
                  medicare.patients + small.practitioner  + hospital.based.clinician +
                  clinician.specialty + hpsa.clinician +  facility.based +
                  us_census_subregion, data = subset_r)

lm2_urban_exp <- lm2_urban %>% tbl_regression()
lm2_rural_exp <- lm2_rural %>% tbl_regression()

merged_lm_strat <-
  tbl_merge(
    tbls = list(lm2_urban_exp, lm2_rural_exp),
    tab_spanner = c("Urban", "Rural")
  )

merged_lm_strat <- as_tibble(merged_lm_strat)

write.csv(merged_lm_strat, file = "/path/to/output/directory", row.names = FALSE)

models <- list(
  "Urban Participants" = lm2_urban,
  "Rural Participants" = lm2_rural
)

lm_compared <- ggcoef_compare(models,
                              type = "faceted",
                              include = c(
                                "extreme.hardship",
                                "practice.size",
                                "years.in.medicare",
                                "medicare.patients",
                                "small.practitioner",
                                "hpsa.clinician",
                                "hospital.based.clinician",
                                "facility.based",
                                "us_census_subregion"),
                              variable_labels = c(
                                extreme.hardship = "Extreme Hardship",
                                small.practitioner = "Small Practitioner",
                                practice.size = "Practice Size",
                                facility.based = "Facility-Based",
                                hpsa.clinician = "HPSA",
                                hospital.based.clinician = "Hospital-Based",
                                years.in.medicare = "Years in Medicare",
                                medicare.patients = "Number of Medicare Patients",
                                us_census_subregion = "Census Subregion"))

plot(lm_compared)

# Modifying the plot function from ggstats for ggocoef_table()
# to print CIs with 2 significant figures.
ggcoef_table <- function(
    model,
    tidy_fun = broom.helpers::tidy_with_broom_or_parameters,
    tidy_args = NULL,
    conf.int = TRUE,
    conf.level = .95,
    exponentiate = FALSE,
    variable_labels = NULL,
    term_labels = NULL,
    interaction_sep = " * ",
    categorical_terms_pattern = "{level}",
    add_reference_rows = TRUE,
    no_reference_row = NULL,
    intercept = FALSE,
    include = dplyr::everything(),
    add_pairwise_contrasts = FALSE,
    pairwise_variables = broom.helpers::all_categorical(),
    keep_model_terms = FALSE,
    pairwise_reverse = TRUE,
    emmeans_args = list(),
    significance = 1 - conf.level,
    significance_labels = NULL,
    show_p_values = FALSE,
    signif_stars = FALSE,
    table_stat = c("estimate", "ci", "p.value"),
    table_header = NULL,
    table_text_size = 3,
    table_stat_label = NULL,
    ci_pattern = "{conf.low}, {conf.high}",
    table_witdhs = c(3, 2),
    plot_title = NULL,
    ...) {
  args <- list(...)

  # undocumented feature, we can pass directly `data`
  # used by ggcoef_multicomponents()
  if (is.null(args$data)) {
    data <- ggcoef_data(
      model = model,
      tidy_fun = tidy_fun,
      tidy_args = {{ tidy_args }},
      conf.int = conf.int,
      conf.level = conf.level,
      exponentiate = exponentiate,
      variable_labels = variable_labels,
      term_labels = term_labels,
      interaction_sep = interaction_sep,
      categorical_terms_pattern = categorical_terms_pattern,
      add_reference_rows = add_reference_rows,
      no_reference_row = {{ no_reference_row }},
      intercept = intercept,
      include = {{ include }},
      add_pairwise_contrasts = add_pairwise_contrasts,
      pairwise_variables = {{ pairwise_variables }},
      keep_model_terms = keep_model_terms,
      pairwise_reverse = pairwise_reverse,
      emmeans_args = emmeans_args,
      significance = significance,
      significance_labels = significance_labels
    )
  } else {
    data <- args$data
  }

  if (show_p_values && signif_stars) {
    data$add_to_label <- paste0(data$p_value_label, data$signif_stars)
  }
  if (show_p_values && !signif_stars) {
    data$add_to_label <- data$p_value_label
  }
  if (!show_p_values && signif_stars) {
    data$add_to_label <- data$signif_stars
  }

  if (show_p_values || signif_stars) {
    data$label <- forcats::fct_inorder(
      factor(
        paste0(
          data$label,
          ifelse(
            data$add_to_label == "",
            "",
            paste0(" (", data$add_to_label, ")")
          )
        )
      )
    )
    data$label_light <- forcats::fct_inorder(
      factor(
        paste0(
          data$label_light,
          ifelse(
            data$add_to_label == "",
            "",
            paste0(" (", data$add_to_label, ")")
          )
        )
      )
    )
  }

  args$data <- data
  args$exponentiate <- exponentiate

  if (!"y" %in% names(args) && !"facet_row" %in% names(args)) {
    args$y <- "label_light"
  }

  if (!"colour" %in% names(args) && !all(is.na(data$var_label))) {
    args$colour <- "var_label"
    if (!"colour_guide" %in% names(args)) {
      args$colour_guide <- FALSE
    }
  }

  if (!"y" %in% names(args)) args$y <- "label"
  if (!"facet_row" %in% names(args)) args$facet_row <- "var_label"
  if (!"stripped_rows" %in% names(args)) args$stripped_rows <- TRUE
  if (!"strips_odd" %in% names(args)) args$strips_odd <- "#11111111"
  if (!"strips_even" %in% names(args)) args$strips_even <- "#00000000"

  coef_plot <- do.call(ggcoef_plot, args)

  if (!is.null(plot_title)) {
    coef_plot <- coef_plot +
      ggplot2::ggtitle(plot_title) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        plot.title.position = "plot"
      )
  }

  if (args$stripped_rows) {
    if (!"term" %in% names(data)) {
      data$term <- data[[args$y]]
    }
    data <- data %>%
      dplyr::mutate(.fill = dplyr::if_else(
        as.integer(.in_order(.data$term)) %% 2L == 1,
        args$strips_even,
        args$strips_odd
      ))
  }

  # building the coefficient table ----
  tbl_data <- data

  if (!"estimate" %in% names(table_stat_label)) {
    table_stat_label$estimate <- scales::label_number(accuracy = .01)
  }
  if (!"conf.low" %in% names(table_stat_label)) {
    table_stat_label$conf.low <- scales::label_number(accuracy = .01)
  }
  if (!"conf.high" %in% names(table_stat_label)) {
    table_stat_label$conf.high <- scales::label_number(accuracy = .01)
  }
  if (!"p.value" %in% names(table_stat_label)) {
    table_stat_label$p.value <- scales::label_pvalue(add_p = FALSE)
  }
  for (v in names(table_stat_label)) {
    tbl_data[[v]] <- table_stat_label[[v]](tbl_data[[v]])
    tbl_data[[v]][is.na(tbl_data[[v]])] <- ""
  }

  tbl_data$ci <- stringr::str_glue_data(tbl_data, ci_pattern)
  tbl_data$ci[is.na(data$conf.low) & is.na(data$conf.high)] <- " "
  tbl_data <- tbl_data %>%
    tidyr::pivot_longer(
      dplyr::any_of(table_stat),
      names_to = "stat",
      values_to = "value",
      values_transform = as.character
    )
  tbl_data$stat <- factor(tbl_data$stat, levels = table_stat)

  if (!is.null(table_header) && length(table_header) != length(table_stat)) {
    cli::cli_abort("{.arg table_header} should have the same length as {.arg table_stat}.") # nolint
  }

  if (is.null(table_header)) {
    table_header <- table_stat
    if ("estimate" %in% table_header) {
      table_header[table_header == "estimate"] <-
        attr(data, "coefficients_label")
    }
    if ("ci" %in% table_header) {
      table_header[table_header == "ci"] <-
        paste(scales::percent(conf.level), "CI")
    }
    if ("p.value" %in% table_header) {
      table_header[table_header == "p.value"] <- "p"
    }
  }

  table_plot <- ggplot2::ggplot(tbl_data) +
    ggplot2::aes(
      x = .data[["stat"]],
      y = .data[[args$y]],
      label = .data[["value"]]
    ) +
    ggplot2::geom_text(hjust = .5, vjust = .5, size = table_text_size) +
    ggplot2::scale_x_discrete(position = "top", labels = table_header) +
    ggplot2::scale_y_discrete(
      limits = rev,
      expand = ggplot2::expansion(mult = 0, add = .5)
    ) +
    ggplot2::facet_grid(
      rows = args$facet_row,
      scales = "free_y", space = "free_y", switch = "y"
    ) +
    ggplot2::theme_light() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(face = "bold", hjust = .5),
      axis.text.y = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      strip.text = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )

  if (args$stripped_rows) {
    table_plot <- table_plot +
      geom_stripped_rows(
        mapping = ggplot2::aes(
          odd = .data[[".fill"]], even = .data[[".fill"]],
          colour = NULL, linetype = NULL
        )
      )
  }

  # join the plots
  patchwork::wrap_plots(coef_plot, table_plot, nrow = 1, widths = table_witdhs)
}


# not exporting ggcoef_data
ggcoef_data <- function(
    model,
    tidy_fun = broom.helpers::tidy_with_broom_or_parameters,
    tidy_args = NULL,
    conf.int = TRUE,
    conf.level = .95,
    exponentiate = FALSE,
    variable_labels = NULL,
    term_labels = NULL,
    interaction_sep = " * ",
    categorical_terms_pattern = "{level}",
    add_reference_rows = TRUE,
    no_reference_row = NULL,
    intercept = FALSE,
    include = dplyr::everything(),
    add_pairwise_contrasts = FALSE,
    pairwise_variables = broom.helpers::all_categorical(),
    keep_model_terms = FALSE,
    pairwise_reverse = TRUE,
    emmeans_args = list(),
    significance = conf.level,
    significance_labels = NULL) {
  rlang::check_installed("broom.helpers")

  if (length(significance) == 0) {
    significance <- NULL
  }

  data <- rlang::inject(broom.helpers::tidy_plus_plus(
    model = model,
    tidy_fun = tidy_fun,
    conf.int = conf.int,
    conf.level = conf.level,
    exponentiate = exponentiate,
    variable_labels = variable_labels,
    term_labels = term_labels,
    interaction_sep = interaction_sep,
    categorical_terms_pattern = categorical_terms_pattern,
    add_reference_rows = add_reference_rows,
    no_reference_row = {{ no_reference_row }},
    add_pairwise_contrasts = add_pairwise_contrasts,
    pairwise_variables = {{ pairwise_variables }},
    keep_model_terms = keep_model_terms,
    pairwise_reverse = pairwise_reverse,
    emmeans_args = emmeans_args,
    add_estimate_to_reference_rows = TRUE,
    add_header_rows = FALSE,
    intercept = intercept,
    include = {{ include }},
    keep_model = FALSE,
    !!!tidy_args
  ))

  if (!"p.value" %in% names(data)) {
    data$p.value <- NA_real_
    significance <- NULL
  }

  if (!is.null(significance)) {
    if (is.null(significance_labels)) {
      significance_labels <- paste(c("p \u2264", "p >"), significance)
    }
    data$significance <- factor(
      !is.na(data$p.value) & data$p.value <= significance,
      levels = c(TRUE, FALSE),
      labels = significance_labels
    )
  }

  data$signif_stars <- signif_stars(data$p.value, point = NULL)

  data$p_value_label <- ifelse(
    is.na(data$p.value),
    "",
    scales::pvalue(data$p.value, add_p = TRUE)
  )

  data <- data[!is.na(data$estimate), ]

  data$term <- .in_order(data$term)
  data$var_label <- .in_order(data$var_label)
  data$variable <- .in_order(data$variable)
  data$label <- .in_order(data$label)

  data$label_light <- dplyr::if_else(
    as.character(data$label) == as.character(data$var_label) &
      ((!grepl("^nmatrix", data$var_class)) | is.na(data$var_class)),
    "",
    as.character(data$label)
  ) %>%
    .in_order()

  data
}

.in_order <- function(x) {
  # droping unobserved value if needed
  forcats::fct_inorder(as.character(x))
}

##### Figure 4 Code #####

# Load the RUCC data, which is available here: https://www.ers.usda.gov/data-products/rural-urban-continuum-codes.aspx
rucc_data <- read.csv('/path/to/ruca/file')

# Ensure FIPS codes have leading zeros and are character type
rucc_data <- rucc_data %>%
  mutate(FIPS = sprintf("%05d", FIPS))

# Define the state_to_subregion list for mapping states to Census Divisions
state_to_subregion <- list(
  ME='New England', NH='New England', VT='New England', MA='New England',
  RI='New England', CT='New England',
  NY='Middle Atlantic', NJ='Middle Atlantic', PA='Middle Atlantic',
  WI='East North Central', MI='East North Central', IL='East North Central',
  IN='East North Central', OH='East North Central',
  MO='West North Central', ND='West North Central', SD='West North Central',
  NE='West North Central', KS='West North Central',
  MN='West North Central', IA='West North Central',
  DE='South Atlantic', MD='South Atlantic', DC='South Atlantic',
  VA='South Atlantic', WV='South Atlantic', NC='South Atlantic',
  SC='South Atlantic', GA='South Atlantic', FL='South Atlantic',
  KY='East South Central', TN='East South Central', MS='East South Central',
  AL='East South Central',
  OK='West South Central', AR='West South Central', TX='West South Central',
  LA='West South Central',
  ID='Mountain', MT='Mountain', WY='Mountain', NV='Mountain', UT='Mountain',
  CO='Mountain', AZ='Mountain', NM='Mountain',
  AK='Pacific', WA='Pacific', OR='Pacific', CA='Pacific', HI='Pacific',
  AS='U.S. Territories', GU='U.S. Territories', MP='U.S. Territories',
  PR='U.S. Territories', VI='U.S. Territories'
)

# Make state uppercase
rucc_data <- rucc_data %>%
  mutate(State = toupper(State))

# Add a column for Census Division
rucc_data <- rucc_data %>%
  mutate(us_census_subregion = sapply(State, function(x) state_to_subregion[[x]])) %>%
  mutate(us_census_subregion = as.character(us_census_subregion))

# Handle potential NA values in us_census_subregion
rucc_data <- rucc_data %>%
  mutate(us_census_subregion = ifelse(is.na(us_census_subregion), "Unknown", us_census_subregion))

# Categorize counties as Urban or Rural based on RUCC
rucc_data <- rucc_data %>%
  mutate(category = ifelse(RUCC_2013 <= 3, "Urban", "Rural"))

# Load county map data
counties <- get_urbn_map("counties", sf = TRUE)

# Merge map data with RUCC data
map_data <- counties %>%
  left_join(rucc_data, by = c("county_fips" = "FIPS"))

# Replace NA values in the category column with "Rural"
map_data <- map_data %>%
  mutate(category = ifelse(is.na(category), "Rural", category))

# Extract state boundaries for outlining states
state_boundaries <- map_data %>%
  group_by(state_name) %>%
  summarize(geometry = st_union(geometry), .groups = 'drop')

# Extract Census division boundaries for outlining subregions
subregion_boundaries <- map_data %>%
  group_by(us_census_subregion) %>%
  summarize(geometry = st_union(geometry), .groups = 'drop')

# Create a dataframe with average PI scores by subregion and rurality
avg_pi_scores <- data.frame(
  us_census_subregion = c('East North Central', 'East North Central', 'East South Central', 'East South Central',
                          'Middle Atlantic', 'Middle Atlantic', 'Mountain', 'Mountain',
                          'New England', 'New England', 'Pacific', 'Pacific',
                          'South Atlantic', 'South Atlantic', 'U.S. Territories',
                          'West North Central', 'West North Central', 'West South Central', 'West South Central'),
  rural.clinician = c('Rural', 'Urban', 'Rural', 'Urban',
                      'Rural', 'Urban', 'Rural', 'Urban',
                      'Rural', 'Urban', 'Rural', 'Urban',
                      'Rural', 'Urban', 'Urban',
                      'Rural', 'Urban', 'Rural', 'Urban'),
  avg_pi_score = c(54.4, 69.5, 42.2, 56.3,
                   33.8, 62.8, 61.5, 75.2,
                   87.0, 40.3, 57.6, 74.8,
                   64.4, 69.6, 3.10,
                   66.2, 73.0, 47.0, 61.2)
)

# Determine the overall min and max PI scores to use the same scale across both urban and rural
min_pi_score <- min(avg_pi_scores$avg_pi_score, na.rm = TRUE)
max_pi_score <- max(avg_pi_scores$avg_pi_score, na.rm = TRUE)

# Function to create a combined map with the same color palette and formatting
create_combined_map_with_colors <- function(data, avg_data, title, state_boundaries, subregion_boundaries, min_score, max_score) {
  data <- data %>%
    left_join(avg_data, by = c("us_census_subregion" = "us_census_subregion", "category" = "rural.clinician")) %>%
    mutate(color_category = ifelse(category == "Urban", "Urban", "Rural"))

  ggplot() +
    geom_sf(data = data, aes(fill = avg_pi_score), color = NA) +
    geom_sf(data = state_boundaries, fill = NA, color = "black", size = 0.2) +
    geom_sf(data = subregion_boundaries, fill = NA, color = "black", size = 0.8) +
    scale_fill_viridis_c(option = "plasma", limits = c(min_score, max_score), direction = -1, na.value = "grey50", name = "Avg PI Score") +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid = element_blank(),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8)) +
    labs(title = title,
         fill = "Avg PI Score")
}

# Create the combined map with the same color palette for urban and rural counties
combined_map_with_colors <- create_combined_map_with_colors(map_data, avg_pi_scores,
                                                            "Combined Urban and Rural Counties - Promoting Interoperability Score by Census Division",
                                                            state_boundaries, subregion_boundaries, min_pi_score, max_pi_score)

# Print the combined map
print(combined_map_with_colors)
