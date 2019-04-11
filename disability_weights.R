# Load Packages
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggrepel)

# Load and reshape responses
responses <- read_xlsx("responses.xlsx")
responses_long <- responses %>% 
  gather(description, severity, -Timestamp)

# Create a short description (sorted by the mean severity for plotting)
responses_long$short_description <- reorder(
  factor(substr(responses_long$description, 1, 100)), 
  responses_long$severity, 
  mean
)

# Look at histograms of the disability weight for each description
ggplot(responses_long) +
  geom_histogram(mapping = aes(x = severity)) +
  facet_wrap(~short_description)
ggsave("charts/hist_cause.png", width=10, height = 10)
  
# Create a violin plot to show distribution of responses (by cause)
ggplot(responses_long, aes(short_description, severity)) +
  geom_violin(draw_quantiles = c(0.5)) +
  labs(x="Description", y="Disability Weight", 
       title="Distribution of Disability Weights") +
  coord_flip()
ggsave("charts/violin_cause.png", width=10, height = 10)

# Create a timestamp (sorted by the mean severity for plotting)
responses_long$person <- reorder(
  factor(as.character(responses_long$Timestamp)), 
  responses_long$severity, 
  median
)


# Create a violin plot to show distribution of responses (by person)
ggplot(responses_long, aes(person, severity)) +
  geom_violin(draw_quantiles = c(0.5)) +
  labs(x="Person", y="Disability Weight", 
       title="Distribution of Disability Weights") +
  coord_flip()
ggsave("charts/violin_person.png", width=10, height = 10)

# Load and format GBD data
gbd_file <- "IHME_GBD_2016_DISABILITY_WEIGHTS_3/IHME_GBD_2016_DISABILITY_WEIGHTS_Y2017M09D14.CSV"
ihme_weights <- read.csv(gbd_file, stringsAsFactors = F) %>% 
  rename(description = Health.state.lay.description) %>% 
  distinct(description, .keep_all=T) %>% 
  mutate(short_description = substr(description, 1, 100))

# Group by description
grouped <- responses_long %>% group_by(description) %>% 
  summarize(mean_score = mean(severity, na.rm=T))

# Join data
joined <- grouped %>% 
  left_join(ihme_weights, by="description")

# Create labeled scatter
ggplot(joined) +
  geom_label_repel(aes(x=mean_score/10, y=disability.weight, label=Health.state.name)) +
  labs(x="In Class", y="IHME", title="In Class v.s. IHME Weights") +
  xlim(0, 1) + 
  ylim(0, 1) +
  geom_abline(slope=1, intercept=0, alpha = .3)

ggsave("charts/comparison_labeled.png", width=10, height = 10)

# Create scatter
ggplot(joined) +
  geom_point(aes(x=mean_score/10, y=disability.weight)) +
  labs(x="In Class", y="IHME", title="In Class v.s. IHME Weights") +
  xlim(0, 1) + 
  ylim(0, 1) +
  geom_abline(slope=1, intercept=0, alpha = .3)

ggsave("charts/comparison.png", width=10, height = 10)
