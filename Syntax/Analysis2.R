library(tidyverse)
library(pheatmap)
library(ggrepel)

setwd(")

codes <- read.csv("Codes.csv")

data <- list()

for (file in codes$Filename) {
  data[[file]] <- read.csv(file)
}

df <- Reduce(function(x, y) merge(x, y, by = c("Name", "Category"), all = TRUE), data)
df[is.na(df)] <- 0
df[df == "Present"] <- 999.999
excluded_columns <- c("Name", "Category")
df[ , !names(df) %in% excluded_columns] <- lapply(df[ , !names(df) %in% excluded_columns], as.double)
d <- pivot_longer(df, cols = c(-Name, -Category), names_to = "Media", values_to = "Concentration")


fix_names <- function(d) {
  d$Name <- gsub("\\s*\\(.*", "", d$Name)
  d$Name <- ifelse(grepl("^L-Tyrosine", d$Name), "L-Tyrosine", d$Name)
  d$Name <- ifelse(grepl("^L-Arginine", d$Name), "L-Arginine", d$Name)
  d$Name <- ifelse(grepl("^L-Asparagine", d$Name), "L-Asparagine", d$Name)
  d$Name <- ifelse(grepl("^L-Cysteine", d$Name), "L-Cysteine", d$Name)
  d$Name <- ifelse(grepl("^L-Cystine", d$Name), "L-Cystine", d$Name)
  d$Name <- ifelse(grepl("^L-Histidine", d$Name), "L-Hystidine", d$Name)
  d$Name <- gsub("Zinc Sulfate • 7H2O", "Zinc sulphate", d$Name)
  d$Name <- gsub(" • Na", "", d$Name)
  d$Name <- gsub(" • \\d+H2O", "", d$Name)
  d$Name <- gsub("• HCl", "hydrochloride", d$Name)
  d$Name <- gsub("2HCl", "hydrochloride", d$Name)
  d$Name <- gsub("-H2O", "", d$Name)
  d$Name <- gsub("Hypoxanthine Na", "Hypoxanthine", d$Name)
  # d$Name <- str_to_title(d$Name)
  d$Name <- gsub("Zinc sulphate", "Zinc sulfate", d$Name)
  d$Name <- gsub("2- mercaptoethanol", "Mercaptoethanol", d$Name)
  d$Name <- gsub("2-Mercaptoethanol", "Mercaptoethanol", d$Name)
  d$Name <- gsub("Ammonium metavanadate", "Ammonium Metavanadate", d$Name)
  d$Name <- gsub("Ascorbic acid", "Ascorbic Acid", d$Name)
  d$Name <- gsub("Barium", "Ba", d$Name)
  d$Name <- gsub("BSA", "Albumin", d$Name)
  d$Name <- gsub("Choline chloride", "Choline Chloride", d$Name)
  d$Name <- gsub("Cupric sulfate", "Cupric Sulfate", d$Name)
  d$Name <- gsub("D-Biotin", "Biotin", d$Name)
  d$Name <- gsub("d-Biotin", "Biotin", d$Name)
  d$Name <- gsub("bFGF", "FGF", d$Name)
  d$Name <- gsub("FGF-basic", "FGF", d$Name)
  d$Name <- gsub("FGF2", "FGF", d$Name)
  d$Name <- gsub("glutathione", "Glutathione", d$Name)
  d$Name <- gsub("Insulin Recombinant Full Chain", "Insulin", d$Name)  
  d$Name <- gsub("^L-", "", d$Name) 
  d$Name <- gsub("^L ", "", d$Name) 
  # d$Name <- gsub("MnSO4 • H2O", "Manganese Sulfate", d$Name)  
  d$Name <- ifelse(grepl("Inositol", d$Name), "Inositol", d$Name)
  d$Name <- ifelse(grepl("Insulin", d$Name), "Insulin", d$Name)
  d$Name <- ifelse(grepl("Albumin", d$Name), "Albumin", d$Name)
  d$Name <- ifelse(grepl("albumin", d$Name), "Albumin", d$Name)
  d$Name <- ifelse(grepl("TGF", d$Name), "TGF", d$Name)
  d$Name <- ifelse(grepl("FGF", d$Name), "FGF", d$Name)
  d$Name <- ifelse(grepl("ransferrin", d$Name), "Transferrin", d$Name)
  d$Name <- gsub("naHCO3", "Sodium Bicarbonate", d$Name) 
  d$Name <- gsub("NaHCO3", "Sodium Bicarbonate", d$Name) 
  d$Name <- gsub("Sodium Phosphate dibasic", "Sodium Phosphate Dibasic", d$Name) 
  d$Name <- gsub("Sodium Phosphate monobasic", "Sodium Phosphate Monobasic", d$Name) 
  d$Name <- gsub("Sodium Meta Silicate Na2SiO3 9H2O", "Sodium Metasilicate", d$Name)
  d$Name <- gsub("Thiamine ", "Thiamine", d$Name)   
  d$Name <- gsub("Tin Chloride", "Stannous Chloride", d$Name) 
  d$Name <- gsub("glutamine", "Glutamine", d$Name)
  d$Name <- gsub("Aspartic acid", "Aspartic Acid", d$Name)
  d$Name <- gsub("selenium", "Selenium", d$Name)
  d$Name <- gsub("DL-Thioctic Acid", "Lipoic Acid", d$Name)
  d$Name <- gsub("DL-alpha-Lipoic Acid", "Lipoic Acid", d$Name)
  d
}

d <- fix_names(d)
d$Category <- str_to_title(d$Category)
d <- d %>%
  group_by(Media, Name, Category) %>%
  mutate(Concentration = max(Concentration)) %>%
  ungroup()
d <- unique(d)

d   <- d %>%
  filter(Media != "GlutaMAX")

df <- pivot_wider(d, names_from = "Media", values_from = "Concentration" )
df <- df %>%
  arrange(Category) 
write.csv(df, "Data.csv")
#-------------------------------------------------------------------------------
# Overview
total <- n_distinct(d$Name)
total
d %>%
  group_by(Category) %>%
  summarize(perc = n_distinct(Name)/total*100)

d.count <- d

d.count$Category <- ifelse(d.count$Category %in% c("Proteins", "Carbohydrates"), "Other", d.count$Category)
d.count$Category <- ifelse(d.count$Category %in% c("Trace Elements", "Inorganic Salts"), "Inorganic Salts and Trace Elements", d.count$Category)

d.count <- d.count %>%
  filter(Concentration > 0) %>%
  group_by(Name, Category) %>%
  summarize(count = n_distinct(Media),
            percent = count/n_distinct(d$Media)*100,
            component = first(Media), .groups = 'drop')

# Most common in each category
d.plot <- d.count %>%
  group_by(Category) %>%
  arrange(desc(count)) %>% 
  filter(count != 1) %>%
  slice(1:11)

ggplot(d.plot, aes(x = Name, y = count, fill = Category)) + 
  facet_wrap( ~ Category, scales = "free_x") +
  geom_bar(stat = "identity") + 
  labs(x = "Media component", y = "Count")  +   
  theme(
    legend.position = "none",
    plot.margin = margin(0.2, 0.8, 0.2, 0.2, "cm"),
    axis.text.x = element_text(angle = -55, hjust = 0, vjust = 1)
  )

ggplot(d.plot, aes(x = Name, y = percent, fill = Category)) + 
  facet_wrap( ~ Category, scales = "free_x") +
  geom_bar(stat = "identity") + 
  labs(x = "Media component", y = "Percent") +   
  theme(
    legend.position = "none",
    plot.margin = margin(0.2, 0.8, 0.2, 0.2, "cm"),
    axis.text.x = element_text(angle = -55, hjust = 0, vjust = 1)
  )

#===============================================================================
# Basal Media

d.c <- codes %>%
  select(Code, Group, Cell)
colnames(d.c) <- c("Media", "Group", "Cell")

d.data <- merge(d, d.c, by = "Media" )
d.data <- d.data %>%
  filter(Group == "Basal Media") %>%
  select(-Group, -Cell) %>%
  unique()

total <- n_distinct(d.data %>%
                      filter(Concentration != 0) %>%
                      select(Name))
total
d.data %>%
  filter(Concentration != 0) %>%
  group_by(Category) %>%
  summarize(perc = n_distinct(Name)/total*100)

# Heatmap

# Concentration

d.c <- codes %>%
  select(Code, Group, Cell)
colnames(d.c) <- c("Media", "Group", "Cell")

d.data <- merge(d, d.c, by = "Media" )
d.data <- d.data %>%
  filter(Group == "Basal Media") %>%
  select(-Group, -Cell) %>%
  unique()
d.data <- pivot_wider(d.data, names_from = "Media", values_from = "Concentration" )

d.heat <- d.data %>%
  select(-Category)

d.heat[d.heat == 999.999] <- 0

d.heat <- as.data.frame(d.heat)
m.heat <- as.matrix(d.heat[, -1]) 
rownames(m.heat) <- c(d.heat[, 1]) # This sets the names
colnames(m.heat) <- gsub("\\.", "-", colnames(m.heat)) # This modifies the sample names for each column (optional)


# rownames(m.heat) <- NULL # This removes the names to make the heatmap smaller
p <- pheatmap(m.heat,
              # scale = "row",
              clustering_method = "complete",  # Hierarchical clustering method
              cluster_rows = FALSE, 
              display_numbers = FALSE,  
              fontsize_row = 4,  
              fontsize_col = 12,  
              main = "" ,
              angle_col = 315
)

p

# Presence/Absence

d.data <- merge(d, d.c, by = "Media" )
d.data <- d.data %>%
  filter(Group == "Basal Media") %>%
  select(-Group, -Cell) %>%
  unique()
d.data <- pivot_wider(d.data, names_from = "Media", values_from = "Concentration" )

d.heat <- d.data %>%
  select(-Category)

d.heat[sapply(d.heat, is.numeric)] <- lapply(d.heat[sapply(d.heat, is.numeric)], function(x) ifelse(x != 0, 1, 0))


d.heat <- d.heat[rowSums(d.heat[, -1]) != 0 & rowSums(d.heat[, -1]) != ncol(d.heat) - 1, ] 
d.heat <- as.data.frame(d.heat)
d.heat <- na.omit(d.heat)
m.heat <- as.matrix(d.heat[, -1]) 
rownames(m.heat) <- c(d.heat[, 1]) # This sets the names
colnames(m.heat) <- gsub("\\.", "-", colnames(m.heat)) # This modifies the sample names for each column (optional)


# rownames(m.heat) <- NULL # This removes the names to make the heatmap smaller
p <- pheatmap(m.heat,
              color = c("#9a98ed", "#ff6863"), 
              # scale = "row",
              clustering_method = "complete",  # Hierarchical clustering method
              cluster_rows = FALSE, 
              display_numbers = FALSE,  
              fontsize_row = 4,  
              fontsize_col = 12,  
              main = "" ,
              legend_breaks = c(0,1),
              legend_labels = c("0" = "Absence", "1" = "Presence"),
              angle_col = 315
)

p

#-------------------------------------------------------------------------------
# Component count 
# Presence /Absence

d.c <- codes %>%
  select(Code, Group, Cell)
colnames(d.c) <- c("Media", "Group", "Cell")

d.data <- merge(d, d.c, by = "Media" )
d.data <- d.data %>%
  filter(Group == "Basal Media") %>%
  select(-Group, -Cell) %>%
  unique()

d.count <- d.data 

d.count$Category <- ifelse(d.count$Category %in% c("Proteins", "Carbohydrates"), "Other", d.count$Category)
d.count$Category <- ifelse(d.count$Category %in% c("Trace Elements"), "Inorganic Salts", d.count$Category)

d.count <- d.count %>%
  filter(Concentration > 0) %>%
  filter(Concentration != 999.999) %>%
  group_by(Name, Category) %>%
  summarize(count = n_distinct(Media),
            percent = count/n_distinct(d.data$Media)*100,
            component = paste0(Media, collapse = ", "), .groups = 'drop')

# Most common in each category
d.plot <- d.count %>%
  group_by(Category) %>%
  arrange(desc(count)) %>% 
  filter(count != 1) %>%
  slice(1:11)

ggplot(d.plot, aes(x = Name, y = count, fill = Category)) + 
  facet_wrap( ~ Category, scales = "free_x") +
  geom_bar(stat = "identity") + 
  labs(x = "Media component", y = "Count")  +   
  theme(
    legend.position = "none",
    plot.margin = margin(0.2, 0.8, 0.2, 0.2, "cm"),
    axis.text.x = element_text(angle = -55, hjust = 0, vjust = 1)
  )

ggplot(d.plot, aes(x = Name, y = percent, fill = Category)) + 
  facet_wrap( ~ Category, scales = "free_x") +
  geom_bar(stat = "identity") + 
  labs(x = "Media component", y = "Percent") +   
  theme(
    legend.position = "none",
    plot.margin = margin(0.2, 0.8, 0.2, 0.2, "cm"),
    axis.text.x = element_text(angle = -55, hjust = 0, vjust = 1)
  )

# Unique components
d.unique <- d.count %>%
  filter(count < 4)

view(d.unique)

write.csv(d.unique, "unique-basal.csv")

#-------------------------------------------------------------------------------
# Unique components
# Presence /Absence
d.stats <- d %>%
  filter(Concentration != 0) %>%
  group_by(Name) %>%
  filter(n_distinct(Media) < 2) %>%
  group_by(Name, Media) %>%
  summarise(n = n())
#-------------------------------------------------------------------------------
# Media composition breakdown

# Presence /Absence

d.plot <- d.data %>%
  filter(Concentration != 0) %>%
  group_by(Media) %>%
  mutate(n.component = n_distinct(Name)) %>%
  group_by(Media, Category) %>%
  summarize(n = n_distinct(Name),
            percentage = n_distinct(Name)/n.component*100) %>%
  unique()

d.plot <- distinct(d.plot)


ggplot(d.plot, aes(x = Media, y = percentage, fill = Category)) +
  geom_bar(stat = "identity", color = "black") +
  # geom_text(position = position_stack(vjust = 0.5), color = "black", size = 3, aes(label = paste(Category, paste0("(", percentage, ")"), sep = " "))) +
  labs(title = "",
       x = "Media",
       y = "Percent",
       fill = "") +
  theme(
    plot.margin = margin(0.2, 0.8, 0.2, 0.2, "cm"),
    legend.position = "top",
    axis.text.x = element_text(angle = -55, hjust = 0, vjust = 1)
  )

ggplot(d.plot, aes(x = Media, y = n, fill = Category)) +
  geom_bar(stat = "identity", color = "black") +
  # geom_text(position = position_stack(vjust = 0.5), color = "black", size = 3, aes(label = paste(Category, paste0("(", percentage, ")"), sep = " "))) +
  labs(title = "",
       x = "Media",
       y = "Number of components",
       fill = "") +
  theme(
    plot.margin = margin(0.2, 0.8, 0.2, 0.2, "cm"),
    legend.position = "top",
    axis.text.x = element_text(angle = -55, hjust = 0, vjust = 1)
  )



#-------------------------------------------------------------------------------
# Concentration

d.plot <- d.data %>%
  filter(Concentration > 0) %>%
  filter(Concentration != 999.999) %>%
  group_by(Media) %>%
  mutate(sum = sum(Concentration)) %>%
  group_by(Media, Category) %>%
  summarize(mass = sum(Concentration),
            percentage = sum(Concentration)/sum*100) 

d.plot <- distinct(d.plot)


ggplot(d.plot, aes(x = Media, y = percentage, fill = Category)) +
  geom_bar(stat = "identity", color = "black") +
  # geom_text(position = position_stack(vjust = 0.5), color = "black", size = 3, aes(label = paste(Category, paste0("(", percentage, ")"), sep = " "))) +
  labs(title = "",
       x = "Media",
       y = "Mass percent",
       fill = "") +
  theme(
    plot.margin = margin(0.2, 0.8, 0.2, 0.2, "cm"),
    legend.position = "top",
    axis.text.x = element_text(angle = -55, hjust = 0, vjust = 1)
  )

#===============================================================================
# Supplements

d.df12 <- d %>%
  filter(Media == "DMEM.F12") %>%
  filter(Concentration != 0)
d.imdm <- d %>%
  filter(Media == "IMDM") %>%
  filter(Concentration != 0)

d.c <- codes %>%
  select(Code, Group, Cell)
colnames(d.c) <- c("Media", "Group", "Cell")

d.dc <- merge(d, d.c, by = "Media" )
d.data <- d.dc %>%
  filter(Group == "Serum Free Media") %>%
  select(-Group, -Cell) %>%
  unique()

d.data <- d.data %>%
  filter(
    !((Media == "StemSpan") & (Name %in% d.imdm$Name)),
    !((Media != "StemSpan") & (Name %in% d.df12$Name))
  )

d.data2 <- d.dc %>%
  filter(Group == "Serum-Replacement Supplement") %>%
  select(-Group, -Cell) %>%
  unique()

d.data <- rbind(d.data, d.data2)

total <- n_distinct(d.data %>%
                      filter(Concentration != 0) %>%
                      select(Name))
total
d.data %>%
  filter(Concentration != 0) %>%
  group_by(Category) %>%
  summarize(perc = n_distinct(Name)/total*100)

# Heatmap

# Concentration

d.heat <- pivot_wider(d.data, names_from = "Media", values_from = "Concentration" )

d.heat <- d.heat %>%
  select(-Category)

d.heat[d.heat == 999.999] <- 0

d.heat <- as.data.frame(d.heat)
m.heat <- as.matrix(d.heat[, -1]) 
rownames(m.heat) <- c(d.heat[, 1]) # This sets the names
colnames(m.heat) <- gsub("\\.", "-", colnames(m.heat)) # This modifies the sample names for each column (optional)

m.heat.log <- log10(m.heat + 1e-6)

# rownames(m.heat) <- NULL # This removes the names to make the heatmap smaller
p <- pheatmap(m.heat,
              # scale = "row",
              clustering_method = "complete",  # Hierarchical clustering method
              cluster_rows = FALSE, 
              display_numbers = FALSE,  
              fontsize_row = 4,  
              fontsize_col = 12,  
              main = "" ,
              angle_col = 315
)

p

# Presence/Absence


d.heat <- pivot_wider(d.data, names_from = "Media", values_from = "Concentration" )

d.heat <- d.heat %>%
  select(-Category)

d.heat[sapply(d.heat, is.numeric)] <- lapply(d.heat[sapply(d.heat, is.numeric)], function(x) ifelse(x != 0, 1, 0))


d.heat <- d.heat[rowSums(d.heat[, -1]) != 0 & rowSums(d.heat[, -1]) != ncol(d.heat) - 1, ] 
d.heat <- as.data.frame(d.heat)
d.heat <- na.omit(d.heat)
m.heat <- as.matrix(d.heat[, -1]) 
rownames(m.heat) <- c(d.heat[, 1]) # This sets the names
colnames(m.heat) <- gsub("\\.", "-", colnames(m.heat)) # This modifies the sample names for each column (optional)


# rownames(m.heat) <- NULL # This removes the names to make the heatmap smaller
p <- pheatmap(m.heat,
              color = c("#9a98ed", "#ff6863"), 
              # scale = "row",
              clustering_method = "complete",  # Hierarchical clustering method
              cluster_rows = FALSE, 
              display_numbers = FALSE,  
              fontsize_row = 4,  
              fontsize_col = 12,  
              main = "" ,
              legend_breaks = c(0,1),
              legend_labels = c("0" = "Absence", "1" = "Presence"),
              angle_col = 315
)

p

#-------------------------------------------------------------------------------
# Component count 
# Presence /Absence


d.dc <- merge(d, d.c, by = "Media" )
d.data <- d.dc %>%
  filter(Group == "Serum Free Media") %>%
  unique()

d.data <- d.data %>%
  filter(
    !(Media == "StemSpan" & Name %in% d.imdm$Name),
    !(Media != "StemSpan" & Name %in% d.df12$Name)
  )

d.data2 <- d.dc %>%
  filter(Group == "Serum-Replacement Supplement") %>%
  unique()

d.data <- rbind(d.data, d.data2)

d.cell <- d.data %>%
  filter(Concentration > 0) %>%
  group_by(Cell, Name) %>%
  summarize(count = n_distinct(Media),
            percent = count/n_distinct(d.data$Media)*100)


d.cell <- d.data %>%
  filter(Concentration > 0) %>%
  group_by(Name) %>%
  filter(n_distinct(Cell) == 1) %>%
  group_by(Cell, Name) %>%
  summarize(count = n_distinct(Media),
            percent = count/n_distinct(d.data$Media)*100)

d.count <- d.data 

d.count$Category <- ifelse(d.count$Category %in% c("Proteins", "Carbohydrates"), "Other", d.count$Category)
d.count$Category <- ifelse(d.count$Category %in% c("Trace Elements"), "Inorganic Salts", d.count$Category)

d.count <- d.count %>%
  filter(Concentration > 0) %>%
  group_by(Name, Category) %>%
  summarize(count = n_distinct(Media),
            percent = count/n_distinct(d.data$Media)*100)

# Most common in each category
d.count$Name <- str_replace(d.count$Name, "^(.)", ~toupper(.x))

d.plot <- d.count %>%
  arrange(Category, desc(count)) %>%
  mutate(
    Name = fct_inorder(Name),      
    Category = fct_inorder(Category)    
  )


ggplot(d.plot, aes(x = Name, y = count, fill = Category)) + 
  geom_bar(stat = "identity") + 
  labs(x = "Media component", y = "Count")  +   
  theme(
    legend.position = "none",
    plot.margin = margin(0.2, 0.8, 0.2, 0.2, "cm"),
    axis.text.x = element_text(angle = -55, hjust = 0, vjust = 1)
  )

ggplot(d.plot, aes(x = Name, y = percent, fill = Category)) + 
  geom_bar(stat = "identity") + 
  labs(x = "Media component", y = "Percent") +   
  theme(
    legend.position = "none",
    plot.margin = margin(0.2, 0.8, 0.2, 0.2, "cm"),
    axis.text.x = element_text(angle = -55, hjust = 0, vjust = 1)
  )


#-------------------------------------------------------------------------------
# Unique components
# Presence /Absence
d.unique <- d.data %>%
  filter(Concentration != 0) %>%
  group_by(Name) %>%
  filter(n_distinct(Media) < 2) %>%
  group_by(Name, Media) %>%
  summarise(n = n())

view(d.unique)

write.csv(d.unique, "unique-supplements.csv")
#-------------------------------------------------------------------------------
# Media composition breakdown

# Presence /Absence

d.plot <- d.data %>%
  filter(Concentration != 0) %>%
  group_by(Media) %>%
  mutate(n.component = n_distinct(Name)) %>%
  group_by(Media, Category) %>%
  summarize(n = n_distinct(Name),
            percentage = n_distinct(Name)/n.component*100) %>%
  unique()

d.plot <- distinct(d.plot)


ggplot(d.plot, aes(x = Media, y = percentage, fill = Category)) +
  geom_bar(stat = "identity", color = "black") +
  # geom_text(position = position_stack(vjust = 0.5), color = "black", size = 3, aes(label = paste(Category, paste0("(", percentage, ")"), sep = " "))) +
  labs(title = "",
       x = "Media",
       y = "Percent",
       fill = "") +
  theme(
    plot.margin = margin(0.2, 0.8, 0.2, 0.2, "cm"),
    legend.position = "top",
    axis.text.x = element_text(angle = -55, hjust = 0, vjust = 1)
  )

ggplot(d.plot, aes(x = Media, y = n, fill = Category)) +
  geom_bar(stat = "identity", color = "black") +
  # geom_text(position = position_stack(vjust = 0.5), color = "black", size = 3, aes(label = paste(Category, paste0("(", percentage, ")"), sep = " "))) +
  labs(title = "",
       x = "Media",
       y = "Number of components",
       fill = "") +
  theme(
    plot.margin = margin(0.2, 0.8, 0.2, 0.2, "cm"),
    legend.position = "top",
    axis.text.x = element_text(angle = -55, hjust = 0, vjust = 1)
  )




#-------------------------------------------------------------------------------
# Concentration

d.plot <- d.data %>%
  filter(Concentration > 0) %>%
  group_by(Media) %>%
  mutate(sum = sum(Concentration)) %>%
  group_by(Media, Category) %>%
  summarize(mass = sum(Concentration),
            percentage = sum(Concentration)/sum*100) 

d.plot <- distinct(d.plot)


ggplot(d.plot, aes(x = Media, y = percentage, fill = Category)) +
  geom_bar(stat = "identity", color = "black") +
  # geom_text(position = position_stack(vjust = 0.5), color = "black", size = 3, aes(label = paste(Category, paste0("(", percentage, ")"), sep = " "))) +
  labs(title = "",
       x = "Media",
       y = "Mass percent",
       fill = "") +
  theme(
    plot.margin = margin(0.2, 0.8, 0.2, 0.2, "cm"),
    legend.position = "top",
    axis.text.x = element_text(angle = -55, hjust = 0, vjust = 1)
  )
#-------------------------------------------------------------------------------
# Cell types

d.cell <- d.data %>%
  separate_rows(Cell, sep = ", ")
unique(d.cell$Cell)

d.count <- d.cell %>%
  filter(Concentration > 0) %>%
  group_by(Name) %>%
  summarise(n.media = n_distinct(Media),
            n.cell = n_distinct(Cell),
            cell = paste0(unique(Cell), collapse = ", "), 
            media = paste0(unique(Media), collapse = ", "))
write.csv(d.count, "cell-media.csv")
unique <- d.count %>%
  filter(n.cell < 2)
d.plot <- d.cell %>%
  filter(Concentration > 0) %>%
  filter(Name %in% unique$Name) %>%
  group_by(Cell) %>%
  summarise(Component = paste0(unique(Name), collapse = ", "))
write.csv(d.plot, "unique-cell.csv")
#===============================================================================
# PCA

# Concentration


d.pc <- d
d.pc[d.pc == 999.999] <- 0
d.pc <- d.pc %>%
  select(-Category) %>%
  group_by(Media, Name) %>%
  summarise(Concentration = sum(Concentration)) %>%
  ungroup()

d.pc <- pivot_wider(d.pc, names_from = "Name", 
                    values_from = "Concentration")



d.pca <- d.pc %>% select(-Media)
d.pca[is.na(d.pca)] <- 0
d.pca <- d.pca[, colSums(d.pca != 0) > 0]
pca.result <- prcomp(d.pca, center = TRUE, scale. = TRUE)
summary(pca.result)

plot_score(pca.result)
plot_loading(pca.result)



#-------------------------------------------------------------------------------

plot_score <- function(pca.result, pcs = 1:2) {
  
  pca.scores <- pca.result$x
  pc.data <- as.data.frame(pca.scores[, 1:2])
  pc.data$Code = d.pc$Media
  pc.data$Code = gsub("DMEM.F12", "DMEM-F12", pc.data$Code)
  
  d.scores <- merge(pc.data, codes, by = "Code")
  
  std_dev <- pca.result$sdev
  variance_explained <- std_dev^2
  variance <- variance_explained / sum(variance_explained)*100
  variance <- paste(round(variance, digits = 1))
  
  
  p <- ggplot(d.scores) +
    geom_point(aes(x = PC1, y = PC2, colour = Group), alpha = 0.5, size = 4) +
    stat_ellipse(aes(x = PC1, y = PC2, colour = Group), level = 0.95) +
    # coord_cartesian(xlim = c(-8, 8), ylim = c(-8,8)) +
    scale_colour_brewer(palette = "Dark2") +
    theme(legend.position = "none") +
    labs(title = "",
         color = "") +
    xlab(paste0("Component 1 (",variance[1], "%)")) +
    ylab(paste0("Component 2 (",variance[2], "%)"))
  p
  
}



plot_loading <- function(pca.result, pcs = 1:2) {
  
  loadings <- pca.result$rotation
  loadings.df <- as.data.frame(loadings)
  loadings.df$compounds <- rownames(loadings.df)
  loadings.df <- loadings.df %>%
    select(compounds, all_of(paste0("PC", pcs)))
  
  group <- d %>%
    select(Name, Category) %>%
    distinct()
  
  colnames(group) <- c("compounds", "group")
  
  loadings.df <- merge(loadings.df, group, by = "compounds")
  
  std_dev <- pca.result$sdev
  variance_explained <- std_dev^2
  variance <- variance_explained / sum(variance_explained)*100
  variance <- paste(round(variance, digits = 1))
  
  
  circle_data <- data.frame(
    x = cos(seq(0, 2 * pi, length.out = 200))/2,
    y = sin(seq(0, 2 * pi, length.out = 200))/2
  )
  
  p <- ggplot(loadings.df) +
    geom_point(aes(x = PC1, y = PC2, color = group), size = 3.5) +
    geom_point(data = circle_data, aes(x, y), color = "black", size = 0.5) +
    geom_abline(intercept = 0, slope = 0, color = "black") +
    geom_abline(intercept = 0, slope = 1e17, color = "black") +
    geom_text_repel(aes(x = PC1, y = PC2, label = compounds, color = group),
                    force = 1.5, hjust = -0.1, max.overlaps = 30, size = 4) +
    scale_colour_brewer(palette = "Dark2") +
    xlim(-0.5, 0.5) +
    ylim(-0.5, 0.5) +
    theme(legend.position = "none") +
    xlab(paste0("Component 1 loading (",variance[1], "%)")) +
    ylab(paste0("Component 2 loading (",variance[2], "%)"))
  
  p  
}

#-------------------------------------------------------------------------------
