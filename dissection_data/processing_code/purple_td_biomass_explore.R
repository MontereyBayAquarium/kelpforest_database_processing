

################################################################################
# About
# 


################################################################################

rm(list=ls())

librarian::shelf(tidyverse,here, janitor, googlesheets4)
gs4_auth()

#set paths
figdir <- here::here("dissection_data","figures")

#read urchin data
urch_dat_orig <- read_sheet("https://docs.google.com/spreadsheets/d/1Ih-hBXRtfXVMdxw5ibZnXy_dZErdcx5FfeKMSc0HEc4/edit?gid=0#gid=0") %>% clean_names() %>%
  filter(species == "purple_urchin") %>%
  filter(!(is.na(test_diameter_mm) | is.na(animal_24hr_mass_g))) %>%
  filter(animal_24hr_mass_g < 100)


################################################################################
# derive parameters

#take a look
plot(urch_dat_orig$test_diameter_mm, urch_dat_orig$animal_24hr_mass_g)

# Initial estimates based on data
a_init <- -20
b_init <- 10
c_init <- 0.03

# Fit the biomass_fun model to the data with initial estimates
set.seed(1985)
fit <- nls(animal_24hr_mass_g ~ a + b * exp(c * test_diameter_mm), 
           data = urch_dat_orig,
           start = list(a = a_init, b = b_init, c = c_init))


# Extract the estimated parameters
a_est <- coef(fit)["a"]
b_est <- coef(fit)["b"]
c_est <- coef(fit)["c"]

# Print the estimated parameters
cat("Estimated Parameters:\n")
cat("a:", a_est, "\n")
cat("b:", b_est, "\n")
cat("c:", c_est, "\n")


################################################################################
#determine fit
# Calculate the predicted values from the model
predicted_values <- fitted(fit)

# Calculate the residuals
residuals <- urch_dat_orig$animal_24hr_mass_g - predicted_values

# Calculate the RSS (Residual Sum of Squares)
rss <- sum(residuals^2)

# Calculate the TSS (Total Sum of Squares)
mean_soft_mass <- mean(urch_dat_orig$animal_24hr_mass_g)
tss <- sum((urch_dat_orig$animal_24hr_mass_g - mean_soft_mass)^2)

# Calculate R-squared
r_squared <- 1 - (rss / tss)

# Print the R-squared value
cat("R-squared:", r_squared, "\n")


################################################################################
#plot


base_theme <-  theme(axis.text=element_text(size=12, color = "black"),
                     axis.title=element_text(size=12,color = "black"),
                     plot.tag=element_text(size=9,color = "black"),
                     plot.title=element_text(size=12,color = "black", face = "bold"),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key.size = unit(0.3, "cm"), 
                     #legend.key = element_rect(fill = "white"), # Set it to transparent
                     legend.spacing.y = unit(0.1, "cm"),  
                     legend.text=element_text(size=8,color = "black"),
                     legend.title=element_blank(),
                     #legend.key.height = unit(0.1, "cm"),
                     #legend.background = element_rect(fill=alpha('blue', 0)),
                     #facets
                     strip.text = element_text(size=10, face = "bold",color = "black", hjust=0),
                     strip.background = element_blank())

# Generate equation expression
equation_text <- substitute(italic(y) == a + b %*% e^(c * italic(x)) * "," ~ italic(R)^2 ~ "=" ~ r2,
                            list(a = round(a_est, 2), 
                                 b = round(b_est, 2), 
                                 c = round(c_est, 2),
                                 r2 = round(r_squared, 2)))

# Sample size
n <- nrow(urch_dat_orig)
sample_size_text <- paste("n =", n)

# Create the plot
g <- ggplot(urch_dat_orig, aes(x = test_diameter_mm, y = animal_24hr_mass_g)) +
  geom_point() +
  geom_line(aes(y = a_est + b_est * exp(c_est * test_diameter_mm)), color = "purple", size = 1) +
  labs(x = "Test Diameter (mm)", y = "Purple sea urchin biomass (g)") +
  theme_bw() + 
  base_theme +
  annotate("text", x = min(urch_dat_orig$test_diameter_mm), y = max(urch_dat_orig$animal_24hr_mass_g), 
           label = as.expression(equation_text), hjust = 0, vjust = 1, size = 5, color = "black") +
  annotate("text", x = min(urch_dat_orig$test_diameter_mm), y = max(urch_dat_orig$animal_24hr_mass_g) - 5, 
           label = sample_size_text, hjust = 0, vjust = 1, size = 5, color = "black")

g

ggsave(g, file = file.path(figdir, "purple_urchin_td_biomass.png"), width = 6.5,
       height = 6.5, units = "in")
