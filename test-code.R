# Library loads ----
library(tidyverse)
library(sf)
library(RColorBrewer)
library(readxl)
library(R2jags)

# Data ----
# . Read in the data ----
# Sampling data
plants2024 <- read_xlsx("data/2024 Survey Data.xlsx", sheet = "master sheet")
plants2024$Lon <- as.numeric(plants2024$Lon)
plants2024$Lat <- as.numeric(plants2024$Lat)

# Species codes
codes2024 <- read_xlsx("data/2024 Survey Data.xlsx", sheet = "species codes")

# . Data manipulation ----
# Stack data into long form
long_data <- pivot_longer(data = plants2024,
                          cols = c(8:ncol(plants2024)),
                          names_to = "species",
                          values_to = "observed")

# Merge species codes with long_data
plants2 = merge(x = long_data, y = codes2024, by = "species")

# Combine genus and species
plants2$name <- paste(plants2$genus, plants2$epithet, sep = " ")


# . Data formatting ----
# Select a lake for testing
hortonia <- plants2 %>% 
  filter(Waterbody == "HOR")

# Replace '1's with other categories for simulation
resample <- function(x){
  x <- sample(c(1, 2, 3, 4), length(x), replace = TRUE)
  return(x)
}

hortonia$observed[hortonia$observed > 0 ] <- resample(hortonia$observed[hortonia$observed > 0 ])

# hortonia <- hortonia[1:1000, ]

# Make a capture history
plant_array <- reshape::cast(data = hortonia, 
                             formula =  name ~ Waterbody ~ Point ~ Toss,
                             value = "observed",
                             drop = FALSE,
                             fill = NA, 
                             add.missing = TRUE)

z_init_array <- reshape::cast(data = hortonia, 
                              formula =  name ~ Waterbody ~ Point,
                              value = "observed",
                              fun.aggregate = max,
                              drop = FALSE,
                              fill = NA, 
                              add.missing = TRUE)


# JAGS Data ----
# . Capture matrix ----
y <- plant_array + 1

# Initial values for occupancy state
z_init <- apply(y, c(1, 2, 3), max)
z_init[z_init == 1] <- 2

p_init <- array(0, 
                dim = c(4, # Number of observable states/categories
                        length(unique(hortonia$name)), 
                        length(unique(hortonia$Waterbody)), 
                        length(unique(hortonia$Point))))

# Package the data for jags
jags_data <- list(
  y = y, # Capture matrix
  n_species = length(unique(hortonia$name)),
  n_lakes = length(unique(hortonia$Waterbody)),
  n_sites = length(unique(hortonia$Point)),
  n_reps = length(unique(hortonia$Toss))
)

# . Initial values ----
# Function for initial values
inits <- function(){
  list(
    z = z_init
  )
}

# . Parameters to save ----
params <- c("psi", "z") # We might actually want logit scale instead

# Run the model ---- 
jags_fit <- jags(data = jags_data, 
                 inits = inits, 
                 parameters.to.save = params,
                 model.file = "categorical_ijt_constrained.jags",
                 n.chains = 3, 
                 n.iter = 500,
                 n.burnin = 250,
                 n.thin = 3)

print(jags_fit)

# Results ----
posts <- jags_fit$BUGSoutput$sims.list

save(jags_fit, file = "we_cooking_chat.rda")

# . Occupancy state ----
z_test <- posts$z

z_ests <- reshape::melt(z_test)
names(z_ests) <- c("iteration", "species", "Waterbody", "Point",
                     "estimate")

z_summary <- z_ests %>% 
  group_by(species, Point) %>% 
  summarize(fit = mean(estimate, na.rm = TRUE),
            lwr = quantile(estimate, 0.025, na.rm = TRUE),
            upr = quantile(estimate, 0.975, na.rm = TRUE),
            Q1 = quantile(estimate, 0.25, na.rm = TRUE),
            Q3 = quantile(estimate, 0.75, na.rm = TRUE))

z_summary$species <- sort(unique(hortonia$species))[z_summary$species]

plotter <- merge(hortonia, z_summary, by = c("species", "Point"))

myPalette <- colorRampPalette(rev(brewer.pal(5, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(5), limits=c(1, 5))

plotter %>% 
  filter(name == "Chara sp") %>% 
  ggplot(aes(x = Lon, y = Lat, color = fit)) +
  geom_point(size = 3) +
  coord_sf() +
  xlab("Longitude (Decimal degrees)") +
  ylab("Lattitude (Decimal degrees)") +
  labs(color = "Abundance") +
  sc
# We cooking

