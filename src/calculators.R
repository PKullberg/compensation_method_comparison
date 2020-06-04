## Functions for aggregating condition indices
## The multuiplicative metric is  based on the method and parameters that are described in the "Elinympäristöjen tilan edistäminen suomessa" report (Kotiahoh et al eds. 2015, https://helda.helsinki.fi/bitstream/handle/10138/156982/SY_8_2015.pdf) ##
## Author: Peter Kullberg, peter.kullberg@ymparisto.fi

# libraries
library(dplyr)

## Create lists of reference values and weights for the three forested ecosystem groups mentioned in the Kotiaho et al (2015)
# the ecoregion names refer to: 
# kangas = Lehtomaiset, tuoreet ja kuivahkot Kangasmetsät; lehdot = lehdot; karukot = kuivat- ja karukkokankaat
# dead_wood = lahopuu (m3/ha), large_trunks = järeä puu (kpl/ha), broad_leaved = lehtipuu (m3/ha) and burnt_area = palanut ala (ha/v)
# Additional habitat Kangas_new divides dead wood in three classes

reference_variables <- list(
  kangas = c(dead_wood = 80, large_trunks = 20, broad_leaved = 50),
  lehto = c(dead_wood = 100, large_trunks = 30, broad_leaved = 100),
  karukko = c(dead_wood = 40, large_trunks = 10, burnt_area = 5000),
  Kangas_new = c(dead_wood_1 = 26, dead_wood_2 = 26, dead_wood_3 = 26, large_trunks = 20, broad_leaved = 50)
)

feature_weights <- list(
  kangas = c(dead_wood = 0.6, large_trunks = 0.4, broad_leaved = 0.4),
  lehto = c(dead_wood = 0.4, large_trunks = 0.4, broad_leaved = 0.6),
  karukko = c(dead_wood = 0.4, large_trunks = 0.4, burnt_area = 0.5),
  Kangas_new = c(dead_wood_1 = 0.26, dead_wood_2 = 0.26, dead_wood_3 = 0.26, large_trunks = 0.40, broad_leaved = 0.40)
)

# Function for calcualting the condition index
# Inputs: 
# cur_values = the measured values in the area of interest as a named list, names must match the names in the reference variable list
# habitat = name of the habitat as given in the variable and weight lists
# reference_variable_list = named list of named vectors of reference variables
# weight_variable_list = named list of named vectors of weights for each variable
# exceed_warnings = warn if some value exceeds the reference value

ELITE_value_scaled <- function(cur_values, habitat, reference_variable_list = reference_variables, weight_list = feature_weights, exceed_warnings = T) {
  
  stopifnot(habitat %in% names(reference_variable_list))
  
  # read right reference values and weights
  ref_values <- reference_variable_list[[habitat]]
  weights <- weight_list[[habitat]]
  
  # if inputs exceed reference values replace them with reference
  if(exceed_warnings) if(any(cur_values > ref_values)) warning("One or more input values exceed the reference value. Replacing exceeding values with reference value.\n")
  cur_values[cur_values > ref_values] <- ref_values[cur_values > ref_values]
  
  scaler <- prod(1 - weights)
  
  # compute the scaled ELITE value
  (prod(1 - weights * (1 - cur_values / ref_values)) - scaler) / (1 - scaler)

}

# mean method (aka Habitat Hectares)
mean_value <- function(cur_values, habitat, reference_variable_list = reference_variables, weight_list = feature_weights) {
  ref_values <- reference_variable_list[[habitat]]
  weights <- weight_list[[habitat]]
  
  adj_weights <- weights / sum(weights, na.rm = T)
  
  sum((cur_values / ref_values) * adj_weights)
  
}