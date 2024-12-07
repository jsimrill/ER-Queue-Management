#Model Implementation
#Jack Simrill
#CS 4632 W01 - Modeling and Simulation

library(simmer)
library(simmer.plot)
library(dplyr)
library(ggplot2)
library(tidyr)

Resource <- setRefClass("Resource", 
                        fields = list(id = "character", availability = "logical"),
                        methods = list(
                          allocate = function() {
                            availability <<- FALSE
                            cat("Resource", id, "allocated.\n")
                          }
                        )
                      )

Doctor <-setRefClass("Doctor", contains = "Resource")

Nurse <- setRefClass("Nurse", contains = "Resource")

Bed <- setRefClass("Bed", contains = "Resource", 
                   fields = list(roomNumber = "character"))

Patient <- setRefClass("Patient",
                       fields = list(arrivalTime = "numeric",
                                     treatmentTime = "numeric",
                                     dischargeTime = "numeric",
                                     priority = "numeric"),  # Priority as numeric
                       methods = list(
                         getTreated = function() {
                           cat("Patient treated for", treatmentTime, "minutes.\n")
                           dischargeTime <<- arrivalTime + treatmentTime
                         }
                       )
)

# Function to Run Simulation for Each Strategy
run_simulation <- function(strategy, arrival_rate, num_patients) {
  # Reset Environment
  env <- simmer("ER_Simulation")
  
  env %>%
    add_resource("doctor", n_doctors) %>%
    add_resource("nurse", n_nurses) %>%
    add_resource("bed", n_beds)
  
  for (i in 1:num_patients) {
    # Assign random priority for priority queue strategy
    priority <- ifelse(strategy == "Priority Queue", sample(1:3, 1), NA_real_)
    
    # Generate random treatment time
    treatment_time <- runif(1, min = 15, max = 45) # minutes
    
    # Create patient trajectory with priority-based scheduling if applicable
    traj <- patient_trajectory(env, priority, treatment_time)
    
    if (strategy == "Priority Queue") {
      env %>% add_generator(paste0("patient_", i), traj, at(i * arrival_rate), priority = priority)
    } else {
      env %>% add_generator(paste0("patient_", i), traj, at(i * arrival_rate))
    }
  }
  
  # Run Simulation
  env %>% run(until = 15000)
  
  # Return Results
  results <- get_mon_arrivals(env) %>%
    mutate(wait_time = end_time - start_time - activity_time,
           treatment_time = activity_time)  # Capture treatment time explicitly
  
  # Resource usage data
  resource_usage <- get_mon_resources(env) %>%
    filter(resource %in% c("doctor", "nurse", "bed")) %>%
    group_by(resource) %>%
    summarise(avg_usage = mean(server))
  
  list(results = results, resource_usage = resource_usage)
}

# Run Simulations for Each Strategy
results_fifo <- run_simulation("FIFO", 25, 100)
results_priority <- run_simulation("Priority Queue", 25, 100)
results_staffing <- run_simulation("Staffing Increase", 25, 100)

# Combine results
compare_results <- rbind(
  results_fifo$results %>% mutate(strategy = "FIFO"),
  results_priority$results %>% mutate(strategy = "Priority Queue"),
  results_staffing$results %>% mutate(strategy = "Staffing Increase")
)

resource_utilization <- rbind(
  results_fifo$resource_usage %>% mutate(strategy = "FIFO"),
  results_priority$resource_usage %>% mutate(strategy = "Priority Queue"),
  results_staffing$resource_usage %>% mutate(strategy = "Staffing Increase")
)

# Plot 1: Boxplot of Patient Wait Times by Strategy
ggplot(compare_results, aes(x = strategy, y = wait_time, fill = strategy)) +
  geom_boxplot() +
  labs(title = "Patient Wait Times by Strategy",
       x = "Queue Management Strategy",
       y = "Wait Time (minutes)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# Plot 2: Density Plot of Treatment Times by Strategy
ggplot(compare_results, aes(x = treatment_time, fill = strategy)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Treatment Times by Strategy",
       x = "Treatment Time (minutes)",
       y = "Density") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

# Plot 3: Resource Utilization by Strategy
ggplot(resource_utilization, aes(x = resource, y = avg_usage, fill = strategy)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Resource Utilization by Strategy",
       x = "Resource",
       y = "Average Usage") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# Plot 4: Histogram of Arrival and Discharge Times to observe bottlenecks
arrival_discharge <- compare_results %>%
  select(strategy, start_time, end_time) %>%
  pivot_longer(cols = c(start_time, end_time), names_to = "event", values_to = "time")

ggplot(arrival_discharge, aes(x = time, fill = event)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
  facet_wrap(~strategy) +
  labs(title = "Distribution of Arrival and Discharge Times by Strategy",
       x = "Time",
       y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = c("start_time" = "#56B4E9", "end_time" = "#E69F00"))
  