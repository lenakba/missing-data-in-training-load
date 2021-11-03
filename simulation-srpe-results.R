
# script for reading data produced in simulation-srpe.R
# and calculating performance measures

# so we don't have to deal with scientific notations
# and strings aren't automatically read as factors
options(scipen = 17, 
        stringsAsFactors = FALSE)

# loading packages
library(tidyverse) # for datawrangling
library(mice) # multiple imputation package

# fetching functions for performance parameters
# we assume working directory is the same location as this script
source("performance-measure-functions.R", encoding = "UTF-8")

# output from the for-loops and functions below are saved as "simulation_results_perfparams_srpe.csv"
# read the csv file to save time, or run all the for-loops and functions again
perf_estimates_srpe = read_delim("simulation_results_perfparams_srpe.csv", delim = ";") %>% mutate(var_extra = "No extra variables")

#--------------------------------Read data and calculate performance measures on model fits

base_folder = "my\\data\\folder\\"
folder_fits = paste0(base_folder, "srpe_fits\\")

# vector of chosen missing proportions
# if we ever want to change it or add more proportions, easily done here.
missing_prop_mcar = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
missing_prop_mar = c("light", "medium", "strong")

# reading the simulated results from fits
files_fits = list.files(path = folder_fits)
n_sim = length(files_fits)/(length(missing_prop_mcar) + length(missing_prop_mar)) # divide by the number of missing type and level combinations
d_fit_estimates = data.frame()
for(i in 1:n_sim){
  temp_data_mcar = map(missing_prop_mcar, ~readRDS(paste0(folder_fits, i,"_d_srpe_fits_mcar_",.,".rds"))) %>% bind_rows()
  temp_data_mar = map(missing_prop_mar, ~readRDS(paste0(folder_fits, i,"_d_srpe_fits_mar_",.,".rds"))) %>% bind_rows()
  d_fit_estimates = rbind(d_fit_estimates, temp_data_mcar, temp_data_mar)
}

# or, simply read the data
d_fit_estimates = read_delim("simulation_results_fits_srpe.csv", delim = ";")

# comparing estimates to target estimate, the estimate from fitting a logistic regression
# using target coefficient is ideal
# the real coefficient is: 0.003
target_coef = 0.003
d_fit_estimates_srpe = d_fit_estimates %>% 
  mutate(target_est = target_coef) %>% 
  filter(term == "srpe")

perf_estimates_targetcoef = d_fit_estimates_srpe %>% 
  group_by(method, missing_type, missing_amount) %>% 
  summarise(rb = raw_bias(estimate, target_est),
            pb = abs_percent_bias(estimate, target_est),
            rmse = rmse(estimate, target_est),
            coverage = coverage(CI_low, CI_high, target_est, n()),
            average_width = average_width(CI_low, CI_high),
            power = power(p, n()),
            mcse_rmse = mcse_rmse(estimate, target_est, n_sim),
            mcse_coverage = mcse_coverage(CI_low, CI_high, target_est, n(), n_sim)) %>% 
  arrange(missing_type, missing_amount, desc(rmse)) %>% ungroup()

# save to csv
# save results
# write_delim is preferable, but write_excel_csv is required for excel to understand
# that the file encoding is UTF-8
write_excel_csv(d_fit_estimates_srpe, "simulation_results_fits_srpe.csv", delim = ";", na = "")
write_excel_csv(perf_estimates_targetcoef, "simulation_results_perfparams_srpe.csv", delim = ";", na = "")

tab_mar_srpe_full = perf_estimates_targetcoef %>% filter(missing_type == "mar", method != "No Imputation")
tab_mar_srpe_mean = tab_mar_srpe_full %>% group_by(method) %>% 
  mutate_if(is.numeric, abs) %>% 
  summarise_if(is.numeric, mean) %>% 
  mutate(pb = round(pb, 1),
         rb = round(rb, 8),
         rmse = round(rmse, 7),
         coverage = round(coverage, 1),
         average_width = round(average_width, 7))

tab_mar_srpe = tab_mar_srpe_full %>% 
  select(-power, -missing_type) %>% 
  arrange(missing_amount, method) %>% 
  mutate(pb = round(pb, 1),
         rb = round(rb, 8),
         rmse = round(rmse, 7),
         coverage = round(coverage, 1),
         average_width = round(average_width, 7))

tab_mcar_srpe_full = perf_estimates_srpe %>% filter(missing_type == "mcar", method != "No Imputation")
tab_mcar_srpe_mean = tab_mcar_srpe_full %>% group_by(method) %>% 
  mutate_if(is.numeric, abs) %>% 
  summarise_if(is.numeric, mean) %>% 
  mutate(pb = round(pb, 1),
         rb = round(rb, 8),
         rmse = round(rmse, 7),
         coverage = round(coverage, 1),
         average_width = round(average_width, 7))

tab_mcar_srpe = tab_mcar_srpe_full %>% 
  select(-power, -missing_type) %>% 
  arrange(missing_amount, method) %>% 
  mutate(pb = round(pb, 1),
         rb = round(rb, 8),
         rmse = round(rmse, 7),
         coverage = round(coverage, 1),
         average_width = round(average_width, 7))

write_excel_csv(tab_mar_srpe_mean, "tab_mar_srpe_mean.csv", delim = ";", na = "")
write_excel_csv(tab_mar_srpe, "tab_mar_srpe.csv", delim = ";", na = "")
write_excel_csv(tab_mcar_srpe_mean, "tab_mcar_srpe_mean.csv", delim = ";", na = "")
write_excel_csv(tab_mcar_srpe, "tab_mcar_srpe.csv", delim = ";", na = "")

#------------------- figures
d_fig = perf_estimates_targetcoef %>% 
  select(method, missing_type, missing_amount, pb, rmse) %>% 
  mutate(pb = pb/100)  %>% 
  filter(method != "No Imputation") %>% 
  mutate(method = case_when(method == "Mean Imputation - Mean per player" ~ "Mean per player",
                            method == "Mean Imputation - Mean per week" ~ "Mean per week",
                            method == "MI - PMM" ~ "MI - Predicted Mean Matching",
                            TRUE ~ method))

d_fig_mcar = d_fig %>% filter(missing_type == "mcar")
d_fig_mar = d_fig %>% filter(missing_type == "mar") %>% 
            mutate(missing_amount = case_when(missing_amount == "light" ~ "Light",
                   missing_amount == "medium" ~ "Medium",
                   missing_amount == "strong" ~ "Strong"))

library(ggpubr) # for multiple plots in one thanks to ggarrange()
library(devEMF) # for saving emf files
text_size = 20
plot_mcar_pb = ggplot(d_fig_mcar, aes(x = as.numeric(missing_amount), y = pb, group = method, color = method)) +
  geom_hline(yintercept = 0.05, size = 1, alpha = 0.3) +
  geom_line(size = 1) +
  geom_point(size = 2)+
  scale_y_continuous(breaks = scales::breaks_width(0.05, 0)) +
  ylab("% Bias") + 
  xlab("% Missing under MCAR") + 
  scale_x_continuous(breaks = scales::breaks_width(0.1, 0))  +
  theme(legend.title=element_blank(),
        panel.border = element_blank(), 
        panel.background = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank()) +
  coord_cartesian(ylim=c(0, 0.3))

emf("srpe_mcar.emf", width = 12, height = 6)
plot_mcar_pb
dev.off()

cairo_pdf("Figure 1 Colour Image.pdf", width = 12, height = 6)
plot_mcar_pb
dev.off()

plot_mar_pb = ggplot(d_fig_mar, aes(x = missing_amount, y = pb, group = method, color = method)) +
  geom_hline(yintercept = 0.05, size = 1, alpha = 0.3) +
  geom_line(size = 1) +
  geom_point(size = 2)+
  scale_y_continuous(breaks = scales::breaks_width(0.05, 0)) +
  ylab("% Bias") + 
  xlab("Missing amount under MAR") + 
  theme(legend.title=element_blank(),
        legend.text=element_text(size=text_size),
        panel.border = element_blank(), 
        panel.background = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank()) +
  coord_cartesian(ylim=c(0, 0.3))

emf("srpe_mar.emf", width = 12, height = 6)
plot_mar_pb
dev.off()

cairo_pdf("Figure 2 Colour Image.pdf", width = 12, height = 7)
plot_mar_pb
dev.off()



#--------------------------------Checking if having the player position changes anything
# the code below can be skipped by reading in already combined data
perf_estimates_srpe = read_delim("simulation_results_perfparams_srpe.csv", delim = ";") %>% mutate(var_extra = "Player position not available")
perf_estimates_srpe_pos = read_delim("simulation_results_perfparams_srpe_pos.csv", delim = ";") %>% mutate(var_extra = "Player position available")
perf_estimates = bind_rows(perf_estimates_srpe, perf_estimates_srpe_pos)

#--------------------fetching results from when position is among the variables in the imputation model

folder_fits_pos = paste0(base_folder, "srpe_fits_pos\\")

# reading the simulated results from fits
files_fits_pos = list.files(path = folder_fits_pos)
n_sim = length(files_fits_pos)/(length(missing_prop_mcar) + length(missing_prop_mar)) # divide by the number of missing type and level combinations
d_fit_estimates_pos = data.frame()
for(i in 1:n_sim){
  temp_data_mcar = map(missing_prop_mcar, ~readRDS(paste0(folder_fits_pos, i,"_d_srpe_fits_mcar_",.,".rds"))) %>% bind_rows()
  temp_data_mar = map(missing_prop_mar, ~readRDS(paste0(folder_fits_pos, i,"_d_srpe_fits_mar_",.,".rds"))) %>% bind_rows()
  d_fit_estimates_pos = rbind(d_fit_estimates_pos, temp_data_mcar, temp_data_mar)
}

# or, simply read the data
d_fit_estimates = read_delim("simulation_results_fits_srpe_pos.csv", delim = ";")

# comparing estimates to target estimate, the estimate from fitting a logistic regression
# using target coefficient is ideal
# the real coefficient is: 0.003
target_coef = 0.003
d_fit_estimates_srpe_pos = d_fit_estimates_pos %>% 
  mutate(target_est = target_coef) %>% 
  filter(term == "srpe")

perf_estimates_targetcoef_pos = d_fit_estimates_srpe_pos %>% 
  group_by(method, missing_type, missing_amount) %>% 
  summarise(rb = raw_bias(estimate, target_est),
            pb = abs_percent_bias(estimate, target_est),
            rmse = rmse(estimate, target_est),
            coverage = coverage(CI_low, CI_high, target_est, n()),
            average_width = average_width(CI_low, CI_high),
            power = power(p, n()),
            mcse_rmse = mcse_rmse(estimate, target_est, n_sim),
            mcse_coverage = mcse_coverage(CI_low, CI_high, target_est, n(), n_sim)) %>% 
  arrange(missing_type, missing_amount, desc(rmse)) %>% ungroup()

# save to csv
# save results
# write_delim is preferable, but write_excel_csv is required for excel to understand
# that the file encoding is UTF-8
write_excel_csv(perf_estimates_targetcoef_pos, "simulation_results_perfparams_srpe_pos.csv", delim = ";", na = "")
write_excel_csv(d_fit_estimates_pos, "simulation_results_fits_srpe_pos.csv", delim = ";", na = "")

#--------------- Figures
d_fig_all = perf_estimates %>% 
  filter(method != "No Imputation") %>% 
  select(method, missing_type, missing_amount, pb, rmse, var_extra) %>% mutate(pb = pb/100) %>% 
  mutate(method = case_when(method == "Mean Imputation - Mean per player" ~ "Mean per player",
                            method == "Mean Imputation - Mean per week" ~ "Mean per week",
                            method == "MI - PMM" ~ "MI - Predicted Mean Matching",
                            TRUE ~ method))
d_fig_mcar_all = d_fig_all %>% filter(missing_type == "mcar") 
d_fig_mar_all = d_fig_all %>% filter(missing_type == "mar") %>% mutate(missing_amount = case_when(missing_amount == "light" ~ "Light",
                                                                                                  missing_amount == "medium" ~ "Medium",
                                                                                                  missing_amount == "strong" ~ "Strong"))

library(ggpubr) # for multiple plots in one thanks to ggarrange()
library(devEMF) # for saving emf files
text_size = 16
plot_mcar_pb = ggplot(d_fig_mcar_all, aes(x = as.numeric(missing_amount), y = pb, group = method, color = method)) +
  facet_wrap(~var_extra) + 
  geom_hline(yintercept = 0.05, size = 1, alpha = 0.3) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(breaks = scales::breaks_width(0.05, 0)) +
  ylab("% Bias") + 
  xlab("% Missing under MCAR") + 
  scale_x_continuous(breaks = scales::breaks_width(0.2, 0))  +
  theme(legend.title=element_blank(),
        legend.text=element_text(size=text_size),
        panel.border = element_blank(), 
        panel.background = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = text_size+2, colour="black", face = "bold")) +
  coord_cartesian(ylim=c(0, 0.3))

emf("srpe_pb_mcar_pos_vs_nopos.emf", width = 12, height = 4)
plot_mcar_pb
dev.off()

plot_mar_pb = ggplot(d_fig_mar_all, aes(x = missing_amount, y = pb, group = method, color = method)) +
  facet_wrap(~var_extra) + 
  geom_hline(yintercept = 0.05, size = 1, alpha = 0.3) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  ylab("% Bias") + 
  xlab("Missing amount under MAR") + 
  scale_y_continuous(breaks = scales::breaks_width(0.05, 0))  +
  theme(legend.title=element_blank(),
        legend.text=element_text(size=text_size),
        panel.border = element_blank(), 
        panel.background = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = text_size+2, colour="black", face = "bold")) +
  coord_cartesian(ylim=c(0, 0.3))


emf("srpe_pb_mar_pos_vs_nopos.emf", width = 12, height = 4)
plot_mar_pb
dev.off()


emf("srpe_pb_mcar_mar_pos_vs_nopos.emf", width = 12, height = 8)
ggarrange(plot_mcar_pb, plot_mar_pb, ncol = 1, labels = "AUTO")
dev.off()


#--------------------------------Compare real vs. imputed data

# where the imputed datasets are saved
folder_imps = paste0(base_folder, "srpe_imps\\")
n_sim = 1
# we assume it is the same number of simulations for both simulations
# reading the simulated imputation datasets
files_imps = list.files(path = folder_imps)
d_imp = data.frame()
for(i in 1:n_sim){
  temp_data_mcar = map(missing_prop_mcar, ~readRDS(paste0(folder_imps, i,"_d_srpe_imps_mcar_",.,".rds"))) %>% bind_rows()
  temp_data_mar = map(missing_prop_mar, ~readRDS(paste0(folder_imps, i,"_d_srpe_imps_mar_",.,".rds"))) %>% bind_rows()
  d_imp = rbind(d_imp, temp_data_mcar, temp_data_mar)
}

d_imp = d_imp %>% mutate(index = 1:n())
ting = d_imp %>% filter((method == "MI - PMM" | method == "Regression Imputation") & missing_type == "mar") 
d_imp_mar = d_imp_mar %>% mutate(missing_type = "mar")
d_imp = bind_rows(d_imp %>% filter(!index %in% ting$index) %>% select(-index), d_imp_mar) 

d_imp = d_imp %>%
  mutate(method = case_when(method == "Mean Imputation - Mean per player" ~ "Mean per player",
                            method == "Mean Imputation - Mean per week" ~ "Mean per week",
                            method == "MI - PMM" ~ "MI - Predicted Mean Matching",
                            TRUE ~ method))

# Missing Completely at Random
d_realdata = d_imp %>% filter(missing_type == "mcar", missing_amount == 0.5)
d_cc_target =  d_realdata %>% filter(method == "Mean per player") %>% select(target) %>% rownames_to_column()
d_cc = d_imp %>% filter(method == "Complete Case Analysis", missing_type == "mcar", missing_amount == 0.5) %>% select(-target)
d_cc = d_cc %>% rownames_to_column() %>% full_join(d_cc_target, by = "rowname") %>% fill(method)
d_realdata = d_realdata %>% filter(method != "Complete Case Analysis") %>% bind_rows(., d_cc)
d_imps = d_imp %>% filter(method != "Complete Case Analysis", imp_place == 1, missing_type == "mcar", missing_amount == 0.5)
d_impdata = bind_rows(d_cc, d_imps)

text_size = 18  
plot_mcar = ggplot(d_impdata, aes(x=srpe, group = dataset_n)) +
  facet_wrap(~method, scales = "free") + 
  geom_density(data = d_realdata, aes(x=target, group = dataset_n), position = "identity", size = 0.8) +
  geom_density(position = "identity", size = 0.6) +
  xlab("sRPE (AU)") +
  ylab("Density") + 
  theme(panel.border = element_blank(), 
        panel.background = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = text_size + 2, colour="black", face = "bold")) +
  coord_cartesian(xlim=c(NA, 1500), ylim = c(NA, 0.005))

# Missing at Random

folder_imps = paste0(base_folder, "srpe_imps_mar\\")
n_sim = 1
# we assume it is the same number of simulations for both simulations
# reading the simulated imputation datasets
files_imps = list.files(path = folder_imps)
d_imp = data.frame()
for(i in 1:n_sim){
  temp_data_mar = map(missing_prop_mar, ~readRDS(paste0(folder_imps, i,"_d_srpe_fits_",.,".rds"))) %>% bind_rows()
  d_imp_mar = rbind(d_imp, temp_data_mar)
}

d_imp_mar = d_imp_mar %>%
  mutate(method = case_when(method == "Mean Imputation - Mean per player" ~ "Mean per player",
                            method == "Mean Imputation - Mean per week" ~ "Mean per week",
                            method == "MI - PMM" ~ "MI - Predicted Mean Matching",
                            TRUE ~ method))

d_realdata_mar = d_imp_mar %>% filter(missing_type == "mar", missing_amount == "strong")
d_cc_target_mar =  d_realdata_mar %>% filter(method == "Mean per player") %>% select(target) %>% rownames_to_column()
d_cc_mar = d_imp_mar %>% filter(method == "Complete Case Analysis", missing_type == "mar", missing_amount == "strong") %>% select(-target)
d_cc_mar = d_cc_mar %>% rownames_to_column() %>% full_join(d_cc_target_mar, by = "rowname") %>% fill(method)
d_realdata_mar = d_realdata_mar %>% filter(method != "Complete Case Analysis") %>% bind_rows(., d_cc_mar)
d_imps_mar = d_imp %>% filter(method != "Complete Case Analysis", imp_place == 1, missing_type == "mar", missing_amount == "strong")
d_impdata_mar = bind_rows(d_cc_mar, d_imps_mar)

plot_mar = ggplot(d_impdata_mar, aes(x=srpe, group = dataset_n)) +
  facet_wrap(~method, scales = "free") + 
  geom_density(data = d_realdata_mar, aes(x=target, group = dataset_n), position = "identity", size = 0.8) +
  geom_density(position = "identity", size = 0.6) +
  xlab("sRPE (AU)") +
  ylab("Density") + 
  theme_line(text_size) +
  theme(panel.border = element_blank(), 
        panel.background = element_blank(),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = text_size+2, colour="black", face = "bold"),
        axis.ticks = element_line(color = nih_distinct[4])) +
  coord_cartesian(xlim=c(NA, 1500), ylim = c(NA, 0.005))

library(devEMF)
emf("srpe_imp_vs_real_mar.emf", width = 12, height = 8)
plot_mar
dev.off()

emf("srpe_imp_vs_real_mcar.emf", width = 12, height = 8)
plot_mcar
dev.off()
