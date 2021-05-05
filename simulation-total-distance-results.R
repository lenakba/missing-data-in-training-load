# script for reading data produced in simulation-total-distance.R
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

#--------------------------------Read data and calculate performance measures on model fits
base_folder = "O:\\Prosjekter\\Bache-Mathiesen-002-missing-data\\Data\\simulations\\"

# vector of chosen missing proportions
# if we ever want to change it or add more proportions, easily done here.
missing_prop_mcar = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
missing_prop_mar = c("light", "medium", "strong")
n_missingvariations = length(missing_prop_mcar) + length(missing_prop_mar)

# output from the for-loops and functions below are saved as "simulation_results_perfparams_td.csv"
# read the csv file to save time, or run all the for-loops and functions again
perf_estimates_all = read_delim("simulation_results_perfparams_td.csv", delim = ";")

#------------no extra variables, missing in total distance only
folder_fits = paste0(base_folder, "td_fits\\")

# reading the simulated results from fits
files_fits = list.files(path = folder_fits)
n_sim = length(files_fits)/n_missingvariations # divide by the number of missing type and level combinations
d_fit_estimates = data.frame()
for(i in 1:n_sim){
  temp_data_mcar = map(missing_prop_mcar, ~readRDS(paste0(folder_fits, i,"_d_td_fits_mcar_",.,".rds"))) %>% bind_rows()
  temp_data_mar = map(missing_prop_mar, ~readRDS(paste0(folder_fits, i,"_d_td_fits_mar_",.,".rds"))) %>% bind_rows()
  d_fit_estimates = rbind(d_fit_estimates, temp_data_mcar, temp_data_mar)
}

#------------position available, missing in total distance only
# folder of fits for the srpe version
folder_fits_pos = paste0(base_folder, "td_fits_pos\\")
files_fits_pos = list.files(path = folder_fits_pos)
n_sim = length(files_fits_pos)/n_missingvariations
d_fit_estimates_pos = data.frame()
for(i in 1:n_sim){
  temp_data_mcar = map(missing_prop_mcar, ~readRDS(paste0(folder_fits_pos, i,"_d_td_fits_mcar_",.,".rds"))) %>% bind_rows()
  temp_data_mar = map(missing_prop_mar, ~readRDS(paste0(folder_fits_pos, i,"_d_td_fits_mar_",.,".rds"))) %>% bind_rows()
  d_fit_estimates_pos = rbind(d_fit_estimates_pos, temp_data_mcar, temp_data_mar)
}

#----------- Both sRPE and player position, missing only in total distance
folder_fits_srpe_pos = paste0(base_folder, "td_fits_srpe_pos\\")
files_fits_srpe_pos = list.files(path = folder_fits_srpe_pos)
n_sim = length(files_fits_srpe_pos)/n_missingvariations 
d_fit_estimates_srpe_pos = data.frame()
for(i in 1:n_sim){
  temp_data_mcar = map(missing_prop_mcar, ~readRDS(paste0(folder_fits_srpe_pos, i,"_d_td_fits_mcar_",.,".rds"))) %>% bind_rows()
  temp_data_mar = map(missing_prop_mar, ~readRDS(paste0(folder_fits_srpe_pos, i,"_d_td_fits_mar_",.,".rds"))) %>% bind_rows()
  d_fit_estimates_srpe_pos = rbind(d_fit_estimates_srpe_pos, temp_data_mcar, temp_data_mar)
}

#-----------No extra variables, missing in all GPS variables
folder_fits_nogps = paste0(base_folder, "td_fits_nogps\\")
files_fits_nogps = list.files(path = folder_fits_nogps)
n_sim = length(files_fits_nogps)/n_missingvariations 
d_fit_estimates_nogps = data.frame()
for(i in 1:n_sim){
  temp_data_mcar = map(missing_prop_mcar, ~readRDS(paste0(folder_fits_nogps, i,"_d_td_fits_mcar_",.,".rds"))) %>% bind_rows()
  temp_data_mar = map(missing_prop_mar, ~readRDS(paste0(folder_fits_nogps, i,"_d_td_fits_mar_",.,".rds"))) %>% bind_rows()
  d_fit_estimates_nogps = rbind(d_fit_estimates_nogps, temp_data_mcar, temp_data_mar)
}

#----------------Player position alone, missing in all GPS variables
folder_fits_nogps_pos = paste0(base_folder, "td_fits_nogps_pos\\")
files_fits_nogps_pos = list.files(path = folder_fits_nogps_pos)
n_sim = length(files_fits_nogps_pos)/n_missingvariations
d_fit_estimates_nogps_pos = data.frame()
for(i in 1:n_sim){
  temp_data_mcar = map(missing_prop_mcar, ~readRDS(paste0(folder_fits_nogps_pos, i,"_d_td_fits_mcar_",.,".rds"))) %>% bind_rows()
  temp_data_mar = map(missing_prop_mar, ~readRDS(paste0(folder_fits_nogps_pos, i,"_d_td_fits_mar_",.,".rds"))) %>% bind_rows()
  d_fit_estimates_nogps_pos = rbind(d_fit_estimates_nogps_pos, temp_data_mcar, temp_data_mar)
}

#----------------Player position and sRPE, missing in all gps variables
folder_fits_nogps_srpe_pos = paste0(base_folder, "td_fits_nogps_srpe_pos\\")
files_fits_nogps_srpe_pos = list.files(path = folder_fits_nogps_srpe_pos)
n_sim = length(files_fits_nogps_srpe_pos)/n_missingvariations
d_fit_estimates_nogps_srpe_pos = data.frame()
for(i in 1:n_sim){
  temp_data_mcar = map(missing_prop_mcar, ~readRDS(paste0(folder_fits_nogps_srpe_pos, i,"_d_td_fits_mcar_",.,".rds"))) %>% bind_rows()
  temp_data_mar = map(missing_prop_mar, ~readRDS(paste0(folder_fits_nogps_srpe_pos, i,"_d_td_fits_mar_",.,".rds"))) %>% bind_rows()
  d_fit_estimates_nogps_srpe_pos = rbind(d_fit_estimates_nogps_srpe_pos, temp_data_mcar, temp_data_mar)
}


# comparing estimates to target estimate, the estimate from fitting a logistic regression
# using target coefficient is ideal
# the real coefficient is: 0.003
target_coef = 0.0003

add_target = function(d_estimates, target){
  d_estimates = d_estimates %>% 
    mutate(target_est = target) %>% 
    filter(term == "gps_td")
  d_estimates
}

d_fit_estimates_td = add_target(d_fit_estimates, target_coef)
d_fit_estimates_td_pos = add_target(d_fit_estimates_pos, target_coef)
d_fit_estimates_td_srpe_pos = add_target(d_fit_estimates_srpe_pos, target_coef)
d_fit_estimates_td_nogps = add_target(d_fit_estimates_nogps, target_coef)
d_fit_estimates_td_nogps_pos = add_target(d_fit_estimates_nogps_pos, target_coef)
d_fit_estimates_td_nogps_srpe_pos = add_target(d_fit_estimates_nogps_srpe_pos, target_coef)

# number of permutations needed for MCSE for bias of 0.5
d_fit_estimates_td %>% 
  filter(rep <= 100) %>% 
  mutate(rb = estimate - target_est) %>% 
  group_by(method, missing_amount) %>% 
  summarise(variance_est = var(rb, na.rm = TRUE), n_sim = (variance_est^2)/0.25)

calc_perf_params = function(d_td, var_extra, var_gps){
  perf_estimates_targetcoef = d_td %>% 
    group_by(method, missing_type, missing_amount) %>% 
    summarise(rb = raw_bias(estimate, target_est),
              pb = percent_bias(estimate, target_est),
              rmse = rmse(estimate, target_est),
              coverage = coverage(CI_low, CI_high, target_est, n()),
              average_width = average_width(CI_low, CI_high),
              power = power(p, n()),
              mcse_bias = mcse_bias(estimate, target_est, n_sim),
              mcse_rmse = mcse_rmse(estimate, target_est, n_sim),
              mcse_coverage = mcse_coverage(CI_low, CI_high, target_est, n(), n_sim)) %>% 
    arrange(missing_type, missing_amount, desc(rmse)) %>% ungroup()
  perf_estimates_targetcoef = perf_estimates_targetcoef %>% 
    mutate(var_extra = var_extra, 
           var_gps = var_gps)
  perf_estimates_targetcoef
}

tot_only = "Total Distance Only"
all_gps = "All GPS variables"
perf_estimates_targetcoef = calc_perf_params(d_fit_estimates_td, "No extra variables", tot_only)
perf_estimates_targetcoef_pos = calc_perf_params(d_fit_estimates_td_pos, "Player Position", tot_only)
perf_estimates_targetcoef_srpe_pos = calc_perf_params(d_fit_estimates_td_srpe_pos, "Player Position and sRPE", tot_only)
perf_estimates_targetcoef_nogps = calc_perf_params(d_fit_estimates_td_nogps, "No extra variables", all_gps)
perf_estimates_targetcoef_nogps_pos = calc_perf_params(d_fit_estimates_td_nogps_pos, "Player Position", all_gps)
perf_estimates_targetcoef_nogps_srpe_pos = calc_perf_params(d_fit_estimates_td_nogps_srpe_pos, "Player Position and sRPE", all_gps)

# combining into 1 dataset
fit_estimates_all = bind_rows(
  d_fit_estimates_td,
  d_fit_estimates_td_pos,
  d_fit_estimates_td_srpe_pos,
  d_fit_estimates_td_nogps,
  d_fit_estimates_td_nogps_pos,
  d_fit_estimates_td_nogps_srpe_pos
)
perf_estimates_all = bind_rows(
  perf_estimates_targetcoef,
  perf_estimates_targetcoef_pos,
  perf_estimates_targetcoef_srpe_pos,
  perf_estimates_targetcoef_nogps,
  perf_estimates_targetcoef_nogps_pos,
  perf_estimates_targetcoef_nogps_srpe_pos
)
# save to csv
write_excel_csv(fit_estimates_all, "simulation_results_fits_td.csv", delim = ";", na = "")
write_excel_csv(perf_estimates_all, "simulation_results_perfparams_td.csv", delim = ";", na = "")
  
tab_mar_td = perf_estimates_all %>% filter(missing_type == "mar")
tab_mar_td_mean = tab_mar_td %>% group_by(var_gps, method)  %>% 
  mutate_if(is.numeric, abs) %>% 
  summarise_if(is.numeric, mean) %>% 
  mutate(pb = round(pb, 1),
         rb = round(rb, 8),
         rmse = round(rmse, 7),
         coverage = round(coverage, 1),
         average_width = round(average_width, 7))

tab_mar_td_noextravar_tdonly = tab_mar_td %>% filter(var_extra == "No extra variables", var_gps == "Total Distance Only") %>% 
  select(-var_extra, -var_gps, -power, -missing_type) %>% 
  arrange(missing_amount, method) %>% 
  mutate(pb = round(pb, 1),
         rb = round(rb, 8),
         rmse = round(rmse, 7),
         coverage = round(coverage, 1),
         average_width = round(average_width, 7))

tab_mcar_td = perf_estimates_all %>% filter(missing_type == "mcar")
tab_mcar_td_mean = tab_mcar_td %>% group_by(var_gps, method)  %>% 
  mutate_if(is.numeric, abs) %>%  
  summarise_if(is.numeric, mean) %>% 
  mutate(pb = round(pb, 1),
         rb = round(rb, 8),
         rmse = round(rmse, 7),
         coverage = round(coverage, 1),
         average_width = round(average_width, 7))

tab_mcar_td_noextravar_tdonly = tab_mcar_td %>% filter(var_extra == "No extra variables", var_gps == "Total Distance Only") %>% 
  select(-var_extra, -var_gps, -power, -missing_type) %>% 
  arrange(missing_amount, method) %>% 
  mutate(pb = round(pb, 1),
         rb = round(rb, 8),
         rmse = round(rmse, 7),
         coverage = round(coverage, 1),
         average_width = round(average_width, 7))

write_excel_csv(tab_mar_td_mean, "tab_mar_td_mean.csv", delim = ";", na = "")
write_excel_csv(tab_mar_td_noextravar_tdonly, "tab_mar_td_noextravar_tdonly.csv", delim = ";", na = "")
write_excel_csv(tab_mcar_td_mean, "tab_mcar_td_mean.csv", delim = ";", na = "")
write_excel_csv(tab_mcar_td_noextravar_tdonly, "tab_mcar_td_noextravar_tdonly.csv", delim = ";", na = "")



#--------------- Figures
d_fig_all = perf_estimates_all %>% 
            filter(method != "No Imputation") %>% 
            select(method, missing_type, missing_amount, pb, rmse, var_extra, var_gps) %>% mutate(pb = pb/100) %>% 
            mutate(method = case_when(method == "Mean Imputation - Mean per player" ~ "Mean per player",
                                      method == "Mean Imputation - Mean per week" ~ "Mean per week",
                                      method == "MI - PMM" ~ "MI - Predicted Mean Matching",
                                      TRUE ~ method))
d_fig_mcar_all = d_fig_all %>% filter(missing_type == "mcar") 
d_fig_mar_all = d_fig_all %>% filter(missing_type == "mar") %>% mutate(missing_amount = case_when(missing_amount == "light" ~ "Light",
                                                                                                  missing_amount == "medium" ~ "Medium",
                                                                                                  missing_amount == "strong" ~ "Strong"))
library(lmisc) # ggplot2 themes
library(ggpubr) # for multiple plots in one thanks to ggarrange()
library(devEMF) # for saving emf files
library(egg) # for denoting figs with A, B, C etc.
text_size = 16

plot_mcar_pb = ggplot(d_fig_mcar_all, aes(x = as.numeric(missing_amount), y = pb, group = method, color = method)) +
  facet_wrap(c("var_gps", "var_extra"), ncol = 3, scales= "free") + 
  geom_hline(yintercept = 0, size = 1, alpha = 0.15) +
  geom_hline(yintercept = 0.05, size = 1, alpha = 0.3, colour = nih_contrast[2]) +
  geom_hline(yintercept = -0.05, size = 1, alpha = 0.3, colour = nih_contrast[2]) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_line(text_size, legend = TRUE) +
  scale_color_manual(values = nih_distinct) +
  scale_y_continuous(labels = axis_percent, breaks = scales::breaks_width(0.1, 0)) +
  ylab("% Bias") + 
  xlab("% Missing under MCAR") + 
  scale_x_continuous(labels = axis_percent, breaks = scales::breaks_width(0.2, 0))  +
  theme(legend.title=element_blank(),
        legend.text=element_text(size=text_size, family = "Trebuchet MS"),
        legend.position = "bottom",
        panel.border = element_blank(), 
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(color = nih_distinct[4]),
        strip.background = element_blank(),
        strip.text.x = element_text(size = text_size+2, family="Trebuchet MS", colour="black", face = "bold"),
        axis.ticks = element_line(color = nih_distinct[4])) +
  coord_cartesian(ylim = c(-0.3, 0.3))

emf("td_pb_mcar.emf", width = 14, height = 10)
tag_facet(plot_mcar_pb, tag_pool = LETTERS)
dev.off()

cairo_pdf("Figure 3.pdf", width = 14, height = 10)
tag_facet(plot_mcar_pb, tag_pool = LETTERS)
dev.off()

plot_mar_pb = ggplot(d_fig_mar_all, aes(x = missing_amount, y = pb, group = method, color = method)) +
  facet_wrap(c("var_gps", "var_extra"), ncol = 3, scales = "free") + 
  geom_hline(yintercept = 0, size = 1, alpha = 0.15) +
  geom_hline(yintercept = 0.05, size = 1, alpha = 0.3, colour = nih_contrast[2]) +
  geom_hline(yintercept = -0.05, size = 1, alpha = 0.3, colour = nih_contrast[2]) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_line(text_size, legend = TRUE) +
  scale_color_manual(values = nih_distinct) +
  ylab("% Bias") + 
  xlab("Missing amount under MAR") + 
  scale_y_continuous(labels = axis_percent, breaks = scales::breaks_width(0.1, 0))  +
  theme(legend.title=element_blank(),
        legend.text=element_text(size=text_size, family = "Trebuchet MS"),
        legend.position = "bottom",
        panel.border = element_blank(), 
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(color = nih_distinct[4]),
        strip.background = element_blank(),
        strip.text.x = element_text(size = text_size+2, family="Trebuchet MS", colour="black", face = "bold"),
        axis.ticks = element_line(color = nih_distinct[4])) +
  coord_cartesian(ylim = c(-0.3, 0.3))

emf("td_pb_mar.emf", width = 14, height = 10)
tag_facet(plot_mar_pb, tag_pool = LETTERS)
dev.off()

cairo_pdf("Figure 4.pdf", width = 14, height = 10)
tag_facet(plot_mar_pb, tag_pool = LETTERS)
dev.off()

#--------------------------------Read data and calculate performance measures on the raw data

# where the imputed datasets are saved
folder_imps_nogps_pos = paste0(base_folder, "td_imps_nogps_pos\\")
n_sim = 1
# we assume it is the same number of simulations for both simulations
# reading the simulated imputation datasets
d_imp_nogps_pos = data.frame()
for(i in 1:n_sim){
  temp_data_mcar = map(missing_prop_mcar, ~readRDS(paste0(folder_imps_nogps_pos, i,"_d_td_imps_mcar_",.,".rds"))) %>% bind_rows()
  temp_data_mar = map(missing_prop_mar, ~readRDS(paste0(folder_imps_nogps_pos, i,"_d_td_imps_mar_",.,".rds"))) %>% bind_rows()
  d_imp_nogps_pos = rbind(d_imp_nogps_pos, temp_data_mcar, temp_data_mar)
}

d_imp_nogps_pos_red = d_imp_nogps_pos %>%
                          mutate(method = case_when(method == "Mean Imputation - Mean per player" ~ "Mean per player",
                          method == "Mean Imputation - Mean per week" ~ "Mean per week",
                          method == "MI - PMM" ~ "MI - Predicted Mean Matching",
                          TRUE ~ method))

# Missing Completely at Random
d_realdata = d_imp_nogps_pos_red %>% filter(missing_type == "mcar", missing_amount == 0.5)
d_cc_target =  d_realdata %>% filter(method == "Mean per player") %>% select(target) %>% rownames_to_column()
d_cc = d_imp_nogps_pos_red %>% filter(method == "Complete Case Analysis", missing_type == "mcar", missing_amount == 0.5) %>% select(-target)
d_cc = d_cc %>% rownames_to_column() %>% full_join(d_cc_target, by = "rowname") %>% fill(method)
d_realdata = d_realdata %>% filter(method != "Complete Case Analysis") %>% bind_rows(., d_cc)
d_imps = d_imp_nogps_pos_red %>% filter(method != "Complete Case Analysis", imp_place == 1, missing_type == "mcar", missing_amount == 0.5)
d_impdata = bind_rows(d_cc, d_imps)

text_size = 18  
plot_mcar = ggplot(d_impdata, aes(x=gps_td, group = dataset_n)) +
  facet_wrap(~method, scales = "free") + 
  geom_density(data = d_realdata, aes(x=target, group = dataset_n), position = "identity", colour = nih_distinct[1], size = 0.8) +
  geom_density(position = "identity", colour = nih_distinct[4], size = 0.6) +
  xlab("Total Distance (M)") +
  ylab("Density") + 
  theme_line(text_size) +
  theme(panel.border = element_blank(), 
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(color = nih_distinct[4]),
        strip.background = element_blank(),
        strip.text.x = element_text(size = text_size+2, family="Trebuchet MS", colour="black", face = "bold"),
        axis.ticks = element_line(color = nih_distinct[4])) 

# Missing at Random
d_realdata_mar = d_imp_nogps_pos_red %>% filter(missing_type == "mar", missing_amount == "strong")
d_cc_target_mar =  d_realdata_mar %>% filter(method == "Mean per player") %>% select(target) %>% rownames_to_column()
d_cc_mar = d_imp_nogps_pos_red %>% filter(method == "Complete Case Analysis", missing_type == "mar", missing_amount == "strong") %>% select(-target)
d_cc_mar = d_cc_mar %>% rownames_to_column() %>% full_join(d_cc_target_mar, by = "rowname") %>% fill(method)
d_realdata_mar = d_realdata_mar %>% filter(method != "Complete Case Analysis") %>% bind_rows(., d_cc_mar)
d_imps_mar = d_imp_nogps_pos_red %>% filter(method != "Complete Case Analysis", imp_place == 1, missing_type == "mar", missing_amount == "strong")
d_impdata_mar = bind_rows(d_cc_mar, d_imps_mar)

plot_mar = ggplot(d_impdata_mar, aes(x=gps_td, group = dataset_n)) +
  facet_wrap(~method, scales = "free") + 
  geom_density(data = d_realdata_mar, aes(x=target, group = dataset_n), position = "identity", colour = nih_distinct[1], size = 0.8) +
  geom_density(position = "identity", colour = nih_distinct[4], size = 0.6) +
  xlab("Total Distance (M)") +
  ylab("Density") + 
  theme_line(text_size) +
  theme(panel.border = element_blank(), 
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(color = nih_distinct[4]),
        strip.background = element_blank(),
        strip.text.x = element_text(size = text_size+2, family="Trebuchet MS", colour="black", face = "bold"),
        axis.ticks = element_line(color = nih_distinct[4])) 

emf("td_imp_vs_real_mar.emf", width = 12, height = 8)
plot_mar
dev.off()

emf("td_imp_vs_real_mcar.emf", width = 12, height = 8)
plot_mcar
dev.off()

#-------------------------------Checking how much multiple imputation affected the performance of PMM

folder_fits = paste0(base_folder, "td_fits_singleimp\\")

# reading the simulated results from fits
files_fits = list.files(path = folder_fits)
n_sim = length(files_fits)/30 # divide by the number of missing type and level combinations
d_fit_estimates = data.frame()
for(i in 1:n_sim){
  temp_data_mcar_noextra = map(missing_prop_mcar, ~readRDS(paste0(folder_fits, i,"_d_td_fits_",.,".rds"))) %>% bind_rows() %>% mutate(data = "noextra")
  d_fit_estimates = rbind(d_fit_estimates, temp_data_mcar_noextra)
}

d_fit_estimates = d_fit_estimates %>% filter(method == "SI - PMM" | method == "MI - PMM")
d_td_term = add_target(d_fit_estimates, target_coef)

d_perf = d_td_term %>% 
  group_by(data, method, missing_amount) %>% 
  summarise(bias = raw_bias(estimate, target_est),
            pb = percent_bias(estimate, target_est),
            mean_se = mean(std.error, na.rm = TRUE)) %>% 
  arrange(missing_amount) %>% ungroup()

key_cols = c("data", "missing_amount", "n_imp")
d_perf = d_perf %>% mutate(n_imp = ifelse(str_detect(d_perf$method, "SI"), "SI", "MI")) %>% 
  arrange(data, method,missing_amount) %>% select(-method)

d_perf_pb = d_perf %>% select(all_of(key_cols), pb) %>% spread(., key = n_imp, value = pb) %>% rename(PB_MI = MI, PB_SI = SI)
d_perf_se = d_perf %>% select(all_of(key_cols), mean_se) %>% spread(., key = n_imp, value = mean_se)  %>% rename(SE_MI = MI, SE_SI = SI)
d_si_v_mi = d_perf_pb %>% left_join(d_perf_se, by = c("data", "missing_amount"))

# calculate the mean for the table
d_si_v_mi %>% summarise(mean_mi = mean(abs(PB_MI)), mean_si = mean(abs(PB_SI)), mean_se_mi = mean(SE_MI), mean_se_si = mean(SE_SI))


# save dataset used for table
d_si_v_mi_rounded = d_si_v_mi %>% mutate(SE_MI = round(SE_MI, 7), 
                                         SE_SI = round(SE_SI, 7),
                                         PB_MI = round(PB_MI, 1),
                                         PB_SI = round(PB_SI, 1))
write_excel_csv(d_si_v_mi_rounded, "td_si_vs_mi.csv", delim = ";", na = "")


