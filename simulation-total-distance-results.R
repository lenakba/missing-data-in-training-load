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
    filter(term == "td", method != "No Imputation")
  d_estimates
}

d_fit_estimates_td = add_target(d_fit_estimates, target_coef)
# renamed the term for the last simulation
d_fit_estimates_pos = d_fit_estimates_pos %>% mutate(term = ifelse(term == "gps_td", "td", term))
d_fit_estimates_td_pos = add_target(d_fit_estimates_pos, target_coef)
d_fit_estimates_td_srpe_pos = add_target(d_fit_estimates_srpe_pos, target_coef)
d_fit_estimates_td_nogps = add_target(d_fit_estimates_nogps, target_coef)
d_fit_estimates_nogps_pos = d_fit_estimates_nogps_pos %>% mutate(term = ifelse(term == "gps_td", "td", term))
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
all_gps = "All GPS"
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

# save time by reading in the saved data


#--------------- Figures
d_fig_all = perf_estimates_all %>% 
            select(method, missing_type, missing_amount, pb, rmse, var_extra, var_gps) %>% mutate(pb = pb/100) %>% 
            mutate(method = case_when(method == "Mean Imputation - Mean per player" ~ "Mean per player",
                                      method == "Mean Imputation - Mean per week" ~ "Mean per week",
                                      TRUE ~ method))
d_fig_mcar_all = d_fig_all %>% filter(missing_type == "mcar") 
d_fig_mar_all = d_fig_all %>% filter(missing_type == "mar") %>% mutate(missing_amount = case_when(missing_amount == "light" ~ "Light",
                                                                                                  missing_amount == "medium" ~ "Medium",
                                                                                                  missing_amount == "strong" ~ "Strong"))
library(lmisc) # ggplot2 themes
library(ggpubr) # for multiple plots in one thanks to ggarrange()
library(devEMF) # for saving emf files
text_size = 16

plot_mcar_pb = ggplot(d_fig_mcar_all, aes(x = as.numeric(missing_amount), y = pb, group = method, color = method)) +
  facet_wrap(c("var_extra", "var_gps"), ncol = 2, scales= "free") + 
  geom_hline(yintercept = 0, size = 1, alpha = 0.15) +
  geom_hline(yintercept = 0.05, size = 1, alpha = 0.3, colour = nih_contrast[2]) +
  geom_hline(yintercept = -0.05, size = 1, alpha = 0.3, colour = nih_contrast[2]) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_line(text_size, legend = TRUE) +
  scale_color_manual(values = nih_distinct) +
  scale_y_continuous(labels = axis_percent, breaks = scales::breaks_width(0.2, 0)) +
  ylab("% Bias") + 
  xlab("% Missing under MCAR") + 
  scale_x_continuous(labels = axis_percent, breaks = scales::breaks_width(0.2, 0))  +
  theme(legend.title=element_blank(),
        legend.text=element_text(size=text_size, family = "Trebuchet MS"),
        panel.border = element_blank(), 
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(color = nih_distinct[4]),
        strip.background = element_blank(),
        strip.text.x = element_text(size = text_size+2, family="Trebuchet MS", colour="black", face = "bold"),
        axis.ticks = element_line(color = nih_distinct[4])) +
  coord_cartesian(ylim = c(-0.4, 0.25))


emf("td_pb_mcar.emf", width = 12, height = 12)
plot_mcar_pb
dev.off()

cairo_pdf("Figure 3 Colour Image.pdf", width = 12, height = 12)
plot_mcar_pb
dev.off()

plot_mar_pb = ggplot(d_fig_mar_all, aes(x = missing_amount, y = pb, group = method, color = method)) +
  facet_wrap(c("var_extra", "var_gps"), ncol = 2, scales = "free") + 
  geom_hline(yintercept = 0, size = 1, alpha = 0.15) +
  geom_hline(yintercept = 0.05, size = 1, alpha = 0.3, colour = nih_contrast[2]) +
  geom_hline(yintercept = -0.05, size = 1, alpha = 0.3, colour = nih_contrast[2]) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_line(text_size, legend = TRUE) +
  scale_color_manual(values = nih_distinct) +
  ylab("% Bias") + 
  xlab("Missing amount under MAR") + 
  scale_y_continuous(labels = axis_percent, breaks = scales::breaks_width(0.2, 0))  +
  theme(legend.title=element_blank(),
        legend.text=element_text(size=text_size, family = "Trebuchet MS"),
        panel.border = element_blank(), 
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(color = nih_distinct[4]),
        strip.background = element_blank(),
        strip.text.x = element_text(size = text_size+2, family="Trebuchet MS", colour="black", face = "bold"),
        axis.ticks = element_line(color = nih_distinct[4])) +
  coord_cartesian(ylim = c(-0.3, 0.25))

emf("td_pb_mar.emf", width = 12, height = 12)
plot_mar_pb
dev.off()

emf("td_pb_mar_mcar.emf", width = 16, height = 10)
ggarrange(plot_mcar_pb, plot_mar_pb, ncol = 1, labels = "AUTO")
dev.off()

cairo_pdf("Figure 4 Colour Image.pdf", width = 12, height = 12)
plot_mar_pb
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
plot_mcar = ggplot(d_impdata, aes(x=td, group = dataset_n)) +
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
d_cc_mar = d_cc_mar %>% rownames_to_column() %>% full_join(d_cc_target, by = "rowname") %>% fill(method)
d_realdata_mar = d_realdata_mar %>% filter(method != "Complete Case Analysis") %>% bind_rows(., d_cc)
d_imps_mar = d_imp_nogps_pos_red %>% filter(method != "Complete Case Analysis", imp_place == 1, missing_type == "mar", missing_amount == "strong")
d_impdata_mar = bind_rows(d_cc_mar, d_imps_mar)

plot_mar = ggplot(d_impdata_mar, aes(x=td, group = dataset_n)) +
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
  temp_data_mcar_noextra = map(missing_prop_mcar, ~readRDS(paste0(folder_fits, i,"_d_td_fits_d_exdata_td_noextra_mcar_",.,".rds"))) %>% bind_rows() %>% mutate(data = "noextra")
  temp_data_mcar_pos = map(missing_prop_mcar, ~readRDS(paste0(folder_fits, i,"_d_td_fits_d_exdata_td_pos_mcar_",.,".rds"))) %>% bind_rows() %>% mutate(data = "pos")
  temp_data_mcar_srpe_pos = map(missing_prop_mcar, ~readRDS(paste0(folder_fits, i,"_d_td_fits_d_exdata_td_srpe_pos_mcar_",.,".rds"))) %>% bind_rows() %>% mutate(data = "srpe_pos")
  d_fit_estimates = rbind(temp_data_mcar_noextra, temp_data_mcar_pos, temp_data_mcar_srpe_pos)
}

d_fit_estimates = d_fit_estimates %>% mutate(term = ifelse(term == "gps_td", "td", term)) %>% filter(method == "SI - PMM" | method == "MI - PMM")
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

# save dataset
d_si_v_mi_rounded = d_si_v_mi %>% mutate(SE_MI = round(SE_MI, 7), 
                                         SE_SI = round(SE_SI, 7),
                                         PB_MI = round(PB_MI, 1),
                                         PB_SI = round(PB_SI, 1))
write_excel_csv(d_si_v_mi_rounded, "td_si_vs_mi.csv", delim = ";", na = "")

#TODO? calc performance measures of imputed values
perf_esimates_impvalues = d_imp_nogps_pos %>% filter(method != "Complete Case Analysis", imp_place == 1) %>% 
  group_by(missing_type, missing_amount, method) %>% 
  summarise(rb = raw_bias(td, target),
            pb = percent_bias(td, target),
            rmse = rmse(td, target)) %>% 
  arrange(missing_type, missing_amount, rmse)

## TODO visualize imputations
# densityplot_itt = densityplot(x=imp.itt, data = ~td)
# densityplot_jav = densityplot(x=imp.jav, data = ~td)
# densityplot_pas = densityplot(x=imp.pas, data = ~td)
# densityplot_id = densityplot(x=imp.id, data = ~td)

# xyplot(imp.itt, injury ~ td)

# nested-loop-plot?

