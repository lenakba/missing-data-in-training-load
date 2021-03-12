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
folder_fits = paste0(base_folder, "td_fits\\")

# vector of chosen missing proportions
# if we ever want to change it or add more proportions, easily done here.
missing_prop_mcar = c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
missing_prop_mar = c("light", "medium", "strong")

# reading the simulated results from fits
files_fits = list.files(path = folder_fits)
n_sim = length(files_fits)/(length(missing_prop_mcar) + length(missing_prop_mar)) # divide by the number of missing type and level combinations
d_fit_estimates = data.frame()
for(i in 1:n_sim){
  temp_data_mcar = map(missing_prop_mcar, ~readRDS(paste0(folder_fits, i,"_d_td_fits_mcar_",.,".rds"))) %>% bind_rows()
  temp_data_mar = map(missing_prop_mar, ~readRDS(paste0(folder_fits, i,"_d_td_fits_mar_",.,".rds"))) %>% bind_rows()
  d_fit_estimates = rbind(d_fit_estimates, temp_data_mcar, temp_data_mar)
}

# number of permutations needed for MCSE for bias of 0.5
d_fit_estimates_td %>% 
  filter(rep <= 100) %>% 
  mutate(rb = estimate - target_est) %>% 
  group_by(method, missing_amount) %>% 
  summarise(variance_est = var(rb, na.rm = TRUE), n_sim = (variance_est^2)/0.25)


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
  perf_estimates_targetcoef %>% 
    mutate(var_extra = var_extra, 
           var_gps = var_gps)
  perf_estimates_targetcoef
}

perf_estimates_targetcoef = calc_perf_params(d_fit_estimates_td, "No extra variables", "Total Distance Only")

# save to csv
# save results
# write_delim is preferable, but write_excel_csv is required for excel to understand
# that the file encoding is UTF-8
write_excel_csv(perf_estimates_targetcoef, "simulation_results_fits_td.csv", delim = ";", na = "")

#--------------------------------------------is there any difference for having sRPE, and having no gps, vs. missing total distance without sRPE?
# folder of fits for the srpe version
folder_fits_srpe = paste0(base_folder, "td_fits_srpe\\")

# reading the simulated results from fits
files_fits_srpe = list.files(path = folder_fits_srpe)
n_sim = length(files_fits_srpe)/(length(missing_prop_mcar) + length(missing_prop_mar)) # divide by the number of missing type and level combinations
d_fit_estimates_srpe = data.frame()
for(i in 1:n_sim){
  temp_data_mcar = map(missing_prop_mcar, ~readRDS(paste0(folder_fits_srpe, i,"_d_td_fits_mcar_",.,".rds"))) %>% bind_rows()
  temp_data_mar = map(missing_prop_mar, ~readRDS(paste0(folder_fits_srpe, i,"_d_td_fits_mar_",.,".rds"))) %>% bind_rows()
  d_fit_estimates_srpe = rbind(d_fit_estimates_srpe, temp_data_mcar, temp_data_mar)
}

d_fit_estimates_td_srpe = d_fit_estimates_srpe %>% 
  mutate(target_est = target_coef) %>% 
  filter(term == "td", method != "No Imputation")

perf_estimates_targetcoef_srpe = d_fit_estimates_td_srpe %>% 
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
  arrange(missing_type, missing_amount, desc(rmse)) %>% ungroup()  %>% 
  mutate(var_extra = "sRPE", 
         var_gps = "Total Distance Only")

# same for no gps data
folder_fits_nogps = paste0(base_folder, "td_fits_nogps\\")

# reading the simulated results from fits
files_fits_nogps = list.files(path = folder_fits_nogps)
n_sim = length(files_fits_nogps)/(length(missing_prop_mcar) + length(missing_prop_mar)) # divide by the number of missing type and level combinations
d_fit_estimates_nogps = data.frame()
for(i in 1:n_sim){
  temp_data_mcar = map(missing_prop_mcar, ~readRDS(paste0(folder_fits_nogps, i,"_d_td_fits_mcar_",.,".rds"))) %>% bind_rows()
  temp_data_mar = map(missing_prop_mar, ~readRDS(paste0(folder_fits_nogps, i,"_d_td_fits_mar_",.,".rds"))) %>% bind_rows()
  d_fit_estimates_nogps = rbind(d_fit_estimates_nogps, temp_data_mcar, temp_data_mar)
}

d_fit_estimates_td_nogps = d_fit_estimates_nogps %>% 
  mutate(target_est = target_coef) %>% 
  filter(term == "td", method != "No Imputation")

perf_estimates_targetcoef_nogps = d_fit_estimates_td_nogps %>% 
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
  arrange(missing_type, missing_amount, desc(rmse)) %>% ungroup() %>% mutate(var_combo = "No extra information") %>% 
  mutate(var_extra = "No extra variables", 
         var_gps = "All GPS")



# folder of fits for the srpe and position version
folder_fits_srpe_pos = paste0(base_folder, "td_fits_srpe_pos\\")

# reading the simulated results from fits
files_fits_srpe_pos = list.files(path = folder_fits_srpe_pos)
n_sim = length(files_fits_srpe_pos)/(length(missing_prop_mcar) + length(missing_prop_mar)) # divide by the number of missing type and level combinations
d_fit_estimates_srpe_pos = data.frame()
for(i in 1:n_sim){
  temp_data_mcar = map(missing_prop_mcar, ~readRDS(paste0(folder_fits_srpe_pos, i,"_d_td_fits_mcar_",.,".rds"))) %>% bind_rows()
  temp_data_mar = map(missing_prop_mar, ~readRDS(paste0(folder_fits_srpe_pos, i,"_d_td_fits_mar_",.,".rds"))) %>% bind_rows()
  d_fit_estimates_srpe_pos = rbind(d_fit_estimates_srpe_pos, temp_data_mcar, temp_data_mar)
}

d_fit_estimates_td_srpe_pos = d_fit_estimates_srpe_pos %>% 
  mutate(target_est = target_coef) %>% 
  filter(term == "td", method != "No Imputation")

perf_estimates_targetcoef_srpe_pos = d_fit_estimates_td_srpe_pos %>% 
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
  arrange(missing_type, missing_amount, desc(rmse)) %>% ungroup() %>% 
  mutate(var_extra = "Player position and sRPE", 
         var_gps = "Total Distance Only")


# folder of fits for the srpe and position version
folder_fits_nogps_pos = paste0(base_folder, "td_fits_nogps_pos\\")

# reading the simulated results from fits
files_fits_nogps_pos = list.files(path = folder_fits_nogps_pos)
n_sim = length(files_fits_nogps_pos)/(length(missing_prop_mcar) + length(missing_prop_mar)) # divide by the number of missing type and level combinations
d_fit_estimates_nogps_pos = data.frame()
for(i in 1:n_sim){
  temp_data_mcar = map(missing_prop_mcar, ~readRDS(paste0(folder_fits_srpe_pos, i,"_d_td_fits_mcar_",.,".rds"))) %>% bind_rows()
  temp_data_mar = map(missing_prop_mar, ~readRDS(paste0(folder_fits_srpe_pos, i,"_d_td_fits_mar_",.,".rds"))) %>% bind_rows()
  d_fit_estimates_nogps_pos = rbind(d_fit_estimates_nogps_pos, temp_data_mcar, temp_data_mar)
}

d_fit_estimates_td_nogps_pos = d_fit_estimates_nogps_pos %>% 
  mutate(target_est = target_coef) %>% 
  filter(term == "td", method != "No Imputation")

perf_estimates_targetcoef_nogps_pos = d_fit_estimates_td_nogps_pos %>% 
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
  arrange(missing_type, missing_amount, desc(rmse)) %>% 
  ungroup() %>% 
  mutate(var_extra = "Player position", 
         var_gps = "All GPS")



# adding name onto original simulation
perf_estimates_targetcoef = perf_estimates_targetcoef  %>% 
  mutate(var_extra = "No extra variables", 
         var_gps = "Total Distance Only")

# combining into 1 dataset
perf_estimates_all = bind_rows(
  perf_estimates_targetcoef,
  perf_estimates_targetcoef_srpe,
  perf_estimates_targetcoef_srpe_pos,
  perf_estimates_targetcoef_nogps_pos,
  perf_estimates_targetcoef_nogps
)
# save to csv
write_excel_csv(perf_estimates_all, "simulation_results_fits_td.csv", delim = ";", na = "")

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


emf("td_pb_mcar.emf", width = 12, height = 8)
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

emf("td_pb_mar.emf", width = 12, height = 8)
plot_mar_pb
dev.off()

emf("td_pb_mar_mcar.emf", width = 16, height = 10)
ggarrange(plot_mcar_pb, plot_mar_pb, ncol = 1, labels = "AUTO")
dev.off()

#--------------------------------Read data and calculate performance measures on the raw data

# where the imputed datasets are saved
folder_imps = paste0(base_folder, "td_imps\\")
n_sim= 5
# we assume it is the same number of simulations for both simulations
# reading the simulated imputation datasets
files_imps = list.files(path = folder_imps)
d_imp = data.frame()
for(i in 1:n_sim){
  temp_data_mcar = map(missing_prop_mcar, ~readRDS(paste0(folder_imps, i,"_d_td_imps_mcar_",.,".rds"))) %>% bind_rows()
  temp_data_mar = map(missing_prop_mar, ~readRDS(paste0(folder_imps, i,"_d_td_imps_mar_",.,".rds"))) %>% bind_rows()
  d_imp = rbind(d_imp, temp_data_mcar, temp_data_mar)
}

perf_esimates_impvalues = d_imp %>% filter(method != "Complete Case Analysis", imp_place == 1) %>% 
  group_by(missing_type, missing_amount, method) %>% 
  summarise(rb = raw_bias(td, target),
            pb = percent_bias(td, target),
            rmse = rmse(td, target),
            mcse_rmse = mcse_rmse(td, target, n_sim)) %>% 
  arrange(missing_type, missing_amount, rmse)

## TODO visualize imputations
# densityplot_itt = densityplot(x=imp.itt, data = ~td)
# densityplot_jav = densityplot(x=imp.jav, data = ~td)
# densityplot_pas = densityplot(x=imp.pas, data = ~td)
# densityplot_id = densityplot(x=imp.id, data = ~td)

# xyplot(imp.itt, injury ~ td)

# nested-loop-plot?


# dotplot version
# plot_mar_pb =  ggplot(d_fig_mar_all, aes(x = pb, y = method)) + 
#   facet_wrap(c("missing_amount", "var_combo")) +
#   geom_vline(xintercept = 0, size = 1, alpha = 0.3, colour = bjsm_blue) +
#   geom_vline(xintercept = 0.05, size = 1, alpha = 0.3) +
#   geom_vline(xintercept = -0.05, size = 1, alpha = 0.3) +
#   geom_point(size = 3) + 
#   theme_dot() + 
#   xlab("Percent Bias") +
#   ylab(NULL) +
#   scale_x_continuous(labels = axis_percent) +
#   theme(axis.text = element_text(size=text_size),
#         strip.text.x = element_text(size = text_size),
#         axis.title =  element_text(size=text_size))