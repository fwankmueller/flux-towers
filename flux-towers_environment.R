### Setup: use library to load missing packages (within R project we need to use renv::install() to install missing packages)
lapply(c("tidyverse", "data.table", "patchwork"), library, character.only = T)

### READ data & files ----
# List csv files containing half-hourly Eddy-Covariance Fluxes
files_path <- list.files(path = "~/work/polybox-teaching/flux-towers/", pattern = ".csv", full.names = T, recursive = T)

# extract site name and give it to files_path
names_ec <-
  basename(path = files_path) %>% 
  str_extract(pattern = ".*(?=_FLUXNET)")

setattr(files_path, 'names', names_ec)

# read files: results in a list
list_ec_fluxes <-
  lapply(
    X = files_path,
    FUN = fread, na.strings = "-9999"
  )


# It's a list of non-rectangular data.tables
list_ec_fluxes %>% lapply(names) %>% View


# # single Data Table
# ec_fluxes <- rbindlist(list_ec_fluxes, fill = T, idcol = "site_id")
#
# View(ec_fluxes[, .SD[c(1, .N)], site_id])


# ------------------------------------------------------------------------------------------
### Let's start working with list format for now:

for (ii in seq_along(list_ec_fluxes)) {
    list_ec_fluxes[[ii]][, ':=' (day = as_date(substr(TIMESTAMP_START, 1, 8), format = "%Y%m%d"),
                                 datetime = as_datetime(substr(TIMESTAMP_START, 1, 12), format = "%Y%m%d%H%M", tz = "UTC"))]
}

# ec_filtered <-
# for (ii in seq_along(list_ec_fluxes)) {
#     list_ec_fluxes[[ii]] %>%
#         .[NIGHT == 0] %>%
#         .[LE_F_MDS >= 0 & H_F_MDS >= 0] %>%                             # xx Only positive surface energy fluxes (--> positve EF)
#         .[SWC_F_MDS_1 >= 0 & VPD_F >= 0] %>%                            # xx No negative VPD, SM
#         na.omit()
# }


# Are the columns of interest present in all sites?
list_ec_subset <-
lapply(list_ec_fluxes, function(x) x[, .(SWC_F_MDS_1, VPD_F, LE_F_MDS, H_F_MDS, SW_IN_F, NIGHT, WS_F, TA_F, day, datetime)])


### Monthly Temperatures Ridgeplot:
l_ridgeplot <- list()
for (ii in seq_along(list_ec_subset)) {
    l_ridgeplot[[names(list_ec_subset)[ii]]] <-
        list_ec_subset[[ii]][NIGHT == 0] %>%
           .[LE_F_MDS >= 0 & H_F_MDS >= 0] %>%                             # xx Only positive surface energy fluxes (--> positve EF)
           .[SWC_F_MDS_1 >= 0 & VPD_F >= 0] %>%
           # .[, .(tmp_MEAN = mean(TA_F, na.rm = T),
           #       tmp_MEDIAN = median(TA_F, na.rm = T)), by = month(day)] %>%
           #%>%                            # xx No negative VPD, SM
           #na.omit()
           #) %>% # rbindlist(idcol = "site_id") %>%
           ggplot(aes(x = TA_F, y = factor(month(day)), fill = ..x..)) +
           geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
           scale_fill_viridis(name = "Temp. [°C]", option = "C") +
           labs(title = names(list_ec_subset)[[ii]]) +
           coord_cartesian(xlim = c(-10, 40)) +
           theme_ipsum() +
           theme(
               legend.position="none", #"top",
               panel.spacing = unit(0.1, "lines"),
               strip.text.x = element_text(size = 8)
           )
}

# l_ridgeplot %>%
#     cowplot::plot_grid(plotlist = l_ridgeplot, nrow = 2)

p_tmps <-
    l_ridgeplot %>%
    patchwork::wrap_plots(ncol = 9)

# p_tmps %>%
#     ggsave(filename = "monly_tmps_sites.pdf",
#            width = 45, height = 25, units = "cm")

l_ridgeplot[["CN-Sw2"]] + coord_cartesian(xlim = c(-25,40))


### median daytime VPD of three hottest months
l_med_months <- list(); l_med_site <- list(); l_med_site_filtered <- list()
for (ii in seq_along(list_ec_subset)) {
    l_med_months[[names(list_ec_subset)[ii]]] <- list_ec_subset[[ii]] %>% .[, lapply(.SD, median, na.rm = T), by = month(day)]
    n_months <- l_med_months[[ii]] %>% .[order(-TA_F)] %>% .[1:3, month]
    # max_months <- list_ec_subset[[ii]][, .(avg_tmp = median(TA_F, na.rm = T)), by = month(day)][order(-avg_tmp)]
    l_med_site[[names(list_ec_subset)[ii]]] <- list_ec_subset[[ii]] %>% .[month(day) %in% n_months, lapply(.SD, median, na.rm = T)]
    l_med_site_filtered[[names(list_ec_subset)[ii]]] <-
        list_ec_subset[[ii]] %>%
        .[NIGHT == 0] %>%                                               # Only daytime values
        .[LE_F_MDS >= 0 & H_F_MDS >= 0] %>%                             # Only positive surface energy fluxes (--> positve EF)
        .[SWC_F_MDS_1 >= 0 & VPD_F >= 0] %>%                            # No negative VPD, SM
        .[month(day) %in% n_months, lapply(.SD, median, na.rm = T)]     # MEDIAN of the 3 hottest months (or what is chosen above)
}
med_site <- rbindlist(l_med_site, idcol = "site_id")
med_site_filtered <- rbindlist(l_med_site_filtered, idcol = "site_id")

# Sys.setlocale(locale = "C")
med_site[med_site_filtered, on = "site_id"][, .(.SD, VPD_minus_i.VPD = VPD_F - i.VPD_F)] %>%
    setorder(.SD.site_id) %>%
    fwrite(file = "med_site-AND-med_site_filtered.csv")



# aa <- copy(list_ec_subset[[ii]])[, tmp_MED := median(TA_F, na.rm = T), by = month(day)][]
#
# aa[order(tmp_MED, decreasing = T)][, median(TA_F), by = tmp_MED]
# aa[, .SD[head(tmp_MED %>% order(decreasing = T), 3)]]
#
# tmp_max <- aa$tmp_MED %>% unique %>% sort(decreasing = T) %>% head(3)
# aa[tmp_MED %in% tmp_max, lapply(.SD, median, na.rm = T), month(day)]
#
# aa[, lapply(.SD, median, na.rm = T), by = month(day)]
# aa[tmp_MED %in% tmp_MED %>% unique %>% sort(decreasing = T) %>% head(3),
#    lapply(.SD, median, na.rm = T), by = month(day)]
#
# aa %>% arrange(desc(tmp_MED)) %>% slice_max(order_by = tmp_MED, n = 3)



# ----------------------------------------------------------------------------------------------------------------
# ### Exkurs: Microbnechmarking Lapply vs. for-loop
# update_with_lapply <- function(list) {
#     lapply(
#     X = list_ec_fluxes,
#     FUN = function(x) x[, ':=' (day = as_date(substr(TIMESTAMP_START, 1, 8), format = "%Y%m%d"),
#                                 datetime = as_datetime(substr(TIMESTAMP_START, 1, 12), format = "%Y%m%d%H%M", tz = "UTC"))]
#     )
# }
#
# # a <- list_ec_fluxes[[1]]
# # a[, ':=' (day = as_date(substr(TIMESTAMP_START, 1, 8), format = "%Y%m%d"),
# #           datetime = as_datetime(substr(TIMESTAMP_START, 1, 12), format = "%Y%m%d%H%M", tz = "UTC"))]
#
# update_with_for <- function(list) {
#     for (ii in seq_along(list_ec_fluxes)) {
#     list_ec_fluxes[[ii]][, ':=' (day = as_date(substr(TIMESTAMP_START, 1, 8), format = "%Y%m%d"),
#                                  datetime = as_datetime(substr(TIMESTAMP_START, 1, 12), format = "%Y%m%d%H%M", tz = "UTC"))]
#     }
# }
#
# aa <-
# microbenchmark::microbenchmark(
#     lapply_version = update_with_lapply(list),
#     for_loop_version = update_with_for(list),
#     times = 10
# ) # %>% autoplot()
#
# microbenchmark:::autoplot.microbenchmark(aa)
#
# # >>> So as a conclusion - and found online - the speed advantage of lapply is debunked as a myth!
