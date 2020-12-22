xfun::pkg_attach(c('tidyverse', 'rgl', 'Morpho', 'Rvcg', 'soilmesh', 'ecmfuns', 'diRtscience', 'tbrdec2020', 'cowplot'))
theme_set(theme_cowplot()+
            background_grid()+
            theme(panel.grid.major = element_line(linetype = 'dotted'),
                  panel.grid.minor = element_line(linetype = 'dotted')))

day1meshes <- soilmesh::read_meshfiles('analysis/data/derived_data/downsampled_meshes/2020-12-16') %>%
  mutate(date= '2020-12-16')

day2meshes <- soilmesh::read_meshfiles('analysis/data/derived_data/downsampled_meshes/2020-12-18') %>%
  mutate(date= '2020-12-18')

non_mesh_data <- list.files(pattern = 'water_content_and_physical_vol_raw_data', recursive = T, full.names = T) %>%
  map(read_csv) %>%
  reduce(rbind)

test_metadata <- list.files(pattern= 'mesh_filename_metadata.csv', recursive = T, full.names = T) %>%
  read_csv() %>%
  mutate(date = str_replace_all(string = date, pattern = '/', replacement = '-')) %>%
  mutate(date= lubridate::mdy(date))

####

processed_meshes_list <- rbind(day1meshes, day2meshes) %>%
  .$mesh_object %>%
  map(soilmesh::pre_process_mesh, rot_tr_matr= soilmesh::method_development_rtmatrix)

processed_meshes_list

rbindedmeshes <- rbind(day1meshes, day2meshes)

processed_meshes <- rbind(day1meshes, day2meshes) %>%
  mutate(processed_mesh = processed_meshes_list) %>%
  filter(str_detect(string = rbindedmeshes$mesh_file_basename, pattern= 'post') )

# write processed meshes to new files for later analysis
writeable_processed_meshes <- rbind(day1meshes, day2meshes) %>%
  mutate(processed_mesh = processed_meshes_list) %>%
  mutate(filename = paste0('analysis/data/derived_data/processed_meshes/', date, "_", mesh_file_basename) ) %>%
  select(filename, processed_mesh) %>%
  rename(mesh= processed_mesh)
writeable_processed_meshes

writeable_processed_meshes %>%
  pwalk(.f = Rvcg::vcgPlyWrite)




# render a few meshes to check them ---------------------------------------

clear3d()

processed_meshes$processed_mesh[[14]] %>%
   shade3d(color= 'brown')
shade3d(ref_circ)
processed_meshes$processed_mesh[[14]] %>%
  vol_above()

processed_meshes$processed_mesh[[14]] %>%
  vol_below()


# calculate the volume change for each cylinder, first with the physical method
# and then with the scanning method -------------------------------

physvols <- non_mesh_data %>%
  mutate(sand_mass = sand_cup_mass_before_filling_g - sand_cup_mass_after_filling_g,
         physvol_below = sand_mass / 1.55,
         physvol_above= OD_soil_mass_above_ref_plane / 1.9,
         phystotal_vol= physvol_below + physvol_above)

scanvols <- processed_meshes %>%
  mutate(scanvolume_above = map_dbl(processed_mesh, soilmesh::vol_above),
         scanvolume_below = map_dbl(processed_mesh, soilmesh::vol_below),
         scantotal_vol= scanvolume_above + scanvolume_below,
         date= lubridate::as_date(date),
         cylinder_ID= physvols$cylinder_ID)

metadata <- tibble(
  cylinder_ID= rep(c(paste0("0", 1:9), "10", "11", "12")),
  sand_pct= rep(rep(c(0.6, 0.75), each=3), times=2),
  effort= rep(c('standard', 'modified'), each=6)
)

vol_comp <- scanvols %>%
  left_join(physvols) %>%
  left_join(metadata)
vol_comp

w_conts <- non_mesh_data %>%
  left_join(test_metadata) %>%
  left_join(diRtscience::asi468_tin_tares$`2020-05-24`) %>%
  add_w() %>%
  select(cylinder_ID:date, water_content)

vol_data <- metadata %>%
  left_join(vol_comp) %>%
  left_join(w_conts) %>%
  mutate(replicate= rep(letters[1:3], times= 8)) %>%
  rename(vol_change= phystotal_vol)

# visualize the whole data set  -------------------------------------------

vol_data %>%
  ggplot(aes(water_content, vol_change, color=factor(sand_pct), shape= factor(date), label= replicate))+
  geom_point()+
  geom_text(color= 'black', nudge_y = 3)+
  scale_x_continuous(name='Water content, % g/g', breaks=scales::breaks_width(0.01), labels = scales::label_percent(suffix = "", accuracy = 1))+
  scale_y_continuous(bquote("Disturbed soil volume, cm"^3))+
  facet_grid(effort~date)+
  theme_bw()


# compare correlations among measurement methods --------------------------

# total volume

vol_comp %>%
  ggplot(aes(phystotal_vol, y = scantotal_vol))+
  geom_point(color= 'darkblue', fill='darkblue', shape= 21, alpha=1/2)+
  geom_smooth(se=F, method=lm,
              formula = y~x, aes(color=NULL),
              size= 0.5, color='black')+
  cowplot::theme_cowplot()+
  geom_abline(slope= 1, intercept= 0, linetype= 'dotted', color= 'grey25')+
  coord_fixed(ratio = 1)+
  expand_limits(x=0, y=0)+
  scale_x_continuous(
    name= "\"Backfill and weigh\" method",
    breaks = scales::breaks_width(width = 20, offset = 0) )+
  scale_y_continuous(
    name = "3D scanning method",
    breaks = scales::breaks_width(width = 20, offset = 0) )+
  labs(title= "Departure from planarity",
       subtitle= bquote("Volumes (cm"^3~") obtained via 2 methods."),
       color= "Compaction effort")+
  theme(legend.position = c(0.1, 0.8) )

# volume above only
vol_comp %>%
  ggplot(aes(physvol_above, scanvolume_above))+
  geom_point(color= 'darkblue', fill='darkblue', shape= 21, alpha=1/2)+
  geom_smooth(se=F, method=lm,
              formula = y~x, aes(color=NULL),
              size= 0.5, color='black')+
  cowplot::theme_cowplot()+
  geom_abline(slope= 1, intercept= 0, linetype= 'dotted', color= 'grey25')+
  coord_fixed(ratio = 1, xlim = c(0,40), ylim= c(0,40))+
  scale_x_continuous("\"Backfill and weigh\" method")+
  scale_y_continuous("3D scanning method")+
  labs(title= "Disturbed soil ABOVE reference plane",
       subtitle= "Results obtained from 2 different measurement methods.")


# volume below only
vol_comp %>%
  ggplot(aes(physvol_below, scanvolume_below))+
  geom_point(color= 'darkblue', fill='darkblue', shape= 21, alpha=1/2)+
  geom_smooth(se=F, method=lm,
              formula = y~x, aes(color=NULL),
              size= 0.5, color='black')+
  cowplot::theme_cowplot()+
  geom_abline(slope= 1, intercept= 0, linetype= 'dotted', color= 'grey25')+
  coord_fixed(ratio = 1, xlim = c(0,40), ylim= c(0,40))+
  scale_x_continuous("\"Backfill and weigh\" method")+
  scale_y_continuous("3D scanning method")+
  labs(title= "Disturbed soil BELOW reference plane",
       subtitle= "Results obtained from 2 different measurement methods.")


# fit models to data  -----------------------------------------------------

# create new data set from only samples built with the standard compaction effort
standard_data <- vol_data %>%
  filter(effort == 'standard') %>%
  mutate(sand_pct= as_factor(sand_pct),
         date= factor(date),
         replicate= factor(replicate))
standard_data

# If replicate is treated as a normal factor, it has too many degrees of freedom....
# it is considered as a "treatment" instead of a blocking variable since the "levels"
# are the same (a, b, c) for both soils....

model1 <- standard_data %>%
  lm(data= .,
     formula= vol_change ~ factor(sand_pct) + replicate + factor(date) + water_content)

anova(model1)


# Treat sand_pct and date as fixed effect factors, replicate as a fixed effect
# factor NESTED under sand_pct, and and water content as the covariate

model2 <- standard_data %>%
  lm(data= .,
     formula = vol_change ~ sand_pct/replicate + date + water_content)

anova(model2)

# Treat date and replicate as random effects, and sand_pct and water_content as
# covariates. Don't nest replicate (this is wrong, see model1 above, but I want
# to be sure I don't crash R on the first try)

library(lme4)

model3 <- standard_data %>%
  lm(data= .,
     formula = vol_change ~ sand_pct + (1 + replicate) + (1 + date) + water_content)
anova(model3)

# looks good save for the wrong df term for replicate.


model4 <- standard_data %>%
  lm(data= .,
     formula = vol_change ~ sand_pct/(1 + replicate) + (1 + date) + water_content)
anova(model4)

model5 <- standard_data %>%
  lm(data=.,
     formula = vol_change ~ sand_pct + (1 | sand_pct/replicate) + (1 | sand_pct/date) + water_content)

anova(model5)

