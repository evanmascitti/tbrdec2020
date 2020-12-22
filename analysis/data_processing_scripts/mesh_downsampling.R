xfun::pkg_attach(c('tidyverse', 'Morpho', 'Rvcg', 'rgl', 'ecmfuns', 'soilmesh', 'diRtscience'))

# The smallest mesh file has around 400,000 faces, so I will down-sample all of
# them to 250000 for faster processing. Still need to compare the down-sampling
# titration to see if data is lost

raw_mesh_filenames <-list.files(
  path = 'analysis/data/raw_data/meshes',
  recursive = T, full.names = T, pattern = '.ply')

file_paths_to_save <- paste0('analysis/data/derived_data/downsampled_meshes/',
                             str_sub(string = raw_mesh_filenames, start = 31) )

raw_meshes <- raw_mesh_filenames %>%
   purrr::map(vcgImport)

beepr::beep()

raw_meshes %>%
  purrr::map(~Rvcg::vcgQEdecim(mesh = ., tarface = 250000)) %>%
  purrr::set_names(nm= file_paths_to_save) %>%
  purrr::map2(.y = file_paths_to_save, .f= Rvcg::vcgPlyWrite)

beepr::beep()
