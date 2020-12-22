#' @title Process a 3D soil mesh for analysis
#'
#' @description This function has been saved in the soilmesh package. I am just leaving it here in case I need to look at it later on.
#'  Pipe together several other internal processing functions
#'
#' @param mesh a mesh3d object
#' @param rot_tr_matr a 4x4 roto-translation matrix. User can select a matrix
#'   from the date of calibration (these matrices are stored in the
#'   [`rot_tr_matrices`] data object inside **soilmesh**`; alternatively, the
#'   matrix can be manually specified
#' @param ... other arguments passed to [`remove_cyl()`], should user wish to
#'   adjust the radius of retained mesh vertices
#'
#' @return processed mesh3d object
#' @details Order of operations is important. The steps are as follows:
#'
#' 1. Sample is rigidly (i.e. no deformation or scaling) re-oriented using a
#' user-specified roto-translation matrix.
#' 2. Sample is trimmed of any points more than 50 mm beneath x-y plane. This
#' removes points registered from the scanner turntable or the cylinder base to
#' ensure step (3) is performed accurately.
#' 3. Sample is adjusted in the x and y directions based on its largest values
#' in these dimensions; this returns the true center of the specimen to the
#' horizontal origin (i.e. x=0 and y=0)
#' 4. Remove the specimen holder and the outermost ring of soil (default radius
#' is 70 mm per [`remove_cyl()`])
#' 5. Sample the z-elevations and adjust sample accordingly; see [`adjust_z()`]
#' @export
#'
piped_mesh_processing <- function(mesh, rot_tr_matr, ...){

  warning('`piped_mesh_processing` is deprecated; please use soilmesh::pre_process_mesh()` instead.')

  if(missing(rot_tr_matr)){
    stop('\nNo roto-translation matrix specified;
         please choose one from the `rot_tr_matrices` data object in `soilmesh`')
  }

  mesh %>%
    soilmesh::orient_sample(mesh = ., rot_tr_matr = rot_tr_matr) %>%
        Morpho::cutMeshPlane(v1 = c(1, 0, -50),
                     v2= c(0, 1, -50),
                     v3= c(-1, 0, -50),
                     keep.upper = TRUE) %>%
    soilmesh::adjust_xy() %>%
    soilmesh::remove_cyl(...) %>%
    soilmesh::adjust_z()
    }
