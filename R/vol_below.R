#' \lifecycle{experimental}
#'
#' @title Calculate soil volume below a reference plane
#'
#' @description One half of the total volume computation.
#'
#' @param mesh triangular mesh of class "mesh3d"
#' @param z_displ_mm vertical offset to prevent removal of vertices near the origin
#' @param ball_radius_mm radius in mm for the ball-pivoting mesh reconstruction
#'
#' @return numeric vector of length 1, representing the cm3 of soil
#'   residing above the reference plane
#'
#' @details This applies a sort of hack to slice the mesh and keep only points
#'   residing above the x-y plane. The "hack" portion is translating the
#'   actual reference plane away from the mesh by a very small distance. So long
#'   as the ball radius is smaller than this distance, one can be confident that
#'   the reference plane will be preserved as part of the new mesh. The volume
#'   attributable to the offset is subtracted before completing the calculation.
#'   The meshes used in this project can _never_ be "watertight" and it is my
#'   hope that so long as the mesh is 2-manifold, the volume computation is
#'   accurate. This function calls [`Rvcg::vcgClean()`] to remove non-manifold
#'   faces, then remove unreferenced vertices, and (finally) remove non-manifold
#'   vertices. This approach seems to resolve the non-manifoldness of the meshes
#'   induced by downsampling, cylinder removal, and top/bottom removal.
#' @export
#'

vol_below <- function(mesh, z_displ_mm = 0.01, ball_radius_mm = 0.005){

  # slice the mesh along the x-y plane and keep only what's above

  sliced_mesh <- Morpho::cutMeshPlane(mesh = mesh,
                                      v1= c(1,0,0),
                                      v2= c(0,1,0),
                                      v3= c(-1,0,0),
                                      keep.upper = FALSE)

  # merge the sliced portion and translate the reference plane away from it by a small amount
  # then perform a re-meshing with the ball pivoting method and clean the mesh
  # the cleaning step applies three actions: remove non-manifold faces, remove
  # unreferenced vertices, and remove non-manifold vertices; operations iterate
  # until none of the above remain

  vol_diff_mm3 <- Morpho::mergeMeshes(sliced_mesh,
                                      rgl::translate3d(obj = soilmesh::ref_circ,
                                                       x= 0,
                                                       y= 0,
                                                       z= z_displ_mm)) %>%
    Rvcg::vcgBallPivoting(radius = ball_radius_mm) %>%
    Rvcg::vcgClean(sel = c(2,1,4), iterate = T) %>%
    Rvcg::vcgVolume()

  # convert to cm3 and then subtract the extra volume added by offsetting the
  # reference plane....it is very small but just to be complete

  vol_diff_cm3 <- vol_diff_mm3*0.001 - ((z_displ_mm*0.1)*186.2348)

  return(vol_diff_cm3)

}
