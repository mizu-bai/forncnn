!------------------------------------------------------------------------------
! MODULE: ncnn
!
! DESCRIPTION:
!>  Umbrella module: `use ncnn` brings in the whole forncnn API.
!
!>  Each sub-module keeps an explicit private/public list, so only the
!>  intended API is re-exported here. Because Fortran does not re-export
!>  use-associated entities from a module that declares `private`, this module
!>  deliberately has no private statement of its own.
!
!>  For finer-grained control, use the sub-modules directly with `use, only`:
!>      use ncnn_net,     only: ncnn_net_t, net_create, net_load_param
!>      use ncnn_mat,     only: ncnn_mat_t, mat_create_2d, mat_get_data
!>      use ncnn_extractor, only: ncnn_extractor_t, extractor_create
!------------------------------------------------------------------------------
module ncnn
    use ncnn_types
    use ncnn_core
    use ncnn_mat
    use ncnn_blob
    use ncnn_paramdict
    use ncnn_datareader
    use ncnn_modelbin
    use ncnn_layer
    use ncnn_net
    use ncnn_extractor
    implicit none
end module ncnn
