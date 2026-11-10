!------------------------------------------------------------------------------
! MODULE: ncnn_modelbin
!
! DESCRIPTION:
!>  ncnn ModelBin API (3 functions).
!------------------------------------------------------------------------------
module ncnn_modelbin
    use, intrinsic :: iso_c_binding
    use ncnn_types, only: ncnn_modelbin_t, ncnn_datareader_t, ncnn_mat_t
    implicit none
    private

    public :: modelbin_create_from_datareader, modelbin_create_from_mat_array, &
              modelbin_destroy

    interface

        function ncnn_modelbin_create_from_datareader(dr) bind(c)
            import :: c_ptr
            type(c_ptr), value :: dr
            type(c_ptr) :: ncnn_modelbin_create_from_datareader
        end function

        ! const ncnn_mat_t* weights -- array of opaque handles
        function ncnn_modelbin_create_from_mat_array(weights, n) bind(c)
            import :: c_int, c_ptr
            integer(c_int), value :: n
            type(c_ptr)        :: weights(n)
            type(c_ptr) :: ncnn_modelbin_create_from_mat_array
        end function

        subroutine ncnn_modelbin_destroy(mb) bind(c)
            import :: c_ptr
            type(c_ptr), value :: mb
        end subroutine

    end interface

contains

    !> ncnn_modelbin_t ncnn_modelbin_create_from_datareader(const ncnn_datareader_t dr)
    function modelbin_create_from_datareader(dr) result(mb)
        type(ncnn_datareader_t), intent(in) :: dr
        type(ncnn_modelbin_t)               :: mb
        mb%ptr = ncnn_modelbin_create_from_datareader(dr%ptr)
    end function modelbin_create_from_datareader

    !> ncnn_modelbin_t ncnn_modelbin_create_from_mat_array(const ncnn_mat_t* weights, int n)
    function modelbin_create_from_mat_array(weights) result(mb)
        type(ncnn_mat_t), intent(in) :: weights(:)
        type(ncnn_modelbin_t)        :: mb

        type(c_ptr), allocatable :: ptrs(:)
        integer :: n, i

        n = size(weights)
        allocate (ptrs(n))
        do i = 1, n
            ptrs(i) = weights(i)%ptr
        end do

        mb%ptr = ncnn_modelbin_create_from_mat_array(ptrs, n)
    end function modelbin_create_from_mat_array

    !> void ncnn_modelbin_destroy(ncnn_modelbin_t mb)
    subroutine modelbin_destroy(mb)
        type(ncnn_modelbin_t), intent(inout) :: mb
        call ncnn_modelbin_destroy(mb%ptr)
        mb%ptr = c_null_ptr
    end subroutine modelbin_destroy

end module ncnn_modelbin
