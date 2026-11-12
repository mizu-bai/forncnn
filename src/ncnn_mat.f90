!------------------------------------------------------------------------------
! MODULE: ncnn_mat_t
!
! DESCRIPTION:
!>  ncnn Mat API (52 functions).
!>
!>  Channel and batch indices are 1-based, as Fortran expects; the binding
!>  subtracts one before calling into C. Data accessors return owned copies and
!>  are elemsize-safe, unlike the raw C void* pointers.
!------------------------------------------------------------------------------
module ncnn_mat
    use, intrinsic :: iso_c_binding
    use ncnn_types, only: ncnn_mat_t, ncnn_allocator_t, ncnn_option_t

    implicit none
    private

    public :: border_constant, border_replicate, border_reflect, border_transparent

    public :: mat_create, mat_create_1d, mat_create_2d, mat_create_3d, mat_create_4d, &
              mat_create_1d_batch, mat_create_2d_batch, mat_create_3d_batch, &
              mat_create_4d_batch, &
              mat_create_external_1d, mat_create_external_2d, mat_create_external_3d, &
              mat_create_external_4d, &
              mat_create_1d_elem, mat_create_2d_elem, mat_create_3d_elem, &
              mat_create_4d_elem, &
              mat_create_1d_elem_batch, mat_create_2d_elem_batch, &
              mat_create_3d_elem_batch, mat_create_4d_elem_batch, &
              mat_create_external_1d_elem, mat_create_external_2d_elem, &
              mat_create_external_3d_elem, mat_create_external_4d_elem, &
              mat_destroy, mat_fill_float, mat_clone, &
              mat_reshape_1d, mat_reshape_2d, mat_reshape_3d, mat_reshape_4d, &
              mat_get_dims, mat_get_w, mat_get_h, mat_get_d, mat_get_c, mat_get_n, &
              mat_get_elemsize, mat_get_elempack, mat_get_cstep, mat_get_nstep, &
              mat_get_data, mat_get_data_bytes, mat_get_channel_data, &
              mat_get_batch_data, &
              mat_substract_mean_normalize, &
              mat_convert_packing, mat_flatten, &
              mat_copy_make_border, mat_copy_make_border_3d, &
              mat_copy_cut_border, mat_copy_cut_border_3d

    ! ncnn::BorderType
    integer(c_int), parameter :: border_constant    = 0
    integer(c_int), parameter :: border_replicate   = 1
    integer(c_int), parameter :: border_reflect     = 2
    integer(c_int), parameter :: border_transparent = -233

    !--------------------------------------------------------------------------
    ! RAW C DECLARATIONS
    !--------------------------------------------------------------------------
    interface

        function ncnn_mat_create() bind(c)
            import :: c_ptr
            type(c_ptr) :: ncnn_mat_create
        end function

        function ncnn_mat_create_1d(w, allocator) bind(c)
            import :: c_int, c_ptr
            integer(c_int), value :: w
            type(c_ptr), value    :: allocator
            type(c_ptr) :: ncnn_mat_create_1d
        end function

        function ncnn_mat_create_2d(w, h, allocator) bind(c)
            import :: c_int, c_ptr
            integer(c_int), value :: w
            integer(c_int), value :: h
            type(c_ptr), value    :: allocator
            type(c_ptr) :: ncnn_mat_create_2d
        end function

        function ncnn_mat_create_3d(w, h, c, allocator) bind(c)
            import :: c_int, c_ptr
            integer(c_int), value :: w
            integer(c_int), value :: h
            integer(c_int), value :: c
            type(c_ptr), value    :: allocator
            type(c_ptr) :: ncnn_mat_create_3d
        end function

        function ncnn_mat_create_4d(w, h, d, c, allocator) bind(c)
            import :: c_int, c_ptr
            integer(c_int), value :: w
            integer(c_int), value :: h
            integer(c_int), value :: d
            integer(c_int), value :: c
            type(c_ptr), value    :: allocator
            type(c_ptr) :: ncnn_mat_create_4d
        end function

        function ncnn_mat_create_1d_batch(w, n, allocator) bind(c)
            import :: c_int, c_ptr
            integer(c_int), value :: w
            integer(c_int), value :: n
            type(c_ptr), value    :: allocator
            type(c_ptr) :: ncnn_mat_create_1d_batch
        end function

        function ncnn_mat_create_2d_batch(w, h, n, allocator) bind(c)
            import :: c_int, c_ptr
            integer(c_int), value :: w
            integer(c_int), value :: h
            integer(c_int), value :: n
            type(c_ptr), value    :: allocator
            type(c_ptr) :: ncnn_mat_create_2d_batch
        end function

        function ncnn_mat_create_3d_batch(w, h, c, n, allocator) bind(c)
            import :: c_int, c_ptr
            integer(c_int), value :: w
            integer(c_int), value :: h
            integer(c_int), value :: c
            integer(c_int), value :: n
            type(c_ptr), value    :: allocator
            type(c_ptr) :: ncnn_mat_create_3d_batch
        end function

        function ncnn_mat_create_4d_batch(w, h, d, c, n, allocator) bind(c)
            import :: c_int, c_ptr
            integer(c_int), value :: w
            integer(c_int), value :: h
            integer(c_int), value :: d
            integer(c_int), value :: c
            integer(c_int), value :: n
            type(c_ptr), value    :: allocator
            type(c_ptr) :: ncnn_mat_create_4d_batch
        end function

        function ncnn_mat_create_external_1d(w, data, allocator) bind(c)
            import :: c_int, c_float, c_ptr
            integer(c_int), value :: w
            real(c_float)         :: data(w)
            type(c_ptr), value    :: allocator
            type(c_ptr) :: ncnn_mat_create_external_1d
        end function

        function ncnn_mat_create_external_2d(w, h, data, allocator) bind(c)
            import :: c_int, c_float, c_ptr
            integer(c_int), value :: w
            integer(c_int), value :: h
            real(c_float)         :: data(w * h)
            type(c_ptr), value    :: allocator
            type(c_ptr) :: ncnn_mat_create_external_2d
        end function

        function ncnn_mat_create_external_3d(w, h, c, data, allocator) bind(c)
            import :: c_int, c_float, c_ptr
            integer(c_int), value :: w
            integer(c_int), value :: h
            integer(c_int), value :: c
            real(c_float)         :: data(w * h * c)
            type(c_ptr), value    :: allocator
            type(c_ptr) :: ncnn_mat_create_external_3d
        end function

        function ncnn_mat_create_external_4d(w, h, d, c, data, allocator) bind(c)
            import :: c_int, c_float, c_ptr
            integer(c_int), value :: w
            integer(c_int), value :: h
            integer(c_int), value :: d
            integer(c_int), value :: c
            real(c_float)         :: data(w * h * d * c)
            type(c_ptr), value    :: allocator
            type(c_ptr) :: ncnn_mat_create_external_4d
        end function

        function ncnn_mat_create_1d_elem(w, elemsize, elempack, allocator) bind(c)
            import :: c_int, c_size_t, c_ptr
            integer(c_int), value    :: w
            integer(c_size_t), value :: elemsize
            integer(c_int), value    :: elempack
            type(c_ptr), value       :: allocator
            type(c_ptr) :: ncnn_mat_create_1d_elem
        end function

        function ncnn_mat_create_2d_elem(w, h, elemsize, elempack, allocator) bind(c)
            import :: c_int, c_size_t, c_ptr
            integer(c_int), value    :: w
            integer(c_int), value    :: h
            integer(c_size_t), value :: elemsize
            integer(c_int), value    :: elempack
            type(c_ptr), value       :: allocator
            type(c_ptr) :: ncnn_mat_create_2d_elem
        end function

        function ncnn_mat_create_3d_elem(w, h, c, elemsize, elempack, allocator) bind(c)
            import :: c_int, c_size_t, c_ptr
            integer(c_int), value    :: w
            integer(c_int), value    :: h
            integer(c_int), value    :: c
            integer(c_size_t), value :: elemsize
            integer(c_int), value    :: elempack
            type(c_ptr), value       :: allocator
            type(c_ptr) :: ncnn_mat_create_3d_elem
        end function

        function ncnn_mat_create_4d_elem(w, h, d, c, elemsize, elempack, allocator) bind(c)
            import :: c_int, c_size_t, c_ptr
            integer(c_int), value    :: w
            integer(c_int), value    :: h
            integer(c_int), value    :: d
            integer(c_int), value    :: c
            integer(c_size_t), value :: elemsize
            integer(c_int), value    :: elempack
            type(c_ptr), value       :: allocator
            type(c_ptr) :: ncnn_mat_create_4d_elem
        end function

        function ncnn_mat_create_1d_elem_batch(w, elemsize, elempack, n, allocator) bind(c)
            import :: c_int, c_size_t, c_ptr
            integer(c_int), value    :: w
            integer(c_size_t), value :: elemsize
            integer(c_int), value    :: elempack
            integer(c_int), value    :: n
            type(c_ptr), value       :: allocator
            type(c_ptr) :: ncnn_mat_create_1d_elem_batch
        end function

        function ncnn_mat_create_2d_elem_batch(w, h, elemsize, elempack, n, allocator) bind(c)
            import :: c_int, c_size_t, c_ptr
            integer(c_int), value    :: w
            integer(c_int), value    :: h
            integer(c_size_t), value :: elemsize
            integer(c_int), value    :: elempack
            integer(c_int), value    :: n
            type(c_ptr), value       :: allocator
            type(c_ptr) :: ncnn_mat_create_2d_elem_batch
        end function

        function ncnn_mat_create_3d_elem_batch(w, h, c, elemsize, elempack, n, allocator) bind(c)
            import :: c_int, c_size_t, c_ptr
            integer(c_int), value    :: w
            integer(c_int), value    :: h
            integer(c_int), value    :: c
            integer(c_size_t), value :: elemsize
            integer(c_int), value    :: elempack
            integer(c_int), value    :: n
            type(c_ptr), value       :: allocator
            type(c_ptr) :: ncnn_mat_create_3d_elem_batch
        end function

        function ncnn_mat_create_4d_elem_batch(w, h, d, c, elemsize, elempack, n, allocator) bind(c)
            import :: c_int, c_size_t, c_ptr
            integer(c_int), value    :: w
            integer(c_int), value    :: h
            integer(c_int), value    :: d
            integer(c_int), value    :: c
            integer(c_size_t), value :: elemsize
            integer(c_int), value    :: elempack
            integer(c_int), value    :: n
            type(c_ptr), value       :: allocator
            type(c_ptr) :: ncnn_mat_create_4d_elem_batch
        end function

        ! external + elem: element type is dynamic, so data is passed as a raw
        ! pointer obtained from c_loc()
        function ncnn_mat_create_external_1d_elem(w, data, elemsize, elempack, allocator) bind(c)
            import :: c_int, c_size_t, c_ptr
            integer(c_int), value    :: w
            type(c_ptr), value       :: data
            integer(c_size_t), value :: elemsize
            integer(c_int), value    :: elempack
            type(c_ptr), value       :: allocator
            type(c_ptr) :: ncnn_mat_create_external_1d_elem
        end function

        function ncnn_mat_create_external_2d_elem(w, h, data, elemsize, elempack, allocator) bind(c)
            import :: c_int, c_size_t, c_ptr
            integer(c_int), value    :: w
            integer(c_int), value    :: h
            type(c_ptr), value       :: data
            integer(c_size_t), value :: elemsize
            integer(c_int), value    :: elempack
            type(c_ptr), value       :: allocator
            type(c_ptr) :: ncnn_mat_create_external_2d_elem
        end function

        function ncnn_mat_create_external_3d_elem(w, h, c, data, elemsize, elempack, allocator) bind(c)
            import :: c_int, c_size_t, c_ptr
            integer(c_int), value    :: w
            integer(c_int), value    :: h
            integer(c_int), value    :: c
            type(c_ptr), value       :: data
            integer(c_size_t), value :: elemsize
            integer(c_int), value    :: elempack
            type(c_ptr), value       :: allocator
            type(c_ptr) :: ncnn_mat_create_external_3d_elem
        end function

        function ncnn_mat_create_external_4d_elem(w, h, d, c, data, elemsize, elempack, allocator) bind(c)
            import :: c_int, c_size_t, c_ptr
            integer(c_int), value    :: w
            integer(c_int), value    :: h
            integer(c_int), value    :: d
            integer(c_int), value    :: c
            type(c_ptr), value       :: data
            integer(c_size_t), value :: elemsize
            integer(c_int), value    :: elempack
            type(c_ptr), value       :: allocator
            type(c_ptr) :: ncnn_mat_create_external_4d_elem
        end function

        subroutine ncnn_mat_destroy(mat) bind(c)
            import :: c_ptr
            type(c_ptr), value :: mat
        end subroutine

        subroutine ncnn_mat_fill_float(mat, v) bind(c)
            import :: c_ptr, c_float
            type(c_ptr), value   :: mat
            real(c_float), value :: v
        end subroutine

        function ncnn_mat_clone(mat, allocator) bind(c)
            import :: c_ptr
            type(c_ptr), value :: mat
            type(c_ptr), value :: allocator
            type(c_ptr) :: ncnn_mat_clone
        end function

        function ncnn_mat_reshape_1d(mat, w, allocator) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: mat
            integer(c_int), value :: w
            type(c_ptr), value    :: allocator
            type(c_ptr) :: ncnn_mat_reshape_1d
        end function

        function ncnn_mat_reshape_2d(mat, w, h, allocator) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: mat
            integer(c_int), value :: w
            integer(c_int), value :: h
            type(c_ptr), value    :: allocator
            type(c_ptr) :: ncnn_mat_reshape_2d
        end function

        function ncnn_mat_reshape_3d(mat, w, h, c, allocator) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: mat
            integer(c_int), value :: w
            integer(c_int), value :: h
            integer(c_int), value :: c
            type(c_ptr), value    :: allocator
            type(c_ptr) :: ncnn_mat_reshape_3d
        end function

        function ncnn_mat_reshape_4d(mat, w, h, d, c, allocator) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: mat
            integer(c_int), value :: w
            integer(c_int), value :: h
            integer(c_int), value :: d
            integer(c_int), value :: c
            type(c_ptr), value    :: allocator
            type(c_ptr) :: ncnn_mat_reshape_4d
        end function

        function ncnn_mat_get_dims(mat) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: mat
            integer(c_int) :: ncnn_mat_get_dims
        end function

        function ncnn_mat_get_w(mat) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: mat
            integer(c_int) :: ncnn_mat_get_w
        end function

        function ncnn_mat_get_h(mat) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: mat
            integer(c_int) :: ncnn_mat_get_h
        end function

        function ncnn_mat_get_d(mat) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: mat
            integer(c_int) :: ncnn_mat_get_d
        end function

        function ncnn_mat_get_c(mat) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: mat
            integer(c_int) :: ncnn_mat_get_c
        end function

        function ncnn_mat_get_n(mat) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: mat
            integer(c_int) :: ncnn_mat_get_n
        end function

        function ncnn_mat_get_elemsize(mat) bind(c)
            import :: c_ptr, c_size_t
            type(c_ptr), value :: mat
            integer(c_size_t) :: ncnn_mat_get_elemsize
        end function

        function ncnn_mat_get_elempack(mat) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: mat
            integer(c_int) :: ncnn_mat_get_elempack
        end function

        function ncnn_mat_get_cstep(mat) bind(c)
            import :: c_ptr, c_size_t
            type(c_ptr), value :: mat
            integer(c_size_t) :: ncnn_mat_get_cstep
        end function

        function ncnn_mat_get_nstep(mat) bind(c)
            import :: c_ptr, c_size_t
            type(c_ptr), value :: mat
            integer(c_size_t) :: ncnn_mat_get_nstep
        end function

        function ncnn_mat_get_data(mat) bind(c)
            import :: c_ptr
            type(c_ptr), value :: mat
            type(c_ptr) :: ncnn_mat_get_data
        end function

        function ncnn_mat_get_batch_data(mat, b) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: mat
            integer(c_int), value :: b
            type(c_ptr) :: ncnn_mat_get_batch_data
        end function

        function ncnn_mat_get_channel_data(mat, c) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: mat
            integer(c_int), value :: c
            type(c_ptr) :: ncnn_mat_get_channel_data
        end function

        ! assumed-size: the C side takes a bare const float* with no count
        subroutine ncnn_mat_substract_mean_normalize(mat, mean_vals, norm_vals) bind(c)
            import :: c_ptr, c_float
            type(c_ptr), value :: mat
            real(c_float)      :: mean_vals(*)
            real(c_float)      :: norm_vals(*)
        end subroutine

        subroutine ncnn_convert_packing(src, dst, elempack, opt) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: src
            type(c_ptr)           :: dst
            integer(c_int), value :: elempack
            type(c_ptr), value    :: opt
        end subroutine

        subroutine ncnn_flatten(src, dst, opt) bind(c)
            import :: c_ptr
            type(c_ptr), value :: src
            type(c_ptr)        :: dst
            type(c_ptr), value :: opt
        end subroutine

        subroutine ncnn_copy_make_border(src, dst, top, bottom, left, right, type, v, opt) bind(c)
            import :: c_int, c_float, c_ptr
            type(c_ptr), value    :: src
            type(c_ptr), value    :: dst
            integer(c_int), value :: top
            integer(c_int), value :: bottom
            integer(c_int), value :: left
            integer(c_int), value :: right
            integer(c_int), value :: type
            real(c_float), value  :: v
            type(c_ptr), value    :: opt
        end subroutine

        subroutine ncnn_copy_make_border_3d(src, dst, top, bottom, left, right, front, behind, type, v, opt) bind(c)
            import :: c_int, c_float, c_ptr
            type(c_ptr), value    :: src
            type(c_ptr), value    :: dst
            integer(c_int), value :: top
            integer(c_int), value :: bottom
            integer(c_int), value :: left
            integer(c_int), value :: right
            integer(c_int), value :: front
            integer(c_int), value :: behind
            integer(c_int), value :: type
            real(c_float), value  :: v
            type(c_ptr), value    :: opt
        end subroutine

        subroutine ncnn_copy_cut_border(src, dst, top, bottom, left, right, opt) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: src
            type(c_ptr), value    :: dst
            integer(c_int), value :: top
            integer(c_int), value :: bottom
            integer(c_int), value :: left
            integer(c_int), value :: right
            type(c_ptr), value    :: opt
        end subroutine

        subroutine ncnn_copy_cut_border_3d(src, dst, top, bottom, left, right, front, behind, opt) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: src
            type(c_ptr), value    :: dst
            integer(c_int), value :: top
            integer(c_int), value :: bottom
            integer(c_int), value :: left
            integer(c_int), value :: right
            integer(c_int), value :: front
            integer(c_int), value :: behind
            type(c_ptr), value    :: opt
        end subroutine

    end interface

contains

    !--------------------------------------------------------------------------
    ! create
    !--------------------------------------------------------------------------

    !> ncnn_mat_t ncnn_mat_create()
    function mat_create() result(mat)
        type(ncnn_mat_t) :: mat
        mat%ptr = ncnn_mat_create()
    end function mat_create

    !> ncnn_mat_t ncnn_mat_create_1d(int w, ncnn_allocator_t allocator)
    function mat_create_1d(w, allocator) result(mat)
        integer, intent(in)                :: w
        type(ncnn_allocator_t), intent(in)   :: allocator
        type(ncnn_mat_t)                     :: mat
        mat%ptr = ncnn_mat_create_1d(w, allocator%ptr)
    end function mat_create_1d

    !> ncnn_mat_t ncnn_mat_create_2d(int w, int h, ncnn_allocator_t allocator)
    function mat_create_2d(w, h, allocator) result(mat)
        integer, intent(in)                :: w
        integer, intent(in)                :: h
        type(ncnn_allocator_t), intent(in)   :: allocator
        type(ncnn_mat_t)                     :: mat
        mat%ptr = ncnn_mat_create_2d(w, h, allocator%ptr)
    end function mat_create_2d

    !> ncnn_mat_t ncnn_mat_create_3d(int w, int h, int c, ncnn_allocator_t allocator)
    function mat_create_3d(w, h, c, allocator) result(mat)
        integer, intent(in)                :: w
        integer, intent(in)                :: h
        integer, intent(in)                :: c
        type(ncnn_allocator_t), intent(in)   :: allocator
        type(ncnn_mat_t)                     :: mat
        mat%ptr = ncnn_mat_create_3d(w, h, c, allocator%ptr)
    end function mat_create_3d

    !> ncnn_mat_t ncnn_mat_create_4d(int w, int h, int d, int c, ncnn_allocator_t allocator)
    function mat_create_4d(w, h, d, c, allocator) result(mat)
        integer, intent(in)                :: w
        integer, intent(in)                :: h
        integer, intent(in)                :: d
        integer, intent(in)                :: c
        type(ncnn_allocator_t), intent(in)   :: allocator
        type(ncnn_mat_t)                     :: mat
        mat%ptr = ncnn_mat_create_4d(w, h, d, c, allocator%ptr)
    end function mat_create_4d

    !> ncnn_mat_t ncnn_mat_create_1d_batch(int w, int n, ncnn_allocator_t allocator)
    function mat_create_1d_batch(w, n, allocator) result(mat)
        integer, intent(in)                :: w
        integer, intent(in)                :: n
        type(ncnn_allocator_t), intent(in)   :: allocator
        type(ncnn_mat_t)                     :: mat
        mat%ptr = ncnn_mat_create_1d_batch(w, n, allocator%ptr)
    end function mat_create_1d_batch

    !> ncnn_mat_t ncnn_mat_create_2d_batch(int w, int h, int n, ncnn_allocator_t allocator)
    function mat_create_2d_batch(w, h, n, allocator) result(mat)
        integer, intent(in)                :: w
        integer, intent(in)                :: h
        integer, intent(in)                :: n
        type(ncnn_allocator_t), intent(in)   :: allocator
        type(ncnn_mat_t)                     :: mat
        mat%ptr = ncnn_mat_create_2d_batch(w, h, n, allocator%ptr)
    end function mat_create_2d_batch

    !> ncnn_mat_t ncnn_mat_create_3d_batch(int w, int h, int c, int n, ncnn_allocator_t allocator)
    function mat_create_3d_batch(w, h, c, n, allocator) result(mat)
        integer, intent(in)                :: w
        integer, intent(in)                :: h
        integer, intent(in)                :: c
        integer, intent(in)                :: n
        type(ncnn_allocator_t), intent(in)   :: allocator
        type(ncnn_mat_t)                     :: mat
        mat%ptr = ncnn_mat_create_3d_batch(w, h, c, n, allocator%ptr)
    end function mat_create_3d_batch

    !> ncnn_mat_t ncnn_mat_create_4d_batch(int w, int h, int d, int c, int n, ncnn_allocator_t allocator)
    function mat_create_4d_batch(w, h, d, c, n, allocator) result(mat)
        integer, intent(in)                :: w
        integer, intent(in)                :: h
        integer, intent(in)                :: d
        integer, intent(in)                :: c
        integer, intent(in)                :: n
        type(ncnn_allocator_t), intent(in)   :: allocator
        type(ncnn_mat_t)                     :: mat
        mat%ptr = ncnn_mat_create_4d_batch(w, h, d, c, n, allocator%ptr)
    end function mat_create_4d_batch

    !--------------------------------------------------------------------------
    ! create external -- elemsize is fixed at 4 (float32), so the Fortran side
    ! can take a real(c_float) array directly.
    !
    ! WARNING: the returned Mat does NOT own data. data must be a named,
    ! contiguous array variable whose lifetime covers the Mat's. Two ways to
    ! get a dangling pointer:
    !   - a non-contiguous section such as a(1:100:2) -- the compiler silently
    !     materialises a temporary and ncnn keeps the temporary's address
    !   - an expression such as reshape(...) or a(*) + 1 -- the temporary dies
    !     at the end of the statement, so the Mat is dangling immediately
    !--------------------------------------------------------------------------

    !> ncnn_mat_t ncnn_mat_create_external_1d(int w, void* data, ncnn_allocator_t allocator)
    function mat_create_external_1d(w, data, allocator) result(mat)
        integer, intent(in)                :: w
        real(c_float), intent(in)          :: data(w)
        type(ncnn_allocator_t), intent(in)   :: allocator
        type(ncnn_mat_t)                     :: mat
        mat%ptr = ncnn_mat_create_external_1d(w, data, allocator%ptr)
    end function mat_create_external_1d

    !> ncnn_mat_t ncnn_mat_create_external_2d(int w, int h, void* data, ncnn_allocator_t allocator)
    function mat_create_external_2d(w, h, data, allocator) result(mat)
        integer, intent(in)                :: w
        integer, intent(in)                :: h
        real(c_float), intent(in)          :: data(w * h)
        type(ncnn_allocator_t), intent(in)   :: allocator
        type(ncnn_mat_t)                     :: mat
        mat%ptr = ncnn_mat_create_external_2d(w, h, data, allocator%ptr)
    end function mat_create_external_2d

    !> ncnn_mat_t ncnn_mat_create_external_3d(int w, int h, int c, void* data, ncnn_allocator_t allocator)
    function mat_create_external_3d(w, h, c, data, allocator) result(mat)
        integer, intent(in)                :: w
        integer, intent(in)                :: h
        integer, intent(in)                :: c
        real(c_float), intent(in)          :: data(w * h * c)
        type(ncnn_allocator_t), intent(in)   :: allocator
        type(ncnn_mat_t)                     :: mat
        mat%ptr = ncnn_mat_create_external_3d(w, h, c, data, allocator%ptr)
    end function mat_create_external_3d

    !> ncnn_mat_t ncnn_mat_create_external_4d(int w, int h, int d, int c, void* data, ncnn_allocator_t allocator)
    function mat_create_external_4d(w, h, d, c, data, allocator) result(mat)
        integer, intent(in)                :: w
        integer, intent(in)                :: h
        integer, intent(in)                :: d
        integer, intent(in)                :: c
        real(c_float), intent(in)          :: data(w * h * d * c)
        type(ncnn_allocator_t), intent(in)   :: allocator
        type(ncnn_mat_t)                     :: mat
        mat%ptr = ncnn_mat_create_external_4d(w, h, d, c, data, allocator%ptr)
    end function mat_create_external_4d

    !--------------------------------------------------------------------------
    ! create elem -- elemsize/elempack are runtime values
    !--------------------------------------------------------------------------

    !> ncnn_mat_t ncnn_mat_create_1d_elem(int w, size_t elemsize, int elempack, ncnn_allocator_t allocator)
    function mat_create_1d_elem(w, elemsize, elempack, allocator) result(mat)
        integer, intent(in)                :: w
        integer(c_size_t), intent(in)      :: elemsize
        integer, intent(in)                :: elempack
        type(ncnn_allocator_t), intent(in)   :: allocator
        type(ncnn_mat_t)                     :: mat
        mat%ptr = ncnn_mat_create_1d_elem(w, elemsize, elempack, allocator%ptr)
    end function mat_create_1d_elem

    !> ncnn_mat_t ncnn_mat_create_2d_elem(int w, int h, size_t elemsize, int elempack, ncnn_allocator_t allocator)
    function mat_create_2d_elem(w, h, elemsize, elempack, allocator) result(mat)
        integer, intent(in)                :: w
        integer, intent(in)                :: h
        integer(c_size_t), intent(in)      :: elemsize
        integer, intent(in)                :: elempack
        type(ncnn_allocator_t), intent(in)   :: allocator
        type(ncnn_mat_t)                     :: mat
        mat%ptr = ncnn_mat_create_2d_elem(w, h, elemsize, elempack, allocator%ptr)
    end function mat_create_2d_elem

    !> ncnn_mat_t ncnn_mat_create_3d_elem(int w, int h, int c, size_t elemsize, int elempack, ncnn_allocator_t allocator)
    function mat_create_3d_elem(w, h, c, elemsize, elempack, allocator) result(mat)
        integer, intent(in)                :: w
        integer, intent(in)                :: h
        integer, intent(in)                :: c
        integer(c_size_t), intent(in)      :: elemsize
        integer, intent(in)                :: elempack
        type(ncnn_allocator_t), intent(in)   :: allocator
        type(ncnn_mat_t)                     :: mat
        mat%ptr = ncnn_mat_create_3d_elem(w, h, c, elemsize, elempack, allocator%ptr)
    end function mat_create_3d_elem

    !> ncnn_mat_t ncnn_mat_create_4d_elem(int w, int h, int d, int c, size_t elemsize, int elempack, ncnn_allocator_t allocator)
    function mat_create_4d_elem(w, h, d, c, elemsize, elempack, allocator) result(mat)
        integer, intent(in)                :: w
        integer, intent(in)                :: h
        integer, intent(in)                :: d
        integer, intent(in)                :: c
        integer(c_size_t), intent(in)      :: elemsize
        integer, intent(in)                :: elempack
        type(ncnn_allocator_t), intent(in)   :: allocator
        type(ncnn_mat_t)                     :: mat
        mat%ptr = ncnn_mat_create_4d_elem(w, h, d, c, elemsize, elempack, allocator%ptr)
    end function mat_create_4d_elem

    !> ncnn_mat_t ncnn_mat_create_1d_elem_batch(int w, size_t elemsize, int elempack, int n, ncnn_allocator_t allocator)
    function mat_create_1d_elem_batch(w, elemsize, elempack, n, allocator) result(mat)
        integer, intent(in)                :: w
        integer(c_size_t), intent(in)      :: elemsize
        integer, intent(in)                :: elempack
        integer, intent(in)                :: n
        type(ncnn_allocator_t), intent(in)   :: allocator
        type(ncnn_mat_t)                     :: mat
        mat%ptr = ncnn_mat_create_1d_elem_batch(w, elemsize, elempack, n, allocator%ptr)
    end function mat_create_1d_elem_batch

    !> ncnn_mat_t ncnn_mat_create_2d_elem_batch(int w, int h, size_t elemsize, int elempack, int n, ncnn_allocator_t allocator)
    function mat_create_2d_elem_batch(w, h, elemsize, elempack, n, allocator) result(mat)
        integer, intent(in)                :: w
        integer, intent(in)                :: h
        integer(c_size_t), intent(in)      :: elemsize
        integer, intent(in)                :: elempack
        integer, intent(in)                :: n
        type(ncnn_allocator_t), intent(in)   :: allocator
        type(ncnn_mat_t)                     :: mat
        mat%ptr = ncnn_mat_create_2d_elem_batch(w, h, elemsize, elempack, n, allocator%ptr)
    end function mat_create_2d_elem_batch

    !> ncnn_mat_t ncnn_mat_create_3d_elem_batch(int w, int h, int c, size_t elemsize, int elempack, int n, ncnn_allocator_t allocator)
    function mat_create_3d_elem_batch(w, h, c, elemsize, elempack, n, allocator) result(mat)
        integer, intent(in)                :: w
        integer, intent(in)                :: h
        integer, intent(in)                :: c
        integer(c_size_t), intent(in)      :: elemsize
        integer, intent(in)                :: elempack
        integer, intent(in)                :: n
        type(ncnn_allocator_t), intent(in)   :: allocator
        type(ncnn_mat_t)                     :: mat
        mat%ptr = ncnn_mat_create_3d_elem_batch(w, h, c, elemsize, elempack, n, allocator%ptr)
    end function mat_create_3d_elem_batch

    !> ncnn_mat_t ncnn_mat_create_4d_elem_batch(int w, int h, int d, int c, size_t elemsize, int elempack, int n, ncnn_allocator_t allocator)
    function mat_create_4d_elem_batch(w, h, d, c, elemsize, elempack, n, allocator) result(mat)
        integer, intent(in)                :: w
        integer, intent(in)                :: h
        integer, intent(in)                :: d
        integer, intent(in)                :: c
        integer(c_size_t), intent(in)      :: elemsize
        integer, intent(in)                :: elempack
        integer, intent(in)                :: n
        type(ncnn_allocator_t), intent(in)   :: allocator
        type(ncnn_mat_t)                     :: mat
        mat%ptr = ncnn_mat_create_4d_elem_batch(w, h, d, c, elemsize, elempack, n, allocator%ptr)
    end function mat_create_4d_elem_batch

    !--------------------------------------------------------------------------
    ! create external elem -- element type is dynamic, so data comes from c_loc.
    ! Same lifetime/contiguity warning as the plain external variants.
    !--------------------------------------------------------------------------

    !> ncnn_mat_t ncnn_mat_create_external_1d_elem(int w, void* data, size_t elemsize, int elempack, ncnn_allocator_t allocator)
    function mat_create_external_1d_elem(w, data, elemsize, elempack, allocator) result(mat)
        integer, intent(in)                :: w
        type(c_ptr), intent(in), value     :: data
        integer(c_size_t), intent(in)      :: elemsize
        integer, intent(in)                :: elempack
        type(ncnn_allocator_t), intent(in)   :: allocator
        type(ncnn_mat_t)                     :: mat
        mat%ptr = ncnn_mat_create_external_1d_elem(w, data, elemsize, elempack, allocator%ptr)
    end function mat_create_external_1d_elem

    !> ncnn_mat_t ncnn_mat_create_external_2d_elem(int w, int h, void* data, size_t elemsize, int elempack, ncnn_allocator_t allocator)
    function mat_create_external_2d_elem(w, h, data, elemsize, elempack, allocator) result(mat)
        integer, intent(in)                :: w
        integer, intent(in)                :: h
        type(c_ptr), intent(in), value     :: data
        integer(c_size_t), intent(in)      :: elemsize
        integer, intent(in)                :: elempack
        type(ncnn_allocator_t), intent(in)   :: allocator
        type(ncnn_mat_t)                     :: mat
        mat%ptr = ncnn_mat_create_external_2d_elem(w, h, data, elemsize, elempack, allocator%ptr)
    end function mat_create_external_2d_elem

    !> ncnn_mat_t ncnn_mat_create_external_3d_elem(int w, int h, int c, void* data, size_t elemsize, int elempack, ncnn_allocator_t allocator)
    function mat_create_external_3d_elem(w, h, c, data, elemsize, elempack, allocator) result(mat)
        integer, intent(in)                :: w
        integer, intent(in)                :: h
        integer, intent(in)                :: c
        type(c_ptr), intent(in), value     :: data
        integer(c_size_t), intent(in)      :: elemsize
        integer, intent(in)                :: elempack
        type(ncnn_allocator_t), intent(in)   :: allocator
        type(ncnn_mat_t)                     :: mat
        mat%ptr = ncnn_mat_create_external_3d_elem(w, h, c, data, elemsize, elempack, allocator%ptr)
    end function mat_create_external_3d_elem

    !> ncnn_mat_t ncnn_mat_create_external_4d_elem(int w, int h, int d, int c, void* data, size_t elemsize, int elempack, ncnn_allocator_t allocator)
    function mat_create_external_4d_elem(w, h, d, c, data, elemsize, elempack, allocator) result(mat)
        integer, intent(in)                :: w
        integer, intent(in)                :: h
        integer, intent(in)                :: d
        integer, intent(in)                :: c
        type(c_ptr), intent(in), value     :: data
        integer(c_size_t), intent(in)      :: elemsize
        integer, intent(in)                :: elempack
        type(ncnn_allocator_t), intent(in)   :: allocator
        type(ncnn_mat_t)                     :: mat
        mat%ptr = ncnn_mat_create_external_4d_elem(w, h, d, c, data, elemsize, elempack, allocator%ptr)
    end function mat_create_external_4d_elem

    !--------------------------------------------------------------------------
    ! destroy / fill / clone / reshape
    !--------------------------------------------------------------------------

    !> void ncnn_mat_destroy(ncnn_mat_t mat)
    subroutine mat_destroy(mat)
        type(ncnn_mat_t), intent(inout) :: mat
        call ncnn_mat_destroy(mat%ptr)
        mat%ptr = c_null_ptr
    end subroutine mat_destroy

    !> void ncnn_mat_fill_float(ncnn_mat_t mat, float v)
    subroutine mat_fill_float(mat, v)
        type(ncnn_mat_t), intent(inout) :: mat
        real(c_float), intent(in)     :: v
        call ncnn_mat_fill_float(mat%ptr, v)
    end subroutine mat_fill_float

    !> ncnn_mat_t ncnn_mat_clone(const ncnn_mat_t mat, ncnn_allocator_t allocator)
    function mat_clone(mat, allocator) result(new_mat)
        type(ncnn_mat_t), intent(in)       :: mat
        type(ncnn_allocator_t), intent(in) :: allocator
        type(ncnn_mat_t)                   :: new_mat
        new_mat%ptr = ncnn_mat_clone(mat%ptr, allocator%ptr)
    end function mat_clone

    !> ncnn_mat_t ncnn_mat_reshape_1d(const ncnn_mat_t mat, int w, ncnn_allocator_t allocator)
    !> The result shares storage with mat and must be destroyed in its own right.
    function mat_reshape_1d(mat, w, allocator) result(new_mat)
        type(ncnn_mat_t), intent(in)       :: mat
        integer, intent(in)              :: w
        type(ncnn_allocator_t), intent(in) :: allocator
        type(ncnn_mat_t)                   :: new_mat
        new_mat%ptr = ncnn_mat_reshape_1d(mat%ptr, w, allocator%ptr)
    end function mat_reshape_1d

    !> ncnn_mat_t ncnn_mat_reshape_2d(const ncnn_mat_t mat, int w, int h, ncnn_allocator_t allocator)
    function mat_reshape_2d(mat, w, h, allocator) result(new_mat)
        type(ncnn_mat_t), intent(in)       :: mat
        integer, intent(in)              :: w
        integer, intent(in)              :: h
        type(ncnn_allocator_t), intent(in) :: allocator
        type(ncnn_mat_t)                   :: new_mat
        new_mat%ptr = ncnn_mat_reshape_2d(mat%ptr, w, h, allocator%ptr)
    end function mat_reshape_2d

    !> ncnn_mat_t ncnn_mat_reshape_3d(const ncnn_mat_t mat, int w, int h, int c, ncnn_allocator_t allocator)
    function mat_reshape_3d(mat, w, h, c, allocator) result(new_mat)
        type(ncnn_mat_t), intent(in)       :: mat
        integer, intent(in)              :: w
        integer, intent(in)              :: h
        integer, intent(in)              :: c
        type(ncnn_allocator_t), intent(in) :: allocator
        type(ncnn_mat_t)                   :: new_mat
        new_mat%ptr = ncnn_mat_reshape_3d(mat%ptr, w, h, c, allocator%ptr)
    end function mat_reshape_3d

    !> ncnn_mat_t ncnn_mat_reshape_4d(const ncnn_mat_t mat, int w, int h, int d, int c, ncnn_allocator_t allocator)
    function mat_reshape_4d(mat, w, h, d, c, allocator) result(new_mat)
        type(ncnn_mat_t), intent(in)       :: mat
        integer, intent(in)              :: w
        integer, intent(in)              :: h
        integer, intent(in)              :: d
        integer, intent(in)              :: c
        type(ncnn_allocator_t), intent(in) :: allocator
        type(ncnn_mat_t)                   :: new_mat
        new_mat%ptr = ncnn_mat_reshape_4d(mat%ptr, w, h, d, c, allocator%ptr)
    end function mat_reshape_4d

    !--------------------------------------------------------------------------
    ! accessors
    !--------------------------------------------------------------------------

    !> int ncnn_mat_get_dims(const ncnn_mat_t mat)
    function mat_get_dims(mat) result(dims)
        type(ncnn_mat_t), intent(in) :: mat
        integer :: dims
        dims = ncnn_mat_get_dims(mat%ptr)
    end function mat_get_dims

    !> int ncnn_mat_get_w(const ncnn_mat_t mat)
    function mat_get_w(mat) result(w)
        type(ncnn_mat_t), intent(in) :: mat
        integer :: w
        w = ncnn_mat_get_w(mat%ptr)
    end function mat_get_w

    !> int ncnn_mat_get_h(const ncnn_mat_t mat)
    function mat_get_h(mat) result(h)
        type(ncnn_mat_t), intent(in) :: mat
        integer :: h
        h = ncnn_mat_get_h(mat%ptr)
    end function mat_get_h

    !> int ncnn_mat_get_d(const ncnn_mat_t mat)
    function mat_get_d(mat) result(d)
        type(ncnn_mat_t), intent(in) :: mat
        integer :: d
        d = ncnn_mat_get_d(mat%ptr)
    end function mat_get_d

    !> int ncnn_mat_get_c(const ncnn_mat_t mat)
    function mat_get_c(mat) result(c)
        type(ncnn_mat_t), intent(in) :: mat
        integer :: c
        c = ncnn_mat_get_c(mat%ptr)
    end function mat_get_c

    !> int ncnn_mat_get_n(const ncnn_mat_t mat)
    function mat_get_n(mat) result(n)
        type(ncnn_mat_t), intent(in) :: mat
        integer :: n
        n = ncnn_mat_get_n(mat%ptr)
    end function mat_get_n

    !> size_t ncnn_mat_get_elemsize(const ncnn_mat_t mat)
    function mat_get_elemsize(mat) result(elemsize)
        type(ncnn_mat_t), intent(in) :: mat
        integer(c_size_t) :: elemsize
        elemsize = ncnn_mat_get_elemsize(mat%ptr)
    end function mat_get_elemsize

    !> int ncnn_mat_get_elempack(const ncnn_mat_t mat)
    function mat_get_elempack(mat) result(elempack)
        type(ncnn_mat_t), intent(in) :: mat
        integer :: elempack
        elempack = ncnn_mat_get_elempack(mat%ptr)
    end function mat_get_elempack

    !> size_t ncnn_mat_get_cstep(const ncnn_mat_t mat)
    !> Elements per channel, not bytes.
    function mat_get_cstep(mat) result(cstep)
        type(ncnn_mat_t), intent(in) :: mat
        integer(c_size_t) :: cstep
        cstep = ncnn_mat_get_cstep(mat%ptr)
    end function mat_get_cstep

    !> size_t ncnn_mat_get_nstep(const ncnn_mat_t mat)
    !> Elements per batch image, not bytes.
    function mat_get_nstep(mat) result(nstep)
        type(ncnn_mat_t), intent(in) :: mat
        integer(c_size_t) :: nstep
        nstep = ncnn_mat_get_nstep(mat%ptr)
    end function mat_get_nstep

    !--------------------------------------------------------------------------
    ! data access
    !
    ! total elements in a Mat is cstep * c -- this is elemsize-agnostic, so the
    ! float accessors can refuse a non-fp32 Mat instead of over-reading.
    !--------------------------------------------------------------------------

    !> void* ncnn_mat_get_data(const ncnn_mat_t mat)
    !> Returns a copy of all elements as float32. The Mat must have elemsize 4.
    function mat_get_data(mat) result(data)
        type(ncnn_mat_t), intent(in)      :: mat
        real(c_float), allocatable      :: data(:)

        integer(c_size_t) :: n
        real(c_float), pointer :: p(:) => null()

        if (ncnn_mat_get_elemsize(mat%ptr) /= 4_c_size_t) then
            error stop 'ncnn: mat_get_data requires a float32 Mat (elemsize == 4); ' // &
                        'use mat_get_data_bytes for other element types'
        end if

        n = ncnn_mat_get_cstep(mat%ptr) * ncnn_mat_get_c(mat%ptr)
        allocate (data(n))
        if (n > 0) then
            call c_f_pointer(ncnn_mat_get_data(mat%ptr), p, [n])
            data = p
        end if
    end function mat_get_data

    !> Raw bytes of the Mat, any elemsize. n bytes total.
    function mat_get_data_bytes(mat) result(data)
        type(ncnn_mat_t), intent(in)      :: mat
        integer(c_int8_t), allocatable  :: data(:)

        integer(c_size_t) :: n
        integer(c_int8_t), pointer :: p(:) => null()

        n = ncnn_mat_get_cstep(mat%ptr) * ncnn_mat_get_c(mat%ptr) &
            * ncnn_mat_get_elemsize(mat%ptr)
        allocate (data(n))
        if (n > 0) then
            call c_f_pointer(ncnn_mat_get_data(mat%ptr), p, [n])
            data = p
        end if
    end function mat_get_data_bytes

    !> void* ncnn_mat_get_channel_data(const ncnn_mat_t mat, int c)
    !> c is 1-based. Returns cstep elements as float32; elemsize must be 4.
    function mat_get_channel_data(mat, c) result(data)
        type(ncnn_mat_t), intent(in)      :: mat
        integer, intent(in)             :: c
        real(c_float), allocatable      :: data(:)

        integer(c_size_t) :: n
        real(c_float), pointer :: p(:) => null()

        if (c < 1 .or. c > ncnn_mat_get_c(mat%ptr)) then
            error stop 'ncnn: mat_get_channel_data channel index out of range'
        end if

        if (ncnn_mat_get_elemsize(mat%ptr) /= 4_c_size_t) then
            error stop 'ncnn: mat_get_channel_data requires a float32 Mat (elemsize == 4)'
        end if

        n = ncnn_mat_get_cstep(mat%ptr)
        allocate (data(n))
        if (n > 0) then
            call c_f_pointer(ncnn_mat_get_channel_data(mat%ptr, c - 1), p, [n])
            data = p
        end if
    end function mat_get_channel_data

    !> void* ncnn_mat_get_batch_data(const ncnn_mat_t mat, int b)
    !> b is 1-based. Returns nstep elements as float32; elemsize must be 4.
    function mat_get_batch_data(mat, b) result(data)
        type(ncnn_mat_t), intent(in)      :: mat
        integer, intent(in)             :: b
        real(c_float), allocatable      :: data(:)

        integer(c_size_t) :: n
        real(c_float), pointer :: p(:) => null()

        if (b < 1 .or. b > ncnn_mat_get_n(mat%ptr)) then
            error stop 'ncnn: mat_get_batch_data batch index out of range'
        end if

        if (ncnn_mat_get_elemsize(mat%ptr) /= 4_c_size_t) then
            error stop 'ncnn: mat_get_batch_data requires a float32 Mat (elemsize == 4)'
        end if

        n = ncnn_mat_get_nstep(mat%ptr)
        allocate (data(n))
        if (n > 0) then
            call c_f_pointer(ncnn_mat_get_batch_data(mat%ptr, b - 1), p, [n])
            data = p
        end if
    end function mat_get_batch_data

    !> void ncnn_mat_substract_mean_normalize(ncnn_mat_t mat, const float* mean_vals, const float* norm_vals)
    !> mean_vals and norm_vals must each have at least mat_get_c(mat) elements;
    !> the C side takes a bare pointer and reads c values from each, so a short
    !> array is an out-of-bounds read rather than an error.
    subroutine mat_substract_mean_normalize(mat, mean_vals, norm_vals)
        type(ncnn_mat_t), intent(inout) :: mat
        real(c_float), intent(in)     :: mean_vals(:)
        real(c_float), intent(in)     :: norm_vals(:)

        if (size(mean_vals) < mat_get_c(mat) .or. size(norm_vals) < mat_get_c(mat)) then
            error stop 'ncnn: mat_substract_mean_normalize needs at least ' // &
                       'mat_get_c(mat) elements in mean_vals and norm_vals'
        end if

        call ncnn_mat_substract_mean_normalize(mat%ptr, mean_vals, norm_vals)
    end subroutine mat_substract_mean_normalize

    !--------------------------------------------------------------------------
    ! process
    !--------------------------------------------------------------------------

    !> void ncnn_convert_packing(const ncnn_mat_t src, ncnn_mat_t* dst, int elempack, const ncnn_option_t opt)
    !> Returns the converted Mat; destroy it when done.
    function mat_convert_packing(src, elempack, opt) result(dst)
        type(ncnn_mat_t), intent(in)       :: src
        integer, intent(in)              :: elempack
        type(ncnn_option_t), intent(in)    :: opt
        type(ncnn_mat_t)                   :: dst
        ! dst%ptr starts as c_null_ptr; ncnn_convert_packing only writes it
        call ncnn_convert_packing(src%ptr, dst%ptr, elempack, opt%ptr)
    end function mat_convert_packing

    !> void ncnn_flatten(const ncnn_mat_t src, ncnn_mat_t* dst, const ncnn_option_t opt)
    !> Returns the flattened Mat; destroy it when done.
    function mat_flatten(src, opt) result(dst)
        type(ncnn_mat_t), intent(in)       :: src
        type(ncnn_option_t), intent(in)    :: opt
        type(ncnn_mat_t)                   :: dst
        call ncnn_flatten(src%ptr, dst%ptr, opt%ptr)
    end function mat_flatten

    !> void ncnn_copy_make_border(const ncnn_mat_t src, ncnn_mat_t dst, int top, int bottom, int left, int right, int type, float v, const ncnn_option_t opt)
    !> dst must be an existing Mat (mat_create is enough); it is filled in place.
    subroutine mat_copy_make_border(src, dst, top, bottom, left, right, type, v, opt)
        type(ncnn_mat_t), intent(in)       :: src
        type(ncnn_mat_t), intent(inout)    :: dst
        integer, intent(in)              :: top
        integer, intent(in)              :: bottom
        integer, intent(in)              :: left
        integer, intent(in)              :: right
        integer, intent(in)              :: type
        real(c_float), intent(in)        :: v
        type(ncnn_option_t), intent(in)    :: opt
        call ncnn_copy_make_border(src%ptr, dst%ptr, top, bottom, left, right, type, v, opt%ptr)
    end subroutine mat_copy_make_border

    !> void ncnn_copy_make_border_3d(const ncnn_mat_t src, ncnn_mat_t dst, int top, int bottom, int left, int right, int front, int behind, int type, float v, const ncnn_option_t opt)
    subroutine mat_copy_make_border_3d(src, dst, top, bottom, left, right, front, behind, type, v, opt)
        type(ncnn_mat_t), intent(in)       :: src
        type(ncnn_mat_t), intent(inout)    :: dst
        integer, intent(in)              :: top
        integer, intent(in)              :: bottom
        integer, intent(in)              :: left
        integer, intent(in)              :: right
        integer, intent(in)              :: front
        integer, intent(in)              :: behind
        integer, intent(in)              :: type
        real(c_float), intent(in)        :: v
        type(ncnn_option_t), intent(in)    :: opt
        call ncnn_copy_make_border_3d(src%ptr, dst%ptr, top, bottom, left, right, &
                                      front, behind, type, v, opt%ptr)
    end subroutine mat_copy_make_border_3d

    !> void ncnn_copy_cut_border(const ncnn_mat_t src, ncnn_mat_t dst, int top, int bottom, int left, int right, const ncnn_option_t opt)
    subroutine mat_copy_cut_border(src, dst, top, bottom, left, right, opt)
        type(ncnn_mat_t), intent(in)       :: src
        type(ncnn_mat_t), intent(inout)    :: dst
        integer, intent(in)              :: top
        integer, intent(in)              :: bottom
        integer, intent(in)              :: left
        integer, intent(in)              :: right
        type(ncnn_option_t), intent(in)    :: opt
        call ncnn_copy_cut_border(src%ptr, dst%ptr, top, bottom, left, right, opt%ptr)
    end subroutine mat_copy_cut_border

    !> void ncnn_copy_cut_border_3d(const ncnn_mat_t src, ncnn_mat_t dst, int top, int bottom, int left, int right, int front, int behind, const ncnn_option_t opt)
    subroutine mat_copy_cut_border_3d(src, dst, top, bottom, left, right, front, behind, opt)
        type(ncnn_mat_t), intent(in)       :: src
        type(ncnn_mat_t), intent(inout)    :: dst
        integer, intent(in)              :: top
        integer, intent(in)              :: bottom
        integer, intent(in)              :: left
        integer, intent(in)              :: right
        integer, intent(in)              :: front
        integer, intent(in)              :: behind
        type(ncnn_option_t), intent(in)    :: opt
        call ncnn_copy_cut_border_3d(src%ptr, dst%ptr, top, bottom, left, right, &
                                     front, behind, opt%ptr)
    end subroutine mat_copy_cut_border_3d

end module ncnn_mat
