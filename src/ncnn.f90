!------------------------------------------------------------------------------
! MODULE: ncnn
!
!> @author
!> mizu-bai
!
! DESCRIPTION: 
!>  Fortran binding for ncnn
!
! REVISION HISTORY:
! 17 Dec 2022 - Initial Version
!------------------------------------------------------------------------------
module ncnn
    use, intrinsic :: iso_c_binding
    implicit none

    !> ncnn_allocator_t
    type :: ncnn_allocator_t
        type(c_ptr) :: ptr = c_null_ptr
    end type

    !> ncnn_option_t
    type :: ncnn_option_t
        type(c_ptr) :: ptr = c_null_ptr
    end type

    !> ncnn_mat_t
    type :: ncnn_mat_t
        type(c_ptr) :: ptr = c_null_ptr
    end type

contains

!> const char* ncnn_version()
function version()
    use iso_c_binding, only: c_char, c_f_pointer
    implicit none

    character(len=255) :: version
    character(len=1, kind=c_char), pointer :: version_chars(:) => NULL()
    integer i

    interface
        type(c_ptr) function ncnn_version() bind(c)
            import
            implicit none
        end function ncnn_version
    end interface

    call c_f_pointer(ncnn_version(), version_chars, (/255/))

    version = ""

    do i = 1, 255
        if (version_chars(i) == c_null_char) then
            exit
        end if
        version(i: i) = version_chars(i)
    end do

    version = trim(version)
end function version


!> allocator api

!> ncnn_allocator_t ncnn_allocator_create_pool_allocator()
function allocator_create_pool_allocator() result(allocator)
    implicit none

    type(ncnn_allocator_t) :: allocator

    interface
        type(c_ptr) function ncnn_allocator_create_pool_allocator() bind(c)
            use iso_c_binding, only: c_ptr
            implicit none
        end function ncnn_allocator_create_pool_allocator
    end interface

    allocator%ptr = ncnn_allocator_create_pool_allocator()
end function allocator_create_pool_allocator

!> ncnn_allocator_t ncnn_allocator_create_unlocked_pool_allocator()
function allocator_create_unlocked_pool_allocator() result(allocator)
    implicit none

    type(ncnn_allocator_t) :: allocator

    interface
        type(c_ptr) function ncnn_allocator_create_unlocked_pool_allocator() bind(c)
            import c_ptr
            implicit none
        end function ncnn_allocator_create_unlocked_pool_allocator
    end interface

    allocator%ptr = ncnn_allocator_create_unlocked_pool_allocator()
end function allocator_create_unlocked_pool_allocator

!> void ncnn_allocator_destroy(ncnn_allocator_t allocator)
subroutine allocator_destroy(allocator)
    implicit none

    type(ncnn_allocator_t), intent(inout) :: allocator

    interface
        subroutine ncnn_allocator_destroy(allocator_ptr) bind(c)
            import c_ptr
            implicit none
            type(c_ptr), value :: allocator_ptr
        end subroutine ncnn_allocator_destroy
    end interface

    call ncnn_allocator_destroy(allocator%ptr)
end subroutine


!> option api

!> ncnn_option_t ncnn_option_create()
function option_create() result(opt)
    implicit none

    type(ncnn_option_t) :: opt

    interface
        type(c_ptr) function ncnn_option_create() bind(c)
            import c_ptr
            implicit none
        end function ncnn_option_create
    end interface

    opt%ptr = ncnn_option_create()
end function option_create

!> void ncnn_option_destroy(ncnn_option_t opt)
subroutine option_destroy(opt)
    implicit none

    type(ncnn_option_t), intent(inout) :: opt

    interface
        subroutine ncnn_option_destroy(opt_ptr) bind(c)
            import c_ptr
            implicit none
            type(c_ptr), value :: opt_ptr
        end subroutine ncnn_option_destroy
    end interface

    call ncnn_option_destroy(opt%ptr)
end subroutine

!> int ncnn_option_get_num_threads(const ncnn_option_t opt);
function option_get_num_threads(opt) result(num_threads)
    implicit none

    type(ncnn_option_t), intent(in) :: opt
    integer                         :: num_threads

    interface
        integer(c_int) function ncnn_option_get_num_threads(opt_ptr) bind(c)
            import c_int, c_ptr
            implicit none
            type(c_ptr), value :: opt_ptr
        end function ncnn_option_get_num_threads
    end interface

    num_threads = ncnn_option_get_num_threads(opt%ptr)
end function option_get_num_threads

!> void ncnn_option_set_num_threads(ncnn_option_t opt, int num_threads)
subroutine option_set_num_threads(opt, num_threads)
    implicit none

    type(ncnn_option_t), intent(inout) :: opt
    integer, intent(in)                :: num_threads

    interface 
        subroutine ncnn_option_set_num_threads(opt_ptr, num_threads) bind(c)
            import c_ptr, c_int
            implicit none
            type(c_ptr), value    :: opt_ptr
            integer(c_int), value :: num_threads
        end subroutine ncnn_option_set_num_threads
    end interface

    call ncnn_option_set_num_threads(opt%ptr, num_threads)
end subroutine option_set_num_threads

!> int ncnn_option_get_use_local_pool_allocator(const ncnn_option_t opt)
function option_get_use_local_pool_allocator(opt) result(use_local_pool_allocator)
    implicit none

    type(ncnn_option_t), intent(in) :: opt
    integer                         :: use_local_pool_allocator

    interface
        integer(c_int) function ncnn_option_get_use_local_pool_allocator(opt_ptr) bind(c)
            import c_ptr, c_int
            implicit none
            type(c_ptr), value :: opt_ptr
        end function ncnn_option_get_use_local_pool_allocator
    end interface
    
    use_local_pool_allocator = ncnn_option_get_use_local_pool_allocator(opt%ptr)
end function option_get_use_local_pool_allocator

!> void ncnn_option_set_use_local_pool_allocator(ncnn_option_t opt, int use_local_pool_allocator)
subroutine option_set_use_local_pool_allocator(opt, use_local_pool_allocator)
    implicit none

    type(ncnn_option_t), intent(inout) :: opt
    integer, intent(in)                :: use_local_pool_allocator

    interface
        subroutine ncnn_option_set_use_local_pool_allocator(opt_ptr, use_local_pool_allocator) bind(c)
            import c_ptr, c_int
            implicit none
            type(c_ptr), value    :: opt_ptr
            integer(c_int), value :: use_local_pool_allocator
        end subroutine ncnn_option_set_use_local_pool_allocator
    end interface
    
    call ncnn_option_set_use_local_pool_allocator(opt%ptr, use_local_pool_allocator)
end subroutine option_set_use_local_pool_allocator

!> void ncnn_option_set_blob_allocator(ncnn_option_t opt, ncnn_allocator_t allocator)
subroutine option_set_blob_allocator(opt, allocator)
    implicit none

    type(ncnn_option_t), intent(inout) :: opt
    type(ncnn_allocator_t), intent(in) :: allocator

    interface
        subroutine ncnn_option_set_blob_allocator(opt_ptr, allocator_ptr) bind(c)
            import c_ptr
            implicit none
            type(c_ptr), value :: opt_ptr
            type(c_ptr), value :: allocator_ptr
        end subroutine ncnn_option_set_blob_allocator
    end interface

    call ncnn_option_set_blob_allocator(opt%ptr, allocator%ptr)
end subroutine option_set_blob_allocator

!> void ncnn_option_set_workspace_allocator(ncnn_option_t opt, ncnn_allocator_t allocator)
subroutine option_set_workspace_allocator(opt, allocator)
    implicit none

    type(ncnn_option_t), intent(inout) :: opt
    type(ncnn_allocator_t), intent(in) :: allocator

    interface
        subroutine ncnn_option_set_workspace_allocator(opt_ptr, allocator_ptr) bind(c)
            import c_ptr
            implicit none
            type(c_ptr), value :: opt_ptr
            type(c_ptr), value :: allocator_ptr
        end subroutine ncnn_option_set_workspace_allocator
    end interface

    call ncnn_option_set_workspace_allocator(opt%ptr, allocator%ptr)
end subroutine option_set_workspace_allocator

!> int ncnn_option_get_use_vulkan_compute(const ncnn_option_t opt)
function option_get_use_vulkan_compute(opt) result(use_vulkan_compute)
    implicit none

    type(ncnn_option_t), intent(in) :: opt
    integer                         :: use_vulkan_compute

    interface
        integer(c_int) function ncnn_option_get_use_vulkan_compute(opt_ptr) bind(c)
            import c_ptr, c_int
            implicit none
            type(c_ptr), value :: opt_ptr
        end function ncnn_option_get_use_vulkan_compute
    end interface
    
    use_vulkan_compute = ncnn_option_get_use_vulkan_compute(opt%ptr)
end function option_get_use_vulkan_compute

!> void ncnn_option_set_use_vulkan_compute(ncnn_option_t opt, int use_vulkan_compute)
subroutine option_set_use_vulkan_compute(opt, use_vulkan_compute)
    implicit none

    type(ncnn_option_t), intent(inout) :: opt
    integer, intent(in)                :: use_vulkan_compute

    interface
        subroutine ncnn_option_set_use_vulkan_compute(opt_ptr, use_vulkan_compute) bind(c)
            import c_ptr, c_int
            implicit none
            type(c_ptr), value    :: opt_ptr
            integer(c_int), value :: use_vulkan_compute
        end subroutine ncnn_option_set_use_vulkan_compute
    end interface
    
    call ncnn_option_set_use_vulkan_compute(opt%ptr, use_vulkan_compute)
end subroutine option_set_use_vulkan_compute


!> mat api

!> ncnn_mat_t ncnn_mat_create()
function mat_create() result(mat)
    implicit none

    type(ncnn_mat_t) :: mat

    interface
        type(c_ptr) function ncnn_mat_create() bind(c)
            import c_ptr
            implicit none
        end function ncnn_mat_create
    end interface

    mat%ptr = ncnn_mat_create()
end function mat_create

!> ncnn_mat_t ncnn_mat_create_1d(int w, ncnn_allocator_t allocator)
function mat_create_1d(w, allocator) result(mat)
    implicit none

    integer, intent(in)                :: w
    type(ncnn_allocator_t), intent(in) :: allocator
    type(ncnn_mat_t)                   :: mat

    interface
        type(c_ptr) function ncnn_mat_create_1d(w, allocator_ptr) bind(c)
            import c_int, c_ptr
            implicit none
            integer(c_int), value :: w
            type(c_ptr), value    :: allocator_ptr
        end function ncnn_mat_create_1d
    end interface

    mat%ptr = ncnn_mat_create_1d(w, allocator%ptr)
end function mat_create_1d

!> ncnn_mat_t ncnn_mat_create_2d(int w, int h, ncnn_allocator_t allocator)
function mat_create_2d(w, h, allocator) result(mat)
    implicit none
    
    integer, intent(in)                :: w
    integer, intent(in)                :: h
    type(ncnn_allocator_t), intent(in) :: allocator
    type(ncnn_mat_t)                   :: mat

    interface
        type(c_ptr) function ncnn_mat_create_2d(w, h, allocator_ptr) bind(c)
            import c_int, c_ptr
            implicit none
            integer(c_int), value :: w
            integer(c_int), value :: h
            type(c_ptr), value    :: allocator_ptr
        end function ncnn_mat_create_2d
    end interface

    mat%ptr = ncnn_mat_create_2d(w, h, allocator%ptr)
end function mat_create_2d

!> ncnn_mat_t ncnn_mat_create_3d(int w, int h, int c, ncnn_allocator_t allocator)
function mat_create_3d(w, h, c, allocator) result(mat)
    implicit none
    
    integer, intent(in)                :: w
    integer, intent(in)                :: h
    integer, intent(in)                :: c
    type(ncnn_allocator_t), intent(in) :: allocator
    type(ncnn_mat_t)                   :: mat

    interface
        type(c_ptr) function ncnn_mat_create_3d(w, h, c, allocator_ptr) bind(c)
            import c_int, c_ptr
            implicit none
            integer(c_int), value :: w
            integer(c_int), value :: h
            integer(c_int), value :: c
            type(c_ptr), value    :: allocator_ptr
        end function ncnn_mat_create_3d
    end interface

    mat%ptr = ncnn_mat_create_3d(w, h, c, allocator%ptr)
end function mat_create_3d

!> ncnn_mat_t ncnn_mat_create_4d(int w, int h, int d, int c, ncnn_allocator_t allocator)
function mat_create_4d(w, h, d, c, allocator) result(mat)
    implicit none
    
    integer, intent(in)                :: w
    integer, intent(in)                :: h
    integer, intent(in)                :: d
    integer, intent(in)                :: c
    type(ncnn_allocator_t), intent(in) :: allocator
    type(ncnn_mat_t)                   :: mat

    interface
        type(c_ptr) function ncnn_mat_create_4d(w, h, d, c, allocator_ptr) bind(c)
            import c_int, c_ptr
            implicit none
            integer(c_int), value :: w
            integer(c_int), value :: h
            integer(c_int), value :: d
            integer(c_int), value :: c
            type(c_ptr), value    :: allocator_ptr
        end function ncnn_mat_create_4d
    end interface

    mat%ptr = ncnn_mat_create_4d(w, h, d, c, allocator%ptr)
end function mat_create_4d

!> ncnn_mat_t ncnn_mat_create_external_1d(int w, void* data, ncnn_allocator_t allocator)
function mat_create_external_1d(w, data, allocator) result(mat)
    implicit none
    
    integer, intent(in)                :: w
    real(kind=4), intent(in)           :: data(w)
    type(ncnn_allocator_t), intent(in) :: allocator
    type(ncnn_mat_t)                   :: mat

    interface
        type(c_ptr) function ncnn_mat_create_external_1d(w, data, allocator_ptr) bind(c)
            import c_int, c_ptr, c_float
            implicit none
            integer(c_int), value :: w
            real(c_float)         :: data(w)
            type(c_ptr), value    :: allocator_ptr
        end function ncnn_mat_create_external_1d
    end interface

    mat%ptr = ncnn_mat_create_external_1d(w, data, allocator%ptr)
end function mat_create_external_1d

!> ncnn_mat_t ncnn_mat_create_external_2d(int w, int h, void* data, ncnn_allocator_t allocator)
function mat_create_external_2d(w, h, data, allocator) result(mat)
    implicit none
    
    integer, intent(in)                :: w
    integer, intent(in)                :: h
    real(kind=4), intent(in)           :: data(w * h)
    type(ncnn_allocator_t), intent(in) :: allocator
    type(ncnn_mat_t)                   :: mat

    interface
        type(c_ptr) function ncnn_mat_create_external_2d(w, h, data, allocator_ptr) bind(c)
            import c_int, c_ptr, c_float
            implicit none
            integer(c_int), value :: w
            integer(c_int), value :: h
            real(c_float)         :: data(w * h)
            type(c_ptr), value    :: allocator_ptr
        end function ncnn_mat_create_external_2d
    end interface

    mat%ptr = ncnn_mat_create_external_2d(w, h, data, allocator%ptr)
end function mat_create_external_2d

!> ncnn_mat_t ncnn_mat_create_external_3d(int w, int h, int c, void* data, ncnn_allocator_t allocator)
function mat_create_external_3d(w, h, c, data, allocator) result(mat)
    implicit none
    
    integer, intent(in)                :: w
    integer, intent(in)                :: h
    integer, intent(in)                :: c
    real(kind=4), intent(in)           :: data(w * h * c)
    type(ncnn_allocator_t), intent(in) :: allocator
    type(ncnn_mat_t)                   :: mat

    interface
        type(c_ptr) function ncnn_mat_create_external_3d(w, h, c, data, allocator_ptr) bind(c)
            import c_int, c_ptr, c_float
            implicit none
            integer(c_int), value :: w
            integer(c_int), value :: h
            integer(c_int), value :: c
            real(c_float)         :: data(w * h)
            type(c_ptr), value    :: allocator_ptr
        end function ncnn_mat_create_external_3d
    end interface

    mat%ptr = ncnn_mat_create_external_3d(w, h, c, data, allocator%ptr)
end function mat_create_external_3d

!> ncnn_mat_t ncnn_mat_create_external_4d(int w, int h, int d, int c, void* data, ncnn_allocator_t allocator)
function mat_create_external_4d(w, h, d, c, data, allocator) result(mat)
    integer, intent(in)                :: w
    integer, intent(in)                :: h
    integer, intent(in)                :: d
    integer, intent(in)                :: c
    real(kind=4), intent(in)           :: data(w * h * d * c)
    type(ncnn_allocator_t), intent(in) :: allocator
    type(ncnn_mat_t)                   :: mat

    interface
        type(c_ptr) function ncnn_mat_create_external_4d(w, h, d, c, data, allocator_ptr) bind(c)
            import c_int, c_ptr, c_float
            implicit none
            integer(c_int), value :: w
            integer(c_int), value :: h
            integer(c_int), value :: d
            integer(c_int), value :: c
            real(c_float)         :: data(w * h * d * c)
            type(c_ptr), value    :: allocator_ptr
        end function ncnn_mat_create_external_4d
    end interface

    mat%ptr = ncnn_mat_create_external_4d(w, h, d, c, data, allocator%ptr)
end function mat_create_external_4d

!> ncnn_mat_t ncnn_mat_create_1d_elem(int w, size_t elemsize, int elempack, ncnn_allocator_t allocator)
function mat_create_1d_elem(w, elemsize, elempack, allocator) result(mat)
    implicit none
    
    integer, intent(in)                :: w
    integer, intent(in)                :: elemsize
    integer, intent(in)                :: elempack
    type(ncnn_allocator_t), intent(in) :: allocator
    type(ncnn_mat_t)                   :: mat

    interface
        type(c_ptr) function ncnn_mat_create_1d_elem(w, elemsize, elempack, allocator_ptr) bind(c)
            import c_int, c_ptr, c_size_t
            implicit none

            integer(c_int), value    :: w
            integer(c_size_t), value :: elemsize
            integer(c_int), value    :: elempack
            type(c_ptr), value       :: allocator_ptr
            
        end function ncnn_mat_create_1d_elem
    end interface

    mat%ptr = ncnn_mat_create_1d_elem(w, int8(elemsize), elempack, allocator%ptr)
end function mat_create_1d_elem

!> ncnn_mat_t ncnn_mat_create_2d_elem(int w, int h, size_t elemsize, int elempack, ncnn_allocator_t allocator)
function mat_create_2d_elem(w, h, elemsize, elempack, allocator) result(mat)
    implicit none
    
    integer, intent(in)                :: w
    integer, intent(in)                :: h
    integer, intent(in)                :: elemsize
    integer, intent(in)                :: elempack
    type(ncnn_allocator_t), intent(in) :: allocator
    type(ncnn_mat_t)                   :: mat

    interface
        type(c_ptr) function ncnn_mat_create_2d_elem(w, h, elemsize, elempack, allocator_ptr) bind(c)
            import c_int, c_ptr, c_size_t
            implicit none

            integer(c_int), value    :: w
            integer(c_int), value    :: h
            integer(c_size_t), value :: elemsize
            integer(c_int), value    :: elempack
            type(c_ptr), value       :: allocator_ptr
            
        end function ncnn_mat_create_2d_elem
    end interface

    mat%ptr = ncnn_mat_create_2d_elem(w, h, int8(elemsize), elempack, allocator%ptr)
end function mat_create_2d_elem

!> ncnn_mat_t ncnn_mat_create_3d_elem(int w, int h, int c, size_t elemsize, int elempack, ncnn_allocator_t allocator)
function mat_create_3d_elem(w, h, c, elemsize, elempack, allocator) result(mat)
    implicit none
    
    integer, intent(in)                :: w
    integer, intent(in)                :: h
    integer, intent(in)                :: c
    integer, intent(in)                :: elemsize
    integer, intent(in)                :: elempack
    type(ncnn_allocator_t), intent(in) :: allocator
    type(ncnn_mat_t)                   :: mat

    interface
        type(c_ptr) function ncnn_mat_create_3d_elem(w, h, c, elemsize, elempack, allocator_ptr) bind(c)
            import c_int, c_ptr, c_size_t
            implicit none

            integer(c_int), value    :: w
            integer(c_int), value    :: h
            integer(c_int), value    :: c
            integer(c_size_t), value :: elemsize
            integer(c_int), value    :: elempack
            type(c_ptr), value       :: allocator_ptr
            
        end function ncnn_mat_create_3d_elem
    end interface

    mat%ptr = ncnn_mat_create_3d_elem(w, h, c, int8(elemsize), elempack, allocator%ptr)
end function mat_create_3d_elem

!> ncnn_mat_t ncnn_mat_create_external_4d_elem(int w, int h, int d, int c, void* data, size_t elemsize, int elempack, ncnn_allocator_t allocator)
function mat_create_4d_elem(w, h, d, c, elemsize, elempack, allocator) result(mat)
    implicit none
    
    integer, intent(in)                :: w
    integer, intent(in)                :: h
    integer, intent(in)                :: d
    integer, intent(in)                :: c
    integer, intent(in)                :: elemsize
    integer, intent(in)                :: elempack
    type(ncnn_allocator_t), intent(in) :: allocator
    type(ncnn_mat_t)                   :: mat

    interface
        type(c_ptr) function ncnn_mat_create_4d_elem(w, h, d, c, elemsize, elempack, allocator_ptr) bind(c)
            import c_int, c_ptr, c_size_t
            implicit none

            integer(c_int), value    :: w
            integer(c_int), value    :: h
            integer(c_int), value    :: d
            integer(c_int), value    :: c
            integer(c_size_t), value :: elemsize
            integer(c_int), value    :: elempack
            type(c_ptr), value       :: allocator_ptr
            
        end function ncnn_mat_create_4d_elem
    end interface

    mat%ptr = ncnn_mat_create_4d_elem(w, h, d, c, int8(elemsize), elempack, allocator%ptr)
end function mat_create_4d_elem

!> ncnn_mat_t ncnn_mat_create_external_1d_elem(int w, void* data, size_t elemsize, int elempack, ncnn_allocator_t allocator)
function mat_create_external_1d_elem(w, data, elemsize, elempack, allocator) result(mat)
    implicit none
    
    integer, intent(in)                :: w
    real(kind=4), intent(in)           :: data(w)
    integer, intent(in)                :: elemsize
    integer, intent(in)                :: elempack
    type(ncnn_allocator_t), intent(in) :: allocator
    type(ncnn_mat_t)                   :: mat

    interface
        type(c_ptr) function ncnn_mat_create_external_1d_elem(w, data, elemsize, elempack, allocator_ptr) bind(c)
            import c_int, c_ptr, c_size_t, c_float
            implicit none

            integer(c_int), value     :: w
            real(c_float), intent(in) :: data(w)
            integer(c_size_t), value  :: elemsize
            integer(c_int), value     :: elempack
            type(c_ptr), value        :: allocator_ptr
            
        end function ncnn_mat_create_external_1d_elem
    end interface

    mat%ptr = ncnn_mat_create_external_1d_elem(w, data, int8(elemsize), elempack, allocator%ptr)
end function mat_create_external_1d_elem

!> ncnn_mat_t ncnn_mat_create_external_2d_elem(int w, int h, void* data, size_t elemsize, int elempack, ncnn_allocator_t allocator)
function mat_create_external_2d_elem(w, h, data, elemsize, elempack, allocator) result(mat)
    implicit none
    
    integer, intent(in)                :: w
    integer, intent(in)                :: h
    real(kind=4), intent(in)           :: data(w * h)
    integer, intent(in)                :: elemsize
    integer, intent(in)                :: elempack
    type(ncnn_allocator_t), intent(in) :: allocator
    type(ncnn_mat_t)                   :: mat

    interface
        type(c_ptr) function ncnn_mat_create_external_2d_elem(w, h, data, elemsize, elempack, allocator_ptr) bind(c)
            import c_int, c_ptr, c_size_t, c_float
            implicit none

            integer(c_int), value     :: w
            integer(c_int), value     :: h
            real(c_float), intent(in) :: data(w * h)
            integer(c_size_t), value  :: elemsize
            integer(c_int), value     :: elempack
            type(c_ptr), value        :: allocator_ptr
            
        end function ncnn_mat_create_external_2d_elem
    end interface

    mat%ptr = ncnn_mat_create_external_2d_elem(w, h, data, int8(elemsize), elempack, allocator%ptr)
end function mat_create_external_2d_elem

!> ncnn_mat_t ncnn_mat_create_external_3d_elem(int w, int h, int c, void* data, size_t elemsize, int elempack, ncnn_allocator_t allocator)
function mat_create_external_3d_elem(w, h, c, data, elemsize, elempack, allocator) result(mat)
    implicit none
    
    integer, intent(in)                :: w
    integer, intent(in)                :: h
    integer, intent(in)                :: c
    real(kind=4), intent(in)           :: data(w * h * c)
    integer, intent(in)                :: elemsize
    integer, intent(in)                :: elempack
    type(ncnn_allocator_t), intent(in) :: allocator
    type(ncnn_mat_t)                   :: mat

    interface
        type(c_ptr) function ncnn_mat_create_external_3d_elem(w, h, c, data, elemsize, elempack, allocator_ptr) bind(c)
            import c_int, c_ptr, c_size_t, c_float
            implicit none

            integer(c_int), value     :: w
            integer(c_int), value     :: h
            integer(c_int), value     :: c
            real(c_float), intent(in) :: data(w * h * c)
            integer(c_size_t), value  :: elemsize
            integer(c_int), value     :: elempack
            type(c_ptr), value        :: allocator_ptr
            
        end function ncnn_mat_create_external_3d_elem
    end interface

    mat%ptr = ncnn_mat_create_external_3d_elem(w, h, c, data, int8(elemsize), elempack, allocator%ptr)
end function mat_create_external_3d_elem

!> ncnn_mat_t ncnn_mat_create_external_4d_elem(int w, int h, int d, int c, void* data, size_t elemsize, int elempack, ncnn_allocator_t allocator)
function mat_create_external_4d_elem(w, h, d, c, data, elemsize, elempack, allocator) result(mat)
    implicit none
    
    integer, intent(in)                :: w
    integer, intent(in)                :: h
    integer, intent(in)                :: d
    integer, intent(in)                :: c
    real(kind=4), intent(in)           :: data(w * h * d * c)
    integer, intent(in)                :: elemsize
    integer, intent(in)                :: elempack
    type(ncnn_allocator_t), intent(in) :: allocator
    type(ncnn_mat_t)                   :: mat

    interface
        type(c_ptr) function ncnn_mat_create_external_4d_elem(w, h, d, c, data, elemsize, elempack, allocator_ptr) bind(c)
            import c_int, c_ptr, c_size_t, c_float
            implicit none

            integer(c_int), value     :: w
            integer(c_int), value     :: h
            integer(c_int), value     :: d
            integer(c_int), value     :: c
            real(c_float), intent(in) :: data(w * h * d * c)
            integer(c_size_t), value  :: elemsize
            integer(c_int), value     :: elempack
            type(c_ptr), value        :: allocator_ptr
            
        end function ncnn_mat_create_external_4d_elem
    end interface

    mat%ptr = ncnn_mat_create_external_4d_elem(w, h, d, c, data, int8(elemsize), elempack, allocator%ptr)
end function mat_create_external_4d_elem

!> void ncnn_mat_destroy(ncnn_mat_t mat)
subroutine mat_destory(mat)
    implicit none
    
    type(ncnn_mat_t), intent(inout) :: mat

    interface
        subroutine ncnn_mat_destroy(mat_ptr) bind(c)
            import c_ptr
            implicit none
            
            type(c_ptr), value :: mat_ptr

        end subroutine ncnn_mat_destroy
    end interface

    call ncnn_mat_destroy(mat%ptr)
end subroutine mat_destory

!> void ncnn_mat_fill_float(ncnn_mat_t mat, float v)
subroutine mat_fill_float(mat, v)
    implicit none
    
    type(ncnn_mat_t), intent(inout) :: mat
    real(kind=4), intent(in)        :: v

    interface
        subroutine ncnn_mat_fill_float(mat_ptr, v) bind(c)
            import c_ptr, c_float
            implicit none
            
            type(c_ptr), value   :: mat_ptr
            real(c_float), value :: v

        end subroutine ncnn_mat_fill_float
    end interface

    call ncnn_mat_fill_float(mat%ptr, v)

end subroutine mat_fill_float

!> ncnn_mat_t ncnn_mat_clone(const ncnn_mat_t mat, ncnn_allocator_t allocator)
function mat_clone(mat, allocator) result(new_mat)
    implicit none
    
    type(ncnn_mat_t), intent(in)       :: mat
    type(ncnn_allocator_t), intent(in) :: allocator
    type(ncnn_mat_t)                   :: new_mat

    interface
        type(c_ptr) function ncnn_mat_clone(mat_ptr, allocator_ptr) bind(c)
            import c_ptr
            implicit none
            
            type(c_ptr), value :: mat_ptr
            type(c_ptr), value :: allocator_ptr

        end function ncnn_mat_clone
    end interface

    new_mat%ptr = ncnn_mat_clone(mat%ptr, allocator%ptr)
end function mat_clone

!> ncnn_mat_t ncnn_mat_reshape_1d(const ncnn_mat_t mat, int w, ncnn_allocator_t allocator)
function mat_reshape_1d(mat, w, allocator) result(new_mat)
    implicit none

    type(ncnn_mat_t), intent(in)       :: mat
    integer                            :: w
    type(ncnn_allocator_t), intent(in) :: allocator
    type(ncnn_mat_t)                   :: new_mat

    interface
        type(c_ptr) function ncnn_mat_reshape_1d(mat_ptr, w, allocator_ptr) bind(c)
            import c_ptr, c_int
            implicit none
            
            type(c_ptr), value    :: mat_ptr
            integer(c_int), value :: w
            type(c_ptr), value    :: allocator_ptr

        end function ncnn_mat_reshape_1d
    end interface

    new_mat%ptr = ncnn_mat_reshape_1d(mat%ptr, w, allocator%ptr)
end function mat_reshape_1d

!> ncnn_mat_t ncnn_mat_reshape_2d(const ncnn_mat_t mat, int w, int h, ncnn_allocator_t allocator)
function mat_reshape_2d(mat, w, h, allocator) result(new_mat)
    implicit none

    type(ncnn_mat_t), intent(in)       :: mat
    integer                            :: w
    integer                            :: h
    type(ncnn_allocator_t), intent(in) :: allocator
    type(ncnn_mat_t)                   :: new_mat

    interface
        type(c_ptr) function ncnn_mat_reshape_2d(mat_ptr, w, h, allocator_ptr) bind(c)
            import c_ptr, c_int
            implicit none
            
            type(c_ptr), value    :: mat_ptr
            integer(c_int), value :: w
            integer(c_int), value :: h
            type(c_ptr), value    :: allocator_ptr

        end function ncnn_mat_reshape_2d
    end interface

    new_mat%ptr = ncnn_mat_reshape_2d(mat%ptr, w, h, allocator%ptr)
end function mat_reshape_2d

!> ncnn_mat_t ncnn_mat_reshape_3d(const ncnn_mat_t mat, int w, int h, int c, ncnn_allocator_t allocator)
function mat_reshape_3d(mat, w, h, c, allocator) result(new_mat)
    implicit none

    type(ncnn_mat_t), intent(in)       :: mat
    integer                            :: w
    integer                            :: h
    integer                            :: c
    type(ncnn_allocator_t), intent(in) :: allocator
    type(ncnn_mat_t)                   :: new_mat

    interface
        type(c_ptr) function ncnn_mat_reshape_3d(mat_ptr, w, h, c, allocator_ptr) bind(c)
            import c_ptr, c_int
            implicit none
            
            type(c_ptr), value    :: mat_ptr
            integer(c_int), value :: w
            integer(c_int), value :: h
            integer(c_int), value :: c
            type(c_ptr), value    :: allocator_ptr

        end function ncnn_mat_reshape_3d
    end interface

    new_mat%ptr = ncnn_mat_reshape_3d(mat%ptr, w, h, c, allocator%ptr)
end function mat_reshape_3d

!> ncnn_mat_t ncnn_mat_reshape_4d(const ncnn_mat_t mat, int w, int h, int d, int c, ncnn_allocator_t allocator)
function mat_reshape_4d(mat, w, h, d, c, allocator) result(new_mat)
    implicit none

    type(ncnn_mat_t), intent(in)       :: mat
    integer                            :: w
    integer                            :: h
    integer                            :: d
    integer                            :: c
    type(ncnn_allocator_t), intent(in) :: allocator
    type(ncnn_mat_t)                   :: new_mat

    interface
        type(c_ptr) function ncnn_mat_reshape_4d(mat_ptr, w, h, d, c, allocator_ptr) bind(c)
            import c_ptr, c_int
            implicit none
            
            type(c_ptr), value    :: mat_ptr
            integer(c_int), value :: w
            integer(c_int), value :: h
            integer(c_int), value :: d
            integer(c_int), value :: c
            type(c_ptr), value    :: allocator_ptr

        end function ncnn_mat_reshape_4d
    end interface

    new_mat%ptr = ncnn_mat_reshape_4d(mat%ptr, w, h, d, c, allocator%ptr)
end function mat_reshape_4d

!> int ncnn_mat_get_dims(const ncnn_mat_t mat)
function mat_get_dims(mat) result(dims)
    implicit none

    type(ncnn_mat_t), intent(in) :: mat
    integer                      :: dims

    interface
        integer(c_int) function ncnn_mat_get_dims(mat_ptr) bind(c)
            import c_ptr, c_int
            implicit none
            type(c_ptr), value :: mat_ptr
        end function ncnn_mat_get_dims
    end interface

    dims = ncnn_mat_get_dims(mat%ptr)
end function mat_get_dims

!> int ncnn_mat_get_w(const ncnn_mat_t mat)
function mat_get_w(mat) result(w)
    implicit none

    type(ncnn_mat_t), intent(in) :: mat
    integer                      :: w

    interface
        integer(c_int) function ncnn_mat_get_w(mat_ptr) bind(c)
            import c_ptr, c_int
            implicit none
            type(c_ptr), value :: mat_ptr
        end function ncnn_mat_get_w
    end interface

    w = ncnn_mat_get_w(mat%ptr)
end function mat_get_w

!> int ncnn_mat_get_h(const ncnn_mat_t mat)
function mat_get_h(mat) result(h)
    implicit none

    type(ncnn_mat_t), intent(in) :: mat
    integer                      :: h

    interface
        integer(c_int) function ncnn_mat_get_h(mat_ptr) bind(c)
            import c_ptr, c_int
            implicit none
            type(c_ptr), value :: mat_ptr
        end function ncnn_mat_get_h
    end interface

    h = ncnn_mat_get_h(mat%ptr)
end function mat_get_h

!> int ncnn_mat_get_d(const ncnn_mat_t mat)
function mat_get_d(mat) result(d)
    implicit none

    type(ncnn_mat_t), intent(in) :: mat
    integer                      :: d

    interface
        integer(c_int) function ncnn_mat_get_d(mat_ptr) bind(c)
            import c_ptr, c_int
            implicit none
            type(c_ptr), value :: mat_ptr
        end function ncnn_mat_get_d
    end interface

    d = ncnn_mat_get_d(mat%ptr)
end function mat_get_d

!> int ncnn_mat_get_c(const ncnn_mat_t mat)
function mat_get_c(mat) result(c)
    implicit none

    type(ncnn_mat_t), intent(in) :: mat
    integer                      :: c

    interface
        integer(c_int) function ncnn_mat_get_c(mat_ptr) bind(c)
            import c_ptr, c_int
            implicit none
            type(c_ptr), value :: mat_ptr
        end function ncnn_mat_get_c
    end interface

    c = ncnn_mat_get_c(mat%ptr)
end function mat_get_c

!> size_t ncnn_mat_get_elemsize(const ncnn_mat_t mat)
function mat_get_elemsize(mat) result(elemsize)
    implicit none

    type(ncnn_mat_t), intent(in) :: mat
    integer                      :: elemsize

    interface
        integer(c_size_t) function ncnn_mat_get_elemsize(mat_ptr) bind(c)
            import c_ptr, c_size_t
            implicit none
            type(c_ptr), value :: mat_ptr
        end function ncnn_mat_get_elemsize
    end interface

    elemsize = int(ncnn_mat_get_elemsize(mat%ptr))
end function mat_get_elemsize

!> int ncnn_mat_get_elempack(const ncnn_mat_t mat)
function mat_get_elempack(mat) result(elempack)
    implicit none

    type(ncnn_mat_t), intent(in) :: mat
    integer                      :: elempack

    interface
        integer(c_int) function ncnn_mat_get_elempack(mat_ptr) bind(c)
            import c_ptr, c_int
            implicit none
            type(c_ptr), value :: mat_ptr
        end function ncnn_mat_get_elempack
    end interface

    elempack = ncnn_mat_get_elempack(mat%ptr)
end function mat_get_elempack

!> size_t ncnn_mat_get_cstep(const ncnn_mat_t mat)
function mat_get_cstep(mat) result(cstep)
    implicit none

    type(ncnn_mat_t), intent(in) :: mat
    integer                      :: cstep

    interface
        integer(c_size_t) function ncnn_mat_get_cstep(mat_ptr) bind(c)
            import c_ptr, c_size_t
            implicit none
            type(c_ptr), value :: mat_ptr
        end function ncnn_mat_get_cstep
    end interface

    cstep = int(ncnn_mat_get_cstep(mat%ptr))
end function mat_get_cstep

!> void* ncnn_mat_get_data(const ncnn_mat_t mat)
function mat_get_data(mat) result(data)
    implicit none
    type(ncnn_mat_t), intent(in) :: mat
    real(4), allocatable         :: data(:)
    real(4), pointer             :: data_pointer(:)
    integer                      :: data_size
    integer                      :: i

    interface
        type(c_ptr) function ncnn_mat_get_data(mat_ptr) bind(c)
            import c_ptr
            implicit none
            type(c_ptr), value :: mat_ptr 
        end function ncnn_mat_get_data
    end interface

    data_size = 1
    data_size = data_size * mat_get_w(mat)
    data_size = data_size * mat_get_h(mat)
    data_size = data_size * mat_get_d(mat)
    data_size = data_size * mat_get_c(mat)

    allocate(data(data_size))

    call c_f_pointer(ncnn_mat_get_data(mat%ptr), data_pointer, (/data_size/))

    do i = 1, data_size
        data(i) = data_pointer(i)
    end do

end function mat_get_data

!> void* ncnn_mat_get_channel_data(const ncnn_mat_t mat, int c)
function mat_get_channel_data(mat, c) result(data)
    implicit none
    type(ncnn_mat_t), intent(in) :: mat
    integer, intent(in)          :: c
    real(4), allocatable         :: data(:)
    real(4), pointer             :: data_pointer(:)
    integer                      :: data_size
    integer                      :: i

    interface
        type(c_ptr) function ncnn_mat_get_channel_data(mat_ptr, c) bind(c)
            import c_ptr, c_int
            implicit none
            type(c_ptr), value    :: mat_ptr
            integer(c_int), value :: c
        end function ncnn_mat_get_channel_data
    end interface

    data_size = 1
    data_size = data_size * mat_get_w(mat)
    data_size = data_size * mat_get_h(mat)
    data_size = data_size * mat_get_d(mat)

    allocate(data(data_size))

    call c_f_pointer(ncnn_mat_get_channel_data(mat%ptr, c - 1), data_pointer, (/data_size/))

    do i = 1, data_size
        data(i) = data_pointer(i)
    end do

end function mat_get_channel_data

end module ncnn