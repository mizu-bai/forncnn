module ncnn
    use, intrinsic :: iso_c_binding
    implicit none

    public :: ncnn_allocator_t

    !> ncnn_allocator_t
    type :: ncnn_allocator_t
        type(c_ptr) :: ptr = c_null_ptr
    end type

    !> ncnn_option_t
    type :: ncnn_option_t
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

end module ncnn