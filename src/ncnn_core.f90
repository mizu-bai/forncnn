!------------------------------------------------------------------------------
! MODULE: ncnn_core
!
! DESCRIPTION:
!>  ncnn version, allocator and option APIs.
!>
!>  All raw bind(c) declarations live in one interface block at the top of the
!>  module so the ABI can be audited in a single place. The module defaults to
!>  private and exports an explicit public list, so the raw C names stay
!>  internal.
!------------------------------------------------------------------------------
module ncnn_core
    use, intrinsic :: iso_c_binding
    use ncnn_types, only: ncnn_allocator_t, ncnn_option_t, c_f_string
    implicit none
    private

    public :: version, version_number, &
              allocator_create_pool, allocator_create_unlocked_pool, allocator_destroy, &
              option_create, option_destroy, &
              option_get_num_threads, option_set_num_threads, &
              option_set_blob_allocator, option_set_workspace_allocator, &
              option_set_kvcache_allocator, option_set_kvcache_max_seqlen_hint, &
              option_get_use_vulkan_compute, option_set_use_vulkan_compute, &
              option_get_use_local_pool_allocator, option_set_use_local_pool_allocator, &
              option_get_use_winograd_convolution, option_set_use_winograd_convolution, &
              option_get_use_sgemm_convolution, option_set_use_sgemm_convolution, &
              option_get_use_packing_layout, option_set_use_packing_layout, &
              option_get_use_fp16_packed, option_set_use_fp16_packed, &
              option_get_use_fp16_storage, option_set_use_fp16_storage, &
              option_get_use_fp16_arithmetic, option_set_use_fp16_arithmetic, &
              option_get_use_int8_packed, option_set_use_int8_packed, &
              option_get_use_int8_storage, option_set_use_int8_storage, &
              option_get_use_int8_arithmetic, option_set_use_int8_arithmetic, &
              option_get_use_int16_packed, option_set_use_int16_packed, &
              option_get_use_int16_storage, option_set_use_int16_storage, &
              option_get_use_bf16_packed, option_set_use_bf16_packed, &
              option_get_use_bf16_storage, option_set_use_bf16_storage, &
              option_get_use_shader_local_memory, option_set_use_shader_local_memory, &
              option_get_use_cooperative_matrix, option_set_use_cooperative_matrix

    !--------------------------------------------------------------------------
    ! RAW C DECLARATIONS -- audited in one place
    !--------------------------------------------------------------------------
    interface

        function ncnn_version() bind(c)
            import :: c_ptr
            type(c_ptr) :: ncnn_version
        end function

        function ncnn_version_number() bind(c)
            import :: c_int
            integer(c_int) :: ncnn_version_number
        end function

        function ncnn_allocator_create_pool_allocator() bind(c)
            import :: c_ptr
            type(c_ptr) :: ncnn_allocator_create_pool_allocator
        end function

        function ncnn_allocator_create_unlocked_pool_allocator() bind(c)
            import :: c_ptr
            type(c_ptr) :: ncnn_allocator_create_unlocked_pool_allocator
        end function

        subroutine ncnn_allocator_destroy(allocator) bind(c)
            import :: c_ptr
            type(c_ptr), value :: allocator
        end subroutine

        function ncnn_option_create() bind(c)
            import :: c_ptr
            type(c_ptr) :: ncnn_option_create
        end function

        subroutine ncnn_option_destroy(opt) bind(c)
            import :: c_ptr
            type(c_ptr), value :: opt
        end subroutine

        function ncnn_option_get_num_threads(opt) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int) :: ncnn_option_get_num_threads
        end function

        subroutine ncnn_option_set_num_threads(opt, num_threads) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int), value :: num_threads
        end subroutine

        subroutine ncnn_option_set_blob_allocator(opt, allocator) bind(c)
            import :: c_ptr
            type(c_ptr), value :: opt
            type(c_ptr), value :: allocator
        end subroutine

        subroutine ncnn_option_set_workspace_allocator(opt, allocator) bind(c)
            import :: c_ptr
            type(c_ptr), value :: opt
            type(c_ptr), value :: allocator
        end subroutine

        subroutine ncnn_option_set_kvcache_allocator(opt, allocator) bind(c)
            import :: c_ptr
            type(c_ptr), value :: opt
            type(c_ptr), value :: allocator
        end subroutine

        subroutine ncnn_option_set_kvcache_max_seqlen_hint(opt, max_seqlen_hint) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int), value :: max_seqlen_hint
        end subroutine

        function ncnn_option_get_use_vulkan_compute(opt) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int) :: ncnn_option_get_use_vulkan_compute
        end function

        subroutine ncnn_option_set_use_vulkan_compute(opt, enable) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int), value :: enable
        end subroutine

        function ncnn_option_get_use_local_pool_allocator(opt) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int) :: ncnn_option_get_use_local_pool_allocator
        end function

        subroutine ncnn_option_set_use_local_pool_allocator(opt, enable) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int), value :: enable
        end subroutine

        function ncnn_option_get_use_winograd_convolution(opt) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int) :: ncnn_option_get_use_winograd_convolution
        end function

        subroutine ncnn_option_set_use_winograd_convolution(opt, enable) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int), value :: enable
        end subroutine

        function ncnn_option_get_use_sgemm_convolution(opt) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int) :: ncnn_option_get_use_sgemm_convolution
        end function

        subroutine ncnn_option_set_use_sgemm_convolution(opt, enable) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int), value :: enable
        end subroutine

        function ncnn_option_get_use_packing_layout(opt) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int) :: ncnn_option_get_use_packing_layout
        end function

        subroutine ncnn_option_set_use_packing_layout(opt, enable) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int), value :: enable
        end subroutine

        function ncnn_option_get_use_fp16_packed(opt) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int) :: ncnn_option_get_use_fp16_packed
        end function

        subroutine ncnn_option_set_use_fp16_packed(opt, enable) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int), value :: enable
        end subroutine

        function ncnn_option_get_use_fp16_storage(opt) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int) :: ncnn_option_get_use_fp16_storage
        end function

        subroutine ncnn_option_set_use_fp16_storage(opt, enable) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int), value :: enable
        end subroutine

        function ncnn_option_get_use_fp16_arithmetic(opt) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int) :: ncnn_option_get_use_fp16_arithmetic
        end function

        subroutine ncnn_option_set_use_fp16_arithmetic(opt, enable) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int), value :: enable
        end subroutine

        function ncnn_option_get_use_int8_packed(opt) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int) :: ncnn_option_get_use_int8_packed
        end function

        subroutine ncnn_option_set_use_int8_packed(opt, enable) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int), value :: enable
        end subroutine

        function ncnn_option_get_use_int8_storage(opt) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int) :: ncnn_option_get_use_int8_storage
        end function

        subroutine ncnn_option_set_use_int8_storage(opt, enable) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int), value :: enable
        end subroutine

        function ncnn_option_get_use_int8_arithmetic(opt) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int) :: ncnn_option_get_use_int8_arithmetic
        end function

        subroutine ncnn_option_set_use_int8_arithmetic(opt, enable) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int), value :: enable
        end subroutine

        function ncnn_option_get_use_int16_packed(opt) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int) :: ncnn_option_get_use_int16_packed
        end function

        subroutine ncnn_option_set_use_int16_packed(opt, enable) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int), value :: enable
        end subroutine

        function ncnn_option_get_use_int16_storage(opt) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int) :: ncnn_option_get_use_int16_storage
        end function

        subroutine ncnn_option_set_use_int16_storage(opt, enable) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int), value :: enable
        end subroutine

        function ncnn_option_get_use_bf16_packed(opt) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int) :: ncnn_option_get_use_bf16_packed
        end function

        subroutine ncnn_option_set_use_bf16_packed(opt, enable) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int), value :: enable
        end subroutine

        function ncnn_option_get_use_bf16_storage(opt) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int) :: ncnn_option_get_use_bf16_storage
        end function

        subroutine ncnn_option_set_use_bf16_storage(opt, enable) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int), value :: enable
        end subroutine

        function ncnn_option_get_use_shader_local_memory(opt) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int) :: ncnn_option_get_use_shader_local_memory
        end function

        subroutine ncnn_option_set_use_shader_local_memory(opt, enable) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int), value :: enable
        end subroutine

        function ncnn_option_get_use_cooperative_matrix(opt) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int) :: ncnn_option_get_use_cooperative_matrix
        end function

        subroutine ncnn_option_set_use_cooperative_matrix(opt, enable) bind(c)
            import :: c_ptr, c_int
            type(c_ptr), value :: opt
            integer(c_int), value :: enable
        end subroutine

    end interface

contains

    !--------------------------------------------------------------------------
    ! version api
    !--------------------------------------------------------------------------

    !> const char* ncnn_version()
    function version() result(v)
        character(len=:), allocatable :: v
        v = c_f_string(ncnn_version())
    end function version

    !> int ncnn_version_number()
    function version_number() result(n)
        integer :: n
        n = ncnn_version_number()
    end function version_number

    !--------------------------------------------------------------------------
    ! allocator api
    !--------------------------------------------------------------------------

    !> ncnn_allocator_t ncnn_allocator_create_pool_allocator()
    function allocator_create_pool() result(allocator)
        type(ncnn_allocator_t) :: allocator
        allocator%ptr = ncnn_allocator_create_pool_allocator()
    end function allocator_create_pool

    !> ncnn_allocator_t ncnn_allocator_create_unlocked_pool_allocator()
    function allocator_create_unlocked_pool() result(allocator)
        type(ncnn_allocator_t) :: allocator
        allocator%ptr = ncnn_allocator_create_unlocked_pool_allocator()
    end function allocator_create_unlocked_pool

    !> void ncnn_allocator_destroy(ncnn_allocator_t allocator)
    subroutine allocator_destroy(allocator)
        type(ncnn_allocator_t), intent(inout) :: allocator
        call ncnn_allocator_destroy(allocator%ptr)
        allocator%ptr = c_null_ptr
    end subroutine allocator_destroy

    !--------------------------------------------------------------------------
    ! option api
    !--------------------------------------------------------------------------

    !> ncnn_option_t ncnn_option_create()
    function option_create() result(opt)
        type(ncnn_option_t) :: opt
        opt%ptr = ncnn_option_create()
    end function option_create

    !> void ncnn_option_destroy(ncnn_option_t opt)
    subroutine option_destroy(opt)
        type(ncnn_option_t), intent(inout) :: opt
        call ncnn_option_destroy(opt%ptr)
        opt%ptr = c_null_ptr
    end subroutine option_destroy

    !> int ncnn_option_get_num_threads(const ncnn_option_t opt)
    function option_get_num_threads(opt) result(num_threads)
        type(ncnn_option_t), intent(in) :: opt
        integer :: num_threads
        num_threads = ncnn_option_get_num_threads(opt%ptr)
    end function option_get_num_threads

    !> void ncnn_option_set_num_threads(ncnn_option_t opt, int num_threads)
    subroutine option_set_num_threads(opt, num_threads)
        type(ncnn_option_t), intent(in) :: opt
        integer, intent(in)           :: num_threads
        call ncnn_option_set_num_threads(opt%ptr, num_threads)
    end subroutine option_set_num_threads

    !> void ncnn_option_set_blob_allocator(ncnn_option_t opt, ncnn_allocator_t allocator)
    subroutine option_set_blob_allocator(opt, allocator)
        type(ncnn_option_t), intent(in)    :: opt
        type(ncnn_allocator_t), intent(in) :: allocator
        call ncnn_option_set_blob_allocator(opt%ptr, allocator%ptr)
    end subroutine option_set_blob_allocator

    !> void ncnn_option_set_workspace_allocator(ncnn_option_t opt, ncnn_allocator_t allocator)
    subroutine option_set_workspace_allocator(opt, allocator)
        type(ncnn_option_t), intent(in)    :: opt
        type(ncnn_allocator_t), intent(in) :: allocator
        call ncnn_option_set_workspace_allocator(opt%ptr, allocator%ptr)
    end subroutine option_set_workspace_allocator

    !> void ncnn_option_set_kvcache_allocator(ncnn_option_t opt, ncnn_allocator_t allocator)
    subroutine option_set_kvcache_allocator(opt, allocator)
        type(ncnn_option_t), intent(in)    :: opt
        type(ncnn_allocator_t), intent(in) :: allocator
        call ncnn_option_set_kvcache_allocator(opt%ptr, allocator%ptr)
    end subroutine option_set_kvcache_allocator

    !> void ncnn_option_set_kvcache_max_seqlen_hint(ncnn_option_t opt, int max_seqlen_hint)
    subroutine option_set_kvcache_max_seqlen_hint(opt, max_seqlen_hint)
        type(ncnn_option_t), intent(in) :: opt
        integer, intent(in)           :: max_seqlen_hint
        call ncnn_option_set_kvcache_max_seqlen_hint(opt%ptr, max_seqlen_hint)
    end subroutine option_set_kvcache_max_seqlen_hint

    ! -- option flag getters/setters -------------------------------------------
    ! Every use_* flag is a C int used as a boolean; 0 = disabled, 1 = enabled.

    function option_get_use_vulkan_compute(opt) result(v)
        type(ncnn_option_t), intent(in) :: opt
        integer :: v
        v = ncnn_option_get_use_vulkan_compute(opt%ptr)
    end function

    subroutine option_set_use_vulkan_compute(opt, enable)
        type(ncnn_option_t), intent(in) :: opt
        integer, intent(in)           :: enable
        call ncnn_option_set_use_vulkan_compute(opt%ptr, enable)
    end subroutine

    function option_get_use_local_pool_allocator(opt) result(v)
        type(ncnn_option_t), intent(in) :: opt
        integer :: v
        v = ncnn_option_get_use_local_pool_allocator(opt%ptr)
    end function

    subroutine option_set_use_local_pool_allocator(opt, enable)
        type(ncnn_option_t), intent(in) :: opt
        integer, intent(in)           :: enable
        call ncnn_option_set_use_local_pool_allocator(opt%ptr, enable)
    end subroutine

    function option_get_use_winograd_convolution(opt) result(v)
        type(ncnn_option_t), intent(in) :: opt
        integer :: v
        v = ncnn_option_get_use_winograd_convolution(opt%ptr)
    end function

    subroutine option_set_use_winograd_convolution(opt, enable)
        type(ncnn_option_t), intent(in) :: opt
        integer, intent(in)           :: enable
        call ncnn_option_set_use_winograd_convolution(opt%ptr, enable)
    end subroutine

    function option_get_use_sgemm_convolution(opt) result(v)
        type(ncnn_option_t), intent(in) :: opt
        integer :: v
        v = ncnn_option_get_use_sgemm_convolution(opt%ptr)
    end function

    subroutine option_set_use_sgemm_convolution(opt, enable)
        type(ncnn_option_t), intent(in) :: opt
        integer, intent(in)           :: enable
        call ncnn_option_set_use_sgemm_convolution(opt%ptr, enable)
    end subroutine

    function option_get_use_packing_layout(opt) result(v)
        type(ncnn_option_t), intent(in) :: opt
        integer :: v
        v = ncnn_option_get_use_packing_layout(opt%ptr)
    end function

    subroutine option_set_use_packing_layout(opt, enable)
        type(ncnn_option_t), intent(in) :: opt
        integer, intent(in)           :: enable
        call ncnn_option_set_use_packing_layout(opt%ptr, enable)
    end subroutine

    function option_get_use_fp16_packed(opt) result(v)
        type(ncnn_option_t), intent(in) :: opt
        integer :: v
        v = ncnn_option_get_use_fp16_packed(opt%ptr)
    end function

    subroutine option_set_use_fp16_packed(opt, enable)
        type(ncnn_option_t), intent(in) :: opt
        integer, intent(in)           :: enable
        call ncnn_option_set_use_fp16_packed(opt%ptr, enable)
    end subroutine

    function option_get_use_fp16_storage(opt) result(v)
        type(ncnn_option_t), intent(in) :: opt
        integer :: v
        v = ncnn_option_get_use_fp16_storage(opt%ptr)
    end function

    subroutine option_set_use_fp16_storage(opt, enable)
        type(ncnn_option_t), intent(in) :: opt
        integer, intent(in)           :: enable
        call ncnn_option_set_use_fp16_storage(opt%ptr, enable)
    end subroutine

    function option_get_use_fp16_arithmetic(opt) result(v)
        type(ncnn_option_t), intent(in) :: opt
        integer :: v
        v = ncnn_option_get_use_fp16_arithmetic(opt%ptr)
    end function

    subroutine option_set_use_fp16_arithmetic(opt, enable)
        type(ncnn_option_t), intent(in) :: opt
        integer, intent(in)           :: enable
        call ncnn_option_set_use_fp16_arithmetic(opt%ptr, enable)
    end subroutine

    function option_get_use_int8_packed(opt) result(v)
        type(ncnn_option_t), intent(in) :: opt
        integer :: v
        v = ncnn_option_get_use_int8_packed(opt%ptr)
    end function

    subroutine option_set_use_int8_packed(opt, enable)
        type(ncnn_option_t), intent(in) :: opt
        integer, intent(in)           :: enable
        call ncnn_option_set_use_int8_packed(opt%ptr, enable)
    end subroutine

    function option_get_use_int8_storage(opt) result(v)
        type(ncnn_option_t), intent(in) :: opt
        integer :: v
        v = ncnn_option_get_use_int8_storage(opt%ptr)
    end function

    subroutine option_set_use_int8_storage(opt, enable)
        type(ncnn_option_t), intent(in) :: opt
        integer, intent(in)           :: enable
        call ncnn_option_set_use_int8_storage(opt%ptr, enable)
    end subroutine

    function option_get_use_int8_arithmetic(opt) result(v)
        type(ncnn_option_t), intent(in) :: opt
        integer :: v
        v = ncnn_option_get_use_int8_arithmetic(opt%ptr)
    end function

    subroutine option_set_use_int8_arithmetic(opt, enable)
        type(ncnn_option_t), intent(in) :: opt
        integer, intent(in)           :: enable
        call ncnn_option_set_use_int8_arithmetic(opt%ptr, enable)
    end subroutine

    function option_get_use_int16_packed(opt) result(v)
        type(ncnn_option_t), intent(in) :: opt
        integer :: v
        v = ncnn_option_get_use_int16_packed(opt%ptr)
    end function

    subroutine option_set_use_int16_packed(opt, enable)
        type(ncnn_option_t), intent(in) :: opt
        integer, intent(in)           :: enable
        call ncnn_option_set_use_int16_packed(opt%ptr, enable)
    end subroutine

    function option_get_use_int16_storage(opt) result(v)
        type(ncnn_option_t), intent(in) :: opt
        integer :: v
        v = ncnn_option_get_use_int16_storage(opt%ptr)
    end function

    subroutine option_set_use_int16_storage(opt, enable)
        type(ncnn_option_t), intent(in) :: opt
        integer, intent(in)           :: enable
        call ncnn_option_set_use_int16_storage(opt%ptr, enable)
    end subroutine

    function option_get_use_bf16_packed(opt) result(v)
        type(ncnn_option_t), intent(in) :: opt
        integer :: v
        v = ncnn_option_get_use_bf16_packed(opt%ptr)
    end function

    subroutine option_set_use_bf16_packed(opt, enable)
        type(ncnn_option_t), intent(in) :: opt
        integer, intent(in)           :: enable
        call ncnn_option_set_use_bf16_packed(opt%ptr, enable)
    end subroutine

    function option_get_use_bf16_storage(opt) result(v)
        type(ncnn_option_t), intent(in) :: opt
        integer :: v
        v = ncnn_option_get_use_bf16_storage(opt%ptr)
    end function

    subroutine option_set_use_bf16_storage(opt, enable)
        type(ncnn_option_t), intent(in) :: opt
        integer, intent(in)           :: enable
        call ncnn_option_set_use_bf16_storage(opt%ptr, enable)
    end subroutine

    function option_get_use_shader_local_memory(opt) result(v)
        type(ncnn_option_t), intent(in) :: opt
        integer :: v
        v = ncnn_option_get_use_shader_local_memory(opt%ptr)
    end function

    subroutine option_set_use_shader_local_memory(opt, enable)
        type(ncnn_option_t), intent(in) :: opt
        integer, intent(in)           :: enable
        call ncnn_option_set_use_shader_local_memory(opt%ptr, enable)
    end subroutine

    function option_get_use_cooperative_matrix(opt) result(v)
        type(ncnn_option_t), intent(in) :: opt
        integer :: v
        v = ncnn_option_get_use_cooperative_matrix(opt%ptr)
    end function

    subroutine option_set_use_cooperative_matrix(opt, enable)
        type(ncnn_option_t), intent(in) :: opt
        integer, intent(in)           :: enable
        call ncnn_option_set_use_cooperative_matrix(opt%ptr, enable)
    end subroutine

end module ncnn_core
