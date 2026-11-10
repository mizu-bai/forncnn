!------------------------------------------------------------------------------
! MODULE: ncnn_layer
!
! DESCRIPTION:
!>  ncnn layer introspection API (30 functions).
!
!>  NOT BOUND: the layer method table (load_param, load_model, create_pipeline,
!>  destroy_pipeline, forward_1, forward_n, forward_inplace_1,
!>  forward_inplace_n). Those are function pointers inside the ncnn_layer_t
!>  struct rather than standalone C functions, so calling them would mean
!>  reproducing the struct layout in Fortran and dispatching through
!>  c_f_procpointer. The practical consequence is that implementing a custom
!>  layer body in pure Fortran is not supported.
!------------------------------------------------------------------------------
module ncnn_layer
    use, intrinsic :: iso_c_binding
    use ncnn_types, only: ncnn_layer_t, ncnn_shape, c_f_string, fill_c_string
    implicit none
    private

    public :: layer_create, layer_create_by_typeindex, layer_create_by_type, &
              layer_type_to_index, layer_destroy, &
              layer_get_name, layer_get_typeindex, layer_get_type, &
              layer_get_one_blob_only, layer_set_one_blob_only, &
              layer_get_support_inplace, layer_set_support_inplace, &
              layer_get_support_vulkan, layer_set_support_vulkan, &
              layer_get_support_packing, layer_set_support_packing, &
              layer_get_support_bf16_storage, layer_set_support_bf16_storage, &
              layer_get_support_fp16_storage, layer_set_support_fp16_storage, &
              layer_get_support_vulkan_packing, layer_set_support_vulkan_packing, &
              layer_get_support_any_packing, layer_set_support_any_packing, &
              layer_get_support_vulkan_any_packing, layer_set_support_vulkan_any_packing, &
              layer_get_bottom_count, layer_get_bottom, &
              layer_get_top_count, layer_get_top, &
              layer_get_bottom_shape, layer_get_top_shape

    interface

        function ncnn_layer_create() bind(c)
            import :: c_ptr
            type(c_ptr) :: ncnn_layer_create
        end function

        function ncnn_layer_create_by_typeindex(typeindex) bind(c)
            import :: c_int, c_ptr
            integer(c_int), value :: typeindex
            type(c_ptr) :: ncnn_layer_create_by_typeindex
        end function

        function ncnn_layer_create_by_type(type) bind(c)
            import :: c_ptr
            type(c_ptr), value :: type
            type(c_ptr) :: ncnn_layer_create_by_type
        end function

        function ncnn_layer_type_to_index(type) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value :: type
            integer(c_int) :: ncnn_layer_type_to_index
        end function

        subroutine ncnn_layer_destroy(layer) bind(c)
            import :: c_ptr
            type(c_ptr), value :: layer
        end subroutine

        function ncnn_layer_get_name(layer) bind(c)
            import :: c_ptr
            type(c_ptr), value :: layer
            type(c_ptr) :: ncnn_layer_get_name
        end function

        function ncnn_layer_get_typeindex(layer) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value :: layer
            integer(c_int) :: ncnn_layer_get_typeindex
        end function

        function ncnn_layer_get_type(layer) bind(c)
            import :: c_ptr
            type(c_ptr), value :: layer
            type(c_ptr) :: ncnn_layer_get_type
        end function

        function ncnn_layer_get_one_blob_only(layer) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value :: layer
            integer(c_int) :: ncnn_layer_get_one_blob_only
        end function

        function ncnn_layer_get_support_inplace(layer) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value :: layer
            integer(c_int) :: ncnn_layer_get_support_inplace
        end function

        function ncnn_layer_get_support_vulkan(layer) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value :: layer
            integer(c_int) :: ncnn_layer_get_support_vulkan
        end function

        function ncnn_layer_get_support_packing(layer) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value :: layer
            integer(c_int) :: ncnn_layer_get_support_packing
        end function

        function ncnn_layer_get_support_bf16_storage(layer) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value :: layer
            integer(c_int) :: ncnn_layer_get_support_bf16_storage
        end function

        function ncnn_layer_get_support_fp16_storage(layer) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value :: layer
            integer(c_int) :: ncnn_layer_get_support_fp16_storage
        end function

        function ncnn_layer_get_support_vulkan_packing(layer) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value :: layer
            integer(c_int) :: ncnn_layer_get_support_vulkan_packing
        end function

        function ncnn_layer_get_support_any_packing(layer) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value :: layer
            integer(c_int) :: ncnn_layer_get_support_any_packing
        end function

        function ncnn_layer_get_support_vulkan_any_packing(layer) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value :: layer
            integer(c_int) :: ncnn_layer_get_support_vulkan_any_packing
        end function

        subroutine ncnn_layer_set_one_blob_only(layer, enable) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: layer
            integer(c_int), value :: enable
        end subroutine

        subroutine ncnn_layer_set_support_inplace(layer, enable) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: layer
            integer(c_int), value :: enable
        end subroutine

        subroutine ncnn_layer_set_support_vulkan(layer, enable) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: layer
            integer(c_int), value :: enable
        end subroutine

        subroutine ncnn_layer_set_support_packing(layer, enable) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: layer
            integer(c_int), value :: enable
        end subroutine

        subroutine ncnn_layer_set_support_bf16_storage(layer, enable) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: layer
            integer(c_int), value :: enable
        end subroutine

        subroutine ncnn_layer_set_support_fp16_storage(layer, enable) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: layer
            integer(c_int), value :: enable
        end subroutine

        subroutine ncnn_layer_set_support_vulkan_packing(layer, enable) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: layer
            integer(c_int), value :: enable
        end subroutine

        subroutine ncnn_layer_set_support_any_packing(layer, enable) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: layer
            integer(c_int), value :: enable
        end subroutine

        subroutine ncnn_layer_set_support_vulkan_any_packing(layer, enable) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: layer
            integer(c_int), value :: enable
        end subroutine

        function ncnn_layer_get_bottom_count(layer) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value :: layer
            integer(c_int) :: ncnn_layer_get_bottom_count
        end function

        function ncnn_layer_get_bottom(layer, i) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: layer
            integer(c_int), value :: i
            integer(c_int) :: ncnn_layer_get_bottom
        end function

        function ncnn_layer_get_top_count(layer) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value :: layer
            integer(c_int) :: ncnn_layer_get_top_count
        end function

        function ncnn_layer_get_top(layer, i) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: layer
            integer(c_int), value :: i
            integer(c_int) :: ncnn_layer_get_top
        end function

        ! C name is ncnn_blob_get_bottom_shape; it takes a layer, hence the
        ! Fortran name below.
        subroutine ncnn_blob_get_bottom_shape(layer, i, dims, w, h, c) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: layer
            integer(c_int), value :: i
            integer(c_int)        :: dims
            integer(c_int)        :: w
            integer(c_int)        :: h
            integer(c_int)        :: c
        end subroutine

        subroutine ncnn_blob_get_top_shape(layer, i, dims, w, h, c) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: layer
            integer(c_int), value :: i
            integer(c_int)        :: dims
            integer(c_int)        :: w
            integer(c_int)        :: h
            integer(c_int)        :: c
        end subroutine

    end interface

contains

    !> ncnn_layer_t ncnn_layer_create(void)
    function layer_create() result(layer)
        type(ncnn_layer_t) :: layer
        layer%ptr = ncnn_layer_create()
    end function layer_create

    !> ncnn_layer_t ncnn_layer_create_by_typeindex(int typeindex)
    function layer_create_by_typeindex(typeindex) result(layer)
        integer, intent(in) :: typeindex
        type(ncnn_layer_t)  :: layer
        layer%ptr = ncnn_layer_create_by_typeindex(typeindex)
    end function layer_create_by_typeindex

    !> ncnn_layer_t ncnn_layer_create_by_type(const char* type)
    function layer_create_by_type(type) result(layer)
        character(len=*), intent(in) :: type
        type(ncnn_layer_t)           :: layer

        character(kind=c_char), target :: ctype(len_trim(type) + 1)

        call fill_c_string(type, ctype)
        layer%ptr = ncnn_layer_create_by_type(c_loc(ctype))
    end function layer_create_by_type

    !> int ncnn_layer_type_to_index(const char* type)
    !> Returns -1 when the type name is not registered.
    function layer_type_to_index(type) result(index)
        character(len=*), intent(in) :: type
        integer                      :: index

        character(kind=c_char), target :: ctype(len_trim(type) + 1)

        call fill_c_string(type, ctype)
        index = ncnn_layer_type_to_index(c_loc(ctype))
    end function layer_type_to_index

    !> void ncnn_layer_destroy(ncnn_layer_t layer)
    subroutine layer_destroy(layer)
        type(ncnn_layer_t), intent(inout) :: layer
        call ncnn_layer_destroy(layer%ptr)
        layer%ptr = c_null_ptr
    end subroutine layer_destroy

    !> const char* ncnn_layer_get_name(const ncnn_layer_t layer)
    !>
    !> ncnn only fills Layer::name and Layer::type in Layer::load_param, which
    !> the C API does not expose as a callable function. A layer you create
    !> yourself therefore reports an empty name and type until something has
    !> loaded params into it; layer_get_typeindex is populated immediately and
    !> is the reliable identifier.
    function layer_get_name(layer) result(name)
        type(ncnn_layer_t), intent(in) :: layer
        character(len=:), allocatable  :: name
        name = c_f_string(ncnn_layer_get_name(layer%ptr))
    end function layer_get_name

    !> int ncnn_layer_get_typeindex(const ncnn_layer_t layer)
    function layer_get_typeindex(layer) result(typeindex)
        type(ncnn_layer_t), intent(in) :: layer
        integer :: typeindex
        typeindex = ncnn_layer_get_typeindex(layer%ptr)
    end function layer_get_typeindex

    !> const char* ncnn_layer_get_type(const ncnn_layer_t layer)
    !> See the note on layer_get_name: empty until load_param has run.
    function layer_get_type(layer) result(type)
        type(ncnn_layer_t), intent(in) :: layer
        character(len=:), allocatable  :: type
        type = c_f_string(ncnn_layer_get_type(layer%ptr))
    end function layer_get_type

    ! -- flags ---------------------------------------------------------------

    function layer_get_one_blob_only(layer) result(v)
        type(ncnn_layer_t), intent(in) :: layer
        integer :: v
        v = ncnn_layer_get_one_blob_only(layer%ptr)
    end function

    subroutine layer_set_one_blob_only(layer, enable)
        type(ncnn_layer_t), intent(inout) :: layer
        integer, intent(in)               :: enable
        call ncnn_layer_set_one_blob_only(layer%ptr, enable)
    end subroutine

    function layer_get_support_inplace(layer) result(v)
        type(ncnn_layer_t), intent(in) :: layer
        integer :: v
        v = ncnn_layer_get_support_inplace(layer%ptr)
    end function

    subroutine layer_set_support_inplace(layer, enable)
        type(ncnn_layer_t), intent(inout) :: layer
        integer, intent(in)               :: enable
        call ncnn_layer_set_support_inplace(layer%ptr, enable)
    end subroutine

    function layer_get_support_vulkan(layer) result(v)
        type(ncnn_layer_t), intent(in) :: layer
        integer :: v
        v = ncnn_layer_get_support_vulkan(layer%ptr)
    end function

    subroutine layer_set_support_vulkan(layer, enable)
        type(ncnn_layer_t), intent(inout) :: layer
        integer, intent(in)               :: enable
        call ncnn_layer_set_support_vulkan(layer%ptr, enable)
    end subroutine

    function layer_get_support_packing(layer) result(v)
        type(ncnn_layer_t), intent(in) :: layer
        integer :: v
        v = ncnn_layer_get_support_packing(layer%ptr)
    end function

    subroutine layer_set_support_packing(layer, enable)
        type(ncnn_layer_t), intent(inout) :: layer
        integer, intent(in)               :: enable
        call ncnn_layer_set_support_packing(layer%ptr, enable)
    end subroutine

    function layer_get_support_bf16_storage(layer) result(v)
        type(ncnn_layer_t), intent(in) :: layer
        integer :: v
        v = ncnn_layer_get_support_bf16_storage(layer%ptr)
    end function

    subroutine layer_set_support_bf16_storage(layer, enable)
        type(ncnn_layer_t), intent(inout) :: layer
        integer, intent(in)               :: enable
        call ncnn_layer_set_support_bf16_storage(layer%ptr, enable)
    end subroutine

    function layer_get_support_fp16_storage(layer) result(v)
        type(ncnn_layer_t), intent(in) :: layer
        integer :: v
        v = ncnn_layer_get_support_fp16_storage(layer%ptr)
    end function

    subroutine layer_set_support_fp16_storage(layer, enable)
        type(ncnn_layer_t), intent(inout) :: layer
        integer, intent(in)               :: enable
        call ncnn_layer_set_support_fp16_storage(layer%ptr, enable)
    end subroutine

    function layer_get_support_vulkan_packing(layer) result(v)
        type(ncnn_layer_t), intent(in) :: layer
        integer :: v
        v = ncnn_layer_get_support_vulkan_packing(layer%ptr)
    end function

    subroutine layer_set_support_vulkan_packing(layer, enable)
        type(ncnn_layer_t), intent(inout) :: layer
        integer, intent(in)               :: enable
        call ncnn_layer_set_support_vulkan_packing(layer%ptr, enable)
    end subroutine

    function layer_get_support_any_packing(layer) result(v)
        type(ncnn_layer_t), intent(in) :: layer
        integer :: v
        v = ncnn_layer_get_support_any_packing(layer%ptr)
    end function

    subroutine layer_set_support_any_packing(layer, enable)
        type(ncnn_layer_t), intent(inout) :: layer
        integer, intent(in)               :: enable
        call ncnn_layer_set_support_any_packing(layer%ptr, enable)
    end subroutine

    function layer_get_support_vulkan_any_packing(layer) result(v)
        type(ncnn_layer_t), intent(in) :: layer
        integer :: v
        v = ncnn_layer_get_support_vulkan_any_packing(layer%ptr)
    end function

    subroutine layer_set_support_vulkan_any_packing(layer, enable)
        type(ncnn_layer_t), intent(inout) :: layer
        integer, intent(in)               :: enable
        call ncnn_layer_set_support_vulkan_any_packing(layer%ptr, enable)
    end subroutine

    ! -- graph topology ------------------------------------------------------

    !> int ncnn_layer_get_bottom_count(const ncnn_layer_t layer)
    function layer_get_bottom_count(layer) result(n)
        type(ncnn_layer_t), intent(in) :: layer
        integer :: n
        n = ncnn_layer_get_bottom_count(layer%ptr)
    end function layer_get_bottom_count

    !> int ncnn_layer_get_bottom(const ncnn_layer_t layer, int i)
    !> i is 0-based in C and is kept 0-based here.
    function layer_get_bottom(layer, i) result(b)
        type(ncnn_layer_t), intent(in) :: layer
        integer, intent(in)            :: i
        integer                        :: b
        b = ncnn_layer_get_bottom(layer%ptr, i)
    end function layer_get_bottom

    !> int ncnn_layer_get_top_count(const ncnn_layer_t layer)
    function layer_get_top_count(layer) result(n)
        type(ncnn_layer_t), intent(in) :: layer
        integer :: n
        n = ncnn_layer_get_top_count(layer%ptr)
    end function layer_get_top_count

    !> int ncnn_layer_get_top(const ncnn_layer_t layer, int i)
    !> i is 0-based in C and is kept 0-based here.
    function layer_get_top(layer, i) result(t)
        type(ncnn_layer_t), intent(in) :: layer
        integer, intent(in)            :: i
        integer                        :: t
        t = ncnn_layer_get_top(layer%ptr, i)
    end function layer_get_top

    !> void ncnn_blob_get_bottom_shape(const ncnn_layer_t layer, int i, int* dims, int* w, int* h, int* c)
    !> i is 0-based in C and is kept 0-based here.
    function layer_get_bottom_shape(layer, i) result(shape)
        type(ncnn_layer_t), intent(in) :: layer
        integer, intent(in)            :: i
        type(ncnn_shape)               :: shape

        integer(c_int) :: dims, w, h, c

        call ncnn_blob_get_bottom_shape(layer%ptr, i, dims, w, h, c)
        shape%dims = dims
        shape%w    = w
        shape%h    = h
        shape%d    = 0
        shape%c    = c
    end function layer_get_bottom_shape

    !> void ncnn_blob_get_top_shape(const ncnn_layer_t layer, int i, int* dims, int* w, int* h, int* c)
    !> i is 0-based in C and is kept 0-based here.
    function layer_get_top_shape(layer, i) result(shape)
        type(ncnn_layer_t), intent(in) :: layer
        integer, intent(in)            :: i
        type(ncnn_shape)               :: shape

        integer(c_int) :: dims, w, h, c

        call ncnn_blob_get_top_shape(layer%ptr, i, dims, w, h, c)
        shape%dims = dims
        shape%w    = w
        shape%h    = h
        shape%d    = 0
        shape%c    = c
    end function layer_get_top_shape

end module ncnn_layer
