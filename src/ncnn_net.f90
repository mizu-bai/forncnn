!------------------------------------------------------------------------------
! MODULE: ncnn_net
!
! DESCRIPTION:
!>  ncnn net API (20 functions).
!
!>  NOT BOUND: ncnn_net_set_vulkan_device (requires a Vulkan build) and
!>  ncnn_net_register_custom_layer_by_type / _by_typeindex. The latter two take
!>  a layer creator callback, but the resulting layer would have no usable
!>  method table from Fortran -- see the note in ncnn_layer.
!------------------------------------------------------------------------------
module ncnn_net
    use, intrinsic :: iso_c_binding
    use ncnn_types, only: ncnn_net_t, ncnn_option_t, ncnn_datareader_t, c_f_string, fill_c_string
    implicit none
    private

    public :: net_create, net_destroy, net_get_option, net_set_option, net_clear, &
              net_load_param, net_load_param_bin, net_load_model, &
              net_load_param_memory, net_load_param_bin_memory, net_load_model_memory, &
              net_load_param_datareader, net_load_param_bin_datareader, &
              net_load_model_datareader, &
              net_get_input_count, net_get_output_count, &
              net_get_input_name, net_get_output_name, &
              net_get_input_index, net_get_output_index

    interface

        function ncnn_net_create() bind(c)
            import :: c_ptr
            type(c_ptr) :: ncnn_net_create
        end function

        subroutine ncnn_net_destroy(net) bind(c)
            import :: c_ptr
            type(c_ptr), value :: net
        end subroutine

        function ncnn_net_get_option(net) bind(c)
            import :: c_ptr
            type(c_ptr), value :: net
            type(c_ptr) :: ncnn_net_get_option
        end function

        subroutine ncnn_net_set_option(net, opt) bind(c)
            import :: c_ptr
            type(c_ptr), value :: net
            type(c_ptr), value :: opt
        end subroutine

        function ncnn_net_load_param(net, path) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: net
            type(c_ptr), value    :: path
            integer(c_int) :: ncnn_net_load_param
        end function

        function ncnn_net_load_param_bin(net, path) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: net
            type(c_ptr), value    :: path
            integer(c_int) :: ncnn_net_load_param_bin
        end function

        function ncnn_net_load_model(net, path) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: net
            type(c_ptr), value    :: path
            integer(c_int) :: ncnn_net_load_model
        end function

        function ncnn_net_load_param_memory(net, mem) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value :: net
            type(c_ptr), value :: mem
            integer(c_int) :: ncnn_net_load_param_memory
        end function

        function ncnn_net_load_param_bin_memory(net, mem) bind(c)
            import :: c_ptr, c_size_t
            type(c_ptr), value    :: net
            type(c_ptr), value    :: mem
            integer(c_size_t) :: ncnn_net_load_param_bin_memory
        end function

        function ncnn_net_load_model_memory(net, mem) bind(c)
            import :: c_ptr, c_size_t
            type(c_ptr), value    :: net
            type(c_ptr), value    :: mem
            integer(c_size_t) :: ncnn_net_load_model_memory
        end function

        function ncnn_net_load_param_datareader(net, dr) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value :: net
            type(c_ptr), value :: dr
            integer(c_int) :: ncnn_net_load_param_datareader
        end function

        function ncnn_net_load_param_bin_datareader(net, dr) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value :: net
            type(c_ptr), value :: dr
            integer(c_int) :: ncnn_net_load_param_bin_datareader
        end function

        function ncnn_net_load_model_datareader(net, dr) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value :: net
            type(c_ptr), value :: dr
            integer(c_int) :: ncnn_net_load_model_datareader
        end function

        subroutine ncnn_net_clear(net) bind(c)
            import :: c_ptr
            type(c_ptr), value :: net
        end subroutine

        function ncnn_net_get_input_count(net) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value :: net
            integer(c_int) :: ncnn_net_get_input_count
        end function

        function ncnn_net_get_output_count(net) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value :: net
            integer(c_int) :: ncnn_net_get_output_count
        end function

        function ncnn_net_get_input_name(net, i) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: net
            integer(c_int), value :: i
            type(c_ptr) :: ncnn_net_get_input_name
        end function

        function ncnn_net_get_output_name(net, i) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: net
            integer(c_int), value :: i
            type(c_ptr) :: ncnn_net_get_output_name
        end function

        function ncnn_net_get_input_index(net, i) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: net
            integer(c_int), value :: i
            integer(c_int) :: ncnn_net_get_input_index
        end function

        function ncnn_net_get_output_index(net, i) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: net
            integer(c_int), value :: i
            integer(c_int) :: ncnn_net_get_output_index
        end function

    end interface

contains

    !> ncnn_net_t ncnn_net_create(void)
    function net_create() result(net)
        type(ncnn_net_t) :: net
        net%ptr = ncnn_net_create()
    end function net_create

    !> void ncnn_net_destroy(ncnn_net_t net)
    subroutine net_destroy(net)
        type(ncnn_net_t), intent(inout) :: net
        call ncnn_net_destroy(net%ptr)
        net%ptr = c_null_ptr
    end subroutine net_destroy

    !> ncnn_option_t ncnn_net_get_option(ncnn_net_t net)
    !> The returned handle is owned by the net; do not destroy it.
    function net_get_option(net) result(opt)
        type(ncnn_net_t), intent(in) :: net
        type(ncnn_option_t)          :: opt
        opt%ptr = ncnn_net_get_option(net%ptr)
    end function net_get_option

    !> void ncnn_net_set_option(ncnn_net_t net, ncnn_option_t opt)
    subroutine net_set_option(net, opt)
        type(ncnn_net_t), intent(in)    :: net
        type(ncnn_option_t), intent(in) :: opt
        call ncnn_net_set_option(net%ptr, opt%ptr)
    end subroutine net_set_option

    !> void ncnn_net_clear(ncnn_net_t net)
    subroutine net_clear(net)
        type(ncnn_net_t), intent(in) :: net
        call ncnn_net_clear(net%ptr)
    end subroutine net_clear

    !--------------------------------------------------------------------------
    ! load from file
    !--------------------------------------------------------------------------

    !> int ncnn_net_load_param(ncnn_net_t net, const char* path)
    !> Returns 0 on success, negative on failure.
    function net_load_param(net, path) result(stat)
        type(ncnn_net_t), intent(inout) :: net
        character(len=*), intent(in)    :: path
        integer                         :: stat

        character(kind=c_char), target :: cpath(len_trim(path) + 1)

        call fill_c_string(path, cpath)
        stat = ncnn_net_load_param(net%ptr, c_loc(cpath))
    end function net_load_param

    !> int ncnn_net_load_param_bin(ncnn_net_t net, const char* path)
    function net_load_param_bin(net, path) result(stat)
        type(ncnn_net_t), intent(inout) :: net
        character(len=*), intent(in)    :: path
        integer                         :: stat

        character(kind=c_char), target :: cpath(len_trim(path) + 1)

        call fill_c_string(path, cpath)
        stat = ncnn_net_load_param_bin(net%ptr, c_loc(cpath))
    end function net_load_param_bin

    !> int ncnn_net_load_model(ncnn_net_t net, const char* path)
    function net_load_model(net, path) result(stat)
        type(ncnn_net_t), intent(inout) :: net
        character(len=*), intent(in)    :: path
        integer                         :: stat

        character(kind=c_char), target :: cpath(len_trim(path) + 1)

        call fill_c_string(path, cpath)
        stat = ncnn_net_load_model(net%ptr, c_loc(cpath))
    end function net_load_model

    !--------------------------------------------------------------------------
    ! load from memory -- the *_bin_memory and load_model variants return the
    ! number of bytes consumed
    !--------------------------------------------------------------------------

    !> int ncnn_net_load_param_memory(ncnn_net_t net, const char* mem)
    !> mem must be a NUL-terminated c_char buffer holding the param file text.
    function net_load_param_memory(net, mem) result(stat)
        type(ncnn_net_t), intent(inout) :: net
        type(c_ptr), intent(in), value  :: mem
        integer                         :: stat
        stat = ncnn_net_load_param_memory(net%ptr, mem)
    end function net_load_param_memory

    !> size_t ncnn_net_load_param_bin_memory(ncnn_net_t net, const unsigned char* mem)
    !> Returns the number of bytes consumed.
    function net_load_param_bin_memory(net, mem) result(used)
        type(ncnn_net_t), intent(inout) :: net
        type(c_ptr), intent(in), value  :: mem
        integer(c_size_t)               :: used
        used = ncnn_net_load_param_bin_memory(net%ptr, mem)
    end function net_load_param_bin_memory

    !> size_t ncnn_net_load_model_memory(ncnn_net_t net, const unsigned char* mem)
    !> Returns the number of bytes consumed.
    function net_load_model_memory(net, mem) result(used)
        type(ncnn_net_t), intent(inout) :: net
        type(c_ptr), intent(in), value  :: mem
        integer(c_size_t)               :: used
        used = ncnn_net_load_model_memory(net%ptr, mem)
    end function net_load_model_memory

    !--------------------------------------------------------------------------
    ! load from datareader
    !--------------------------------------------------------------------------

    !> int ncnn_net_load_param_datareader(ncnn_net_t net, const ncnn_datareader_t dr)
    function net_load_param_datareader(net, dr) result(stat)
        type(ncnn_net_t), intent(inout)        :: net
        type(ncnn_datareader_t), intent(in)    :: dr
        integer                                :: stat
        stat = ncnn_net_load_param_datareader(net%ptr, dr%ptr)
    end function net_load_param_datareader

    !> int ncnn_net_load_param_bin_datareader(ncnn_net_t net, const ncnn_datareader_t dr)
    function net_load_param_bin_datareader(net, dr) result(stat)
        type(ncnn_net_t), intent(inout)        :: net
        type(ncnn_datareader_t), intent(in)    :: dr
        integer                                :: stat
        stat = ncnn_net_load_param_bin_datareader(net%ptr, dr%ptr)
    end function net_load_param_bin_datareader

    !> int ncnn_net_load_model_datareader(ncnn_net_t net, const ncnn_datareader_t dr)
    function net_load_model_datareader(net, dr) result(stat)
        type(ncnn_net_t), intent(inout)        :: net
        type(ncnn_datareader_t), intent(in)    :: dr
        integer                                :: stat
        stat = ncnn_net_load_model_datareader(net%ptr, dr%ptr)
    end function net_load_model_datareader

    !--------------------------------------------------------------------------
    ! introspection
    !--------------------------------------------------------------------------

    !> int ncnn_net_get_input_count(const ncnn_net_t net)
    function net_get_input_count(net) result(n)
        type(ncnn_net_t), intent(in) :: net
        integer :: n
        n = ncnn_net_get_input_count(net%ptr)
    end function net_get_input_count

    !> int ncnn_net_get_output_count(const ncnn_net_t net)
    function net_get_output_count(net) result(n)
        type(ncnn_net_t), intent(in) :: net
        integer :: n
        n = ncnn_net_get_output_count(net%ptr)
    end function net_get_output_count

    !> const char* ncnn_net_get_input_name(const ncnn_net_t net, int i)
    !> i is 1-based.
    function net_get_input_name(net, i) result(name)
        type(ncnn_net_t), intent(in) :: net
        integer, intent(in)          :: i
        character(len=:), allocatable :: name

        if (i < 1 .or. i > ncnn_net_get_input_count(net%ptr)) then
            error stop 'ncnn: net_get_input_name index out of range'
        end if
        name = c_f_string(ncnn_net_get_input_name(net%ptr, i - 1))
    end function net_get_input_name

    !> const char* ncnn_net_get_output_name(const ncnn_net_t net, int i)
    !> i is 1-based.
    function net_get_output_name(net, i) result(name)
        type(ncnn_net_t), intent(in) :: net
        integer, intent(in)          :: i
        character(len=:), allocatable :: name

        if (i < 1 .or. i > ncnn_net_get_output_count(net%ptr)) then
            error stop 'ncnn: net_get_output_name index out of range'
        end if
        name = c_f_string(ncnn_net_get_output_name(net%ptr, i - 1))
    end function net_get_output_name

    !> int ncnn_net_get_input_index(const ncnn_net_t net, int i)
    !> i is 1-based; the returned blob index is 0-based, as in C.
    function net_get_input_index(net, i) result(index)
        type(ncnn_net_t), intent(in) :: net
        integer, intent(in)          :: i
        integer                      :: index

        if (i < 1 .or. i > ncnn_net_get_input_count(net%ptr)) then
            error stop 'ncnn: net_get_input_index index out of range'
        end if
        index = ncnn_net_get_input_index(net%ptr, i - 1)
    end function net_get_input_index

    !> int ncnn_net_get_output_index(const ncnn_net_t net, int i)
    !> i is 1-based; the returned blob index is 0-based, as in C.
    function net_get_output_index(net, i) result(index)
        type(ncnn_net_t), intent(in) :: net
        integer, intent(in)          :: i
        integer                      :: index

        if (i < 1 .or. i > ncnn_net_get_output_count(net%ptr)) then
            error stop 'ncnn: net_get_output_index index out of range'
        end if
        index = ncnn_net_get_output_index(net%ptr, i - 1)
    end function net_get_output_index

end module ncnn_net
