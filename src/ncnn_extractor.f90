!------------------------------------------------------------------------------
! MODULE: ncnn_extractor
!
! DESCRIPTION:
!>  ncnn extractor API (7 functions).
!>
!>  extractor_extract / extractor_extract_index return both the result Mat and
!>  a status, so they are subroutines with two intent(out) arguments rather
!>  than functions. The result Mat is owned by the caller and must be
!>  destroyed; a failed extract leaves it with a null handle.
!------------------------------------------------------------------------------
module ncnn_extractor
    use, intrinsic :: iso_c_binding
    use ncnn_types, only: ncnn_extractor_t, ncnn_net_t, ncnn_option_t, &
                          ncnn_mat_t, fill_c_string
    implicit none
    private

    public :: extractor_create, extractor_destroy, extractor_set_option, &
              extractor_input, extractor_extract, &
              extractor_input_index, extractor_extract_index

    interface

        function ncnn_extractor_create(net) bind(c)
            import :: c_ptr
            type(c_ptr), value :: net
            type(c_ptr) :: ncnn_extractor_create
        end function

        subroutine ncnn_extractor_destroy(ex) bind(c)
            import :: c_ptr
            type(c_ptr), value :: ex
        end subroutine

        subroutine ncnn_extractor_set_option(ex, opt) bind(c)
            import :: c_ptr
            type(c_ptr), value :: ex
            type(c_ptr), value :: opt
        end subroutine

        function ncnn_extractor_input(ex, name, mat) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: ex
            type(c_ptr), value    :: name
            type(c_ptr), value    :: mat
            integer(c_int) :: ncnn_extractor_input
        end function

        function ncnn_extractor_extract(ex, name, mat) bind(c) result(stat)
            import :: c_int, c_ptr
            type(c_ptr), value    :: ex
            type(c_ptr), value    :: name
            type(c_ptr)           :: mat
            integer(c_int) :: stat
        end function

        function ncnn_extractor_input_index(ex, index, mat) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: ex
            integer(c_int), value :: index
            type(c_ptr), value    :: mat
            integer(c_int) :: ncnn_extractor_input_index
        end function

        function ncnn_extractor_extract_index(ex, index, mat) bind(c) result(stat)
            import :: c_int, c_ptr
            type(c_ptr), value    :: ex
            integer(c_int), value :: index
            type(c_ptr)           :: mat
            integer(c_int) :: stat
        end function

    end interface

contains

    !> ncnn_extractor_t ncnn_extractor_create(ncnn_net_t net)
    function extractor_create(net) result(ex)
        type(ncnn_net_t), intent(in) :: net
        type(ncnn_extractor_t)       :: ex
        ex%ptr = ncnn_extractor_create(net%ptr)
    end function extractor_create

    !> void ncnn_extractor_destroy(ncnn_extractor_t ex)
    subroutine extractor_destroy(ex)
        type(ncnn_extractor_t), intent(inout) :: ex
        call ncnn_extractor_destroy(ex%ptr)
        ex%ptr = c_null_ptr
    end subroutine extractor_destroy

    !> void ncnn_extractor_set_option(ncnn_extractor_t ex, const ncnn_option_t opt)
    subroutine extractor_set_option(ex, opt)
        type(ncnn_extractor_t), intent(in) :: ex
        type(ncnn_option_t), intent(in)    :: opt
        call ncnn_extractor_set_option(ex%ptr, opt%ptr)
    end subroutine extractor_set_option

    !> int ncnn_extractor_input(ncnn_extractor_t ex, const char* name, const ncnn_mat_t mat)
    function extractor_input(ex, name, mat) result(stat)
        type(ncnn_extractor_t), intent(in) :: ex
        character(len=*), intent(in)       :: name
        type(ncnn_mat_t), intent(in)       :: mat
        integer                            :: stat

        character(kind=c_char), target :: cname(len_trim(name) + 1)

        call fill_c_string(name, cname)
        stat = ncnn_extractor_input(ex%ptr, c_loc(cname), mat%ptr)
    end function extractor_input

    !> int ncnn_extractor_extract(ncnn_extractor_t ex, const char* name, ncnn_mat_t* mat)
    !> stat is 0 on success.
    !>
    !> The C side allocates a new Mat unconditionally, so mat%ptr is set even
    !> when the extraction fails -- it then wraps an empty Mat rather than
    !> being null. Destroy it either way, or it leaks.
    subroutine extractor_extract(ex, name, mat, stat)
        type(ncnn_extractor_t), intent(in)  :: ex
        character(len=*), intent(in)        :: name
        type(ncnn_mat_t), intent(out)       :: mat
        integer, intent(out)                :: stat

        character(kind=c_char), target :: cname(len_trim(name) + 1)

        call fill_c_string(name, cname)
        stat = ncnn_extractor_extract(ex%ptr, c_loc(cname), mat%ptr)
    end subroutine extractor_extract

    !> int ncnn_extractor_input_index(ncnn_extractor_t ex, int index, const ncnn_mat_t mat)
    !> index is the 0-based blob index, as in C.
    function extractor_input_index(ex, index, mat) result(stat)
        type(ncnn_extractor_t), intent(in) :: ex
        integer, intent(in)                :: index
        type(ncnn_mat_t), intent(in)       :: mat
        integer                            :: stat
        stat = ncnn_extractor_input_index(ex%ptr, index, mat%ptr)
    end function extractor_input_index

    !> int ncnn_extractor_extract_index(ncnn_extractor_t ex, int index, ncnn_mat_t* mat)
    !> index is the 0-based blob index, as in C.
    !> As with extractor_extract, mat is set even on failure and must be
    !> destroyed either way.
    subroutine extractor_extract_index(ex, index, mat, stat)
        type(ncnn_extractor_t), intent(in)  :: ex
        integer, intent(in)                 :: index
        type(ncnn_mat_t), intent(out)       :: mat
        integer, intent(out)                :: stat

        stat = ncnn_extractor_extract_index(ex%ptr, index, mat%ptr)
    end subroutine extractor_extract_index

end module ncnn_extractor
