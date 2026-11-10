!------------------------------------------------------------------------------
! MODULE: ncnn_paramdict
!
! DESCRIPTION:
!>  ncnn ParamDict API (9 functions).
!
!>  paramdict_get_type returns ncnn-internal type codes. The values written by
!>  the setters are 2 = int, 3 = float, 4 = array. Codes read back from a
!>  parsed param file differ, so the raw value is returned unmodified rather
!>  than being mapped onto a Fortran enum.
!------------------------------------------------------------------------------
module ncnn_paramdict
    use, intrinsic :: iso_c_binding
    use ncnn_types, only: ncnn_paramdict_t, ncnn_mat_t
    implicit none
    private

    public :: paramdict_create, paramdict_destroy, &
              paramdict_get_type, paramdict_get_int, paramdict_get_float, &
              paramdict_get_array, &
              paramdict_set_int, paramdict_set_float, paramdict_set_array

    interface

        function ncnn_paramdict_create() bind(c)
            import :: c_ptr
            type(c_ptr) :: ncnn_paramdict_create
        end function

        subroutine ncnn_paramdict_destroy(pd) bind(c)
            import :: c_ptr
            type(c_ptr), value :: pd
        end subroutine

        function ncnn_paramdict_get_type(pd, id) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: pd
            integer(c_int), value :: id
            integer(c_int) :: ncnn_paramdict_get_type
        end function

        function ncnn_paramdict_get_int(pd, id, def) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: pd
            integer(c_int), value :: id
            integer(c_int), value :: def
            integer(c_int) :: ncnn_paramdict_get_int
        end function

        function ncnn_paramdict_get_float(pd, id, def) bind(c)
            import :: c_float, c_int, c_ptr
            type(c_ptr), value    :: pd
            integer(c_int), value :: id
            real(c_float), value  :: def
            real(c_float) :: ncnn_paramdict_get_float
        end function

        function ncnn_paramdict_get_array(pd, id, def) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value :: pd
            integer(c_int), value :: id
            type(c_ptr), value :: def
            type(c_ptr) :: ncnn_paramdict_get_array
        end function

        subroutine ncnn_paramdict_set_int(pd, id, i) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: pd
            integer(c_int), value :: id
            integer(c_int), value :: i
        end subroutine

        subroutine ncnn_paramdict_set_float(pd, id, f) bind(c)
            import :: c_float, c_int, c_ptr
            type(c_ptr), value    :: pd
            integer(c_int), value :: id
            real(c_float), value  :: f
        end subroutine

        subroutine ncnn_paramdict_set_array(pd, id, v) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: pd
            integer(c_int), value :: id
            type(c_ptr), value    :: v
        end subroutine

    end interface

contains

    !> ncnn_paramdict_t ncnn_paramdict_create(void)
    function paramdict_create() result(pd)
        type(ncnn_paramdict_t) :: pd
        pd%ptr = ncnn_paramdict_create()
    end function paramdict_create

    !> void ncnn_paramdict_destroy(ncnn_paramdict_t pd)
    subroutine paramdict_destroy(pd)
        type(ncnn_paramdict_t), intent(inout) :: pd
        call ncnn_paramdict_destroy(pd%ptr)
        pd%ptr = c_null_ptr
    end subroutine paramdict_destroy

    !> int ncnn_paramdict_get_type(const ncnn_paramdict_t pd, int id)
    function paramdict_get_type(pd, id) result(t)
        type(ncnn_paramdict_t), intent(in) :: pd
        integer, intent(in)                :: id
        integer                            :: t
        t = ncnn_paramdict_get_type(pd%ptr, id)
    end function paramdict_get_type

    !> int ncnn_paramdict_get_int(const ncnn_paramdict_t pd, int id, int def)
    function paramdict_get_int(pd, id, def) result(i)
        type(ncnn_paramdict_t), intent(in) :: pd
        integer, intent(in)                :: id
        integer, intent(in)                :: def
        integer                            :: i
        i = ncnn_paramdict_get_int(pd%ptr, id, def)
    end function paramdict_get_int

    !> float ncnn_paramdict_get_float(const ncnn_paramdict_t pd, int id, float def)
    function paramdict_get_float(pd, id, def) result(f)
        type(ncnn_paramdict_t), intent(in) :: pd
        integer, intent(in)                :: id
        real(c_float), intent(in)          :: def
        real(c_float)                      :: f
        f = ncnn_paramdict_get_float(pd%ptr, id, def)
    end function paramdict_get_float

    !> ncnn_mat_t ncnn_paramdict_get_array(const ncnn_paramdict_t pd, int id, const ncnn_mat_t def)
    !> def is optional; a null default is passed when it is absent.
    !> The result must be destroyed by the caller.
    function paramdict_get_array(pd, id, def) result(mat)
        type(ncnn_paramdict_t), intent(in)           :: pd
        integer, intent(in)                          :: id
        type(ncnn_mat_t), intent(in), optional       :: def
        type(ncnn_mat_t)                             :: mat

        if (present(def)) then
            mat%ptr = ncnn_paramdict_get_array(pd%ptr, id, def%ptr)
        else
            mat%ptr = ncnn_paramdict_get_array(pd%ptr, id, c_null_ptr)
        end if
    end function paramdict_get_array

    !> void ncnn_paramdict_set_int(ncnn_paramdict_t pd, int id, int i)
    subroutine paramdict_set_int(pd, id, i)
        type(ncnn_paramdict_t), intent(inout) :: pd
        integer, intent(in)                   :: id
        integer, intent(in)                   :: i
        call ncnn_paramdict_set_int(pd%ptr, id, i)
    end subroutine paramdict_set_int

    !> void ncnn_paramdict_set_float(ncnn_paramdict_t pd, int id, float f)
    subroutine paramdict_set_float(pd, id, f)
        type(ncnn_paramdict_t), intent(inout) :: pd
        integer, intent(in)                   :: id
        real(c_float), intent(in)             :: f
        call ncnn_paramdict_set_float(pd%ptr, id, f)
    end subroutine paramdict_set_float

    !> void ncnn_paramdict_set_array(ncnn_paramdict_t pd, int id, const ncnn_mat_t v)
    subroutine paramdict_set_array(pd, id, v)
        type(ncnn_paramdict_t), intent(inout) :: pd
        integer, intent(in)                   :: id
        type(ncnn_mat_t), intent(in)          :: v
        call ncnn_paramdict_set_array(pd%ptr, id, v%ptr)
    end subroutine paramdict_set_array

end module ncnn_paramdict
