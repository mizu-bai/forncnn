!------------------------------------------------------------------------------
! MODULE: ncnn_datareader
!
! DESCRIPTION:
!>  ncnn DataReader API (3 of 4 functions).
!
!>  ncnn_datareader_create_from_stdio is intentionally NOT bound: it takes a
!>  FILE*, which Fortran cannot portably produce. Use
!>  datareader_create_from_memory with the file contents read into a byte array
!>  instead.
!------------------------------------------------------------------------------
module ncnn_datareader
    use, intrinsic :: iso_c_binding
    use ncnn_types, only: ncnn_datareader_t
    implicit none
    private

    public :: datareader_create, datareader_create_from_memory, datareader_destroy

    interface

        function ncnn_datareader_create() bind(c)
            import :: c_ptr
            type(c_ptr) :: ncnn_datareader_create
        end function

        ! const unsigned char** mem -- passed by reference, so the C side sees a
        ! pointer to the c_ptr. ncnn::DataReaderFromMemory holds a reference to
        ! it and advances it as data is consumed, hence intent(inout).
        function ncnn_datareader_create_from_memory(mem) bind(c)
            import :: c_ptr
            type(c_ptr) :: mem
            type(c_ptr) :: ncnn_datareader_create_from_memory
        end function

        subroutine ncnn_datareader_destroy(dr) bind(c)
            import :: c_ptr
            type(c_ptr), value :: dr
        end subroutine

    end interface

contains

    !> ncnn_datareader_t ncnn_datareader_create(void)
    function datareader_create() result(dr)
        type(ncnn_datareader_t) :: dr
        dr%ptr = ncnn_datareader_create()
    end function datareader_create

    !> ncnn_datareader_t ncnn_datareader_create_from_memory(const unsigned char** mem)
    !> mem must hold c_loc() of a byte array (typically integer(c_int8_t)).
    !> On return, mem has been advanced past whatever the reader consumed.
    function datareader_create_from_memory(mem) result(dr)
        type(c_ptr), intent(inout)   :: mem
        type(ncnn_datareader_t)      :: dr
        dr%ptr = ncnn_datareader_create_from_memory(mem)
    end function datareader_create_from_memory

    !> void ncnn_datareader_destroy(ncnn_datareader_t dr)
    subroutine datareader_destroy(dr)
        type(ncnn_datareader_t), intent(inout) :: dr
        call ncnn_datareader_destroy(dr%ptr)
        dr%ptr = c_null_ptr
    end subroutine datareader_destroy

end module ncnn_datareader
