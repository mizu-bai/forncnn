!------------------------------------------------------------------------------
! MODULE: ncnn_blob
!
! DESCRIPTION:
!>  ncnn blob introspection API (6 functions).
!>  Blob handles are obtained from a net after it has been loaded; this module
!>  never creates them.
!------------------------------------------------------------------------------
module ncnn_blob
    use, intrinsic :: iso_c_binding
    use ncnn_types, only: ncnn_blob_t, ncnn_shape, c_f_string
    implicit none
    private

    public :: blob_get_name, blob_get_producer, blob_get_consumer, blob_get_shape

    interface

        function ncnn_blob_get_name(blob) bind(c)
            import :: c_ptr
            type(c_ptr), value :: blob
            type(c_ptr) :: ncnn_blob_get_name
        end function

        function ncnn_blob_get_producer(blob) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value :: blob
            integer(c_int) :: ncnn_blob_get_producer
        end function

        function ncnn_blob_get_consumer(blob) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value :: blob
            integer(c_int) :: ncnn_blob_get_consumer
        end function

        subroutine ncnn_blob_get_shape(blob, dims, w, h, c) bind(c)
            import :: c_int, c_ptr
            type(c_ptr), value    :: blob
            integer(c_int)        :: dims
            integer(c_int)        :: w
            integer(c_int)        :: h
            integer(c_int)        :: c
        end subroutine

    end interface

contains

    !> const char* ncnn_blob_get_name(const ncnn_blob_t blob)
    function blob_get_name(blob) result(name)
        type(ncnn_blob_t), intent(in) :: blob
        character(len=:), allocatable :: name
        name = c_f_string(ncnn_blob_get_name(blob%ptr))
    end function blob_get_name

    !> int ncnn_blob_get_producer(const ncnn_blob_t blob)
    !> 0-based layer index in C; returned as-is, documented as 0-based.
    function blob_get_producer(blob) result(i)
        type(ncnn_blob_t), intent(in) :: blob
        integer :: i
        i = ncnn_blob_get_producer(blob%ptr)
    end function blob_get_producer

    !> int ncnn_blob_get_consumer(const ncnn_blob_t blob)
    !> 0-based layer index in C; returned as-is, documented as 0-based.
    function blob_get_consumer(blob) result(i)
        type(ncnn_blob_t), intent(in) :: blob
        integer :: i
        i = ncnn_blob_get_consumer(blob%ptr)
    end function blob_get_consumer

    !> void ncnn_blob_get_shape(const ncnn_blob_t blob, int* dims, int* w, int* h, int* c)
    !> Replaces the C API's four int* out-parameters with a shape result.
    !>
    !> The C API has no parameter for the depth dimension, so shape%d is
    !> always 0 here even for a 4-D blob; only dims/w/h/c are available.
    function blob_get_shape(blob) result(shape)
        type(ncnn_blob_t), intent(in) :: blob
        type(ncnn_shape)              :: shape

        integer(c_int) :: dims, w, h, c

        call ncnn_blob_get_shape(blob%ptr, dims, w, h, c)
        shape%dims = dims
        shape%w    = w
        shape%h    = h
        shape%d    = 0
        shape%c    = c
    end function blob_get_shape

end module ncnn_blob
