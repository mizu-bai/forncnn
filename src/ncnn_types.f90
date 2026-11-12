!------------------------------------------------------------------------------
! MODULE: ncnn_types
!
! DESCRIPTION:
!>  Opaque handle types for the ncnn C API, plus small conversion helpers.
!>
!>  Every handle wraps a single c_ptr. Handles are copyable: copying one copies
!>  the pointer, so both copies refer to the same underlying ncnn object.
!>  There are deliberately no finalisers -- see the ownership note in README.
!------------------------------------------------------------------------------
module ncnn_types
    use, intrinsic :: iso_c_binding
    implicit none
    private

    public :: ncnn_allocator_t, ncnn_option_t, ncnn_mat_t, ncnn_blob_t, ncnn_paramdict_t, &
              ncnn_datareader_t, ncnn_modelbin_t, ncnn_layer_t, ncnn_net_t, ncnn_extractor_t, &
              ncnn_shape, c_f_string, fill_c_string

    !> opaque ncnn::Allocator handle
    type :: ncnn_allocator_t
        type(c_ptr) :: ptr = c_null_ptr
    end type

    !> opaque ncnn::Option handle
    type :: ncnn_option_t
        type(c_ptr) :: ptr = c_null_ptr
    end type

    !> opaque ncnn::Mat handle
    type :: ncnn_mat_t
        type(c_ptr) :: ptr = c_null_ptr
    end type

    !> opaque ncnn::Blob handle
    type :: ncnn_blob_t
        type(c_ptr) :: ptr = c_null_ptr
    end type

    !> opaque ncnn::ParamDict handle
    type :: ncnn_paramdict_t
        type(c_ptr) :: ptr = c_null_ptr
    end type

    !> opaque ncnn::DataReader handle
    type :: ncnn_datareader_t
        type(c_ptr) :: ptr = c_null_ptr
    end type

    !> opaque ncnn::ModelBin handle
    type :: ncnn_modelbin_t
        type(c_ptr) :: ptr = c_null_ptr
    end type

    !> opaque ncnn::Layer handle
    type :: ncnn_layer_t
        type(c_ptr) :: ptr = c_null_ptr
    end type

    !> opaque ncnn::Net handle
    type :: ncnn_net_t
        type(c_ptr) :: ptr = c_null_ptr
    end type

    !> opaque ncnn::Extractor handle
    type :: ncnn_extractor_t
        type(c_ptr) :: ptr = c_null_ptr
    end type

    !> shape of a blob, replacing the C API's four int* out-parameters
    type :: ncnn_shape
        integer :: dims = 0
        integer :: w    = 0
        integer :: h    = 0
        integer :: d    = 0
        integer :: c    = 0
    end type

contains

    !> convert a NUL-terminated C string to a deferred-length Fortran string.
    !> Returns an empty string when cp is not associated.
    function c_f_string(cp) result(s)
        type(c_ptr), intent(in)           :: cp
        character(len=:), allocatable     :: s

        character(kind=c_char), pointer   :: chars(:) => null()
        integer                           :: n
        integer                           :: i

        if (.not. c_associated(cp)) then
            s = ''
            return
        end if

        ! the descriptor is unbounded on purpose; only the bytes up to the
        ! terminating NUL are ever touched
        call c_f_pointer(cp, chars, [huge(0)])

        ! ncnn's strings are names, paths and the version -- all far below a
        ! megabyte. The cap exists so a missing terminator costs a wrong answer
        ! instead of walking off into unmapped memory.
        n = 0
        do
            if (chars(n + 1) == c_null_char) exit
            n = n + 1
            if (n >= 1048576) then
                n = 0
                exit
            end if
        end do

        allocate (character(len=n) :: s)
        do i = 1, n
            s(i:i) = chars(i)
        end do
    end function c_f_string

    !> Copy a Fortran string into a caller-owned c_char buffer, NUL terminated.
    !>
    !> The buffer must be declared with at least len_trim(s) + 1 elements and
    !> must be a `target` automatic array in the calling scope, so that the
    !> c_loc() of it stays valid for the duration of the C call. This is a
    !> subroutine rather than a function precisely because a function's local
    !> buffer would dangle once it returned.
    subroutine fill_c_string(s, buf)
        character(len=*), intent(in)         :: s
        character(kind=c_char), intent(out), target :: buf(:)

        integer :: n, i

        n = len_trim(s)
        if (size(buf) < n + 1) then
            error stop 'ncnn: c_char buffer too small for string'
        end if

        do i = 1, n
            buf(i) = s(i:i)
        end do
        buf(n + 1) = c_null_char
    end subroutine fill_c_string

end module ncnn_types
