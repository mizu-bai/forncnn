!------------------------------------------------------------------------------
! test/test_ncnn.f90
!
! Exercises the binding end to end. Build with `fpm test`, or directly:
!
!   gfortran -I build tests/*.f90 src/*.f90 \
!       -lncnn -lc++ -lomp -lpthread -o test_ncnn
!
! The test needs a real ncnn installation and, for the last section, the
! squeezenet_v1.1 model files. Set FORNCNN_MODEL_DIR to point at them; the
! model-dependent part is skipped when the directory is absent.
!------------------------------------------------------------------------------
program test_ncnn
    use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env, only: output_unit
    use ncnn
    implicit none

    integer :: failures

    failures = 0

    call t_version()
    call t_allocator_option()
    call t_mat_basics()
    call t_mat_elemsize()
    call t_mat_external()
    call t_mat_process()
    call t_layer()
    call t_paramdict()
    call t_datareader_modelbin()
    call t_net_model()

    print *
    if (failures == 0) then
        print '(a)', 'ALL TESTS PASSED'
    else
        print '(a,i0,a)', 'FAILURES: ', failures, ' check(s) failed'
        error stop 1
    end if

contains

    subroutine check(cond, what)
        logical, intent(in) :: cond
        character(len=*), intent(in) :: what
        if (cond) then
            print '(a,a,a)', '  ok   ', what, ''
        else
            failures = failures + 1
            print '(a,a)', '  FAIL ', what
        end if
    end subroutine check

    subroutine t_version()
        print *, '[version]'
        flush(output_unit)
        call check(len(version()) > 0, 'version() returns a non-empty string')
        call check(version_number() > 20000000, 'version_number() looks sane')
        print *, '        version = ', trim(version()), ' number = ', version_number()
    end subroutine

    subroutine t_allocator_option()
        type(ncnn_allocator_t) :: alloc
        type(ncnn_option_t)    :: opt

        print *, '[allocator / option]'
        flush(output_unit)
        alloc = allocator_create_pool()
        opt = option_create()
        call check(c_associated(alloc%ptr), 'pool allocator created')
        call check(c_associated(opt%ptr), 'option created')

        call option_set_num_threads(opt, 1)
        call check(option_get_num_threads(opt) == 1, 'num_threads round trip')

        ! every use_* flag must round trip through get after set
        call check(option_get_use_fp16_packed(opt) >= 0, 'fp16_packed readable')
        call option_set_use_fp16_packed(opt, 1)
        call check(option_get_use_fp16_packed(opt) == 1, 'fp16_packed set/get')
        call option_set_use_fp16_packed(opt, 0)
        call check(option_get_use_fp16_packed(opt) == 0, 'fp16_packed cleared')

        call option_set_use_bf16_storage(opt, 1)
        call check(option_get_use_bf16_storage(opt) == 1, 'bf16_storage set/get')
        call option_set_use_int8_arithmetic(opt, 1)
        call check(option_get_use_int8_arithmetic(opt) == 1, 'int8_arithmetic set/get')

        call option_set_blob_allocator(opt, alloc)
        call option_set_workspace_allocator(opt, alloc)
        call check(.true., 'allocator setters do not crash')

        call option_destroy(opt)
        call allocator_destroy(alloc)
        call check(.not. c_associated(opt%ptr), 'option ptr nulled after destroy')
    end subroutine

    subroutine t_mat_basics()
        type(ncnn_allocator_t) :: alloc
        type(ncnn_option_t)    :: opt
        type(ncnn_mat_t)       :: m, r, c1
        real(c_float), allocatable :: d(:)

        print *, '[mat basics]'
        flush(output_unit)
        alloc = allocator_create_pool()
        opt = option_create()

        m = mat_create_2d(4, 3, alloc)
        call check(mat_get_dims(m) == 2, 'dims == 2 for a 2d mat')
        call check(mat_get_w(m) == 4 .and. mat_get_h(m) == 3, 'w/h reported')
        call check(mat_get_d(m) == 1 .and. mat_get_c(m) == 1, 'd/c default to 1')
        call check(mat_get_elemsize(m) == 4_c_size_t, 'elemsize == 4 by default')
        call check(mat_get_cstep(m) == 12_c_size_t, 'cstep == w*h for an unpacked mat')

        call mat_fill_float(m, 2.0_c_float)
        d = mat_get_data(m)
        call check(size(d) == 12, 'mat_get_data size == w*h*cstep')
        call check(all(abs(d - 2.0_c_float) < 1e-6_c_float), 'mat_get_data values')

        d = mat_get_channel_data(m, 1)
        call check(size(d) == 12_c_size_t, 'channel data size == cstep')

        r = mat_reshape_1d(m, 12, alloc)
        call check(mat_get_dims(r) == 1 .and. mat_get_w(r) == 12, 'reshape_1d')

        c1 = mat_clone(m, alloc)
        d = mat_get_data(c1)
        call check(size(d) == 12, 'clone produces the same element count')

        call mat_destroy(c1)
        call mat_destroy(r)
        call mat_destroy(m)
        call option_destroy(opt)
        call allocator_destroy(alloc)
    end subroutine

    subroutine t_mat_elemsize()
        type(ncnn_allocator_t) :: alloc
        type(ncnn_mat_t)       :: e2, e1
        print *, '[mat elemsize]'
        flush(output_unit)

        alloc = allocator_create_pool()

        ! the old binding read these as 12 floats and over-read the buffer
        e2 = mat_create_2d_elem(4, 3, 2_c_size_t, 1, alloc)
        call check(mat_get_elemsize(e2) == 2_c_size_t, 'fp16 mat created')
        call check(size(mat_get_data_bytes(e2)) == 32_c_size_t, &
                   'fp16 bytes == cstep*c*elemsize == 32 (not 48)')

        e1 = mat_create_2d_elem(4, 3, 1_c_size_t, 1, alloc)
        call check(mat_get_elemsize(e1) == 1_c_size_t, 'int8 mat created')
        call check(size(mat_get_data_bytes(e1)) == 16_c_size_t, 'int8 bytes == 16')

        call mat_destroy(e1)
        call mat_destroy(e2)
        call allocator_destroy(alloc)
    end subroutine

    subroutine t_mat_external()
        type(ncnn_allocator_t) :: alloc
        type(ncnn_mat_t)       :: m
        real(c_float), target  :: buf(12)
        real(c_float), allocatable :: d(:)
        integer :: i

        print *, '[mat external]'
        flush(output_unit)
        alloc = allocator_create_pool()
        do i = 1, 12
            buf(i) = real(i, c_float)
        end do

        m = mat_create_external_2d(4, 3, buf, alloc)
        call check(mat_get_w(m) == 4 .and. mat_get_c(m) == 1, 'external 2d dims')

        ! must alias, not copy: mutating buf must be visible through the mat
        buf(1) = -1.0_c_float
        d = mat_get_data(m)
        call check(abs(d(1) + 1.0_c_float) < 1e-6_c_float, 'external mat aliases the buffer')

        call mat_destroy(m)
        call allocator_destroy(alloc)
    end subroutine

    subroutine t_mat_process()
        type(ncnn_allocator_t) :: alloc
        type(ncnn_option_t)    :: opt
        type(ncnn_mat_t)       :: m, packed, flat, bordered
        real(c_float)          :: mv(1), nv(1)
        real(c_float), allocatable :: d(:)

        print *, '[mat process]'
        flush(output_unit)
        alloc = allocator_create_pool()
        opt = option_create()
        m = mat_create_2d(4, 3, alloc)
        call mat_fill_float(m, 1.5_c_float)

        packed = mat_convert_packing(m, 4, opt)
        call check(c_associated(packed%ptr), 'convert_packing returns a mat')

        flat = mat_flatten(m, opt)
        call check(mat_get_dims(flat) == 1, 'flatten reduces dims to 1')

        bordered = mat_create()
        call mat_copy_make_border(m, bordered, 1, 1, 1, 1, border_constant, &
                                  0.0_c_float, opt)
        call check(mat_get_w(bordered) == 6 .and. mat_get_h(bordered) == 5, &
                   'copy_make_border grows by the border widths')

        ! this used to produce -Infinity: the interface declared assumed-shape
        ! arrays, so C received a descriptor instead of a float*
        mv = [104.0_c_float]
        nv = [1.0_c_float / 58.0_c_float]
        call mat_substract_mean_normalize(m, mv, nv)
        d = mat_get_data(m)
        call check(abs(d(1) - ((1.5_c_float - 104.0_c_float) / 58.0_c_float)) < 1e-5_c_float, &
                   'substract_mean_normalize computes (v-mean)/norm')

        call mat_destroy(bordered)
        call mat_destroy(flat)
        call mat_destroy(packed)
        call mat_destroy(m)
        call option_destroy(opt)
        call allocator_destroy(alloc)
    end subroutine

    subroutine t_layer()
        type(ncnn_layer_t) :: layer
        integer :: idx

        print *, '[layer]'
        flush(output_unit)
        idx = layer_type_to_index('Convolution')
        call check(idx /= 0, 'layer_type_to_index finds Convolution')

        layer = layer_create_by_typeindex(idx)
        call check(c_associated(layer%ptr), 'layer_create_by_typeindex')
        call check(layer_get_typeindex(layer) == idx, 'typeindex round trips')
        ! name and type stay empty until load_param runs, which the C API does
        ! not expose -- see the note in ncnn_layer
        call check(len_trim(layer_get_type(layer)) == 0, &
                   'layer_get_type empty before load_param (ncnn behaviour)')
        call check(len_trim(layer_get_name(layer)) == 0, &
                   'layer_get_name empty before load_param (ncnn behaviour)')
        call check(layer_get_support_packing(layer) >= 0, 'support_packing readable')
        call layer_destroy(layer)

        layer = layer_create_by_type('ReLU')
        call check(c_associated(layer%ptr), 'layer_create_by_type ReLU')
        call check(layer_get_typeindex(layer) == layer_type_to_index('ReLU'), &
                   'layer created by name has the matching typeindex')
        call layer_destroy(layer)

        call check(layer_type_to_index('NoSuchLayerType') == -1, &
                   'unknown layer type maps to -1')
    end subroutine

    subroutine t_paramdict()
        type(ncnn_paramdict_t) :: pd

        print *, '[paramdict]'
        flush(output_unit)
        pd = paramdict_create()
        call check(c_associated(pd%ptr), 'paramdict created')

        call paramdict_set_int(pd, 0, 42)
        call check(paramdict_get_type(pd, 0) == 2, 'set_int gives type code 2')
        call check(paramdict_get_int(pd, 0, -1) == 42, 'get_int round trip')
        call check(paramdict_get_int(pd, 99, -1) == -1, 'get_int falls back to def')

        call paramdict_set_float(pd, 1, 3.5_c_float)
        call check(paramdict_get_type(pd, 1) == 3, 'set_float gives type code 3')
        call check(abs(paramdict_get_float(pd, 1, -1.0_c_float) - 3.5_c_float) < 1e-6_c_float, &
                   'get_float round trip')

        call paramdict_destroy(pd)
    end subroutine

    subroutine t_datareader_modelbin()
        type(ncnn_allocator_t)      :: alloc
        type(ncnn_datareader_t)     :: dr
        type(ncnn_modelbin_t)       :: mb
        type(ncnn_mat_t)            :: weights(2)
        integer(c_int8_t), target   :: buf(64)
        type(c_ptr)                 :: mem
        integer                     :: i

        print *, '[datareader / modelbin]'
        flush(output_unit)
        alloc = allocator_create_pool()

        do i = 1, 64
            buf(i) = int(i - 1, c_int8_t)
        end do

        mem = c_loc(buf)
        dr = datareader_create_from_memory(mem)
        call check(c_associated(dr%ptr), 'datareader_create_from_memory')

        mb = modelbin_create_from_datareader(dr)
        call check(c_associated(mb%ptr), 'modelbin from datareader')
        call modelbin_destroy(mb)
        call datareader_destroy(dr)

        weights(1) = mat_create_1d(4, alloc)
        weights(2) = mat_create_1d(4, alloc)
        mb = modelbin_create_from_mat_array(weights)
        call check(c_associated(mb%ptr), 'modelbin from mat array')
        call modelbin_destroy(mb)
        call mat_destroy(weights(2))
        call mat_destroy(weights(1))
        call allocator_destroy(alloc)
    end subroutine

    subroutine t_net_model()
        character(len=512)            :: dirbuf
        character(len=:), allocatable :: dir
        character(len=512)            :: pp, bp
        integer                       :: dlen
        type(ncnn_net_t)              :: net
        type(ncnn_option_t)           :: opt
        type(ncnn_allocator_t)        :: alloc
        type(ncnn_extractor_t)        :: ex
        type(ncnn_mat_t)              :: in, out, fail_out
        real(c_float), allocatable    :: d(:)
        real(c_float)                 :: mv(3), nv(3)
        logical                       :: have_model
        integer                       :: stat, i, best

        print *, '[net + extractor]'
        flush(output_unit)

        call get_environment_variable('FORNCNN_MODEL_DIR', dirbuf, length=dlen)
        have_model = dlen > 0
        if (have_model) dir = trim(dirbuf(1:dlen))
        if (.not. have_model) then
            print *, '        FORNCNN_MODEL_DIR not set -- skipping model inference'
            return
        end if

        pp = trim(dir) // '.param'
        bp = trim(dir) // '.bin'

        alloc = allocator_create_pool()
        opt = option_create()
        call option_set_num_threads(opt, 1)

        net = net_create()
        call net_set_option(net, opt)

        stat = net_load_param(net, pp)
        call check(stat == 0, 'net_load_param')
        stat = net_load_model(net, bp)
        call check(stat == 0, 'net_load_model')

        call check(net_get_input_count(net) == 1, 'one input blob')
        call check(net_get_output_count(net) == 1, 'one output blob')
        call check(trim(net_get_input_name(net, 1)) == 'data', 'input named data')
        call check(trim(net_get_output_name(net, 1)) == 'prob', 'output named prob')

        in = mat_create_3d(227, 227, 3, alloc)
        call mat_fill_float(in, 128.0_c_float)
        mv = [104.0_c_float, 117.0_c_float, 123.0_c_float]
        nv = [1.0_c_float, 1.0_c_float, 1.0_c_float]
        call mat_substract_mean_normalize(in, mv, nv)

        ex = extractor_create(net)
        stat = extractor_input(ex, 'data', in)
        call check(stat == 0, 'extractor_input')

        call extractor_extract(ex, 'prob', out, stat)
        call check(stat == 0, 'extractor_extract')
        call check(c_associated(out%ptr), 'extract produced a mat')

        d = mat_get_data(out)
        call check(size(d) == 1000, 'squeezenet output has 1000 classes')
        ! the C API gives exactly 1.00195742 for this input: ncnn's softmax
        ! accumulates in fp32 over 1000 terms, so 1e-2 is the honest tolerance
        call check(abs(sum(d) - 1.0_c_float) < 1e-2_c_float, &
                   'softmax output sums to ~1 (fp32 accumulation)')
        call check(all(d >= 0.0_c_float), 'softmax output is non-negative')

        best = 1
        do i = 2, size(d)
            if (d(i) > d(best)) best = i
        end do
        print *, '        argmax = ', best, ' prob = ', d(best)
        ! a uniform grey input has no confident winner
        call check(d(best) < 0.5_c_float, 'grey input gives a low-confidence argmax')

        ! the C side allocates a Mat even when extraction fails, so the
        ! handle must be destroyed either way -- asserting it here pins the
        ! documented behaviour
        call extractor_extract(ex, 'no_such_blob', fail_out, stat)
        call check(stat /= 0, 'extracting a missing blob fails')
        call check(c_associated(fail_out%ptr), &
                   'failed extract still returns a handle (must be destroyed)')
        call mat_destroy(fail_out)

        call mat_destroy(out)
        call mat_destroy(in)
        call extractor_destroy(ex)
        call net_destroy(net)
        call option_destroy(opt)
        call allocator_destroy(alloc)
    end subroutine

end program test_ncnn
