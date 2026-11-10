!------------------------------------------------------------------------------
! examples/squeezenet/main.f90
!
! Loads squeezenet_v1.1 and runs one inference on a constant grey image.
!
! Build (fpm not required):
!
!   gfortran -I build src/*.f90 examples/squeezenet/main.f90 \
!       -lncnn -lc++ -lomp -lpthread -o squeezenet
!
! Run:
!
!   FORNCNN_MODEL_DIR=/path/to/squeezenet_v1.1 ./squeezenet
!
! Get the model files from
! https://github.com/nihui/ncnn-assets/tree/master/models
!------------------------------------------------------------------------------
program main
    use, intrinsic :: iso_c_binding
    use ncnn
    implicit none

    character(len=512)            :: dirbuf
    character(len=:), allocatable :: dir
    character(len=512)            :: param_path, bin_path
    integer                       :: dlen

    type(ncnn_allocator_t)  :: alloc
    type(ncnn_option_t)     :: opt
    type(ncnn_net_t)        :: net
    type(ncnn_extractor_t)  :: ex
    type(ncnn_mat_t)        :: in, out
    real(c_float), allocatable :: d(:)
    real(c_float)           :: mv(3), nv(3)

    integer :: stat, i, best

    call get_environment_variable('FORNCNN_MODEL_DIR', dirbuf, length=dlen)
    if (dlen == 0) then
        print '(a)', 'FORNCNN_MODEL_DIR is not set'
        print '(a)', 'point it at the directory holding squeezenet_v1.1.param/.bin'
        error stop 1
    end if
    dir = trim(dirbuf(1:dlen))
    param_path = trim(dir) // '/squeezenet_v1.1.param'
    bin_path   = trim(dir) // '/squeezenet_v1.1.bin'

    print '(a)', 'ncnn version: ' // trim(version())

    alloc = allocator_create_pool()
    opt = option_create()
    call option_set_num_threads(opt, 1)

    net = net_create()
    call net_set_option(net, opt)

    stat = net_load_param(net, param_path)
    if (stat /= 0) error stop 'net_load_param failed -- check FORNCNN_MODEL_DIR'
    stat = net_load_model(net, bin_path)
    if (stat /= 0) error stop 'net_load_model failed -- check FORNCNN_MODEL_DIR'

    print *, 'input  : ', net_get_input_name(net, 1)
    print *, 'output : ', net_get_output_name(net, 1)

    ! squeezenet's preprocessing: 227x227x3, mean-subtracted, no scaling
    in = mat_create_3d(227, 227, 3, alloc)
    call mat_fill_float(in, 128.0_c_float)
    mv = [104.0_c_float, 117.0_c_float, 123.0_c_float]
    nv = [1.0_c_float, 1.0_c_float, 1.0_c_float]
    call mat_substract_mean_normalize(in, mv, nv)

    ex = extractor_create(net)
    stat = extractor_input(ex, 'data', in)
    if (stat /= 0) error stop 'extractor_input failed'

    call extractor_extract(ex, 'prob', out, stat)
    if (stat /= 0) error stop 'extractor_extract failed'

    d = mat_get_data(out)
    best = 1
    do i = 2, size(d)
        if (d(i) > d(best)) best = i
    end do
    print *, 'classes: ', size(d)
    print *, 'argmax : ', best, '  prob = ', d(best)

    call mat_destroy(out)
    call mat_destroy(in)
    call extractor_destroy(ex)
    call net_destroy(net)
    call option_destroy(opt)
    call allocator_destroy(alloc)
end program main
