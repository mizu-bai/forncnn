program main
    use ncnn
    use iso_c_binding
    implicit none
        
    interface
        subroutine pretty_print(mat_ptr) bind(c, name="c_pretty_print")
            import c_ptr
            type(c_ptr), value :: mat_ptr
        end subroutine pretty_print
    end interface

end program main
