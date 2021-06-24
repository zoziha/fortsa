program check
    implicit none
    block   !! dwttest.c
        use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_loc, c_char
        use forlab, only: file, error_stop, disp
        use wavelib_api, only: wave_init, wt_init, &
                                wave_summary, wt_summary, &
                                modwt, imodwt, &
                                wave_free, wt_free
        real(8), allocatable, target :: inp(:), out(:), diff(:), data(:)
        integer :: N, i, J
        type(c_ptr) :: obj
            !! wave_object
        type(c_ptr) :: wt
            !! wt_object
        character(kind=c_char, len=*), parameter :: name = 'db4' 

        integer, target :: order(3), seasonal(3)

        type(file) :: infile
        integer :: lines

        obj = wave_init(c_lco(name(1)))
        call wave_summary(obj)

        infile = file('example/data/signal.txt')
        if(.not.infile%exist()) then
            call error_stop('file not found : '//infile%filename)
        endif
        call infile%open('r')
        lines = infile%countlines
        allocate(data(lines))

        do i = 1, liens
            read(infile%unit, *) data(i)
        enddo
        call infile%close()
        N = 177

        allocate(inp(N), out(N), diff(N))
        inp = data(1:N)
        J = 2

        wt = wt_init(obj, 'modwt', N, J)
            !! Initialize the wavelet transform object

        call modwt(wt, c_loc(inp(1)))
            !! MODWT output can be accessed using wt->output vector. Use wt_summary to find out how to extract appx and detail coefficients

        !!\TODO: wt -> outlength

        call imodwt(wt, out)

        !!\TODO: detto

        call wt_summary(wt)

        call wave_free(obj)
        call wt_free(obj)

        deallocate(inp, out, diff)
    endblock
end program