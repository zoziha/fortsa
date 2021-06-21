!! `fpm test` or `fpm test test`

program main
    
    block
        use forlab, only: disp
        call disp('Hello Fortran-TSA!')
    end block

    block   !! acftest
        use forlab, only: disp, file
        use fortran_tsa, only: acvf, acvf_opt, acvf2acf
        use iso_c_binding
        type(file) :: infile
        integer :: lines
        real(8),target,allocatable :: inp(:), acf(:)
        integer :: method

        infile = file('data/seriesC.txt')
        call infile%open('r')
        lines = infile%countlines()
        allocate(inp(lines), acf(10))
        call disp(lines)

        block
            integer :: i
            do i = 1, lines
                read(infile%unit, *) inp(i)
                ! call disp(inp(i))
            enddo
        endblock

        call disp('Default Method: acvf')
        call acvf(c_loc(inp(1)), lines, c_loc(acf(1)), 10)
        call disp(acf)

        method = 0
        call disp('acvf_opt Method 0 General Method:')
        call acvf_opt(c_loc(inp(1)), lines, method, c_loc(acf(1)), 10)
        call disp(acf)

        method = 1
        call disp('acvf_opt Method 1 FFT Method:')
        call acvf_opt(c_loc(inp(1)), lines, method, c_loc(acf(1)), 10)
        call disp(acf)

        call disp('Autocorrelation:')
        call acvf2acf(c_loc(acf(1)), 10)
        call disp(acf)

        deallocate(inp)
        deallocate(acf)
        call infile%close()

    endblock

    block   !! arimatest.c
        use forlab, only: disp, file
        use fortran_tsa, only: arima_set, arima_init, &
                                arima_setMethod,arima_exec, &
                                arima_summary, arima_predict, &
                                arima_free, arima_setOptMethod
        use, intrinsic :: iso_c_binding
        integer :: i, d, L
        integer :: p, q
        real(8), target, allocatable :: xpred(:), amse(:)
        type(c_ptr) :: obj = c_null_ptr
        target obj
        ! type(arima_set), target :: set
        ! target set
        ! type(arima_set) :: obj
        type(file) :: infile
        integer :: line
        real(8), target, allocatable :: inp(:)

        p = 0
        d = 1
        q = 0

        L = 5

        line = 0

        infile = file('data/seriesA.txt')
        call infile%open('r')
        line = infile%countlines()
        allocate(inp(line), xpred(L), amse(L))

        do i = 1, line
            read(infile%unit, *) inp(i)
        enddo

        ! obj = c_loc(set)
        ! call c_f_pointer(obj, set)

        obj = arima_init(p, d, q, line)
        call arima_setMethod(obj, 0)
        call arima_setOptMethod(obj, 5)
        call arima_exec(obj, c_loc(inp(1)))
        call arima_summary(obj)
        call arima_predict(obj, c_loc(inp(1)), L, c_loc(xpred(1)), c_loc(amse(1)))
        
        call disp('Predicted Values : ')
        call disp(xpred)
        call disp('Standard Errors : ')
        call disp(sqrt(amse))

        ! call arima_free(obj)
        deallocate(inp, xpred, amse)
        call infile%close()

    endblock

    block   !! arimatest.c
        use forlab, only: disp, file
        use fortran_tsa, only: ar_init, ar_exec, &
                                ar_summary, ar_predict, &
                                ar_free
        use, intrinsic :: iso_c_binding
        integer :: i, d, L
        integer :: p, q
        real(8), target, allocatable :: xpred(:), amse(:)
        type(c_ptr) :: obj = c_null_ptr
        target obj
        ! type(arima_set), target :: set
        ! target set
        ! type(arima_set) :: obj
        type(file) :: infile
        integer :: line
        real(8), target, allocatable :: inp(:)

        p = 0
        d = 1
        q = 0

        L = 5

        line = 0

        infile = file('data/seriesA.txt')
        call infile%open('r')
        line = infile%countlines()
        allocate(inp(line), xpred(L), amse(L))

        do i = 1, line
            read(infile%unit, *) inp(i)
        enddo

        ! obj = c_loc(set)
        ! call c_f_pointer(obj, set)

        obj = ar_init(0, line)
        call ar_exec(obj, c_loc(inp(1)))
        call ar_summary(obj)
        call ar_predict(obj, c_loc(inp(1)), L, c_loc(xpred(1)), c_loc(amse(1)))
        
        call disp('Predicted Values : ')
        call disp(xpred)
        call disp('Standard Errors : ')
        call disp(sqrt(amse))

        ! call ar_free(obj)
        deallocate(inp, xpred, amse)
        call infile%close()

    endblock


end program
