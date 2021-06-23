!! `fpm test` or `fpm test test`

program test
    
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

        infile = file('example/data/seriesC.txt')
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

        infile = file('example/data/seriesA.txt')
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

        call arima_free(obj)
        deallocate(inp, xpred, amse)
        call infile%close()

    endblock

    block   !! artest.c
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

        infile = file('example/data/seriesA.txt')
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

        call ar_free(obj)
            !!\FIXME:
        deallocate(inp, xpred, amse)
        call infile%close()

    endblock

    block   !! artest2.c
        use forlab, only: disp, file, mean
        use fortran_tsa, only: ar_init, ar_exec, &
                                ar_summary, ar_predict, &
                                ar_free
        use fortran_tsa, only: yw, burg, hr
        use, intrinsic :: iso_c_binding
        integer :: i, d, L
        integer :: p, q
        real(8), target, allocatable :: phi(:), theta(:)
        type(file) :: infile
        integer :: line
        real(8), target, allocatable :: inp(:)
        real(8) :: wmean
        real(c_double), target :: var
            !! mean var

        p = 7
        d = 1
        q = 0

        L = 5

        line = 0

        infile = file('example/data/seriesA.txt')
        call infile%open('r')
        line = infile%countlines()
        allocate(inp(line), phi(p))

        do i = 1, line
            read(infile%unit, *) inp(i)
        enddo

        wmean = mean(inp)
            !! forlab mean

        ! obj = c_loc(set)
        ! call c_f_pointer(obj, set)
        call disp('AR Coefficients Using Yule Walker Algorithm : ')
        call yw(c_loc(inp(1)), line, p, c_loc(phi(1)), c_loc(var))
        call disp(phi, 'PHI : ')
        call disp(var, 'VAR : ')

        call disp('AR Coefficients Using Burg Algorithm : ')
        call burg(c_loc(inp(1)), line, p, c_loc(phi(1)), c_loc(var))
        call disp(phi, 'PHI : ')
        call disp(var, 'VAR : ')

        p = 1
        q = 1

        deallocate(phi)
        allocate(phi(p), theta(q))
        call disp('ARMA Coefficients Using Hannan Rissanen Algorithm : ')
        call hr(c_loc(inp), line, p, q, c_loc(phi(1)), c_loc(theta(1)), c_loc(var))
        call disp(phi, 'PHI : ')
        call disp(theta, 'THETA : ')
        call disp(var, 'VAR : ')

        deallocate(inp, phi, theta)
        call infile%close()

    endblock

    block   !! autoarimatest1.c
        use forlab, only: disp, file
        use fortran_tsa, only: auto_arima_init,auto_arima_setApproximation, auto_arima_exec, &
                                auto_arima_summary, auto_arima_predict, &
                                auto_arima_free, auto_arima_setStepwise, auto_arima_setVerbose
        use, intrinsic :: iso_c_binding
        integer :: i, s, r, L
        integer :: p, d, q, p_, d_ , q_
        real(8), target, allocatable :: xpred(:), amse(:)
        type(c_ptr) :: obj = c_null_ptr
        target obj
        ! type(arima_set), target :: set
        ! target set
        ! type(arima_set) :: obj
        type(file) :: infile
        integer :: line
        real(8), target, allocatable :: inp(:)
        integer, target :: order(3), seasonal(3)

        p = 5
        d = 2
        q = 5
        p_ = 2
        d_ = 1
        q_ = 2

        order = [p, d, q]
        seasonal = [p_, d_, q_]

        s = 0
        r = 0
        L = 5

        infile = file('example/data/seriesA.txt')
        call infile%open('r')
        line = infile%countlines()
        allocate(inp(line), xpred(L), amse(L))

        do i = 1, line
            read(infile%unit, *) inp(i)
        enddo

        ! obj = c_loc(set)
        ! call c_f_pointer(obj, set)

        obj = auto_arima_init(c_loc(order(1)), c_loc(seasonal(1)), s, r, line)
        call auto_arima_setApproximation(obj, 1)
        call auto_arima_setStepwise(obj, 1)
        call auto_arima_setVerbose(obj, 1)

        call auto_arima_exec(obj, c_loc(inp(1)), c_null_ptr)
        call auto_arima_summary(obj)
        call auto_arima_predict(obj, c_loc(inp(1)), c_null_ptr, L, c_null_ptr, c_loc(xpred(1)), c_loc(amse(1)))
        
        call disp('Predicted Values : ')
        call disp(xpred)
        call disp('Standard Errors : ')
        call disp(sqrt(amse))

        call auto_arima_free(obj)
            !!\FIXME:
        deallocate(inp, xpred, amse)
        call infile%close()

    endblock

    block
        use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_loc
        use forlab, only: file, error_stop, disp
        use fortran_tsa, only: auto_arima_init, auto_arima_setApproximation, auto_arima_setStepwise, auto_arima_setVerbose, &
                                auto_arima_exec, auto_arima_summary, auto_arima_predict, auto_arima_free
        integer :: i, N, d, d_, L
        real(8), allocatable, target :: inp(:)
        integer :: p, q, p_, q_, s, r
        real(8), allocatable, target :: xpred(:), amse(:)
        type(c_ptr) :: obj

        integer, target :: order(3), seasonal(3)

        type(file) :: infile
        integer :: lines
        !! Make sure all the parameter values are correct and consistent with other values. eg., if xreg is NULL r should be 0
        !! or if P = D = Q = 0 then make sure that s is also 0. 
        !! Recheck the values if the program fails to execute.
        p = 5
        d = 2
        q = 5
        s = 12
        p_ = 2
        d_ = 1
        q_ = 2
        r = 0

        order = [p, d, q]
        seasonal = [p_, d_, q_]

        L = 5

        infile = file('example/data/seriesG.txt')
        if(.not.infile%exist()) then
            call error_stop('file not found : '//infile%filename)
        endif
        call infile%open('r')
        lines = infile%countlines()

        allocate(inp(lines), xpred(L), amse(L))
        do i = 1, lines
            read(infile%unit, *) inp(i)
        enddo

        obj = auto_arima_init(c_loc(order(1)), c_loc(seasonal(1)), s, r, lines)
        call auto_arima_setApproximation(obj, 0)
        call auto_arima_setStepwise(obj, 1)
        call auto_arima_setVerbose(obj, 1)

        call auto_arima_exec(obj, c_loc(inp(1)), c_null_ptr)
        call auto_arima_summary(obj)
        call auto_arima_predict(obj, c_loc(inp(1)), c_null_ptr, L, c_null_ptr, c_loc(xpred(1)), c_loc(amse(1)))

        call disp('Forecast : 5 Point Look Ahead')
        call disp(xpred, 'Predicted Values : ')
        call disp(sqrt(amse), 'Standard Errors  : ')

        call auto_arima_free(obj)

        deallocate(inp, xpred, amse)
        call infile%close()

    endblock

end program
