program check
    implicit none
    block
        use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr, c_loc
        use forlab, only: file, error_stop, disp
        use fortran_tsa, only: auto_arima_init, auto_arima_setApproximation, auto_arima_setStepwise, auto_arima_setVerbose, &
                                auto_arima_exec, auto_arima_summary, auto_arima_predict, auto_arima_free
        integer :: i, N, d, d_, L
        real(8), allocatable, target :: inp(:)
        integer :: p, q, p_, q_, s, r
        real(8), allocatable, target :: xpred(:), amse(:), xreg(:), newxreg(:)
        type(c_ptr) :: obj

        integer, target :: order(3), seasonal(3)

        type(file) :: infile
        integer :: lines
        real(8), allocatable :: data(:, :)
        !! Make sure all the parameter values are correct and consistent with other values. eg., if xreg is NULL r should be 0
        !! or if P = D = Q = 0 then make sure that s is also 0. 
        !! Recheck the values if the program fails to execute.
        p = 5
        d = 2
        q = 5
        
        p_ = 2
        d_ = 1
        q_ = 2
        r = 0

        order = [p, d, q]
        seasonal = [p_, d_, q_]

        s = 0
        r = 2
        L = 5

        infile = file('example/data/e1m.dat')
        if(.not.infile%exist()) then
            call error_stop('file not found : '//infile%filename)
        endif
        call infile%open('r')
        lines = infile%countlines()

        allocate(data(lines, 3))
        allocate(inp(lines-L), xreg(2*(lines-L)), newxreg(2*L), xpred(L), amse(L))
        do i = 1, lines
            read(infile%unit, *) data(i, :)
                !! read temp data
        enddo
        
        inp  = data(1:lines-L, 1)
        xreg(:lines-L)   = data(1:lines-L, 2)
        xreg(lines-L+1:) = data(1:lines-L, 3)

        newxreg(:L)   = data(lines-L+1:, 2)
        newxreg(L+1:) = data(lines-L+1:, 3)

        obj = auto_arima_init(c_loc(order(1)), c_loc(seasonal(1)), s, r, lines-L)
        call auto_arima_setApproximation(obj, 1)
        call auto_arima_setStepwise(obj, 1)
        call auto_arima_setVerbose(obj, 1)

        call auto_arima_exec(obj, c_loc(inp(1)), c_loc(xreg(1)))
        call auto_arima_summary(obj)
        call auto_arima_predict(obj, c_loc(inp(1)), c_loc(xreg(1)), L, &
                                c_loc(newxreg(1)), c_loc(xpred(1)), c_loc(amse(1)))

        call disp('Forecast : 5 Point Look Ahead')
        call disp(xpred, 'Predicted Values : ')
        call disp(sqrt(amse), 'Standard Errors  : ')

        call auto_arima_free(obj)

        deallocate(inp, xpred, amse, xreg, newxreg)
        call infile%close()

    endblock
end program