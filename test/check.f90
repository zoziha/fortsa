program check
    implicit none
    block
        use, intrinsic :: iso_c_binding, only: c_ptr
        use forlab, only: file, error_stop
        use fortran_tsa, only: auto_arima_init
        integer :: i, N, d, D, L
        real(8), allocatable, target :: inp(:)
        integer :: p, q, p_, q_, s, r
        type(c_ptr) :: obj

        integer :: order(3), seasonal(3)

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

        L = 5

        infile = file('example/data/seriesG.txt')
        if(.not.infile%exist()) then
            call error_stop('file not found : '//infile%filename)
        endif
        call infile%open('r')
        lines = infile%countlines()

        allocate(inp(lines), xpred(L), amse(L))
        obj = auto_arima_init(order, c_loc())

    endblock
end program