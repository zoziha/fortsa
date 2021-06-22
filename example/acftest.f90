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

    endblock

end program
