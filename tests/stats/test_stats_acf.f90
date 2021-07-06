program test_acf_acf

    use forlab_io, only: disp, file
    use fortsa_stats, only: acvf, acvf_opt, acvf2acf
    use iso_c_binding, only: c_loc
    implicit none
    type(file) :: infile
    real(8), target, allocatable :: inp(:), acf(:)
    integer :: method

    infile = file('example/data/seriesC.txt', 'r')
    call infile%open()
    call infile%countlines()
    allocate (inp(infile%lines), acf(10))
    call disp(infile%lines, 'Linenumber in file is: ')

    block
        integer :: i
        do i = 1, infile%lines
            read (infile%unit, *) inp(i)
            ! call disp(inp(i))
        end do
    end block

    call disp('Default Method: acvf')
    call acvf(c_loc(inp(1)), infile%lines, c_loc(acf(1)), 10)
    call disp(acf)

    method = 0
    call disp('acvf_opt Method 0 General Method:')
    call acvf_opt(c_loc(inp(1)), infile%lines, method, c_loc(acf(1)), 10)
    call disp(acf)

    method = 1
    call disp('acvf_opt Method 1 FFT Method:')
    call acvf_opt(c_loc(inp(1)), infile%lines, method, c_loc(acf(1)), 10)
    call disp(acf)

    call disp('Autocorrelation:')
    call acvf2acf(c_loc(acf(1)), 10)
    call disp(acf)

    deallocate (inp)
    deallocate (acf)
    call infile%close()

end program test_acf_acf
