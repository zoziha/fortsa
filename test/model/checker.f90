program checker

    use, intrinsic :: iso_fortran_env, only: error_unit
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_model_autoarima, only: collect_model_autoarima
    use test_model_arima, only: collect_model_arima
    use test_model_ar, only: collect_model_ar
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
                 new_testsuite("test_model_ar", collect_model_ar), &
                 new_testsuite("test_model_autoarima", collect_model_autoarima), &
                 new_testsuite("test_model_arima", collect_model_arima) &
                 ]

    do is = 1, size(testsuites)
        write (error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write (error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if

end program checker
