module test_model_arima

    use, intrinsic :: iso_c_binding, only: c_ptr, c_null_ptr
    use fortsa_model_m, only: arima_set, arima_init, &
                              arima_setMethod, arima_exec, &
                              arima_summary, arima_predict, &
                              arima_free, arima_setOptMethod
    use testdrive, only: new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_model_arima

contains

    subroutine collect_model_arima(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("test_model_arima vaild", test_model_arima_vaild) &
                    ]

    end subroutine collect_model_arima

    subroutine test_model_arima_vaild(error)
        type(error_type), allocatable, intent(out) :: error

        integer :: i, d, L
        integer :: p, q, line_num, unit
        real(8), allocatable :: xpred(:), amse(:)
        type(c_ptr) :: obj = c_null_ptr
        target obj
        real(8) :: checked(5)

        real(8), allocatable :: inp(:)

        p = 1
        d = 1
        q = 1

        L = 5

        line_num = countlines('ctsa/data/seriesA.txt')
        open (newunit=unit, file='ctsa/data/seriesA.txt')
        allocate (inp(line_num), xpred(L), amse(L))

        do i = 1, line_num
            read (unit, *) inp(i)
        end do

        obj = arima_init(p, d, q, line_num)
        call arima_setMethod(obj, 0)
        call arima_setOptMethod(obj, 5)
        call arima_exec(obj, inp)
        ! call arima_summary(obj)
        call arima_predict(obj, inp, L, xpred, amse)

        checked = [17.48, 17.50, 17.50, 17.50, 17.50]
        do i = 1, size(xpred)
            call check(error, xpred(i), checked(i), thr=0.01_8)
            if (allocated(error)) return
        end do

        checked = [0.3138, 0.3375, 0.3477, 0.3557, 0.3631]
        do i = 1, size(amse)
            call check(error, sqrt(amse(i)), checked(i), thr=0.001_8)
            if (allocated(error)) return
        end do

        call arima_free(obj)
        deallocate (inp, xpred, amse)
        close (unit)

    end subroutine test_model_arima_vaild
    
    include "../countlines.finc"

end module test_model_arima
