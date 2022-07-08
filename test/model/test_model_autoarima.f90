module test_model_autoarima

    use, intrinsic :: iso_c_binding, only: c_loc, c_ptr, c_null_ptr, c_f_pointer
    use fortsa_model_m, only: auto_arima_init, auto_arima_setApproximation, auto_arima_exec, &
                            auto_arima_summary, auto_arima_predict, &
                            auto_arima_free, auto_arima_setStepwise, auto_arima_setVerbose, &
                            auto_arima_set
    use testdrive, only: new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_model_autoarima

contains

    subroutine collect_model_autoarima(testsuite)

        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("`test_model_autoarima1` vaild", test_model_autoarima1), &
                    new_unittest("`test_model_autoarima2` vaild", test_model_autoarima2) &
                    , new_unittest("`test_model_autoarima3` vaild", test_model_autoarima3) &
                    ]

    end subroutine collect_model_autoarima

    subroutine test_model_autoarima1(error)

        type(error_type), allocatable, intent(out) :: error
        integer :: i, s, r, L = 5
        real(8), allocatable :: xpred(:), amse(:)
        real(8) :: checked(5)
        type(c_ptr) :: obj = c_null_ptr
        integer :: unit
        real(8), allocatable :: inp(:)
        integer :: order(3), seasonal(3), line_num

        type(auto_arima_set), pointer :: auto_arima_set_ptr !! Pointer to auto_arima_set in C-lang

        order = [5, 2, 5]       !! Initialize order
        seasonal = [2, 1, 2]    !! Initialize seasonal

        line_num = countlines('ctsa/data/seriesA.txt')
        open (newunit=unit, file='ctsa/data/seriesA.txt')
        call check(error, line_num, 197)
        if (allocated(error)) return

        allocate (inp(line_num), xpred(L), amse(L))
        do i = 1, line_num
            read (unit, *) inp(i)
        end do
        close (unit)

        !> Initialize auto_arima_set
        obj = auto_arima_init(order, seasonal, 0, 0, line_num)
        call c_f_pointer(obj, auto_arima_set_ptr)   !! Linked
        call auto_arima_setApproximation(obj, 0)
        call auto_arima_setStepwise(obj, 0)

        call auto_arima_exec(obj, inp)
        ! call auto_arima_summary(obj)
        call auto_arima_predict(obj, inp,%ref([0.0d0]), L,%ref([0.0d0]), xpred, amse)

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

        call check(error, auto_arima_set_ptr%N, 197)
        if (allocated(error)) return

        call auto_arima_free(obj)

    end subroutine test_model_autoarima1

    subroutine test_model_autoarima2(error)

        type(error_type), allocatable, intent(out) :: error
        integer :: i, s, r
        real(8), allocatable :: xpred(:), amse(:)
        real(8) :: checked(5)
        type(c_ptr) :: obj = c_null_ptr
        integer :: unit
        real(8), allocatable :: inp(:)
        integer :: order(3), seasonal(3)
        integer :: line_num

        type(auto_arima_set), pointer :: auto_arima_set_ptr !! Pointer to auto_arima_set in C-lang

        order = [5, 2, 5]       !! Initialize order
        seasonal = [2, 1, 2]    !! Initialize seasonal

        line_num = countlines('ctsa/data/seriesG.txt')
        open (newunit=unit, file='ctsa/data/seriesG.txt')
        call check(error, line_num, 144)
        if (allocated(error)) return

        allocate (inp(line_num), xpred(5), amse(5))
        do i = 1, line_num
            read (unit, *) inp(i)
        end do
        close (unit)

        !> Initialize auto_arima_set
        obj = auto_arima_init(order, seasonal, 12, 0, line_num)
        call c_f_pointer(obj, auto_arima_set_ptr)   !! Linked
        call auto_arima_setApproximation(obj, 1)
        call auto_arima_setStepwise(obj, 1)
        ! call auto_arima_setVerbose(obj, 1)

        call auto_arima_exec(obj, inp)
        ! call auto_arima_summary(obj)
        call auto_arima_predict(obj, inp,%ref([0.0d0]), 5,%ref([0.0d0]), xpred, amse)

        checked = [446.8, 420.8, 448.8, 490.8, 501.8]
        do i = 1, size(xpred)
            call check(error, xpred(i), checked(i), thr=0.1_8)
            if (allocated(error)) return
        end do

        checked = [11.72, 14.18, 16.27, 18.13, 19.81]
        do i = 1, size(amse)
            call check(error, sqrt(amse(i)), checked(i), thr=0.01_8)
            if (allocated(error)) return
        end do

        call check(error, auto_arima_set_ptr%N, 144)
        if (allocated(error)) return

        call auto_arima_free(obj)

    end subroutine test_model_autoarima2

    subroutine test_model_autoarima3(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: i, d, d_, L, line_num
        real(8), allocatable, target :: inp(:)
        integer :: p, q, p_, q_, s, r
        real(8), allocatable, target :: xpred(:), amse(:), xreg(:), newxreg(:)
        type(c_ptr) :: obj

        integer, target :: order(3), seasonal(3), unit
        real(8), allocatable :: data(:, :)
        real(8) :: checked(5)

        !! Make sure all the parameter values are correct and consistent with other values. eg., if xreg is NULL r should be 0
        !! or if P = D = Q = 0 then make sure that s is also 0.
        !! Recheck the values if the program fails to execute.

        p = 5
        d = 2
        q = 5

        p_ = 2
        d_ = 1
        q_ = 2

        order = [p, d, q]
        seasonal = [p_, d_, q_]

        s = 0
        r = 2
        L = 5

        line_num = countlines('ctsa/data/e1m.dat')
        open (newunit=unit, file='ctsa/data/e1m.dat')

        allocate (data(line_num, 3))
        allocate (inp(line_num - L), xreg(2*(line_num - L)), newxreg(2*L), xpred(L), amse(L))
        do i = 1, line_num
            read (unit, *) data(i, :)
            !! read temp data
        end do

        inp = data(1:line_num - L, 1)
        xreg(:line_num - L) = data(1:line_num - L, 2)
        xreg(line_num - L + 1:) = data(1:line_num - L, 3)

        newxreg(:L) = data(line_num - L + 1:, 2)
        newxreg(L + 1:) = data(line_num - L + 1:, 3)

        obj = auto_arima_init(order, seasonal, s, r, line_num - L)
        call auto_arima_setApproximation(obj, 1)
        call auto_arima_setStepwise(obj, 1)
        ! call auto_arima_setVerbose(obj, 1)

        call auto_arima_exec(obj, inp, xreg)
        ! call auto_arima_summary(obj)
        call auto_arima_predict(obj, inp, xreg, L, &
                                newxreg, xpred, amse)

        checked = [612.06, 620.96, 634.68, 637.84, 642.55]
        do i = 1, size(xpred)
            call check(error, xpred(i), checked(i), thr=0.1_8)
            if (allocated(error)) return
        end do

        checked = [15.35, 21.30, 25.61, 29.03, 31.87]
        do i = 1, size(amse)
            call check(error, sqrt(amse(i)), checked(i), thr=0.01_8)
            if (allocated(error)) return
        end do

        call auto_arima_free(obj)

        deallocate (inp, xpred, amse, xreg, newxreg)
        close (unit)
    end subroutine test_model_autoarima3
    
    include "../countlines.finc"

end module test_model_autoarima
