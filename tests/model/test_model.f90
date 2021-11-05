module test_model

    use testdrive, only: new_unittest, unittest_type, error_type, check
    implicit none
    private

    public :: collect_model

contains

    subroutine collect_model(testsuite)

        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("`test_model_autoarima1` vaild", test_model_autoarima1) &
                    ]

    end subroutine collect_model

    subroutine test_model_autoarima1(error)

        use forlab_io, only: file
        use fortsa_model, only: auto_arima_init, auto_arima_setApproximation, auto_arima_exec, &
                                auto_arima_summary, auto_arima_predict, &
                                auto_arima_free, auto_arima_setStepwise, auto_arima_setVerbose, &
                                auto_arima_set
        use, intrinsic :: iso_c_binding, only: c_loc, c_ptr, c_null_ptr, c_f_pointer
        type(error_type), allocatable, intent(out) :: error
        integer :: i, s, r, L = 5
        real(8), allocatable :: xpred(:), amse(:)
        real(8) :: checked(5)
        type(c_ptr) :: obj = c_null_ptr
        type(file) :: infile
        real(8), allocatable :: inp(:)
        integer :: order(3), seasonal(3)

        type(auto_arima_set), pointer :: auto_arima_set_ptr !! pointer to auto_arima_set in C-lang

        order = [5, 2, 5]       !! Initialize order
        seasonal = [2, 1, 2]    !! Initialize seasonal

        infile = file('example/data/seriesA.txt', 'r')
        if (.not. infile%exist()) error stop 'Error: file not exist, '//infile%filename
        call infile%open()
        call infile%countlines()
        call check(error, infile%lines, 197)
        if (allocated(error)) return

        allocate (inp(infile%lines), xpred(L), amse(L))
        do i = 1, infile%lines
            read (infile%unit, *) inp(i)
        end do
        call infile%close()

        !> Initialize auto_arima_set
        obj = auto_arima_init(order, seasonal, 0, 0, infile%lines)
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

end module test_model
