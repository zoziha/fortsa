module fortsa_stats

    use, intrinsic :: iso_c_binding, only: c_int, c_double
    implicit none
    private

    public :: acvf, acvf_opt, acvf2acf
    public :: pacf, pacf_opt

    interface
        !> Auto Covariance Function
        subroutine acvf(vec, N, par, M) bind(c, name='acvf')
            import c_int, c_double
            real(kind=c_double), intent(in) :: vec(*)
            real(kind=c_double), intent(out) :: par(*)
            integer(kind=c_int), value, intent(in) :: N, M
        end subroutine acvf

        !> Method 0 : Regular Method. Slow for large input length.
        !> Method 1 : FFT based Method. Use it if data length is large
        subroutine acvf_opt(vec, N, method, par, M) bind(c, name='acvf_opt')
            import c_int, c_double
            real(kind=c_double), intent(in) :: vec(*)
            real(kind=c_double), intent(out) :: par(*)
            integer(kind=c_int), value, intent(in) :: N, method, M
        end subroutine acvf_opt

        !> Converts Autocovariance to autocorrelation function
        pure subroutine acvf2acf(acf, M) bind(c, name='acvf2acf')
            import c_int, c_double
            real(kind=c_double), intent(inout) :: acf(*)
            integer(kind=c_int), value, intent(in) :: M
        end subroutine acvf2acf

        !> Partial Auto Covariance Function
        pure subroutine pacf(vec, N, par, M) bind(c, name='pacf')
            import c_int, c_double
            real(kind=c_double), intent(in) :: vec(*)
            real(kind=c_double), intent(out) :: par(*)
            integer(kind=c_int), value, intent(in) :: N, M
        end subroutine pacf

        !> Method 0 : Yule-Walker
        !> Method 1 : Burg
        !> Method 2 : Box-Jenkins Conditional MLE
        subroutine pacf_opt(vec, N, method, par, M) bind(c, name='pacf_opt')
            import c_int, c_double
            real(kind=c_double), intent(in) :: vec(*)
            real(kind=c_double), intent(out) :: par(*)
            integer(kind=c_int), value, intent(in) :: N, method, M
        end subroutine pacf_opt
    end interface

end module fortsa_stats
