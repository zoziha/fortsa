module fortsa_stats

    use, intrinsic :: iso_c_binding, only: c_int, c_double
    use, intrinsic :: iso_fortran_env, only: dp => real64, int32
    implicit none
    private

    public :: acvf, acvf_opt, acvf2acf
    public :: pacf, pacf_opt

    interface
        subroutine ctsa_acvf(vec, N, par, M) bind(c, name='acvf')
            import c_int, c_double
            real(kind=c_double), intent(in) :: vec(*)
            real(kind=c_double), intent(out) :: par(*)
            integer(kind=c_int), value, intent(in) :: N, M
        end subroutine ctsa_acvf

        subroutine ctsa_acvf_opt(vec, N, method, par, M) bind(c, name='acvf_opt')
            import c_int, c_double
            real(kind=c_double), intent(in) :: vec(*)
            real(kind=c_double), intent(out) :: par(*)
            integer(kind=c_int), value, intent(in) :: N, method, M
        end subroutine ctsa_acvf_opt

        pure subroutine ctsa_acvf2acf(acf, M) bind(c, name='acvf2acf')
            import c_int, c_double
            real(kind=c_double), intent(inout) :: acf(*)
            integer(kind=c_int), value, intent(in) :: M
        end subroutine ctsa_acvf2acf

        pure subroutine ctsa_pacf(vec, N, par, M) bind(c, name='pacf')
            import c_int, c_double
            real(kind=c_double), intent(in) :: vec(*)
            real(kind=c_double), intent(out) :: par(*)
            integer(kind=c_int), value, intent(in) :: N, M
        end subroutine ctsa_pacf
        
        subroutine ctsa_pacf_opt(vec, N, method, par, M) bind(c, name='pacf_opt')
            import c_int, c_double
            real(kind=c_double), intent(in) :: vec(*)
            real(kind=c_double), intent(out) :: par(*)
            integer(kind=c_int), value, intent(in) :: N, method, M
        end subroutine ctsa_pacf_opt
    end interface
    
contains

    !> Auto Covariance Function
    subroutine acvf(vec, par)
        real(kind=dp), intent(in) :: vec(:)
        real(kind=dp), intent(out) :: par(:)
        
        call ctsa_acvf(vec, size(vec), par, size(par))
        
    end subroutine acvf
    
    !> Method 0 : Regular Method. Slow for large input length.
    !> Method 1 : FFT based Method. Use it if data length is large
    subroutine acvf_opt(vec, method, par)
        real(kind=dp), intent(in) :: vec(:)
        integer(int32), intent(in) :: method
        real(kind=dp), intent(out) :: par(:)
        
        call ctsa_acvf_opt(vec, size(vec), method, par, size(par))
        
    end subroutine acvf_opt
    
    !> Converts Autocovariance to autocorrelation function
    subroutine acvf2acf(vec)
        real(kind=dp), intent(inout) :: vec(:)
        
        call ctsa_acvf2acf(vec, size(vec))
        
    end subroutine acvf2acf
    
    !> Partial Auto Covariance Function
    subroutine pacf(vec, par)
        real(kind=dp), intent(in) :: vec(:)
        real(kind=dp), intent(out) :: par(:)
        
        call ctsa_pacf(vec, size(vec), par, size(par))
        
    end subroutine pacf
    
    !> Method 0 : Yule-Walker
    !> Method 1 : Burg
    !> Method 2 : Box-Jenkins Conditional MLE
    subroutine pacf_opt(vec, method, par)
        real(kind=dp), intent(in) :: vec(:)
        integer(int32), intent(in) :: method
        real(kind=dp), intent(out) :: par(:)
        
        call ctsa_pacf_opt(vec, size(vec), method, par, size(par))
        
    end subroutine pacf_opt
    

end module fortsa_stats
