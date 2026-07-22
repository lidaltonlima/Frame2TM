module GQInt
    ! (ENG-NA-001)
    ! Calculate numerical integral use Gauss Quadrature
    use GQTable, only: getTC
    implicit none
    private
    public :: intGQ

contains
    pure function intGQ(a, b, f, n) result(int)
        ! Calculate the integral using Gauss Quadrature

        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! I/O
        integer, intent(in) :: n ! Number of points
        real(8), intent(in) :: a ! Lower limit
        real(8), intent(in) :: b ! Upper limit
        real(8) :: int ! Result of integration using Gauss Quadrature
        real(8), allocatable :: t(:) ! Root values
        real(8), allocatable :: c(:) ! Coefficients

        ! Internal variables
        integer :: i ! Index

        ! =========================================================================================
        ! Function interfaces
        ! =========================================================================================
        interface
            pure function f(x) result(y)
                ! Function for integration
                real(8), intent(in) :: x
                real(8) :: y
            end function f
        end interface

        ! =========================================================================================
        ! Allocation
        ! =========================================================================================
        allocate(t(n))
        allocate(c(n))

        ! =========================================================================================
        ! Calculation
        ! =========================================================================================
        ! Get roots and coefficients from table
        call getTC(n, t, c)

        ! Calculate Integral
        int = 0
        do i = 1, n
            int = int + (f(new_x(a, b, t(i))) * c(i)) * ((b - a) / 2)
        end do
    end function intGQ

    pure real(8) function new_x(a, b, t) result(x)
        ! Calculate the new value of x for new limits of integration

        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! I/O
        real(8), intent(in) :: a, b ! Limits of integration
        real(8), intent(in) :: t ! Value to transformation

        ! =========================================================================================
        ! Calculation
        ! =========================================================================================
        x = ((b - a) * t + (b + a)) / 2
    end function new_x
end module GQInt
