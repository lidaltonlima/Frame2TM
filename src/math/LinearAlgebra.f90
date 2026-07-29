module LinearAlgebra
    use iso_fortran_env, only: real64
    implicit none
    private
    public :: cross, inv, LagPol

contains
    pure function cross(a, b) result(c)
        ! Calculates the cross product (Produto vetorial)

        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! I/O *************************************************************************************
        real(real64), intent(in) :: a(3)  ! first vector
        real(real64), intent(in) :: b(3)  ! second vector
        real(real64) :: c(3)  ! vector result of the cross product

        ! =========================================================================================
        ! Calculation
        ! =========================================================================================
        c(1) = (a(2) * b(3)) - (a(3) * b(2))
        c(2) = (a(3) * b(1)) - (a(1) * b(3))
        c(3) = (a(1) * b(2)) - (a(2) * b(1))
    end function cross

    function inv(mat) result(mat_inv)
        ! Calculates the inverse of matrix

        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! I/O *************************************************************************************
        real(real64), intent(in) :: mat(:, :)
        real(real64), allocatable :: mat_inv(:, :)

        ! Auxiliary *******************************************************************************
        integer :: dim  ! dimension of the matrix
        integer :: info  ! error code (Lapack)
        integer, allocatable :: ipiv(:)  ! pivot for LU factorization (Lapack)
        real(real64), allocatable :: work(:)  ! work vector (Lapack)

        ! External functions (Lapack) *************************************************************
        external :: dgetrf  ! LU factorization
        external :: dgetri  ! inverse calculation

        ! =========================================================================================
        ! Calculation
        ! =========================================================================================
        ! Size
        dim = size(mat, 1)

        ! Verification
        if (size(mat, 2) /= dim) error stop 'inv requires a square matrix'

        ! Allocation
        allocate(mat_inv(dim, dim))
        allocate(ipiv(dim))
        allocate(work(dim))

        ! Initialize vars
        mat_inv = mat

        ! LU factorization (Lapack)
        call dgetrf(dim, dim, mat_inv, dim, ipiv, info)
        if (info /= 0) error stop 'DGETRF failed while inverting matrix in inv'

        ! Inverse calculation (Lapack)
        call dgetri(dim, mat_inv, dim, ipiv, work, dim, info)
        if (info /= 0) error stop 'DGETRI failed while inverting matrix in inv'
    end function inv

    pure function LagPol(px, py, x) result(y)
        ! Calculates the value of function in the x point using Lagrangian Polynomial
        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! I/O *************************************************************************************
        real(real64), intent(in) :: px(:), py(:) ! Points for interpolation
        real(real64), intent(in) :: x ! Point for calculate
        real(real64) :: y

        ! Aux *************************************************************************************
        integer :: n ! Number of points
        integer :: i, j ! Indexes for looping
        real(real64) :: lag ! Lagrangian Polynomial

        ! =========================================================================================
        ! Vars initialization
        ! =========================================================================================
        y = 0
        lag = 1

        if ( size(px) == size(py) ) then
            n = size(px)
        else
            error stop 'size of x points not equals size y points'
        end if

        ! =========================================================================================
        ! Calculation
        ! =========================================================================================
        ! Sun
        do i = 1, n
            ! Prod
            do j = 1, n
                if ( i == j ) then
                    cycle
                end if
                lag = lag * ((x - px(j)) / (px(i) - px(j)))
            end do
            y = y + py(i) * lag
            lag = 1
        end do
    end function LagPol
end module LinearAlgebra
