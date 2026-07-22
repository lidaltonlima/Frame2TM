module Lagrange
    ! Calculate Lagrangian Interpolation Polynomial
    implicit none
    private
    public LagPol
contains
    pure function LagPol(px, py, x) result(y)
        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! I/O
        real(8), intent(in) :: px(:), py(:) ! Points for interpolation
        real(8), intent(in) :: x ! Point for calculate
        real(8) :: y

        ! Auxiliary
        integer :: n ! Number of points
        integer :: i, j ! Indexes
        real(8) :: L ! Lagrangian Polynomial

        ! =========================================================================================
        ! Vars initialization
        ! =========================================================================================
        y = 0
        L = 1

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
                L = L * ((x - px(j)) / (px(i) - px(j)))
            end do
            y = y + py(i) * L
            L = 1
        end do
    end function LagPol
end module Lagrange
