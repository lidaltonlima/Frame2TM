module Efforts
    use iso_fortran_env, only: real64
    implicit none
    private
    public :: calc_efforts
contains
    subroutine calc_efforts(Eff, El_reactions, bars, nodes, nel)
        ! I/O *************************************************************************************
        real(real64), intent(inout) :: Eff(:, :)  ! elements efforts
        real(real64), intent(in) :: El_reactions(:, :)  ! elements reactions
        real(real64), intent(in) :: nodes(:, :)
        integer, intent(in) :: bars(:, :)
        integer, intent(in) :: nel  ! Number of elements

        ! Aux *************************************************************************************
        integer :: i  ! index
        real(real64) :: L  ! length
        real(real64) :: dx, dy

        ! =========================================================================================
        ! Calculation
        ! =========================================================================================
        Eff = 0d0
        do i = 1, nel
            dx = nodes(bars(i, 4), 1) - nodes(bars(i, 3), 1)
            dy = nodes(bars(i, 4), 2) - nodes(bars(i, 3), 2)
            L = sqrt(dx**2 + dy**2)

            Eff(i, 1) = -El_reactions(i, 1)
            Eff(i, 2) = -El_reactions(i, 2)
            Eff(i, 3) = -El_reactions(i, 3)
            Eff(i, 4) = -El_reactions(i, 1)
            Eff(i, 5) = -El_reactions(i, 2)
            Eff(i, 6) = -El_reactions(i, 3) + El_reactions(i, 2) * L
        end do
    end subroutine calc_efforts
end module Efforts
