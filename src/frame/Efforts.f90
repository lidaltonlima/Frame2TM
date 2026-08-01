module Efforts
    use iso_fortran_env, only: real64

    use StructureData

    implicit none
    private
    public :: calc_efforts
contains
    subroutine calc_efforts
        ! Aux *************************************************************************************
        integer :: i  ! index
        real(real64) :: L  ! length
        real(real64) :: dx, dy

        ! =========================================================================================
        ! Calculation
        ! =========================================================================================
        eff = 0d0
        do i = 1, nel
            dx = nodes(bars(i, 4), 1) - nodes(bars(i, 3), 1)
            dy = nodes(bars(i, 4), 2) - nodes(bars(i, 3), 2)
            L = sqrt(dx**2 + dy**2)

            eff(i, 1) = -el_reactions(i, 1)
            eff(i, 2) = -el_reactions(i, 2)
            eff(i, 3) = -el_reactions(i, 3)
            eff(i, 4) = -el_reactions(i, 1)
            eff(i, 5) = -el_reactions(i, 2)
            eff(i, 6) = -el_reactions(i, 3) + el_reactions(i, 2) * L
        end do
    end subroutine calc_efforts
end module Efforts
