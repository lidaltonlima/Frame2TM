module Efforts
    implicit none
    private
    public :: get_efforts
contains
    subroutine get_efforts(Eff, El_reactions, bars, nodes, nel)
        ! I/O
        real(8), allocatable, intent(inout) :: Eff(:, :)  ! elements efforts
        real(8), allocatable, intent(in) :: El_reactions(:, :)  ! elements reactions
        real(8), allocatable, intent(in) :: nodes(:, :)
        integer, allocatable, intent(in) :: bars(:, :)
        integer, intent(in) :: nel  ! Number of elements

        ! Aux
        integer :: i  ! index
        real(8) :: L  ! length
        real(8) :: dx, dy

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
    end subroutine get_efforts
end module Efforts
