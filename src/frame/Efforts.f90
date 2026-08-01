module Efforts
    use iso_fortran_env, only: real64

    use StructureData

    implicit none
    private
    public :: calc_EEl
contains
    subroutine calc_EEl
        ! Aux *************************************************************************************
        integer :: i  ! index
        real(real64) :: L  ! length
        real(real64) :: dx, dy

        ! =========================================================================================
        ! Calculation
        ! =========================================================================================
        EEl = 0d0
        do i = 1, nel
            dx = nodes(bars(i, 4), 1) - nodes(bars(i, 3), 1)
            dy = nodes(bars(i, 4), 2) - nodes(bars(i, 3), 2)
            L = sqrt(dx**2 + dy**2)

            EEl(i, 1) = -ERl(i, 1)
            EEl(i, 2) = -ERl(i, 2)
            EEl(i, 3) = -ERl(i, 3)
            EEl(i, 4) = -ERl(i, 1)
            EEl(i, 5) = -ERl(i, 2)
            EEl(i, 6) = -ERl(i, 3) + ERl(i, 2) * L
        end do
    end subroutine calc_EEl
end module Efforts
