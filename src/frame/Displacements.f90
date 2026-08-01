module Displacements
    use iso_fortran_env, only: real64

    use StructureData

    implicit none
    private

    public :: calc_dc
contains
    subroutine calc_dc
        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! Aux *************************************************************************************
        integer :: i, dir  ! indices
        integer :: i_dir  ! index of bound direction
        real(real64), allocatable :: K_aux(:, :)
        real(real64), allocatable :: F_aux(:)

        ! External ********************************************************************************
        external :: dposv  ! solve symmetric positive defined matrix system (Lapack)

        integer :: info  ! status of operation (dposv - Lapack)

        ! =========================================================================================
        ! Initialization
        ! =========================================================================================
        ! Allocation
        allocate(K_aux(dim, dim))
        allocate(F_aux(dim))

        ! Start values
        K_aux = Kg
        F_aux = Fg

        ! =========================================================================================
        ! Calculation
        ! =========================================================================================
        ! Add boundary ****************************************************************************
        do i = 1, nccdesl
            do dir = 1, ndofn
                i_dir = (ndofn * (nnr(i) - 1)) + dir

                if (itydisp(i, dir)) then
                    K_aux(i_dir, :) = 0d0
                    K_aux(:, i_dir) = 0d0
                    K_aux(i_dir, i_dir) = 1d0
                    F_aux(i_dir) = 0d0
                end if
            end do
        end do

        ! Solution the system *********************************************************************
        Dg = F_aux
        call dposv('U', dim, 1, K_Aux, dim, Dg, dim, info)

        if (info /= 0) error stop 'DPOSV - calc_D - Displacements: solution system.'

        ! Sum the prescribed displacement *********************************************************
        do i = 1, nccdesl
            do dir = 1, ndofn
                i_dir = (ndofn * (nnr(i) - 1)) + dir

                if (itydisp(i, dir)) then
                    Dg(i_dir) = disp(i, dir)
                end if
            end do
        end do
    end subroutine calc_dc
end module Displacements
