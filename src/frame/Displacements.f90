module Displacements
    implicit none
    private

    public :: calc_D
contains
    subroutine calc_D(D, K, F, nccdesl, nnr, itydisp, ndofn, dim, disp)
        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! I/O *************************************************************************************
        real(8), intent(inout) :: D(:)  ! displacements

        real(8), intent(in) :: K(:, :)  ! Global stiffness global
        real(8), intent(in) :: F(:)  ! Vector of loads

        integer, intent(in) :: nccdesl  ! Number of boundaries condition
        integer, intent(in) :: nnr(:)  ! index of bound node
        logical, intent(in) :: itydisp(:, :) ! type of bound
        real(8), intent(in) :: disp(:, :)  ! displacement value

        integer, intent(in) :: ndofn  ! Number of degrees of freedom per node
        integer, intent(in) :: dim  ! dimension of matrices and vectors

        ! Aux *************************************************************************************
        integer :: i, dir  ! indices
        integer :: i_dir  ! index of bound direction
        real(8), allocatable :: K_aux(:, :)
        real(8), allocatable :: F_aux(:)

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
        K_aux = K
        F_aux = F

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
        D = F_aux
        call dposv('U', dim, 1, K_Aux, dim, D, dim, info)

        if (info /= 0) error stop 'DPOSV - calc_D - Displacements: solution system.'

        ! Sum the prescribed displacement *********************************************************
        do i = 1, nccdesl
            do dir = 1, ndofn
                i_dir = (ndofn * (nnr(i) - 1)) + dir

                if (itydisp(i, dir)) then
                    D(i_dir) = disp(i, dir)
                end if
            end do
        end do
    end subroutine calc_D
end module Displacements
