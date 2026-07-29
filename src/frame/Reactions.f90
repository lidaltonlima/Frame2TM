module Reactions
    implicit none
    private
    public :: calc_reactions, calc_el_reactions
contains
    subroutine calc_reactions(reactions, K, D, F, nccdesl, nnr, itydisp, disp, ndofn, dim)
        ! =========================================================================================
        ! Vars Statements
        ! =========================================================================================
        ! I/O *************************************************************************************
        real(8), intent(inout) :: reactions(:)  ! reactions

        real(8), intent(in) :: K(:, :)
        real(8), intent(in) :: D(:)  ! Displacements
        real(8), intent(in) :: F(:)  ! Vector of loads

        integer, intent(in) :: ndofn  ! Number of degrees of freedom per node

        integer, intent(in) :: nnr(:)  ! index of bound node
        integer, intent(in) :: nccdesl  ! Number of boundaries condition
        logical, intent(in) :: itydisp(:, :) ! type of bound
        real(8), intent(in) :: disp(:, :)  ! displacement value

        integer, intent(in) :: dim  ! dimension of matrices and vectors

        ! Control *********************************************************************************
        integer :: i, j, dir, i_dir ! indexes
        real(8) :: D_aux(dim)


        ! =========================================================================================
        ! Calculation
        ! =========================================================================================
        D_aux = D
        do i =1, nccdesl
            do dir = 1, ndofn
                if (itydisp(i, dir)) then
                    i_dir = (ndofn * (nnr(i) - 1)) + dir
                    D_aux(i_dir) = D_aux(i_dir) - (disp(i, dir))
                end if
            end do
        end do
        do i = 1, nccdesl
            do dir = 1, ndofn
                i_dir = (ndofn * (nnr(i) - 1)) + dir

                if (itydisp(i, dir)) then
                    reactions(i_dir) = reactions(i_dir) - F(i_dir)

                    do j = 1, dim
                        reactions(i_dir) = reactions(i_dir) + K(i_dir, j) * D_aux(j)
                    end do
                end if
            end do
        end do
    end subroutine calc_reactions

    subroutine calc_el_reactions(El_reactions, nel, ndofn, bars, kl, rot, D, el_dim)
        ! =========================================================================================
        ! Vars Statements
        ! =========================================================================================
        ! I/O
        real(8), intent(inout) :: El_reactions(:, :)  ! elements efforts
        real(8), intent(in) :: D(:)  ! Displacements
        real(8), intent(in) :: kl(:, :, :)  ! Stiffness matrix kl(element_id, i, j)
        integer, intent(in) :: nel  ! Number of elements
        integer, intent(in) :: ndofn  ! Number of degrees of freedom per node
        integer, intent(in) :: bars(:, :)
        real(8), intent(in) :: rot(:, :, :)  ! Matrix of rotation
        integer, intent(in) :: el_dim  ! dimension of matrices and vectors

        ! Aux
        integer :: i
        integer :: si, ei
        integer :: sf, ef
        real(8) :: De(el_dim)
        real(8) :: Del(el_dim)


        do i = 1, nel
            si = (ndofn * (bars(i, 3) - 1)) + 1
            ei = si + ndofn - 1

            sf = (ndofn * (bars(i, 4) - 1)) + 1
            ef = sf + ndofn - 1

            De(:ndofn) = D(si:ei)
            De(ndofn+1:) = D(sf:ef)

            Del = matmul(rot(i, :, :), De)
            El_reactions(i, :) = matmul(kl(i, :, :), Del)
        end do
    end subroutine calc_el_reactions
end module Reactions
