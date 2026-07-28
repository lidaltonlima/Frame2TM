module Reactions
    implicit none
    private
    public :: get_reactions, get_el_reactions
contains
    subroutine get_reactions(reactions, Kwb, D, F, nccdesl, nnr, itydisp, ndofn, nno)
        ! =========================================================================================
        ! Vars Statements
        ! =========================================================================================
        ! I/O
        real(8), allocatable, intent(inout) :: reactions(:)  ! reactions
        real(8), allocatable, intent(in) :: Kwb(:, :)
        real(8), allocatable, intent(in) :: D(:)  ! Displacements
        real(8), allocatable, intent(in) :: F(:)  ! Vector of loads
        integer, intent(in) :: nccdesl  ! Number of boundaries condition
        integer, allocatable, intent(in) :: nnr(:)  ! index of bound node
        logical, allocatable, intent(in) :: itydisp(:, :) ! type of bound
        integer, intent(in) :: ndofn  ! Number of degrees of freedom per node
        integer, intent(in) :: nno  ! Number of nodes

        ! Aux
        integer :: i, j, dir, i_dir


        do i = 1, nccdesl
            do dir = 1, ndofn
                i_dir = (ndofn * (nnr(i) - 1)) + dir

                if (itydisp(i, dir)) then
                    reactions(i_dir) = reactions(i_dir) - F(i_dir)

                    do j = 1, ndofn*nno
                        reactions(i_dir) = reactions(i_dir) + Kwb(i_dir, j) * D(j)
                    end do
                end if
            end do
        end do
    end subroutine get_reactions

    subroutine get_el_reactions(Eff, nel, kl, rot, D)
        ! =========================================================================================
        ! Vars Statements
        ! =========================================================================================
        ! I/O
        real(8), allocatable, intent(inout) :: Eff(:, :)  ! elements efforts
        real(8), allocatable, intent(in) :: D(:)  ! Displacements
        real(8), allocatable, intent(in) :: kl(:, :, :)  ! Stiffness matrix kl(element_id, i, j)
        integer, intent(in) :: nel  ! Number of elements
        real(8), allocatable, intent(in) :: rot(:, :, :)  ! Matrix of rotation

        ! Aux
        integer :: i
        real(8) :: kg(6, 6)


        do i = 1, nel
            kg = matmul(matmul(transpose(rot(i, :, :)), kl(i, :, :)), rot(i, :, :))
            Eff(i, :) = matmul(kg, D)
        end do
    end subroutine get_el_reactions
end module Reactions
