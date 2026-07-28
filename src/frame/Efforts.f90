module Efforts
    implicit none
    private
    public :: get_efforts
contains
    subroutine get_efforts(Eff, nel, kl, rot, D, F)
        ! =========================================================================================
        ! Vars Statements
        ! =========================================================================================
        ! I/O
        real(8), allocatable, intent(in) :: D(:)  ! Displacements
        real(8), allocatable, intent(in) :: F(:)  ! Vector of loads
        real(8), allocatable, intent(in) :: kl(:, :, :)  ! Stiffness matrix kl(element_id, i, j)
        real(8), allocatable, intent(inout) :: Eff(:, :)  ! elements efforts
        integer, intent(in) :: nel  ! Number of elements
        real(8), allocatable, intent(in) :: rot(:, :, :)  ! Matrix of rotation

        ! Aux
        integer :: i
        real(8) :: kg(6, 6)


        do i = 1, nel
            kg = matmul(matmul(transpose(rot(i, :, :)), kl(i, :, :)), rot(i, :, :))
            Eff(i, :) = matmul(kg, D)
        end do
    end subroutine get_efforts
end module Efforts
