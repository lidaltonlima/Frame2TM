module Boundaries
    implicit none
    private

    public :: add_boundaries
contains
    subroutine add_boundaries(nccdesl, nnr, itydisp, disp, ndofn, K)
        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! I/O
        integer, intent(in) :: nccdesl  ! Number of boundaries condition
        integer, allocatable, intent(in) :: nnr(:)  ! index of bound node
        logical, allocatable, intent(in) :: itydisp(:, :) ! type of bound
        real(8), allocatable, intent(in) :: disp(:, :)  ! displacement value

        integer, intent(in) :: ndofn  ! Number of degrees of freedom per node
        real(8), allocatable, intent(inout) :: K(:, :)  ! Global stiffness global

        ! Auxiliaries
        integer :: i, dir  ! indices
        integer :: i_dir  ! index of bound direction

        do i = 1, nccdesl
            do dir = 1, ndofn
                i_dir = (ndofn * (nnr(i) - 1)) + dir

                if (itydisp(i, dir)) then
                    K(i_dir, :) = 0d0
                    K(:, i_dir) = 0d0
                    K(i_dir, i_dir) = 1d0
                end if
            end do
            print *
        end do
    end subroutine add_boundaries
end module Boundaries
