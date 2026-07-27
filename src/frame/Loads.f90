module Loads
    implicit none
    private
    public :: get_F
contains
    subroutine get_F(nno, ndofn, nnc, nnoc, ccno, nccdesl, nnr, itydisp, disp, K, F)
        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! I/O
        integer, intent(in) :: nno  ! Number of nodes
        integer, intent(in) :: nnc  ! Number of nodes with point load
        integer, intent(in):: ndofn  ! Number of degrees of freedom per node
        integer, allocatable, intent(in) :: nnoc(:)  ! index of node with load
        real(8), allocatable, intent(in) :: ccno(:, :)  ! value of point load in node
        integer :: nccdesl  ! Number of boundaries condition
        integer, allocatable :: nnr(:)  ! index of bound node
        logical, allocatable :: itydisp(:, :) ! type of bound
        real(8), allocatable :: disp(:, :)  ! displacement value
        real(8), allocatable :: K(:, :)  ! Global stiffness global
        real(8), allocatable :: F(:)  ! Vector of loads

        ! Auxiliaries
        integer :: i, dir  ! indices
        integer :: i_dir  ! index of bound direction
        real(8), allocatable :: Dp(:)  ! Vector of loads


        ! Allocation
        allocate(F(nno*ndofn))
        allocate(Dp(nno*ndofn))

        F = 0d0
        do i = 1, nnc
            do dir = 1, ndofn
                i_dir = (ndofn * (nnoc(i) - 1)) + dir
                F(i_dir) = F(i_dir) + ccno(i, dir)
            end do
        end do

        Dp = 0d0
        do i = 1, nccdesl
            do dir = 1, ndofn
                i_dir = (ndofn * (nnr(i) - 1)) + dir

                if (itydisp(i, dir)) then
                    Dp(i_dir) = disp(i, dir)
                end if
            end do
        end do

        F = F - matmul(K, Dp)
    end subroutine get_F
end module Loads
