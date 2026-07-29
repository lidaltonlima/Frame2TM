module Loads
    implicit none
    private
    public :: calc_F
contains
    subroutine calc_F(nno, ndofn, nnc, nnoc, ccno, nccdesl, nnr, itydisp, disp, K, F)
        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! I/O *************************************************************************************
        integer, intent(in) :: nno  ! Number of nodes
        integer, intent(in) :: nnc  ! Number of nodes with point load
        integer, intent(in) :: ndofn  ! Number of degrees of freedom per node
        integer, intent(in) :: nnoc(:)  ! index of node with load
        real(8), intent(in) :: ccno(:, :)  ! value of point load in node
        integer, intent(in) :: nccdesl  ! Number of boundaries condition
        integer, intent(in) :: nnr(:)  ! index of bound node
        logical, intent(in) :: itydisp(:, :) ! type of bound
        real(8), intent(in) :: disp(:, :)  ! displacement value
        real(8), intent(in) :: K(:, :)  ! Global stiffness global
        real(8), intent(inout) :: F(:)  ! Vector of loads

        ! Aux *************************************************************************************
        integer :: i, dir  ! indices
        integer :: i_dir  ! index of bound direction
        real(8), allocatable :: Dp(:)  ! Vector of loads


        ! =========================================================================================
        ! Initialization
        ! =========================================================================================
        ! Allocation
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
    end subroutine calc_F
end module Loads
