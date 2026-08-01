module Loads
    use iso_fortran_env, only: real64

    use StructureData

    implicit none
    private
    public :: calc_Fg
contains
    subroutine calc_Fg
        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! Aux *************************************************************************************
        integer :: i, dir  ! indices
        integer :: i_dir  ! index of bound direction
        real(real64), allocatable :: Dp(:)  ! Vector of loads


        ! =========================================================================================
        ! Initialization
        ! =========================================================================================
        ! Allocation
        allocate(Dp(nno*ndofn))

        Fg = 0d0
        do i = 1, nnc
            do dir = 1, ndofn
                i_dir = (ndofn * (nnoc(i) - 1)) + dir
                Fg(i_dir) = Fg(i_dir) + ccno(i, dir)
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

        Fg = Fg - matmul(Kg, Dp)
    end subroutine calc_Fg
end module Loads
