module CalcReactions
    use iso_fortran_env, only: real64

    use StructureData
    use Rotation, only: R
    use Stiffness, only: EKl

    implicit none
    private
    public :: calc_Rg, calc_ERl
contains
    subroutine calc_Rg
        ! =========================================================================================
        ! Vars Statements
        ! =========================================================================================
        ! Control *********************************************************************************
        integer :: i, j, dir, i_dir ! indexes
        real(real64) :: D_aux(dim)


        ! =========================================================================================
        ! Calculation
        ! =========================================================================================
        D_aux = Dg
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
                    Rg(i_dir) = Rg(i_dir) - Fg(i_dir)

                    do j = 1, dim
                        Rg(i_dir) = Rg(i_dir) + Kg(i_dir, j) * D_aux(j)
                    end do
                end if
            end do
        end do
    end subroutine calc_Rg

    subroutine calc_ERl
        ! =========================================================================================
        ! Vars Statements
        ! =========================================================================================on of matrices and vectors

        ! Aux
        integer :: i
        integer :: si, ei  ! start and end index in initial node
        integer :: sf, ef  ! start and end index in end node
        real(real64) :: EDg(E_dim)  ! element displacement in global system
        real(real64) :: EDl(E_dim)  ! element displacement in local system


        do i = 1, nel
            si = (ndofn * (bars(i, 3) - 1)) + 1
            ei = si + ndofn - 1

            sf = (ndofn * (bars(i, 4) - 1)) + 1
            ef = sf + ndofn - 1

            EDg(:ndofn) = Dg(si:ei)
            EDg(ndofn+1:) = Dg(sf:ef)

            EDl = matmul(R(i), EDg)
            ERl(i, :) = matmul(EKl(i), EDl)
        end do
    end subroutine calc_ERl
end module CalcReactions
