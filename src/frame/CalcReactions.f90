module CalcReactions
    use iso_fortran_env, only: real64

    use StructureData

    implicit none
    private
    public :: calc_reactions, calc_el_reactions
contains
    subroutine calc_reactions
        ! =========================================================================================
        ! Vars Statements
        ! =========================================================================================
        ! Control *********************************************************************************
        integer :: i, j, dir, i_dir ! indexes
        real(real64) :: D_aux(dim)


        ! =========================================================================================
        ! Calculation
        ! =========================================================================================
        D_aux = dc
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
                    reactions(i_dir) = reactions(i_dir) - fc(i_dir)

                    do j = 1, dim
                        reactions(i_dir) = reactions(i_dir) + kc(i_dir, j) * D_aux(j)
                    end do
                end if
            end do
        end do
    end subroutine calc_reactions

    subroutine calc_el_reactions
        ! =========================================================================================
        ! Vars Statements
        ! =========================================================================================on of matrices and vectors

        ! Aux
        integer :: i
        integer :: si, ei
        integer :: sf, ef
        real(real64) :: d_e(el_dim)
        real(real64) :: d_el(el_dim)


        do i = 1, nel
            si = (ndofn * (bars(i, 3) - 1)) + 1
            ei = si + ndofn - 1

            sf = (ndofn * (bars(i, 4) - 1)) + 1
            ef = sf + ndofn - 1

            d_e(:ndofn) = dc(si:ei)
            d_e(ndofn+1:) = dc(sf:ef)

            d_el = matmul(rot_mat(i, :, :), d_e)
            el_reactions(i, :) = matmul(kl(i, :, :), d_el)
        end do
    end subroutine calc_el_reactions
end module CalcReactions
