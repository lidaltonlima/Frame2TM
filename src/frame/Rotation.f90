module Rotation
    use iso_fortran_env, only: real64

    use StructureData
    use LinearAlgebra, only: cross

    implicit none
    private

    public :: calc_rot_mat
contains
    subroutine calc_rot_mat
        ! Calculate the rotation matrix

        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! Auxiliaries *****************************************************************************
        integer :: id
        real(real64) :: e_vec(3)
        real(real64) :: n_vec(3)
        real(real64) :: x_vec(3)
        real(real64) :: y_vec(3)
        real(real64) :: z_vec(3)

        ! =========================================================================================
        ! Calculation
        ! =========================================================================================
        R = 0d0
        do id = 1, nel
            e_vec = 0d0
            e_vec = [ &
                nodes(bars(id, 4), 1) - nodes(bars(id, 3), 1), &
                nodes(bars(id, 4), 2) - nodes(bars(id, 3), 2), &
                0d0]

            if (e_vec(1) > 0) then
                n_vec = [e_vec(1), e_vec(2) + 1, 0d0]
            else if (e_vec(1) < 0) then
                n_vec = [e_vec(1), e_vec(2) - 1, 0d0]
            else
                if (e_vec(2) > 0) then
                    n_vec = [e_vec(1) - 1, e_vec(2), 0d0]
                else
                    n_vec = [e_vec(1) + 1, e_vec(2), 0d0]
                end if
            end if

            x_vec = e_vec / norm2(e_vec)

            z_vec = cross(x_vec, n_vec)
            z_vec = z_vec / norm2(z_vec)

            y_vec = cross(z_vec, x_vec)

            R(id, 1, :3) = x_vec
            R(id, 2, :3) = y_vec
            R(id, 3, :3) = z_vec

            R(id, 4:, 4:) = R(id, :3, :3)
        end do
    end subroutine calc_rot_mat
end module Rotation
