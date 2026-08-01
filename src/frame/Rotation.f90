module Rotation
    use iso_fortran_env, only: real64

    use StructureData, only: bars, nodes, E_dim
    use LinearAlgebra, only: cross

    implicit none
    private

    public :: R
contains
    function R(id)
        ! Calculate the element rotation matrix on demand.
        integer, intent(in) :: id
        real(real64), allocatable :: R(:, :)
        real(real64) :: e_vec(3)
        real(real64) :: n_vec(3)
        real(real64) :: x_vec(3)
        real(real64) :: y_vec(3)
        real(real64) :: z_vec(3)

        allocate(R(E_dim, E_dim))
        R = 0d0

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

        R(1, :3) = x_vec
        R(2, :3) = y_vec
        R(3, :3) = z_vec

        R(4:, 4:) = R(:3, :3)
    end function R
end module Rotation
