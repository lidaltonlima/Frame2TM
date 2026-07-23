module Rotation
    use LinearAlgebra, only: cross

    implicit none
    private

    public :: getRotMat
contains
    pure function getRotMat(nel, ndofn, nodes, bars) result(rot)
        ! Calculate the rotation matrix

        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! I/O *************************************************************************************
        integer, intent(in) :: nel  ! Number of elements
        integer, intent(in) :: ndofn  ! Number of degrees of freedom per node
        real(8), intent(in) :: nodes(:, :)
        integer, intent(in) :: bars(:, :)
        real(8), allocatable :: rot(:, :, :) ! Matrix of rotation

        ! Auxiliaries *****************************************************************************
        integer :: id
        real(8) :: aux_vec(3)
        real(8) :: e_vec(3)
        real(8) :: n_vec(3)
        real(8) :: x_vec(3)
        real(8) :: y_vec(3)
        real(8) :: z_vec(3)

        ! =========================================================================================
        ! Calculation
        ! =========================================================================================
        ! Allocation
        allocate(rot(nel, 2 * ndofn, 2 * ndofn))

        rot = 0d0
        do id = 1, nel
            e_vec = 0d0
            e_vec = nodes(bars(id, 4), :) - nodes(bars(id, 3), :)

            if (e_vec(1) > 0) then
                aux_vec = [e_vec(1), e_vec(2) + 1, 0d0]
            else if (e_vec(1) < 0) then
                aux_vec = [e_vec(1), e_vec(2) - 1, 0d0]
            else
                if (e_vec(2) > 0) then
                    aux_vec = [e_vec(1) - 1, e_vec(2), 0d0]
                else
                    aux_vec = [e_vec(1) + 1, e_vec(2), 0d0]
                end if
            end if

            n_vec = aux_vec - nodes(bars(id, 3), :)

            x_vec = e_vec / norm2(e_vec)

            z_vec = cross(x_vec, n_vec)
            z_vec = z_vec / norm2(z_vec)

            y_vec = cross(z_vec, x_vec)

            rot(id, 1, :3) = x_vec
            rot(id, 2, :3) = y_vec
            rot(id, 3, :3) = z_vec

            rot(id, 4:, 4:) = rot(id, :3, :3)
        end do
    end function getRotMat
end module Rotation
