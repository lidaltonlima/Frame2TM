module Stiffness
    implicit none
    private

    public :: get_kl
contains
    pure function get_kl(nel, ndofn, materials, sections, nodes, bars) result(kl)
        ! Calculate the stiffness matrix local for all elements

        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! I/O
        real(8), allocatable:: kl(:, :, :) ! Stiffness matrix kl(i, j, element_id)
        integer, intent(in) :: nel  ! Number of elements
        integer, intent(in) :: ndofn  ! Number of degrees of freedom per node
        real(8), intent(in) :: materials(:, :)
        real(8), intent(in) :: sections(:, :)
        real(8), intent(in) :: nodes(:, :)
        integer, intent(in) :: bars(:, :)

        ! Control
        integer :: id   ! Index id

        ! Auxiliary
        integer :: kl_dim  ! Dimension of stiffness matrix element
        real(8) :: E  ! Elasticity module
        real(8) :: A  ! Area
        real(8) :: L  ! Length
        real(8) :: I  ! Inertia
        real(8) :: dx, dy  ! Delta x and delta y

        kl_dim = 2 * ndofn  ! 2 nodes per element

        ! =========================================================================================
        ! Calculation
        ! =========================================================================================
        allocate(kl(nel, kl_dim, kl_dim))

        kl = 0D+00
        do id = 1, nel
            E = materials(bars(id, 1), 1)
            A = sections(bars(id, 2), 1)
            I = sections(bars(id, 2), 2)

            dx = nodes(bars(id, 4), 1) - nodes(bars(id, 3), 1)
            dy = nodes(bars(id, 4), 2) - nodes(bars(id, 3), 2)
            L = sqrt(dx**2 + dy**2)


            ! 1st line
            kl(id, 1, 1) = (E * A) / L
            kl(id, 1, 4) = -kl(id, 1, 1)

            ! 2nd line
            kl(id, 2, 2) = (12 * E * I) / L**3
            kl(id, 2, 3) = (6 * E * I) / L**2
            kl(id, 2, 5) = -kl(id, 2, 2)
            kl(id, 2, 6) = kl(id, 2, 3)

            ! 3rd line
            kl(id, 3, 2) = kl(id, 2, 3)
            kl(id, 3, 3) = (4 * E * I) / L
            kl(id, 3, 5) = -kl(id, 2, 3)
            kl(id, 3, 6) = (2 * E * I) / L

            ! 4th line
            kl(id, 4, 1) = kl(id, 1, 4)
            kl(id, 4, 4) = kl(id, 1, 1)

            ! 5th line
            kl(id, 5, 2) = kl(id, 2, 5)
            kl(id, 5, 3) = kl(id, 3, 5)
            kl(id, 5, 5) = kl(id, 2, 2)
            kl(id, 5, 6) = kl(id, 3, 5)

            ! 6th line
            kl(id, 6, 2)= kl(id, 2, 3)
            kl(id, 6, 3)= kl(id, 3, 6)
            kl(id, 6, 5)= kl(id, 3, 5)
            kl(id, 6, 6)= kl(id, 3, 3)
        end do
    end function get_kl
end module Stiffness
