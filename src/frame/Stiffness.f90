module Stiffness
    use GQint, only: intGQ
    use LinearAlgebra, only: inv, LagPol
    implicit none
    private

    public :: get_kl, get_K

    real(8) :: E  ! Elasticity module
    real(8) :: G
    real(8), allocatable :: A(:)  ! Area
    real(8), allocatable :: As(:)
    real(8) :: L  ! Length
    real(8), allocatable :: I(:)  ! Inertia
    character(2) :: theory_g

    real(8), allocatable :: el_px(:)  ! Points of sample sections

contains
    function get_kl(nel, nsa, ndofn, theory, materials, sections, nodes, bars) result(kl)
        ! Calculate the stiffness matrix local for all elements

        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! I/O
        real(8), allocatable:: kl(:, :, :) ! Stiffness matrix kl(i, j, element_id)
        integer, intent(in) :: nel  ! Number of elements
        integer, intent(in) :: nsa
        integer, intent(in) :: ndofn  ! Number of degrees of freedom per node
        real(8), intent(in) :: materials(:, :)
        real(8), intent(in) :: sections(:, :, :)
        real(8), intent(in) :: nodes(:, :)
        integer, intent(in) :: bars(:, :)

        character(2), intent(in) :: theory

        ! Control
        integer :: id   ! Index id

        ! Auxiliary
        real(8) :: fIi(3, 3)
        real(8) :: fIf(3, 3)
        real(8) :: fFi(3, 3)
        real(8) :: fFf(3, 3)
        real(8) :: AII(3, 3)
        real(8) :: AFF(3, 3)
        real(8) :: EII(3, 3)

        integer :: kl_dim  ! Dimension of stiffness matrix element
        real(8) :: dx, dy  ! Delta x and delta y
        real(8) :: step  ! Step to get sections samples
        integer :: index

        kl_dim = 2 * ndofn  ! 2 nodes per element

        if (nsa < 2) then
            error stop 'get_kl requires nsa >= 2'
        end if

        allocate(el_px(nsa))
        allocate(A(nsa))
        allocate(As(nsa))
        allocate(I(nsa))

        ! =========================================================================================
        ! Calculation
        ! =========================================================================================
        allocate(kl(nel, kl_dim, kl_dim))
        theory_g = theory

        kl = 0D+00
        AII = 0d0
        AFF = 0d0
        do id = 1, nel
            E = materials(bars(id, 1), 1)
            G = E / (2 * (1 + materials(bars(id, 1), 2)))
            A = sections(bars(id, 2), 1, :)
            As = sections(bars(id, 2), 2, :)
            I = sections(bars(id, 2), 3, :)

            dx = nodes(bars(id, 4), 1) - nodes(bars(id, 3), 1)
            dy = nodes(bars(id, 4), 2) - nodes(bars(id, 3), 2)
            L = sqrt(dx**2 + dy**2)

            ! Get points of sample sections
            step = L / (nsa - 1)
            do index = 1, nsa
                el_px(index) = step * (index - 1)
            end do

            EII = 0d0
            EII(1, 1) = 1
            EII(2, 2) = 1
            EII(3, 3) = 1
            EII(3, 2) = -L

            AII(1, 1) = intGQ(0d0, L, a11, 4)
            AII(2, 2) = intGQ(0d0, L, a22, 4)
            AII(2, 3) = intGQ(0d0, L, a23, 4)
            AII(3, 2) = intGQ(0d0, L, a32, 4)
            AII(3, 3) = intGQ(0d0, L, a33, 4)

            AFF(1, 1) = intGQ(0d0, L, a44, 4)
            AFF(2, 2) = intGQ(0d0, L, a55, 4)
            AFF(2, 3) = intGQ(0d0, L, a56, 4)
            AFF(3, 2) = intGQ(0d0, L, a65, 4)
            AFF(3, 3) = intGQ(0d0, L, a66, 4)

            fIi = inv(AII)
            fFf = inv(AFF)
            fIf = matmul(-inv(EII), fFf)
            fFi = matmul(-EII, fIi)

            kl(id, :3, :3) = fIi
            kl(id, :3, 4:) = fIf
            kl(id, 4:, 4:) = fFf
            kl(id, 4:, :3) = fFi
        end do
    end function get_kl

    subroutine get_K(nno, nel, ndofn, bars, kl, rot, K)
        ! Calculate the global stiffness matrix

        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! I/O
        integer, intent(in) :: nno  ! Number of nodes
        integer, intent(in) :: nel  ! Number of elements
        integer, intent(in) :: ndofn  ! Number of degrees of freedom per node

        integer, allocatable :: bars(:, :)
        real(8), allocatable, intent(in) :: kl(:, :, :)  ! Stiffness matrix kl(element_id, i, j)
        real(8), allocatable, intent(in) :: rot(:, :, :)  ! Matrix of rotation

        real(8), allocatable, intent(out) :: K(:, :)  ! Global stiffness global

        ! Auxiliaries
        integer :: element  ! index

        allocate(K(nno*ndofn, nno*ndofn))

        K = 0d0

        do element = 1, nel
            call add_k(element, ndofn, kl, rot, bars, K)
        end do
    end subroutine

    subroutine add_k(id, ndofn, kl, rot, bars, K)
        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! I/O
        integer, intent(in) :: id
        integer, intent(in) :: ndofn  ! Number of degrees of freedom per node
        real(8), allocatable, intent(in) :: kl(:, :, :)  ! Stiffness matrix
        real(8), allocatable, intent(in) :: rot(:, :, :)  ! Matrix of rotation
        integer, allocatable :: bars(:, :)
        real(8), allocatable, intent(inout) :: K(:, :)  ! Global stiffness global

        ! Auxiliaries
        real(8), allocatable :: kg(:, :)  ! Stiffness matrix
        integer :: si, ei  ! indices position of start node
        integer :: sj, ej  ! indices position of start node

        kg = kl(id, :, :)
        kg = matmul(matmul(transpose(rot(id, :, :)), kg), rot(id, :, :))

        si = (ndofn * (bars(id, 3) - 1)) + 1  ! Start index of initial node
        ei = si + ndofn - 1  ! End index of initial node

        sj = (ndofn * (bars(id, 4) - 1)) + 1  ! Start index of end node
        ej = sj + ndofn - 1  ! End index of end node


        K(si:ei, si:ei) = K(si:ei, si:ei) + kg(:3, :3)  ! k_ii
        K(si:ei, sj:ej) = K(si:ei, sj:ej) + kg(:3, 4:)  ! k_ij
        K(sj:ej, si:ei) = K(sj:ej, si:ei) + kg(4:, :3)  ! k_ji
        K(sj:ej, sj:ej) = K(sj:ej, sj:ej) + kg(4:, 4:)  ! k_jj
    end subroutine add_k

    pure function ka(x) result(y)
        real(8), intent(in) :: x
        real(8) :: y

        y = E * LagPol(el_px, A, x)
    end function ka

    pure function kb(x) result(y)
        real(8), intent(in) :: x
        real(8) :: y

        y = E * LagPol(el_px, I, x)
    end function kb

    pure function ks(x) result(y)
        real(8), intent(in) :: x
        real(8) :: y

        y = G * LagPol(el_px, As, x)
    end function ks

    pure function a11(x) result(y)
        real(8), intent(in) :: x
        real(8) :: y

        y = 1 / ka(x)
    end function a11

    pure function a22(x) result(y)
        real(8), intent(in) :: x
        real(8) :: y

        y = x**2 / kb(x) + merge(1 / ks(x), 0d0, theory_g == 'TM')
    end function a22

    pure function a23(x) result(y)
        real(8), intent(in) :: x
        real(8) :: y

        y = -x / kb(x)
    end function a23

    pure function a32(x) result(y)
        real(8), intent(in) :: x
        real(8) :: y

        y = -x / kb(x)
    end function a32

    pure function a33(x) result(y)
        real(8), intent(in) :: x
        real(8) :: y

        y = 1 / kb(x)
    end function a33

    pure function a44(x) result(y)
        real(8), intent(in) :: x
        real(8) :: y

        y = 1 / ka(x)
    end function a44

    pure function a55(x) result(y)
        real(8), intent(in) :: x
        real(8) :: y

        y = (L - x)**2 / kb(x) + merge(1 / ks(x), 0d0, theory_g == 'TM')
    end function a55

    pure function a56(x) result(y)
        real(8), intent(in) :: x
        real(8) :: y

        y = (L - x) / kb(x)
    end function a56

    pure function a65(x) result(y)
        real(8), intent(in) :: x
        real(8) :: y

        y = (L - x) / kb(x)
    end function a65

    pure function a66(x) result(y)
        real(8), intent(in) :: x
        real(8) :: y

        y = 1 / kb(x)
    end function a66
end module Stiffness
