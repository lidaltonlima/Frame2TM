module Stiffness
    use Lagrange, only: LagPol
    use GQint, only: intGQ
    implicit none
    private

    public :: get_kl

    real(8) :: E  ! Elasticity module
    real(8) :: G
    real(8) :: A(3)  ! Area
    real(8), parameter :: As(3) = [5d-2, 5d-2, 5d-2]
    real(8) :: L  ! Length
    real(8) :: I(3)  ! Inertia
    character(2) :: theory_g

contains
    function get_kl(nel, ndofn, theory, materials, sections, nodes, bars) result(kl)
        ! Calculate the stiffness matrix local for all elements

        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! I/O
        real(8), allocatable:: kl(:, :, :) ! Stiffness matrix kl(i, j, element_id)
        integer, intent(in) :: nel  ! Number of elements
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

        kl_dim = 2 * ndofn  ! 2 nodes per element

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
            G = E / (2 * (1 + 0.3))
            A = sections(bars(id, 2), 1, :)
            I = sections(bars(id, 2), 2, :)

            dx = nodes(bars(id, 4), 1) - nodes(bars(id, 3), 1)
            dy = nodes(bars(id, 4), 2) - nodes(bars(id, 3), 2)
            L = sqrt(dx**2 + dy**2)

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

    function inv(mat) result(mat_inv)
        ! Calculate the inverse of matrix

        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! I/O
        real(8), intent(in) :: mat(:, :)

        ! Auxiliary
        integer :: n
        integer :: info
        integer :: lwork

        integer, allocatable :: ipiv(:)

        real(8), allocatable :: work(:)
        real(8), allocatable :: mat_inv(:, :)

        ! External functions
        external :: dgetrf, dgetri

        n = size(mat, 1)
        if (size(mat, 2) /= n) error stop 'inv requires a square matrix'

        ! Allocate
        allocate(mat_inv(n, n))
        allocate(ipiv(n))

        ! Initialize vars
        mat_inv = mat
        lwork = n

        call dgetrf(n, n, mat_inv, n, ipiv, info)
        if (info /= 0) error stop 'DGETRF failed while inverting matrix in inv'

        allocate(work(lwork))
        call dgetri(n, mat_inv, n, ipiv, work, lwork, info)
        if (info /= 0) error stop 'DGETRI failed while inverting matrix in inv'
    end function inv

    pure function ka(x) result(y)
        real(8), intent(in) :: x
        real(8) :: y

        y = E * LagPol([0d0, L/2, L], A, x)
    end function ka

    pure function kb(x) result(y)
        real(8), intent(in) :: x
        real(8) :: y

        y = E * LagPol([0d0, L/2, L], I, x)
    end function kb

    pure function ks(x) result(y)
        real(8), intent(in) :: x
        real(8) :: y

        y = G * LagPol([0d0, L/2, L], As, x)
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
