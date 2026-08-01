module Stiffness
    use iso_fortran_env, only: real64

    use StructureData
    use GQint, only: intGQ
    use LinearAlgebra, only: inv, LagPol

    implicit none
    private
    public :: calc_EKl, calc_Kg

    ! =============================================================================================
    ! Global vars
    ! =============================================================================================
    real(real64) :: E  ! elasticity module
    real(real64) :: G  ! shear elasticity module
    real(real64), allocatable :: A(:)  ! area
    real(real64), allocatable :: As(:)  ! shear area
    real(real64) :: L  ! length of element
    real(real64), allocatable :: Iz(:)  ! inertia
    character(2) :: theory_g  ! theory used

    real(real64), allocatable :: el_px(:)  ! Points of sample sections
contains
    subroutine calc_EKl
        ! Calculate the stiffness matrix local for all elements

        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! Control *********************************************************************************
        integer :: id   ! Index id

        ! Aux *************************************************************************************
        real(real64) :: fIi(3, 3)
        real(real64) :: fIf(3, 3)
        real(real64) :: fFi(3, 3)
        real(real64) :: fFf(3, 3)
        real(real64) :: AII(3, 3)
        real(real64) :: AFF(3, 3)
        real(real64) :: EII(3, 3)

        real(real64) :: dx, dy  ! Delta x and delta y
        real(real64) :: step  ! Step to get sections samples
        integer :: index

        ! =========================================================================================
        ! Initialization
        ! =========================================================================================
        if (nsa < 2) then
            error stop 'get_kl requires nsa >= 2'
        end if

        if (.not. allocated(el_px)) allocate(el_px(nsa))
        if (.not. allocated(A)) allocate(A(nsa))
        if (.not. allocated(As)) allocate(As(nsa))
        if (.not. allocated(Iz)) allocate(Iz(nsa))

        ! =========================================================================================
        ! Calculation
        ! =========================================================================================
        theory_g = theory

        EKl = 0d0
        AII = 0d0
        AFF = 0d0
        do id = 1, nel
            E = materials(bars(id, 1), 1)
            G = E / (2 * (1 + materials(bars(id, 1), 2)))
            A = sections(bars(id, 2), 1, :)
            As = sections(bars(id, 2), 2, :)
            Iz = sections(bars(id, 2), 3, :)

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

            EKl(id, :3, :3) = fIi
            EKl(id, :3, 4:) = fIf
            EKl(id, 4:, 4:) = fFf
            EKl(id, 4:, :3) = fFi
        end do
    end subroutine

    subroutine calc_Kg
        ! Calculate the global stiffness matrix

        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! Aux *************************************************************************************
        integer :: element  ! index

        allocate(Kg(nno*ndofn, nno*ndofn))

        Kg = 0d0

        do element = 1, nel
            call add_k(Kg, element)
        end do
    end subroutine

    subroutine add_k(K, id)
        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! I/O *************************************************************************************
        integer, intent(in) :: id  ! index of element
        real(real64), allocatable, intent(inout) :: K(:, :)  ! Global stiffness global

        ! Auxiliaries *****************************************************************************
        real(real64), allocatable :: EKg(:, :)  ! element stiffness matrix in global system
        integer :: si, ei  ! start and end index in initial node
        integer :: sj, ej  ! start and end index in end node

        ! =========================================================================================
        ! Calculates
        ! =========================================================================================
        EKg = EKl(id, :, :)
        EKg = matmul(matmul(transpose(R(id, :, :)), EKg), R(id, :, :))

        si = (ndofn * (bars(id, 3) - 1)) + 1  ! Start index of initial node
        ei = si + ndofn - 1  ! End index of initial node

        sj = (ndofn * (bars(id, 4) - 1)) + 1  ! Start index of end node
        ej = sj + ndofn - 1  ! End index of end node


        K(si:ei, si:ei) = K(si:ei, si:ei) + EKg(:3, :3)  ! k_ii
        K(si:ei, sj:ej) = K(si:ei, sj:ej) + EKg(:3, 4:)  ! k_ij
        K(sj:ej, si:ei) = K(sj:ej, si:ei) + EKg(4:, :3)  ! k_ji
        K(sj:ej, sj:ej) = K(sj:ej, sj:ej) + EKg(4:, 4:)  ! k_jj
    end subroutine add_k

    pure function ka(x) result(y)
        real(real64), intent(in) :: x
        real(real64) :: y

        y = E * LagPol(el_px, A, x)
    end function ka

    pure function kb(x) result(y)
        real(real64), intent(in) :: x
        real(real64) :: y

        y = E * LagPol(el_px, Iz, x)
    end function kb

    pure function ks(x) result(y)
        real(real64), intent(in) :: x
        real(real64) :: y

        y = G * LagPol(el_px, As, x)
    end function ks

    pure function a11(x) result(y)
        real(real64), intent(in) :: x
        real(real64) :: y

        y = 1 / ka(x)
    end function a11

    pure function a22(x) result(y)
        real(real64), intent(in) :: x
        real(real64) :: y

        y = x**2 / kb(x) + merge(1 / ks(x), 0d0, theory_g == 'TM')
    end function a22

    pure function a23(x) result(y)
        real(real64), intent(in) :: x
        real(real64) :: y

        y = -x / kb(x)
    end function a23

    pure function a32(x) result(y)
        real(real64), intent(in) :: x
        real(real64) :: y

        y = -x / kb(x)
    end function a32

    pure function a33(x) result(y)
        real(real64), intent(in) :: x
        real(real64) :: y

        y = 1 / kb(x)
    end function a33

    pure function a44(x) result(y)
        real(real64), intent(in) :: x
        real(real64) :: y

        y = 1 / ka(x)
    end function a44

    pure function a55(x) result(y)
        real(real64), intent(in) :: x
        real(real64) :: y

        y = (L - x)**2 / kb(x) + merge(1 / ks(x), 0d0, theory_g == 'TM')
    end function a55

    pure function a56(x) result(y)
        real(real64), intent(in) :: x
        real(real64) :: y

        y = (L - x) / kb(x)
    end function a56

    pure function a65(x) result(y)
        real(real64), intent(in) :: x
        real(real64) :: y

        y = (L - x) / kb(x)
    end function a65

    pure function a66(x) result(y)
        real(real64), intent(in) :: x
        real(real64) :: y

        y = 1 / kb(x)
    end function a66
end module Stiffness
