program main
    use StructureData, only: get_structure_data
    use Stiffness, only: get_kl, get_K
    use Rotation, only: getRotMat
    use Boundaries, only: add_boundaries
    use Loads, only: get_F
    implicit none

    ! =============================================================================================
    ! Vars statement
    ! =============================================================================================
    ! Structure data
    integer :: nno  ! Number of nodes
    integer :: nel  ! Number of elements
    integer :: ndofn  ! Number of degrees of freedom per node
    integer :: ntm  ! Number of materials
    integer :: nts  ! Number of sections
    integer :: nnc  ! Number of nodes with point load
    character(2) :: theory ! Theory used

    real(8), allocatable :: materials(:, :)
    real(8), allocatable :: sections(:, :, :)
    real(8), allocatable :: nodes(:, :)
    integer, allocatable :: bars(:, :)

    integer :: nccdesl  ! Number of boundaries condition
    integer, allocatable :: nnr(:)  ! index of bound node
    logical, allocatable :: itydisp(:, :) ! type of bound
    real(8), allocatable :: disp(:, :)  ! displacement value

    integer, allocatable :: nnoc(:)  ! index of node with load
    real(8), allocatable :: ccno(:, :)  ! value of point load in node
    real(8), allocatable :: F(:)  ! Vector of loads

    real(8), allocatable :: K(:, :)  ! Global stiffness global
    real(8), allocatable :: D(:)  ! Displacements


    ! Calculate data
    real(8), allocatable :: kl(:, :, :)  ! Stiffness matrix kl(element_id, i, j)
    real(8), allocatable :: rot(:, :, :)  ! Matrix of rotation

    ! Controls
    integer :: i, j  ! Indexes
    integer :: id  ! Index id

    ! Auxiliaries
    real(8), allocatable :: K_aux(:, :)

    ! =============================================================================================
    ! Calculation
    ! =============================================================================================
    call get_structure_data(nno, nel, ndofn, ntm, nts, nccdesl, nnr, nnc, theory, &
        itydisp, disp, nnoc, ccno, &
        materials, sections, nodes, bars)

    kl = get_kl(nel, ndofn, theory, materials, sections, nodes, bars)
    rot = getRotMat(nel, ndofn, nodes, bars)
    call get_K(nno, nel, ndofn, bars, kl, rot, K)
    call add_boundaries(nccdesl, nnr, itydisp, disp, ndofn, K)
    call get_F(nno, ndofn, nnc, nnoc, ccno, F)

    allocate(K_aux(nno*ndofn, nno*ndofn))
    allocate(D(nno*ndofn))

    K_aux = K
    D = F

    call solver()



    ! =============================================================================================
    ! Debug
    ! =============================================================================================
    call show_debug(1.0d-50)

contains
    subroutine solver
        ! Auxiliaries
        integer :: info  ! status of operation

        ! External
        external :: dposv

        call dposv('U', nno*ndofn, 1, K_aux, nno*ndofn, D, nno*ndofn, info)
    end subroutine solver

    subroutine show_debug(tolerance)
        real(8), intent(in) :: tolerance

        100 format(1A6, ':', 1I10)
        ! Title *******************************************************************************
        do i = 1, 100
            write(*, '(A)', advance='no') '='
        end do

        write(*, '(/, A)') 'Debug'

        do i = 1, 100
            write(*, '(A)', advance='no') '='
        end do
        write(*, *)

        ! Controls ********************************************************************************
        write(*, '(A9)', advance='no') 'Controls '

        do i = 1, 91
            write(*, '(A1)', advance='no') '/'
        end do
        print *

        write(*, 100) 'nno', nno
        write(*, 100) 'nel', nel
        write(*, 100) 'ndofn', ndofn
        write(*, 100) 'nmat', ntm
        write(*, 100) 'nsec', nts
        write(*, 100) 'nccdesl', nccdesl
        write(*, 100) 'nnc', nnc
        write(*, '(1A6, ":", 1A10)') 'theory', theory
        print *
        print *

        ! Materials *******************************************************************************
        write(*, '(A10)', advance='no') 'Materials '
        do i = 1, 90
            write(*, '(A1)', advance='no') '/'
        end do
        print *

        write(*, '(1A4, 3A15)') 'Id', 'E', 'nu', 'rho'
        do i = 1, ntm
            write(*, '(1I4, 1ES15.4, 3F15.4)') i, materials(i, :)
        end do
        print *
        print *

        ! Sections ********************************************************************************
        write(*, '(A9)', advance='no') 'SECTIONS '
        do i = 1, 91
            write(*, '(A1)', advance='no') '/'
        end do
        print *

        write(*, '(1A4, T7, 1A4, T37, 1A10, T64, 1A10)') 'Id','Area', 'Shear Area', 'Inertia'
        do i = 1, nts
            write(*, '(1I4, 9ES10.2)') &
                i, sections(i, 1, :), sections(i, 2, :),sections(i, 3, :)
        end do
        print *
        print *

        ! Nodes ***********************************************************************************
        write(*, '(A6)', advance='no') 'NODES '
        do i = 1, 94
            write(*, '(A1)', advance='no') '/'
        end do
        print *

        write(*, '(1A4, T5, 1A5, T10, 1A10)') 'Id', 'X', 'Y'
        do i = 1, nno
            write(*, '(1I4, 2F10.4)') i, nodes(i, :)
        end do
        print *
        print *

        ! Bars ************************************************************************************
        write(*, '(A5)', advance='no') 'BARS '
        do i = 1, 95
            write(*, '(A1)', advance='no') '/'
        end do
        print *

        write(*, '(1A4, 4A15)') 'id', 'Material', 'Section', 'Start Node', 'End Node'
        do i = 1, nel
            write(*, '(1I4, 4I15)') i, bars(i, :)
        end do
        print *
        print *

        ! Boundaries ******************************************************************************
        write(*, '(A7)', advance='no') 'Bounds '
        do i = 1, 93
            write(*, '(A1)', advance='no') '/'
        end do
        print *

        write(*, '(1A4, *(A7))', advance='no') 'Id', 'node', 'Dx', 'Dy', 'Rz'
        write(*, '(*(A20))') 'Dx', 'Dy', 'Rz'
        do i = 1, nccdesl
            write(*, '(1I4, 1I7, 1L7)', advance='no') i, nnr(i)
            write(*, '(*(L7))', advance='no') itydisp(i, :)
            write(*, '(*(F20.4))', advance='no') disp(i, :)
            print *
        end do
        print *
        print *

        ! Nodal Loads *****************************************************************************
        write(*, '(A6)', advance='no') 'Loads '
        do i = 1, 94
            write(*, '(A1)', advance='no') '/'
        end do
        print *

        write(*, '(1A4, 1A7, *(A10))') 'Id', 'node', 'Fx', 'Fy', 'Mz'
        do i = 1, nnc
            write(*, '(1I4, 1I7, *(F10.2))') i, nnoc(i), ccno(i, :)
            print *
        end do
        print *
        print *

        ! Local Stiffness Matrix ******************************************************************
        write(*, '(A17)', advance='no') 'Stiffness Matrix '
        do i = 1, 83
            write(*, '(A1)', advance='no') '/'
        end do
        print *

        do id = 1, nel
            write(*, '(1A13, 1I4)') 'Element ID: ', id
            do i = 1, 2 * ndofn
                do j = 1, 2 * ndofn
                    if (abs(kl(id, i, j)) < tolerance) then
                        write(*, '(ES15.4)', advance='no') 0.0d0
                    else
                        write(*, '(ES15.4)', advance='no') kl(id, i, j)
                    end if
                end do
                print *
            end do
        end do
        print *
        print *

        ! Rot Matrix ******************************************************************************
        write(*, '(A16)', advance='no') 'Rotation Matrix '
        do i = 1, 84
            write(*, '(A1)', advance='no') '/'
        end do
        print *
        do id = 1, nel
            write(*, '(1A13, 1I4)') 'Element ID: ', id
            do i = 1, 2 * ndofn
                do j = 1, 2 * ndofn
                    if (abs(rot(id, i, j)) < tolerance) then
                        write(*, '(F10.4)', advance='no') 0.0d0
                    else
                        write(*, '(F10.4)', advance='no') rot(id, i, j)
                    end if
                end do
                print *
            end do
        end do
        print *
        print *

        ! Global Matrix ***************************************************************************
        write(*, '(A14)', advance='no') 'Global Matrix '
        do i = 1, 87
            write(*, '(A1)', advance='no') '/'
        end do
        print *
        do i = 1, nno*ndofn
            do j = 1, nno*ndofn
                if (abs(K(i, j)) < tolerance) then
                    write(*, '(ES10.2)', advance='no') 0.0d0
                else
                    write(*, '(ES10.2)', advance='no') K(i, j)
                end if
            end do
            print *
        end do
        print *
        print *

        ! Load Vector *****************************************************************************
        write(*, '(A11)', advance='no') 'Lad Vector '
        do i = 1, 89
            write(*, '(A1)', advance='no') '/'
        end do
        print *
        do i = 1, nno*ndofn
            if (abs(F(i)) < tolerance) then
                write(*, '(ES10.2)', advance='no') 0.0d0
            else
                write(*, '(ES10.2)', advance='no') F(i)
            end if
        end do
        print *
        print *
        print *

        ! Displacements ***************************************************************************
        write(*, '(A14)', advance='no') 'Displacements '
        do i = 1, 86
            write(*, '(A1)', advance='no') '/'
        end do
        print *
        do i = 1, nno*ndofn
            if (abs(D(i)) < tolerance) then
                write(*, '(ES10.2)', advance='no') 0.0d0
            else
                write(*, '(ES10.2)', advance='no') D(i)
            end if
        end do
        print *
        print *
        print *
    end subroutine show_debug
end program main
