program main
    use StructureData, only: get_structure_data
    use Stiffness, only: get_kl
    use Rotation, only: getRotMat
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
    integer :: nccdesl  ! Number of boundaries condition

    real(8), allocatable :: materials(:, :)
    real(8), allocatable :: sections(:, :, :)
    real(8), allocatable :: nodes(:, :)
    integer, allocatable :: bars(:, :)
    integer, allocatable :: nnr(:)  ! index of bound node
    logical, allocatable :: itydisp(:, :) ! type of bound
    real(8), allocatable :: disp(:, :)  ! displacement value

    character(2) :: theory ! Theory used

    ! Calculate data
    real(8), allocatable :: kl(:, :, :)  ! Stiffness matrix kl(element_id, i, j)
    real(8), allocatable :: rot(:, :, :)  ! Matrix of rotation

    ! Controls
    integer :: i, j  ! Indexes
    integer :: id  ! Index id

    ! =============================================================================================
    ! Calculation
    ! =============================================================================================
    call get_structure_data(nno, nel, ndofn, ntm, nts, nccdesl, nnr, theory, itydisp, disp, &
        materials, sections, nodes, bars)

    kl = get_kl(nel, ndofn, theory, materials, sections, nodes, bars)
    rot = getRotMat(nel, ndofn, nodes, bars)

    ! =============================================================================================
    ! Debug
    ! =============================================================================================
    call show_debug()

contains
    subroutine show_debug()
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
        write(*, '(A9)', advance='no') 'CONTROLS '

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
        write(*, '(1A6, ":", 1A10)') 'theory', theory
        print *
        print *

        ! Materials ***************************************************************************
        write(*, '(A9)', advance='no') 'MATERIALS '
        do i = 1, 91
            write(*, '(A1)', advance='no') '/'
        end do
        print *

        write(*, '(1A4, 3A15)') 'Id', 'E', 'nu', 'rho'
        do i = 1, ntm
            write(*, '(1I4, 1ES15.4, 3F15.4)') i, materials(i, :)
        end do
        print *
        print *

        ! Sections ****************************************************************************
        write(*, '(A9)', advance='no') 'SECTIONS '
        do i = 1, 91
            write(*, '(A1)', advance='no') '/'
        end do
        print *

        write(*, '(1A4, T9, 1A5, T52, 1A10)') 'Id','Area', 'Inertia'
        do i = 1, nts
            write(*, '(1I4, 3ES15.4, 3ES15.4)') i, sections(i, 1, :), sections(i, 2, :)
        end do
        print *
        print *

        ! Nodes *******************************************************************************
        write(*, '(A9)', advance='no') 'NODES    '
        do i = 1, 91
            write(*, '(A1)', advance='no') '/'
        end do
        print *

        write(*, '(1A4, T5, 1A5, T10, 1A10)') 'Id', 'X', 'Y'
        do i = 1, nno
            write(*, '(1I4, 2F10.4)') i, nodes(i, :)
        end do
        print *
        print *

        ! Bars ********************************************************************************
        write(*, '(A9)', advance='no') 'BARS     '
        do i = 1, 91
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
        write(*, '(A9)', advance='no') 'Bounds   '
        do i = 1, 91
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

        ! Local Stiffness Matrix ******************************************************************
        write(*, '(A9)', advance='no') 'Stiffness '
        do i = 1, 91
            write(*, '(A1)', advance='no') '/'
        end do
        print *

        do id = 1, nel
            write(*, '(1A13, 1I4)') 'Element ID: ', id
            do i = 1, 2 * ndofn
                do j = 1, 2 * ndofn
                    write(*, '(ES15.4)', advance='no') kl(id, i, j)
                end do
                print *
            end do
        end do
        print *
        print *

        ! Rot Matrix ******************************************************************************
        write(*, '(A9)', advance='no') 'Rot Mat  '
        do i = 1, 91
            write(*, '(A1)', advance='no') '/'
        end do
        print *
        do id = 1, nel
            write(*, '(1A13, 1I4)') 'Element ID: ', id
            do i = 1, 2 * ndofn
                do j = 1, 2 * ndofn
                    write(*, '(F10.4)', advance='no') rot(id, i, j)
                end do
                print *
            end do
        end do
    end subroutine show_debug
end program main
