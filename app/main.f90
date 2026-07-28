program main
    use StructureData, only: get_structure_data, save_data_file
    use Stiffness, only: get_kl, get_K
    use Rotation, only: getRotMat
    use Boundaries, only: add_boundaries
    use Loads, only: get_F
    use Reactions, only: get_reactions, get_el_reactions
    use Efforts, only: get_efforts
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
    integer :: nsa  ! Number of sample sections
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
    real(8), allocatable :: reactions(:)  ! reactions


    ! Calculate data
    real(8), allocatable :: kl(:, :, :)  ! Stiffness matrix kl(element_id, i, j)
    real(8), allocatable :: rot(:, :, :)  ! Matrix of rotation
    real(8), allocatable :: El_reactions(:, :)  ! elements reactions
    real(8), allocatable :: Eff(:, :)  ! elements efforts

    ! Controls
    integer :: i, j  ! Indexes
    integer :: id  ! Index id

    ! Auxiliaries
    real(8), allocatable :: Kwb(:, :)  ! stiffness matrix without boundary condiciones
    real(8), allocatable :: Fwb(:)
    integer :: dir
    integer :: i_dir

    ! =============================================================================================
    ! Calculation
    ! =============================================================================================
    call get_structure_data(nno, nel, ndofn, ntm, nts, nccdesl, nnr, nnc, nsa, theory, &
        itydisp, disp, nnoc, ccno, &
        materials, sections, nodes, bars)

    allocate(Kwb(nno*ndofn, nno*ndofn))
    allocate(Fwb(nno*ndofn))
    allocate(D(nno*ndofn))
    allocate(reactions(nno*ndofn))
    allocate(El_reactions(nel, 2*ndofn))
    allocate(Eff(nel, 2*ndofn))

    kl = get_kl(nel, ndofn, theory, materials, sections, nodes, bars)
    rot = getRotMat(nel, ndofn, nodes, bars)
    call get_K(nno, nel, ndofn, bars, kl, rot, K)

    call get_F(nno, ndofn, nnc, nnoc, ccno, nccdesl, nnr, itydisp, disp, K, F)

    Kwb = K
    Fwb = F

    call add_boundaries(nccdesl, nnr, itydisp, disp, ndofn, K, F)

    D = F

    call solver()
    call get_reactions(reactions, kwb, D, Fwb, nccdesl, nnr, itydisp, ndofn, nno)

    ! Sum the previus displacement
    do i = 1, nccdesl
        do dir = 1, ndofn
            i_dir = (ndofn * (nnr(i) - 1)) + dir

            if (itydisp(i, dir)) then
                D(i_dir) = disp(i, dir)
            end if
        end do
    end do

    call get_el_reactions(El_reactions, nel, ndofn, bars, kl, rot, D)
    call get_efforts(Eff, El_reactions, bars, nodes, nel)

    ! =============================================================================================
    ! Show and save values
    ! =============================================================================================
    call save_results(1.0d-15)

contains
    subroutine solver
        ! Auxiliaries
        integer :: info  ! status of operation

        ! External
        external :: dposv

        call dposv('U', nno*ndofn, 1, K, nno*ndofn, D, nno*ndofn, info)
    end subroutine solver

    subroutine save_results(tolerance)
        real(8), intent(in) :: tolerance

        ! Auxiliary vars
        character(10) :: int2str1
        character(10) :: int2str2

        ! File vars
        integer :: file_unit  ! Unit to file

        call save_data_file('results', file_unit)

        100 format(1A6, ':', 1I10)
        ! Title *******************************************************************************
        do i = 1, 100
            write(file_unit, '(A)', advance='no') '='
        end do

        write(file_unit, '(/, A)') 'Debug'

        do i = 1, 100
            write(file_unit, '(A)', advance='no') '='
        end do
        write(file_unit, *)

        ! Controls ********************************************************************************
        write(file_unit, '(A9)', advance='no') 'Controls '

        do i = 1, 91
            write(file_unit, '(A1)', advance='no') '/'
        end do
        write(file_unit, *)

        write(file_unit, 100) 'nno', nno
        write(file_unit, 100) 'nel', nel
        write(file_unit, 100) 'ndofn', ndofn
        write(file_unit, 100) 'nmat', ntm
        write(file_unit, 100) 'nsec', nts
        write(file_unit, 100) 'nccdesl', nccdesl
        write(file_unit, 100) 'nnc', nnc
        write(file_unit, 100) 'nsa', nsa
        write(file_unit, '(1A6, ":", 1A10)') 'theory', theory
        write(file_unit, *)
        write(file_unit, *)

        ! Materials *******************************************************************************
        write(file_unit, '(A10)', advance='no') 'Materials '
        do i = 1, 90
            write(file_unit, '(A1)', advance='no') '/'
        end do
        write(file_unit, *)

        write(file_unit, '(1A4, 3A15)') 'Id', 'E', 'nu', 'rho'
        do i = 1, ntm
            write(file_unit, '(1I4, 1ES15.4, 3F15.4)') i, materials(i, :)
        end do
        write(file_unit, *)
        write(file_unit, *)

        ! Sections ********************************************************************************
        write(file_unit, '(A9)', advance='no') 'SECTIONS '
        do i = 1, 91
            write(file_unit, '(A1)', advance='no') '/'
        end do
        write(file_unit, *)

        write(int2str1, '(I0)') nsa*10 + 7
        write(int2str2, '(I0)') nsa*10*2 + 4
        write(file_unit, '(1A4, T7, 1A4, T' // int2str1 // ', 1A10, T' // int2str2 // ', 1A10)') &
            'Id','Area', 'Shear Area', 'Inertia'
        write(int2str1, '(I0)') nsa*3
        do i = 1, nts

            write(file_unit, '(1I4,' // int2str1 // 'ES10.2)') &
                i, sections(i, 1, :), sections(i, 2, :),sections(i, 3, :)
        end do
        write(file_unit, *)
        write(file_unit, *)

        ! Nodes ***********************************************************************************
        write(file_unit, '(A6)', advance='no') 'NODES '
        do i = 1, 94
            write(file_unit, '(A1)', advance='no') '/'
        end do
        write(file_unit, *)

        write(file_unit, '(1A4, T5, 1A5, T10, 1A10)') 'Id', 'X', 'Y'
        do i = 1, nno
            write(file_unit, '(1I4, 2F10.4)') i, nodes(i, :)
        end do
        write(file_unit, *)
        write(file_unit, *)

        ! Bars ************************************************************************************
        write(file_unit, '(A5)', advance='no') 'BARS '
        do i = 1, 95
            write(file_unit, '(A1)', advance='no') '/'
        end do
        write(file_unit, *)

        write(file_unit, '(1A4, 4A15)') 'id', 'Material', 'Section', 'Start Node', 'End Node'
        do i = 1, nel
            write(file_unit, '(1I4, 4I15)') i, bars(i, :)
        end do
        write(file_unit, *)
        write(file_unit, *)

        ! Boundaries ******************************************************************************
        write(file_unit, '(A7)', advance='no') 'Bounds '
        do i = 1, 93
            write(file_unit, '(A1)', advance='no') '/'
        end do
        write(file_unit, *)

        write(file_unit, '(1A4, *(A7))', advance='no') 'Id', 'node', 'Dx', 'Dy', 'Rz'
        write(file_unit, '(*(A20))') 'Dx', 'Dy', 'Rz'
        do i = 1, nccdesl
            write(file_unit, '(1I4, 1I7, 1L7)', advance='no') i, nnr(i)
            write(file_unit, '(*(L7))', advance='no') itydisp(i, :)
            write(file_unit, '(*(F20.4))', advance='no') disp(i, :)
            write(file_unit, *)
        end do
        write(file_unit, *)
        write(file_unit, *)

        ! Nodal Loads *****************************************************************************
        write(file_unit, '(A6)', advance='no') 'Loads '
        do i = 1, 94
            write(file_unit, '(A1)', advance='no') '/'
        end do
        write(file_unit, *)

        write(file_unit, '(1A4, 1A7, *(A10))') 'Id', 'node', 'Fx', 'Fy', 'Mz'
        do i = 1, nnc
            write(file_unit, '(1I4, 1I7, *(F10.2))') i, nnoc(i), ccno(i, :)
            write(file_unit, *)
        end do
        write(file_unit, *)
        write(file_unit, *)

        ! Local Stiffness Matrix ******************************************************************
        write(file_unit, '(A17)', advance='no') 'Stiffness Matrix '
        do i = 1, 83
            write(file_unit, '(A1)', advance='no') '/'
        end do
        write(file_unit, *)

        do id = 1, nel
            write(file_unit, '(1A13, 1I4)') 'Element ID: ', id
            do i = 1, 2 * ndofn
                do j = 1, 2 * ndofn
                    if (abs(kl(id, i, j)) < tolerance) then
                        write(file_unit, '(ES15.4)', advance='no') 0.0d0
                    else
                        write(file_unit, '(ES15.4)', advance='no') kl(id, i, j)
                    end if
                end do
                write(file_unit, *)
            end do
        end do
        write(file_unit, *)
        write(file_unit, *)

        ! Rot Matrix ******************************************************************************
        write(file_unit, '(A16)', advance='no') 'Rotation Matrix '
        do i = 1, 84
            write(file_unit, '(A1)', advance='no') '/'
        end do
        write(file_unit, *)
        do id = 1, nel
            write(file_unit, '(1A13, 1I4)') 'Element ID: ', id
            do i = 1, 2 * ndofn
                do j = 1, 2 * ndofn
                    if (abs(rot(id, i, j)) < tolerance) then
                        write(file_unit, '(F10.4)', advance='no') 0.0d0
                    else
                        write(file_unit, '(F10.4)', advance='no') rot(id, i, j)
                    end if
                end do
                write(file_unit, *)
            end do
        end do
        write(file_unit, *)

        ! Element Reactions ***********************************************************************
        write(file_unit, '(A18)', advance='no') 'Element Reactions '
        do i = 1, 82
            write(file_unit, '(A1)', advance='no') '/'
        end do
        write(file_unit, *)
        do id = 1, nel
            write(file_unit, '(1A13, 1I4)') 'Element ID: ', id
            write(file_unit, '(*(ES15.4))') El_reactions(id, :)

            if (id /= nel) then
                write(file_unit, *)
            end if
        end do
        write(file_unit, *)
        write(file_unit, *)

        ! Element Efforts *************************************************************************
        write(file_unit, '(A16)', advance='no') 'Element Efforts '
        do i = 1, 84
            write(file_unit, '(A1)', advance='no') '/'
        end do
        write(file_unit, *)
        do id = 1, nel
            write(file_unit, '(1A13, 1I4)') 'Element ID: ', id
            write(file_unit, '(*(ES15.4))') Eff(id, :)

            if (id /= nel) then
                write(file_unit, *)
            end if
        end do
        write(file_unit, *)
        write(file_unit, *)

        ! Global Matrix ***************************************************************************
        write(file_unit, '(A14)', advance='no') 'Global Matrix '
        do i = 1, 87
            write(file_unit, '(A1)', advance='no') '/'
        end do
        write(file_unit, *)
        do i = 1, nno*ndofn
            do j = 1, nno*ndofn
                if (abs(Kwb(i, j)) < tolerance) then
                    write(file_unit, '(ES10.2)', advance='no') 0.0d0
                else
                    write(file_unit, '(ES10.2)', advance='no') Kwb(i, j)
                end if
            end do
            write(file_unit, *)
        end do
        write(file_unit, *)
        write(file_unit, *)

        ! Load Vector *****************************************************************************
        write(file_unit, '(A11)', advance='no') 'Lad Vector '
        do i = 1, 89
            write(file_unit, '(A1)', advance='no') '/'
        end do
        write(file_unit, *)
        do i = 1, nno*ndofn
            if (abs(Fwb(i)) < tolerance) then
                write(file_unit, '(ES10.2)', advance='no') 0.0d0
            else
                write(file_unit, '(ES10.2)', advance='no') Fwb(i)
            end if
        end do
        write(file_unit, *)
        write(file_unit, *)
        write(file_unit, *)

        ! Displacements ***************************************************************************
        write(file_unit, '(A14)', advance='no') 'Displacements '
        do i = 1, 86
            write(file_unit, '(A1)', advance='no') '/'
        end do
        write(file_unit, *)
        do i = 1, nno*ndofn
            if (abs(D(i)) < tolerance) then
                write(file_unit, '(ES13.4)', advance='no') 0.0d0
            else
                write(file_unit, '(ES13.4)', advance='no') D(i)
            end if
        end do
        write(file_unit, *)
        write(file_unit, *)
        write(file_unit, *)

        ! Reactions *******************************************************************************
        write(file_unit, '(A10)', advance='no') 'Reactions '
        do i = 1, 90
            write(file_unit, '(A1)', advance='no') '/'
        end do
        write(file_unit, *)
        do i = 1, nno*ndofn
            if (abs(reactions(i)) < tolerance) then
                write(file_unit, '(ES13.4)', advance='no') 0.0d0
            else
                write(file_unit, '(ES13.4)', advance='no') reactions(i)
            end if
        end do
        write(file_unit, *)
        write(file_unit, *)
        write(file_unit, *)
    end subroutine save_results
end program main
