module StructureData
    use iso_fortran_env, only: real64
    implicit none
    private
    public :: get_structure_data, save_data_file, save_results

contains
    subroutine open_data_file(file_name,  file_unit)
        ! Open the file to get data

        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! I/O
        character(*), intent(in) :: file_name  ! File name
        integer, intent(out) :: file_unit  ! Unit to file

        ! Parameters
        character(7), parameter :: data_folder = './data/'  ! Data file location
        character(4), parameter :: file_extension = '.dat'  ! Data file extension

        ! aux
        integer :: file_stat  ! State of file
        character(:), allocatable :: file_error  ! Message to file error
        character(:), allocatable :: file_path  ! Complete path to file

        ! =========================================================================================
        ! Process
        ! =========================================================================================
        ! Open ************************************************************************************
        file_path = data_folder // trim(file_name) // file_extension
        open(newUnit=file_unit, &
            file=file_path, &
            status='old', &
            action='read', &
            ioStat=file_stat, &
            ioMsg=file_error)


        ! Error ***********************************************************************************
        if ( file_stat /= 0) then
            print *, 'State: ', file_stat
            print *, 'MSG: ', file_error
            error stop 'File open'
        end if
    end subroutine open_data_file

    subroutine save_data_file(file_name,  file_unit)
        ! Save data in file

        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! I/O
        character(*), intent(in) :: file_name  ! File name
        integer, intent(out) :: file_unit  ! Unit to file
        character(11), parameter :: data_folder = './data/res/'  ! Data file location
        character(4), parameter :: file_extension = '.dat'  ! Data file extension
        integer :: file_stat  ! State of file
        character(30) :: file_error  ! Message to file error
        character(30) :: file_path  ! Complete path to file

        ! =========================================================================================
        ! Process
        ! =========================================================================================
        ! Open ************************************************************************************
        file_path = data_folder // trim(file_name) // file_extension
        open(newUnit=file_unit, &
            file=file_path, &
            status='unknown', &
            action='write', &
            ioStat=file_stat, &
            ioMsg=file_error)


        ! Error ***********************************************************************************
        if ( file_stat /= 0) then
            print *, 'State: ', file_stat
            print *, 'MSG: ', file_error
            error stop 'File open'
        end if
    end subroutine save_data_file

    subroutine get_structure_data(nno, nel, ndofn, ntm, nts, nccdesl, nnr, nnc, nsa, theory, &
        itydisp, disp, nnoc, ccno, &
        materials, sections, nodes, bars)
        ! Get the data structure

        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        ! I/O *************************************************************************************
        integer, intent(out) :: nno  ! Number of nodes
        integer, intent(out) :: nel  ! Number of elements
        integer, intent(out) :: ndofn  ! Number of degrees of freedom per node
        integer, intent(out) :: ntm  ! Number of materials
        integer, intent(out) :: nts  ! Number of sections
        integer, intent(out) :: nccdesl  ! Number of boundaries condition
        integer, intent(out) :: nnc  ! Number of nodes with point load
        integer, intent(out) :: nsa  ! Number of sample sections
        character(2), intent(out) :: theory ! Theory used (EB: Euler-Bernoulli or TM: Timoshenko)

        real(real64), intent(out), allocatable :: materials(:, :)
        real(real64), intent(out), allocatable :: sections(:, :, :)
        real(real64), intent(out), allocatable :: nodes(:, :)
        integer, intent(out), allocatable :: bars(:, :)

        integer, intent(out), allocatable :: nnr(:)  ! index of bound node
        logical, intent(out), allocatable :: itydisp(:, :) ! type of bound
        real(real64), intent(out), allocatable :: disp(:, :)  ! displacement value

        integer, allocatable, intent(out) :: nnoc(:)  ! index of node with load
        real(real64), allocatable, intent(out) :: ccno(:, :)  ! value of point load in node

        ! Controls ********************************************************************************
        integer :: file_unit  ! Unit to file
        integer :: read_stat  ! State of current read
        integer :: id  ! Object ID
        character(20) :: line_label

        ! Temp ************************************************************************************
        integer :: temp_int

        ! =========================================================================================
        ! CONTROLS
        ! =========================================================================================
        ! Open ************************************************************************************
        call open_data_file('controls', file_unit)

        ! Read ************************************************************************************
        CONTROLS: do
            read(file_unit, *, ioStat=read_stat) line_label, temp_int

            if (read_stat == 0) then
                select case (line_label)
                    case ('nno')
                        nno = temp_int
                    case ('nel')
                        nel = temp_int
                    case ('ndofn')
                        ndofn = temp_int
                    case ('ntm')
                        ntm = temp_int
                    case ('nts')
                        nts = temp_int
                    case ('nccdesl')
                        nccdesl = temp_int
                    case ('nnc')
                        nnc = temp_int
                    case ('nsa')
                        nsa = temp_int
                    case ('theory')
                        if (temp_int == 0) then
                            theory = 'OB'
                        else
                            theory = 'TM'
                        end if
                end select
            else if (read_stat == -1) then
                exit CONTROLS
            else
                write(*, *) 'Read stat:', read_stat
                error stop 'Error in CONTROLS read'
            end if
        end do CONTROLS

        ! Close ***********************************************************************************
        close(file_unit)

        ! =========================================================================================
        ! MATERIALS
        ! =========================================================================================
        ! Allocation ******************************************************************************
        allocate(materials(ntm, 3))

        ! Open ************************************************************************************
        call open_data_file('materials', file_unit)

        ! Read ************************************************************************************
        read(file_unit, *) ! titles line
        do id = 1, ntm
            read(file_unit, *) materials(id, :)
        end do

        ! Close ***********************************************************************************
        close(file_unit)

        ! =========================================================================================
        ! SECTIONS
        ! =========================================================================================
        ! Allocation ******************************************************************************
        allocate(sections(nts, 3, nsa))

        ! Open ************************************************************************************
        call open_data_file('sections', file_unit)

        ! Read ************************************************************************************
        read(file_unit, *) ! titles line
        do id = 1, nts
            read(file_unit, *) sections(id, 1, :), sections(id, 2, :), sections(id, 3, :)
        end do

        ! Close ***********************************************************************************
        close(file_unit)

        ! =========================================================================================
        ! NODES
        ! =========================================================================================
        ! Allocation ******************************************************************************
        allocate(nodes(nno, 2))

        ! Open ************************************************************************************
        call open_data_file('nodes', file_unit)

        ! Read ************************************************************************************
        read(file_unit, *) ! titles line
        do id = 1, nno
            read(file_unit, *) nodes(id, 1), nodes(id, 2)
        end do

        ! Close ***********************************************************************************
        close(file_unit)

        ! =========================================================================================
        ! BARS
        ! =========================================================================================
        ! Allocation ******************************************************************************
        allocate(bars(nel, 4))

        ! Open ************************************************************************************
        call open_data_file('bars', file_unit)

        ! Read ************************************************************************************
        read(file_unit, *) ! titles line
        do id = 1, nel
            read(file_unit, *) bars(id, 1), bars(id, 2), bars(id, 3), bars(id, 4)
        end do

        ! Close ***********************************************************************************
        close(file_unit)

        ! =========================================================================================
        ! Bound
        ! =========================================================================================
        ! Allocation ******************************************************************************
        allocate(nnr(nccdesl))
        allocate(itydisp(nccdesl, ndofn))
        allocate(disp(nccdesl, ndofn))

        ! Open ************************************************************************************
        call open_data_file('boundaries', file_unit)

        ! Read ************************************************************************************
        read(file_unit, *) ! titles line
        do id = 1, nccdesl
            read(file_unit, *) nnr(id), itydisp(id, :), disp(id, :)
        end do

        ! =========================================================================================
        ! Loads
        ! =========================================================================================
        ! Allocation ******************************************************************************
        allocate(nnoc(nnc))
        allocate(ccno(nnc, ndofn))

        ! Open ************************************************************************************
        call open_data_file('node_loads', file_unit)

        ! Read ************************************************************************************
        read(file_unit, *) ! titles line
        do id = 1, nnc
            read(file_unit, *) nnoc(id), ccno(id, :)
        end do

        ! Close ***********************************************************************************
        close(file_unit)
    end subroutine get_structure_data

    subroutine save_results(tolerance, nno, nel, ndofn, ntm, nts, nnc, nsa, theory, &
        materials, sections, nodes, bars, &
        nccdesl, nnr, itydisp, disp, nnoc, ccno, &
        kl, rot, reactions, El_reactions, Eff, Kwb, Fwb, D)
        real(real64), intent(in) :: tolerance
        ! =========================================================================================
        ! Vars statement
        ! =========================================================================================
        integer, intent(in) :: nno  ! Number of nodes
        integer, intent(in) :: nel  ! Number of elements
        integer, intent(in) :: ndofn  ! Number of degrees of freedom per node
        integer, intent(in) :: ntm  ! Number of materials
        integer, intent(in) :: nts  ! Number of sections
        integer, intent(in) :: nnc  ! Number of nodes with point load
        integer, intent(in) :: nsa  ! Number of sample sections
        character(2), intent(in) :: theory ! Theory used

        real(real64), intent(in) :: materials(:, :)
        real(real64), intent(in) :: sections(:, :, :)
        real(real64), intent(in) :: nodes(:, :)
        integer, intent(in) :: bars(:, :)

        integer, intent(in) :: nccdesl  ! Number of boundaries condition
        integer, intent(in) :: nnr(:)  ! index of bound node
        logical, intent(in) :: itydisp(:, :) ! type of bound
        real(real64), intent(in) :: disp(:, :)  ! displacement value

        integer, intent(in) :: nnoc(:)  ! index of node with load
        real(real64), intent(in) :: ccno(:, :)  ! value of point load in node

        real(real64), intent(in) :: kl(:, :, :)  ! Stiffness matrix
        real(real64), intent(in) :: rot(:, :, :)  ! Matrix of rotation

        real(real64), intent(in) :: reactions(:)  ! reactions
        real(real64), intent(in) :: El_reactions(:, :)  ! elements reactions
        real(real64), intent(in) :: Eff(:, :)  ! elements efforts

        real(real64), intent(in) :: Kwb(:, :)  ! stiffness matrix without boundary condiciones
        real(real64), intent(in) :: Fwb(:)
        real(real64), intent(in) :: D(:)  ! Displacements

        ! File ************************************************************************************
        integer :: file_unit  ! Unit to file

        ! Aux *************************************************************************************
        character(10) :: int2str1
        character(10) :: int2str2
        integer :: i, j, id ! index

        call save_data_file('results', file_unit)

        ! =========================================================================================
        ! Processes
        ! =========================================================================================
        ! Title *******************************************************************************
        100 format(1A6, ':', 1I10)
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
end module StructureData
