module StructureData
    use OutputTerminal, only: print_table, print_structure_data
    implicit none

    private

    public :: get_structure_data

contains
    subroutine open_data_file(file_name,  file_unit)
        ! File vars
        character(*), intent(in) :: file_name  ! File name
        integer, intent(out) :: file_unit  ! Unit to file
        character(7), parameter :: data_folder = './data/'  ! Data file location
        character(4), parameter :: file_extension = '.dat'  ! Data file extension
        integer :: file_stat  ! State of file
        character(30) :: file_error  ! Message to file error
        character(30) :: file_path  ! Complete path to file


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

    subroutine get_structure_data(nno, nel, ndofn, ntm, nts, theory, materials, sections, nodes, bars)
        ! PURPOSE: Get da data structure

        ! I/O vars
        integer, intent(out) :: nno  ! Number of nodes
        integer, intent(out) :: nel  ! Number of elements
        integer, intent(out) :: ndofn  ! Number of degrees of freedom per node
        integer, intent(out) :: ntm  ! Number of materials
        integer, intent(out) :: nts  ! Number of sections

        real(8), intent(out), allocatable :: materials(:, :)
        real(8), intent(out), allocatable :: sections(:, :)
        real(8), intent(out), allocatable :: nodes(:, :)
        integer, intent(out), allocatable :: bars(:, :)

        character(2), intent(out) :: theory ! Theory used

        ! File vars
        character(7), parameter :: data_folder = './data/'  ! Data file location
        character(4), parameter :: file_extension = '.dat'  ! Data file extension
        integer :: file_unit  ! Unit to file

        ! Read vars
        integer :: read_stat  ! State of current read

        ! Control vars
        integer :: id  ! Object ID
        character(20) :: line_label

        ! Temp vars
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
        allocate(materials(ntm, 2))

        ! Open ************************************************************************************
        call open_data_file('materials', file_unit)

        ! Read ************************************************************************************
        read(file_unit, *) ! titles line
        do id = 1, ntm
            read(file_unit, *) materials(id, 1), materials(id, 2)
        end do

        ! Close ***********************************************************************************
        close(file_unit)

        ! =========================================================================================
        ! SECTIONS
        ! =========================================================================================
        ! Allocation ******************************************************************************
        allocate(sections(nts, 2))

        ! Open ************************************************************************************
        call open_data_file('sections', file_unit)

        ! Read ************************************************************************************
        read(file_unit, *) ! titles line
        do id = 1, nts
            read(file_unit, *) sections(id, 1), sections(id, 2)
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
    end subroutine get_structure_data
end module StructureData
