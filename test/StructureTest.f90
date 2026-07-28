program StructureTest
    use iso_fortran_env, only: error_unit
    implicit none

    ! =============================================================================================
    ! Var statements
    ! =============================================================================================
    ! Control parameters
    logical, parameter :: detail = .false.

    ! General
    integer :: file_unit_t, file_unit_r  ! number of unit file
    character(300) :: line_t, line_r  ! value of line readd
    integer :: ios  ! state of read


    ! Aux
    integer :: line_number  ! number of current line
    integer :: qtd_lines  ! quantity of lines in the right document
    character :: line  ! text in the line
    integer :: i  ! index for loops

    ! Header **************************************************************************************
    do i = 1, 100
        write(*, '(1A)', advance='no') '='
    end do
    write(*, *)
    write(*, '(1A33)') 'Structure Test - To Compare files'
    do i = 1, 100
        write(*, '(1A)', advance='no') '='
    end do
    write(*, *)

    ! Get number of lines *************************************************************************
    open(newUnit=file_unit_r, &
        file='./test/data/results.dat', &
        status='old', &
        action='read')

    do
        read(file_unit_r, '(A)', ioStat=ios) line
        if (ios /= 0) exit   ! sai do loop ao encontrar EOF ou erro
        qtd_lines = qtd_lines + 1
    end do

    close(file_unit_r)

    ! Open ****************************************************************************************
    open(newUnit=file_unit_t, &
        file='./data/res/results.dat', &
        status='old', &
        action='read')
    open(newUnit=file_unit_r, &
        file='./test/data/results.dat', &
        status='old', &
        action='read')

    ! Compare *************************************************************************************
    do line_number = 1, qtd_lines
        read(file_unit_t, '(1A300)') line_t
        read(file_unit_r, '(1A300)') line_r

        call show_test_value
    end do

    ! Close ***************************************************************************************
    close(file_unit_t)
    close(file_unit_r)
    write(*, *)
    write(*, *) 'Tests finished'

contains
    subroutine show_details
        if (line_t /= line_r) then
            write(error_unit, '(1A17, 1I4)') 'Wrong values in: ', line_number
            write(error_unit, *)
            write(error_unit, *) 'Expected:'
            write(error_unit, *) line_r
            write(error_unit, *)
            write(error_unit, *) 'Obtained:'
            write(error_unit, *) line_t
        end if
    end subroutine

    subroutine show_test_value
        if (detail) then
            call show_details
        else
            if (line_t == line_r) then
                write(*, '(1A1)', advance='no') '.'
            else
                write(*, '(1A1)', advance='no') 'F'
            end if
        end if
    end subroutine
end program StructureTest
