module TypeConversion
    use iso_fortran_env, only: real64
    implicit none

contains
    pure function char2int(text) result(number)
        character(*), intent(in) :: text
        integer :: number

        read(text, *) number
    end function

    pure function char2real(text) result(number)
        character(*), intent(in) :: text
        real :: number

        read(text, *) number
    end function

    pure function char2real64(text) result(number)
        character(*), intent(in) :: text
        real(real64) :: number

        read(text, *) number
    end function

    pure function int2char(number) result(text)
        integer, intent(in) :: number
        character(:), allocatable :: text
        character(20) :: temp

        write(temp, '(I0)') number
        text = trim(temp)
    end function

    pure function real2char(number, nDec) result(text)
        real, intent(in) :: number
        integer, intent(in), optional :: nDec
        character(:), allocatable :: text
        character(64) :: buffer
        character(32) :: fmt


        if (present(nDec)) then
            write(fmt, '( "(ES0.", I0, ")" )') nDec
        else
            fmt = '(ES0.15)'
        end if

        write(buffer, fmt) number

        text = trim(buffer)
    end function real2char

end module TypeConversion
