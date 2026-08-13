module Elementary
    implicit none

contains
    recursive function fact(number) result(answer)
        integer, intent(in) :: number
        integer :: answer

        if (number >= 1) then
            answer = number * fact(number - 1)
        else
            answer = 1
        end if
    end function
end module
