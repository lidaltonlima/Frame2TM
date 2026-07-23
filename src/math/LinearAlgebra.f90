module LinearAlgebra
    implicit none
    private
    public :: cross
contains
    pure function cross(a, b) result(c)
        real(8), dimension(3), intent(in) :: a, b
        real(8), dimension(3) :: c

        c(1) = (a(2) * b(3)) - (a(3) * b(2))
        c(2) = (a(3) * b(1)) - (a(1) * b(3))
        c(3) = (a(1) * b(2)) - (a(2) * b(1))
    end function cross
end module LinearAlgebra
