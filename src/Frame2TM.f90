module Frame2TM
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, Frame2TM!"
  end subroutine say_hello
end module Frame2TM
