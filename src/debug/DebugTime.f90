module DebugTime
    implicit none
    save

    integer :: count_i  ! initial count
    integer :: count_f  ! end count
    integer :: count_r  ! rate count
    real :: time  ! time in seconds

contains

    subroutine time_count_start
        ! =========================================================================================
        ! Start count time of execution code.
        ! =========================================================================================

        call system_clock(count_i, count_r)
    end subroutine time_count_start

    subroutine time_count_update
        ! =========================================================================================
        ! Update the time var.
        ! =========================================================================================

        call system_clock(count_f)

        time = real(count_f - count_i) / real(count_r)
    end subroutine time_count_update

    subroutine time_count_show
        ! =========================================================================================
        ! Update and show the time var.
        ! =========================================================================================

        call system_clock(count_f)

        time = real(count_f - count_i) / real(count_r)

        write(*, '(ES10.4)') time
    end subroutine time_count_show

    subroutine time_count_get(time_res)
        ! =========================================================================================
        ! Update and get the time into time_res var.
        ! =========================================================================================
        real, intent(out) :: time_res

        call system_clock(count_f)

        time = real(count_f - count_i) / real(count_r)
        time_res = real(count_f - count_i) / real(count_r)
    end subroutine time_count_get

end module DebugTime
