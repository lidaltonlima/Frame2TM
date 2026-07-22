program main
    use StructureData, only: get_structure_data
    use Stiffness, only: get_kl
    use OutputTerminal, only: print_structure_data
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

    real(8), allocatable :: materials(:, :)
    real(8), allocatable :: sections(:, :)
    real(8), allocatable :: nodes(:, :)
    integer, allocatable :: bars(:, :)

    ! Calculate data
    real(8), allocatable :: kl(:, :, :)  ! Stiffness matrix kl(element_id, i, j)

    ! Controls
    integer :: i, j  ! Indexes
    integer :: id   ! Index id
    logical, parameter :: debug = .true.

    ! =============================================================================================
    ! Calculation
    ! =============================================================================================
    call get_structure_data(nno, nel, ndofn, ntm, nts, materials, sections, nodes, bars)

    kl = get_kl(nel, ndofn, materials, sections, nodes, bars)

    ! =============================================================================================
    ! Debug
    ! =============================================================================================
    if ( debug ) then
        call print_structure_data(nno, nel, ndofn, ntm, nts, &
            materials, sections, nodes, bars)
        do id = 1, nel
            write(*, *) 'Element ID: ', id
            do i = 1, 2 * ndofn
                do j = 1, 2 * ndofn
                    write(*, '(ES15.4)', advance='no') kl(id, i, j)
                end do
                print *
            end do
        end do
    end if
end program main
