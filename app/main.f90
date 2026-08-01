program main
    use iso_fortran_env, only: real64

    use StructureData
    use StructureFiles, only: get_structure_data, save_results
    use Stiffness, only: EKl, calc_Kg
    use Displacements, only: calc_Dg
    use Loads, only: calc_Fg
    use CalcReactions, only: calc_Rg, calc_ERl
    use Efforts, only: calc_EEl

    implicit none

    ! =============================================================================================
    ! Initialization
    ! =============================================================================================
    ! Get data from files
    call get_structure_data

    dim = nno * ndofn
    E_dim = 2 * ndofn

    allocate(Dg(dim))
    allocate(Fg(dim))
    allocate(Rg(dim))
    allocate(ERl(nel, E_dim))
    allocate(EEl(nel, E_dim))

    ! =============================================================================================
    ! Calculation
    ! =============================================================================================
    call calc_Kg
    call calc_Fg
    call calc_Dg
    call calc_Rg
    call calc_ERl
    call calc_EEl

    ! =============================================================================================
    ! Show and save values
    ! =============================================================================================
    call save_results
end program main
