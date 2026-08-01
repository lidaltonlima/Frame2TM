program main
    use iso_fortran_env, only: real64

    use StructureData
    use StructureFiles, only: get_structure_data, save_results
    use Stiffness, only: calc_kl, calc_kc
    use Rotation, only: calc_rot_mat
    use Displacements, only: calc_dc
    use Loads, only: calc_fc
    use CalcReactions, only: calc_reactions, calc_el_reactions
    use Efforts, only: calc_efforts

    implicit none

    ! =============================================================================================
    ! Initialization
    ! =============================================================================================
    ! Get data from files
    call get_structure_data

    dim = nno * ndofn
    el_dim = 2 * ndofn

    allocate(dc(dim))
    allocate(fc(dim))
    allocate(reactions(dim))
    allocate(el_reactions(nel, el_dim))
    allocate(eff(nel, el_dim))
    allocate(rot_mat(nel, el_dim, el_dim))
    allocate(kl(nel, el_dim, el_dim))

    ! =============================================================================================
    ! Calculation
    ! =============================================================================================
    call calc_kl
    call calc_rot_mat
    call calc_kc
    call calc_fc
    call calc_dc
    call calc_reactions
    call calc_el_reactions
    call calc_efforts

    ! =============================================================================================
    ! Show and save values
    ! =============================================================================================
    call save_results
end program main
