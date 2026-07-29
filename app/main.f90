program main
    use iso_fortran_env, only: real64
    use StructureData, only: get_structure_data, save_results
    use Stiffness, only: calc_kl, calc_kc
    use Rotation, only: calc_rot_mat
    use Displacements, only: calc_dc
    use Loads, only: calc_fc
    use Reactions, only: calc_reactions, calc_el_reactions
    use Efforts, only: calc_efforts
    implicit none

    ! =============================================================================================
    ! Vars statement
    ! =============================================================================================
    ! Structure data ******************************************************************************
    real(real64), parameter :: disp_tol = 1.0d-15  ! tolerance to zero number
    real(real64), parameter :: force_tol = 1.0d-5  ! tolerance to zero number

    ! Structure data ******************************************************************************
    integer :: nno  ! number of nodes
    integer :: nel  ! number of elements
    integer :: ndofn  ! number of degrees of freedom per node
    integer :: ntm  ! number of materials
    integer :: nts  ! number of sections
    integer :: nnc  ! number of nodes with point load
    integer :: nsa  ! number of sample sections
    character(2) :: theory ! Theory used

    real(real64), allocatable :: materials(:, :)
    real(real64), allocatable :: sections(:, :, :)
    real(real64), allocatable :: nodes(:, :)
    integer, allocatable :: bars(:, :)

    integer :: nccdesl  ! number of boundaries condition
    integer, allocatable :: nnr(:)  ! index of bound node
    logical, allocatable :: itydisp(:, :) ! type of bound
    real(real64), allocatable :: disp(:, :)  ! displacement value

    integer, allocatable :: nnoc(:)  ! index of node with load
    real(real64), allocatable :: ccno(:, :)  ! value of point load in node

    ! Calculate data ******************************************************************************
    real(real64), allocatable :: kl(:, :, :)  ! stiffness matrix kl(element_id, i, j)
    real(real64), allocatable :: rot_mat(:, :, :)  ! matrix of rotation
    real(real64), allocatable :: kc(:, :)  ! global stiffness global
    real(real64), allocatable :: fc(:)  ! vector of loads
    real(real64), allocatable :: dc(:)  ! displacements
    real(real64), allocatable :: reactions(:)  ! reactions
    real(real64), allocatable :: el_reactions(:, :)  ! elements reactions
    real(real64), allocatable :: eff(:, :)  ! elements efforts

    ! Aux *****************************************************************************************
    integer :: dim  ! dimension of matrices and vectors
    integer :: el_dim  ! dimension of matrices and vectors

    ! =============================================================================================
    ! Initialization
    ! =============================================================================================
    ! Get data from files
    call get_structure_data(nno, nel, ndofn, ntm, nts, nccdesl, nnr, nnc, nsa, theory, &
        itydisp, disp, nnoc, ccno, &
        materials, sections, nodes, bars)

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
    call calc_kl(kl, nel, nsa, theory, materials, sections, nodes, bars)
    call calc_rot_mat(rot_mat, nel, nodes, bars)
    call calc_kc(kc, nno, nel, ndofn, bars, kl, rot_mat)
    call calc_fc(nno, ndofn, nnc, nnoc, ccno, nccdesl, nnr, itydisp, disp, kc, fc)
    call calc_dc(dc, kc, fc, nccdesl, nnr, itydisp, ndofn, dim, disp)
    call calc_reactions(reactions, kc, dc, fc, nccdesl, nnr, itydisp, disp, ndofn, dim)
    call calc_el_reactions(el_reactions, nel, ndofn, bars, kl, rot_mat, dc, el_dim)
    call calc_efforts(eff, el_reactions, bars, nodes, nel)

    ! =============================================================================================
    ! Show and save values
    ! =============================================================================================
    call save_results(disp_tol, force_tol, nno, nel, ndofn, ntm, nts, nnc, nsa, theory, &
        materials, sections, nodes, bars, nccdesl, nnr, itydisp, disp, nnoc, ccno, &
        kl, rot_mat, reactions, el_reactions, eff, kc, fc, dc)
end program main
