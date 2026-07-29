program main
    use StructureData, only: get_structure_data, save_data_file, save_results
    use Stiffness, only: calc_kl, calc_K
    use Rotation, only: calc_Rot
    use Displacements, only: calc_D
    use Loads, only: calc_F
    use Reactions, only: calc_reactions, calc_el_reactions
    use Efforts, only: calc_efforts
    implicit none

    ! =============================================================================================
    ! Vars statement
    ! =============================================================================================
    ! Structure data ******************************************************************************
    real(8), parameter :: tolerance = 1.0d-15  ! tolerance to zero number

    ! Structure data ******************************************************************************
    integer :: nno  ! number of nodes
    integer :: nel  ! number of elements
    integer :: ndofn  ! number of degrees of freedom per node
    integer :: ntm  ! number of materials
    integer :: nts  ! number of sections
    integer :: nnc  ! number of nodes with point load
    integer :: nsa  ! number of sample sections
    character(2) :: theory ! Theory used

    real(8), allocatable :: materials(:, :)
    real(8), allocatable :: sections(:, :, :)
    real(8), allocatable :: nodes(:, :)
    integer, allocatable :: bars(:, :)

    integer :: nccdesl  ! number of boundaries condition
    integer, allocatable :: nnr(:)  ! index of bound node
    logical, allocatable :: itydisp(:, :) ! type of bound
    real(8), allocatable :: disp(:, :)  ! displacement value

    integer, allocatable :: nnoc(:)  ! index of node with load
    real(8), allocatable :: ccno(:, :)  ! value of point load in node

    ! Calculate data ******************************************************************************
    real(8), allocatable :: kl(:, :, :)  ! stiffness matrix kl(element_id, i, j)
    real(8), allocatable :: rot(:, :, :)  ! matrix of rotation
    real(8), allocatable :: K(:, :)  ! global stiffness global
    real(8), allocatable :: F(:)  ! vector of loads
    real(8), allocatable :: D(:)  ! displacements
    real(8), allocatable :: reactions(:)  ! reactions
    real(8), allocatable :: El_reactions(:, :)  ! elements reactions
    real(8), allocatable :: Eff(:, :)  ! elements efforts

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

    allocate(K(dim, dim))
    allocate(D(dim))
    allocate(F(dim))
    allocate(reactions(dim))
    allocate(El_reactions(nel, el_dim))
    allocate(Eff(nel, el_dim))
    allocate(rot(nel, el_dim, el_dim))
    allocate(kl(nel, el_dim, el_dim))

    ! =============================================================================================
    ! Calculation
    ! =============================================================================================
    call calc_kl(kl, nel, nsa, theory, materials, sections, nodes, bars)
    call calc_Rot(rot, nel, nodes, bars)
    call calc_K(K, nno, nel, ndofn, bars, kl, rot)
    call calc_F(nno, ndofn, nnc, nnoc, ccno, nccdesl, nnr, itydisp, disp, K, F)
    call calc_D(D, K, F, nccdesl, nnr, itydisp, ndofn, dim, disp)
    call calc_reactions(reactions, K, D, F, nccdesl, nnr, itydisp, disp, ndofn, dim)
    call calc_el_reactions(El_reactions, nel, ndofn, bars, kl, rot, D, el_dim)
    call calc_efforts(Eff, El_reactions, bars, nodes, nel)

    ! =============================================================================================
    ! Show and save values
    ! =============================================================================================
    call save_results(tolerance, nno, nel, ndofn, ntm, nts, nnc, nsa, theory, &
        materials, sections, nodes, bars, nccdesl, nnr, itydisp, disp, nnoc, ccno, &
        kl, rot, reactions, El_reactions, Eff, K, F, D)
end program main
