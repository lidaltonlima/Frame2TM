program main
    use StructureData, only: get_structure_data, save_data_file, save_results
    use Stiffness, only: calc_kl, calc_K
    use Rotation, only: calc_Rot
    use Boundaries, only: add_boundaries
    use Loads, only: calc_F
    use Reactions, only: get_reactions, get_el_reactions
    use Efforts, only: get_efforts
    implicit none

    ! =============================================================================================
    ! Vars statement
    ! =============================================================================================
    ! Structure data ******************************************************************************
    real(8), parameter :: tolerance = 1.0d-15  ! tolerance to zero number

    ! Structure data ******************************************************************************
    integer :: nno  ! Number of nodes
    integer :: nel  ! Number of elements
    integer :: ndofn  ! Number of degrees of freedom per node
    integer :: ntm  ! Number of materials
    integer :: nts  ! Number of sections
    integer :: nnc  ! Number of nodes with point load
    integer :: nsa  ! Number of sample sections
    character(2) :: theory ! Theory used

    real(8), allocatable :: materials(:, :)
    real(8), allocatable :: sections(:, :, :)
    real(8), allocatable :: nodes(:, :)
    integer, allocatable :: bars(:, :)

    integer :: nccdesl  ! Number of boundaries condition
    integer, allocatable :: nnr(:)  ! index of bound node
    logical, allocatable :: itydisp(:, :) ! type of bound
    real(8), allocatable :: disp(:, :)  ! displacement value

    integer, allocatable :: nnoc(:)  ! index of node with load
    real(8), allocatable :: ccno(:, :)  ! value of point load in node

    ! Calculate data ******************************************************************************
    real(8), allocatable :: kl(:, :, :)  ! Stiffness matrix kl(element_id, i, j)
    real(8), allocatable :: rot(:, :, :)  ! Matrix of rotation
    real(8), allocatable :: K(:, :)  ! Global stiffness global
    real(8), allocatable :: F(:)  ! Vector of loads
    real(8), allocatable :: D(:)  ! Displacements
    real(8), allocatable :: reactions(:)  ! reactions
    real(8), allocatable :: El_reactions(:, :)  ! elements reactions
    real(8), allocatable :: Eff(:, :)  ! elements efforts

    ! Controls ************************************************************************************
    integer :: i ! Indexes

    ! Aux *****************************************************************************************
    real(8), allocatable :: K_aux(:, :)  ! stiffness matrix without boundary condiciones
    real(8), allocatable :: F_aux(:)
    integer :: dir  ! direction of degree of freedom
    integer :: i_dir  ! index of direction of degree of freedom
    integer :: dim  ! Dimension of matrices and vectors
    integer :: info  ! status of operation (dposv - Lapack)

    ! External ************************************************************************************
    external :: dposv  ! solve symmetric positive defined matrix system (Lapack)

    ! =============================================================================================
    ! Initialize
    ! =============================================================================================
    ! Get data from files
    call get_structure_data(nno, nel, ndofn, ntm, nts, nccdesl, nnr, nnc, nsa, theory, &
        itydisp, disp, nnoc, ccno, &
        materials, sections, nodes, bars)

    dim = nno * ndofn

    ! Allocation
    allocate(K_aux(dim, dim))
    allocate(F_aux(dim))
    allocate(D(dim))
    allocate(reactions(dim))
    allocate(El_reactions(nel, 2*ndofn))
    allocate(Eff(nel, 2*ndofn))

    ! =============================================================================================
    ! Calculation
    ! =============================================================================================
    call calc_kl(kl, nel, nsa, ndofn, theory, materials, sections, nodes, bars)
    call calc_Rot(rot, nel, ndofn, nodes, bars)
    call calc_K(nno, nel, ndofn, bars, kl, rot, K)
    call calc_F(nno, ndofn, nnc, nnoc, ccno, nccdesl, nnr, itydisp, disp, K, F)

    K_aux = K
    F_aux = F
    call add_boundaries(nccdesl, nnr, itydisp, disp, ndofn, K_aux, F_aux)

    D = F_aux
    call dposv('U', dim, 1, K_aux, dim, D, dim, info)

    call get_reactions(reactions, K, D, F, nccdesl, nnr, itydisp, ndofn, nno)

    ! Sum the previus displacement
    do i = 1, nccdesl
        do dir = 1, ndofn
            i_dir = (ndofn * (nnr(i) - 1)) + dir

            if (itydisp(i, dir)) then
                D(i_dir) = disp(i, dir)
            end if
        end do
    end do

    call get_el_reactions(El_reactions, nel, ndofn, bars, kl, rot, D)
    call get_efforts(Eff, El_reactions, bars, nodes, nel)

    ! =============================================================================================
    ! Show and save values
    ! =============================================================================================
    call save_results(tolerance, nno, nel, ndofn, ntm, nts, nnc, nsa, theory, &
        materials, sections, nodes, bars, nccdesl, nnr, itydisp, disp, nnoc, ccno, &
        kl, rot, reactions, El_reactions, Eff, K, F, D)

contains
end program main
