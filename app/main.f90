program main
    use StructureData, only: get_structure_data, save_data_file, save_results
    use Stiffness, only: get_kl, get_K
    use Rotation, only: getRotMat
    use Boundaries, only: add_boundaries
    use Loads, only: get_F
    use Reactions, only: get_reactions, get_el_reactions
    use Efforts, only: get_efforts
    implicit none

    ! =============================================================================================
    ! Vars statement
    ! =============================================================================================
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
    real(8), allocatable :: Kwb(:, :)  ! stiffness matrix without boundary condiciones
    real(8), allocatable :: Fwb(:)
    integer :: dir
    integer :: i_dir

    ! =============================================================================================
    ! Calculation
    ! =============================================================================================
    call get_structure_data(nno, nel, ndofn, ntm, nts, nccdesl, nnr, nnc, nsa, theory, &
        itydisp, disp, nnoc, ccno, &
        materials, sections, nodes, bars)

    allocate(Kwb(nno*ndofn, nno*ndofn))
    allocate(Fwb(nno*ndofn))
    allocate(D(nno*ndofn))
    allocate(reactions(nno*ndofn))
    allocate(El_reactions(nel, 2*ndofn))
    allocate(Eff(nel, 2*ndofn))

    kl = get_kl(nel, nsa, ndofn, theory, materials, sections, nodes, bars)
    rot = getRotMat(nel, ndofn, nodes, bars)
    call get_K(nno, nel, ndofn, bars, kl, rot, K)

    call get_F(nno, ndofn, nnc, nnoc, ccno, nccdesl, nnr, itydisp, disp, K, F)

    Kwb = K
    Fwb = F

    call add_boundaries(nccdesl, nnr, itydisp, disp, ndofn, K, F)

    D = F

    call solver()
    call get_reactions(reactions, kwb, D, Fwb, nccdesl, nnr, itydisp, ndofn, nno)

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
    call save_results(1.0d-15, nno, nel, ndofn, ntm, nts, nnc, nsa, theory, &
        materials, sections, nodes, bars, nccdesl, nnr, itydisp, disp, nnoc, ccno, &
        kl, rot, reactions, El_reactions, Eff, Kwb, Fwb, D)

contains
    subroutine solver
        ! Auxiliaries
        integer :: info  ! status of operation

        ! External
        external :: dposv

        call dposv('U', nno*ndofn, 1, K, nno*ndofn, D, nno*ndofn, info)
    end subroutine solver
end program main
