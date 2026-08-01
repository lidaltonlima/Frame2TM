module StructureData
    use iso_fortran_env, only: real64
    implicit none
    save

    ! =============================================================================================
    ! Vars to Structure Data
    ! =============================================================================================
    ! Parameters **********************************************************************************
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
    real(real64), allocatable :: Kg(:, :)  ! global stiffness global
    real(real64), allocatable :: Fg(:)  ! global vector of loads
    real(real64), allocatable :: Dg(:)  ! global displacements
    real(real64), allocatable :: Rg(:)  ! reactions
    real(real64), allocatable :: ERl(:, :)  ! elements reactions
    real(real64), allocatable :: EEl(:, :)  ! elements efforts

    ! Aux *****************************************************************************************
    integer :: dim  ! dimension of matrices and vectors
    integer :: E_dim  ! dimension of matrices and vectors
end module StructureData
