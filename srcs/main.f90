program main
implicit none
include 'mpif.h'


!ierr : error status
!nproc: num of process
!ID   : process id
integer :: ierr, nproc, ID

integer :: ni, nj
integer :: nmax, nstep
real(8) :: xl, yl
real(8) :: dx, dy, dt
real(8) :: rhol, rhog, mul, mug, sigma
real(8) :: uwall, ls

real(8), dimension(:,:), allocatable :: u, v, un, vn
real(8), dimension(:,:), allocatable :: rho, mu

integer :: i, j

!>mpi init=====================================================================

call mpi_init(ierr)
call mpi_comm_size(mpi_comm_world, nproc, ierr)
call mpi_comm_rank(mpi_comm_world, ID, ierr)

!>setting parameters===========================================================
!You have to set each parameter in this section appropriately.

!ni, nj: number of grid points over the entire region(ni,nj)
ni    = 32
nj    = 8

if (mod(ni, nproc).ne. 0 .or. mod(nj, nproc) .ne. 0) then
  if (ID .eq. 0) then
    write(0, '("nproc=", 1i9)') nproc
    write(0, *) 'Invalid number of PE'
    write(0, *) 'set PE (=nproc) to satisfy ni%nproc=0 and nj%nproc=0'
  endif
  call mpi_finalize(ierr)
  stop
endif

!nmax: num of max steps
!dt: time step
nmax  = 120000
dt    = 0.1d-2

!xl, yl: lengthes in x, y and z directions to describe domain size
!rho, mu: density, viscosity
!sigma: surface tension
xl    = 6.8d1
yl    = 1.36d1
rhol  = 0.81d0
rhog  = 0.81d0
mul   = 1.95d0
mug   = 1.95d0
sigma = 5.5d0

!uwall: velocity of the upper(lower) wall
!ls: slip length of the wall
uwall = 0.5d0
ls    = 1.625d0

if (ID .eq. 0) then
  call output_parameters(nproc, ni, nj, nmax, dt, xl, yl, rhol, rhog, &
                         mul, mug, sigma, uwall, ls)
endif
call mpi_barrier(mpi_comm_world, ierr)
call flush(6)


!>initialize parameters========================================================

!allocate memory for each variable
include 'allocate.h'

call init(ni, nj, u, v, un, vn, rho, mu, rhol, mul)
call mpi_barrier(mpi_comm_world, ierr)
call flush(6)

!dx, dy: grid widths
dx = xl / dble(ni)
dy = yl / dble(nj)

!>impose boundary conditions===================================================

call bnd_velocity(ni, nj, u, v, dy, uwall, ls)
call mpi_barrier(mpi_comm_world, ierr)
call flush(6)

!start solver==================================================================

do nstep = 1, nmax

call solve_couette_flow(ni, nj, u, un, rho, mu, dx, dy, dt)
call cpy(ni, nj, un, u)
call bnd_velocity(ni, nj, u, v, dy, uwall, ls)

!>end solver===================================================================

!enddo nstep
enddo

!>mpi finished=================================================================

call mpi_finalize(ierr)


stop
end program main