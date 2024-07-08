program main
implicit none
include 'mpif.h'


!ierr : error status
!nproc: num of process
!ID   : process id
integer :: ierr, nproc, ID

integer :: ni, nj, nmax
real(8) :: dt
real(8) :: xl, yl
real(8) :: rhol, rhog, mul, mug, sigma


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
nmax  = 1
dt    = 1.0d-2

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

call output_parameters(nproc, ni, nj, nmax, dt, xl, yl, rhol, rhog, &
                       mul, mug, sigma)

!>mpi finished=================================================================

call mpi_finalize(ierr)


stop
end program main