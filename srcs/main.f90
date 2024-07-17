program main
implicit none
include 'mpif.h'


!ierr : error status
!nproc: num of process
!ID   : process id
integer :: ierr, nproc, ID

integer :: ni, nj, nk
integer :: nmax, nstep
real(8) :: xl, yl, zl
real(8) :: dx, dy, dz, dxinv, dyinv, dzinv, dt
real(8) :: rhol, rhog, mul, mug, sigma
real(8) :: uwall, ls

real(8), dimension(:, :, :), allocatable :: u, v, w, un, vn, wn
real(8), dimension(:, :, :), allocatable :: rho, mu
real(8), dimension(:, :, :, :), allocatable :: s, tau
real(8), dimension(:, :, :), allocatable :: vis_u, vis_v, vis_w

integer :: i, j, k

!>mpi init=====================================================================

call mpi_init(ierr)
call mpi_comm_size(mpi_comm_world, nproc, ierr)
call mpi_comm_rank(mpi_comm_world, ID, ierr)

!>setting parameters===========================================================
!You have to set each parameter in this section appropriately.

!ni, nj: number of grid points over the entire region(ni,nj)
ni    = 32
nj    = 8
nk    = 2

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
dt    = 0.1d-2

!xl, yl: lengthes in x, y and z directions to describe domain size
!rho, mu: density, viscosity
!sigma: surface tension
xl    = 6.8d1
yl    = 1.36d1
zl    = 1.7d0
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
  call output_parameters(nproc, ni, nj, nk, nmax, dt, xl, yl, zl, &
                         rhol, rhog, mul, mug, sigma, uwall, ls)
endif
call mpi_barrier(mpi_comm_world, ierr)
call flush(6)


!>initialize parameters========================================================

!allocate memory for each variable
include 'allocate.h'

call init(ni, nj, nk, u, v, w, un, vn, wn, rho, mu, s, tau, &
          vis_u, vis_v, vis_w, rhol, mul)
call mpi_barrier(mpi_comm_world, ierr)
call flush(6)

!dx, dy: grid widths
dx = xl / dble(ni)
dy = yl / dble(nj)
dz = zl / dble(nk)
dxinv = 1.0d0 / dx
dyinv = 1.0d0 / dy
dzinv = 1.0d0 / dz

!>impose boundary conditions===================================================

call bnd_velocity(ni, nj, nk, u, v, w, dy, uwall, ls)
call mpi_barrier(mpi_comm_world, ierr)
call flush(6)

!start solver==================================================================

do nstep = 1, nmax

call solve_couette_flow(ni, nj, nk, u, un, rho, mu, dx, dy, dt)

!implement instead of solve_couette_flow
call calc_sij(ni, nj, nk, dxinv, dyinv, dzinv, u, v, w, s)
call calc_arith_tau(ni, nj, nk, s, mu, tau)
call cpy(ni, nj, nk, un, u)
call bnd_velocity(ni, nj, nk, u, v, w, dy, uwall, ls)
call calc_div_tensor(ni, nj, nk, dxinv, dyinv, dzinv, tau, vis_u, vis_v, vis_w)

!>end solver===================================================================

!enddo nstep
enddo

!>debug
write(*, *)
write(*,'("sij4  = ",1E20.10)') s(ni, nj, 1, 4)
write(*,'("tauij4= ",1E20.10)') tau(ni, nj, 1, 4)
write(*,'("vis_u= ",1E20.10)') vis_u(16, 8, 1)
write(*,'("un= ",1E20.10)') un(16, 8, 1)

!>mpi finished=================================================================

call mpi_finalize(ierr)


stop
end program main
