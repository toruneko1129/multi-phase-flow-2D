!impose velocity boundary condition
!x direction: periodic
!y direction: navier slip
subroutine bnd_velocity(ni, nj, u, v, dy, uwall, ls)
  implicit none
  integer :: ni, nj
  real(8) :: u(0:ni+1, 0:nj+1)
  real(8) :: v(0:ni+1, 0:nj+1)
  real(8) :: dy, uwall, ls

  integer :: i, j


!>x direction: periodic========================================================

  !$OMP PARALLEL DO &
  !$OMP SCHEDULE(static, 1) &
  !$OMP DEFAULT(none) &
  !$OMP PRIVATE(j) &
  !$OMP SHARED(ni, nj) &
  !$OMP SHARED(u, v) 
  do j = 0, nj+1
    u(0   , j) = u(ni, j)
    u(ni+1, j) = u(1 , j)

    v(0   , j) = v(ni, j)
    v(ni+1, j) = v(1 , j)
  enddo
  !$OMP END PARALLEL DO

!>y direction: navier slip=====================================================

  !$OMP PARALLEL DO &
  !$OMP SCHEDULE(static, 1) &
  !$OMP DEFAULT(none) &
  !$OMP PRIVATE(i) &
  !$OMP SHARED(ni, nj) &
  !$OMP SHARED(u, v) &
  !$OMP SHARED(dy, uwall, ls)

  do i = 0, ni+1
    u(i, 0   ) = (2*ls-dy)/(2*ls+dy)*u(i, 1)
    u(i, nj+1) = 2*dy/(2*ls+dy)*uwall &
               + (2*ls-dy)/(2*ls+dy)*u(i, nj)
    v(i, 0   ) = 0.0d0
    v(i, nj+1) = 0.0d0
  enddo

  !$OMP END PARALLEL DO


  return
end subroutine bnd_velocity