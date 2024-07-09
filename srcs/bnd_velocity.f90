!impose velocity boundary condition
!x, z direction: periodic
!y direction: navier slip
subroutine bnd_velocity(ni, nj, nk, u, v, w, dy, uwall, ls)
  implicit none
  integer :: ni, nj, nk
  real(8) :: u(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) :: v(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) :: w(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) :: dy, uwall, ls

  integer :: i, j, k


!>x direction: periodic========================================================

  !$OMP PARALLEL DO &
  !$OMP SCHEDULE(static, 1) &
  !$OMP DEFAULT(none) &
  !$OMP PRIVATE(j, k) &
  !$OMP SHARED(ni, nj, nk) &
  !$OMP SHARED(u, v, w) 
  do k = -2, nk+3
    do j = -2, nj+3
      u(-2  , j, k) = u(ni-2, j, k)
      u(-1  , j, k) = u(ni-1, j, k)
      u(0   , j, k) = u(ni  , j, k)
      u(ni+1, j, k) = u(1   , j, k)
      u(ni+2, j, k) = u(2   , j, k)
      u(ni+3, j, k) = u(3   , j, k)

      v(-2  , j, k) = v(ni-2, j, k)
      v(-1  , j, k) = v(ni-1, j, k)
      v(0   , j, k) = v(ni  , j, k)
      v(ni+1, j, k) = v(1   , j, k)
      v(ni+2, j, k) = v(2   , j, k)
      v(ni+3, j, k) = v(3   , j, k)

      w(-2  , j, k) = w(ni-2, j, k)
      w(-1  , j, k) = w(ni-1, j, k)
      w(0   , j, k) = w(ni  , j, k)
      w(ni+1, j, k) = w(1   , j, k)
      w(ni+2, j, k) = w(2   , j, k)
      w(ni+3, j, k) = w(3   , j, k)
    enddo
  enddo
  !$OMP END PARALLEL DO

!>y direction: navier slip=====================================================

  !$OMP PARALLEL DO &
  !$OMP SCHEDULE(static, 1) &
  !$OMP DEFAULT(none) &
  !$OMP PRIVATE(i, k) &
  !$OMP SHARED(ni, nj, nk) &
  !$OMP SHARED(u, v, w) &
  !$OMP SHARED(dy, uwall, ls)
  do k = -2, nk+3
    do i = -2, ni+3
      u(i, 0   , k) = (2*ls-dy)/(2*ls+dy)*u(i, 1   , k)
      u(i, nj+1, k) = 2*dy/(2*ls+dy)*uwall &
                    + (2*ls-dy)/(2*ls+dy)*u(i, nj  , k)
      
      v(i, -2  , k) = 0.0d0
      v(i, -1  , k) = 0.0d0
      v(i, 0   , k) = 0.0d0
      v(i, nj  , k) = 0.0d0
      v(i, nj+1, k) = 0.0d0
      v(i, nj+2, k) = 0.0d0

      w(i, -2  , k) = 0.0d0
      w(i, -1  , k) = 0.0d0
      w(i, 0   , k) = 0.0d0
      w(i, nj  , k) = 0.0d0
      w(i, nj+1, k) = 0.0d0
      w(i, nj+2, k) = 0.0d0
    enddo
  enddo
  !$OMP END PARALLEL DO

!>z direction: periodic========================================================

  !$OMP PARALLEL DO &
  !$OMP SCHEDULE(static, 1) &
  !$OMP DEFAULT(none) &
  !$OMP PRIVATE(i, j) &
  !$OMP SHARED(ni, nj, nk) &
  !$OMP SHARED(u, v, w)
  do j = -2, nj+3
    do i = -2, ni+3
      u(i, j, -2  ) = u(i, j, nk-2)
      u(i, j, -1  ) = u(i, j, nk-1)
      u(i, j, 0   ) = u(i, j, nk  )
      u(i, j, nk+1) = u(i, j, 1   )
      u(i, j, nk+2) = u(i, j, 2   )
      u(i, j, nk+3) = u(i, j, 3   )

      v(i, j, -2  ) = v(i, j, nk-2)
      v(i, j, -1  ) = v(i, j, nk-1)
      v(i, j, 0   ) = v(i, j, nk  )
      v(i, j, nk+1) = v(i, j, 1   )
      v(i, j, nk+2) = v(i, j, 2   )
      v(i, j, nk+3) = v(i, j, 3   )

      w(i, j, -2  ) = w(i, j, nk-2)
      w(i, j, -1  ) = w(i, j, nk-1)
      w(i, j, 0   ) = w(i, j, nk  )
      w(i, j, nk+1) = w(i, j, 1   )
      w(i, j, nk+2) = w(i, j, 2   )
      w(i, j, nk+3) = w(i, j, 3   )
    enddo
  enddo
  !$OMP END PARALLEL DO


  return
end subroutine bnd_velocity
