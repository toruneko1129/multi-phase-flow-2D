!calculate strain rate tensor sk
subroutine calc_sij(ni, nj, dxinv, dyinv, u, v, s)
  implicit none
  integer :: ni, nj
  real(8) :: dxinv, dyinv
  real(8) :: u(0:ni+1, 0:nj+1)
  real(8) :: v(0:ni+1, 0:nj+1)
  real(8) :: s(0:ni+1, 0:nj+1, 3)

  integer :: i, j

!>calc s11======================================================================

  !$OMP PARALLEL DO &
  !$OMP SCHEDULE(static, 1) &
  !$OMP DEFAULT(none) &
  !$OMP PRIVATE(i, j) &
  !$OMP SHARED(ni, nj) &
  !$OMP SHARED(dxinv, u, s)
  do j = 0, nj+1
    do i = 1, ni+1
      s(i, j, 1) = (-u(i-1, j) + u(i, j)) * dxinv
    enddo
  enddo
  !$OMP END PARALLEL DO

!>calc s22======================================================================

  !$OMP PARALLEL DO &
  !$OMP SCHEDULE(static, 1) &
  !$OMP DEFAULT(none) &
  !$OMP PRIVATE(i, j) &
  !$OMP SHARED(ni, nj) &
  !$OMP SHARED(dyinv, v, s)
  do j = 1, nj+1
    do i = 0, ni+1
      s(i, j, 2) = (-v(i, j-1) + v(i, j)) * dyinv
    enddo
  enddo
  !$OMP END PARALLEL DO

!>calc s12(s21)=================================================================

  !$OMP PARALLEL DO &
  !$OMP SCHEDULE(static, 1) &
  !$OMP DEFAULT(none) &
  !$OMP PRIVATE(i, j) &
  !$OMP SHARED(ni, nj) &
  !$OMP SHARED(dxinv, dyinv, u, v, s)
  do j = 0, nj
    do i = 0, ni
      s(i, j, 3) = ((-u(i, j) + u(i  , j+1)) * dyinv &
                 +  (-v(i, j) + v(i+1, j  )) * dxinv) * 0.5d0
    enddo
  enddo
  !$OMP END PARALLEL DO

  return
end subroutine calc_sij