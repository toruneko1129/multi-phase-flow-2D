!initialize 2darray
!fill 2D array values ​​with 0
subroutine init_3d(ni, nj, nk, arr3d, value)
  implicit none
  integer :: ni, nj, nk
  real(8) :: arr3d(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) :: value

  integer :: i, j, k


  !$OMP PARALLEL DO &
  !$OMP SCHEDULE(static, 1) &
  !$OMP DEFAULT(none) &
  !$OMP PRIVATE(i, j, k) &
  !$OMP SHARED(ni, nj, nk, arr3d, value)
  do k = -2, nk+3
    do j = -2, nj+3
      do i = -2, ni+3
        arr3d(i, j, k) = value
      enddo
    enddo
  enddo
  !$OMP END PARALLEL DO


  return
end subroutine init_3d