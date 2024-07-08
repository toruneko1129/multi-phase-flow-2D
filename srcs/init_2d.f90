!initialize 2darray
!fill 2D array values ​​with 0
subroutine init_2d(ni, nj, arr2d, value)
  implicit none
  integer :: ni, nj
  real(8) :: arr2d(0:ni+1, 0:nj+1)
  real(8) :: value

  integer :: i, j


  !$OMP PARALLEL DO &
  !$OMP SCHEDULE(static, 1) &
  !$OMP DEFAULT(none) &
  !$OMP PRIVATE(i, j) &
  !$OMP SHARED(ni, nj, arr2d, value)
  do j = 0, nj+1
    do i = 0, ni+1
      arr2d(i, j) = value
    enddo
  enddo
  !$OMP END PARALLEL DO


  return
end subroutine init_2d