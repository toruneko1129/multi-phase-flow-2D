!initialize 4darray
!fill 4D array values ​​with 0
subroutine init_4d(ni, nj, nk, nl, arr4d, value)
  implicit none
  integer :: ni, nj, nk, nl
  real(8) :: arr4d(-2:ni+3, -2:nj+3, -2:nk+3, nl)
  real(8) :: value

  integer :: i, j, k, l


  !$OMP PARALLEL DO &
  !$OMP SCHEDULE(static, 1) &
  !$OMP DEFAULT(none) &
  !$OMP PRIVATE(i, j, k, l) &
  !$OMP SHARED(ni, nj, nk, nl, arr4d, value)
  do l = 1, nl
    do k = -2, nk+3
      do j = -2, nj+3
        do i = -2, ni+3
          arr4d(i, j, k, l) = value
        enddo
      enddo
    enddo
  enddo
  !$OMP END PARALLEL DO


  return
end subroutine init_4d