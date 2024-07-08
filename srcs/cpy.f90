!copy src variables to dst
subroutine cpy(ni, nj, src, dst)
  implicit none
  integer ni, nj
  real(8) :: src(0:ni+1, 0:nj+1)
  real(8) :: dst(0:ni+1, 0:nj+1)

  integer i, j

	!$OMP PARALLEL DO &
  !$OMP SCHEDULE(static, 1) &
  !$OMP DEFAULT(none) &
  !$OMP PRIVATE(i, j) &
  !$OMP SHARED(ni, nj) &
  !$OMP SHARED(src, dst)
  do j = 0, nj+1
    do i = 0, ni+1
      dst(i, j) = src(i, j)
    enddo
  enddo
	!$OMP END PARALLEL DO


  return
end subroutine cpy