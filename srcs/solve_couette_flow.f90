subroutine solve_couette_flow(ni, nj, u, un, rho, mu, dx, dy, dt)
	implicit none
	integer :: ni, nj
	real(8) ::   u(0:ni+1, 0:nj+1)
	real(8) ::  un(0:ni+1, 0:nj+1)
	real(8) :: rho(0:ni+1, 0:nj+1)
	real(8) ::  mu(0:ni+1, 0:nj+1)
	real(8) :: dx, dy, dt

	real(8) :: nu, alphax, alphay

	integer :: i, j

	
	!$OMP PARALLEL DO &
  !$OMP SCHEDULE(static, 1) &
  !$OMP DEFAULT(none) &
  !$OMP PRIVATE(i, j) &
  !$OMP PRIVATE(nu, alphax, alphay) &
  !$OMP SHARED(ni, nj) &
  !$OMP SHARED(u, un, rho, mu) &
  !$OMP SHARED(dx, dy, dt)
	do j = -1, nj+2
		do i = -1, ni+2
			nu = mu(i, j) / rho(i, j)
			alphax = nu * dt / (dx*dx)
			alphay = nu * dt / (dy*dy)
			un(i, j) = u(i, j) &
							 + alphax * (u(i-1, j  ) - 2.0d0*u(i, j) + u(i+1, j  )) &
               + alphay * (u(i  , j-1) - 2.0d0*u(i, j) + u(i  , j+1))
		enddo
	enddo
	!$OMP END PARALLEL DO


	return
end subroutine solve_couette_flow