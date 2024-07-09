subroutine solve_couette_flow(ni, nj, nk, u, un, rho, mu, dx, dy, dt)
	implicit none
	integer :: ni, nj, nk
	real(8) ::   u(-2:ni+3, -2:nj+3, -2:nk+3)
	real(8) ::  un(-2:ni+3, -2:nj+3, -2:nk+3)
	real(8) :: rho(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::  mu(-2:ni+3, -2:nj+3, -2:nk+3)
	real(8) :: dx, dy, dt

	real(8) :: nu, alphax, alphay

	integer :: i, j, k

	
	!$OMP PARALLEL DO &
  !$OMP SCHEDULE(static, 1) &
  !$OMP DEFAULT(none) &
  !$OMP PRIVATE(i, j, k) &
  !$OMP PRIVATE(nu, alphax, alphay) &
  !$OMP SHARED(ni, nj, nk) &
  !$OMP SHARED(u, un, rho, mu) &
  !$OMP SHARED(dx, dy, dt)
	do k = -2, nk+3
		do j = -1, nj+2
			do i = -1, ni+2
				nu = mu(i, j, k) / rho(i, j, k)
				alphax = nu * dt / (dx*dx)
				alphay = nu * dt / (dy*dy)
				un(i, j, k) = u(i, j, k) &
							 	+ alphax * (u(i-1, j  , k) - 2.0d0*u(i, j, k) + u(i+1, j  , k)) &
               	+ alphay * (u(i  , j-1, k) - 2.0d0*u(i, j, k) + u(i  , j+1, k))
				enddo
		enddo
	enddo
	!$OMP END PARALLEL DO


	return
end subroutine solve_couette_flow