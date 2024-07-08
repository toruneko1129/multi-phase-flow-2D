!initialize all parameters to 0
subroutine init(ni, nj, u, un, rho, mu, rhol, mul)
	implicit none


  integer :: ni, nj
  real(8) :: u(0:ni+1, 0:nj+1)
  real(8) :: un(0:ni+1, 0:nj+1)
  real(8) :: rho(0:ni+1, 0:nj+1)
  real(8) :: mu(0:ni+1, 0:nj+1)
	
  real(8) :: rhol, mul


	call init_2d(ni, nj, u, 0.0d0)
	call init_2d(ni, nj, un, 0.0d0)
	call init_2d(ni, nj, rho, rhol)
	call init_2d(ni, nj, mu, mul)

  return
end subroutine init