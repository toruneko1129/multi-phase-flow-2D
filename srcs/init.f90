!initialize all parameters to 0
subroutine init(ni, nj, nk, u, v, w, un, vn, wn, rho, mu, rhol, mul, s)
	implicit none
  integer :: ni, nj, nk
  real(8) ::   u(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::   v(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::   w(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::  un(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::  vn(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::  wn(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) :: rho(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::  mu(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::   s(-2:ni+3, -2:nj+3, -2:nk+3, 6)
  real(8) :: rhol, mul


	call init_3d(ni, nj, nk, u  , 0.0d0)
	call init_3d(ni, nj, nk, v  , 0.0d0)
	call init_3d(ni, nj, nk, w  , 0.0d0)
	call init_3d(ni, nj, nk, un , 0.0d0)
	call init_3d(ni, nj, nk, vn , 0.0d0)
	call init_3d(ni, nj, nk, wn , 0.0d0)
	call init_3d(ni, nj, nk, rho, rhol )
	call init_3d(ni, nj, nk, mu , mul  )


  return
end subroutine init