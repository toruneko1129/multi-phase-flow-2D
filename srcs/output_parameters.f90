!output setting parameters
subroutine output_parameters(nproc, ni, nj, nmax, dt, xl, yl, rhol, rhog, &
                             mul, mug, sigma)
  implicit none
  integer :: nproc, ni, nj, nk, nmax
  real(8) :: dt, xl, yl, zl, rhol, rhog, mul, mug, sigma

  write(*,'("nproc=",1i9)') nproc
  write(*,'("ni   =",1i9)') ni
  write(*,'("nj   =",1i9)') nj
  write(*,'("nmax =",1i9)') nmax
  write(*,'("dt   =",20e20.10)') dt
  write(*,'("xl   =",20e20.10)') xl
  write(*,'("yl   =",20e20.10)') yl
  write(*,'("rhol =",20e20.10)') rhol
  write(*,'("rhog =",20e20.10)') rhog
  write(*,'("mul  =",20e20.10)') mul
  write(*,'("mug  =",20e20.10)') mug
  write(*,'("sigma=",20e20.10)') sigma

  return
end subroutine output_parameters