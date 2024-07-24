!output setting parameters
subroutine output_parameters(nproc, ni, nj, nk, nmax, imon_t, dt, xl, yl, zl, &
                             rhol, rhog, mul, mug, sigma, uwall, ls)
  implicit none
  integer :: nproc, ni, nj, nk, nmax, imon_t
  real(8) :: dt, xl, yl, zl, rhol, rhog, mul, mug, sigma, uwall, ls

  write(*, '("nproc =",1i9)') nproc
  write(*, '("ni    =",1i9)') ni
  write(*, '("nj    =",1i9)') nj
  write(*, '("nk    =",1i9)') nk
  write(*, '("nmax  =",1i9)') nmax
  write(*, '("imon_t=",1i9)') imon_t
  write(*, '("dt    =",20e20.10)') dt
  write(*, '("xl    =",20e20.10)') xl
  write(*, '("yl    =",20e20.10)') yl
  write(*, '("zl    =",20e20.10)') zl
  write(*, '("rhol  =",20e20.10)') rhol
  write(*, '("rhog  =",20e20.10)') rhog
  write(*, '("mul   =",20e20.10)') mul
  write(*, '("mug   =",20e20.10)') mug
  write(*, '("sigma =",20e20.10)') sigma
  write(*, '("uwall =",20e20.10)') uwall
  write(*, '("ls    =",20e20.10)') ls

  return
end subroutine output_parameters