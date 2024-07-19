!initialize all parameters=====================================================
subroutine init(ni, nj, nk, u, v, w, un, vn, wn, rho, mu, rhon, mun, &
                adv_u, adv_v, adv_w, adv_uo, adv_vo, adv_wo, &
                prs_u, prs_v, prs_w, s, tau, vis_u, vis_v, vis_w, &
                sum_fst_u, sum_fst_v, sum_fst_w, &
                sum_fst_un, sum_fst_vn, sum_fst_wn, src_u, src_v, src_w, &
                rhol, mul)
	implicit none
  integer :: ni, nj, nk
  real(8) ::          u(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::          v(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::          w(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::         un(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::         vn(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::         wn(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::        rho(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::         mu(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::       rhon(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::        mun(-2:ni+3, -2:nj+3, -2:nk+3)

  real(8) ::      adv_u(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::      adv_v(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::      adv_w(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::     adv_uo(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::     adv_vo(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::     adv_wo(-2:ni+3, -2:nj+3, -2:nk+3)

  real(8) ::      prs_u(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::      prs_v(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::      prs_w(-2:ni+3, -2:nj+3, -2:nk+3)

  real(8) ::          s(-2:ni+3, -2:nj+3, -2:nk+3, 6)
  real(8) ::        tau(-2:ni+3, -2:nj+3, -2:nk+3, 6)
  real(8) ::      vis_u(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::      vis_v(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::      vis_w(-2:ni+3, -2:nj+3, -2:nk+3)

  real(8) ::  sum_fst_u(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::  sum_fst_v(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::  sum_fst_w(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) :: sum_fst_un(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) :: sum_fst_vn(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) :: sum_fst_wn(-2:ni+3, -2:nj+3, -2:nk+3)
  
  real(8) ::      src_u(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::      src_v(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) ::      src_w(-2:ni+3, -2:nj+3, -2:nk+3)
  real(8) :: rhol, mul


	call init_3d(ni, nj, nk, u         , 0.0d0)
	call init_3d(ni, nj, nk, v         , 0.0d0)
	call init_3d(ni, nj, nk, w         , 0.0d0)
	call init_3d(ni, nj, nk, un        , 0.0d0)
	call init_3d(ni, nj, nk, vn        , 0.0d0)
	call init_3d(ni, nj, nk, wn        , 0.0d0)
	call init_3d(ni, nj, nk, rho       , rhol )
	call init_3d(ni, nj, nk, mu        , mul  )
  call init_3d(ni, nj, nk, rhon      , rhol )
	call init_3d(ni, nj, nk, mun       , mul  )

	call init_3d(ni, nj, nk, adv_u     , 0.0d0)
	call init_3d(ni, nj, nk, adv_v     , 0.0d0)
	call init_3d(ni, nj, nk, adv_w     , 0.0d0)
  call init_3d(ni, nj, nk, adv_uo    , 0.0d0)
	call init_3d(ni, nj, nk, adv_vo    , 0.0d0)
	call init_3d(ni, nj, nk, adv_wo    , 0.0d0)

  call init_3d(ni, nj, nk, prs_u     , 0.0d0)
	call init_3d(ni, nj, nk, prs_v     , 0.0d0)
	call init_3d(ni, nj, nk, prs_w     , 0.0d0)

	call init_4d(ni, nj, nk, 6, s      , 0.0d0)
	call init_4d(ni, nj, nk, 6, tau    , 0.0d0)
	call init_3d(ni, nj, nk, vis_u     , 0.0d0)
	call init_3d(ni, nj, nk, vis_v     , 0.0d0)
	call init_3d(ni, nj, nk, vis_w     , 0.0d0)

	call init_3d(ni, nj, nk, sum_fst_u , 0.0d0)
	call init_3d(ni, nj, nk, sum_fst_v , 0.0d0)
	call init_3d(ni, nj, nk, sum_fst_w , 0.0d0)
  call init_3d(ni, nj, nk, sum_fst_un, 0.0d0)
	call init_3d(ni, nj, nk, sum_fst_vn, 0.0d0)
	call init_3d(ni, nj, nk, sum_fst_wn, 0.0d0)

	call init_3d(ni, nj, nk, src_u     , 0.0d0)
	call init_3d(ni, nj, nk, src_v     , 0.0d0)
	call init_3d(ni, nj, nk, src_w     , 0.0d0)




  return
end subroutine init