program main
implicit none
include 'mpif.h'


!ierr : error status
!nproc: num of process
!ID   : process id
integer :: ierr, nproc, ID

integer :: ni, nj, nk
integer :: nmax, nstep, imon_t
real(8) :: xl, yl, zl
real(8) :: dx, dy, dz, dxinv, dyinv, dzinv, dt
real(8) :: rhol, rhog, mul, mug, sigma
real(8) :: uwall, ls

real(8), dimension(:, :, :), allocatable :: u, v, w, un, vn, wn
real(8), dimension(:, :, :), allocatable :: rho, mu, rhon, mun

real(8), dimension(:, :, :), allocatable :: adv_u, adv_v, adv_w
real(8), dimension(:, :, :), allocatable :: adv_uo, adv_vo, adv_wo

real(8), dimension(:, :, :), allocatable :: prs_u, prs_v, prs_w

real(8), dimension(:, :, :, :), allocatable :: s, tau
real(8), dimension(:, :, :), allocatable :: vis_u, vis_v, vis_w

real(8), dimension(:, :, :), allocatable :: sum_fst_u, sum_fst_v, sum_fst_w
real(8), dimension(:, :, :), allocatable :: sum_fst_un, sum_fst_vn, sum_fst_wn

real(8), dimension(:, :, :), allocatable :: src_u, src_v, src_w

real(8), dimension(:, :, :), allocatable :: au_w_u, au_e_u, au_s_u, au_n_u
real(8), dimension(:, :, :), allocatable :: au_b_u, au_t_u, au_p_u
real(8), dimension(:, :, :), allocatable :: av_ws_u, av_es_u, av_wn_u, av_en_u
real(8), dimension(:, :, :), allocatable :: aw_wb_u, aw_eb_u, aw_wt_u, aw_et_u
real(8), dimension(:, :, :), allocatable :: av_w_v, av_e_v, av_s_v, av_n_v
real(8), dimension(:, :, :), allocatable :: av_b_v, av_t_v, av_p_v
real(8), dimension(:, :, :), allocatable :: au_sw_v, au_nw_v, au_se_v, au_ne_v
real(8), dimension(:, :, :), allocatable :: aw_sb_v, aw_nb_v, aw_st_v, aw_nt_v
real(8), dimension(:, :, :), allocatable :: aw_w_w, aw_e_w, aw_s_w, aw_n_w
real(8), dimension(:, :, :), allocatable :: aw_b_w, aw_t_w, aw_p_w
real(8), dimension(:, :, :), allocatable :: au_bw_w, au_tw_w, au_be_w, au_te_w
real(8), dimension(:, :, :), allocatable :: av_bs_w, av_ts_w, av_bn_w, av_tn_w


integer :: i, j, k

!>mpi init=====================================================================

call mpi_init(ierr)
call mpi_comm_size(mpi_comm_world, nproc, ierr)
call mpi_comm_rank(mpi_comm_world, ID, ierr)

!>setting parameters===========================================================
!You have to set each parameter in this section appropriately.

!ni, nj: number of grid points over the entire region(ni,nj)
ni     = 32
nj     = 8
nk     = 2

if (mod(ni, nproc).ne. 0 .or. mod(nj, nproc) .ne. 0) then
  if (ID .eq. 0) then
    write(0, '("nproc=", 1i9)') nproc
    write(0, *) 'Invalid number of PE'
    write(0, *) 'set PE (=nproc) to satisfy ni%nproc=0 and nj%nproc=0'
  endif
  call mpi_finalize(ierr)
  stop
endif

!nmax: num of max steps
!imon_t: num of monitor steps
!dt: time step
nmax   = 50000
imon_t = 1000
dt     = 0.1d-2

!xl, yl: lengthes in x, y and z directions to describe domain size
!rho, mu: density, viscosity
!sigma: surface tension
xl     = 6.8d1
yl     = 1.36d1
zl     = 1.7d0
rhol   = 0.81d0
rhog   = 0.81d0
mul    = 1.95d0
mug    = 1.95d0
sigma  = 5.5d0

!uwall: velocity of the upper(lower) wall
!ls: slip length of the wall
uwall  = 0.5d0
ls     = 1.625d0

if (ID .eq. 0) then
  call output_parameters(nproc, ni, nj, nk, nmax, imon_t, dt, xl, yl, zl, &
                         rhol, rhog, mul, mug, sigma, uwall, ls)
endif
call mpi_barrier(mpi_comm_world, ierr)
call flush(6)


!>initialize parameters========================================================

!allocate memory for each variable
include 'allocate.h'

call init(ni, nj, nk, u, v, w, un, vn, wn, rho, mu, rhon, mun, &
          adv_u, adv_v, adv_w, adv_uo, adv_vo, adv_wo, &
          prs_u, prs_v, prs_w, s, tau, vis_u, vis_v, vis_w, &
          sum_fst_u, sum_fst_v, sum_fst_w, &
          sum_fst_un, sum_fst_vn, sum_fst_wn, src_u, src_v, src_w, &
          au_w_u, au_e_u, au_s_u, au_n_u, au_b_u, au_t_u, au_p_u, &
          av_ws_u, av_es_u, av_wn_u, av_en_u, &
          aw_wb_u, aw_eb_u, aw_wt_u, aw_et_u, &
          av_w_v, av_e_v, av_s_v, av_n_v, av_b_v, av_t_v, av_p_v, &
          au_sw_v, au_nw_v, au_se_v, au_ne_v, &
          aw_sb_v, aw_nb_v, aw_st_v, aw_nt_v, &
          aw_w_w, aw_e_w, aw_s_w, aw_n_w, aw_b_w, aw_t_w, aw_p_w, &
          au_bw_w, au_tw_w, au_be_w, au_te_w, &
          av_bs_w, av_ts_w, av_bn_w, av_tn_w, &
          rhol, mul)
call mpi_barrier(mpi_comm_world, ierr)
call flush(6)

!dx, dy: grid widths
dx = xl / dble(ni)
dy = yl / dble(nj)
dz = zl / dble(nk)
dxinv = 1.0d0 / dx
dyinv = 1.0d0 / dy
dzinv = 1.0d0 / dz


!start solver==================================================================

do nstep = 1, nmax

!>impose boundary conditions===================================================

call bnd_velocity(ni, nj, nk, u, v, w, dy, uwall, ls)
call mpi_barrier(mpi_comm_world, ierr)
call flush(6)

!>solve viscous term===========================================================

call calc_sij(ni, nj, nk, dxinv, dyinv, dzinv, u, v, w, s)
call calc_arith_tau(ni, nj, nk, s, mu, tau)
call calc_div_tensor(ni, nj, nk, dxinv, dyinv, dzinv, tau, vis_u, vis_v, vis_w)

!>calculate ustar(src_[uvw])===================================================

call calc_srcu(ni, nj, nk, dt, &
               u, v, w, rho, rhon, &
               adv_u     , adv_v     , adv_w     , &
               adv_uo    , adv_vo    , adv_wo    , &
               prs_u     , prs_v     , prs_w     , &
               vis_u     , vis_v     , vis_w     , &
               sum_fst_u , sum_fst_v , sum_fst_w , &
               sum_fst_un, sum_fst_vn, sum_fst_wn, &
               src_u     , src_v     , src_w)
call calc_arith_coef_vis(ni, nj, nk, dxinv, dyinv, dzinv, mun, &
                         au_w_u , au_e_u , au_s_u , au_n_u , &
                         au_b_u , au_t_u , au_p_u ,          &
                         av_ws_u, av_es_u, av_wn_u, av_en_u, &
                         aw_wb_u, aw_eb_u, aw_wt_u, aw_et_u, &
                         av_w_v , av_e_v , av_s_v , av_n_v , &
                         av_b_v , av_t_v , av_p_v ,          &
                         au_sw_v, au_nw_v, au_se_v, au_ne_v, &
                         aw_sb_v, aw_nb_v, aw_st_v, aw_nt_v, &
                         aw_w_w , aw_e_w , aw_s_w , aw_n_w , &
                         aw_b_w , aw_t_w , aw_p_w ,          &
                         au_bw_w, au_tw_w, au_be_w, au_te_w, &
                         av_bs_w, av_ts_w, av_bn_w, av_tn_w)
call solu_sor4(ni, nj, nk, nstep, imon_t, dt, rho, rhon, &
               au_w_u , au_e_u , au_s_u , au_n_u , &
               au_b_u , au_t_u , au_p_u ,          &
               av_ws_u, av_es_u, av_wn_u, av_en_u, &
               aw_wb_u, aw_eb_u, aw_wt_u, aw_et_u, &
               av_w_v , av_e_v , av_s_v , av_n_v , &
               av_b_v , av_t_v , av_p_v ,          &
               au_sw_v, au_nw_v, au_se_v, au_ne_v, &
               aw_sb_v, aw_nb_v, aw_st_v, aw_nt_v, &
               aw_w_w , aw_e_w , aw_s_w , aw_n_w , &
               aw_b_w , aw_t_w , aw_p_w ,          &
               au_bw_w, au_tw_w, au_be_w, au_te_w, &
               av_bs_w, av_ts_w, av_bn_w, av_tn_w, &
               src_u, src_v, src_w, un, vn, wn, dy, uwall, ls)

call cpy(ni, nj, nk, un, u)
call bnd_velocity(ni, nj, nk, u, v, w, dy, uwall, ls)

!>end solver===================================================================

!enddo nstep
enddo

!>mpi finished=================================================================

write(*, *)
do j = 1, nj
write(*,'("u= ",1E20.10," uhat= ",1E20.10)') u(ni/2, j, 1), 0.5d0 * ((dble(j)-0.5d0)*dy+1.625d0) / (yl+3.25d0)
enddo

call mpi_finalize(ierr)


stop
end program main