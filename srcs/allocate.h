allocate(         u(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(         v(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(         w(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(        un(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(        vn(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(        wn(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(       rho(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(        mu(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(      rhon(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(       mun(-2:ni+3, -2:nj+3, -2:nk+3))

allocate(     adv_u(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(     adv_v(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(     adv_w(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(    adv_uo(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(    adv_vo(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(    adv_wo(-2:ni+3, -2:nj+3, -2:nk+3))

allocate(     prs_u(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(     prs_v(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(     prs_w(-2:ni+3, -2:nj+3, -2:nk+3))

allocate(         s(-2:ni+3, -2:nj+3, -2:nk+3, 6))
allocate(       tau(-2:ni+3, -2:nj+3, -2:nk+3, 6))
allocate(     vis_u(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(     vis_v(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(     vis_w(-2:ni+3, -2:nj+3, -2:nk+3))

allocate( sum_fst_u(-2:ni+3, -2:nj+3, -2:nk+3))
allocate( sum_fst_v(-2:ni+3, -2:nj+3, -2:nk+3))
allocate( sum_fst_w(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(sum_fst_un(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(sum_fst_vn(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(sum_fst_wn(-2:ni+3, -2:nj+3, -2:nk+3))

allocate(     src_u(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(     src_v(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(     src_w(-2:ni+3, -2:nj+3, -2:nk+3))

allocate(    au_w_u(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(    au_e_u(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(    au_s_u(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(    au_n_u(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(    au_b_u(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(    au_t_u(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(    au_p_u(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(   av_ws_u(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(   av_es_u(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(   av_wn_u(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(   av_en_u(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(   aw_wb_u(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(   aw_eb_u(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(   aw_wt_u(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(   aw_et_u(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(    av_w_v(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(    av_e_v(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(    av_s_v(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(    av_n_v(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(    av_b_v(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(    av_t_v(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(    av_p_v(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(   au_sw_v(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(   au_nw_v(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(   au_se_v(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(   au_ne_v(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(   aw_sb_v(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(   aw_nb_v(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(   aw_st_v(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(   aw_nt_v(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(    aw_w_w(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(    aw_e_w(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(    aw_s_w(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(    aw_n_w(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(    aw_b_w(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(    aw_t_w(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(    aw_p_w(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(   au_bw_w(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(   au_tw_w(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(   au_be_w(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(   au_te_w(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(   av_bs_w(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(   av_ts_w(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(   av_bn_w(-2:ni+3, -2:nj+3, -2:nk+3))
allocate(   av_tn_w(-2:ni+3, -2:nj+3, -2:nk+3))
