#!/bin/bash

# 変数の定義
NAME="a.out"
SRCDIR="./srcs"
OBJDIR="./objs"
F90SRCS=(
    bnd_velocity.f90
    init_3d.f90
    init_4d.f90
    init.f90
    main.f90
    output_parameters.f90
    solve_couette_flow.f90
)
FCSRCS=(
    calc_arith_coef_vis.f
    calc_arith_tau.f
    calc_div_tensor.f
    calc_sij.f
    calc_srcu.f
    cpy.f
    solution_sor4.f
)
FFLAGS="-fopenmp"
FC="mpif90"  # または "mpifort" に変更

# オブジェクトディレクトリの作成
mkdir -p "$OBJDIR"

# オブジェクトファイルのリストを初期化
OBJS=()

# .f90ファイルのコンパイル
for src in "${F90SRCS[@]}"; do
    obj="$OBJDIR/${src%.f90}.o"
    "$FC" -c $FFLAGS "$SRCDIR/$src" -o "$obj"
    OBJS+=("$obj")
done

# .fファイルのコンパイル
for src in "${FCSRCS[@]}"; do
    obj="$OBJDIR/${src%.f}.o"
    "$FC" -c $FFLAGS "$SRCDIR/$src" -o "$obj"
    OBJS+=("$obj")
done

# オブジェクトファイルのリンク
"$FC" "${OBJS[@]}" $FFLAGS -o "$NAME"