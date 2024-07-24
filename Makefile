NAME	=	a.out
FFLAGS	=	-Kfast,parallel,openmp
FC		=	mpifrtpx
SRCDIR	=	./srcs
F90SRCS =   bnd_velocity.f90 \
            init_3d.f90 \
            init_4d.f90 \
            init.f90 \
            main.f90 \
            output_parameters.f90 \
            solve_couette_flow.f90
FCSRCS	=   calc_arith_coef_vis.f \
			calc_arith_tau.f \
			calc_div_tensor.f \
            calc_sij.f \
			calc_srcu.f \
            cpy.f \
			solution_sor4.f
SRCS	=	$(addprefix $(SRCDIR)/, $(F90SRCS)) \
			$(addprefix $(SRCDIR)/, $(FCSRCS))
OBJDIR	=	./objs
OBJS	=	$(F90SRCS:%.f90=$(OBJDIR)/%.o) \
			$(FCSRCS:%.f=$(OBJDIR)/%.o)

$(OBJDIR)/%.o: $(SRCDIR)/%.f90
		mkdir -p $(OBJDIR)
		$(FC) -c $(FFLAGS) $< -o $@

$(OBJDIR)/%.o: $(SRCDIR)/%.f
		mkdir -p $(OBJDIR)
		$(FC) -c $(FFLAGS) $< -o $@

$(NAME): $(OBJS)
	$(FC) $(OBJS) $(FFLAGS) -o $(NAME)

all: $(NAME)

clean:
	rm -rf $(OBJDIR)

fclean: clean
	rm -rf $(NAME)

re: fclean all

.SUFFIXES: .o .f .f90