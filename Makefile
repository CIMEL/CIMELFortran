f90 = ifort
#f90 = gfortran
IFORT_FLAGS= -O0 -debug -save -r8 -g -traceback 
#IFORT_FLAGS= -fast -save -r8
#IFORT_FLAGS= -O0 -fno-automatic -fdefault-real-8 -g -fbacktrace 
target_name = iP_ifort.out
#target_name = iP_gfort.out

OBJECT_LIST =  COMMON_shura.o \
        BIBATM.o \
		BIBOD.o  \
        mo_par_DLS.o \
        iP2003LAST_shura.o \
		back_out_shura.o \
        iterqP_N.o \
        fishmxSP_POL.o \
        SMOOTH.JUL.01.o \
        intrpl_linear.o \
        intrpl_spline.o \
        phase_func.o \
        matrix_optchr_LS.o \
        matrix_intrpl_LS.o \
        matrix_fixget_S.o \
        sizedstr.o 

###########################################################################	


$(target_name) : $(OBJECT_LIST)
	$(f90) -o $(target_name) $(IFORT_FLAGS) $(OBJECT_LIST)   

COMMON_shura.o : COMMON_shura.f
	$(f90) -c  $(IFORT_FLAGS) COMMON_shura.f

BIBATM.o : BIBATM.f
	$(f90) -c $(IFORT_FLAGS) BIBATM.f

BIBOD.o : BIBOD.f
	$(f90) -c $(IFORT_FLAGS) BIBOD.f

mo_par_DLS.o: mo_par_DLS.f90
	$(f90) -c $(IFORT_FLAGS) mo_par_DLS.f90
	
iP2003LAST_shura.o : iP2003LAST_shura.f
	$(f90) -c $(IFORT_FLAGS) iP2003LAST_shura.f  

back_out_shura.o : back_out_shura.f
	$(f90) -c $(IFORT_FLAGS) back_out_shura.f 

iterqP_N.o : iterqP_N.f
	$(f90) -c $(IFORT_FLAGS) iterqP_N.f

fishmxSP_POL.o : fishmxSP_POL.f
	$(f90) -c $(IFORT_FLAGS) fishmxSP_POL.f 

SMOOTH.JUL.01.o : SMOOTH.JUL.01.f
	$(f90) -c $(IFORT_FLAGS) SMOOTH.JUL.01.f

intrpl_linear.o : intrpl_linear.f90 
	$(f90) -c $(IFORT_FLAGS) intrpl_linear.f90  
	
intrpl_spline.o : intrpl_spline.f90 
	$(f90) -c $(IFORT_FLAGS) intrpl_spline.f90   

phase_func.o : phase_func.f90 
	$(f90) -c $(IFORT_FLAGS) phase_func.f90	

matrix_optchr_LS.o : matrix_optchr_LS.f 
	$(f90) -c $(IFORT_FLAGS) matrix_optchr_LS.f	

matrix_intrpl_LS.o : matrix_intrpl_LS.f 
	$(f90) -c $(IFORT_FLAGS) matrix_intrpl_LS.f
	
matrix_fixget_S.o : matrix_fixget_S.f 
	$(f90) -c $(IFORT_FLAGS) matrix_fixget_S.f
	
sizedstr.o : sizedstr.f90 
	$(f90) -c $(IFORT_FLAGS) sizedstr.f90
	
################################################################################	
#CLEANUP RULES
cleanall: 
	-rm -f *.o 
	-rm -f *.mod 
	-rm -f $(target_name) 
	

	
